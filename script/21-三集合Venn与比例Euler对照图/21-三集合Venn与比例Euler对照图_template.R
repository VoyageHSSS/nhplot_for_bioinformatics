#!/usr/bin/env Rscript

# plot_nh shared color system bootstrap (do not remove)
local({
  color_module <- Sys.getenv("PLOT_NH_COLOR_MODULE", unset = "")
  if (!nzchar(color_module) || !file.exists(color_module)) {
    file_arg <- grep("^--file=", commandArgs(), value = TRUE)
    start_dir <- if (length(file_arg)) dirname(sub("^--file=", "", file_arg[[1L]])) else getwd()
    start_dir <- normalizePath(start_dir, mustWork = FALSE)
    for (level in seq_len(12L)) {
      candidates <- c(
        file.path(start_dir, "manual_library", "scripts", "plot_nh_colors.R"),
        file.path(start_dir, "scripts", "plot_nh_colors.R"),
        file.path(start_dir, "plot_nh_colors.R")
      )
      hit <- candidates[file.exists(candidates)]
      if (length(hit)) {
        color_module <- hit[[1L]]
        break
      }
      parent_dir <- dirname(start_dir)
      if (identical(parent_dir, start_dir)) break
      start_dir <- parent_dir
    }
  }
  if (!nzchar(color_module) || !file.exists(color_module)) {
    stop("plot_nh shared color module not found; set PLOT_NH_COLOR_MODULE.", call. = FALSE)
  }
  source(normalizePath(color_module, mustWork = TRUE), local = .GlobalEnv)
  plot_nh_color_context()
})

or_else <- function(x, y) if (is.null(x)) y else x
`%||%` <- or_else
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  flag <- paste0("--", name)
  index <- match(flag, args)
  if (!is.na(index) && index < length(args)) return(args[[index + 1L]])
  inline <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(inline)) return(sub(paste0("^", flag, "="), "", inline[[1L]]))
  default
}

config_path <- arg_value("config")
if (is.null(config_path) || !file.exists(config_path)) stop("--config is required", call. = FALSE)
packages <- c("jsonlite", "ggplot2", "eulerr", "patchwork", "ragg", "svglite")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)

cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
params <- or_else(cfg$params, list())
gp <- function(name, default = NULL) or_else(params[[name]], default)
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
source_files <- vapply(sys.frames(), function(frame) frame$ofile %||% "", character(1L))
source_file <- tail(source_files[nzchar(source_files)], 1L)
script_file <- if (length(source_file)) source_file else if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else NULL
script_dir <- dirname(normalizePath(script_file, mustWork = TRUE))
output_dir <- arg_value("output-dir", cfg$output_dir %||% file.path(script_dir, "runtime", "output"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(name, default, lower = -Inf, upper = Inf) {
  if (is.list(gp(name, default))) stop(name, " must be a complete scalar", call. = FALSE)
  value <- suppressWarnings(as.numeric(gp(name, default)))
  if (length(value) != 1L || !is.finite(value) || value < lower || value > upper) {
    stop(sprintf("%s must be between %s and %s", name, lower, upper), call. = FALSE)
  }
  value
}
chr <- function(name, default, allow_empty = FALSE) {
  if (is.list(gp(name, default))) stop(name, " must be a complete scalar", call. = FALSE)
  value <- as.character(gp(name, default))
  if (length(value) != 1L || is.na(value)) stop(name, " must be one text value", call. = FALSE)
  value <- trimws(value)
  if (!allow_empty && !nzchar(value)) stop(name, " cannot be empty", call. = FALSE)
  value
}
bool <- function(name, default) {
  value <- gp(name, default)
  if (is.logical(value) && length(value) == 1L && !is.na(value)) return(value)
  if (is.list(value) || length(value) != 1L || anyNA(value)) stop(name, " must be a complete scalar", call. = FALSE)
  value <- tolower(trimws(as.character(value)))
  if (length(value) != 1L || !value %in% c("true", "false", "1", "0", "yes", "no")) stop(name, " must be true or false", call. = FALSE)
  value %in% c("true", "1", "yes")
}
pick <- function(name, default) {
  value <- chr(name, default)
  if (!grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", value)) stop(name, " must be a hex color", call. = FALSE)
  value
}
split_text <- function(value, separator = ",") {
  values <- trimws(strsplit(value, separator, fixed = TRUE)[[1L]])
  values[nzchar(values)]
}
palette_value <- function(name, default) {
  values <- split_text(chr(name, default))
  if (!length(values) || any(!grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", values))) {
    stop(name, " must be comma-separated hex colors", call. = FALSE)
  }
  values
}
resolve_font <- function(value) switch(value, "Times New Roman" = "Times", value)

resolve_input <- function(value) {
  if (!nzchar(value)) return(file.path(script_dir, "assets", "otu.txt"))
  candidates <- unique(c(value, file.path(dirname(config_path), value), file.path(script_dir, value), file.path(getwd(), value)))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Input file not found: ", value, call. = FALSE)
  normalizePath(hit[[1L]], mustWork = TRUE)
}
read_input <- function(path, sheet) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "rds") return(as.data.frame(readRDS(path), check.names = FALSE))
  signature <- readBin(path, what = "raw", n = 8L)
  is_excel <- (length(signature) >= 2L && identical(as.integer(signature[1:2]), c(80L, 75L))) ||
    (length(signature) >= 8L && identical(as.integer(signature[1:8]), c(208L, 207L, 17L, 224L, 161L, 177L, 26L, 225L)))
  if (is_excel || extension %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Install readxl to read Excel files", call. = FALSE)
    sheet_arg <- if (nzchar(sheet)) sheet else 1L
    return(as.data.frame(readxl::read_excel(path, sheet = sheet_arg, col_types = "text", .name_repair = "minimal"), check.names = FALSE))
  }
  header <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(header)) stop("Input file is empty: ", path, call. = FALSE)
  if (grepl("\t", header, fixed = TRUE)) {
    utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")
  } else if (grepl(",", header, fixed = TRUE)) {
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")
  } else {
    utils::read.table(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")
  }
}

excel_sheet <- chr("excel_sheet", "", allow_empty = TRUE)
input_path <- resolve_input(chr("data_path", "", allow_empty = TRUE))
builtin_data <- normalizePath(input_path) %in% normalizePath(file.path(script_dir, "assets", c("otu.txt", "data.xlsx")))
raw <- read_input(input_path, excel_sheet)
if (!is.data.frame(raw) || nrow(raw) < 1L || ncol(raw) < 4L) stop("Input must contain an ID column and at least three measurement columns", call. = FALSE)
if (anyNA(names(raw)) || any(!nzchar(trimws(names(raw)))) || anyDuplicated(names(raw))) {
  stop("Input column names must be complete and unique", call. = FALSE)
}

id_column <- chr("id_column", "OTUID")
if (!id_column %in% names(raw)) stop("ID column not found: ", id_column, call. = FALSE)
feature_ids <- trimws(as.character(raw[[id_column]]))
if (anyNA(feature_ids) || any(!nzchar(feature_ids)) || anyDuplicated(feature_ids)) {
  stop("Feature IDs must be complete and unique", call. = FALSE)
}

set_names <- split_text(chr("set_names", "A,B,C"))
column_groups <- split_text(chr("set_columns", "A1,A2,A3|B1,B2,B3|C1,C2,C3"), "|")
column_groups <- lapply(column_groups, split_text)
if (any(grepl("&", set_names, fixed = TRUE))) stop("Set names cannot contain & (Euler region separator)", call. = FALSE)
if (length(set_names) != 3L || anyDuplicated(set_names)) stop("set_names must contain exactly three unique names", call. = FALSE)
if (length(column_groups) != 3L || any(!lengths(column_groups))) stop("set_columns must contain three pipe-separated non-empty column groups", call. = FALSE)
selected_columns <- unlist(column_groups, use.names = FALSE)
if (id_column %in% selected_columns) stop("ID and measurement roles must differ", call. = FALSE)
if (anyDuplicated(selected_columns)) stop("Each measurement column may belong to only one set", call. = FALSE)
missing_columns <- setdiff(selected_columns, names(raw))
if (length(missing_columns)) stop("Missing measurement columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)

measurements <- raw[selected_columns]
measurements[] <- lapply(measurements, function(x) suppressWarnings(as.numeric(as.character(x))))
if (anyNA(measurements) || any(!is.finite(as.matrix(measurements)))) stop("Measurement columns must contain finite numeric values without missing data", call. = FALSE)
if (any(as.matrix(measurements) < 0)) stop("Measurement values must be non-negative", call. = FALSE)

presence_threshold <- num("presence_threshold", 0, 0)
minimum_present_replicates <- num("minimum_present_replicates", 1, 1)
if (minimum_present_replicates != floor(minimum_present_replicates)) stop("minimum_present_replicates must be an integer", call. = FALSE)
minimum_present_replicates <- as.integer(minimum_present_replicates)
if (any(minimum_present_replicates > lengths(column_groups))) {
  stop("minimum_present_replicates cannot exceed the number of columns in a set", call. = FALSE)
}
sets <- lapply(column_groups, function(columns) {
  present <- rowSums(as.data.frame(measurements[columns]) > presence_threshold) >= minimum_present_replicates
  feature_ids[present]
})
names(sets) <- set_names
if (any(!lengths(sets))) stop("Every set must contain at least one feature", call. = FALSE)
union_ids <- Reduce(union, sets)
if (!length(union_ids)) stop("No features pass the presence rule", call. = FALSE)

a <- sets[[1L]]
b <- sets[[2L]]
c_set <- sets[[3L]]
regions <- c(
  length(setdiff(a, union(b, c_set))),
  length(setdiff(b, union(a, c_set))),
  length(setdiff(c_set, union(a, b))),
  length(setdiff(intersect(a, b), c_set)),
  length(setdiff(intersect(a, c_set), b)),
  length(setdiff(intersect(b, c_set), a)),
  length(Reduce(intersect, sets))
)
names(regions) <- c(set_names, paste(set_names[c(1, 1, 2)], set_names[c(2, 3, 3)], sep = "&"), paste(set_names, collapse = "&"))
if (sum(regions) != length(union_ids)) stop("Internal region-count invariant failed", call. = FALSE)
set_sizes <- vapply(sets, length, integer(1L))
if (set_sizes[[1L]] != sum(regions[c(1, 4, 5, 7)]) ||
    set_sizes[[2L]] != sum(regions[c(2, 4, 6, 7)]) ||
    set_sizes[[3L]] != sum(regions[c(3, 5, 6, 7)])) {
  stop("Internal set-size invariant failed", call. = FALSE)
}

proportional_shape <- chr("proportional_shape", "ellipse")
if (!proportional_shape %in% c("ellipse", "circle")) stop("proportional_shape must be ellipse or circle", call. = FALSE)
set.seed(195) # Reproducible geometry fitting; original measurements/counts remain unchanged.
euler_fit <- eulerr::euler(regions, shape = proportional_shape)
original_values <- euler_fit$original.values
fitted_values <- euler_fit$fitted.values[names(original_values)]
if (length(fitted_values) != 7L || any(!is.finite(fitted_values))) stop("Euler fitted areas must be finite and complete", call. = FALSE)
relative_area_error <- abs(fitted_values - original_values) / pmax(original_values, 1)
max_relative_area_error <- num("max_relative_area_error", 0.25, 0, 10)
if (max(relative_area_error) > max_relative_area_error) {
  worst <- names(which.max(relative_area_error))
  stop(sprintf(
    "Euler area fit exceeds max_relative_area_error for %s: observed=%g fitted=%0.3f relative_error=%0.3f; use ellipse or a non-area-proportional display",
    worst, original_values[[worst]], fitted_values[[worst]], max(relative_area_error)
  ), call. = FALSE)
}

set_colors <- palette_value("set_colors", "#FF4C4C,#0099E5,#34BF49")
if (length(set_colors) < 3L) stop("set_colors must provide one color per set", call. = FALSE)
set_palette <- stats::setNames(set_colors[seq_len(3L)], set_names)
fill_alpha <- num("fill_alpha", 0.55, 0, 1)
edge_color <- pick("edge_color", "#FFFFFF")
edge_width <- num("edge_width", .8, 0, 6)
background_color <- pick("background_color", "#FFFFFF")
text_color <- pick("text_color", "#15191D")
muted_text_color <- pick("muted_text_color", "#606870")

show_percentages <- bool("show_percentages", TRUE)
percentage_digits <- num("percentage_digits", 1, 0, 3)
if (percentage_digits != floor(percentage_digits)) stop("percentage_digits must be an integer", call. = FALSE)
percentage_digits <- as.integer(percentage_digits)
show_panel_tags <- bool("show_panel_tags", TRUE)
set_label_size <- num("set_label_size", 3.2, 1.5, 10)
quantity_size <- num("quantity_size", 2.7, 1.5, 10)
euler_set_label_cex <- num("euler_set_label_cex", .8, 0.4, 4)
euler_quantity_cex <- num("euler_quantity_cex", .64, 0.4, 4)

title <- chr("title", "", allow_empty = TRUE)
subtitle <- chr("subtitle", "", allow_empty = TRUE)
caption <- chr("caption", "", allow_empty = TRUE)
if (!nzchar(caption)) caption <- paste(
  paste0("A: equal-circle Venn (area not quantitative); B: fitted Euler; max fitted-area error ", if (100 * max(relative_area_error) < .01) "<0.01%" else sprintf("%.2f%%", 100 * max(relative_area_error)), "."),
  sprintf("Presence: value > %g in at least %d column(s)/set; union n=%d, input n=%d, excluded n=%d.", presence_threshold, minimum_present_replicates, length(union_ids), length(feature_ids), length(feature_ids)-length(union_ids)),
  if (show_percentages) "Both panels: observed-union percentages; counts are exact, areas approximate; source provenance unverified." else "Both panels: exact region counts; Euler areas approximate; source provenance unverified.", sep = "\n")
family <- resolve_font(chr("family", "Arial"))
title_size <- num("title_size", 10, 5, 30)
subtitle_size <- num("subtitle_size", 7.5, 4, 20)
caption_size <- num("caption_size", 6, 4, 16)
panel_tag_size <- num("panel_tag_size", 10, 6, 26)
width_mm <- num("width_mm", 180, 100, 400)
height_mm <- num("height_mm", 100.2, 60, 300)
dpi <- num("dpi", 600, 96, 1200)

theta <- seq(0, 2 * pi, length.out = 361L)
centres <- data.frame(
  set = set_names,
  x = c(-0.62, 0.62, 0),
  y = c(0.35, 0.35, -0.72),
  stringsAsFactors = FALSE
)
circle_data <- do.call(rbind, lapply(seq_len(3L), function(i) {
  data.frame(
    set = set_names[[i]],
    x = centres$x[[i]] + 1.02 * cos(theta),
    y = centres$y[[i]] + 1.02 * sin(theta)
  )
}))
circle_data$set <- factor(circle_data$set, levels = set_names)
region_positions <- data.frame(
  region = names(regions),
  x = c(-0.88, 0.88, 0, 0, -0.43, 0.43, 0),
  y = c(0.52, 0.52, -1.10, 0.62, -0.34, -0.34, -0.10),
  count = as.integer(regions),
  stringsAsFactors = FALSE
)
region_positions$label <- if (show_percentages) {
  sprintf(
    paste0("%d\n(%.", percentage_digits, "f%%)"),
    region_positions$count,
    100 * region_positions$count / length(union_ids)
  )
} else {
  as.character(region_positions$count)
}
set_label_positions <- data.frame(
  label = set_names,
  x = c(-0.90, 0.90, 0),
  y = c(1.47, 1.47, -1.82),
  stringsAsFactors = FALSE
)

regular_plot <- ggplot2::ggplot(circle_data, ggplot2::aes(x = x, y = y, group = set, fill = set)) +
  ggplot2::geom_polygon(alpha = fill_alpha, colour = edge_color, linewidth = edge_width * 0.35) +
  ggplot2::geom_text(
    data = region_positions,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = family,
    size = quantity_size,
    lineheight = 0.95,
    colour = text_color
  ) +
  ggplot2::geom_text(
    data = set_label_positions,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = family,
    fontface = "bold",
    size = set_label_size,
    colour = text_color
  ) +
  ggplot2::scale_fill_manual(values = set_palette, breaks = set_names, guide = "none") +
  ggplot2::coord_equal(xlim = c(-1.75, 1.75), ylim = c(-1.95, 1.68), expand = FALSE, clip = "off") +
  ggplot2::theme_void(base_family = family) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = background_color, colour = NA),
    plot.margin = ggplot2::margin(2, 2, 2, 2, unit = "mm")
  )

euler_quantity_labels <- stats::setNames(if (show_percentages) sprintf(paste0("%d (%.", percentage_digits, "f%%)"), regions, 100 * regions / length(union_ids)) else as.character(regions), names(regions))
quantity_types <- if (show_percentages) c("counts", "percent") else "counts"
build_combined <- function() {
  euler_grob <- plot(
    euler_fit,
    fills = list(fill = unname(set_palette), alpha = fill_alpha),
    labels = list(col = text_color, font = 2, fontfamily = family, cex = euler_set_label_cex),
    edges = list(col = edge_color, lwd = edge_width, lty = 1),
    quantities = list(labels = euler_quantity_labels, cex = euler_quantity_cex, fontfamily = family, col = text_color)
  )
  proportional_plot <- patchwork::wrap_elements(full = euler_grob)
  combined <- regular_plot + proportional_plot +
    patchwork::plot_layout(widths = c(0.96, 1.04))
  combined <- combined & ggplot2::theme(
    plot.tag = ggplot2::element_text(family = family, size = panel_tag_size, face = "bold", colour = text_color),
    plot.tag.position = c(0.01, 0.99)
  )
  annotation_theme <- ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = background_color, colour = NA),
    plot.title = ggplot2::element_text(family = family, size = title_size, face = "bold", colour = text_color),
    plot.subtitle = ggplot2::element_text(family = family, size = subtitle_size, colour = muted_text_color),
    plot.caption = ggplot2::element_text(family = family, size = caption_size, colour = muted_text_color, hjust = 0),
    plot.tag = ggplot2::element_text(family = family, size = panel_tag_size, face = "bold", colour = text_color),
    plot.tag.position = c(0.01, 0.99),
    plot.margin = ggplot2::margin(2, 3, 3, 3, unit = "mm")
  )
  combined <- combined + patchwork::plot_annotation(
    title = if (nzchar(title)) title else NULL,
    subtitle = if (nzchar(subtitle)) subtitle else NULL,
    caption = if (nzchar(caption)) caption else NULL,
    tag_levels = if (show_panel_tags) "A" else NULL,
    theme = annotation_theme
  )
  combined
}

visible_text <- c(title, subtitle, caption, set_names)
if (any(grepl("case[ _-]*[0-9]+|cover_[0-9]+|debug|案例[ ]*[0-9]+", visible_text, ignore.case = TRUE))) stop("Internal case or debug labels cannot appear in figures", call. = FALSE)
if (family == "Arial" && !"Arial" %in% names(grDevices::pdfFonts())) grDevices::pdfFonts(Arial = grDevices::pdfFonts("ArialMT")[[1L]])
render <- function(kind) {
  old_padding <- eulerr::eulerr_options(padding = grid::unit(2, "mm"))
  on.exit(eulerr::eulerr_options(old_padding), add = TRUE)
  target <- file.path(output_dir, paste0("plot.", kind))
  if (kind == "png") ragg::agg_png(target, width = width_mm, height = height_mm, units = "mm", res = dpi, background = background_color)
  if (kind == "pdf") grDevices::pdf(target, width = width_mm / 25.4, height = height_mm / 25.4, useDingbats = FALSE, bg = background_color, family = family)
  if (kind == "svg") svglite::svglite(target, width = width_mm / 25.4, height = height_mm / 25.4, bg = background_color)
  tryCatch({
    grid::grid.newpage()
    measure <- function(labels, size, face = "plain") vapply(labels, function(label) grid::convertWidth(grid::grobWidth(grid::textGrob(label, gp = grid::gpar(fontsize = size, fontfamily = family, fontface = face))), "mm", valueOnly = TRUE), numeric(1L))
    if (measure(title, title_size, "bold") > width_mm - 6 || any(measure(strsplit(subtitle, "\n")[[1L]], subtitle_size) > width_mm - 6)) stop("Title or subtitle does not fit", call. = FALSE)
    if (any(measure(strsplit(caption, "\n")[[1L]], caption_size) > width_mm - 6)) stop("Caption does not fit", call. = FALSE)
    combined <- build_combined(); grid::grid.draw(patchwork::patchworkGrob(combined)); grid::grid.force()
    v <- grid::grid.ls(viewports = TRUE, grobs = FALSE, recursive = TRUE, print = FALSE); i <- grep("^panel[.][0-9]", v$name)
    if (length(i) != 1L) stop("Cannot identify conventional Venn panel", call. = FALSE)
    path <- c(strsplit(v$vpPath[i], "::", fixed = TRUE)[[1L]][-1L], v$name[i]); grid::upViewport(0); grid::downViewport(do.call(grid::vpPath, as.list(path)))
    pw <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE); ph <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    if (pw < 35 || ph < 35) stop("Venn panel is too small", call. = FALSE)
    if (any(measure(set_names, set_label_size * ggplot2::.pt, "bold") > pw * .4)) stop("Set labels do not fit; shorten names", call. = FALSE)
    labels <- region_positions$label; widths <- measure(labels, quantity_size * ggplot2::.pt); heights <- grid::convertHeight(grid::grobHeight(grid::textGrob(labels[1], gp = grid::gpar(fontsize = quantity_size * ggplot2::.pt, fontfamily = family, lineheight = .95))), "mm", valueOnly = TRUE)
    xy <- cbind((region_positions$x+1.75)/3.5*pw, (region_positions$y+1.95)/3.63*ph)
    for (j in seq_len(nrow(xy)-1)) for (k in (j+1):nrow(xy)) if (abs(xy[j,1]-xy[k,1]) < (widths[j]+widths[k])/2 && abs(xy[j,2]-xy[k,2]) < heights) stop("Venn region labels overlap; reduce quantity_size", call. = FALSE)
    grid::upViewport(0)
  }, finally = grDevices::dev.off())
  if (!file.exists(target) || file.info(target)$size <= 0) stop("Failed to create ", target, call. = FALSE)
}

for (kind in c("png", "pdf", "svg")) render(kind)
cat(paste(sprintf("%s=%d", names(set_sizes), set_sizes), collapse = "; "), "\n")
cat(paste(sprintf("%s=%d", names(regions), regions), collapse = "; "), "\n")
cat(sprintf(
  "Union=%d; universe=%d; excluded=%d; Euler shape=%s; stress=%0.6g; max relative area error=%0.6g\n",
  length(union_ids), length(feature_ids), length(feature_ids) - length(union_ids),
  proportional_shape, euler_fit$stress, max(relative_area_error)
))
