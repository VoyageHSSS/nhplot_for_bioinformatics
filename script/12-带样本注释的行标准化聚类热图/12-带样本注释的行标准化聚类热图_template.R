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

`%||%` <- function(x, y) if (is.null(x)) y else x
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
packages <- c("jsonlite", "pheatmap", "ragg", "svglite")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)

cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
params <- cfg$params %||% list()
gp <- function(name, default = NULL) params[[name]] %||% default
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
parse_list <- function(value) {
  if (is.list(value) || length(value) != 1L || anyNA(value)) stop("List parameters must be complete scalars", call. = FALSE)
  value <- trimws(as.character(value))
  if (!nzchar(value)) return(character())
  result <- trimws(strsplit(value, "[,;]", perl = TRUE)[[1L]])
  result[nzchar(result)]
}
parse_colors <- function(name, default) {
  colors <- parse_list(gp(name, default))
  if (length(colors) < 2L || any(!grepl("^#[0-9A-Fa-f]{6}$", colors))) {
    stop(name, " must contain at least two comma-separated #RRGGBB colors", call. = FALSE)
  }
  colors
}
resolve_font <- function(value) switch(value, "Times New Roman" = "Times", value)

resolve_input <- function(value, default_name) {
  if (!nzchar(value)) return(file.path(script_dir, "assets", default_name))
  candidates <- unique(c(value, file.path(dirname(config_path), value), file.path(script_dir, value), file.path(getwd(), value)))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Input file not found: ", value, call. = FALSE)
  normalizePath(hit[[1L]], mustWork = TRUE)
}
read_input <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "rds") return(readRDS(path))
  signature <- readBin(path, what = "raw", n = 8L)
  is_excel <- (length(signature) >= 2L && identical(as.integer(signature[1:2]), c(80L, 75L))) ||
    (length(signature) >= 8L && identical(as.integer(signature[1:8]), c(208L, 207L, 17L, 224L, 161L, 177L, 26L, 225L)))
  if (is_excel) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Install readxl to read Excel files", call. = FALSE)
    return(as.data.frame(readxl::read_excel(path, col_types = "text", .name_repair = "minimal"), check.names = FALSE))
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
validate_names <- function(values, label) {
  values <- trimws(as.character(values))
  if (anyNA(values) || any(!nzchar(values)) || anyDuplicated(values)) stop(label, " must be complete and unique", call. = FALSE)
  values
}

data_path <- chr("data_path", "", allow_empty = TRUE)
raw <- read_input(resolve_input(data_path, "data.txt"))
row_id_column <- chr("row_id_column", "gene")
if (is.matrix(raw)) raw <- as.data.frame(raw, check.names = FALSE)
if (!is.data.frame(raw) || nrow(raw) < 2L || ncol(raw) < 3L) stop("Data must contain at least 2 rows and 2 numeric sample columns", call. = FALSE)
if (anyNA(names(raw)) || any(!nzchar(trimws(names(raw)))) || anyDuplicated(names(raw))) stop("Input column names must be complete and unique", call. = FALSE)

if (row_id_column %in% names(raw)) {
  row_ids <- validate_names(raw[[row_id_column]], "Row identifiers")
  raw[[row_id_column]] <- NULL
} else {
  raw_names <- rownames(raw)
  default_names <- identical(raw_names, as.character(seq_len(nrow(raw))))
  if (is.null(raw_names) || default_names) stop("Missing row identifier column: ", row_id_column, call. = FALSE)
  row_ids <- validate_names(raw_names, "Row identifiers")
}
sample_ids <- validate_names(names(raw), "Sample names")
numeric_columns <- lapply(raw, function(x) suppressWarnings(as.numeric(as.character(x))))
mat <- as.matrix(as.data.frame(numeric_columns, check.names = FALSE))
dimnames(mat) <- list(row_ids, sample_ids)
if (anyNA(mat) || any(!is.finite(mat))) stop("All matrix cells must be finite numeric values", call. = FALSE)
if (nrow(mat) > 300L || ncol(mat) > 100L) stop("Heatmap supports at most 300 rows and 100 columns", call. = FALSE)

drop_zero_variance <- bool("drop_zero_variance", TRUE)
zero_variance <- apply(mat, 1L, function(x) all(x == x[1L]))
row_variance <- apply(mat, 1L, stats::var)
if (any(!is.finite(row_variance[!zero_variance]) | row_variance[!zero_variance] <= 0)) stop("Nonconstant row variance exceeds numerical precision; rescale input units", call. = FALSE)
if (any(zero_variance) && !drop_zero_variance) stop("Matrix contains zero-variance rows; enable drop_zero_variance or revise the data", call. = FALSE)
mat <- mat[!zero_variance, , drop = FALSE]
if (nrow(mat) < 2L) stop("At least two non-constant rows are required", call. = FALSE)
scaled_mat <- t(scale(t(mat)))
if (anyNA(scaled_mat) || any(!is.finite(scaled_mat))) stop("Row-wise z-score scaling produced non-finite values", call. = FALSE)

annotation_path <- chr("annotation_path", "", allow_empty = TRUE)
annotation_sample_column <- chr("annotation_sample_column", "sample")
annotation_fields <- parse_list(gp("annotation_fields", "Critical period, Post treatment, Batch, Treatment"))
annotation_col <- NULL
annotation_colors <- NULL
default_annotation <- file.path(script_dir, "assets", "annotation.tsv")
annotation_file <- if (nzchar(annotation_path)) resolve_input(annotation_path, "annotation.tsv") else default_annotation
if (nzchar(annotation_path) || (!nzchar(data_path) && file.exists(annotation_file))) {
  annotation_raw <- read_input(annotation_file)
  if (!is.data.frame(annotation_raw) || !annotation_sample_column %in% names(annotation_raw)) stop("Annotation table must contain ", annotation_sample_column, call. = FALSE)
  if (anyNA(names(annotation_raw)) || anyDuplicated(names(annotation_raw)) || any(!nzchar(trimws(names(annotation_raw))))) stop("Annotation column names must be complete and unique", call. = FALSE)
  if (anyDuplicated(annotation_fields) || annotation_sample_column %in% annotation_fields) stop("Annotation fields must be distinct from each other and the sample ID", call. = FALSE)
  annotation_ids <- validate_names(annotation_raw[[annotation_sample_column]], "Annotation sample identifiers")
  rownames(annotation_raw) <- annotation_ids
  annotation_raw[[annotation_sample_column]] <- NULL
  if (!nzchar(annotation_path) && !setequal(colnames(scaled_mat), annotation_ids)) {
    annotation_raw <- NULL
  }
  if (!is.null(annotation_raw)) {
    if (!setequal(colnames(scaled_mat), annotation_ids)) stop("Annotation sample identifiers must match matrix columns exactly", call. = FALSE)
    if (!length(annotation_fields)) annotation_fields <- names(annotation_raw)
    missing_fields <- setdiff(annotation_fields, names(annotation_raw))
    if (length(missing_fields)) stop("Missing annotation fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
    # pheatmap draws annotation columns from bottom to top; reverse once so the
    # user-facing field order remains top to bottom.
    annotation_col <- annotation_raw[colnames(scaled_mat), rev(annotation_fields), drop = FALSE]
    for (field in names(annotation_col)) {
      values <- trimws(as.character(annotation_col[[field]]))
      if (anyNA(values) || any(!nzchar(values))) stop("Annotation field contains missing labels: ", field, call. = FALSE)
      annotation_col[[field]] <- factor(values, levels = unique(values))
    }

    known_colors <- list(
      Treatment = c(Saline = "#000000", Cocaine = "#575757", LSD = "#E79600", MDMA = "#A42422", Ketamine = "#C53A8E"),
      Batch = c("1" = "#3953A3", "2" = "#EF4A4A", "3" = "#009848"),
      `Post treatment` = c("48 h" = "#64838C", "2 wk" = "#2C3A3E"),
      `Critical period` = c(Closed = "#94C83D", Open = "#4E2469")
    )
    fallback_palette <- parse_colors("annotation_palette", "#4C78A8, #F58518, #54A24B, #E45756, #B279A2, #72B7B2, #FF9DA6, #9D755D")
    annotation_colors <- lapply(names(annotation_col), function(field) {
      levels_here <- levels(annotation_col[[field]])
      known <- known_colors[[field]]
      if (!is.null(known) && all(levels_here %in% names(known))) return(known[levels_here])
      generated <- if (length(levels_here) == 1L) fallback_palette[[1L]] else grDevices::colorRampPalette(fallback_palette)(length(levels_here))
      stats::setNames(generated, levels_here)
    })
    names(annotation_colors) <- names(annotation_col)
  }
}

low_color <- pick("low_color", "#3952A2")
mid_color <- pick("mid_color", "#000000")
high_color <- pick("high_color", "#F5EA14")
border_color <- pick("border_color", "#8A8A8A")
na_color <- pick("na_color", "#D9D9D9")
background_color <- pick("background_color", "#FFFFFF")
z_limit <- num("z_limit", 2.6, 0.25, 20)
legend_break_step <- num("legend_break_step", 1, 0.1, z_limit)
palette <- grDevices::colorRampPalette(c(low_color, mid_color, high_color))(100L)
breaks <- seq(-z_limit, z_limit, length.out = length(palette) + 1L)
if (2 * z_limit / legend_break_step > 100) stop("At most 100 legend intervals are supported", call. = FALSE)
legend_breaks <- seq(ceiling(-z_limit / legend_break_step) * legend_break_step, floor(z_limit / legend_break_step) * legend_break_step, by = legend_break_step)

legend_breaks <- legend_breaks[legend_breaks + z_limit >= legend_break_step / 2 & z_limit - legend_breaks >= legend_break_step / 2]
legend_breaks <- sort(unique(c(-z_limit, legend_breaks, z_limit)))

cluster_rows <- bool("cluster_rows", TRUE)
cluster_columns <- bool("cluster_columns", TRUE)
distance_rows <- chr("distance_rows", "euclidean")
distance_columns <- chr("distance_columns", "euclidean")
allowed_distances <- c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
if (!distance_rows %in% allowed_distances || !distance_columns %in% allowed_distances) stop("Unsupported clustering distance", call. = FALSE)
clustering_method <- chr("clustering_method", "complete")
allowed_methods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
if (!clustering_method %in% allowed_methods) stop("Unsupported clustering method", call. = FALSE)

family <- resolve_font(chr("family", "Helvetica"))
if (family == "Arial" && !"Arial" %in% names(grDevices::pdfFonts())) grDevices::pdfFonts(Arial = grDevices::pdfFonts("ArialMT")[[1L]])
title <- chr("title", "Row-wise standardized values", allow_empty = TRUE)
base_size <- num("base_size", 8, 4, 30)
row_label_size <- num("row_label_size", 6.5, 3, 20)
column_label_size <- num("column_label_size", 7.5, 3, 20)
title_size <- num("title_size", 10, 5, 30)
tree_height_row <- num("tree_height_row", 35, 0, 120)
tree_height_column <- num("tree_height_column", 20, 0, 120)
column_label_angle <- num("column_label_angle", 45, 0, 90)
show_row_names <- bool("show_row_names", TRUE)
show_column_names <- bool("show_column_names", FALSE)
italic_row_labels <- bool("italic_row_labels", TRUE)
show_annotation_legend <- bool("show_annotation_legend", TRUE)
show_value_legend <- bool("show_value_legend", TRUE)

row_labels <- rownames(scaled_mat)

if (grepl("(?i)case [0-9]|案例|cover_|debug", title, perl = TRUE)) stop("Internal titles are not allowed", call. = FALSE)
clipped <- sum(abs(scaled_mat) > z_limit)
caption <- paste0("Row z = (value - row mean) / sample SD; no comparison of absolute values across rows.",
  "\n", sum(zero_variance), " constant rows removed; ", clipped, " cells outside +/-", format(z_limit, trim = TRUE), " use endpoint colors.",
  "\nSource and sample annotation meanings require verification.")
make_heatmap <- function() {
heatmap <- pheatmap::pheatmap(
  scaled_mat,
  color = palette,
  breaks = breaks,
  border_color = border_color,
  na_col = na_color,
  cluster_rows = cluster_rows,
  cluster_cols = cluster_columns,
  clustering_distance_rows = distance_rows,
  clustering_distance_cols = distance_columns,
  clustering_method = clustering_method,
  treeheight_row = tree_height_row,
  treeheight_col = tree_height_column,
  annotation_col = annotation_col,
  annotation_colors = annotation_colors,
  annotation_legend = show_annotation_legend,
  annotation_names_col = TRUE,
  show_rownames = show_row_names,
  show_colnames = show_column_names,
  labels_row = row_labels,
  angle_col = as.character(if (column_label_angle %in% c(0, 90)) column_label_angle else 45),
  fontsize = base_size,
  fontsize_row = row_label_size,
  fontsize_col = column_label_size,
  fontsize_number = base_size,
  main = if (nzchar(title)) title else NA,
  legend = show_value_legend,
  legend_breaks = legend_breaks,
  legend_labels = format(legend_breaks, trim = TRUE, scientific = FALSE),
  silent = TRUE,
  fontfamily = family
)

  gt <- heatmap$gtable
  id <- function(name) match(name, gt$layout$name)
  if (show_row_names) gt$grobs[[id("row_names")]]$gp$font <- if (italic_row_labels) 3L else 1L
  label_grobs <- intersect(c("row_names", "col_annotation_names"), gt$layout$name)
  if (length(label_grobs)) {
    old_width <- gt$widths[4]
    gt$widths[4] <- max(old_width, do.call(grid::unit.pmax, lapply(label_grobs,
      function(name) grid::grobWidth(gt$grobs[[id(name)]]) + grid::unit(10, "bigpts"))))
    gt$widths[3] <- gt$widths[3] + old_width - gt$widths[4]
  }
  if (nzchar(title)) {
    gt$grobs[[id("main")]]$gp$fontsize <- title_size
    old_height <- gt$heights[1]
    gt$heights[1] <- grid::grobHeight(gt$grobs[[id("main")]]) * 1.5
    gt$heights[4] <- gt$heights[4] + old_height - gt$heights[1]
  }
  if (show_column_names) {
    col <- gt$grobs[[id("col_names")]]; col$rot <- column_label_angle
    gt$grobs[[id("col_names")]] <- col
    labels <- lapply(col$label, function(label) grid::textGrob(label, rot = column_label_angle, gp = col$gp))
    old_height <- gt$heights[5]
    gt$heights[5] <- do.call(grid::unit.pmax, lapply(labels, grid::grobHeight)) + grid::unit(10, "bigpts")
    gt$heights[4] <- gt$heights[4] + old_height - gt$heights[5]
  }
  # Native pheatmap supports fixed border colour, but its default stroke dominates narrow cells.
  gt$grobs[[id("matrix")]]$children[[1]]$gp$lwd <- .45
  if (!is.na(id("col_annotation"))) gt$grobs[[id("col_annotation")]]$gp$lwd <- .45
  if (show_value_legend) {
    top <- grid::unit(1, "npc") - grid::unit(14, "bigpts")
    h <- grid::unit(120, "bigpts")
    lower <- head(breaks, -1); upper <- tail(breaks, -1)
    gt$grobs[[id("legend")]] <- grid::grobTree(
      grid::textGrob("Z-score", x = 0, y = 1, hjust = 0, vjust = 1, gp = grid::gpar(fontsize = base_size, fontfamily = family, fontface = "bold")),
      grid::rectGrob(x = 0, y = top - h + (lower + z_limit) / (2 * z_limit) * h,
        width = grid::unit(10, "bigpts"), height = (upper - lower) / (2 * z_limit) * h, hjust = 0, vjust = 0,
        gp = grid::gpar(fill = palette, col = NA)),
      grid::textGrob(format(legend_breaks, trim = TRUE, scientific = FALSE), x = grid::unit(14, "bigpts"),
        y = top - h + (legend_breaks + z_limit) / (2 * z_limit) * h, hjust = 0,
        gp = grid::gpar(fontsize = base_size, fontfamily = family)))
    old_width <- gt$widths[5]
    gt$widths[5] <- grid::unit.pmax(
      grid::grobWidth(gt$grobs[[id("legend")]]$children[[1]]) + grid::unit(4, "bigpts"),
      grid::grobWidth(gt$grobs[[id("legend")]]$children[[3]]) + grid::unit(18, "bigpts"))
    gt$widths[3] <- gt$widths[3] + old_width - gt$widths[5]
  }
  heatmap$gtable <- gt
  heatmap
}

width_mm <- num("width_mm", 180, 80, 400)
height_mm <- num("height_mm", 215, 90, 500)
dpi <- num("dpi", 600, 96, 1200)
render_one <- function(kind) {
  target <- file.path(output_dir, paste0("plot.", kind))
  opened <- FALSE
  on.exit(if (opened && grDevices::dev.cur() > 1L) grDevices::dev.off(), add = TRUE)
  if (kind == "png") ragg::agg_png(target, width = width_mm, height = height_mm, units = "mm", res = dpi, background = background_color)
  if (kind == "pdf") grDevices::pdf(target, width = width_mm / 25.4, height = height_mm / 25.4, bg = background_color, useDingbats = FALSE, family = family)
  if (kind == "svg") svglite::svglite(target, width = width_mm / 25.4, height = height_mm / 25.4, bg = background_color)
  opened <- TRUE
  heatmap <- make_heatmap()
  gt <- heatmap$gtable
  grid::grid.newpage()
  caption_height <- grid::unit(12, "mm")
  measure <- function(labels, size) vapply(labels, function(label) grid::convertWidth(grid::grobWidth(grid::textGrob(label,
    gp = grid::gpar(fontsize = size, fontfamily = family))), "mm", valueOnly = TRUE), numeric(1L))
  if (any(measure(strsplit(caption, "\n")[[1L]], 6) > width_mm - 4)) stop("Caption does not fit", call. = FALSE)
  grid::pushViewport(grid::viewport(x = grid::unit(2, "mm"), y = grid::unit(1, "npc") - grid::unit(2, "mm"),
    width = grid::unit(1, "npc") - grid::unit(4, "mm"), height = grid::unit(1, "npc") - caption_height - grid::unit(2, "mm"),
    just = c("left", "top")))
  # Width/height null units are resolved on the actual output device.
  grid::pushViewport(gt$vp)
  mw <- grid::convertWidth(gt$widths[3], "mm", valueOnly = TRUE)
  mh <- grid::convertHeight(gt$heights[4], "mm", valueOnly = TRUE)
  if (mw <= 0 || mh <= 0) stop("Heatmap panel has no usable area", call. = FALSE)
  if (show_row_names && row_label_size / (72.27 / 25.4) * 1.05 > mh / nrow(scaled_mat)) stop("Row labels overlap; enlarge canvas or hide labels", call. = FALSE)
  if (show_column_names) {
    angle <- column_label_angle * pi / 180
    required <- min(max(measure(colnames(scaled_mat), column_label_size)) / max(cos(angle), .001),
      column_label_size / (72.27 / 25.4) * 1.05 / max(sin(angle), .001))
    if (required > mw / ncol(scaled_mat)) stop("Column labels overlap; enlarge canvas or rotate further", call. = FALSE)
  }
  if (nzchar(title) && grid::convertWidth(grid::grobWidth(gt$grobs[[match("main", gt$layout$name)]]), "mm", valueOnly = TRUE) > mw) stop("Title does not fit matrix width", call. = FALSE)
  if (show_value_legend && (min(diff(legend_breaks))/(2*z_limit)*120 < base_size * 1.05 || mh * 72 / 25.4 + grid::convertHeight(gt$heights[3], "bigpts", valueOnly = TRUE) < 140)) stop("Z-score legend labels do not fit", call. = FALSE)
  if (!is.null(annotation_col) && show_annotation_legend) {
    text_h <- grid::convertHeight(grid::grobHeight(grid::textGrob("FGH", gp = grid::gpar(fontsize = base_size, fontfamily = family))), "bigpts", valueOnly = TRUE)
    required <- (2 * sum(lengths(annotation_colors)) + 3 * (ncol(annotation_col) - 1) + 1.5) * text_h
    available <- grid::convertHeight(sum(gt$heights[3:5]), "bigpts", valueOnly = TRUE)
    if (required > available - 2) stop("Annotation legend exceeds available height", call. = FALSE)
  }
  grid::popViewport()
  grid::grid.draw(gt)
  grid::popViewport()
  grid::grid.text(caption, x = grid::unit(2, "mm"), y = grid::unit(2, "mm"), just = c("left", "bottom"),
    gp = grid::gpar(fontsize = 6, fontfamily = family, lineheight = 1.1))
  grDevices::dev.off()
  opened <- FALSE
  if (!file.exists(target) || file.info(target)$size <= 0) stop("Failed to create ", target, call. = FALSE)
}

for (kind in c("png", "pdf", "svg")) render_one(kind)
cat(sprintf(
  "Rendered %d × %d row-z-scored heatmap with %d annotation tracks (%d zero-variance rows removed) to %s\n",
  nrow(scaled_mat), ncol(scaled_mat), if (is.null(annotation_col)) 0L else ncol(annotation_col), sum(zero_variance), output_dir
))
