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
packages <- c("jsonlite", "ggplot2", "patchwork", "ragg", "svglite")
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
split_text <- function(value) {
  values <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
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

resolve_input <- function(value, fallback) {
  if (!nzchar(value)) return(file.path(script_dir, "assets", fallback))
  candidates <- unique(c(value, file.path(dirname(config_path), value), file.path(script_dir, value), file.path(getwd(), value)))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Input file not found: ", value, call. = FALSE)
  normalizePath(hit[[1L]], mustWork = TRUE)
}
read_input <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "rds") return(as.data.frame(readRDS(path), check.names = FALSE))
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
validate_frame <- function(data, label) {
  if (!is.data.frame(data) || !nrow(data)) stop(label, " must contain at least one row", call. = FALSE)
  if (anyNA(names(data)) || any(!nzchar(trimws(names(data)))) || anyDuplicated(names(data))) {
    stop(label, " column names must be complete and unique", call. = FALSE)
  }
}
numeric_frame <- function(data, columns, label) {
  values <- lapply(data[columns], function(x) suppressWarnings(as.numeric(as.character(x))))
  if (any(vapply(values, function(x) anyNA(x) || any(!is.finite(x)), logical(1L)))) {
    stop(label, " value columns must contain finite numeric values", call. = FALSE)
  }
  as.data.frame(values, check.names = FALSE)
}

response_path <- resolve_input(chr("response_path", "", allow_empty = TRUE), "data1.txt")
dispersion_path <- resolve_input(chr("dispersion_path", "", allow_empty = TRUE), "data2.txt")
response <- read_input(response_path)
dispersion <- read_input(dispersion_path)
validate_frame(response, "Response table")
validate_frame(dispersion, "Dispersion table")

trait_column <- chr("trait_column", "sample")
category_column <- chr("category_column", "group")
dispersion_trait_column <- chr("dispersion_trait_column", "sample")
response_value_columns <- split_text(chr("response_value_columns", "", allow_empty = TRUE))
if (trait_column == category_column) stop("Response role columns must differ", call. = FALSE)
for (column in c(trait_column, category_column)) if (!column %in% names(response)) stop("Missing response column: ", column, call. = FALSE)
if (!dispersion_trait_column %in% names(dispersion)) stop("Missing dispersion trait column: ", dispersion_trait_column, call. = FALSE)
if (!length(response_value_columns)) response_value_columns <- setdiff(names(response), c(trait_column, category_column))
if (anyDuplicated(response_value_columns) || any(response_value_columns %in% c(trait_column, category_column)) || length(response_value_columns) < 2L || any(!response_value_columns %in% names(response))) {
  stop("response_value_columns must identify at least two columns", call. = FALSE)
}

trait_names <- trimws(as.character(response[[trait_column]]))
dispersion_traits <- trimws(as.character(dispersion[[dispersion_trait_column]]))
categories <- trimws(as.character(response[[category_column]]))
if (anyNA(trait_names) || any(!nzchar(trait_names)) || anyDuplicated(trait_names)) stop("Response trait names must be non-empty and unique", call. = FALSE)
if (anyNA(dispersion_traits) || any(!nzchar(dispersion_traits)) || anyDuplicated(dispersion_traits)) stop("Dispersion trait names must be non-empty and unique", call. = FALSE)
if (!setequal(trait_names, dispersion_traits)) stop("Response and dispersion tables must contain the same traits", call. = FALSE)
if (anyNA(categories) || any(!nzchar(categories))) stop("Trait categories cannot be empty", call. = FALSE)
dispersion <- dispersion[match(trait_names, dispersion_traits), , drop = FALSE]

response_values <- numeric_frame(response, response_value_columns, "Response")
response_long <- data.frame(
  trait = rep(trait_names, times = length(response_value_columns)),
  category = rep(categories, times = length(response_value_columns)),
  observation = rep(response_value_columns, each = length(trait_names)),
  value = unlist(response_values, use.names = FALSE),
  stringsAsFactors = FALSE
)
trait_levels <- rev(trait_names)
response_long$trait <- factor(response_long$trait, levels = trait_levels)
category_levels <- unique(categories)
response_long$category <- factor(response_long$category, levels = category_levels)

control_pattern <- chr("control_pattern", "^Control")
warming_pattern <- chr("warming_pattern", "^Warming")
candidate_columns <- setdiff(names(dispersion), dispersion_trait_column)
control_columns <- grep(control_pattern, candidate_columns, value = TRUE, perl = TRUE)
warming_columns <- grep(warming_pattern, candidate_columns, value = TRUE, perl = TRUE)
if (length(control_columns) < 2L || length(warming_columns) < 2L) stop("Each condition needs at least two replicate columns for uncertainty", call. = FALSE)
if (length(intersect(control_columns, warming_columns))) stop("Control and warming patterns must not overlap", call. = FALSE)
control_values <- numeric_frame(dispersion, control_columns, "Control")
warming_values <- numeric_frame(dispersion, warming_columns, "Warming")
control_label <- chr("control_label", "Control")
warming_label <- chr("warming_label", "Warming")
if (control_label == warming_label) stop("Condition labels must differ", call. = FALSE)

make_long <- function(values, columns, condition) {
  data.frame(
    trait = rep(trait_names, times = length(columns)),
    condition = condition,
    replicate = rep(columns, each = length(trait_names)),
    value = unlist(values, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}
dispersion_long <- rbind(
  make_long(control_values, control_columns, control_label),
  make_long(warming_values, warming_columns, warming_label)
)
dispersion_long$trait <- factor(dispersion_long$trait, levels = trait_levels)
dispersion_long$condition <- factor(dispersion_long$condition, levels = c(control_label, warming_label))

error_type <- chr("error_type", "SE")
if (!error_type %in% c("SE", "SD", "95% CI")) stop("error_type must be SE, SD, or 95% CI", call. = FALSE)
summary_rows <- lapply(trait_names, function(trait) {
  lapply(c(control_label, warming_label), function(condition) {
    values <- dispersion_long$value[as.character(dispersion_long$trait) == trait & dispersion_long$condition == condition]
    n <- length(values)
    standard_deviation <- stats::sd(values)
    standard_error <- standard_deviation / sqrt(n)
    spread <- switch(error_type, "SE" = standard_error, "SD" = standard_deviation, "95% CI" = stats::qt(0.975, df = n - 1L) * standard_error)
    data.frame(trait = trait, condition = condition, mean = mean(values), lower = mean(values) - spread, upper = mean(values) + spread, n = n)
  })
})
dispersion_summary <- do.call(rbind, unlist(summary_rows, recursive = FALSE))
if (any(!is.finite(as.matrix(dispersion_summary[c("mean", "lower", "upper")])))) stop("Summary exceeds numerical precision", call. = FALSE)
dispersion_summary$trait <- factor(dispersion_summary$trait, levels = trait_levels)
dispersion_summary$condition <- factor(dispersion_summary$condition, levels = c(control_label, warming_label))

section_breaks <- suppressWarnings(as.numeric(split_text(chr("section_breaks", "2,16", allow_empty = TRUE))))
if (anyNA(section_breaks) || any(!is.finite(section_breaks)) || any(section_breaks != round(section_breaks)) || any(section_breaks < 1L) || any(section_breaks >= length(trait_names)) || is.unsorted(section_breaks, strictly = TRUE)) {
  stop("section_breaks must be increasing row numbers between 1 and the final trait", call. = FALSE)
}
section_starts <- c(1L, section_breaks + 1L)
section_ends <- c(section_breaks, length(trait_names))
section_colors <- palette_value("section_colors", "#FFF5DB,#EDF2F6,#F1F7EE")
if (length(section_colors) != length(section_starts)) stop("section_colors must contain one color per section", call. = FALSE)
sections <- data.frame(
  ymin = length(trait_names) - section_ends + 0.5,
  ymax = length(trait_names) - section_starts + 1.5,
  fill = section_colors,
  stringsAsFactors = FALSE
)

category_colors <- palette_value("category_colors", "#D9A65F,#8E75B2,#63A57B,#CC5E72,#4F86BD,#EE7648")
if (length(category_colors) < length(category_levels)) {
  category_colors <- plot_nh_palette_categorical(length(category_levels), style = "extended", allow_fallback = TRUE)
} else {
  category_colors <- category_colors[seq_along(category_levels)]
}
names(category_colors) <- category_levels
control_color <- pick("control_color", "#6FB68B")
warming_color <- pick("warming_color", "#E8AD73")
condition_colors <- stats::setNames(c(control_color, warming_color), c(control_label, warming_label))

significance_path <- chr("significance_path", "", allow_empty = TRUE)
show_significance <- bool("show_significance", TRUE)
significance <- data.frame()
if (show_significance && nzchar(significance_path)) {
  significance <- read_input(resolve_input(significance_path, ""))
  validate_frame(significance, "Significance table")
  required_significance <- c("panel", "trait", "p_value", "method", "adjustment", "comparison")
  missing_significance <- setdiff(required_significance, names(significance))
  if (length(missing_significance)) stop("Significance table is missing: ", paste(missing_significance, collapse = ", "), call. = FALSE)
  significance$panel <- tolower(trimws(as.character(significance$panel)))
  significance$trait <- trimws(as.character(significance$trait))
  significance$p_value <- suppressWarnings(as.numeric(as.character(significance$p_value)))
  if (any(!significance$panel %in% c("response", "dispersion"))) stop("Significance panel must be response or dispersion", call. = FALSE)
  if (any(!significance$trait %in% trait_names)) stop("Significance table contains unknown traits", call. = FALSE)
  if (anyNA(significance$p_value) || any(significance$p_value < 0 | significance$p_value > 1)) stop("Significance p_value must be between 0 and 1", call. = FALSE)
  if (anyNA(significance[c("method", "adjustment", "comparison")]) || any(!nzchar(trimws(as.character(significance$method)))) || any(!nzchar(trimws(as.character(significance$adjustment)))) || any(!nzchar(trimws(as.character(significance$comparison))))) {
    stop("Significance method, adjustment, and comparison cannot be empty", call. = FALSE)
  }
  if (anyDuplicated(significance[c("panel", "trait")])) stop("Significance table must have at most one row per panel and trait", call. = FALSE)
  significance$label <- ifelse(significance$p_value < 0.001, "***", ifelse(significance$p_value < 0.01, "**", ifelse(significance$p_value < 0.05, "*", "")))
  significance <- significance[nzchar(significance$label), , drop = FALSE]
}

left_title <- chr("left_title", "Trait response")
right_title <- chr("right_title", "Dispersion summary")
left_x_title <- chr("left_x_title", "Response value")
right_x_title <- chr("right_x_title", "Dispersion value")
family <- resolve_font(chr("family", "Arial"))
if (family == "Arial" && !"Arial" %in% names(grDevices::pdfFonts())) grDevices::pdfFonts(Arial = grDevices::pdfFonts("ArialMT")[[1L]])
text_color <- pick("text_color", "#202020")
axis_color <- pick("axis_color", "#3F3F3F")
background_color <- pick("background_color", "#FFFFFF")

axis_text_size <- num("axis_text_size", 8, 4, 24)
axis_title_size <- num("axis_title_size", 9, 5, 28)
strip_text_size <- num("strip_text_size", 9, 5, 28)
legend_text_size <- num("legend_text_size", 7.5, 4, 24)
caption_size <- num("caption_size", 6.5, 4, 18)
box_width <- num("box_width", 0.62, 0.15, 0.95)
box_line_width <- num("box_line_width", 0.35, 0.1, 3)
outlier_size <- num("outlier_size", 1.5, 0.1, 8)
mean_marker_size <- num("mean_marker_size", 1.5, 0.5, 8)
point_size <- num("point_size", 1.8, 0.3, 12)
errorbar_line_width <- num("errorbar_line_width", 0.4, 0.1, 3)
zero_line_width <- num("zero_line_width", 0.4, 0.1, 3)
panel_border_width <- num("panel_border_width", 0.35, 0.1, 3)
significance_size <- num("significance_size", 3.4, 1, 10)

response_floor <- num("response_axis_min", -100)
response_ceiling <- num("response_axis_max", 110)
dispersion_floor <- num("dispersion_axis_min", 30)
dispersion_ceiling <- num("dispersion_axis_max", 110)
if (response_floor >= response_ceiling || dispersion_floor >= dispersion_ceiling) stop("Axis minima must be below maxima", call. = FALSE)
response_range <- range(c(response_floor, response_ceiling, response_long$value), finite = TRUE)
response_range <- response_range + c(-1, 1) * .025 * diff(response_range)
dispersion_range <- range(c(dispersion_floor, dispersion_ceiling, dispersion_summary$lower, dispersion_summary$upper), finite = TRUE)
dispersion_range <- dispersion_range + c(-1, 1) * .025 * diff(dispersion_range)

legend_inside <- bool("show_legend", TRUE) && chr("legend_position", "bottom") == "inside"
y_expansion <- ggplot2::expansion(add = c(.5, if (legend_inside) 3.5 else .5))
left_means <- aggregate(value ~ trait, response_long, mean)
if (nrow(significance)) {
  left_significance <- significance[significance$panel == "response", , drop = FALSE]
  right_significance <- significance[significance$panel == "dispersion", , drop = FALSE]
} else {
  left_significance <- right_significance <- data.frame()
}
if (nrow(left_significance)) {
  left_significance$panel <- left_title
  maxima <- tapply(response_long$value, response_long$trait, max)
  span <- diff(response_range)
  left_significance$x <- maxima[left_significance$trait] + 0.035 * span
  response_range[2L] <- max(response_range[2L], left_significance$x + .08 * span)
  left_significance$trait <- factor(left_significance$trait, levels = trait_levels)
}
if (nrow(right_significance)) {
  right_significance$panel <- right_title
  maxima <- tapply(dispersion_summary$upper, dispersion_summary$trait, max)
  span <- diff(dispersion_range)
  right_significance$x <- maxima[right_significance$trait] + 0.035 * span
  dispersion_range[2L] <- max(dispersion_range[2L], right_significance$x + .08 * span)
  right_significance$trait <- factor(right_significance$trait, levels = trait_levels)
}

common_theme <- ggplot2::theme_bw(base_family = family) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = axis_color, fill = NA, linewidth = panel_border_width),
    axis.text = ggplot2::element_text(size = axis_text_size, color = text_color),
    axis.title = ggplot2::element_text(size = axis_title_size, color = text_color),
    axis.ticks = ggplot2::element_line(color = axis_color, linewidth = 0.3),
    strip.background = ggplot2::element_rect(fill = "#D2D2D2", color = axis_color, linewidth = panel_border_width),
    strip.text = ggplot2::element_text(size = strip_text_size, face = "bold", color = text_color, margin = ggplot2::margin(1.3, 1, 1.3, 1, unit = "mm")),
    plot.background = ggplot2::element_rect(fill = background_color, color = NA),
    panel.background = ggplot2::element_rect(fill = background_color, color = NA)
  )

response_long$panel <- left_title
left_plot <- ggplot2::ggplot(response_long, ggplot2::aes(x = value, y = trait))
for (i in seq_len(nrow(sections))) {
  left_plot <- left_plot + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = sections$ymin[[i]], ymax = sections$ymax[[i]], fill = sections$fill[[i]])
}
left_plot <- left_plot +
  ggplot2::geom_vline(xintercept = 0, linetype = "22", color = "#999999", linewidth = zero_line_width) +
  ggplot2::geom_boxplot(
    ggplot2::aes(fill = category, color = category),
    width = box_width,
    linewidth = box_line_width,
    median.colour = "#FFFFFF", median.linewidth = .4,
    outlier.shape = 18,
    outlier.size = outlier_size,
    outlier.alpha = 0.9
  ) +
  ggplot2::geom_point(data = left_means, ggplot2::aes(x = value, y = trait), inherit.aes = FALSE, shape = 21, size = mean_marker_size, fill = "#FFFFFF", color = text_color, stroke = .25) +
  ggplot2::scale_fill_manual(values = category_colors, guide = "none") +
  ggplot2::scale_color_manual(values = category_colors, guide = "none") +
  ggplot2::scale_x_continuous(limits = response_range, breaks = scales::breaks_pretty(n = 5), expand = ggplot2::expansion(mult = c(0, 0))) +
  ggplot2::scale_y_discrete(limits = trait_levels, drop = FALSE, expand = y_expansion) +
  ggplot2::facet_grid(cols = ggplot2::vars(panel)) +
  ggplot2::labs(x = left_x_title, y = NULL) +
  common_theme +
  ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(2, 1.5, 2, 2, unit = "mm"))
if (nrow(left_significance)) {
  left_plot <- left_plot + ggplot2::geom_text(
    data = left_significance,
    ggplot2::aes(x = x, y = trait, label = label),
    inherit.aes = FALSE,
    family = family,
    fontface = "bold",
    size = significance_size,
    color = text_color,
    hjust = 0
  )
}

dispersion_summary$panel <- right_title
dodge <- ggplot2::position_dodge(width = 0.58)
right_plot <- ggplot2::ggplot(dispersion_summary, ggplot2::aes(x = mean, y = trait, color = condition))
for (i in seq_len(nrow(sections))) {
  right_plot <- right_plot + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = sections$ymin[[i]], ymax = sections$ymax[[i]], fill = sections$fill[[i]])
}
right_plot <- right_plot +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    orientation = "y",
    position = dodge,
    width = 0,
    linewidth = errorbar_line_width
  ) +
  ggplot2::geom_point(position = dodge, shape = 18, size = point_size) +
  ggplot2::scale_color_manual(values = condition_colors, name = NULL) +
  ggplot2::scale_x_continuous(limits = dispersion_range, breaks = scales::breaks_pretty(n = 3), expand = ggplot2::expansion(mult = c(0, 0))) +
  ggplot2::scale_y_discrete(limits = trait_levels, drop = FALSE, expand = y_expansion) +
  ggplot2::facet_grid(cols = ggplot2::vars(panel)) +
  ggplot2::labs(x = right_x_title, y = NULL) +
  common_theme +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = legend_text_size, color = text_color),
    legend.key = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(fill = grDevices::adjustcolor(background_color, alpha.f = 0.88), color = NA),
    legend.spacing.y = grid::unit(0.2, "mm"),
    plot.margin = ggplot2::margin(2, 2, 2, 0, unit = "mm")
  )
if (bool("show_legend", TRUE)) {
  legend_position <- chr("legend_position", "bottom")
  if (!legend_position %in% c("inside", "top", "bottom", "right")) stop("legend_position is invalid", call. = FALSE)
  if (legend_position == "inside") {
    right_plot <- right_plot + ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.73, 0.985),
      legend.justification = c(0.5, 1),
      legend.direction = "vertical"
    )
  } else {
    right_plot <- right_plot + ggplot2::theme(legend.position = legend_position, legend.direction = if (legend_position %in% c("top", "bottom")) "horizontal" else "vertical")
  }
} else {
  right_plot <- right_plot + ggplot2::theme(legend.position = "none")
}
if (nrow(right_significance)) {
  right_plot <- right_plot + ggplot2::geom_text(
    data = right_significance,
    ggplot2::aes(x = x, y = trait, label = label),
    inherit.aes = FALSE,
    family = family,
    fontface = "bold",
    size = significance_size,
    color = text_color,
    hjust = 0
  )
}

caption <- paste0("Left: box = IQR, white line = median, circle = mean; n = ", length(response_value_columns), ".",
  "\nRight: mean ± ", error_type, "; n = ", length(control_columns), "/", length(warming_columns), " per condition. ",
  if (nrow(significance)) "Stars: supplied results; see test/adjustment metadata." else "No significance tests supplied.",
  "\nSource units and replicate independence are unverified.")
for (label in c(left_title, right_title, left_x_title, right_x_title, control_label, warming_label, trait_names)) {
  if (grepl("(?i)case [0-9]|案例|cover_|debug", label, perl = TRUE)) stop("Internal labels are not allowed", call. = FALSE)
}

left_width_ratio <- num("left_width_ratio", 1, 0.5, 3)
plot <- patchwork::wrap_plots(left_plot, right_plot, nrow = 1, widths = c(left_width_ratio, 1), guides = if (bool("show_legend", TRUE) && chr("legend_position", "bottom") == "bottom") "collect" else "auto") +
  patchwork::plot_annotation(
    caption = caption,
    theme = ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = background_color, color = NA),
      legend.position = if (bool("show_legend", TRUE)) chr("legend_position", "bottom") else "none",
      plot.caption = ggplot2::element_text(family = family, size = caption_size, color = text_color, hjust = 0, lineheight = 1.15, margin = ggplot2::margin(t = 1.5, unit = "mm"))
    )
  )

width_mm <- num("width_mm", 180, 90, 400)
height_mm <- num("height_mm", 172, 90, 400)
dpi <- num("dpi", 600, 96, 1200)
render_one <- function(kind) {
  target <- file.path(output_dir, paste0("plot.", kind))
  opened <- FALSE
  on.exit(if (opened && grDevices::dev.cur() > 1L) grDevices::dev.off(), add = TRUE)
  if (kind == "png") ragg::agg_png(target, width = width_mm, height = height_mm, units = "mm", res = dpi, background = background_color)
  if (kind == "pdf") grDevices::pdf(target, width = width_mm / 25.4, height = height_mm / 25.4, bg = background_color, useDingbats = FALSE, family = family)
  if (kind == "svg") svglite::svglite(target, width = width_mm / 25.4, height = height_mm / 25.4, bg = background_color)
  opened <- TRUE
  g <- patchwork::patchworkGrob(plot)
  grid::grid.draw(g)
  grid::grid.force()
  v <- grid::grid.ls(viewports = TRUE, grobs = FALSE, recursive = TRUE, print = FALSE)
  indices <- grep("^panel-1-1[.]", v$name)
  if (length(indices) != 2L) stop("Cannot identify aligned panels", call. = FALSE)
  bounds <- lapply(indices, function(i) {
    path <- c(strsplit(v$vpPath[i], "::", fixed = TRUE)[[1L]][-1L], v$name[i])
    grid::upViewport(0); grid::downViewport(do.call(grid::vpPath, as.list(path)))
    origin <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), valueOnly = TRUE)
    c(origin$x * 25.4, origin$y * 25.4, grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE),
      grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE))
  })
  if (max(abs(bounds[[1L]][c(2,4)] - bounds[[2L]][c(2,4)])) > .05) stop("Trait rows are not aligned", call. = FALSE)
  measure <- function(labels, size, face = "plain") vapply(labels, function(label) grid::convertWidth(grid::grobWidth(grid::textGrob(label,
    gp = grid::gpar(fontsize = size, fontfamily = family, fontface = face))), "mm", valueOnly = TRUE), numeric(1L))
  if (any(vapply(bounds, function(b) b[3] < 20 || b[4] < 30, logical(1L)))) stop("Panels are too small", call. = FALSE)
  row_height <- bounds[[1L]][4] / (length(trait_names) + if (legend_inside) 3 else 0)
  if (row_height < axis_text_size / ggplot2::.pt * 1.15) stop("Trait labels overlap", call. = FALSE)
  radius <- .375 * (point_size * ggplot2::.pt + .5 * ggplot2::.stroke / 2) / ggplot2::.pt
  if (2 * radius > .29 * row_height) stop("Condition points overlap; enlarge height or reduce point_size", call. = FALSE)
  mean_radius <- .375 * (mean_marker_size * ggplot2::.pt + .25 * ggplot2::.stroke / 2) / ggplot2::.pt
  if (2 * mean_radius > .95 * row_height) stop("Mean markers overlap", call. = FALSE)
  if (nrow(significance) && 1.15 * significance_size > row_height) stop("Significance labels overlap", call. = FALSE)
  if (legend_inside && (max(measure(c(control_label, warming_label), legend_text_size)) + 8 > bounds[[2L]][3] || 2.6 * legend_text_size / ggplot2::.pt + 4 > 3 * row_height)) stop("Inset legend does not fit reserved rows", call. = FALSE)
  if (measure(left_title, strip_text_size, "bold") > bounds[[1L]][3] - 2 || measure(right_title, strip_text_size, "bold") > bounds[[2L]][3] - 2) stop("Panel titles do not fit", call. = FALSE)
  if (measure(left_x_title, axis_title_size) > bounds[[1L]][3] || measure(right_x_title, axis_title_size) > bounds[[2L]][3]) stop("Axis titles do not fit", call. = FALSE)
  if (any(measure(strsplit(caption, "\n")[[1L]], caption_size) > width_mm - 4)) stop("Caption does not fit", call. = FALSE)
  for (j in seq_len(2L)) {
    pp <- ggplot2::ggplot_build(if (j == 1L) left_plot else right_plot)$layout$panel_params[[1L]]
    if (j == 1L && any((left_means$value - pp$x.range[1L]) / diff(pp$x.range) * bounds[[1L]][3] < mean_radius | (pp$x.range[2L] - left_means$value) / diff(pp$x.range) * bounds[[1L]][3] < mean_radius)) stop("Mean markers exceed panel bounds", call. = FALSE)
    annotation <- if (j == 1L) left_significance else right_significance
    if (nrow(annotation) && any((annotation$x - pp$x.range[1L]) / diff(pp$x.range) * bounds[[j]][3] + measure(annotation$label, significance_size * ggplot2::.pt, "bold") > bounds[[j]][3])) stop("Significance labels exceed panel bounds", call. = FALSE)
    positions <- pp$x$get_breaks(); labels <- pp$x$get_labels(); ok <- !is.na(positions)
    positions <- positions[ok]; widths <- measure(labels[ok], axis_text_size)
    if (any(diff(positions) / diff(pp$x.range) * bounds[[j]][3] < (head(widths, -1L) + tail(widths, -1L)) / 2 + .2)) stop("X ticks overlap", call. = FALSE)
  }
  grid::upViewport(0)
  grDevices::dev.off()
  opened <- FALSE
  if (!file.exists(target) || file.info(target)$size <= 0) stop("Failed to create ", target, call. = FALSE)
}

for (kind in c("png", "pdf", "svg")) render_one(kind)
cat(sprintf(
  "Rendered aligned response and functional-diversity panels for %d traits (%d response observations; %s intervals; %d supplied significant labels) to %s\n",
  length(trait_names), nrow(response_long), error_type, nrow(significance), output_dir
))
