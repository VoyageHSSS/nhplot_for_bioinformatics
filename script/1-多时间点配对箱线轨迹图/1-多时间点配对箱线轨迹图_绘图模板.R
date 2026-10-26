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

packages <- c("jsonlite", "ggplot2", "gapminder", "readr", "ragg", "svglite")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)

suppressPackageStartupMessages(library(ggplot2))

cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
params <- cfg$params %||% list()
gp <- function(name, default = NULL) params[[name]] %||% default

script <- if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) sys.frame(1)$ofile else
  sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
script_dir <- dirname(normalizePath(script, mustWork = TRUE))
output_dir <- arg_value("output-dir", cfg$output_dir %||% file.path(script_dir, "output"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

as_number <- function(name, default, lower = -Inf, upper = Inf) {
  value <- suppressWarnings(as.numeric(gp(name, default)))
  if (length(value) != 1L || is.na(value) || value < lower || value > upper) {
    stop(sprintf("%s must be between %s and %s", name, lower, upper), call. = FALSE)
  }
  value
}

as_label <- function(name, default) {
  value <- as.character(gp(name, default))
  if (length(value) != 1L || !nzchar(trimws(value))) default else value
}

hex_pattern <- "^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$"
as_color <- function(name, default) {
  value <- as.character(gp(name, default))
  if (length(value) != 1L || !grepl(hex_pattern, value)) stop("Invalid color for ", name, call. = FALSE)
  value
}

as_colors <- function(value) {
  value <- unlist(value, use.names = FALSE)
  if (length(value) == 1L && grepl("[,;]", value)) value <- strsplit(value, "[,;]")[[1L]]
  value <- trimws(as.character(value))
  if (!length(value) || any(!grepl(hex_pattern, value))) stop("palette_colors contains an invalid HEX color", call. = FALSE)
  value
}

data_path <- as.character(gp("data_path", ""))
if (length(data_path) != 1L) stop("data_path must be a single path", call. = FALSE)

if (nzchar(data_path)) {
  candidates <- unique(c(data_path, file.path(dirname(config_path), data_path), file.path(script_dir, data_path)))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Input file not found: ", data_path, call. = FALSE)
  input_path <- normalizePath(hit[[1L]], mustWork = TRUE)
  raw <- if (identical(tolower(tools::file_ext(input_path)), "csv")) {
    readr::read_csv(input_path, show_col_types = FALSE, name_repair = "minimal")
  } else {
    readr::read_tsv(input_path, show_col_types = FALSE, name_repair = "minimal")
  }
  if (anyDuplicated(names(raw))) stop("Unique input column names required")
  if (all(c("country", "year", "lifeExp", "continent") %in% names(raw))) {
    life_expectancy_input <- TRUE
    data <- data.frame(subject = raw$country, time = raw$year, value = raw$lifeExp, facet = raw$continent)
  } else if (all(c("subject_id", "time", "value", "facet") %in% names(raw))) {
    life_expectancy_input <- FALSE
    data <- data.frame(subject = raw$subject_id, time = raw$time, value = raw$value, facet = raw$facet)
  } else {
    stop("Input must contain country/year/lifeExp/continent or subject_id/time/value/facet", call. = FALSE)
  }
} else {
  life_expectancy_input <- TRUE
  raw <- as.data.frame(gapminder::gapminder)
  raw <- raw[raw$year %in% c(1957, 2002, 2007) & raw$continent != "Oceania", , drop = FALSE]
  data <- data.frame(subject = raw$country, time = raw$year, value = raw$lifeExp, facet = raw$continent)
  time_levels <- c("1957", "2002", "2007")
  facet_levels <- c("Africa", "Americas", "Asia", "Europe")
}

if (anyNA(data)) stop("Missing subject, time, facet or value fields are not allowed")
data$subject <- trimws(as.character(data$subject))
data$time <- trimws(as.character(data$time))
data$facet <- trimws(as.character(data$facet))
data$value <- suppressWarnings(as.numeric(data$value))
if (any(!nzchar(data$subject) | !nzchar(data$time) | !nzchar(data$facet) | !is.finite(data$value))) {
  stop("Input contains missing or invalid subject, time, facet, or value fields", call. = FALSE)
}
time_levels <- unique(data$time)
order_arg <- unlist(gp("time_order", ""), use.names = FALSE)
if (length(order_arg) == 1L && nzchar(order_arg)) order_arg <- trimws(strsplit(order_arg, ",", fixed = TRUE)[[1L]])
if (length(order_arg) && any(nzchar(order_arg))) {
  if (length(order_arg) != length(time_levels) || anyDuplicated(order_arg) || !setequal(order_arg, time_levels)) stop("time_order must list each observed time exactly once")
  time_levels <- order_arg
} else if (all(grepl("^-?[0-9]+(\\.[0-9]+)?$", time_levels))) {
  numeric_times <- as.numeric(time_levels)
  if (anyDuplicated(numeric_times)) stop("Ambiguous numeric time labels refer to the same time")
  time_levels <- time_levels[order(numeric_times)]
} else if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", time_levels)) && !anyNA(as.Date(time_levels))) {
  time_levels <- sort(time_levels)
} else stop("Non-numeric time labels require explicit time_order")
facet_levels <- sort(unique(data$facet))
if (length(time_levels) != 3L) stop("This template requires exactly three time points", call. = FALSE)
if (anyDuplicated(paste(data$facet, data$subject, data$time, sep = "\r"))) {
  stop("Each facet/subject/time combination must be unique", call. = FALSE)
}
subject_key <- paste(data$facet, data$subject, sep = "\r")
if (any(table(subject_key) != length(time_levels))) {
  stop("Every subject must have one observation at each of the three time points", call. = FALSE)
}
facet_counts <- vapply(facet_levels, function(f) length(unique(data$subject[data$facet == f])), integer(1L))
if (any(facet_counts < 3L)) stop("Every facet must contain at least three paired subjects", call. = FALSE)

data$time <- factor(data$time, levels = time_levels)
data$facet <- factor(data$facet, levels = facet_levels)
data$time_index <- match(data$time, time_levels)
data$pair_id <- interaction(data$facet, data$subject, drop = TRUE)
data <- data[order(data$facet, data$subject, data$time_index), , drop = FALSE]

pairs <- utils::combn(seq_along(time_levels), 2L)
stats_rows <- list()
row_id <- 0L
for (facet_name in facet_levels) {
  facet_data <- data[data$facet == facet_name, , drop = FALSE]
  for (column in seq_len(ncol(pairs))) {
    row_id <- row_id + 1L
    first <- pairs[1L, column]
    second <- pairs[2L, column]
    left <- facet_data[facet_data$time_index == first, c("subject", "value")]
    right <- facet_data[facet_data$time_index == second, c("subject", "value")]
    ids <- intersect(left$subject, right$subject)
    x <- left$value[match(ids, left$subject)]
    y <- right$value[match(ids, right$subject)]
    p <- suppressWarnings(stats::wilcox.test(x, y, paired = TRUE)$p.value)
    stats_rows[[row_id]] <- data.frame(facet = facet_name, x1 = first, x2 = second, n = length(ids), p = p, reason = if (is.finite(p)) "estimable" else "not estimable")
  }
}
stats_data <- do.call(rbind, stats_rows)
stats_data$p_adj <- stats::p.adjust(stats_data$p, method = "bonferroni")
stats_data$label <- ifelse(
  stats_data$p_adj <= 1e-4, "****",
  ifelse(stats_data$p_adj <= 1e-3, "***", ifelse(stats_data$p_adj <= 1e-2, "**", ifelse(stats_data$p_adj <= 0.05, "*", "ns")))
)
stats_data$label[!is.finite(stats_data$p_adj)] <- "NE"
stats_data$facet <- factor(stats_data$facet, levels = facet_levels)

value_range <- range(data$value)
value_span <- diff(value_range)
if (!is.finite(value_span) || value_span <= 0) value_span <- max(abs(value_range), 1)
comparison_level <- match(paste(stats_data$x1, stats_data$x2), c("1 2", "1 3", "2 3"))
stats_data$y <- value_range[[2L]] + (comparison_level * 0.075 + 0.005) * value_span
tip <- 0.018 * value_span
segments <- rbind(
  transform(stats_data, x = x1, xend = x2, yend = y),
  transform(stats_data, x = x1, xend = x1, yend = y - tip),
  transform(stats_data, x = x2, xend = x2, yend = y - tip)
)

family <- as.character(gp("family", "Helvetica"))
base_size <- as_number("base_size", 8, 4, 30)
axis_title_size <- as_number("axis_title_size", 8, 4, 30)
axis_text_size <- as_number("axis_text_size", 8, 4, 24)
strip_text_size <- as_number("strip_text_size", 8.5, 4, 30)
annotation_size <- as_number("annotation_size", 3, 1, 12)
caption_size <- as_number("caption_size", 6.2, 4, 20)
point_size <- as_number("point_size", 1.35, 0.1, 10)
point_alpha <- as_number("point_alpha", 0.72, 0, 1)
point_stroke <- as_number("point_stroke", 0.18, 0, 2)
trajectory_width <- as_number("trajectory_width", 0.24, 0.05, 3)
trajectory_alpha <- as_number("trajectory_alpha", 0.28, 0, 1)
box_width <- as_number("box_width", 0.36, 0.1, 0.9)
box_alpha <- as_number("box_alpha", 0.22, 0, 1)
outline_width <- as_number("outline_width", 0.32, 0.08, 3)
bracket_width <- as_number("bracket_width", 0.28, 0.08, 3)
panel_gap_mm <- as_number("panel_gap_mm", 2, 0, 12)

text_color <- as_color("text_color", plot_nh_colors$surface$ink)
trajectory_color <- as_color("trajectory_color", "#8A939D")
grid_color <- as_color("grid_color", plot_nh_colors$surface$grid)
panel_background <- as_color("panel_background", plot_nh_colors$surface$panel)

original_cols <- c("#3B528B", "#21918C", "#5EC962")
palette_source <- as.character(cfg$palette_source %||% "original")
mapping <- unlist(cfg$palette_mapping %||% list(), use.names = TRUE)
if (!identical(palette_source, "original") && length(mapping) && all(time_levels %in% names(mapping))) {
  cols <- as_colors(mapping[time_levels])
} else {
  supplied <- as_colors(gp("palette_colors", original_cols))
  cols <- rep(supplied, length.out = length(time_levels))
}
names(cols) <- time_levels

show_legend <- isTRUE(gp("show_legend", FALSE))
legend_position <- if (show_legend) as.character(gp("legend_position", "top")) else "none"
if (!legend_position %in% c("bottom", "right", "top", "left", "none")) stop("Invalid legend_position", call. = FALSE)

facet_labels <- stats::setNames(paste0(facet_levels, "  ·  n=", facet_counts), facet_levels)
y_upper <- max(stats_data$y) + 0.055 * value_span
y_lower <- value_range[[1L]] - 0.045 * value_span

p_final <- ggplot(data, aes(.data$time_index, .data$value)) +
  geom_line(
    aes(group = .data$pair_id), colour = trajectory_color,
    linewidth = trajectory_width, alpha = trajectory_alpha
  ) +
  geom_boxplot(
    aes(group = .data$time, fill = .data$time), width = box_width,
    outlier.shape = NA, alpha = box_alpha, colour = text_color,
    linewidth = outline_width, show.legend = show_legend
  ) +
  geom_point(
    aes(fill = .data$time), shape = 21, size = point_size,
    alpha = point_alpha, stroke = point_stroke, colour = text_color,
    show.legend = show_legend
  ) +
  geom_segment(
    data = segments,
    aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend),
    inherit.aes = FALSE, colour = text_color, linewidth = bracket_width
  ) +
  geom_text(
    data = stats_data,
    aes(x = (.data$x1 + .data$x2) / 2, y = .data$y + 0.012 * value_span, label = .data$label),
    inherit.aes = FALSE, family = family, size = annotation_size,
    fontface = "bold", colour = text_color, vjust = 0
  ) +
  facet_wrap(
    vars(.data$facet), nrow = 1, scales = "fixed",
    labeller = labeller(facet = facet_labels)
  ) +
  scale_fill_manual(values = cols, breaks = time_levels, drop = FALSE) +
  scale_x_continuous(breaks = seq_along(time_levels), labels = time_levels) +
  scale_y_continuous(limits = c(y_lower, y_upper), expand = expansion(mult = c(0, 0))) +
  labs(
    x = as_label("x_title", if (life_expectancy_input) "Year (categorical positions)" else "Time (categorical positions)"),
    y = as_label("y_title", if (life_expectancy_input) "Life expectancy (years)" else "Value"),
    fill = as_label("legend_title", if (life_expectancy_input) "Year" else "Time"),
    caption = paste0(if (!nzchar(data_path)) "Gapminder countries; " else "Supplied matched subjects; ", "equal time spacing does not represent elapsed-time slopes.\nPaired Wilcoxon signed-rank tests; Bonferroni across all comparisons. NE: not estimable; sampling independence unconfirmed.")
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(alpha = 0.85, size = 2.5))) +
  theme_minimal(base_size = base_size, base_family = family) +
  theme(
    text = element_text(colour = text_color),
    panel.background = element_rect(fill = panel_background, colour = NA),
    plot.background = element_rect(fill = panel_background, colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = grid_color, linewidth = 0.24),
    panel.border = element_rect(fill = NA, colour = grDevices::adjustcolor(text_color, alpha.f = 0.48), linewidth = outline_width),
    strip.background = element_rect(fill = grDevices::adjustcolor(grid_color, alpha.f = 0.42), colour = NA),
    strip.text = element_text(size = strip_text_size, face = "bold", margin = margin(3, 1, 3, 1)),
    axis.title = element_text(size = axis_title_size, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 4)),
    axis.title.y = element_text(margin = margin(r = 4)),
    axis.text = element_text(size = axis_text_size, colour = text_color),
    axis.text.x = element_text(face = "bold", margin = margin(t = 2)),
    axis.ticks = element_line(colour = text_color, linewidth = 0.28),
    panel.spacing = grid::unit(panel_gap_mm, "mm"),
    legend.position = legend_position,
    legend.title = element_text(size = 7.2, face = "bold"),
    legend.text = element_text(size = 6.8),
    legend.key.size = grid::unit(3.4, "mm"),
    plot.caption = element_text(size = caption_size, colour = grDevices::adjustcolor(text_color, alpha.f = 0.82), hjust = 0, margin = margin(t = 4)),
    plot.margin = margin(5, 6, 5, 6)
  )

width_mm <- as_number("width_mm", 180, 100, 300)
height_mm <- as_number("height_mm", 105, 65, 220)
dpi <- as_number("dpi", 300, 96, 1200)
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

ggsave(file.path(output_dir, "plot.png"), plot_nh_apply_format(p_final, cfg, plot_nh_format_output(cfg)), width = width_in, height = height_in, units = "in", dpi = dpi, device = ragg::agg_png, bg = panel_background)
ggsave(file.path(output_dir, "plot.pdf"), plot_nh_apply_format(p_final, cfg, plot_nh_format_output(cfg)), width = width_in, height = height_in, units = "in", device = grDevices::pdf, bg = panel_background)
ggsave(file.path(output_dir, "plot.svg"), plot_nh_apply_format(p_final, cfg, plot_nh_format_output(cfg)), width = width_in, height = height_in, units = "in", device = svglite::svglite, bg = panel_background)

write.table(data, file.path(output_dir, "paired_observations.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
write.table(stats_data, file.path(output_dir, "paired_wilcoxon_comparisons.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
