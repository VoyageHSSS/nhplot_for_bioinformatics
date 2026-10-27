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

packages <- c("jsonlite", "ggplot2", "ggbeeswarm", "readr", "ragg", "svglite")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages(library(ggplot2))

cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
params <- cfg$params %||% list()
gp <- function(name, default = NULL) params[[name]] %||% default

file_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_file <- if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) sys.frame(1)$ofile else sub("^--file=", "", file_arg[[1L]])
script_dir <- dirname(normalizePath(script_file, mustWork = TRUE))
output_dir <- arg_value("output-dir", cfg$output_dir %||% file.path(script_dir, "output"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

resolve_input <- function(path, fallback) {
  path <- as.character(path %||% "")
  if (!nzchar(path)) path <- fallback
  candidates <- unique(c(path, file.path(dirname(config_path), path), file.path(script_dir, path)))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Input file not found: ", path, call. = FALSE)
  normalizePath(hit[[1L]], mustWork = TRUE)
}

as_number <- function(name, default, lower = -Inf, upper = Inf) {
  value <- as.numeric(gp(name, default))
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

data_path <- resolve_input(gp("data_path", NULL), file.path(script_dir, "assets", "input.txt"))
raw_data <- if (identical(tolower(tools::file_ext(data_path)), "csv")) {
  readr::read_csv(data_path, show_col_types = FALSE, name_repair = "minimal")
} else {
  readr::read_tsv(data_path, show_col_types = FALSE, name_repair = "minimal")
}
if (!"Stage" %in% names(raw_data) || anyDuplicated(names(raw_data)) || !nrow(raw_data)) stop("Unique column names and nonempty Stage expression table required", call. = FALSE)
id_column <- names(raw_data)[[1L]]
if(id_column=="Stage")stop("First column must be sample ID, distinct from Stage")
ids<-trimws(as.character(raw_data[[id_column]]))
if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids))stop("Complete unique sample IDs required")
raw_data$Stage<-trimws(as.character(raw_data$Stage))
if(anyNA(raw_data$Stage)||any(!nzchar(raw_data$Stage)))stop("Complete nonblank Stage values required")
gene_columns <- setdiff(names(raw_data), c(id_column, "Stage"))
if (!length(gene_columns)||!all(vapply(raw_data[gene_columns],function(z)is.numeric(z)&&all(is.finite(z)),logical(1)))) stop("All supplied expression columns must be finite numeric values; no silent gene/sample filtering")

observed_stages <- unique(as.character(raw_data$Stage[!is.na(raw_data$Stage)]))
canonical_stages <- c("Stage I", "Stage II", "Stage III", "Stage IV")
stage_levels <- c(intersect(canonical_stages, observed_stages), setdiff(observed_stages, canonical_stages))
if (length(stage_levels) < 2L) stop("Stage must contain at least two groups", call. = FALSE)

data <- data.frame(
  Stage = factor(rep(raw_data$Stage, times = length(gene_columns)), levels = stage_levels),
  Gene = factor(rep(gene_columns, each = nrow(raw_data)), levels = gene_columns),
  Expression = as.numeric(unlist(raw_data[gene_columns], use.names = FALSE)),
  check.names = FALSE
)
if(any(table(raw_data$Stage)<2))stop("At least two observations per Stage required for density display")
if (!nrow(data)) stop("No usable observations remain after filtering", call. = FALSE)

test_gene <- function(gene) {
  frame <- droplevels(data[data$Gene == gene, c("Expression", "Stage"), drop = FALSE])
  if (nlevels(frame$Stage) < 2L || length(unique(frame$Expression)) < 2L) return(NA_real_)
  if (nlevels(frame$Stage) > 2L) {
    stats::kruskal.test(Expression ~ Stage, data = frame)$p.value
  } else {
    suppressWarnings(stats::wilcox.test(Expression ~ Stage, data = frame)$p.value)
  }
}
p_values <- vapply(gene_columns, test_gene, numeric(1L))
stars <- ifelse(
  is.na(p_values), "",
  ifelse(p_values < 0.001, "***", ifelse(p_values < 0.01, "**", ifelse(p_values < 0.05, "*", "")))
)
facet_labels <- stats::setNames(paste0(gene_columns, stars), gene_columns)

family <- as.character(gp("family", "Helvetica"))
base_size <- as_number("base_size", 8, 4, 30)
axis_title_size <- as_number("axis_title_size", 8, 4, 30)
axis_text_size <- as_number("axis_text_size", 7.2, 4, 24)
strip_text_size <- as_number("strip_text_size", 8.1, 4, 30)
legend_title_size <- as_number("legend_title_size", 7.2, 4, 30)
legend_text_size <- as_number("legend_text_size", 7.2, 4, 24)
point_size <- as_number("point_size", 0.38, 0.1, 10)
point_alpha <- as_number("point_alpha", 0.25, 0, 1)
point_width <- as_number("point_width", 0.12, 0, 0.4)
violin_width <- as_number("violin_width", 0.88, 0.2, 1)
violin_alpha <- as_number("violin_alpha", 0.42, 0, 1)
box_width <- as_number("box_width", 0.16, 0.04, 0.4)
line_width <- as_number("line_width", 0.25, 0.1, 2)
panel_gap_mm <- as_number("panel_gap_mm", 1.4, 0, 12)

text_color <- as_color("text_color", plot_nh_colors$surface$ink)
point_color <- as_color("point_color", plot_nh_colors$surface$ink)
grid_color <- as_color("grid_color", plot_nh_colors$surface$grid)
panel_background <- as_color("panel_background", plot_nh_colors$surface$panel)

original_cols <- plot_nh_palette_categorical(n = length(stage_levels), style = "core", labels = stage_levels)
palette_source <- as.character(cfg$palette_source %||% "original")
mapping <- unlist(cfg$palette_mapping %||% list(), use.names = TRUE)
if (!identical(palette_source, "original") && length(mapping) && all(stage_levels %in% names(mapping))) {
  cols <- as_colors(mapping[stage_levels])
} else {
  supplied <- as_colors(gp("palette_colors", unname(original_cols)))
  cols <- rep(supplied, length.out = length(stage_levels))
}
names(cols) <- stage_levels

show_points <- isTRUE(gp("show_points", TRUE))
show_legend <- isTRUE(gp("show_legend", TRUE))
legend_position <- if (show_legend) as.character(gp("legend_position", "bottom")) else "none"
if (!legend_position %in% c("bottom", "right", "top", "left", "none")) stop("Invalid legend_position", call. = FALSE)

group_counts <- table(factor(raw_data$Stage, levels = stage_levels))
legend_labels <- stats::setNames(paste0(stage_levels, " (n=", as.integer(group_counts), ")"), stage_levels)

p_final <- ggplot(data, aes(.data$Stage, .data$Expression, fill = .data$Stage)) +
  geom_violin(
    width = violin_width, trim = TRUE, scale = "width",
    colour = text_color, linewidth = line_width, alpha = violin_alpha,
    show.legend = show_legend
  )
if (show_points) {
  p_final <- p_final +
    ggbeeswarm::geom_quasirandom(
      colour = point_color, size = point_size, alpha = point_alpha,
      width = point_width, show.legend = FALSE
    )
}
p_final <- p_final +
  geom_boxplot(
    width = box_width, fill = panel_background, colour = text_color,
    linewidth = line_width, outlier.shape = if (show_points) NA else 16,
    outlier.size = max(0.7, point_size * 1.7), show.legend = FALSE
  ) +
  facet_wrap(
    vars(.data$Gene), nrow = 1, scales = "fixed",
    labeller = labeller(Gene = facet_labels)
  ) +
  scale_fill_manual(values = cols, breaks = stage_levels, labels = legend_labels, drop = FALSE) +
  scale_x_discrete(labels = stats::setNames(sub("^Stage\\s+", "", stage_levels), stage_levels)) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.07))) +
  labs(
    x = as_label("x_title", "Clinical stage"),
    y = as_label("y_title", "Gene expression"),
    fill = "Stage",
    caption = paste0(if(length(stage_levels)>2)"Per-gene omnibus Kruskal-Wallis" else "Per-gene unpaired Wilcoxon", "; nominal p, unadjusted. *<0.05, **<0.01, ***<0.001.\nSupplied samples; expression units/preprocessing, provenance and independence unconfirmed.")
  ) +
  guides(fill = guide_legend(ncol = if(legend_position%in%c("bottom","top"))length(stage_levels) else 1, byrow = TRUE, override.aes = list(alpha = 0.8))) +
  theme_minimal(base_size = base_size, base_family = family) +
  theme(
    text = element_text(colour = text_color),
    panel.background = element_rect(fill = panel_background, colour = NA),
    plot.background = element_rect(fill = panel_background, colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = grid_color, linewidth = 0.18),
    panel.border = element_rect(fill = NA, colour = grDevices::adjustcolor(text_color, alpha.f = 0.58), linewidth = line_width),
    strip.background = element_rect(fill = grDevices::adjustcolor(grid_color, alpha.f = 0.48), colour = grDevices::adjustcolor(text_color, alpha.f = 0.58), linewidth = line_width),
    strip.text = element_text(size = strip_text_size, face = "bold", margin = margin(2.4, 1, 2.4, 1)),
    axis.title = element_text(size = axis_title_size, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 4)),
    axis.title.y = element_text(margin = margin(r = 4)),
    axis.text = element_text(size = axis_text_size, colour = text_color),
    axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 2)),
    axis.ticks = element_line(colour = text_color, linewidth = 0.28),
    panel.spacing = grid::unit(panel_gap_mm, "mm"),
    legend.position = legend_position,
    legend.title = element_text(size = legend_title_size, face = "bold"),
    legend.text = element_text(size = legend_text_size),
    legend.key.size = grid::unit(3.4, "mm"),
    legend.spacing.y = grid::unit(0.6, "mm"),
    plot.caption = element_text(size = 6.1, colour = grDevices::adjustcolor(text_color, alpha.f = 0.82), hjust = 0, margin = margin(t = 4)),
    plot.margin = margin(5, 6, 5, 6)
  )

width_mm <- as_number("width_mm", 180, 100, 300)
height_mm <- as_number("height_mm", 100, 60, 200)
dpi <- as_number("dpi", 300, 96, 1200)
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

ggsave(file.path(output_dir, "plot.png"), plot_nh_apply_format(p_final, cfg, plot_nh_format_output(cfg)), width = width_in, height = height_in, units = "in", dpi = dpi, device = ragg::agg_png, bg = panel_background)
ggsave(file.path(output_dir, "plot.pdf"), plot_nh_apply_format(p_final, cfg, plot_nh_format_output(cfg)), width = width_in, height = height_in, units = "in", device = grDevices::pdf, bg = panel_background)
ggsave(file.path(output_dir, "plot.svg"), plot_nh_apply_format(p_final, cfg, plot_nh_format_output(cfg)), width = width_in, height = height_in, units = "in", device = svglite::svglite, bg = panel_background)

write.table(data.frame(gene=gene_columns,test=if(length(stage_levels)>2)'Kruskal-Wallis omnibus' else 'unpaired Wilcoxon',nominal_p=p_values,star=stars),file.path(output_dir,'per_gene_nominal_tests.tsv'),sep='\t',row.names=FALSE,quote=FALSE)
write.table(data.frame(Stage=stage_levels,n=as.integer(group_counts)),file.path(output_dir,'observed_stage_counts.tsv'),sep='\t',row.names=FALSE,quote=FALSE)
