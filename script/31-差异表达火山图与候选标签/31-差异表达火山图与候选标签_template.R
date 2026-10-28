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
packages <- c("jsonlite", "ggplot2", "ggrepel", "ragg", "svglite")
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
text <- function(name, default, allow_empty = FALSE) {
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
color <- function(name, default) {
  value <- text(name, default)
  if (!grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", value)) stop(name, " must be a hex color", call. = FALSE)
  value
}
split_text <- function(value) {
  if (!nzchar(value)) return(character())
  values <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  values[nzchar(values)]
}
palette_value <- function(name, default, minimum = 1L) {
  values <- split_text(text(name, default))
  if (length(values) < minimum || any(!grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", values))) {
    stop(name, " must be comma-separated hex colors", call. = FALSE)
  }
  values
}
resolve_font <- function(value) switch(value, "Times New Roman" = "Times", value)
blend_colors <- function(foreground, background, alpha) {
  fg <- grDevices::col2rgb(foreground)
  bg <- grDevices::col2rgb(background)[, 1L]
  mixed <- fg * alpha + bg * (1 - alpha)
  grDevices::rgb(mixed[1L, ], mixed[2L, ], mixed[3L, ], maxColorValue = 255)
}

resolve_input <- function(value) {
  fallback <- file.path(script_dir, "assets", "data.txt")
  if (!nzchar(value)) return(fallback)
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
    return(as.data.frame(readxl::read_excel(path, sheet = if (nzchar(sheet)) sheet else 1L, .name_repair = "minimal", col_types="text"), check.names = FALSE))
  }
  header <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(header)) stop("Input file is empty: ", path, call. = FALSE)
  if (grepl("\t", header, fixed = TRUE)) {
    utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE, colClasses="character", fileEncoding = "UTF-8-BOM")
  } else if (grepl(",", header, fixed = TRUE)) {
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, colClasses="character", fileEncoding = "UTF-8-BOM")
  } else {
    utils::read.table(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, colClasses="character", fileEncoding = "UTF-8-BOM")
  }
}

input_path<-resolve_input(text("data_path","",allow_empty=TRUE))
using_bundled_data<-normalizePath(input_path)%in%normalizePath(file.path(script_dir,"assets",c("data.txt","data.xlsx")))
raw<-read_input(input_path,text("excel_sheet","",allow_empty=TRUE))
if (!is.data.frame(raw) || nrow(raw) < 3L) stop("Input must contain at least three tested features", call. = FALSE)
if (anyNA(names(raw)) || any(!nzchar(trimws(names(raw)))) || anyDuplicated(names(raw))) {
  stop("Input column names must be complete and unique", call. = FALSE)
}

feature_column <- text("feature_column", "gene")
effect_column <- text("effect_column", "log2FoldChange")
significance_column <- text("significance_column", "pvalue")
required_columns <- c(feature_column, effect_column, significance_column)
if (length(unique(required_columns)) != 3L) stop("Feature, effect, and significance columns must be different", call. = FALSE)
missing_columns <- setdiff(required_columns, names(raw))
if (length(missing_columns)) stop("Missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)

dat <- data.frame(
  feature = trimws(as.character(raw[[feature_column]])),
  effect = suppressWarnings(as.numeric(as.character(raw[[effect_column]]))),
  significance = suppressWarnings(as.numeric(as.character(raw[[significance_column]]))),
  stringsAsFactors = FALSE
)
if (anyNA(dat$feature) || any(!nzchar(dat$feature)) || anyDuplicated(dat$feature)) {
  stop("Feature identifiers must be complete and unique", call. = FALSE)
}
if (anyNA(dat$effect) || any(!is.finite(dat$effect))) stop("Effect values must be complete, numeric, and finite", call. = FALSE)
if(!is.finite(diff(range(dat$effect))))stop("Effect span must be finite",call.=FALSE)
if (anyNA(dat$significance) || any(!is.finite(dat$significance)) || any(dat$significance <= 0 | dat$significance > 1)) {
  stop("Significance values must be finite and in the interval (0, 1]", call. = FALSE)
}
dat <- dat[order(dat$feature, method = "radix"), , drop = FALSE]
dat$neg_log10_significance <- -log10(dat$significance)

significance_cutoff <- num("significance_cutoff", 0.05, 0, 1)
effect_cutoff <- num("effect_cutoff", 2, 0, Inf)
label_significance_cutoff <- num("label_significance_cutoff", 0.05, 0, 1)
label_effect_cutoff <- num("label_effect_cutoff", 4, 0, Inf)
if(any(c(significance_cutoff,effect_cutoff,label_significance_cutoff,label_effect_cutoff)<=0))stop("All significance and effect cutoffs must be positive",call.=FALSE)
if (label_significance_cutoff > significance_cutoff) {
  stop("label_significance_cutoff cannot exceed significance_cutoff", call. = FALSE)
}
if (label_effect_cutoff < effect_cutoff) stop("label_effect_cutoff cannot be smaller than effect_cutoff", call. = FALSE)

dat$status <- ifelse(
  dat$significance < significance_cutoff & abs(dat$effect) >= effect_cutoff,
  ifelse(dat$effect >= effect_cutoff, "up", "down"),
  "NS"
)
dat$status <- factor(dat$status, levels = c("down", "NS", "up"))
dat$is_label <- dat$significance < label_significance_cutoff & abs(dat$effect) >= label_effect_cutoff
labels <- dat[dat$is_label, , drop = FALSE]
dat$label <- ifelse(dat$is_label, dat$feature, "")
max_labels <- num("max_labels", 100, 0, 1000)
if(max_labels!=floor(max_labels))stop("max_labels must be an integer",call.=FALSE)
if (nrow(labels) > max_labels) stop("Label thresholds select more features than max_labels", call. = FALSE)

dat<-dat[order(dat$status!="NS",dat$feature,method="radix"),,drop=FALSE]
counts<-table(dat$status)
legend_labels<-c(sprintf("Negative (%d)",counts[["down"]]),sprintf("Other (%d)",counts[["NS"]]),sprintf("Positive (%d)",counts[["up"]]))

down_color <- color("down_color", "#0000FF")
ns_color <- color("ns_color", "#BEBEBE")
up_color <- color("up_color", "#FF0000")
status_colors <- c(down = down_color, NS = ns_color, up = up_color)
point_outline_color <- color("point_outline_color", "#000000")
threshold_color <- color("threshold_color", "#000000")
label_threshold_color <- color("label_threshold_color", "#FF0000")
label_color <- color("label_color", "#333C41")
label_segment_color <- color("label_segment_color", "#000000")
text_color <- color("text_color", "#333C41")

show_gradient <- bool("show_gradient", FALSE)
gradient_alpha <- num("gradient_alpha", 0.2, 0, 1)
gradient_bands <- num("gradient_bands", 120, 30, 240)
if(gradient_bands!=floor(gradient_bands))stop("gradient_bands must be an integer",call.=FALSE)
gradient_colors <- palette_value(
  "gradient_colors",
  "#543005,#8C510A,#BF812D,#DFC27D,#F6E8C3,#F5F5F5,#C7EAE5,#80CDC1,#35978F,#01665E,#003C30",
  minimum = 3L
)
page_background_color <- color("page_background_color", "#FFFFFF")
gradient <- character()
if (show_gradient && gradient_alpha > 0) {
  fills <- rev(grDevices::colorRampPalette(gradient_colors)(gradient_bands))
  foreground <- grDevices::col2rgb(fills)
  background <- grDevices::col2rgb(page_background_color)[, 1L]
  blended <- foreground * gradient_alpha + background * (1 - gradient_alpha)
  gradient <- grDevices::rgb(blended[1L, ], blended[2L, ], blended[3L, ], maxColorValue = 255)
}

family <- resolve_font(text("family", "Helvetica"))
point_size <- num("point_size", 1.4, 0.1, 10)
point_alpha <- num("point_alpha", 0.6, 0.05, 1)
point_stroke <- num("point_stroke", 0.12, 0.05, 2)
threshold_line_width <- num("threshold_line_width", 0.35, 0.1, 3)
label_threshold_line_width <- num("label_threshold_line_width", 0.3, 0.1, 3)
label_segment_width <- num("label_segment_width", 0.25, 0.05, 2)
label_box_padding <- num("label_box_padding", 0.8, 0, 5)
label_point_padding <- num("label_point_padding", 0.8, 0, 5)
label_seed <- num("label_seed", 206, 1, 2147483647)
if(label_seed!=floor(label_seed))stop("label_seed must be an integer",call.=FALSE)

show_threshold_lines <- bool("show_threshold_lines", TRUE)
show_label_threshold_lines <- bool("show_label_threshold_lines", TRUE)
show_legend <- bool("show_legend", TRUE)
legend_position <- text("legend_position", "right")
if (!legend_position %in% c("right", "left", "top", "bottom")) stop("legend_position must be right, left, top, or bottom", call. = FALSE)

title <- text("title", "", allow_empty = TRUE)
x_title <- text("x_title", "", allow_empty = TRUE)
if(!nzchar(x_title))x_title<-effect_column
y_title <- text("y_title", "", allow_empty = TRUE)
if(!nzchar(y_title))y_title<-paste0("-log10(",significance_column,")")
title_size <- num("title_size", 9, 4, 30)
axis_text_size <- num("axis_text_size", 8, 4, 24)
axis_title_size <- num("axis_title_size", 9, 4, 28)
legend_text_size <- num("legend_text_size", 8, 4, 24)
label_size <- num("label_size", 2.7, 1, 10)
panel_border_width <- num("panel_border_width", 0.3, 0.05, 2)
axis_line_width <- num("axis_line_width", 0.3, 0.05, 2)
x_expand_fraction <- num("x_expand_fraction", 0.05, 0, 0.3)
y_expand_fraction <- num("y_expand_fraction", 0.05, 0, 0.3)

caption_size<-num("caption_size",6,4,12)
caption<-text("caption","",allow_empty=TRUE)
if(!nzchar(caption))caption<-paste(if(using_bundled_data&&significance_column=="pvalue")"Bundled raw pvalue: unadjusted candidate screening, not FDR-confirmed findings."else paste0("Input metric: ",significance_column,"; adjustment method and test family unverified."),sprintf("All %d features shown; Negative/Positive meet %s < %g and |effect| >= %g; Other fails either rule.",nrow(dat),significance_column,significance_cutoff,effect_cutoff),sprintf("%d labels: %s < %g and |effect| >= %g; labels are candidates, not validated biomarkers.",nrow(labels),significance_column,label_significance_cutoff,label_effect_cutoff),"Effect direction needs a defined contrast; input design and model are unverified.",sep="\n")
if(show_gradient)caption<-paste(caption,"Background gradient is decorative and encodes no variable.",sep="\n")
if(any(grepl("case[ _-]*[0-9]+|cover_[0-9]+|debug|案例[ ]*[0-9]+",c(title,x_title,y_title,caption,labels$feature),ignore.case=TRUE)))stop("Internal case or debug labels cannot appear in figures",call.=FALSE)

plot <- ggplot2::ggplot(
  dat,
  ggplot2::aes(x = effect, y = neg_log10_significance, fill = status)
) +
  ggplot2::geom_point(
    shape = 21, colour = point_outline_color, alpha = point_alpha,
    size = point_size, stroke = point_stroke, show.legend = show_legend
  )

if (show_threshold_lines) {
  plot <- plot +
    ggplot2::geom_vline(
      xintercept = c(-effect_cutoff, effect_cutoff), linetype = 3,
      colour = threshold_color, linewidth = threshold_line_width
    ) +
    ggplot2::geom_hline(
      yintercept = -log10(significance_cutoff), linetype = 3,
      colour = threshold_color, linewidth = threshold_line_width
    )
}
if (show_label_threshold_lines) {
  plot <- plot + ggplot2::geom_vline(
    xintercept = c(-label_effect_cutoff, label_effect_cutoff), linetype = 3,
    colour = label_threshold_color, linewidth = label_threshold_line_width
  )
}
if (nrow(labels)) {
  plot <- plot + ggrepel::geom_label_repel(
    data = dat,
    ggplot2::aes(label = label),
    family = family, colour = label_color, size = label_size,
    fill=page_background_color,linewidth=0,label.padding=grid::unit(.1,"lines"),label.r=grid::unit(0,"lines"),
    nudge_x=ifelse(dat$is_label,sign(dat$effect)*diff(range(dat$effect))*.04,0),
    box.padding = grid::unit(label_box_padding, "lines"),
    point.padding = grid::unit(label_point_padding, "lines"),
    segment.colour = label_segment_color, segment.size = label_segment_width,
    max.overlaps = Inf, seed = label_seed, max.time=Inf,max.iter=10000,force=5,force_pull=0,point.size=point_size, show.legend = FALSE
  )
}

plot <- plot +
  ggplot2::scale_fill_manual(
    values = status_colors, breaks = c("down", "NS", "up"),
    labels = legend_labels, drop = FALSE, name = NULL
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = x_expand_fraction)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = y_expand_fraction)) +
  ggplot2::labs(
    title = if (nzchar(title)) title else NULL,
    x = if (nzchar(x_title)) x_title else NULL,
    y = if (nzchar(y_title)) y_title else NULL,caption=caption
  ) +
  ggplot2::theme_bw(base_family = family, base_size = 11) +
  ggplot2::theme(
    text = ggplot2::element_text(colour = text_color),
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(fill = NA, colour = text_color, linewidth = panel_border_width),
    axis.text = ggplot2::element_text(colour = text_color, size = axis_text_size),
    axis.title = ggplot2::element_text(colour = text_color, size = axis_title_size),
    axis.ticks = ggplot2::element_line(colour = text_color, linewidth = axis_line_width),
    axis.ticks.length = grid::unit(1.1, "mm"),
    plot.title = ggplot2::element_text(colour = text_color, size = title_size, face = "plain", hjust = 0),
    plot.caption=ggplot2::element_text(size=caption_size,hjust=0,lineheight=1.1,margin=ggplot2::margin(t=5)),
    legend.position = if (show_legend) legend_position else "none",
    legend.text = ggplot2::element_text(colour = text_color, size = legend_text_size),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key.width = grid::unit(4, "mm"),
    legend.key.height = grid::unit(4, "mm"),
    legend.spacing.y = grid::unit(0.8, "mm"),
    legend.box.spacing = grid::unit(3, "mm"),
    plot.margin = ggplot2::margin(2, 2, 2, 2.5, unit = "mm")
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(
    override.aes = list(shape = 21, colour = point_outline_color, alpha = point_alpha, size = point_size, stroke = point_stroke)
  ))

width_mm <- num("width_mm", 180, 90, 320)
height_mm <- num("height_mm", 136.5, 70, 320)
dpi <- num("dpi", 600, 96, 1200)
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

draw_plot <- function() {
  grid::grid.newpage()
  if (length(gradient)) {
    count <- length(gradient)
    grid::grid.rect(
      x = 0.5, y = (seq_len(count) - 0.5) / count,
      width = 1, height = 1 / count + 0.0001,
      gp = grid::gpar(fill = gradient, col = NA)
    )
  }
  grid::grid.draw(ggplot2::ggplotGrob(plot));grid::grid.force()
  built<-ggplot2::ggplot_build(plot);xr<-built$layout$panel_params[[1]]$x.range;yr<-built$layout$panel_params[[1]]$y.range
  if(any(!is.finite(c(xr,yr)))||any(!is.finite(c(diff(xr),diff(yr))))||any(c(diff(xr),diff(yr))<=0))stop("Plot spans must be finite and positive",call.=FALSE)
  v<-grid::grid.ls(viewports=TRUE,grobs=FALSE,recursive=TRUE,print=FALSE);ii<-grep("^panel[.]",v$name)
  if(length(ii)!=1)stop("Cannot identify volcano panel",call.=FALSE)
  i<-ii[1];vp<-c(strsplit(v$vpPath[i],"::",fixed=TRUE)[[1]][-1],v$name[i]);grid::upViewport(0);grid::downViewport(do.call(grid::vpPath,as.list(vp)));pw<-grid::convertWidth(grid::unit(1,"npc"),"mm",valueOnly=TRUE);ph<-grid::convertHeight(grid::unit(1,"npc"),"mm",valueOnly=TRUE)
  if(pw<50||ph<40)stop("Volcano panel is too small",call.=FALSE)
  bb<-built$data[[1]];radius<-(.375*(point_size*ggplot2::.pt+point_stroke*ggplot2::.stroke/2)+.75*point_stroke*ggplot2::.stroke/4)*25.4/72;xx<-(bb$x-xr[1])/diff(xr)*pw;yy<-(bb$y-yr[1])/diff(yr)*ph
  if(any(xx<radius|xx>pw-radius|yy<radius|yy>ph-radius))stop("Raw point footprint exceeds panel bounds",call.=FALSE)
  measure<-function(labels,size)vapply(labels,function(label)grid::convertWidth(grid::grobWidth(grid::textGrob(label,gp=grid::gpar(fontfamily=family,fontsize=size))),"mm",valueOnly=TRUE),numeric(1))
  for(axis in c("x","y")){p<-built$layout$panel_params[[1]][[axis]];labs<-p$get_labels();breaks<-p$get_breaks();keep<-is.finite(breaks);labs<-labs[keep];breaks<-breaks[keep];if(anyDuplicated(labs))stop("Axis tick labels must be distinguishable",call.=FALSE);spacing<-if(length(breaks)>1)min(diff(breaks))/diff(if(axis=="x")xr else yr)*(if(axis=="x")pw else ph)else Inf;need<-if(axis=="x")max(measure(labs,axis_text_size))else axis_text_size*25.4/72;if(need+.4>spacing)stop("Axis tick labels overlap",call.=FALSE)}
  if(any(measure(strsplit(caption,"\n")[[1]],caption_size)>pw)||measure(title,title_size)>pw)stop("Caption or title does not fit volcano panel",call.=FALSE)
  if(length(labels$feature)&&any(measure(labels$feature,label_size*ggplot2::.pt)>pw*.6))stop("Feature label is too wide for volcano panel",call.=FALSE)
  grid::upViewport(0)
}

ragg::agg_png(
  file.path(output_dir, "plot.png"), width = width_in, height = height_in,
  units = "in", res = dpi, background = page_background_color
)
draw_plot()
invisible(grDevices::dev.off())
grDevices::pdf(
  file.path(output_dir, "plot.pdf"), width = width_in, height = height_in,
  bg = page_background_color, family=family,useDingbats = FALSE
)
draw_plot()
invisible(grDevices::dev.off())
svglite::svglite(
  file.path(output_dir, "plot.svg"), width = width_in, height = height_in,
  bg = page_background_color
)
draw_plot()
invisible(grDevices::dev.off())

counts <- table(dat$status)
cat(sprintf(
  "Rendered volcano plot: %d features; down=%d, NS=%d, up=%d; labels=%d; metric=%s\n",
  nrow(dat), counts[["down"]], counts[["NS"]], counts[["up"]], nrow(labels), significance_column
))
