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
packages <- c("jsonlite", "ggplot2", "readxl", "dplyr", "maps", "ggsankey", "patchwork", "ragg", "svglite")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
params <- cfg$params %||% list()
gp <- function(name, default = NULL) params[[name]] %||% default

# Keep every declared control visible to plot_nh's lexical runtime inspector.
.registry_parameter_bindings <- list(
  data_path = gp("data_path", NULL), excel_sheet = gp("excel_sheet", NULL),
  sample_id_column = gp("sample_id_column", NULL), longitude_column = gp("longitude_column", NULL),
  latitude_column = gp("latitude_column", NULL), continent_column = gp("continent_column", NULL),
  landcover_column = gp("landcover_column", NULL), habitat_column = gp("habitat_column", NULL),
  continent_order = gp("continent_order", NULL), continent_colors = gp("continent_colors", NULL),
  aggregate_flows = gp("aggregate_flows", NULL), max_points = gp("max_points", NULL),
  show_sankey_labels = gp("show_sankey_labels", NULL), show_map_legend = gp("show_map_legend", NULL),
  show_annotation = gp("show_annotation", NULL), title = gp("title", NULL),
  subtitle = gp("subtitle", NULL), caption = gp("caption", NULL),
  flow_alpha = gp("flow_alpha", NULL), node_fill = gp("node_fill", NULL),
  flow_fill = gp("flow_fill", NULL), map_fill = gp("map_fill", NULL),
  map_border = gp("map_border", NULL), map_point_size = gp("map_point_size", NULL),
  map_point_alpha = gp("map_point_alpha", NULL), base_size = gp("base_size", NULL),
  label_size = gp("label_size", NULL), legend_text_size = gp("legend_text_size", NULL),
  family = gp("family", NULL), width_mm = gp("width_mm", NULL),
  height_mm = gp("height_mm", NULL), dpi = gp("dpi", NULL)
)

file_arg <- grep("^--file=", commandArgs(), value = TRUE)
source_files <- vapply(sys.frames(), function(frame) frame$ofile %||% "", character(1L))
source_file <- tail(source_files[nzchar(source_files)], 1L)
script_file <- if (length(source_file)) source_file else if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else NULL
script_dir <- dirname(normalizePath(script_file, mustWork = TRUE))
output_dir <- arg_value("output-dir", cfg$output_dir %||% file.path(script_dir, "output"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(name, default, lower = -Inf, upper = Inf) {
  if(is.list(gp(name,default)))stop(name," must be a complete scalar",call.=FALSE)
  value <- suppressWarnings(as.numeric(gp(name, default)))
  if (length(value) != 1L || !is.finite(value) || value < lower || value > upper) stop(sprintf("%s must be between %s and %s", name, lower, upper), call. = FALSE)
  value
}
integer_param <- function(name, default, lower, upper) {
  value <- num(name, default, lower, upper)
  if (value != floor(value)) stop(name, " must be an integer", call. = FALSE)
  as.integer(value)
}
text_param <- function(name, default, allow_empty = FALSE) {
  if(is.list(gp(name,default)))stop(name," must be a complete scalar",call.=FALSE)
  value <- as.character(gp(name, default))
  if (length(value) != 1L || is.na(value)) stop(name, " must be one text value", call. = FALSE)
  value <- trimws(value)
  if (!allow_empty && !nzchar(value)) stop(name, " cannot be empty", call. = FALSE)
  value
}
bool <- function(name, default) {
  value <- gp(name, default)
  if(is.list(value)||length(value)!=1L||anyNA(value))stop(name," must be a complete scalar",call.=FALSE)
  if (is.logical(value) && length(value) == 1L && !is.na(value)) return(value)
  key <- tolower(trimws(as.character(value)))
  if (length(key) == 1L && key %in% c("true", "1", "yes")) return(TRUE)
  if (length(key) == 1L && key %in% c("false", "0", "no")) return(FALSE)
  stop(name, " must be true or false", call. = FALSE)
}
choice <- function(name, default, choices) {
  value <- text_param(name, default)
  if (!value %in% choices) stop(name, " must be one of: ", paste(choices, collapse = ", "), call. = FALSE)
  value
}
csv_values <- function(name, default = "", allow_empty = TRUE) {
  value <- text_param(name, default, allow_empty = allow_empty)
  if (!nzchar(value)) return(character())
  output <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  output <- output[nzchar(output)]
  if (!length(output) && !allow_empty) stop(name, " cannot be empty", call. = FALSE)
  if (anyDuplicated(output)) stop(name, " contains duplicate values", call. = FALSE)
  output
}
validate_colours <- function(values, name) {
  invisible(tryCatch(grDevices::col2rgb(values), error = function(e) stop(name, " contains an invalid colour", call. = FALSE)))
  values
}

resolve_input <- function(value, default_path) {
  candidate <- if (nzchar(value)) value else default_path
  if (!file.exists(candidate)) {
    local_candidate <- file.path(script_dir, candidate)
    if (file.exists(local_candidate)) candidate <- local_candidate
  }
  if (!file.exists(candidate)) stop("Input file does not exist: ", candidate, call. = FALSE)
  normalizePath(candidate, mustWork = TRUE)
}
read_table <- function(path, sheet = "") {
  info <- file.info(path)
  if (is.na(info$size) || info$size == 0) stop("Input file is empty", call. = FALSE)
  extension <- tolower(tools::file_ext(path))
  if (extension == "csv") out <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character")
  else if (extension %in% c("tsv", "txt")) {
    first_line <- readLines(path, n = 1L, warn = FALSE)
    if (extension == "tsv" || (length(first_line) && grepl("\t", first_line, fixed = TRUE))) {
      out <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character")
    } else out <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character")
  } else if (extension %in% c("xls", "xlsx")) {
    read_args <- list(path = path, .name_repair = "minimal",col_types="text")
    if (nzchar(sheet)) read_args$sheet <- sheet
    out <- as.data.frame(do.call(readxl::read_excel, read_args), check.names = FALSE, stringsAsFactors = FALSE)
  } else if (extension == "rds") {
    out <- readRDS(path)
    if (!is.data.frame(out)) stop("RDS input must contain a data frame", call. = FALSE)
    out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  names(out) <- sub("^\ufeff", "", names(out))
  out
}
parse_number <- function(values, name) {
  raw <- trimws(as.character(values))
  if (any(is.na(values) | is.na(raw) | !nzchar(raw))) stop(name, " contains missing values", call. = FALSE)
  output <- suppressWarnings(as.numeric(raw))
  if (anyNA(output) || any(!is.finite(output))) stop(name, " contains non-finite or non-numeric values", call. = FALSE)
  output
}
resolve_required_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
}

default_data_path <- file.path(script_dir, "assets", "41467_2023_42142_MOESM8_ESM.xlsx")
data_path <- text_param("data_path", "assets/41467_2023_42142_MOESM8_ESM.xlsx", allow_empty = TRUE)
excel_sheet <- text_param("excel_sheet", "Fig.1", allow_empty = TRUE)
df <- read_table(resolve_input(data_path, default_data_path), excel_sheet)
if (!nrow(df) || !ncol(df)) stop("Input table is empty", call. = FALSE)
if(anyNA(names(df))||any(!nzchar(trimws(names(df))))||anyDuplicated(names(df)))stop("Input column names must be complete and unique",call.=FALSE)

sample_id_column <- text_param("sample_id_column", "SampleId")
longitude_column <- text_param("longitude_column", "longitude")
latitude_column <- text_param("latitude_column", "latitude")
continent_column <- text_param("continent_column", "Continent")
landcover_column <- text_param("landcover_column", "LandCoverType")
habitat_column <- text_param("habitat_column", "Habitat")
required_columns <- c(sample_id_column, longitude_column, latitude_column, continent_column, landcover_column, habitat_column)
if (anyDuplicated(required_columns)) stop("Data role columns must be distinct", call. = FALSE)
resolve_required_columns(df, required_columns)
sample_id <- trimws(as.character(df[[sample_id_column]]))
continent <- trimws(as.character(df[[continent_column]]))
landcover <- trimws(as.character(df[[landcover_column]]))
habitat <- trimws(as.character(df[[habitat_column]]))
if (anyNA(sample_id) || any(!nzchar(sample_id))) stop("Sample ID column contains missing or empty labels", call. = FALSE)
if (anyDuplicated(sample_id)) stop("Sample ID column must be unique", call. = FALSE)
if (anyNA(continent) || any(!nzchar(continent))) stop("Continent column contains missing or empty labels", call. = FALSE)
if (anyNA(landcover) || any(!nzchar(landcover))) stop("LandCoverType column contains missing or empty labels", call. = FALSE)
if (anyNA(habitat) || any(!nzchar(habitat))) stop("Habitat column contains missing or empty labels", call. = FALSE)
longitude <- parse_number(df[[longitude_column]], longitude_column)
latitude <- parse_number(df[[latitude_column]], latitude_column)
if (any(longitude < -180 | longitude > 180)) stop(longitude_column, " must be between -180 and 180", call. = FALSE)
if (any(latitude < -90 | latitude > 90)) stop(latitude_column, " must be between -90 and 90", call. = FALSE)
max_points <- integer_param("max_points", 25000, 1, 2000000)
if (nrow(df) > max_points) stop("Input has ", nrow(df), " rows; increase max_points to plot them", call. = FALSE)

default_continent_order <- c("North America", "South America", "Europe", "Africa", "Asia", "Australia", "Antarctica", "Pacific Ocean", "Atlantic Ocean", "Indian Ocean", "Arctic Ocean")
requested_continent_order <- csv_values("continent_order", paste(default_continent_order, collapse = ","))
observed_continents <- unique(continent)
if(!length(requested_continent_order))requested_continent_order<-sort(observed_continents)
if (any(!observed_continents %in% requested_continent_order)) stop("continent_order must include every observed continent", call. = FALSE)
continent_levels <- requested_continent_order[requested_continent_order %in% observed_continents]
df_plot <- data.frame(SampleId = sample_id, longitude = longitude, latitude = latitude,
                      Continent = factor(continent, levels = continent_levels),
                      LandCoverType = landcover, Habitat = habitat,
                      stringsAsFactors = FALSE)
df_plot <- df_plot[order(df_plot$Continent, df_plot$longitude, df_plot$latitude, df_plot$SampleId), , drop = FALSE]

default_continent_colors <- c(
  "North America" = "#0072B2", "South America" = "#D55E00", "Europe" = "#E69F00",
  "Africa" = "#CC79A7", "Asia" = "#009E73", "Australia" = "#56B4E9",
  "Antarctica" = "#7F7F7F", "Pacific Ocean" = "#332288", "Atlantic Ocean" = "#AA4499",
  "Indian Ocean" = "#999933", "Arctic Ocean" = "#117733"
)
continent_colors_input <- csv_values("continent_colors", paste(unname(default_continent_colors), collapse = ","))
if (identical(continent_colors_input, unname(default_continent_colors)) && all(continent_levels %in% names(default_continent_colors))) {
  continent_colors <- unname(default_continent_colors[continent_levels])
} else {
  if (length(continent_colors_input) != length(continent_levels)) stop("continent_colors must match observed continent count", call. = FALSE)
  continent_colors <- continent_colors_input
}
continent_colors <- validate_colours(continent_colors, "continent_colors")
names(continent_colors) <- continent_levels
if(anyDuplicated(vapply(continent_colors,function(c)paste(grDevices::col2rgb(c),collapse=","),character(1))))stop("Continent colours must be distinct",call.=FALSE)

aggregate_flows <- bool("aggregate_flows", TRUE)
if (aggregate_flows) {
  flow_counts <- df_plot |>
    dplyr::count(Continent, LandCoverType, Habitat, name = "value") |>
    dplyr::mutate(Global = "Global")
} else {
  flow_counts <- df_plot |>
    dplyr::transmute(Global = "Global", Continent = as.character(Continent),
                     LandCoverType = LandCoverType, Habitat = Habitat, value = 1)
}
flow_counts <- flow_counts |>
  dplyr::mutate(dplyr::across(c(Global, Continent, LandCoverType, Habitat), as.character))
flow_counts <- flow_counts[order(match(flow_counts$Continent, continent_levels), flow_counts$LandCoverType, flow_counts$Habitat), , drop = FALSE]
# Integer keys keep equal labels in different stages separate, and enforce top-to-bottom order.
stage_names<-c("Global","Continent","LandCoverType","Habitat")
stage_levels<-list("Global",continent_levels,sort(unique(landcover)),sort(unique(habitat)))
node_lookup<-data.frame(stage=rep(stage_names,lengths(stage_levels)),label=unlist(stage_levels),stringsAsFactors=FALSE);node_lookup$key<-paste0("N",seq_len(nrow(node_lookup)));node_lookup$count<-unlist(lapply(seq_along(stage_names),function(i)if(i==1)length(sample_id)else as.integer(table(factor(df_plot[[stage_names[i]]],levels=stage_levels[[i]])))))
node_labels<-setNames(paste0(node_lookup$label," (",node_lookup$count,")"),node_lookup$key)
keyed_flows<-flow_counts
for(k in seq_along(stage_names)){j<-which(node_lookup$stage==stage_names[k]);keyed_flows[[stage_names[k]]]<-node_lookup$key[j][match(flow_counts[[stage_names[k]]],node_lookup$label[j])]}
sankey_data <- ggsankey::make_long(keyed_flows, Global, Continent, LandCoverType, Habitat, value = value)
sankey_data$node<-factor(sankey_data$node,levels=rev(node_lookup$key));sankey_data$next_node<-factor(sankey_data$next_node,levels=rev(node_lookup$key))

show_sankey_labels <- bool("show_sankey_labels", TRUE)
show_map_legend <- bool("show_map_legend", TRUE)
show_annotation <- bool("show_annotation", TRUE)
title <- text_param("title", "", allow_empty = TRUE)
subtitle <- text_param("subtitle", "", allow_empty = TRUE)
caption <- text_param("caption", "", allow_empty = TRUE)
flow_alpha <- num("flow_alpha", 0.60, 0.05, 1)
node_fill <- text_param("node_fill", "#8DD3C7")
flow_fill <- text_param("flow_fill", "#87929C")
map_fill <- text_param("map_fill", "#D9DEE3")
map_border <- text_param("map_border", "#FFFFFF")
invisible(validate_colours(c(node_fill, flow_fill, map_fill, map_border), "plot colours"))
map_point_size <- num("map_point_size", 0.70, 0.1, 3)
map_point_alpha <- num("map_point_alpha", 0.60, 0.05, 1)
base_size <- num("base_size", 9, 6, 20)
label_size <- num("label_size", 2.4, 1, 6)
legend_text_size <- num("legend_text_size", 7, 5, 14)
family <- text_param("family", "Helvetica")
if (!family %in% c("Helvetica", "sans")) stop("family must be Helvetica or sans", call. = FALSE)
width_mm <- num("width_mm", 240, 160, 420)
height_mm <- num("height_mm", 205, 120, 360)
dpi <- num("dpi", 600, 96, 1200)

north_america<-df_plot[df_plot$Continent=="North America",,drop=FALSE]
annotation_text<-sprintf("North America: %d supplied IDs, %d land-cover types, %d habitats.",nrow(north_america),length(unique(north_america$LandCoverType)),length(unique(north_america$Habitat)))
if(!nzchar(caption))caption<-paste(sprintf("%d supplied IDs; %d distinct coordinate pairs; four-stage widths and node labels = counts.",nrow(df_plot),nrow(unique(df_plot[c("longitude","latitude")]))),"Coincident sample points overlap; equal-angle world map is not equal-area; sampling density is not abundance or prevalence.",if(show_annotation)annotation_text else"Source, independent sampling and geographic category assignments unverified.",sep="\n")else if(show_annotation)caption<-paste(caption,annotation_text,sep="\n")
if(any(grepl("case[ _-]*[0-9]+|cover_[0-9]+|debug|案例[ ]*[0-9]+",c(node_lookup$label,title,subtitle,caption),ignore.case=TRUE)))stop("Internal case or debug labels cannot appear in figures",call.=FALSE)

sankey_plot <- ggplot2::ggplot(
  sankey_data,
  ggplot2::aes(x = x, next_x = next_x, node = node, next_node = next_node,
               value = value, group = 1)
) +
  ggsankey::geom_sankey(flow.alpha = flow_alpha, node.fill = node_fill,
                        flow.fill = flow_fill, width = 0.18, smooth = 8) +
  ggplot2::scale_x_discrete(
    position = "top",
    labels = c(Global = "Global", Continent = "Continent",
               LandCoverType = "Land cover", Habitat = "Habitat")
  ) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = base_size, base_family = family) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = base_size * 0.95, colour = "#263238", margin = ggplot2::margin(b = 3)),
    plot.margin = ggplot2::margin(4, 8, 2, 8),
    text = ggplot2::element_text(family = family, colour = "#263238")
  )
if (show_sankey_labels) {
  node_positions<-ggplot2::ggplot_build(sankey_plot)$data[[2]]
  node_positions$label<-unname(node_labels[as.character(node_positions$node)])
  node_positions$x<-(node_positions$xmin+node_positions$xmax)/2
  node_positions$y<-(node_positions$ymin+node_positions$ymax)/2
  sankey_plot <- sankey_plot +
    ggplot2::geom_text(data=node_positions,ggplot2::aes(x=x,y=y,label=label),inherit.aes=FALSE,size=label_size,
                      family=family,colour="#263238",show.legend=FALSE)
}

world <- ggplot2::map_data("world")
map_plot <- ggplot2::ggplot() +
  ggplot2::geom_polygon(
    data = world,
    ggplot2::aes(x = long, y = lat, group = group),
    fill = map_fill, colour = map_border, linewidth = 0.12
  ) +
  ggplot2::geom_point(
    data = df_plot,
    ggplot2::aes(x = longitude, y = latitude, colour = Continent),
    size = map_point_size, alpha = map_point_alpha, shape=16,stroke = 0
  ) +
  ggplot2::scale_colour_manual(values = continent_colors, drop = FALSE, name = "Continent") +
  ggplot2::coord_fixed(ratio=1,xlim = c(-182, 182), ylim = c(-92, 92), expand = FALSE) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_void(base_size = base_size, base_family = family) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
    panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
    legend.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
    legend.position = if (show_map_legend) "bottom" else "none",
    legend.title = ggplot2::element_text(size = legend_text_size, face = "bold"),
    legend.text = ggplot2::element_text(size = legend_text_size),
    legend.key.width = grid::unit(0.8, "lines"),
    legend.key.height = grid::unit(0.55, "lines"),
    plot.margin = ggplot2::margin(2, 5, 2, 5)
  ) +
  ggplot2::guides(colour = ggplot2::guide_legend(
    nrow = 2, byrow = TRUE, override.aes = list(size = 2.8, alpha = 1)
  ))
plot_obj <- sankey_plot / map_plot +
  patchwork::plot_layout(ncol = 1, heights = c(1.45, 2.0), guides = "keep")
if (nzchar(title) || nzchar(subtitle) || nzchar(caption)) {
  plot_obj <- plot_obj + patchwork::plot_annotation(
    title = if (nzchar(title)) title else NULL,
    subtitle = if (nzchar(subtitle)) subtitle else NULL,
    caption = if (nzchar(caption)) caption else NULL,
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(family = family, face = "bold", size = base_size * 1.3, colour = "#263238", hjust = 0),
      plot.subtitle = ggplot2::element_text(family = family, size = base_size, colour = "#4B5563", hjust = 0),
      plot.caption = ggplot2::element_text(family = family, size = 6, colour = "#4B5563", hjust = 0,lineheight=1.1),
      plot.margin = ggplot2::margin(8, 10, 6, 10)
    )
  )
}

width_in <- width_mm / 25.4
height_in <- height_mm / 25.4
render_one<-function(kind){
 path<-file.path(output_dir,paste0("plot.",kind));if(kind=="png")ragg::agg_png(path,width=width_in,height=height_in,units="in",res=dpi,background="white")else if(kind=="pdf")grDevices::pdf(path,width=width_in,height=height_in,family=if(family=="sans")"Helvetica"else family,useDingbats=FALSE,bg="white")else svglite::svglite(path,width=width_in,height=height_in,bg="white")
 tryCatch({g<-patchwork::patchworkGrob(plot_obj);grid::grid.draw(g);grid::grid.force();v<-grid::grid.ls(viewports=TRUE,grobs=FALSE,recursive=TRUE,print=FALSE);ii<-grep("^panel-1[.]|^panel[.]",v$name);if(length(ii)!=2)stop("Cannot identify Sankey/map panels",call.=FALSE)
 for(i in ii){vp<-c(strsplit(v$vpPath[i],"::",fixed=TRUE)[[1]][-1],v$name[i]);grid::upViewport(0);grid::downViewport(do.call(grid::vpPath,as.list(vp)));pw<-grid::convertWidth(grid::unit(1,"npc"),"mm",valueOnly=TRUE);ph<-grid::convertHeight(grid::unit(1,"npc"),"mm",valueOnly=TRUE);if(pw<80||ph<40)stop("Sankey/map panels are too small",call.=FALSE)
 if(grepl("^panel-1[.]",v$name[i])&&show_sankey_labels){b<-ggplot2::ggplot_build(sankey_plot);d<-b$data[[3]];xr<-b$layout$panel_params[[1]]$x.range;yr<-b$layout$panel_params[[1]]$y.range;xx<-(d$x-xr[1])/diff(xr)*pw;yy<-(d$y-yr[1])/diff(yr)*ph;ww<-vapply(d$label,function(z)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=label_size*ggplot2::.pt))),"mm",valueOnly=TRUE),numeric(1));hh<-label_size*1.1
 if(any(xx-ww/2<0|xx+ww/2>pw|yy-hh/2<0|yy+hh/2>ph))stop("Node label footprint exceeds panel bounds",call.=FALSE)
 for(j in seq_len(nrow(d)))if(j<nrow(d))for(k in(j+1):nrow(d))if(abs(xx[j]-xx[k])<(ww[j]+ww[k])/2&&abs(yy[j]-yy[k])<hh)stop("Node labels overlap; increase canvas or reduce label_size",call.=FALSE)
 }
 if(grepl("^panel[.]",v$name[i])){rad<-.375*map_point_size*ggplot2::.pt*25.4/72;xx<-(df_plot$longitude+182)/364*pw;yy<-(df_plot$latitude+92)/184*ph;if(any(xx<rad|xx>pw-rad|yy<rad|yy>ph-rad))stop("Map point footprint exceeds panel bounds",call.=FALSE)}
 }
 grid::upViewport(0);for(z in strsplit(caption,"\n")[[1]])if(grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=6))),"mm",valueOnly=TRUE)>width_mm-20*25.4/72)stop("Caption does not fit canvas",call.=FALSE)
 },finally=grDevices::dev.off())
}
for(kind in c("png","pdf","svg"))render_one(kind)
cat(sprintf("Rendered all %d supplied IDs; %d continents; %d unique coordinate pairs\n",nrow(df_plot),length(continent_levels),nrow(unique(df_plot[c("longitude","latitude")]))))
