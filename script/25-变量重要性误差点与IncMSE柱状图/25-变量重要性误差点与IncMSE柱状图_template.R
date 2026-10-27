#!/usr/bin/env Rscript

local({
  color_module <- Sys.getenv("PLOT_NH_COLOR_MODULE", unset = "")
  if (!nzchar(color_module) || !file.exists(color_module)) {
    file_arg <- grep("^--file=", commandArgs(), value = TRUE)
    start_dir <- if (length(file_arg)) dirname(sub("^--file=", "", file_arg[[1L]])) else getwd()
    start_dir <- normalizePath(start_dir, mustWork = FALSE)
    for (level in seq_len(12L)) {
      candidates <- c(file.path(start_dir, "manual_library", "scripts", "plot_nh_colors.R"),
                      file.path(start_dir, "scripts", "plot_nh_colors.R"),
                      file.path(start_dir, "plot_nh_colors.R"))
      hit <- candidates[file.exists(candidates)]
      if (length(hit)) { color_module <- hit[[1L]]; break }
      parent_dir <- dirname(start_dir)
      if (identical(parent_dir, start_dir)) break
      start_dir <- parent_dir
    }
  }
  if (!nzchar(color_module) || !file.exists(color_module)) stop("plot_nh shared color module not found; set PLOT_NH_COLOR_MODULE.", call. = FALSE)
  source(normalizePath(color_module, mustWork = TRUE), local = .GlobalEnv)
  plot_nh_color_context()
})

or_else<-function(x,y)if(is.null(x))y else x
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
packages <- c("jsonlite", "ggplot2", "cowplot", "ragg", "svglite", "readxl")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
params <- if (is.null(cfg$params)) list() else cfg$params
gp <- function(name, default = NULL) if (is.null(params[[name]])) default else params[[name]]
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
source_files <- vapply(sys.frames(), function(frame) frame$ofile %||% "", character(1L))
source_file <- tail(source_files[nzchar(source_files)], 1L)
script_file <- if (length(source_file)) source_file else if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else NULL
script_dir <- dirname(normalizePath(script_file, mustWork = TRUE))
output_dir <- arg_value("output-dir", if (is.null(cfg$output_dir)) file.path(script_dir, "output") else cfg$output_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

.registry_parameter_bindings <- list(
  data_path = gp("data_path", NULL), excel_sheet = gp("excel_sheet", NULL), group_column = gp("group_column", NULL),
  type_column = gp("type_column", NULL), mean_column = gp("mean_column", NULL), error_column = gp("error_column", NULL),
  incmse_column = gp("incmse_column", NULL), group_order = gp("group_order", NULL), type_order = gp("type_order", NULL),
  palette_colors = gp("palette_colors", NULL), point_size = gp("point_size", NULL), errorbar_width = gp("errorbar_width", NULL),
  line_width = gp("line_width", NULL), bar_width = gp("bar_width", NULL), show_legend = gp("show_legend", NULL),
  legend_position = gp("legend_position", NULL), family = gp("family", NULL), base_size = gp("base_size", NULL),
  axis_text_size = gp("axis_text_size", NULL), x_title_left = gp("x_title_left", NULL), x_title_right = gp("x_title_right", NULL),
  width_mm = gp("width_mm", NULL), height_mm = gp("height_mm", NULL), dpi = gp("dpi", NULL)
)

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
resolve_input <- function(value, fallback, required = TRUE) {
  value <- trimws(as.character(value))
  if (!nzchar(value)) {
    if (!is.null(fallback) && file.exists(fallback)) return(normalizePath(fallback, mustWork = TRUE))
    if (required) stop("Input path is required", call. = FALSE)
    return(NULL)
  }
  candidate <- if (file.exists(value)) value else file.path(script_dir, value)
  if (!file.exists(candidate)) {
    if (!required) return(NULL)
    stop("Input file does not exist: ", candidate, call. = FALSE)
  }
  normalizePath(candidate, mustWork = TRUE)
}
read_table <- function(path, sheet = "") {
  extension <- tolower(tools::file_ext(path))
  read_delimited <- function() {
    first_line <- readLines(path, n = 1L, warn = FALSE)
    if (length(first_line) && grepl("\t", first_line, fixed = TRUE)) utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character")
    else utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character")
  }
  if (extension %in% c("csv", "tsv", "txt")) out <- read_delimited()
  else if (extension %in% c("xls", "xlsx")) {
    out <- tryCatch({
      read_args <- list(path = path, .name_repair = "minimal",col_types="text")
      if (nzchar(sheet)) read_args$sheet <- sheet
      as.data.frame(do.call(readxl::read_excel, read_args), check.names = FALSE, stringsAsFactors = FALSE)
    }, error = function(e) {
      first_line <- readLines(path, n = 1L, warn = FALSE)
      if (length(first_line) && grepl("\t", first_line, fixed = TRUE)) read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character")
      else stop("Could not read table as Excel or delimited text: ", conditionMessage(e), call. = FALSE)
    })
  } else if (extension == "rds") {
    out <- readRDS(path)
    if (!is.data.frame(out)) stop("RDS input must contain a data frame", call. = FALSE)
    out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  names(out) <- sub("^\ufeff", "", names(out))
  out
}
resolve_column <- function(requested, available, label) {
  if (requested %in% available) return(requested)
  aliases <- available[make.names(available) == make.names(requested)]
  if (length(aliases) == 1L) return(aliases[[1L]])
  stop(label, " column not found: ", requested, call. = FALSE)
}
parse_palette <- function(value) {
  if(is.list(value)||length(value)!=1||anyNA(value))stop("palette_colors must be a complete scalar",call.=FALSE)
  raw <- trimws(as.character(value))
  if (grepl("=", raw, fixed = TRUE)) {
    tokens <- if (grepl(";", raw, fixed = TRUE)) strsplit(raw, ";", fixed = TRUE)[[1L]] else strsplit(raw, ",", fixed = TRUE)[[1L]]
    pairs <- lapply(tokens, function(token) {
      positions <- gregexpr("=", token, fixed = TRUE)[[1L]]
      if(positions[1]<0)stop("palette_colors must use label=color pairs",call.=FALSE)
      split_at <- max(positions)
      label <- trimws(substr(token, 1L, split_at - 1L))
      colour <- trimws(substr(token, split_at + 1L, nchar(token)))
      if (!nzchar(label) || !nzchar(colour)) stop("palette_colors has an empty label or colour", call. = FALSE)
      tryCatch(grDevices::col2rgb(colour), error = function(e) stop("palette_colors has invalid colour: ", colour, call. = FALSE))
      c(label = label, colour = colour)
    })
    labels <- vapply(pairs, `[[`, character(1L), "label")
    colours <- vapply(pairs, `[[`, character(1L), "colour")
    if (anyDuplicated(labels)) stop("palette_colors has duplicate labels", call. = FALSE)
    return(stats::setNames(colours, labels))
  }
  raw <- trimws(unlist(strsplit(gsub("['\"]", "", raw), "[,;]")))
  raw <- raw[nzchar(raw)]
  if (!length(raw)) stop("palette_colors must contain at least one colour", call. = FALSE)
  vapply(seq_along(raw), function(i) {
    tryCatch(grDevices::col2rgb(raw[[i]]), error = function(e) stop("palette_colors has invalid colour: ", raw[[i]], call. = FALSE))
    raw[[i]]
  }, character(1L))
}
data_path <- resolve_input(text_param("data_path", "assets/data.xls"), file.path(script_dir, "assets", "data.xls"), required = TRUE)
df_raw <- read_table(data_path, text_param("excel_sheet", "", allow_empty = TRUE))
if(!nrow(df_raw)||anyNA(names(df_raw))||any(!nzchar(trimws(names(df_raw))))||anyDuplicated(names(df_raw)))stop("Input must have rows and complete unique column names",call.=FALSE)
group_col <- resolve_column(text_param("group_column", "group"), names(df_raw), "group")
type_col <- resolve_column(text_param("type_column", "type"), names(df_raw), "type")
mean_col <- resolve_column(text_param("mean_column", "mean"), names(df_raw), "mean")
error_col <- resolve_column(text_param("error_column", "error"), names(df_raw), "error")
incmse_col <- resolve_column(text_param("incmse_column", "IncMSE (%)"), names(df_raw), "IncMSE")
if(anyDuplicated(c(group_col,type_col,mean_col,error_col,incmse_col)))stop("Data roles must be distinct",call.=FALSE)
group <- trimws(as.character(df_raw[[group_col]]))
type <- trimws(as.character(df_raw[[type_col]]))
if (!length(group) || anyNA(group) || any(!nzchar(group)) || anyNA(type) || any(!nzchar(type))) stop("group and type must be non-empty", call. = FALSE)
as_number <- function(column, label) {
  raw <- as.character(column)
  value <- suppressWarnings(as.numeric(raw))
  invalid <- (!is.na(raw) & nzchar(trimws(raw)) & is.na(value))|(!is.na(value)&!is.finite(value))
  if (any(invalid)) stop(label, " contains non-numeric values", call. = FALSE)
  value
}
mean_value <- as_number(df_raw[[mean_col]], "mean")
error_value <- as_number(df_raw[[error_col]], "error")
incmse_value <- as_number(df_raw[[incmse_col]], "IncMSE")
if (any(error_value < 0, na.rm = TRUE)) stop("error must be non-negative", call. = FALSE)
if (!any(is.finite(mean_value)) || !any(is.finite(incmse_value))) stop("mean and IncMSE must each contain at least one finite value", call. = FALSE)
df <- data.frame(group = group, type = type, mean = mean_value, error = error_value, incmse = incmse_value, stringsAsFactors = FALSE)
if(anyDuplicated(df[,c("group","type")]))stop("Group/type pairs must be unique",call.=FALSE)
group_order <- choice("group_order", "reference", c("reference", "reverse_alphabetical", "alphabetical", "input"))
type_order <- choice("type_order", "source", c("source", "alphabetical", "input"))
reference_display <- c("Silt+Clay", "pH", "Mncd", "MAT", "MAP", "Fungal quantity", "Fungal composition", "Fungal Chao1", "Fungal-to-bacterial ratio", "Feox", "FeHCl", "Fecd-ox", "Cacd", "Bulk N", "Bulk C/N", "Bacterial quantity", "Alox")
group_levels <- if (group_order == "input") rev(unique(df$group)) else if (group_order == "reference") {
  rev(c(intersect(reference_display, unique(df$group)), setdiff(sort(unique(df$group), method = "radix"), reference_display)))
} else rev(sort(unique(df$group), decreasing = identical(group_order, "reverse_alphabetical"), method = "radix"))
type_levels <- if (type_order == "input") unique(df$type) else if (type_order == "source") {
  source_types <- c("fieldlignin", "lignin", "litter", "SOC")
  c(intersect(source_types, unique(df$type)), setdiff(sort(unique(df$type), method = "radix"), source_types))
} else sort(unique(df$type), method = "radix")
df$group <- factor(df$group, levels = group_levels)
df$type <- factor(df$type, levels = type_levels)
df <- df[order(df$group, df$type, method = "radix"), , drop = FALSE]

palette <- parse_palette(text_param("palette_colors", "fieldlignin=#E64B35;lignin=#4DBBD5;litter=#00A087;SOC=#3C5488"))
if (!is.null(names(palette))) {
  missing_types <- setdiff(type_levels, names(palette))
  if (length(missing_types)) stop("palette_colors is missing types: ", paste(missing_types, collapse = ", "), call. = FALSE)
  palette_map <- palette[type_levels]
} else {
  if (length(palette) < length(type_levels)) stop("palette_colors has too few colours for type levels", call. = FALSE)
  palette_map <- stats::setNames(palette[seq_along(type_levels)], type_levels)
}
if(anyDuplicated(vapply(palette_map,function(c)paste(grDevices::col2rgb(c),collapse=","),character(1))))stop("Type colours must be distinct",call.=FALSE)
family <- choice("family", "Helvetica", c("Helvetica", "sans"))
base_size <- num("base_size", 9, 5, 20)
axis_text_size <- num("axis_text_size", 8, 4, 18)
point_size <- num("point_size", 1.8, 0.3, 6)
errorbar_width <- num("errorbar_width", 0.4, 0.05, 1)
line_width <- num("line_width", 0.45, 0.1, 2)
bar_width <- num("bar_width", 0.56, 0.2, 1)
show_legend <- bool("show_legend", TRUE)
legend_position <- choice("legend_position", "bottom", c("inside", "right", "left", "top", "bottom", "none"))
x_title_left <- text_param("x_title_left", "standardized coefficient")
x_title_right <- text_param("x_title_right", "IncMSE (%)")
width_mm <- num("width_mm", 180, 100, 400)
height_mm <- num("height_mm", 185, 100, 400)
dpi <- num("dpi", 600, 96, 1200)

df$row_y<-as.integer(df$group)+((as.integer(df$type)-.5)/length(type_levels)-.5)*.72
paired<-is.finite(df$mean)&is.finite(df$error);mean_bounds<-c(df$mean[is.finite(df$mean)],df$mean[paired]-df$error[paired],df$mean[paired]+df$error[paired]);if(any(!is.finite(mean_bounds)))stop("Mean/error endpoints must be finite",call.=FALSE)
span<-diff(range(mean_bounds));if(!is.finite(span))stop("Mean/error range must be finite",call.=FALSE);if(span<=0)span<-max(abs(mean_bounds),1);mean_xlim<-range(c(0,mean_bounds))+c(-.08,.08)*span
inc_range<-range(c(0,df$incmse[is.finite(df$incmse)]));inc_span<-diff(inc_range);if(!is.finite(inc_span))stop("IncMSE range must be finite",call.=FALSE);if(inc_span==0)inc_span<-1
zero_inc<-df[!is.na(df$incmse)&df$incmse==0,,drop=FALSE];inc_xlim<-inc_range+c(if(inc_range[1]<0||nrow(zero_inc))-.05*inc_span else 0,.08*inc_span)
shared_y<-ggplot2::scale_y_continuous(breaks=seq_along(group_levels),labels=group_levels,limits=c(.5,length(group_levels)+.5),expand=c(0,0))
caption<-paste(sprintf("%d supplied rows; %d mean points / %d intervals / %d finite IncMSE entries; NA mean/error/IncMSE = %d/%d/%d.",nrow(df),sum(is.finite(df$mean)),sum(paired),sum(is.finite(df$incmse)),sum(is.na(df$mean)),sum(is.na(df$error)),sum(is.na(df$incmse))),"NA slots stay empty; IncMSE zero = open circle; intervals are supplied mean +/- error (type/n unknown).","Model, coefficient scaling, IncMSE normalization/permutations and independent sampling unverified.",sep="\n")
if(any(grepl("case[ _-]*[0-9]+|cover_[0-9]+|debug|案例[ ]*[0-9]+",c(group,type,x_title_left,x_title_right),ignore.case=TRUE)))stop("Internal case or debug labels cannot appear in figures",call.=FALSE)
common_theme <- ggplot2::theme_bw(base_size = base_size, base_family = family) +
  ggplot2::theme(
    text = ggplot2::element_text(family = family, colour = "#111111"),
    axis.text = ggplot2::element_text(size = axis_text_size, face = "bold", colour = "#111111"),
    axis.title = ggplot2::element_text(size = base_size, face = "bold", colour = "#111111"),
    panel.grid.major = ggplot2::element_line(colour = "#E4E4E4", linewidth = 0.2),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "#202020", fill = NA, linewidth = 0.35),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.margin = ggplot2::margin(3, 2, 3, 2, unit = "mm")
  )
legend_theme <- if (!show_legend || legend_position == "none") {
  ggplot2::theme(legend.position = "none")
} else if (legend_position == "inside") {
  ggplot2::theme(legend.position = "inside",legend.position.inside=c(.03,.03), legend.justification = c(0, 0))
} else {
  ggplot2::theme(legend.position = legend_position)
}
p1 <- ggplot2::ggplot(df, ggplot2::aes(x = mean, y = row_y, colour = type)) +
  ggplot2::geom_vline(xintercept = 0, colour = "#666666", linewidth = 0.4, linetype = "dashed") +
  ggplot2::geom_errorbar(ggplot2::aes(xmin = mean - error, xmax = mean + error),
                         width = errorbar_width/length(type_levels), linewidth = line_width, orientation = "y", na.rm = TRUE) +
  ggplot2::geom_point(size=point_size,shape=16,stroke=0,na.rm=TRUE) +
  ggplot2::scale_colour_manual(values = palette_map, breaks = type_levels, drop = FALSE, name = NULL) +
  ggplot2::scale_x_continuous(limits = mean_xlim, expand = c(0, 0)) +
  shared_y + ggplot2::labs(x = x_title_left, y = NULL) +
  common_theme + legend_theme +
  ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 7, face = "bold"),
                 legend.key = ggplot2::element_blank(), legend.key.height = grid::unit(3.5, "mm"),
                 legend.key.width = grid::unit(3.5, "mm"),plot.margin=ggplot2::margin(3,4,3,2,unit="mm")) + ggplot2::guides(colour=ggplot2::guide_legend(nrow=if(legend_position%in%c("top","bottom"))1 else NULL))
p2 <- ggplot2::ggplot(df, ggplot2::aes(x = incmse, y = row_y, fill = type)) +
  ggplot2::geom_col(width=bar_width/length(type_levels),orientation="y",na.rm=TRUE) +
  ggplot2::scale_fill_manual(values = palette_map, breaks = type_levels, drop = FALSE, guide = "none") +
  ggplot2::scale_x_continuous(limits = inc_xlim, expand = c(0, 0)) +
  shared_y + ggplot2::labs(x = x_title_right, y = NULL) +
  common_theme + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),plot.margin=ggplot2::margin(3,2,3,6,unit="mm"))
if(nrow(zero_inc))p2<-p2+ggplot2::geom_point(data=zero_inc,ggplot2::aes(x=incmse,y=row_y,colour=type),inherit.aes=FALSE,shape=1,size=1.3,stroke=.3,show.legend=FALSE)+ggplot2::scale_colour_manual(values=palette_map,guide="none")
width_in<-width_mm/25.4;height_in<-height_mm/25.4
render_one<-function(kind){
 path<-file.path(output_dir,paste0("plot.",kind));if(kind=="png")ragg::agg_png(path,width=width_in,height=height_in,units="in",res=dpi,background="white")else if(kind=="pdf")grDevices::pdf(path,width=width_in,height=height_in,family=if(family=="sans")"Helvetica"else family,useDingbats=FALSE,bg="white")else svglite::svglite(path,width=width_in,height=height_in,bg="white")
 tryCatch({grid::grid.newpage();panels<-cowplot::plot_grid(p1,p2,nrow=1,align="h",axis="tb",rel_widths=c(.60,.40));plot_obj<-cowplot::ggdraw()+cowplot::draw_plot(panels,x=0,y=.08,width=1,height=.92)+cowplot::draw_label(caption,x=.015,y=.01,hjust=0,vjust=0,fontfamily=family,size=6,lineheight=1.1);g<-ggplot2::ggplotGrob(plot_obj);grid::grid.draw(g);grid::grid.force();v<-grid::grid.ls(viewports=TRUE,grobs=FALSE,recursive=TRUE,print=FALSE);i<-grep("^panel[.]",v$name);depth<-lengths(strsplit(v$vpPath[i],"::",fixed=TRUE));i<-i[depth==max(depth)];if(length(i)!=2L)stop("Could not measure both data panels",call.=FALSE)
 for(j in seq_along(i)){k<-i[j];vp<-c(strsplit(v$vpPath[k],"::",fixed=TRUE)[[1]][-1],v$name[k]);grid::upViewport(0);grid::downViewport(do.call(grid::vpPath,as.list(vp)));pw<-grid::convertWidth(grid::unit(1,"npc"),"mm",valueOnly=TRUE);ph<-grid::convertHeight(grid::unit(1,"npc"),"mm",valueOnly=TRUE);slot<-ph/length(group_levels)*.72/length(type_levels)
 if(j==1L){radius<-.375*point_size*ggplot2::.pt*25.4/72;if(2*radius>slot||ph/length(group_levels)<axis_text_size*25.4/72*1.1)stop("Point or group text exceeds available row spacing",call.=FALSE);x<-(df$mean-mean_xlim[1])/diff(mean_xlim)*pw;y<-(df$row_y-.5)/length(group_levels)*ph;ok<-is.finite(df$mean);if(any(x[ok]-radius<0|x[ok]+radius>pw|y[ok]-radius<0|y[ok]+radius>ph))stop("Mean point footprint exceeds panel",call.=FALSE);lengths<-2*df$error[paired]/diff(mean_xlim)*pw*72/25.4;if(any(df$error[paired]>0&lengths<.005))stop("Nonzero interval is below vector export precision",call.=FALSE)
 }else{lengths<-abs(df$incmse)/diff(inc_xlim)*pw*72/25.4;if(any(df$incmse!=0&lengths<.005,na.rm=TRUE))stop("Nonzero IncMSE bar is below vector export precision",call.=FALSE);if(nrow(zero_inc)){radius<-.375*(1.3*ggplot2::.pt+.3*ggplot2::.stroke/2)+.75*.3*ggplot2::.stroke/4;radius<-radius*25.4/72;x<-(0-inc_xlim[1])/diff(inc_xlim)*pw;if(radius*2>slot||x-radius<0||x+radius>pw)stop("Zero marker footprint exceeds panel or row spacing",call.=FALSE)}}
 };grid::upViewport(0);for(z in strsplit(caption,"\n")[[1]])if(grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=6))),"mm",valueOnly=TRUE)>width_mm*.97)stop("Caption does not fit canvas",call.=FALSE)},finally=grDevices::dev.off())
}
for(kind in c("png","pdf","svg"))render_one(kind)
cat(sprintf("Rendered %d rows/%d groups/%d types; %d mean points/%d intervals/%d finite IncMSE (%d zero)\n",nrow(df),length(group_levels),length(type_levels),sum(is.finite(df$mean)),sum(paired),sum(is.finite(df$incmse)),nrow(zero_inc)))
