#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  flag <- paste0("--", name)
  inline <- args[startsWith(args, paste0(flag, "="))]
  if (length(inline)) return(sub(paste0(flag, "="), "", inline[[1]], fixed = TRUE))
  index <- match(flag, args)
  if (!is.na(index) && index < length(args)) return(args[[index + 1L]])
  default
}
script <- if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) sys.frame(1)$ofile else
  sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
folder <- dirname(normalizePath(script))
color_module <- Sys.getenv("PLOT_NH_COLOR_MODULE", "")
if (!file.exists(color_module)) {
  root <- folder
  while (!file.exists(file.path(root, "manual_library/scripts/plot_nh_colors.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Shared color module not found")
    root <- parent
  }
  color_module <- file.path(root, "manual_library/scripts/plot_nh_colors.R")
}
source(color_module)
plot_nh_color_context()
source(file.path(folder, "source.R"))
config_path <- arg_value("config", file.path(folder, "config.json"))
cfg <- if (file.exists(config_path)) jsonlite::read_json(config_path, simplifyVector = TRUE) else list()
params <- if (is.null(cfg$params)) list() else cfg$params
gp <- function(name, default) if (is.null(params[[name]])) default else params[[name]]
font_family <- gp("font_family","Helvetica")
base_size <- as.numeric(gp("base_size",8))
axis_label_size <- as.numeric(gp("axis_label_size",2.7))
scale_label_size <- as.numeric(gp("scale_label_size",2.1))
title_size <- as.numeric(gp("title_size",8.5))
note_title_size <- as.numeric(gp("note_title_size",2.8))
note_text_size <- as.numeric(gp("note_text_size",2.4))
fill_alpha <- as.numeric(gp("fill_alpha",0.18))
line_width <- as.numeric(gp("line_width",0.5))
point_size <- as.numeric(gp("point_size",0.9))
grid_width <- as.numeric(gp("grid_width",0.18))
plot_margin <- as.numeric(gp("plot_margin",1))
radar_height <- as.numeric(gp("radar_height",1))
notes_height <- as.numeric(gp("notes_height",0.38))
width_mm <- as.numeric(gp("width_mm",180))
height_mm <- as.numeric(gp("height_mm",105))
dpi <- as.numeric(gp("dpi",300))
group_cols <- c(`PG3:High`=gp("pg3_color","#E6A04C"),
 `PG2:Low`=gp("pg2_color","#63A8C5"),`PG1:Low`=gp("pg1_color","#5BAA78"))
seed <- if(is.null(cfg$seed)) 20260908L else as.integer(cfg$seed)
d <- simulate_data(seed)
theta <- 3*pi/4-(seq_along(dimensions)-1)*2*pi/length(dimensions)
spokes <- data.frame(x=0,y=0,xend=cos(theta),yend=sin(theta))
rings <- do.call(rbind,lapply(seq(.2,1,.2),function(v) {
 a <- seq(0,2*pi,length.out=181)
 data.frame(level=v,x=v*cos(a),y=v*sin(a))
}))
axis_labels <- data.frame(x=1.12*cos(theta),y=1.12*sin(theta),label=dimensions)
scale_labels <- data.frame(x=0.035,y=seq(.2,1,.2),label=format(seq(.2,1,.2),nsmall=1))
stopifnot(all(d$value>=0&d$value<=1),fill_alpha>=0,fill_alpha<=1,line_width>0)
draw_radar <- function(g) {
 poly <- d[d$group==g,]
 poly$x <- poly$value*cos(theta[as.integer(poly$dimension)])
 poly$y <- poly$value*sin(theta[as.integer(poly$dimension)])
 closed <- rbind(poly,poly[1,])
 colour <- unname(group_cols[g])
 ggplot2::ggplot() +
  ggplot2::geom_path(data=rings,ggplot2::aes(x,y,group=level),colour="#D5DADF",linewidth=grid_width) +
  ggplot2::geom_segment(data=spokes,ggplot2::aes(x,y,xend=xend,yend=yend),colour="#D5DADF",linewidth=grid_width) +
  ggplot2::geom_polygon(data=closed,ggplot2::aes(x,y),fill=colour,alpha=fill_alpha,colour=NA) +
  ggplot2::geom_path(data=closed,ggplot2::aes(x,y),colour=colour,linewidth=line_width) +
  ggplot2::geom_point(data=poly,ggplot2::aes(x,y),colour=colour,size=point_size) +
  ggplot2::geom_text(data=axis_labels,ggplot2::aes(x,y,label=label),family=font_family,size=axis_label_size,colour="#333333") +
  ggplot2::geom_text(data=scale_labels,ggplot2::aes(x,y,label=label),family=font_family,size=scale_label_size,
   colour="#777777",hjust=0) +
  ggplot2::coord_fixed(ratio=1,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2),expand=FALSE,clip="off") +
  ggplot2::labs(title=g) +
  ggplot2::theme_void(base_family=font_family,base_size=base_size) +
  ggplot2::theme(plot.title=ggplot2::element_text(size=title_size,hjust=.5,colour="#333333"),
   plot.margin=ggplot2::margin(plot_margin,plot_margin,plot_margin,plot_margin,"mm"))
}
panels <- lapply(groups,draw_radar)
note_panel <- function(title,text,fill) {
 ggplot2::ggplot() +
  ggplot2::annotate("rect",xmin=0,xmax=1,ymin=0,ymax=1,fill=fill,colour=NA) +
  ggplot2::annotate("text",x=.04,y=.87,label=title,hjust=0,size=note_title_size,family=font_family,colour="#333333") +
  ggplot2::annotate("text",x=.04,y=.42,label=text,hjust=0,size=note_text_size,
   family=font_family,lineheight=1.05,colour="#444444") +
  ggplot2::coord_cartesian(xlim=c(0,1),ylim=c(0,1),expand=FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.margin=ggplot2::margin(1,2,1,2,"mm"))
}
notes <- list(note_panel("Peptide antigen","A: Anchor hydrophobicity\nB: Charge   C: Bulkiness   D: Hydrophobicity\nB-D: Exposed residues","#F1F8FA"),
 note_panel("TCR beta","E: TCR beta length\nF: Charge   G: Bulkiness   H: Hydrophobicity\nF-H: CDR3 beta","#F4F5F5"))
radars <- patchwork::wrap_plots(panels,nrow=1,widths=c(1,1,1))
note_row <- patchwork::wrap_plots(notes,nrow=1,widths=c(1,1))
p <- patchwork::wrap_plots(radars,note_row,ncol=1,heights=c(radar_height,notes_height)) +
 patchwork::plot_annotation(caption="Simulated profiles on a common 0-1 scale; no measured peptide or TCR properties.",
  theme=ggplot2::theme(plot.caption=ggplot2::element_text(size=6.5,hjust=0,family=font_family)))
od <- arg_value("output-dir",file.path(folder,"runtime/output"))
dir.create(od,recursive=TRUE,showWarnings=FALSE)
ggplot2::ggsave(file.path(od,"plot.png"),p,width=width_mm,height=height_mm,units="mm",dpi=dpi,device=ragg::agg_png,bg="white")
ggplot2::ggsave(file.path(od,"plot.pdf"),p,width=width_mm,height=height_mm,units="mm",device=grDevices::pdf,family=font_family,bg="white")
ggplot2::ggsave(file.path(od,"plot.svg"),p,width=width_mm,height=height_mm,units="mm",device=svglite::svglite,bg="white")
