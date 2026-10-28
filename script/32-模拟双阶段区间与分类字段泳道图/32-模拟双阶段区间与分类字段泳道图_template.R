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
suppressPackageStartupMessages({library(ggplot2);library(patchwork)})
font_family <- gp("font_family","Helvetica")
axis_text <- as.numeric(gp("axis_text",7))
axis_title <- as.numeric(gp("axis_title",8))
legend_text <- as.numeric(gp("legend_text",7))
panel_title <- as.numeric(gp("panel_title",8.5))
phase_width <- as.numeric(gp("phase_width",1.7))
point_size <- as.numeric(gp("point_size",1.35))
margin_mm <- as.numeric(gp("margin_mm",2))
width_mm <- as.numeric(gp("width_mm",210))
height_mm <- as.numeric(gp("height_mm",175))
dpi <- as.numeric(gp("dpi",300))
original <- build_plot(20260909L)
raw_tiles <- original[[1]]$layers[[1]]$data
raw_segments <- original[[1]]$layers[[2]]$data
raw_marks <- original[[1]]$layers[[3]]$data
tiles <- raw_tiles
colours <- c("#9BCEB5","#E28FAA","#D3D5E9","#F1B3C3","#EFC46F")
tiles$level <- factor(letters[match(tiles$fill,colours)],levels=letters[1:5])
segments <- raw_segments
segments$kind <- factor(rep(c("Interval","Phase A","Phase B"),each=34),levels=c("Interval","Phase A","Phase B"))
segments$width <- ifelse(segments$kind=="Interval",phase_width+.7,phase_width)
marks <- raw_marks
ylimit <- c(.25,34.75)
common <- theme_classic(base_family=font_family)+theme(axis.text=element_text(size=axis_text,colour="#222222"),
 axis.title=element_text(size=axis_title),axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.2),
 legend.text=element_text(size=legend_text),legend.title=element_text(size=legend_text),legend.key.size=grid::unit(3,"mm"),
 plot.title=element_text(size=panel_title,face="bold"),plot.margin=margin(margin_mm,margin_mm,margin_mm,margin_mm,"mm"))
A <- ggplot(tiles,aes(x,y,fill=level))+
 geom_tile(width=.8,height=.72,colour="#777777",linewidth=.12)+
 geom_text(aes(label=level),size=2.2,family=font_family,colour="#222222")+
 scale_fill_manual(values=setNames(colours,letters[1:5]),breaks=letters[1:5],labels=paste("Level",letters[1:5]),name="Fields",guide=guide_legend(nrow=1,order=3))+
 scale_x_continuous(breaks=1:5,labels=paste0("F",1:5),position="top",expand=c(0,0))+
 scale_y_continuous(breaks=1:34,labels=sprintf("R%02d",1:34),expand=c(0,0))+
 coord_cartesian(xlim=c(.5,5.5),ylim=ylimit,expand=FALSE)+labs(x=NULL,y=NULL,title="A  Arbitrary fields")+common+
 theme(axis.line=element_blank(),axis.ticks=element_blank())
B <- ggplot()+
 geom_segment(data=segments,aes(x,y,xend=xend,yend=y,colour=kind,linewidth=width),lineend="butt")+
 geom_point(data=marks,aes(x,y,shape=mark),size=point_size,colour="#333333",fill="white",stroke=.3)+
 scale_colour_manual(values=c(Interval="#D1D1D1",`Phase A`="#ED6C70",`Phase B`="#678EBF"),breaks=c("Interval","Phase A","Phase B"),name="Segments",guide=guide_legend(nrow=1,order=1,override.aes=list(linewidth=1.2)))+
 scale_linewidth_identity()+scale_shape_manual(values=c(open=21,filled=16),breaks=c("open","filled"),labels=c("Event A","Event B"),name="Events",guide=guide_legend(nrow=1,order=2))+
 scale_x_continuous(breaks=c(0,20,40,60,80),expand=c(0,0))+
 scale_y_continuous(breaks=1:34,expand=c(0,0))+
 coord_cartesian(xlim=c(0,max(segments$xend)+4),ylim=ylimit,expand=FALSE)+
 labs(x="Simulated time (a.u.)",y=NULL,title="B  Simulated intervals")+common+
 theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
p <- wrap_plots(A,B,ncol=2,widths=c(.27,1),guides="collect")+
 plot_annotation(caption="34 simulated rows. Two contiguous phases span each interval; open/filled symbols are arbitrary simulated events.\nFive categorical fields use the same five source levels. No clinical response, drug identity or stopping status is inferred.",
 theme=theme(plot.caption=element_text(size=7,hjust=.5,family=font_family),plot.margin=margin(2,2,2,2,"mm")))
p <- p & theme(legend.position="bottom",legend.box="vertical",legend.spacing=grid::unit(1,"mm"),
 legend.margin=margin(0,0,0,0),legend.box.spacing=grid::unit(1,"mm"))
output_dir <- arg_value("output-dir",file.path(folder,"runtime/output"))
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
for(ext in c("png","pdf","svg")){
 device<-switch(ext,png=ragg::agg_png,pdf=grDevices::pdf,svg=svglite::svglite)
 ggsave(file.path(output_dir,paste0("plot.",ext)),p,width=width_mm,height=height_mm,units="mm",dpi=dpi,device=device,bg="white")
}
