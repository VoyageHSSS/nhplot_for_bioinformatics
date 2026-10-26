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
suppressPackageStartupMessages(library(ggplot2))
font_family <- gp("font_family","Helvetica")
label_size <- as.numeric(gp("label_size",2.4))
hub_size <- as.numeric(gp("hub_size",2.5))
edge_alpha <- as.numeric(gp("edge_alpha",.55))
edge_width <- as.numeric(gp("edge_width",.2))
margin_mm <- as.numeric(gp("margin_mm",2))
width_mm <- as.numeric(gp("width_mm",220))
height_mm <- as.numeric(gp("height_mm",225))
dpi <- as.numeric(gp("dpi",300))
net <- simulate_network(20260908L)
leaves <- net$leaves
leaves$value <- leaves$outer-leaves$inner
leaves$group_id <- match(as.character(leaves$category),categories)
leaves$id <- seq_len(nrow(leaves))
leaves$display_label <- paste0(LETTERS[leaves$group_id],sprintf("%02d",ave(leaves$id,leaves$group_id,FUN=seq_along)))
# Keep the raw table intact; only the rendered radial baseline is standardized.
render <- leaves
render$inner <- .9;render$outer <- .9+render$value
render$x <- .9*cos(render$theta);render$y <- .9*sin(render$theta)
render$xend <- render$outer*cos(render$theta);render$yend <- render$outer*sin(render$theta)
label_theta <- unlist(lapply(seq_len(10),function(i)net$centers$theta[i]+seq(-.24,.24,length.out=sum(render$group_id==i))))
render$lx <- 1.36*cos(label_theta);render$ly <- 1.36*sin(label_theta)
render$hjust <- ifelse(cos(label_theta)>=0,0,1)
render$label_angle <- ifelse(cos(label_theta)>=0,label_theta*180/pi,label_theta*180/pi+180)
centers <- net$centers
centers$display_label <- paste0("Group ",LETTERS[seq_len(nrow(centers))],"\nn=",as.integer(table(leaves$category)))
centers$lx <- .47*cos(centers$theta);centers$ly <- .47*sin(centers$theta)
connections <- data.frame(category=render$category,x=centers$x[render$group_id],y=centers$y[render$group_id],xend=render$x,yend=render$y)
bars <- make_bar_polygons(render,width=.012)
ring <- data.frame(theta=seq(0,2*pi,length.out=361));ring$x<-.9*cos(ring$theta);ring$y<-.9*sin(ring$theta)
leaders <- data.frame(x=(render$outer+.015)*cos(render$theta),y=(render$outer+.015)*sin(render$theta),xend=1.335*cos(label_theta),yend=1.335*sin(label_theta))
p <- ggplot()+geom_path(data=ring,aes(x,y),colour="#CCCCCC",linewidth=.15)+
 geom_curve(data=net$center_edges,aes(x,y,xend=xend,yend=yend,colour=category),curvature=.1,linewidth=edge_width,alpha=edge_alpha)+
 geom_curve(data=connections,aes(x,y,xend=xend,yend=yend,colour=category),curvature=.10,linewidth=edge_width,alpha=edge_alpha)+
 geom_polygon(data=bars,aes(x,y,group=id,fill=category),colour=NA,alpha=.95)+
 geom_segment(data=leaders,aes(x,y,xend=xend,yend=yend),colour="#BBBBBB",linewidth=.10)+
 geom_point(data=centers,aes(x,y,fill=category),shape=21,colour="white",size=2.6,stroke=.2)+
 annotate("point",x=0,y=0,size=1.5,colour="#555555")+
 geom_label(data=centers,aes(lx,ly,label=display_label),fill="white",colour="#333333",linewidth=0,size=hub_size,family=font_family,lineheight=.95,label.padding=grid::unit(.35,"mm"))+
 geom_text(data=render,aes(lx,ly,label=display_label,hjust=hjust,angle=label_angle),colour="#333333",size=label_size,family=font_family)+
 annotate("segment",x=-1.32,xend=-1.12,y=-1.46,yend=-1.46,linewidth=.5,colour="#555555")+
 annotate("segment",x=c(-1.32,-1.12),xend=c(-1.32,-1.12),y=-1.44,yend=-1.48,linewidth=.2,colour="#555555")+
 annotate("text",x=-1.22,y=-1.55,label="0.2 simulated length units",size=2.4,family=font_family)+
 scale_colour_manual(values=cat_cols,guide="none")+scale_fill_manual(values=cat_cols,guide="none")+
 coord_equal(xlim=c(-1.62,1.62),ylim=c(-1.62,1.62),expand=FALSE,clip="on")+
 labs(caption="Simulated grouped star topology: 10 groups, 88 unique leaf identifiers.\nBar length encodes a simulated value from a common radius; edges are unweighted membership links.")+
 theme_void(base_family=font_family)+theme(plot.margin=margin(margin_mm,margin_mm,margin_mm,margin_mm,"mm"),plot.caption=element_text(size=7,hjust=.5,margin=margin(t=4)))
output_dir <- arg_value("output-dir",file.path(folder,"runtime/output"))
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
for(ext in c("png","pdf","svg")){
 device <- switch(ext,png=ragg::agg_png,pdf=grDevices::pdf,svg=svglite::svglite)
 ggsave(file.path(output_dir,paste0("plot.",ext)),p,width=width_mm,height=height_mm,units="mm",dpi=dpi,device=device,bg="white")
}
