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
panel_title <- as.numeric(gp("panel_title",8.5))
ridge_height <- as.numeric(gp("ridge_height",.9))
line_width <- as.numeric(gp("line_width",.22))
margin_mm <- as.numeric(gp("margin_mm",2))
width_mm <- as.numeric(gp("width_mm",190))
height_mm <- as.numeric(gp("height_mm",180))
dpi <- as.numeric(gp("dpi",300))
original <- list(make_ridge(20260910,FALSE,"year"),make_ridge(20260911,FALSE,"jif"),
 make_ridge(20260912,TRUE,"year"),make_ridge(20260913,TRUE,"jif"))
raw <- lapply(original,function(z)z$data)
density_tables <- polygons <- labels <- bandwidths <- vector("list",4)
ridge_plot <- function(i,title,hide_y=FALSE){
 d <- raw[[i]];n <- length(levels(d$region));d$row <- as.integer(d$region)
 year <- i%%2==1
 limits <- if(year)c(1975,2030) else c(-15,110)
 bw <- bw.nrd0(d$x);bandwidths[[i]] <<- bw
 dens <- do.call(rbind,lapply(seq_len(n),function(j){
  v <- d$x[d$row==j];z <- density(v,bw=bw,from=limits[1],to=limits[2],n=256)
  data.frame(row=j,region=levels(d$region)[j],x=z$x,density=z$y,n=length(v))
 }))
 density_tables[[i]] <<- dens
 peak <- max(dens$density)
 poly <- do.call(rbind,lapply(seq_len(n),function(j){
  z <- dens[dens$row==j,]
  rbind(data.frame(row=j,region=z$region[1],x=limits[1],y=j),
  data.frame(row=j,region=z$region,x=z$x,y=j+ridge_height*z$density/peak),
  data.frame(row=j,region=z$region[1],x=limits[2],y=j))
 }))
 polygons[[i]] <<- poly
 lab <- paste0("Group ",rev(LETTERS[seq_len(n)])," (n=",as.integer(table(d$region)),")")
 labels[[i]] <<- lab
 outline <- dens[dens$density/peak>=.01,]
 outline$y <- outline$row+ridge_height*outline$density/peak
 # Source palette is reversed so the top group receives the first reference colour.
 pal <- original[[i]]$scales$get_scales("fill")$palette(n)
 pal <- setNames(rev(unname(pal)),levels(d$region))
 p <- ggplot()+geom_hline(yintercept=seq_len(n),colour="#D0D0D0",linetype="dotted",linewidth=.15)+
 geom_polygon(data=poly,aes(x,y,group=row,fill=region),colour=NA,alpha=.9)+
 geom_path(data=outline,aes(x,y,group=row),colour="#666666",linewidth=line_width)+
 geom_segment(data=d,aes(x=x,xend=x,y=row,yend=row-.09),colour="#444444",linewidth=.14,alpha=.4)+
 scale_fill_manual(values=pal,guide="none")+
 scale_x_continuous(breaks=if(year)c(1980,2000,2020) else c(0,50,100),expand=c(0,0))+
 scale_y_continuous(breaks=seq_len(n),labels=lab,expand=c(0,0))+
 coord_cartesian(xlim=limits,ylim=c(.75,n+1),expand=FALSE)+
 labs(x=if(year)"Simulated year" else "Simulated score",y=NULL,title=title)+
 theme_classic(base_family=font_family)+theme(axis.text=element_text(size=axis_text,colour="#222222"),
 axis.title=element_text(size=axis_title),axis.ticks.y=element_blank(),axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.2),
 plot.title=element_text(size=panel_title,face="bold"),plot.margin=margin(margin_mm,margin_mm,margin_mm,margin_mm,"mm"))
 if(hide_y)p<-p+theme(axis.text.y=element_blank(),axis.line.y=element_blank())
 p
}
A <- ridge_plot(1,"A  Set 1: simulated year")
B <- ridge_plot(2,"B  Set 1: simulated score",TRUE)
C <- ridge_plot(3,"C  Set 2: simulated year")
D <- ridge_plot(4,"D  Set 2: simulated score",TRUE)
p <- wrap_plots(A,B,C,D,ncol=2,widths=c(1,1),heights=c(9,12))+
 plot_annotation(caption="Four independent simulated datasets. Counts are actual generated observations; the source generator enforces n>=6.\nGaussian KDE uses one pooled-data bandwidth per panel and one common peak scaling. Rugs belong to each group; scores are not JIF percentiles.",
 theme=theme(plot.caption=element_text(size=7,hjust=.5,family=font_family),plot.margin=margin(2,2,2,2,"mm")))
output_dir <- arg_value("output-dir",file.path(folder,"runtime/output"))
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
for(ext in c("png","pdf","svg")){
 device <- switch(ext,png=ragg::agg_png,pdf=grDevices::pdf,svg=svglite::svglite)
 ggsave(file.path(output_dir,paste0("plot.",ext)),p,width=width_mm,height=height_mm,units="mm",dpi=dpi,device=device,bg="white")
}
