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
axis_text_size <- as.numeric(gp("axis_text_size",7))
axis_title_size <- as.numeric(gp("axis_title_size",8))
legend_text_size <- as.numeric(gp("legend_text_size",6.5))
group_a_color <- gp("group_a_color","#5E8FB4")
group_b_color <- gp("group_b_color","#D35E58")
hist_alpha <- as.numeric(gp("hist_alpha",0.5))
hist_linewidth <- as.numeric(gp("hist_linewidth",0.15))
density_linewidth <- as.numeric(gp("density_linewidth",0.45))
axis_linewidth <- as.numeric(gp("axis_linewidth",0.35))
bin_width <- as.numeric(gp("bin_width",1))
marginal_ratio <- as.numeric(gp("marginal_ratio",0.24))
plot_margin <- as.numeric(gp("plot_margin",1.5))
seed <- if (is.null(cfg$seed)) 20260908L else as.integer(cfg$seed)
dat <- simulate_data(seed)
limits <- c(30,80)
breaks <- seq(30,80,bin_width)
palette <- c(GroupA=group_a_color,GroupB=group_b_color)
common <- theme_classic(base_size=8,base_family=font_family)+
 theme(axis.title=element_text(size=axis_title_size),axis.text=element_text(size=axis_text_size,colour="#333333"),
 axis.line=element_line(linewidth=axis_linewidth),axis.ticks=element_line(linewidth=axis_linewidth),
 plot.margin=margin(plot_margin,plot_margin,plot_margin,plot_margin,"mm"))
top <- ggplot(dat,aes(X_lab,fill=group,colour=group))+
 geom_histogram(aes(y=after_stat(density)),breaks=breaks,position="identity",
  alpha=hist_alpha,linewidth=hist_linewidth)+
 geom_density(linewidth=density_linewidth,adjust=.9,fill=NA)+
 scale_fill_manual(values=palette,guide="none")+scale_colour_manual(values=palette,guide="none")+
 scale_x_continuous(limits=limits,breaks=seq(30,80,10),expand=c(0,0))+
 scale_y_continuous(breaks=c(0,.1),expand=expansion(mult=c(0,.08)))+
 labs(x=NULL,y="Density")+common+
 theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
density_guide <- function(order) guide_colourbar(order=order,direction="horizontal",title.position="top",
 barwidth=grid::unit(20,"mm"),barheight=grid::unit(2,"mm"),display="rectangles")
main <- ggplot()+
 stat_density_2d(data=subset(dat,group=="GroupA"),aes(X_lab,Y_lab,fill=after_stat(level)),
 contour_var="ndensity",geom="polygon",breaks=seq(.1,.9,.1),alpha=.8,colour=NA)+
 scale_fill_gradientn(colours=c("#FFFFFF","#B8D6E5",group_a_color),limits=c(0,1),breaks=c(0,.5,1),
 name="Group A",guide=density_guide(1))+
 ggnewscale::new_scale_fill()+
 stat_density_2d(data=subset(dat,group=="GroupB"),aes(X_lab,Y_lab,fill=after_stat(level)),
 contour_var="ndensity",geom="polygon",breaks=seq(.1,.9,.1),alpha=.8,colour=NA)+
 scale_fill_gradientn(colours=c("#FFFFFF","#F1B8AD",group_b_color),limits=c(0,1),breaks=c(0,.5,1),
 name="Group B",guide=density_guide(2))+
 scale_x_continuous(limits=limits,breaks=seq(30,80,10),expand=c(0,0))+
 scale_y_continuous(limits=limits,breaks=seq(30,80,10),expand=c(0,0))+
 coord_fixed(ratio=1,expand=FALSE)+labs(x="Simulated X",y="Simulated Y")+common+
 theme(panel.border=element_rect(fill=NA,colour="#333333",linewidth=axis_linewidth),
 legend.position="right",legend.box="vertical",legend.title=element_text(size=legend_text_size),
 legend.text=element_text(size=legend_text_size),legend.margin=margin(0,0,0,0),
 legend.spacing.y=grid::unit(2,"mm"))
legend <- cowplot::get_legend(main)
main <- main+theme(legend.position="none")
right <- ggplot(dat,aes(y=Y_lab,fill=group,colour=group))+
 geom_histogram(aes(x=after_stat(density)),orientation="y",breaks=breaks,
 position="identity",alpha=hist_alpha,linewidth=hist_linewidth)+
 scale_fill_manual(values=palette,guide="none")+scale_colour_manual(values=palette,guide="none")+
 scale_y_continuous(limits=limits,breaks=seq(30,80,10),expand=c(0,0))+
 scale_x_continuous(limits=c(0,.11),breaks=c(0,.1),expand=c(0,0))+
 labs(x="Density",y=NULL)+common+
 theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
p <- patchwork::wrap_plots(A=top,B=patchwork::wrap_elements(full=legend),C=main,D=right,
 design="AB\nCD",widths=c(1,marginal_ratio),heights=c(marginal_ratio,1))+
 patchwork::plot_annotation(caption="Simulated observations. Histograms: per-group density.\nKDE shading: contour density / group peak (levels 0.1-0.9).",
 theme=theme(plot.caption=element_text(size=7,family=font_family,hjust=0),plot.margin=margin(2,2,2,2,"mm")))
output_dir <- arg_value("output-dir",file.path(folder,"runtime/output"))
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
width_mm <- as.numeric(gp("width_mm",180))
height_mm <- as.numeric(gp("height_mm",185))
dpi <- as.numeric(gp("dpi",300))
ggsave(file.path(output_dir,"plot.png"),p,width=width_mm,height=height_mm,units="mm",dpi=dpi,device=ragg::agg_png,bg="white")
ggsave(file.path(output_dir,"plot.pdf"),p,width=width_mm,height=height_mm,units="mm",device=grDevices::pdf,family=font_family,bg="white")
ggsave(file.path(output_dir,"plot.svg"),p,width=width_mm,height=height_mm,units="mm",device=svglite::svglite,bg="white")
