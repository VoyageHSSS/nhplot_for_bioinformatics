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
config_path <- arg_value("config", file.path(folder, "config.json"))
cfg <- if (file.exists(config_path)) jsonlite::read_json(config_path, simplifyVector = TRUE) else list()
params <- if (is.null(cfg$params)) list() else cfg$params
gp <- function(name, default) if (is.null(params[[name]])) default else params[[name]]
suppressPackageStartupMessages(library(ggplot2))
font_family <- gp("font_family","Helvetica")
table_text <- as.numeric(gp("table_text",7.5))
legend_text <- as.numeric(gp("legend_text",7))
border_width <- as.numeric(gp("border_width",.14))
width_mm <- as.numeric(gp("width_mm",190))
height_mm <- as.numeric(gp("height_mm",125))
dpi <- as.numeric(gp("dpi",300))
input <- if(is.null(params$data_path))file.path(folder,'assets/data-ASIR.txt') else normalizePath(params$data_path,mustWork=TRUE)
raw <- read.delim(input,check.names=FALSE,stringsAsFactors=FALSE)
if(!all(c('location','2019-2') %in% names(raw)) || !nrow(raw))stop('Input requires location and 2019-2 columns')
if(anyNA(raw$location)||any(!nzchar(trimws(raw$location)))||anyDuplicated(raw$location))stop('Location names must be complete and unique')
pattern <- '^\\s*([0-9.]+)\\s*\\[([0-9.]+)\\s*-\\s*([0-9.]+)\\]\\s*$'
if(anyNA(raw[['2019-2']])||any(!grepl(pattern,raw[['2019-2']])))stop('2019-2 must contain an estimate [lower-upper]')
values <- lapply(1:3,function(i)as.numeric(sub(pattern,paste0('\\',i),raw[['2019-2']])))
asir <- data.frame(location=raw$location,ASIR=values[[1]],lower=values[[2]],upper=values[[3]])
if(any(!is.finite(as.matrix(asir[-1])))||any(asir$lower<0)||any(asir$ASIR<asir$lower)||any(asir$upper<asir$ASIR))stop('ASIR estimates and bounds must be finite nonnegative and ordered')
world <- ggplot2::map_data('world');world <- world[world$region!='Antarctica',]
# Exact reviewed name aliases only; no transfer of mainland values to territories.
aliases <- c('North Korea'="Democratic People's Republic of Korea",'South Korea'='Republic of Korea',Russia='Russian Federation',USA='United States of America',Iran='Iran (Islamic Republic of)',Vietnam='Viet Nam',Laos="Lao People's Democratic Republic",Syria='Syrian Arab Republic',Moldova='Republic of Moldova','Czech Republic'='Czechia',UK='United Kingdom','Ivory Coast'="Cote d'Ivoire",'Republic of Congo'='Congo',Tanzania='United Republic of Tanzania',Venezuela='Venezuela (Bolivarian Republic of)',Bolivia='Bolivia (Plurinational State of)',Brunei='Brunei Darussalam','Cape Verde'='Cabo Verde',Micronesia='Micronesia (Federated States of)',Swaziland='Eswatini',Antigua='Antigua and Barbuda',Barbuda='Antigua and Barbuda',Trinidad='Trinidad and Tobago',Tobago='Trinidad and Tobago',Grenadines='Saint Vincent and the Grenadines','Saint Vincent'='Saint Vincent and the Grenadines')
regions <- data.frame(region=unique(world$region),stringsAsFactors=FALSE)
regions$location <- regions$region
hit <- match(regions$region,names(aliases));regions$location[!is.na(hit)]<-unname(aliases[hit[!is.na(hit)]])
i <- match(regions$location,asir$location)
regions$ASIR <- asir$ASIR[i];regions$matched <- !is.na(i)
world$ASIR <- regions$ASIR[match(world$region,regions$region)]
unmapped_input <- asir[!asir$location %in% regions$location,]
max_value <- max(52,ceiling(max(asir$ASIR)))
palette_cols <- c('#5EA9F3','#91B7BC','#D9D476','#FF9A46','#FF1A0A')
map_plot <- ggplot(world,aes(long,lat,group=group,fill=ASIR))+
 geom_polygon(colour='#353535',linewidth=border_width)+
 scale_fill_gradientn(colours=palette_cols,limits=c(0,max_value),na.value='#D8D8D8',guide='none')+
 annotate('segment',x=-167,xend=-167,y=68,yend=80,linewidth=.3,arrow=grid::arrow(length=grid::unit(1.4,'mm'),type='closed'))+
 annotate('text',x=-167,y=85,label='N',family=font_family,size=legend_text/ggplot2::.pt)+
 coord_fixed(ratio=1.12,xlim=c(-180,180),ylim=c(-60,90),expand=FALSE)+
 theme_void(base_family=font_family)+theme(plot.margin=margin(0,0,0,0))
selected <- c('India','Bhutan','Bangladesh','United States of America','Australia','Poland')
table_data <- asir[asir$location %in% selected,c('location','ASIR')]
table_data <- table_data[order(-table_data$ASIR),]
table_display <- table_data;table_display$location[table_display$location=='United States of America']<-'United States'
names(table_display)<-c('Location','ASIR');table_display$ASIR<-sprintf('%.2f',table_display$ASIR)
table_grob <- gridExtra::tableGrob(table_display,rows=NULL,theme=gridExtra::ttheme_minimal(base_size=table_text,base_family=font_family,padding=grid::unit(c(1.5,2),'mm'),core=list(bg_params=list(fill=rep(c('#DBE7F4','#F1F5FA'),length.out=nrow(table_display)),col=NA),fg_params=list(col='#202020')),colhead=list(bg_params=list(fill='#4C82BD',col=NA),fg_params=list(col='white',fontface='bold'))))
legend_plot <- ggplot(data.frame(x=c(0,max_value),y=0),aes(x,y,fill=x))+geom_tile(alpha=0)+
 scale_fill_gradientn(colours=palette_cols,limits=c(0,max_value),breaks=unique(c(0,seq(10,max_value-5,10),max_value)),name='ASIR (supplied values)',guide=guide_colourbar(display='rectangles',direction='horizontal',barwidth=grid::unit(77,'mm'),barheight=grid::unit(3,'mm'),nbin=101))+
 theme_void(base_family=font_family)+theme(legend.position='bottom',legend.title=element_text(size=legend_text),legend.text=element_text(size=legend_text),legend.title.position='top',legend.margin=margin(0,0,0,0),plot.margin=margin(0,0,0,0))
legend_grob <- cowplot::get_legend(legend_plot)
p <- cowplot::ggdraw()+cowplot::draw_plot(map_plot,x=.01,y=.17,width=.98,height=.80)+
 cowplot::draw_grob(table_grob,x=.015,y=.145,width=.28,height=.27)+
 cowplot::draw_grob(legend_grob,x=.34,y=.095,width=.47,height=.11)+
 cowplot::draw_label('2019',x=.51,y=.255,size=13,fontfamily=font_family,colour='#555555')+
 cowplot::draw_label('No matched data',x=.915,y=.15,size=legend_text,fontfamily=font_family)+
 cowplot::draw_grob(grid::rectGrob(gp=grid::gpar(fill='#D8D8D8',col='#454545',lwd=.4)),x=.837,y=.139,width=.018,height=.022)+
 cowplot::draw_label(paste0(nrow(asir),' supplied estimates; ',nrow(asir)-nrow(unmapped_input),' matched locations across ',sum(regions$matched),' mapped regions.\nLongitude/latitude schematic; Antarctica excluded. Interval bounds retained in companion table, not encoded in fill.\nDisease, units, standard population and provenance are unconfirmed; no area-based comparison.'),x=.5,y=.044,size=6.5,fontfamily=font_family,hjust=.5,lineheight=1.05)
output_dir <- arg_value('output-dir',file.path(folder,'runtime/output'));dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
for(ext in c('png','pdf','svg'))ggsave(file.path(output_dir,paste0('plot.',ext)),p,width=width_mm,height=height_mm,units='mm',dpi=dpi,device=switch(ext,png=ragg::agg_png,pdf=grDevices::pdf,svg=svglite::svglite),bg='white')
write.table(asir,file.path(output_dir,'estimates_with_bounds.tsv'),sep='\t',row.names=FALSE,quote=FALSE)
write.table(regions,file.path(output_dir,'region_matching.tsv'),sep='\t',row.names=FALSE,quote=FALSE)
write.table(unmapped_input,file.path(output_dir,'unmapped_input.tsv'),sep='\t',row.names=FALSE,quote=FALSE)
