#!/usr/bin/env Rscript
# Panel E reconstruction: explicit taxonomy with curved branches and annotation rings.
options(device=function(...) stop('Implicit device prohibited'))
suppressPackageStartupMessages(library(ggplot2))
`%||%` <- function(x,y) if(is.null(x)) y else x
args <- commandArgs(trailingOnly=TRUE)
arg <- function(key, default=NULL) {
  flag <- paste0('--',key); i <- match(flag,args)
  if(!is.na(i)) { if(i==length(args)) stop('Missing ',flag); return(args[i+1]) }
  inline <- grep(paste0('^',flag,'='),args,value=TRUE)
  if(length(inline)) sub(paste0('^',flag,'='),'',inline[1]) else default
}
script_dir <- dirname(normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])))
color_module <- Sys.getenv('PLOT_NH_COLOR_MODULE','')
base <- script_dir
for(i in 1:12) {
  if(nzchar(color_module) && file.exists(color_module)) break
  candidate <- file.path(base,'manual_library/scripts/plot_nh_colors.R')
  if(file.exists(candidate)) color_module <- candidate
  base <- dirname(base)
}
if(!file.exists(color_module)) stop('Set PLOT_NH_COLOR_MODULE to the shared color module')
source(color_module); invisible(plot_nh_color_context())
cfg <- jsonlite::fromJSON(arg('config'),simplifyVector=FALSE)
if(!is.list(cfg) || (!is.null(cfg$params) && !is.list(cfg$params))) stop('Invalid config object')
params <- cfg$params %||% list()
gp <- function(name,default=NULL) params[[name]] %||% default
num <- function(x,lo,hi) {
  x <- suppressWarnings(as.numeric(x))
  if(length(x)!=1 || !is.finite(x) || x<lo || x>hi) stop('Numeric parameter out of range')
  x
}
hex <- function(x) {x<-unlist(x,use.names=FALSE);if(!length(x)||anyNA(x)||any(!grepl('^#[0-9a-fA-F]{6}$',x)))stop('Colors must be #RRGGBB');x}
width_mm <- num(gp('width_mm',270),220,400); height_mm <- num(gp('height_mm',265),220,400)
dpi <- num(gp('dpi',300),96,600)
label_size <- num(gp('label_size',7.8),6,10)/2.8453
branch_label_size <- num(gp('branch_label_size',6.5),5,9)/2.8453
lineage_size <- num(gp('lineage_size',10),8,13)/2.8453
legend_size <- num(gp('legend_size',6.5),5,9)/2.8453
branch_scale <- num(gp('branch_scale',1),.65,1.5)
ring_width <- num(gp('ring_width',.041),.03,.05)
family <- gp('font_family','Arial'); if(!family %in% c('Arial','Helvetica','sans')) stop('Unknown font')
font_path <- systemfonts::match_fonts(family)
bold_path <- systemfonts::match_fonts(family,weight='bold')
if(font_path$index!=0 || bold_path$index!=0) stop('Select Arial for an addressable font face')
sysfonts::font_add('panel_font',regular=font_path$path,bold=bold_path$path)
family <- 'panel_font'
showtext::showtext_opts(dpi=dpi)
showtext::showtext_auto()
ink <- hex(gp('text_color',plot_nh_colors$surface$ink))
muted <- hex(gp('annotation_color',plot_nh_colors$surface$muted))
background <- hex(gp('background_color',plot_nh_colors$surface$paper))
div_default <- plot_nh_palette_diverging(n=3,style='blue_red')
heat <- c(hex(gp('heat_low',div_default[1])),hex(gp('heat_mid',div_default[2])),hex(gp('heat_high',div_default[3])))
prop_high <- hex(gp('proportion_color','#555B61'))
groups <- c('ILC','B cell','Myeloid','CD4 T','Other T')
core <- plot_nh_palette_categorical(style='core')
original <- unname(c(core['purple'],core['vermillion'],core['teal'],core['blue'],plot_nh_colors$tol_muted['cyan']))
cols <- gp('palette_colors',original)
if(is.character(cols) && length(cols)==1) cols <- trimws(strsplit(cols,'[,;]')[[1]])
mapping <- unlist(cfg$palette_mapping)
if(!identical(cfg$palette_source %||% 'original','original') && all(groups %in% names(mapping))) cols <- mapping[groups]
cols <- hex(cols); if(length(cols)!=5) stop('Exactly five lineage colors required')
names(cols) <- groups
blend <- function(a,b,t) grDevices::rgb((1-t)*col2rgb(a)[1,]/255+t*col2rgb(b)[1,]/255,
  (1-t)*col2rgb(a)[2,]/255+t*col2rgb(b)[2,]/255,(1-t)*col2rgb(a)[3,]/255+t*col2rgb(b)[3,]/255)
# Darker direct labels preserve category identity and sufficient contrast on paper.
contrast_ink <- if(plot_nh_contrast_ratio('#000000',background)>plot_nh_contrast_ratio('#FFFFFF',background)) '#000000' else '#FFFFFF'
label_cols <- vapply(cols,function(c){while(plot_nh_contrast_ratio(c,background)<4.5)c<-blend(c,contrast_ink,.12);c},character(1))
data_path <- gp('data_path',file.path(script_dir,'assets/cell_summary.csv'))
if(!nzchar(data_path)) data_path <- file.path(script_dir,'assets/cell_summary.csv')
if(!file.exists(data_path)) stop('Data CSV does not exist')
is_demo <- identical(normalizePath(data_path),normalizePath(file.path(script_dir,'assets/cell_summary.csv')))
d <- read.csv(data_path,stringsAsFactors=FALSE,check.names=FALSE)
tracks <- c('sex_rna','sex_atac','rna_proportion','age_rna','age_atac','atac_proportion')
required <- c('lineage','state','subtype',tracks,paste0(tracks[c(1,2,4,5)],'_q'))
if(!all(required %in% names(d))) stop('Missing CSV columns: ',paste(setdiff(required,names(d)),collapse=', '))
if(nrow(d)<20 || nrow(d)>90 || anyNA(d[required]) || anyDuplicated(d$subtype) ||
   !setequal(unique(d$lineage),groups) || any(!nzchar(d$state)) || any(!nzchar(d$subtype))) stop('Invalid taxonomy (20-90 unique tips, all five lineages required)')
numeric_cols <- setdiff(required,c('lineage','state','subtype'))
if(any(!vapply(d[numeric_cols],is.numeric,logical(1))) || any(!is.finite(as.matrix(d[numeric_cols])))) stop('Non-finite numeric data')
for(key in c('rna_proportion','atac_proportion')) if(any(d[[key]]<0|d[[key]]>1)||abs(sum(d[[key]])-1)>.01) stop('Proportions must sum to one')
for(key in paste0(tracks[c(1,2,4,5)],'_q')) if(any(d[[key]]<0|d[[key]]>1)) stop('q values must be in [0,1]')
d <- d[order(match(d$lineage,groups)),]; n <- nrow(d)
# Require contiguous subtypes of each state so the hierarchy cannot cross itself.
state_key <- paste(d$lineage,d$state)
if(anyDuplicated(rle(state_key)$values)) stop('Rows for each lineage/state must be contiguous')
d$theta <- pi/2 - (seq_len(n)-.5)*2*pi/n
d$index <- seq_len(n); d$col <- cols[d$lineage]
xy <- function(r,a) data.frame(x=r*cos(a),y=r*sin(a))
sector <- function(a,b,r0,r1,id,col) {q<-seq(a,b,length.out=18);z<-rbind(xy(r0,q),xy(r1,rev(q)));z$id<-id;z$col<-col;z}
polys <- list(); paths <- list(); texts <- list()
add_poly <- function(p) {polys[[length(polys)+1L]] <<- p}
add_text <- function(r,a,label,col,size,bold=FALSE) {
  z<-xy(r,a);z$label<-label;z$col<-col;z$size<-size;z$angle<-a*180/pi
  z$hjust<-.5;z$face<-if(bold)'bold' else 'plain'
  if(cos(a)<0) z$angle<-z$angle+180
  texts[[length(texts)+1L]] <<- z
}
ribbon <- function(r0,a0,r1,a1,w0,w1,col) {
  t<-seq(0,1,length.out=60);p0<-as.numeric(xy(r0,a0));p3<-as.numeric(xy(r1,a1))
  p1<-as.numeric(xy(r0+(r1-r0)*.65,a0));p2<-as.numeric(xy(r0+(r1-r0)*.45,a1))
  x<-(1-t)^3*p0[1]+3*(1-t)^2*t*p1[1]+3*(1-t)*t^2*p2[1]+t^3*p3[1]
  y<-(1-t)^3*p0[2]+3*(1-t)^2*t*p1[2]+3*(1-t)*t^2*p2[2]+t^3*p3[2]
  dx<-c(diff(x),tail(diff(x),1));dy<-c(diff(y),tail(diff(y),1));norm<-sqrt(dx^2+dy^2)
  w<-(w0*(1-t)+w1*t)*branch_scale
  z<-data.frame(x=c(x-dy/norm*w,rev(x+dy/norm*w)),y=c(y+dx/norm*w,rev(y-dx/norm*w)))
  z$id<-paste0('branch',length(polys));z$col<-col;add_poly(z)
  ii<-43L
  data.frame(x=x[ii],y=y[ii],angle=atan2(dy[ii],dx[ii])*180/pi)
}
for(g in groups) {
  ix<-which(d$lineage==g);a<-mean(d$theta[ix]);c<-cols[g]
  add_poly(sector(max(d$theta[ix])+pi/n,min(d$theta[ix])-pi/n,.06,.998,paste0('bg',g),blend(c,background,.92)))
  ribbon(.025,a,.40,a,.022,.036,c)
  add_text(.27,a,g,if(plot_nh_contrast_ratio(c,'#FFFFFF')>3)'#FFFFFF' else ink,lineage_size,TRUE)
  states <- unique(d$state[ix])
  for(j in seq_along(states)) {
    ss<-ix[d$state[ix]==states[j]];sa<-mean(d$theta[ss])
    sc<-blend(c,if(j%%2)ink else background,if(j%%2).16 else .16)
    while(plot_nh_contrast_ratio(sc,background)<3) sc<-blend(sc,contrast_ink,.10)
    label_pos<-ribbon(.4,a,.76,sa,.030,.018+sqrt(length(ss))*.005,sc)
    for(i in ss) {
      ribbon(.76,sa,.996,d$theta[i],.009,.0055,sc)
      add_poly(sector(d$theta[i]+pi/n,d$theta[i]-pi/n,.996,1.058,paste0('tip',i),sc))
      add_text(1.025,d$theta[i],as.character(i),if(plot_nh_contrast_ratio(sc,'#FFFFFF')>3)'#FFFFFF' else ink,branch_label_size)
    }
    # Label states only once; branch widths encode hierarchy, never abundance.
    label_pos$label<-states[j];label_pos$size<-branch_label_size
    label_pos$col<-if(plot_nh_contrast_ratio(sc,'#FFFFFF')>3)'#FFFFFF' else ink
    label_pos$hjust<-.5;label_pos$face<-'bold'
    if(label_pos$angle>90 || label_pos$angle< -90)label_pos$angle<-label_pos$angle+180
    texts[[length(texts)+1L]]<-label_pos
  }
}
ring_start <- 1.07; label_r <- ring_start + 6*ring_width+.032
effect_limit <- max(.5,max(abs(as.matrix(d[tracks[c(1,2,4,5)]]))))
effect_colors <- grDevices::colorRampPalette(heat)(257)
prop_colors <- grDevices::colorRampPalette(c(background,prop_high))(257)
stars <- list()
for(j in seq_along(tracks)) {
  key<-tracks[j];r0<-ring_start+(j-1)*ring_width
  for(i in seq_len(n)) {
    v<-d[[key]][i]
    if(grepl('proportion',key)) {fill<-prop_colors[1+round(v/max(d[[key]])*256)]} else {
      fill<-effect_colors[1+round((v/effect_limit+1)*128)]
      q<-d[[paste0(key,'_q')]][i];symbol<-if(q<.001)'***' else if(q<.01)'**' else if(q<.05)'*' else ''
      if(nzchar(symbol)) {
        z<-xy(r0+ring_width/2,d$theta[i]);z$label<-symbol;z$angle<-d$theta[i]*180/pi-90
        z$col<-if(plot_nh_contrast_ratio(fill,'#FFFFFF')>3.5)'#FFFFFF' else ink;stars[[length(stars)+1L]]<-z
      }
    }
    add_poly(sector(d$theta[i]+pi/n*.985,d$theta[i]-pi/n*.985,r0,r0+ring_width*.94,paste0('ring',j,'-',i),fill))
  }
}
outer <- cbind(d,xy(label_r,d$theta))
outer$label <- paste0(d$subtype,'\n(',sprintf('%.2f',d$rna_proportion*100),'%)')
outer$angle <- d$theta*180/pi; outer$hjust <- ifelse(cos(d$theta)>=0,0,1)
outer$angle <- ifelse(cos(d$theta)<0,outer$angle+180,outer$angle)
outer$col <- label_cols[d$lineage]
p <- ggplot()+geom_polygon(data=do.call(rbind,polys),aes(x,y,group=id,fill=col),colour=NA)+
  geom_text(data=do.call(rbind,texts),aes(x,y,label=label,colour=col,size=size,angle=angle,fontface=face),family=family)+
  geom_text(data=outer,aes(x,y,label=label,angle=angle,hjust=hjust,colour=col),size=label_size,family=family,lineheight=.94)
if(length(stars)) p<-p+geom_text(data=do.call(rbind,stars),aes(x,y,label=label,angle=angle,colour=col),size=legend_size*.9,family=family)
# Compact track key in the free lower-right corner, with identical radial ordering.
cx<-1.39;cy<--1.37;mini<-list()
for(j in 1:6) for(i in 1:36) {
  col<-if(j%%3==0)prop_colors[1+round((i-1)/35*256)] else effect_colors[1+round((i-1)/35*256)]
  z<-sector(2*pi*(i-1)/36,2*pi*i/36,.095+(j-1)*.014,.095+j*.014,paste(j,i),col)
  z$x<-z$x+cx;z$y<-z$y+cy;mini[[length(mini)+1L]]<-z
}
caption <- paste0(if(is_demo)'Synthetic donor data' else 'User-supplied summary',
  '  |  Taxonomy, not inferred ancestry\nAge: per SD; sex: female vs male; color: log2 proportion effect\nBH q: * <0.05, ** <0.01, *** <0.001; outer labels: mean RNA %')
p<-p+geom_polygon(data=do.call(rbind,mini),aes(x,y,group=id,fill=col),colour=NA)+
  annotate('text',x=cx,y=cy,label='Inner\nto outer',size=legend_size*.85,family=family,colour=ink)+
  annotate('text',x=1.11,y=-1.67,hjust=0,label='1 Sex RNA   2 Sex ATAC   3 RNA %\n4 Age RNA   5 Age ATAC   6 ATAC %',size=legend_size*.82,family=family,colour=ink,lineheight=1.25)+
  annotate('text',x=-1.80,y=-1.67,hjust=0,label=caption,size=legend_size*.85,family=family,colour=muted,lineheight=1.25)
# A shared zero-centered scale for all association tracks.
key <- data.frame(x=seq(1.15,1.64,length.out=257),y=-1.10,col=effect_colors)
prop_key <- data.frame(x=seq(1.15,1.64,length.out=257),y=-1.18,col=prop_colors)
p<-p+geom_tile(data=key,aes(x,y,fill=col),width=.49/256,height=.025)+
  geom_tile(data=prop_key,aes(x,y,fill=col),width=.49/256,height=.02)+
  annotate('text',x=1.395,y=-1.135,label=sprintf('0 to max: RNA %.1f%% / ATAC %.1f%%',max(d$rna_proportion)*100,max(d$atac_proportion)*100),size=legend_size*.7,family=family,colour=ink)+
  annotate('text',x=c(1.15,1.395,1.64),y=-1.05,label=c(sprintf('%.2f',-effect_limit),'0',sprintf('%.2f',effect_limit)),size=legend_size*.8,family=family,colour=ink)+
  scale_fill_identity()+scale_colour_identity()+scale_size_identity()+
  coord_fixed(xlim=c(-1.85,1.85),ylim=c(-1.77,1.80),expand=FALSE,clip='off')+
  theme_void(base_family=family)+theme(plot.background=element_rect(fill=background,colour=NA),plot.margin=margin(2,2,2,2,'mm'))
out <- arg('output-dir',cfg$output_dir %||% file.path(script_dir,'output'))
dir.create(out,recursive=TRUE,showWarnings=FALSE)
ggsave(file.path(out,'plot.png'),p,width=width_mm,height=height_mm,units='mm',dpi=dpi,bg=background,device=ragg::agg_png)
ggsave(file.path(out,'plot.pdf'),p,width=width_mm,height=height_mm,units='mm',device=grDevices::pdf,bg=background,useDingbats=FALSE)
ggsave(file.path(out,'plot.svg'),p,width=width_mm,height=height_mm,units='mm',device=svglite::svglite,bg=background)
stopifnot(all(file.info(file.path(out,paste0('plot.',c('png','pdf','svg'))))$size>1000))
stopifnot(all(plot_nh_validate_palette(label_cols,background)$pass_text_4_5_to_1))
cat('Rendered ',n,' tips; three native exports; effect range +/-',effect_limit,'\n')
