#!/usr/bin/env Rscript

# Tutorial genomic loci: explicit hg38 background, exact endpoints and unverified provenance.
options(device=function(...)stop("Implicit graphics device prohibited",call.=FALSE))
local({
  color_module <- Sys.getenv("PLOT_NH_COLOR_MODULE", unset = "")
  if (!nzchar(color_module) || !file.exists(color_module)) {
    file_arg <- grep("^--file=", commandArgs(), value = TRUE)
    start_dir <- if (length(file_arg)) dirname(sub("^--file=", "", file_arg[[1L]])) else getwd()
    start_dir <- normalizePath(start_dir, mustWork = FALSE)
    for (level in seq_len(12L)) {
      candidates <- c(file.path(start_dir, "manual_library", "scripts", "plot_nh_colors.R"), file.path(start_dir, "scripts", "plot_nh_colors.R"), file.path(start_dir, "plot_nh_colors.R"))
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

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  flag <- paste0("--", name); index <- match(flag, args)
  if (!is.na(index) && index < length(args)) return(args[[index + 1L]])
  inline <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(inline)) return(sub(paste0("^", flag, "="), "", inline[[1L]]))
  default
}
config_path <- arg_value("config")
if (is.null(config_path) || !file.exists(config_path)) stop("--config is required", call. = FALSE)
required_packages <- c("jsonlite", "svglite", "ragg", "readxl", "showtext", "sysfonts", "systemfonts")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE); params <- if (is.null(cfg$params)) list() else cfg$params
if(!is.list(cfg)||!is.list(params)||(!is.null(names(params))&&any(!nzchar(names(params)))))stop("config/params must be objects",call.=FALSE)
gp <- function(name, default = NULL) {value<-params[[name]];if(is.null(value))for(section in c("data","model","selection","colors","typography","geometry","axis","legend","layout","labels","statistics","analysis","clustering","export"))if(is.list(params[[section]])&&!is.null(params[[section]][[name]])){value<-params[[section]][[name]];break};if(is.null(value))default else value}
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
`%||%`<-function(x,y)if(is.null(x))y else x
source_files<-vapply(sys.frames(),function(frame)frame$ofile %||% "",character(1));source_file<-tail(source_files[nzchar(source_files)],1);script_file<-if(length(source_file))source_file else if(length(file_arg))sub("^--file=","",file_arg[[1]])else NULL
script_dir<-dirname(normalizePath(script_file,mustWork=TRUE))
output_dir <- arg_value("output-dir", if (is.null(cfg$output_dir)) file.path(script_dir, "output") else cfg$output_dir); dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(name, default, lower = -Inf, upper = Inf) {
  if(is.list(gp(name,default))||is.logical(gp(name,default)))stop(name," must be a numeric scalar",call.=FALSE)
  value <- suppressWarnings(as.numeric(gp(name, default)))
  if (length(value) != 1L || !is.finite(value) || value < lower || value > upper) stop(sprintf("%s must be between %s and %s", name, lower, upper), call. = FALSE)
  value
}
integer_param<-function(name,default,lower,upper){v<-num(name,default,lower,upper);if(v!=floor(v))stop(name," must be an integer",call.=FALSE);as.integer(v)}
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
  if(is.list(value)||length(value)!=1||anyNA(value))stop(name," must be a complete scalar",call.=FALSE)
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
resolve_input <- function(value, fallback) {
  value <- trimws(as.character(value)); project_app <- dirname(dirname(dirname(script_dir))); candidates <- if (nzchar(value)) c(value, file.path(getwd(), value), file.path(script_dir, value), file.path(project_app, value)) else c(fallback, file.path(script_dir, fallback)); hit <- unique(candidates[file.exists(candidates)]); if (!length(hit)) stop("Input file does not exist: ", if (nzchar(value)) value else fallback, call. = FALSE); normalizePath(hit[[1L]], mustWork = TRUE)
}
read_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if(ext=="xls"){magic<-readBin(path,"raw",n=8);binary<-identical(magic,as.raw(c(208,207,17,224,161,177,26,225)))||identical(head(magic,2),charToRaw("PK"));if(identical(head(magic,2),charToRaw("PK")))ext<-"xlsx"else if(!binary)ext<-"txt"}
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt", "md", "maf")) { seps<-c("\t",";",",");counts<-vapply(seps,function(sep){v<-tryCatch(utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE),error=function(e)0L);max(c(0L,v),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; fields<-utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE);fields<-fields[!is.na(fields)];if(!length(fields)||any(fields!=fields[1]))stop("Every table record must match the header field count",call.=FALSE); if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for XLS/XLSX input", call. = FALSE)
    as.data.frame((if(ext=="xlsx")readxl::read_xlsx else readxl::read_xls)(path, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}


















chroms<-c(paste0('chr',1:22),'chrX','chrY');genome<-read.delim(file.path(script_dir,'assets/chromosomes-hg38.tsv'),stringsAsFactors=FALSE);cyto<-read.delim(file.path(script_dir,'assets/cytobands-hg38.tsv'),stringsAsFactors=FALSE);lengths<-setNames(genome$length,genome$chr)
data_path<-resolve_input(text_param('data_path','',TRUE),'assets/input.txt');raw<-read_table(data_path);field_names<-c('chr_field','start_field','end_field','value_field');defaults<-c('chr','start','end','value');fields<-vapply(seq_len(4),function(i)text_param(field_names[i],defaults[i]),character(1));if(anyDuplicated(fields)||anyDuplicated(names(raw))||!all(fields%in%names(raw)))stop('Distinct fields and unique complete headers required',call.=FALSE)
cr<-as.character(raw[[fields[1]]]);st<-parse_numeric(raw[[fields[2]]],'start');en<-parse_numeric(raw[[fields[3]]],'end');val<-parse_numeric(raw[[fields[4]]],'value');if(!nrow(raw)||nrow(raw)>20000||anyNA(cr)||any(!cr%in%chroms)||any(!is.finite(c(st,en,val)))||any(st!=floor(st)|en!=floor(en)|st<1|en<st)||any(en>lengths[cr]))stop('1..20000 complete canonical human intervals, source 1-based endpoints within hg38 and finite values required; no row dropping',call.=FALSE)
dat<-data.frame(row=seq_len(nrow(raw)),chr=cr,start=st,end=en,value=val,midpoint=(st+en)/2);vr<-range(val);spread<-diff(vr);if(spread==0)spread<-max(abs(val[1])*.1,1);yrange<-vr+c(-.04,.04)*spread;legacy_range<-c(as.numeric(quantile(val,.1)),as.numeric(quantile(val,.9))*2);legacy_out<-sum(val<legacy_range[1]|val>legacy_range[2])
width_mm<-num('width_mm',210,180,240);height_mm<-num('height_mm',175,170,215);dpi<-integer_param('dpi',600,96,1200);font_size<-num('font_size',8.5,8,9.5);tick_font_size<-num('tick_font_size',6,5.5,6.5);point_size<-num('point_size',1.1,.6,1.3);point_alpha<-num('point_alpha',.9,.5,1);band_height<-num('band_height',.9,.5,1);axis_width<-num('axis_width',.23,.12,.3);major_mb<-choice('major_mb','10',c('10','20'));show_zero<-bool('show_zero',TRUE);show_minor<-bool('show_minor',TRUE);point_color<-toupper(text_param('accent_color','#E31A1C'));if(!grepl('^#[0-9A-F]{6}$',point_color)||point_color=='#FFFFFF')stop('Visible #RRGGBB point colour required',call.=FALSE);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
left<-16;right<-width_mm-9;bottom<-17;top<-height_mm-13;pitch<-(top-bottom)/24;plot_height<-pitch-band_height-2.2;if(plot_height<2.5)stop('Canvas too short for separate points, ideogram and ticks',call.=FALSE);genome<-genome[match(chroms,genome$chr),];genome$y_mm<-top-(seq_len(24)-.5)*pitch;genome$right_mm<-left+(genome$length-1)/(max(lengths)-1)*(right-left);dat$x_mm<-left+(dat$midpoint-1)/(max(lengths)-1)*(right-left);dat$y_mm<-genome$y_mm[match(cr,genome$chr)]+band_height/2+.7+(val-yrange[1])/diff(yrange)*plot_height
cyto$x1_mm<-left+(cyto$start-1)/(max(lengths)-1)*(right-left);cyto$x2_mm<-left+(cyto$end-1)/(max(lengths)-1)*(right-left);cyto$y_mm<-genome$y_mm[match(cyto$chr,genome$chr)];stain_colors<-c(gneg='#FFFFFF',gpos25='#C8C8C8',gpos33='#D2D2D2',gpos50='#C8C8C8',gpos66='#A0A0A0',gpos75='#828282',gpos100='#000000',gpos='#000000',stalk='#647FA4',acen='#D92F27',gvar='#DCDCDC');cyto$color<-stain_colors[cyto$stain];if(anyNA(cyto$color))stop('Unsupported cytoband stain')
title<-'Genomic interval values on hg38';caption<-c(sprintf('%d intervals; all values shown; within-row height: %.3f to %.3f; coordinates in Mb.',nrow(dat),yrange[1],yrange[2]),'Supplied interval build / value units / biological interpretation unverified; no liftover.')
font_file<-function(weight='normal',italic=FALSE){h<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(h$index!=0L)h<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);h$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces))
render<-function(){grid::grid.newpage();mm<-function(x)grid::unit(x,'mm');txt<-function(z,x,y,size=font_size,face='plain',just='centre')grid::grid.text(z,mm(x),mm(y),just=just,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face,col='#111111'));ln<-function(x,y,col='#444444',w=axis_width)grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=col,lwd=w*72.27/25.4));txt(title,width_mm/2,height_mm-5,10,'bold')
for(i in seq_len(24)){g<-genome[i,];txt(g$chr,left-2,g$y_mm,font_size,just='right');if(show_zero&&yrange[1]<0&&yrange[2]>0){zero<-g$y_mm+band_height/2+.7+(0-yrange[1])/diff(yrange)*plot_height;ln(c(left,g$right_mm),rep(zero,2),'#D5D5D5',.08)};if(show_minor){minor<-seq(1e6,g$length,by=1e6);xx<-left+(minor-1)/(max(lengths)-1)*(right-left);for(x in xx)ln(rep(x,2),c(g$y_mm-band_height/2,g$y_mm-band_height/2-.3),'#666666',.08)};at<-c(1,seq(as.numeric(major_mb)*1e6,g$length,by=as.numeric(major_mb)*1e6));for(bp in at){x<-left+(bp-1)/(max(lengths)-1)*(right-left);ln(rep(x,2),c(g$y_mm-band_height/2,g$y_mm-band_height/2-.55))}}
# Shared Mb labels avoid collision with values in the next chromosome row.
ln(c(left,right),rep(top+2,2));for(bp in c(1,seq(as.numeric(major_mb)*1e6,max(lengths),by=as.numeric(major_mb)*1e6))){x<-left+(bp-1)/(max(lengths)-1)*(right-left);ln(rep(x,2),c(top+2,top+2.6));txt(if(bp==1)'0'else format(bp/1e6,trim=TRUE),x,top+4.5,tick_font_size)}
for(i in seq_len(nrow(cyto)))grid::grid.rect(mm((cyto$x1_mm[i]+cyto$x2_mm[i])/2),mm(cyto$y_mm[i]),width=mm(cyto$x2_mm[i]-cyto$x1_mm[i]),height=mm(band_height),gp=grid::gpar(fill=cyto$color[i],col='#222222',lwd=axis_width*72.27/25.4))
for(i in seq_len(nrow(dat)))grid::grid.rect(mm(dat$x_mm[i]),mm(dat$y_mm[i]),width=mm(point_size),height=mm(point_size),gp=grid::gpar(fill=point_color,col=NA,alpha=point_alpha))
# Shared quantitative key is in the naturally empty space beside short chromosomes.
kx<-width_mm-38;ky<-33;kh<-24;ln(rep(kx,2),c(ky,ky+kh));for(v in c(yrange[1],0,yrange[2]))if(v>=yrange[1]&&v<=yrange[2]){y<-ky+(v-yrange[1])/diff(yrange)*kh;ln(c(kx,kx+1),rep(y,2));txt(sprintf('%.2f',v),kx+2,y,6,just='left')};txt('Value scale',kx,ky+kh+4,7,'bold');txt('height above chromosome',kx,ky-4,6);for(i in seq_along(caption))txt(caption[i],4,8.5-(i-1)*3.7,6,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
write.csv(dat,file.path(output_dir,'actual_points.csv'),row.names=FALSE);write.csv(cyto,file.path(output_dir,'actual_cytobands.csv'),row.names=FALSE);write.csv(genome,file.path(output_dir,'actual_chromosomes.csv'),row.names=FALSE);saveRDS(list(raw=raw,dat=dat,cyto=cyto,genome=genome),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(fields=as.list(fields),data_path=data_path,interval_count=nrow(dat),negative_count=sum(val<0),cytoband_count=nrow(cyto),chromosomes=as.list(chroms),width_mm=width_mm,height_mm=height_mm,dpi=dpi,font_size=font_size,tick_font_size=tick_font_size,point_size=point_size,point_alpha=point_alpha,band_height=band_height,axis_width=axis_width,major_mb=major_mb,show_zero=show_zero,show_minor=show_minor,accent_color=point_color,family=family,pdf_faces=pdf_faces,title=title,caption=as.list(caption),yrange=as.list(yrange),legacy_range=as.list(legacy_range),legacy_out_of_track_count=legacy_out,geometry=list(left=left,right=right,bottom=bottom,top=top,pitch=pitch,plot_height=plot_height,max_bp=max(lengths)),scope='All supplied intervals preserved; source midpoint and 1-based numeric endpoints retained; shared full value range within rows, legacy quantile range changed explicitly to prevent out-of-track points; hg38 reference only, source assembly/units unverified.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',nrow(dat),'complete interval values;',legacy_out,'originally outside source quantile track\n')
