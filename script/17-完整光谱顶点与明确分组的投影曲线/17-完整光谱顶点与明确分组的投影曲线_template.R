#!/usr/bin/env Rscript

# Tutorial spectra: full waveform vertices, explicit grouping and unverified calibration.
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

data_path<-resolve_input(text_param('data_path','',TRUE),'assets/data.txt');raw<-read_table(data_path);x_field<-text_param('x_field','Wavelength');series_csv<-text_param('series_fields','',TRUE);series<-if(nzchar(series_csv))trimws(strsplit(series_csv,',',fixed=TRUE)[[1]])else setdiff(names(raw),x_field);if(anyDuplicated(names(raw))||anyDuplicated(c(x_field,series))||!all(c(x_field,series)%in%names(raw))||length(series)<2||length(series)>8||any(grepl('Case[ _:#-]*[0-9]|cover_[0-9]',series,ignore.case=TRUE)))stop('Unique X and 2..8 non-debug amplitude fields required',call.=FALSE);x<-parse_numeric(raw[[x_field]],'wavelength');mat<-sapply(series,function(g)parse_numeric(raw[[g]],g));if(nrow(raw)<3||nrow(raw)>20000||any(!is.finite(c(x,mat)))||anyDuplicated(x))stop('3..20000 complete finite unique wavelengths and every amplitude required',call.=FALSE);ord<-order(x);x<-x[ord];mat<-mat[ord,,drop=FALSE];n<-length(series);group_csv<-text_param('series_groups','',TRUE);default_groups<-c('groupA','groupA','groupB','groupB','groupC','groupC','groupD');groups<-if(nzchar(group_csv))trimws(strsplit(group_csv,',',fixed=TRUE)[[1]])else if(identical(series,paste0('Amplitude_',1:7)))default_groups else series;if(length(groups)!=n||any(!nzchar(groups))||any(grepl('Case[ _:#-]*[0-9]|cover_[0-9]',groups,ignore.case=TRUE)))stop('One complete non-debug group per amplitude field required',call.=FALSE);group_levels<-unique(groups);palette<-toupper(trimws(strsplit(text_param('palette','#0099E5,#FF4C4C,#34BF49,#7D3F98,#B76B16,#566573,#1D4E89,#AD598C'),',',fixed=TRUE)[[1]]));if(length(palette)<length(group_levels)||any(!grepl('^#[0-9A-F]{6}$',palette))||anyDuplicated(palette)||any(palette=='#FFFFFF'))stop('Distinct visible colours for every group required',call.=FALSE);cols<-plot_nh_format_palette(setNames(palette[seq_along(group_levels)],group_levels), tryCatch(as.character(group_levels), error = function(e) NULL), cfg)
width_mm<-num('width_mm',185,180,300);height_mm<-num('height_mm',75,70,125);dpi<-integer_param('dpi',600,96,1200);font_size<-num('font_size',8,7.5,9);label_size<-num('label_size',8.5,7,8.5);axis_width<-num('axis_width',.35,.25,.5);line_width<-num('line_width',.8,.5,1);grid_width<-num('grid_width',.18,.1,.3);angle<-num('angle',40,25,55);depth_scale<-num('depth_scale',.9,.65,1.15);show_grid<-bool('show_grid',TRUE);show_legend<-bool('show_legend',TRUE);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
xticks<-pretty(range(x),n=5,min.n=2.5);xs<-round(diff(xticks[1:2]),12);xticks<-xticks[xticks>=min(x)&xticks<=max(x)];yticks<-pretty(c(0,n),n=5,min.n=2.5);ys<-round(diff(yticks[1:2]),12);zticks0<-pretty(c(min(0,mat),max(2500,mat)),n=5,min.n=2.5);zs<-round(diff(zticks0[1:2]),12);zlo<-floor(min(0,mat)/(zs*.2))*zs*.2;zhi<-ceiling(max(2500,mat)/zs)*zs;zticks<-seq(ceiling(zlo/zs)*zs,zhi,by=zs);tracks<-seq_len(n)-.5;xf<-(1-angle/90)*depth_scale/ys;yf<-angle/90*depth_scale/ys;project<-function(xx,tt,zz)data.frame(u=xx/xs+xf*tt,v=zz/zs+yf*tt);dat<-do.call(rbind,lapply(seq_len(n),function(i){p<-project(x,tracks[i],mat[,i]);data.frame(series=series[i],group=groups[i],row=seq_along(x),wavelength=x,track=tracks[i],value=mat[,i],u=p$u,v=p$v,color=cols[[groups[i]]])}));umin<-min(x)/xs;umax<-max(x)/xs+xf*n;vmin<-min(zlo/zs,dat$v)-.03;vmax<-max(zhi/zs,dat$v)+.12;left<-22;right<-width_mm-24;bottom<-22;legend_rows<-if(length(group_levels)>4)2L else length(group_levels);legend_cols<-ceiling(length(group_levels)/legend_rows);top<-height_mm-if(show_legend&&legend_cols>1)14 else 1;scale_u<-(right-left)/(umax-umin);scale_v<-(top-bottom)/(vmax-vmin);physical<-function(xx,tt,zz){p<-project(xx,tt,zz);data.frame(x_mm=left+(p$u-umin)*scale_u,y_mm=bottom+(p$v-vmin)*scale_v)};dat$x_mm<-left+(dat$u-umin)*scale_u;dat$y_mm<-bottom+(dat$v-vmin)*scale_v
labels<-do.call(rbind,lapply(seq_len(n),function(i){p<-physical(max(x),tracks[i],tail(mat[,i],1));data.frame(series=series[i],color=cols[[groups[i]]],anchor_x=p$x_mm,anchor_y=p$y_mm,x_mm=p$x_mm+2,y_mm=p$y_mm)}));spacing<-label_size*25.4/72+.8;for(i in 2:n)labels$y_mm[i]<-max(labels$y_mm[i],labels$y_mm[i-1]+spacing);excess<-max(0,max(labels$y_mm)+label_size*25.4/144-(height_mm-1));labels$y_mm<-labels$y_mm-excess;if(min(labels$y_mm)<14)stop('Track labels require a taller canvas',call.=FALSE);label_position<-if(max(abs(labels$y_mm-labels$anchor_y))>2)'column'else'stair';if(label_position=='column')labels$x_mm<-right+2
edges<-list();edge<-function(kind,xx,tt,zz){p<-physical(xx,tt,zz);edges[[length(edges)+1]]<<-data.frame(kind=kind,x1=p$x_mm[1],y1=p$y_mm[1],x2=p$x_mm[2],y2=p$y_mm[2])};if(show_grid){for(t in xticks)edge('grid',rep(t,2),c(0,n),c(0,0));for(t in tracks)edge('grid',range(x),rep(t,2),c(0,0))};edge('axis',range(x),c(0,0),c(0,0));edge('axis',rep(min(x),2),c(0,0),c(zlo,zhi));edges<-do.call(rbind,edges);draw_order<-rev(seq_len(n));x_unit<-text_param('x_unit','tutorial nm',TRUE);value_unit<-text_param('value_unit','tutorial mV',TRUE);if(any(grepl('Case[ _:#-]*[0-9]|cover_[0-9]|案例[ ]*[0-9]',c(x_field,x_unit,value_unit),ignore.case=TRUE)))stop('Axis labels cannot contain internal/debug identifiers',call.=FALSE);x_title<-paste0(x_field,if(nzchar(x_unit))paste0(' (',x_unit,')'));z_title<-paste0('Amplitude',if(nzchar(value_unit))paste0(' (',value_unit,')'));caption<-c(sprintf('%d raw amplitude values; %d negatives retained; straight segments; track index is layout.',length(mat),sum(mat<0)),'Waveform data; calibration and group meaning unverified; oblique view, not 3D distance.')
font_file<-function(weight='normal',italic=FALSE){h<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(h$index!=0L)h<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);h$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces));probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=s))),'mm',TRUE);if(max(vapply(series,tw,numeric(1),s=label_size))>22||max(vapply(caption,tw,numeric(1),s=5.5))>width_mm-8||max(vapply(group_levels,tw,numeric(1),s=label_size))>30||tw(x_title,font_size+2)>right-left||tw(z_title,font_size+2)>height_mm-20)stop('Full labels require shorter fields or larger canvas',call.=FALSE)},finally={dev.off();unlink(probe)})
plot_nh_parameter_context(cfg, plot_nh_format_output(cfg), groups = group_levels, palette = cols)
render<-function(){grid::grid.newpage();mm<-function(x)grid::unit(x,'mm');txt<-function(z,x,y,s=font_size,col='#111111',face='plain',just='centre',rot=0)grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=s,fontface=face,col=col));ln<-function(xx,yy,col='#222222',w=axis_width)grid::grid.lines(mm(xx),mm(yy),gp=grid::gpar(col=col,lwd=w*72.27/25.4,lineend='butt',linejoin='round'));for(i in seq_len(nrow(edges))){e<-edges[i,];ln(c(e$x1,e$x2),c(e$y1,e$y2),if(e$kind=='grid')'#C8C8C8'else'#222222',if(e$kind=='grid')grid_width else axis_width)};for(t in xticks){q<-physical(t,0,0);ln(rep(q$x_mm,2),q$y_mm+c(-.8,0));txt(format(t,trim=TRUE),q$x_mm,q$y_mm-3.6,font_size)};for(t in zticks){q<-physical(min(x),0,t);ln(q$x_mm+c(-.8,0),rep(q$y_mm,2));txt(format(t,trim=TRUE),q$x_mm-1.8,q$y_mm,font_size,just='right')};q<-physical(mean(range(x)),0,0);txt(x_title,q$x_mm,14,font_size+2,face='bold');q<-physical(min(x),0,mean(c(zlo,zhi)));txt(z_title,5,q$y_mm,font_size+2,face='bold',rot=90)
for(i in draw_order){d<-dat[dat$series==series[i],];ln(d$x_mm,d$y_mm,d$color[1],line_width)};for(i in seq_len(n)){l<-labels[i,];if(abs(l$y_mm-l$anchor_y)>.15||abs(l$x_mm-l$anchor_x-2)>.15)ln(c(l$anchor_x,l$x_mm-.5),c(l$anchor_y,l$y_mm),l$color,.16);txt(l$series,l$x_mm,l$y_mm,label_size,l$color,just='left')};if(show_legend){for(i in seq_along(group_levels)){col<-(i-1)%/%legend_rows;row<-(i-1)%%legend_rows;x0<-left+2+col*37;y0<-height_mm-5-row*4.5;ln(c(x0,x0+6),rep(y0,2),cols[i],line_width);txt(group_levels[i],x0+8,y0,label_size,just='left')}};for(i in seq_along(caption))txt(caption[i],4,7.2-(i-1)*3.2,5.5,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
write.csv(dat,file.path(output_dir,'actual_curve_vertices.csv'),row.names=FALSE);write.csv(labels,file.path(output_dir,'actual_series_labels.csv'),row.names=FALSE);write.csv(edges,file.path(output_dir,'actual_edges.csv'),row.names=FALSE);saveRDS(list(raw=raw,x=x,mat=mat,dat=dat,labels=labels,edges=edges),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(data_path=data_path,x_field=x_field,series=as.list(series),series_groups=as.list(groups),group_levels=as.list(group_levels),sample_count=length(x),value_count=length(mat),negative_count=sum(mat<0),width_mm=width_mm,height_mm=height_mm,dpi=dpi,font_size=font_size,label_size=label_size,axis_width=axis_width,line_width=line_width,grid_width=grid_width,angle=angle,depth_scale=depth_scale,show_grid=show_grid,show_legend=show_legend,legend_rows=legend_rows,legend_cols=legend_cols,label_position=label_position,palette=as.list(unname(cols)),family=family,pdf_faces=pdf_faces,x_title=x_title,z_title=z_title,x_unit=x_unit,value_unit=value_unit,caption=as.list(caption),draw_order=as.list(draw_order),geometry=list(left=left,right=right,bottom=bottom,top=top,xs=xs,ys=ys,zs=zs,xf=xf,yf=yf,umin=umin,umax=umax,vmin=vmin,vmax=vmax,scale_u=scale_u,scale_v=scale_v,xticks=as.list(xticks),zticks=as.list(zticks),zlo=zlo,zhi=zhi),scope='All provided waveform values and straight-segment vertices retained; no smoothing or clipping of negatives. Original scatterplot3d oblique angle/depth normalization; independent projected canvas scales, not 3D metric. Origin tutorial data, calibration and group provenance unverified; default literal group mapping from original script only.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',length(mat),'raw amplitudes,',length(x),'wavelengths and',n,'full line tracks\n')
