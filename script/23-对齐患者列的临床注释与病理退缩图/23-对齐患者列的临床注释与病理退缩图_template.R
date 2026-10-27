#!/usr/bin/env Rscript

# Source clinical annotations and aligned downward pathological regression.
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
  if(ext=="xls"){magic<-readBin(path,"raw",n=8);binary<-identical(magic,as.raw(c(208,207,17,224,161,177,26,225)))||identical(head(magic,2),charToRaw("PK"));if(!binary)ext<-"txt"}
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt", "md", "maf")) { seps<-c("\t",";",",");counts<-vapply(seps,function(sep){v<-tryCatch(utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE),error=function(e)0L);max(c(0L,v),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; fields<-utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE);fields<-fields[!is.na(fields)];if(!length(fields)||any(fields!=fields[1]))stop("Every table record must match the header field count",call.=FALSE); if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for XLS/XLSX input", call. = FALSE)
    as.data.frame(readxl::read_excel(path, sheet=integer_param("sheet",4,1,100), .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}
















ids<-function(v){v<-as.character(v);if(anyNA(v)||any(!nzchar(v))||anyDuplicated(v)||any(v!=trimws(v))||any(grepl('[[:cntrl:]]',v)))stop('IDs require unique complete exact values',call.=FALSE);v}
input_sheet<-integer_param('sheet',4,1,100)
data_path<-resolve_input(text_param('data_path','',TRUE),'assets/41467_2022_35431_MOESM8_ESM.xlsx');raw<-read_table(data_path);features<-c('MSI','PD-L1','ERBB amplification','RECIST',"Lauren' classification",'Tstage');req<-c('ID',features,'Pathological regression (%)','group');if(anyDuplicated(names(raw))||!all(req%in%names(raw)))stop('All nine distinct source fields required, no annotation rows omitted',call.=FALSE);ID<-ids(raw$ID);if(nrow(raw)<2||nrow(raw)>24)stop('2–24 complete patient records required',call.=FALSE);response<-parse_numeric(raw[['Pathological regression (%)']],'Regression');group<-as.character(raw$group);if(any(!is.finite(response))||any(response< -100|response>0)||anyNA(group)||any(!group%in%c('MPR','non-MPR')))stop('Complete negative regression in [-100,0] and supplied MPR/non-MPR labels required',call.=FALSE)
allowed<-list(c('MSS','MSI-H'),c('Negative','Positive'),c('Negative','Positive'),c('SD','PR'),c('Intestinal/mixed','Diffused'),c('T4a','T4b'));M<-as.matrix(raw[features]);M[is.na(M)|M=='NA'|M=='']<-'Missing';for(j in 1:6)if(any(!M[,j]%in%c(allowed[[j]],'Missing')))stop(features[j],' contains unknown categorical values; no silent recoding',call.=FALSE)
patient_order<-choice('patient_order','source',c('source','regression'));ord<-if(patient_order=='source')seq_along(ID)else order(response,decreasing=TRUE);ID<-ID[ord];M<-M[ord,,drop=FALSE];response<-response[ord];group<-group[ord];width_mm<-num('width_mm',210,205,240);height_mm<-num('height_mm',180,175,205);font_size<-num('font_size',10.5,9.5,11.5);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice;dpi<-integer_param('dpi',600,96,1200);tile_fraction<-num('tile_fraction',.9,.8,.95);bar_fraction<-num('bar_fraction',.9,.75,.95);line_width<-num('line_width',.25,.15,.4);sw<-width_mm/210;sh<-height_mm/180;left<-50*sw;right<-204*sw;dx<-(right-left)/length(ID);heat_top<-153*sh;heat_bottom<-109*sh;dy<-(heat_top-heat_bottom)/6;bar_top<-104*sh;bar_bottom<-14*sh;bar_height<-bar_top-bar_bottom;xx<-left+(seq_along(ID)-.5)*dx;yy<-function(v)bar_top+v/100*bar_height
colors<-c('MSS'='#080922','MSI-H'='#C64543','Negative'='#080922','Positive'='#C64543','SD'='#080922','PR'='#C64543','Intestinal/mixed'='#842064','Diffused'='#E8E166','T4a'='#842064','T4b'='#E8E166','Missing'='#FFFFFF');gc<-c('MPR'='#FABE01','non-MPR'='#74C6BC');cells<-expand.grid(patient=seq_along(ID),feature=1:6);cells$ID<-ID[cells$patient];cells$feature_name<-features[cells$feature];cells$value<-as.vector(M);cells$fill<-unname(colors[cells$value]);cells$x_mm<-xx[cells$patient];cells$y_mm<-heat_top-(cells$feature-.5)*dy;cells$width_mm<-dx*tile_fraction;cells$height_mm<-dy*tile_fraction;bars<-data.frame(ID=ID,response=response,group=group,fill=unname(gc[group]),x_mm=xx,y_mm=(bar_top+yy(response))/2,width_mm=dx*bar_fraction,height_mm= -response/100*bar_height);missing<-cells[cells$value=='Missing',];caption<-c('Supplied response labels; negative values encode downward pathological regression.','Descriptive display; no between-group test or causal treatment-effect estimate.')
legend<-data.frame(title=c('MSI','PD-L1 / ERBB','RECIST',"Lauren classification",'T stage','Response'),x_mm=rep(c(50,101,155)*sw,2),y_mm=rep(c(175,164)*sh,each=3),v1=c('MSS','Negative','SD','Intestinal/mixed','T4a','non-MPR'),v2=c('MSI-H','Positive','PR','Diffused','T4b','MPR'));keys<-do.call(rbind,lapply(seq_len(6),function(i)data.frame(value=c(legend$v1[i],legend$v2[i]),x_mm=legend$x_mm[i]+c(2,27)*sw,y_mm=rep(legend$y_mm[i]-4*sh,2),width_mm=4*sw,height_mm=4*sh)));keys$fill<-unname(c(colors,gc)[keys$value])
sysfonts::font_add(family,regular=systemfonts::match_fonts(family)$path,bold=systemfonts::match_fonts(family,weight='bold')$path,italic=systemfonts::match_fonts(family,italic=TRUE)$path,bolditalic=systemfonts::match_fonts(family,weight='bold',italic=TRUE)$path);probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=s))),'mm',TRUE);if(max(vapply(features,tw,numeric(1),s=font_size))>left-4||max(vapply(ID,tw,numeric(1),s=9))>dx*.9||max(vapply(caption,tw,numeric(1),s=5.8))>width_mm-8)stop('Complete row/ID labels or caption require larger permitted canvas or smaller font',call.=FALSE);for(i in seq_len(6))if(tw(legend$v1[i],7.5)>22*sw||tw(legend$v2[i],7.5)>19*sw)stop('Full feature legend requires larger canvas',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(){grid::grid.newpage();mm<-function(z)grid::unit(z,'mm');txt<-function(z,x,y,s=font_size,face='plain',just='center',rot=0)grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=s,fontface=face,col='#111111'));rect<-function(d)grid::grid.rect(mm(d$x_mm),mm(d$y_mm),mm(d$width_mm),mm(d$height_mm),gp=grid::gpar(fill=d$fill,col=NA));ln<-function(x,y,w=line_width,col='#222222',lty='solid')grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=col,lwd=w*72.27/25.4,lty=lty));rect(cells);for(i in 1:6)txt(features[i],left-1.5,heat_top-(i-.5)*dy,just='right');if(nrow(missing))for(i in seq_len(nrow(missing))){r<-missing[i,];len<-min(r$width_mm,r$height_mm)*.46;ln(r$x_mm+c(-len,len),r$y_mm+c(-len,len),.45);ln(r$x_mm+c(-len,len),r$y_mm+c(len,-len),.45)};rect(bars);for(v in c(-50,-75,-90))ln(c(left,right),rep(yy(v),2),line_width,'#999999','dashed');ln(c(left,right,right,left,left),c(bar_top,bar_top,bar_bottom,bar_bottom,bar_top));for(v in c(0,-25,-50,-75,-100)){ln(c(left,left-1),rep(yy(v),2));txt(as.character(v),left-2,yy(v),9,just='right')};txt('Pathological regression (%)',8*sw,mean(c(bar_top,bar_bottom)),12,rot=90);for(i in seq_along(ID))txt(ID[i],xx[i],bar_bottom-3.2,9);rect(keys);for(i in 1:6){txt(legend$title[i],legend$x_mm[i],legend$y_mm[i],7.5,face='bold',just='left');txt(legend$v1[i],legend$x_mm[i]+4.4*sw,legend$y_mm[i]-4*sh,7.5,just='left');txt(legend$v2[i],legend$x_mm[i]+29.4*sw,legend$y_mm[i]-4*sh,7.5,just='left')};txt(caption[1],4,5.5,5.8,just='left');txt(caption[2],4,2.5,5.8,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())};for(k in c('cells','bars','missing','keys'))write.csv(get(k),file.path(output_dir,paste0('actual_',k,'.csv')),row.names=FALSE);saveRDS(list(raw=raw,M=M,ord=ord,cells=cells,bars=bars,missing=missing,keys=keys),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(data_path=data_path,sheet=input_sheet,IDs=ID,features=features,order=ord,patient_order=patient_order,colors=as.list(colors),group_colors=as.list(gc),width_mm=width_mm,height_mm=height_mm,font_size=font_size,family=family,tile_fraction=tile_fraction,bar_fraction=bar_fraction,line_width=line_width,geometry=list(left=left,right=right,heat_top=heat_top,heat_bottom=heat_bottom,dx=dx,dy=dy,bar_top=bar_top,bar_bottom=bar_bottom,bar_height=bar_height),caption=caption,scope='Source categorical and negative regression data, no group recomputation or causal efficacy inference; response thresholds descriptive'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',length(ID),'patients,',nrow(cells),'full clinical cells and',nrow(missing),'missing markers\n')
