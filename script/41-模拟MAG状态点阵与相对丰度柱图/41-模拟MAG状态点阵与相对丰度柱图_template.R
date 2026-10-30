#!/usr/bin/env Rscript

# Supplied state matrix and keyed abundance, explicit tutorial simulation.
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
    as.data.frame(readxl::read_excel(path, sheet=input_sheet, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}

















input_sheet<-integer_param('input_sheet',1,1,100);data_path<-resolve_input(text_param('data_path','',TRUE),'assets/data-1.txt');abundance_path<-resolve_input(text_param('abundance_path','',TRUE),'assets/data-2.txt');raw<-read_table(data_path);abundance_raw<-read_table(abundance_path)
if(names(raw)[1]!='sample'||nrow(raw)<2||nrow(raw)>16||ncol(raw)<3||ncol(raw)>19||anyNA(names(raw))||any(!nzchar(names(raw)))||anyDuplicated(names(raw)))stop('State table requires unique sample first header, 2–16 rows and 2–18 unique MAG columns',call.=FALSE)
rows<-as.character(raw[[1]]);IDs<-names(raw)[-1];if(anyNA(rows)||any(!nzchar(trimws(rows)))||anyDuplicated(rows))stop('State row labels must be complete and unique',call.=FALSE)
S<-matrix(unlist(lapply(raw[-1],as.character)),nrow=nrow(raw),dimnames=list(rows,IDs));missing<-is.na(S)|S=='';S[missing]<-'';if(any(!S%in%c('Expressed','Gene encoded','')))stop('State values must be Expressed, Gene encoded, or blank/NA; unknown states cannot be silently blanked',call.=FALSE)
if(anyDuplicated(names(abundance_raw))||anyNA(names(abundance_raw))||any(!nzchar(names(abundance_raw)))||!all(c('sample','group','value')%in%names(abundance_raw)))stop('Abundance table requires unique sample/group/value headers',call.=FALSE)
bid<-as.character(abundance_raw$sample);if(anyNA(bid)||any(!nzchar(trimws(bid)))||anyDuplicated(bid)||length(bid)!=length(IDs)||!setequal(bid,IDs))stop('Abundance must contain exactly one record for every state-table MAG ID and no extras',call.=FALSE)
abundance<-abundance_raw[match(IDs,bid),,drop=FALSE];value<-parse_numeric(abundance$value,'Abundance');if(any(!is.finite(value))||any(value<0|value>100))stop('Abundance percentages must be finite between 0 and 100',call.=FALSE);groups<-as.character(abundance$group);if(anyNA(groups)||any(!nzchar(groups))||any(!groups%in%c('A','B')))stop('Tutorial grouping requires complete A/B values; no phylum inference',call.=FALSE)
width_mm<-num('width_mm',180,175,225);height_mm<-num('height_mm',144,140,180);dpi<-integer_param('dpi',600,96,1200);font_size<-num('font_size',11.5,10.5,12.5);point_size<-num('point_size',6.8,5,7.5);point_alpha<-num('point_alpha',1,.8,1);line_width<-num('line_width',.3,.2,.45);bar_width<-num('bar_width',.9,.7,.98);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
hex<-function(name,default){v<-text_param(name,default);if(!grepl('^#[0-9A-Fa-f]{6}$',v))stop(name,' must be #RRGGBB',call.=FALSE);toupper(v)};gc<-c(A=hex('primary_color','#E64B35'),B=hex('accent_color','#4DBBD5'));if(gc[1]==gc[2])stop('Two group colors must remain distinguishable',call.=FALSE)
sw<-width_mm/180;sh<-height_mm/144;left<-19*sw;right<-122*sw;top<-102*sh;bottom<-23*sh;dx<-(right-left)/length(IDs);dy<-(top-bottom)/length(rows);xx<-left+(seq_along(IDs)-.5)*dx;yy<-top-(seq_along(rows)-.5)*dy;if(point_size>min(dx,dy)*.85)stop('Point diameter too large for complete matrix; enlarge canvas or reduce point_size',call.=FALSE)
bar_bottom<-103.5*sh;bar_height<-29.5*sh;ymax<-max(3,max(value)*1.04);if(max(value)==0)ymax<-3;ticks<-pretty(c(0,ymax),n=3);ticks<-ticks[ticks>=0&ticks<=ymax]
cells<-expand.grid(row_index=seq_along(rows),id_index=seq_along(IDs));cells$row<-rows[cells$row_index];cells$ID<-IDs[cells$id_index];cells$status<-as.vector(S);cells$was_missing<-as.vector(missing);cells$x_mm<-xx[cells$id_index];cells$y_mm<-yy[cells$row_index];points<-cells[cells$status!='',];points$fill<-rep('#E5E5E5',nrow(points));points$stroke<-ifelse(points$status=='Expressed','#111111',NA_character_);points$radius_mm<-rep(point_size/2,nrow(points));bars<-data.frame(ID=IDs,group=groups,value=value,x_mm=xx,y_mm=bar_bottom+value/ymax*bar_height/2,width_mm=rep(dx*bar_width,length(IDs)),height_mm=value/ymax*bar_height,fill=unname(gc[groups]));demo_state<-read_table(file.path(script_dir,'assets/data-1.txt'));demo_abundance<-read_table(file.path(script_dir,'assets/data-2.txt'));D<-matrix(unlist(lapply(demo_state[-1],as.character)),nrow=nrow(demo_state),dimnames=list(demo_state[[1]],names(demo_state)[-1]));D[is.na(D)]<-'';is_demo<-setequal(rows,rownames(D))&&setequal(IDs,colnames(D))&&identical(unname(S),unname(D[rows,IDs,drop=FALSE]))&&identical(groups,as.character(demo_abundance$group[match(IDs,demo_abundance$sample)]))&&all(value==parse_numeric(demo_abundance$value,'Demo abundance')[match(IDs,demo_abundance$sample)]);caption<-if(is_demo)'Tutorial simulation; blank = not recorded; A/B groups as supplied.'else'Supplied assignments (origin unverified); blank = not recorded; A/B as supplied.'
sysfonts::font_add(family,regular=systemfonts::match_fonts(family)$path,bold=systemfonts::match_fonts(family,weight='bold')$path,italic=systemfonts::match_fonts(family,italic=TRUE)$path,bolditalic=systemfonts::match_fonts(family,weight='bold',italic=TRUE)$path);probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=s))),'mm',TRUE);if(max(vapply(rows,tw,numeric(1),s=font_size))>left-4||font_size*25.4/72*1.2>dy||max(vapply(IDs,tw,numeric(1),s=font_size))/sqrt(2)+font_size*25.4/72>bottom-6||max(vapply(c('Expressed','Gene encoded','Not recorded'),tw,numeric(1),s=font_size))>width_mm-143*sw-4||tw(caption,6)>width_mm-8)stop('Complete labels require larger permitted canvas or smaller font',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(){grid::grid.newpage();mm<-function(z)grid::unit(z,'mm');txt<-function(z,x,y,s=font_size,face='plain',just='center',rot=0)grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=s,fontface=face,col='#111111'));ln<-function(x,y,w=line_width,col='#888888')grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=col,lwd=w*72.27/25.4,lineend='butt'));for(x in seq(left,right,length.out=length(IDs)+1))ln(c(x,x),c(bottom,top));for(y in seq(bottom,top,length.out=length(rows)+1))ln(c(left,right),c(y,y));if(nrow(points))grid::grid.circle(mm(points$x_mm),mm(points$y_mm),mm(points$radius_mm),gp=grid::gpar(fill=grDevices::adjustcolor(points$fill,alpha.f=point_alpha),col=grDevices::adjustcolor(points$stroke,alpha.f=point_alpha),lwd=.32*72.27/25.4));for(i in seq_along(rows))txt(rows[i],left-1.5,yy[i],face='bold',just='right');for(i in seq_along(IDs))txt(IDs[i],xx[i],bottom-1.5,font_size,just='right',rot=45);grid::grid.rect(mm(bars$x_mm),mm(bars$y_mm),mm(bars$width_mm),mm(bars$height_mm),gp=grid::gpar(fill=bars$fill,col=NA));ln(c(left,right),rep(bar_bottom,2),.3,'#111111');ln(rep(left,2),c(bar_bottom,bar_bottom+bar_height),.3,'#111111');for(v in ticks){y<-bar_bottom+v/ymax*bar_height;ln(c(left,left-1),c(y,y),.3,'#111111');txt(format(v,trim=TRUE),left-2,y,font_size,face='bold',just='right')};txt('Condition MAG',46*sw,134*sh,13.5);txt('relative abundance (%)',46*sw,127.5*sh,13.5)
 for(i in 1:2){y<-if(i==1)73*sh else 61*sh;grid::grid.circle(mm(134*sw),mm(y),mm(point_size/2),gp=grid::gpar(fill=grDevices::adjustcolor('#E5E5E5',alpha.f=point_alpha),col=if(i==1)grDevices::adjustcolor('#111111',alpha.f=point_alpha)else NA,lwd=.32*72.27/25.4));txt(c('Expressed','Gene encoded')[i],143*sw,y,font_size,face='bold',just='left')};grid::grid.rect(mm(134*sw),mm(49*sh),mm(5),mm(5),gp=grid::gpar(fill='white',col='#888888',lwd=line_width*72.27/25.4));txt('Not recorded',143*sw,49*sh,font_size,just='left');txt('Supplied group',132*sw,122*sh,10.5,face='bold',just='left');for(i in 1:2){y<-(115-(i-1)*9)*sh;grid::grid.rect(mm(134*sw),mm(y),mm(4),mm(4),gp=grid::gpar(fill=gc[i],col=NA));txt(names(gc)[i],143*sw,y,10.5,just='left')};txt(caption,4,3.2,6,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())};for(k in c('cells','points','bars'))write.csv(get(k),file.path(output_dir,paste0('actual_',k,'.csv')),row.names=FALSE);saveRDS(list(raw=raw,abundance_raw=abundance_raw,abundance=abundance,S=S,missing=missing,cells=cells,points=points,bars=bars),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(data_path=data_path,abundance_path=abundance_path,input_sheet=input_sheet,rows=rows,IDs=IDs,states=as.list(table(factor(S,levels=c('Expressed','Gene encoded','')))),groups=as.list(gc),is_demo=is_demo,caption=caption,labels=labels<-c(rows,IDs,'Expressed','Gene encoded','Not recorded','Supplied group','A','B','Condition MAG','relative abundance (%)'),width_mm=width_mm,height_mm=height_mm,font_size=font_size,family=family,point_size=point_size,point_alpha=point_alpha,line_width=line_width,bar_width=bar_width,ymax=ymax,ticks=ticks,geometry=list(left=left,right=right,top=top,bottom=bottom,dx=dx,dy=dy,bar_bottom=bar_bottom,bar_height=bar_height),scope='Tutorial explicitly simulated; MAG IDs are columns, genes are rows; blank not recorded, no independent samples/statistical inference; phylum column retained without assigning A/B phyla'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',nrow(cells),'full states,',nrow(points),'state points and',nrow(bars),'keyed bars\n')
