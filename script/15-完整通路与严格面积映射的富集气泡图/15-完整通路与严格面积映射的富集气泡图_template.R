#!/usr/bin/env Rscript

# Complete two-group descriptive distributions; patient overlap and inference limits explicit.
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
  if(ext=="xls"){magic<-readBin(path,"raw",n=8);binary<-identical(magic,as.raw(c(208,207,17,224,161,177,26,225)))||identical(head(magic,2),charToRaw("PK"));if(!binary)ext<-"txt" else if(identical(head(magic,2),charToRaw("PK")))ext<-"xlsx"}
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt", "md", "maf")) { seps<-c("\t",";",",");counts<-vapply(seps,function(sep){v<-tryCatch(utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE),error=function(e)0L);max(c(0L,v),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; fields<-utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE);fields<-fields[!is.na(fields)];if(!length(fields)||any(fields!=fields[1]))stop("Every table record must match the header field count",call.=FALSE); if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for XLS/XLSX input", call. = FALSE)
    as.data.frame((if(ext=="xlsx")readxl::read_xlsx else readxl::read_xls)(path,sheet=if(nzchar(text_param('sheet','',TRUE)))text_param('sheet','')else 1, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}



ids<-function(v,label){v<-as.character(v);if(anyNA(v)||any(!nzchar(v))||anyDuplicated(v)||any(v!=trimws(v))||any(grepl('[[:cntrl:]]',v))||any(grepl('Case[ _:#-]*[0-9]|cover_[0-9]',v,ignore.case=TRUE)))stop(label,' require unique complete exact non-debug labels',call.=FALSE);v}
data_path<-resolve_input(text_param('data_path','',TRUE),'assets/input.txt');raw<-read_table(data_path);fields<-c('Term','Ratio','Count','FDR');if(anyDuplicated(names(raw))||!all(fields%in%names(raw)))stop('Distinct Term/Ratio/Count/FDR fields required',call.=FALSE);terms<-ids(raw$Term,'Terms');if('ID'%in%names(raw))invisible(ids(raw$ID,'Pathway IDs'));if(nrow(raw)<1||nrow(raw)>45)stop('1–45 complete enrichment rows required',call.=FALSE);ratio<-parse_numeric(raw$Ratio,'Ratio');counts<-parse_numeric(raw$Count,'Count');q<-parse_numeric(raw$FDR,'FDR');if(any(!is.finite(c(ratio,counts,q)))||any(ratio<0|ratio>1)||any(counts<0|counts!=floor(counts)|counts>1e6)||max(counts)==0||any(q<0|q>1))stop('All complete Ratio/FDR in [0,1] and nonnegative integer Counts with one positive value required; no rows dropped',call.=FALSE);if('pvalue'%in%names(raw)){pv<-parse_numeric(raw$pvalue,'pvalue');if(any(!is.finite(pv))||any(pv<0|pv>1))stop('pvalue must be complete in [0,1]',call.=FALSE)}
color_scale<-choice('color_scale','linear',c('linear','log10'));if(color_scale=='log10'&&any(q==0))stop('log10 needs positive unrounded FDR; no pseudocount',call.=FALSE);font_size<-num('font_size',8.5,7.5,9);width_mm<-num('width_mm',210,205,240);height_mm<-num('height_mm',175,170,240);dpi<-integer_param('dpi',600,96,1200);max_diameter<-num('max_diameter',4.3,3,5.5);point_alpha<-num('point_alpha',1,.6,1);axis_width<-num('axis_width',.25,.15,.4);point_outline<-num('point_outline',.12,0,.25);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice;hex<-function(n,d){v<-toupper(text_param(n,d));if(!grepl('^#[0-9A-F]{6}$',v)||v=='#FFFFFF')stop(n,' must be visible #RRGGBB',call.=FALSE);v};low<-hex('low_color','#D73027');high<-hex('high_color','#4575B4');if(low==high)stop('Distinct color endpoints required',call.=FALSE)
ord<-order(-ratio,terms,method='radix');rt<-raw[ord,,drop=FALSE];rt$Ratio<-ratio[ord];rt$Count<-counts[ord];rt$FDR<-q[ord];sw<-width_mm/210;left<-85*sw;right<-173*sw;bottom<-20;top<-height_mm-10;dy<-(top-bottom)/nrow(rt);if(dy<max(max_diameter+.3,font_size*25.4/72*1.25))stop('All rows and maximum bubble require taller canvas or smaller diameter/font',call.=FALSE);rr<-range(ratio);span<-diff(rr);if(span==0)span<-max(.1,rr[1]*.2);xlim<-c(max(0,rr[1]-.12*span),rr[2]+.12*span);xx<-function(v)left+(v-xlim[1])/diff(xlim)*(right-left);xticks<-pretty(xlim,4);xticks<-xticks[xticks>=xlim[1]&xticks<=xlim[2]];ct<-if(color_scale=='linear')rt$FDR else log10(rt$FDR);ran<-range(ct);ramp<-colorRamp(c(low,high),space='Lab');color<-function(z){co<-ramp(z);rgb(co[,1],co[,2],co[,3],maxColorValue=255)};rt$fill<-color(if(diff(ran)==0)rep(.5,nrow(rt))else(ct-ran[1])/diff(ran));rt$x_mm<-xx(rt$Ratio);rt$y_mm<-top-(seq_len(nrow(rt))-.5)*dy;rt$radius_mm<-max_diameter/2*sqrt(rt$Count/max(counts));caption<-sprintf('%d supplied pathways; area scales with Count; universe / FDR method unverified; enrichment not rerun.',nrow(rt));lx<-181*sw;cb_bottom<-height_mm*.57;cb_height<-35;count_ys<-height_mm*.34-c(0,10,20);count_ticks<-pretty(c(0,max(counts)),3);count_ticks<-count_ticks[count_ticks>0&count_ticks<=max(counts)];if(!length(count_ticks))count_ticks<-max(counts);if(length(count_ticks)>3)count_ticks<-c(min(count_ticks),count_ticks[ceiling(length(count_ticks)/2)],max(count_ticks));count_ys<-height_mm*.34-(seq_along(count_ticks)-1)*10;legend_title<-paste0('FDR (',color_scale,')');legend_values<-if(diff(ran)==0)rt$FDR[1]else if(color_scale=='linear')pretty(range(q),4)else 10^seq(ceiling(min(ct)),floor(max(ct)));legend_values<-unique(legend_values[legend_values>=min(q)&legend_values<=max(q)]);if(!length(legend_values))legend_values<-range(q);legend_labels<-vapply(legend_values,function(v)if(color_scale=='linear')format(v,trim=TRUE,scientific=FALSE,digits=3)else format(v,trim=TRUE,scientific=TRUE,digits=2),character(1));font_file<-function(weight='normal',italic=FALSE){hit<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(hit$index!=0L)hit<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);hit$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces));probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=s))),'mm',TRUE);if(max(vapply(terms,tw,numeric(1),s=font_size))>left-5||tw(caption,6)>width_mm-8||max(vapply(c(legend_title,legend_labels),tw,numeric(1),s=8))>width_mm-lx-7||any(rt$x_mm-rt$radius_mm<left|rt$x_mm+rt$radius_mm>right))stop('Full labels/legend and unclipped bubbles require wider permitted canvas or smaller diameter/font',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(){grid::grid.newpage();mm<-function(v)grid::unit(v,'mm');txt<-function(z,x,y,s=font_size,face='plain',just='center')grid::grid.text(z,mm(x),mm(y),just=just,gp=grid::gpar(fontfamily=family,fontsize=s,fontface=face,col='#111111'));ln<-function(x,y,w=axis_width,col='#333333')grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=col,lwd=w*72.27/25.4));for(v in xticks)ln(rep(xx(v),2),c(bottom,top),.12,'#E6E9ED');for(y in rt$y_mm)ln(c(left,right),rep(y,2),.1,'#E6E9ED');ln(c(left,right,right,left,left),c(bottom,bottom,top,top,bottom));for(i in seq_len(nrow(rt))){if(rt$Count[i]>0)grid::grid.circle(mm(rt$x_mm[i]),mm(rt$y_mm[i]),mm(rt$radius_mm[i]),gp=grid::gpar(fill=rt$fill[i],col='#333333',alpha=point_alpha,lwd=point_outline*72.27/25.4));txt(rt$Term[i],left-1.5,rt$y_mm[i],just='right')};for(v in xticks){ln(rep(xx(v),2),c(bottom,bottom-1));txt(format(v,trim=TRUE),xx(v),bottom-4,8)};txt('Supplied gene ratio',mean(c(left,right)),bottom-11,9,'bold');txt(legend_title,lx,cb_bottom+cb_height+5,8,'bold',just='left');if(diff(ran)==0){grid::grid.rect(mm(lx+2),mm(cb_bottom+cb_height/2),mm(4),mm(4),gp=grid::gpar(fill=color(.5),col=NA,alpha=point_alpha));txt(legend_labels,lx+6,cb_bottom+cb_height/2,7,just='left');txt('Constant FDR',lx,cb_bottom+cb_height/2-5,7,just='left')}else{for(i in 1:256)grid::grid.rect(mm(lx+2),mm(cb_bottom+(i-.5)/256*cb_height),mm(4),mm(cb_height/256+.01),gp=grid::gpar(fill=color((i-.5)/256),col=NA,alpha=point_alpha));for(i in seq_along(legend_values)){v<-if(color_scale=='linear')legend_values[i]else log10(legend_values[i]);txt(legend_labels[i],lx+6,cb_bottom+(v-ran[1])/diff(ran)*cb_height,7,just='left')}};txt('Count',lx,count_ys[1]+9,8,'bold',just='left');for(i in seq_along(count_ticks)){grid::grid.circle(mm(lx+2),mm(count_ys[i]),mm(max_diameter/2*sqrt(count_ticks[i]/max(counts))),gp=grid::gpar(fill='#333333',col=NA));txt(count_ticks[i],lx+7,count_ys[i],7.5,just='left')};txt(caption,4,3,6,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())};write.csv(rt,file.path(output_dir,'actual_bubbles.csv'),row.names=FALSE);saveRDS(list(raw=raw,bubbles=rt,ord=ord),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(data_path=data_path,terms=rt$Term,order=ord,color_scale=color_scale,color_domain=ran,palette=c(low,high),count_max=max(counts),max_diameter=max_diameter,point_alpha=point_alpha,point_outline=point_outline,axis_width=axis_width,width_mm=width_mm,height_mm=height_mm,font_size=font_size,family=family,pdf_faces=pdf_faces,xlim=xlim,xticks=as.list(xticks),legend_title=legend_title,legend_values=as.list(legend_values),legend_labels=as.list(legend_labels),count_ticks=as.list(count_ticks),count_ys=as.list(count_ys),geometry=list(left=left,right=right,bottom=bottom,top=top,dy=dy,lx=lx,cb_bottom=cb_bottom,cb_height=cb_height),caption=caption,scope='Supplied summary only; no enrichment rerun. Pathway ontology/source, gene universe, ratio denominator and FDR adjustment family unverified. Area exactly proportional to Count, including no area for zero.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',nrow(rt),'full supplied rows; area proportional to Count\n')
