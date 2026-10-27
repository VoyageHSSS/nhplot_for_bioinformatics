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



if(!requireNamespace('survival',quietly=TRUE))stop('survival required',call.=FALSE);data_path<-resolve_input(text_param('data_path','',TRUE),'assets/input.txt');raw<-read_table(data_path);id_field<-text_param('id_field','id');time_field<-text_param('time_field','futime');event_field<-text_param('event_field','fustat');variable<-text_param('variable','MATN3');fields<-c(id_field,time_field,event_field,variable);if(anyDuplicated(fields)||anyDuplicated(names(raw))||!all(fields%in%names(raw))||any(grepl('Case[ _:#-]*[0-9]|cover_[0-9]',fields,ignore.case=TRUE)))stop('Distinct non-debug ID/time/event/variable fields required',call.=FALSE);ids<-as.character(raw[[id_field]]);time<-parse_numeric(raw[[time_field]],'Time');event<-parse_numeric(raw[[event_field]],'Event');value<-parse_numeric(raw[[variable]],'Marker');if(anyNA(ids)||any(!nzchar(ids))||any(ids!=trimws(ids))||anyDuplicated(ids)||any(grepl('[[:cntrl:]]',ids))||any(!is.finite(c(time,event,value)))||any(time<0)||any(!event%in%c(0,1)))stop('All unique complete IDs, finite nonnegative time, binary 0/1 event and finite marker required; no filtering or time shift',call.=FALSE);split_quantile<-num('split_quantile',.5,.1,.9);cutoff<-as.numeric(quantile(value,split_quantile,type=7));group<-ifelse(value>cutoff,'High','Low');if(any(table(factor(group,levels=c('High','Low')))<2))stop('Both marker groups need at least two records',call.=FALSE);dat<-data.frame(ID=ids,time=time,event=event,value=value,group=group,stringsAsFactors=FALSE);dat<-dat[order(dat$group,dat$ID,method='radix'),];rownames(dat)<-NULL;groups<-c('High','Low');fit_list<-lapply(groups,function(g)survival::survfit(survival::Surv(time,event)~1,data=dat[dat$group==g,],conf.type='log',conf.int=.95,timefix=FALSE));names(fit_list)<-groups;km<-do.call(rbind,lapply(groups,function(g){z<-fit_list[[g]];data.frame(group=g,time=z$time,survival=z$surv,lower=z$lower,upper=z$upper,n_risk=z$n.risk,n_event=z$n.event,n_censor=z$n.censor)}));legacy<-if(sum(dat$event)>0)tryCatch(survival::survdiff(survival::Surv(time,event)~group,data=dat),error=function(e)NULL)else NULL;legacy_p<-if(is.null(legacy)||!is.finite(legacy$chisq))NULL else stats::pchisq(legacy$chisq,1,lower.tail=FALSE)
width_mm<-num('width_mm',150,145,185);height_mm<-num('height_mm',145,145,185);dpi<-integer_param('dpi',600,96,1200);font_size<-num('font_size',8.5,7.5,9.5);line_width<-num('line_width',.45,.3,.65);censor_size<-num('censor_size',1.2,.8,1.6);censor_width<-num('censor_width',.18,.12,.25);conf_int<-bool('conf_int',TRUE);conf_int_alpha<-num('conf_int_alpha',.16,.08,.25);break_time<-num('break_time',1,.25,5);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice;hex<-function(n,d){v<-toupper(text_param(n,d));if(!grepl('^#[0-9A-F]{6}$',v)||v=='#FFFFFF')stop(n,' needs visible #RRGGBB',call.=FALSE);v};cols<-c(High=hex('high_color','#C44959'),Low=hex('low_color','#356FA7'));if(cols[1]==cols[2])stop('Groups need distinct colors',call.=FALSE);x_title<-text_param('x_title','Follow-up time (supplied units)');if(grepl('Case[ _:#-]*[0-9]|cover_[0-9]',x_title,ignore.case=TRUE))stop('Internal axis title prohibited',call.=FALSE);sw<-width_mm/150;sh<-height_mm/145;left<-24*sw;right<-146*sw;bottom<-52*sh;top<-118*sh;xmax<-max(1,max(dat$time))*1.045;ticks<-seq(0,max(dat$time),by=break_time);if(length(ticks)>16)stop('Risk table needs fewer ticks or larger explicit break_time',call.=FALSE);xx<-function(t)left+t/xmax*(right-left);yy<-function(s)bottom+2*sh+s*(top-bottom-4*sh);risk<-do.call(rbind,lapply(seq_along(groups),function(i){g<-groups[i];data.frame(group=g,time=ticks,n_risk=vapply(ticks,function(t)sum(dat$group==g&dat$time>=t),integer(1)),x_mm=xx(ticks),y_mm=(35-(i-1)*7)*sh)}));curves<-list();bands<-list();censors<-list();for(g in groups){d<-km[km$group==g,];steps<-function(v)as.vector(rbind(c(1,head(v,-1)),v));tt<-c(0,as.vector(rbind(d$time,d$time)));sv<-c(1,steps(d$survival));curves[[g]]<-data.frame(group=g,time=tt,survival=sv,x_mm=xx(tt),y_mm=yy(sv),color=cols[[g]]);lo<-c(1,steps(d$lower));hi<-c(1,steps(d$upper));ok<-is.finite(lo)&is.finite(hi);runs<-split(which(ok),cumsum(c(TRUE,diff(which(ok))!=1)));bands[[g]]<-lapply(runs,function(idx){data.frame(group=g,x_mm=c(xx(tt[idx]),rev(xx(tt[idx]))),y_mm=c(yy(hi[idx]),rev(yy(lo[idx]))),color=cols[[g]])});c<-dat[dat$group==g&dat$event==0,];if(nrow(c))c$survival<-d$survival[match(c$time,d$time)]else c$survival<-numeric();c$x_mm<-xx(c$time);c$y_mm<-yy(c$survival);c$color<-rep(cols[[g]],nrow(c));censors[[g]]<-c};curves<-do.call(rbind,curves);censors<-do.call(rbind,censors);band_records<-do.call(rbind,lapply(groups,function(g){bs<-bands[[g]];do.call(rbind,lapply(seq_along(bs),function(i){z<-bs[[i]];z$segment<-i;z}))}));counts<-vapply(groups,function(g)sum(dat$group==g),integer(1));zero_times<-sum(dat$time==0);zero_events<-sum(dat$time==0&dat$event==1);title<-sprintf('%s groups: cutoff %.6g (quantile %.2g)',variable,cutoff,split_quantile);captions<-c(sprintf('%s; %d zero-time records (%d events) retained; > cutoff is High.',if(conf_int)'Pointwise 95% log CIs'else'CIs omitted',zero_times,zero_events),'Units / censoring / cutpoint design unverified; no certified p or clinical inference.');font_file<-function(weight='normal',italic=FALSE){hit<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(hit$index!=0L)hit<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);hit$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces));probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=s))),'mm',TRUE);if(tw(title,10)>width_mm-8||tw(x_title,9)>right-left||max(vapply(captions,tw,numeric(1),s=6))>width_mm-8||length(ticks)>1&&min(diff(xx(ticks)))<max(vapply(as.character(risk$n_risk),tw,numeric(1),s=font_size))+.8)stop('Full title/captions/risk counts need wider canvas or fewer ticks',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(){grid::grid.newpage();mm<-function(v)grid::unit(v,'mm');txt<-function(z,x,y,s=font_size,face='plain',just='center',rot=0,col='#111111')grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=s,fontface=face,col=col));ln<-function(x,y,lw=.25,col='#263238')grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=col,lwd=lw*72.27/25.4,lineend='butt'));txt(title,4,140*sh,10,'bold',just='left');for(i in 1:2){x<-(27+(i-1)*60)*sw;ln(c(x,x+6),rep(129*sh,2),line_width,cols[i]);txt(sprintf('%s (n = %d)',groups[i],counts[i]),x+8,129*sh,8,just='left')};if(conf_int)for(g in groups)for(b in bands[[g]])if(nrow(b)>=4)grid::grid.polygon(mm(b$x_mm),mm(b$y_mm),gp=grid::gpar(fill=cols[[g]],col=NA,alpha=conf_int_alpha));for(t in c(0,.25,.5,.75,1)){ln(c(left-1,left),rep(yy(t),2));txt(sprintf('%.2f',t),left-2,yy(t),8,just='right')};ln(rep(left,2),c(bottom,top));ln(c(left,right),rep(bottom,2));for(g in groups){d<-curves[curves$group==g,];ln(d$x_mm,d$y_mm,line_width,cols[[g]])};if(nrow(censors))for(i in seq_len(nrow(censors))){d<-censors[i,];ln(d$x_mm+c(-1,1)*censor_size/2,rep(d$y_mm,2),censor_width,d$color);ln(rep(d$x_mm,2),d$y_mm+c(-1,1)*censor_size/2,censor_width,d$color)};txt('Survival probability',6*sw,mean(c(bottom,top)),9,'bold',rot=90);for(t in ticks)ln(rep(xx(t),2),c(bottom,bottom-1));txt('Number at risk (just before time)',left,44*sh,8,'bold',just='left');for(i in 1:2)txt(groups[i],left-5,(35-(i-1)*7)*sh,font_size,'bold',just='right',col=cols[i]);for(i in seq_len(nrow(risk)))txt(risk$n_risk[i],risk$x_mm[i],risk$y_mm[i]);ln(c(left,right),rep(23*sh,2));for(t in ticks){ln(rep(xx(t),2),c(23,22)*sh);txt(format(t,trim=TRUE),xx(t),19*sh,8)};txt(x_title,mean(c(left,right)),12*sh,9,'bold');for(i in 1:2)txt(captions[i],4,(6.5-(i-1)*3.5)*sh,6,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())};write.csv(dat,file.path(output_dir,'actual_samples.csv'),row.names=FALSE);write.csv(km,file.path(output_dir,'actual_km.csv'),row.names=FALSE);write.csv(curves,file.path(output_dir,'actual_curves.csv'),row.names=FALSE);write.csv(band_records,file.path(output_dir,'actual_bands.csv'),row.names=FALSE);write.csv(censors,file.path(output_dir,'actual_censors.csv'),row.names=FALSE);write.csv(risk,file.path(output_dir,'actual_risk.csv'),row.names=FALSE);saveRDS(list(raw=raw,dat=dat,km=km,fit_list=fit_list,legacy=legacy),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(data_path=data_path,fields=as.list(fields),groups=as.list(groups),counts=as.list(counts),cutoff=cutoff,split_quantile=split_quantile,sample_count=nrow(dat),event_count=sum(dat$event),censor_count=nrow(censors),zero_times=zero_times,zero_events=zero_events,legacy_logrank_p=legacy_p,inferential_p_rendered=FALSE,ci_method='95% pointwise log / standard nonrobust KM Greenwood variance, usual limits; undefined bounds not fabricated',conf_int=conf_int,conf_int_alpha=conf_int_alpha,width_mm=width_mm,height_mm=height_mm,font_size=font_size,line_width=line_width,censor_size=censor_size,censor_width=censor_width,palette=as.list(unname(cols)),family=family,pdf_faces=pdf_faces,x_title=x_title,title=title,captions=as.list(captions),xmax=xmax,ticks=as.list(ticks),geometry=list(left=left,right=right,bottom=bottom,top=top,sw=sw,sh=sh),scope='All supplied records retained, no time shift or missing filtering. Marker quantile groups descriptive; independence/censoring/units/cutpoint design unverified, no clinical or certified p claim.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',nrow(dat),'complete records;',zero_times,'zero times retained\n')
