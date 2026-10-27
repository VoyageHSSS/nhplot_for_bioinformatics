#!/usr/bin/env Rscript

# Complete grouped x/y observations with descriptive group/pooled OLS and optional Pearson teaching inference.
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
required_packages <- c("jsonlite", "svglite", "ragg", "readxl", "showtext", "sysfonts", "systemfonts", "scales")
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
read_table <- function(path, sheet=1L) {
  ext <- tolower(tools::file_ext(path))
  if(ext=="xls"){magic<-readBin(path,"raw",n=8);binary<-identical(magic,as.raw(c(208,207,17,224,161,177,26,225)))||identical(head(magic,2),charToRaw("PK"));if(identical(head(magic,2),charToRaw("PK")))ext<-"xlsx"else if(!binary)stop("XLS input must contain an actual workbook",call.=FALSE)}
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt")) { seps<-c("\t",";",",");counts<-vapply(seps,function(sep){v<-tryCatch(utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE),error=function(e)0L);max(c(0L,v),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; fields<-utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE);fields<-fields[!is.na(fields)];if(!length(fields)||any(fields!=fields[1]))stop("Every table record must match the header field count",call.=FALSE); if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for XLS/XLSX input", call. = FALSE)
    as.data.frame((if(ext=="xlsx")readxl::read_xlsx else readxl::read_xls)(path, sheet=sheet, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}

data_path<-resolve_input(text_param('data_path','assets/data.txt'),'assets/data.txt');if(dir.exists(data_path))stop('Table file required',call.=FALSE);sheet<-integer_param('sheet',1,1,20);raw<-read_table(data_path,sheet)
if(!nrow(raw)||nrow(raw)>2000||anyDuplicated(names(raw))||!all(c('group','x','y')%in%names(raw))||'source_row'%in%names(raw))stop('Complete group/x/y table,1..2000 rows required; source_row reserved',call.=FALSE)
observations<-data.frame(source_row=seq_len(nrow(raw)),group=as.character(raw$group),x=parse_numeric(raw$x,'x'),y=parse_numeric(raw$y,'y'))
if(anyNA(observations$group)||any(!nzchar(observations$group))||any(observations$group!=trimws(observations$group))||any(!is.finite(observations$x))||any(!is.finite(observations$y)))stop('Complete unpadded groups and finite x/y required; no excluded rows',call.=FALSE)
if('sample'%in%names(raw)){observations$sample<-as.character(raw$sample);if(anyNA(observations$sample)||any(!nzchar(observations$sample))||any(observations$sample!=trimws(observations$sample))||anyDuplicated(observations[c('group','sample')]))stop('Complete sample IDs unique within group required',call.=FALSE)}
groups<-trimws(strsplit(text_param('group_order','A,B,C,D,E'),',',fixed=TRUE)[[1]]);if(!length(groups)||length(groups)>6||any(!nzchar(groups))||anyDuplicated(groups)||!setequal(groups,observations$group))stop('group_order must list every observed group exactly once,1..6 groups',call.=FALSE)
if(any(grepl('cover_[0-9]|Case[ _:#-]*[0-9]|案例[ ]*[0-9]',groups,ignore.case=TRUE)))stop('Internal identifiers cannot be displayed',call.=FALSE)
observations$group_id<-match(observations$group,groups);observations<-observations[order(observations$group_id,observations$x,observations$y,if('sample'%in%names(observations))observations$sample else observations$source_row,method='radix'),,drop=FALSE];rownames(observations)<-NULL
width_mm<-num('width_mm',220,190,300);height_mm<-num('height_mm',143,135,220);dpi<-integer_param('dpi',600,96,1200)
point_diameter<-num('point_diameter_mm',2,.9,3);top_alpha<-num('facet_point_alpha',.5,.3,1);pool_alpha<-num('pooled_point_alpha',.7,.3,1);group_line_width<-num('group_line_width',.6,.4,1);pool_line_width<-num('pooled_line_width',.75,.5,1.2);axis_width<-num('axis_width',.35,.25,.6);facet_gap<-num('facet_gap_mm',1.5,.8,3)
show_summary<-bool('show_summary',TRUE);inference_mode<-choice('inference_mode','descriptive',c('descriptive','pearson_demo'));p_adjust_mode<-choice('p_adjust_mode','holm',c('holm','none'));test_family<-choice('test_family','all_panels',c('all_panels','groups_only','pooled_only'))
tick_size<-num('axis_text_size_pt',11,9,13);summary_size<-num('summary_size_pt',9.5,8,11);header_size<-num('header_size_pt',10.5,9,12);caption_size<-num('caption_size_pt',7.5,7,9);axis_size<-num('axis_title_size_pt',11.5,9,14);title_size<-num('title_size_pt',14,11,18);family_choice<-choice('font_family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
x_title<-text_param('x_title','',TRUE);y_title<-text_param('y_title','',TRUE);title<-text_param('title','',TRUE);for(n in c('x_title','y_title','title'))if(grepl('cover_[0-9]|Case[ _:#-]*[0-9]|案例[ ]*[0-9]',get(n),ignore.case=TRUE))assign(n,'')
colors<-toupper(c(text_param('group_color_1','#1E90FF'),text_param('group_color_2','#CB78A6'),text_param('group_color_3','#E6CF00'),text_param('group_color_4','#FE0076'),text_param('group_color_5','#28E5A1'),text_param('group_color_6','#8C77B5')));pool_color<-toupper(text_param('pooled_fit_color','#1E90FF'));if(any(!grepl('^#[0-9A-F]{6}$',c(colors,pool_color)))||any(c(colors,pool_color)=='#FFFFFF')||anyDuplicated(colors))stop('Distinct visible HEX group colors and visible pooled color required',call.=FALSE);group_colors<-plot_nh_format_palette(setNames(colors[seq_along(groups)],groups), tryCatch(as.character(groups), error = function(e) NULL), cfg)
model<-function(z,panel,group){n<-nrow(z);mx<-mean(z$x);my<-mean(z$y);xc<-z$x-mx;yc<-z$y-my;if(any(!is.finite(c(mx,my,xc,yc))))stop('Model exceeds finite precision',call.=FALSE);xs<-max(abs(xc));ys<-max(abs(yc));u<-if(xs>0)xc/xs else rep(0,n);v<-if(ys>0)yc/ys else rep(0,n);sxx<-sum(u*u);syy<-sum(v*v);sxy<-sum(u*v);fa<-n>=2&&xs>0;ca<-fa&&ys>0;b<-if(fa)if(sxy==0||ys==0)0 else(sxy/sxx)*ys/xs else NA_real_;a<-if(fa)my-b*mx else NA_real_;r<-if(ca)sxy/sqrt(sxx*syy)else NA_real_;if(ca){if(abs(r)>1+1e-12)stop('Pearson r exceeds finite precision',call.=FALSE);r<-max(-1,min(1,r))};if(fa&&any(!is.finite(c(a,b))))stop('OLS coefficients exceed finite precision',call.=FALSE)
 data.frame(panel=panel,group=group,n=n,x_mean=mx,y_mean=my,x_scale=xs,y_scale=ys,sxx_scaled=sxx,syy_scaled=syy,sxy_scaled=sxy,fit_available=fa,cor_available=ca,fit_reason=if(n<2)'n<2'else if(xs==0)'constant x'else'available',cor_reason=if(n<2)'n<2'else if(xs==0||ys==0)'constant x/y'else'available',intercept=a,slope=b,r=r,p_raw=NA_real_,p_adjusted=NA_real_,t=NA_real_,df=NA_real_,test_id=NA_integer_)}
summary<-do.call(rbind,c(lapply(seq_along(groups),function(i)model(observations[observations$group_id==i,],paste0('group_',i),groups[i])),list(model(observations,'pooled',''))));tests<-data.frame()
if(inference_mode=='pearson_demo'){
 selected<-if(test_family=='groups_only')seq_along(groups)else if(test_family=='pooled_only')nrow(summary)else seq_len(nrow(summary));keys<-character();rows<-list()
 for(i in selected){s<-summary[i,];z<-if(s$panel=='pooled')observations else observations[observations$group==s$group,];if(nrow(z)<3||!s$cor_available)stop('Pearson teaching tests require n>=3 and varying x/y in every selected panel',call.=FALSE);key<-paste(sort(z$source_row),collapse=',');id<-match(key,keys);if(is.na(id)){ct<-stats::cor.test(z$x,z$y,method='pearson',alternative='two.sided');if(!is.finite(ct$p.value)||!is.finite(ct$parameter)||ct$p.value<0||ct$p.value>1)stop('Pearson test undefined',call.=FALSE);keys<-c(keys,key);id<-length(keys);rows[[id]]<-data.frame(test_id=id,n=nrow(z),r=unname(ct$estimate),t=unname(ct$statistic),df=unname(ct$parameter),p_raw=ct$p.value)};summary$test_id[i]<-id}
 tests<-do.call(rbind,rows);tests$p_adjusted<-stats::p.adjust(tests$p_raw,method=p_adjust_mode);for(i in selected){j<-summary$test_id[i];summary$p_raw[i]<-tests$p_raw[j];summary$p_adjusted[i]<-tests$p_adjusted[j];summary$t[i]<-tests$t[j];summary$df[i]<-tests$df[j]}
}
font_file<-function(){h<-systemfonts::match_fonts(family);if(h$index!=0L){family<<-'Arial';h<-systemfonts::match_fonts(family)};h$path}
regular_file<-font_file();sysfonts::font_add(family,regular=regular_file);text_font_files<-setNames(list(regular_file),family)
text_font_for<-function(z){chars<-unique(strsplit(enc2utf8(as.character(z)),'',fixed=TRUE)[[1]]);supports<-function(f)all(systemfonts::glyph_info(chars,path=text_font_files[[f]],index=0)$index>0);if(supports(family))return(family);fallback<-'Arial Unicode MS';if(is.null(text_font_files[[fallback]])){h<-systemfonts::match_fonts(fallback);if(h$index!=0)stop('Unicode font must have addressable regular face',call.=FALSE);text_font_files[[fallback]]<<-h$path;sysfonts::font_add(fallback,regular=h$path)};if(!supports(fallback))stop('Font lacks displayed glyph: ',z,call.=FALSE);fallback}
probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()
tw<-function(z,s,fontfamily=text_font_for(z))grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=fontfamily,fontsize=s))),'mm',TRUE)
text_height<-function(z,s,ff=text_font_for(z))s*25.4/72*if(ff=='Arial Unicode MS')1.5 else 1.15
fmt<-function(v)if(max(abs(v))>=1e5||max(abs(v))<.001)vapply(v,function(z)sprintf('%.3g',z),character(1))else format(v,trim=TRUE,digits=4,scientific=FALSE)


lines<-list();points<-list();labels<-list();panels<-list();keys<-list();fits<-list()
label<-function(z,x,y,size,role,panel='',group='',just='centre',rotation=0){if(!nzchar(z))return();labels[[length(labels)+1]]<<-data.frame(label=as.character(z),x_mm=unname(x),y_mm=unname(y),size=size,role=role,panel=panel,group=group,just=just,color='#000000',rotation=rotation,face='plain',family=text_font_for(z))}
line<-function(x,y,role,panel,group='',color='#000000',lw=axis_width){lines[[length(lines)+1]]<<-data.frame(line_id=length(lines)+1,vertex=seq_along(x),x_mm=unname(x),y_mm=unname(y),color=color,width=lw,role=role,panel=panel,group=group)}
short<-function(x,d=3)formatC(x,format='g',digits=d);eq<-function(s)if(!s$fit_available)paste0('OLS unavailable (',s$fit_reason,')')else if(s$slope==0)paste0('y = ',short(s$intercept))else paste0('y = ',short(s$intercept),if(s$slope<0)' - 'else' + ',short(abs(s$slope)),'x');rlab<-function(s)paste0(if(s$cor_available)paste0('r = ',short(s$r))else'r unavailable',if(is.finite(s$p_adjusted))if(s$p_adjusted==0)'; p < 2.23e-308'else paste0('; p = ',short(s$p_adjusted,2))else'')
tryCatch({
 caption<-paste0(if(inference_mode=='descriptive')'Descriptive OLS'else paste0('Pearson demo (',p_adjust_mode,'); ',test_family),'; pooled slope includes between-group differences')
 summary_h<-text_height('Pooled',summary_size);cor_gap<-max(7.7,1.2+text_height('0',tick_size)+.8+summary_h/2);eq_gap<-cor_gap+max(4.5,summary_h+.6);row_gap<-max(17,eq_gap+summary_h/2+1.3)
 caption_h<-text_height(caption,caption_size);caption_y<-max(1.8,caption_h/2+.25);pool_summary_y<-caption_y+caption_h/2+text_height('Pooled',summary_size)/2+.4
 pool_bottom<-max(13,pool_summary_y+text_height('Pooled',summary_size)/2+text_height('0',tick_size)+1.3+if(nzchar(x_title))text_height(x_title,axis_size)+1 else 0)
 left<-max(.04*width_mm,max(vapply(fmt(c(min(observations$y),max(observations$y))),tw,numeric(1),s=tick_size))+3+if(nzchar(y_title))text_height(y_title,axis_size)+1 else 0);right<-width_mm-2;pw<-(right-left-(length(groups)-1)*facet_gap)/length(groups);top_top<-height_mm-max(7,text_height('A',header_size)+2)-if(nzchar(title))text_height(title,title_size)+2 else 0;top_bottom<-height_mm*.565;pool_top<-top_bottom-if(show_summary)row_gap else 7
 if(pw<28||top_top-top_bottom<45||pool_top-pool_bottom<40)stop('Canvas too small for readable facet geometry',call.=FALSE)
 range_domain<-function(z,pad){a<-range(z);s<-diff(a);if(!is.finite(s))stop('Numeric domain exceeds finite precision',call.=FALSE);if(s==0){s<-max(1,abs(a[1]));a<-a+c(-.5,.5)*s};a+c(-pad,pad)*s}
 pad_x<-max(.05,(point_diameter/2+.75*axis_width/2+.2)/(pw-point_diameter-.75*axis_width-.4));pad_y<-max(.05,(point_diameter/2+.75*axis_width/2+.2)/(min(top_top-top_bottom,pool_top-pool_bottom)-point_diameter-.75*axis_width-.4));xd<-range_domain(observations$x,pad_x);fit_extents<-unlist(lapply(seq_len(nrow(summary)),function(i){s<-summary[i,];if(!s$fit_available)return(numeric());z<-if(s$panel=='pooled')observations else observations[observations$group==s$group,];s$y_mean+s$slope*(range(z$x)-s$x_mean)}));if(any(!is.finite(fit_extents)))stop('OLS prediction exceeds finite precision',call.=FALSE);yd<-range_domain(c(observations$y,fit_extents),pad_y);if(any(!is.finite(c(xd,yd,diff(xd),diff(yd)))))stop('Numeric domain exceeds finite precision',call.=FALSE)
 xt<-scales::breaks_extended(n=5,Q=c(2,4,1,5,2.5))(xd);xt<-xt[xt>xd[1]&xt<xd[2]];yt<-scales::breaks_extended(n=5,Q=c(2,4,1,5,2.5))(yd);yt<-yt[yt>yd[1]&yt<yd[2]];top_xt<-xt
 while(length(top_xt)>2&&min(diff(top_xt))/diff(xd)*pw<max(vapply(fmt(top_xt),tw,numeric(1),s=tick_size))+1){top_xt<-top_xt[seq(1,length(top_xt),by=2)]}
 while(length(groups)>1&&length(top_xt)>1&&pw+facet_gap-diff(range(top_xt))/diff(xd)*pw<(tw(fmt(tail(top_xt,1)),tick_size)+tw(fmt(head(top_xt,1)),tick_size))/2+.8){top_xt<-head(top_xt,-1)}
 for(i in seq_len(nrow(summary))){s<-summary[i,];pooled<-s$panel=='pooled';l<-if(pooled)left else left+(i-1)*(pw+facet_gap);r<-if(pooled)right else l+pw;b<-if(pooled)pool_bottom else top_bottom;t<-if(pooled)pool_top else top_top;panels[[i]]<-data.frame(panel=s$panel,group=s$group,left_mm=l,right_mm=r,bottom_mm=b,top_mm=t,xmin=xd[1],xmax=xd[2],ymin=yd[1],ymax=yd[2]);fx<-function(v)l+(v-xd[1])/diff(xd)*(r-l);fy<-function(v)b+(v-yd[1])/diff(yd)*(t-b);z<-if(pooled)observations else observations[observations$group==s$group,];points[[i]]<-data.frame(z,panel=s$panel,x_mm=fx(z$x),y_mm=fy(z$y),diameter_mm=point_diameter,color=unname(group_colors[z$group]),alpha=if(pooled)pool_alpha else top_alpha,role='observation')
 if(s$fit_available){grid<-seq(min(z$x),max(z$x),length.out=80);pred<-s$y_mean+s$slope*(grid-s$x_mean);if(any(!is.finite(pred)))stop('OLS prediction exceeds finite precision',call.=FALSE);fits[[length(fits)+1]]<-data.frame(panel=s$panel,group=s$group,grid=seq_along(grid),x=grid,y=pred,x_mm=fx(grid),y_mm=fy(pred),color=if(pooled)pool_color else group_colors[[s$group]]);line(fx(grid),fy(pred),'fit',s$panel,s$group,color=if(pooled)pool_color else group_colors[[s$group]],lw=if(pooled)pool_line_width else group_line_width)}
 if(pooled||i==1)line(c(l,l,r),c(t,b,b),'axes',s$panel,s$group)else line(c(l,r),c(b,b),'axes',s$panel,s$group)
 for(v in if(pooled)xt else top_xt){line(rep(fx(v),2),c(b-.7,b),'xtick',s$panel,s$group);label(fmt(v),fx(v),b-1.2-text_height(fmt(v),tick_size)/2,tick_size,'xtick',s$panel,s$group)}
 if(pooled||i==1)for(v in yt){line(c(l-.7,l),rep(fy(v),2),'ytick',s$panel,s$group);label(fmt(v),l-1.4,fy(v),tick_size,'ytick',s$panel,s$group,just='right')}
 if(!pooled){head<-paste0(s$group,' (n=',s$n,')');hw<-tw(head,header_size);keys[[i]]<-data.frame(panel=s$panel,group=s$group,x_mm=mean(c(l,r))-hw/2-2.2,y_mm=t+2.5,diameter_mm=1.8,color=group_colors[[s$group]],alpha=1,role='legend_key');label(head,mean(c(l,r)),t+2.5,header_size,'header',s$panel,s$group);if(show_summary){label(rlab(s),l,b-cor_gap,summary_size,'correlation',s$panel,s$group,just='left');label(eq(s),l,b-eq_gap,summary_size,'equation',s$panel,s$group,just='left')}}
 else if(show_summary)label(paste0('Pooled: ',rlab(s),'; ',eq(s)),mean(c(l,r)),pool_summary_y,summary_size,'pooled_summary',s$panel)
 }
 label(caption,mean(c(left,right)),caption_y,caption_size,'caption');if(nzchar(x_title))label(x_title,mean(c(left,right)),pool_bottom-text_height('0',tick_size)-text_height(x_title,axis_size)/2-2,axis_size,'x_title');if(nzchar(y_title))label(y_title,text_height(y_title,axis_size)/2+.25,mean(c(pool_bottom,top_top)),axis_size,'y_title',rotation=90);if(nzchar(title))label(title,mean(c(left,right)),height_mm-text_height(title,title_size)/2-.5,title_size,'title')
 labels<-do.call(rbind,labels);ww<-vapply(seq_len(nrow(labels)),function(i)tw(labels$label[i],labels$size[i],fontfamily=labels$family[i]),numeric(1));hh<-vapply(seq_len(nrow(labels)),function(i)text_height(labels$label[i],labels$size[i],labels$family[i]),numeric(1));boxes<-list()
 for(i in seq_len(nrow(labels))){z<-labels[i,];w<-if(z$rotation==90)hh[i]else ww[i];h<-if(z$rotation==90)ww[i]else hh[i];l<-z$x_mm-if(z$just=='right')w else if(z$just=='centre')w/2 else 0;b<-c(l,l+w,z$y_mm-h/2,z$y_mm+h/2);if(b[1]<.15||b[2]>width_mm-.15||b[3]<.15||b[4]>height_mm-.15)stop('Text outside canvas: ',z$label,call.=FALSE);if(length(boxes)&&any(vapply(boxes,function(q)b[1]<q[2]-.05&&b[2]>q[1]+.05&&b[3]<q[4]-.05&&b[4]>q[3]+.05,logical(1))))stop('Text collision: ',z$label,call.=FALSE);boxes[[i]]<-b}
 label_geometry<-cbind(labels,data.frame(left_mm=vapply(boxes,'[',numeric(1),1),right_mm=vapply(boxes,'[',numeric(1),2),bottom_mm=vapply(boxes,'[',numeric(1),3),top_mm=vapply(boxes,'[',numeric(1),4)))
},finally={dev.off();unlink(probe)})
points<-do.call(rbind,points);panels<-do.call(rbind,panels);keys<-do.call(rbind,keys);lines<-do.call(rbind,lines);fits<-if(length(fits))do.call(rbind,fits)else data.frame()
plot_nh_parameter_context(cfg, plot_nh_format_output(cfg), groups = groups, palette = group_colors)
render<-function(){grid::grid.newpage();mm<-function(v)grid::unit(v,'mm')
 for(i in seq_len(nrow(points))){z<-points[i,];grid::grid.circle(mm(z$x_mm),mm(z$y_mm),r=mm(z$diameter_mm/2),gp=grid::gpar(fill=grDevices::adjustcolor(z$color,alpha.f=z$alpha),col=NA))}
 for(id in unique(lines$line_id)){z<-lines[lines$line_id==id,];grid::grid.lines(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(col=z$color[1],lwd=z$width[1]*72.27/25.4,lineend='butt'))}
 for(i in seq_len(nrow(keys))){z<-keys[i,];grid::grid.circle(mm(z$x_mm),mm(z$y_mm),r=mm(z$diameter_mm/2),gp=grid::gpar(fill=z$color,col=NA))}
 for(i in seq_len(nrow(labels))){z<-labels[i,];grid::grid.text(z$label,mm(z$x_mm),mm(z$y_mm),just=z$just,rot=z$rotation,gp=grid::gpar(fontfamily=z$family,fontsize=z$size,col=z$color))}
}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
tables<-c('observations','summary','tests','fits','panels','lines','points','keys','labels','label_geometry');for(n in tables)write.csv(get(n),file.path(output_dir,paste0('actual_',n,'.csv')),row.names=FALSE);saveRDS(mget(c('raw',tables)),file.path(output_dir,'parsed-analysis.RDS'))
jsonlite::write_json(list(raw_rows=nrow(raw),groups=as.list(groups),width_mm=width_mm,height_mm=height_mm,dpi=dpi,x_domain=as.list(xd),y_domain=as.list(yd),top_x_ticks=as.list(top_xt),pooled_x_ticks=as.list(xt),y_ticks=as.list(yt),point_diameter_mm=point_diameter,facet_point_alpha=top_alpha,pooled_point_alpha=pool_alpha,group_line_width=group_line_width,pooled_line_width=pool_line_width,axis_width=axis_width,facet_gap_mm=facet_gap,show_summary=show_summary,inference_mode=inference_mode,p_adjust_mode=p_adjust_mode,test_family=test_family,family=family,text_font_files=text_font_files,colors=as.list(group_colors),pooled_fit_color=pool_color,scope='TXT/XLSX Sheet1 identical500 rows,100each A-E;provenance/units/independence uncertified. All rows/extra fields/source_row and exact x/y preserved without jitter. Descriptive centered/scaled OLS and Pearson r only;constant x or n<2 has no invented fit,constant y has genuine zero-slope OLS but undefined correlation. Lines stop at observed x ranges,no extrapolation. Pooled association includes between-group composition;source covariance99.8766% between groups,not certified within effect or causality. Optional Pearson teaching inference requires n>=3 and varying x/y in each selected planned family,Holm or none;duplicate identical pooled/single-group tests deduplicated. Invalid/undefined tests fail,not NS. White background,headers define group color mapping and statistics separate from data,no raster gradient or overlapping legend. Shared native PNG/PDF/SVG.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15)
cat('Rendered',nrow(raw),'unchanged x/y observations;',inference_mode,'; shared-domain group and pooled descriptive OLS\n')
