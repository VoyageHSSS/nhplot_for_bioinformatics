#!/usr/bin/env Rscript

# Actual supplied enrichment Count/FDR rows, explicit source order and color scale.
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
















input_sheet<-integer_param('input_sheet',1,1,100);data_path<-resolve_input(text_param('data_path','',TRUE),'assets/data.txt');raw<-read_table(data_path)
if(ncol(raw)<3||ncol(raw)>5||names(raw)[1]!='Type'||anyNA(names(raw))||any(!nzchar(names(raw)))||anyDuplicated(names(raw)))stop('Require Type and 2–4 unique treatment columns',call.=FALSE)
if(nrow(raw)<2||anyNA(raw$Type)||any(!nzchar(trimws(raw$Type))))stop('Timepoint labels must be complete',call.=FALSE)
M<-matrix(unlist(lapply(raw[-1],parse_numeric,label='Observations')),nrow=nrow(raw),dimnames=list(NULL,names(raw)[-1]));if(any(!is.finite(M))||any(M<0))stop('All observations must be finite nonnegative counts, including unselected timepoints',call.=FALSE)
timepoint<-text_param('timepoint','Day 5');if(!timepoint%in%raw$Type)stop('Requested timepoint not found; no silent fallback',call.=FALSE);selected<-which(raw$Type==timepoint);if(length(selected)<2)stop('Every selected treatment needs at least two observations for SD and ANOVA',call.=FALSE)
labels<-names(raw)[-1];ng<-length(labels);ids<-paste0('G',seq_len(ng));display<-trimws(sub(' CAR T$','',labels));if(anyDuplicated(display)||any(!nzchar(display)))stop('Display treatment labels must remain unique',call.=FALSE)
values<-M[selected,,drop=FALSE];long<-data.frame(timepoint=timepoint,source_row=rep(selected,ng),group_id=rep(ids,each=length(selected)),group=rep(labels,each=length(selected)),value=as.vector(values));long$factor<-factor(long$group_id,levels=ids)
if(sum(apply(values,2,var))<=0)stop('ANOVA requires actual within-group variation; floating-point residuals cannot create significance',call.=FALSE)
fit<-stats::aov(value~factor,data=long);anova<-summary(fit)[[1]];if(!is.finite(anova[2,'Mean Sq'])||anova[2,'Mean Sq']<=0)stop('ANOVA residual variance must be positive; no fabricated p values',call.=FALSE)
tukey<-stats::TukeyHSD(fit)$factor;pair<-utils::combn(seq_len(ng),2);pairs<-data.frame(group1_id=ids[pair[1,]],group2_id=ids[pair[2,]],group1=labels[pair[1,]],group2=labels[pair[2,]],x1=pair[1,],x2=pair[2,]);match_rows<-match(paste0(pairs$group2_id,'-',pairs$group1_id),rownames(tukey));if(anyNA(match_rows))stop('Tukey pair keys failed',call.=FALSE);pairs$diff<-tukey[match_rows,'diff'];pairs$lwr<-tukey[match_rows,'lwr'];pairs$upr<-tukey[match_rows,'upr'];pairs$p_adj<-tukey[match_rows,'p adj'];pairs$symbol<-ifelse(pairs$p_adj<.01,'**',ifelse(pairs$p_adj<.05,'*','ns'))
summary_df<-data.frame(group_id=ids,group=labels,n=length(selected),mean=colMeans(values),sd=apply(values,2,sd));summary_df$lower<-summary_df$mean-summary_df$sd;summary_df$upper<-summary_df$mean+summary_df$sd
width_mm<-num('width_mm',105,100,150);height_mm<-num('height_mm',115,110,160);dpi<-integer_param('dpi',600,96,1200);font_size<-num('font_size',8,7.5,9);point_size<-num('point_size',3,2.4,3.6);point_alpha<-num('point_alpha',1,.8,1);line_width<-num('line_width',.4,.3,.6);show_significance<-bool('show_significance',TRUE);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
hex<-function(name,default){v<-text_param(name,default);if(!grepl('^#[0-9A-Fa-f]{6}$',v))stop(name,' must be #RRGGBB',call.=FALSE);toupper(v)};colors<-c(hex('primary_color','#679289'),hex('accent_color','#EE2E31'),hex('third_color','#C9CBA3'),hex('fourth_color','#F4C095'))[seq_len(ng)];if(anyDuplicated(colors))stop('Group colors must remain distinct',call.=FALSE)
sw<-width_mm/105;sh<-height_mm/115;left<-19*sw;right<-101*sw;bottom<-19*sh;top<-87*sh;dx<-(right-left)/ng;xx<-left+(seq_len(ng)-.5)*dx;sig<-pairs[pairs$p_adj<.05&show_significance,,drop=FALSE];base_max<-max(values,summary_df$upper);base_min<-min(0,summary_df$lower);data_span<-max(base_max-base_min,1);sig$y<-base_max+data_span*(.12+.11*seq_len(nrow(sig)));sig$tip<-sig$y-data_span*.035;ymax<-max(base_max*1.12,if(nrow(sig))max(sig$y)+data_span*.09 else base_max);ymin<-if(base_min<0)base_min-data_span*.04 else 0;ticks<-pretty(c(ymin,ymax),n=4);ticks<-ticks[ticks>=ymin&ticks<=ymax];yy<-function(v)bottom+(v-ymin)/(ymax-ymin)*(top-bottom)
set.seed(336);points<-long[c('timepoint','source_row','group_id','group','value')];points$x_mm<-rep(xx,each=length(selected))+runif(nrow(long),-.1,.1)*dx;points$y_mm<-yy(long$value);points$shape<-rep(seq_len(ng),each=length(selected));points$fill<-rep(colors,each=length(selected));for(gg in seq_len(ng)){ii<-which(points$shape==gg);placed<-integer();candidate<-unique(c(0,as.vector(rbind(seq(point_size+.3,dx*.4,length.out=80),-seq(point_size+.3,dx*.4,length.out=80)))));for(j in ii[order(points$y_mm[ii])]){ok<-vapply(candidate,function(v)all(abs(xx[gg]+v-points$x_mm[placed])>=point_size+.25|abs(points$y_mm[j]-points$y_mm[placed])>=point_size+.25),logical(1));if(!any(ok))stop('Dense points require a larger canvas or smaller point_size',call.=FALSE);points$x_mm[j]<-xx[gg]+candidate[which(ok)[1]];placed<-c(placed,j)}};summary_df$x_mm<-xx;summary_df$y_mm<-yy(summary_df$mean);summary_df$lower_mm<-yy(summary_df$lower);summary_df$upper_mm<-yy(summary_df$upper);sig$x1_mm<-xx[sig$x1];sig$x2_mm<-xx[sig$x2];sig$y_mm<-yy(sig$y);sig$tip_mm<-yy(sig$tip);sig$text_mm<-yy(sig$y+data_span*.045)
reference<-read_table(file.path(script_dir,'assets/paper-fig5e.tsv'));R<-matrix(unlist(lapply(reference[-1],parse_numeric,label='Reference')),nrow=nrow(reference));is_source<-setequal(labels,names(reference)[-1])&&nrow(raw)==nrow(reference)&&identical(as.character(raw$Type),as.character(reference$Type))&&all(M==R[,match(labels,names(reference)[-1]),drop=FALSE]);origin<-if(is_source)'Source Data Fig. 5e verified; study assumptions not independently audited.'else'Supplied observations; study origin and independence unverified.'
caption1<-paste0(timepoint,'; n = ',length(selected),' per group; mean ± SD.');caption2<-'ANOVA + Tukey (all pairs within selected timepoint).';caption3<-'* p < 0.05; ** p < 0.01; nonsignificant pairs omitted.'
legend<-data.frame(shape=seq_len(ng),fill=colors,label=display,x_mm=rep(c(22,62)*sw,length.out=ng),y_mm=rep(c(107,99)*sh,each=2,length.out=ng));if(ng==3)legend$y_mm<-c(107,107,99)*sh
for(ff in unique(c(family,'Helvetica')))sysfonts::font_add(ff,regular=systemfonts::match_fonts(ff)$path,bold=systemfonts::match_fonts(ff,weight='bold')$path,italic=systemfonts::match_fonts(ff,italic=TRUE)$path,bolditalic=systemfonts::match_fonts(ff,weight='bold',italic=TRUE)$path)
probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=s,fontface='bold'))),'mm',TRUE);if(max(vapply(display,tw,numeric(1),s=font_size))>35*sw||max(vapply(c(caption1,caption2,caption3,origin),tw,numeric(1),s=5.5))>width_mm-8)stop('Complete labels need larger permitted width or smaller font',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(){grid::grid.newpage();mm<-function(z)grid::unit(z,'mm');txt<-function(z,x,y,s=font_size,face='plain',just='center',rot=0,ff=family)grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=ff,fontsize=s,fontface=face,col='#111111'));ln<-function(x,y,w=line_width,col='#111111')grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=col,lwd=w*72.27/25.4,lineend='butt'));shape<-function(x,y,k,fill){r<-point_size/2;gp<-grid::gpar(fill=grDevices::adjustcolor(fill,alpha.f=point_alpha),col=grDevices::adjustcolor(fill,alpha.f=point_alpha),lwd=.23*72.27/25.4);if(k==1)grid::grid.circle(mm(x),mm(y),mm(r),gp=gp)else if(k==2)grid::grid.rect(mm(x),mm(y),mm(point_size),mm(point_size),gp=gp)else if(k==3)grid::grid.polygon(mm(x+c(-r,0,r,0)),mm(y+c(0,r,0,-r)),gp=gp)else grid::grid.polygon(mm(x+c(-r,0,r)),mm(y+c(-r*.577,r*1.155,-r*.577)),gp=gp)}
 ln(c(left,left,right),c(top,bottom,bottom),.55);for(t in ticks){y<-yy(t);ln(c(left,left-1.3),c(y,y),.55);txt(format(t,trim=TRUE),left-2.2,y,font_size,just='right')};txt('CAR T cells (×10³/g)',6.5*sw,(bottom+top)/2,10,face='bold',rot=90,ff='Helvetica')
 for(i in seq_len(ng)){ln(rep(xx[i],2),c(summary_df$lower_mm[i],summary_df$upper_mm[i]),line_width,colors[i]);for(y in c(summary_df$lower_mm[i],summary_df$upper_mm[i]))ln(xx[i]+c(-.1,.1)*dx,c(y,y),line_width,colors[i]);ln(xx[i]+c(-.2,.2)*dx,rep(summary_df$y_mm[i],2),.4)}
 for(i in seq_len(nrow(points)))shape(points$x_mm[i],points$y_mm[i],points$shape[i],points$fill[i]);if(nrow(sig))for(i in seq_len(nrow(sig))){ln(c(sig$x1_mm[i],sig$x1_mm[i],sig$x2_mm[i],sig$x2_mm[i]),c(sig$tip_mm[i],sig$y_mm[i],sig$y_mm[i],sig$tip_mm[i]),.35);txt(sig$symbol[i],(sig$x1_mm[i]+sig$x2_mm[i])/2,sig$text_mm[i],10,face='bold')};for(i in seq_len(ng)){shape(legend$x_mm[i],legend$y_mm[i],legend$shape[i],legend$fill[i]);txt(legend$label[i],legend$x_mm[i]+3,legend$y_mm[i],font_size,face='bold',just='left')};txt(if(all(grepl(' CAR T$',labels)))'CAR T treatments'else'Treatments as supplied',22*sw,92*sh,7,just='left');for(i in 1:4)txt(c(caption1,caption2,caption3,origin)[i],4,(13-(i-1)*3.1)*sh,5.5,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
for(k in c('points','summary_df','pairs','sig','legend'))write.csv(get(k),file.path(output_dir,paste0('actual_',k,'.csv')),row.names=FALSE);saveRDS(list(raw=raw,M=M,values=values,fit=fit,anova=anova,tukey=tukey),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(data_path=data_path,input_sheet=input_sheet,timepoint=timepoint,groups=labels,n=length(selected),observations=nrow(long),all_observations=length(M),F=anova[1,'F value'],anova_p=anova[1,'Pr(>F)'],df_between=ng-1,df_residual=nrow(long)-ng,is_source=is_source,origin=origin,colors=colors,width_mm=width_mm,height_mm=height_mm,font_size=font_size,family=family,point_size=point_size,point_alpha=point_alpha,line_width=line_width,show_significance=show_significance,ymin=ymin,ymax=ymax,ticks=ticks,geometry=list(left=left,right=right,bottom=bottom,top=top,dx=dx),scope='Paper Fig5e numeric values verified; selected-day one-way ANOVA/Tukey; no across-day inference, no replicate identity/assumption certification'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',nrow(long),'observations for',timepoint,'with keyed Tukey pairs\n')
