#!/usr/bin/env Rscript

# Diverging bars: preserve all supplied observations without unsupported clinical marks.
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
required_packages <- c("jsonlite", "svglite", "ragg", "ggplot2", "readxl", "RColorBrewer")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE); params <- if (is.null(cfg$params)) list() else cfg$params
if(!is.list(cfg)||!is.list(params)||(!is.null(names(params))&&any(!nzchar(names(params)))))stop("config/params must be objects",call.=FALSE)
gp <- function(name, default = NULL) {value<-params[[name]];if(is.null(value))for(section in c("data","model","selection","colors","typography","geometry","axis","legend","layout","labels","statistics","export"))if(is.list(params[[section]])&&!is.null(params[[section]][[name]])){value<-params[[section]][[name]];break};if(is.null(value))default else value}
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
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt", "md", "maf")) { first <- readLines(path, n = 1L, warn = FALSE); seps<-c("\t",";",",");counts<-vapply(seps,function(sep){con<-textConnection(first);on.exit(close(con));sum(utils::count.fields(con,sep=sep,quote="\"",comment.char=""),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; fields<-utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE);if(any(fields[!is.na(fields)]!=fields[1]))stop("Every table record must match the header field count",call.=FALSE); if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for XLS/XLSX input", call. = FALSE)
    as.data.frame(readxl::read_excel(path, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}






scenario<-choice('scenario','cancer_bar',c('auto','cancer_bar','gsva_threshold','gsva_significance'));fallback<-if(scenario%in%c('auto','cancer_bar'))'easy_input1.csv'else if(scenario=='gsva_threshold')'easy_input2.csv'else'easy_input3.csv';data_file<-resolve_input(text_param('data_path','',TRUE),file.path(script_dir,'assets',fallback));raw<-read_table(data_file);if(anyDuplicated(names(raw))||!all(c('ID','score')%in%names(raw))||!nrow(raw))stop('Input requires rows with unique ID/score column headers',call.=FALSE);if(scenario=='auto'){if(!'pval'%in%names(raw))stop('Two-column input is ambiguous: select cancer_bar or gsva_threshold explicitly',call.=FALSE);scenario<-'gsva_significance'} # Two-column cancer and enrichment tables cannot be distinguished by their schema.
df<-raw;df$ID<-as.character(raw$ID);if(anyNA(df$ID)||any(!nzchar(trimws(df$ID)))||any(grepl('[[:cntrl:]]',df$ID)))stop('ID must be complete single-line text',call.=FALSE);df$score<-parse_numeric(raw$score,'score');if(any(!is.finite(df$score)))stop('score must contain finite numeric values',call.=FALSE);df$source_row<-seq_len(nrow(df));if(scenario=='gsva_significance'&&!'pval'%in%names(df))stop('gsva_significance requires pval',call.=FALSE);if('pval'%in%names(df)){df$pval<-parse_numeric(raw$pval,'pval');if(any(!is.finite(df$pval))||any(df$pval<0|df$pval>1))stop('pval must be complete within [0,1]',call.=FALSE)}
family<-choice('family','Arial',c('Arial','Helvetica','Times New Roman','sans'));family<-if(family%in%c('Arial','Helvetica'))'sans'else if(family=='Times New Roman')'serif'else family;base_size<-num('base_size',9,7,20);label_size<-num('label_size',7.2,5,18);line_width<-num('line_width',.35,.2,2);bar_width<-num('bar_width',.82,.3,1);legend_position<-choice('legend_position','top',c('top','bottom','left','right','none'));palette_mode<-choice('palette','original',c('original','alternative'));dpi<-integer_param('dpi',600,96,1200);width_mm<-num('width_mm',if(scenario=='cancer_bar')210 else 180,100,320);height_mm<-num('height_mm',if(scenario=='cancer_bar')130 else 270,90,380);title<-text_param('title','',TRUE);if(!nzchar(title))title<-if(scenario=='cancer_bar')'Maximum tumour-size change'else'Provided enrichment statistics';y_title<-text_param('y_title','',TRUE);if(!nzchar(y_title))y_title<-if(scenario=='cancer_bar')'Maximum change in tumour size (%)'else'Provided score';if(any(grepl('[[:cntrl:]]|Case [0-9]|cover_|debug',c(title,y_title,df$ID),ignore.case=TRUE)))stop('Remove internal/control identifiers from plot labels',call.=FALSE)
caption<-if(scenario=='cancer_bar')c('All supplied scores shown on a continuous scale; no unsupported clinical or significance symbols.',if(identical(data_file,normalizePath(file.path(script_dir,'assets/easy_input1.csv'))))'Provided example data: Cancer IDs have no verified clinical mapping; reference lines: -30, 0, +20.'else'Reference lines -30, 0, +20 are display guides; metric and tumour-type provenance require verification.')else if(scenario=='gsva_threshold')c('Negative: score <= -4; neutral: -4 < score <= +4; positive: score > +4.','Score thresholds are display categories, not significance tests; repeated terms retained; no GSVA analysis performed.')else c('Colour: supplied raw p <= 0.05 and score direction; score = 0 is neutral.','No multiple-testing adjustment performed; repeated terms retained; no GSVA analysis or test recomputed.')
plot_theme<-ggplot2::theme_classic(base_size=base_size,base_family=family)+ggplot2::theme(plot.title=ggplot2::element_text(size=base_size+1.5,face='bold',hjust=0,margin=ggplot2::margin(b=5)),axis.title=ggplot2::element_text(size=base_size-.5,face='bold'),axis.title.x=ggplot2::element_text(margin=ggplot2::margin(t=6)),axis.title.y=ggplot2::element_text(margin=ggplot2::margin(r=6)),axis.text=ggplot2::element_text(size=base_size-1.5,colour='#334E68'),axis.line=ggplot2::element_line(linewidth=.25),axis.ticks=ggplot2::element_line(linewidth=.25),legend.position=legend_position,legend.text=ggplot2::element_text(size=7.2),legend.key.height=grid::unit(3,'mm'),legend.key.width=grid::unit(5,'mm'),legend.margin=ggplot2::margin(2,0,2,0,unit='mm'),plot.caption=ggplot2::element_text(size=6,hjust=0,lineheight=1.15,colour='#52606D',margin=ggplot2::margin(t=5)),plot.margin=ggplot2::margin(4,4,4,4,unit='mm'))
if(scenario=='cancer_bar'){
 df<-df[order(-df$score,df$source_row),,drop=FALSE];df$index<-seq_len(nrow(df));ids<-unique(as.character(raw$ID));df$group<-factor(df$ID,levels=ids);base_palette<-if(palette_mode=='original')RColorBrewer::brewer.pal(12,'Paired')else c('#0072B2','#D55E00','#009E73','#CC79A7','#E69F00','#56B4E9','#F0E442','#000000','#999999','#8C564B','#17BECF','#BCBD22');fill_values<-setNames(if(length(ids)<=12)base_palette[seq_along(ids)]else grDevices::hcl.colors(length(ids),'Dynamic'),ids);axis_range<-c(min(-90,floor(min(df$score)/10)*10),max(50,ceiling(max(df$score)/10)*10));refs<-c(-30,0,20);outliers<-df[df$score>50,,drop=FALSE];outliers$label<-format(outliers$score,trim=TRUE,digits=6)
 plot_obj<-ggplot2::ggplot(df,ggplot2::aes(index,score,fill=group))+ggplot2::geom_col(width=bar_width,colour=NA)+ggplot2::geom_hline(yintercept=refs,linetype='dashed',colour='#7A7A7A',linewidth=line_width)+ggplot2::scale_fill_manual(values=fill_values,name=NULL,drop=FALSE)+ggplot2::scale_x_continuous(limits=c(.35,nrow(df)+.65),expand=c(0,0),breaks=NULL)+ggplot2::scale_y_continuous(limits=axis_range,breaks=pretty(axis_range,n=8),expand=ggplot2::expansion(mult=c(.015,.05)))+ggplot2::labs(x=NULL,y=y_title,title=title,caption=paste(caption,collapse='\n'))+plot_theme+ggplot2::theme(axis.line.x=ggplot2::element_blank(),axis.ticks.x=ggplot2::element_blank())+ggplot2::guides(fill=ggplot2::guide_legend(ncol=4,byrow=TRUE))
 if(nrow(outliers))plot_obj<-plot_obj+ggplot2::geom_text(data=outliers,ggplot2::aes(index,score,label=label),inherit.aes=FALSE,vjust=-.7,size=label_size/ggplot2::.pt,family=family,colour='#1F2933')
}else{
 df<-df[order(df$score,df$source_row),,drop=FALSE];df$index<-seq_len(nrow(df));df$group<-if(scenario=='gsva_threshold')ifelse(df$score<= -4,'negative',ifelse(df$score>4,'positive','neutral'))else ifelse(df$pval<=.05&df$score<0,'negative',ifelse(df$pval<=.05&df$score>0,'positive','neutral'));df$group<-factor(df$group,levels=c('negative','neutral','positive'));df$label_colour<-ifelse(df$group=='neutral','#59636D','#1F2933');df$label<-df$ID;df$hjust<-ifelse(df$score>0,1,0);if(any(nchar(df$label)>120))stop('Term labels exceed 120 characters; provide concise scientific labels',call.=FALSE)
 fill_values<-c(negative='#A1D99B',neutral='#D9D9D9',positive='#225EA8');if(palette_mode=='alternative')fill_values<-c(negative='#E69F00',neutral='#D9D9D9',positive='#0072B2');max_abs<-max(6,abs(df$score));axis_range<-c(-max_abs,max_abs)*1.06;refs<-if(scenario=='gsva_threshold')c(-4,0,4)else 0;df$label_x<-ifelse(df$score>0,-max_abs*.02,max_abs*.02)
 plot_obj<-ggplot2::ggplot(df,ggplot2::aes(score,index,fill=group))+ggplot2::geom_col(width=bar_width,orientation='y',colour=NA)+ggplot2::geom_vline(xintercept=0,colour='#78818A',linetype='solid',linewidth=line_width)+ggplot2::geom_text(ggplot2::aes(x=label_x,label=label,hjust=hjust,colour=label_colour),family=family,size=label_size/ggplot2::.pt)+ggplot2::scale_fill_manual(values=fill_values,drop=FALSE,guide='none')+ggplot2::scale_colour_identity()+ggplot2::scale_x_continuous(limits=axis_range,expand=c(0,0))+ggplot2::scale_y_continuous(limits=c(.35,nrow(df)+.65),expand=c(0,0),breaks=NULL)+ggplot2::labs(x=y_title,y=NULL,title=title,caption=paste(caption,collapse='\n'))+plot_theme+ggplot2::theme(axis.line.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank(),panel.border=ggplot2::element_rect(colour='#52606D',fill=NA,linewidth=.25),legend.position='none')
 if(scenario=='gsva_threshold'){threshold_marks<-df[abs(df$score)>=4,,drop=FALSE];threshold_marks$cut<-sign(threshold_marks$score)*4;plot_obj<-plot_obj+ggplot2::geom_segment(data=threshold_marks,ggplot2::aes(x=cut,xend=cut,y=index-bar_width/2,yend=index+bar_width/2),inherit.aes=FALSE,colour='white',linetype='dashed',linewidth=line_width)}
}
build<-ggplot2::ggplot_build(plot_obj);geometry<-NULL
render<-function(kind){path<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(path,width=width_mm,height=height_mm,units='mm',res=dpi,bg='white')else if(kind=='pdf')grDevices::pdf(path,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE,bg='white')else svglite::svglite(path,width=width_mm/25.4,height=height_mm/25.4,bg='white');tryCatch({grid::grid.newpage();for(v in c(title,caption)){fs<-if(v==title)base_size+1.5 else 6;if(grid::convertWidth(grid::grobWidth(grid::textGrob(v,gp=grid::gpar(fontfamily=family,fontsize=fs,fontface=if(v==title)'bold'else'plain'))),'mm',valueOnly=TRUE)>width_mm-8)stop('Title/caption cannot fit canvas',call.=FALSE)};grid::grid.draw(ggplot2::ggplotGrob(plot_nh_apply_format(plot_obj, cfg, plot_nh_format_output(cfg))));grid::grid.force();vv<-grid::grid.ls(viewports=TRUE,grobs=FALSE,recursive=TRUE,print=FALSE);ii<-which(grepl('^panel[.]',vv$name));if(length(ii)!=1)stop('Expected one actual panel',call.=FALSE);pv<-c(strsplit(vv$vpPath[ii],'::',fixed=TRUE)[[1]][-1],vv$name[ii]);grid::downViewport(do.call(grid::vpPath,as.list(pv)));pw<-grid::convertWidth(grid::unit(1,'npc'),'mm',valueOnly=TRUE);ph<-grid::convertHeight(grid::unit(1,'npc'),'mm',valueOnly=TRUE);loc<-grid::deviceLoc(grid::unit(0,'npc'),grid::unit(0,'npc'),valueOnly=TRUE);pp<-build$layout$panel_params[[1]]
 if(scenario!='cancer_bar'){lineheight<-grid::convertHeight(grid::grobHeight(grid::textGrob('Mg',gp=grid::gpar(fontfamily=family,fontsize=label_size))),'mm',valueOnly=TRUE)*1.4;if(ph/diff(pp$y.range)<lineheight)stop('Rows too dense for label_size; enlarge height or reduce text',call.=FALSE);for(j in seq_len(nrow(df))){tw<-grid::convertWidth(grid::grobWidth(grid::textGrob(df$label[j],gp=grid::gpar(fontfamily=family,fontsize=label_size))),'mm',valueOnly=TRUE);space<-if(df$hjust[j]==1)(df$label_x[j]-pp$x.range[1])/diff(pp$x.range)*pw else(pp$x.range[2]-df$label_x[j])/diff(pp$x.range)*pw;if(tw>space-.5)stop('Term label exceeds opposite-side space; increase width or reduce text',call.=FALSE)}}else{if(pw/nrow(df)*bar_width<.45)stop('Cancer bars too narrow; enlarge width or reduce observations',call.=FALSE);boxes<-list();if(nrow(outliers))for(j in seq_len(nrow(outliers))){tw<-grid::convertWidth(grid::grobWidth(grid::textGrob(outliers$label[j],gp=grid::gpar(fontfamily=family,fontsize=label_size))),'mm',valueOnly=TRUE);th<-grid::convertHeight(grid::grobHeight(grid::textGrob('Mg',gp=grid::gpar(fontfamily=family,fontsize=label_size))),'mm',valueOnly=TRUE)*1.4;x<-(outliers$index[j]-pp$x.range[1])/diff(pp$x.range)*pw;y<-(outliers$score[j]-pp$y.range[1])/diff(pp$y.range)*ph;box<-c(x-tw/2,x+tw/2,y+th*.5,y+th*1.8);if(box[1]<0||box[2]>pw||box[4]>ph)stop('Outlier value labels clip; increase dimensions or reduce label_size',call.=FALSE);for(other in boxes)if(box[1]<other[2]&&box[2]>other[1]&&box[3]<other[4]&&box[4]>other[3])stop('Outlier labels overlap; increase dimensions or reduce label_size',call.=FALSE);boxes[[j]]<-box}}
 if(kind=='svg')geometry<<-list(x_mm=loc$x*25.4,y_mm=loc$y*25.4,width_mm=pw,height_mm=ph,xrange=pp$x.range,yrange=pp$y.range);grid::upViewport(0)},finally=grDevices::dev.off())};for(kind in c('png','pdf','svg'))render(kind)
write_tsv<-function(d,n)write.table(d,file.path(output_dir,n),sep='\t',quote=TRUE,row.names=FALSE,na='NA',qmethod='double');write_tsv(raw,'source-records.tsv');write_tsv(df,'sorted-records.tsv');for(i in seq_along(build$data))write_tsv(build$data[[i]],paste0('layer-',i,'.tsv'));saveRDS(list(raw=raw,data=df,scenario=scenario,fill_values=fill_values,build=build),file.path(output_dir,'parsed-analysis.RDS'));metadata<-list(input=data_file,scenario=scenario,rows=nrow(df),unique_ids=length(unique(df$ID)),duplicate_id_rows=nrow(df)-length(unique(df$ID)),score_range=range(df$score),axis_range=axis_range,reference_lines=refs,groups=as.list(table(df$group)),fill_values=as.list(fill_values),title=title,y_title=y_title,caption=caption,width_mm=width_mm,height_mm=height_mm,bar_width=bar_width,label_size=label_size,line_width=line_width,panel_geometry=geometry);jsonlite::write_json(metadata,file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered ',scenario,': ',nrow(df),' supplied rows, all scores retained\n')
