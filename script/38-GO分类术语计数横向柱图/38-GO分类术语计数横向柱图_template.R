#!/usr/bin/env Rscript

# Complete GO ontology term counts; no enrichment test recomputation.
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
gp <- function(name, default = NULL) {value<-params[[name]];if(is.null(value))for(section in c("data","model","selection","colors","typography","geometry","axis","legend","layout","labels","statistics","analysis","clustering","density","export"))if(is.list(params[[section]])&&!is.null(params[[section]][[name]])){value<-params[[section]][[name]];break};if(is.null(value))default else value}
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
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt")) { seps<-c("\t",";",",");counts<-vapply(seps,function(sep){v<-tryCatch(utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE),error=function(e)0L);max(c(0L,v),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; fields<-utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE);fields<-fields[!is.na(fields)];if(!length(fields)||any(fields!=fields[1]))stop("Every table record must match the header field count",call.=FALSE); table_source<-textConnection(paste0(rawToChar(readBin(path,"raw",n=file.info(path)$size)),"\n"));on.exit(close(table_source),add=TRUE); if (sep=="\t") utils::read.delim(table_source, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(table_source,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
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
data_path<-resolve_input(text_param('data_path','assets/input.txt'),'assets/input.txt');sheet<-integer_param('sheet',1,1,20);raw<-read_table(data_path,sheet)
ontology_column<-text_param('ontology_column','ONTOLOGY');term_column<-text_param('term_column','Term');count_column<-text_param('count_column','Count');id_column<-text_param('id_column','',TRUE);group_order_text<-text_param('group_order','BP,CC,MF');sort_mode<-choice('sort_mode','count_ascending',c('count_ascending','source_order','count_descending'))
roles<-c(ontology_column,term_column,count_column,if(nzchar(id_column))id_column);if(!nrow(raw)||nrow(raw)>60||anyDuplicated(names(raw))||any(!nzchar(names(raw)))||'source_row'%in%names(raw)||anyDuplicated(roles)||!all(roles%in%names(raw)))stop('Require1..60rows, unique headers and distinct existing roles; source_row reserved',call.=FALSE)
clean_label<-function(x,k){z<-trimws(as.character(x));if(anyNA(z)||any(!nzchar(z))||any(grepl('[[:cntrl:]]',z))||any(grepl('cover_[0-9]|Case[ _:#-]*[0-9]|案例[ ]*[0-9]',z,ignore.case=TRUE)))stop(k,' requires complete meaningful labels',call.=FALSE);z}
ontology<-clean_label(raw[[ontology_column]],'ontology');terms<-clean_label(raw[[term_column]],'term');if(any(!ontology%in%c('BP','CC','MF')))stop('Every ontology must be BP, CC or MF; no silent exclusion',call.=FALSE)
keys<-paste(ontology,terms,sep=' | ');if(anyDuplicated(keys))stop('Ontology/term pairs must be unique; no aggregation',call.=FALSE)
ids<-if(nzchar(id_column))clean_label(raw[[id_column]],'ID')else keys;if(anyDuplicated(ids))stop('Explicit term IDs must be unique',call.=FALSE)
counts<-parse_numeric(raw[[count_column]],count_column);if(any(!is.finite(counts))||any(counts<0)||any(counts!=trunc(counts))||any(counts>2^53-1))stop('Every Count must be a finite nonnegative exactly representable integer; no filtering or normalization',call.=FALSE)
groups<-clean_label(strsplit(group_order_text,',',fixed=TRUE)[[1]],'group_order');if(length(groups)!=3||anyDuplicated(groups)||!setequal(groups,c('BP','CC','MF')))stop('group_order must contain BP, CC and MF exactly once',call.=FALSE)
ord<-unlist(lapply(groups,function(g){ix<-which(ontology==g);if(sort_mode=='source_order')ix else ix[order(if(sort_mode=='count_descending')-counts[ix]else counts[ix],ix)]}),use.names=FALSE)
observations<-data.frame(source_row=ord,source_column=count_column,ontology=ontology[ord],term=terms[ord],term_id=ids[ord],count=counts[ord],order_position=seq_along(ord),stringsAsFactors=FALSE);nterms<-nrow(observations);present_groups<-groups[groups%in%ontology]
width_mm<-num('width_mm',210,180,420);height_mm<-num('height_mm',150,120,360);dpi<-integer_param('dpi',600,96,1200);bar_width<-num('bar_width',.64,.4,.85);bar_border_width<-num('bar_border_width',.1,0,.4);axis_width<-num('axis_width',.3,.15,.6);legend_key_mm<-num('legend_key_mm',6,3,8);legend_gap_mm<-num('legend_gap_mm',2,1,5);legend_position<-choice('legend_position','right',c('right','none'))
family_choice<-choice('font_family','Arial',c('Arial','Helvetica','Times New Roman','sans'));family<-if(family_choice=='sans')'Helvetica'else family_choice;term_size<-num('term_text_size_pt',12.7,7,16);tick_size<-num('axis_text_size_pt',13.4,7,16);axis_title_size<-num('axis_title_size_pt',14,9,18);legend_size<-num('legend_text_size_pt',12,8,16);legend_title_size<-num('legend_title_size_pt',14,9,18);caption_size<-num('caption_size_pt',6.5,6,8);title_size<-num('title_size_pt',18,12,22)
title<-text_param('title','',TRUE);x_title<-text_param('x_title','Count');y_title<-text_param('y_title','Term');legend_title<-text_param('legend_title','ONTOLOGY');for(k in c('title','x_title','y_title','legend_title'))if(grepl('cover_[0-9]|Case[ _:#-]*[0-9]|案例[ ]*[0-9]',get(k),ignore.case=TRUE))assign(k,'')
bp_color<-toupper(text_param('bp_color','#3B4992'));cc_color<-toupper(text_param('cc_color','#EE0000'));mf_color<-toupper(text_param('mf_color','#008B45'));text_color<-toupper(text_param('text_color','#111111'));border_color<-toupper(text_param('border_color','#FFFFFF'));axis_color<-toupper(text_param('axis_color','#111111'));if(any(!grepl('^#[0-9A-F]{6}$',c(bp_color,cc_color,mf_color,text_color,border_color,axis_color)))||any(c(bp_color,cc_color,mf_color,text_color,axis_color)=='#FFFFFF')||anyDuplicated(c(bp_color,cc_color,mf_color)))stop('Visible distinct ontology HEX colors required',call.=FALSE)
palette<-c(BP=bp_color,CC=cc_color,MF=mf_color);bars<-data.frame(bar_id=seq_len(nterms),observations,color=unname(palette[observations$ontology]),stringsAsFactors=FALSE)
font_file<-function(){h<-systemfonts::match_fonts(family);if(h$index!=0L){family<<-'Arial';h<-systemfonts::match_fonts(family)};h$path}
regular_file<-font_file();bh<-systemfonts::match_fonts(family,weight='bold');bold_file<-if(bh$index==0L)bh$path else '/System/Library/Fonts/Supplemental/Arial Bold.ttf';sysfonts::font_add(family,regular=regular_file,bold=bold_file);text_font_files<-setNames(list(regular_file),family)
text_font_for<-function(z){chars<-unique(strsplit(enc2utf8(as.character(z)),'',fixed=TRUE)[[1]]);supports<-function(f)all(systemfonts::glyph_info(chars,path=text_font_files[[f]],index=0)$index>0);if(supports(family))return(family);fallback<-'Arial Unicode MS';if(is.null(text_font_files[[fallback]])){h<-systemfonts::match_fonts(fallback);if(h$index!=0)stop('Unicode font must have addressable regular face',call.=FALSE);text_font_files[[fallback]]<<-h$path;sysfonts::font_add(fallback,regular=h$path)};if(!supports(fallback))stop('Font lacks displayed glyph: ',z,call.=FALSE);fallback}
probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()
tw<-function(z,s,fontfamily=if(nzchar(z))text_font_for(z)else family,face='plain')if(!nzchar(z))0 else grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=fontfamily,fontsize=s,fontface=face))),'mm',TRUE)
text_height<-function(z,s,ff=if(nzchar(z))text_font_for(z)else family){if(!nzchar(z))return(0);q<-systemfonts::glyph_info(strsplit(enc2utf8(z),'',fixed=TRUE)[[1]],path=text_font_files[[ff]],index=0,size=s,res=720);(max(q$y_bearing)-min(q$y_bearing-q$height))/10*25.4/72+.12}
fmt<-function(v)if(max(abs(v))>=1e5||max(abs(v))<.001)vapply(v,function(z)sprintf('%.3g',z),character(1))else format(v,trim=TRUE,digits=4,scientific=FALSE)
lines<-list();labels<-list();rects<-list();panels<-list()
label<-function(z,x,y,size,role,just='centre',rotation=0,color=text_color,panel='figure',bar_id=NA_integer_){if(!nzchar(z))return();labels[[length(labels)+1]]<<-data.frame(label=as.character(z),x_mm=unname(x),y_mm=unname(y),size=size,role=role,just=just,color=color,rotation=rotation,face='plain',family=text_font_for(z),panel=panel,bar_id=bar_id)}
line<-function(x,y,role,color=axis_color,lw=axis_width,lty='solid',panel='bars',bar_id=NA_integer_){lines[[length(lines)+1]]<<-data.frame(line_id=length(lines)+1,vertex=seq_along(x),x_mm=unname(x),y_mm=unname(y),color=color,width=lw,linetype=lty,role=role,panel=panel,bar_id=bar_id)}
tryCatch({
 fmt<-function(v)vapply(v,function(x)if(x==0)'0'else if(abs(x)>=1e5)sprintf('%.3g',x)else format(x,trim=TRUE,scientific=FALSE),character(1))
 caption<-'Supplied GO term counts; overlapping genes are not independent totals; no enrichment test recomputed.';caption_y<-1+text_height(caption,caption_size)/2;footer_top<-caption_y+text_height(caption,caption_size)/2
 bottom<-max(.10*height_mm,footer_top+text_height(x_title,axis_title_size)+text_height('0',tick_size)+3);top<-.982*height_mm-if(nzchar(title))text_height(title,title_size)+1 else 0
 label_width<-max(vapply(observations$term,tw,numeric(1),s=term_size));left<-max(.545*width_mm,label_width+text_height(y_title,axis_title_size)+6);legend_width<-if(legend_position=='right')max(tw(legend_title,legend_title_size),legend_key_mm+2+max(vapply(present_groups,tw,numeric(1),s=legend_size)))else 0;legend_left<-width_mm-legend_width-4;right<-if(legend_position=='right')min(.808*width_mm,legend_left-6)else width_mm-4
 if(right-left<35||top-bottom<20)stop('Labels/legend leave too little data area; enlarge canvas or reduce label size',call.=FALSE)
 pitch<-(top-bottom)/nterms;max_term_h<-max(vapply(observations$term,text_height,numeric(1),s=term_size));if(pitch<max_term_h+.1)stop('Term labels cannot fit row pitch; increase height or reduce text',call.=FALSE)
 max_count<-max(observations$count);axis_max<-if(max_count==0)1 else max_count;xticks<-if(max_count==0)c(0,1)else pretty(c(0,max_count),n=4);xticks<-xticks[xticks>=0&xticks<=axis_max];xmap<-function(z)left+z/axis_max*(right-left);cy<-bottom+(seq_len(nterms)-.5)*pitch
 panels<-list(data.frame(panel='bars',left_mm=left,right_mm=right,bottom_mm=bottom,top_mm=top,xmin=0,xmax=axis_max,ymin=.5,ymax=nterms+.5,pitch_x_mm=NA_real_,pitch_y_mm=pitch,axis_left_mm=left,axis_right_mm=right))
 for(i in seq_len(nterms)){z<-bars[i,];hh<-bar_width*pitch;rects[[length(rects)+1]]<-data.frame(rect_id=length(rects)+1,bar_id=i,left_mm=left,right_mm=xmap(z$count),bottom_mm=cy[i]-hh/2,top_mm=cy[i]+hh/2,color=z$color,border_color=border_color,border_width=bar_border_width,alpha=1,role='count_bar',panel='bars',value=z$count,ontology=z$ontology,visible=z$count>0);if(z$count==0)line(rep(left,2),c(cy[i]-hh/2,cy[i]+hh/2),'zero_bar',z$color,max(bar_border_width,.2),bar_id=i);line(c(left-.7,left),rep(cy[i],2),'axis_ytick',bar_id=i);label(z$term,left-1.5,cy[i],term_size,'term_label',just='right',panel='bars',bar_id=i)}
 line(rep(left,2),c(bottom,top),'axis_left');line(c(left,right),rep(bottom,2),'axis_bottom');for(v in xticks){x<-xmap(v);line(rep(x,2),c(bottom-.7,bottom),'axis_xtick');label(fmt(v),x,bottom-1.1-text_height(fmt(v),tick_size)/2,tick_size,'x_tick',panel='bars')}
 label(x_title,(left+right)/2,bottom-2.5-text_height('0',tick_size)-text_height(x_title,axis_title_size)/2,axis_title_size,'x_title');label(y_title,text_height(y_title,axis_title_size)/2+1.2,(top+bottom)/2,axis_title_size,'y_title',rotation=90)
 if(legend_position=='right'){lh<-text_height(legend_title,legend_title_size);rowh<-max(legend_key_mm,text_height('BP',legend_size))+legend_gap_mm;totalh<-lh+2+length(present_groups)*rowh;ly<-(bottom+top+totalh)/2;label(legend_title,legend_left,ly-lh/2,legend_title_size,'legend_title',just='left');for(j in seq_along(present_groups)){g<-present_groups[j];yy<-ly-lh-2-(j-.5)*rowh;rects[[length(rects)+1]]<-data.frame(rect_id=length(rects)+1,bar_id=NA_integer_,left_mm=legend_left,right_mm=legend_left+legend_key_mm,bottom_mm=yy-legend_key_mm/2,top_mm=yy+legend_key_mm/2,color=palette[g],border_color='transparent',border_width=0,alpha=1,role='legend_key',panel='legend',value=NA_real_,ontology=g,visible=TRUE);label(g,legend_left+legend_key_mm+2,yy,legend_size,'legend_label',just='left')}}
 label(caption,width_mm/2,caption_y,caption_size,'caption');if(nzchar(title))label(title,width_mm/2,height_mm-text_height(title,title_size)/2-.4,title_size,'title')
 labels<-do.call(rbind,labels);boxes<-list();corners_all<-list();sat<-function(a,b){for(poly in list(a,b)){e<-poly[c(2:4,1),]-poly;for(j in 1:4){v<-c(-e[j,2],e[j,1]);v<-v/sqrt(sum(v^2));aa<-a%*%v;bb<-b%*%v;if(max(aa)<=min(bb)+.05||max(bb)<=min(aa)+.05)return(FALSE)}};TRUE};for(k in seq_len(nrow(labels))){z<-labels[k,];w<-tw(z$label,z$size,z$family,z$face);h<-text_height(z$label,z$size,z$family);xs<-if(z$just=='right')-w else if(z$just=='centre')-w/2 else 0;corners<-rbind(c(xs,-h/2),c(xs+w,-h/2),c(xs+w,h/2),c(xs,h/2));th<-z$rotation*pi/180;rc<-cbind(corners[,1]*cos(th)-corners[,2]*sin(th)+z$x_mm,corners[,1]*sin(th)+corners[,2]*cos(th)+z$y_mm);b<-c(min(rc[,1]),max(rc[,1]),min(rc[,2]),max(rc[,2]));if(b[1]<.15||b[2]>width_mm-.15||b[3]<.15||b[4]>height_mm-.15)stop('Text outside canvas: ',z$label,call.=FALSE);if(length(corners_all)&&any(vapply(corners_all,function(q)sat(rc,q),logical(1))))stop('Text collision: ',z$label,call.=FALSE);boxes[[k]]<-b;corners_all[[k]]<-rc};label_geometry<-cbind(labels,data.frame(left_mm=vapply(boxes,'[',numeric(1),1),right_mm=vapply(boxes,'[',numeric(1),2),bottom_mm=vapply(boxes,'[',numeric(1),3),top_mm=vapply(boxes,'[',numeric(1),4)))
},finally={dev.off();unlink(probe)})
bind<-function(z)if(length(z))do.call(rbind,z)else data.frame();lines<-bind(lines);rects<-bind(rects);panels<-bind(panels)
render<-function(){grid::grid.newpage();mm<-function(z)grid::unit(z,'mm');for(k in seq_len(nrow(rects))){z<-rects[k,];if(z$visible)grid::grid.rect(mm(mean(c(z$left_mm,z$right_mm))),mm(mean(c(z$bottom_mm,z$top_mm))),width=mm(z$right_mm-z$left_mm),height=mm(z$top_mm-z$bottom_mm),gp=grid::gpar(fill=z$color,col=z$border_color,lwd=z$border_width*72.27/25.4))};for(id in c(unique(lines$line_id[lines$role!='zero_bar']),unique(lines$line_id[lines$role=='zero_bar']))){z<-lines[lines$line_id==id,];grid::grid.lines(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(col=z$color[1],lwd=z$width[1]*72.27/25.4,lty=z$linetype[1],lineend='butt',linejoin='round'))};for(k in seq_len(nrow(labels))){z<-labels[k,];grid::grid.text(z$label,mm(z$x_mm),mm(z$y_mm),just=z$just,rot=z$rotation,gp=grid::gpar(fontfamily=z$family,fontsize=z$size,fontface=z$face,col=z$color))}}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
tables<-c('observations','bars','panels','rects','lines','labels','label_geometry');for(n in tables)write.csv(get(n),file.path(output_dir,paste0('actual_',n,'.csv')),row.names=FALSE);saveRDS(mget(c('raw',tables)),file.path(output_dir,'parsed-analysis.RDS'))
jsonlite::write_json(list(raw_rows=nrow(raw),terms=nterms,ontology_column=ontology_column,term_column=term_column,count_column=count_column,id_column=id_column,group_order=groups,present_groups=present_groups,sort_mode=sort_mode,width_mm=width_mm,height_mm=height_mm,dpi=dpi,bar_width=bar_width,bar_border_width=bar_border_width,axis_width=axis_width,legend_position=legend_position,legend_key_mm=legend_key_mm,legend_gap_mm=legend_gap_mm,x_domain=c(0,axis_max),x_ticks=xticks,family=family,text_font_files=text_font_files,palette=as.list(palette),scope='Complete selected GO term counts, exact nonnegative integers, allraw/extra/P/ratio/geneID preserved; no row filtering, enrichment test recomputation, normalization or unique-gene total inference. BP/CC/MF categorical identity; group order exact, within-group sort stable on source row, composite identity permits same term across different ontology. Native horizontal rectangles, same geometry3exports, no raster.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15,na='null');cat('Rendered',nterms,'GO ontology term counts; no enrichment tests recomputed\n')
