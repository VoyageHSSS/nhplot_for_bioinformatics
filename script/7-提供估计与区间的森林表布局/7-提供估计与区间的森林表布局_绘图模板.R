#!/usr/bin/env Rscript

# Supplied estimates/bounds only; default values transcribed from reference, not empirical.
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
required_packages <- c("jsonlite", "svglite", "ragg", "showtext", "sysfonts", "systemfonts", "scales")
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
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}
if(!is.null(cfg$data_path))stop('data_path must be declared params.data_path',call.=FALSE)
data_path<-resolve_input(text_param('data_path','assets/forest_data.csv'),'assets/forest_data.csv')
ext<-tolower(tools::file_ext(data_path));if(!ext%in%c('csv','tsv'))stop('Only CSV/TSV input is supported',call.=FALSE)
sep<-if(ext=='tsv')'\t'else',';fields<-utils::count.fields(data_path,sep=sep,quote='"',comment.char='',blank.lines.skip=TRUE);fields<-fields[!is.na(fields)];if(!length(fields)||any(fields!=fields[1]))stop('Every record must match the header field count',call.=FALSE)
raw_data<-utils::read.table(data_path,header=TRUE,sep=sep,quote='"',comment.char='',check.names=FALSE,stringsAsFactors=FALSE,colClasses='character',encoding='UTF-8',na.strings=character(),fill=FALSE);raw_headers<-names(raw_data);names(raw_data)<-trimws(sub('^\ufeff','',enc2utf8(names(raw_data))))
required<-c('risk_factor','estimate','lower','upper','p_value','contribution','pooled');if(anyDuplicated(names(raw_data))||!all(required%in%names(raw_data))||nrow(raw_data)!=9)stop('Need nine rows with unique columns: ',paste(required,collapse=','),call.=FALSE)
identity<-trimws(raw_data$risk_factor);if(anyNA(identity)||any(!nzchar(identity))||any(grepl('[\r\n\t]',identity))||anyDuplicated(identity))stop('Nine factor/summary identities must be nonempty and unique',call.=FALSE)
pool_key<-tolower(trimws(raw_data$pooled));if(any(!pool_key%in%c('true','false','t','f','1','0')))stop('pooled must be TRUE/FALSE or 1/0, no missing',call.=FALSE);pool<-pool_key%in%c('true','t','1');if(sum(pool)!=1)stop('Need eight factor rows and exactly one supplied summary',call.=FALSE)
d<-data.frame(source_row=1:9,risk_factor=identity,estimate=parse_numeric(raw_data$estimate,'estimate'),lower=parse_numeric(raw_data$lower,'lower'),upper=parse_numeric(raw_data$upper,'upper'),p_value=parse_numeric(raw_data$p_value,'p_value'),contribution=parse_numeric(raw_data$contribution,'contribution'),pooled=pool,stringsAsFactors=FALSE)
if(any(!is.finite(unlist(d[c('estimate','lower','upper','contribution')]))))stop('Estimates/bounds/contribution must be complete finite values',call.=FALSE)
if(any(d$lower<=0|d$estimate<=0|d$upper<=0|d$lower>d$estimate|d$estimate>d$upper))stop('Provided ratios require 0<lower<=estimate<=upper',call.=FALSE)
if(any(d$contribution<0|d$contribution>100)||any(!is.na(d$p_value)&(!is.finite(d$p_value)|d$p_value<0|d$p_value>1)))stop('Provided percent must be 0..100; optional p must be empty or 0..1',call.=FALSE)
for(k in 'cap_height')if(!is.null(gp(k)))stop(k,' retired; use cap_height_mm for explicit physical size',call.=FALSE)
width_mm<-num('width_mm',200,190,340);height_mm<-num('height_mm',136,130,240);dpi<-integer_param('dpi',600,96,1200)
family<-choice('family','Times New Roman',c('Times New Roman','Arial','Helvetica'));requested_family<-family
header_size<-num('header_size',11,9,14);row_text_size<-num('row_text_size',10.5,8,13);value_text_size<-num('value_text_size',10,8,12);axis_text_size<-num('axis_text_size',8.5,7.5,11);axis_title_size<-num('axis_title_size',10.5,9,14);annotation_size<-num('annotation_size',8,7,10);caption_size<-num('caption_size_pt',6.5,6.5,8)
point_size<-num('point_size',3,2,4);ci_line_width<-num('ci_line_width',.60,.3,.9);cap_height_mm<-num('cap_height_mm',2.4,1.5,4);axis_line_width<-num('axis_line_width',.35,.2,.6);reference_line_width<-num('reference_line_width',.30,.2,.5);grid_width<-num('grid_width',.18,.1,.3);summary_height_mm<-num('summary_height_mm',4,3,6)
axis_scale<-choice('axis_scale','ratio_linear',c('ratio_linear','ratio_log'));show_reported_p<-bool('show_reported_p',FALSE)
clean<-function(z){z<-gsub('(?:案例|Case)[[:space:]_#-]*[0-9]+[[:space:]]*[-—–:：.]?[[:space:]]*','',z,perl=TRUE);trimws(gsub('cover_[0-9]+\\.(webp|png|jpg)|任务[[:space:]]*[0-9]+|debug[^[:space:]]*','',z,perl=TRUE,ignore.case=TRUE))}
if(any(!nzchar(clean(identity))))stop('Factor identities cannot consist only of internal case/file labels',call.=FALSE)
x_label<-clean(text_param('x_label','Provided ratio'));if(!nzchar(x_label))x_label<-'Provided ratio'
color<-function(z,k){if(length(z)!=1||is.list(z)||!grepl('^#[0-9a-fA-F]{6}$',z))stop(k,' must be one #RRGGBB',call.=FALSE);toupper(z)}
header_color<-color(text_param('header_color','#2D6669'),'header_color');text_color<-color(text_param('text_color','#111111'),'text_color');grid_color<-color(text_param('grid_color','#D8D8D8'),'grid_color');background_color<-color(text_param('background_color','#FFFFFF'),'background_color')
default_cols<-c('#E9364A','#D80731','#9FD4CF','#477FA2','#244260','#F3A459','#E66545','#2A9B8F','#294B55');cols<-gp('palette_colors',default_cols);if(is.list(cols))cols<-unlist(cols,use.names=FALSE);if(length(cols)==1)cols<-trimws(strsplit(as.character(cols),'[,;]')[[1]]);if(length(cols)!=9||anyNA(cols))stop('palette_colors must have exactly nine complete colors in source row order',call.=FALSE);cols<-vapply(cols,color,character(1),k='palette_colors');mapping<-cfg$palette_mapping;palette_source<-cfg$palette_source %||% 'original';if(!is.character(palette_source)||length(palette_source)!=1)stop('palette_source must be scalar',call.=FALSE)
if(palette_source!='original'){if(!is.list(mapping)||is.null(names(mapping))||anyDuplicated(names(mapping))||!setequal(identity,names(mapping)))stop('Toolbar mapping must name every source identity without fallback',call.=FALSE);cols<-vapply(identity,function(k)color(mapping[[k]],k),character(1))}
cols<-unname(cols);if(any(cols==background_color)||text_color==background_color||header_color==background_color||grid_color==background_color)stop('Elements must be visible on background',call.=FALSE)
forest_left_ratio<-num('forest_left_ratio',.267,.255,.28);forest_width_ratio<-num('forest_width_ratio',.333,.31,.35);forest_bottom_ratio<-num('forest_bottom_ratio',.152,.145,.17);forest_top_ratio<-num('forest_top_ratio',.815,.79,.83);header_height_ratio<-num('header_height_ratio',.073,.065,.085);rail_width_ratio<-num('rail_width_ratio',.052,.045,.06);row_gap_mm<-num('row_gap_mm',0,0,1)
# Keep the reference linear ratio axis by default; optional log mode is explicit.
x_limits<-c(min(.85,d$lower),max(1.10,d$upper));if(min(d$lower)<.85||max(d$upper)>1.10){if(axis_scale=='ratio_log')x_limits<-exp(log(x_limits)+c(-1,1)*diff(log(x_limits))*.03)else x_limits<-x_limits+c(-1,1)*diff(x_limits)*.03};if(x_limits[1]<=0)x_limits[1]<-min(d$lower)*.9
x_transform<-if(axis_scale=='ratio_log')log else function(z)z
ticks<-if(axis_scale=='ratio_log'&&x_limits[2]/x_limits[1]>4)10^seq(floor(log10(x_limits[1])),ceiling(log10(x_limits[2])))else pretty(x_limits,n=5);ticks<-ticks[ticks>=x_limits[1]&ticks<=x_limits[2]];if(length(ticks)<2)ticks<-x_limits
is_demo<-identical(readBin(data_path,'raw',n=file.info(data_path)$size),readBin(file.path(script_dir,'assets/forest_data.csv'),'raw',n=file.info(file.path(script_dir,'assets/forest_data.csv'))$size))
font_file<-function(){h<-systemfonts::match_fonts(family);if(h$index!=0L){family<<-'Arial';h<-systemfonts::match_fonts(family)};h$path}
regular_file<-font_file();bh<-systemfonts::match_fonts(family,weight='bold');bold_file<-if(bh$index==0L)bh$path else '/System/Library/Fonts/Supplemental/Arial Bold.ttf';sysfonts::font_add(family,regular=regular_file,bold=bold_file);text_font_files<-setNames(list(regular_file),family);text_bold_font_files<-setNames(list(bold_file),family)
text_font_for<-function(z){chars<-unique(strsplit(enc2utf8(as.character(z)),'',fixed=TRUE)[[1]]);supports<-function(f)all(systemfonts::glyph_info(chars,path=text_font_files[[f]],index=0)$index>0);if(supports(family))return(family);fallback<-'Arial Unicode MS';if(is.null(text_font_files[[fallback]])){h<-systemfonts::match_fonts(fallback);if(h$index!=0)stop('Unicode font must have addressable regular face',call.=FALSE);text_font_files[[fallback]]<<-h$path;text_bold_font_files[[fallback]]<<-h$path;sysfonts::font_add(fallback,regular=h$path)};if(!supports(fallback))stop('Font lacks displayed glyph: ',z,call.=FALSE);fallback}
probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()
tw<-function(z,s,fontfamily=if(nzchar(z))text_font_for(z)else family,face='plain')if(!nzchar(z))0 else grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=fontfamily,fontsize=s,fontface=face))),'mm',TRUE)
text_height<-function(z,s,ff=if(nzchar(z))text_font_for(z)else family,face='plain'){if(!nzchar(z))return(0);q<-systemfonts::glyph_info(strsplit(enc2utf8(z),'',fixed=TRUE)[[1]],path=if(face=='bold')text_bold_font_files[[ff]]else text_font_files[[ff]],index=0,size=s,res=720);(max(q$y_bearing)-min(q$y_bearing-q$height))/10*25.4/72+.12}
fmt<-function(v)if(max(abs(v))>=1e5||max(abs(v))<.001)vapply(v,function(z)sprintf('%.3g',z),character(1))else format(v,trim=TRUE,digits=4,scientific=FALSE)
points<-list();polygons<-list();rects<-list();lines<-list();labels<-list();panels<-list();summaries<-list()
label<-function(z,x,y,s,role,just='centre',rotation=0,face='plain',color=text_color){z<-clean(z);if(!nzchar(z))return();ff<-text_font_for(z);if(ff!=family)face<-'plain';labels[[length(labels)+1]]<<-data.frame(label=z,x_mm=x,y_mm=y,size=s,role=role,just=just,color=color,rotation=rotation,face=face,family=ff)}
line<-function(x,y,role,col=text_color,lw=axis_line_width,lty='solid'){lines[[length(lines)+1]]<<-data.frame(line_id=length(lines)+1,vertex=seq_along(x),x_mm=x,y_mm=y,color=unname(col),width=lw,lty=lty,role=role)}
rect<-function(l,r,b,t,fill,role,color=NA_character_,lw=0){rects[[length(rects)+1]]<<-data.frame(left_mm=l,right_mm=r,bottom_mm=b,top_mm=t,fill=unname(fill),color=color,width=lw,role=role)}
polygon<-function(x,y,fill,row,role){polygons[[length(polygons)+1]]<<-data.frame(polygon_id=length(polygons)+1,vertex=seq_along(x),x_mm=x,y_mm=y,fill=unname(fill),source_row=row,role=role)}
ink<-function(fill){v<-as.numeric(grDevices::col2rgb(fill))/255;v<-ifelse(v<=.04045,v/12.92,((v+.055)/1.055)^2.4);lum<-sum(v*c(.2126,.7152,.0722));if((lum+.05)/.05>=1.05/(lum+.05))'#000000'else'#FFFFFF'}
tryCatch({
 fl<-forest_left_ratio*width_mm;fr<-fl+forest_width_ratio*width_mm;fb<-forest_bottom_ratio*height_mm;ft<-forest_top_ratio*height_mm;ht<-ft+header_height_ratio*height_mm;rl<-fl-rail_width_ratio*width_mm;row_pitch<-(ft-fb)/9;yy<-ft-(seq_len(9)-.5)*row_pitch;factor_x<-rl-5;percent_x<-.685*width_mm;bounds_x<-.85*width_mm
 if(fr+4>.63*width_mm||rl<.20*width_mm||row_pitch<9.0||row_gap_mm>=row_pitch/2)stop('Table column rails or row pitch do not fit',call.=FALSE)
 mx<-function(z)fl+(x_transform(z)-x_transform(x_limits[1]))/diff(x_transform(x_limits))*(fr-fl)
 rect(fl,fr,ft,ht,header_color,'forest_heading',text_color,axis_line_width)
 label('Factors',factor_x,mean(c(ft,ht)),header_size,'factor_header',just='right',face='bold');label('Provided estimates',mean(c(fl,fr)),mean(c(ft,ht)),header_size,'forest_header',face='bold',color=ink(header_color));label('Provided %',percent_x,mean(c(ft,ht)),header_size,'percent_header',face='bold');label('Estimate (bounds)',bounds_x,mean(c(ft,ht)),header_size,'bounds_header',face='bold')
 for(v in ticks)line(rep(mx(v),2),c(fb,ft),'grid',grid_color,grid_width);line(rep(mx(1),2),c(fb,ft),'null_ratio_guide',text_color,reference_line_width,'dashed');line(c(fl,fr,fr,fl,fl),c(fb,fb,ft,ft,fb),'forest_frame')
 for(i in 1:9){z<-d[i,];yb<-yy[i];rect(rl,fl,yb-row_pitch/2+row_gap_mm/2,yb+row_pitch/2-row_gap_mm/2,cols[i],'row_color_rail',background_color,.15);ff<-if(z$pooled)'bold'else'plain';display_name<-if(is_demo&&z$pooled)'Provided summary'else z$risk_factor;label(display_name,factor_x,yb,row_text_size,'factor_label',just='right',face=ff);label(sprintf('%.1f%%',z$contribution),percent_x,yb,value_text_size,'provided_percent',face=ff);label(sprintf('%.2f (%.2f, %.2f)',z$estimate,z$lower,z$upper),bounds_x,yb,value_text_size,'provided_estimate_bounds',face=ff)
 if(z$pooled){polygon(mx(c(z$lower,z$estimate,z$upper,z$estimate)),yb+c(0,summary_height_mm/2,0,-summary_height_mm/2),cols[i],i,'supplied_summary_diamond')}else{line(mx(c(z$lower,z$upper)),rep(yb,2),'provided_interval',cols[i],ci_line_width);for(v in c(z$lower,z$upper))line(rep(mx(v),2),yb+c(-1,1)*cap_height_mm/2,'provided_bound_cap',cols[i],ci_line_width);polygon(mx(z$estimate)+c(-1,0,1,0)*point_size/2,yb+c(0,1,0,-1)*point_size/2,cols[i],i,'provided_estimate_diamond')}
 if(show_reported_p&&!is.na(z$p_value))label(sprintf('p supplied: %.3g',z$p_value),bounds_x,yb+3.5,annotation_size,'supplied_p_only')
 summaries[[i]]<-data.frame(source_row=i,risk_factor=z$risk_factor,display_name=display_name,estimate=z$estimate,lower=z$lower,upper=z$upper,contribution=z$contribution,p_value=z$p_value,pooled=z$pooled,x_mm=mx(z$estimate),lower_mm=mx(z$lower),upper_mm=mx(z$upper),y_mm=yb,color=cols[i])
 }
 for(v in ticks){line(rep(mx(v),2),c(fb,fb-1.0),'x_tick');label(format(v,trim=TRUE,scientific=FALSE,digits=3),mx(v),fb-4.2,axis_text_size,'x_tick_label',rotation=45)}
 label(paste0(x_label,if(axis_scale=='ratio_log')' (log scale)'else' (linear scale)'),mean(c(fl,fr)),fb-10.5,axis_title_size,'x_axis_title',face='bold')
 label(if(is_demo)'Illustrative transcribed values; not empirical data. Bounds, percentages and summary are supplied.'else'User-supplied values/bounds/percentages; no model fit or pooling performed.',width_mm/2,5.8,caption_size,'provenance_caption');label(if(show_reported_p)'Shown p values are supplied only; no test or confidence level is calculated.'else'No p values or confidence level inferred; ratio=1 is a visual guide.',width_mm/2,2.7,caption_size,'method_caption')
 panels<-data.frame(panel='provided_forest',left_mm=fl,right_mm=fr,bottom_mm=fb,top_mm=ft,x_min=x_limits[1],x_max=x_limits[2],scale=axis_scale)
 labels<-do.call(rbind,labels);boxes<-list();corners<-list();sat<-function(a,b){for(poly in list(a,b)){e<-poly[c(2:4,1),]-poly;for(j in 1:4){v<-c(-e[j,2],e[j,1]);v<-v/sqrt(sum(v^2));aa<-a%*%v;bb<-b%*%v;if(max(aa)<=min(bb)+.05||max(bb)<=min(aa)+.05)return(FALSE)}};TRUE}
 for(k in seq_len(nrow(labels))){z<-labels[k,];w<-tw(z$label,z$size,z$family,z$face);h<-text_height(z$label,z$size,z$family,z$face);xx<-if(z$just=='right')-w else if(z$just=='centre')-w/2 else 0;cc<-rbind(c(xx,-h/2),c(xx+w,-h/2),c(xx+w,h/2),c(xx,h/2));a<-z$rotation*pi/180;cc<-cbind(cc[,1]*cos(a)-cc[,2]*sin(a)+z$x_mm,cc[,1]*sin(a)+cc[,2]*cos(a)+z$y_mm);q<-c(min(cc[,1]),max(cc[,1]),min(cc[,2]),max(cc[,2]));if(q[1]<.2||q[2]>width_mm-.2||q[3]<.2||q[4]>height_mm-.2)stop('Text outside canvas: ',z$label,call.=FALSE);if(length(corners)&&any(vapply(corners,function(c)sat(cc,c),logical(1))))stop('Text collision: ',z$label,call.=FALSE);boxes[[k]]<-q;corners[[k]]<-cc};label_geometry<-cbind(labels,data.frame(left_mm=vapply(boxes,'[',numeric(1),1),right_mm=vapply(boxes,'[',numeric(1),2),bottom_mm=vapply(boxes,'[',numeric(1),3),top_mm=vapply(boxes,'[',numeric(1),4)))
},finally={dev.off();unlink(probe)})
bind<-function(x)if(length(x))do.call(rbind,x)else data.frame();for(n in c('lines','rects','polygons','summaries'))assign(n,bind(get(n)));observations<-d
render<-function(){grid::grid.newpage();mm<-function(z)grid::unit(z,'mm');grid::grid.rect(gp=grid::gpar(fill=background_color,col=NA));for(k in seq_len(nrow(rects))){z<-rects[k,];grid::grid.rect(mm(mean(c(z$left_mm,z$right_mm))),mm(mean(c(z$bottom_mm,z$top_mm))),width=mm(z$right_mm-z$left_mm),height=mm(z$top_mm-z$bottom_mm),gp=grid::gpar(fill=z$fill,col=z$color,lwd=z$width*96/25.4))};for(id in unique(lines$line_id)){z<-lines[lines$line_id==id,];grid::grid.lines(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(col=z$color[1],lwd=z$width[1]*96/25.4,lty=z$lty[1],lineend='butt'))};for(id in unique(polygons$polygon_id)){z<-polygons[polygons$polygon_id==id,];grid::grid.polygon(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(fill=z$fill[1],col=NA))};for(k in seq_len(nrow(labels))){z<-labels[k,];grid::grid.text(z$label,mm(z$x_mm),mm(z$y_mm),rot=z$rotation,just=z$just,gp=grid::gpar(fontfamily=z$family,fontface=z$face,fontsize=z$size,col=z$color))}}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background=background_color),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg=background_color));tryCatch(render(),finally=dev.off())}
tables<-c('raw_data','observations','summaries','panels','polygons','rects','lines','labels','label_geometry');for(n in tables)write.csv(get(n),file.path(output_dir,paste0('actual_',n,'.csv')),row.names=FALSE);saveRDS(mget(c('raw_headers','d',tables)),file.path(output_dir,'parsed-analysis.RDS'))
jsonlite::write_json(list(input=data_path,input_md5=as.character(tools::md5sum(data_path)),is_demo=is_demo,illustrative_only=is_demo,simulation_only=FALSE,width_mm=width_mm,height_mm=height_mm,dpi=dpi,family=family,font_family_requested=requested_family,text_font_files=text_font_files,text_bold_font_files=text_bold_font_files,x_limits=x_limits,ticks=ticks,axis_scale=axis_scale,show_reported_p=show_reported_p,palette_source=palette_source,row_colors=as.list(setNames(cols,identity)),header_color=header_color,text_color=text_color,grid_color=grid_color,background_color=background_color,header_size=header_size,row_text_size=row_text_size,value_text_size=value_text_size,axis_text_size=axis_text_size,axis_title_size=axis_title_size,annotation_size=annotation_size,caption_size_pt=caption_size,point_size=point_size,ci_line_width=ci_line_width,cap_height_mm=cap_height_mm,axis_line_width=axis_line_width,reference_line_width=reference_line_width,grid_width=grid_width,summary_height_mm=summary_height_mm,forest_left_ratio=forest_left_ratio,forest_width_ratio=forest_width_ratio,forest_bottom_ratio=forest_bottom_ratio,forest_top_ratio=forest_top_ratio,header_height_ratio=header_height_ratio,rail_width_ratio=rail_width_ratio,row_gap_mm=row_gap_mm,rows=nrow(d),pooled=sum(d$pooled),factor_contribution_sum=sum(d$contribution[!d$pooled]),summary_computed=FALSE,inference_computed=FALSE,confidence_level_inferred=FALSE,scope='Reference-only transcription for default, not empirical data. All raw seven columns/extra columns/context/order retained. Input9rows/eightfactors+one supplied summary required; all positive ratios and ordered bounds, supplied percent0..100 and optional supplied p0..1 validated; no assumption factor percents sum100. No fitting/tests/CI confidence level/summary inference. Default reference linear ratio axis retained; explicit ratio_log option only. Actual bounds contained, no clipping. Summary diamond provided lower/estimate/upper only, not recomputed; p hidden by default and explicit p-supplied option displays supplied values outside forest grid. Original9palette fixed, fully named toolbar overrides strict, neutral highcontrast table text. True-mm native vectors/fonts/sizes/scene and same logic threeformats.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15,na='null');cat('Rendered nine supplied rows/eight factors plus one supplied summary; scale',axis_scale,'; p supplied visible',show_reported_p,'\n')
