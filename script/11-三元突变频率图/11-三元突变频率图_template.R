#!/usr/bin/env Rscript

# Radial counts: use actual systems and recorded-count shares, never positional synthetic groups.
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
required_packages <- c("jsonlite", "svglite", "ragg", "readxl")
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








data_path<-resolve_input(text_param('data_path','',TRUE),file.path(script_dir,'assets/easy_input.csv'));raw<-read_table(data_path)
if(nrow(raw)<3||nrow(raw)>60||anyDuplicated(names(raw))||any(!nzchar(names(raw))))stop('Require 3..60 rows and complete unique columns',call.=FALSE)
data_mode<-choice('data_mode','auto',c('auto','easy','not_easy'));if(data_mode=='auto')data_mode<-if(all(c('Frequency.in.sGBM','Frequency.in.pGBM','Frequency.in.LGG')%in%names(raw)))'easy'else'not_easy'
column<-function(key,default){n<-text_param(key,'',TRUE);if(!nzchar(n))n<-default;if(!n%in%names(raw))stop('Missing requested column: ',n,call.=FALSE);n}
label_col<-column('label_col','Gene');x_col<-column('x_col',if(data_mode=='easy')'Frequency.in.pGBM'else'Frequency in pGBM');y_col<-column('y_col',if(data_mode=='easy')'Frequency.in.LGG'else'Frequency in LGG');z_col<-column('z_col',if(data_mode=='easy')'Frequency.in.sGBM'else'Frequency in sGBM');size_col<-column('size_col',if(data_mode=='easy')'allfrequency'else'Alteration Frequency in glioma')
if(anyDuplicated(c(x_col,y_col,z_col)))stop('Three frequency mappings must be distinct',call.=FALSE)
labels<-trimws(as.character(raw[[label_col]]));if(anyNA(labels)||any(!nzchar(labels))||anyDuplicated(labels)||any(grepl('[[:cntrl:]]',labels)))stop('Observation labels must be complete and unique',call.=FALSE)
freq<-sapply(c(x_col,y_col,z_col),function(n)parse_numeric(raw[[n]],n));if(any(!is.finite(freq))||any(freq<0|freq>1)||any(rowSums(freq)<=0))stop('Each frequency must be finite in [0,1], with positive row sum',call.=FALSE)
size_raw<-parse_numeric(raw[[size_col]],size_col);if(any(!is.finite(size_raw))||any(size_raw<=0))stop('Size values must be positive and finite',call.=FALSE)
size_threshold<-num('size_threshold',.1,0,1);size_high_multiplier<-num('size_high_multiplier',3,.1,100);size_low_multiplier<-num('size_low_multiplier',50,.1,1000)
score<-if(data_mode=='not_easy'){if(any(size_raw>1))stop('Overall input frequencies require [0,1]',call.=FALSE);ifelse(size_raw>=size_threshold,10^size_raw*size_high_multiplier,size_low_multiplier*size_raw)}else size_raw
share<-freq/rowSums(freq);maximum<-apply(share,1,max);ties<-rowSums(abs(share-maximum)<1e-12)>1;dominant<-c('pGBM','LGG','sGBM')[max.col(share,ties.method='first')];dominant[ties]<-'Tie'
font_family<-choice('font_family','Arial',c('Arial','Helvetica','Times New Roman'));family<-if(font_family=='Times New Roman')'serif'else'sans';base_size<-num('base_size',9,7,15);axis_text_size<-num('axis_text_size',7,5.5,12);axis_title_size<-num('axis_title_size',8.5,7,14);label_size<-num('label_size',2.6,2,3.5);label_pt<-label_size*72.27/25.4;point_alpha<-num('point_alpha',.85,.1,1);line_width<-num('line_width',.3,.1,1);point_min<-num('point_min',1.5,.2,10);point_max<-num('point_max',6.5,point_min,12);color_mode<-choice('color_mode','dominant',c('dominant','layered'));width_mm<-num('width_mm',180,150,300);height_mm<-num('height_mm',160,140,260);dpi<-integer_param('dpi',600,96,1200)
x_title<-text_param('x_title','Primary GBM');y_title<-text_param('y_title','Low grade glioma');z_title<-text_param('z_title','Secondary GBM');title<-text_param('title','Relative subtype alteration frequencies');if(any(grepl('[[:cntrl:]]|Case [0-9]|cover_|debug',c(title,x_title,y_title,z_title),ignore.case=TRUE)))stop('Titles must omit internal/control text',call.=FALSE)
# The legacy panel label is intentionally ignored: a stand-alone chart is not paper panel B.
colors<-c(pGBM='#0072B2',LGG='#009E73',sGBM='#D55E00',Tie='#72777D');fill<-unname(colors[dominant]);color_note<-'Color: largest normalized component; axes in percent; ties gray'
if(color_mode=='layered'){
 low<-text_param('color_low','#E31A1C');mid<-text_param('color_mid','#2447D8');high<-text_param('color_high','#8E2AAE');midpoint<-num('color_midpoint',.33,0,1);rgb<-tryCatch(grDevices::col2rgb(c(low,mid,high))/255,error=function(e)stop('Invalid gradient colors',call.=FALSE));lo<-min(freq);hi<-max(freq);map<-function(v){if(lo==hi)return(rgb[,2]);if(v<=midpoint){a<-if(midpoint<=lo)0 else max(0,min(1,(v-lo)/(midpoint-lo)));rgb[,1]*(1-a)+rgb[,2]*a}else{a<-if(hi<=midpoint)0 else max(0,min(1,(v-midpoint)/(hi-midpoint)));rgb[,2]*(1-a)+rgb[,3]*a}};fill<-vapply(seq_len(nrow(freq)),function(i){v<-rep(1,3);for(j in c(3,1,2))v<-map(freq[i,j])*point_alpha+v*(1-point_alpha);grDevices::rgb(v[1],v[2],v[3])},character(1));color_note<-'Color: legacy three-layer frequency blend; not a unique scalar'
}
diameter<-point_max*sqrt(score/max(score));if(any(diameter<point_min))stop('Area-proportional bubbles fall below point_min; lower the visibility guard or revise explicit size mapping',call.=FALSE)
tri_width<-min(width_mm-56,(height_mm-65)*2/sqrt(3));tri_height<-tri_width*sqrt(3)/2;left<-(width_mm-tri_width)/2;bottom<-28;apex_x<-width_mm/2;apex_y<-bottom+tri_height
px<-left+tri_width*(share[,3]+share[,2]/2);py<-bottom+tri_height*share[,2];records<-data.frame(label=labels,pGBM=freq[,1],LGG=freq[,2],sGBM=freq[,3],row_sum=rowSums(freq),pGBM_share=share[,1],LGG_share=share[,2],sGBM_share=share[,3],size_input=size_raw,size_score=score,diameter_mm=diameter,dominant=dominant,fill=fill,x_mm=px,y_mm=py);positions<-NULL
caption<-c('Position = each supplied subtype frequency / sum of its three subtype frequencies.',if(data_mode=='easy')'Circle area = supplied tutorial display score; it is not a raw alteration frequency.'else'Size score = legacy piecewise transform of overall frequency; area maps this score.', 'Raw frequencies and size inputs are retained in the exported table; no significance test.')
# Deterministic labels are placed in measured mm once, then shared by every device.
probe<-file.path(output_dir,'label-probe.pdf');grDevices::pdf(probe,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE)
tryCatch({grid::grid.newpage();tw<-function(s,size,face='plain')grid::convertWidth(grid::grobWidth(grid::textGrob(s,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face))),'mm',TRUE);if(tw(title,base_size+1.5,'bold')>width_mm-12)stop('Title too wide',call.=FALSE);for(s in c(caption,color_note))if(tw(s,6)>width_mm-12)stop('Caption too wide',call.=FALSE)
for(s in c(x_title,z_title))if(tw(s,axis_title_size,'bold')>55)stop('Lower vertex title too wide; use a concise title',call.=FALSE);if(tw(y_title,axis_title_size,'bold')>width_mm-50)stop('Top vertex title too wide',call.=FALSE)
lw<-vapply(labels,tw,numeric(1),size=label_pt)+1;lh<-label_pt*25.4/72*1.3;boxes<-matrix(numeric(),0,4);reserve<-function(s,x,y,size,face='plain'){w<-tw(s,size,face)+.8;h<-size*25.4/72*1.3;boxes<<-rbind(boxes,c(x-w/2,x+w/2,y-h/2,y+h/2))};for(v in seq(0,1,.2)){reserve(as.character(round((1-v)*100)),left+tri_width*v/2-5,bottom+tri_height*v,axis_text_size);reserve(as.character(round(v*100)),left+tri_width*(1-v/2)+5,bottom+tri_height*v,axis_text_size);reserve(as.character(round(v*100)),left+tri_width*v,bottom-4.5,axis_text_size)};reserve(x_title,left,bottom-10,axis_title_size,'bold');reserve(y_title,apex_x,apex_y+6,axis_title_size,'bold');reserve(z_title,left+tri_width,bottom-10,axis_title_size,'bold');positions<-records[,c('label','x_mm','y_mm')];positions$label_x_mm<-positions$label_y_mm<-NA_real_;bounds<-c(8,width_mm-8,bottom+1,apex_y-4)
for(i in order(-diameter,seq_along(diameter))){best<-NULL;bestcost<-Inf;for(dist in seq(diameter[i]/2+lh/2+1,40,by=2))for(a in seq(0,345,by=15)){x<-px[i]+dist*cos(a*pi/180);y<-py[i]+dist*sin(a*pi/180);b<-c(x-lw[i]/2,x+lw[i]/2,y-lh/2,y+lh/2);if(b[1]<bounds[1]||b[2]>bounds[2]||b[3]<bounds[3]||b[4]>bounds[4])next;if(nrow(boxes)&&any(b[1]<boxes[,2]+.7&b[2]>boxes[,1]-.7&b[3]<boxes[,4]+.7&b[4]>boxes[,3]-.7))next;dx<-pmax(b[1]-px,0,px-b[2]);dy<-pmax(b[3]-py,0,py-b[4]);if(any(sqrt(dx^2+dy^2)<diameter/2+.5))next;cost<-dist+.001*abs(a-90);if(cost<bestcost){best<-c(x,y);bestcost<-cost}}
if(is.null(best))stop('All labels cannot fit at requested physical size; enlarge canvas or reduce number of rows',call.=FALSE);positions$label_x_mm[i]<-best[1];positions$label_y_mm[i]<-best[2];boxes<-rbind(boxes,c(best[1]-lw[i]/2,best[1]+lw[i]/2,best[2]-lh/2,best[2]+lh/2))}
},finally=grDevices::dev.off());unlink(probe)
render<-function(kind){p<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(p,width=width_mm,height=height_mm,units='mm',res=dpi,bg='white')else if(kind=='pdf')grDevices::pdf(p,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE)else svglite::svglite(p,width=width_mm/25.4,height=height_mm/25.4,bg='white');tryCatch({grid::grid.newpage();text<-function(s,x,y,size=7,face='plain',just='center',col='#25313C')grid::grid.text(s,grid::unit(x,'mm'),grid::unit(y,'mm'),just=just,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face,col=col));line<-function(x,y,col='#D6DDE2',lwd=.4,lty=1)grid::grid.lines(grid::unit(x,'mm'),grid::unit(y,'mm'),gp=grid::gpar(col=col,lwd=lwd,lty=lty));text(title,6,height_mm-6,base_size+1.5,'bold','left');text(color_note,6,height_mm-12,6,just='left')
if(color_mode=='dominant')for(j in seq_along(colors)){x<-6+(j-1)*27;grid::grid.circle(grid::unit(x+1,'mm'),grid::unit(height_mm-21,'mm'),grid::unit(1.2,'mm'),gp=grid::gpar(fill=colors[j],col=NA));text(names(colors)[j],x+4,height_mm-21,6.5,just='left')}
vals<-unique(c(min(score),median(score),max(score)));for(j in seq_along(vals)){x<-width_mm-60+(j-1)*18;r<-point_max*sqrt(vals[j]/max(score))/2;grid::grid.circle(grid::unit(x,'mm'),grid::unit(height_mm-21,'mm'),grid::unit(r,'mm'),gp=grid::gpar(fill='#E8ECEF',col='#52606D',lwd=.4));text(format(vals[j],digits=3),x,height_mm-27,6)};text('Display score',width_mm-38,height_mm-15,6.5)
xy<-function(s)c(left+tri_width*(s[3]+s[2]/2),bottom+tri_height*s[2]);for(v in seq(.2,.8,.2))for(k in 1:3){others<-setdiff(1:3,k);a<-b<-rep(0,3);a[k]<-b[k]<-v;a[others[1]]<-1-v;b[others[2]]<-1-v;p1<-xy(a);p2<-xy(b);line(c(p1[1],p2[1]),c(p1[2],p2[2]))}
line(c(left,apex_x,left+tri_width,left),c(bottom,apex_y,bottom,bottom),'#52606D',1)
for(v in seq(0,1,.2)){p<-xy(c(1-v,v,0));text(as.character(round((1-v)*100)),p[1]-5,p[2],axis_text_size,col='#0072B2');p<-xy(c(0,v,1-v));text(as.character(round(v*100)),p[1]+5,p[2],axis_text_size,col='#009E73');p<-xy(c(1-v,0,v));text(as.character(round(v*100)),p[1],p[2]-4.5,axis_text_size,col='#D55E00')}
text(x_title,left,bottom-10,axis_title_size,'bold',col=colors[1]);text(y_title,apex_x,apex_y+6,axis_title_size,'bold',col=colors[2]);text(z_title,left+tri_width,bottom-10,axis_title_size,'bold',col=colors[3]);for(i in seq_len(nrow(records))){dx<-positions$label_x_mm[i]-px[i];dy<-positions$label_y_mm[i]-py[i];d<-sqrt(dx^2+dy^2);w<-grid::convertWidth(grid::grobWidth(grid::textGrob(labels[i],gp=grid::gpar(fontfamily=family,fontsize=label_pt))),'mm',TRUE)+1;h<-label_pt*25.4/72*1.3;fraction<-min(if(dx==0)Inf else w/2/abs(dx),if(dy==0)Inf else h/2/abs(dy));line(c(px[i]+dx*diameter[i]/2/d,positions$label_x_mm[i]-dx*fraction),c(py[i]+dy*diameter[i]/2/d,positions$label_y_mm[i]-dy*fraction),'#8A9197',line_width*72.27/25.4)}
for(i in seq_len(nrow(records)))grid::grid.circle(grid::unit(px[i],'mm'),grid::unit(py[i],'mm'),grid::unit(diameter[i]/2,'mm'),gp=grid::gpar(fill=if(color_mode=='layered')fill[i]else grDevices::adjustcolor(fill[i],alpha.f=point_alpha),col='#25313C',lwd=.55))
for(i in seq_len(nrow(records)))text(labels[i],positions$label_x_mm[i],positions$label_y_mm[i],label_pt)
for(i in seq_along(caption))text(caption[i],6,10-(i-1)*2.8,6,just='left',col='#52606D')},finally=grDevices::dev.off())};for(kind in c('png','pdf','svg'))render(kind)
utils::write.csv(records,file.path(output_dir,'ternary_values.csv'),row.names=FALSE);utils::write.csv(positions,file.path(output_dir,'label_positions.csv'),row.names=FALSE);saveRDS(list(raw=raw,frequencies=freq,shares=share,records=records,labels=positions),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(input=data_path,data_mode=data_mode,n_observations=nrow(records),mapping=list(label=label_col,x=x_col,y=y_col,z=z_col,size=size_col),size_encoding='Circle area proportional to explicit supplied or legacy-transformed display score; no false overall frequency legend',color_mode=color_mode,color_note=color_note,normalization='Per-row division by sum of three independent subtype frequencies; not within-cohort absolute incidence',row_sum_range=range(rowSums(freq)),geometry=list(width_mm=width_mm,height_mm=height_mm,left_mm=left,bottom_mm=bottom,triangle_width_mm=tri_width,triangle_height_mm=tri_height),caption=caption),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered ',nrow(records),' complete ternary observations; normalized positions and explicit display scores\n')
