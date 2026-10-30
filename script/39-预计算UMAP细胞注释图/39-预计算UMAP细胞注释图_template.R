#!/usr/bin/env Rscript

# Precomputed cell embeddings: preserve all IDs, coordinates and supplied annotations.
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










read_embedding<-function(path){ext<-tolower(tools::file_ext(path));if(ext%in%c('txt','tsv')){first<-readLines(path,n=1,warn=FALSE);seps<-c('\t',';',',');counts<-vapply(seps,function(sep){con<-textConnection(first);on.exit(close(con));sum(utils::count.fields(con,sep=sep,quote='"',comment.char=''),na.rm=TRUE)},integer(1));if(max(counts)==1){fields<-utils::count.fields(path,sep='',quote='"',comment.char='',blank.lines.skip=TRUE);f<-fields[!is.na(fields)];implicit<-length(f)>1&&all(f[-1]==f[1]+1);if(!implicit&&any(f[-1]!=f[1]))stop('Whitespace records must match header fields or contain one consistent implicit cell ID',call.=FALSE);d<-utils::read.table(path,header=TRUE,row.names=if(implicit)1 else NULL,check.names=FALSE,stringsAsFactors=FALSE,colClasses='character',na.strings=character(),quote='"',comment.char='');if(implicit)d<-data.frame(cell_id=rownames(d),d,check.names=FALSE);return(d)}};read_table(path)}
data_path<-resolve_input(text_param('data_path','',TRUE),file.path(script_dir,'assets/GSE133345_Annotations.txt'));raw<-read_embedding(data_path);names(raw)<-trimws(names(raw));if(nrow(raw)<2||anyDuplicated(names(raw))||any(!nzchar(names(raw))))stop('At least two complete records and unique column names required',call.=FALSE);x_col<-text_param('x_col','UMAP1');y_col<-text_param('y_col','UMAP2');group_col<-text_param('group_col','cluster');cell_col<-text_param('cell_col',if('cell_id'%in%names(raw))'cell_id'else names(raw)[1]);if(length(unique(c(x_col,y_col,group_col,cell_col)))!=4||!all(c(x_col,y_col,group_col,cell_col)%in%names(raw)))stop('Actual distinct cell ID, group and two coordinate columns required',call.=FALSE);ids<-trimws(as.character(raw[[cell_col]]));groups<-trimws(as.character(raw[[group_col]]));x<-parse_numeric(raw[[x_col]],x_col);y<-parse_numeric(raw[[y_col]],y_col);if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids)||anyNA(groups)||any(!nzchar(groups))||any(grepl('[[:cntrl:]]',c(ids,groups,names(raw))))||any(!is.finite(x))||any(!is.finite(y)))stop('Every cell requires unique complete ID, annotation and finite coordinates; no silent filtering',call.=FALSE);if(diff(range(x))==0||diff(range(y))==0)stop('Both coordinate dimensions must vary for a 2D embedding',call.=FALSE)
levels<-sort(unique(groups),method='radix');if(length(levels)>20)stop('At most 20 labeled groups supported; colors alone do not certify distinguishability',call.=FALSE);cells<-data.frame(cell_id=ids,x=x,y=y,group=groups,stringsAsFactors=FALSE);centroids<-aggregate(cbind(x,y)~group,cells,mean);centroids$n<-as.integer(table(factor(groups,levels=centroids$group)));fixed<-c('#D1495B','#E07A5F','#F2A65A','#C9A227','#7AA95C','#2A9D8F','#2F80C0','#277DA1','#5E60CE','#8E5BB7','#B565A7','#D45087','#A44A6B','#6C8EAD','#43AA8B');palette_name<-choice('palette','fixed',c('fixed','Dark 3','Set 3','Warm','Cold'));if(palette_name=='fixed'){if(length(levels)>length(fixed))stop('Fixed palette supports at most 15 groups; select a full HCL palette explicitly',call.=FALSE);palette<-fixed[seq_along(levels)]}else palette<-grDevices::hcl.colors(length(levels),palette_name);names(palette)<-levels;cells$color<-unname(palette[cells$group]);centroids$color<-unname(palette[centroids$group])
font_family<-choice('font_family','Arial',c('Arial','Helvetica','Times New Roman'));family<-if(font_family=='Times New Roman')'serif'else'sans';base_size<-num('base_size',9,7,15);axis_text_size<-num('axis_text_size',7,6,11);point_size<-num('point_size',.65,.2,2);point_alpha<-num('point_alpha',.78,.1,1);label_size<-num('label_size',2.6,2,3.5);label_pt<-label_size*72.27/25.4;label_mode<-choice('label_mode','centroid',c('none','centroid'));label_color<-text_param('label_color','#25313C');tryCatch(invisible(grDevices::col2rgb(label_color)),error=function(e)stop('Invalid label color',call.=FALSE));show_legend<-bool('show_legend',TRUE);legend_position<-choice('legend_position','right',c('right','left','top','bottom'));width_mm<-num('width_mm',190,150,320);height_mm<-num('height_mm',110,100,260);dpi<-integer_param('dpi',600,96,1200);title<-text_param('title','Supplied UMAP cell annotations');if(grepl('[[:cntrl:]]|Case [0-9]|cover_|debug',title,ignore.case=TRUE))stop('Remove internal title',call.=FALSE)
xr<-range(x);yr<-range(y);xr<-xr+c(-1,1)*diff(xr)*.06;yr<-yr+c(-1,1)*diff(yr)*.08;if(any(!is.finite(c(xr,yr))))stop('Coordinate plotting range overflow',call.=FALSE);left<-17;right<-width_mm-6;bottom<-25;top<-height_mm-23;if(show_legend){if(legend_position=='right')right<-width_mm-43;if(legend_position=='left')left<-52;if(legend_position=='top')top<-height_mm-38;if(legend_position=='bottom')bottom<-40};unit_scale<-min((right-left)/diff(xr),(top-bottom)/diff(yr));pw<-diff(xr)*unit_scale;ph<-diff(yr)*unit_scale;left<-left+(right-left-pw)/2;bottom<-bottom+(top-bottom-ph)/2;if(pw<70||ph<38)stop('Equal-aspect embedding panel too small; enlarge canvas or move legend',call.=FALSE);xx<-function(v)left+(v-xr[1])*unit_scale;yy<-function(v)bottom+(v-yr[1])*unit_scale;cells$x_mm<-xx(cells$x);cells$y_mm<-yy(cells$y);centroids$x_mm<-xx(centroids$x);centroids$y_mm<-yy(centroids$y);centroids$label_x_mm<-centroids$label_y_mm<-centroids$leader_x_mm<-centroids$leader_y_mm<-NA_real_;caption<-c('Precomputed coordinates and supplied cell labels; no new UMAP, clustering, or cell filtering.','Equal coordinate scales; leader labels point to arithmetic centroids, not inferred lineage or significance.')
probe<-tempfile('umap-label-',tmpdir=tempdir(),fileext='.pdf');grDevices::pdf(probe,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE);tryCatch({grid::grid.newpage();tw<-function(s,size,face='plain')grid::convertWidth(grid::grobWidth(grid::textGrob(s,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face))),'mm',TRUE);if(tw(title,base_size+1.5,'bold')>width_mm-12)stop('Title too wide',call.=FALSE);for(s in caption)if(tw(s,6)>width_mm-12)stop('Caption too wide',call.=FALSE);legend_labels<-paste0(levels,' (',as.integer(table(factor(groups,levels=levels))),')');legend_widths<-vapply(legend_labels,tw,numeric(1),size=7)+5;legend_positions<-matrix(NA_real_,length(levels),2);if(show_legend){if(legend_position%in%c('left','right')){if(max(legend_widths)>36||length(levels)*4.5>height_mm-39)stop('Side legend too large; enlarge canvas or shorten confirmed annotations',call.=FALSE);legend_positions[,1]<-if(legend_position=='left')6 else width_mm-37;legend_positions[,2]<-height_mm-27-(seq_along(levels)-1)*4.5}else{row<-0;offset<-6;for(j in seq_along(levels)){if(offset+legend_widths[j]>width_mm-6){row<-row+1;offset<-6};if(row>1)stop('Legend requires more than 2 rows; select a side layout',call.=FALSE);legend_positions[j,]<-c(offset,if(legend_position=='top')height_mm-20-row*5 else 17+row*5);offset<-offset+legend_widths[j]}}}
if(label_mode=='centroid'){boxes<-matrix(numeric(),0,4);lh<-label_pt*25.4/72*1.35;for(i in order(-centroids$n,centroids$group)){w<-tw(centroids$group[i],label_pt)+1;if(w>pw*.45)stop('Cell group label too wide',call.=FALSE);best<-NULL;cost<-Inf;for(dist in seq(lh/2+.8,30,by=1.5))for(a in seq(0,345,by=15)){cx<-centroids$x_mm[i]+dist*cos(a*pi/180);cy<-centroids$y_mm[i]+dist*sin(a*pi/180);b<-c(cx-w/2,cx+w/2,cy-lh/2,cy+lh/2);if(b[1]<left+1||b[2]>left+pw-1||b[3]<bottom+1||b[4]>bottom+ph-1)next;if(nrow(boxes)&&any(b[1]<boxes[,2]+.6&b[2]>boxes[,1]-.6&b[3]<boxes[,4]+.6&b[4]>boxes[,3]-.6))next;dx<-pmax(b[1]-cells$x_mm,0,cells$x_mm-b[2]);dy<-pmax(b[3]-cells$y_mm,0,cells$y_mm-b[4]);if(any(sqrt(dx^2+dy^2)<point_size/2+.35))next;cst<-dist+.001*abs(a-90);if(cst<cost){best<-c(cx,cy,b);cost<-cst}};if(is.null(best))stop('Cannot place all centroid labels without hiding cells; enlarge canvas or use label_mode=none',call.=FALSE);centroids$label_x_mm[i]<-best[1];centroids$label_y_mm[i]<-best[2];boxes<-rbind(boxes,best[3:6]);vx<-best[1]-centroids$x_mm[i];vy<-best[2]-centroids$y_mm[i];t<-min(if(vx!=0)(w/2)/abs(vx)else Inf,if(vy!=0)(lh/2)/abs(vy)else Inf);centroids$leader_x_mm[i]<-best[1]-vx*t;centroids$leader_y_mm[i]<-best[2]-vy*t}}
},finally={grDevices::dev.off();unlink(probe)})
render<-function(kind){path<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(path,width=width_mm,height=height_mm,units='mm',res=dpi,bg='white')else if(kind=='pdf')grDevices::pdf(path,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE)else svglite::svglite(path,width=width_mm/25.4,height=height_mm/25.4,bg='white');tryCatch({grid::grid.newpage();text<-function(s,x,y,size=7,face='plain',just='center',col='#25313C',rot=0)grid::grid.text(s,grid::unit(x,'mm'),grid::unit(y,'mm'),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face,col=col));line<-function(x,y,col='#8A9197',lwd=.5)grid::grid.lines(grid::unit(x,'mm'),grid::unit(y,'mm'),gp=grid::gpar(col=col,lwd=lwd));circle<-function(x,y,r,fill)grid::grid.circle(grid::unit(x,'mm'),grid::unit(y,'mm'),grid::unit(r,'mm'),gp=grid::gpar(fill=fill,col=NA));text(title,6,height_mm-6,base_size+1.5,'bold','left');text(sprintf('%d cells; %d supplied groups; coordinate means used for label anchors',nrow(cells),length(levels)),6,height_mm-12,7,just='left');if(show_legend)for(j in seq_along(levels)){lx<-legend_positions[j,1];ly<-legend_positions[j,2];circle(lx+1,ly,.65,palette[j]);text(legend_labels[j],lx+3,ly,7,just='left')};xt<-pretty(xr,n=5);xt<-xt[xt>=xr[1]&xt<=xr[2]];yt<-pretty(yr,n=4);yt<-yt[yt>=yr[1]&yt<=yr[2]];for(v in xt){line(c(xx(v),xx(v)),c(bottom,bottom-1));text(format(v,digits=6,trim=TRUE),xx(v),bottom-4,axis_text_size)};for(v in yt){line(c(left-1,left),c(yy(v),yy(v)));text(format(v,digits=6,trim=TRUE),left-3,yy(v),axis_text_size,just='right')};grid::grid.rect(grid::unit(left+pw/2,'mm'),grid::unit(bottom+ph/2,'mm'),grid::unit(pw,'mm'),grid::unit(ph,'mm'),gp=grid::gpar(fill=NA,col='#8A9197',lwd=.5));text(x_col,left+pw/2,bottom-10,8.5,'bold');text(y_col,left-12,bottom+ph/2,8.5,'bold',rot=90)
if(label_mode=='centroid')for(i in seq_len(nrow(centroids)))line(c(centroids$x_mm[i],centroids$leader_x_mm[i]),c(centroids$y_mm[i],centroids$leader_y_mm[i]),'#AAB4BE',.4);for(i in seq_len(nrow(cells)))circle(cells$x_mm[i],cells$y_mm[i],point_size/2,grDevices::adjustcolor(cells$color[i],alpha.f=point_alpha));if(label_mode=='centroid')for(i in seq_len(nrow(centroids)))text(centroids$group[i],centroids$label_x_mm[i],centroids$label_y_mm[i],label_pt,col=label_color);for(i in seq_along(caption))text(caption[i],6,8-(i-1)*3,6,just='left',col='#52606D')},finally=grDevices::dev.off())};for(kind in c('png','pdf','svg'))render(kind)
utils::write.csv(cells,file.path(output_dir,'umap_cells.csv'),row.names=FALSE);utils::write.csv(centroids,file.path(output_dir,'cluster_centroids.csv'),row.names=FALSE,na='NA');saveRDS(list(raw=raw,cells=cells,centroids=centroids),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(input=data_path,n_cells=nrow(cells),n_groups=length(levels),cell_column=cell_col,x=x_col,y=y_col,group=group_col,groups=levels,group_colors=as.list(palette),group_counts=as.list(table(factor(groups,levels=levels))),point_diameter_mm=point_size,point_alpha=point_alpha,label_mode=label_mode,show_legend=show_legend,legend_position=legend_position,geometry=list(width_mm=width_mm,height_mm=height_mm,x_mm=left,y_mm=bottom,width_panel_mm=pw,height_panel_mm=ph,xrange=xr,yrange=yr,mm_per_coordinate_unit=unit_scale),caption=caption),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15,na='null');cat('Rendered all ',nrow(cells),' precomputed cells with equal scales and truthful centroid leaders\n')
