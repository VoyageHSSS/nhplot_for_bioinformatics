#!/usr/bin/env Rscript

# Correlation chords: preserve supplied values and report matrix consistency.
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
required_packages <- c("jsonlite", "svglite", "ragg", "circlize", "readxl")
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






links_path<-resolve_input(text_param('data_path','',TRUE),file.path(script_dir,'assets/easy_input.txt'));raw<-read_table(links_path);needed<-c('Gene_1','Gene_2','Correlation');if(anyDuplicated(names(raw))||!all(needed%in%names(raw))||!nrow(raw))stop('Input requires unique columns Gene_1/Gene_2/Correlation and at least one edge',call.=FALSE)
links<-raw[,needed,drop=FALSE];links$Gene_1<-trimws(as.character(links$Gene_1));links$Gene_2<-trimws(as.character(links$Gene_2));if(anyNA(links[,1:2])||any(!nzchar(unlist(links[,1:2])))||any(grepl('[[:cntrl:]]',unlist(links[,1:2]))))stop('Gene names must be complete single-line text',call.=FALSE)
links$Correlation<-parse_numeric(links$Correlation,'Correlation');if(any(!is.finite(links$Correlation))||any(abs(links$Correlation)>1))stop('Correlation must be finite within [-1,1]',call.=FALSE);if(any(links$Gene_1==links$Gene_2))stop('Self-links are not supported',call.=FALSE)
pair_key<-paste(pmin(links$Gene_1,links$Gene_2),pmax(links$Gene_1,links$Gene_2),sep='\r');if(anyDuplicated(pair_key))stop('Duplicate undirected gene pairs are not supported',call.=FALSE)
genes<-unique(c(links$Gene_1,links$Gene_2));gene_list_value<-text_param('gene_list_path','',TRUE);gene_list_path<-if(nzchar(gene_list_value))resolve_input(gene_list_value,'')else if(identical(links_path,normalizePath(file.path(script_dir,'assets/easy_input.txt'))))file.path(script_dir,'assets/GeneList.txt')else''
if(nzchar(gene_list_path)){gene_list<-trimws(readLines(gene_list_path,warn=FALSE));if(any(!nzchar(gene_list))||anyDuplicated(gene_list)||!setequal(gene_list,genes))stop('Gene list must contain every input gene exactly once',call.=FALSE);genes<-gene_list}
weight <- abs(links$Correlation)
totals <- setNames(numeric(length(genes)), genes)
for (i in seq_len(nrow(links))) {
  totals[[links$Gene_1[[i]]]] <- totals[[links$Gene_1[[i]]]] + weight[[i]]
  totals[[links$Gene_2[[i]]]] <- totals[[links$Gene_2[[i]]]] + weight[[i]]
}
if (any(totals <= 0)) stop("Each gene must have at least one non-zero correlation", call. = FALSE)
cursor <- setNames(numeric(length(genes)), genes)
start_1 <- end_1 <- start_2 <- end_2 <- numeric(nrow(links))
for (i in seq_len(nrow(links))) {
  g1 <- links$Gene_1[[i]]; g2 <- links$Gene_2[[i]]; w <- weight[[i]]
  start_1[[i]] <- cursor[[g1]]; end_1[[i]] <- cursor[[g1]] + w; cursor[[g1]] <- end_1[[i]]
  start_2[[i]] <- cursor[[g2]]; end_2[[i]] <- cursor[[g2]] + w; cursor[[g2]] <- end_2[[i]]
}


font_family<-choice('font_family',text_param('family','Arial'),c('Arial','Helvetica','sans','Times New Roman'));font_family<-if(font_family%in%c('Arial','Helvetica'))'sans'else if(font_family=='Times New Roman')'serif'else font_family
base_size<-num('base_size',9,7,18);label_size<-num('label_size',1,.35,1.5);title_size<-num('title_size',1.167,.6,2);title<-text_param('title','Gene-gene correlation',TRUE);if(any(grepl('[[:cntrl:]]|Case [0-9]|cover_|debug',c(title,genes),ignore.case=TRUE)))stop('Remove internal/control identifiers from labels',call.=FALSE)
start_degree<-num('start_degree',90,0,360);gap_degree<-num('gap_degree',5,.5,20);if(length(genes)*gap_degree>=180)stop('Total gaps must be below 180 degrees',call.=FALSE)
track_height<-num('track_height',.06,.04,.18);link_alpha<-num('link_alpha',.72,.1,1);if(!is.null(gp('link_min_width')))num('link_min_width',.6,.1,3) # Legacy border-only parameter: ribbons have no border; never inflate r=0.
width_mm<-num('width_mm',180,120,300);height_mm<-num('height_mm',180,120,300);dpi<-integer_param('dpi',600,96,1200)
link_colors<-grDevices::colorRampPalette(c('#2166AC','#F7F7F7','#B2182B'))(201);link_index<-round((links$Correlation+1)*100)+1L;links$color<-link_colors[link_index];links$abs_correlation<-weight;links$start_1<-start_1;links$end_1<-end_1;links$start_2<-start_2;links$end_2<-end_2
sector_colors<-setNames(grDevices::hcl.colors(length(genes),palette='Dynamic'),genes)
complete_pairs<-nrow(links)==choose(length(genes),2);cm<-diag(length(genes));dimnames(cm)<-list(genes,genes);for(i in seq_len(nrow(links))){cm[links$Gene_1[i],links$Gene_2[i]]<-links$Correlation[i];cm[links$Gene_2[i],links$Gene_1[i]]<-links$Correlation[i]};eigenvalues<-if(complete_pairs)eigen(cm,symmetric=TRUE,only.values=TRUE)$values else NULL;matrix_status<-if(!complete_pairs)'Incomplete edge list; matrix consistency not assessed'else if(min(eigenvalues)< -1e-8)'Non-PSD matrix: verify coefficient provenance'else'PSD matrix; measurement provenance still required'
caption<-c('Ribbon width = |r|; sector length = sum of incident |r|. Zero-width edges are exported, not drawn.',matrix_status)
sector_records<-NULL;label_records<-NULL;device_geometry<-NULL
render<-function(){
 graphics::par(mar=rep(0,4),family=font_family,ps=base_size,xpd=FALSE,xaxs='i',yaxs='i')
 circlize::circos.clear();on.exit(circlize::circos.clear(),add=TRUE)
 circlize::circos.par(start.degree=start_degree,gap.degree=gap_degree,track.margin=c(0,0),cell.padding=c(0,0,0,0),canvas.xlim=c(-1.22,1.22),canvas.ylim=c(-1.52,1.3),points.overflow.warning=TRUE)
 circlize::circos.initialize(genes,xlim=cbind(0,unname(totals[genes])))
 ss<-list();ll<-list()
 circlize::circos.trackPlotRegion(ylim=c(0,1),track.height=track_height,bg.border=NA,panel.fun=function(x,y){g<-circlize::CELL_META$sector.index;m<-circlize::CELL_META;circlize::circos.rect(m$xlim[1],0,m$xlim[2],1,col=sector_colors[[g]],border=NA);theta<-(m$cell.start.degree+m$cell.end.degree)/2;rad<-1.075;xx<-rad*cos(theta*pi/180);yy<-rad*sin(theta*pi/180);angle<-theta%%180-90
  # Single complete text label, tangent to the sector; no per-letter bending.
  graphics::text(xx,yy,g,srt=angle,cex=label_size,font=2,col='#1F2933')
  ll[[length(ll)+1]]<<-data.frame(gene=g,x=xx,y=yy,angle=angle)
  ss[[length(ss)+1]]<<-data.frame(gene=g,total=totals[[g]],start_degree=m$cell.start.degree,end_degree=m$cell.end.degree,outer=m$cell.top.radius,inner=m$cell.bottom.radius,color=sector_colors[[g]])
 })
 sector_records<<-do.call(rbind,ss);label_records<<-do.call(rbind,ll);rou<-1-track_height
 for(i in seq_len(nrow(links)))if(weight[i]>0)circlize::circos.link(links$Gene_1[i],c(start_1[i],end_1[i]),links$Gene_2[i],c(start_2[i],end_2[i]),col=grDevices::adjustcolor(links$color[i],alpha.f=link_alpha),border=NA,rou=rou)
 graphics::text(0,1.23,title,cex=title_size,font=2,col='#1F2933')
 x0<--.45;x1<-.45;y0<--1.29;y1<--1.25;xs<-seq(x0,x1,length.out=202);graphics::rect(xs[-202],y0,xs[-1],y1,col=grDevices::adjustcolor(link_colors,alpha.f=link_alpha),border=NA)
 graphics::text(c(x0,0,x1),-1.335,c('-1','0','+1'),cex=.78,col='#334E68');graphics::text(0,-1.18,'Correlation',cex=.88,font=2,col='#1F2933')
 graphics::text(0,c(-1.42,-1.48),caption,cex=.66,col=if(matrix_status=='Non-PSD matrix: verify coefficient provenance')c('#334E68','#9C2F2F')else'#334E68')
 usr<-graphics::par('usr');pin<-graphics::par('pin');device_geometry<<-list(usr=usr,pin=pin,canvas_mm=c(width_mm,height_mm),circle_diameter_mm=2/diff(usr[1:2])*pin[1]*25.4)
 # Real font extents after a device is open; enforce all rotated label bounds and inter-label gaps.
 boxes<-list();labels<-c(genes,title,'Correlation','-1','0','+1',caption);cx<-c(label_records$x,0,0,x0,0,x1,0,0);cy<-c(label_records$y,1.23,-1.18,-1.335,-1.335,-1.335,-1.42,-1.48);ang<-c(label_records$angle,rep(0,7));cex<-c(rep(label_size,length(genes)),title_size,.88,rep(.78,3),rep(.66,2));font<-c(rep(2,length(genes)),2,2,rep(1,5))
 for(i in seq_along(labels)){w<-graphics::strwidth(labels[i],cex=cex[i],font=font[i]);h<-graphics::strheight('Mg',cex=cex[i],font=font[i])*1.35;th<-ang[i]*pi/180;corners<-cbind(c(-1,1,1,-1)*w/2,c(-1,-1,1,1)*h/2)%*%matrix(c(cos(th),-sin(th),sin(th),cos(th)),2);corners<-sweep(corners,2,c(cx[i],cy[i]),'+');if(any(corners[,1]<usr[1]|corners[,1]>usr[2]|corners[,2]<usr[3]|corners[,2]>usr[4]))stop('Text exceeds canvas; reduce label/title size or increase dimensions',call.=FALSE);boxes[[i]]<-c(min(corners[,1]),max(corners[,1]),min(corners[,2]),max(corners[,2]))}
 for(i in seq_along(boxes))if(i>1)for(j in seq_len(i-1)){a<-boxes[[i]];b<-boxes[[j]];if(a[1]<b[2]&&a[2]>b[1]&&a[3]<b[4]&&a[4]>b[3])stop('Text labels overlap: ',labels[i],' / ',labels[j],'; reduce labels or increase dimensions',call.=FALSE)}
}
save_plot<-function(kind){path<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(path,width=width_mm,height=height_mm,units='mm',res=dpi,background='white',pointsize=base_size);if(kind=='pdf')grDevices::pdf(path,width=width_mm/25.4,height=height_mm/25.4,pointsize=base_size,useDingbats=FALSE,bg='white');if(kind=='svg')svglite::svglite(path,width=width_mm/25.4,height=height_mm/25.4,pointsize=base_size,bg='white');on.exit(grDevices::dev.off());render()}
for(kind in c('png','pdf','svg'))save_plot(kind)
utils::write.csv(links,file.path(output_dir,'link_summary.csv'),row.names=FALSE);utils::write.csv(sector_records,file.path(output_dir,'sector_summary.csv'),row.names=FALSE);utils::write.csv(label_records,file.path(output_dir,'label_positions.csv'),row.names=FALSE);utils::write.csv(raw,file.path(output_dir,'source-records.csv'),row.names=FALSE);if(complete_pairs)utils::write.csv(cm,file.path(output_dir,'correlation_matrix.csv'),row.names=TRUE)
metadata<-list(input=links_path,gene_list=gene_list_path,genes=genes,n_genes=length(genes),n_links=nrow(links),drawn_links=sum(weight>0),zero_links=sum(weight==0),positive_links=sum(links$Correlation>0),negative_links=sum(links$Correlation<0),correlation_range=range(links$Correlation),complete_pairs=complete_pairs,eigenvalues=eigenvalues,matrix_status=matrix_status,caption=caption,title=title,base_size=base_size,label_size=label_size,title_size=title_size,link_alpha=link_alpha,track_height=track_height,gap_degree=gap_degree,start_degree=start_degree,font_family=font_family,device_geometry=device_geometry)
jsonlite::write_json(metadata,file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,null='null',digits=16);saveRDS(list(raw=raw,links=links,genes=genes,totals=totals,matrix=if(complete_pairs)cm else NULL,metadata=metadata),file.path(output_dir,'parsed-analysis.RDS'));message('Rendered ',length(genes),' genes / ',nrow(links),' supplied edges; ',matrix_status)
