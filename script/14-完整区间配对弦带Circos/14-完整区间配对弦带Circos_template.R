#!/usr/bin/env Rscript

# Tutorial genomic loci: explicit hg38 background, exact endpoints and unverified provenance.
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
required_packages <- c("jsonlite", "svglite", "ragg", "readxl", "showtext", "sysfonts", "systemfonts", "circlize")
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
  if(ext=="xls"){magic<-readBin(path,"raw",n=8);binary<-identical(magic,as.raw(c(208,207,17,224,161,177,26,225)))||identical(head(magic,2),charToRaw("PK"));if(identical(head(magic,2),charToRaw("PK")))ext<-"xlsx"else if(!binary)ext<-"txt"}
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt", "md", "maf")) { seps<-c("\t",";",",");counts<-vapply(seps,function(sep){v<-tryCatch(utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE),error=function(e)0L);max(c(0L,v),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; fields<-utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE);fields<-fields[!is.na(fields)];if(!length(fields)||any(fields!=fields[1]))stop("Every table record must match the header field count",call.=FALSE); if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for XLS/XLSX input", call. = FALSE)
    as.data.frame((if(ext=="xlsx")readxl::read_xlsx else readxl::read_xls)(path, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}

















chroms<-c(paste0('chr',1:22),'chrX','chrY');cyto<-read.delim(file.path(script_dir,'assets/cytoband-hg19.tsv'),header=FALSE,stringsAsFactors=FALSE);lengths<-tapply(cyto[[3]],cyto[[1]],max)
p1<-text_param('data_path','',TRUE);p2<-text_param('bed2_path','',TRUE);if(xor(nzchar(p1),nzchar(p2)))stop('Both BED files must be supplied together; no one-sided replacement',call.=FALSE)
data_path<-resolve_input(p1,'assets/bed1.txt');bed2_path<-resolve_input(p2,'assets/bed2.txt');fields<-vapply(seq_len(3),function(i)text_param(c('chr_field','start_field','end_field')[i],c('chr','start','end')[i]),character(1));if(anyDuplicated(fields))stop('Distinct BED fields required',call.=FALSE)
read_bed<-function(path){raw<-read_table(path);if(anyDuplicated(names(raw))||!all(fields%in%names(raw)))stop('Unique headers with all BED fields required',call.=FALSE);cr<-as.character(raw[[fields[1]]]);st<-parse_numeric(raw[[fields[2]]],'start');en<-parse_numeric(raw[[fields[3]]],'end');if(!nrow(raw)||nrow(raw)>200||anyNA(cr)||any(!cr%in%chroms)||any(!is.finite(c(st,en)))||any(st!=floor(st)|en!=floor(en)|st<0|en<=st)||any(en>lengths[cr]))stop('1..200 complete canonical human intervals within hg19 bounds required; no row dropping',call.=FALSE);data.frame(chr=cr,start=st,end=en,stringsAsFactors=FALSE)}
bed1<-read_bed(data_path);bed2<-read_bed(bed2_path);if(nrow(bed1)!=nrow(bed2))stop('BED files must have equal row counts, paired by exact row order',call.=FALSE);n<-nrow(bed1)
width_mm<-num('width_mm',180,170,230);height_mm<-num('height_mm',190,180,240);dpi<-integer_param('dpi',600,96,1200);font_size<-num('font_size',9,8,10);tick_font_size<-num('tick_font_size',5.5,5,6);axis_width<-num('axis_width',.28,.2,.45);band_height<-num('band_height',2.5,2,3.5);link_alpha<-num('link_alpha',.8,.5,1);gap_degree<-num('gap_degree',1,.5,2);major_mb<-choice('major_mb','100',c('100','150'));color_mode<-choice('color_mode','source',c('source','muted'));show_axis<-bool('show_axis',TRUE);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
colors<-if(color_mode=='source')grDevices::rainbow(n)else grDevices::hcl.colors(n,'Dark 3');colors<-grDevices::adjustcolor(colors,alpha.f=link_alpha)
font_file<-function(weight='normal',italic=FALSE){h<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(h$index!=0L)h<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);h$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces))
caption<-c(sprintf('%d supplied interval pairs; colours identify rows, not categories; coordinates in Mb.',n),'hg19 background; interval build / coordinate convention / biological interaction unverified.')
render<-function(kind){par(fig=c(0,1,.065,1),mar=c(0,0,0,0),family=family,ps=12,cex=1,xpd=NA);circlize::circos.clear();on.exit(circlize::circos.clear(),add=TRUE);circlize::circos.par(start.degree=0,gap.after=rep(gap_degree,24),cell.padding=c(0,0,0,0),track.margin=c(.005,.005),canvas.xlim=c(-1.02,1.02),canvas.ylim=c(-1.02,1.02),points.overflow.warning=TRUE)
circlize::circos.initializeWithIdeogram(cytoband=cyto,plotType=NULL,chromosome.index=chroms,sort.chr=FALSE)
circlize::circos.trackPlotRegion(ylim=c(0,1),track.height=circlize::mm_h(7),bg.border=NA,panel.fun=function(x,y){cr<-circlize::CELL_META$sector.index;circlize::circos.text(mean(circlize::CELL_META$xlim),.7,sub('chr','',cr),cex=font_size/12,col='#111111',facing='outside',niceFacing=TRUE,font=1);if(show_axis){at<-seq(0,lengths[cr],by=as.numeric(major_mb)*1e6);labels<-ifelse(lengths[cr]-at<28e6,'',as.character(at/1e6));circlize::circos.axis(h='bottom',major.at=at,labels=labels,labels.cex=tick_font_size/12,labels.facing='outside',minor.ticks=4,major.tick.length=circlize::mm_y(.8),lwd=axis_width*72.27/25.4,col='#222222',labels.col='#222222',labels.pos.adjust=FALSE)}})
circlize::circos.genomicIdeogram(cytoband=cyto,track.height=circlize::mm_h(band_height));radius<-circlize::get_most_inside_radius();sector<-do.call(rbind,lapply(chroms,function(cr)data.frame(chr=cr,start_degree=circlize::get.cell.meta.data('cell.start.degree',sector.index=cr),end_degree=circlize::get.cell.meta.data('cell.end.degree',sector.index=cr),length=lengths[cr],row.names=NULL)))
anchors<-data.frame(row=seq_len(n),chr1=bed1$chr,start1=bed1$start,end1=bed1$end,chr2=bed2$chr,start2=bed2$start,end2=bed2$end,color=colors)
for(j in 1:2)for(ep in c('start','end')){cr<-anchors[[paste0('chr',j)]];pos<-anchors[[paste0(ep,j)]];ss<-sector[match(cr,sector$chr),];theta<-ss$start_degree-pos/ss$length*(ss$start_degree-ss$end_degree);anchors[[paste0(ep,j,'_angle')]]<-theta;anchors[[paste0(ep,j,'_x_mm')]]<-grconvertX(cos(theta*pi/180)*radius,'user','inches')*25.4;anchors[[paste0(ep,j,'_y_mm')]]<-grconvertY(sin(theta*pi/180)*radius,'user','inches')*25.4}
circlize::circos.genomicLink(bed1,bed2,col=colors,border=NA,directional=0,reduce_to_mid_line=FALSE)
if(kind=='svg'){write.csv(anchors,file.path(output_dir,'actual_links.csv'),row.names=FALSE);write.csv(sector,file.path(output_dir,'actual_sectors.csv'),row.names=FALSE);saveRDS(list(bed1=bed1,bed2=bed2,cyto=cyto,sector=sector,anchors=anchors,radius=radius),file.path(output_dir,'parsed-analysis.RDS'))}
circlize::circos.clear();par(fig=c(0,1,0,.065),new=TRUE,mar=c(0,0,0,0),xpd=NA);plot.new();plot.window(c(0,width_mm),c(0,height_mm*.065),xaxs='i',yaxs='i');text(rep(width_mm/2,2),c(7.5,3.5),caption,cex=6/12)}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(kind),finally=dev.off())}
jsonlite::write_json(list(data_path=data_path,bed2_path=bed2_path,fields=as.list(fields),pair_count=n,chromosomes=as.list(chroms),band_count=nrow(cyto),colors=as.list(colors),width_mm=width_mm,height_mm=height_mm,dpi=dpi,font_size=font_size,tick_font_size=tick_font_size,axis_width=axis_width,band_height=band_height,link_alpha=link_alpha,gap_degree=gap_degree,major_mb=major_mb,color_mode=color_mode,show_axis=show_axis,family=family,pdf_faces=pdf_faces,caption=as.list(caption),background_assembly='hg19',interval_assembly_verified=FALSE,pairing='exact row order',scope='All supplied interval widths preserved; non-directional links, no inferred event weights or biological interaction certification'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',n,'full interval ribbons, 24 sectors,',nrow(cyto),'cytobands\n')
