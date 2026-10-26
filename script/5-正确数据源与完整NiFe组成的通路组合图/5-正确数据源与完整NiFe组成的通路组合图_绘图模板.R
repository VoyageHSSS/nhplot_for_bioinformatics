#!/usr/bin/env Rscript

# Actual five-sheet marine pathway data with complete profile clustering and keyed NiFe bars.
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
required_packages <- c("jsonlite", "svglite", "ragg", "readxl", "showtext", "sysfonts", "systemfonts", "RColorBrewer", "ggtree")
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
    as.data.frame(readxl::read_excel(path, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}
















ids<-function(v,label){v<-as.character(v);if(anyNA(v)||any(!nzchar(v))||any(v!=trimws(v))||any(grepl('[[:cntrl:]]',v)))stop(label,' require complete exact labels',call.=FALSE);v}
data_path<-resolve_input(text_param('data_path','',TRUE),'assets/41564_2023_1322_MOESM5_ESM.xlsx');if(!tolower(tools::file_ext(data_path))%in%c('xls','xlsx'))stop('Five-sheet source XLS/XLSX workbook required',call.=FALSE)
sheets<-lapply(1:5,function(i)as.data.frame(readxl::read_excel(data_path,sheet=i,.name_repair='minimal',trim_ws=FALSE),check.names=FALSE))
need<-list(c('Sample ID','Metabolic pathway','Average copies per organism','Data source','Sample Name','Capped copies per organism'),c('Sample Name','Metabolic pathway','Total RPKM','Data source'),c('Sample ID','Subgroup','Average copies per organism','Display group','Sample name'),c('Subgroup','Location','Depth','Average copies per organism','Sample ID','Display group'),c('Subgroup','Location','Depth','Average RPKM','Sample ID','Display group'))
for(i in 1:5)if(anyDuplicated(names(sheets[[i]]))||!setequal(names(sheets[[i]]),need[[i]]))stop('Source sheet ',i,' requires exact distinct complete fields',call.=FALSE)
source_levels<-c('This study (metagenomes)','Tara Oceans (metagenomes)','Tara Oceans (metatranscriptomes)');G<-sheets[[1]];T<-sheets[[2]]
for(d in list(G,T)){ids(d[['Sample Name']],'Samples');ids(d[['Metabolic pathway']],'Pathways');ids(d[['Data source']],'Data sources');if(anyDuplicated(d[c('Sample Name','Metabolic pathway')]))stop('Duplicate actual sample/pathway pairs prohibited',call.=FALSE)}
if(!setequal(unique(G[['Data source']]),source_levels[1:2])||!identical(unique(T[['Data source']]),source_levels[3]))stop('Exact source labels required; no source filtering',call.=FALSE)
if(any(vapply(split(G[['Data source']],G[['Sample Name']]),function(z)length(unique(z))!=1,logical(1))))stop('Sample cannot map to multiple sources',call.=FALSE)
ids(G[['Sample ID']],'Source sample IDs');if(any(vapply(split(G[['Sample ID']],G[['Sample Name']]),function(z)length(unique(z))!=1,logical(1))))stop('Each sample name requires one exact sample ID',call.=FALSE)
paths<-unique(G[['Metabolic pathway']]);if(!setequal(paths,unique(T[['Metabolic pathway']]))||length(paths)<2||length(paths)>60)stop('All pathways must match both heatmaps',call.=FALSE)
gv<-parse_numeric(G[['Capped copies per organism']],'Capped copies');gu<-parse_numeric(G[['Average copies per organism']],'Average copies');tv<-parse_numeric(T[['Total RPKM']],'RPKM')
if(any(!is.finite(c(gv,gu,tv)))||any(gv<0|gv>1)||any(c(gu,tv)<0)||any(abs(gv-pmin(1,gu))>1e-10))stop('Finite nonnegative actual values and exact pmin(1, copies) source cap required',call.=FALSE)
make_matrix<-function(d,values){samples<-unique(d[['Sample Name']]);if(nrow(d)!=length(paths)*length(samples))stop('Complete source sample/pathway Cartesian matrix required; no NA zero fill',call.=FALSE);M<-matrix(NA_real_,length(paths),length(samples),dimnames=list(paths,samples));M[cbind(match(d[['Metabolic pathway']],paths),match(d[['Sample Name']],samples))]<-values;if(anyNA(M))stop('Missing actual matrix pairs prohibited',call.=FALSE);M}
MG<-make_matrix(G,gv);MT<-make_matrix(T,tv)
groups<-c('Group 1d','Group 1e','Group 1l','Group 2a','Group 2b','Group 3b','Group 3d','Other NiFe');bar_colors<-setNames(c('#E64B35','#4DBBD5','#00A087','#3C5488','#F39B7F','#8491B4','#91D1C2','#BFC3C5'),groups)
bars_raw<-list()
for(i in 3:5){d<-sheets[[i]];sn<-ids(d[[if(i==3)'Sample name'else 'Sample ID']],'NiFe samples');subgroup<-ids(d$Subgroup,'NiFe subgroup');gr<-ids(d[['Display group']],'NiFe display group');v<-parse_numeric(d[[if(i==5)'Average RPKM'else 'Average copies per organism']],'NiFe value');if(any(!is.finite(v))||any(v<0)||any(!gr%in%groups)||anyDuplicated(data.frame(sn,subgroup)))stop('NiFe values/groups/sample-subgroup pairs must be finite nonnegative exact and distinct',call.=FALSE);if(any(vapply(split(gr,subgroup),function(z)length(unique(z))!=1,logical(1))))stop('Each subgroup must map to one display group',call.=FALSE);bars_raw[[i-2]]<-data.frame(sample=sn,subgroup=subgroup,group=gr,value=v,source=source_levels[i-2])}
local<-sheets[[3]];ids(local[['Sample ID']],'Local source IDs');local_ids<-G[['Sample ID']][match(local[['Sample name']],G[['Sample Name']])];if(anyNA(local_ids)||any(local[['Sample ID']]!=local_ids))stop('Local subgroup sample names must match their exact source Sample ID',call.=FALSE)
bars_raw<-do.call(rbind,bars_raw);bars<-aggregate(value~sample+group+source,bars_raw,sum)
if(!'[NiFe] hydrogenases'%in%paths)stop('Source NiFe total pathway required to validate subgroup sums',call.=FALSE)
for(face in source_levels){h<-if(face==source_levels[3])T else G;hv<-if(face==source_levels[3])tv else gu;expected<-unique(h[['Sample Name']][h[['Data source']]==face]);actual<-unique(bars$sample[bars$source==face]);if(face==source_levels[1])expected<-setdiff(expected,'Negative control');if(!setequal(actual,expected))stop('All actual NiFe samples must match their TRUE heatmap source; only explicit negative control may lack bars',call.=FALSE);b<-bars[bars$source==face,];if(nrow(b)!=length(actual)*length(groups))stop('All eight NiFe groups required per sample',call.=FALSE);totals<-tapply(b$value,b$sample,sum);idx<-match(names(totals),h[['Sample Name']][h[['Metabolic pathway']]=='[NiFe] hydrogenases']);ht<-hv[h[['Metabolic pathway']]=='[NiFe] hydrogenases'];if(any(totals<=0)||any(abs(totals-ht[idx])>1e-9*pmax(1,ht[idx])))stop('NiFe group totals must equal source total hydrogenase abundance and be positive',call.=FALSE)}
control<-choice('negative_control','include',c('include','exclude'));if(control=='exclude'&&'Negative control'%in%colnames(MG))MG<-MG[,colnames(MG)!='Negative control',drop=FALSE]
sample_order<-choice('sample_order','source_lexical',c('source_lexical','input'));width_mm<-num('width_mm',320,315,360);height_mm<-num('height_mm',236,230,260);dpi<-integer_param('dpi',600,96,1200);font_size<-num('font_size',8,7.5,8.5);sample_font_size<-num('sample_font_size',5.4,5,5.8);tree_width<-num('tree_width',.35,.2,.45);bar_fraction<-num('bar_fraction',.8,.7,.9);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
hc<-hclust(dist(MG),method='complete');if(max(hc$height)==0)stop('Constant profiles cannot support clustering',call.=FALSE)
td<-as.data.frame(suppressWarnings(ggtree::ggtree(hc,layout='rectangular',branch.length='none')$data));tips<-td[td$isTip,];ord<-tips$label[order(tips$y,decreasing=TRUE)]
scale<-width_mm/320;lefts<-c(90,132,218)*scale;widths<-c(40,84,60)*scale;top<-height_mm-10;bottom<-82;dy<-(top-bottom)/length(paths);if(dy<font_size*25.4/72*1.2)stop('Rows require taller permitted canvas or smaller font',call.=FALSE)
sample_lists<-lapply(source_levels,function(face){d<-if(face==source_levels[3])T else G;v<-unique(d[['Sample Name']][d[['Data source']]==face]);if(face!=source_levels[3])v<-v[v%in%colnames(MG)];if(sample_order=='source_lexical')sort(v)else v})
if(any(vapply(sample_lists,length,integer(1))<1))stop('Each source must contain actual samples',call.=FALSE)
cells<-list();for(k in 1:3){face<-source_levels[k];sm<-sample_lists[[k]];dx<-widths[k]/length(sm);d<-expand.grid(pathway=ord,sample=sm,stringsAsFactors=FALSE);M<-if(k==3)MT else MG;d$value<-M[cbind(match(d$pathway,rownames(M)),match(d$sample,colnames(M)))];d$display_value<-if(k==3)log10(d$value+1)else d$value;d$source<-face;d$x_mm<-lefts[k]+(match(d$sample,sm)-.5)*dx;d$y_mm<-top-(match(d$pathway,ord)-.5)*dy;d$width_mm<-dx;d$height_mm<-dy;cells[[k]]<-d};cells<-do.call(rbind,cells)
pal<-rev(RColorBrewer::brewer.pal(6,'RdBu'));ramp<-colorRamp(pal,space='Lab');trange<-range(log10(MT+1));if(diff(trange)==0)stop('Nondegenerate transcriptome scale required',call.=FALSE)
normalized<-ifelse(cells$source==source_levels[3],(cells$display_value-trange[1])/diff(trange),cells$display_value);co<-ramp(normalized);cells$fill<-rgb(co[,1],co[,2],co[,3],maxColorValue=255)
bar_bottom<-48;bar_height<-27;bar_segments<-list();ii<-0L
for(k in 1:3){sm<-sample_lists[[k]];dx<-widths[k]/length(sm);if(dx*sin(65*pi/180)<sample_font_size*25.4/72*1.21)stop('Complete diagonal sample labels require wider permitted canvas or smaller font',call.=FALSE);for(j in seq_along(sm)){z<-bars[bars$source==source_levels[k]&bars$sample==sm[j],];if(!nrow(z)){if(sm[j]!='Negative control')stop('Missing NiFe bar prohibited',call.=FALSE);next};z<-z[match(groups,z$group),];z$prop<-z$value/sum(z$value);z$lower<-c(0,head(cumsum(z$prop),-1));z$upper<-cumsum(z$prop);if(abs(tail(z$upper,1)-1)>1e-12)stop('Every full NiFe bar must reach 100%',call.=FALSE);z$x_mm<-lefts[k]+(j-.5)*dx;z$y_mm<-bar_bottom+(z$lower+z$upper)/2*bar_height;z$width_mm<-dx*bar_fraction;z$height_mm<-z$prop*bar_height;z$fill<-bar_colors[z$group];ii<-ii+1L;bar_segments[[ii]]<-z}}
bar_segments<-do.call(rbind,bar_segments);td$x_mm<-5*scale+td$x/max(td$x)*10*scale;td$y_mm<-bottom+(td$y-.5)*dy;segments<-list();ii<-0L
for(i in seq_len(nrow(td))){p<-match(td$parent[i],td$node);if(p!=i){ii<-ii+1L;segments[[ii]]<-data.frame(x0=td$x_mm[p],y0=td$y_mm[i],x1=td$x_mm[i],y1=td$y_mm[i])}}
for(i in which(!td$isTip)){kids<-which(td$parent==td$node[i]&td$node!=td$node[i]);if(length(kids)){ii<-ii+1L;segments[[ii]]<-data.frame(x0=td$x_mm[i],y0=min(td$y_mm[kids]),x1=td$x_mm[i],y1=max(td$y_mm[kids]))}}
segments<-do.call(rbind,segments);captions<-c(sprintf('%d pathways; %d metagenome and %d transcriptome profiles; complete Euclidean clustering, not phylogeny.',length(paths),ncol(MG),ncol(MT)),sprintf('Actual NiFe subgroup sums / source total; %d full 100%% bars. Negative control: %s, no NiFe bar supplied.',length(unique(paste(bar_segments$source,bar_segments$sample))),control))
sysfonts::font_add(family,regular=systemfonts::match_fonts(family)$path,bold=systemfonts::match_fonts(family,weight='bold')$path,italic=systemfonts::match_fonts(family,italic=TRUE)$path,bolditalic=systemfonts::match_fonts(family,weight='bold',italic=TRUE)$path)
probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()
tryCatch({tw<-function(z,size)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=size))),'mm',TRUE);if(max(vapply(paths,tw,numeric(1),size=font_size))>lefts[1]-18*scale||max(vapply(unlist(sample_lists),tw,numeric(1),size=sample_font_size))*sin(65*pi/180)>29||any(vapply(captions,tw,numeric(1),size=6)>width_mm-10))stop('Complete path/sample labels require larger canvas or smaller font',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(){grid::grid.newpage();mm<-function(z)grid::unit(z,'mm');txt<-function(z,x,y,size=font_size,face='plain',just='center',rot=0,col='#111111')grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face,col=col));rect<-function(d)grid::grid.rect(mm(d$x_mm),mm(d$y_mm),mm(d$width_mm),mm(d$height_mm),gp=grid::gpar(fill=d$fill,col=NA));line<-function(x,y,w=.15,col='#555555')grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=col,lwd=w*72.27/25.4));rect(cells);for(i in seq_len(nrow(segments)))line(c(segments$x0[i],segments$x1[i]),c(segments$y0[i],segments$y1[i]),tree_width,'#111111');for(i in seq_along(ord))txt(ord[i],lefts[1]-2,top-(i-.5)*dy,just='right');title<-c('This study','Tara Oceans','Tara Oceans');subtitle<-c('Metagenomes','Metagenomes','Metatranscriptomes');for(k in 1:3){x<-lefts[k]+widths[k]/2;txt(title[k],x,top+6,9,face='bold');txt(sprintf('%s: %d profiles',subtitle[k],length(sample_lists[[k]])),x,top+2.5,7.5);line(c(lefts[k],lefts[k]+widths[k]),c(bottom,bottom));line(rep(lefts[k],2),c(bottom,top));line(rep(lefts[k]+widths[k],2),c(bottom,top));for(v in c(0,.25,.5,.75,1))line(c(lefts[k],lefts[k]+widths[k]),rep(bar_bottom+v*bar_height,2),.1,'#DDDDDD');line(c(lefts[k],lefts[k]+widths[k]),rep(bar_bottom,2));for(j in seq_along(sample_lists[[k]])){sm<-sample_lists[[k]][j];x<-lefts[k]+(j-.5)*widths[k]/length(sample_lists[[k]]);txt(sm,x,bar_bottom-1.5,sample_font_size,just='right',rot=65,col=if(sm=='Negative control')'#A1212A'else '#111111');if(sm=='Negative control')txt('NC',x,bar_bottom+bar_height/2,6,col='#A1212A')}};rect(bar_segments);for(v in c(0,.25,.5,.75,1))txt(paste0(v*100,'%'),lefts[1]-2,bar_bottom+v*bar_height,6.5,just='right');txt('NiFe subgroup proportion',lefts[1]-11,bar_bottom+bar_height/2,7,rot=90);txt('a',3,top+5,11,face='bold');txt('b',3,bar_bottom+bar_height,11,face='bold')
lx<-286*scale;for(k in 1:2){yy<-top-(k-1)*67;txt(if(k==1)'Copies / organism'else 'log10(RPKM + 1)',lx,yy,8,face='bold',just='left');bh<-46;bb<-yy-52;for(i in 1:101){v<-(i-.5)/101;co<-ramp(v);rect(data.frame(x_mm=lx+2,y_mm=bb+v*bh,width_mm=4,height_mm=bh/101+.01,fill=rgb(co[1],co[2],co[3],maxColorValue=255)))};ran<-if(k==1)c(0,1)else trange;vals<-if(k==1)c(0,.25,.5,.75,1)else pretty(ran,4);for(v in vals[vals>=ran[1]&vals<=ran[2]])txt(if(k==1&&v==1)'1 (capped)'else format(v,trim=TRUE),lx+6,bb+(v-ran[1])/diff(ran)*bh,7,just='left')}
txt('NiFe groups',5,12,7.5,face='bold',just='left');for(i in seq_along(groups)){x<-36+(i-1)*34;rect(data.frame(x_mm=x,y_mm=12,width_mm=3,height_mm=3,fill=bar_colors[i]));txt(groups[i],x+3,12,6.5,just='left')};for(i in seq_along(captions))txt(captions[i],5,6-(i-1)*3,6,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
write.csv(cells,file.path(output_dir,'actual_heatmap_cells.csv'),row.names=FALSE);write.csv(bar_segments,file.path(output_dir,'actual_bar_segments.csv'),row.names=FALSE);write.csv(segments,file.path(output_dir,'actual_tree_segments.csv'),row.names=FALSE);saveRDS(list(sheets=sheets,MG=MG,MT=MT,hc=hc,td=td,order=ord,samples=sample_lists,cells=cells,bars_raw=bars_raw,bars=bars,bar_segments=bar_segments,segments=segments),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(data_path=data_path,source_levels=source_levels,paths=paths,order=ord,samples=sample_lists,groups=groups,bar_colors=as.list(bar_colors),negative_control=control,sample_order=sample_order,method='complete',heatmap_cells=nrow(cells),bar_segments=nrow(bar_segments),trange=trange,palette=pal,width_mm=width_mm,height_mm=height_mm,font_size=font_size,sample_font_size=sample_font_size,tree_width=tree_width,bar_fraction=bar_fraction,geometry=list(lefts=lefts,widths=widths,top=top,bottom=bottom,dy=dy,bar_bottom=bar_bottom,bar_height=bar_height),caption=captions,scope='Actual averaged source profiles, not original short-read sample counts; native source complete profile tree, not paper phylogeny or statistical inference; NiFe fractions are within-hydrogenase composition'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered actual aligned pathway heatmaps and full keyed NiFe composition bars\n')
