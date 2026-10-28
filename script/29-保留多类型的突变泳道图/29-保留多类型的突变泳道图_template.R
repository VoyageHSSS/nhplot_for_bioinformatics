#!/usr/bin/env Rscript

# Actual supplied mutation calls and q results; partition multiple types without inventing clinical labels.
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
required_packages <- c("jsonlite", "svglite", "ragg", "readxl", "showtext", "sysfonts", "systemfonts", "RColorBrewer")
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
















ids<-function(v,label){v<-as.character(v);if(anyNA(v)||any(!nzchar(v))||anyDuplicated(v)||any(v!=trimws(v))||any(grepl('[[:cntrl:]]',v)))stop(label,' require unique complete exact IDs',call.=FALSE);v}
maf_path<-resolve_input(text_param('maf_path','',TRUE),'assets/easy_input_meta.local mutation.full.txt');sig_path<-resolve_input(text_param('sig_genes_path','',TRUE),'assets/meta.local mutation.full_MutSig_output.sig_genes.txt');meta_path<-resolve_input(text_param('meta_samples_path','',TRUE),'assets/easy_input_meta.mut.txt');local_path<-resolve_input(text_param('local_samples_path','',TRUE),'assets/easy_input_local.mut.txt');maf<-read_table(maf_path);sig<-read_table(sig_path);if(anyDuplicated(names(maf))||!all(c('Hugo_Symbol','Tumor_Sample_Barcode','Variant_Classification')%in%names(maf))||anyDuplicated(names(sig))||!all(c('gene','q')%in%names(sig)))stop('Unique MAF Hugo_Symbol/Tumor_Sample_Barcode/Variant_Classification and sig gene/q headers required',call.=FALSE);for(k in c('Hugo_Symbol','Tumor_Sample_Barcode','Variant_Classification')){v<-as.character(maf[[k]]);if(anyNA(v)||any(!nzchar(v))||any(v!=trimws(v))||any(grepl('[[:cntrl:]]',v)))stop('Every MAF call needs complete exact labels',call.=FALSE);maf[[k]]<-v};sig$gene<-ids(sig$gene,'Significance gene');sig$q<-parse_numeric(sig$q,'Supplied q');if(any(!is.finite(sig$q))||any(sig$q<0|sig$q>1))stop('Every supplied q must be finite in [0,1]',call.=FALSE);read_samples<-function(p){if(tolower(tools::file_ext(p))%in%c('csv','tsv','rds','xlsx','xls')){d<-read_table(p);if(ncol(d)!=1||names(d)!='sample')stop('Sample table needs exactly one sample header',call.=FALSE);ext<-tolower(tools::file_ext(p));if(ext%in%c('csv','tsv')){fc<-utils::count.fields(p,sep=if(ext=='csv')','else'\t',quote='\"',comment.char='',blank.lines.skip=FALSE);if(length(fc)!=nrow(d)+1L||anyNA(fc)||any(fc!=1L))stop('Every sample-list record must contain one ID; blank records cannot be silently removed',call.=FALSE)};ids(d[[1]],'Cohort')}else ids(readLines(p,warn=FALSE),'Cohort')};ms<-read_samples(meta_path);ls<-read_samples(local_path);if(!length(ms)||!length(ls)||length(intersect(ms,ls)))stop('Both cohort lists must be nonempty and disjoint',call.=FALSE);samples<-c(ms,ls);if(length(samples)>250||!all(samples%in%maf$Tumor_Sample_Barcode))stop('At most 250 actual cohort IDs, every ID must occur in supplied MAF; no silent intersection',call.=FALSE);q_cutoff<-num('q_cutoff',.05,.000001,1);genes<-sig$gene[sig$q<q_cutoff];if(!length(genes)||length(genes)>24||!all(genes%in%maf$Hugo_Symbol))stop('1..24 q-selected genes must occur in supplied MAF; no silent intersection',call.=FALSE);selected<-maf[maf$Hugo_Symbol%in%genes&maf$Tumor_Sample_Barcode%in%samples,,drop=FALSE];if(!nrow(selected))stop('No supplied calls in selected gene/cohort scope',call.=FALSE);alias<-c("5'Flank"='Flank5',"3'Flank"='Flank3',"5'UTR"='UTR5',"3'UTR"='UTR3');selected$display_type<-selected$Variant_Classification;hit<-match(selected$display_type,names(alias));selected$display_type[!is.na(hit)]<-alias[hit[!is.na(hit)]];base_types<-c('Missense_Mutation','Nonsense_Mutation','Frame_Shift_Del','Frame_Shift_Ins','UTR5','Splice_Site','Silent','UTR3','In_Frame_Del','Flank5','Intron','In_Frame_Ins');known<-c(base_types,'Flank3','RNA','Nonstop_Mutation','Translation_Start_Site');if(any(!selected$display_type%in%known))stop('Unknown selected mutation classifications cannot be silently omitted',call.=FALSE);types<-known[known%in%selected$display_type];cols<-c(setNames(colorRampPalette(RColorBrewer::brewer.pal(11,'Spectral'))(12)[12:1],base_types),Flank3='#333333',RNA='#CC79A7',Nonstop_Mutation='#009E73',Translation_Start_Site='#0072B2')[types];ng<-length(genes);ns<-length(samples);arr<-array(FALSE,c(ng,ns,length(types)),dimnames=list(genes,samples,types));for(i in seq_len(nrow(selected)))arr[selected$Hugo_Symbol[i],selected$Tumor_Sample_Barcode[i],selected$display_type[i]]<-TRUE;present<-apply(arr,c(1,2),any);count_matrix<-apply(arr,c(1,2),sum);gene_order<-order(-rowSums(present),match(genes,sig$gene));genes<-genes[gene_order];arr<-arr[gene_order,,,drop=FALSE];present<-present[gene_order,,drop=FALSE];count_matrix<-count_matrix[gene_order,,drop=FALSE];sample_order<-choice('sample_order','memo_sort',c('memo_sort','grouped_input','grouped_memo'));score<-apply(count_matrix,2,function(v)sum(ifelse(v>0,2^(ng-seq_len(ng)/v),0)));ord<-if(sample_order=='memo_sort')order(-score,seq_along(samples))else if(sample_order=='grouped_memo')c(order(-score[seq_along(ms)],seq_along(ms)),length(ms)+order(-score[length(ms)+seq_along(ls)],seq_along(ls)))else seq_along(samples);samples<-samples[ord];arr<-arr[,ord,,drop=FALSE];present<-present[,ord,drop=FALSE];count_matrix<-count_matrix[,ord,drop=FALSE];group<-ifelse(samples%in%ms,'Meta','Local');gc<-c(Meta='#EABF00',Local='#2874C5');width_mm<-num('width_mm',300,280,340);height_mm<-num('height_mm',120,110,155);dpi<-integer_param('dpi',600,96,1200);font_size<-num('font_size',8.5,7.5,9);legend_size<-num('legend_size',8.5,7.5,9);cell_gap<-num('cell_gap',.3,.12,.4);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='Times New Roman')'Times New Roman'else'Arial';scale<-width_mm/300;left<-10*scale;right<-174*scale;bottom<-18;top<-height_mm-23;dx<-(right-left)/ns;dy<-(top-bottom)/ng;if(dx<=cell_gap+.1||dy<=cell_gap+.5)stop('Cell geometry requires wider/taller canvas or smaller gap',call.=FALSE);gene_x<-right+2*scale;bar_left<-193*scale;bar_width<-31*scale;legend_x<-232*scale;group_x<-275*scale;top_bottom<-top+2;top_height<-18;top_counts<-apply(arr,c(2,3),sum);row_counts<-apply(arr,c(1,3),sum);top_max<-max(1,pretty(c(0,max(rowSums(top_counts))),n=4));row_max<-max(1,pretty(c(0,max(rowSums(row_counts))),n=4));cells<-expand.grid(gene=genes,sample=samples,stringsAsFactors=FALSE);cells$x_mm<-left+(match(cells$sample,samples)-.5)*dx;cells$y_mm<-top-(match(cells$gene,genes)-.5)*dy;cells$width_mm<-dx-cell_gap;cells$height_mm<-dy-cell_gap*1.7;cells$n_types<-as.vector(count_matrix);segments<-list();ii<-0L;for(j in seq_len(ns))for(i in seq_len(ng)){tt<-types[arr[i,j,]];if(length(tt))for(k in seq_along(tt)){ii<-ii+1L;segments[[ii]]<-data.frame(gene=genes[i],sample=samples[j],type=tt[k],x_mm=left+(j-.5)*dx,y_mm=top-(i-.5)*dy+(dy-cell_gap*1.7)*(.5-(k-.5)/length(tt)),width_mm=dx-cell_gap,height_mm=(dy-cell_gap*1.7)/length(tt),fill=cols[tt[k]])}};segments<-do.call(rbind,segments);row_summary<-data.frame(gene=genes,q=sig$q[match(genes,sig$gene)],affected=rowSums(present),denominator=ns,percent=100*rowSums(present)/ns,distinct_type_presences=rowSums(row_counts));caption<-c(sprintf('%d selected MAF records; %d occupied cells (%d multi-type); %d q-selected genes; supplied q < %g; %d cohort IDs (Meta %d, Local %d).',nrow(selected),sum(present),sum(count_matrix>1),ng,q_cutoff,ns,length(ms),length(ls)),sprintf('Bars count distinct gene/sample/type presence, not raw events or TMB. %s order; all selected records and actual IDs exported.',sample_order),'Blank = no supplied call, not verified wild type; multi-type cells partitioned. MutSigCV not rerun; cohort clinical meaning unverified.')
sysfonts::font_add(family,regular=systemfonts::match_fonts(family)$path,bold=systemfonts::match_fonts(family,weight='bold')$path,italic=systemfonts::match_fonts(family,italic=TRUE)$path,bolditalic=systemfonts::match_fonts(family,weight='bold',italic=TRUE)$path);probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,size)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=size))),'mm',TRUE);if(any(vapply(genes,tw,numeric(1),size=font_size)>bar_left-gene_x-1)||any(vapply(types,tw,numeric(1),size=legend_size)>group_x-legend_x-5)||any(vapply(caption,tw,numeric(1),size=6)>width_mm-8)||dy<font_size*25.4/72*1.2)stop('Complete labels/caption need larger permitted canvas or smaller font',call.=FALSE)},finally={dev.off();unlink(probe)})
bar_segments<-list();ii<-0L;for(j in seq_len(ns)){lo<-0;for(k in seq_along(types)){v<-top_counts[j,k];if(v>0){ii<-ii+1L;bar_segments[[ii]]<-data.frame(role='sample',id=samples[j],type=types[k],x_mm=left+(j-.5)*dx,y_mm=top_bottom+(lo+v/2)/top_max*top_height,width_mm=dx-cell_gap,height_mm=v/top_max*top_height,fill=cols[k])};lo<-lo+v}};for(i in seq_len(ng)){lo<-0;for(k in seq_along(types)){v<-row_counts[i,k];if(v>0){ii<-ii+1L;bar_segments[[ii]]<-data.frame(role='gene',id=genes[i],type=types[k],x_mm=bar_left+(lo+v/2)/row_max*bar_width,y_mm=top-(i-.5)*dy,width_mm=v/row_max*bar_width,height_mm=dy*.8,fill=cols[k])};lo<-lo+v}};bar_segments<-do.call(rbind,bar_segments)
render<-function(){grid::grid.newpage();mm<-function(z)grid::unit(z,'mm');txt<-function(z,x,y,size=font_size,face='plain',just='center')grid::grid.text(z,mm(x),mm(y),just=just,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face,col='#111111'));ln<-function(x,y,w=.3)grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col='#111111',lwd=w*72.27/25.4));rect<-function(d)grid::grid.rect(mm(d$x_mm),mm(d$y_mm),mm(d$width_mm),mm(d$height_mm),gp=grid::gpar(fill=d$fill,col=NA));cc<-cells;cc$fill<-'#DCDDDE';rect(cc);rect(segments);rect(bar_segments);for(j in seq_len(ns))grid::grid.rect(mm(left+(j-.5)*dx),mm(bottom-2.5),mm(dx),mm(3),gp=grid::gpar(fill=gc[group[j]],col=NA));for(i in seq_len(ng)){yy<-top-(i-.5)*dy;txt(paste0(round(row_summary$percent[i]),'%'),left-2,yy,just='right');txt(genes[i],gene_x,yy,just='left')};ln(rep(left-1,2),c(top_bottom,top_bottom+top_height));for(v in pretty(c(0,top_max),n=4)){yy<-top_bottom+v/top_max*top_height;ln(c(left-2,left-1),rep(yy,2));txt(v,left-3,yy,7,just='right')};ln(c(bar_left,bar_left+bar_width),rep(top+1,2));rticks<-pretty(c(0,row_max),n=4);for(v in rticks[rticks<=row_max]){xx<-bar_left+v/row_max*bar_width;ln(rep(xx,2),c(top+1,top+2));txt(v,xx,top+4,7)};txt('Alterations',legend_x,top-1,9,face='bold',just='left');for(k in seq_along(types)){yy<-top-7-(k-1)*4;grid::grid.rect(mm(legend_x+1),mm(yy),mm(2),mm(3),gp=grid::gpar(fill=cols[k],col=NA));txt(types[k],legend_x+3,yy,legend_size,just='left')};txt('Cohort',group_x,top-1,9,face='bold',just='left');for(k in seq_along(gc)){yy<-top-7-(k-1)*4;grid::grid.rect(mm(group_x+1),mm(yy),mm(2),mm(3),gp=grid::gpar(fill=gc[k],col=NA));txt(names(gc)[k],group_x+3,yy,8,just='left')};for(k in seq_along(caption))txt(caption[k],4,8-(k-1)*3,6,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())};write.csv(selected,file.path(output_dir,'selected_records.csv'),row.names=FALSE);write.csv(cells,file.path(output_dir,'matrix_cells.csv'),row.names=FALSE);write.csv(segments,file.path(output_dir,'actual_type_segments.csv'),row.names=FALSE);write.csv(bar_segments,file.path(output_dir,'actual_bar_segments.csv'),row.names=FALSE);write.csv(row_summary,file.path(output_dir,'gene_frequencies_q.csv'),row.names=FALSE);write.csv(data.frame(sample=samples,cohort=group,display_index=seq_along(samples),distinct_type_presence=rowSums(top_counts)),file.path(output_dir,'sample_order_counts.csv'),row.names=FALSE);saveRDS(list(maf=maf,sig=sig,selected=selected,arr=arr,present=present,cells=cells,segments=segments,bars=bar_segments,meta=ms,local=ls,top_counts=top_counts,row_counts=row_counts,summary=row_summary),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(maf_path=maf_path,sig_genes_path=sig_path,records=nrow(maf),selected_records=nrow(selected),selected_exact_duplicate_records=sum(duplicated(selected)),occupied=sum(present),multi_type_cells=sum(count_matrix>1),distinct_type_presences=sum(arr),genes=genes,samples=samples,types=types,colors=as.list(cols),cohort_colors=as.list(gc),q_cutoff=q_cutoff,sample_order=sample_order,font_size=font_size,legend_size=legend_size,cell_gap=cell_gap,width_mm=width_mm,height_mm=height_mm,geometry=list(left_mm=left,right_mm=right,bottom_mm=bottom,top_mm=top,dx=dx,dy=dy,bar_left=bar_left,bar_width=bar_width,top_max=top_max,row_max=row_max),caption=caption,scope='Supplied q only; MutSigCV not rerun. Cohorts not remapped to clinical LN labels. Presence counts not TMB or raw event burden.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',length(genes),'supplied q-selected genes and',length(samples),'actual cohort IDs; all',nrow(selected),'selected records retained with multi-type partitions\n')
