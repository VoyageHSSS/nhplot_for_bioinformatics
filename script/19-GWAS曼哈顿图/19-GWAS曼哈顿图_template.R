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
required_packages <- c("jsonlite", "svglite", "ragg", "ggplot2", "readxl")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE); params <- if (is.null(cfg$params)) list() else cfg$params
if(!is.list(cfg)||!is.list(params)||(!is.null(names(params))&&any(!nzchar(names(params)))))stop("config/params must be objects",call.=FALSE)
gp <- function(name, default = NULL) {value<-params[[name]];if(is.null(value))for(section in c("data","model","selection","colors","typography","geometry","axis","legend","layout","labels","statistics","threshold","export"))if(is.list(params[[section]])&&!is.null(params[[section]][[name]])){value<-params[[section]][[name]];break};if(is.null(value))default else value}
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







data_file<-resolve_input(text_param('data_path','',TRUE),file.path(script_dir,'assets/easy_input.csv'));raw<-read_table(data_file);needed<-c('CHR','SNP','POS','P_value');if(!nrow(raw)||anyDuplicated(names(raw))||!all(needed%in%names(raw)))stop('GWAS requires distinct headers CHR/SNP/POS/P_value and rows',call.=FALSE);gwas<-raw;gwas$source_row<-seq_len(nrow(raw));for(v in c('CHR','SNP')){gwas[[v]]<-trimws(as.character(gwas[[v]]));if(anyNA(gwas[[v]])||any(!nzchar(gwas[[v]]))||any(grepl('[[:cntrl:]]',gwas[[v]])))stop(v,' must be complete text identifiers',call.=FALSE)};if(anyDuplicated(gwas$SNP))stop('SNP IDs must identify distinct input rows',call.=FALSE)
gwas$POS<-parse_numeric(gwas$POS,'POS');gwas$P_value<-parse_numeric(gwas$P_value,'P_value');if(any(!is.finite(gwas$POS))||any(gwas$POS<0|gwas$POS!=floor(gwas$POS)))stop('POS must be a finite nonnegative integer',call.=FALSE);if(any(!is.finite(gwas$P_value))||any(gwas$P_value<=0|gwas$P_value>1))stop('P_value must be finite in (0,1]; provide positive representable values rather than underflowed zero',call.=FALSE)
if(!'type'%in%names(gwas))gwas$type<-'Other';if(!'gene'%in%names(gwas))gwas$gene<-'';for(v in c('type','gene')){gwas[[v]]<-trimws(as.character(gwas[[v]]));if(anyNA(gwas[[v]])||any(grepl('[[:cntrl:]]',gwas[[v]])))stop(v,' must not contain missing/control text',call.=FALSE)};known_types<-c('non','Other','Loci identified in previous GWAS','Loci identified in this study');if(any(!gwas$type%in%known_types))stop('type requires non/Other or one of the two documented locus categories',call.=FALSE)
karyotype_input<-text_param('karyotype_path','',TRUE);bundled<-identical(normalizePath(data_file),normalizePath(file.path(script_dir,'assets/easy_input.csv')));karyotype_file<-if(nzchar(karyotype_input))resolve_input(karyotype_input,'')else if(bundled)file.path(script_dir,'assets/Karyotype.txt')else NULL
if(!is.null(karyotype_file)){chrom<-read_table(karyotype_file);if(anyDuplicated(names(chrom))||!all(c('chr','chr_len')%in%names(chrom))||!nrow(chrom))stop('Karyotype requires chr/chr_len rows',call.=FALSE);chrom$chr<-trimws(as.character(chrom$chr));chrom$chr_len<-parse_numeric(chrom$chr_len,'chr_len');if(anyNA(chrom$chr)||any(!nzchar(chrom$chr))||anyDuplicated(chrom$chr)||any(!is.finite(chrom$chr_len))||any(chrom$chr_len<=0|chrom$chr_len!=floor(chrom$chr_len)))stop('Karyotype requires unique complete chromosome IDs and positive integer lengths',call.=FALSE);if(any(!gwas$CHR%in%chrom$chr))stop('GWAS contains chromosomes absent from the supplied karyotype',call.=FALSE);if(any(gwas$POS>chrom$chr_len[match(gwas$CHR,chrom$chr)]))stop('POS exceeds its supplied chromosome length',call.=FALSE)}else{order_chr<-unique(gwas$CHR);chrom<-data.frame(chr=order_chr,chr_len=vapply(order_chr,function(g)max(gwas$POS[gwas$CHR==g]),numeric(1)));if(any(chrom$chr_len<=0))stop('Cannot derive a positive chromosome span from all-zero positions; supply karyotype',call.=FALSE)}
chrom$offset<-c(0,head(cumsum(chrom$chr_len),-1));if(any(!is.finite(chrom$offset)))stop('Chromosome cumulative offsets are not finite',call.=FALSE);chrom$center<-chrom$offset+chrom$chr_len/2;chrom$observed_variants<-vapply(chrom$chr,function(g)sum(gwas$CHR==g),integer(1));chr_order<-chrom$chr;gwas$SNPx<-gwas$POS+chrom$offset[match(gwas$CHR,chrom$chr)];gwas$SNPy<--log10(gwas$P_value)
cutoff<-num('logp_cutoff',4,0,320);point_size<-num('point_size',.55,.1,2);hit_size<-num('hit_size',1.8,.2,5);point_alpha<-num('point_alpha',.8,.2,1);label_size<-num('label_size',2.6,1.5,5);line_width<-num('line_width',.3,.15,1);base_size<-num('base_size',9,7,20);width_mm<-num('width_mm',180,140,360);height_mm<-num('height_mm',120,90,300);dpi<-integer_param('dpi',600,96,1200);family<-choice('family','Arial',c('Arial','Helvetica','Times New Roman'));family<-if(family=='Times New Roman')'serif'else'sans';color<-function(n,d){v<-text_param(n,d);if(!grepl('^#[0-9a-fA-F]{6}$',v))stop(n,' requires #RRGGBB',call.=FALSE);v};threshold_color<-color('threshold_color','#B54040');text_color<-color('text_color','#25313C');title<-text_param('title','Variant associations',TRUE);x_title<-text_param('x_title','Chromosome',TRUE);y_title<-text_param('y_title','-log10(P)',TRUE);if(any(grepl('[[:cntrl:]]|Case [0-9]|cover_|debug',c(title,x_title,y_title),ignore.case=TRUE)))stop('Remove internal/control labels',call.=FALSE)
gwas$above_cutoff<-gwas$SNPy>cutoff;type_map<-c('non'='Other','Other'='Other','Loci identified in previous GWAS'='Previous GWAS','Loci identified in this study'='This study');gwas$type_label<-factor(unname(type_map[gwas$type]),levels=c('Other','Previous GWAS','This study'));gwas$point_size<-ifelse(gwas$above_cutoff,hit_size,point_size);gwas$label<-ifelse(nzchar(gwas$gene)&gwas$gene!=gwas$SNP,paste(gwas$gene,gwas$SNP,sep='\n'),gwas$SNP);shape_values<-c('Other'=16,'Previous GWAS'=17,'This study'=8);palette<-setNames(rep(c('#405A75','#72777D'),length.out=nrow(chrom)),chr_order);ymax<-max(cutoff,max(gwas$SNPy))+max(1.7,.2*max(cutoff,max(gwas$SNPy)));xspan<-sum(chrom$chr_len);if(!is.finite(xspan)||xspan<=0)stop('Genome span must be finite and positive',call.=FALSE);xlim<-c(-.012*xspan,1.012*xspan)
caption<-if(bundled)'Provided example: Gene identifiers and genome build are unverified; locus types are supplied annotations.'else'P-values and locus types are supplied inputs; no association test or multiple-testing correction is computed.';caption<-paste(caption,sprintf('Display cutoff: -log10(P) > %g (P < %s); not automatically a genome-wide significance claim.',cutoff,format(10^-cutoff,scientific=TRUE,digits=3)),sep='\n')
plot_obj<-ggplot2::ggplot(gwas,ggplot2::aes(SNPx,SNPy))+ggplot2::geom_point(ggplot2::aes(colour=CHR,shape=type_label,size=point_size),alpha=point_alpha,stroke=.3)+ggplot2::scale_colour_manual(values=palette,guide='none')+ggplot2::scale_shape_manual(values=shape_values,drop=FALSE,name='Supplied locus type')+ggplot2::scale_size_identity()+ggplot2::geom_hline(yintercept=cutoff,colour=threshold_color,linewidth=line_width,linetype='dashed')+ggplot2::scale_x_continuous(breaks=chrom$center,labels=chrom$chr,guide=ggplot2::guide_axis(n.dodge=2),expand=c(0,0))+ggplot2::scale_y_continuous(expand=c(0,0))+ggplot2::coord_cartesian(xlim=xlim,ylim=c(-.025*ymax,ymax),expand=FALSE,clip='on')+ggplot2::labs(title=title,x=x_title,y=y_title,caption=caption)+ggplot2::theme_classic(base_family=family,base_size=base_size)+ggplot2::theme(axis.line=ggplot2::element_line(linewidth=.25,colour=text_color),axis.ticks=ggplot2::element_line(linewidth=.25,colour=text_color),axis.text=ggplot2::element_text(size=7.5,colour=text_color),axis.title=ggplot2::element_text(size=8.5,face='bold',colour=text_color),axis.title.x=ggplot2::element_text(margin=ggplot2::margin(t=5)),axis.title.y=ggplot2::element_text(margin=ggplot2::margin(r=5)),plot.title=ggplot2::element_text(size=base_size+1.5,face='bold',margin=ggplot2::margin(b=4)),legend.position='top',legend.title=ggplot2::element_text(size=7.2,face='bold'),legend.text=ggplot2::element_text(size=7.2),legend.margin=ggplot2::margin(0,0,3,0),legend.key.width=grid::unit(3,'mm'),legend.key.height=grid::unit(3,'mm'),plot.caption=ggplot2::element_text(size=6,hjust=0,lineheight=1.15,colour='#52606D',margin=ggplot2::margin(t=4)),plot.margin=ggplot2::margin(4,4,4,4,unit='mm'))+ggplot2::guides(shape=ggplot2::guide_legend(override.aes=list(size=2,colour=text_color,alpha=1)))
# ponytail: deterministic greedy packing for at most 40 labels; dense hit sets need an explicit labelled subset.
hits<-gwas[gwas$above_cutoff,,drop=FALSE];if(nrow(hits)>40)stop('More than 40 labels exceed this single-panel design; choose a justified display cutoff or a separate labelling design',call.=FALSE)
probe<-tempfile('manhattan-layout-',tmpdir=tempdir(),fileext='.pdf');grDevices::pdf(probe,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE);positions<-data.frame();geometry<-NULL
tryCatch({grid::grid.newpage();grid::grid.draw(ggplot2::ggplotGrob(plot_obj));grid::grid.force();v<-grid::grid.ls(viewports=TRUE,grobs=FALSE,recursive=TRUE,print=FALSE);ii<-grep('^panel[.]',v$name);if(length(ii)!=1)stop('Expected actual Manhattan panel',call.=FALSE);pv<-c(strsplit(v$vpPath[ii],'::',fixed=TRUE)[[1]][-1],v$name[ii]);grid::downViewport(do.call(grid::vpPath,as.list(pv)));pw<-grid::convertWidth(grid::unit(1,'npc'),'mm',valueOnly=TRUE);ph<-grid::convertHeight(grid::unit(1,'npc'),'mm',valueOnly=TRUE);pp<-ggplot2::ggplot_build(plot_obj)$layout$panel_params[[1]];xr<-pp$x.range;yr<-pp$y.range;loc<-grid::deviceLoc(grid::unit(0,'npc'),grid::unit(0,'npc'),valueOnly=TRUE);geometry<-list(x_mm=loc$x*25.4,y_mm=loc$y*25.4,width_mm=pw,height_mm=ph,xrange=xr,yrange=yr);if(pw<110||ph<55)stop('Plot panel too small for readable variant labels',call.=FALSE);boxes<-list();hx<-(hits$SNPx-xr[1])/diff(xr)*pw;hy<-(hits$SNPy-yr[1])/diff(yr)*ph;fs<-label_size*ggplot2::.pt
for(j in order(-hits$SNPy,hits$source_row)){lab<-hits$label[j];tw<-grid::convertWidth(grid::grobWidth(grid::textGrob(lab,gp=grid::gpar(fontfamily=family,fontsize=fs))),'mm',valueOnly=TRUE)+1;lines<-length(strsplit(lab,'\n',fixed=TRUE)[[1]]);th<-fs*25.4/72*(1.1+(lines-1)*1.32)+.8;if(tw>pw*.45)stop('Variant label too wide; use a concise gene/SNP annotation',call.=FALSE);chosen<-NULL;best_cost<-Inf;for(dy in c(0,5,10,15,20,25,30)){for(dx in c(0,as.vector(rbind(seq(8,pw,8),-seq(8,pw,8))))){cx<-min(pw-tw/2-1,max(tw/2+1,hx[j]+dx));cy<-max(hy[j]+hit_size*.75+th/2+1,((cutoff-yr[1])/diff(yr))*ph+th/2+1)+dy;b<-c(cx-tw/2,cx+tw/2,cy-th/2,cy+th/2);if(b[3]<0||b[4]>ph-1)next;if(any(vapply(boxes,function(z)b[1]<z[2]+.8&&b[2]>z[1]-.8&&b[3]<z[4]+.8&&b[4]>z[3]-.8,logical(1))))next;if(any(hx>=b[1]-hit_size&hx<=b[2]+hit_size&hy>=b[3]-hit_size&hy<=b[4]+hit_size))next;cost<-(cx-hx[j])^2+(cy-hy[j]-th/2)^2;if(cost<best_cost){best_cost<-cost;chosen<-c(cx,cy,b)}};};if(!is.null(chosen)){boxes[[length(boxes)+1]]<-chosen[3:6];cx<-chosen[1];cy<-chosen[2];ex<-min(chosen[4],max(chosen[3],hx[j]));ey<-chosen[5];vx<-hx[j]-ex;vy<-hy[j]-ey;dist<-sqrt(vx^2+vy^2);stop_pad<-hit_size*.8;tx<-hx[j]-vx/dist*stop_pad;ty<-hy[j]-vy/dist*stop_pad;positions<-rbind(positions,data.frame(source_row=hits$source_row[j],label=lab,x=xr[1]+cx/pw*diff(xr),y=yr[1]+cy/ph*diff(yr),segment_x=xr[1]+ex/pw*diff(xr),segment_y=yr[1]+ey/ph*diff(yr),target_x=xr[1]+tx/pw*diff(xr),target_y=yr[1]+ty/ph*diff(yr),width_mm=tw,height_mm=th))}else stop('Label cannot fit: ',hits$SNP[j],'; panel ',round(pw,2),' x ',round(ph,2),'mm; box ',round(tw,2),' x ',round(th,2),'mm. Enlarge canvas or reduce label_size',call.=FALSE)};grid::upViewport(0)},finally={grDevices::dev.off();unlink(probe)})
if(nrow(positions))plot_obj<-plot_obj+ggplot2::geom_segment(data=positions,ggplot2::aes(x=segment_x,y=segment_y,xend=target_x,yend=target_y),inherit.aes=FALSE,linewidth=.25,colour='#52606D')+ggplot2::geom_text(data=positions,ggplot2::aes(x,y,label=label),inherit.aes=FALSE,size=label_size,family=family,colour=text_color,lineheight=1.1)
build<-ggplot2::ggplot_build(plot_obj);render<-function(kind){path<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(path,width=width_mm,height=height_mm,units='mm',res=dpi,bg='white')else if(kind=='pdf')grDevices::pdf(path,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE)else svglite::svglite(path,width=width_mm/25.4,height=height_mm/25.4);tryCatch({grid::grid.newpage();for(s in c(title,strsplit(caption,'\n',fixed=TRUE)[[1]])){fs<-if(s==title)base_size+1.5 else 6;if(grid::convertWidth(grid::grobWidth(grid::textGrob(s,gp=grid::gpar(fontfamily=family,fontsize=fs))),'mm',valueOnly=TRUE)>width_mm-8)stop('Title/caption exceeds canvas',call.=FALSE)};grid::grid.draw(ggplot2::ggplotGrob(plot_obj));grid::grid.force();if(kind=='svg'){v<-grid::grid.ls(viewports=TRUE,grobs=FALSE,recursive=TRUE,print=FALSE);ii<-grep('^panel[.]',v$name);pv<-c(strsplit(v$vpPath[ii],'::',fixed=TRUE)[[1]][-1],v$name[ii]);grid::downViewport(do.call(grid::vpPath,as.list(pv)));pw<-grid::convertWidth(grid::unit(1,'npc'),'mm',valueOnly=TRUE);ph<-grid::convertHeight(grid::unit(1,'npc'),'mm',valueOnly=TRUE);loc<-grid::deviceLoc(grid::unit(0,'npc'),grid::unit(0,'npc'),valueOnly=TRUE);geometry<<-list(x_mm=loc$x*25.4,y_mm=loc$y*25.4,width_mm=pw,height_mm=ph,xrange=build$layout$panel_params[[1]]$x.range,yrange=build$layout$panel_params[[1]]$y.range);grid::upViewport(0)}},finally=grDevices::dev.off())};for(kind in c('png','pdf','svg'))render(kind)
write_tsv<-function(d,n)write.table(d,file.path(output_dir,n),sep='\t',quote=TRUE,row.names=FALSE,na='NA',qmethod='double');write_tsv(raw,'source-records.tsv');write_tsv(gwas,'variant-coordinates.tsv');write_tsv(chrom,'chromosome-spans.tsv');write_tsv(positions,'label-positions.tsv');for(i in seq_along(build$data))write_tsv(build$data[[i]],paste0('layer-',i,'.tsv'));saveRDS(list(raw=raw,gwas=gwas,chrom=chrom,positions=positions,palette=palette,cutoff=cutoff),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(input=data_file,karyotype=karyotype_file,karyotype_semantics=if(is.null(karyotype_file))'observed maximum POS; not assembly lengths'else'supplied lengths; genome build unverified',n_variants=nrow(gwas),chromosomes=chr_order,n_hits=nrow(hits),threshold=cutoff,comparison='strict SNPy > cutoff',zero_positions=sum(gwas$POS==0),point_size=point_size,hit_size=hit_size,alpha=point_alpha,label_size=label_size,line_width=line_width,width_mm=width_mm,height_mm=height_mm,palette=as.list(palette),geometry=geometry,caption=caption),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered ',nrow(gwas),' unchanged supplied variants; ',nrow(hits),' above display cutoff\n')
