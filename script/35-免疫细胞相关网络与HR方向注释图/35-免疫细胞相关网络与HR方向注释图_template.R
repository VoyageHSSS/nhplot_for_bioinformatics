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










immune_path<-resolve_input(text_param('immune_path','',TRUE),file.path(script_dir,'assets/easy_input_immune.csv'));hr_path<-resolve_input(text_param('hr_path','',TRUE),file.path(script_dir,'assets/easy_input_HR.csv'));links_path<-resolve_input(text_param('links_path','',TRUE),file.path(script_dir,'assets/output_links.csv'));imraw<-read_table(immune_path);hrraw<-read_table(hr_path);lr<-read_table(links_path)
if(names(imraw)[1]=='')names(imraw)[1]<-'sample_id';if(names(lr)[1]=='')names(lr)[1]<-'record_id'
for(d in list(imraw,hrraw,lr))if(anyDuplicated(names(d))||any(!nzchar(names(d))))stop('Unique complete table headers required',call.=FALSE)
ids<-function(x){v<-trimws(as.character(x));if(anyNA(v)||any(!nzchar(v))||anyDuplicated(v)||any(grepl('[[:cntrl:]]',v)))stop('Complete unique text IDs required',call.=FALSE);v};samples<-ids(imraw[[1]]);nodes<-ids(names(imraw)[-1]);hn<-ids(hrraw[[1]]);if(length(samples)<3||length(nodes)<3||length(nodes)>30||!setequal(nodes,hn))stop('3 to 30 nodes, at least 3 samples, and exactly matched immune/HR node IDs required',call.=FALSE)
immune<-sapply(nodes,function(n)parse_numeric(imraw[[n]],n));dimnames(immune)<-list(samples,nodes);if(any(!is.finite(immune)))stop('Immune values must all be finite; no imputation',call.=FALSE)
if(!all(c('HR','log_rank_p')%in%names(hrraw))||!all(c('from','to','cor','pvalue')%in%names(lr)))stop('Actual HR/log_rank_p and from/to/cor/pvalue columns required',call.=FALSE)
hr<-data.frame(node=hn,HR=parse_numeric(hrraw$HR,'HR'),p=parse_numeric(hrraw$log_rank_p,'log_rank_p'));hr<-hr[match(nodes,hr$node),];if(any(!is.finite(as.matrix(hr[-1])))||any(hr$HR<=0)||any(hr$p<=0|hr$p>1))stop('HR must be positive finite; node P values must be in (0,1]',call.=FALSE)
e<-data.frame(from=trimws(as.character(lr$from)),to=trimws(as.character(lr$to)),cor=parse_numeric(lr$cor,'cor'),pvalue=parse_numeric(lr$pvalue,'pvalue'));if(anyNA(e)||any(!is.finite(e$cor))||any(!is.finite(e$pvalue))||any(abs(e$cor)>1)||any(e$pvalue<=0|e$pvalue>1)||any(!e$from%in%nodes|!e$to%in%nodes)||any(e$from==e$to))stop('Every supplied edge must have known distinct nodes, cor in [-1,1], P in (0,1]',call.=FALSE)
keys<-vapply(seq_len(nrow(e)),function(i)paste(sort(c(e$from[i],e$to[i])),collapse='\r'),character(1));if(anyDuplicated(keys))stop('Duplicate undirected pairs must be resolved explicitly',call.=FALSE);e$weight<--log10(e$pvalue);if('weight'%in%names(lr)){w<-parse_numeric(lr$weight,'weight');if(any(!is.finite(w))||any(abs(w-e$weight)>1e-8*pmax(1,e$weight)))stop('Supplied edge weight must equal -log10(P)',call.=FALSE)}
p_cutoff<-num('p_cutoff',1e-4,1e-6,1);e<-e[e$pvalue<p_cutoff,,drop=FALSE];cluster_mode<-choice('cluster_mode','source',c('source','hclust'));palette<-c(A='#FDBF6F',B='#1F78B4',C='#E31A1C',D='#8C510A');if(cluster_mode=='source'){ref<-read_table(file.path(script_dir,'assets/source_clusters.csv'));if(!setequal(nodes,ref$node))stop('Source mode requires the original 23 named nodes; select hclust for other data',call.=FALSE);cluster<-setNames(ref$cluster,ref$node)[nodes]}else{hc<-hclust(dist(t(immune)));cluster<-setNames(c('A','B','C','D')[cutree(hc,min(4,length(nodes)))],nodes)}
font_family<-choice('font_family','Arial',c('Arial','Helvetica','Times New Roman'));family<-if(font_family=='Times New Roman')'serif'else'sans';width_mm<-num('width_mm',245,235,360);height_mm<-num('height_mm',135,135,240);dpi<-integer_param('dpi',600,96,1200);edge_base<-num('edge_base',.08,.05,.2);edge_scale<-num('edge_scale',.07,0,.15);node_base<-num('node_base',2.2,1.8,3);node_scale<-num('node_scale',1.25,.7,1.6);label_size<-num('label_size',2.5,2.3,3);legend_text_size<-num('legend_text_size',2.5,2.3,3);label_pt<-label_size*72.27/25.4;legend_pt<-legend_text_size*72.27/25.4;pos<-text_param('positive_edge_color','#E8898A');neg<-text_param('negative_edge_color','#9FC2E0');risk<-text_param('hr_risk_color','#111111');fav<-text_param('hr_favorable_color','#B7DE78');tryCatch(invisible(grDevices::col2rgb(c(pos,neg,risk,fav))),error=function(e)stop('Invalid color',call.=FALSE))
ord<-order(factor(cluster,levels=names(palette)),nodes);nd<-data.frame(node=nodes[ord],cluster=unname(cluster[ord]),HR=hr$HR[ord],log_rank_p=hr$p[ord]);nd$diameter_mm<-node_base+node_scale*sqrt(-log10(nd$log_rank_p)+1);nd$fill<-unname(palette[nd$cluster]);nd$center_color<-ifelse(nd$HR>1,risk,ifelse(nd$HR<1,fav,'#72777D'));cx<-94;cy<-(height_mm+5)/2;radius<-min(45,(height_mm-40)/2);angles<-seq(pi/2,pi/2+2*pi,length.out=nrow(nd)+1)[-1];nd$x_mm<-cx+radius*cos(angles);nd$y_mm<-cy+radius*sin(angles);nd$side<-ifelse(cos(angles)>=0,1,-1);nd$label_x_mm<-cx+nd$side*(radius+6);nd$label_y_mm<-nd$y_mm
for(side in c(-1,1)){ix<-which(nd$side==side);ix<-ix[order(nd$label_y_mm[ix])];yy<-nd$label_y_mm[ix];gap<-label_pt*25.4/72*1.45;for(k in seq_along(yy))if(k>1)yy[k]<-max(yy[k],yy[k-1]+gap);yy<-yy-max(0,max(yy)-(height_mm-22));if(min(yy)<20)stop('Node labels too dense for this canvas',call.=FALSE);nd$label_y_mm[ix]<-yy}
e$width_mm<-edge_base+edge_scale*pmin(4,sqrt(e$weight)/2);e$color<-ifelse(e$cor>=0,pos,neg);curves<-vector('list',nrow(e));for(i in seq_len(nrow(e))){a<-nd[match(e$from[i],nd$node),];b<-nd[match(e$to[i],nd$node),];u<-c(a$x_mm,a$y_mm);v<-c(b$x_mm,b$y_mm);delta<-v-u;len<-sqrt(sum(delta^2));control<-(u+v)/2+c(-delta[2],delta[1])*.12;du<-control-u;dv<-control-v;u<-u+du/sqrt(sum(du^2))*(a$diameter_mm/2+.3);v<-v+dv/sqrt(sum(dv^2))*(b$diameter_mm/2+.3);t<-seq(0,1,length.out=41);curves[[i]]<-cbind(x=(1-t)^2*u[1]+2*(1-t)*t*control[1]+t^2*v[1],y=(1-t)^2*u[2]+2*(1-t)*t*control[2]+t^2*v[2])}
caption<-c('Supplied edges only; unadjusted P threshold; no causal or survival-significance claim.', 'Node diameter = base + scale x sqrt(-log10(P) + 1); area is not proportional to -log10(P).')
# Freeze PDF-measured bounds once; all three devices use the identical geometry.
probe<-file.path(output_dir,'label-probe.pdf');grDevices::pdf(probe,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE);tryCatch({grid::grid.newpage();tw<-function(s,size)grid::convertWidth(grid::grobWidth(grid::textGrob(s,gp=grid::gpar(fontfamily=family,fontsize=size))),'mm',TRUE);nd$label_width_mm<-vapply(nd$node,tw,numeric(1),size=label_pt);if(any(ifelse(nd$side<0,nd$label_x_mm-nd$label_width_mm<5,nd$label_x_mm+nd$label_width_mm>191)))stop('Cell label exceeds its column; reduce font or shorten actual input names',call.=FALSE);for(s in caption)if(tw(s,6)>width_mm-12)stop('Caption too wide',call.=FALSE)},finally=grDevices::dev.off());unlink(probe)
render<-function(kind){path<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(path,width=width_mm,height=height_mm,units='mm',res=dpi,bg='white')else if(kind=='pdf')grDevices::pdf(path,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE)else svglite::svglite(path,width=width_mm/25.4,height=height_mm/25.4,bg='white');tryCatch({grid::grid.newpage();text<-function(s,x,y,size=7,just='left',face='plain',col='#25313C')grid::grid.text(s,grid::unit(x,'mm'),grid::unit(y,'mm'),just=just,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face,col=col));line<-function(x,y,col,lwd=.4)grid::grid.lines(grid::unit(x,'mm'),grid::unit(y,'mm'),gp=grid::gpar(col=col,lwd=lwd,lineend='round'));circle<-function(x,y,r,fill)grid::grid.circle(grid::unit(x,'mm'),grid::unit(y,'mm'),grid::unit(r,'mm'),gp=grid::gpar(fill=fill,col='#333333',lwd=.5));text('Immune-cell correlation network',6,height_mm-7,11,face='bold');text(sprintf('%d samples; %d nodes; %d supplied edges with P < %g',length(samples),nrow(nd),nrow(e),p_cutoff),6,height_mm-13,7)
for(i in seq_len(nrow(e)))line(curves[[i]][,'x'],curves[[i]][,'y'],grDevices::adjustcolor(e$color[i],alpha.f=.65),e$width_mm[i]*72.27/25.4)
for(i in seq_len(nrow(nd))){dx<-nd$label_x_mm[i]-nd$x_mm[i];dy<-nd$label_y_mm[i]-nd$y_mm[i];len<-sqrt(dx^2+dy^2);rr<-nd$diameter_mm[i]/2+.4;line(c(nd$x_mm[i]+dx/len*rr,nd$label_x_mm[i]-nd$side[i]),c(nd$y_mm[i]+dy/len*rr,nd$label_y_mm[i]),'#ADB5BD',.35);circle(nd$x_mm[i],nd$y_mm[i],nd$diameter_mm[i]/2,nd$fill[i]);circle(nd$x_mm[i],nd$y_mm[i],.7,nd$center_color[i]);text(nd$node[i],nd$label_x_mm[i],nd$label_y_mm[i],label_pt,if(nd$side[i]<0)'right'else'left')}
lx<-197;top<-height_mm-28;text(if(cluster_mode=='source')'Source curated groups'else'Hierarchical groups',lx,top,8,face='bold');for(j in 1:4){yy<-top-9-(j-1)*5;circle(lx+1,yy,1.5,palette[j]);text(paste('Cluster',names(palette)[j]),lx+5,yy,legend_pt)};yy<-top-32;text('Supplied HR direction',lx,yy,8,face='bold');for(j in 1:3){y<-yy-7-(j-1)*5;circle(lx+1,y,.7,c(risk,fav,'#72777D')[j]);text(c('HR > 1','HR < 1','HR = 1')[j],lx+5,y,legend_pt)};yy<-top-57;text('Spearman correlation',lx,yy,8,face='bold');for(j in 1:2){y<-yy-6-(j-1)*5;line(c(lx,lx+5),c(y,y),c(pos,neg)[j],.6);text(c('Positive','Negative')[j],lx+8,y,legend_pt)};text(sprintf('P < %g (unadjusted)',p_cutoff),lx,yy-17,6.8);yy<-top-82;text('Log-rank P (diameter)',lx,yy,8,face='bold');pv<-c(.05,.001,1e-5,1e-8);for(j in 1:4){x<-lx+(j-1)*11;circle(x+2,yy-8,(node_base+node_scale*sqrt(-log10(pv[j])+1))/2,'#D9D9D9');text(format(pv[j],scientific=TRUE),x+2,yy-15,6,just='center')};for(j in seq_along(caption))text(caption[j],6,10-(j-1)*3,6,col='#52606D')},finally=grDevices::dev.off())};for(kind in c('png','pdf','svg'))render(kind)
utils::write.csv(nd,file.path(output_dir,'network_nodes.csv'),row.names=FALSE);utils::write.csv(e,file.path(output_dir,'network_edges.csv'),row.names=FALSE);saveRDS(list(immune_raw=imraw,hr_raw=hrraw,links_raw=lr,immune=immune,nodes=nd,edges=e,curves=curves,cluster=cluster),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(n_samples=length(samples),n_nodes=nrow(nd),n_edges=nrow(e),p_cutoff=p_cutoff,p_operator='<',cluster_mode=cluster_mode,cluster_colors=as.list(palette),node_diameter_formula='base + scale * sqrt(-log10(P) + 1)',width_mm=width_mm,height_mm=height_mm,radius_mm=radius,center_mm=c(cx,cy),edge_alpha=.65,caption=caption),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered ',nrow(nd),' nodes and ',nrow(e),' supplied edges without rescaling line widths\n')
