#!/usr/bin/env Rscript

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
required_packages <- c("jsonlite", "svglite", "ragg")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE); params <- if (is.null(cfg$params)) list() else cfg$params
if(!is.list(cfg)||!is.list(params)||(!is.null(names(params))&&any(!nzchar(names(params)))))stop("config/params must be objects",call.=FALSE)
gp <- function(name, default = NULL) if (is.null(params[[name]])) default else params[[name]]
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
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
validate_colour <- function(name, value) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !nzchar(value)) stop(name, " has invalid colour", call. = FALSE)
  tryCatch(grDevices::col2rgb(value), error = function(e) stop(name, " has invalid colour", call. = FALSE))
  if(grDevices::col2rgb(value,alpha=TRUE)[4,1]!=255)stop(name," must be opaque",call.=FALSE)
  value
}
resolve_input <- function(value, fallback) {
  value <- trimws(as.character(value)); candidates <- if (nzchar(value)) c(value, file.path(script_dir, value)) else c(fallback, file.path(script_dir, fallback)); candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  hit <- candidates[file.exists(candidates)]; if (!length(hit)) stop("Input file does not exist: ", if (nzchar(value)) value else fallback, call. = FALSE); normalizePath(hit[[1L]], mustWork = TRUE)
}
read_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt", "md")) { first <- readLines(path, n = 1L, warn = FALSE); seps<-c("\t",";",",");counts<-vapply(seps,function(sep){con<-textConnection(first);on.exit(close(con));sum(utils::count.fields(con,sep=sep,quote="\"",comment.char=""),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for XLS/XLSX input", call. = FALSE)
    as.data.frame(readxl::read_excel(path, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  raw <- trimws(as.character(x)); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}

.registry_parameter_bindings <- list(cluster_k=gp("cluster_k",NULL), matrix_path = gp("matrix_path", NULL), data_path = gp("data_path", NULL), family = gp("family", NULL), base_size = gp("base_size", NULL), axis_text_size = gp("axis_text_size", NULL), line_width = gp("line_width", NULL), primary_color = gp("primary_color", NULL), accent_color = gp("accent_color", NULL), width_mm = gp("width_mm", NULL), height_mm = gp("height_mm", NULL), dpi = gp("dpi", NULL))


pick_column<-function(data,candidates,label){normalized<-tolower(gsub("[^a-zA-Z0-9]+","_",names(data)));hit<-which(normalized%in%candidates);if(length(hit)!=1)stop("Required unambiguous ",label," column",call.=FALSE);names(data)[hit]}
complete_table<-function(x){if(!nrow(x)||anyNA(names(x))||any(!nzchar(names(x)))||anyDuplicated(names(x)))stop("Complete unique table headers and rows required",call.=FALSE);x}
matrix_path<-resolve_input(text_param("matrix_path","assets/kappa_matrix.txt"),file.path(script_dir,"assets/kappa_matrix.txt"));matrix_raw<-complete_table(read_table(matrix_path));if(ncol(matrix_raw)<3)stop("At least two terms plus leading ID column required",call.=FALSE)
terms<-trimws(enc2utf8(as.character(matrix_raw[[1]])));cols<-names(matrix_raw)[-1]
if(nrow(matrix_raw)!=length(cols)||anyNA(terms)||any(!nzchar(terms))||anyDuplicated(terms)||!setequal(terms,cols))stop("Kappa requires square matrix with unique complete row/column term IDs matching by name",call.=FALSE)
matrix_values<-do.call(cbind,lapply(seq_along(cols),function(j)parse_numeric(matrix_raw[[j+1]],cols[j])));dimnames(matrix_values)<-list(terms,cols);matrix_values<-matrix_values[,match(terms,cols),drop=FALSE]
if(any(!is.finite(matrix_values))||any(matrix_values< -1|matrix_values>1))stop("Kappa must be finite and within [-1,1]; values not clamped",call.=FALSE)
if(max(abs(matrix_values-t(matrix_values)))>1e-8||max(abs(diag(matrix_values)-1))>1e-8)stop("Kappa must be symmetric with diagonal 1",call.=FALSE)
terms<-sort(terms,method="radix");matrix_values<-matrix_values[terms,terms,drop=FALSE];hc<-stats::hclust(stats::as.dist(1-matrix_values),method="average");term_order<-hc$labels[hc$order];n_terms<-length(terms);cluster_k<-integer_param("cluster_k",min(4L,n_terms),1,min(10L,n_terms));cluster<-stats::cutree(hc,k=cluster_k)
cluster_palette<-if(cluster_k<=4)c("#0072B2","#D55E00","#009E73","#CC79A7")[seq_len(cluster_k)]else grDevices::hcl.colors(cluster_k,"Dark 3");names(cluster_palette)<-as.character(seq_len(cluster_k))
data_path<-resolve_input(text_param("data_path","assets/easy_input.csv"),file.path(script_dir,"assets/easy_input.csv"));raw<-complete_table(read_table(data_path));annotation_col<-pick_column(raw,c("annotation_information","annotation","term","pathway","go_term"),"annotation term");pval_col<-pick_column(raw,c("adj_pval","adjusted_p_value","padj","qvalue","pvalue","p_value"),"supplied probability");z_col<-pick_column(raw,c("zscore","z_score","z"),"z-score")
stats_raw<-data.frame(term=trimws(enc2utf8(as.character(raw[[annotation_col]]))),p_value=parse_numeric(raw[[pval_col]],pval_col),zscore=parse_numeric(raw[[z_col]],z_col))
if(anyNA(stats_raw$term)||any(!nzchar(stats_raw$term))||any(!is.finite(stats_raw$p_value)|stats_raw$p_value<=0|stats_raw$p_value>1)||any(!is.finite(stats_raw$zscore)))stop("Complete terms, finite 0<p<=1 and finite z-scores required; zero needs unrounded probability",call.=FALSE)
if(!setequal(terms,stats_raw$term))stop("Statistics and kappa term sets must match, without silently dropping extras",call.=FALSE)
for(t in terms){s<-stats_raw[stats_raw$term==t,c("p_value","zscore"),drop=FALSE];if(nrow(unique(s))!=1)stop("Conflicting statistics for repeated term: ",t,call.=FALSE)}
stats_df<-stats_raw[match(term_order,stats_raw$term),,drop=FALSE];rownames(stats_df)<-NULL;stats_df$neglog10_p<- -log10(stats_df$p_value);stats_df$cluster<-unname(cluster[stats_df$term])
if(any(grepl("case[ _-]*[0-9]+|cover_[0-9]+|debug|案例[ ]*[0-9]+",c(terms,pval_col,z_col),ignore.case=TRUE))||anyNA(iconv(c(terms,pval_col,z_col),from="UTF-8",to="latin1"))||any(grepl("[\r\n]",terms)))stop("Rendered labels must be single Latin-1 lines and exclude internal identifiers",call.=FALSE)
font<-choice("family","Helvetica",c("Helvetica","sans"));base_size<-num("base_size",9.5,5,24);axis_size<-num("axis_text_size",8.5,4,18);line_width<-num("line_width",.30,.2,3);primary<-validate_colour("primary_color",text_param("primary_color","#0072B2"));accent<-validate_colour("accent_color",text_param("accent_color","#D55E00"));if(identical(unname(grDevices::col2rgb(primary)),unname(grDevices::col2rgb(accent))))stop("Summary colours must be distinct",call.=FALSE);width_mm<-num("width_mm",200,140,400);height_mm<-num("height_mm",180,100,400);dpi<-integer_param("dpi",600,96,1200)
left<-4;tree_width<-36;label_start<-left+tree_width+3;heat_z_x<-width_mm-4-14;heat_p_x<-heat_z_x-2-14;strip_x<-heat_p_x-3-4;label_end<-strip_x-3
row_top<-height_mm-26;row_bottom<-40;row_step<-(row_top-row_bottom)/max(1,n_terms-1);stats_df$y_mm<-row_top-(seq_len(n_terms)-1)*row_step
if(row_step<axis_size*25.4/72*1.25||line_width>row_step*.35||label_end-label_start<30)stop("Insufficient row/term space; enlarge canvas or reduce font/line width",call.=FALSE)
max_height<-max(hc$height);tree_scale_max<-if(max_height>0)max_height else 1
height_x<-function(h)left+tree_width*(1-h/tree_scale_max)
# Three genuine branches per merge; constructing them directly avoids ggdendro's implicit device.
node_y<-numeric(n_terms-1);seg_tree<-data.frame(x=numeric(),y=numeric(),xend=numeric(),yend=numeric(),merge=integer())
child<-function(j){if(j<0){term<-hc$labels[-j];list(y=stats_df$y_mm[match(term,stats_df$term)],height=0)}else list(y=node_y[j],height=hc$height[j])}
for(i in seq_len(n_terms-1)){a<-child(hc$merge[i,1]);b<-child(hc$merge[i,2]);node_y[i]<-(a$y+b$y)/2;xx<-height_x(hc$height[i]);seg_tree<-rbind(seg_tree,data.frame(x=c(xx,xx,xx),y=c(a$y,b$y,a$y),xend=c(height_x(a$height),height_x(b$height),xx),yend=c(a$y,b$y,b$y),merge=i))}
p_domain<-c(0,max(1,stats_df$neglog10_p));z_abs<-max(abs(stats_df$zscore));if(z_abs==0)z_abs<-1;z_domain<-c(-z_abs,z_abs)
p_color<-grDevices::colorRampPalette(c("#F7F7F7",accent))(101);z_color<-grDevices::colorRampPalette(c(primary,"#F7F7F7",accent))(201)
map_p<-function(v)p_color[1+round(v/p_domain[2]*100)];map_z<-function(v)z_color[1+round((.5+.5*(v/z_abs))*200)]
caption<-c(paste0(n_terms," supplied terms; ",nrow(raw)," annotation rows preserved; average linkage of 1-kappa; cut k=",cluster_k,"."),paste0("Tiles: -log10(",pval_col,") and ",z_col,"; repeated term statistics must agree; no new tests."),"Kappa provenance, adjusted-p method and z-score meaning unverified; groups are descriptive.")
primitives<-list();rectangles<-list();text_records<-list()
render_one<-function(kind){file<-file.path(output_dir,paste0("plot.",kind));if(kind=="png")ragg::agg_png(file,width=width_mm,height=height_mm,units="mm",res=dpi,background="white")else if(kind=="pdf")grDevices::pdf(file,width=width_mm/25.4,height=height_mm/25.4,family="Helvetica",useDingbats=FALSE,bg="white")else svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg="white")
 tryCatch({grid::grid.newpage()
 line<-function(x1,y1,x2,y2,col="#4D4D4D",lw=line_width){grid::grid.segments(x0=grid::unit(x1,"mm"),y0=grid::unit(y1,"mm"),x1=grid::unit(x2,"mm"),y1=grid::unit(y2,"mm"),gp=grid::gpar(col=col,lwd=lw*96/25.4,lineend="butt"));if(kind=="svg")primitives[[length(primitives)+1]]<<-list(x1=x1,y1=y1,x2=x2,y2=y2,col=col,width_mm=lw)}
 rect<-function(x,y,w,h,fill,role,term=""){grid::grid.rect(x=grid::unit(x,"mm"),y=grid::unit(y,"mm"),width=grid::unit(w,"mm"),height=grid::unit(h,"mm"),just=c(0,.5),gp=grid::gpar(fill=fill,col=NA));if(kind=="svg")rectangles[[length(rectangles)+1]]<<-list(x=x,y=y,width=w,height=h,fill=fill,role=role,term=term)}
 text<-function(label,x,y,fs=axis_size,bold=FALSE,hjust=0,col="#222222",available=width_mm-8){if(!nzchar(label))return();gp<-grid::gpar(fontfamily=font,fontsize=fs,fontface=if(bold)"bold"else"plain",col=col);grid::pushViewport(grid::viewport(gp=gp));tw<-grid::convertWidth(grid::stringWidth(label),"mm",valueOnly=TRUE);m<-grid::calcStringMetric("X");grid::upViewport();if(tw>available||x-hjust*tw<0||x+(1-hjust)*tw>width_mm)stop("Term/annotation text exceeds available width; enlarge canvas or reduce font",call.=FALSE);grid::grid.text(label,x=grid::unit(x,"mm"),y=grid::unit(y,"mm"),just=c(hjust,.5),gp=gp);if(kind=="svg")text_records[[length(text_records)+1]]<<-list(label=label,x=x,y=y,fontsize=fs,bold=bold,hjust=hjust,col=col,ascent=m$ascent*72,descent=m$descent*72)}
 text("Functional term clustering",left,height_mm-7,base_size+2,TRUE);text(paste0("Supplied kappa; average linkage; k=",cluster_k," descriptive groups"),left,height_mm-12,8)
 text("Annotation term",label_start,height_mm-20,8.5,TRUE,available=label_end-label_start);text(paste0("k=",cluster_k),strip_x+2,height_mm-20,7.5,hjust=.5,available=8);text(pval_col,heat_p_x+7,height_mm-20,7.5,TRUE,.5,available=22);text(z_col,heat_z_x+7,height_mm-20,7.5,TRUE,.5,available=22)
 for(i in seq_len(nrow(seg_tree))){s<-seg_tree[i,];line(s$x,s$y,s$xend,s$yend)}
 for(i in seq_len(n_terms)){s<-stats_df[i,];text(s$term,label_start,s$y_mm,available=label_end-label_start);rect(strip_x,s$y_mm,4,row_step*.88,cluster_palette[as.character(s$cluster)],"cluster",s$term);rect(heat_p_x,s$y_mm,14,row_step*.88,map_p(s$neglog10_p),"probability",s$term);rect(heat_z_x,s$y_mm,14,row_step*.88,map_z(s$zscore),"zscore",s$term)}
 ticks<-c(0,tree_scale_max/2,tree_scale_max);line(left,29,left+tree_width,29,lw=.25);for(t in ticks){xx<-height_x(t);line(xx,29,xx,28,lw=.25);text(formatC(t,format="g",digits=2),xx,25,7,hjust=.5,available=tree_width)};text("Merge height (1-kappa)",left+tree_width/2,20,7,hjust=.5,available=tree_width)
 # Vector legends share exactly the same discrete colour functions as data tiles.
 for(role in c("probability","zscore")){legend_width<-min(40,(width_mm-4-52-18)/2);start<-if(role=="probability")52 else 52+legend_width+18;domain<-if(role=="probability")p_domain else z_domain;fun<-if(role=="probability")map_p else map_z;text(if(role=="probability")paste0("-log10(",pval_col,")")else z_col,start,31,7.5,available=55);for(i in 0:79){v<-domain[1]*(1-i/79)+domain[2]*(i/79);rect(start+i*legend_width/80,23,legend_width/80,3,fun(v),paste0("legend-",role))};values<-c(domain[1],if(role=="probability")mean(domain)else 0,domain[2]);for(j in seq_along(values)){v<-values[j];xx<-start+(j-1)/2*legend_width;line(xx,21.5,xx,20.5,lw=.2);text(formatC(v,format="g",digits=2),xx,18.5,7,hjust=.5,available=20)}}
 for(j in seq_along(caption))text(caption[j],left,4+(3-j)*3.2,6.5)
 },finally=grDevices::dev.off())}
for(kind in c("png","pdf","svg"))render_one(kind)
for(pair in list(list("supplied-matrix",matrix_raw),list("supplied-annotations",raw),list("term-statistics",stats_df),list("tree-segments",seg_tree),list("cluster-assignments",data.frame(term=terms,cluster=unname(cluster[terms]),color=unname(cluster_palette[as.character(cluster[terms])])))))write.table(pair[[2]],file.path(output_dir,paste0(pair[[1]],".tsv")),sep="\t",quote=TRUE,qmethod="double",row.names=FALSE)
jsonlite::write_json(list(term_order=term_order,terms=n_terms,annotation_rows=nrow(raw),p_column=pval_col,z_column=z_col,linkage="average, not DAVID fuzzy clustering or source neighbor joining",cluster_k=cluster_k,kappa_range=range(matrix_values),kappa_symmetric_error=max(abs(matrix_values-t(matrix_values))),merge_heights=hc$height,merge=hc$merge,row_step_mm=row_step,tree_bounds_mm=c(left,left+tree_width),tree_scale_max=tree_scale_max,p_domain=p_domain,z_domain=z_domain,primitives=primitives,rectangles=rectangles,text_records=text_records,caption=caption),file.path(output_dir,"analysis.json"),auto_unbox=TRUE,digits=15)
cat(sprintf("Rendered %d aligned terms; %d original annotation rows; k=%d chosen cut; supplied statistics only\n",n_terms,nrow(raw),cluster_k))
