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


.registry_parameter_bindings<-list(data_path=gp("data_path",NULL),enrichment_path=gp("enrichment_path",NULL),family=gp("family",NULL),base_size=gp("base_size",NULL),axis_text_size=gp("axis_text_size",NULL),node_width=gp("node_width",NULL),flow_alpha=gp("flow_alpha",NULL),primary_color=gp("primary_color",NULL),accent_color=gp("accent_color",NULL),width_mm=gp("width_mm",NULL),height_mm=gp("height_mm",NULL),dpi=gp("dpi",NULL))
pick_column<-function(data,candidates,label){normalized<-tolower(gsub("[^a-zA-Z0-9]+","_",names(data)));hit<-which(normalized%in%candidates);if(length(hit)!=1)stop("Required unambiguous ",label," column",call.=FALSE);names(data)[hit]}
complete_table<-function(x){if(!nrow(x)||anyNA(names(x))||any(!nzchar(names(x)))||anyDuplicated(names(x)))stop("Complete unique table headers and rows required",call.=FALSE);x}
data_path<-resolve_input(text_param("data_path","assets/df.csv"),file.path(script_dir,"assets/df.csv"));raw_links<-complete_table(read_table(data_path));gene_col<-pick_column(raw_links,c("gene","gene_id","symbol"),"gene");path_col<-pick_column(raw_links,c("pathway","term","annotation","description"),"pathway")
links<-data.frame(source_row=seq_len(nrow(raw_links)),gene=trimws(as.character(raw_links[[gene_col]])),pathway=trimws(as.character(raw_links[[path_col]])),stringsAsFactors=FALSE)
if(anyNA(links$gene)||anyNA(links$pathway)||any(!nzchar(links$gene))||any(!nzchar(links$pathway)))stop("Complete gene and pathway IDs required; rows are not filtered",call.=FALSE)
# Each original relation row is one association, as in the original make_long implementation.
# Non-unit frequencies require a weighted-flow contract, rather than silently ignoring them.
if("freq"%in%tolower(names(raw_links))){freq<-parse_numeric(raw_links[[which(tolower(names(raw_links))=="freq")]],"freq");if(any(!is.finite(freq)|freq!=1))stop("Unweighted relation rows require freq=1; weighted frequencies are not supported",call.=FALSE)}
enrichment_path<-resolve_input(text_param("enrichment_path","assets/Enrichment.csv"),file.path(script_dir,"assets/Enrichment.csv"));raw_enrichment<-complete_table(read_table(enrichment_path));description_col<-pick_column(raw_enrichment,c("description","pathway","term"),"enrichment description");ratio_col<-pick_column(raw_enrichment,c("generatio","gene_ratio","ratio","gene_fraction"),"gene ratio");q_col<-pick_column(raw_enrichment,c("log_q_value_","log_q_value","neglog_q","logq","qvalue","q_value"),"q-value encoding");count_col<-pick_column(raw_enrichment,c("count","gene_count","n"),"gene count")
stats_df<-data.frame(pathway=trimws(as.character(raw_enrichment[[description_col]])),ratio=parse_numeric(raw_enrichment[[ratio_col]],ratio_col),q_input=parse_numeric(raw_enrichment[[q_col]],q_col),count=parse_numeric(raw_enrichment[[count_col]],count_col),stringsAsFactors=FALSE)
if(anyNA(stats_df$pathway)||any(!nzchar(stats_df$pathway))||anyDuplicated(stats_df$pathway)||any(!is.finite(stats_df$ratio)|stats_df$ratio<0|stats_df$ratio>1)||any(!is.finite(stats_df$q_input))||any(!is.finite(stats_df$count)|stats_df$count<=0|stats_df$count!=floor(stats_df$count)))stop("Unique complete pathways, finite ratio [0,1], finite q field and positive integer Count required; no filtering/deduplication",call.=FALSE)
q_key<-tolower(gsub("[^a-zA-Z0-9]+","_",q_col));q_encoding<-if(q_key=="neglog_q")"negative-log10-q"else if(q_key%in%c("qvalue","q_value"))"probability-q"else"log10-q"
if(q_encoding=="log10-q"){if(any(stats_df$q_input>0))stop("Log(q-value)/logq means log10(q)<=0; use neglog_q for already negative-log10 values",call.=FALSE);stats_df$neglog_q<- -stats_df$q_input}else if(q_encoding=="negative-log10-q"){if(any(stats_df$q_input<0))stop("neglog_q must be nonnegative",call.=FALSE);stats_df$neglog_q<-stats_df$q_input}else{if(any(stats_df$q_input<=0|stats_df$q_input>1))stop("qvalue/q_value must satisfy 0<q<=1; zero needs unrounded probability",call.=FALSE);stats_df$neglog_q<- -log10(stats_df$q_input)}
path_levels<-stats_df$pathway;if(length(setdiff(links$pathway,path_levels)))stop("Every linked pathway needs a matching enrichment row",call.=FALSE)
links<-links[order(match(links$pathway,path_levels),links$gene,links$source_row,method="radix"),,drop=FALSE];rownames(links)<-NULL;gene_levels<-unique(links$gene);n_genes<-length(gene_levels);n_paths<-length(path_levels)
labels<-c(gene_levels,path_levels);if(anyNA(iconv(labels,from="UTF-8",to="latin1"))||any(grepl("[\r\n]|case[ _-]*[0-9]+|cover_[0-9]+|debug|案例[ ]*[0-9]+",labels,ignore.case=TRUE)))stop("Labels must be single Latin-1 lines without internal identifiers",call.=FALSE)
font<-choice("family","Helvetica",c("Helvetica","sans"));base_size<-num("base_size",9.5,5,24);axis_size<-num("axis_text_size",8,4,18);node_width<-num("node_width",.14,.04,.4);flow_alpha<-num("flow_alpha",.72,.1,1);primary<-validate_colour("primary_color",text_param("primary_color","#0072B2"));accent<-validate_colour("accent_color",text_param("accent_color","#D55E00"));if(identical(unname(col2rgb(primary)),unname(col2rgb(accent))))stop("Summary colours must be distinct",call.=FALSE);width_mm<-num("width_mm",260,160,420);height_mm<-num("height_mm",180,100,360);dpi<-integer_param("dpi",600,96,1200)
row_top<-height_mm-27;row_bottom<-40;span<-row_top-row_bottom;row_step<-span/max(1,n_paths-1);stats_df$y_mm<-if(n_paths==1)mean(c(row_top,row_bottom))else row_top-(seq_len(n_paths)-1)*row_step
stats_df$association_rows<-as.integer(table(factor(links$pathway,levels=path_levels)));gene_degree<-as.integer(table(factor(links$gene,levels=gene_levels)))
label_height<-axis_size*25.4/72*1.20
if(span<=0||row_step<label_height+1)stop("Insufficient pathway row space; enlarge canvas or reduce font",call.=FALSE)
unit_thickness<-min(1.2,(span-max(0,n_genes-1)*label_height*.70)/nrow(links),row_step*.80/max(stats_df$association_rows))
gene_gap<-if(n_genes>1)(span-nrow(links)*unit_thickness)/(n_genes-1)else 0
if(unit_thickness<=.15||(n_genes>1&&min(gene_degree)*unit_thickness+gene_gap<label_height))stop("Insufficient gene/flow spacing; enlarge canvas or reduce font",call.=FALSE)
gene_nodes<-data.frame(gene=gene_levels,association_rows=gene_degree,height_mm=gene_degree*unit_thickness,y_mm=0)
cursor<-row_top;for(i in seq_len(n_genes)){gene_nodes$y_mm[i]<-if(n_genes==1)mean(c(row_top,row_bottom))else cursor-gene_nodes$height_mm[i]/2;cursor<-cursor-gene_nodes$height_mm[i]-gene_gap}
# Stage-specific IDs avoid conflating a gene symbol with an identically named pathway.
gene_x<-29;path_x<-82;node_mm<-node_width*(path_x-gene_x);flow_x1<-gene_x+node_mm/2;flow_x2<-path_x-node_mm/2;term_x<-path_x+node_mm/2+3;bubble_left<-width_mm-61;bubble_right<-width_mm-5;term_end<-bubble_left-4
if(term_end-term_x<35)stop("Insufficient pathway label and bubble columns; enlarge width",call.=FALSE)
links$gene_top_mm<-NA_real_;links$path_top_mm<-NA_real_;links$gene_bottom_mm<-NA_real_;links$path_bottom_mm<-NA_real_
for(g in gene_levels){idx<-which(links$gene==g);node<-gene_nodes[gene_nodes$gene==g,];links$gene_top_mm[idx]<-node$y_mm+node$height_mm/2-(seq_along(idx)-1)*unit_thickness}
for(p in path_levels){idx<-which(links$pathway==p);s<-stats_df[stats_df$pathway==p,];links$path_top_mm[idx]<-s$y_mm+length(idx)*unit_thickness/2-(seq_along(idx)-1)*unit_thickness}
links$gene_bottom_mm<-links$gene_top_mm-unit_thickness;links$path_bottom_mm<-links$path_top_mm-unit_thickness
path_palette<-setNames(grDevices::hcl.colors(n_paths,"Dark 3"),sort(path_levels,method="radix"));q_domain<-range(stats_df$neglog_q);if(diff(q_domain)==0)q_domain<-c(0,max(1,q_domain[2]));q_palette<-grDevices::colorRampPalette(c(primary,accent))(101);map_q<-function(v)q_palette[1+round((v/q_domain[2]-q_domain[1]/q_domain[2])/(1-q_domain[1]/q_domain[2])*100)]
ratio_domain<-range(stats_df$ratio);if(diff(ratio_domain)==0)ratio_domain<-c(0,1);bubble_radius<-min(2.7,row_step*.28);bubble_x0<-bubble_left+bubble_radius+1;bubble_x1<-bubble_right-bubble_radius-1;ratio_x<-function(v)bubble_x0+(v-ratio_domain[1])/diff(ratio_domain)*(bubble_x1-bubble_x0);stats_df$x_mm<-ratio_x(stats_df$ratio);stats_df$radius_mm<-bubble_radius*sqrt(stats_df$count/max(stats_df$count));stats_df$color<-map_q(stats_df$neglog_q)
caption<-c(paste0(nrow(links)," unit relation rows; ",n_genes," genes; ",n_paths," pathways. Flow width = supplied association rows."),"Bubble area = supplied Count; Gene ratio and q retained. Connection counts need not equal enrichment Count.","Enrichment background, ratio denominator and q-adjustment method unverified; no new enrichment tests.")
primitives<-list();rectangles<-list();circles<-list();polygons<-list();text_records<-list()
render_one<-function(kind){file<-file.path(output_dir,paste0("plot.",kind));if(kind=="png")ragg::agg_png(file,width=width_mm,height=height_mm,units="mm",res=dpi,background="white")else if(kind=="pdf")grDevices::pdf(file,width=width_mm/25.4,height=height_mm/25.4,family="Helvetica",useDingbats=FALSE,bg="white")else svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg="white")
tryCatch({grid::grid.newpage()
line<-function(x1,y1,x2,y2,col="#555555",lw=.25){grid::grid.segments(x0=grid::unit(x1,"mm"),y0=grid::unit(y1,"mm"),x1=grid::unit(x2,"mm"),y1=grid::unit(y2,"mm"),gp=grid::gpar(col=col,lwd=lw*96/25.4,lineend="butt"));if(kind=="svg")primitives[[length(primitives)+1]]<<-list(x1=x1,y1=y1,x2=x2,y2=y2,col=col,width_mm=lw)}
rect<-function(x,y,w,h,fill,role,label=""){if(h==0)return();grid::grid.rect(x=grid::unit(x,"mm"),y=grid::unit(y,"mm"),width=grid::unit(w,"mm"),height=grid::unit(h,"mm"),just=c(0,.5),gp=grid::gpar(fill=fill,col=NA));if(kind=="svg")rectangles[[length(rectangles)+1]]<<-list(x=x,y=y,width=w,height=h,fill=fill,role=role,label=label)}
circle<-function(x,y,r,fill,role,label=""){grid::grid.circle(x=grid::unit(x,"mm"),y=grid::unit(y,"mm"),r=grid::unit(r,"mm"),gp=grid::gpar(fill=fill,col=NA));if(kind=="svg")circles[[length(circles)+1]]<<-list(x=x,y=y,radius=r,fill=fill,role=role,label=label)}
 text<-function(label,x,y,fs=axis_size,bold=FALSE,hjust=0,col="#222222",available=width_mm-8){if(!nzchar(label))return();gp<-grid::gpar(fontfamily=font,fontsize=fs,fontface=if(bold)"bold"else"plain",col=col);grid::pushViewport(grid::viewport(gp=gp));tw<-grid::convertWidth(grid::stringWidth(label),"mm",valueOnly=TRUE);m<-grid::calcStringMetric("X");grid::upViewport();if(tw>available||x-hjust*tw<0||x+(1-hjust)*tw>width_mm)stop("Term/annotation text exceeds available width; enlarge canvas or reduce font",call.=FALSE);grid::grid.text(label,x=grid::unit(x,"mm"),y=grid::unit(y,"mm"),just=c(hjust,.5),gp=gp);if(kind=="svg")text_records[[length(text_records)+1]]<<-list(label=label,x=x,y=y,fontsize=fs,bold=bold,hjust=hjust,col=col,ascent=m$ascent*72,descent=m$descent*72)}

text("Functional enrichment overview",4,height_mm-7,base_size+2,TRUE);text("Unit gene-pathway links and supplied enrichment statistics",4,height_mm-12,8)
text("Genes",gene_x-node_mm/2-2,height_mm-20,base_size,TRUE,1,available=25);text("Gene-pathway links",(gene_x+path_x)/2,height_mm-20,8,TRUE,.5,available=55);text("Pathway",term_x,height_mm-20,base_size,TRUE,available=term_end-term_x);text("Gene ratio",(bubble_left+bubble_right)/2,height_mm-20,base_size,TRUE,.5,available=55)
ticks<-unique(c(ratio_domain[1],pretty(ratio_domain,3),ratio_domain[2]));ticks<-sort(ticks[ticks>=ratio_domain[1]&ticks<=ratio_domain[2]]);if(length(ticks)>1&&any(diff(ticks)*100<1))ticks<-ratio_domain
for(t in ticks){xx<-ratio_x(t);line(xx,row_bottom-4,xx,row_top+bubble_radius,col="#E5E7EB",lw=.15);line(xx,row_bottom-4,xx,row_bottom-5,lw=.25);text(paste0(formatC(t*100,format="fg",digits=3),"%"),xx,row_bottom-8,7,hjust=.5,available=16)};line(bubble_x0,row_bottom-4,bubble_x1,row_bottom-4)
# ponytail: fixed 81 samples approximate cubic outlines; increase only for demonstrable export artefacts.
t<-seq(0,1,length.out=81);bezier_x<-(1-t)^3*flow_x1+3*(1-t)^2*t*(flow_x1+.45*(flow_x2-flow_x1))+3*(1-t)*t^2*(flow_x2-.45*(flow_x2-flow_x1))+t^3*flow_x2
for(i in seq_len(nrow(links))){r<-links[i,];top<-(1-t)^2*(1+2*t)*r$gene_top_mm+t^2*(3-2*t)*r$path_top_mm;bottom<-(1-t)^2*(1+2*t)*r$gene_bottom_mm+t^2*(3-2*t)*r$path_bottom_mm;xx<-c(bezier_x,rev(bezier_x));yy<-c(top,rev(bottom));grid::grid.polygon(x=grid::unit(xx,"mm"),y=grid::unit(yy,"mm"),gp=grid::gpar(fill="#AEB8C4",col=NA,alpha=flow_alpha));if(kind=="svg")polygons[[length(polygons)+1]]<<-list(source_row=r$source_row,gene=r$gene,pathway=r$pathway,x=xx,y=yy,fill="#AEB8C4",alpha=flow_alpha)}
for(i in seq_len(n_genes)){r<-gene_nodes[i,];rect(gene_x-node_mm/2,r$y_mm,node_mm,r$height_mm,"#CBD5E1","gene",r$gene);text(r$gene,gene_x-node_mm/2-2,r$y_mm,hjust=1,available=gene_x-node_mm/2-4)}
for(i in seq_len(n_paths)){r<-stats_df[i,];rect(path_x-node_mm/2,r$y_mm,node_mm,r$association_rows*unit_thickness,path_palette[r$pathway],"pathway",r$pathway);text(r$pathway,term_x,r$y_mm,available=term_end-term_x);circle(r$x_mm,r$y_mm,r$radius_mm,r$color,"enrichment",r$pathway)}
legend_x<-term_x;legend_w<-48;legend_y<-22;text("-log10(q)",legend_x,29,7.5,TRUE,available=50)
for(i in 0:79){v<-q_domain[1]*(1-i/79)+q_domain[2]*(i/79);rect(legend_x+i*legend_w/80,legend_y,legend_w/80,3,map_q(v),"q-legend")};for(j in 1:3){xx<-legend_x+(j-1)/2*legend_w;v<-q_domain[1]*(1-(j-1)/2)+q_domain[2]*(j-1)/2;line(xx,20.5,xx,19.5,lw=.2);text(formatC(v,format="g",digits=3),xx,17,7,hjust=.5,available=18)}
count_x<-bubble_left;text("Count (bubble area)",count_x,29,7.5,TRUE,available=55);count_values<-sort(unique(c(min(stats_df$count),round(.5*min(stats_df$count)+.5*max(stats_df$count)),max(stats_df$count))));for(j in seq_along(count_values)){v<-count_values[j];xx<-count_x+6+(j-1)*18;circle(xx,22,bubble_radius*sqrt(v/max(stats_df$count)),"#777777","count-legend",as.character(v));text(formatC(v,format="g",digits=3),xx,17,7,hjust=.5,available=16)}
for(j in seq_along(caption))text(caption[j],4,4+(3-j)*3.2,6.5)
},finally=grDevices::dev.off())}
for(kind in c("png","pdf","svg"))render_one(kind)
for(pair in list(list("supplied-relations",raw_links),list("supplied-enrichment",raw_enrichment),list("association-flows",links),list("gene-nodes",gene_nodes),list("enrichment-statistics",stats_df)))write.table(pair[[2]],file.path(output_dir,paste0(pair[[1]],".tsv")),sep="\t",quote=TRUE,qmethod="double",row.names=FALSE)
jsonlite::write_json(list(link_rows=nrow(links),genes=n_genes,pathways=n_paths,q_column=q_col,q_encoding=q_encoding,q_domain=q_domain,ratio_domain=ratio_domain,unit_thickness_mm=unit_thickness,gene_gap_mm=gene_gap,row_step_mm=row_step,gene_x=gene_x,path_x=path_x,node_width_mm=node_mm,flow_x1=flow_x1,flow_x2=flow_x2,caption=caption,primitives=primitives,rectangles=rectangles,circles=circles,polygons=polygons,text_records=text_records),file.path(output_dir,"analysis.json"),auto_unbox=TRUE,digits=15)
cat(sprintf("Rendered %d real unit flows, %d genes and %d row-aligned enrichment pathways\n",nrow(links),n_genes,n_paths))
