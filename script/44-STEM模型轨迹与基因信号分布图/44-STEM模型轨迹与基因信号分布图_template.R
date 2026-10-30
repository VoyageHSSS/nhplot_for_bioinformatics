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
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}











read_rep2<-function(path){canonical<-normalizePath(file.path(script_dir,'assets/g27_2.txt'));if(path!=canonical)return(read_table(path));lines<-readLines(path,warn=FALSE);fields<-utils::count.fields(path,sep='\t',quote='"',comment.char='',blank.lines.skip=TRUE);bad<-which(fields!=7);if(!identical(bad,as.integer(c(length(lines)-1,length(lines))))||!identical(lines[bad],c('24191\t\t-1.295','24192\t\t0.718')))stop('Unexpected malformed bundled source records',call.=FALSE);utils::read.delim(path,check.names=FALSE,stringsAsFactors=FALSE,colClasses='character',na.strings=character(),fill=TRUE)}
profile_path<-resolve_input(text_param('profile_path','',TRUE),'assets/defaultsGuilleminSample_profiletable.txt');gene_path<-resolve_input(text_param('gene_path','',TRUE),'assets/defaultsGuilleminSample_genetable.txt');rep1_path<-resolve_input(text_param('rep1_path','',TRUE),'assets/g27_1.txt');rep2_path<-resolve_input(text_param('rep2_path','',TRUE),'assets/g27_2.txt');raw<-list(profile=read_table(profile_path),genes=read_table(gene_path),rep1=read_table(rep1_path),rep2=read_rep2(rep2_path));profile<-raw$profile;genes<-raw$genes;reps<-list(raw$rep1,raw$rep2)
time_cols<-c('0h','0.5h','3h','6h','12h');required<-list(profile=c('Profile ID','Profile Model','Cluster (-1 non-significant)','# Genes Assigned','p-value'),genes=c('Gene Symbol','Profile',time_cols),rep1=c('Gene Symbol',time_cols),rep2=c('Gene Symbol',time_cols));for(n in names(required))if(!all(required[[n]]%in%names(raw[[n]]))||anyDuplicated(names(raw[[n]])))stop(n,' requires unique source fields and all five time columns',call.=FALSE)
complete_ids<-function(x,label){x<-trimws(as.character(x));if(anyNA(x)||any(!nzchar(x))||anyDuplicated(x)||any(grepl('[[:cntrl:]]',x)))stop(label,' must be unique complete text IDs',call.=FALSE);x};profile[['Profile ID']]<-complete_ids(profile[['Profile ID']],'Profile ID');genes[['Gene Symbol']]<-complete_ids(genes[['Gene Symbol']],'Gene Symbol');if(!all(genes$Profile%in%profile[['Profile ID']]))stop('Every gene profile must be listed',call.=FALSE)
cluster<-parse_numeric(profile[['Cluster (-1 non-significant)']],'cluster');assigned<-parse_numeric(profile[['# Genes Assigned']],'assigned');pv<-parse_numeric(profile[['p-value']],'profile p-value');if(any(!is.finite(c(cluster,assigned,pv)))||any(cluster!=floor(cluster))||any(cluster< -1)||any(assigned<0)||any(pv<0|pv>1))stop('Invalid source profile statistics',call.=FALSE)
models<-lapply(profile[['Profile Model']],function(z){v<-parse_numeric(strsplit(z,',',fixed=TRUE)[[1]],'model');if(length(v)!=5||any(!is.finite(v)))stop('Every model needs five finite values',call.=FALSE);v});for(t in time_cols){genes[[t]]<-parse_numeric(genes[[t]],paste('trajectory',t));if(any(!is.finite(genes[[t]])))stop('Gene trajectories must be complete finite values',call.=FALSE)}
# Missing replicate measurements are recorded, not interpreted as zero. Source matching stays by Gene Symbol.
for(k in seq_along(reps)){reps[[k]][['Gene Symbol']]<-trimws(reps[[k]][['Gene Symbol']]);if(anyNA(reps[[k]][['Gene Symbol']]))stop('Use explicit blank gene annotations, not NA IDs',call.=FALSE);for(t in time_cols){reps[[k]][[t]]<-parse_numeric(reps[[k]][[t]],paste('replicate',k,t));if(any(is.infinite(reps[[k]][[t]])))stop('Infinite replicate measurement',call.=FALSE)};reps[[k]]$replicate<-k}
if(bool('show_significance',FALSE))stop('Time-comparison significance is unavailable: no untested stars or invented tests',call.=FALSE)
max_profiles<-integer_param('max_profiles',10,2,20);sel<-which(cluster!=-1);if(length(sel)<2)stop('At least two source-selected profiles required',call.=FALSE);ends<-vapply(models,function(v)v[5],numeric(1));pid_numeric<-suppressWarnings(as.numeric(profile[['Profile ID']]));sel<-sel[order(ends[sel],if(all(is.finite(pid_numeric)))pid_numeric[sel]else profile[['Profile ID']][sel])];sel<-head(sel,max_profiles);sig<-profile[sel,,drop=FALSE];ids<-sig[['Profile ID']];allrep<-rbind(reps[[1]],reps[[2]]);matched_names<-intersect(genes[['Gene Symbol']],unique(allrep[['Gene Symbol']][nzchar(allrep[['Gene Symbol']])]))
med_parts<-lapply(matched_names,function(g){d<-allrep[allrep[['Gene Symbol']]==g,,drop=FALSE];do.call(rbind,lapply(time_cols,function(t){v<-d[[t]];v<-v[!is.na(v)];data.frame(gene=g,time=t,value=if(length(v))median(v)else NA_real_,n_measurements=length(v),n_missing=sum(is.na(d[[t]])),n_rep1=sum(!is.na(d[[t]])&d$replicate==1),n_rep2=sum(!is.na(d[[t]])&d$replicate==2))}))});if(!length(med_parts))stop('No source gene symbols match replicate matrices',call.=FALSE);med<-do.call(rbind,med_parts);if(any(!is.finite(med$value)))stop('At least one matched gene/time has no measurement; do not silently discard',call.=FALSE)
line_df<-do.call(rbind,lapply(ids,function(pid){d<-genes[genes$Profile==pid,,drop=FALSE];if(!nrow(d))stop('Selected profile has no actual trajectories',call.=FALSE);do.call(rbind,lapply(time_cols,function(t)data.frame(profile=pid,gene=d[['Gene Symbol']],time=t,value=d[[t]])))}));model_df<-do.call(rbind,lapply(sel,function(i)data.frame(profile=profile[['Profile ID']][i],time=time_cols,value=models[[i]])));box_df<-do.call(rbind,lapply(ids,function(pid){gn<-genes[['Gene Symbol']][genes$Profile==pid];d<-med[med$gene%in%gn,,drop=FALSE];if(!nrow(d))stop('Selected profile has no matched signal',call.=FALSE);d$profile<-pid;d}));omitted<-genes[!genes[['Gene Symbol']]%in%matched_names,c('Gene Symbol','Profile'),drop=FALSE];omitted$reason<-'No exact annotated Gene Symbol in the supplied replicate matrices; no guessed SPOT mapping'
summary<-data.frame(profile=ids,source_assigned=assigned[sel],trajectory_n=vapply(ids,function(p)sum(genes$Profile==p),integer(1)),signal_gene_n=vapply(ids,function(p)length(unique(box_df$gene[box_df$profile==p])),integer(1)),source_cluster=cluster[sel],source_profile_p=pv[sel]);summary$omitted_signal_n<-summary$trajectory_n-summary$signal_gene_n
box_stats<-do.call(rbind,lapply(ids,function(pid)do.call(rbind,lapply(time_cols,function(t){v<-box_df$value[box_df$profile==pid&box_df$time==t];q<-quantile(v,c(.25,.5,.75),type=7,names=FALSE);inside<-v[v>=q[1]-1.5*(q[3]-q[1])&v<=q[3]+1.5*(q[3]-q[1])];data.frame(profile=pid,time=t,n=length(v),q1=q[1],median=q[2],q3=q[3],low=min(inside),high=max(inside))}))));outliers<-box_df[FALSE,,drop=FALSE];for(i in seq_len(nrow(box_stats))){b<-box_stats[i,];d<-box_df[box_df$profile==b$profile&box_df$time==b$time,,drop=FALSE];outliers<-rbind(outliers,d[d$value<b$low|d$value>b$high,,drop=FALSE])}
font_family<-choice('font_family','Arial',c('Arial','Helvetica','Times New Roman'));family<-if(font_family=='Times New Roman')'serif'else'sans';line_width<-num('line_width',.14,.05,.4);line_alpha<-num('line_alpha',.38,.1,.8);box_linewidth<-num('box_linewidth',.25,.1,.5);model_linewidth<-num('model_linewidth',.45,.2,.8);width_mm<-num('width_mm',300,280,360);height_mm<-num('height_mm',220,140,400);dpi<-integer_param('dpi',600,96,1200)
line_range<-range(c(line_df$value,model_df$value));line_range<-c(floor(line_range[1]-.5),ceiling(line_range[2]+.5));box_range<-range(box_df$value);box_range<-c(floor(box_range[1]-.5),ceiling(box_range[2]+.5));if(any(!is.finite(c(line_range,box_range))))stop('Plot range overflow',call.=FALSE);line_breaks<-pretty(line_range,n=3);line_breaks<-line_breaks[line_breaks>=line_range[1]&line_breaks<=line_range[2]];box_breaks<-pretty(box_range,n=3);box_breaks<-box_breaks[box_breaks>=box_range[1]&box_breaks<=box_range[2]]
nrows<-ceiling(length(ids)/2);pitch<-(height_mm-47)/nrows;panel_h<-pitch-4;if(panel_h<20)stop('Rows too short; increase height or reduce displayed profiles',call.=FALSE);xscale<-width_mm/300;positions<-do.call(rbind,lapply(seq_along(ids),function(i){block<-(i-1)%/%nrows;row<-(i-1)%%nrows;origin<-(7+150*block)*xscale;data.frame(profile=ids[i],block=block,row=row,origin=origin,line_left=origin+25*xscale,box_left=origin+77*xscale,panel_width=35*xscale,bottom=height_mm-29-(row+1)*pitch+4,panel_height=panel_h,gene_x=origin+117*xscale)}));caption<-c('Red: STEM model (not a representative gene); gray: supplied transformed trajectories; time points are equally spaced categories.','Boxes: per-gene median across annotated spots and two supplied matrices; outliers shown; nT/nB count trajectory/signal genes, not independent replicates.','Exact Gene Symbol matching only; unmatched identifiers and missing measurements are exported. No time-comparison significance is asserted.')
probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE);tryCatch({grid::grid.newpage();tw<-function(z,size)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=size))),'mm',TRUE);if(any(vapply(caption,tw,numeric(1),size=6)>width_mm-12))stop('Caption exceeds canvas',call.=FALSE);for(pid in ids){gg<-head(genes[['Gene Symbol']][genes$Profile==pid],4);if(any(vapply(gg,tw,numeric(1),size=6.5)>26*xscale))stop('Example gene identifier too wide; enlarge canvas',call.=FALSE)}},finally={dev.off();unlink(probe)})
colors<-setNames(c('#66C2A5','#FC8D62','#8DA0CB','#E78AC3','#A6D854'),time_cols);mm<-function(z)grid::unit(z,'mm');draw<-function(){grid::grid.newpage();txt<-function(z,x,y,size=7,face='plain',just='center',rot=0)grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face,col='#25313C'));ln<-function(x,y,col='#69737D',lwd=.5,alpha=1)grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=grDevices::adjustcolor(col,alpha.f=alpha),lwd=lwd));rect<-function(l,r,b,t,fill=NA,lwd=.5)grid::grid.rect(mm((l+r)/2),mm((b+t)/2),mm(r-l),mm(t-b),gp=grid::gpar(fill=fill,col='#69737D',lwd=lwd));txt('STEM profiles and supplied signal distributions',6,height_mm-6,11,'bold','left');txt(sprintf('%d source-selected profiles; %d trajectory identifiers; %d matched signal genes; no time-comparison tests',length(ids),sum(summary$trajectory_n),sum(summary$signal_gene_n)),6,height_mm-12,7,just='left')
for(block in 0:1){o<-(7+150*block)*xscale;txt(c('A','B')[block+1],o,height_mm-22,10,'bold','left');txt('Transformed trajectories',o+42.5*xscale,height_mm-22,7,'bold');txt('Supplied median signal',o+94.5*xscale,height_mm-22,7,'bold');txt('Example IDs',o+130*xscale,height_mm-22,6.5,'bold')}
for(i in seq_len(nrow(positions))){p<-positions[i,];pid<-p$profile;ss<-summary[summary$profile==pid,];b<-p$bottom;t<-b+p$panel_height;yl<-function(v)b+(v-line_range[1])/diff(line_range)*p$panel_height;yb<-function(v)b+(v-box_range[1])/diff(box_range)*p$panel_height;xs<-function(j,left)left+(.08+.84*(j-1)/4)*p$panel_width;txt(paste0('P',pid),p$origin+3*xscale,(b+t)/2+4,7,'bold');txt(paste0('nT=',ss$trajectory_n),p$origin+3*xscale,(b+t)/2,6.2);txt(paste0('nB=',ss$signal_gene_n),p$origin+3*xscale,(b+t)/2-4,6.2);rect(p$line_left,p$line_left+p$panel_width,b,t);rect(p$box_left,p$box_left+p$panel_width,b,t);txt('STEM value',p$line_left-10*xscale,(b+t)/2,6.3,rot=90);txt('Supplied signal',p$box_left-10*xscale,(b+t)/2,6.3,rot=90)
for(v in line_breaks){ln(c(p$line_left-1,p$line_left),c(yl(v),yl(v)));txt(format(v,trim=TRUE),p$line_left-2,yl(v),5.8,just='right')};for(v in box_breaks){ln(c(p$box_left-1,p$box_left),c(yb(v),yb(v)));txt(format(v,trim=TRUE),p$box_left-2,yb(v),5.8,just='right')}
gg<-line_df[line_df$profile==pid,];for(gene in unique(gg$gene)){d<-gg[gg$gene==gene,];d<-d[match(time_cols,d$time),];ln(xs(1:5,p$line_left),yl(d$value),'#A9AFB6',line_width*72.27/25.4,line_alpha)};md<-model_df[model_df$profile==pid,];ln(xs(1:5,p$line_left),yl(md$value),'#E31A1C',model_linewidth*72.27/25.4)
for(j in 1:5){bs<-box_stats[box_stats$profile==pid&box_stats$time==time_cols[j],];x<-xs(j,p$box_left);hw<-p$panel_width*.06;ln(c(x,x),yb(c(bs$low,bs$high)),lwd=box_linewidth*72.27/25.4);for(v in c(bs$low,bs$high))ln(c(x-hw*.5,x+hw*.5),rep(yb(v),2),lwd=box_linewidth*72.27/25.4);rect(x-hw,x+hw,yb(bs$q1),yb(bs$q3),colors[j],box_linewidth*72.27/25.4);ln(c(x-hw,x+hw),rep(yb(bs$median),2),lwd=box_linewidth*1.5*72.27/25.4);oo<-outliers[outliers$profile==pid&outliers$time==time_cols[j],];if(nrow(oo))for(v in oo$value)grid::grid.circle(mm(x),mm(yb(v)),mm(.25),gp=grid::gpar(fill='#25313C',col=NA));for(l in c(p$line_left,p$box_left)){xx<-xs(j,l);ln(c(xx,xx),c(b,b-1));if(p$row==min(nrows-1,sum(positions$block==p$block)-1))txt(time_cols[j],xx,b-3.2,6)}}
examples<-head(genes[['Gene Symbol']][genes$Profile==pid],4);for(j in seq_along(examples))txt(examples[j],p$gene_x,(b+t)/2+(2.5-j)*3.6,6.5,just='left')};for(i in seq_along(caption))txt(caption[i],6,13-(i-1)*3.2,6,just='left')}
for(kind in c('png','pdf','svg')){path<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(path,width=width_mm,height=height_mm,units='mm',res=dpi,bg='white')else if(kind=='pdf')pdf(path,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE)else svglite::svglite(path,width=width_mm/25.4,height=height_mm/25.4);tryCatch(draw(),finally=dev.off())}
for(n in c('sig','summary','line_df','model_df','med','box_df','box_stats','outliers','omitted','positions'))utils::write.csv(get(n),file.path(output_dir,paste0(n,'.csv')),row.names=FALSE);utils::write.csv(sig,file.path(output_dir,'selected_profiles.csv'),row.names=FALSE);utils::write.csv(line_df,file.path(output_dir,'profile_lines.csv'),row.names=FALSE);utils::write.csv(box_df,file.path(output_dir,'profile_box_values.csv'),row.names=FALSE);saveRDS(list(raw=raw,profile=profile,genes=genes,reps=reps,selected=sig,summary=summary,lines=line_df,models=model_df,medians=med,boxes=box_df,box_stats=box_stats,outliers=outliers,omitted=omitted,positions=positions),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(profile_ids=ids,n_profiles=length(ids),n_genes=nrow(genes),n_replicate_rows=nrow(allrep),n_matrices=2,bundled_rep2_short_records=if(rep2_path==normalizePath(file.path(script_dir,'assets/g27_2.txt')))c('24191','24192')else character(),timepoints=time_cols,time_spacing='ordinal equally spaced categories',line_range=line_range,box_range=box_range,line_width_mm=line_width,line_alpha=line_alpha,model_linewidth_mm=model_linewidth,box_linewidth_mm=box_linewidth,n_outliers=nrow(outliers),unmatched_signal_identifiers=nrow(omitted),missing_matched_measurements=sum(med$n_missing),width_mm=width_mm,height_mm=height_mm,source_profile_significance='Provided cluster != -1; not recomputed',time_comparison_significance='None; no tests or stars',signal_summary='Per-gene pooled spot/matrix median, not independent replicate distribution',caption=caption),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',length(ids),'source-selected profiles; no untested significance\n')
