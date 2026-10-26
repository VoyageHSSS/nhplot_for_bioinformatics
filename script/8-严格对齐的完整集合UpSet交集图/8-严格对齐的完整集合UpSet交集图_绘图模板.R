#!/usr/bin/env Rscript

# Exact exclusive memberships and aligned UpSet panels from supplied lists.
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
required_packages <- c("jsonlite", "svglite", "ragg", "showtext", "sysfonts", "systemfonts")
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

keys<-c('BLCA','ESCA','LIHC','MESO','PCPG','TGCT','UVM');requested<-gp('set_files',NULL);if(is.character(requested)&&length(requested)==1&&grepl('^\\s*\\[',requested))requested<-jsonlite::fromJSON(requested);if(is.null(requested)||identical(requested,'')){source_paths<-vapply(keys,function(k)resolve_input('',paste0('assets/',k,'.txt')),character(1));names(source_paths)<-keys}else{if(is.list(requested))requested<-unlist(requested,use.names=FALSE);if(!is.character(requested)||anyNA(requested)||length(requested)<2||length(requested)>10||any(!nzchar(trimws(requested))))stop('set_files needs 2–10 complete paths; no silent fallback',call.=FALSE);source_paths<-vapply(requested,function(x)resolve_input(x,''),character(1));if(anyDuplicated(source_paths))stop('Each set file must be distinct',call.=FALSE);keys<-tools::file_path_sans_ext(basename(source_paths));if(anyDuplicated(keys))stop('Distinct filenames/set labels required',call.=FALSE);names(source_paths)<-keys};read_set<-function(path){ext<-tolower(tools::file_ext(path));v<-if(ext=='rds')readRDS(path)else if(ext%in%c('txt','tsv'))readLines(path,warn=FALSE,encoding='UTF-8')else stop('Headerless one-ID-per-line TXT/TSV or character RDS required',call.=FALSE);if(!is.character(v)||anyNA(v)||any(grepl('[\t\r\n,]',v)))stop('Complete single-ID character vectors required',call.=FALSE);if(length(v))v[1]<-sub('^\ufeff','',v[1]);v};raw_sets<-lapply(source_paths,read_set);sets<-lapply(raw_sets,function(v)unique(trimws(v)[nzchar(trimws(v))]));union_ids<-sort(unique(unlist(sets,use.names=FALSE)),method='radix');if(!length(union_ids))stop('Union must contain IDs',call.=FALSE);membership<-sapply(sets,function(v)union_ids%in%v);combos<-apply(membership,1,function(v)paste0(as.integer(v),collapse=''));freq<-table(combos);all_patterns<-names(freq);degree<-vapply(strsplit(all_patterns,'',fixed=TRUE),function(v)sum(v=='1'),integer(1));order_by<-choice('order_by','freq',c('freq','degree'));ord<-if(order_by=='degree')order(-degree,-as.integer(freq),all_patterns)else order(-as.integer(freq),all_patterns);max_intersections<-integer_param('max_intersections',50,1,100);selected<-all_patterns[head(ord,max_intersections)];counts<-as.integer(freq[selected]);n<-length(selected);sizes<-lengths(sets);set_order<-order(sizes,keys);ordered_keys<-keys[set_order];ns<-length(keys);labels<-gp('set_labels',NULL);if(is.null(labels)||identical(labels,''))labels<-ordered_keys;if(is.list(labels))labels<-unlist(labels,use.names=FALSE);if(is.character(labels)&&length(labels)==1&&ns>1)labels<-strsplit(labels,',',fixed=TRUE)[[1]];if(!is.character(labels)||length(labels)!=ns||anyNA(labels)||any(!nzchar(trimws(labels)))||anyDuplicated(trimws(labels)))stop('Distinct labels required in ascending set-size order',call.=FALSE);labels<-trimws(labels);if(any(grepl('Case[ _:#-]*[0-9]|cover_[0-9]|preview\\.png',labels,ignore.case=TRUE)))stop('Internal/debug labels prohibited',call.=FALSE)
width_mm<-num('width_mm',250,245,300);height_mm<-num('height_mm',155,150,200);dpi<-integer_param('dpi',600,96,1200);base_size<-num('base_size',8,7,9.5);count_size<-num('count_size',6,5.5,7);point_size<-num('point_size',1.35,.8,2);line_width<-num('line_width',.35,.2,.6);bar_width<-num('bar_width',.66,.4,.8);point_alpha<-num('point_alpha',1,.5,1);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice;hex<-function(name,default){v<-text_param(name,default);if(!grepl('^#[0-9A-Fa-f]{6}$',v))stop(name,' must be #RRGGBB',call.=FALSE);toupper(v)};primary<-hex('primary_color','#3B454E');accent<-hex('accent_color','#C44242');if(primary=='#FFFFFF'||accent=='#FFFFFF')stop('Bars and active memberships need visible colors',call.=FALSE);sw<-width_mm/250;sh<-height_mm/155;left<-68*sw;right<-246*sw;dx<-(right-left)/n;cx<-left+((1:n)-.5)*dx;if(point_size>=dx*.75)stop('Membership dots need spacing at selected intersection count',call.=FALSE);cy<-seq(47*sh,19*sh,length.out=ns);bar_bottom<-54*sh;bar_top<-143*sh;ticks<-pretty(c(0,max(counts)*1.08),n=5);ticks<-unique(c(0,ticks[ticks>=0&ticks==floor(ticks)]));ymax<-max(ticks);if(ymax<=max(counts))ymax<-max(counts)*1.15;yy<-function(v)bar_bottom+v/ymax*(bar_top-bar_bottom);sxmax<-max(sizes)*1.08;sx<-function(v)(49-v/sxmax*32)*sw;set_ticks<-pretty(c(0,max(sizes)),n=3);set_ticks<-set_ticks[set_ticks>=0&set_ticks<=max(sizes)&set_ticks==floor(set_ticks)];active<-sapply(selected,function(z)strsplit(z,'',fixed=TRUE)[[1]]=='1')[set_order,,drop=FALSE];if(!is.matrix(active))active<-matrix(active,nrow=ns);connectors<-do.call(rbind,lapply(seq_len(n),function(j){ii<-which(active[,j]);if(length(ii)<2)return(NULL);data.frame(column=j,x=cx[j],ymin=min(cy[ii]),ymax=max(cy[ii]))}));omitted_patterns<-length(freq)-n;omitted_ids<-sum(freq)-sum(counts);captions<-c(sprintf('%d of %d exclusive intersections shown; %d of %d unique IDs shown; omitted: %d IDs in %d intersections.',n,length(freq),sum(counts),length(union_ids),omitted_ids,omitted_patterns),'Each column is an exclusive membership pattern; set bars use full lists. List provenance/selection unverified.')
font_file<-function(weight='normal',italic=FALSE){hit<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(hit$index!=0L)hit<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);hit$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces));probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,size,face='plain')grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face))),'mm',TRUE);if(max(vapply(labels,tw,numeric(1),size=base_size,face='bold'))>19*sw)stop('Set labels exceed reserved label column',call.=FALSE);if(max(vapply(captions,tw,numeric(1),size=6.5))>width_mm-8)stop('Caption needs wider canvas',call.=FALSE);cw<-vapply(as.character(counts),tw,numeric(1),size=count_size);if(any(cw*cos(75*pi/180)+count_size/72*25.4*sin(75*pi/180)>dx*.96))stop('Count labels require fewer intersections or smaller count_size/wider canvas',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(){grid::grid.newpage();mm<-function(x)grid::unit(x,'mm');txt<-function(z,x,y,size=base_size,face='plain',just='center',rot=0,col='#111111')grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=size,fontface=face,col=col));line<-function(x,y,x2,y2,lw=.25,col='#263238')grid::grid.lines(mm(c(x,x2)),mm(c(y,y2)),gp=grid::gpar(col=col,lwd=lw*72.27/25.4,lineend='butt'));txt('Exclusive intersections of supplied gene lists',4,151*sh,10,'bold','left');for(t in ticks[ticks<=ymax]){y<-yy(t);line(left,y,right,y,.12,'#E6E9EC');txt(format(t,trim=TRUE),left-2*sw,y,base_size,just='right')};line(left,bar_bottom,left,bar_top,.3);line(left,bar_bottom,right,bar_bottom,.3);txt('Exclusive intersection size',54*sw,98.5*sh,base_size+1,'bold',rot=90);for(j in seq_len(n)){yt<-yy(counts[j]);grid::grid.rect(mm(cx[j]),mm((yt+bar_bottom)/2),mm(dx*bar_width),mm(yt-bar_bottom),gp=grid::gpar(fill=primary,col=NA));txt(as.character(counts[j]),cx[j],yt+1*sh,count_size,just=c('left','bottom'),rot=75)};for(i in seq_len(ns)){grid::grid.rect(mm((left+right)/2),mm(cy[i]),mm(right-left),mm(3.4*sh),gp=grid::gpar(fill=if(i%%2)'#F3F5F6'else'#FFFFFF',col=NA));xe<-sx(sizes[set_order[i]]);grid::grid.rect(mm((49*sw+xe)/2),mm(cy[i]),mm(49*sw-xe),mm(2.8*sh),gp=grid::gpar(fill=primary,col=NA));txt(as.character(sizes[set_order[i]]),xe-1*sw,cy[i],6.5,just='right');txt(labels[i],62*sw,cy[i],base_size,'bold','right')};if(!is.null(connectors))for(i in seq_len(nrow(connectors)))line(connectors$x[i],connectors$ymin[i],connectors$x[i],connectors$ymax[i],line_width,accent);for(j in seq_len(n))for(i in seq_len(ns))grid::grid.circle(mm(cx[j]),mm(cy[i]),r=mm(point_size/2),gp=grid::gpar(fill=if(active[i,j])accent else'#DCE1E5',col=NA,alpha=if(active[i,j])point_alpha else 1));line(17*sw,17*sh,49*sw,17*sh,.3);for(t in set_ticks){x<-sx(t);line(x,17*sh,x,16*sh,.25);txt(format(t,trim=TRUE),x,13.7*sh,6.5)};txt('Full set size',33*sw,9.3*sh,base_size,'bold');for(i in 1:2)txt(captions[i],4,(4.5-(i-1)*3)*sh,6.5,just='left')}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())};write.csv(data.frame(ID=union_ids,membership,pattern=combos,check.names=FALSE),file.path(output_dir,'actual_membership.csv'),row.names=FALSE);write.csv(data.frame(pattern=all_patterns,count=as.integer(freq),degree=degree,displayed=all_patterns%in%selected),file.path(output_dir,'all_intersections.csv'),row.names=FALSE);write.csv(data.frame(pattern=selected,count=counts,x=cx,y=yy(counts)),file.path(output_dir,'displayed_intersections.csv'),row.names=FALSE);write.csv(data.frame(set=ordered_keys,label=labels,size=sizes[set_order],y=cy,x_end=sx(sizes[set_order])),file.path(output_dir,'actual_sets.csv'),row.names=FALSE);writeLines(sort(Reduce(intersect,sets),method='radix'),file.path(output_dir,'intersectGenes.txt'));saveRDS(list(raw_sets=raw_sets,sets=sets,membership=membership,combos=combos,freq=freq,selected=selected,set_order=set_order,active=active,connectors=connectors),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(source_paths=source_paths,keys=keys,labels=labels,raw_counts=lengths(raw_sets),unique_counts=sizes,union_count=length(union_ids),intersection_count=length(Reduce(intersect,sets)),pattern_count=length(freq),omitted_patterns=omitted_patterns,omitted_ids=omitted_ids,order_by=order_by,max_intersections=max_intersections,width_mm=width_mm,height_mm=height_mm,dpi=dpi,base_size=base_size,count_size=count_size,point_size=point_size,point_alpha=point_alpha,line_width=line_width,bar_width=bar_width,primary=primary,accent=accent,family=family,pdf_faces=pdf_faces,captions=captions,ticks=as.list(ticks[ticks<=ymax]),set_ticks=as.list(set_ticks),geometry=list(left=left,right=right,dx=dx,cx=cx,cy=cy,bar_bottom=bar_bottom,bar_top=bar_top,ymax=ymax,sxmax=sxmax,sw=sw,sh=sh)),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',length(union_ids),'IDs;',n,'of',length(freq),'exclusive intersections; omitted',omitted_ids,'IDs\n')
