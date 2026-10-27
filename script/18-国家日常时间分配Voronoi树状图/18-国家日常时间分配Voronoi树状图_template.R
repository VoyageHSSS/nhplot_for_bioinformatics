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
required_packages <- c("jsonlite", "svglite", "ragg", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE); params <- if (is.null(cfg$params)) list() else cfg$params
if(!is.list(cfg)||!is.list(params)||(!is.null(names(params))&&any(!nzchar(names(params)))))stop("config/params must be objects",call.=FALSE)
gp <- function(name, default = NULL) {value<-params[[name]];if(is.null(value))for(section in c("data","selection","colors","typography","geometry","axis","legend","layout","export"))if(is.list(params[[section]])&&!is.null(params[[section]][[name]])){value<-params[[section]][[name]];break};if(is.null(value))default else value}
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
  value <- trimws(as.character(value)); project_app <- dirname(dirname(dirname(script_dir))); candidates <- if (nzchar(value)) c(value, file.path(getwd(), value), file.path(script_dir, value), file.path(project_app, value)) else c(fallback, file.path(script_dir, fallback)); hit <- unique(candidates[file.exists(candidates)]); if (!length(hit)) stop("Input file does not exist: ", if (nzchar(value)) value else fallback, call. = FALSE); normalizePath(hit[[1L]], mustWork = TRUE)
}
read_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if(ext=="xls"){magic<-readBin(path,"raw",n=8);binary<-identical(magic,as.raw(c(208,207,17,224,161,177,26,225)))||identical(head(magic,2),charToRaw("PK"));if(!binary)ext<-"txt"}
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt", "md", "maf")) { first <- readLines(path, n = 1L, warn = FALSE); seps<-c("\t",";",",");counts<-vapply(seps,function(sep){con<-textConnection(first);on.exit(close(con));sum(utils::count.fields(con,sep=sep,quote="\"",comment.char=""),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
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

.registry_parameter_bindings<-list(data_path=gp('data_path',NULL),country_regions_path=gp('country_regions_path',NULL),country=gp('country',NULL),family=gp('family',NULL),base_size=gp('base_size',NULL),axis_text_size=gp('axis_text_size',NULL),line_width=gp('line_width',NULL),label_color=gp('label_color',NULL),width_mm=gp('width_mm',NULL),height_mm=gp('height_mm',NULL),dpi=gp('dpi',NULL))
raw<-read_table(resolve_input(text_param('data_path','assets/all_countries.csv'),file.path(script_dir,'assets/all_countries.csv')));mapping<-read_table(resolve_input(text_param('country_regions_path','assets/country_regions.csv'),file.path(script_dir,'assets/country_regions.csv')));country<-text_param('country','Australia')
if(anyDuplicated(names(raw))||!all(c('Category','Subcategory','hoursPerDayCombined','country_iso3')%in%names(raw))||!nrow(raw))stop('Unique Category/Subcategory/hoursPerDayCombined/country_iso3 required',call.=FALSE)
if(anyDuplicated(names(mapping))||!all(c('country_iso3','country_name')%in%names(mapping))||anyNA(mapping$country_iso3)||any(!nzchar(mapping$country_iso3))||anyDuplicated(mapping$country_iso3)||anyNA(mapping$country_name)||any(!nzchar(mapping$country_name)))stop('Complete one-to-one ISO country mapping required',call.=FALSE)
for(k in c('Category','Subcategory','country_iso3'))if(anyNA(raw[[k]])||any(!nzchar(trimws(raw[[k]])))||any(grepl('[\r\n]',raw[[k]])))stop('Complete single-line identifiers required: ',k,call.=FALSE)
mi<-match(raw$country_iso3,mapping$country_iso3);if(anyNA(mi))stop('Unmapped country ISO codes; no inner-join row loss allowed',call.=FALSE);selected<-which(mapping$country_name[mi]==country);if(!length(selected))stop('No rows found for country: ',country,call.=FALSE);if(length(unique(raw$country_iso3[selected]))!=1)stop('Country name maps to multiple ISO codes',call.=FALSE)
dat<-raw[selected,,drop=FALSE];if(anyDuplicated(dat[c('Category','Subcategory')]))stop('Duplicate hierarchy paths require explicit aggregation before plotting',call.=FALSE);values<-parse_numeric(dat$hoursPerDayCombined,'hoursPerDayCombined');if(any(!is.finite(values)|values<0))stop('Hours must be finite and nonnegative; no silent row removal',call.=FALSE);if(sum(values)<=0)stop('Country needs positive hours',call.=FALSE)
# ponytail: bounded10-category/60-leaf capacity solve; use a dedicated geometry engine for larger hierarchies.
keys<-unique(dat$Category);totals<-vapply(keys,function(k)sum(values[dat$Category==k]),numeric(1));total<-sum(values);positive<-which(totals>0);if(length(keys)>10||nrow(dat)>60)stop('This compact hierarchy supports at most10 categories/60 subcategories',call.=FALSE)
family<-choice('family','Helvetica',c('Helvetica','sans'));base_size<-num('base_size',10,6,24);axis_text_size<-num('axis_text_size',6.5,5,18);line_width<-num('line_width',.3,.1,1.5);label_color<-validate_colour('label_color',text_param('label_color','#1F2933'));rgb<-grDevices::col2rgb(label_color)/255;if(mean(rgb)> .7)stop('Label colour must contrast with the white background',call.=FALSE);width_mm<-num('width_mm',220,150,420);height_mm<-num('height_mm',165,120,360);dpi<-integer_param('dpi',600,96,1200)
area<-function(p)if(nrow(p)<3)0 else abs(sum(p[,1]*p[c(2:nrow(p),1),2]-p[,2]*p[c(2:nrow(p),1),1]))/2
clip<-function(p,a,b,c){if(nrow(p)<3)return(matrix(numeric(),ncol=2));d<-p[,1]*a+p[,2]*b-c;inside<-d<=0;q<-p[c(2:nrow(p),1),,drop=FALSE];dq<-d[c(2:length(d),1)];cross<-inside!=inside[c(2:length(inside),1)];out<-matrix(NA_real_,nrow=2*nrow(p),ncol=2);k<-0L;for(i in seq_len(nrow(p))){if(inside[i]){k<-k+1L;out[k,]<-p[i,]};if(cross[i]){k<-k+1L;out[k,]<-p[i,]+d[i]/(d[i]-dq[i])*(q[i,]-p[i,])}};if(!k)return(matrix(numeric(),ncol=2));out[seq_len(k),,drop=FALSE]}
cells<-function(bound,sites,w){lapply(seq_len(nrow(sites)),function(i){p<-bound;for(j in setdiff(seq_len(nrow(sites)),i)){d<-2*(sites[j,]-sites[i,]);p<-clip(p,d[1],d[2],sum(sites[j,]^2)-sum(sites[i,]^2)+w[i]-w[j]);if(!nrow(p))break};p})}
weighted<-function(bound,values){n<-length(values);if(n==1)return(list(bound));goal<-values/sum(values);tot<-area(bound);center<-colMeans(bound);lim<-apply(bound,2,range);sites<-matrix(NA_real_,n,2);for(i in seq_len(n)){repeat{v<-c(runif(1,lim[1,1],lim[2,1]),runif(1,lim[1,2],lim[2,2]));edges<-bound[c(2:nrow(bound),1),]-bound;rel<-matrix(v,nrow(bound),2,byrow=TRUE)-bound;z<-edges[,1]*rel[,2]-edges[,2]*rel[,1];if(all(z>=-1e-12)||all(z<=1e-12)){sites[i,]<-v;break}}};
 # Weights optimize area, never the scientific values or coordinates of data.
 calc<-function(w)vapply(cells(bound,sites,c(w,0)),area,numeric(1))/tot
 w<-rep(0,n-1);for(iter in 1:80){s<-calc(w);r<-s[-n]-goal[-n];if(max(abs(s-goal)/goal)<1e-6)break;eps<-1e-5;J<-sapply(seq_along(w),function(j){up<-dn<-w;up[j]<-up[j]+eps;dn[j]<-dn[j]-eps;(calc(up)[-n]-calc(dn)[-n])/(2*eps)});step<-tryCatch(solve(J,r),error=function(e)NULL);if(is.null(step)){fit<-optim(w,function(v)sum((calc(v)-goal)^2),method='BFGS',control=list(maxit=300,reltol=1e-14));w<-fit$par;next};ok<-FALSE;for(k in 0:25){trial<-w-step*2^-k;if(all(calc(trial)>goal*.001)&&sum((calc(trial)-goal)^2)<sum((s-goal)^2)){w<-trial;ok<-TRUE;break}};if(!ok)break};p<-cells(bound,sites,c(w,0));actual<-vapply(p,area,numeric(1))/tot;if(max(abs(actual-goal)/goal)>1e-5)stop('Area capacity solver did not converge');attr(p,"sites")<-sites;attr(p,"weights")<-c(w,0);attr(p,"relative_error")<-max(abs(actual-goal)/goal);p}
theta<-seq(0,2*pi,length.out=361)[-361];bound<-cbind(cos(theta),sin(theta));set.seed(17325);groups<-weighted(bound,totals[positive]);names(groups)<-keys[positive];children<-list();leaf_polygons<-vector('list',nrow(dat));for(j in seq_along(positive)){ii<-which(dat$Category==keys[positive[j]]&values>0);p<-weighted(groups[[j]],values[ii]);children[[keys[positive[j]]]]<-list(indices=ii,diagram=p);for(k in seq_along(ii))leaf_polygons[[ii[k]]]<-p[[k]]}
colours<-setNames(unname(plot_nh_palette_categorical(length(keys),style='tol_muted',allow_fallback=FALSE)),keys);mix<-function(hex,t){c<-grDevices::col2rgb(hex)/255;grDevices::rgb(c[1]+(1-c[1])*t,c[2]+(1-c[2])*t,c[3]+(1-c[3])*t)};leaf_fill<-character(nrow(dat));for(k in keys){ii<-which(dat$Category==k);r<-rank(-values[ii],ties.method='first');leaf_fill[ii]<-vapply(r,function(v)mix(colours[[k]],if(length(ii)==1).1 else .08+.25*(v-1)/(length(ii)-1)),character(1))}
root_area<-area(bound);category_area<-setNames(rep(0,length(keys)),keys);category_area[positive]<-vapply(groups,area,numeric(1));leaf_area<-vapply(leaf_polygons,function(p)if(is.null(p))0 else area(p),numeric(1));if(any(abs(leaf_area/root_area-values/total)>1e-5*pmax(values/total,1e-12)))stop('Nested area capacity mismatch',call.=FALSE)
category_summary<-data.frame(Category=keys,hours=totals,area=unname(category_area),share=unname(category_area)/root_area,target_share=totals/total,colour=unname(colours));leaf_summary<-data.frame(source_row=selected,Category=dat$Category,Subcategory=dat$Subcategory,hours=values,area=leaf_area,share=leaf_area/root_area,target_share=values/total,colour=leaf_fill);polygons<-list();texts<-list();rects<-list();geometry<-NULL;title<-paste0(country,': daily time allocation');subtitle<-sprintf('%s | %.2f supplied hours; area proportional to hours',unique(dat$country_iso3),total);caps<-c(sprintf('%d supplied subcategories; %d zero-hour entries have no area. Totals retain source rounding.',nrow(dat),sum(values==0)),'Parent hues and white borders show categories; lighter cells show subcategories. Supplied uncertainty is not plotted.')
render_one<-function(kind){file<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white')else if(kind=='pdf')grDevices::pdf(file,width=width_mm/25.4,height=height_mm/25.4,family='Helvetica',useDingbats=FALSE,bg='white')else svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white')
tryCatch({grid::grid.newpage();grid::pushViewport(grid::viewport(xscale=c(0,width_mm),yscale=c(0,height_mm)));measure<-function(s,fs,face='plain')grid::convertWidth(grid::grobWidth(grid::textGrob(s,gp=grid::gpar(fontfamily=family,fontsize=fs,fontface=face))),'mm',valueOnly=TRUE);text<-function(s,x,y,fs,face='plain',h=0,role='text'){grid::grid.text(s,x=grid::unit(x,'native'),y=grid::unit(y,'native'),just=c(h,.5),gp=grid::gpar(fontfamily=family,fontsize=fs,fontface=face,col=label_color));if(kind=='svg'){grid::pushViewport(grid::viewport(gp=grid::gpar(fontfamily=family,fontsize=fs,fontface=face)));m<-grid::calcStringMetric('X');grid::popViewport();texts[[length(texts)+1]]<<-data.frame(role=role,label=s,x_mm=x,y_mm=y,font_size=fs,font_face=face,hjust=h,baseline_shift_pt=(m$ascent-m$descent)*72/2)}};poly<-function(p,fill,stroke,lw,role,id){grid::grid.polygon(x=grid::unit(cx+p[,1]*radius,'native'),y=grid::unit(cy+p[,2]*radius,'native'),gp=grid::gpar(fill=fill,col=stroke,lwd=lw*96/25.4,linejoin='round'));if(kind=='svg')polygons[[length(polygons)+1]]<<-data.frame(role=role,id=unname(id),vertex=seq_len(nrow(p)),x_mm=cx+p[,1]*radius,y_mm=cy+p[,2]*radius,fill=fill,stroke=stroke,width_mm=lw)};rect<-function(x,y,fill){grid::grid.rect(x=grid::unit(x,'native'),y=grid::unit(y,'native'),width=grid::unit(2,'native'),height=grid::unit(2,'native'),gp=grid::gpar(fill=fill,col=NA));if(kind=='svg')rects[[length(rects)+1]]<<-data.frame(x_mm=x,y_mm=y,width_mm=2,height_mm=2,fill=fill)}
radius<-min((height_mm-33)/2,(width_mm-85)/2);cx<-5+radius;cy<-19+radius;tx<-2*radius+20;table_top<-height_mm-22;table_fs<-axis_text_size;child_step<-max(3.5,table_fs*25.4/72*1.3);parent_step<-max(4.8,(table_fs+1)*25.4/72*1.3);needed<-length(keys)*parent_step+nrow(dat)*child_step;if(radius<38||table_top-needed<17||tx>width_mm-50||measure(title,base_size+1,'bold')>width_mm-10||measure(subtitle,base_size-2)>width_mm-10||any(vapply(caps,measure,numeric(1),fs=6)>width_mm-10))stop('Insufficient circle/table/title canvas space',call.=FALSE)
for(i in seq_len(nrow(dat))){if(measure(dat$Subcategory[i],table_fs)>width_mm-5-tx-16)stop('Subcategory and hours columns collide',call.=FALSE);if(!is.null(leaf_polygons[[i]]))poly(leaf_polygons[[i]],leaf_fill[i],'#FFFFFF',.1,'subcategory',i)}
for(j in seq_along(groups))poly(groups[[j]],NA_character_,'#FFFFFF',line_width,'category border',positive[j])
text(title,5,height_mm-5,base_size+1,'bold',role='title');text(subtitle,5,height_mm-12,base_size-2,role='subtitle');text('Activity',tx,table_top+5,table_fs+1,'bold',role='table header');text('Hours/day',width_mm-5,table_top+5,table_fs+1,'bold',h=1,role='table header');y<-table_top
for(j in seq_along(keys)){k<-keys[j];parent_label<-k;if(measure(parent_label,table_fs+1,'bold')>width_mm-5-tx-13)stop('Category and total columns collide',call.=FALSE);rect(tx-3,y,colours[[k]]);text(parent_label,tx,y,table_fs+1,'bold',role='category');text(sprintf('%.2f',totals[j]),width_mm-5,y,table_fs+1,'bold',h=1,role='category hours');y<-y-parent_step;for(i in which(dat$Category==k)){rect(tx-3,y,leaf_fill[i]);text(dat$Subcategory[i],tx,y,table_fs,role='subcategory');text(sprintf('%.2f',values[i]),width_mm-5,y,table_fs,h=1,role='subcategory hours');y<-y-child_step}}
text(caps[1],5,9,6,role='caption');text(caps[2],5,5,6,role='caption');if(kind=='svg')geometry<<-list(radius_mm=radius,cx_mm=cx,cy_mm=cy,table_x_mm=tx,table_top_mm=table_top,canvas_width_mm=width_mm,canvas_height_mm=height_mm);grid::popViewport()
},finally=grDevices::dev.off())}
for(kind in c('png','pdf','svg'))render_one(kind)
write_tsv<-function(x,name)write.table(x,file.path(output_dir,name),sep='\t',quote=TRUE,row.names=FALSE,na='NA',qmethod='double',fileEncoding='UTF-8');write_tsv(raw,'supplied-countries.tsv');write_tsv(mapping,'supplied-mapping.tsv');write_tsv(dat,'selected-country.tsv');write_tsv(category_summary,'category-summary.tsv');write_tsv(leaf_summary,'subcategory-summary.tsv');write_tsv(do.call(rbind,polygons),'native-polygons.tsv');write_tsv(do.call(rbind,texts),'native-text.tsv');write_tsv(do.call(rbind,rects),'native-rects.tsv');saveRDS(list(raw=raw,mapping=mapping,selected=dat,selected_rows=selected,values=values,bound=bound,groups=groups,children=children,leaf_polygons=leaf_polygons,category_summary=category_summary,leaf_summary=leaf_summary),file.path(output_dir,'parsed-analysis.rds'));jsonlite::write_json(list(country=country,iso=unique(dat$country_iso3),supplied_rows=nrow(raw),selected_rows=nrow(dat),categories=length(keys),positive_categories=length(positive),positive_subcategories=sum(values>0),zero_subcategories=sum(values==0),total_hours=total,max_relative_area_error=max(abs(leaf_area[values>0]/root_area-values[values>0]/total)/(values[values>0]/total)),geometry=geometry,caption=caps,line_width=line_width,seed=17325,method='nested capacity-constrained power diagrams; source hours unchanged'),file.path(output_dir,'analysis.json'),auto_unbox=TRUE,digits=15);cat('Rendered true-area two-level power diagram; all source rows retained\n')
