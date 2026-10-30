#!/usr/bin/env Rscript

# Auditable Gaussian density with explicit complete-case policy and native geometry.
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
required_packages <- c("jsonlite", "svglite", "ragg", "readxl", "showtext", "sysfonts", "systemfonts", "scales")
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
read_table <- function(path, sheet=1L) {
  ext <- tolower(tools::file_ext(path))
  if(ext=="xls"){magic<-readBin(path,"raw",n=8);binary<-identical(magic,as.raw(c(208,207,17,224,161,177,26,225)))||identical(head(magic,2),charToRaw("PK"));if(identical(head(magic,2),charToRaw("PK")))ext<-"xlsx"else if(!binary)stop("XLS input must contain an actual workbook",call.=FALSE)}
  out <- if (ext == "rds") readRDS(path) else if (ext %in% c("csv", "tsv", "txt")) { seps<-c("\t",";",",");counts<-vapply(seps,function(sep){v<-tryCatch(utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE),error=function(e)0L);max(c(0L,v),na.rm=TRUE)},integer(1));sep<-seps[which.max(counts)]; fields<-utils::count.fields(path,sep=sep,quote="\"",comment.char="",blank.lines.skip=TRUE);fields<-fields[!is.na(fields)];if(!length(fields)||any(fields!=fields[1]))stop("Every table record must match the header field count",call.=FALSE); if (sep=="\t") utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) else utils::read.table(path,header=TRUE,sep=sep,quote="\"",comment.char="",check.names=FALSE,stringsAsFactors=FALSE,colClasses="character",encoding="UTF-8",na.strings=character()) } else if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for XLS/XLSX input", call. = FALSE)
    as.data.frame((if(ext=="xlsx")readxl::read_xlsx else readxl::read_xls)(path, sheet=sheet, .name_repair = "minimal",col_types="text",trim_ws=FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  } else stop("Supported inputs are CSV, TSV, TXT, XLS, XLSX, and RDS", call. = FALSE)
  if (!is.data.frame(out)) stop("Input must contain a data frame", call. = FALSE)
  names(out) <- trimws(enc2utf8(sub("^\ufeff", "", names(out)))); out
}
parse_numeric <- function(x, label) {
  if (is.numeric(x)) return(as.numeric(x))
  raw <- as.character(x); missing <- is.na(raw) | !nzchar(raw) | toupper(raw) %in% c("NA", "N/A", "NAN", "NULL"); value <- suppressWarnings(as.numeric(raw)); bad <- !missing & is.na(value)
  if (any(bad)) stop(label, " contains non-numeric values: ", paste(unique(raw[bad])[seq_len(min(3L, sum(bad)))], collapse = ", "), call. = FALSE); value[missing] <- NA_real_; value
}



data_path<-resolve_input(text_param('data_path','assets/data.txt'),'assets/data.txt');if(dir.exists(data_path))stop('Table file required',call.=FALSE);sheet<-integer_param('sheet',1,1,20);raw<-read_table(data_path,sheet);required<-c('species','flipper_length_mm','sex');if(!nrow(raw)||nrow(raw)>3000||anyDuplicated(names(raw))||!all(required%in%names(raw)))stop('species/flipper_length_mm/sex table of 1..3000 rows required',call.=FALSE)
missing<-function(v)is.na(v)|trimws(toupper(as.character(v)))%in%c('','NA','N/A','NAN','NULL');miss<-as.data.frame(lapply(raw,missing));value<-parse_numeric(raw$flipper_length_mm,'flipper_length_mm');if(any(!is.na(value)&(!is.finite(value)|value<=0|value>1e9)))stop('Observed flipper lengths must be finite and positive <=1e9',call.=FALSE);policy<-choice('missing_policy','source_complete_cases',c('source_complete_cases','required_only','reject_missing'));bad_required<-miss$species|is.na(value)|miss$sex;bad<-if(policy=='source_complete_cases')rowSums(miss)>0 else bad_required;if(policy=='reject_missing'&&any(bad_required))stop('Required plot fields missing',call.=FALSE)
exclusions<-data.frame(source_row=which(bad),reason=if(any(bad))vapply(which(bad),function(j)paste(names(raw)[unlist(miss[j,],use.names=FALSE)],collapse=','),character(1))else character());observations<-data.frame(source_row=which(!bad),species=as.character(raw$species[!bad]),value=value[!bad],sex=as.character(raw$sex[!bad]),stringsAsFactors=FALSE);if(!nrow(observations))stop('No complete observations',call.=FALSE);if(length(setdiff(unique(as.character(raw$species[!miss$species])),observations$species)))stop('Selected policy removes an entire species',call.=FALSE);if(any(grepl('cover_[0-9]|Case[ _:#-]*[0-9]|案例[ ]*[0-9]',c(observations$species,observations$sex),ignore.case=TRUE)))stop('Internal labels prohibited',call.=FALSE)
order_param<-function(z,v,maxn){ans<-if(nzchar(z))trimws(strsplit(z,',',fixed=TRUE)[[1]])else sort(unique(v),method='radix');if(any(!nzchar(ans))||anyDuplicated(ans)||!setequal(ans,v)||length(ans)>maxn)stop('Complete unique group order required',call.=FALSE);ans};groups<-order_param(text_param('species_order','',TRUE),observations$species,3);sexes<-order_param(text_param('sex_order','',TRUE),observations$sex,2);if(any(table(factor(observations$species,groups),factor(observations$sex,sexes))==0))stop('All displayed species/sex combinations require observations',call.=FALSE)
width_mm<-num('width_mm',240,220,300);height_mm<-num('height_mm',135,125,190);dpi<-integer_param('dpi',600,96,1200);point_size<-num('point_size',1.5,1,2);point_alpha<-num('point_alpha',.5,.3,1);violin_width<-num('violin_line_width',.25,.15,.5);box_width<-num('box_line_width',.7,.4,1);axis_size<-num('axis_title_size_pt',12,10,14);tick_size<-num('axis_text_size_pt',9.5,8,11);legend_size<-num('legend_text_size_pt',9,8,11);bw_adjust<-num('bandwidth_adjust',1,.5,2);fixed_bw<-num('bandwidth_mm',0,0,1e7);family_choice<-choice('font_family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
sc<-toupper(c(text_param('species_color_1','#5CC3E8'),text_param('species_color_2','#FFDB00'),text_param('species_color_3','#79CEB8')));sx<-toupper(c(text_param('sex_color_1','#5CC3E8'),text_param('sex_color_2','#FFDB00')));for(v in list(sc,sx))if(any(!grepl('^#[0-9A-F]{6}$',v))||anyDuplicated(v)||any(v=='#FFFFFF'))stop('Distinct visible HEX colors required within each palette',call.=FALSE);sc<-setNames(sc[seq_along(groups)],groups);sx<-setNames(sx[seq_along(sexes)],sexes)
summary_fn<-function(v){q<-quantile(v,c(.025,.17,.25,.5,.75,.83,.975),names=FALSE,type=7);iq<-q[5]-q[3];w<-range(v[v>=q[3]-1.5*iq&v<=q[5]+1.5*iq]);data.frame(n=length(v),min=min(v),max=max(v),mean=mean(v),q025=q[1],q17=q[2],q25=q[3],median=q[4],q75=q[5],q83=q[6],q975=q[7],whisker_low=w[1],whisker_high=w[2])};summary<-do.call(rbind,lapply(groups,function(g)cbind(species=g,summary_fn(observations$value[observations$species==g]))));sex_summary<-do.call(rbind,lapply(groups,function(g)do.call(rbind,lapply(sexes,function(s)cbind(species=g,sex=s,summary_fn(observations$value[observations$species==g&observations$sex==s]))))))
kde<-function(v){if(fixed_bw==0&&(length(v)<2||length(unique(v))<2))stop('Automatic density needs >=2 distinct values in each species/sex group; set explicit positive bandwidth_mm for declared smoothing',call.=FALSE);bw<-(if(fixed_bw>0)fixed_bw else bw.nrd0(v))*bw_adjust;span<-range(v);if(diff(span)==0)span<-span+c(-3,3)*bw;d<-density(v,bw=bw,n=512,from=span[1],to=span[2]);data.frame(value=d$x,density=d$y,bw=bw)}
densities<-do.call(rbind,lapply(groups,function(g)cbind(species=g,sex='',kde(observations$value[observations$species==g]))));sex_densities<-do.call(rbind,lapply(groups,function(g)do.call(rbind,lapply(sexes,function(s)cbind(species=g,sex=s,kde(observations$value[observations$species==g&observations$sex==s]))))));v_range<-range(densities$value,sex_densities$value,observations$value);domain<-v_range+c(-.05,.05)*diff(v_range);if(!all(is.finite(domain))||diff(domain)<=0)stop('Finite nonzero display range required',call.=FALSE);vticks<-scales::breaks_extended(n=6)(domain);vticks<-vticks[vticks>=domain[1]&vticks<=domain[2]];axis_factor<-if(max(abs(domain))>=1e4||max(abs(domain))<.1)10^(floor(log10(max(abs(domain)))/3)*3)else 1;value_axis_label<-if(axis_factor==1)'Flipper length (mm)'else paste0('Flipper length (x',format(axis_factor,scientific=FALSE,trim=TRUE),' mm)');fmt<-function(v){v<-v/axis_factor;ifelse(abs(v)>=1e5|abs(v)<.01,format(v,scientific=TRUE,trim=TRUE,digits=2),format(v,trim=TRUE,digits=5))}
frames<-data.frame(panel=1:6,left=c(.051,.384,.717,.051,.384,.752)*width_mm,right=c(.327,.660,.911,.327,.660,.994)*width_mm,bottom=c(rep(.575,3),rep(.075,3))*height_mm,top=c(rep(.990,3),rep(.489,3))*height_mm);ng<-length(groups);cat_domain<-c(.4,ng+.6);box_cat_domain<-c(.4,ng+.75);fx<-function(v,k){cd<-if(k==5)box_cat_domain else cat_domain;frames$left[k]+(v-cd[1])/diff(cd)*(frames$right[k]-frames$left[k])};fy<-function(v,k)frames$bottom[k]+(v-domain[1])/diff(domain)*(frames$top[k]-frames$bottom[k]);hx<-function(v)frames$left[6]+(v-domain[1])/diff(domain)*(frames$right[6]-frames$left[6]);horizontal_cat_domain<-c(.4,ng+.85);hy<-function(v)frames$bottom[6]+(v-horizontal_cat_domain[1])/diff(horizontal_cat_domain)*(frames$top[6]-frames$bottom[6])
polys<-list();lines<-list();points<-list();labels<-list();poly<-function(x,y,color,role,g,k,stroke='#333333',lw=violin_width){polys[[length(polys)+1]]<<-data.frame(poly_id=length(polys)+1,vertex=seq_along(x),x_mm=unname(x),y_mm=unname(y),color=color,stroke=stroke,width_mm=lw,role=role,species=g,panel=k)};line<-function(x,y,k,color='#444444',lw=.2,role='frame',g=''){lines[[length(lines)+1]]<<-data.frame(line_id=length(lines)+1,vertex=seq_along(x),x_mm=unname(x),y_mm=unname(y),color=color,width_mm=lw,role=role,species=g,panel=k)};point<-function(x,y,color,k,row=0,role='observation',diam=point_size,alpha=point_alpha,stroke='#111111',lw=.18){points[[length(points)+1]]<<-data.frame(x_mm=unname(x),y_mm=unname(y),color=color,panel=k,source_row=row,role=role,diameter_mm=diam,alpha=alpha,stroke=stroke,width_mm=lw)};label<-function(z,x,y,size,role,k,just='centre',rotation=0,color='#111111'){labels[[length(labels)+1]]<<-data.frame(label=as.character(z),x_mm=unname(x),y_mm=unname(y),size=size,role=role,panel=k,just=just,rotation=rotation,color=color,face='plain')}
# Exact observation-axis values; fixed-seed offsets only along category axes.
canonical<-order(observations$species,observations$value,observations$sex,method='radix');set.seed(411);observations$category_offset<-numeric(nrow(observations));observations$category_offset[canonical]<-runif(nrow(observations),-.18,.18);set.seed(412);observations$sina_unit<-numeric(nrow(observations));observations$sina_unit[canonical]<-runif(nrow(observations),-1,1);maxd<-max(densities$density);maxsex<-max(sex_densities$density);slabmax<-max(vapply(groups,function(g)max(densities$density[densities$species==g])*sum(observations$species==g),numeric(1)))
for(j in seq_along(groups)){g<-groups[j];d<-densities[densities$species==g,];c<-unname(sc[g]);w<-.45*d$density/maxd
for(k in c(1,2)){poly(c(fx(j-w,k),rev(fx(j+w,k))),c(fy(d$value,k),rev(fy(d$value,k))),c,'violin',g,k)}
poly(c(fx(j-w,4),rep(fx(j,4),nrow(d))),c(fy(d$value,4),rev(fy(d$value,4))),c,'left_half',g,4)
poly(c(rep(fx(j+.25,5),nrow(d)),rev(fx(j+.25+.4*d$density/maxd,5))),c(fy(d$value,5),rev(fy(d$value,5))),c,'right_half',g,5,NA)
s<-summary[summary$species==g,];line(rep(fx(j,5),2),fy(c(s$whisker_low,s$whisker_high),5),5,lw=box_width,role='whisker',g=g);poly(fx(j+c(-.2,.2,.2,-.2),5),fy(c(s$q25,s$q25,s$q75,s$q75),5),c,'box',g,5,'#111111',box_width);line(fx(j+c(-.2,.2),5),rep(fy(s$median,5),2),5,lw=box_width,role='median',g=g)
z<-observations[observations$species==g,];dw<-approx(d$value,w,xout=z$value,rule=2)$y;for(i in seq_len(nrow(z))){point(fx(j+z$sina_unit[i]*dw[i],2),fy(z$value[i],2),'#000000',2,z$source_row[i]);point(fx(j+z$category_offset[i],5),fy(z$value[i],5),c,5,z$source_row[i],alpha=1)}
# Count-weighted density slab and exact-value stacks, with declared descriptive quantiles.
base<-j+.25;slab<-.52*d$density*s$n/slabmax;poly(c(hx(d$value),rev(hx(d$value))),c(rep(hy(base),512),rev(hy(base+slab))),c,'count_weighted_slab',g,6,NA);dup<-table(z$value);maxdup<-max(dup);stack_diam<-min(.53,(hy(.45)-hy(0))/(maxdup+.5));if(stack_diam<.25)stop('Exact-value dot stacks too dense; reduce rows or enlarge canvas',call.=FALSE);z$stack_index<-ave(seq_len(nrow(z)),z$value,FUN=seq_along);for(i in seq_len(nrow(z)))point(hx(z$value[i]),hy(base)-z$stack_index[i]*stack_diam,c,6,z$source_row[i],'exact_value_stack',stack_diam,1,NA,0)
line(hx(c(s$q025,s$q975)),rep(hy(base),2),6,'#111111',.5,'descriptive95',g);line(hx(c(s$q17,s$q83)),rep(hy(base),2),6,'#111111',1.1,'descriptive66',g);point(hx(s$median),hy(base),'#111111',6,role='median',diam=1.8,alpha=1,stroke=NA,lw=0)
for(h in seq_along(sexes)){sex<-sexes[h];ds<-sex_densities[sex_densities$species==g&sex_densities$sex==sex,];centre<-j+(h-(length(sexes)+1)/2)*.45;ww<-.215*ds$density/maxsex;poly(c(fx(centre-ww,3),rev(fx(centre+ww,3))),c(fy(ds$value,3),rev(fy(ds$value,3))),unname(sx[sex]),'sex_violin',g,3);zz<-z[z$sex==sex,];wd<-approx(ds$value,ww,xout=zz$value,rule=2)$y;for(i in seq_len(nrow(zz)))point(fx(centre+zz$sina_unit[i]*wd[i],3),fy(zz$value[i],3),'#000000',3,zz$source_row[i])}}
x_title_offset<-max(7.2,(axis_size+tick_size)*25.4/72*.575+2.6)
for(k in 1:6){f<-frames[k,];line(c(f$left,f$right,f$right,f$left,f$left),c(f$bottom,f$bottom,f$top,f$top,f$bottom),k);if(k<=5){for(v in vticks){line(c(f$left-.7,f$left),rep(fy(v,k),2),k,role='tick');label(fmt(v),f$left-1.1,fy(v,k),tick_size,'value_tick',k,'right')};for(j in seq_along(groups)){line(rep(fx(j,k),2),c(f$bottom-.7,f$bottom),k,role='tick');label(groups[j],fx(j,k),f$bottom-2.5,tick_size,'group_tick',k)};label('Species',mean(c(f$left,f$right)),f$bottom-x_title_offset,axis_size,'axis',k);label(value_axis_label,max(axis_size*25.4/72*.575+.25,f$left-10),mean(c(f$bottom,f$top)),axis_size,'axis',k,rotation=90)}else{for(v in vticks){line(rep(hx(v),2),c(f$bottom-.7,f$bottom),k,role='tick');label(fmt(v),hx(v),f$bottom-2.5,tick_size,'value_tick',k)};for(j in seq_along(groups)){line(c(f$left-.7,f$left),rep(hy(j+.25),2),k,role='tick');label(groups[j],f$left-1.1,hy(j+.25),tick_size,'group_tick',k,'right')};label(value_axis_label,mean(c(f$left,f$right)),f$bottom-x_title_offset,axis_size,'axis',k);label('Species',f$left-19.8,mean(c(f$bottom,f$top)),axis_size,'axis',k,rotation=90)}}
legend_rects<-data.frame(sex=sexes,xmin=.930*width_mm,xmax=.947*width_mm,ymin=.745*height_mm-seq_along(sexes)*5-2,ymax=.745*height_mm-seq_along(sexes)*5+2,color=unname(sx));label('Sex',.930*width_mm,.765*height_mm,legend_size+1,'legend_title',3,'left',color='#FF0000');for(j in seq_along(sexes)){label(sexes[j],.951*width_mm,.745*height_mm-j*5,legend_size,'legend',3,'left');point(.9385*width_mm,.745*height_mm-j*5,'#000000',3,role='legend',diam=1.2)}
polys<-do.call(rbind,polys);lines<-do.call(rbind,lines);points<-do.call(rbind,points);labels<-do.call(rbind,labels)
font_file<-function(weight='normal',italic=FALSE){h<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(h$index!=0L)h<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);h$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces));probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({ww<-vapply(seq_len(nrow(labels)),function(i)grid::convertWidth(grid::grobWidth(grid::textGrob(labels$label[i],gp=grid::gpar(fontfamily=family,fontsize=labels$size[i]))),'mm',TRUE),numeric(1));hh<-labels$size*25.4/72*1.15;for(k in 1:5){ia<-which(labels$panel==k&labels$role=='axis'&labels$rotation==90);it<-which(labels$panel==k&labels$role=='value_tick');labels$x_mm[ia]<-max(hh[ia]/2+.25,frames$left[k]-1.1-max(ww[it])-.7-hh[ia]/2)};ia<-which(labels$panel==6&labels$role=='axis'&labels$rotation==90);it<-which(labels$panel==6&labels$role=='group_tick');labels$x_mm[ia]<-frames$left[6]-1.1-max(ww[it])-.7-hh[ia]/2;boxes<-list();for(i in seq_len(nrow(labels))){z<-labels[i,];if(z$rotation==90)b<-c(z$x_mm-hh[i]/2,z$x_mm+hh[i]/2,z$y_mm-ww[i]/2,z$y_mm+ww[i]/2)else{left<-z$x_mm-if(z$just=='right')ww[i]else if(z$just=='centre')ww[i]/2 else 0;b<-c(left,left+ww[i],z$y_mm-hh[i]/2,z$y_mm+hh[i]/2)};if(b[1]<=0||b[2]>=width_mm||b[3]<=0||b[4]>=height_mm)stop('Label outside canvas: ',z$label,call.=FALSE);if(length(boxes)&&any(vapply(boxes,function(q)b[1]<q[2]-.05&&b[2]>q[1]+.05&&b[3]<q[4]-.05&&b[4]>q[3]+.05,logical(1))))stop('Label collision: ',z$label,' panel ',z$panel,' at ',z$x_mm,',',z$y_mm,'; enlarge canvas or reduce fonts',call.=FALSE);boxes[[i]]<-b}},finally={dev.off();unlink(probe)})
for(v in list(polys,lines,points))if(any(v$x_mm<=0|v$x_mm>=width_mm|v$y_mm<=0|v$y_mm>=height_mm))stop('Geometry outside canvas',call.=FALSE)
render<-function(){grid::grid.newpage();mm<-function(v)grid::unit(v,'mm');for(id in unique(polys$poly_id)){z<-polys[polys$poly_id==id,];grid::grid.polygon(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(fill=z$color[1],col=z$stroke[1],lwd=z$width_mm[1]*72.27/25.4))};for(id in unique(lines$line_id)){z<-lines[lines$line_id==id,];grid::grid.lines(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(col=z$color[1],lwd=z$width_mm[1]*72.27/25.4))};for(i in seq_len(nrow(legend_rects))){z<-legend_rects[i,];grid::grid.rect(mm((z$xmin+z$xmax)/2),mm((z$ymin+z$ymax)/2),mm(z$xmax-z$xmin),mm(z$ymax-z$ymin),gp=grid::gpar(fill=z$color,col='#333333',lwd=violin_width*72.27/25.4))};for(i in seq_len(nrow(points))){z<-points[i,];grid::grid.circle(mm(z$x_mm),mm(z$y_mm),r=mm(z$diameter_mm/2),gp=grid::gpar(fill=grDevices::adjustcolor(z$color,alpha.f=z$alpha),col=if(is.na(z$stroke))NA else grDevices::adjustcolor(z$stroke,alpha.f=z$alpha),lwd=z$width_mm*72.27/25.4))};for(i in seq_len(nrow(labels))){z<-labels[i,];grid::grid.text(z$label,mm(z$x_mm),mm(z$y_mm),just=z$just,rot=z$rotation,gp=grid::gpar(fontfamily=family,fontsize=z$size,col=z$color))}}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
for(n in c('observations','exclusions','summary','sex_summary','densities','sex_densities','frames','polys','lines','points','labels','legend_rects'))write.csv(get(n),file.path(output_dir,paste0('actual_',n,'.csv')),row.names=FALSE);saveRDS(mget(c('raw','observations','exclusions','summary','sex_summary','densities','sex_densities','frames','polys','lines','points','labels','legend_rects')),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(raw_rows=nrow(raw),included_rows=nrow(observations),excluded_rows=nrow(exclusions),missing_policy=policy,groups=as.list(groups),sexes=as.list(sexes),width_mm=width_mm,height_mm=height_mm,dpi=dpi,family=family,pdf_faces=pdf_faces,colors=as.list(sc),sex_colors=as.list(sx),bandwidth_mm=fixed_bw,bandwidth_adjust=bw_adjust,point_size=point_size,point_alpha=point_alpha,violin_line_width=violin_width,box_line_width=box_width,axis_title_size_pt=axis_size,axis_text_size_pt=tick_size,legend_text_size_pt=legend_size,domain=as.list(domain),horizontal_cat_domain=as.list(horizontal_cat_domain),box_cat_domain=as.list(box_cat_domain),axis_factor=axis_factor,value_axis_label=value_axis_label,scope='Palmer Penguins example; all raw retained with explicit common missing policy across six panels. Trimmed Gaussian KDE and area width normalization; category-only seeded offsets; exact-value dot stacks all observations; median and type7 equal-tail66/95 percent empirical descriptive intervals, not CI. Count-weighted slab pdf*n normalized jointly. Native implementation explicitly substitutes ggdist dot algorithm; not pixel-identical or upstream research certification.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered six panels from',nrow(observations),'included of',nrow(raw),'raw rows\n')
