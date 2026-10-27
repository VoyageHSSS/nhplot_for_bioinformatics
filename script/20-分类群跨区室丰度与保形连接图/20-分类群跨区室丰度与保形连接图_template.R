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



data_path<-resolve_input(text_param('data_path','assets/data.txt'),'assets/data.txt');if(dir.exists(data_path))stop('Table file required',call.=FALSE);sheet<-integer_param('sheet',1,1,20);raw<-read_table(data_path,sheet);required<-c('Compartment','Genus','RA');if(!nrow(raw)||nrow(raw)>500||anyDuplicated(names(raw))||!all(required%in%names(raw)))stop('Complete Compartment/Genus/RA table required',call.=FALSE);df<-raw[required];for(n in c('Compartment','Genus')){df[[n]]<-as.character(df[[n]]);if(anyNA(df[[n]])||any(!nzchar(trimws(df[[n]])))||any(df[[n]]!=trimws(df[[n]])))stop('Complete unpadded taxon/compartment identifiers required',call.=FALSE)};df$RA<-parse_numeric(df$RA,'RA');if(any(!is.finite(df$RA)|df$RA<0|df$RA>100))stop('RA must be complete percentages in [0,100]; no inferred rescaling or dropped records',call.=FALSE);if(anyDuplicated(df[c('Compartment','Genus')]))stop('Each taxon x compartment pair must be unique; no aggregate/duplicate observations inferred',call.=FALSE);if(any(grepl('cover_[0-9]|Case[ _:#-]*[0-9]|案例[ ]*[0-9]',c(df$Compartment,df$Genus),ignore.case=TRUE)))stop('Internal labels prohibited',call.=FALSE);df$source_row<-seq_len(nrow(df));include_bs<-bool('include_bs',FALSE);excluded<-df[df$Compartment=='BS'&!include_bs,,drop=FALSE];excluded$reason<-rep('BS explicitly excluded by include_bs=false',nrow(excluded));observations<-df[!(df$Compartment=='BS'&!include_bs),,drop=FALSE];if(!nrow(observations)||length(setdiff(df$Genus,observations$Genus)))stop('Selection removes all records or an entire taxon',call.=FALSE)
order_param<-function(z,v,maxn,default=NULL){ans<-if(nzchar(z))trimws(strsplit(z,',',fixed=TRUE)[[1]])else if(!is.null(default)&&setequal(v,default))default else sort(unique(v),method='radix');if(any(!nzchar(ans))||anyDuplicated(ans)||!setequal(ans,v)||length(ans)>maxn)stop('Complete unique order of observed categories required',call.=FALSE);ans};original_order<-c('RS','RE','VE','SE','LE','P');if(include_bs&&'BS'%in%observations$Compartment)original_order<-c(original_order,'BS');compartments<-order_param(text_param('compartment_order','',TRUE),observations$Compartment,8,original_order);taxa<-order_param(text_param('taxon_order','',TRUE),observations$Genus,7);if(length(compartments)<2||any(table(factor(observations$Genus,taxa),factor(observations$Compartment,compartments))!=1))stop('Complete taxon x compartment grid (>=2 compartments) required; never connect across missing records',call.=FALSE)
mode<-choice('curve_mode','pchip',c('pchip','linear'));steps<-integer_param('segment_steps',64,24,96);width_mm<-num('width_mm',150,140,260);height_mm<-num('height_mm',107,100,180);dpi<-integer_param('dpi',600,96,1200);line_width<-num('line_width',.4,.25,.8);point_size<-num('point_size',2.4,1.8,3.2);point_alpha<-num('point_alpha',1,.5,1);axis_size<-num('axis_title_size_pt',12,10,15);tick_size<-num('axis_text_size_pt',10,8,12);legend_size<-num('legend_text_size_pt',10,8,12);family_choice<-choice('font_family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice;show_background<-bool('show_background',TRUE);show_legend<-bool('show_legend',TRUE);x_title<-text_param('x_title','Compartments');y_title<-text_param('y_title','Relative abundance (%)');for(n in c('x_title','y_title'))if(grepl('cover_[0-9]|Case[ _:#-]*[0-9]|案例[ ]*[0-9]',get(n),ignore.case=TRUE))assign(n,if(n=='x_title')'Compartments'else'Relative abundance (%)')
palette<-text_param('palette','',TRUE);colors<-plot_nh_format_palette(if(nzchar(palette))toupper(trimws(strsplit(palette,',',fixed=TRUE)[[1]]))else c('#85BA8F','#A3C8DC','#349839','#EA5D2D','#000000','#F09594','#2072A8')[seq_along(taxa)], tryCatch(as.character(taxa), error = function(e) NULL), cfg);if(length(colors)!=length(taxa)||any(!grepl('^#[0-9A-F]{6}$',colors))||anyDuplicated(colors)||any(colors=='#FFFFFF'))stop('One distinct visible HEX color per taxon required; no repeated palette recycling',call.=FALSE);colors<-plot_nh_format_palette(setNames(colors,taxa), tryCatch(as.character(taxa), error = function(e) NULL), cfg);bg_top<-toupper(text_param('background_top','#EEDDCE'));bg_bottom<-toupper(text_param('background_bottom','#DBD4E7'));if(any(!grepl('^#[0-9A-F]{6}$',c(bg_top,bg_bottom))))stop('Background colors must be six-digit HEX',call.=FALSE)
# PCHIP with harmonic internal slopes and one-sided, sign-limited endpoints.
pchip_slopes<-function(x,y){n<-length(x);h<-diff(x);d<-diff(y)/h;if(n==2)return(rep(d,2));m<-numeric(n);for(j in 2:(n-1)){if(d[j-1]*d[j]<=0)m[j]<-0 else{w1<-2*h[j]+h[j-1];w2<-h[j]+2*h[j-1];m[j]<-(w1+w2)/(w1/d[j-1]+w2/d[j])}};endpoint<-function(h1,h2,d1,d2){v<-((2*h1+h2)*d1-h1*d2)/(h1+h2);if(sign(v)!=sign(d1))v<-0 else if(sign(d1)!=sign(d2)&&abs(v)>3*abs(d1))v<-3*d1;v};m[1]<-endpoint(h[1],h[2],d[1],d[2]);m[n]<-endpoint(h[n-1],h[n-2],d[n-1],d[n-2]);m}
curves<-list();slopes<-list();segments<-list();for(g in taxa){z<-observations[observations$Genus==g,];z$x<-match(z$Compartment,compartments);z<-z[order(z$x),];x<-z$x;y<-z$RA;m<-if(mode=='pchip')pchip_slopes(x,y)else rep(NA_real_,length(x));slopes[[length(slopes)+1]]<-data.frame(Genus=g,Compartment=z$Compartment,x=x,RA=y,slope=m)
for(j in seq_len(length(x)-1)){h<-x[j+1]-x[j];if(mode=='pchip'){a<-2*y[j]-2*y[j+1]+h*(m[j]+m[j+1]);b<--3*y[j]+3*y[j+1]-h*(2*m[j]+m[j+1]);c<-h*m[j];d<-y[j];roots<-if(abs(a)<1e-14){if(abs(b)<1e-14)numeric()else-c/(2*b)}else{dis<-4*b*b-12*a*c;if(dis<0)numeric()else(-2*b+c(-1,1)*sqrt(dis))/(6*a)};critical<-c(0,1,roots[roots>0&roots<1]);extreme<-((a*critical+b)*critical+c)*critical+d}else{a<-0;b<-0;c<-y[j+1]-y[j];d<-y[j];extreme<-y[c(j,j+1)]};tol<-max(1e-10,max(abs(y))*1e-12);if(any(extreme<min(y[c(j,j+1)])-tol|extreme>max(y[c(j,j+1)])+tol))stop('Connector violates observed endpoint bounds; never clamp negative/overshooting values',call.=FALSE);t<-seq(0,1,length.out=steps+1);v<-((a*t+b)*t+c)*t+d;v[c(1,length(v))]<-y[c(j,j+1)];segments[[length(segments)+1]]<-data.frame(Genus=g,segment=j,x0=x[j],x1=x[j+1],y0=y[j],y1=y[j+1],a=a,b=b,c=c,d=d,min_extreme=min(extreme),max_extreme=max(extreme));curves[[length(curves)+1]]<-data.frame(Genus=g,segment=j,vertex=seq_along(t),x=x[j]+h*t,RA=v,color=unname(colors[g]))}}
curves<-do.call(rbind,curves);slopes<-do.call(rbind,slopes);segments<-do.call(rbind,segments);peak<-max(observations$RA);if(peak==0)peak<-1;domain<-c(-.05*peak,1.05*peak);bounds<-c(left=.083*width_mm,right=.988*width_mm,bottom=.288*height_mm,top=.984*height_mm);nx<-length(compartments);cat_domain<-c(.4,nx+.6);fx<-function(v)bounds['left']+(v-cat_domain[1])/diff(cat_domain)*(bounds['right']-bounds['left']);fy<-function(v)bounds['bottom']+(v-domain[1])/diff(domain)*(bounds['top']-bounds['bottom']);curves$x_mm<-unname(fx(curves$x));curves$y_mm<-unname(fy(curves$RA));curves$width_mm<-line_width;observations$x_mm<-unname(fx(match(observations$Compartment,compartments)));observations$y_mm<-unname(fy(observations$RA));observations$color<-unname(colors[observations$Genus]);observations$diameter_mm<-point_size;observations$alpha<-point_alpha
background<-data.frame(ymin=seq(0,height_mm,length.out=61)[-61],ymax=seq(0,height_mm,length.out=61)[-1],color=if(show_background)grDevices::colorRampPalette(c(bg_bottom,'#FFFFFF',bg_top))(60)else rep('#FFFFFF',60));lines<-list();points<-list();labels<-list();label<-function(z,x,y,size,role,just='centre',rotation=0,color='#333C41'){labels[[length(labels)+1]]<<-data.frame(label=as.character(z),x_mm=unname(x),y_mm=unname(y),size=size,role=role,just=just,rotation=rotation,color=color,face='plain')};line<-function(x,y,role,color='#555555',lw=.25){lines[[length(lines)+1]]<<-data.frame(line_id=length(lines)+1,vertex=seq_along(x),x_mm=unname(x),y_mm=unname(y),color=color,width_mm=lw,role=role)};point<-function(x,y,color,role,diam=point_size,alpha=point_alpha){points[[length(points)+1]]<<-data.frame(x_mm=unname(x),y_mm=unname(y),color=color,role=role,diameter_mm=diam,alpha=alpha)}
line(c(bounds['left'],bounds['right'],bounds['right'],bounds['left'],bounds['left']),c(bounds['bottom'],bounds['bottom'],bounds['top'],bounds['top'],bounds['bottom']),'frame',lw=.25);vticks<-scales::breaks_extended(n=4)(domain);vticks<-vticks[vticks>=0&vticks<=domain[2]];for(v in vticks){line(c(bounds['left']-.8,bounds['left']),rep(fy(v),2),'tick',lw=.25);label(format(v,trim=TRUE,digits=4),bounds['left']-1.4,fy(v),tick_size,'ytick',just='right')};for(j in seq_along(compartments)){line(rep(fx(j),2),c(bounds['bottom']-.8,bounds['bottom']),'tick',lw=.25);label(compartments[j],fx(j),bounds['bottom']-2.7,tick_size,'xtick')};label(x_title,mean(bounds[c('left','right')]),min(.222*height_mm,bounds['bottom']-2.7-(axis_size+tick_size)*25.4/72*.575-.8),axis_size,'axis');label(y_title,3.8,mean(bounds[c('bottom','top')]),axis_size,'axis',rotation=90)
font_file<-function(weight='normal',italic=FALSE){h<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(h$index!=0L)h<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);h$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces));probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=s))),'mm',TRUE);if(show_legend){nc<-ceiling(length(taxa)/2);widths<-vapply(seq_len(nc),function(j)8+max(vapply(taxa[intersect(c(2*j-1,2*j),seq_along(taxa))],tw,numeric(1),s=legend_size)),numeric(1));total<-sum(widths)+3*(nc-1);if(total>width_mm-6)stop('Legend cannot fit; enlarge canvas/reduce font',call.=FALSE);starts<-(width_mm-total)/2+c(0,cumsum(head(widths,-1)+3));for(j in seq_along(taxa)){k<-ceiling(j/2);y<-if(j%%2==1).12*height_mm else .066*height_mm;x<-starts[k];line(c(x,x+5.5),rep(y,2),'legend',unname(colors[j]),line_width);point(x+2.75,y,unname(colors[j]),'legend');label(taxa[j],x+7.5,y,legend_size,'legend',just='left')}};labels<-do.call(rbind,labels);ww<-vapply(seq_len(nrow(labels)),function(i)tw(labels$label[i],labels$size[i]),numeric(1));hh<-labels$size*25.4/72*1.15;ia<-which(labels$role=='axis'&labels$rotation==90);it<-which(labels$role=='ytick');labels$x_mm[ia]<-max(hh[ia]/2+.25,bounds['left']-1.4-max(ww[it])-.7-hh[ia]/2);boxes<-list();for(i in seq_len(nrow(labels))){z<-labels[i,];if(z$rotation==90)b<-c(z$x_mm-hh[i]/2,z$x_mm+hh[i]/2,z$y_mm-ww[i]/2,z$y_mm+ww[i]/2)else{left<-z$x_mm-if(z$just=='right')ww[i]else if(z$just=='centre')ww[i]/2 else 0;b<-c(left,left+ww[i],z$y_mm-hh[i]/2,z$y_mm+hh[i]/2)};if(b[1]<=0||b[2]>=width_mm||b[3]<=0||b[4]>=height_mm)stop('Label outside canvas: ',z$label,call.=FALSE);if(length(boxes)&&any(vapply(boxes,function(q)b[1]<q[2]-.05&&b[2]>q[1]+.05&&b[3]<q[4]-.05&&b[4]>q[3]+.05,logical(1))))stop('Label collision: ',z$label,'; enlarge canvas/reduce fonts',call.=FALSE);boxes[[i]]<-b}},finally={dev.off();unlink(probe)})
lines<-do.call(rbind,lines);legend_points<-if(length(points))do.call(rbind,points)else data.frame(x_mm=numeric(),y_mm=numeric(),color=character(),role=character(),diameter_mm=numeric(),alpha=numeric());points<-rbind(data.frame(x_mm=observations$x_mm,y_mm=observations$y_mm,color=observations$color,role='observation',diameter_mm=point_size,alpha=point_alpha),legend_points)
plot_nh_parameter_context(cfg, plot_nh_format_output(cfg), groups = taxa, palette = colors)
render<-function(){grid::grid.newpage();mm<-function(v)grid::unit(v,'mm');for(i in seq_len(nrow(background))){z<-background[i,];grid::grid.rect(mm(width_mm/2),mm((z$ymin+z$ymax)/2),mm(width_mm),mm(z$ymax-z$ymin+.01),gp=grid::gpar(fill=z$color,col=NA))};for(g in taxa){z<-curves[curves$Genus==g,];grid::grid.lines(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(col=z$color[1],lwd=line_width*72.27/25.4,linejoin='round'))};for(id in unique(lines$line_id)){z<-lines[lines$line_id==id,];grid::grid.lines(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(col=z$color[1],lwd=z$width_mm[1]*72.27/25.4))};for(i in seq_len(nrow(points))){z<-points[i,];grid::grid.circle(mm(z$x_mm),mm(z$y_mm),r=mm(z$diameter_mm/2),gp=grid::gpar(fill=grDevices::adjustcolor(z$color,alpha.f=z$alpha),col=NA))};for(i in seq_len(nrow(labels))){z<-labels[i,];grid::grid.text(z$label,mm(z$x_mm),mm(z$y_mm),just=z$just,rot=z$rotation,gp=grid::gpar(fontfamily=family,fontsize=z$size,col=z$color))}}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
for(n in c('df','observations','excluded','curves','slopes','segments','background','lines','points','labels'))write.csv(get(n),file.path(output_dir,paste0('actual_',n,'.csv')),row.names=FALSE);saveRDS(mget(c('raw','df','observations','excluded','curves','slopes','segments','background','lines','points','labels')),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(raw_rows=nrow(raw),included_rows=nrow(observations),excluded_rows=nrow(excluded),include_bs=include_bs,compartments=as.list(compartments),taxa=as.list(taxa),curve_mode=mode,segment_steps=steps,width_mm=width_mm,height_mm=height_mm,dpi=dpi,family=family,pdf_faces=pdf_faces,colors=as.list(colors),line_width=line_width,point_size=point_size,point_alpha=point_alpha,axis_title_size_pt=axis_size,axis_text_size_pt=tick_size,legend_text_size_pt=legend_size,show_background=show_background,show_legend=show_legend,background_top=bg_top,background_bottom=bg_bottom,bounds=as.list(bounds),domain=as.list(domain),cat_domain=as.list(cat_domain),x_title=x_title,y_title=y_title,scope='49 original source records, explicit BS exclusion42 or include49, no silently filtered RA/unknown compartments/missing pairs. Supplied RA percentages retained [0,100], no rescaling/compositional sum assumptions. Genus is source column name; plotted names are taxon series, not certified common rank. PCHIP geometric connector includes all observed knots and analytically validated segment extrema within endpoint range; no negative clamping or overshoot. Categorical equal spacing is display order, not time/distance or measured between-compartment prediction. Not original ggalt xspline algorithm; no inference or provenance/biological-validity certification.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',nrow(observations),'of',nrow(raw),'records with',mode,'bounded categorical connectors\n')
