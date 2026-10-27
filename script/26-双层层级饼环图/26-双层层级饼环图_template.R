#!/usr/bin/env Rscript

# True two-level record composition with inherited exploded geometry.
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
required_packages <- c("jsonlite", "svglite", "ragg", "readxl", "showtext", "sysfonts", "systemfonts")
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



data_path<-resolve_input(text_param('data_path','assets/data.txt'),'assets/data.txt');if(dir.exists(data_path))stop('Input must be a table file',call.=FALSE);sheet<-integer_param('sheet',1,1,20);raw<-read_table(data_path,sheet);aggregation<-choice('aggregation','records',c('records','value'));required<-c('sample','group1','group2','value');if(!nrow(raw)||nrow(raw)>10000||anyDuplicated(names(raw))||!all(required%in%names(raw)))stop('Complete sample/group1/group2/value table required; group3 and extra columns retained but not drawn',call.=FALSE)
observations<-data.frame(source_row=seq_len(nrow(raw)),sample=as.character(raw$sample),group1=as.character(raw$group1),group2=as.character(raw$group2),value=parse_numeric(raw$value,'value'),stringsAsFactors=FALSE);if(anyNA(observations)||any(!is.finite(observations$value))||any(abs(observations$value)>1e12)||any(!nzchar(trimws(unlist(observations[c('sample','group1','group2')]))))||anyDuplicated(observations$sample))stop('Complete unique row IDs/labels and finite raw values required; no row drops',call.=FALSE);if(aggregation=='value'&&any(observations$value<0))stop('Weighted composition requires nonnegative values; records mode never uses value as weight',call.=FALSE);observations$weight<-if(aggregation=='records')1 else observations$value
internal<-function(z)any(grepl('cover_[0-9]|Case[ _:#-]*[0-9]|案例[ ]*[0-9]',z,ignore.case=TRUE));if(internal(c(observations$group1,observations$group2)))stop('Internal debug categories cannot be rendered',call.=FALSE);mapping<-unique(observations[c('group1','group2')]);if(anyDuplicated(mapping$group1))stop('Each child label must map to exactly one parent',call.=FALSE);parent_map<-setNames(mapping$group2,mapping$group1)
ordered<-function(z,values,default,name){v<-if(nzchar(z))trimws(strsplit(z,',',fixed=TRUE)[[1]])else default;if(any(!nzchar(v))||anyDuplicated(v)||!setequal(v,values))stop(name,' must be a complete unique order',call.=FALSE);v};parents<-ordered(text_param('parent_order','',TRUE),observations$group2,sort(unique(observations$group2),method='radix'),'parent_order');children<-ordered(text_param('child_order','',TRUE),observations$group1,unlist(lapply(parents,function(p)sort(names(parent_map)[parent_map==p],method='radix')),use.names=FALSE),'child_order');if(!identical(as.character(rle(unname(parent_map[children]))$values),parents))stop('Child blocks must be contiguous and follow parent order',call.=FALSE);if(length(parents)>3||length(children)>5)stop('At most three parents and five children fit this reference',call.=FALSE)
selected<-function(z,values,name){if(z=='first')return(values[1]);if(!nzchar(z))return(character());v<-trimws(strsplit(z,',',fixed=TRUE)[[1]]);if(any(!nzchar(v))||anyDuplicated(v)||any(!v%in%values))stop(name,' must select present distinct labels, first or empty',call.=FALSE);v};parent_explode<-selected(text_param('explode_parent','first',TRUE),parents,'explode_parent');child_explode<-selected(text_param('explode_child','first',TRUE),children,'explode_child');total<-sum(observations$weight);if(!is.finite(total)||total<=0)stop('Positive finite total required; never invent percentages',call.=FALSE)
summaries<-list();for(level in 1:2){key<-paste0('group',level);gs<-if(level==1)children else parents;v<-vapply(gs,function(g)sum(observations$weight[observations[[key]]==g]),numeric(1));count<-vapply(gs,function(g)sum(observations[[key]]==g),integer(1));rel<-v/total;end<-cumsum(rel);summaries[[level]]<-data.frame(level=level,node=gs,parent=if(level==1)unname(parent_map[gs])else'',record_count=unname(count),value=unname(v),fraction=unname(rel),start=c(0,head(end,-1)),end=unname(end),percentage=sprintf('%.1f%%',100*rel),visible=v>0)};summaries<-do.call(rbind,summaries);rownames(summaries)<-NULL
for(p in parents){ch<-summaries[summaries$level==1&summaries$parent==p,];pa<-summaries[summaries$level==2&summaries$node==p,];if(abs(sum(ch$value)-pa$value)>1e-10*total||abs(min(ch$start)-pa$start)>1e-12||abs(max(ch$end)-pa$end)>1e-12)stop('Parent and child sums/angles must match',call.=FALSE)}
parent_colors<-toupper(c(text_param('parent_A_color','#E41A1C'),text_param('parent_B_color','#377EB8'),text_param('parent_C_color','#4DAF4A')));if(any(!grepl('^#[0-9A-F]{6}$',parent_colors))||anyDuplicated(parent_colors)||any(parent_colors=='#FFFFFF'))stop('Provide distinct visible six-digit HEX parent colors',call.=FALSE);parent_colors<-setNames(parent_colors[seq_along(parents)],parents)
# Children inherit the current parent color family; reference shades are retained
# for the source defaults, never attached to hardcoded child names.
child_colors<-setNames(character(length(children)),children);base_defaults<-c('#E41A1C','#377EB8','#4DAF4A');reference_shades<-list(c('#FFDFD6','#FFBCAA'),c('#A4BED9'),c('#8CCB80','#6ABA61'))
for(j in seq_along(parents)){p<-parents[j];nodes<-children[parent_map[children]==p];base<-unname(parent_colors[p]);if(base==base_defaults[j]&&length(nodes)==length(reference_shades[[j]])){shades<-reference_shades[[j]]}else{f<-if(length(nodes)==1).55 else seq(.78,.35,length.out=length(nodes));rgb<-grDevices::col2rgb(base)/255;shades<-vapply(f,function(a)grDevices::rgb(rgb[1]+(1-rgb[1])*a,rgb[2]+(1-rgb[2])*a,rgb[3]+(1-rgb[3])*a),character(1))};child_colors[nodes]<-toupper(shades)}

width_mm<-num('width_mm',105,100,180);height_mm<-num('height_mm',118,108,200);dpi<-integer_param('dpi',600,96,1200);r0<-num('inner_hole',.4,.3,.55);r1<-num('inner_radius',1,.8,1.2);r2<-num('outer_radius',1.6,1.4,1.9);if(r0>=r1||r1>=r2)stop('Require inner_hole < inner_radius < outer_radius',call.=FALSE);explode_pos<-num('explode_pos',.1,0,.18);start_angle<-num('start_angle',pi/2,-2*pi,2*pi);line_width<-num('line_width',.3,.2,.5);parent_size<-num('label_size_pt',10,8.5,11);child_size<-num('child_label_size_pt',8.5,7.5,9);center_size<-num('center_label_size_pt',17.5,12,18);title_size<-num('title_size_pt',12,10,14);alpha<-num('fill_alpha',.8,.65,1);external_threshold<-num('external_threshold',.1,0,.15);family_choice<-choice('family','Arial',c('Arial','Helvetica','sans','Times New Roman'));family<-if(family_choice=='sans')'Helvetica'else family_choice
clean<-function(z){if(internal(z)||tolower(z)%in%c('this is a title','title','debug'))''else z};title<-clean(text_param('title','',TRUE));if(!nzchar(title))title<-if(aggregation=='records')'Record composition'else'Weighted composition';center_label<-clean(text_param('center_label','',TRUE));if(!nzchar(center_label))center_label<-if(aggregation=='records')paste0('n = ',nrow(observations))else format(total,trim=TRUE,scientific=abs(total)>=1e4||total<.01,digits=3)
sectors<-summaries;sectors$a0<-90-start_angle*180/pi-360*sectors$start;sectors$a1<-90-start_angle*180/pi-360*sectors$end;sectors$mid<-(sectors$a0+sectors$a1)/2*pi/180;sectors$r0<-ifelse(sectors$level==2,r0,r1);sectors$r1<-ifelse(sectors$level==2,r1,r2);sectors$dx<-0;sectors$dy<-0;sectors$parent_selected<-FALSE;sectors$child_selected<-FALSE
for(i in seq_len(nrow(sectors))){s<-sectors[i,];p<-if(s$level==2)s$node else s$parent;parent_index<-which(sectors$level==2&sectors$node==p);if(p%in%parent_explode){sectors$dx[i]<-explode_pos*cos(sectors$mid[parent_index]);sectors$dy[i]<-explode_pos*sin(sectors$mid[parent_index]);sectors$parent_selected[i]<-TRUE};if(s$level==1&&s$node%in%child_explode){sectors$dx[i]<-sectors$dx[i]+explode_pos*cos(s$mid);sectors$dy[i]<-sectors$dy[i]+explode_pos*sin(s$mid);sectors$child_selected[i]<-TRUE}}
polys<-list();for(i in seq_len(nrow(sectors))){s<-sectors[i,];if(!s$visible)next;ang<-seq(s$a0,s$a1,length.out=max(3,ceiling(abs(s$a1-s$a0)/.5)+1))*pi/180;xx<-c(s$dx+s$r1*cos(ang),rev(s$dx+s$r0*cos(ang)));yy<-c(s$dy+s$r1*sin(ang),rev(s$dy+s$r0*sin(ang)));polys[[length(polys)+1]]<-data.frame(poly_id=length(polys)+1,vertex=seq_along(xx),level=s$level,node=s$node,x_unit=unname(xx),y_unit=unname(yy),color=if(s$level==2)unname(parent_colors[s$node])else unname(child_colors[s$node]),alpha=alpha,width_mm=line_width)};polys<-do.call(rbind,polys)
# Fit true translated geometry, including selected-child + inherited-parent displacement.
bx<-range(polys$x_unit);by<-range(polys$y_unit);side_margin<-if(any(sectors$level==1&sectors$visible&sectors$fraction<external_threshold))12 else 2;unit_scale<-min((width_mm-2*side_margin)/diff(bx),(height_mm-16)/diff(by));cx<-width_mm/2-mean(bx)*unit_scale;cy<-(height_mm-12)/2-mean(by)*unit_scale;polys$x_mm<-cx+polys$x_unit*unit_scale;polys$y_mm<-cy+polys$y_unit*unit_scale;sectors$cx_mm<-cx+sectors$dx*unit_scale;sectors$cy_mm<-cy+sectors$dy*unit_scale
labels<-list();lines<-list();label<-function(z,x,y,size,role,node='',level=0){labels[[length(labels)+1]]<<-data.frame(label=z,x_mm=unname(x),y_mm=unname(y),size=size,face='plain',just='centre',role=role,rotation=0,node=node,level=level)}
for(i in seq_len(nrow(sectors))){s<-sectors[i,];if(!s$visible)next;ext<-s$level==1&&s$fraction<external_threshold;rr<-if(ext)r2+.15 else(s$r0+s$r1)/2;x<-s$cx_mm+rr*cos(s$mid)*unit_scale;y<-s$cy_mm+rr*sin(s$mid)*unit_scale;sz<-if(s$level==2)parent_size else child_size;if(ext){lines[[length(lines)+1]]<-data.frame(line_id=length(lines)+1,vertex=1:2,node=s$node,x_mm=c(s$cx_mm+s$r1*cos(s$mid)*unit_scale,x),y_mm=c(s$cy_mm+s$r1*sin(s$mid)*unit_scale,y),color='#555555',width_mm=.18)};if(s$level==1){label(paste0(s$node,'(',s$percentage,')'),x,y,sz,'child',s$node,s$level)}else{gap<-sz*25.4/72*1.12;label(s$node,x,y+gap/2,sz,'parent',s$node,s$level);label(paste0('(',s$percentage,')'),x,y-gap/2,sz,'percentage',s$node,s$level)}}
center_x<-cx;center_y<-cy;if(length(parents)==1L){s<-sectors[sectors$level==2,];center_x<-s$cx_mm;center_y<-s$cy_mm};label(center_label,center_x,center_y,center_size,'center');label(title,width_mm/2,height_mm-5,title_size,'title');labels<-do.call(rbind,labels);lines<-if(length(lines))do.call(rbind,lines)else data.frame(line_id=integer(),vertex=integer(),node=character(),x_mm=numeric(),y_mm=numeric(),color=character(),width_mm=numeric())
font_file<-function(weight='normal',italic=FALSE){h<-systemfonts::match_fonts(family,weight=weight,italic=italic);if(h$index!=0L)h<-systemfonts::match_fonts('Arial',weight=weight,italic=italic);h$path};pdf_faces<-list(regular=font_file(),bold=font_file('bold'),italic=font_file(italic=TRUE),bolditalic=font_file('bold',TRUE));do.call(sysfonts::font_add,c(list(family=family),pdf_faces));probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin();tryCatch({tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontfamily=family,fontsize=s))),'mm',TRUE);ww<-mapply(tw,labels$label,labels$size);hh<-labels$size*25.4/72*.95;ci<-which(labels$role=='center');if(sqrt((ww[ci]/2)^2+(hh[ci]/2)^2)>r0*unit_scale-.5)stop('Center label must fit completely inside the hole; shorten it or reduce center_label_size_pt',call.=FALSE);if(any(labels$x_mm-ww/2<=0|labels$x_mm+ww/2>=width_mm|labels$y_mm-hh/2<=0|labels$y_mm+hh/2>=height_mm))stop('Complete labels do not fit; shorten labels or expand canvas explicitly',call.=FALSE);for(j in seq_len(nrow(labels)))if(j>1)for(k in seq_len(j-1))if(abs(labels$x_mm[j]-labels$x_mm[k])<(ww[j]+ww[k])/2-.05&&abs(labels$y_mm[j]-labels$y_mm[k])<(hh[j]+hh[k])/2-.05)stop('Label collision: ',labels$label[j],' / ',labels$label[k],'; expand canvas or choose shorter labels',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(){grid::grid.newpage();mm<-function(x)grid::unit(x,'mm');for(id in unique(polys$poly_id)){z<-polys[polys$poly_id==id,];grid::grid.polygon(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(fill=grDevices::adjustcolor(z$color[1],alpha.f=z$alpha[1]),col='white',lwd=z$width_mm[1]*72.27/25.4,linejoin='round'))};for(id in unique(lines$line_id)){z<-lines[lines$line_id==id,];grid::grid.lines(mm(z$x_mm),mm(z$y_mm),gp=grid::gpar(col=z$color[1],lwd=z$width_mm[1]*72.27/25.4))};for(i in seq_len(nrow(labels))){z<-labels[i,];grid::grid.text(z$label,mm(z$x_mm),mm(z$y_mm),just=z$just,gp=grid::gpar(fontfamily=family,fontsize=z$size,col='#111111'))}}
for(kind in c('png','pdf','svg')){file<-file.path(output_dir,paste0('plot.',kind));switch(kind,png=ragg::agg_png(file,width=width_mm,height=height_mm,units='mm',res=dpi,background='white'),pdf={pdf(file,width=width_mm/25.4,height=height_mm/25.4,useDingbats=FALSE);showtext::showtext_begin()},svg=svglite::svglite(file,width=width_mm/25.4,height=height_mm/25.4,bg='white'));tryCatch(render(),finally=dev.off())}
for(n in c('observations','summaries','sectors','polys','labels','lines'))write.csv(get(n),file.path(output_dir,paste0('actual_',n,'.csv')),row.names=FALSE);saveRDS(list(raw=raw,observations=observations,summaries=summaries,sectors=sectors,polys=polys,labels=labels,lines=lines,mapping=mapping),file.path(output_dir,'parsed-analysis.RDS'))
jsonlite::write_json(list(data_path=data_path,sheet=sheet,aggregation=aggregation,total=total,record_count=nrow(observations),parents=as.list(parents),children=as.list(children),parent_explode=as.list(parent_explode),child_explode=as.list(child_explode),colors=list(parent=as.list(parent_colors),child=as.list(child_colors)),width_mm=width_mm,height_mm=height_mm,dpi=dpi,family=family,pdf_faces=pdf_faces,label_size_pt=parent_size,child_label_size_pt=child_size,center_label_size_pt=center_size,title_size_pt=title_size,center_label=center_label,title=title,fill_alpha=alpha,line_width=line_width,external_threshold=external_threshold,inner_hole=r0,inner_radius=r1,outer_radius=r2,explode_pos=explode_pos,start_angle=start_angle,geometry=list(cx=cx,cy=cy,unit_scale=unit_scale,bx=as.list(bx),by=as.list(by)),scope='Default follows original PieDonut2 mapping without count: record totals n=57, A20/B10/C27, children10/10/10/16/11. Explicit value mode changes denominator to supplied value sum; no silent reweighting. Unique-parent contiguous hierarchy, common global denominator. Child inherits parent translation plus selected child radial displacement. Start/explode zero supported. Placeholder/debug titles removed before render. Original values explicitly illustrative; group3/extra columns preserved but unused.'),file.path(output_dir,'analysis_metadata.json'),auto_unbox=TRUE,pretty=TRUE,digits=15);cat('Rendered',aggregation,'composition of',nrow(observations),'unchanged records; total',total,'\n')
