#!/usr/bin/env Rscript

# Actual matched expression/signature correlations with an explicit test family.
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
required_packages <- c("jsonlite", "svglite", "ragg", "readxl", "vegan", "permute")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_packages)) stop("Install required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE); params <- if (is.null(cfg$params)) list() else cfg$params
if(!is.list(cfg)||!is.list(params)||(!is.null(names(params))&&any(!nzchar(names(params)))))stop("config/params must be objects",call.=FALSE)
gp <- function(name, default = NULL) {value<-params[[name]];if(is.null(value))for(section in c("data","model","selection","colors","typography","geometry","axis","legend","layout","labels","statistics","analysis","export"))if(is.list(params[[section]])&&!is.null(params[[section]][[name]])){value<-params[[section]][[name]];break};if(is.null(value))default else value}
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














ids<-function(v,label){v<-as.character(v);if(anyNA(v)||any(!nzchar(v))||any(v!=trimws(v))||anyDuplicated(v)||any(grepl('[[:cntrl:]]',v)))stop(label,' require unique complete exact IDs',call.=FALSE);v}
env_path<-resolve_input(text_param('env_path','',TRUE),'assets/env.txt');otu_path<-resolve_input(text_param('otu_path','',TRUE),'assets/otu.txt');er<-read_table(env_path);orr<-read_table(otu_path);if(nrow(er)<5||nrow(er)>100||ncol(er)<3||ncol(er)>13||nrow(orr)<4||anyDuplicated(names(er))||anyDuplicated(names(orr)))stop('5..100 samples and 2..12 unique environmental variables required',call.=FALSE)
ei<-ids(er[[1]],'Environment samples');vars<-ids(names(er)[-1],'Environmental variables');oi<-ids(names(orr)[-1],'OTU samples');features<-ids(orr[[1]],'OTU features');if(!setequal(ei,oi))stop('Sample ID sets must match completely, no intersect dropping',call.=FALSE);E<-vapply(er[-1],parse_numeric,numeric(nrow(er)),label='Environment');dimnames(E)<-list(ei,vars);O<-t(vapply(orr[-1],parse_numeric,numeric(nrow(orr)),label='OTU counts'));dimnames(O)<-list(oi,features);O<-O[ei,,drop=FALSE];if(any(!is.finite(E))||any(apply(E,2,sd)==0))stop('Environmental variables must be finite and nonconstant',call.=FALSE);if(any(!is.finite(O))||any(O<0)||any(O!=floor(O)))stop('OTU counts must be finite nonnegative whole numbers',call.=FALSE)
group_path<-text_param('group_path','',TRUE);if(nzchar(group_path)){group_path<-resolve_input(group_path,'');mapping<-read_table(group_path);if(!all(c('group','feature')%in%names(mapping))||anyDuplicated(names(mapping)))stop('Group mapping requires group/feature fields',call.=FALSE);mapping$feature<-ids(mapping$feature,'Selected OTUs');mapping$group<-as.character(mapping$group);if(anyNA(mapping$group)||any(!nzchar(mapping$group))||any(mapping$group!=trimws(mapping$group))||any(grepl('[[:cntrl:]]',mapping$group)))stop('Complete group labels required',call.=FALSE)}else{wanted<-paste0('OTU',1:12);if(!all(wanted%in%features))stop('Default source groups require exact OTU1..OTU12; provide group_path for custom features',call.=FALSE);mapping<-data.frame(group=rep(c('OTU1-4','OTU5-8','OTU9-12'),each=4),feature=wanted)};if(!all(mapping$feature%in%features)||length(unique(mapping$group))<2||length(unique(mapping$group))>3)stop('2..3 disjoint actual OTU groups required, every selected feature must exist',call.=FALSE);groups<-unique(mapping$group);splits<-lapply(groups,function(g)mapping$feature[mapping$group==g]);names(splits)<-groups;xd<-lapply(splits,function(fs){z<-O[,fs,drop=FALSE];if(sum(rowSums(z)==0)>1)stop('Two zero-total rows within a selected group give undefined Bray distances',call.=FALSE);d<-suppressWarnings(vegan::vegdist(z,method='bray'));if(any(!is.finite(d)))stop('Undefined Bray distances; no sample dropping',call.=FALSE);if(sd(as.vector(d))==0)stop('Group Bray-Curtis distances are constant',call.=FALSE);d})
method<-choice('cor_method','pearson',c('pearson','spearman'));adjust<-choice('p_adjust','BH',c('BH','none'));seed<-integer_param('seed',363,1,2147483647);nperm<-integer_param('permutations',999,199,9999);blocks_path<-text_param('blocks_path','',TRUE);blocks<-NULL;if(nzchar(blocks_path)){blocks_path<-resolve_input(blocks_path,'');br<-read_table(blocks_path);if(!all(c('sample','block')%in%names(br)))stop('Blocks table requires sample/block',call.=FALSE);bi<-ids(br$sample,'Block samples');if(!setequal(bi,ei)||anyNA(br$block)||any(!nzchar(br$block))||any(br$block!=trimws(br$block)))stop('Blocks must match all samples with complete exact labels',call.=FALSE);blocks<-factor(br$block[match(ei,bi)]);if(all(table(blocks)==1))stop('No exchangeable observations within blocks',call.=FALSE)};set.seed(seed);perms<-suppressMessages(permute::shuffleSet(nrow(E),nset=nperm,control=permute::how(blocks=blocks)));if(nrow(perms)<199)stop('Fewer than 199 available permutations; resolution insufficient for this display',call.=FALSE)
cm<-cor(E,method=method);corr<-expand.grid(row=seq_along(vars),column=seq_along(vars));corr<-corr[corr$row<=corr$column,,drop=FALSE];corr$variable_row<-vars[corr$row];corr$variable_column<-vars[corr$column];corr$r<-cm[cbind(corr$row,corr$column)];corr$p<-NA_real_;off<-which(corr$row!=corr$column);for(i in off)corr$p[i]<-suppressWarnings(cor.test(E[,corr$row[i]],E[,corr$column[i]],method=method,exact=FALSE)$p.value);corr$q<-NA_real_;corr$q[off]<-p.adjust(corr$p[off],'BH');corr$display_p<-if(adjust=='BH')corr$q else corr$p;corr$stars<-ifelse(is.na(corr$display_p),'',ifelse(corr$display_p<.001,'***',ifelse(corr$display_p<.01,'**',ifelse(corr$display_p<.05,'*',''))))
mantel<-expand.grid(group=groups,variable=vars,stringsAsFactors=FALSE);mantel$r<-mantel$p<-NA_real_;mantel_objects<-vector('list',nrow(mantel));for(i in seq_len(nrow(mantel))){fit<-vegan::mantel(xd[[mantel$group[i]]],dist(E[,mantel$variable[i],drop=FALSE]),method='pearson',permutations=perms,parallel=1);mantel_objects[[i]]<-fit;mantel$r[i]<-unname(fit$statistic);mantel$p[i]<-fit$signif};mantel$q<-p.adjust(mantel$p,'BH');mantel$display_p<-if(adjust=='BH')mantel$q else mantel$p;mantel$width_mm<-ifelse(mantel$r<.25,.28,ifelse(mantel$r<.5,.5,.75));mantel$color<-ifelse(mantel$display_p<.01,'#E69F00',ifelse(mantel$display_p<.05,'#9B59B6','#B8BFC6'));mantel$lty<-ifelse(mantel$display_p<.05,'solid','dashed')
width_mm<-num('width_mm',210,200,300);height_mm<-num('height_mm',180,170,260);dpi<-integer_param('dpi',600,96,1200);family_choice<-choice('family','Helvetica',c('Helvetica','Arial','sans','Times New Roman'));family<-if(family_choice=='Times New Roman')'serif'else'sans';label_size<-num('label_size_pt',8.5,7.5,10);border_width<-num('tile_border_width',.18,.1,.4);negative<-text_param('negative_color','#7CAAFF');positive<-text_param('positive_color','#FE6263');cc<-tryCatch(col2rgb(c(negative,positive),alpha=TRUE),error=function(e)stop('Invalid correlation colors',call.=FALSE));if(any(cc[4,]!=255))stop('Opaque correlation colors required',call.=FALSE)
# ponytail: fixed triangular topology for at most twelve variables; separate pages if a larger family is needed.
step<-min((width_mm-54)/length(vars),(height_mm-48)/length(vars));left<-width_mm-16-length(vars)*step;top<-height_mm-23;bottom<-top-length(vars)*step;corr$x_mm<-left+(corr$column-.5)*step;corr$y_mm<-top-(corr$row-.5)*step;corr$side_mm<-sqrt(abs(corr$r))*step;ramp<-colorRamp(c(negative,'#FFFFFF',positive),space='Lab');rgbv<-ramp((corr$r+1)/2);corr$fill<-rgb(rgbv[,1],rgbv[,2],rgbv[,3],maxColorValue=255);gy<-top-c(.30,.59,.87)[seq_along(groups)]*length(vars)*step;gx<-c(24,left+length(vars)*step*.05,left+length(vars)*step*.33)[seq_along(groups)];mantel$x0_mm<-gx[match(mantel$group,groups)];mantel$y0_mm<-gy[match(mantel$group,groups)];mantel$x1_mm<-left+(match(mantel$variable,vars)-1)*step-3;mantel$y1_mm<-top-(match(mantel$variable,vars)-.5)*step
group_totals<-do.call(rbind,lapply(groups,function(g)data.frame(group=g,sample=ei,total=rowSums(O[,splits[[g]],drop=FALSE]))));empty_rows<-group_totals[group_totals$total==0,,drop=FALSE]
caption<-c(sprintf('%d matched samples; %d selected OTUs; %d others excluded; Bray-Curtis vs each single environmental variable.',nrow(E),nrow(mapping),length(setdiff(features,mapping$feature))),sprintf('Mantel: positive one-sided Pearson; %d shared permutations, seed %d, %s; colors/stars use %s.',nrow(perms),seed,if(is.null(blocks))'free (exchangeability unverified)'else'within supplied blocks',if(adjust=='BH')'BH q'else'raw p'),sprintf('Separate BH families: %d links / %d pairs; no diagonal test. Zero-total group rows: %d (Bray = 1 vs nonempty).',nrow(mantel),length(off),nrow(empty_rows)))
probe<-tempfile(fileext='.pdf');pdf(probe,width=width_mm/25.4,height=height_mm/25.4);tryCatch({grid::grid.newpage();tw<-function(z,s)grid::convertWidth(grid::grobWidth(grid::textGrob(z,gp=grid::gpar(fontsize=s,fontfamily=family))),'mm',TRUE);if(any(vapply(vars,tw,numeric(1),s=label_size)>15)||any(vapply(groups,tw,numeric(1),s=label_size)>25)||any(vapply(caption,tw,numeric(1),s=6)>width_mm-12))stop('Complete labels exceed layout; use larger canvas or shorter provided names',call.=FALSE)},finally={dev.off();unlink(probe)})
render<-function(kind){path<-file.path(output_dir,paste0('plot.',kind));if(kind=='png')ragg::agg_png(path,width=width_mm,height=height_mm,units='mm',res=dpi,bg='white')else if(kind=='pdf')pdf(path,width=width_mm/25.4,height=height_mm/25.4,family=if(family=='serif')'Times'else'Helvetica',useDingbats=FALSE)else svglite::svglite(path,width=width_mm/25.4,height=height_mm/25.4);tryCatch({grid::grid.newpage();mm<-function(z)grid::unit(z,'mm');txt<-function(z,x,y,s=7,face='plain',just='center',rot=0)grid::grid.text(z,mm(x),mm(y),just=just,rot=rot,gp=grid::gpar(fontfamily=family,fontsize=s,fontface=face,col='#25313C'));ln<-function(x,y,col='#25313C',w=.3,lty='solid')grid::grid.lines(mm(x),mm(y),gp=grid::gpar(col=col,lwd=w*72.27/25.4,lty=lty));rect<-function(x,y,w,h,fill=NA,col=NA,lw=0)grid::grid.rect(mm(x),mm(y),mm(w),mm(h),gp=grid::gpar(fill=fill,col=col,lwd=lw*72.27/25.4));txt('OTU groups and individual environmental associations',6,height_mm-6,10.5,'bold','left');txt(paste0('Environment: ',method,'; square area = |r|; no causal interpretation'),6,height_mm-12,7,just='left');for(i in order(mantel$display_p,decreasing=TRUE)){m<-mantel[i,];dx<-m$x1_mm-m$x0_mm;dy<-m$y1_mm-m$y0_mm;grid::grid.bezier(mm(c(m$x0_mm,m$x0_mm+dx/3-dy*.16,m$x0_mm+dx*2/3-dy*.16,m$x1_mm)),mm(c(m$y0_mm,m$y0_mm+dy/3+dx*.16,m$y0_mm+dy*2/3+dx*.16,m$y1_mm)),gp=grid::gpar(col=m$color,lwd=m$width_mm*72.27/25.4,lty=m$lty))};rect(corr$x_mm,corr$y_mm,step,step,'#FFFFFF','#AEB5BC',border_width);rect(corr$x_mm,corr$y_mm,corr$side_mm,corr$side_mm,corr$fill,'#7B838A',border_width);for(i in off)if(nzchar(corr$stars[i]))txt(corr$stars[i],corr$x_mm[i],corr$y_mm[i],8,'bold');for(j in seq_along(vars)){txt(vars[j],left+(j-.5)*step,top+5,label_size,'bold',rot=45);txt(vars[j],left+length(vars)*step+2,top-(j-.5)*step,label_size,'bold','left');grid::grid.circle(mm(left+(j-1)*step-3),mm(top-(j-.5)*step),r=mm(.8),gp=grid::gpar(fill='#FFFFFF',col='#475767',lwd=.35*72.27/25.4))};for(j in seq_along(groups)){grid::grid.circle(mm(gx[j]),mm(gy[j]),r=mm(1.15),gp=grid::gpar(fill='#0072B2',col=NA));gw<-grid::convertWidth(grid::grobWidth(grid::textGrob(groups[j],gp=grid::gpar(fontfamily=family,fontsize=label_size,fontface='bold'))),'mm',TRUE);rect(gx[j]-2-gw/2,gy[j]+4,gw+1,4.4,'#FFFFFF');txt(groups[j],gx[j]-2,gy[j]+4,label_size,'bold','right')}
 txt('Mantel r',6,39,7.5,'bold','left');for(j in 1:3){yy<-34-(j-1)*4;ln(c(6,14),c(yy,yy),w=c(.28,.5,.75)[j]);txt(c('< 0.25 (includes negative)','0.25 to < 0.50','>= 0.50')[j],16,yy,6.5,just='left')};txt(if(adjust=='BH')'Mantel BH q'else'Mantel raw p',66,39,7.5,'bold','left');for(j in 1:3){yy<-34-(j-1)*4;ln(c(66,74),c(yy,yy),c('#E69F00','#9B59B6','#B8BFC6')[j],.5,if(j==3)'dashed'else'solid');txt(c('< 0.01','0.01 to < 0.05','>= 0.05')[j],76,yy,6.5,just='left')};txt(paste0(method,' r'),118,39,7.5,'bold','left');for(v in seq(-1,1,length.out=101)){co<-ramp((v+1)/2);rect(118+(v+1)*23,32,46/101+.015,3,rgb(co[1],co[2],co[3],maxColorValue=255))};for(v in c(-1,0,1))txt(v,118+(v+1)*23,27,6.5);txt('* < .05; ** < .01; *** < .001',118,22,6.5,just='left');for(j in seq_along(caption))txt(caption[j],6,13-(j-1)*3.4,6,just='left')},finally=dev.off())};for(kind in c('png','pdf','svg'))render(kind)
utils::write.csv(group_totals,file.path(output_dir,'group_sample_totals.csv'),row.names=FALSE);utils::write.csv(corr,file.path(output_dir,'environment_correlations.csv'),row.names=FALSE);utils::write.csv(mantel,file.path(output_dir,'mantel_links.csv'),row.names=FALSE);utils::write.csv(mapping,file.path(output_dir,'selected_groups.csv'),row.names=FALSE);utils::write.csv(data.frame(feature=setdiff(features,mapping$feature)),file.path(output_dir,'excluded_features.csv'),row.names=FALSE);utils::write.csv(perms,file.path(output_dir,'permutation_indices.csv'),row.names=FALSE);saveRDS(list(E=E,O=O,mapping=mapping,xd=xd,perms=perms,corr=corr,mantel=mantel,fits=mantel_objects,blocks=blocks),file.path(output_dir,'parsed-analysis.RDS'));jsonlite::write_json(list(env_path=env_path,otu_path=otu_path,group_path=group_path,blocks_path=blocks_path,samples=nrow(E),variables=vars,groups=groups,selected_features=mapping$feature,excluded_features=setdiff(features,mapping$feature),cor_method=method,p_adjust=adjust,seed=seed,permutations=nrow(perms),zero_total_group_rows=empty_rows,exchangeability=if(is.null(blocks))'Free, unverified'else'Supplied blocks, other design assumptions unverified',geometry=list(width_mm=width_mm,height_mm=height_mm,step_mm=step,left_mm=left,top_mm=top,bottom_mm=bottom),caption=caption),file.path(output_dir,'analysis_metadata.json'),pretty=TRUE,auto_unbox=TRUE,digits=15);cat('Rendered',nrow(mantel),'actual per-variable Mantel tests and',nrow(corr),'upper-triangle correlations\n')
