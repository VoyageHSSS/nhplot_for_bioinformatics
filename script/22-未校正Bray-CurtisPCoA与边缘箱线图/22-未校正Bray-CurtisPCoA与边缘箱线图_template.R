#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  flag <- paste0("--", name)
  inline <- args[startsWith(args, paste0(flag, "="))]
  if (length(inline)) return(sub(paste0(flag, "="), "", inline[[1]], fixed = TRUE))
  index <- match(flag, args)
  if (!is.na(index) && index < length(args)) return(args[[index + 1L]])
  default
}
script <- if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) sys.frame(1)$ofile else
  sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
folder <- dirname(normalizePath(script))
color_module <- Sys.getenv("PLOT_NH_COLOR_MODULE", "")
if (!file.exists(color_module)) {
  root <- folder
  while (!file.exists(file.path(root, "manual_library/scripts/plot_nh_colors.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Shared color module not found")
    root <- parent
  }
  color_module <- file.path(root, "manual_library/scripts/plot_nh_colors.R")
}
source(color_module)
plot_nh_color_context()
config_path <- arg_value("config", file.path(folder, "config.json"))
cfg <- if (file.exists(config_path)) jsonlite::read_json(config_path, simplifyVector = TRUE) else list()
params <- if (is.null(cfg$params)) list() else cfg$params
gp <- function(name, default) if (is.null(params[[name]])) default else params[[name]]
p<-params
o<-arg_value("output-dir",cfg$output_dir);if(is.null(o))o<-file.path(folder,"runtime/output");dir.create(o,recursive=TRUE,showWarnings=FALSE)
cfg_value<-function(n,d=NULL){value<-cfg[[n]];if(is.null(value)&&!is.null(p[[n]]))value<-p[[n]];if(is.null(value))d else value}
template_dir<-folder
resolve_input<-function(path){if(is.null(path)||!nzchar(path))return(file.path(template_dir,"assets","data.xls"));if(file.exists(path))return(path);if(!grepl("^/",path))for(candidate in c(file.path(template_dir,path),file.path(getwd(),path)))if(file.exists(candidate))return(candidate);path}
library(readr);library(dplyr);library(ggplot2);library(vegan);library(patchwork);library(RColorBrewer)
if (!requireNamespace("multcompView", quietly=TRUE)) stop("Missing R package: multcompView")
raw_df <- read_tsv(resolve_input(gp("data_path",file.path(template_dir,"assets/data.xls"))),show_col_types=FALSE)
sample_candidates <- c("Sample_id", "Sample_ID", "sample_id", "SampleID", "sample")
sample_col <- sample_candidates[sample_candidates %in% names(raw_df)][1]
if (is.na(sample_col)) stop("Input data must contain a sample identifier column; tried: ", paste(sample_candidates, collapse=", "))
if (!"Subtype" %in% names(raw_df)) {
  subtype_candidates <- c("Subtype", "subtype", "Group", "group", "Condition", "condition")
  subtype_col <- subtype_candidates[subtype_candidates %in% names(raw_df)][1]
  if (is.na(subtype_col)) stop("Input data must contain a grouping column; tried: ", paste(subtype_candidates, collapse=", "))
  names(raw_df)[names(raw_df) == subtype_col] <- "Subtype"
}
features <- setdiff(names(raw_df),c(sample_col,'Subtype'))
if(anyNA(raw_df[[sample_col]]) || anyDuplicated(raw_df[[sample_col]]) || any(!nzchar(trimws(as.character(raw_df[[sample_col]])))))stop('Sample IDs must be complete nonblank and unique')
if(anyNA(raw_df$Subtype) || any(!nzchar(trimws(raw_df$Subtype))))stop('Subtype must be complete and nonblank')
if(!length(features) || !all(vapply(raw_df[features],function(z)is.numeric(z)&&all(is.finite(z))&&all(z>=0),logical(1))))stop('Bray inputs must be complete finite nonnegative numeric features')
if(any(rowSums(as.matrix(raw_df[features]))==0))stop('All-zero sample profiles are not supported')
df<-raw_df%>%rename(sample=all_of(sample_col))%>%mutate(Subtype=gsub("-","_",Subtype))
stable_group_order <- c("SCNC_A", "SCNC_N", "SCNC_P", "SCNC_Y")
observed_groups <- unique(as.character(df$Subtype))
groups <- c(intersect(stable_group_order, observed_groups), sort(setdiff(observed_groups, stable_group_order)))
if (length(groups)<2) stop("Input requires at least two Subtype groups")
group_n <- table(factor(df$Subtype,levels=groups))
df$Subtype <- factor(as.character(df$Subtype), levels=groups)
df2<-as.data.frame(df);rownames(df2)<-df2$sample;df2$sample<-NULL;distance<-vegdist(df2%>%select(-Subtype),method="bray");pc<-ape::pcoa(distance,correction="none");if(ncol(pc$vectors)<2)stop("At least two positive PCoA axes are required");xy<-data.frame(PC1=pc$vectors[,1],PC2=pc$vectors[,2])%>%tibble::rownames_to_column("sample")%>%left_join(select(df,sample,Subtype),by="sample")
original_colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(groups))
palette_id <- as.character(cfg_value("palette_id", "Original"))[1]
is_original <- is.na(palette_id) || !nzchar(palette_id) || tolower(palette_id) %in% c("original", "template_original", "default")
clean_hex <- function(value, label) {
  values <- trimws(as.character(unlist(value, use.names=FALSE)))
  if (!length(values) || any(!grepl("^#[0-9A-Fa-f]{6,8}$", values))) {
    stop(label, " must contain valid HEX colors such as #123456")
  }
  toupper(values)
}
mapping_raw <- cfg_value("palette_mapping", NULL)
mapping_values <- if (is.null(mapping_raw)) character() else unlist(mapping_raw, use.names=TRUE)
mapping_names <- names(mapping_values)
mapping_values <- as.character(mapping_values)
names(mapping_values) <- mapping_names
if (!is_original && length(mapping_values)) {
  if (is.null(mapping_names) || any(!nzchar(mapping_names))) {
    stop("palette_mapping must be a named mapping keyed by group names")
  }
  mapping_values <- clean_hex(mapping_values, "palette_mapping")
  names(mapping_values) <- mapping_names
  missing_groups <- setdiff(groups, names(mapping_values))
  if (length(missing_groups)) stop("palette_mapping is missing groups: ", paste(missing_groups, collapse=", "))
  cols <- unname(mapping_values[groups])
} else if (is_original) {
  cols <- original_colors
} else {
  palette_colors <- gp("palette_colors", "#E64B35,#4DBBD5,#00A087,#3C5488,#F39B7F,#8491B4")
  palette_colors <- trimws(unlist(strsplit(gsub("['\" ]", "", as.character(palette_colors)), "[,;]")))
  palette_colors <- clean_hex(palette_colors, "palette_colors")
  cols <- grDevices::colorRampPalette(palette_colors)(length(groups))
}
names(cols) <- groups
point_size<-as.numeric(gp("point_size",2.2)); point_alpha<-as.numeric(gp("point_alpha",.9)); point_shape<-as.numeric(gp("point_shape",16)); point_stroke<-as.numeric(gp("point_stroke",.3)); point_border<-as.character(gp("point_border",NA)); if(!length(point_border)||!nzchar(point_border)) point_border<-NA_character_
base_size<-as.numeric(gp("base_size",8)); axis_text_size<-as.numeric(gp("axis_text_size",7.5)); axis_title_size<-as.numeric(gp("axis_title_size",8.5)); legend_text_size<-as.numeric(gp("legend_text_size",7)); line_width<-as.numeric(gp("line_width",.25)); box_width<-as.numeric(gp("box_width",.5)); box_linewidth<-as.numeric(gp("box_linewidth",line_width)); box_linetype<-as.character(gp("box_linetype","solid")); show_grid<-isTRUE(gp("show_grid",TRUE)); show_zero_lines<-isTRUE(gp("show_zero_lines",TRUE)); show_marginal<-isTRUE(gp("show_marginal",TRUE)); show_letters<-isTRUE(gp("show_letters",TRUE)); letter_size<-as.numeric(gp("letter_size",7.5/ggplot2::.pt))
theme_name<-as.character(gp("theme", "bw")); theme_base<-if(theme_name=="classic") theme_classic(base_size=base_size) else if(theme_name=="minimal") theme_minimal(base_size=base_size) else theme_bw(base_size=base_size,base_family=as.character(gp("family","Helvetica")))
theme_base<-theme_base+theme(panel.grid.major=if(show_grid)element_line(colour="#E4E4E4",linewidth=.15) else element_blank(),panel.grid.minor=element_blank(),text=element_text(family=as.character(gp("family","Helvetica"))),plot.margin=margin(1,1,1,1,"mm"))
legend_position<-as.character(gp("legend_position","bottom")); legend_title<-as.character(gp("legend_title","")); show_legend<-isTRUE(gp("show_legend",TRUE))
axis_x_title<-as.character(gp("x_title",paste0("PCoA 1 (raw rel. eig. ",round(pc$values$Relative_eig[1]*100,2),"%)"))); axis_y_title<-as.character(gp("y_title",paste0("PCoA 2 (raw rel. eig. ",round(pc$values$Relative_eig[2]*100,2),"%)")))
if (identical(axis_x_title, "PC1")) axis_x_title <- paste0("PCoA 1 (raw rel. eig. ", round(pc$values$Relative_eig[1]*100,2), "%)")
if (identical(axis_y_title, "PC2")) axis_y_title <- paste0("PCoA 2 (raw rel. eig. ", round(pc$values$Relative_eig[2]*100,2), "%)")
axis_x_min<-gp("x_min",NA); axis_x_max<-gp("x_max",NA); axis_y_min<-gp("y_min",NA); axis_y_max<-gp("y_max",NA); finite_axis_pair<-function(lo,hi) { values<-suppressWarnings(as.numeric(c(lo,hi))); all(is.finite(values)) && values[1] < values[2] && all(abs(values) < 1e5) }; limx<-if(finite_axis_pair(axis_x_min,axis_x_max)) as.numeric(c(axis_x_min,axis_x_max)) else NULL; limy<-if(finite_axis_pair(axis_y_min,axis_y_max)) as.numeric(c(axis_y_min,axis_y_max)) else NULL
common_theme<-theme_base+theme(axis.title=element_text(size=axis_title_size,face="bold",colour="black"),axis.text=element_text(size=axis_text_size,face="bold",colour="black"),panel.grid.major=if(show_grid) element_line(colour="#E4E4E4",linewidth=.15) else element_blank(),text=element_text(family=as.character(gp("family","Helvetica"))),legend.title=element_blank(),panel.grid.minor=element_blank(),legend.text=element_text(size=legend_text_size),plot.margin=margin(1,1,1,1,"mm"),legend.key=element_blank(),legend.background=element_blank(),legend.box.background=element_rect(colour="black",fill=NA,linewidth=.25),legend.spacing.x=grid::unit(.1,"cm"),legend.key.width=grid::unit(3,"mm"),legend.key.height=grid::unit(3,"mm"))
if (!show_legend || identical(legend_position,"none")) {
  common_theme <- common_theme + theme(legend.position="none")
} else if (identical(legend_position,"inside")) {
  common_theme <- common_theme + theme(legend.position="inside",legend.position.inside=c(.012,.985),legend.justification=c(0,1))
} else {
  common_theme <- common_theme + theme(legend.position=legend_position)
}
zero_layers<-if(show_zero_lines) list(geom_vline(xintercept=0,linetype=as.character(gp("zero_linetype","dotted")),linewidth=line_width),geom_hline(yintercept=0,linetype=as.character(gp("zero_linetype","dotted")),linewidth=line_width)) else list()
point_args<-list(size=point_size,alpha=point_alpha,shape=point_shape,stroke=point_stroke); if(!is.na(point_border)) point_args$colour<-point_border
legend_name<-if(nzchar(legend_title)) legend_title else NULL
legend_labels<-paste0(groups," (n=",as.integer(group_n),")")
q0<-ggplot(xy,aes(PC1,PC2,color=Subtype,fill=Subtype))+do.call(geom_point,point_args)+scale_color_manual(values=cols,name=legend_name,labels=legend_labels)+scale_fill_manual(values=cols,guide="none")+labs(x=axis_x_title,y=axis_y_title)+common_theme
if(length(zero_layers)) q0 <- q0 + zero_layers[[1]] + zero_layers[[2]]
if(is.null(limx))limx<-range(xy$PC1)+c(-.08,.12)*diff(range(xy$PC1))
if(is.null(limy))limy<-range(xy$PC2)+c(-.08,.12)*diff(range(xy$PC2))
q0 <- q0 + scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+coord_fixed(ratio=1,xlim=limx,ylim=limy,expand=FALSE)+guides(colour=guide_legend(nrow=1))
letter_table <- function(response) {
  form <- stats::reformulate("Subtype", response=response)
  fit <- stats::aov(form, data=xy)
  tukey <- stats::TukeyHSD(fit)
  letters <- multcompView::multcompLetters4(fit, tukey)$Subtype$Letters
  maxima <- xy %>% group_by(Subtype) %>% summarise(value_max=max(.data[[response]],na.rm=TRUE),.groups="drop")
  left_join(data.frame(Subtype=factor(names(letters),levels=groups),Letters=unname(letters)),maxima,by="Subtype")
}
text1<-letter_table("PC1"); text2<-letter_table("PC2")
pc1_offset<-diff(range(xy$PC1,na.rm=TRUE))*.035; pc2_offset<-diff(range(xy$PC2,na.rm=TRUE))*.035
bx<-ggplot(xy,aes(Subtype,PC1,fill=Subtype))+(if(show_marginal) geom_boxplot(outlier.shape=16,outlier.size=.6,width=box_width,linewidth=box_linewidth,colour="black",linetype=box_linetype) else geom_blank())+(if(show_marginal&&show_letters) geom_text(data=text1,aes(x=Subtype,y=value_max+pc1_offset,label=Letters),angle=-90,colour="black",size=letter_size,family=as.character(gp("family","Helvetica")),inherit.aes=FALSE) else geom_blank())+scale_fill_manual(values=cols,guide="none")+theme_base+theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),legend.position="none",panel.border=element_rect(colour="black",fill=NA,linewidth=.25))+scale_y_continuous(expand=c(0,0))+coord_flip(xlim=c(.5,length(groups)+.5),ylim=limx,expand=FALSE,clip="on")
by<-ggplot(xy,aes(Subtype,PC2,fill=Subtype))+(if(show_marginal) geom_boxplot(outlier.shape=16,outlier.size=.6,width=box_width,linewidth=box_linewidth,colour="black",linetype=box_linetype) else geom_blank())+(if(show_marginal&&show_letters) geom_text(data=text2,aes(x=Subtype,y=value_max+pc2_offset,label=Letters),colour="black",size=letter_size,family=as.character(gp("family","Helvetica")),inherit.aes=FALSE) else geom_blank())+scale_fill_manual(values=cols,guide="none")+theme_base+theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),legend.position="none",panel.border=element_rect(colour="black",fill=NA,linewidth=.25))+scale_y_continuous(expand=c(0,0))+coord_cartesian(xlim=c(.5,length(groups)+.5),ylim=limy,expand=FALSE,clip="on")
set.seed(20261003)
perm <- vegan::adonis2(df2 %>% select(-Subtype) ~ Subtype, data=df, distance="bray",permutations=999)
ann <- paste0(as.character(gp("permanova_label","PERMANOVA")),"\nDf = ", perm$Df[1], "\nR2 = ", round(perm$R2[1], 5), "\np-value = ", format.pval(perm$`Pr(>F)`[1], digits=3, eps=1e-3))
bp <- if(isTRUE(gp("show_permanova",TRUE))) ggplot() + annotate("text", x=0, y=0, label=ann, fontface=as.character(gp("permanova_fontface","bold")), size=as.numeric(gp("permanova_size",7.5/ggplot2::.pt)),lineheight=1.25) + xlim(-1,1)+ylim(-1,1)+theme_bw(base_size=base_size,base_family=as.character(gp("family","Helvetica")))+theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),panel.grid=element_blank(),panel.border=element_rect(colour="black",fill=NA,linewidth=.25)) else ggplot() + theme_void()
negative_count<-sum(pc$values$Eigenvalues<0)
negative_share<-sum(abs(pc$values$Eigenvalues[pc$values$Eigenvalues<0]))/sum(abs(pc$values$Eigenvalues))
caption <- paste0(nrow(df),' supplied samples / ',length(features),' features; uncorrected Bray-Curtis PCoA. Equal x/y units; marginal boxes share scatter limits.\n',negative_count,' negative eigenvalues (',sprintf('%.1f',100*negative_share),'% of absolute eigenvalue sum); axis percentages use the original signed-sum denominator.\nLetters: original axis-wise ANOVA/Tukey; shared letters mean no detected difference at 0.05. PERMANOVA: 999 free permutations, seed fixed.\nSingleton group(s): ',paste(if(any(group_n==1))names(group_n)[group_n==1] else "none",collapse=', '),'; no within-group distribution estimate. Sampling design and dispersion assumptions are unverified.')
q<-bx+bp+q0+by+plot_layout(ncol=2,nrow=2,widths=as.numeric(unlist(gp("layout_widths",c(-1,.18)))),heights=as.numeric(unlist(gp("layout_heights",c(.16,-1)))),guides=if(legend_position=="inside")"keep" else "collect")+plot_annotation(caption=caption,theme=theme(plot.caption=element_text(size=6.5,hjust=.5,family=as.character(gp("family","Helvetica")),margin=margin(t=1,unit="mm")),plot.margin=margin(2,2,2,2,"mm")))&theme(legend.position=if(show_legend)legend_position else "none")
write.table(xy,file.path(o,"ordination.tsv"),sep="\t",row.names=FALSE,quote=FALSE)
w<-as.numeric(gp("width_mm",180))/25.4;h<-as.numeric(gp("height_mm",140))/25.4;d<-as.numeric(gp("dpi",300));ggsave(file.path(o,"plot.png"),q,width=w,height=h,units="in",dpi=d,device=ragg::agg_png,bg="white");ggsave(file.path(o,"plot.pdf"),q,width=w,height=h,units="in",device=pdf);svglite::svglite(file.path(o,"plot.svg"),width=w,height=h);print(q);dev.off();cat("Rendered PCoA scatter with Bray-Curtis ordination\n")
