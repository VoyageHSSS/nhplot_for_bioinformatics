#!/usr/bin/env Rscript
# Authored reconstruction data generator, NOT the source code of the paper.
# Fixed synthetic donors; no measurements are extracted from the reference image.
args <- commandArgs(trailingOnly = FALSE)
root <- dirname(normalizePath(sub('^--file=', '', grep('^--file=', args, value = TRUE)[1])))
dir.create(file.path(root, 'assets'), showWarnings = FALSE)
blocks <- list(
  ILC = c('ILC2|IL2RA', 'NK bright|XCL1', 'Transit. NK|GZMK,CD160',
          'Terminal NK|FCGR3A', 'Mature NK|IFIT1', 'Prolif. NK|MKI67'),
  'B cell' = c('Transit. B|MME', 'Naive B|TCL1A,IL6,IFIT3',
    'Atypical Bm|ITGAX', 'Unswitched Bm|IL6,CD1C', 'pre-Switched Bm|IFIT3,JAM3',
    'Switched Bm|IGHD,IGHE,CD86', 'Plasmablast|MKI67', 'Plasma|IGHA1,IGHG1'),
  Myeloid = c('HSPC|CD34', 'MK|GP9', 'DC|CSF2RA,CLEC9A,CD1C', 'AS DC|AXL',
    'cMono|CD14,IL1B,IFI44L,CXCL10,GFRA2', 'intMono|C1QA,FCGR3A',
    'ncMono|IFIT1,LST1', 'pDC|GZMB'),
  'CD4 T' = c('CD4 naive T|SOX4,CCR7', 'CD4 Tcm|CCR7,CXCR5',
    'CD4 Tem|CD69,IFITM3', 'CD4 Th1-like|CXCR3,LMNA,IL7R,GZMK',
    'CD4 Th|CCR4,TNFRSF1A', 'CD4 Th17-like|RORC',
    'CD4 Th22-like|CCR10', 'CD4 Tr1|IL10', 'CD4 Treg|FCRL3,FOXP3',
    'CD4 CTL|GZMH,CCL5', 'CD4 memory T|IL32'),
  'Other T' = c('CD8 naive T|CCR7,SOX4', 'CD8 Tcm|IFI44L', 'CD8 Tem|GZMK',
    'CD8 CTL|IFNG,GZMB,MKI67', 'Immature T|CBARA1', 'MAIT|SLC4A10',
    'gdT1|TRDV1', 'gdT2|IL12RB2,GZMK,GZMH', 'NKT|TRAC,KLRC1')
)
rows <- list()
for (group in names(blocks)) for (entry in blocks[[group]]) {
  parts <- strsplit(entry, '|', fixed = TRUE)[[1]]
  for (marker in strsplit(parts[2], ',', fixed = TRUE)[[1]])
    rows[[length(rows)+1L]] <- data.frame(lineage=group, state=parts[1],
                                         subtype=paste(parts[1], marker, sep=' - '))
}
d <- do.call(rbind, rows); rownames(d) <- NULL
stopifnot(nrow(d) == 73L, !anyDuplicated(d$subtype))
set.seed(20261013)
n <- 160L; k <- nrow(d)
donors <- data.frame(donor=sprintf('D%03d', 1:n), age=runif(n, 20, 78),
                     female=sample(rep(0:1, each=n/2)))
age_z <- as.numeric(scale(donors$age))
baseline <- rgamma(k, shape=1.6)
baseline[grepl('MKI67|HSPC|Plasmablast|AS DC', d$subtype)] <- .08
baseline <- baseline / sum(baseline)
age_effect <- .24*sin(seq_len(k)*.37)
sex_effect <- .36*cos(seq_len(k)*.29)
age_effect[seq(3,k,4)] <- 0; sex_effect[seq(2,k,3)] <- 0
shared <- matrix(rnorm(n*k, 0, .43), n, k)
raw <- list(); p_values <- list()
for (assay in c('rna','atac')) {
  latent <- matrix(log(baseline), n, k, byrow=TRUE) + outer(age_z, age_effect) +
    outer(donors$female, sex_effect) + shared + matrix(rnorm(n*k,0,.2),n,k)
  if (assay == 'atac') latent <- latent + matrix(.15*sin(seq_len(k)),n,k,byrow=TRUE)
  probs <- exp(latent); probs <- probs / rowSums(probs)
  totals <- sample(if (assay=='rna') 5000:8000 else 3000:6000, n, replace=TRUE)
  counts <- t(vapply(seq_len(n), function(i) as.vector(rmultinom(1,totals[i],probs[i,])), integer(k)))
  proportion <- counts / totals
  d[[paste0(assay,'_proportion')]] <- colMeans(proportion)
  # Conditional associations in synthetic compositional counts, not causal effects.
  response <- log2((counts+.5)/(totals+.5*k))
  fit <- lapply(seq_len(k), function(j) coef(summary(lm(response[,j] ~ age_z + donors$female))))
  for (term in c('age','sex')) {
    r <- if(term=='age') 2L else 3L
    key <- paste(term,assay,sep='_')
    d[[key]] <- vapply(fit,function(x)x[r,1],numeric(1))
    p_values[[key]] <- vapply(fit,function(x)x[r,4],numeric(1))
  }
  raw[[assay]] <- data.frame(donors, assay=assay, counts, check.names=FALSE)
  names(raw[[assay]])[-seq_len(4)] <- d$subtype
}
q <- matrix(p.adjust(unlist(p_values), method='BH'),nrow=k)
for(j in seq_along(p_values)) d[[paste0(names(p_values)[j],'_q')]] <- q[,j]
stopifnot(all(abs(c(sum(d$rna_proportion),sum(d$atac_proportion))-1)<1e-10),
          all(q >= 0 & q <= 1), all(is.finite(as.matrix(d[,-(1:3)]))))
write.csv(d, file.path(root,'assets','cell_summary.csv'),row.names=FALSE)
write.csv(do.call(rbind,raw),file.path(root,'assets','synthetic_donors.csv'),row.names=FALSE)
cat('Saved 73 cell subtypes, 160 synthetic donors, 2 assays; BH across 292 tests.\n')
