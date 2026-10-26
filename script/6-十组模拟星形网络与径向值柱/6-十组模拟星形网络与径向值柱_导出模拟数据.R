#!/usr/bin/env Rscript
# Export the original fixed-seed tables only; no plotting functions are called.
options(device = function(...) stop("Graphics are disabled for data export"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Pass one new output directory")
output_dir <- args[[1L]]
if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) stop("Output directory must be new or empty")
file_flag <- grep("^--file=", commandArgs(), value = TRUE)[[1L]]
script_dir <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", file_flag), fixed = TRUE)))
source(file.path(script_dir, "6-十组模拟星形网络与径向值柱_模拟数据源.R"))
net <- simulate_network(20260908L)
leaves <- net$leaves
leaves$group <- LETTERS[match(as.character(leaves$category), categories)]
leaves$display_label <- paste0(leaves$group, sprintf("%02d", ave(seq_len(nrow(leaves)), leaves$group, FUN = seq_along)))
leaves$value <- leaves$outer - leaves$inner
stopifnot(nrow(leaves) == 88L, nrow(net$centers) == 10L, nrow(net$center_edges) == 10L,
          !anyDuplicated(leaves$display_label), all(is.finite(leaves$value)),
          identical(as.integer(table(leaves$category)), c(10L,10L,9L,8L,10L,9L,8L,8L,7L,9L)))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
prefix <- "6-十组模拟星形网络与径向值柱"
for (name in c("叶节点", "组中心", "根到组连线")) {
  table <- switch(name, "叶节点" = leaves, "组中心" = net$centers, "根到组连线" = net$center_edges)
  write.table(table, file.path(output_dir, paste0(prefix, "_", name, ".tsv")),
              sep = "\t", quote = TRUE, row.names = FALSE, na = "")
}
writeLines(capture.output(sessionInfo()), file.path(script_dir, paste0(prefix, "_数据导出环境.txt")))
cat("Exported 88 leaves, 10 centers and 10 root edges with seed 20260908; no figure generated.\n")
