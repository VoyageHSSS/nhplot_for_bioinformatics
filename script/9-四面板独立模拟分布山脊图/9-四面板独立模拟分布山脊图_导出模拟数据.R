#!/usr/bin/env Rscript
# Evaluate only the unchanged data-generation expressions before palette/plot code.
options(device = function(...) stop("Plotting is prohibited during table export"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Pass one new or empty output directory")
out <- args[[1L]]
if (dir.exists(out) && length(list.files(out, all.files = TRUE, no.. = TRUE))) stop("Output directory must be empty")
flag <- grep("^--file=", commandArgs(), value = TRUE)[[1L]]
folder <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", flag), fixed = TRUE)))
src <- file.path(folder, "9-四面板独立模拟分布山脊图_模拟数据源.R")
expressions <- parse(src)
assignments <- which(vapply(expressions, function(x) is.call(x) && identical(x[[1L]], as.name("<-")) && identical(x[[2L]], as.name("make_ridge")), logical(1L)))
stopifnot(length(assignments) == 1L)
generator <- eval(expressions[[assignments]])
parts <- as.list(body(generator))[-1L]
cut <- which(vapply(parts, function(x) is.call(x) && identical(x[[1L]], as.name("<-")) && identical(x[[2L]], as.name("pal")), logical(1L)))
stopifnot(length(cut) == 1L, cut == 7L)
data_parts <- parts[seq_len(cut - 1L)]
stopifnot(!any(grepl("ggplot|geom_|density\\(|build_plot", vapply(data_parts, function(x) paste(deparse(x), collapse = " "), character(1L)))))
body(generator) <- as.call(c(list(as.name("{")), data_parts, list(quote(dat))))
observations <- list(); counts <- list()
for (i in 1:4) {
  metric <- if (i %% 2L) "year" else "jif"
  d <- generator(20260909L + i, marine = i >= 3L, metric = metric)
  row <- as.integer(d$region)
  group <- paste0("Group ", rev(LETTERS[seq_along(levels(d$region))]))[row]
  observations[[i]] <- data.frame(panel = LETTERS[i], set = if (i <= 2L) "Set 1" else "Set 2",
    metric = if (i %% 2L) "simulated_year" else "simulated_score", group = group,
    value = d$x, source_label = as.character(d$region), source_region = d$region_raw)
  counts[[i]] <- data.frame(panel = LETTERS[i], group = paste0("Group ", rev(LETTERS[seq_along(levels(d$region))])),
    source_label = levels(d$region), actual_n = as.integer(table(d$region)))
}
stopifnot(identical(vapply(observations, nrow, integer(1L)), c(813L,813L,114L,114L)))
d <- do.call(rbind, observations); n <- do.call(rbind, counts)
stopifnot(nrow(d) == 1854L, nrow(n) == 42L, all(n$actual_n >= 6L))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
prefix <- "9-四面板独立模拟分布山脊图"
write.table(d, file.path(out, paste0(prefix, "_模拟观测.tsv")), sep = "\t", quote = TRUE, row.names = FALSE, na = "")
write.table(n, file.path(out, paste0(prefix, "_实际组样本数.tsv")), sep = "\t", quote = TRUE, row.names = FALSE, na = "")
writeLines(capture.output(sessionInfo()), file.path(folder, paste0(prefix, "_数据导出环境.txt")))
cat("Exported 1854 original simulated observations and 42 panel/group counts; no KDE or figure created.\n")
