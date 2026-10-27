#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c(file.path(script_dir, "template.R"), args)))
quit(status = status)
