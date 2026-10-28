#!/usr/bin/env Rscript
# Reproducible CLI entrypoint. Original tutorial script retained under assets.
file_arg <- grep("^--file=",commandArgs(),value=TRUE)
source(file.path(dirname(normalizePath(sub("^--file=","",file_arg[[1L]]),mustWork=TRUE)),"template.R"),chdir=FALSE)
