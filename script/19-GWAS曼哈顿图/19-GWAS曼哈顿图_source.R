#!/usr/bin/env Rscript
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)) else getwd()
source(file.path(script_dir, "template.R"), local = .GlobalEnv)
