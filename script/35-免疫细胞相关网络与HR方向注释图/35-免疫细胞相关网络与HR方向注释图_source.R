#!/usr/bin/env Rscript
# Reproducible entry point for the native immune correlation network renderer.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
template <- file.path(if (length(file_arg)) dirname(sub("^--file=", "", file_arg[[1L]])) else getwd(), "template.R")
sys.source(template, envir = new.env(parent = globalenv()))
