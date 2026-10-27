args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L)
expr <- parse(file=args[1])
env <- new.env(parent=.GlobalEnv)
allowed <- c('groups','dimensions','simulate_data')
seen <- character()
for (x in expr) {
 if(is.call(x) && as.character(x[[1L]]) %in% c('<-','=') && is.symbol(x[[2L]]) && as.character(x[[2L]]) %in% allowed) {
  eval(x,envir=env); seen <- c(seen,as.character(x[[2L]]))
 }
}
stopifnot(setequal(seen,allowed),length(seen)==3L)
d <- env$simulate_data(20260908L)
stopifnot(nrow(d)==24L,length(unique(d$group))==3L,length(unique(d$dimension))==8L,!anyDuplicated(d[c('group','dimension')]),all(is.finite(d$value)),all(d$value>=.04 & d$value<=.98))
options(digits=17)
write.table(d,file=file.path(args[2],'radar_values.tsv'),sep='\t',row.names=FALSE,quote=FALSE,na='')
capture.output(sessionInfo(),file=file.path(args[2],'radar_export_environment.txt'))
cat('Exported 24 values using original simulate_data seed 20260908; no plot/library/source body executed\n')
