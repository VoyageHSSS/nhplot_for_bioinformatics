args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==2L)
lib<-args[1];out<-args[2];options(digits=17)
# Evaluate only the data-generation prefix of the original swimlane function.
p<-parse(file=file.path(lib,'app/templates/collections/我的收藏/manual__Nature_泳道图_治疗时间/source.R'))
f<-Filter(function(z)is.call(z)&&identical(z[[1]],as.name('<-'))&&identical(z[[2]],as.name('build_plot')),as.list(p));stopifnot(length(f)==1)
fn_env<-new.env(parent=.GlobalEnv);eval(f[[1]],envir=fn_env)
body_expr<-as.list(body(fn_env$build_plot))[-1]
cut<-which(vapply(body_expr,function(z)is.call(z)&&identical(z[[1]],as.name('<-'))&&identical(z[[2]],as.name('main')),logical(1)))
stopifnot(length(cut)==1L,cut>5L)
data_expr<-body_expr[seq_len(cut-1L)];stopifnot(!any(grepl('ggplot|geom_|stat_|density|KDE|plot_grid',vapply(data_expr,function(z)paste(deparse(z),collapse=' '),character(1)))))
e<-new.env(parent=.GlobalEnv);e$seed<-20260909L
for(z in data_expr)eval(z,envir=e)
stopifnot(nrow(e$tiles)==170L,nrow(e$segments)==102L,nrow(e$marks)==68L,length(e$finish)==34L,all(e$base_end<=e$finish),all(e$base_end>=0))
id<-function(y)sprintf('R%02d',y)
intervals<-data.frame(row_id=id(e$rows),source_row=e$rows,start=0,phase_a_end=e$base_end,finish=e$finish)
events<-e$marks;events$row_id<-id(events$y);events$event<-ifelse(events$mark=='open','Event A','Event B')
fields<-e$tiles;fields$row_id<-id(fields$y);fields$field<-paste0('F',fields$x);fields$level<-letters[match(fields$fill,e$left_cols)]
segments<-e$segments;segments$row_id<-id(segments$y);segments$kind<-rep(c('Interval','Phase A','Phase B'),each=34L)
stopifnot(all(events$x>=0),all(events$x<=intervals$finish[match(events$row_id,intervals$row_id)]),!anyNA(fields$level))
for(n in c('intervals','events','fields','segments'))write.table(get(n),file=file.path(out,paste0('swimlane_',n,'.tsv')),sep='\t',row.names=FALSE,quote=FALSE,na='')
# Parse only simulate_data; no source body, library calls, KDE or plot is evaluated.
p<-parse(file=file.path(lib,'app/templates/collections/我的收藏/manual__Nature_二维密度边缘图/source.R'))
f<-Filter(function(z)is.call(z)&&identical(z[[1]],as.name('<-'))&&identical(z[[2]],as.name('simulate_data')),as.list(p));stopifnot(length(f)==1L)
e2<-new.env(parent=.GlobalEnv);eval(f[[1]],envir=e2);xy<-e2$simulate_data(seed=20260908L,n_per_group=2200L)
stopifnot(nrow(xy)==4400L,all(table(xy$group)==2200L),all(is.finite(xy$X_lab)),all(is.finite(xy$Y_lab)),all(xy$X_lab>=30&xy$X_lab<=80&xy$Y_lab>=30&xy$Y_lab<=80))
write.table(xy,file=file.path(out,'density_observations.tsv'),sep='\t',row.names=FALSE,quote=FALSE,na='')
capture.output(sessionInfo(),file=file.path(out,'data_export_environment.txt'))
cat('Data export only: swimlane seed20260909 34 intervals/68events/170fields; bivariate seed20260908 4400 values. No plotting or KDE run.\n')
