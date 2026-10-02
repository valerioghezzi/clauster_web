# Job folders hold a copy of the data. They live in one folder of the system temporary directory, so that
# folders left behind by an abruptly closed session can be found and removed at the next start.
jobs_root <- function() file.path(dirname(tempdir()),"clauster-jobs")
pid_alive <- function(pid) {
  if(is.na(pid)) return(FALSE)
  if(.Platform$OS.type!="windows") return(isTRUE(tools::pskill(pid,signal=0)))
  lines <- tryCatch(system2("tasklist",c("/FI",shQuote(paste("PID eq",pid)),"/NH"),stdout=TRUE,stderr=FALSE),error=function(e)character())
  any(grepl(paste0("\\b",pid,"\\b"),lines))
}
sweep_stale_jobs <- function() {
  dirs <- list.dirs(jobs_root(),recursive=FALSE,full.names=TRUE);dirs <- dirs[startsWith(basename(dirs),"cluster-job-")]
  for(d in dirs) {pid <- suppressWarnings(as.integer(tryCatch(readLines(file.path(d,"pid"),warn=FALSE)[1],error=function(e)NA)))
    old <- as.numeric(difftime(Sys.time(),file.info(d)$mtime,units="hours"))>1
    if((!is.na(pid)&&!pid_alive(pid))||(is.na(pid)&&old)) unlink(d,recursive=TRUE,force=TRUE)}
  invisible(NULL)
}
# In the browser (Shinylive, webR) no separate process can be started: the analysis runs inside the
# application session, with a progress bar. options(clauster.inline = TRUE) forces this mode for tests.
in_browser <- function() identical(R.version$os,"emscripten")||isTRUE(getOption("clauster.inline"))
start_inline_job <- function(data,cfg) {
  cfg<-cw_config(cfg);cfg$.deadline<-as.numeric(Sys.time())+cfg$timeout;st<-new.env(parent=emptyenv());st$done<-FALSE;st$started<-Sys.time()
  run<-function() {
    if(st$done) return(invisible(NULL))
    analyse<-function(progress) run_analysis(data,cfg,progress)
    st$out<-tryCatch(list(status="completed",result=if(is.null(shiny::getDefaultReactiveDomain())) analyse(function(...)NULL) else
      shiny::withProgress(message="Running the analysis",value=0,analyse(function(phase,completed,total)shiny::setProgress(value=completed/max(1,total),detail=phase)))),
      error=function(e)list(status="error",error=conditionMessage(e)))
    st$done<-TRUE;invisible(NULL)
  }
  list(inline=TRUE,run=run,state=st,dir=NULL,deadline=cfg$.deadline+10,started=st$started,
    process=list(is_alive=function()!st$done,kill_tree=function(){st$done<-TRUE;invisible(NULL)},get_result=function()st$out$result,wait=function(...)run()))
}
start_job <- function(data,cfg,root=getwd()) {
  if(in_browser()) return(start_inline_job(data,cfg))
  cfg<-cw_config(cfg);cfg$.deadline<-as.numeric(Sys.time())+cfg$timeout
  if(!dir.exists(file.path(root,"R"))&&dir.exists(file.path(root,"..","R")))root<-file.path(root,"..")
  root<-normalizePath(root);dir<-tempfile("cluster-job-",tmpdir=jobs_root());dir.create(dir,recursive=TRUE)
  cfg$.checkpoint<-file.path(dir,"partial.rds")
  saveRDS(list(data=data,cfg=cfg,root=root,libpath=.libPaths(),parent=Sys.getpid(),windows=.Platform$OS.type=="windows"),file.path(dir,"request.rds"))
  worker<-file.path(root,"worker.R");if(!file.exists(worker))stop("The application worker file is missing.")
  if(.Platform$OS.type=="windows") {
    exe<-file.path(R.home("bin"),"Rscript.exe");args<-c("--vanilla",shQuote(worker),shQuote(dir))
  } else {exe<-file.path(R.home("bin"),"R");args<-c("--vanilla","--slave","-f",shQuote(worker),"--args",shQuote(dir))}
  system2(exe,args,stdout=file.path(dir,"stdout.log"),stderr=file.path(dir,"stderr.log"),wait=FALSE)
  state<-new.env(parent=emptyenv());state$canceled<-FALSE;state$started<-Sys.time()
  get_pid<-function()if(file.exists(file.path(dir,"pid")))suppressWarnings(as.integer(readLines(file.path(dir,"pid"),warn=FALSE)[1]))else NA_integer_
  alive<-function() {
    if(state$canceled||!dir.exists(dir)||file.exists(file.path(dir,"finished")))return(FALSE)
    pid<-get_pid()
    if(is.na(pid)) {
      # A worker that never wrote its process id within 30 seconds has failed to start.
      if(as.numeric(difftime(Sys.time(),state$started,units="secs"))>30)return(FALSE)
      return(TRUE)
    }
    pid_alive(pid)
  }
  kill<-function() {
    if(file.exists(file.path(dir,"finished"))){state$canceled<-TRUE;return(invisible(NULL))}
    file.create(file.path(dir,"cancel"));pid<-get_pid()
    if(!is.na(pid)) {
      if(.Platform$OS.type=="windows")suppressWarnings(system2("taskkill",c("/PID",as.character(pid),"/T","/F"),stdout=FALSE,stderr=FALSE))
      else tools::pskill(pid,signal=9)
    }
    state$canceled<-TRUE;invisible(NULL)
  }
  value<-function() {
    path<-file.path(dir,"result.rds")
    if(!file.exists(path)){log<-tryCatch(readLines(file.path(dir,"stderr.log"),warn=FALSE),error=function(e)character())
      log<-log[nzchar(trimws(log))];last<-gsub("(/|[A-Za-z]:\\\\)[^ :]*[/\\\\]","",tail(log,1))
      stop(paste("The calculation process stopped unexpectedly.",if(length(log))paste("Last message:",last)else"Restart CLAuster and try again."))}
    out<-readRDS(path);if(!is.null(out$error))stop(out$error);out$result
  }
  wait<-function(timeout=10000) {limit<-as.numeric(Sys.time())+timeout/1000;while(alive()&&as.numeric(Sys.time())<limit)Sys.sleep(.05);invisible(NULL)}
  list(process=list(is_alive=alive,kill_tree=kill,get_result=value,wait=wait),dir=dir,
    progress_path=file.path(dir,"progress.rds"),deadline=cfg$.deadline+10,started=Sys.time())
}
cancel_job <- function(job) {if(!is.null(job)){job$process$kill_tree();Sys.sleep(.1);remove_job_dir(job)};invisible(NULL)}
# The job folder holds a copy of the data: it is removed as soon as the job ends.
remove_job_dir <- function(job) if(!is.null(job$dir)&&dir.exists(job$dir)) try(unlink(job$dir,recursive=TRUE,force=TRUE),silent=TRUE)
poll_job <- function(job) {
  if(isTRUE(job$inline)) {job$run();return(job$state$out%||%list(status="error",error="The analysis was canceled."))}
  if(!job$process$is_alive()){out<-tryCatch(list(status="completed",result=job$process$get_result()),error=function(e)list(status="error",error=conditionMessage(e)));remove_job_dir(job);return(out)}
  if(as.numeric(Sys.time())>=job$deadline) {job$process$kill_tree();partial<-file.path(job$dir,"partial.rds");if(file.exists(partial)&&!is.null(r<-tryCatch(readRDS(partial),error=function(e)NULL))){r$status<-"Time limit reached; partial results";remove_job_dir(job);return(list(status="completed",result=r))};remove_job_dir(job);return(list(status="timeout",error="Time limit reached. The previous completed result is retained."))}
  progress<-if(file.exists(job$progress_path))tryCatch(suppressWarnings(readRDS(job$progress_path)),error=function(e)NULL)else NULL
  list(status="running",progress=progress,elapsed=as.numeric(difftime(Sys.time(),job$started,units="secs")))
}
