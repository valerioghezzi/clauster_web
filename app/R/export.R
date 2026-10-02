# Ordinal variables kept as ordered factors (Gower distance) are summarized by their numeric codes.
profile_values <- function(x) {
  if(!is.factor(x)) return(as.numeric(x))
  num <- suppressWarnings(as.numeric(as.character(x)));if(anyNA(num[!is.na(x)])) as.numeric(x) else num
}
cluster_profiles <- function(raw,cluster) {
  k<-max(cluster);sz<-tabulate(cluster,k);out<-data.frame(Cluster=c(seq_len(k),"All"),N=c(sz,length(cluster)),Percent=c(100*sz/length(cluster),100),stringsAsFactors=FALSE,check.names=FALSE)
  for(v in names(raw)) {x<-profile_values(raw[[v]])
    out[[v]]<-c(vapply(seq_len(k),function(g)mean(x[cluster==g],na.rm=TRUE),numeric(1)),mean(x,na.rm=TRUE))}
  out
}
solution_tables <- function(result,k) {
  if(is.null(k)||!length(k)||!nzchar(as.character(k)[1]))return(list())
  sol<-result$solutions[[as.character(k)]]
  if(is.null(sol))return(list())
  e<-sol$evaluation;out<-list(Clusters=e$clusters)
  if(!is.null(result$prepared$raw)&&nrow(result$prepared$raw)==length(sol$cluster))out$Profiles<-cluster_profiles(result$prepared$raw,sol$cluster)
  if(!is.null(e$centers))out$Centroids<-data.frame(Cluster=seq_len(nrow(e$centers)),e$centers,check.names=FALSE)
  if(!is.null(e$SD))out$Within_SD<-data.frame(Cluster=seq_len(nrow(e$SD)),e$SD,check.names=FALSE)
  if(!is.null(e$silhouette)){sw<-unclass(e$silhouette);out$Silhouette<-data.frame(ID=sol$ids,Cluster=as.integer(sw[,1]),Neighbor=as.integer(sw[,2]),Silhouette_width=as.numeric(sw[,3]))}
  if(!is.null(sol$fit$posterior))out$Posterior<-data.frame(ID=sol$ids,sol$fit$posterior)
  if(!is.null(sol$fit$trace))out$Relocation<-sol$fit$trace
  if(!is.null(sol$bootstrap)) {out$Bootstrap<-sol$bootstrap$summary;if(!is.null(sol$bootstrap$overall))out$Bootstrap_agreement<-sol$bootstrap$overall;out$Bootstrap_replicates<-sol$bootstrap$replicates;out$Bootstrap_fit_status<-sol$bootstrap$indices}
  if(!is.null(sol$simulation)) {out$Simulation<-sol$simulation$summary;out$Simulation_replicates<-sol$simulation$replicates}
  out
}
# CSV with a UTF-8 byte-order mark, so Excel shows accented text correctly.
write_csv_bom <- function(x,path) {
  con <- file(path,"wb");on.exit(close(con))
  writeBin(as.raw(c(0xef,0xbb,0xbf)),con)
  utils::write.csv(x,con,row.names=FALSE,na="",fileEncoding="")
}
export_session <- function(result,data,path,root=getwd(),report_k=NULL,labels=NULL,source_name=NULL) {
  path<-file.path(normalizePath(dirname(path),mustWork=TRUE),basename(path))
  dir<-tempfile("cluster-export-");dir.create(dir);on.exit(unlink(dir,recursive=TRUE),add=TRUE)
  cfg<-result$config;cfg$.deadline<-NULL;cfg$.checkpoint<-NULL
  saveRDS(data,file.path(dir,"input.rds"));saveRDS(cfg,file.path(dir,"settings.rds"));saveRDS(result,file.path(dir,"fitted_session.rds"))
  jsonlite::write_json(cfg,file.path(dir,"settings.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
  tables<-c(list(Fit_indices=fit_indices(result)),result$tables)
  if(!is.null(result$assignments))tables$Assignments<-result$assignments
  for(k in names(result$solutions))for(nm in names(solution_tables(result,k)))tables[[paste0(nm,"_K",k)]]<-solution_tables(result,k)[[nm]]
  if(!is.null(result$gap$table))tables$Gap<-result$gap$table
  dir.create(file.path(dir,"tables"))
  for(nm in names(tables))if(is.data.frame(tables[[nm]])&&nrow(tables[[nm]]))write_csv_bom(tables[[nm]],file.path(dir,"tables",paste0(nm,".csv")))
  nonempty<-tables[vapply(tables,function(x)is.data.frame(x)&&nrow(x)>0,logical(1))]
  if(length(nonempty)) {
    wb<-openxlsx::createWorkbook()
    for(i in seq_along(nonempty)) {nm<-substr(paste0(i,"_",names(nonempty)[i]),1,31);openxlsx::addWorksheet(wb,nm);openxlsx::writeData(wb,nm,nonempty[[i]])}
    openxlsx::saveWorkbook(wb,file.path(dir,"tables.xlsx"),overwrite=TRUE)
  }
  if(!dir.exists(file.path(root,"R")) && dir.exists(file.path(root,"..","R")))root<-normalizePath(file.path(root,".."))
  if(!dir.exists(file.path(root,"R")))stop("The analysis engine directory is missing.")
  dir.create(file.path(dir,"R"));file.copy(list.files(file.path(root,"R"),pattern="\\.R$",full.names=TRUE),file.path(dir,"R"))
  writeLines(c("# Run from this directory with Rscript rerun.R.",
    "# The packages used by CLAuster must be available: if they were installed by the CLAuster setup only,",
    "# add its library folder first, for example .libPaths(c('path/to/CLAuster/library', .libPaths())).",
    "need <- c('cluster','mclust','mice','clue','jsonlite')",
    "miss <- need[!vapply(need, requireNamespace, logical(1), quietly=TRUE)]",
    "if (length(miss)) stop('Missing packages: ', paste(miss, collapse=', '), '. Install them with install.packages(c(', paste0(\"'\", miss, \"'\", collapse=', '), ')) or add the CLAuster library folder with .libPaths().', call.=FALSE)",
    "for (f in list.files('R', pattern='[.]R$', full.names=TRUE)) source(f, local=TRUE)",
    "data <- readRDS('input.rds')","settings <- readRDS('settings.rds')","result <- run_analysis(data, settings)",
    "saveRDS(result, 'reproduced_result.rds')","if (!is.null(result$comparison)) write.csv(result$comparison, 'reproduced_comparison.csv', row.names=FALSE)"),file.path(dir,"rerun.R"))
  writeLines(result$session%||%capture.output(sessionInfo()),file.path(dir,"sessionInfo.txt"))
  tryCatch(write_report(result,file.path(dir,"report.pdf"),report_k,labels,FALSE,source_name),error=function(e)writeLines(paste("The PDF report could not be created:",conditionMessage(e)),file.path(dir,"report-error.txt")))
  zip::zipr(path,files=list.files(dir,full.names=FALSE),root=dir)
  invisible(path)
}

# Original data with one membership column per fitted k (empty for cases left out of the analysis).
data_with_clusters <- function(result) {
  d <- result$original;a <- result$assignments
  if(!is.null(a)&&ncol(a)>1) {names(a)[-1] <- paste0("Cluster_k",sub("^K","",names(a)[-1]));d <- cbind(d,a[-1])}
  d
}
write_data_with_clusters <- function(result,file) {
  wb <- openxlsx::createWorkbook();openxlsx::addWorksheet(wb,"Data with clusters");openxlsx::writeData(wb,1,data_with_clusters(result),keepNA=FALSE)
  openxlsx::freezePane(wb,1,firstRow=TRUE);openxlsx::saveWorkbook(wb,file,overwrite=TRUE);invisible(file)
}
