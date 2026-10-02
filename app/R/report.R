# PDF report written with base graphics only (no LaTeX or external tools needed).
# Cairo PDF prints every Unicode character; the plain PDF device is the fallback.
# Some macOS R builds report Cairo but cannot open its devices without XQuartz: test it once.
cairo_works <- local({ok <- NULL;function(){
  if(is.null(ok)) ok <<- isTRUE(capabilities("cairo")) && isTRUE(tryCatch({f <- tempfile(fileext=".pdf");suppressWarnings(grDevices::cairo_pdf(f));grDevices::dev.off();file.exists(f)},error=function(e)FALSE))
  ok}})
svg_works <- local({ok <- NULL;function(){
  if(is.null(ok)) ok <<- isTRUE(capabilities("cairo")) && isTRUE(tryCatch({f <- tempfile(fileext=".svg");suppressWarnings(grDevices::svg(f));grDevices::dev.off();file.exists(f)},error=function(e)FALSE))
  ok}})
open_pdf <- function(file,width,height,title="CLAuster") {
  if(cairo_works()) grDevices::cairo_pdf(file,width=width,height=height,onefile=TRUE,family="sans")
  else grDevices::pdf(file,width=width,height=height,title=title,family="Helvetica",encoding="WinAnsi",onefile=TRUE,useDingbats=FALSE)
}
format_cells <- function(x,digits=3) {
  # One format per column: whole numbers stay whole, otherwise every cell has the same decimals.
  out <- lapply(names(x),function(nm){v <- x[[nm]]
    if(is.numeric(v)){ok <- is.finite(v);whole <- all(abs(v[ok]-round(v[ok]))<1e-9&abs(v[ok])<1e9)
      # 0 and 1 in a proportion or quantile column keep their decimals; counts stay whole.
      if(whole&&!any(abs(v[ok])>1)&&!grepl("^(n|k|cluster|occurrences|missing|singletons|parameters)$|replications|cases|_b$|count",nm,ignore.case=TRUE)&&any(grepl("jaccard|rand|ari|%|recover|dissol|mean|probab|share|silhouette|p$",nm,ignore.case=TRUE))) whole <- FALSE
      d <- if(grepl("^percent$",nm,ignore.case=TRUE)) 1 else digits
      s <- if(whole) format(round(v),big.mark="",scientific=FALSE,trim=TRUE) else formatC(v,format="f",digits=d)
      s[is.na(v)] <- "";s[is.infinite(v)] <- ifelse(v[is.infinite(v)]>0,"Inf","-Inf");s}
    else {s <- as.character(v);s[is.na(s)] <- "";s}})
  out <- as.data.frame(out,stringsAsFactors=FALSE,check.names=FALSE);names(out) <- names(x);out
}
report_new_page <- function(state) {
  par(fig=c(0,1,0,1),mar=c(0,0,0,0),bg="white",new=FALSE)
  plot.new()
  plot.window(c(0,1),c(0,1),xaxs="i",yaxs="i")
  state$page <- state$page+1;state$y <- .955
  text(.94,.025,paste("CLAuster report, page",state$page),adj=c(1,0),cex=.62,col=cw_theme$muted)
  invisible(state)
}
report_coords <- function() {par(fig=c(0,1,0,1),mar=c(0,0,0,0),new=TRUE);plot.new();plot.window(c(0,1),c(0,1),xaxs="i",yaxs="i")}
report_heading <- function(state,title,note=NULL,need=.12) {
  if(state$page==0||state$y-need<.06) report_new_page(state) else report_coords()
  y <- state$y
  text(.06,y,title,adj=c(0,1),font=2,cex=1,col=cw_theme$ink);y <- y-.024
  if(!is.null(note)){text(.06,y,note,adj=c(0,1),cex=.7,col=cw_theme$ink2);y <- y-.02}
  segments(.06,y,.94,y,col=cw_theme$grid);state$y <- y-.012
  invisible(state)
}
# Draws a data frame on the flowing page, splitting wide tables into column blocks and long ones across pages.
report_table <- function(df,title,note=NULL,state,max_rows=Inf,right_from=NULL) {
  if(!is.data.frame(df)||!nrow(df)) return(invisible(state))
  truncated <- nrow(df)>max_rows
  df <- utils::head(df,max_rows);f <- format_cells(df);names(f) <- hy(gsub("_"," ",names(f)));f[] <- lapply(f,hy);title <- hy(title);note <- hy(note)
  cex <- .64;padx <- .011;left <- .06;right <- .94;lh <- .019
  report_heading(state,title,note,need=.07+min(lh*(nrow(f)+1),.6))
  is_right <- function(j) if(!is.null(right_from)) j>=right_from&&j<length(f) else is.numeric(df[[j]])
  widths <- pmin(vapply(seq_along(f),function(j)max(strwidth(c(names(f)[j],f[[j]]),cex=cex,font=2))+2*padx,numeric(1)),.42)
  blocks <- list();cur <- 1;used <- widths[1]
  if(length(f)>1)for(j in 2:length(f)){if(used+widths[j]>right-left){blocks[[length(blocks)+1]] <- cur;cur <- c(1,j);used <- widths[1]+widths[j]}else{cur <- c(cur,j);used <- used+widths[j]}}
  blocks[[length(blocks)+1]] <- cur
  for(bi in seq_along(blocks)) {
    cols <- blocks[[bi]];w <- widths[cols];x0 <- left+c(0,cumsum(w))[seq_along(cols)]
    if(bi>1) state$y <- state$y-.014
    header <- function(){y <- state$y;rect(left,y-lh,left+sum(w),y,col="#f3f5f4",border=NA)
      for(i in seq_along(cols)){num <- is_right(cols[i]);text(if(num)x0[i]+w[i]-padx else x0[i]+padx,y-lh/2,names(f)[cols[i]],adj=c(if(num)1 else 0,.5),cex=cex,font=2,col=cw_theme$ink2)}
      state$y <- y-lh}
    block_h <- lh*(nrow(f)+1)
    if(state$y-2*lh<.06||(state$y-block_h<.06&&block_h<.82)){report_new_page(state);text(.06,state$y,paste(title,"(continued)"),adj=c(0,1),font=2,cex=.85,col=cw_theme$ink2);state$y <- state$y-.03}
    header()
    for(r in seq_len(nrow(f))) {
      if(state$y-lh<.06){report_new_page(state);text(.06,state$y,paste(title,"(continued)"),adj=c(0,1),font=2,cex=.85,col=cw_theme$ink2);state$y <- state$y-.03;header()}
      y <- state$y
      if(r%%2==0)rect(left,y-lh,left+sum(w),y,col="#fafbfa",border=NA)
      for(i in seq_along(cols)){num <- is_right(cols[i])
        text(if(num)x0[i]+w[i]-padx else x0[i]+padx,y-lh/2,f[[cols[i]]][r],adj=c(if(num)1 else 0,.5),cex=cex,col=cw_theme$ink)}
      state$y <- y-lh
    }
    segments(left,state$y,left+sum(w),state$y,col=cw_theme$grid)
  }
  if(truncated){text(left,state$y-.008,sprintf("First %d rows shown; the session export contains the complete table.",max_rows),adj=c(0,1),cex=.6,col=cw_theme$muted);state$y <- state$y-.02}
  state$y <- state$y-.035
  invisible(state)
}
report_note <- function(state,note) {
  lines_v <- hy(strwrap(note,width=118));h <- .016*length(lines_v)
  if(state$y-h<.06) report_new_page(state)
  for(l in lines_v){text(.06,state$y,l,adj=c(0,1),cex=.62,col=cw_theme$ink2);state$y <- state$y-.016}
  state$y <- state$y-.025
  invisible(state)
}
# Plots take half a page each, with the same proportions as on screen.
report_plot <- function(result,k,type,labels,bw,state) {
  h <- .44
  if(state$page==0||state$y-h<.05) report_new_page(state)
  top <- state$y
  par(fig=c(.04,.96,max(0,top-h),top),new=TRUE,mar=c(0,0,0,0))
  plot_solution(result,k,type,labels,bw)
  state$y <- top-h-.02
  report_coords()
  invisible(state)
}
label_of <- function(x,map) unname(ifelse(x %in% names(map),map[x],x))
distance_labels <- c(euclidean="Euclidean",sqeuclidean="Squared Euclidean",manhattan="Manhattan",maximum="Maximum",minkowski="Minkowski (p = 2)",
  mahalanobis="Mahalanobis",correlation="Correlation",cosine="Cosine",gower="Gower",jaccard="Jaccard",matching="Simple matching")
scaling_labels <- c(z="Z scores",none="None",range="Range (0 to 1)",robust="Median and MAD")
missing_labels <- c(complete="Complete cases",neighbor="Close-neighbor imputation",mean="Mean imputation",available="Available coordinates (Gower)",multiple="Multiple imputation")
report_date <- function(t=Sys.time()) {
  # Built by hand: month names and AM/PM would otherwise follow the language of the computer.
  h <- as.integer(format(t,"%H"))
  sprintf("%s %d, %s, %d:%s %s",month.name[as.integer(format(t,"%m"))],as.integer(format(t,"%d")),format(t,"%Y"),(h+11)%%12+1,format(t,"%M"),if(h<12)"AM" else "PM")
}
settings_table <- function(result,source_name=NULL) {
  cfg <- result$config;p <- result$prepared
  item <- function(a,b)data.frame(Setting=a,Value=paste(b,collapse=", "),stringsAsFactors=FALSE)
  rows <- list(item("Data",source_name%||%"Data"),item("Cases in file",nrow(result$original)),
    item("Cases analyzed",if(!is.null(p))length(p$ids) else NA),item("Variables",cfg$variables%||%p$variables),
    item("Method",paste0(method_labels[[cfg$method]]%||%cfg$method,
      if(isTRUE(cfg$method%in%hybrid_methods))paste0(" (start: ",method_labels[[cfg$hybrid_start]]%||%cfg$hybrid_start,
        if(cfg$method=="hybrid_kmeans")paste0("; ",cfg$hybrid_algorithm) else "",")") else "")),item("Distance",label_of(cfg$distance,distance_labels)),item("Scaling",label_of(cfg$scaling,scaling_labels)),
    item("Missing data",label_of(cfg$missing,missing_labels)),item("Range of k",paste(cfg$k_min,"to",cfg$k_max)),
    item("Bootstrap",if(isTRUE(cfg$bootstrap))paste(cfg$B,"replications",if(isTRUE(cfg$subsample))"(subsampling)" else "") else "Not requested"),
    item("Null simulations",if(isTRUE(cfg$simulation))paste(cfg$B,"replications,",if(cfg$null=="gaussian")"one Gaussian population" else "column permutation") else "Not requested"),
    item("Gap statistic",if(isTRUE(cfg$gap))paste(cfg$gap_B,"replications") else "Not requested"),
    item("Random seed",cfg$seed),item("Status",paste0(result$status,if(!is.null(result$elapsed))sprintf(" (%.1f seconds)",result$elapsed) else "")),
    item("Software",paste0("CLAuster ",cw_version,", R ",R.version$major,".",R.version$minor)))
  do.call(rbind,rows)
}
write_report <- function(result,file,k=NULL,labels=NULL,bw=FALSE,source_name=NULL) {
  k <- as.character(k %||% suggested_k(result) %||% "")
  open_pdf(file,8.27,11.69,"CLAuster report")
  on.exit(grDevices::dev.off(),add=TRUE)
  state <- new.env();state$page <- 0;state$y <- 1
  report_new_page(state);y <- state$y
  text(.06,y,"CLAuster report",adj=c(0,1),font=2,cex=1.5,col=cw_theme$accent);y <- y-.035
  text(.06,y,"Cluster Analysis for Applied Research",adj=c(0,1),cex=.74,col=cw_theme$ink2);y <- y-.025
  text(.06,y,paste("Created",report_date()),adj=c(0,1),cex=.78,col=cw_theme$ink2);y <- y-.03
  segments(.06,y,.94,y,col=cw_theme$grid);y <- y-.01
  st <- settings_table(result,source_name)
  for(i in seq_len(nrow(st))){text(.06,y-.012,st$Setting[i],adj=c(0,1),cex=.74,font=2,col=cw_theme$ink2)
    lines_v <- hy(strwrap(st$Value[i],width=78));for(l in seq_along(lines_v)){text(.28,y-.012,lines_v[l],adj=c(0,1),cex=.74,col=cw_theme$ink);if(l<length(lines_v))y <- y-.018}
    y <- y-.024}
  y <- y-.01
  if(length(result$solutions)&&nzchar(k)){text(.06,y,paste0("Detailed solution in this report: k = ",k,". Cluster tables and plots refer to this solution."),adj=c(0,1),cex=.74,col=cw_theme$ink);y <- y-.022}
  text(.06,y,"Indices are descriptive. Simulation probabilities are exploratory and conditional on the chosen preprocessing, method and k.",adj=c(0,1),cex=.66,col=cw_theme$muted)
  state$y <- y-.05
  kf <- key_findings(result,k)
  if(nrow(kf)) {report_heading(state,paste0("Key findings (k = ",k,")"),need=.03*nrow(kf)+.06);y <- state$y-.006
    for(i in seq_len(nrow(kf))){text(.06,y,kf$Topic[i],adj=c(0,1),cex=.74,font=2,col=cw_theme$ink2)
      lines_v <- hy(strwrap(kf$Finding[i],width=84));for(l in seq_along(lines_v)){text(.3,y,lines_v[l],adj=c(0,1),cex=.74,col=cw_theme$ink);if(l<length(lines_v))y <- y-.017}
      y <- y-.024}
    state$y <- y-.02}
  tabs <- result$tables
  fi <- fit_indices(result)
  if(nrow(fi)) {report_table(fi,"Fit indices","Solutions compared side by side",state,right_from=3);report_note(state,fit_indices_note)}
  if(length(result$solutions)&&!is.null(result$solutions[[k]])) {
    sol <- result$solutions[[k]];kk <- sol$k;stabs <- solution_tables(result,k)
    lab <- if(length(labels)==kk) labels else NULL
    with_label <- function(t){if(is.null(lab)||is.null(t$Cluster))return(t);l <- lab[suppressWarnings(as.integer(as.character(t$Cluster)))];l[is.na(l)] <- "";cbind(t[1],Label=l,t[-1],stringsAsFactors=FALSE)}
    pr <- stabs$Profiles
    if(!is.null(pr)){sil <- stabs$Clusters$Mean_silhouette;if(!is.null(sil))pr$Mean_silhouette <- c(sil,mean(unclass(sol$evaluation$silhouette)[,3]%||%NA))[seq_len(nrow(pr))]
      report_table(with_label(pr),paste0("Cluster profiles (k = ",kk,")"),"Size, share and mean of each variable; for binary variables the mean is the share of the second value",state)}
    if(!is.null(sol$bootstrap)){
      b <- sol$bootstrap$summary[c("Cluster","Mean_Jaccard","P025","P975","Recovery","Dissolution","Valid_B")]
      names(b) <- c("Cluster","Mean Jaccard","2.5%","97.5%","Recovered (J >= 0.75)","Dissolved (J <= 0.50)","Valid replications")
      report_table(with_label(b),paste0("Cluster stability (k = ",kk,")"),paste(sol$bootstrap$sampling,"replications; Jaccard similarity between each cluster and its best match"),state)
      ov <- sol$bootstrap$overall;if(!is.null(ov)){names(ov) <- c("Index","Mean","2.5%","97.5%","Valid replications","Requested replications")
        report_table(ov,paste0("Partition stability (k = ",kk,")"),"Agreement between the solution and its re-estimates on resampled data",state)}
    }
    types <- c("Indices","Profiles","Silhouette","Projection","Dendrogram","Bootstrap")
    if(is.null(sol$fit$tree))types <- setdiff(types,"Dendrogram")
    if(is.null(sol$bootstrap))types <- setdiff(types,"Bootstrap")
    if(is.null(sol$evaluation$silhouette))types <- setdiff(types,"Silhouette")
    if(nrow(result$comparison%||%data.frame())<2)types <- setdiff(types,"Indices")
    for(t in types)report_plot(result,k,t,labels,bw,state)
  }
  if(!is.null(tabs$Missingness)&&any(tabs$Missingness$Missing>0))report_table(tabs$Missingness,"Missing values",NULL,state)
  if(is.data.frame(tabs$Warnings)&&nrow(tabs$Warnings))report_table(tabs$Warnings,"Algorithm warnings",NULL,state,max_rows=30)
  invisible(file)
}
