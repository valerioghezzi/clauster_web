# Comparative table of fit indices: one row per index, one column per k, with direction and best k.
fit_index_spec <- data.frame(stringsAsFactors=FALSE,
  column=c("N","Minimum_N","Singletons","EESS","Silhouette","Negative_silhouette_pct","CH","DB","Dunn","Point_biserial","C_index","Gamma","G_plus","WB",
    "Log_likelihood","Parameters","AIC","BIC","SABIC","CAIC","Relative_entropy","Mean_max_posterior",
    "Bootstrap_mean_Jaccard","Bootstrap_min_Jaccard","Bootstrap_ARI","Bootstrap_ARI_P025","Bootstrap_ARI_P975","Bootstrap_Rand","Bootstrap_Rand_P025","Bootstrap_Rand_P975","Bootstrap_valid_B",
    "Simulation_p_Silhouette","Simulation_p_CH","Simulation_p_EESS"),
  label=c("Cases analyzed","Smallest cluster","Single-case clusters","Explained variance (EESS %)","Mean silhouette","Negative silhouettes %","Calinski-Harabasz","Davies-Bouldin","Dunn","Point-biserial correlation","C-index","Goodman-Kruskal Gamma","G-plus","Within/between distance ratio",
    "Log likelihood","Parameters","AIC","BIC","Sample-size adjusted BIC","CAIC","Relative entropy","Mean maximum posterior",
    "Bootstrap Jaccard, mean of clusters","Bootstrap Jaccard, least stable cluster","Bootstrap adjusted Rand index","Bootstrap ARI 2.5%","Bootstrap ARI 97.5%","Bootstrap Rand index","Bootstrap Rand 2.5%","Bootstrap Rand 97.5%","Bootstrap valid replications",
    "Null simulation p, silhouette","Null simulation p, Calinski-Harabasz","Null simulation p, EESS"),
  better=c(NA,NA,NA,NA,"higher","lower","higher","lower","higher","higher","lower","higher","lower","lower",
    NA,NA,"lower","lower","lower","lower","higher","higher",
    "higher","higher","higher",NA,NA,"higher",NA,NA,NA,
    "lower","lower","lower"))
format_index <- function(v) {
  out <- rep("",length(v));out[is.infinite(v)] <- ifelse(v[is.infinite(v)]>0,"Inf","-Inf");ok <- is.finite(v)
  a <- abs(v[ok]);d <- ifelse(a>=1000,1,ifelse(a>=100,2,3))
  int <- rep(all(abs(v[ok]-round(v[ok]))<1e-9 & a<1e7),sum(ok))
  out[ok] <- if(all(int)) formatC(round(v[ok]),format="d",big.mark="") else vapply(seq_along(a),function(i)formatC(v[ok][i],format="f",digits=d[i]),"")
  out
}
fit_indices <- function(result,formatted=TRUE) {
  cmp <- result$comparison
  if(is.null(cmp)||!nrow(cmp)||is.null(cmp$K)) return(data.frame())
  ks <- cmp$K;rows <- list()
  # Solutions with single-case clusters are degenerate: they are shown but not proposed as best k.
  single <- if(is.null(cmp$Singletons)) rep(NA_real_,length(ks)) else suppressWarnings(as.numeric(cmp$Singletons))
  eligible <- is.na(single)|single==0
  if(!any(eligible[ks>1])) eligible[] <- TRUE
  add <- function(label,values,better,best=NULL,skip_k1=FALSE) {
    values <- suppressWarnings(as.numeric(values))
    if(!any(!is.na(values))) return(invisible(NULL))
    cand <- values;if(skip_k1) cand[ks==1] <- NA;cand[!eligible] <- NA
    if(is.null(best)&&!is.na(better)&&sum(!is.na(cand))>1) {
      target <- if(better=="higher") max(cand,na.rm=TRUE) else min(cand,na.rm=TRUE)
      hit <- ks[!is.na(cand)&(cand==target|(is.finite(target)&abs(cand-target)<=1e-12*max(1,abs(target))))]
      best <- if(length(hit)>=sum(!is.na(cand))) "" else if(length(hit)>3) "tie" else paste(hit,collapse=", ")
    }
    rows[[length(rows)+1]] <<- list(label=label,better=if(is.na(better))"" else better,values=values,best=best%||%"")
  }
  for(i in seq_len(nrow(fit_index_spec))) {col <- fit_index_spec$column[i];if(col%in%names(cmp))add(fit_index_spec$label[i],cmp[[col]],fit_index_spec$better[i],skip_k1=col%in%c("Mean_max_posterior","Relative_entropy"))}
  g <- result$gap$table
  if(!is.null(g)) {
    cand <- result$gap$candidate
    add("Gap statistic",g$gap[match(ks,g$K)],NA,best=if(is.null(cand))"" else if(cand%in%ks) as.character(cand) else paste(cand,"(outside range)"))
    add("Gap standard error",g$SE.sim[match(ks,g$K)],NA,best="")
  }
  if(!is.null(cmp$Status)&&any(cmp$Status!="Completed"))
    rows[[length(rows)+1]] <- list(label="Status",better="",values=rep(NA,length(ks)),best="",text=ifelse(cmp$Status=="Completed","Completed",paste("Failed:",cmp$Status)))
  if(!length(rows)) return(data.frame())
  out <- data.frame(Index=vapply(rows,`[[`,"",i="label"),Better=vapply(rows,`[[`,"",i="better"),stringsAsFactors=FALSE,check.names=FALSE)
  for(j in seq_along(ks)) out[[paste("k =",ks[j])]] <- if(formatted) vapply(rows,function(r)if(!is.null(r$text))r$text[j] else format_index(r$values)[j],"") else vapply(rows,function(r)r$values[j],0)
  out[["Best k"]] <- vapply(rows,`[[`,"",i="best")
  out
}
fit_indices_note <- "Better: whether higher or lower values indicate a better solution. Best k: the k with the best value; Gap uses the Tibshirani (2001) one-standard-error rule. EESS, log likelihood and parameters change with k by construction and have no best k. Solutions with single-case clusters are not proposed as best k. Indices often disagree: read them together with profiles, stability and substantive meaning."
# Suggested k: the k favored by most internal indices (a split vote counts in part; a tie goes to the smaller k).
# For Gaussian mixtures the BIC decides, as is customary.
suggested_k <- function(result) {
  ks <- result$comparison$K;if(!length(result$solutions)) return(NULL)
  fi <- fit_indices(result);if(!nrow(fi)) return(names(result$solutions)[1])
  avail <- names(result$solutions)
  bic <- fi$`Best k`[fi$Index=="BIC"]
  if(length(bic)&&bic%in%avail) return(bic)
  rows <- fi[nzchar(fi$Better)&!grepl("^(Bootstrap|Null simulation)",fi$Index)&nzchar(fi$`Best k`)&fi$`Best k`!="tie",,drop=FALSE]
  votes <- setNames(numeric(length(avail)),avail)
  for(b in rows$`Best k`){h <- intersect(trimws(strsplit(b,",")[[1]]),avail);if(length(h))votes[h] <- votes[h]+1/length(h)}
  gap <- result$gap$candidate;if(!is.null(gap)&&as.character(gap)%in%avail) votes[as.character(gap)] <- votes[as.character(gap)]+1
  if(!any(votes>0)) return(avail[1])
  names(votes)[which.max(votes)]
}
# Plain-language reading of a solution. Thresholds: Kaufman and Rousseeuw (1990) for the
# silhouette, Hennig (2007) for bootstrap Jaccard stability.
key_findings <- function(result,k=NULL) {
  k <- as.character(k %||% suggested_k(result) %||% "");sol <- result$solutions[[k]]
  if(is.null(sol)) return(data.frame(Topic=character(),Finding=character()))
  out <- list();add <- function(t,f) out[[length(out)+1]] <<- data.frame(Topic=t,Finding=f,stringsAsFactors=FALSE)
  sk <- suggested_k(result);fi <- fit_indices(result)
  if(length(result$solutions)>1&&!is.null(sk)) {
    rows <- fi[nzchar(fi$Better)&!grepl("^(Bootstrap|Null simulation)",fi$Index)&nzchar(fi$`Best k`)&fi$`Best k`!="tie",,drop=FALSE]
    avail <- names(result$solutions);gap <- result$gap$candidate
    hits <- vapply(avail,function(kk)sum(vapply(strsplit(rows$`Best k`,","),function(z)kk%in%trimws(z),logical(1)))+isTRUE(as.character(gap)==kk),numeric(1))
    total <- nrow(rows)+!is.null(gap)
    other <- setdiff(names(sort(hits,decreasing=TRUE)),sk)[1]
    close <- !is.na(other)&&hits[other]>=hits[sk]-1&&hits[other]>0
    why <- if(any(fi$Index=="BIC")&&identical(fi$`Best k`[fi$Index=="BIC"],sk)) "lowest BIC" else
      paste0(sprintf("best on %d of %d criteria",hits[sk],total),if(close) sprintf("; k = %s is best on %d, so compare both profiles",other,hits[other]) else "")
    add("Suggested solution",sprintf("k = %s (%s).%s",sk,why,if(k!=sk) sprintf(" This page shows k = %s.",k) else ""))
  }
  n <- length(sol$cluster);sz <- tabulate(sol$cluster,sol$k)
  add("Cluster sizes",paste0(paste(sprintf("%d (%.0f%%)",sz,100*sz/n),collapse=", "),
    if(min(sz)<max(5,.05*n)) sprintf(". The smallest cluster has only %d cases: interpret it with caution.",min(sz)) else "."))
  sil <- result$comparison$Silhouette[result$comparison$K==sol$k]
  if(length(sil)&&is.finite(sil)) add("Separation",sprintf("Mean silhouette %.2f: %s.",sil,
    if(sil>.70) "strong structure" else if(sil>.50) "reasonable structure" else if(sil>.25) "weak structure, clusters partly overlap" else "no substantial structure"))
  if(!is.null(sol$bootstrap$summary)) {
    j <- sol$bootstrap$summary$Mean_Jaccard;lo <- min(j,na.rm=TRUE)
    grade <- function(v) if(v>=.85) "highly stable" else if(v>=.75) "stable" else if(v>=.60) "only partly stable (a pattern, but membership is uncertain)" else "not stable"
    ov <- sol$bootstrap$overall;ari <- if(!is.null(ov)) ov[ov[[1]]%in%c("ARI","Adjusted Rand index"),,drop=FALSE] else NULL
    hi <- max(j,na.rm=TRUE)
    add("Stability",sprintf("%s: %s.%s",if(sprintf("%.2f",lo)==sprintf("%.2f",hi)) sprintf("Bootstrap Jaccard %.2f for every cluster",lo) else sprintf("Bootstrap Jaccard from %.2f to %.2f",lo,hi),
      if(length(j)>1&&sprintf("%.2f",lo)!=sprintf("%.2f",hi)) paste("the least stable cluster is",grade(lo)) else paste("the clusters are",grade(lo)),
      if(!is.null(ari)&&nrow(ari)) sprintf(" Adjusted Rand index %.2f (95%% interval %.2f to %.2f).",ari[[2]][1],ari[[3]][1],ari[[4]][1]) else ""))
  }
  ps <- result$comparison$Simulation_p_Silhouette[result$comparison$K==sol$k]
  if(length(ps)&&is.finite(ps)) add("Null simulation",sprintf("p = %.3f for the silhouette: %s.",ps,
    if(ps<=.05) "the structure is stronger than in comparable data without clusters" else "the structure is not clearly stronger than in data without clusters"))
  raw <- result$prepared$raw
  if(!is.null(raw)&&NCOL(raw)>0) {
    raw <- as.data.frame(raw,check.names=FALSE)
    # Eta squared: share of the variance of each variable explained by the clusters (ordinal codes as numbers).
    eff <- vapply(names(raw),function(v){x <- profile_values(raw[[v]]);ok <- !is.na(x);if(sum(ok)<3) return(NA_real_)
      tot <- sum((x[ok]-mean(x[ok]))^2);if(tot<=0) return(NA_real_);m <- ave(x[ok],sol$cluster[ok]);sum((m-mean(x[ok]))^2)/tot},numeric(1))
    eff <- sort(eff[is.finite(eff)],decreasing=TRUE)
    if(length(eff)) {top <- head(names(eff),3)
      add("Distinguishing variables",paste0(paste(sprintf("%s %.2f",top,eff[top]),collapse=", "),
        " (eta squared, the share of variance explained by the clusters). The profile plot shows the direction."))}
  }
  if(!is.null(sol$posterior)&&!is.null(result$comparison$Relative_entropy)) {
    re <- result$comparison$Relative_entropy[result$comparison$K==sol$k]
    if(length(re)&&is.finite(re)) add("Classification",sprintf("Relative entropy %.2f: %s.",re,if(re>=.8) "cases are assigned with high certainty" else if(re>=.6) "assignment is fairly certain" else "many cases are uncertain between clusters"))
  }
  do.call(rbind,out)
}
