# Agglomeration schedule, relative increase of the fusion coefficient, Cohen's kappa between partitions,
# split-half replication (Breckenridge, 1989) and tests of the differences between clusters.

# Agglomeration schedule in the SPSS layout. A cluster is named by its first case (in data order).
# Coefficients: for Ward the within-cluster sum of squares after each stage (the SPSS coefficient with
# squared Euclidean distance); for centroid and median linkage the squared Euclidean distance between the
# merged groups; for the other methods the distance at which the two groups merge.
agglomeration_schedule <- function(tree,method,p) {
  n <- length(tree$order);m <- tree$merge;s <- nrow(m)
  if(is.null(m)||s!=n-1) return(NULL)
  rep_case <- integer(s);formed <- integer(s);member <- function(v) if(v<0) -v else rep_case[v]
  c1 <- c2 <- f1 <- f2 <- integer(s);nxt <- integer(s)
  for(i in seq_len(s)) {
    a <- m[i,1];b <- m[i,2];ra <- member(a);rb <- member(b)
    fa <- if(a<0) 0L else a;fb <- if(b<0) 0L else b
    if(ra>rb) {t <- ra;ra <- rb;rb <- t;t <- fa;fa <- fb;fb <- t}
    c1[i] <- ra;c2[i] <- rb;f1[i] <- fa;f2[i] <- fb;rep_case[i] <- ra
    if(a>0) nxt[a] <- i;if(b>0) nxt[b] <- i
  }
  h <- tree$height
  coef <- if(method %in% c("ward.D2","ward_relocate")) cumsum(h^2/2) else h
  prev <- c(NA,head(coef,-1))
  rel <- ifelse(is.finite(prev)&prev>0,coef/prev-1,NA_real_)
  ids <- p$ids
  data.frame(Stage=seq_len(s),Cluster_1=ids[c1],Cluster_2=ids[c2],Coefficient=coef,Relative_increase=rel,
    Clusters_after_stage=n-seq_len(s),First_stage_cluster_1=f1,First_stage_cluster_2=f2,Next_stage=nxt,
    check.names=FALSE,stringsAsFactors=FALSE)
}
# Hair et al.: stop before the stage with the largest relative increase; k is the number of clusters
# just before that stage, among the k values that were fitted.
schedule_k <- function(sched,ks) {
  if(is.null(sched)||!nrow(sched)) return(NULL)
  r <- sched[is.finite(sched$Relative_increase)&(sched$Clusters_after_stage+1)%in%ks,,drop=FALSE]
  if(!nrow(r)) return(NULL)
  i <- which.max(r$Relative_increase)
  list(k=r$Clusters_after_stage[i]+1,increase=r$Relative_increase[i],stage=r$Stage[i])
}

# Cohen's kappa after the labels of b are matched to those of a so that agreement is largest
# (the equivalent of recoding one classification before crossing them).
kappa_matched <- function(a,b) {
  a <- as.integer(factor(a));b <- as.integer(factor(b));ka <- max(a);kb <- max(b)
  if(ka!=kb) return(c(Kappa=NA_real_,Agreement=NA_real_))
  tab <- table(factor(a,1:ka),factor(b,1:kb))
  perm <- as.integer(clue::solve_LSAP(unclass(tab),maximum=TRUE))
  tab <- tab[,perm,drop=FALSE];n <- sum(tab);po <- sum(diag(tab))/n;pe <- sum(rowSums(tab)*colSums(tab))/n^2
  c(Kappa=if(pe<1) (po-pe)/(1-pe) else NA_real_,Agreement=100*po)
}

# Double cross-validation (Breckenridge, 1989): the cases are split at random into two halves; each half is
# clustered at k, the other half is classified by its nearest centroid, and Cohen's kappa compares the two
# classifications of that half. The coefficient of invariance is the mean of the two kappas.
run_replication <- function(p,cfg,ks) {
  if(!isTRUE(p$numeric)||!is.matrix(p$X)||any(!is.finite(p$X))) stop("it needs numeric variables without missing values after the missing-data step.")
  n <- length(p$ids);set.seed(seed_at(cfg$seed,400000));ord <- sample.int(n);A <- sort(ord[seq_len(floor(n/2))]);B <- sort(setdiff(ord,A))
  cfg$.tree <- NULL;cfg$.extended_search <- cfg$method=="kmeans"
  nearest <- function(x,centers) max.col(-(outer(rowSums(x^2),rep(1,nrow(centers)))-2*x%*%t(centers)+outer(rep(1,nrow(x)),rowSums(centers^2))),ties.method="first")
  centers_of <- function(x,cl) do.call(rbind,lapply(sort(unique(cl)),function(g)colMeans(x[cl==g,,drop=FALSE])))
  rows <- lapply(ks,function(k) {
    out <- tryCatch({
      set.seed(seed_at(cfg$seed,400000+k))
      fa <- fit_partition(subset_prepared(p,A),cfg,k)$cluster;fb <- fit_partition(subset_prepared(p,B),cfg,k)$cluster
      kb <- kappa_matched(fb,nearest(p$X[B,,drop=FALSE],centers_of(p$X[A,,drop=FALSE],fa)))
      ka <- kappa_matched(fa,nearest(p$X[A,,drop=FALSE],centers_of(p$X[B,,drop=FALSE],fb)))
      data.frame(K=k,Kappa_half_B=kb[["Kappa"]],Kappa_half_A=ka[["Kappa"]],Invariance=mean(c(kb[["Kappa"]],ka[["Kappa"]])),Status="Completed")
    },error=function(e)data.frame(K=k,Kappa_half_B=NA_real_,Kappa_half_A=NA_real_,Invariance=NA_real_,Status=conditionMessage(e)))
    out})
  tab <- do.call(rbind,rows);attr(tab,"halves") <- c(length(A),length(B));tab
}

# One-way ANOVA of each variable across the clusters (eta squared, F, p). For the variables that formed the
# clusters the test is descriptive only: the partition was built to make them differ.
variable_tests <- function(values,cluster,role) {
  rows <- lapply(names(values),function(v) {
    y <- profile_values(values[[v]]);ok <- is.finite(y);g <- factor(cluster[ok]);y <- y[ok]
    g <- droplevels(g);k <- nlevels(g);n <- length(y)
    if(k<2||n<=k||var(y)==0) return(NULL)
    # F from the sums of squares, so that clusters with a single case are handled.
    tot <- sum((y-mean(y))^2);bet <- sum(tapply(y,g,function(z)length(z)*(mean(z)-mean(y))^2));wit <- tot-bet
    f <- if(wit>0) (bet/(k-1))/(wit/(n-k)) else NA_real_
    data.frame(Variable=v,Role=role[[v]],Eta_squared=bet/tot,F=f,df1=k-1,df2=n-k,p=if(is.finite(f)) stats::pf(f,k-1,n-k,lower.tail=FALSE) else NA_real_,check.names=FALSE)
  })
  out <- do.call(rbind,rows);if(is.null(out)) return(NULL);rownames(out) <- NULL;out
}
# Compact letter display: clusters that share a letter do not differ significantly (insert-and-absorb).
cluster_letters <- function(k,sig_pairs) {
  sets <- list(seq_len(k))
  for(pr in sig_pairs) {i <- pr[1];j <- pr[2]
    sets <- unlist(lapply(sets,function(s) if(i%in%s&&j%in%s) list(setdiff(s,i),setdiff(s,j)) else list(s)),recursive=FALSE)
    sets <- sets[!vapply(seq_along(sets),function(a)any(vapply(seq_along(sets),function(b)b!=a&&all(sets[[a]]%in%sets[[b]])&&(length(sets[[b]])>length(sets[[a]])||b<a),logical(1))),logical(1))]}
  sets <- sets[order(vapply(sets,min,0))]
  vapply(seq_len(k),function(g)paste(letters[which(vapply(sets,function(s)g%in%s,logical(1)))],collapse=""),"")
}
# Tukey HSD comparisons of every pair of clusters for each variable (alpha = .05 for the letters).
pairwise_tests <- function(values,cluster,role) {
  k <- max(cluster);rows <- list();lets <- list()
  for(v in names(values)) {y <- profile_values(values[[v]]);ok <- is.finite(y);g <- factor(cluster[ok],levels=seq_len(k))
    if(nlevels(droplevels(g))<k||length(y[ok])<=k||var(y[ok])==0) next
    t <- tryCatch(stats::TukeyHSD(stats::aov(y[ok]~g))$g,error=function(e)NULL);if(is.null(t)||any(!is.finite(t[,"p adj"]))) next
    pr <- do.call(rbind,strsplit(rownames(t),"-"))
    rows[[v]] <- data.frame(Variable=v,Role=role[[v]],Cluster_a=as.integer(pr[,2]),Cluster_b=as.integer(pr[,1]),Difference=-t[,"diff"],p_Tukey=t[,"p adj"],check.names=FALSE,row.names=NULL)
    sig <- lapply(which(t[,"p adj"]<.05),function(i)as.integer(pr[i,]))
    lets[[v]] <- paste(cluster_letters(k,sig),collapse=" / ")}
  if(!length(rows)) return(NULL)
  list(table=do.call(rbind,unname(rows)),letters=unlist(lets))
}
# Illustrative variables: numeric columns of the original data, for the cases kept in the analysis.
illustrative_values <- function(p,vars) {
  vars <- setdiff(vars %||% character(),names(p$raw));if(!length(vars)) return(NULL)
  d <- p$original[p$rows,vars,drop=FALSE]
  for(v in vars) {x <- d[[v]];if(is.factor(x)||is.character(x)) {num <- suppressWarnings(as.numeric(as.character(x)));if(any(is.na(num)&!is.na(x)))stop(sprintf("Illustrative variable %s contains text: only numeric variables can be described.",v),call.=FALSE);x <- num}
    d[[v]] <- as.numeric(x)}
  d
}
