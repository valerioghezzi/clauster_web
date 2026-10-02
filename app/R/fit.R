# TRUE when a package is installed and loads. Checking the installation first matters in the browser
# version, where requireNamespace() on a missing package tries a download and fails with an error.
has_package <- function(p) nzchar(system.file(package=p)) && isTRUE(tryCatch(requireNamespace(p,quietly=TRUE),error=function(e)FALSE))
partition_ss <- function(x,c) {
  x <- as.matrix(x);x <- sweep(x,2,colMeans(x));sizes <- as.numeric(table(c));sums <- rowsum(x,c)
  max(0,sum(x^2)-sum(sums^2/sizes))
}
relocate_partition <- function(x,initial,max_iter=100L) {
  x <- as.matrix(x); c <- match(initial,sort(unique(initial))); k <- max(c)
  sizes <- tabulate(c,k); sums <- rowsum(x,c,reorder=TRUE)
  it <- 0L;mv <- 0L;ss <- partition_ss(x,c)
  for(iter in seq_len(max_iter)) {
    moved <- 0L
    for(i in seq_len(nrow(x))) {
      a <- c[i]; na <- sizes[a]
      if(na<=1) next
      xi <- x[i,]; centers <- sums/sizes
      d2 <- colSums((t(centers)-xi)^2)
      costs <- sizes/(sizes+1)*d2-na/(na-1)*d2[a]
      costs[a] <- Inf
      b <- which.min(costs)
      if(costs[b]< -1e-10) {
        c[i] <- b;moved <- moved+1L
        sizes[a] <- sizes[a]-1L;sizes[b] <- sizes[b]+1L
        sums[a,] <- sums[a,]-xi;sums[b,] <- sums[b,]+xi
      }
    }
    it <- c(it,iter);mv <- c(mv,moved);ss <- c(ss,partition_ss(x,c))
    if(!moved) break
  }
  list(cluster=c,trace=data.frame(Iteration=it,Moved=mv,Within_SS=ss),converged=tail(mv,1)==0)
}
method_labels <- c(ward_relocate="Ward + relocation",ward.D2="Ward D2",ward.D="Ward D (legacy)",kmeans="K-means",pam="PAM",
  average="Average linkage",complete="Complete linkage",single="Single linkage",mcquitty="Weighted average linkage",centroid="Centroid linkage",
  median="Median linkage",flexible="Flexible beta",diana="DIANA",fuzzy="Fuzzy clustering",gmm="Gaussian mixture",
  hybrid_kmeans="Hierarchical + K-means",hybrid_relocate="Hierarchical + relocation",hybrid_pam="Hierarchical + PAM")
euclidean_methods <- c("ward.D","ward.D2","centroid","median","ward_relocate","kmeans","fuzzy","gmm","hybrid_kmeans","hybrid_relocate")
hybrid_methods <- c("hybrid_kmeans","hybrid_relocate","hybrid_pam")
# Builds the tree of a hierarchical method; Ward, centroid and median work on Euclidean coordinates.
hierarchical_tree <- function(p,cfg,method) {
  if(method %in% c("ward.D","ward.D2","centroid","median")) {
    if(!p$euclidean) stop("This hierarchical start requires complete numeric data in Euclidean space.")
    d <- if(method %in% c("ward.D","ward.D2")) dist(p$X) else as.dist(as.matrix(dist(p$X))^2)
    hclust(d,method=method)
  } else if(method %in% c("single","complete","average","mcquitty")) hclust(p$distance,method=method)
  else if(method=="flexible") as.hclust(cluster::agnes(p$distance,diss=TRUE,method="flexible",par.method=c((1-cfg$beta)/2,(1-cfg$beta)/2,cfg$beta,0)))
  else if(method=="diana") as.hclust(cluster::diana(p$distance,diss=TRUE))
  else stop("Unknown hierarchical starting method.")
}
tree_methods <- c("single","complete","average","mcquitty","ward.D","ward.D2","centroid","median")
# The tree does not depend on k: the engine builds it once (method_tree) and cuts it for every k.
method_tree <- function(p,cfg) {
  m <- cfg$method
  if(m %in% c(tree_methods,"flexible","diana")) hierarchical_tree(p,cfg,m)
  else if(m=="ward_relocate") hierarchical_tree(p,cfg,"ward.D2")
  else if(m %in% hybrid_methods) hierarchical_tree(p,cfg,cfg$hybrid_start)
  else NULL
}
# Two-stage fit: the hierarchical partition at k seeds K-means, relocation or PAM.
fit_hybrid <- function(p,cfg,k) {
  x <- as.matrix(p$X);tree <- cfg$.tree %||% hierarchical_tree(p,cfg,cfg$hybrid_start);initial <- cutree(tree,k)
  model <- list(initial_cluster=initial,hierarchical_start=cfg$hybrid_start);trace <- NULL
  if(cfg$method=="hybrid_pam") {
    dm <- as.matrix(p$distance)
    med <- vapply(seq_len(k),function(g){ix <- which(initial==g);ix[which.min(rowSums(dm[ix,ix,drop=FALSE]))]},integer(1))
    final <- cluster::pam(p$distance,k,diss=TRUE,medoids=med);c <- final$clustering
    model <- c(model,list(initial_medoids=med,initial_objective=sum(apply(dm[,med,drop=FALSE],1,min)),final=final,refinement="pam"))
  } else {
    centers <- do.call(rbind,lapply(seq_len(k),function(g)colMeans(x[initial==g,,drop=FALSE])))
    model <- c(model,list(initial_centers=centers,initial_withinss=partition_ss(x,initial)))
    if(cfg$method=="hybrid_kmeans") {
      final <- stats::kmeans(x,centers=centers,iter.max=cfg$max_iter,algorithm=cfg$hybrid_algorithm);c <- final$cluster;model$refinement <- "kmeans"
    } else {final <- relocate_partition(x,initial,cfg$max_iter);c <- final$cluster;trace <- final$trace;model$refinement <- "relocation"}
    model$final <- final
  }
  list(cluster=c,tree=tree,model=model,trace=trace)
}
check_method <- function(p,cfg) {
  # These methods work on Euclidean coordinates: another distance choice would be silently ignored.
  if(cfg$method %in% euclidean_methods && p$numeric && cfg$distance=="sqeuclidean")
    stop(sprintf("%s works on Euclidean coordinates, so Squared Euclidean distance would not be used. Select Distance = Euclidean, or choose a linkage method or PAM to work with squared distances.",method_labels[[cfg$method]]),call.=FALSE)
  if(cfg$method %in% euclidean_methods && !p$euclidean)
    stop(sprintf("%s requires numeric variables and Euclidean distance. Select Distance = Euclidean, or choose PAM, a linkage method, flexible beta or DIANA for %s distance.",
      method_labels[[cfg$method]],if(p$numeric) cfg$distance else "mixed-variable Gower"),call.=FALSE)
  if(cfg$method %in% hybrid_methods && cfg$hybrid_start %in% c("ward.D","ward.D2","centroid","median") && !p$euclidean)
    stop("Ward, centroid and median starts require Euclidean distance. Select Hierarchical start = Average linkage (or another linkage), or Distance = Euclidean.",call.=FALSE)
  invisible(TRUE)
}
# k-means++ starting centers (Arthur and Vassilvitskii, 2007).
kmeanspp_centers <- function(x,k) {
  n <- nrow(x);pick <- sample.int(n,1);d <- rowSums(sweep(x,2,x[pick,])^2)
  for(j in seq_len(k-1)) {i <- if(sum(d)>0) sample.int(n,1,prob=d) else sample.int(n,1);pick <- c(pick,i);d <- pmin(d,rowSums(sweep(x,2,x[i,])^2))}
  x[pick,,drop=FALSE]
}
# Wider search for the reported k-means solution: random starts can stop in a worse local optimum,
# so k-means++ starts and the Ward D2 partition are tried as well and the lowest within-SS is kept.
kmeans_extended <- function(x,k,best,starts,iter) {
  try_fit <- function(centers) {m <- tryCatch(suppressWarnings(kmeans(x,centers,iter.max=iter)),error=function(e)NULL)
    if(!is.null(m)&&length(unique(m$cluster))==k&&m$tot.withinss<best$tot.withinss) best <<- m}
  for(i in seq_len(starts)) {ce <- kmeanspp_centers(x,k);if(nrow(unique(ce))==k) try_fit(ce)}
  w <- cutree(hclust(dist(x),"ward.D2"),k);try_fit(do.call(rbind,lapply(seq_len(k),function(g)colMeans(x[w==g,,drop=FALSE]))))
  best
}
fit_partition <- function(p,cfg=list(),k) {
  cfg <- cw_config(cfg);n <- length(p$ids); x <- p$X;method <- cfg$method
  if(length(k)!=1 || !is.finite(k)||k<1||k>n||k!=as.integer(k)) stop("k must be an integer between 1 and N.")
  if(k==1 && method!="gmm") return(list(cluster=rep(1L,n),method=method,k=1L))
  if(method %in% euclidean_methods && !p$euclidean) stop("This method requires complete numeric data in Euclidean space.")
  if(p$numeric && (u <- nrow(unique(as.data.frame(x))))<k)stop(sprintf("k = %d is more than the data allow: the cases have only %d distinct profiles. Lower the \"To k\" value.",k,u),call.=FALSE)
  tree <- NULL;model <- NULL;trace <- NULL;posterior <- NULL
  if(method %in% c(tree_methods,"flexible","diana")) {tree <- cfg$.tree %||% method_tree(p,cfg);c <- cutree(tree,k)}
  else if(method=="ward_relocate") {
    tree <- cfg$.tree %||% method_tree(p,cfg);model <- relocate_partition(x,cutree(tree,k),cfg$max_iter);c <- model$cluster;trace <- model$trace
  } else if(method %in% hybrid_methods) { h <- fit_hybrid(p,cfg,k);tree <- h$tree;model <- h$model;trace <- h$trace;c <- h$cluster }
  else if(method=="kmeans") {
    model <- kmeans(x,k,nstart=cfg$nstart,iter.max=cfg$max_iter)
    if(isTRUE(cfg$.extended_search)) model <- kmeans_extended(x,k,model,cfg$nstart,cfg$max_iter)
    c <- model$cluster }
  else if(method=="pam") { model <- cluster::pam(p$distance,k,diss=TRUE);c <- model$clustering }
  else if(method=="fuzzy") {
    if(k>n/2-1) stop(sprintf("Fuzzy clustering needs at least 2(k + 1) cases: %d cases allow at most %d clusters. Lower the \"To k\" value.",n,max(1,floor(n/2-1))),call.=FALSE);model <- cluster::fanny(dist(x),k,maxit=cfg$max_iter);c <- model$clustering;posterior <- model$membership }
  else if(method=="gmm") {
    if(!has_package("mclust")) stop("Gaussian mixtures need the mclust package, which is not available in this installation. Choose another method.",call.=FALSE)
    mclustBIC <- mclust::mclustBIC
    model <- mclust::Mclust(x,G=k,modelNames=if(cfg$covariance=="all") NULL else cfg$covariance,verbose=FALSE)
    if(is.null(model$classification)||!is.finite(model$loglik)) stop("The Gaussian mixture could not be estimated for this k: the model has more parameters than the data support. Choose a simpler covariance structure (for example EII or VII) or a smaller k.",call.=FALSE)
    c <- model$classification;posterior <- model$z
  } else stop("Unknown clustering method.")
  if(method=="fuzzy"&&length(unique(c))!=k) stop(sprintf("Fuzzy clustering did not separate the cases into %d groups: the memberships are nearly equal (about 1/%d) for every case, which happens with weak structure or many variables. Use fewer variables, a smaller k or another method.",k,k),call.=FALSE)
  if(length(unique(c))!=k) stop(sprintf("The fit collapsed to fewer groups than requested: some of the %d clusters ended up empty. Use a smaller k or another method.",k),call.=FALSE)
  list(cluster=as.integer(c),method=method,k=k,tree=tree,model=model,trace=trace,posterior=posterior)
}
