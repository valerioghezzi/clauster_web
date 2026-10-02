# Quantities of the distance matrix that do not depend on the partition: computed once per analysis.
# Pairs follow the order of as.vector(dist): (2,1), (3,1), ..., (n,1), (3,2), ...
# Ranks (needed only by C-index, Gamma and G-plus) are computed only when requested.
pair_cache <- function(d,ranks=TRUE) {
  n <- attr(d,"Size");dv <- as.vector(d);jj <- rep.int(seq_len(n-1),(n-1):1);ii <- sequence((n-1):1,from=2:n)
  out <- list(n=n,dv=dv,ii=ii,jj=jj)
  if(ranks) {dr <- signif(dv,12);allsort <- sort(dr);values <- unique(allsort);out$rank <- match(dr,values);out$nvalues <- length(values);out$cum <- cumsum(allsort)}
  out
}
# Dunn, Pearson gamma and within/between ratio with the fpc::cluster.stats definitions.
pair_stats <- function(d,c,cache=NULL) {
  pc<-cache%||%pair_cache(d);a<-c[pc$jj];same<-a==c[pc$ii];dv<-pc$dv
  within<-dv[same];between<-dv[!same];sizes<-tabulate(c,max(c))
  avg<-rep(NA_real_,length(sizes));if(length(within)){z<-tapply(within,a[same],mean);avg[as.integer(names(z))]<-z}
  list(dunn=if(length(within)&&length(between))min(between)/max(within) else NA_real_,
    pearsongamma=if(length(within)&&length(between))suppressWarnings(stats::cor(dv,as.numeric(!same))) else NA_real_,
    wb.ratio=if(length(between))stats::weighted.mean(avg,sizes,na.rm=TRUE)/mean(between) else NA_real_)
}
evaluate_partition <- function(p,c,advanced=FALSE,cache=NULL) {
  pc <- cache
  n <- length(c); c <- match(c,sort(unique(c))); k <- max(c); d <- p$distance
  sizes <- tabulate(c,k); base <- list(K=k,N=n,Minimum_N=min(sizes),Singletons=sum(sizes==1),
    EESS=NA_real_,Silhouette=NA_real_,Negative_silhouette_pct=NA_real_,CH=NA_real_,DB=NA_real_,Dunn=NA_real_,Point_biserial=NA_real_,C_index=NA_real_,Gamma=NA_real_,G_plus=NA_real_,WB=NA_real_)
  sil <- NULL
  if(k>1 && k<n) {
    sil <- cluster::silhouette(c,d);base$Silhouette <- mean(sil[,3]);base$Negative_silhouette_pct <- 100*mean(sil[,3]<0)
    pc <- pc %||% pair_cache(d,ranks=advanced);s <- pair_stats(d,c,pc)
    base$Dunn <- s$dunn;base$Point_biserial <- s$pearsongamma;base$WB <- s$wb.ratio
  }
  clusters <- data.frame(Cluster=seq_len(k),N=sizes,Percent=100*sizes/n,Centroid_MSE=NA_real_,Mean_pairwise_ASED=NA_real_,Mean_silhouette=NA_real_)
  if(!is.null(sil)) clusters$Mean_silhouette <- as.numeric(tapply(sil[,3],c,mean))
  centers <- NULL;sd_table <- NULL
  if(p$numeric) {
    centers <- do.call(rbind,lapply(seq_len(k),function(g) colMeans(p$raw[c==g,,drop=FALSE],na.rm=TRUE)))
    sd_table <- do.call(rbind,lapply(seq_len(k),function(g) vapply(p$raw[c==g,,drop=FALSE],sd,numeric(1),na.rm=TRUE)))
  }
  if(p$euclidean) {
    x <- as.matrix(p$X); w <- sum(p$weights); within <- partition_ss(x,c);total <- sum(sweep(x,2,colMeans(x),"-")^2)
    base$EESS <- if(total>0) 100*(1-within/total) else NA_real_
    base$CH <- if(k>1&&n>k) ((total-within)/(k-1))/(within/(n-k)) else NA_real_
    cent <- do.call(rbind,lapply(seq_len(k),function(g) colMeans(x[c==g,,drop=FALSE])))
    scatter <- vapply(seq_len(k),function(g) mean(sqrt(rowSums(sweep(x[c==g,,drop=FALSE],2,cent[g,],"-")^2))),numeric(1))
    if(k>1) { cd <- as.matrix(dist(cent));ratio <- outer(scatter,scatter,"+")/cd;diag(ratio)<- -Inf;base$DB <- mean(apply(ratio,1,max)) }
    for(g in seq_len(k)) {
      z <- x[c==g,,drop=FALSE];clusters$Centroid_MSE[g] <- sum(sweep(z,2,colMeans(z),"-")^2)/(nrow(z)*w)
      clusters$Mean_pairwise_ASED[g] <- if(nrow(z)>1) mean(as.numeric(dist(z))^2)/w else NA_real_
    }
  }
  if(advanced && k>1 && k<n) {
    # Distances are compared at 12 significant digits, so that equal distances with rounding noise are ties.
    if(is.null(pc$rank)) pc <- pair_cache(d);same <- c[pc$ii]==c[pc$jj];N <- length(same);m <- sum(same);cum <- pc$cum;dr <- signif(pc$dv,12)
    if(m>0) {lo <- cum[m];hi <- cum[N]-if(N>m) cum[N-m] else 0;base$C_index <- if(hi>lo) (sum(dr[same])-lo)/(hi-lo) else NA_real_}
    # Ranks of combined distance values count strictly smaller/larger between distances.
    bw <- tabulate(pc$rank[!same],pc$nvalues);below <- c(0,head(cumsum(bw),-1));above <- sum(!same)-cumsum(bw)
    ranks <- pc$rank[same];discord <- sum(below[ranks]);concord <- sum(above[ranks])
    base$Gamma <- if(concord+discord>0) (concord-discord)/(concord+discord) else NA_real_
    base$G_plus <- if(N>1) discord/choose(N,2) else NA_real_
  }
  list(metrics=as.data.frame(base),clusters=clusters,centers=centers,SD=sd_table,silhouette=sil)
}
compare_partitions <- function(a,b) {
  if(anyDuplicated(a$ids)||anyDuplicated(b$ids)) stop("Partition IDs must be unique.")
  ids <- intersect(a$ids,b$ids)
  if(length(ids)<2) stop("The two solutions share fewer than two cases: compare solutions fitted on the same data.",call.=FALSE)
  x <- a$cluster[match(ids,a$ids)];y <- b$cluster[match(ids,b$ids)];tab <- table(x,y);n <- sum(tab)
  p <- tab/n;px <- rowSums(p);py <- colSums(p);nz <- p>0
  mi <- sum(p[nz]*log(p[nz]/outer(px,py)[nz]));h <- function(v) -sum(v[v>0]*log(v[v>0]));hx<-h(px);hy<-h(py)
  pairs <- choose(n,2);sameboth <- sum(choose(tab,2));sa <- sum(choose(rowSums(tab),2));sb <- sum(choose(colSums(tab),2))
  ari <- partition_agreement(x,y)[["ARI"]];kap <- kappa_matched(x,y)
  list(summary=data.frame(Overlap_N=n,Coverage_A=100*n/length(a$ids),Coverage_B=100*n/length(b$ids),ARI=ari,
    Rand=(pairs-sa-sb+2*sameboth)/pairs,VI=hx+hy-2*mi,NMI=if(hx+hy==0) 1 else 2*mi/(hx+hy),Kappa=kap[["Kappa"]],Agreement_pct=kap[["Agreement"]]),table=tab,ids=ids)
}
mixture_metrics <- function(f,n) {
  model <- f$model;ll <- model$loglik;df <- model$df;z <- f$posterior;k <- ncol(z)
  entropy <- -sum(z*log(pmax(z,1e-300)))
  data.frame(Log_likelihood=ll,Parameters=df,AIC=-2*ll+2*df,BIC=-2*ll+log(n)*df,
    SABIC=-2*ll+log((n+2)/24)*df,CAIC=-2*ll+(log(n)+1)*df,
    Relative_entropy=if(k>1) 1-entropy/(n*log(k)) else NA_real_,Mean_max_posterior=mean(apply(z,1,max)))
}

match_centroids <- function(a,b,global=FALSE) {
  a<-as.matrix(a);b<-as.matrix(b)
  if(ncol(a)!=ncol(b))stop("Centroid comparisons require matching variables and scales.")
  d<-matrix(0,nrow(a),nrow(b))
  for(i in seq_len(nrow(a)))for(j in seq_len(nrow(b)))d[i,j]<-mean((a[i,]-b[j,])^2,na.rm=TRUE)
  bad<-!is.finite(d);if(all(bad))stop("Centroids cannot be compared: no variable has observed values in both solutions.")
  if(any(bad))d[bad]<-max(d[!bad])*10+1
  if(global) {
    if(nrow(d)>ncol(d)) {z<-match_centroids(b,a,TRUE);tmp<-z$matches$A;z$matches$A<-z$matches$B;z$matches$B<-tmp;z$distances<-d;return(z)}
    cols<-as.integer(clue::solve_LSAP(d));pairs<-data.frame(A=seq_len(nrow(d)),B=cols,ASED=d[cbind(seq_len(nrow(d)),cols)])
  } else {
    z<-d;pairs<-data.frame()
    for(i in seq_len(min(dim(d)))) {m<-which(z==min(z),arr.ind=TRUE)[1,];pairs<-rbind(pairs,data.frame(A=m[1],B=m[2],ASED=z[m[1],m[2]]));z[m[1],]<-Inf;z[,m[2]]<-Inf}
  }
  list(matches=pairs,distances=d)
}
# Matches the clusters of two solutions by their centroids on a common standardized scale
# (mean and SD of the first solution's data); global minimum-ASED assignment.
compare_centroids <- function(a,b) {
  pa <- a$prepared;pb <- b$prepared
  num <- function(p) names(p$raw)[vapply(p$raw,is.numeric,logical(1))]
  vars <- intersect(num(pa),num(pb))
  if(!length(vars)) stop("Centroid matching needs numeric variables shared by both solutions.")
  xa <- as.matrix(pa$raw[vars]);xb <- as.matrix(pb$raw[vars])
  center <- colMeans(xa,na.rm=TRUE);spread <- apply(xa,2,stats::sd,na.rm=TRUE);spread[!is.finite(spread)|spread==0] <- 1
  za <- sweep(sweep(xa,2,center,"-"),2,spread,"/");zb <- sweep(sweep(xb,2,center,"-"),2,spread,"/")
  cent <- function(z,cl) {k <- max(cl);do.call(rbind,lapply(seq_len(k),function(g)colMeans(z[cl==g,,drop=FALSE],na.rm=TRUE)))}
  ca <- cent(za,a$cluster);cb <- cent(zb,b$cluster)
  mc <- match_centroids(ca,cb,global=TRUE);m <- mc$matches
  raw_d <- outer(seq_len(nrow(ca)),seq_len(nrow(cb)),Vectorize(function(i,j)mean((ca[i,]-cb[j,])^2,na.rm=TRUE)))
  m$ASED <- raw_d[cbind(m$A,m$B)];m$ASED[!is.finite(m$ASED)] <- NA
  na <- tabulate(a$cluster,nrow(ca));nb <- tabulate(b$cluster,nrow(cb))
  out <- data.frame(Cluster_A=as.character(m$A),N_A=na[m$A],Cluster_B=as.character(m$B),N_B=nb[m$B],ASED=m$ASED,stringsAsFactors=FALSE)
  out <- out[order(m$A),,drop=FALSE]
  ua <- setdiff(seq_len(nrow(ca)),m$A);ub <- setdiff(seq_len(nrow(cb)),m$B)
  if(length(ua)) out <- rbind(out,data.frame(Cluster_A=as.character(ua),N_A=na[ua],Cluster_B="not matched",N_B=NA,ASED=NA))
  if(length(ub)) out <- rbind(out,data.frame(Cluster_A="not matched",N_A=NA,Cluster_B=as.character(ub),N_B=nb[ub],ASED=NA))
  rownames(out) <- NULL
  attr(out,"variables") <- vars
  out
}
