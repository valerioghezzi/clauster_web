empirical_p <- function(observed, simulated, higher=TRUE) {
  v <- simulated[is.finite(simulated)]
  if(!length(v)||!is.finite(observed)) return(NA_real_)
  (1+sum(if(higher) v>=observed else v<=observed))/(length(v)+1)
}
# Rand and adjusted Rand index between two labelings of the same cases.
partition_agreement <- function(x,y) {
  tab<-table(x,y);n<-sum(tab);if(n<2)return(c(ARI=NA_real_,Rand=NA_real_))
  pairs<-choose(n,2);sb<-sum(choose(tab,2));sa<-sum(choose(rowSums(tab),2));sc<-sum(choose(colSums(tab),2))
  rand<-(pairs-sa-sc+2*sb)/pairs;expected<-sa*sc/pairs;mx<-(sa+sc)/2
  c(ARI=if(mx==expected)1 else (sb-expected)/(mx-expected),Rand=rand)
}
# Derived seeds stay valid integers for any allowed base seed.
seed_at <- function(seed,offset=0) as.integer((as.numeric(seed)+offset-1)%%(.Machine$integer.max-1)+1)
budget_ok <- function(cfg) is.null(cfg$.deadline) || as.numeric(Sys.time())<cfg$.deadline
subset_prepared <- function(p,ix) {
  q <- p;q$X <- p$X[ix,,drop=FALSE];q$raw<-p$raw[ix,,drop=FALSE];q$ids<-p$ids[ix];q$rows<-p$rows[ix]
  rowwise <- c("euclidean","sqeuclidean","manhattan","maximum","minkowski","correlation","cosine","jaccard","matching")
  q$distance <- if(isTRUE(p$numeric) && p$config$distance %in% c("jaccard","matching")) metric_distance(q$raw,p$config$distance,p$weights) else if(isTRUE(p$numeric) && p$config$distance %in% rowwise) metric_distance(q$X,p$config$distance) else as.dist(as.matrix(p$distance)[ix,ix,drop=FALSE])
  q
}
run_bootstrap <- function(p,fit,cfg=list(),progress=function(...)NULL) {
  cfg<-cw_config(cfg);set.seed(cfg$seed);n<-length(p$ids);k<-fit$k
  out<-matrix(NA_real_,cfg$B,k);indices<-rep(NA_real_,cfg$B);ari<-rep(NA_real_,cfg$B);rand<-rep(NA_real_,cfg$B);errors<-character(cfg$B);done<-0L
  original <- fit$cluster
  for(b in seq_len(cfg$B)) {
    if(!budget_ok(cfg)) break
    set.seed(seed_at(cfg$seed,b))
    ix <- sample.int(n,if(cfg$subsample)min(n,max(min(cfg$k_max,n-1)+1,ceiling(n*cfg$fraction))) else n,replace=!cfg$subsample)
    res<-tryCatch({
      q<-subset_prepared(p,ix);f<-fit_partition(q,cfg,k)
      # Clusterwise Jaccard as in fpc::clusterboot (Hennig, 2007): computed on the resampled cases,
      # repeated cases included; a cluster with no resampled case has no value in that replicate.
      cross<-unclass(table(factor(original[ix],levels=seq_len(k)),factor(f$cluster,levels=seq_len(k))))
      union<-outer(rowSums(cross),colSums(cross),"+")-cross
      jac<-ifelse(union>0,cross/union,0)
      present<-rowSums(cross)>0
      out[b,present]<-apply(jac[present,,drop=FALSE],1,max)
      # ARI and Rand compare distinct cases only; a repeated case takes its most frequent new label.
      unique_ix<-unique(ix)
      counts<-table(factor(ix,levels=unique_ix),factor(f$cluster,levels=seq_len(k)))
      labels<-max.col(unclass(counts),ties.method="first")
      agree<-partition_agreement(original[unique_ix],labels);ari[b]<-agree[["ARI"]];rand[b]<-agree[["Rand"]]
      indices[b]<-if(k>1&&length(unique(f$cluster))>1)mean(cluster::silhouette(f$cluster,q$distance)[,3]) else NA_real_
      NULL
    },error=function(e)conditionMessage(e))
    if(!is.null(res)) errors[b]<-res
    done<-b;progress(paste0(if(cfg$subsample)"Subsampling" else "Bootstrap",", k = ",k),b,cfg$B)
  }
  out<-out[seq_len(done),,drop=FALSE];indices<-indices[seq_len(done)];ari<-ari[seq_len(done)];rand<-rand[seq_len(done)];errors<-errors[seq_len(done)]
  q<-function(v,pr){v<-v[is.finite(v)];if(length(v))unname(stats::quantile(v,pr))else NA_real_}
  fmean<-function(v){v<-v[is.finite(v)];if(length(v))mean(v)else NA_real_}
  overall<-data.frame(Index=c("Adjusted Rand index","Rand index"),Mean=c(fmean(ari),fmean(rand)),
    P025=c(q(ari,.025),q(rand,.025)),P975=c(q(ari,.975),q(rand,.975)),Valid_B=c(sum(is.finite(ari)),sum(is.finite(rand))),Requested_B=cfg$B)
  stats<-do.call(rbind,lapply(seq_len(k),function(g) {
    z<-out[,g];valid<-z[is.finite(z)]
    data.frame(Cluster=g,Mean_Jaccard=if(length(valid))mean(valid) else NA,
      P025=if(length(valid))unname(quantile(valid,.025)) else NA,P975=if(length(valid))unname(quantile(valid,.975)) else NA,
      Dissolution=if(length(valid))mean(valid<=.5) else NA,Recovery=if(length(valid))mean(valid>=.75) else NA,
      Valid_B=length(valid),Failed_B=sum(nzchar(errors)),Completed_B=done,Requested_B=cfg$B)
  }))
  list(summary=stats,overall=overall,replicates=as.data.frame(out),indices=data.frame(Replicate=seq_len(done),Silhouette=indices,ARI=ari,Rand=rand,Error=errors),
    scope="Conditional on prepared data",sampling=if(cfg$subsample)"Subsampling" else "Bootstrap")
}
simulate_prepared <- function(p,cfg) {
  q<-p
  if(cfg$null=="gaussian") {
    if(!p$numeric) stop("The Gaussian reference requires complete numeric data.")
    x<-as.matrix(p$X);cv<-as.matrix(cov(x));e<-eigen(cv,symmetric=TRUE)
    q$X<-matrix(rnorm(length(x)),nrow(x),ncol(x))%*%(diag(sqrt(pmax(e$values,0)),ncol(x))%*%t(e$vectors))
    q$X<-sweep(q$X,2,colMeans(x),"+");colnames(q$X)<-colnames(x)
    q$raw<-as.data.frame(q$X)
  } else {
    orders<-lapply(seq_len(ncol(p$raw)),function(j)sample.int(nrow(p$raw)))
    q$raw<-p$raw
    for(j in seq_along(orders))q$raw[[j]]<-p$raw[[j]][orders[[j]]]
    if(p$numeric) {
      q$X<-p$X
      for(j in seq_along(orders))q$X[,j]<-p$X[orders[[j]],j]
    } else q$X<-q$raw
  }
  q$distance<-if(cfg$distance=="gower") metric_distance(q$raw,"gower",p$weights) else if(cfg$distance%in%c("jaccard","matching")) metric_distance(q$raw,cfg$distance,p$weights) else metric_distance(q$X,cfg$distance)
  q
}
run_simulation <- function(p,fit,cfg=list(),progress=function(...)NULL) {
  cfg<-cw_config(cfg);set.seed(seed_at(cfg$seed,100000))
  obs<-evaluate_partition(p,fit$cluster,cfg$advanced_indices)$metrics
  fields<-c("EESS","Silhouette","CH","DB","Dunn","Point_biserial","C_index","Gamma","G_plus","WB")
  sims<-matrix(NA_real_,cfg$B,length(fields),dimnames=list(NULL,fields));errors<-character(cfg$B);done<-0L
  for(b in seq_len(cfg$B)) {
    if(!budget_ok(cfg)) break
    set.seed(seed_at(cfg$seed,100000+b))
    res<-tryCatch({q<-simulate_prepared(p,cfg);f<-fit_partition(q,cfg,fit$k);e<-evaluate_partition(q,f$cluster,cfg$advanced_indices)$metrics
      sims[b,]<-as.numeric(e[1,fields]);NULL},error=function(e)conditionMessage(e))
    if(!is.null(res))errors[b]<-res
    done<-b;progress(paste0("Null simulation, k = ",fit$k),b,cfg$B)
  }
  sims<-sims[seq_len(done),,drop=FALSE];errors<-errors[seq_len(done)]
  summary<-do.call(rbind,lapply(fields,function(v) {
    z<-sims[,v];z<-z[is.finite(z)];higher<-!v%in%c("DB","C_index","G_plus","WB");pv<-empirical_p(obs[[v]],z,higher)
    data.frame(Index=v,Observed=obs[[v]],Null_mean=if(length(z))mean(z)else NA,Null_SD=if(length(z)>1)sd(z)else NA,
      Empirical_p=pv,MC_SE=if(length(z))sqrt(pv*(1-pv)/(length(z)+1))else NA,Valid_B=length(z),Failed_B=sum(nzchar(errors)),Completed_B=done,Requested_B=cfg$B)
  }))
  list(summary=summary,replicates=as.data.frame(sims),errors=errors,null=cfg$null,
    inference="Exploratory, conditional on prepared data and selected k")
}
run_gap <- function(p,cfg,progress=function(...)NULL) {
  if(!p$euclidean) stop("it needs numeric variables, complete cases and Euclidean or squared Euclidean distance. Select one of these distances to obtain it.")
  set.seed(seed_at(cfg$seed,200000))
  # Reference data sets are clustered with the same method and distance as the analysis.
  fun<-function(x,k) {q<-p;q$X<-x;q$raw<-as.data.frame(x);q$distance<-metric_distance(x,cfg$distance);list(cluster=fit_partition(q,cfg,k)$cluster)}
  g<-cluster::clusGap(p$X,fun,K.max=min(cfg$k_max,length(p$ids)-1),B=cfg$gap_B,d.power=2,spaceH0="scaledPCA",verbose=FALSE)
  list(table=data.frame(K=seq_len(nrow(g$Tab)),g$Tab),candidate=cluster::maxSE(g$Tab[,"gap"],g$Tab[,"SE.sim"],method="Tibs2001SEmax"))
}
