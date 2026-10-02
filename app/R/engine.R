add_warning <- function(w,message) rbind(w,data.frame(K=NA,Message=message,Occurrences=1L))
run_analysis <- function(data,cfg=list(),progress=function(...)NULL) {
  if(!is.data.frame(data)||!nrow(data))stop("Provide a nonempty data table.")
  if(nrow(data)>2000)stop("This release analyzes at most 2,000 cases: select a subset of cases (for example a random sample) and open that file.",call.=FALSE)
  cfg<-cw_config(cfg);start<-Sys.time();cfg$.deadline<-cfg$.deadline%||%(as.numeric(start)+cfg$timeout);set.seed(cfg$seed)
  result<-list(config=cfg,version=cw_version,tables=list(),solutions=list(),original=data,status="Completed")
  vars<-cfg$variables%||%names(data)[vapply(data,is.numeric,logical(1))]
  if(!length(vars))stop("Select at least one analysis variable.")
  if(any(!vars%in%names(data)))stop(paste("Variables not found in the data:",paste(setdiff(vars,names(data)),collapse=", ")))
  result$tables$Descriptives<-describe_data(data,vars)
  result$tables<-c(result$tables,missingness_tables(data,vars))
  if(cfg$missing=="multiple")return(run_multiple_imputation(data,cfg,progress))
  p<-prepare_data(data,cfg);result$prepared<-p;result$tables$Donors<-p$donors;result$tables$Case_audit<-p$case_audit
  if(isTRUE(p$no_overlap>0))result$tables$Warnings<-data.frame(K=NA,Message=if(p$no_overlap==1) "1 pair of cases shares no observed variable; its Gower distance was set to 1, the largest possible value." else sprintf("%d pairs of cases share no observed variable; their Gower distance was set to 1, the largest possible value.",p$no_overlap),Occurrences=1L)
  check_method(p,cfg)
  if(cfg$simulation && cfg$null=="gaussian" && !p$numeric)stop("The Gaussian null reference requires numeric variables; select Column permutation.")
  if(cfg$simulation && cfg$null=="gaussian" && cfg$distance%in%c("jaccard","matching"))stop("The Gaussian null reference is not defined for binary Jaccard or Simple matching distances; select Column permutation.")
  kmax<-min(as.integer(cfg$k_max),length(p$ids)-1);kmin<-as.integer(cfg$k_min)
  if(!is.finite(kmin)||!is.finite(kmax)||kmin<1||kmin>kmax)stop(sprintf("Select a valid range of k: minimum k must not exceed maximum k (at most N - 1 = %d).",length(p$ids)-1))
  warnings_log<-list()
  result$assignments<-data.frame(ID=p$original_ids,stringsAsFactors=FALSE)
  comparison<-list()
  # Settings of the reported fits: one hierarchical tree for all k; wider search for k-means.
  main_cfg<-if(cfg$method=="kmeans") modifyList(cfg,list(nstart=max(cfg$nstart,300L),.extended_search=TRUE)) else cfg
  if(kmax>1) main_cfg$.tree<-tryCatch(method_tree(p,cfg),error=function(e)NULL)
  pairs<-if(kmax>1&&length(p$ids)>2) pair_cache(p$distance) else NULL
  for(k in seq.int(kmin,kmax)) {
    if(!budget_ok(cfg)) {result$status<-"Time limit reached; partial results";break}
    progress(paste0("Fitting k = ",k),k-kmin+1,kmax-kmin+1)
    value<-tryCatch(withCallingHandlers({
      fit<-fit_partition(p,main_cfg,k);e<-evaluate_partition(p,fit$cluster,TRUE,pairs)
      row<-e$metrics;row$Status<-"Completed";sol<-list(ids=p$ids,cluster=fit$cluster,fit=fit,evaluation=e,k=k)
      if(cfg$method=="gmm")row<-cbind(row,mixture_metrics(fit,length(p$ids)))
      if(cfg$bootstrap && k>1 && budget_ok(cfg)) {
        sol$bootstrap<-run_bootstrap(p,fit,cfg,progress);bs<-sol$bootstrap$summary
        jv<-bs$Mean_Jaccard[is.finite(bs$Mean_Jaccard)]
        row$Bootstrap_mean_Jaccard<-if(length(jv))mean(jv)else NA_real_;row$Bootstrap_min_Jaccard<-if(length(jv))min(jv)else NA_real_
        ov<-sol$bootstrap$overall
        row$Bootstrap_ARI<-ov$Mean[1];row$Bootstrap_ARI_P025<-ov$P025[1];row$Bootstrap_ARI_P975<-ov$P975[1]
        row$Bootstrap_Rand<-ov$Mean[2];row$Bootstrap_Rand_P025<-ov$P025[2];row$Bootstrap_Rand_P975<-ov$P975[2]
        row$Bootstrap_valid_B<-min(bs$Valid_B);row$Bootstrap_completed_B<-min(bs$Completed_B)
      }
      if(cfg$simulation && k>1 && budget_ok(cfg)) {
        sol$simulation<-run_simulation(p,fit,cfg,progress);sm<-sol$simulation$summary
        row$Simulation_p_Silhouette<-sm$Empirical_p[match("Silhouette",sm$Index)];row$Simulation_p_CH<-sm$Empirical_p[match("CH",sm$Index)]
        if(p$euclidean)row$Simulation_p_EESS<-sm$Empirical_p[match("EESS",sm$Index)]
        row$Simulation_valid_B<-sm$Valid_B[match("Silhouette",sm$Index)]
      }
      v<-rep(NA_integer_,length(p$original_ids));v[p$rows]<-fit$cluster;result$assignments[[paste0("K",k)]]<-v
      result$solutions[[as.character(k)]]<-sol
      row
    },warning=function(w){warnings_log[[length(warnings_log)+1]]<<-data.frame(K=k,Message=conditionMessage(w));invokeRestart("muffleWarning")}),
    error=function(e)data.frame(K=k,N=length(p$ids),Status=conditionMessage(e)))
    comparison[[length(comparison)+1]]<-value
    if(!is.null(cfg$.checkpoint)) {
      ns<-unique(unlist(lapply(comparison,names)))
      result$comparison<-do.call(rbind,lapply(comparison,function(z){for(v in setdiff(ns,names(z)))z[[v]]<-NA;z[ns]}))
      result$tables$Comparison<-result$comparison
      tmp<-paste0(cfg$.checkpoint,".tmp");saveRDS(result,tmp);if(file.exists(cfg$.checkpoint))unlink(cfg$.checkpoint);file.rename(tmp,cfg$.checkpoint)
    }
  }
  if(!budget_ok(cfg))result$status<-"Time limit reached; partial results"
  if(length(warnings_log)) {
    w<-do.call(rbind,warnings_log);key<-paste(w$K,w$Message,sep="\r")
    result$tables$Warnings<-rbind(result$tables$Warnings,data.frame(K=w$K[!duplicated(key)],Message=w$Message[!duplicated(key)],Occurrences=as.integer(table(key)[unique(key)]),row.names=NULL))
  }
  allnames<-unique(unlist(lapply(comparison,names)))
  result$comparison<-if(length(comparison))do.call(rbind,lapply(comparison,function(z){for(v in setdiff(allnames,names(z)))z[[v]]<-NA;z[allnames]}))else data.frame()
  result$tables$Comparison<-result$comparison
  if(cfg$gap) {
    result$gap<-if(budget_ok(cfg)) tryCatch(run_gap(p,cfg,progress),error=function(e)list(error=conditionMessage(e))) else list(error="the time limit was reached")
    if(!is.null(result$gap$table)){result$tables$Gap<-result$gap$table;result$tables$Gap_candidate<-data.frame(K=result$gap$candidate,Rule="Tibs2001SEmax")}
    else {result$tables$Warnings<-add_warning(result$tables$Warnings,paste0("Gap statistic not computed: ",result$gap$error));result$gap<-NULL}
  }
  if(!length(result$solutions)&&result$status=="Completed")result$status<-"No solution could be fitted; see the Status row of the fit indices"
  result$elapsed<-as.numeric(difftime(Sys.time(),start,units="secs"));result$session<-capture.output(sessionInfo());result
}
run_multiple_imputation <- function(data,cfg,progress) {
  if(nrow(data)>500)stop("Multiple imputation is limited to 500 cases in this release: choose another option under Missing data, or analyze a subset of cases.",call.=FALSE)
  vars<-cfg$variables%||%names(data)[vapply(data,is.numeric,logical(1))];cfg$variables<-vars;x<-data[vars]
  for(j in seq_along(x)){if(is.logical(x[[j]]))x[[j]]<-as.numeric(x[[j]]) else if(!is.numeric(x[[j]]))x[[j]]<-factor(x[[j]])}
  imputed_total<-rep(0,nrow(data))
  mi_warnings<-character();set.seed(cfg$seed);imp<-withCallingHandlers(mice::mice(x,m=cfg$mi_m,maxit=cfg$mi_iter,seed=cfg$seed,printFlag=FALSE),warning=function(w){mi_warnings<<-c(mi_warnings,conditionMessage(w));invokeRestart("muffleWarning")})
  outputs<-list();consensus<-matrix(0,nrow(data),nrow(data));included<-consensus;counter<-0L;latest<-NULL
  for(m in seq_len(cfg$mi_m)) {
    if(!budget_ok(cfg))break
    filled<-mice::complete(imp,m);imputed_total<-imputed_total+rowSums(is.na(data[vars])&!is.na(filled))
    d<-data;d[vars]<-filled;nextcfg<-cfg;nextcfg$missing<-"complete";nextcfg$gap<-FALSE;nextcfg$.checkpoint<-NULL
    r<-run_analysis(d,nextcfg,progress);outputs[[m]]<-r
    k<-tail(names(r$solutions),1);sol<-if(length(k))r$solutions[[k]] else NULL
    if(!is.null(sol)) {
      ix<-r$prepared$rows;consensus[ix,ix]<-consensus[ix,ix]+outer(sol$cluster,sol$cluster,"==");included[ix,ix]<-included[ix,ix]+1;counter<-counter+1L
    }
    r$tables$MI_consensus<-as.data.frame(ifelse(included>0,consensus/pmax(included,1),NA_real_))
    r$tables$MI_pair_inclusions<-as.data.frame(included)
    r$tables$MI_fit_summary<-do.call(rbind,lapply(seq_along(outputs),function(i)cbind(Imputation=i,outputs[[i]]$comparison)))
    if(!is.null(imp$loggedEvents))r$tables$MI_events<-imp$loggedEvents
    if(length(mi_warnings))r$tables$MI_warnings<-data.frame(Message=unique(mi_warnings))
    r$tables$MI_scope<-data.frame(Scope="Resampling conditional on each completed imputation; displayed solution is the last completed imputation; consensus is for maximum k")
    r$imputation<-list(requested_m=cfg$mi_m,completed_m=m,valid_maximum_k=counter,valid=counter,loggedEvents=imp$loggedEvents)
    r$config<-cfg;r$original<-data
    mt<-missingness_tables(data,vars);r$tables$Missingness<-mt$Missingness;r$tables$Missing_patterns<-mt$Missing_patterns
    r$tables$Descriptives<-describe_data(data,vars)
    r$tables$Case_audit$Original_missing<-rowSums(is.na(data[vars]));r$tables$Case_audit$Imputed_cells<-imputed_total/m
    r$prepared$original<-data;r$prepared$original_missing<-is.na(data[vars]);r$prepared$case_audit<-r$tables$Case_audit
    latest<-r
    if(!is.null(cfg$.checkpoint)) {
      tmp<-paste0(cfg$.checkpoint,".tmp");saveRDS(r,tmp);if(file.exists(cfg$.checkpoint))unlink(cfg$.checkpoint);file.rename(tmp,cfg$.checkpoint)
    }
    progress("Multiple imputation",m,cfg$mi_m)
  }
  if(is.null(latest))stop("No imputed data set was analyzed within the time limit: raise the time limit (Run group) or lower the number of imputed data sets.",call.=FALSE)
  if(isTRUE(cfg$gap))latest$tables$Warnings<-add_warning(latest$tables$Warnings,"Gap statistic not computed: it is not defined across multiply imputed data sets.")
  if(length(outputs)<cfg$mi_m || !budget_ok(cfg))latest$status<-"Time limit reached; partial imputations"
  latest$elapsed<-sum(vapply(outputs,function(o)if(is.null(o$elapsed))0 else o$elapsed,numeric(1)))
  latest
}
