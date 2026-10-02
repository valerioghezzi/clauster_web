`%||%` <- function(x,y) if (is.null(x)) y else x
and_list <- function(x) if(length(x)<2) x else paste(paste(head(x,-1),collapse=", "),"and",tail(x,1))
cw_version <- "0.1.1"
cw_config <- function(cfg=list()) {
  defaults <- list(variables=NULL,id=NULL,levels=NULL,weights=NULL,
    scaling="z",missing="complete",distance="euclidean",method="ward.D2",
    k_min=2L,k_max=6L,seed=2026L,B=200L,bootstrap=FALSE,simulation=FALSE,
    null="permutation",subsample=FALSE,fraction=.8,donor_threshold=.5,max_missing=.5,nstart=20L,max_iter=100L,
    beta=-.25,hybrid_start="ward.D2",hybrid_algorithm="Hartigan-Wong",timeout=1800,advanced_indices=FALSE,mi_m=5L,mi_iter=5L,
    covariance="all",gap=FALSE,gap_B=100L)
  cfg<-cfg[!vapply(cfg,is.null,logical(1))];out<-utils::modifyList(defaults,cfg)
  bounds<-list(B=c(1,10000),gap_B=c(1,1000),nstart=c(1,1000),max_iter=c(1,10000),mi_m=c(2,50),mi_iter=c(1,50),k_min=c(1,30),k_max=c(1,30))
  nice<-c(B="Replications",gap_B="Gap replications",nstart="Random starts",max_iter="Maximum iterations",mi_m="Imputed datasets",mi_iter="Imputation iterations",k_min="From k",k_max="To k")
  for(nm in names(bounds)){v<-out[[nm]];b<-bounds[[nm]];if(length(v)!=1||!is.numeric(v)||!is.finite(v)||v!=floor(v)||v<b[1]||v>b[2])stop(sprintf("%s must be a whole number between %s and %s.",nice[[nm]],format(b[1],big.mark=","),format(b[2],big.mark=",")))}
  if(length(out$timeout)!=1||!is.numeric(out$timeout)||!is.finite(out$timeout)||out$timeout<=0||out$timeout>86400)stop("Time limit must be finite, positive, and at most 86,400 seconds (24 hours).")
  if(!out$scaling%in%c("z","none","range","robust"))stop("Unknown scaling option.")
  if(!out$missing%in%c("complete","mean","neighbor","available","multiple"))stop("Unknown missing-data option.")
  if(!out$null%in%c("permutation","gaussian"))stop("Unknown simulation reference.")
  if(!out$hybrid_start%in%c("ward.D2","ward.D","average","complete","single","mcquitty","centroid","median","flexible","diana"))stop("Unknown hierarchical starting method.")
  if(!out$hybrid_algorithm%in%c("Hartigan-Wong","Lloyd","MacQueen"))stop("Unknown K-means refinement algorithm.")
  if(!is.finite(out$fraction)||out$fraction<=0||out$fraction>1)stop("Sampling fraction must be in (0, 1].")
  num_ok <- function(v,lo,hi) length(v)==1 && is.numeric(v) && is.finite(v) && v>=lo && v<=hi
  if(!num_ok(out$max_missing,0,1))stop("Maximum missing proportion must be between 0 and 1.")
  if(!num_ok(out$donor_threshold,0,Inf))stop("Donor distance threshold must be zero or positive.")
  if(!num_ok(out$beta,-1,.999))stop("Flexible beta must be between -1 and 0.999.")
  if(!num_ok(out$seed,1,.Machine$integer.max)||out$seed!=floor(out$seed))stop("Random seed must be a whole number between 1 and 2,147,483,647.")
  out$seed <- as.integer(out$seed)
  out
}
read_delimited <- function(path,header=TRUE) {
  bytes <- readBin(path,"raw",n=file.size(path))
  if(!length(bytes)) stop("The file is empty.")
  if(any(bytes==as.raw(0))) stop("This file does not look like a text table. Save the data as CSV or open the original Excel, SPSS or Stata file.")
  # Excel on Windows writes CSV files in Windows-1252; UTF-8 files may start with a byte-order mark.
  # The text is converted here, so that reading does not depend on the locale of R.
  if(length(bytes)>=3&&identical(bytes[1:3],as.raw(c(0xef,0xbb,0xbf)))) bytes <- bytes[-(1:3)]
  text <- rawToChar(bytes)
  if(!validUTF8(text)) text <- iconv(text,"CP1252","UTF-8",sub="?")
  Encoding(text) <- "UTF-8"
  all_lines <- strsplit(text,"\r\n|\n|\r",perl=TRUE)[[1]]
  lines <- head(all_lines[nzchar(trimws(all_lines))],50)
  if(!length(lines)) stop("The file is empty.")
  count <- function(ch) vapply(lines,function(l) lengths(regmatches(l,gregexpr(ch,l,fixed=TRUE))),numeric(1))
  seps <- c(";","\t",",","|")
  score <- vapply(seps,function(ch){z<-count(ch);if(min(z)>0&&diff(range(z))==0)max(z)+1000 else median(z)},numeric(1))
  sep <- if(max(score)>0) seps[which.max(score)] else ""
  # DAT files have no header. A comma can be either the field separator or the decimal mark
  # in a whitespace-separated table. Treat it as a decimal mark only when every whitespace
  # token is itself a valid number; this keeps comma-delimited DAT files readable as well.
  if(!header && sep==",") {
    ws <- lapply(lines,function(l)strsplit(trimws(l),"[[:space:]]+")[[1]])
    decimal_ws <- length(ws)>0 && all(vapply(ws,function(tok)
      length(tok)>1 && all(grepl("^[+-]?(?:[0-9]+(?:,[0-9]+)?|,[0-9]+)$",tok,perl=TRUE)),logical(1)))
    if(decimal_ws) sep <- ""
  }
  if(header && nzchar(sep)) {
    h <- trimws(gsub('^"|"$',"",strsplit(lines[1],sep,fixed=TRUE)[[1]]));h <- h[nzchar(h)]
    if(anyDuplicated(h)) stop(paste("Duplicate column names:",paste(unique(h[duplicated(h)]),collapse=", ")))
  }
  x <- tryCatch(utils::read.table(text=all_lines,encoding="UTF-8",header=header,sep=sep,quote="\"",comment.char="",check.names=FALSE,colClasses="character",
    stringsAsFactors=FALSE,na.strings=c("","NA","N/A","NaN","."," "),strip.white=TRUE,fill=TRUE,blank.lines.skip=TRUE),
    error=function(e)stop("The file could not be read as a table: rows have different numbers of fields. Check that it is a data table with one row per case and a header row.",call.=FALSE))
  attr(x,"separator") <- sep
  x
}
read_excel_table <- function(path) {
  sheets <- readxl::excel_sheets(path)
  for(sh in sheets) {
    probe <- suppressMessages(readxl::read_excel(path,sheet=sh,col_names=FALSE,n_max=30,.name_repair="minimal"))
    if(!nrow(probe)||ncol(probe)<1) next
    filled <- rowSums(!is.na(probe))
    # Skip title rows above the header: the header is the first row filled like the widest row.
    start <- which(filled>=max(1,ceiling(max(filled)*.6)))[1]
    x <- suppressMessages(readxl::read_excel(path,sheet=sh,skip=start-1,.name_repair="minimal"))
    if(nrow(x)&&ncol(x)) {attr(x,"sheet") <- sh;return(x)}
  }
  stop("No sheet of this Excel file contains a data table.")
}
# Numbers written as text: "1,5" and "1.234,5" (Italian, German, French) or "1.5" and "1,234.5" (US, UK).
number_pattern <- function(dec) {
  d <- if(dec==",") "," else "[.]";t <- if(dec==",") "[. \\x{00a0}']" else "[, \\x{00a0}']"
  paste0("^[-+]?(?:[0-9]{1,3}(?:",t,"[0-9]{3})+|[0-9]*)(?:",d,"[0-9]*)?(?:[eE][-+]?[0-9]+)?$")
}
parse_number <- function(w,dec) {
  ok <- grepl(number_pattern(dec),w,perl=TRUE) & grepl("[0-9]",w)
  out <- rep(NA_real_,length(w))
  z <- gsub(if(dec==",") "[. \\x{00a0}']" else "[, \\x{00a0}']","",w[ok],perl=TRUE)
  if(dec==",") z <- sub(",",".",z,fixed=TRUE)
  out[ok] <- suppressWarnings(as.numeric(z));out
}
# Chooses the decimal mark from the values that can be read in one way only:
# 0,5 or 1.234,5 point to a decimal comma; 0.5 or 1,234.5 to a decimal point.
# Without such values, semicolon-separated files follow the European convention.
decimal_mark <- function(w,sep="") {
  w <- trimws(w[!is.na(w)])
  comma <- sum(grepl("^[-+]?(?:[0-9]*,[0-9]+|[0-9]{1,3}(?:[.][0-9]{3})+,[0-9]*)$",w,perl=TRUE) &
    !grepl("^[-+]?[1-9][0-9]{0,2}(?:,[0-9]{3})+$",w,perl=TRUE))
  dot <- sum(grepl("^[-+]?(?:[0-9]*[.][0-9]+(?:[eE][-+]?[0-9]+)?|[0-9]{1,3}(?:,[0-9]{3})+[.][0-9]*)$",w,perl=TRUE) &
    !grepl("^[-+]?[1-9][0-9]{0,2}(?:[.][0-9]{3})+$",w,perl=TRUE))
  if(comma>dot) "," else if(dot>comma) "." else if(identical(sep,";")) "," else "."
}
clean_imported <- function(x) {
  sep <- attr(x,"separator") %||% ""
  x <- as.data.frame(x,check.names=FALSE,stringsAsFactors=FALSE)
  names(x) <- trimws(as.character(names(x)))
  empty <- !nzchar(names(x)) | is.na(names(x))
  names(x)[empty] <- paste0("V",which(empty))
  text_cols <- vapply(x,function(v)is.character(v)&&!inherits(v,"haven_labelled"),logical(1))
  dec <- decimal_mark(unlist(x[text_cols],use.names=FALSE),sep)
  for(j in seq_along(x)) {
    v <- x[[j]]
    if(inherits(v,"haven_labelled")) {
      v <- if(is.character(unclass(v))) as.character(haven::zap_labels(v)) else as.numeric(haven::zap_labels(v))
    }
    if(is.character(v)) {
      v <- trimws(v);v[v %in% c("","NA","N/A","NaN",".")] <- NA
      w <- v[!is.na(v)]
      # Codes such as 001 keep their leading zeros: such columns stay text (IDs, postcodes).
      if(length(w) && !any(grepl("^[-+]?0[0-9]",w))) {
        num <- parse_number(w,dec)
        # A column written with the other convention is still read, if it can be read in full.
        if(anyNA(num)) {alt <- parse_number(w,if(dec==",") "." else ",");if(!anyNA(alt)&&decimal_mark(w)!=dec)num <- alt}
        if(!anyNA(num)) {out <- rep(NA_real_,length(v));out[!is.na(v)] <- num;v <- out}
      }
    }
    if(inherits(v,c("POSIXt","Date","difftime"))) v <- as.character(v)
    attr(v,"label") <- NULL;attr(v,"format.spss") <- NULL;attr(v,"display_width") <- NULL
    x[[j]] <- v
  }
  rownames(x) <- NULL
  keep <- vapply(x,function(v) !all(is.na(v)),logical(1))
  x <- x[keep]
  x[rowSums(!is.na(x))>0,,drop=FALSE]
}
# Portable files are read with foreign, which also accepts those exported by GNU PSPP; haven is the fallback.
read_portable <- function(path) tryCatch(suppressWarnings(foreign::read.spss(path,to.data.frame=TRUE,use.value.labels=FALSE,reencode=FALSE)),error=function(e)
  tryCatch(haven::read_por(path),error=function(e2)stop("The SPSS portable file could not be read. Open it in SPSS or PSPP and save it as SAV.",call.=FALSE)))
import_data <- function(path, original_name=path) {
  ext <- tolower(tools::file_ext(original_name))
  # Errors of the external readers mention internal temporary paths and file-format details.
  external <- function(expr) tryCatch(expr,error=function(e)if(grepl("^No sheet",conditionMessage(e))) stop(conditionMessage(e),call.=FALSE) else stop(sprintf("The file %s could not be read: it is damaged, incomplete or not a valid %s file. Open it in the program that created it and save it again.",
    basename(original_name),toupper(ext)),call.=FALSE))
  x <- switch(ext,csv=,txt=,tsv=read_delimited(path),
    dat=read_delimited(path,header=FALSE),
    xlsx=,xls=external(read_excel_table(path)),
    sav=,zsav=external(haven::read_sav(path)),por=read_portable(path),dta=external(haven::read_dta(path)),sas7bdat=external(haven::read_sas(path)),
    rds=external(readRDS(path)),stop("Supported files: CSV, TXT, TSV, DAT, XLSX, XLS, SAV, POR, DTA, SAS7BDAT, RDS."))
  if (!is.data.frame(x) && !is.matrix(x)) stop("The file must contain a rectangular data table.")
  x <- clean_imported(x)
  if (!nrow(x) || !ncol(x)) stop("The table is empty: no rows with data were found.")
  if (anyDuplicated(names(x))) stop(paste("Duplicate column names:",paste(unique(names(x)[duplicated(names(x))]),collapse=", ")))
  x
}
scale_model <- function(x, method) {
  x <- as.matrix(x)
  spread <- apply(x,2,function(v) diff(range(v,na.rm=TRUE)))
  if(any(!is.finite(spread)|spread==0)) {v <- colnames(x)[!is.finite(spread)|spread==0]
    stop(if(length(v)==1) paste(v,"has the same value for every case; remove this variable from the analysis variables.") else paste(and_list(v),"have the same value for every case; remove these variables from the analysis variables."),call.=FALSE)}
  if(method=="z") { center <- colMeans(x,na.rm=TRUE); scale <- apply(x,2,sd,na.rm=TRUE) }
  else if(method=="range") { center <- apply(x,2,min,na.rm=TRUE); scale <- spread }
  else if(method=="robust") { center <- apply(x,2,median,na.rm=TRUE); scale <- apply(x,2,mad,na.rm=TRUE) }
  else { center <- rep(0,ncol(x)); scale <- rep(1,ncol(x)) }
  if(any(!is.finite(scale)|scale<=0)) stop(paste0("Median and MAD scaling cannot be used: ",and_list(colnames(x)[!is.finite(scale)|scale<=0])," ",if(sum(!is.finite(scale)|scale<=0)==1)"has" else "have"," a median absolute deviation of zero (most cases share one value). Select Z scores or Range scaling."),call.=FALSE)
  list(center=center,scale=scale,method=method)
}
apply_scale <- function(x,s) sweep(sweep(as.matrix(x),2,s$center,"-"),2,s$scale,"/")
metric_distance <- function(x,metric="euclidean",weights=NULL) {
  if(metric %in% c("jaccard","matching") && any(!as.matrix(x)%in%c(0,1)))stop("Jaccard and simple matching need variables coded 0/1: select only binary variables and Scaling = None.",call.=FALSE)
  if(metric=="gower") {
    binary <- which(vapply(x,function(v)is.numeric(v)&&all(v[!is.na(v)]%in%c(0,1))&&length(unique(v[!is.na(v)]))==2,logical(1)))
    return(cluster::daisy(x,metric="gower",weights=weights %||% rep(1,ncol(x)),type=if(length(binary))list(symm=binary) else list()))
  }
  x <- as.matrix(x)
  if(any(!is.finite(x))) stop("This distance needs complete numeric values: choose an imputation option under Missing data, or Gower distance with Available coordinates.",call.=FALSE)
  if(metric %in% c("correlation","cosine")) {
    y <- x
    if(metric=="correlation") y <- y-rowMeans(y)
    norms <- sqrt(rowSums(y^2))
    if(any(norms<1e-12)) stop("Correlation and cosine distances are undefined for a case whose values are all equal (or all zero). Choose another distance, or remove such cases.",call.=FALSE)
    y <- y/norms
    m <- 1-tcrossprod(y);m[] <- pmax(0,pmin(2,m));diag(m) <- 0
    return(as.dist(m))
  }
  if(metric %in% c("jaccard","matching")) {
    if(any(!x %in% c(0,1))) stop("Jaccard and simple matching need variables coded 0/1: select only binary variables and Scaling = None.",call.=FALSE)
    w <- weights %||% rep(1,ncol(x))
    if(length(w)!=ncol(x)||any(!is.finite(w))||any(w<0)||sum(w)<=0)stop("Binary-distance weights must be nonnegative, finite, and include at least one positive value.")
    xw <- sweep(x,2,sqrt(w),"*");a <- tcrossprod(xw);count <- rowSums(sweep(x,2,w,"*"));union <- outer(count,count,"+")-a
    if(metric=="jaccard") { out <- 1-a/pmax(union,.Machine$double.eps);out[union<=.Machine$double.eps] <- 0 }
    else out <- (outer(count,count,"+")-2*a)/sum(w)
    return(as.dist(out))
  }
  if(metric=="sqeuclidean") return(as.dist(as.matrix(dist(x))^2))
  if(metric=="mahalanobis") {
    if(nrow(x)<=ncol(x)+1) stop("Mahalanobis distance needs more cases than variables plus one: select fewer variables or another distance.",call.=FALSE)
    cv <- cov(x); e <- eigen(cv,symmetric=TRUE)
    if(min(e$values)<1e-10) stop("Mahalanobis distance cannot be computed: some variables are exact (or almost exact) combinations of others. Remove redundant variables or choose another distance.",call.=FALSE)
    return(dist(x %*% e$vectors %*% diag(1/sqrt(e$values),ncol(x))))
  }
  dist(x,method=metric,p=2)
}
as_binary <- function(v,name) {
  if(is.factor(v)) v <- as.character(v)
  if(is.character(v)) {
    u <- sort(unique(trimws(v[!is.na(v)])))
    if(all(u %in% c("0","1"))) return(as.numeric(v))
    if(length(u)==2) return(as.numeric(trimws(v)==u[2]))
    stop(sprintf("Binary variable %s has more than two distinct values: set it to Continuous or Ordinal.",name),call.=FALSE)
  }
  v <- as.numeric(v);u <- unique(v[!is.na(v)])
  if(length(u)>2) stop(sprintf("Binary variable %s has more than two distinct values: set it to Continuous or Ordinal.",name),call.=FALSE)
  v
}
as_ordinal <- function(v,name) {
  if(is.ordered(v)) return(v)
  if(is.factor(v)) return(factor(v,levels=levels(v),ordered=TRUE))
  num <- suppressWarnings(as.numeric(as.character(v)))
  if(any(is.na(num)&!is.na(v))) stop(sprintf("Ordinal variable %s contains text, so its order cannot be inferred. Recode its categories as numbers (1, 2, 3, ...).",name))
  factor(num,levels=sort(unique(num[!is.na(num)])),ordered=TRUE)
}
prepare_data <- function(data,cfg=list()) {
  cfg <- cw_config(cfg)
  vars <- cfg$variables %||% names(data)[vapply(data,is.numeric,logical(1))]
  if(!length(vars) || any(!vars %in% names(data))) stop("Select at least one existing analysis variable.")
  if(!is.null(cfg$id) && cfg$id %in% vars) stop("The Case ID column is also selected as an analysis variable: remove it from the analysis variables or choose another Case ID.",call.=FALSE)
  ids <- if(is.null(cfg$id)) as.character(seq_len(nrow(data))) else as.character(data[[cfg$id]])
  if(length(ids)!=nrow(data)||anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids)) stop("The Case ID column has empty or repeated values: choose another column or Row number as Case ID.",call.=FALSE)
  auto_level <- function(v) if(is.logical(v)) "binary" else if(is.ordered(v)) "ordinal" else if(is.numeric(v)) "continuous" else if(length(unique(v[!is.na(v)]))<=2) "binary"
    else if(!anyNA(suppressWarnings(as.numeric(as.character(v[!is.na(v)]))))) "continuous" else "text"
  levels <- cfg$levels %||% vapply(data[vars],auto_level,"")
  if(!is.null(names(levels))) levels <- unname(levels[vars])
  if(length(levels)!=length(vars)||anyNA(levels)) stop("Specify the measurement level for every variable.")
  # CLAuster analyzes continuous, ordinal and binary variables; unordered categories are not supported.
  bad <- vars[!levels %in% c("continuous","ordinal","binary")]
  if(length(bad)) stop(sprintf("%s: text with more than two categories cannot be analyzed. Recode ordered categories as numbers (1, 2, 3, ...) or leave the variable out.",paste(bad,collapse=", ")),call.=FALSE)
  raw <- data[vars]
  empty <- vars[vapply(raw,function(v)all(is.na(v)),logical(1))]
  if(length(empty)) stop(if(length(empty)==1) paste(empty,"has no observed values; remove it from the analysis variables.") else paste(and_list(empty),"have no observed values; remove them from the analysis variables."),call.=FALSE)
  for(j in seq_along(vars)) if(is.logical(raw[[j]])) raw[[j]] <- as.numeric(raw[[j]])
  for(j in seq_along(vars)) {
    v <- raw[[j]]
    if(levels[j]=="binary") raw[[j]] <- as_binary(v,vars[j])
    else if(levels[j]=="ordinal") {
      o <- as_ordinal(v,vars[j])
      # Numeric codes are kept as they are (1, 2, 5, 10 stays 1, 2, 5, 10); text categories are coded by their rank.
      codes <- suppressWarnings(as.numeric(levels(o)))
      raw[[j]] <- if(cfg$distance=="gower") o else if(!anyNA(codes)) codes[as.integer(o)] else as.numeric(o)
    } else {
      if(is.factor(v)||is.character(v)) {num <- suppressWarnings(as.numeric(as.character(v)))
        if(any(is.na(num)&!is.na(v))) stop(sprintf("Variable %s contains text; set it to Binary if it has two values, or recode its categories as numbers.",vars[j]))
        v <- num}
      raw[[j]] <- as.numeric(v)
    }
    if(is.numeric(raw[[j]])) raw[[j]][!is.finite(raw[[j]])] <- NA_real_
  }
  w <- cfg$weights %||% rep(1,length(vars))
  if(length(w)!=length(vars)||any(!is.finite(w))||any(w<0)||sum(w)<=0) stop(sprintf("Variable weights must be %d nonnegative numbers (one per analysis variable), at least one positive.",length(vars)))
  if(cfg$distance=="mahalanobis" && length(unique(w))>1) stop("Variable weights cannot be combined with Mahalanobis distance, which already standardizes the covariance structure.")
  numeric <- all(vapply(raw,is.numeric,logical(1)))
  original_missing <- is.na(raw)
  donors <- data.frame()
  if(cfg$missing %in% c("mean","neighbor")) {
    if(!numeric) stop("Mean and close-neighbor imputation need numeric coding: with ordinal variables under Gower distance choose Available coordinates or Multiple imputation.",call.=FALSE)
    s0 <- scale_model(raw,cfg$scaling); z <- apply_scale(raw,s0)
    complete <- which(complete.cases(raw));means <- colMeans(raw,na.rm=TRUE)
    for(i in which(!complete.cases(raw))) {
      obs <- !is.na(z[i,]); miss <- which(!obs)
      if(mean(!obs)>cfg$max_missing || !any(obs)) next
      if(cfg$missing=="mean") raw[i,miss] <- as.list(means[miss])
      else if(length(complete)) {
        ds <- rowSums(sweep(sweep(z[complete,obs,drop=FALSE],2,z[i,obs],"-"),2,sqrt(w[obs]),"*")^2)/sum(w[obs])
        m <- which.min(ds)
        if(length(m) && ds[m]<=cfg$donor_threshold) {
          raw[i,miss] <- raw[complete[m],miss]
          donors <- rbind(donors,data.frame(ID=ids[i],Donor_ID=ids[complete[m]],ASED=ds[m]))
        }
      }
    }
  }
  keep <- if(cfg$missing=="available" && cfg$distance=="gower") rowSums(!is.na(raw))>0 else complete.cases(raw)
  if(cfg$missing=="available" && cfg$distance!="gower") stop("Available coordinates need Gower distance: select Distance = Gower, or choose another missing-data option.",call.=FALSE)
  if(sum(keep)<3) stop(sprintf("Only %d %s usable after the missing-data rule; at least 3 are needed. Choose an imputation option under Missing data, or select variables with fewer missing values.",sum(keep),if(sum(keep)==1)"case is" else "cases are"),call.=FALSE)
  if(sum(keep)>2000) stop("This release analyzes at most 2,000 cases: select a subset of cases (for example a random sample) and open that file.",call.=FALSE)
  raw_kept <- raw[keep,,drop=FALSE]
  transform <- if(numeric) scale_model(raw_kept,cfg$scaling) else NULL
  z <- if(numeric) apply_scale(raw_kept,transform) else raw_kept
  x <- if(numeric) sweep(z,2,sqrt(w),"*") else raw_kept
  # Gower and binary distances apply variable weights inside the distance definition.
  dx <- if(cfg$distance=="gower") metric_distance(raw_kept,"gower",w) else if(cfg$distance%in%c("jaccard","matching")) metric_distance(z,cfg$distance,w) else metric_distance(x,cfg$distance)
  # Pairs of cases with no variable observed in both get the largest distance, and the count is reported.
  no_overlap <- 0L
  if(any(!is.finite(dx))) {
    if(cfg$distance!="gower") stop("Some pairs of cases have no variable observed in both: select Gower distance with Available coordinates, or an imputation option.",call.=FALSE)
    bad <- !is.finite(dx);no_overlap <- sum(bad);dx[bad] <- max(1,max(dx[!bad]))
  }
  rows <- which(keep); retained <- ids[keep]
  audit<-data.frame(ID=ids,Original_missing=rowSums(original_missing),Imputed_cells=rowSums(original_missing & !is.na(raw)),Status=ifelse(seq_along(ids)%in%rows,"Retained","Excluded missing data"),stringsAsFactors=FALSE)
  list(X=x,distance=dx,raw=raw_kept,ids=retained,rows=rows,original_ids=ids,case_audit=audit,
    original=data,original_missing=original_missing,numeric=numeric,
    euclidean=numeric && cfg$distance %in% c("euclidean","sqeuclidean"),
    transform=transform,variables=vars,levels=levels,weights=w,config=cfg,donors=donors,no_overlap=no_overlap)
}
describe_data <- function(data,vars) {
  total <- sum(complete.cases(data[vars]))
  do.call(rbind,lapply(vars,function(v) {
    x <- data[[v]];num <- is.numeric(x)
    data.frame(Variable=v,N=sum(!is.na(x)),Missing=sum(is.na(x)),
      Mean=if(num) mean(x,na.rm=TRUE) else NA,SD=if(num) sd(x,na.rm=TRUE) else NA,
      Minimum=if(num && any(!is.na(x))) min(x,na.rm=TRUE) else NA,Maximum=if(num && any(!is.na(x))) max(x,na.rm=TRUE) else NA,
      Cases_recovered=if(length(vars)>1) sum(complete.cases(data[setdiff(vars,v)]))-total else nrow(data)-total)
  }))
}

missingness_tables <- function(data,vars) {
  mask<-is.na(data[vars]);n<-nrow(data)
  labels<-apply(mask,1,function(v)if(!any(v))"Complete"else paste(vars[v],collapse=" | "))
  counts<-sort(table(labels),decreasing=TRUE)
  list(Missingness=data.frame(Variable=vars,N=n,Missing=colSums(mask),Missing_percent=100*colMeans(mask),row.names=NULL),
    Missing_patterns=data.frame(Missing_variables=names(counts),N=as.integer(counts),Percent=100*as.integer(counts)/n,row.names=NULL))
}
