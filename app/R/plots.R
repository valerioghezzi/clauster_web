# Graphics: one quiet visual system for every plot (light surface, recessive grid,
# 2px lines, ringed markers, legend always present, fixed categorical order).
cw_theme <- list(surface="#ffffff",ink="#1d2125",ink2="#52514e",muted="#898781",grid="#e8e7e2",axis="#c3c2b7",
  accent="#174b38",neutral="#f0efec")
cw_palette <- c("#2a78d6","#eb6834","#1baf7a","#eda100","#e87ba4","#008300","#4a3aa7","#e34948")
cw_shapes <- c(21,22,24,23,25)
cluster_style <- function(k,bw=FALSE) {
  g <- seq_len(k)
  if(bw) return(list(col=grDevices::gray(seq(.1,.62,length.out=max(k,2)))[g],pch=cw_shapes[(g-1)%%5+1],lty=c(1,2,3,4,5,6)[(g-1)%%6+1]))
  # Beyond eight clusters colors repeat only together with a different marker shape.
  list(col=cw_palette[(g-1)%%8+1],pch=cw_shapes[((g-1)%/%8)%%5+1],lty=rep(1,k))
}
cw_diverging <- function(n=101) grDevices::colorRampPalette(c("#104281","#3987e5","#b7d3f6",cw_theme$neutral,"#f4b8b0","#e34948","#8f1f1e"))(n)
cw_begin <- function(mar=c(4.2,4.6,3.6,1.2)) {
  keep_new <- isTRUE(par("new"))
  op <- par(bg=cw_theme$surface,fg=cw_theme$axis,col.axis=cw_theme$ink2,col.lab=cw_theme$ink2,col.main=cw_theme$ink,
    family="sans",mar=mar,mgp=c(2.6,.55,0),tcl=0,las=1,cex.axis=.82,cex.lab=.88,xpd=FALSE)
  # Setting the background resets par(new); keep it so a plot can be placed inside a report page.
  if(keep_new) par(new=TRUE)
  op[setdiff(names(op),"bg")]
}
# R's PDF device prints "-" as a minus sign; between letters use the hyphen glyph instead.
hy <- function(x) {
  if(!is.character(x)||!grepl("^pdf",names(grDevices::dev.cur()))) return(x)
  x <- tryCatch(suppressWarnings(gsub("(?<=[A-Za-z0-9)])-(?=[A-Za-z])","\u00ad",x,perl=TRUE)),error=function(e)x)
  # Without Cairo the PDF device knows only Windows-1252 glyphs: transliterate the rest.
  y <- suppressWarnings(iconv(x,"UTF-8","CP1252//TRANSLIT"));ifelse(is.na(y),iconv(x,"UTF-8","ASCII",sub="?"),enc2utf8(iconv(y,"CP1252","UTF-8")))
}
ellipsis <- function() "..."
short_lab <- function(x,n=26) {
  cut <- nchar(x)>n;out <- ifelse(cut,paste0(substr(x,1,n-1),ellipsis()),x)
  # When cutting the end makes labels identical, keep the end instead: it usually tells them apart.
  if(any(cut)&&anyDuplicated(out)) {a <- ceiling((n-3)/2);b <- n-3-a
    out[cut] <- paste0(sub("[ ,;]+$","",substr(x[cut],1,a)),ellipsis(),substr(x[cut],nchar(x[cut])-b+1,nchar(x[cut])))}
  out
}
# Shortens a label (middle ellipsis) until it fits the given width in inches.
fit_label <- function(x,inches,cex) {
  if(strwidth(x,units="inches",cex=cex)<=inches) return(x)
  for(n in seq(nchar(x)-1,4)) {y <- short_lab(c(x,paste0(x,"#")),n)[1];if(strwidth(y,units="inches",cex=cex)<=inches) return(y)}
  substr(x,1,4)
}
narrow_plot <- function() par("fin")[1]<4.6
# Shortens a line so that it fits the figure width.
fit_line <- function(x,cex=.78) {
  if(is.null(x)) return(x)
  avail <- par("fin")[1]-par("mai")[2]-.15
  if(strwidth(x,units="inches",cex=cex)<=avail) return(x)
  while(nchar(x)>8&&strwidth(paste0(x,ellipsis()),units="inches",cex=cex)>avail) x <- substr(x,1,nchar(x)-2)
  paste0(sub("[ ,;]+$","",x),ellipsis())
}
cw_title <- function(title,subtitle=NULL) {
  title <- hy(title);subtitle <- hy(fit_line(subtitle))
  mtext(title,side=3,line=if(is.null(subtitle))1.1 else 1.9,adj=0,font=2,cex=1,col=cw_theme$ink)
  if(!is.null(subtitle))mtext(subtitle,side=3,line=.7,adj=0,cex=.78,col=cw_theme$ink2)
}
cw_grid <- function(y=NULL,x=NULL) {
  if(!is.null(y))abline(h=y,col=cw_theme$grid,lwd=1)
  if(!is.null(x))abline(v=x,col=cw_theme$grid,lwd=1)
}
cw_empty <- function(message) {
  op <- cw_begin(c(1,1,1,1));on.exit(par(op))
  plot.new();text(.5,.5,message,col=cw_theme$ink2,cex=.95)
  invisible(NULL)
}
# Legend columns that fit the figure width; returns columns and rows.
legend_layout <- function(labels,cex=.8) {
  w <- max(strwidth(labels,units="inches",cex=cex))+.55
  avail <- max(1,par("fin")[1]-1.1)
  nc <- max(1,min(length(labels),floor(avail/w)))
  list(ncol=nc,rows=ceiling(length(labels)/nc))
}
legend_rows <- function(labels) if(is.numeric(labels)) 1 else legend_layout(labels)$rows
cw_legend <- function(labels,st,lines=FALSE,where="top") {
  par(xpd=NA)
  usr <- par("usr");y <- usr[4]+diff(usr[3:4])*.02
  labels <- hy(labels);lay <- legend_layout(labels)
  legend(x=mean(usr[1:2]),y=y,xjust=.5,yjust=0,legend=labels,horiz=lay$rows==1&&length(labels)>1,ncol=if(lay$rows==1)1 else lay$ncol,
    pch=st$pch,pt.bg=st$col,col=if(lines)st$col else cw_theme$surface,lty=if(lines)st$lty else NA,lwd=if(lines)2 else 1,pt.lwd=1.2,pt.cex=1.25,
    bty="n",cex=.8,text.col=cw_theme$ink,x.intersp=.7,seg.len=if(lines)1.8 else 1,inset=0,
    text.width=max(strwidth(labels,cex=.8))*1.08)
  par(xpd=FALSE)
}
plot_solution <- function(result,k,type="Profiles",labels=NULL,bw=FALSE) {
  sol <- result$solutions[[as.character(k)]];p <- result$prepared
  if(is.null(sol)) return(cw_empty("No fitted solution for this k."))
  cl <- sol$cluster;K <- sol$k;st <- cluster_style(K,bw)
  sizes <- tabulate(cl,K)
  label <- labels %||% paste("Cluster",seq_len(K))
  if(length(label)!=K||any(!nzchar(label))) label <- paste("Cluster",seq_len(K))
  label <- short_lab(label,30);keyed <- paste0(label," (n = ",sizes,")")
  switch(type,
    Dendrogram=plot_dendrogram(sol,st,K),
    Silhouette=plot_silhouette(sol,st,label,K),
    Indices=plot_indices(result,k),
    Agglomeration=plot_agglomeration(result,k),
    Bootstrap=plot_bootstrap(sol,st,label,K),
    Heatmap=plot_heatmap(p,cl,st,label,K),
    Projection=plot_projection(p,cl,st,keyed,K),
    plot_profiles(p,cl,st,keyed,K))
  invisible(NULL)
}
plot_profiles <- function(p,cl,st,keyed,K) {
  vals <- lapply(p$raw,profile_values)
  numvars <- names(vals)[vapply(vals,function(x)isTRUE(stats::sd(x,na.rm=TRUE)>0),logical(1))]
  if(!length(numvars)) return(cw_empty("No numeric variables to plot. See the Profiles table."))
  z <- vapply(numvars,function(v){x<-vals[[v]];(x-mean(x,na.rm=TRUE))/stats::sd(x,na.rm=TRUE)},numeric(nrow(p$raw)))
  if(!is.matrix(z)) z <- matrix(z,ncol=length(numvars),dimnames=list(NULL,numvars))
  centers <- do.call(rbind,lapply(seq_len(K),function(g)colMeans(z[cl==g,,drop=FALSE],na.rm=TRUE)))
  m <- length(numvars);long <- max(nchar(numvars))*m>70
  lr <- legend_rows(keyed);lab_x <- short_lab(numvars,24);axcex <- if(narrow_plot()) .7 else .82;long <- max(strwidth(lab_x,units="inches",cex=axcex))>(par("fin")[1]-1.0)/m*.95
  op <- cw_begin(c(if(long)min(9,2.8+max(nchar(lab_x))*.4) else 3.4,4.6,4.3+1.1*lr,1.2));on.exit(par(op))
  lim <- range(c(centers,0),finite=TRUE);lim <- lim+c(-1,1)*max(.15,diff(lim)*.08)
  plot.new();plot.window(xlim=c(.7,m+.3),ylim=lim)
  yt <- pretty(lim,6);cw_grid(y=yt);abline(h=0,col=cw_theme$axis,lwd=1.4)
  axis(2,at=yt,lwd=0);axis(1,at=seq_len(m),labels=hy(lab_x),lwd=0,las=if(long)2 else 1,cex.axis=axcex)
  title(ylab="Mean z score")
  for(g in seq_len(K)){
    lines(seq_len(m),centers[g,],col=st$col[g],lwd=2.2,lty=st$lty[g],lend=1,ljoin=1)
    points(seq_len(m),centers[g,],pch=st$pch[g],bg=st$col[g],col=cw_theme$surface,cex=1.45,lwd=1.6)
  }
  cw_legend(keyed,st,lines=TRUE)
  mtext("Cluster profiles",side=3,line=2.8+1.1*lr,adj=0,font=2,cex=1,col=cw_theme$ink)
  mtext(fit_line("Cluster means of each variable, in standard deviations from the overall mean"),side=3,line=1.7+1.1*lr,adj=0,cex=.78,col=cw_theme$ink2)
}
plot_dendrogram <- function(sol,st,K) {
  tree <- sol$fit$tree
  if(is.null(tree)) return(cw_empty("This method does not build a hierarchy."))
  op <- cw_begin(c(1.2,4.6,3.6,1.2));on.exit(par(op))
  h <- sort(tree$height,decreasing=TRUE);cut <- if(K>1&&K<=length(h)) mean(h[c(K-1,K)]) else NA
  plot(stats::as.dendrogram(tree),leaflab="none",edgePar=list(col=cw_theme$ink2,lwd=.8),yaxt="n",ann=FALSE,frame.plot=FALSE)
  yt <- pretty(c(0,max(tree$height)),5);axis(2,at=yt,lwd=0)
  title(ylab="Merge height")
  if(is.finite(cut)){
    abline(h=cut,col=cw_theme$accent,lwd=1.5)
    text(par("usr")[2],cut,paste("cut for k =",K),adj=c(1,-.5),col=cw_theme$accent,cex=.78,xpd=NA)
    ord <- stats::cutree(tree,K)[tree$order];runs <- rle(ord);ends <- cumsum(runs$lengths);starts <- ends-runs$lengths+1
    yb <- par("usr")[3]
    for(i in seq_along(runs$values)) rect(starts[i]-.4,yb,ends[i]+.4,yb+diff(par("usr")[3:4])*.025,col=st$col[runs$values[i]],border=NA)
  }
  cw_title("Dendrogram",paste(length(tree$order),"cases; colored band shows the",K,"clusters in leaf order"))
}
plot_silhouette <- function(sol,st,label,K) {
  s <- sol$evaluation$silhouette
  if(is.null(s)) return(cw_empty("Silhouette is defined only for 2 to N - 1 clusters."))
  s <- unclass(s);w <- as.numeric(s[,3]);g <- as.integer(s[,1])
  o <- order(g,-w);w <- w[o];g <- g[o];n <- length(w)
  gap <- max(1,round(n*.02));pos <- seq_len(n)+(g-1)*gap
  nar <- narrow_plot()
  op <- cw_begin(c(4.2,if(nar)5 else 9.5,4.6,1.2));on.exit(par(op))
  xl <- c(min(0,min(w)),1)
  plot.new();plot.window(xlim=xl,ylim=c(max(pos)+1,0))
  xt <- pretty(xl,6);cw_grid(x=xt);axis(1,at=xt,lwd=0)
  rect(0,pos-.5,w,pos+.5,col=st$col[g],border=NA)
  abline(v=0,col=cw_theme$axis,lwd=1.2)
  avg <- mean(w);abline(v=avg,col=cw_theme$ink,lwd=1.2)
  mtext(sprintf("overall mean %.2f",avg),side=3,at=avg,line=-.1,cex=.75,col=cw_theme$ink)
  # Every cluster gets a label: two lines when its band allows, one line otherwise, the name alone when
  # space is tight; a label is skipped only when it would overlap the previous one.
  line_h <- abs(strheight("M",units="user",cex=.72))*1.7;pad <- line_h*.25;avail <- par("mai")[2]-.12;bottom <- -Inf
  mids <- vapply(seq_len(K),function(h)mean(range(pos[g==h])),0)
  for(h in seq_len(K)){mid <- mids[h];mw <- mean(w[g==h]);band <- diff(range(pos[g==h]))+1+gap;nh <- sum(g==h)
    room_next <- if(h<K) mids[h+1]-mid-line_h/2-pad else Inf
    fits <- function(lines) mid-lines*line_h/2>=bottom+pad&&lines*line_h/2<=room_next
    two <- band>=line_h*2&&fits(2)
    if(!two&&!fits(1)) next
    name <- fit_label(label[h],avail,.72)
    txt <- if(nar) (if(two) sprintf("%s\n%.2f",name,mw) else name) else if(two) sprintf("%s\nn = %d, mean %.2f",name,nh,mw) else {
      full <- sprintf("%s: n = %d, mean %.2f",label[h],nh,mw);if(strwidth(full,units="inches",cex=.72)<=avail) full else name}
    mtext(txt,side=2,at=mid,line=.5,las=1,cex=.72,col=cw_theme$ink,adj=1);bottom <- mid+(if(two)2 else 1)*line_h/2}
  title(xlab="Silhouette width")
  mtext("Silhouette",side=3,line=3.2,adj=0,font=2,cex=1,col=cw_theme$ink);mtext(fit_line("One bar per case, sorted within cluster; negative values suggest a closer neighboring cluster"),side=3,line=2.1,adj=0,cex=.78,col=cw_theme$ink2)
}
plot_indices <- function(result,k) {
  tab <- result$comparison
  fields <- intersect(c("Silhouette","CH","DB","EESS","Dunn","BIC"),names(tab))
  fields <- fields[vapply(fields,function(v)sum(is.finite(suppressWarnings(as.numeric(tab[[v]]))))>1,logical(1))]
  if(!length(fields)) return(cw_empty("At least two values of k are needed to compare indices."))
  nice <- c(Silhouette="Mean silhouette (higher)",CH="Calinski-Harabasz (higher)",DB="Davies-Bouldin (lower)",
    EESS="EESS % (higher)",Dunn="Dunn (higher)",BIC="BIC (lower)")
  maxc <- if(par("fin")[1]<4.6) 1 else 3
  nr <- ceiling(length(fields)/maxc);nc <- min(maxc,length(fields))
  op <- cw_begin(c(3.2,3.6,3.4,1.2));on.exit(par(op))
  # Panels are placed inside the current figure region, so the plot also fits a report page slot.
  f0 <- par("fig");fw <- diff(f0[1:2]);fh <- diff(f0[3:4])*.9
  par(mar=c(0,0,0,0));plot.new()
  mtext("Indices across k",side=3,line=-1.4,adj=.01,font=2,cex=1,col=cw_theme$ink)
  sel <- suppressWarnings(as.numeric(k));i <- 0
  for(v in fields){
    r <- i%/%nc;cc <- i%%nc;i <- i+1
    par(fig=c(f0[1]+fw*cc/nc,f0[1]+fw*(cc+1)/nc,f0[3]+fh*(nr-r-1)/nr,f0[3]+fh*(nr-r)/nr),new=TRUE,mar=if(nc==1)c(2.2,3.6,2,1) else c(3.2,3.6,3.4,1.2),cex=.92)
    y <- suppressWarnings(as.numeric(tab[[v]]));x <- tab$K;ok <- is.finite(y)
    yl <- range(y[ok]);yl <- yl+c(-1,1)*max(diff(yl)*.12,abs(yl[1])*.02,1e-6)
    plot.new();plot.window(xlim=range(x)+c(-.3,.3),ylim=yl)
    yt <- pretty(yl,4);cw_grid(y=yt);axis(2,at=yt,lwd=0);axis(1,at=x,lwd=0)
    if(length(sel)&&is.finite(sel))rect(sel-.35,yl[1]-diff(yl),sel+.35,yl[2]+diff(yl),col="#eef4f1",border=NA)
    lines(x[ok],y[ok],col=cw_palette[1],lwd=2)
    points(x[ok],y[ok],pch=21,bg=cw_palette[1],col=cw_theme$surface,cex=1.35,lwd=1.5)
    mtext(hy(nice[[v]]),side=3,line=.6,adj=0,cex=.82,font=2,col=cw_theme$ink);if(nc>1)mtext("k",side=1,line=1.9,cex=.8,col=cw_theme$ink2)
  }
  par(fig=f0)
}
plot_bootstrap <- function(sol,st,label,K) {
  b <- sol$bootstrap
  if(is.null(b)) return(cw_empty("Bootstrap stability was not requested for this run."))
  s <- b$summary
  op <- cw_begin(c(4.2,4.6,3.6,1.2));on.exit(par(op))
  plot.new();plot.window(xlim=c(.5,K+.5),ylim=c(0,1.03))
  yt <- seq(0,1,.25);cw_grid(y=yt);axis(2,at=yt,lwd=0)
  fits <- sum(strwidth(label,cex=.82))<diff(par("usr")[1:2])*.85
  axis(1,at=seq_len(K),labels=if(fits) label else seq_len(K),lwd=0,las=1)
  if(!fits) title(xlab="Cluster")
  for(r in c(.5,.75)){abline(h=r,col=cw_theme$axis,lwd=1.2)
    text(par("usr")[1],r,if(r==.5)"0.50 dissolved" else "0.75 recovered",adj=c(-.05,-.4),cex=.72,col=cw_theme$ink2)}
  segments(seq_len(K),s$P025,seq_len(K),s$P975,col=st$col,lwd=2.4,lend=1)
  points(seq_len(K),s$Mean_Jaccard,pch=st$pch,bg=st$col,col=cw_theme$surface,cex=1.7,lwd=1.6)
  title(ylab="Jaccard similarity")
  cw_title("Bootstrap stability",sprintf("Mean and 2.5 to 97.5 percentiles over %d valid %s replications",min(s$Valid_B),tolower(b$sampling)))
}
plot_heatmap <- function(p,cl,st,label,K) {
  if(!p$numeric) return(cw_empty("The heatmap needs numeric variables."))
  x <- as.matrix(p$X);colnames(x) <- p$variables;x <- scale(x);x[!is.finite(x)] <- 0
  pc <- if(ncol(x)>1) stats::prcomp(x)$x[,1] else x[,1]
  o <- order(cl,pc);z <- x[o,,drop=FALSE];z[] <- pmax(-2.5,pmin(2.5,z));g <- cl[o];n <- nrow(z);m <- ncol(z)
  colnames(z) <- short_lab(colnames(z),24);long <- max(nchar(colnames(z)))
  nar <- narrow_plot()
  op <- cw_begin(c(3.4,if(nar)min(6,1.5+long*.45) else min(12,2+long*.5),6.4,if(nar)3.4 else 5.2));on.exit(par(op))
  cols <- cw_diverging(101)
  image(seq_len(n),seq_len(m),z,col=cols,zlim=c(-2.5,2.5),axes=FALSE,xlab="",ylab="",useRaster=TRUE)
  axis(2,at=seq_len(m),labels=hy(colnames(z)),lwd=0)
  b <- cumsum(tabulate(g,K));abline(v=b[-K]+.5,col=cw_theme$surface,lwd=3)
  starts <- c(0,b[-K])+.5
  par(xpd=NA);top <- m+.5
  rect(starts,top+.15,b+.5,top+.15+m*.05,col=st$col,border=cw_theme$surface,lwd=2)
  for(h in seq_len(K)) {wd <- b[h]-starts[h];lab <- if(wd>strwidth(label[h],cex=.72)*1.15) label[h] else if(wd>strwidth(as.character(h),cex=.72)*1.3) as.character(h) else NULL
    if(!is.null(lab)) text((starts[h]+b[h]+.5)/2,top+.15+m*.05,lab,pos=3,cex=.72,col=cw_theme$ink,offset=.25)}
  usr <- par("usr");kx <- usr[2]+diff(usr[1:2])*.03;kw <- diff(usr[1:2])*.025
  ky <- seq(usr[3],usr[4],length.out=102)
  rect(kx,ky[-102],kx+kw,ky[-1],col=cols,border=NA)
  text(kx+kw,c(usr[3],mean(usr[3:4]),usr[4]),c("-2.5","0","+2.5"),pos=4,cex=.72,col=cw_theme$ink2)
  par(xpd=FALSE)
  mtext("Cases, ordered by cluster",side=1,line=1,cex=.8,col=cw_theme$ink2)
  mtext("Heatmap",side=3,line=4.9,adj=0,font=2,cex=1,col=cw_theme$ink)
  mtext(fit_line("Standardized values; blue below, red above the overall mean"),side=3,line=3.8,adj=0,cex=.78,col=cw_theme$ink2)
}
plot_projection <- function(p,cl,st,keyed,K) {
  if(p$numeric && ncol(p$X)>1) {pc <- stats::prcomp(p$X);coords <- pc$x[,1:2,drop=FALSE];ve <- 100*pc$sdev^2/sum(pc$sdev^2)
    xl <- sprintf("Principal component 1 (%.0f%%)",ve[1]);yl <- sprintf("Principal component 2 (%.0f%%)",ve[2]);sub <- "Principal components of the prepared variables"}
  else {coords <- suppressWarnings(stats::cmdscale(p$distance,k=2));if(ncol(coords)<2)coords <- cbind(coords,0)
    xl <- "Dimension 1";yl <- "Dimension 2";sub <- "Classical multidimensional scaling of the distance matrix"}
  lr <- legend_rows(keyed)
  op <- cw_begin(c(4.2,4.6,4.3+1.1*lr,1.2));on.exit(par(op))
  xr <- range(coords[,1]);yr <- range(coords[,2]);xr <- xr+c(-1,1)*diff(xr)*.04;yr <- yr+c(-1,1)*max(diff(yr)*.04,1e-6)
  plot.new();plot.window(xlim=xr,ylim=yr)
  xt <- pretty(xr,6);yt <- pretty(yr,6);cw_grid(y=yt,x=xt);axis(1,at=xt,lwd=0);axis(2,at=yt,lwd=0)
  cex <- if(nrow(coords)>800) .9 else if(nrow(coords)>300) 1.05 else 1.25
  o <- sample.int(nrow(coords))
  points(coords[o,1],coords[o,2],pch=st$pch[cl[o]],bg=st$col[cl[o]],col=cw_theme$surface,cex=cex,lwd=.9)
  title(xlab=xl,ylab=yl)
  cw_legend(keyed,st)
  mtext("Projection",side=3,line=2.8+1.1*lr,adj=0,font=2,cex=1,col=cw_theme$ink)
  mtext(fit_line(sub),side=3,line=1.7+1.1*lr,adj=0,cex=.78,col=cw_theme$ink2)
}
# Relative increase of the fusion coefficient by number of clusters (Hair et al.): a large bar means that
# going from k to k - 1 clusters merges two dissimilar groups, so k is a candidate solution.
plot_agglomeration <- function(result,k) {
  ag <- result$tables$Agglomeration
  if(is.null(ag)) return(cw_empty("The agglomeration schedule is available for hierarchical and two-stage methods."))
  r <- ag[is.finite(ag$Relative_increase)&ag$Clusters_after_stage>=1,,drop=FALSE];r <- tail(r,15)
  if(nrow(r)<2) return(cw_empty("Too few stages to show the agglomeration schedule."))
  kk <- r$Clusters_after_stage+1;v <- r$Relative_increase;ord <- order(kk);kk <- kk[ord];v <- v[ord]
  op <- cw_begin(c(4.2,4.6,3.6,1.2));on.exit(par(op))
  plot.new();plot.window(xlim=c(min(kk)-.6,max(kk)+.6),ylim=c(0,max(v)*1.12))
  yt <- pretty(c(0,max(v)),5);cw_grid(y=yt);axis(2,at=yt,labels=paste0(round(100*yt),"%"),lwd=0,las=1)
  axis(1,at=kk,lwd=0);title(xlab="Clusters before the merge (k)",col.lab=cw_theme$ink2)
  sel <- suppressWarnings(as.integer(k));top <- kk[which.max(v)]
  cols <- ifelse(kk==top,cw_palette[2],ifelse(kk%in%sel,cw_palette[1],cw_theme$axis))
  rect(kk-.35,0,kk+.35,v,col=cols,border=NA)
  cw_title("Agglomeration schedule",sprintf("Relative increase of the fusion coefficient; largest before k = %d",top))
  invisible(NULL)
}
