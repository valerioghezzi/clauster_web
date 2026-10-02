library(shiny)
options(shiny.maxRequestSize=500*1024^2)
for(f in list.files("R",pattern="\\.R$",full.names=TRUE))source(f)
try(sweep_stale_jobs(),silent=TRUE)
root<-getwd()
card<-function(title,...,class=NULL,note=NULL)div(class=paste("cw-card",class),if(!is.null(title))div(class="cw-card-head",h3(title),if(!is.null(note))span(class="cw-card-note",note)),div(class="cw-card-body",...))
group<-function(title,...)tags$details(class="cw-group",tags$summary(title),div(class="cw-group-body",...))
method_choices<-c("Ward D2"="ward.D2","Ward + relocation"="ward_relocate","Hierarchical + K-means"="hybrid_kmeans","Hierarchical + relocation"="hybrid_relocate","Hierarchical + PAM"="hybrid_pam","K-means"="kmeans","PAM (k-medoids)"="pam","Average linkage"="average","Complete linkage"="complete","Single linkage"="single","Weighted average linkage"="mcquitty","Centroid linkage"="centroid","Median linkage"="median","Flexible beta"="flexible","DIANA (divisive)"="diana","Fuzzy clustering"="fuzzy","Gaussian mixture"="gmm","Ward D (legacy)"="ward.D")
# Gaussian mixtures need mclust, which may be missing (for example in the browser version).
if(!has_package("mclust"))method_choices<-method_choices[method_choices!="gmm"]
ui<-fluidPage(title="CLAuster",
  theme=bslib::bs_theme(version=5,primary="#174b38",bg="#ffffff",fg="#1d2125",
    base_font=bslib::font_collection("system-ui","-apple-system","Segoe UI","Roboto","Helvetica Neue","Arial","sans-serif"),
    "font-size-base"="0.9rem","border-radius"="0.5rem","input-border-color"="#d6d9dc"),
  tags$head(tags$link(rel="stylesheet",href="style.css"),
    tags$script(HTML("Shiny.addCustomMessageHandler('status-kind',function(k){var e=document.getElementById('status-box');if(e)e.className='status-box '+k;});"))),
  div(class="app-header",
    div(class="brand",tags$img(src="CLAuster-logo.png",class="app-logo",alt="CLAuster"),
      div(div(class="brand-name","CLAuster"),div(class="brand-tag","Cluster Analysis for Applied Research"))),
    uiOutput("data_chip")),
  tabsetPanel(id="tab",type="pills",
    tabPanel(HTML("<span class='step'>1</span>Data"),value="Data",div(class="cw-layout",
      div(class="cw-side",
        card("Data source",fileInput("file",NULL,buttonLabel="Open file",placeholder="CSV, Excel, SPSS, Stata, SAS, RDS",
            accept=c(".csv",".txt",".tsv",".dat",".xlsx",".xls",".sav",".zsav",".por",".dta",".sas7bdat",".rds")),
          actionButton("example","Load example data",class="btn-outline-secondary w-100")),
        card("Variables",selectInput("id","Case ID",choices=c("Row number"="")),
          selectizeInput("variables","Analysis variables",choices=NULL,multiple=TRUE,options=list(plugins=list("remove_button"))),
          NULL)),
      div(class="cw-main",uiOutput("data_main")))),
    tabPanel(HTML("<span class='step'>2</span>Analysis"),value="Analysis",div(class="cw-layout",
      div(class="cw-side",
        card("Method",selectInput("method","Clustering method",choices=method_choices,selected="ward.D2"),uiOutput("method_note"),
          conditionalPanel("input.method.indexOf('hybrid_')===0",selectInput("hybrid_start","Hierarchical start",choices=c("Ward D2"="ward.D2","Ward D"="ward.D","Average linkage"="average","Complete linkage"="complete","Single linkage"="single","Weighted average linkage"="mcquitty","Centroid linkage"="centroid","Median linkage"="median","Flexible beta"="flexible","DIANA"="diana"),selected="ward.D2")),
              conditionalPanel("input.method==='hybrid_kmeans'",selectInput("hybrid_algorithm","K-means refinement",choices=c("Hartigan-Wong"="Hartigan-Wong","Batch / Lloyd"="Lloyd","Sequential / MacQueen"="MacQueen"),selected="Hartigan-Wong")),
          selectInput("distance","Distance",choices=c("Euclidean"="euclidean","Squared Euclidean"="sqeuclidean","Manhattan"="manhattan","Maximum"="maximum","Minkowski (p = 2)"="minkowski","Mahalanobis"="mahalanobis","Correlation"="correlation","Cosine"="cosine","Gower (mixed variables)"="gower","Jaccard (0/1)"="jaccard","Simple matching (0/1)"="matching")),
          selectInput("scaling","Scaling",choices=c("Z scores"="z","None"="none","Range (0 to 1)"="range","Median and MAD"="robust")),
          selectInput("missing","Missing data",choices=c("Complete cases"="complete","Close-neighbor imputation"="neighbor","Mean imputation"="mean","Available coordinates (Gower)"="available","Multiple imputation"="multiple"))),
        card("Number of clusters",div(class="cw-row",numericInput("k_min","From k",2,min=1,max=30),numericInput("k_max","To k",6,min=1,max=30))),
        card("Validation",checkboxInput("bootstrap","Bootstrap stability",TRUE),checkboxInput("simulation","Null simulations",TRUE),
          numericInput("B","Replications",200,min=10,max=10000,step=50),
          div(class="cw-groups",
            group("Resampling",selectInput("null","Simulation reference",choices=c("Column permutation"="permutation","One Gaussian population"="gaussian")),
              checkboxInput("subsample","Subsampling without replacement",FALSE),numericInput("fraction","Sampling fraction",.8,min=.2,max=.95,step=.05),
              checkboxInput("gap","Gap statistic",FALSE),numericInput("gap_B","Gap replications",100,min=10,max=1000),
              checkboxInput("advanced_indices","Null simulations also for C-index, Gamma and G-plus (slower)",FALSE)),
            group("Algorithm",numericInput("nstart","Random starts",20,min=1,max=1000),numericInput("max_iter","Maximum iterations",100,min=10,max=10000),
              numericInput("beta","Flexible beta",-.25,min=-1,max=.999,step=.05),
              selectInput("covariance","Gaussian covariance",choices=c("Best by BIC"="all","EEE","VVV","EEI","VVI","EII","VII")),
              textInput("weights","Variable weights",placeholder="e.g. 1, 1, 2")),
            group("Missing data",numericInput("max_missing","Maximum missing proportion per case",.5,min=0,max=.9,step=.1),
              numericInput("donor_threshold","Donor distance threshold (ASED)",.5,min=0,step=.1),
              numericInput("mi_m","Imputed datasets",5,min=2,max=50),numericInput("mi_iter","Imputation iterations",5,min=1,max=50)),
            group("Run",numericInput("seed","Random seed",2026,min=1,max=1e8),numericInput("timeout","Time limit (seconds)",1800,min=5,max=86400)))),
        NULL),
      div(class="cw-main",
        div(id="status-box",class="status-box neutral",div(class="status-row",div(class="status-text",textOutput("status")),
          div(class="actions",actionButton("run","Run analysis",class="btn-primary",icon=icon("play")),if(!in_browser())actionButton("cancel","Cancel",class="btn-outline-secondary"))),uiOutput("progress")),
        card("Fit indices",uiOutput("comparison_ui"),note="Solutions compared side by side")))),
    tabPanel(HTML("<span class='step'>3</span>Results"),value="Results",div(class="cw-layout results-layout",
      div(class="cw-side",
        card("View",selectInput("solution","Solution",choices=NULL),
          selectInput("plot","Plot",choices=c("Profiles","Projection","Silhouette","Dendrogram","Heatmap","Indices","Bootstrap")),
          selectInput("table","Table",choices=NULL),
          checkboxInput("bw","Black and white",FALSE),textInput("labels","Cluster labels",placeholder="e.g. Low, Medium, High"),uiOutput("labels_hint")),
        card("Export",uiOutput("export_ui")),
        card("Compare solutions",selectInput("compare_a","Solution A",choices=NULL),selectInput("compare_b","Solution B",choices=NULL),
          actionButton("compare","Compare",class="btn-outline-primary w-100"))),
      div(class="cw-main",uiOutput("results_main"))))
  ),
  div(class="app-footer",span(paste("CLAuster",cw_version,if(in_browser())"- runs in this browser; data stay on this computer" else "")),
    if(!in_browser())actionButton("shutdown","Close application",class="btn-outline-secondary btn-sm",icon=icon("power-off"))))
# Automatic measurement level: two values binary, 3 to 7 whole numbers ordinal, otherwise continuous.
# Text with more than two categories is not analyzed ("text").
default_level<-function(v){
  if(is.logical(v))return("binary");if(is.ordered(v))return("ordinal")
  u<-unique(v[!is.na(v)]);if(length(u)==2)return("binary")
  if(is.numeric(v)){if(length(u)>=3&&length(u)<=7&&all(abs(u-round(u))<1e-9))return("ordinal");return("continuous")}
  "text"}
level_summary<-function(v){u<-unique(v[!is.na(v)]);if(!length(u))return("no values")
  if(is.numeric(v)&&length(u)>7)return(sprintf("%s to %s",format(signif(min(u),3)),format(signif(max(u),3))))
  if(is.numeric(v))return(sprintf("%d values: %s",length(u),paste(format(sort(u)),collapse=", ")))
  sprintf("%d categories",length(u))}
display_table<-function(x){if(!is.data.frame(x))return(x);if(is.numeric(x$Percent))x$Percent<-ifelse(is.na(x$Percent),"",sprintf("%.1f",x$Percent));n<-names(x);n<-sub("_pct$"," %",n);n<-sub("^P025$","2.5%",n);n<-sub("^P975$","97.5%",n);names(x)<-gsub("_"," ",n);x}
server<-function(input,output,session) {
  rv<-reactiveValues(data=NULL,result=NULL,job=NULL,status="Open a data file or load the example data in step 1.",progress=NULL,history=list(),agreement=NULL,source=NULL)
  rv$load_id<-0L
  rv$levels<-character()
  session$onSessionEnded(function(){job<-isolate(rv$job);if(!is.null(job))cancel_job(job)})
  load_table<-function(d) {
    if(!is.null(rv$job)){cancel_job(rv$job);rv$job<-NULL};rv$result<-NULL;rv$progress<-NULL
    rv$load_id<-isolate(rv$load_id)+1L;rv$levels<-vapply(d,default_level,"");rv$data<-d;rv$history<-list();rv$agreement<-NULL;rv$centroid_match<-NULL
    updateSelectInput(session,"compare_a",choices=character());updateSelectInput(session,"compare_b",choices=character())
    ids<-names(d)[grepl("(^id$|^id[._ ]|[._ ]id$|case.?id|^codice|^code$|^subject|^soggetto|^partecipante|^participant|^respondent)",names(d),ignore.case=TRUE)]
    ids<-ids[vapply(ids,function(v)!anyNA(d[[v]])&&!anyDuplicated(d[[v]]),logical(1))]
    if(!length(ids)){u<-names(d)[vapply(d,function(v)is.character(v)&&!anyNA(v)&&!anyDuplicated(v),logical(1))];ids<-head(u,1)}
    chosen<-if(length(ids))ids[1]else""
    updateSelectInput(session,"id",choices=c("Row number"="",names(d)),selected=chosen)
    vars<-names(d)[vapply(d,is.numeric,logical(1))];vars<-setdiff(vars,chosen)
    updateSelectizeInput(session,"variables",choices=names(d),selected=vars,server=TRUE)
    rv$status<-paste0("Data loaded: ",nrow(d)," cases and ",ncol(d)," columns. Check the variables, then open step 2.")
    if(nrow(d)>2000)showNotification(sprintf("This file has %d rows. An analysis can use at most 2,000 cases (500 with multiple imputation).",nrow(d)),type="warning",duration=10)
  }
  observeEvent(input$file,{tryCatch({load_table(import_data(input$file$datapath,input$file$name));rv$source<-input$file$name},error=function(e){rv$status<-paste("The file could not be opened:",conditionMessage(e));showNotification(rv$status,type="error",duration=8)})})
  observeEvent(input$example,{
    set.seed(2026);n<-50;g<-rep(1:3,each=n)
    d<-data.frame(ID=sprintf("P%03d",seq_along(g)),Stress=rnorm(3*n,c(1,4,3)[g],.55),Support=rnorm(3*n,c(4,1,4)[g],.55),Engagement=rnorm(3*n,c(4,2,3)[g],.55))
    load_table(d);rv$source<-"Example data"
  })
  # Variables that cannot contribute are removed at once, and text variables switch the distance to Gower.
  observeEvent(input$variables,{
    req(rv$data,all(input$variables%in%names(rv$data)))
    v<-input$variables;d<-rv$data[v]
    empty<-v[vapply(d,function(x)all(is.na(x)),logical(1))]
    const<-setdiff(v[vapply(d,function(x){u<-unique(x[!is.na(x)]);length(u)<2},logical(1))],empty)
    text<-setdiff(v[vapply(d,function(x)default_level(x)=="text",logical(1))],c(empty,const))
    drop<-c(empty,const,text)
    if(length(drop)){updateSelectizeInput(session,"variables",selected=setdiff(v,drop))
      why<-c(if(length(empty))paste0(paste(empty,collapse=", "),": no observed values"),if(length(const))paste0(paste(const,collapse=", "),": the same value for every case"),
        if(length(text))paste0(paste(text,collapse=", "),": text with more than two categories (recode ordered categories as numbers)"))
      showNotification(paste0("Removed from the analysis variables. ",paste(why,collapse="; "),"."),type="message",duration=10);return()}
  },ignoreInit=TRUE)
  observeEvent(input$id,{
    drop<-input$id;drop<-drop[nzchar(drop)]
    if(length(drop)&&any(input$variables%in%drop))updateSelectizeInput(session,"variables",selected=setdiff(input$variables,drop))
  },ignoreInit=TRUE)
  observeEvent(input$method,{
    if(input$method%in%euclidean_methods&&input$distance!="euclidean"){updateSelectInput(session,"distance",selected="euclidean");showNotification("This method uses Euclidean distance; Distance set to Euclidean.",type="message")}
    if(input$method=="hybrid_pam"&&!input$distance%in%c("euclidean","sqeuclidean")&&input$hybrid_start%in%c("ward.D2","ward.D","centroid","median")){updateSelectInput(session,"hybrid_start",selected="average");showNotification("Ward, centroid and median starts need Euclidean distance; Hierarchical start set to Average linkage.",type="message")}
  },ignoreInit=TRUE)
  observeEvent(input$distance,{
    if(input$distance%in%c("jaccard","matching")&&input$scaling!="none"){updateSelectInput(session,"scaling",selected="none");showNotification("Binary distances use 0/1 values; Scaling set to None.",type="message")}
    if(input$distance!="euclidean"&&input$method%in%euclidean_methods){updateSelectInput(session,"method",selected="average");showNotification(paste(method_labels[[input$method]],"requires Euclidean distance; Method set to Average linkage."),type="message")}
    if(input$distance!="gower"&&input$missing=="available")updateSelectInput(session,"missing",selected="complete")
    if(isTRUE(input$method%in%hybrid_methods)&&!input$distance%in%c("euclidean","sqeuclidean")&&isTRUE(input$hybrid_start%in%c("ward.D2","ward.D","centroid","median"))){
      updateSelectInput(session,"hybrid_start",selected="average");showNotification("Ward, centroid and median starts need Euclidean distance; Hierarchical start set to Average linkage.",type="message")}
  },ignoreInit=TRUE)
  observeEvent(input$missing,{if(input$missing=="available"&&input$distance!="gower"){updateSelectInput(session,"distance",selected="gower");showNotification("Available coordinates require Gower distance; Distance set to Gower.",type="message")}},ignoreInit=TRUE)
  output$method_note<-renderUI({
    note<-switch(input$method,hybrid_kmeans="Two stages: the hierarchical tree gives the starting groups, then K-means moves cases to the nearest center. Numeric variables, Euclidean distance.",
      hybrid_relocate="Two stages: the hierarchical tree gives the starting groups, then cases are moved one at a time while the within-cluster sum of squares falls. Numeric variables, Euclidean distance.",
      hybrid_pam="Two stages: the hierarchical tree gives the starting medoids, then PAM swaps medoids while the total distance falls. Any distance.",
      ward.D="Legacy R \"ward\" on unsquared Euclidean distances, kept to reproduce older results: it does not minimize the within-cluster sum of squares. Prefer Ward D2.",
      ward_relocate=,ward.D2=,kmeans=,centroid=,median=,fuzzy="Numeric variables, Euclidean distance.",
      gmm="Numeric variables; compares covariance structures by BIC.",
      "Any distance; Gower for mixed variables.")
    div(class="hint",note)})
  output$levels_ui<-renderUI({
    req(rv$data,input$variables,all(input$variables%in%names(rv$data)))
    lv<-c(continuous="Continuous",ordinal="Ordinal",binary="Binary")
    js<-function(v,l)sprintf("Shiny.setInputValue('level_click',{v:%s,l:'%s',n:Math.random()},{priority:'event'})",jsonlite::toJSON(v,auto_unbox=TRUE),l)
    rows<-lapply(input$variables,function(v){cur<-rv$levels[[v]]%||%default_level(rv$data[[v]])
      tags$tr(tags$td(class="lv-name",title=v,v),tags$td(class="lv-sum",level_summary(rv$data[[v]])),
        tags$td(div(class="lv-seg",lapply(names(lv),function(l)tags$button(type="button",class=paste("lv-btn",if(l==cur)"on"),onclick=js(v,l),lv[[l]])))))})
    card("Measurement levels",
      div(class="lv-bulk",span("Set all:"),
        tags$button(type="button",class="btn btn-sm btn-outline-primary",onclick=js("__all__","auto"),"Detect automatically"),
        tags$button(type="button",class="btn btn-sm btn-outline-secondary",onclick=js("__all__","continuous"),"All continuous"),
        tags$button(type="button",class="btn btn-sm btn-outline-secondary",onclick=js("__all__","ordinal"),"All ordinal")),
      div(class="table-wrap lv-wrap",tags$table(class="table lv-table",tags$tbody(rows))),
      note="One click per variable")
  })
  observeEvent(input$level_click,{
    x<-input$level_click;req(rv$data)
    if(identical(x$v,"__all__")){vars<-input$variables
      rv$levels[vars]<-if(x$l=="auto")vapply(rv$data[vars],default_level,"") else x$l
    } else if(x$v%in%names(rv$data)&&x$l%in%c("continuous","ordinal","binary")) rv$levels[[x$v]]<-x$l
  })
  output$data_chip<-renderUI({
    if(is.null(rv$data))return(div(class="data-chip empty","No data loaded"))
    div(class="data-chip",icon("table"),span(class="chip-name",rv$source%||%"Data"),span(class="chip-meta",HTML(sprintf("%d cases &middot; %d variables selected",nrow(rv$data),length(input$variables)))))
  })
  output$data_main<-renderUI({
    if(is.null(rv$data))return(div(class="empty-state",tags$img(src="CLAuster-logo.png",alt=""),h2("Start with your data"),
      p("Open a file in CSV, TXT, Excel, SPSS, Stata, SAS or RDS format, or load the example data to try the program."),
      p(class="muted","Separators, decimal commas or points and text encodings are recognized automatically. Data stay on this computer.")))
    tagList(uiOutput("levels_ui"),card("Preview",div(class="table-wrap",tableOutput("preview")),note=sprintf("First %d of %d rows",min(15,nrow(rv$data)),nrow(rv$data))),
      card("Descriptive statistics",div(class="table-wrap",tableOutput("descriptives")),note="Selected variables"))
  })
  observe({
    st<-rv$status;kind<-if(!is.null(rv$job))"running" else if(grepl("^Completed",st))"success" else if(grepl("partial|Canceled|Time limit",st))"warning"
      else if(grepl("^Data loaded|^Open a data",st))"neutral" else "error"
    session$sendCustomMessage("status-kind",kind)
  })
  output$preview<-renderTable({req(rv$data);x<-head(rv$data,15);for(j in seq_along(x))if(is.numeric(x[[j]])&&all(is.na(x[[j]])|abs(x[[j]]-round(x[[j]]))<1e-9))x[[j]]<-as.integer(round(x[[j]]));x},rownames=FALSE,hover=TRUE,spacing="s",na="")
  output$descriptives<-renderTable({req(rv$data,input$variables,all(input$variables%in%names(rv$data)));display_table(describe_data(rv$data,input$variables))},digits=3,hover=TRUE,spacing="s",na="")
  config<-reactive({
    vars<-input$variables;lev<-vapply(vars,function(v)rv$levels[[v]]%||%default_level(rv$data[[v]]),"",USE.NAMES=FALSE)
    w<-if(nzchar(trimws(input$weights)))suppressWarnings(as.numeric(strsplit(input$weights,",",fixed=TRUE)[[1]]))else NULL
    cfg<-cw_config(list(variables=vars,id=if(nzchar(input$id))input$id else NULL,
      levels=lev,weights=w,method=input$method,distance=input$distance,scaling=input$scaling,missing=input$missing,k_min=input$k_min,k_max=input$k_max,
      bootstrap=input$bootstrap,simulation=input$simulation,B=as.integer(input$B),seed=as.integer(input$seed),timeout=input$timeout,
      null=input$null,subsample=input$subsample,fraction=input$fraction,gap=input$gap,gap_B=as.integer(input$gap_B),
      donor_threshold=input$donor_threshold,
      max_missing=input$max_missing,nstart=as.integer(input$nstart),max_iter=as.integer(input$max_iter),beta=input$beta,hybrid_start=input$hybrid_start,hybrid_algorithm=input$hybrid_algorithm,
      advanced_indices=input$advanced_indices,mi_m=as.integer(input$mi_m),mi_iter=as.integer(input$mi_iter),covariance=input$covariance))
    cfg
  })
  observeEvent(input$run,{
    if(!is.null(rv$job)){rv$status<-"An analysis is already running.";return()}
    if(is.null(rv$data)||!length(input$variables)){rv$status<-"Open data and select analysis variables.";return()}
    nums<-c(k_min=input$k_min,k_max=input$k_max,B=input$B,seed=input$seed,timeout=input$timeout)
    bad<-names(nums)[vapply(nums,function(v)length(v)!=1||is.na(v),logical(1))]
    if(length(bad)){rv$status<-paste("Enter a number in:",paste(c(k_min="From k",k_max="To k",B="Replications",seed="Random seed",timeout="Time limit")[bad],collapse=", "));return()}
    if(input$k_min>input$k_max){rv$status<-"Minimum k must not exceed maximum k.";return()}
    whole<-c(k_min="From k",k_max="To k",B="Replications",seed="Random seed")
    frac<-names(whole)[vapply(names(whole),function(v)input[[v]]!=round(input[[v]]),logical(1))]
    if(length(frac)){rv$status<-paste("Enter a whole number in:",paste(whole[frac],collapse=", "));return()}
    usable<-sum(complete.cases(rv$data[input$variables]))
    if(input$missing!="complete")usable<-nrow(rv$data)
    if(input$k_max>usable-1&&usable>2)showNotification(sprintf("To k is larger than the data allow; solutions are fitted up to k = %d (cases minus one).",usable-1),type="message",duration=8)
    tryCatch({cfg<-config()
      if(cfg$method%in%hybrid_methods&&!cfg$distance%in%c("euclidean","sqeuclidean")&&cfg$hybrid_start%in%c("ward.D","ward.D2","centroid","median")){
        cfg$hybrid_start<-"average";updateSelectInput(session,"hybrid_start",selected="average")
        showNotification("Ward, centroid and median starts need Euclidean distance; Hierarchical start set to Average linkage.",type="message",duration=8)}
      rv$job<-start_job(rv$data,cfg,root);rv$status<-"Starting analysis...";rv$progress<-NULL},error=function(e)rv$status<-conditionMessage(e))
  })
  observeEvent(input$cancel,{if(!is.null(rv$job)){cancel_job(rv$job);rv$job<-NULL;rv$progress<-NULL;rv$status<-"Canceled. The previous completed result is retained."}})
  observe({invalidateLater(250,session);job<-isolate(rv$job);if(is.null(job))return()
    s<-poll_job(job)
    if(s$status=="running") {rv$progress<-s$progress;rv$status<-sprintf("Running... %s elapsed",if(s$elapsed<60)sprintf("%.0f s",s$elapsed)else sprintf("%d min %02d s",as.integer(s$elapsed%/%60),as.integer(s$elapsed%%60)))}
    else {
      rv$job<-NULL;rv$progress<-NULL
      if(s$status=="completed") {
        r<-s$result;rv$result<-r;rv$fresh<-TRUE;rv$status<-paste0(r$status%||%"Completed",if(!is.null(r$elapsed))sprintf(" in %.1f seconds",r$elapsed)else"",if(length(r$solutions))". Plots and tables are in step 3." else ".")
        sols<-names(r$solutions);sk<-suggested_k(r);updateSelectInput(session,"solution",choices=if(length(sols))setNames(sols,paste0("k = ",sols,ifelse(sols==(sk%||%""),"  (suggested)","")))else character(),selected=if(length(sols))(sk%||%sols[1])else character())
        for(k in names(r$solutions)) {
          key<-paste0(length(rv$history)+1," | ",method_labels[[r$config$method]]%||%r$config$method," | k = ",k)
          sol<-r$solutions[[k]];sol$prepared<-r$prepared;rv$history[[key]]<-sol
        }
        updateSelectInput(session,"compare_a",choices=names(rv$history));updateSelectInput(session,"compare_b",choices=names(rv$history),selected=tail(names(rv$history),1))
      } else rv$status<-s$error
    }
  })
  output$status<-renderText(rv$status)
  output$progress<-renderUI({p<-rv$progress;if(is.null(p))return(NULL);pct<-round(100*p$completed/max(1,p$total))
    tagList(div(class="progress-label",span(p$phase),span(sprintf("%d of %d",p$completed,p$total))),
      div(class="progress",div(class="progress-bar",role="progressbar",style=sprintf("width:%d%%",pct))))})
  output$comparison_ui<-renderUI({
    if(is.null(rv$result))return(div(class="empty-inline","Results appear here after you press Run analysis. Each row summarizes one number of clusters."))
    if(!nrow(rv$result$comparison))return(div(class="empty-inline","No solutions were fitted. See the status message above."))
    tagList(div(class="table-wrap fit-table",tableOutput("comparison")),p(class="table-note",fit_indices_note))
  })
  output$results_main<-renderUI({
    if(is.null(rv$result))return(div(class="empty-state",tags$img(src="CLAuster-logo.png",alt=""),h2("No results yet"),p("Run an analysis in step 2. Plots, tables and exports will appear here.")))
    tagList(uiOutput("findings_ui"),card(NULL,plotOutput("result_plot",height="auto"),class="plot-card"),
      card(textOutput("table_title",inline=TRUE),div(class="table-wrap",tableOutput("result_table")),note="Up to 200 rows shown; exports contain all rows"),
      if(!is.null(rv$agreement))card("Agreement between solutions",div(class="table-wrap",tableOutput("agreement")),note=rv$agreement_label),
      if(!is.null(rv$agreement))card("Matched cluster centroids",
        if(is.character(rv$centroid_match))div(class="empty-inline",rv$centroid_match) else tagList(div(class="table-wrap",tableOutput("centroid_match")),
          p(class="table-note","Each cluster of A is paired with one cluster of B so that the total distance between centroids is smallest. ASED: average squared Euclidean distance between centroids on standardized variables; smaller means more similar profiles.")),
        note="Solution A vs solution B"))
  })
  output$findings_ui<-renderUI({req(rv$result,input$solution);kf<-tryCatch(key_findings(rv$result,input$solution),error=function(e)NULL)
    if(is.null(kf)||!nrow(kf))return(NULL)
    card(paste0("Key findings, k = ",input$solution),tags$dl(class="findings",lapply(seq_len(nrow(kf)),function(i)tagList(tags$dt(kf$Topic[i]),tags$dd(kf$Finding[i])))),
      note="Plain-language reading of the indices")})
  output$table_title<-renderText(gsub("_"," ",input$table%||%"Table"))
  output$comparison<-renderTable({req(rv$result);fit_indices(rv$result)},digits=3,na="",hover=TRUE,spacing="s")
  selected_tables<-reactive({req(rv$result)
    g<-c(list(Fit_indices=fit_indices(rv$result)),rv$result$tables);s<-solution_tables(rv$result,input$solution)
    keep_g<-c("Fit_indices","Descriptives","Missingness","Missing_patterns","Case_audit","Donors","Warnings","MI_fit_summary","MI_events","MI_warnings")
    keep_s<-c("Profiles","Clusters","Bootstrap","Bootstrap_agreement","Simulation","Posterior","Silhouette")
    g<-g[intersect(keep_g,names(g))];if(!any(rv$result$tables$Missingness$Missing>0))g$Missingness<-g$Missing_patterns<-NULL
    c(g[setdiff(names(g),c("Missingness","Missing_patterns","Case_audit","Donors","MI_events","MI_warnings","Warnings"))],s[intersect(keep_s,names(s))],g[intersect(c("Missingness","Missing_patterns","Case_audit","Donors","Warnings","MI_events","MI_warnings"),names(g))])})
  observe({z<-selected_tables();choices<-names(z)[vapply(z,function(t)is.data.frame(t)&&nrow(t)>0,logical(1))];preferred<-intersect(c("Profiles","Fit_indices"),choices)
    keep<-!isTRUE(isolate(rv$fresh))&&!is.null(isolate(input$table))&&isolate(input$table)%in%choices;if("Profiles"%in%choices||!length(isolate(rv$result$solutions)))rv$fresh<-FALSE
    updateSelectInput(session,"table",choices=setNames(choices,gsub("_"," ",choices)),selected=if(keep)isolate(input$table)else c(preferred,choices)[1])})
  shown_table<-reactive({z<-selected_tables();req(input$table,input$table%in%names(z));display_table(head(z[[input$table]][,seq_len(min(ncol(z[[input$table]]),30)),drop=FALSE],200))})
  output$result_table<-renderTable(shown_table(),digits=3,hover=TRUE,spacing="s",na="",
    align=function(){x<-shown_table();if(!is.data.frame(x)||!ncol(x))return(NULL);paste(ifelse(vapply(x,is.numeric,logical(1))|names(x)=="Percent","r","l"),collapse="")})
  output$labels_hint<-renderUI({
    l<-cluster_labels();k<-rv$result$solutions[[input$solution%||%""]]$k
    if(is.null(l)||is.null(k))return(NULL)
    if(length(l)!=k)return(div(class="hint",sprintf("%d labels for %d clusters: default names are used. Separate one label per cluster with commas.",length(l),k)))
    if(any(!nzchar(l)))return(div(class="hint","One label is empty: default names are used."))
    NULL})
  cluster_labels<-reactive({if(!nzchar(trimws(input$labels)))NULL else trimws(strsplit(input$labels,",",fixed=TRUE)[[1]])})
  output$result_plot<-renderPlot({req(rv$result,input$solution);plot_solution(rv$result,input$solution,input$plot,cluster_labels(),input$bw)},res=104,
    height=function(){w<-session$clientData$output_result_plot_width%||%900;if(w<600)max(380,round(w*if(identical(input$plot,"Indices"))2.6 else 1.35)) else max(420,min(640,round(w*.5)))})
  observeEvent(input$compare,{
    if(length(rv$history)<1||!nzchar(input$compare_a%||%"")||!nzchar(input$compare_b%||%"")){showNotification("Run an analysis first; then choose two solutions to compare.",type="warning");return()}
    tryCatch({A<-rv$history[[input$compare_a]];B<-rv$history[[input$compare_b]];rv$agreement<-compare_partitions(A,B);rv$centroid_match<-tryCatch(compare_centroids(A,B),error=function(e)conditionMessage(e));rv$agreement_label<-paste(input$compare_a,"vs",input$compare_b)},error=function(e)rv$status<-conditionMessage(e))})
  output$centroid_match<-renderTable({req(is.data.frame(rv$centroid_match));display_table(rv$centroid_match)},digits=3,hover=TRUE,spacing="s",na="")
  output$agreement<-renderTable({req(rv$agreement);display_table(rv$agreement$summary)},digits=3,hover=TRUE,spacing="s")
  output$export<-downloadHandler(filename=function()"CLAuster_session.zip",content=function(file){req(rv$result);export_session(rv$result,rv$result$original,file,root,input$solution,cluster_labels(),rv$source)})
  output$report<-downloadHandler(filename=function()"CLAuster_report.pdf",content=function(file){req(rv$result)
    tryCatch(write_report(rv$result,file,input$solution,cluster_labels(),isTRUE(input$bw),rv$source),
      error=function(e){showNotification(paste("The report could not be created:",conditionMessage(e)),type="error",duration=8);stop(e)})})
  output$plot_export<-downloadHandler(filename=function()paste0("CLAuster_plot.",tolower(input$plot_format%||%"PDF")),content=function(file){
    req(rv$result);fmt<-input$plot_format%||%"PDF";if(fmt=="PDF")open_pdf(file,8,5)else if(fmt=="SVG")grDevices::svg(file,width=8,height=5)else png(file,width=2400,height=1500,res=300)
    on.exit(dev.off());plot_solution(rv$result,input$solution,input$plot,cluster_labels(),input$bw)
  })
  output$export_ui<-renderUI({
    if(is.null(rv$result))return(div(class="empty-inline","Available after an analysis."))
    div(class="cw-downloads",downloadButton("report","Report (PDF)",class="btn-primary"),downloadButton("export","Session (ZIP)",class="btn-outline-primary"),
      downloadButton("data_export","Data with clusters",class="btn-outline-primary"),
      div(class="cw-row",downloadButton("plot_export","Plot",class="btn-outline-primary"),selectInput("plot_format",NULL,choices=if(svg_works())c("PDF","SVG","PNG") else c("PDF","PNG"),selected=isolate(input$plot_format)%||%"PDF")))
  })
  output$data_export<-downloadHandler(filename=function()"CLAuster_data_with_clusters.xlsx",content=function(file){
    req(rv$result);write_data_with_clusters(rv$result,file)
  })
  observeEvent(input$shutdown,{if(!is.null(rv$job))cancel_job(rv$job)
    showModal(modalDialog(title="CLAuster has been closed","The analysis server has stopped. You can close this browser tab.",footer=NULL,easyClose=FALSE))
    later::later(function()stopApp(),.8)})
}
shinyApp(ui,server)
