# Exports the browser app to a static site and stops if a required package was not bundled.
# Usage: Rscript export-site.R <app folder> <site folder>
args <- commandArgs(TRUE); app <- normalizePath(args[1]); site <- args[2]
needed <- c("cluster","mice","clue","foreign","haven","readxl","openxlsx","zip","jsonlite")
# Work on a copy outside any git checkout: renv skips folders listed in .gitignore files
work <- file.path(tempfile("clauster-app-")); dir.create(work)
file.copy(list.files(app, full.names=TRUE, all.files=FALSE), work, recursive=TRUE)
writeLines(sprintf("library(%s)", needed), file.path(work, "_packages.R"))
unlink(site, recursive=TRUE)
shinylive::export(work, site)
meta <- readRDS(file.path(site, "shinylive", "webr", "packages", "metadata.rds"))
files <- list.files(file.path(site, "shinylive", "webr", "packages"), recursive=TRUE, pattern="\\.tgz$")
has_file <- function(p) any(startsWith(files, paste0(p, "/")))
missing <- setdiff(needed[!vapply(needed, has_file, NA)], "jsonlite")  # jsonlite ships with Shiny in the webR image
cat("Shinylive assets", as.character(shinylive::assets_version()), "\n"); print(list.files(file.path(site, "shinylive", "webr")))
cat("Bundled packages:", length(meta), "\n"); print(vapply(meta, function(m) paste(m$version, m$ref), "")); print(grep("mclust", files, value=TRUE))
if(has_file("mclust")) stop("mclust must not be bundled: its WebAssembly build breaks later library loads")
if(length(missing)) stop("Not bundled for the browser: ", paste(missing, collapse=", "))
# Page title and a loading note: the first visit downloads R and the packages, which takes a while on phones.
idx <- file.path(site, "index.html"); h <- readLines(idx, warn = FALSE, encoding = "UTF-8")
h <- sub("<title>[^<]*</title>", "<title>CLAuster: Cluster Analysis for Applied Research</title>", h)
note <- c('<div id="cl-loading" style="position:fixed;left:0;right:0;bottom:14%;text-align:center;font:15px/1.5 system-ui,-apple-system,Segoe UI,Roboto,sans-serif;color:#52514e;padding:0 24px;z-index:10">',
  '<b style="color:#174b38">CLAuster</b> is starting R in your browser.<br>The first visit can take 1 to 3 minutes (longer on phones and slow connections); later visits are faster.<br><span id="cl-elapsed"></span></div>',
  '<script>(function(){var t0=Date.now(),el=document.getElementById("cl-loading"),s=document.getElementById("cl-elapsed");',
  'var timer=setInterval(function(){var sec=Math.round((Date.now()-t0)/1000);',
  's.textContent=sec<300?"Elapsed: "+sec+" s":"Still loading after "+Math.round(sec/60)+" minutes: check the connection, then reload the page.";',
  'var fr=document.querySelectorAll("iframe");for(var i=0;i<fr.length;i++){try{var d=fr[i].contentDocument;if(d&&d.querySelector(".app-header")){el.remove();clearInterval(timer);return;}}catch(e){}}},1000);})();</script>')
i <- tail(grep("</body>", h), 1)
if (!length(i)) stop("index.html has no </body> tag: the loading note could not be added")
h <- append(h, note, after = i - 1); writeLines(h, idx, useBytes = TRUE)
cat("Title and loading note added to", idx, "\n")
