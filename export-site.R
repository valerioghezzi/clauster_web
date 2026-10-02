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
