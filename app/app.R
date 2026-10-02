# CLAuster in the browser: the desktop app (app_main.R); packages are bundled by export-site.R.
# mclust is left out: its WebAssembly build does not load in the current webR, so Gaussian mixtures are desktop only.
if(identical(R.version$os,'emscripten'))message('CLAUSTER ENV: ',R.version.string,' | libs: ',paste(.libPaths(),collapse=', '),' | ',
  paste(vapply(c('shiny','bslib','sass','htmltools'),function(p)tryCatch(paste(p,packageVersion(p),dirname(find.package(p))),error=function(e)paste(p,'missing')),''),collapse='; '))
withCallingHandlers(source('app_main.R',local=TRUE)$value,error=function(e){
  message('CLAUSTER LOAD ERROR: ',conditionMessage(e))
  message(paste(utils::tail(utils::limitedLabels(sys.calls()),12),collapse='\n'))})
