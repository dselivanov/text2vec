.onUnload = function(libpath) {# nocov start
  library.dynam.unload("text2vec", libpath)
}

.onLoad = function(libname, pkgname) {
  n_cores = 1L
  if(.Platform$OS.type == "unix")
    n_cores = parallel::detectCores(logical = FALSE)
  options("text2vec.mc.cores" = n_cores)

  logger = lgr::get_logger('text2vec')
  logger$set_threshold('info')
  assign('logger', logger, envir = parent.env(environment()))
}# nocov end
