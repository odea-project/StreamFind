.onLoad <- function(libname, pkgname) {
  
  S7::methods_register()
  
  if (!is.null(getOption("StreamFind_cache_mode"))) {
    options("StreamFind_cache_mode" = "rds")
  }
  
  if (!is.null(getOption("StreamFind_cache_dir"))) {
    options("StreamFind_cache_dir" = "cache")
  }
  
  if (!is.null(getOption("StreamFind_cache_file"))) {
    options("StreamFind_cache_file" = "cache.sqlite")
  }
  
  if (!reticulate::virtualenv_exists("r-StreamFind")) {
    reticulate::virtualenv_create("r-StreamFind")
  }
  reticulate::use_virtualenv("r-StreamFind")
  reticulate::configure_environment(pkgname)
}
