.onLoad <- function(libname, pkgname) {
  
  S7::methods_register()
  
  if (is.null(getOption("StreamFind_cache_mode"))) {
    message("Setting cache mode to 'rds'.")
    options("StreamFind_cache_mode" = "rds")
  }
  
  if (is.null(getOption("StreamFind_cache_path"))) {
    if (getOption("StreamFind_cache_mode") %in% "rds") {
      message("Setting cache directory to 'cache'.")
      options("StreamFind_cache_path" = "cache")
    }
  }
  
  if (is.null(getOption("StreamFind_cache_path"))) {
    if (getOption("StreamFind_cache_mode") %in% "sqlite") {
      message("Setting cache file to 'cache.sqlite'.")
      options("StreamFind_cache_path" = "cache.sqlite")
    }
  }
  
  if (!reticulate::virtualenv_exists("r-StreamFind")) {
    reticulate::virtualenv_create("r-StreamFind")
  }
  reticulate::use_virtualenv("r-StreamFind")
  reticulate::configure_environment(pkgname)
}
