.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("StreamFind_cache_mode"))) {
    options("StreamFind_cache_mode" = "rds")
    message("Caching mode set to 'rds'!")
  }
  if (is.null(getOption("StreamFind_cache_path"))) {
    if (getOption("StreamFind_cache_mode") %in% "rds") {
      options("StreamFind_cache_path" = "cache")
      message("Caching directory set to 'cache'!")
    }
  }
  if (is.null(getOption("StreamFind_cache_path"))) {
    if (getOption("StreamFind_cache_mode") %in% "sqlite") {
      options("StreamFind_cache_path" = "cache.sqlite")
      message("Caching file set to 'cache.sqlite'!")
    }
  }
  # Setup Python environment only if not already configured
  tryCatch({
    # Check if we should set up the virtualenv
    if (!reticulate::py_available(initialize = FALSE)) {
      # Python not initialized yet, safe to configure
      if (!reticulate::virtualenv_exists("r-StreamFind")) {
        reticulate::virtualenv_create("r-StreamFind")
        if (reticulate::virtualenv_exists("r-StreamFind")) {
          message("Created virtualenv 'r-StreamFind'.")
        }
      }

      if (reticulate::virtualenv_exists("r-StreamFind")) {
        reticulate::use_virtualenv("r-StreamFind", required = FALSE)
        reticulate::configure_environment(pkgname)
      }
    } else {
      # Python already initialized, just configure environment if needed
      reticulate::configure_environment(pkgname)
    }
  }, error = function(e) {
    message("Python configuration skipped: ", e$message)
  })
}
