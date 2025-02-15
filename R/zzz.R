.onLoad <- function(libname, pkgname) {
  
  S7::methods_register()
  
  if (!reticulate::virtualenv_exists("r-StreamFind")) {
    reticulate::virtualenv_create("r-StreamFind")
  }
  reticulate::use_virtualenv("r-StreamFind")
  reticulate::configure_environment(pkgname)
}
