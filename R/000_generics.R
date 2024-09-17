
# Global generics -----

#' @export
#' @noRd
show <- S7::new_generic("show", "x")

#' @export
#' @noRd
run <- S7::new_generic("run", "x")

#' @export
#' @noRd
save <- S7::new_generic("save", "x")

#' @export
#' @noRd
read <- S7::new_generic("read", "x")

#' @export
#'@noRd
add <- S7::new_generic("add", "x")

#' @export
#'@noRd
remove <- S7::new_generic("remove", "x")

#' @export
#'@noRd
load <- S7::new_generic("load", "x")

# Spectra generics -----

#' @export
#'@noRd
get_spectra_tic <- S7::new_generic("get_spectra_tic", "x")

#' @export
#'@noRd
get_spectra_bpc <- S7::new_generic("get_spectra_bpc", "x")

#' @export
#' @noRd
get_spectra <- S7::new_generic("get_spectra", "x")

#' @export
#' @noRd
get_spectra_eic <- S7::new_generic("get_spectra_eic", "x")

#' @export
#' @noRd
get_spectra_ms1 <- S7::new_generic("get_spectra_ms1", "x")

#' @export
#' @noRd
get_spectra_ms2 <- S7::new_generic("get_spectra_ms2", "x")

#' @export
#' @noRd
get_features_count <- S7::new_generic("get_features_count", "x")

#' @export
#' @noRd
get_features <- S7::new_generic("get_features", "x")

#' @export
#' @noRd
get_features_eic <- S7::new_generic("get_features_eic", "x")

#' @export
#' @noRd
get_features_ms1 <- S7::new_generic("get_features_ms1", "x")

#' @export
#' @noRd
get_features_ms2 <- S7::new_generic("get_features_ms2", "x")

#' @export
#' @noRd
get_groups <- S7::new_generic("get_groups", "x")

#' @export
#' @noRd
get_groups_ms1 <- S7::new_generic("get_groups_ms1", "x")

#' @export
#' @noRd
get_groups_ms2 <- S7::new_generic("get_groups_ms2", "x")

#' @export
#'@noRd
plot_spectra_tic <- S7::new_generic("plot_spectra_tic", "x")

#' @export
#'@noRd
plot_spectra_bpc <- S7::new_generic("plot_spectra_bpc", "x")

#' @export
#' @noRd
plot_spectra <- S7::new_generic("plot_spectra", "x")

#' @export
#' @noRd
plot_spectra_eic <- S7::new_generic("plot_spectra_eic", "x")

#' @export
#' @noRd
plot_spectra_ms1 <- S7::new_generic("plot_spectra_ms1", "x")

#' @export
#' @noRd
plot_spectra_ms2 <- S7::new_generic("plot_spectra_ms2", "x")

#' @export
#' @noRd
plot_features_count <- S7::new_generic("plot_features_count", "x")

#' @export
#' @noRd
plot_features <- S7::new_generic("plot_features", "x")

#' @export
#' @noRd
map_features <- S7::new_generic("map_features", "x")

#' @export
#' @noRd
map_features_intensity <- S7::new_generic("map_features_intensity", "x")

#' @export
#' @noRd
plot_features_eic <- S7::new_generic("plot_features_eic", "x")

#' @export
#' @noRd
plot_features_ms1 <- S7::new_generic("plot_features_ms1", "x")

#' @export
#' @noRd
plot_features_ms2 <- S7::new_generic("plot_features_ms2", "x")

#' @export
#' @noRd
plot_groups <- S7::new_generic("plot_groups", "x")

#' @export
#' @noRd
plot_groups_ms1 <- S7::new_generic("plot_groups_ms1", "x")

#' @export
#' @noRd
plot_groups_ms2 <- S7::new_generic("plot_groups_ms2", "x")

#' @export
#' @noRd
plot_groups_overview <- S7::new_generic("plot_groups_overview", "x")

#' @export
#' @noRd
plot_groups_profile <- S7::new_generic("plot_groups_profile", "x")

# Chromatograms generics -----

#' @export
#' @noRd
get_chromatograms <- S7::new_generic("get_chromatograms", "x")

#' @export
#' @noRd
plot_chromatograms <- S7::new_generic("plot_chromatograms", "x")



















#' @noRd
.process <- function(settings, self, private) {
  UseMethod(".process")
}

#' @title Validate
#' 
#' @description Validates an object.
#' 
#' @param x An object.
#' 
#' @export
#' 
validate <- function(x) {
  UseMethod("validate")
}
