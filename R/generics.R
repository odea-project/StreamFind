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

## Processing Methods -----

#' @noRd
.s3_average_spectra <- function(settings, self, private) {
  UseMethod(".s3_average_spectra")
}

#' @noRd
.s3_bin_spectra <- function(settings, self, private) {
  UseMethod(".s3_bin_spectra")
}

#' @noRd
.s3_cluster_spectra <- function(settings, self, private) {
  UseMethod(".s3_cluster_spectra")
}

#' @noRd
.s3_correct_chromatograms_baseline <- function(settings, self, private) {
  UseMethod(".s3_correct_chromatograms_baseline")
}

#' @noRd
.s3_correct_spectra_baseline <- function(settings, self, private) {
  UseMethod(".s3_correct_spectra_baseline")
}

#' @noRd
.s3_delete_spectra_section <- function(settings, self, private) {
  UseMethod(".s3_delete_spectra_section")
}

#' @noRd
.s3_integrate_chromatograms <- function(settings, self, private) {
  UseMethod(".s3_integrate_chromatograms")
}

#' @noRd
.s3_make_pca_model <- function(settings, self, private) {
  UseMethod(".s3_make_pca_model")
}

#' @noRd
.s3_merge_spectra_time_series <- function(settings, self, private) {
  UseMethod(".s3_merge_spectra_time_series")
}

#' @noRd
.s3_ms_annotate_features <- function(settings, self, private) {
  UseMethod(".s3_ms_annotate_features")
}

#' @noRd
.s3_ms_calculate_quality <- function(settings, self, private) {
  UseMethod(".s3_ms_calculate_quality")
}

#' @noRd
.s3_ms_calculate_spectra_charges <- function(settings, self, private) {
  UseMethod(".s3_ms_calculate_spectra_charges")
}

#' @noRd
.s3_ms_centroid_spectra <- function(settings, self, private) {
  UseMethod(".s3_ms_centroid_spectra")
}

#' @noRd
.s3_ms_deconvolute_spectra <- function(settings, self, private) {
  UseMethod(".s3_ms_deconvolute_spectra")
}

#' @noRd
.s3_ms_filter_features <- function(settings, self, private) {
  UseMethod(".s3_ms_filter_features")
}

#' @noRd
.s3_ms_find_features <- function(settings, self, private) {
  UseMethod(".s3_ms_find_features")
}

#' @noRd
.s3_ms_find_internal_standards <- function(settings, self, private) {
  UseMethod(".s3_ms_find_internal_standards")
}

.s3_ms_generate_compounds <- function(settings, self, private) {
  UseMethod(".s3_ms_generate_compounds")
}

.s3_ms_generate_formulas <- function(settings, self, private) {
  UseMethod(".s3_ms_generate_formulas")
}

#' @noRd
.s3_ms_group_features <- function(settings, self, private) {
  UseMethod(".s3_ms_group_features")
}

#' @noRd
.s3_ms_load_features_eic <- function(settings, self, private) {
  UseMethod(".s3_ms_load_features_eic")
}

#' @noRd
.s3_ms_load_features_ms1 <- function(settings, self, private) {
  UseMethod(".s3_ms_load_features_ms1")
}

#' @noRd
.s3_ms_load_features_ms2 <- function(settings, self, private) {
  UseMethod(".s3_ms_load_features_ms2")
}

#' @noRd
.s3_ms_load_MSPeakLists <- function(settings, self, private) {
  UseMethod(".s3_ms_load_MSPeakLists")
}

#' @noRd
.s3_ms_suspect_screening <- function(settings, self, private) {
  UseMethod(".s3_ms_suspect_screening")
}

#' @noRd
.s3_normalize_spectra <- function(settings, self, private) {
  UseMethod(".s3_normalize_spectra")
}

#' @noRd
.s3_smooth_chromatograms <- function(settings, self, private) {
  UseMethod(".s3_smooth_chromatograms")
}

#' @noRd
.s3_smooth_spectra <- function(settings, self, private) {
  UseMethod(".s3_smooth_spectra")
}

#' @noRd
.s3_subtract_blank_spectra <- function(settings, self, private) {
  UseMethod(".s3_subtract_blank_spectra")
}

#' @noRd
.s3_subtract_spectra_section <- function(settings, self, private) {
  UseMethod(".s3_subtract_spectra_section")
}

## Nor Used Yet -----

.s3_ms_fill_features <- function(settings, self, private) {
  UseMethod(".s3_ms_fill_features")
}

.s3_ms_normalise_intensity <- function(settings, self, private) {
  UseMethod(".s3_ms_normalise_intensity")
}

#' @noRd
.s3_ms_correct_intensity <- function(settings, self, private) {
  UseMethod(".s3_ms_correct_intensity")
}
