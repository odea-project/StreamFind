#' @noRd
#' @export
validate <- function(x) {
  UseMethod("validate")
}

#' @noRd
#' @export
asJSON <- function(x) {
  UseMethod("asJSON")
}

#' @noRd
#' @export
export <- function(x, ...) {
  UseMethod("export")
}

#' @noRd
.s3_ms_centroid_spectra <- function(settings, self) {
  UseMethod(".s3_ms_centroid_spectra")
}

#' @noRd
.s3_ms_bin_spectra <- function(settings, self) {
  UseMethod(".s3_ms_bin_spectra")
}

#' @noRd
.s3_ms_find_features <- function(settings, self) {
  UseMethod(".s3_ms_find_features")
}

#' @noRd
.s3_ms_annotate_features <- function(settings, self) {
  UseMethod(".s3_ms_annotate_features")
}

#' @noRd
.s3_ms_group_features <- function(settings, self) {
  UseMethod(".s3_ms_group_features")
}

#' @noRd
.s3_ms_filter_features <- function(settings, self) {
  UseMethod(".s3_ms_filter_features")
}

.s3_ms_fill_features <- function(settings, self) {
  UseMethod(".s3_ms_fill_features")
}

.s3_ms_correct_intensity <- function(settings, self) {
  UseMethod(".s3_ms_correct_intensity")
}

.s3_ms_normalise_intensity <- function(settings, self) {
  UseMethod(".s3_ms_normalise_intensity")
}

.s3_ms_suspect_screening <- function(settings, self) {
  UseMethod(".s3_ms_suspect_screening")
}

.s3_ms_find_internal_standards <- function(settings, self) {
  UseMethod(".s3_ms_find_internal_standards")
}

.s3_ms_calculate_quality <- function(settings, self) {
  UseMethod(".s3_ms_calculate_quality")
}

.s3_ms_generate_formulas <- function(settings, self) {
  UseMethod(".s3_ms_generate_formulas")
}

.s3_ms_generate_compounds <- function(settings, self) {
  UseMethod(".s3_ms_generate_compounds")
}
