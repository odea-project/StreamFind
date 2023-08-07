#' @noRd
validate <- function(x) {
  UseMethod("validate")
}

#' @noRd
asJSON <- function(x) {
  UseMethod("asJSON")
}

#' @noRd
export <- function(x) {
  UseMethod("export")
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
