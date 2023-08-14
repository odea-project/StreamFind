
#' .S3_ms_centroid_spectra.Settings_centroid_spectra_qCentroids
#'
#' @description Centroids profile spectra.
#'
#' @param spectra A `data.frame`, `data.table` or named `list` with at least
#' scan number (scan), retention time (rt), mass-to-charge ratio (mz) and
#' intensity, representing spectra in profile mode.
#'
#' @return `data.table` with centroided spectra.
#'
#' @noRd
#'
.S3_ms_centroid_spectra.Settings_centroid_spectra_qCentroids <- function(settings, self) {

  spectra <- self$get_spectra()

  # TODO Gerrit Implementation qCentroids method

  # continue development with cpp
  centroided <- rcpp_centroid_spectra(spectra)

  print(class(centroided))

  return(centroided)
}
