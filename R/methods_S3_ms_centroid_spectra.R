
#' .S3_ms_centroid_spectra.Settings_centroid_spectra_qCentroids
#'
#' @description Centroids profile spectra.
#'
#' @noRd
#'
.s3_ms_centroid_spectra.Settings_centroid_spectra_qCentroids <- function(settings, self) {

  message("Centroiding spectra with qCentroids...", appendLF = TRUE)

  if (!any(self$has_loaded_spectra())) self$load_spectra()

  spectra <- self$get_spectra()

  # TODO Gerrit Implementation qCentroids method

  # continue development with cpp
  centroided_spectra <- rcpp_centroid_spectra(spectra)

  # self$add_spectra(centroided_spectra, replace = TRUE)

  FALSE
}
