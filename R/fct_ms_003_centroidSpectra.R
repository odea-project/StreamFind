
#' @title centroid_spectra
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
centroid_spectra = function(spectra = list()) {


  res <- rcpp_centroid_spectra(spectra)





  return(res)
}
