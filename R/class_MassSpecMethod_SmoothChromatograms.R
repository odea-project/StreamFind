# MARK: MassSpecMethod_SmoothChromatograms_movingaverage
#' @title MassSpecMethod_SmoothChromatograms_movingaverage class
#' @description Smooth chromatogram traces with a symmetric moving-average filter.
#' @param windowSize Odd integer; full filter length (e.g. 5 averages ±2 neighbours).
#' @export
MassSpecMethod_SmoothChromatograms_movingaverage <- function(windowSize = 5L) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "SmoothChromatograms",
    required    = "LoadChromatograms",
    algorithm   = "movingaverage",
    input_class = "MassSpecResults_Chromatograms",
    output_class = "MassSpecResults_Chromatograms",
    number_permitted = 1,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(windowSize = as.integer(windowSize))
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_SmoothChromatograms_movingaverage parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_SmoothChromatograms_movingaverage <- function(x) {
  checkmate::assert_choice(x$method, "SmoothChromatograms")
  checkmate::assert_choice(x$algorithm, "movingaverage")
  checkmate::assert_integerish(x$parameters$windowSize, len = 1, lower = 3)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SmoothChromatograms_movingaverage <- function(x, engine = NULL) {
  chrom_results <- engine$Chromatograms
  if (is.null(chrom_results)) { warning("No Chromatograms results found."); return(FALSE) }
  chroms <- query_db(chrom_results, "SELECT * FROM Chromatograms")
  if (nrow(chroms) == 0) { warning("No chromatogram data found."); return(FALSE) }
  smoothed <- rcpp_ms_smooth_chromatograms(
    chroms,
    method      = "movingaverage",
    window_size = x$parameters$windowSize
  )
  .update_chromatograms_intensity(chrom_results, data.table::as.data.table(smoothed))
  invisible(TRUE)
}


# MARK: MassSpecMethod_SmoothChromatograms_savgol
#' @title MassSpecMethod_SmoothChromatograms_savgol class
#' @description Smooth chromatogram traces with a Savitzky-Golay filter.
#' @param fl       Filter length (must be odd).
#' @param forder   Polynomial order.
#' @param dorder   Derivative order (0 = smooth, 1 = 1st derivative).
#' @export
MassSpecMethod_SmoothChromatograms_savgol <- function(fl = 11L, forder = 4L, dorder = 0L) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "SmoothChromatograms",
    required    = "LoadChromatograms",
    algorithm   = "savgol",
    input_class = "MassSpecResults_Chromatograms",
    output_class = "MassSpecResults_Chromatograms",
    number_permitted = 1,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(fl = as.integer(fl), forder = as.integer(forder), dorder = as.integer(dorder))
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_SmoothChromatograms_savgol parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_SmoothChromatograms_savgol <- function(x) {
  checkmate::assert_choice(x$method, "SmoothChromatograms")
  checkmate::assert_choice(x$algorithm, "savgol")
  checkmate::assert_integerish(x$parameters$fl,     len = 1, lower = 3)
  checkmate::assert_integerish(x$parameters$forder, len = 1, lower = 1)
  checkmate::assert_integerish(x$parameters$dorder, len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SmoothChromatograms_savgol <- function(x, engine = NULL) {
  chrom_results <- engine$Chromatograms
  if (is.null(chrom_results)) { warning("No Chromatograms results found."); return(FALSE) }
  chroms <- query_db(chrom_results, "SELECT * FROM Chromatograms")
  if (nrow(chroms) == 0) { warning("No chromatogram data found."); return(FALSE) }
  smoothed <- rcpp_ms_smooth_chromatograms(
    chroms,
    method  = "savgol",
    fl      = x$parameters$fl,
    forder  = x$parameters$forder,
    dorder  = x$parameters$dorder
  )
  .update_chromatograms_intensity(chrom_results, data.table::as.data.table(smoothed))
  invisible(TRUE)
}
