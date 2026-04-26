# MARK: MassSpecMethod_SmoothSpectra_movingaverage
#' @title MassSpecMethod_SmoothSpectra_movingaverage class
#' @description Smooth spectra intensity using a moving-average filter.
#' @param windowSize  Half-window size (odd integer >= 3).
#' @export
MassSpecMethod_SmoothSpectra_movingaverage <- function(windowSize = 5L) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "SmoothSpectra",
    required    = "LoadSpectra",
    algorithm   = "movingaverage",
    input_class = "MassSpecResults_Spectra",
    output_class = "MassSpecResults_Spectra",
    number_permitted = Inf,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(windowSize = as.integer(windowSize))
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_SmoothSpectra_movingaverage parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_SmoothSpectra_movingaverage <- function(x) {
  checkmate::assert_choice(x$method, "SmoothSpectra")
  checkmate::assert_choice(x$algorithm, "movingaverage")
  checkmate::assert_int(x$parameters$windowSize, lower = 3)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SmoothSpectra_movingaverage <- function(x, engine = NULL) {
  spec_results <- engine$Spectra
  if (is.null(spec_results) || !"MassSpecResults_Spectra" %in% class(spec_results)) {
    warning("Engine does not contain MassSpecResults_Spectra.")
    return(FALSE)
  }
  spectra_dt <- query_db(spec_results, "SELECT * FROM Spectra")
  if (nrow(spectra_dt) == 0) { warning("No spectra found."); return(FALSE) }
  # Reuse rcpp chromatogram smoother: treat mz as rt axis, intensity as intensity
  smoothed <- rcpp_ms_smooth_chromatograms(
    as.data.frame(spectra_dt),
    method      = "movingaverage",
    window_size = x$parameters$windowSize
  )
  .update_spectra(spec_results, smoothed)
  invisible(TRUE)
}


# MARK: MassSpecMethod_SmoothSpectra_savgol
#' @title MassSpecMethod_SmoothSpectra_savgol class
#' @description Smooth spectra intensity using a Savitzky-Golay filter.
#' @param fl      Filter length (odd integer >= 5).
#' @param forder  Polynomial order.
#' @param dorder  Derivative order (0 = smoothing only).
#' @export
MassSpecMethod_SmoothSpectra_savgol <- function(fl = 5L, forder = 2L, dorder = 0L) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "SmoothSpectra",
    required    = "LoadSpectra",
    algorithm   = "savgol",
    input_class = "MassSpecResults_Spectra",
    output_class = "MassSpecResults_Spectra",
    number_permitted = Inf,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(
      fl     = as.integer(fl),
      forder = as.integer(forder),
      dorder = as.integer(dorder)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_SmoothSpectra_savgol parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_SmoothSpectra_savgol <- function(x) {
  checkmate::assert_choice(x$method, "SmoothSpectra")
  checkmate::assert_choice(x$algorithm, "savgol")
  checkmate::assert_int(x$parameters$fl, lower = 5)
  checkmate::assert_int(x$parameters$forder, lower = 1)
  checkmate::assert_int(x$parameters$dorder, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SmoothSpectra_savgol <- function(x, engine = NULL) {
  spec_results <- engine$Spectra
  if (is.null(spec_results) || !"MassSpecResults_Spectra" %in% class(spec_results)) {
    warning("Engine does not contain MassSpecResults_Spectra.")
    return(FALSE)
  }
  spectra_dt <- query_db(spec_results, "SELECT * FROM Spectra")
  if (nrow(spectra_dt) == 0) { warning("No spectra found."); return(FALSE) }
  smoothed <- rcpp_ms_smooth_chromatograms(
    as.data.frame(spectra_dt),
    method  = "savgol",
    fl      = x$parameters$fl,
    forder  = x$parameters$forder,
    dorder  = x$parameters$dorder
  )
  .update_spectra(spec_results, smoothed)
  invisible(TRUE)
}
