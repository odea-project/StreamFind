# MARK: MassSpecMethod_DeconvoluteSpectra_native
#' @title MassSpecMethod_DeconvoluteSpectra_native class
#' @description Deconvolute charge-state envelopes into neutral masses.
#' @param clustVal  m/z clustering tolerance for grouping charge series.
#' @param window    Mass window for merging redundant deconvoluted masses.
#' @export
MassSpecMethod_DeconvoluteSpectra_native <- function(clustVal = 0.001, window = 1.5) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "DeconvoluteSpectra",
    required    = c("LoadSpectra", "CalculateSpectraCharges"),
    algorithm   = "native",
    input_class = "MassSpecResults_Spectra",
    output_class = "MassSpecResults_Spectra",
    number_permitted = 1,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(
      clustVal = as.numeric(clustVal),
      window   = as.numeric(window)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_DeconvoluteSpectra_native parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_DeconvoluteSpectra_native <- function(x) {
  checkmate::assert_choice(x$method, "DeconvoluteSpectra")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$clustVal, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$window,   len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_DeconvoluteSpectra_native <- function(x, engine = NULL) {
  spec_results <- engine$Spectra
  if (is.null(spec_results) || !"MassSpecResults_Spectra" %in% class(spec_results)) {
    warning("Engine does not contain MassSpecResults_Spectra.")
    return(FALSE)
  }
  spectra_dt <- query_db(spec_results, "SELECT * FROM Spectra")
  if (nrow(spectra_dt) == 0) { warning("No spectra found."); return(FALSE) }
  charges_dt <- query_db(spec_results, "SELECT * FROM SpectraCharges")
  if (nrow(charges_dt) == 0) { warning("No spectra charges found. Run CalculateSpectraCharges first."); return(FALSE) }

  p <- x$parameters
  peaks_dt <- rcpp_ms_deconvolute_spectra(
    as.data.frame(spectra_dt),
    as.data.frame(charges_dt),
    clust_val = p$clustVal,
    window    = p$window
  )
  if (nrow(peaks_dt) == 0) { warning("Deconvolution returned no peaks."); return(FALSE) }
  .update_spectra_peaks(spec_results, data.table::as.data.table(peaks_dt))
  invisible(TRUE)
}
