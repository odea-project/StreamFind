# MARK: MassSpecMethod_QuantifyChromatographicPeaks_native
#' @title MassSpecMethod_QuantifyChromatographicPeaks_native class
#' @description Quantify chromatographic peaks via OLS calibration using peak area or intensity.
#' @param value  Signal measure to use: "area" (default) or "intensity".
#' @param model  Regression model: "linear" (default), "quadratic", or "cubic".
#' @export
MassSpecMethod_QuantifyChromatographicPeaks_native <- function(
    value = "area",
    model = "linear") {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "QuantifyChromatographicPeaks",
    required    = c("LoadChromatograms", "FindChromPeaks"),
    algorithm   = "native",
    input_class = "MassSpecResults_Chromatograms",
    output_class = "MassSpecResults_Chromatograms",
    number_permitted = 1,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(value = as.character(value), model = as.character(model))
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_QuantifyChromatographicPeaks_native parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_QuantifyChromatographicPeaks_native <- function(x) {
  checkmate::assert_choice(x$method, "QuantifyChromatographicPeaks")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_choice(x$parameters$value, c("area", "intensity"))
  checkmate::assert_choice(x$parameters$model, c("linear", "quadratic", "cubic"))
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_QuantifyChromatographicPeaks_native <- function(x, engine = NULL) {
  chrom_results <- engine$Chromatograms
  if (is.null(chrom_results)) { warning("No Chromatograms results found."); return(FALSE) }
  peaks <- query_db(chrom_results, "SELECT * FROM Peaks")
  if (nrow(peaks) == 0) { warning("No peaks found for quantification."); return(FALSE) }

  # Get concentrations from Analyses (NA for unknowns)
  analyses <- query_db(chrom_results, "SELECT analysis, concentration FROM Analyses")
  conc_vec <- as.numeric(analyses$concentration)
  names(conc_vec) <- analyses$analysis

  quant <- rcpp_ms_quantify_peaks_ols(
    peaks,
    concentrations = conc_vec,
    value          = x$parameters$value,
    model          = x$parameters$model
  )
  .update_chromatograms_peaks(chrom_results, data.table::as.data.table(quant))
  invisible(TRUE)
}
