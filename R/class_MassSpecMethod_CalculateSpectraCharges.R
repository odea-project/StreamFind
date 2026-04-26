# MARK: MassSpecMethod_CalculateSpectraCharges_native
#' @title MassSpecMethod_CalculateSpectraCharges_native class
#' @description Estimate charge states of ions from MS spectra using isotope spacing.
#' @param roundVal      m/z rounding tolerance for isotope matching.
#' @param relLowCut     Relative intensity cut-off (fraction of base peak).
#' @param absLowCut     Absolute intensity cut-off.
#' @param topCharges    Maximum number of charge states to return per ion.
#' @export
MassSpecMethod_CalculateSpectraCharges_native <- function(
    roundVal   = 0.05,
    relLowCut  = 0.05,
    absLowCut  = 500.0,
    topCharges = 3L) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "CalculateSpectraCharges",
    required    = "LoadSpectra",
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
      roundVal   = as.numeric(roundVal),
      relLowCut  = as.numeric(relLowCut),
      absLowCut  = as.numeric(absLowCut),
      topCharges = as.integer(topCharges)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_CalculateSpectraCharges_native parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_CalculateSpectraCharges_native <- function(x) {
  checkmate::assert_choice(x$method, "CalculateSpectraCharges")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$roundVal,   len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$relLowCut,  len = 1, lower = 0, upper = 1)
  checkmate::assert_numeric(x$parameters$absLowCut,  len = 1, lower = 0)
  checkmate::assert_int(x$parameters$topCharges, lower = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_CalculateSpectraCharges_native <- function(x, engine = NULL) {
  spec_results <- engine$Spectra
  if (is.null(spec_results) || !"MassSpecResults_Spectra" %in% class(spec_results)) {
    warning("Engine does not contain MassSpecResults_Spectra.")
    return(FALSE)
  }
  spectra_dt <- query_db(spec_results, "SELECT * FROM Spectra")
  if (nrow(spectra_dt) == 0) { warning("No spectra found."); return(FALSE) }

  p <- x$parameters
  charges_dt <- rcpp_ms_calculate_spectra_charges(
    as.data.frame(spectra_dt),
    round_val   = p$roundVal,
    rel_low_cut = p$relLowCut,
    abs_low_cut = p$absLowCut,
    top_charges = p$topCharges
  )
  if (nrow(charges_dt) == 0) { warning("No charges calculated."); return(FALSE) }
  .update_spectra_charges(spec_results, data.table::as.data.table(charges_dt))
  invisible(TRUE)
}
