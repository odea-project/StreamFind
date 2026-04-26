# MARK: MassSpecMethod_CorrectChromatogramsBaseline_airpls
#' @title MassSpecMethod_CorrectChromatogramsBaseline_airpls class
#' @description Estimate and subtract chromatogram baselines using the airPLS algorithm (Zhang et al. 2010).
#' @param lambda       Smoothing penalty (larger = smoother baseline).
#' @param differences  Order of the finite-difference penalty (1 or 2).
#' @param itermax      Maximum number of iterations.
#' @export
MassSpecMethod_CorrectChromatogramsBaseline_airpls <- function(
    lambda      = 10.0,
    differences = 1L,
    itermax     = 20L) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "CorrectChromatogramsBaseline",
    required    = "LoadChromatograms",
    algorithm   = "airpls",
    input_class = "MassSpecResults_Chromatograms",
    output_class = "MassSpecResults_Chromatograms",
    number_permitted = 1,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = "10.1039/b922045c",
    parameters  = list(
      lambda      = as.numeric(lambda),
      differences = as.integer(differences),
      itermax     = as.integer(itermax)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_CorrectChromatogramsBaseline_airpls parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_CorrectChromatogramsBaseline_airpls <- function(x) {
  checkmate::assert_choice(x$method, "CorrectChromatogramsBaseline")
  checkmate::assert_choice(x$algorithm, "airpls")
  checkmate::assert_numeric(x$parameters$lambda,      len = 1, lower = 0)
  checkmate::assert_integerish(x$parameters$differences, len = 1, lower = 1, upper = 5)
  checkmate::assert_integerish(x$parameters$itermax,  len = 1, lower = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_CorrectChromatogramsBaseline_airpls <- function(x, engine = NULL) {
  chrom_results <- engine$Chromatograms
  if (is.null(chrom_results)) { warning("No Chromatograms results found."); return(FALSE) }
  chroms <- query_db(chrom_results, "SELECT * FROM Chromatograms")
  if (nrow(chroms) == 0) { warning("No chromatogram data found."); return(FALSE) }
  corrected <- rcpp_ms_correct_baseline_airpls(
    chroms,
    lambda      = x$parameters$lambda,
    differences = x$parameters$differences,
    itermax     = x$parameters$itermax
  )
  .update_chromatograms_intensity(chrom_results, data.table::as.data.table(corrected))
  invisible(TRUE)
}


# MARK: MassSpecMethod_CorrectChromatogramsBaseline_baseline_als
#' @title MassSpecMethod_CorrectChromatogramsBaseline_baseline_als class
#' @description Estimate and subtract chromatogram baselines using the Asymmetric Least Squares (ALS) algorithm (Eilers & Boelens 2005).
#' @param lambda  Smoothing penalty.
#' @param p       Asymmetry parameter (0 < p < 0.5; typically 0.001–0.01).
#' @param maxit   Maximum number of iterations.
#' @export
MassSpecMethod_CorrectChromatogramsBaseline_baseline_als <- function(
    lambda = 1e5,
    p      = 0.001,
    maxit  = 10L) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "CorrectChromatogramsBaseline",
    required    = "LoadChromatograms",
    algorithm   = "baseline_als",
    input_class = "MassSpecResults_Chromatograms",
    output_class = "MassSpecResults_Chromatograms",
    number_permitted = 1,
    version     = as.character(packageVersion("StreamFind")),
    software    = "StreamFind",
    developer   = "Ricardo Cunha",
    contact     = "cunha@iuta.de",
    link        = "https://odea-project.github.io/StreamFind",
    doi         = NA_character_,
    parameters  = list(
      lambda = as.numeric(lambda),
      p      = as.numeric(p),
      maxit  = as.integer(maxit)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_CorrectChromatogramsBaseline_baseline_als parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_CorrectChromatogramsBaseline_baseline_als <- function(x) {
  checkmate::assert_choice(x$method, "CorrectChromatogramsBaseline")
  checkmate::assert_choice(x$algorithm, "baseline_als")
  checkmate::assert_numeric(x$parameters$lambda, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$p,      len = 1, lower = 0, upper = 1)
  checkmate::assert_integerish(x$parameters$maxit, len = 1, lower = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_CorrectChromatogramsBaseline_baseline_als <- function(x, engine = NULL) {
  chrom_results <- engine$Chromatograms
  if (is.null(chrom_results)) { warning("No Chromatograms results found."); return(FALSE) }
  chroms <- query_db(chrom_results, "SELECT * FROM Chromatograms")
  if (nrow(chroms) == 0) { warning("No chromatogram data found."); return(FALSE) }
  corrected <- rcpp_ms_correct_baseline_als(
    chroms,
    lambda = x$parameters$lambda,
    p      = x$parameters$p,
    maxit  = x$parameters$maxit
  )
  .update_chromatograms_intensity(chrom_results, data.table::as.data.table(corrected))
  invisible(TRUE)
}
