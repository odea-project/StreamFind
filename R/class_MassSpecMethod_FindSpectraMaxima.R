# MARK: MassSpecMethod_FindSpectraMaxima_native
#' @title MassSpecMethod_FindSpectraMaxima_native class
#' @description Find local intensity maxima in mass spectra (peak picking in the m/z domain).
#' @param minWidth    Minimum peak width in m/z units.
#' @param maxWidth    Maximum peak width in m/z units.
#' @param minHeight   Minimum peak height (absolute intensity).
#' @export
MassSpecMethod_FindSpectraMaxima_native <- function(
    minWidth  = 0.5,
    maxWidth  = 50.0,
    minHeight = 500.0) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "FindSpectraMaxima",
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
      minWidth  = as.numeric(minWidth),
      maxWidth  = as.numeric(maxWidth),
      minHeight = as.numeric(minHeight)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_FindSpectraMaxima_native parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_FindSpectraMaxima_native <- function(x) {
  checkmate::assert_choice(x$method, "FindSpectraMaxima")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$minWidth,  len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$maxWidth,  len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minHeight, len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_FindSpectraMaxima_native <- function(x, engine = NULL) {
  spec_results <- engine$Spectra
  if (is.null(spec_results) || !"MassSpecResults_Spectra" %in% class(spec_results)) {
    warning("Engine does not contain MassSpecResults_Spectra.")
    return(FALSE)
  }
  spectra_dt <- query_db(spec_results, "SELECT * FROM Spectra")
  if (nrow(spectra_dt) == 0) { warning("No spectra found."); return(FALSE) }

  p <- x$parameters
  peaks_dt <- rcpp_ms_find_spectra_maxima(
    as.data.frame(spectra_dt),
    min_height = p$minHeight,
    min_width  = p$minWidth,
    max_width  = p$maxWidth
  )
  if (nrow(peaks_dt) == 0) { warning("No spectra maxima found."); return(FALSE) }
  .update_spectra_peaks(spec_results, data.table::as.data.table(peaks_dt))
  invisible(TRUE)
}
