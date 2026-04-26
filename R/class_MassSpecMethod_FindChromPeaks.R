# MARK: MassSpecMethod_FindChromPeaks_LocalMaxima
#' @title MassSpecMethod_FindChromPeaks_LocalMaxima class
#' @description Find chromatographic peaks by local-maxima expansion (DAD / UV chromatograms).
#' @param minHeight Minimum peak height (0 = no filter).
#' @param minWidth  Minimum peak width in rt units (0 = no filter).
#' @param maxWidth  Maximum peak width in rt units (0 = no filter).
#' @export
MassSpecMethod_FindChromPeaks_LocalMaxima <- function(
    minHeight = 0.0,
    minWidth  = 0.0,
    maxWidth  = 0.0) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "FindChromPeaks",
    required    = "LoadChromatograms",
    algorithm   = "LocalMaxima",
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
      minHeight = as.numeric(minHeight),
      minWidth  = as.numeric(minWidth),
      maxWidth  = as.numeric(maxWidth)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_FindChromPeaks_LocalMaxima parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_FindChromPeaks_LocalMaxima <- function(x) {
  checkmate::assert_choice(x$method, "FindChromPeaks")
  checkmate::assert_choice(x$algorithm, "LocalMaxima")
  checkmate::assert_numeric(x$parameters$minHeight, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minWidth,  len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$maxWidth,  len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_FindChromPeaks_LocalMaxima <- function(x, engine = NULL) {
  chrom_results <- engine$Chromatograms
  if (is.null(chrom_results)) { warning("No Chromatograms results found."); return(FALSE) }
  chroms <- query_db(chrom_results, "SELECT * FROM Chromatograms")
  if (nrow(chroms) == 0) { warning("No chromatogram data found."); return(FALSE) }
  p <- x$parameters
  peaks <- rcpp_ms_find_chrom_peaks_local_maxima(
    chroms,
    min_height = p$minHeight,
    min_width  = p$minWidth,
    max_width  = p$maxWidth
  )
  if (nrow(peaks) == 0) {
    message("\U2717 No peaks found by FindChromPeaks_LocalMaxima.")
    return(FALSE)
  }
  .update_chromatograms_peaks(chrom_results, data.table::as.data.table(peaks))
  invisible(TRUE)
}


# MARK: MassSpecMethod_FindChromPeaks_pracma
#' @title MassSpecMethod_FindChromPeaks_pracma class
#' @description Find and integrate chromatographic peaks using a pracma-style algorithm (alias for IntegrateChromatograms_pracma).
#' @inheritParams MassSpecMethod_IntegrateChromatograms_pracma
#' @export
MassSpecMethod_FindChromPeaks_pracma <- function(
    merge             = TRUE,
    closeByThreshold  = 45.0,
    minPeakHeight     = 0.0,
    minPeakDistance   = 10.0,
    minPeakWidth      = 5.0,
    maxPeakWidth      = 120.0,
    minSN             = 10.0) {
  x <- ProcessingStep(
    type        = "MassSpec",
    method      = "FindChromPeaks",
    required    = "LoadChromatograms",
    algorithm   = "pracma",
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
      merge             = as.logical(merge),
      closeByThreshold  = as.numeric(closeByThreshold),
      minPeakHeight     = as.numeric(minPeakHeight),
      minPeakDistance   = as.numeric(minPeakDistance),
      minPeakWidth      = as.numeric(minPeakWidth),
      maxPeakWidth      = as.numeric(maxPeakWidth),
      minSN             = as.numeric(minSN)
    )
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecMethod_FindChromPeaks_pracma parameters.")
}

#' @export
#' @noRd
validate_object.MassSpecMethod_FindChromPeaks_pracma <- function(x) {
  checkmate::assert_choice(x$method, "FindChromPeaks")
  checkmate::assert_choice(x$algorithm, "pracma")
  p <- x$parameters
  checkmate::assert_logical(p$merge, len = 1)
  checkmate::assert_numeric(p$closeByThreshold, len = 1, lower = 0)
  checkmate::assert_numeric(p$minPeakHeight,    len = 1, lower = 0)
  checkmate::assert_numeric(p$minPeakDistance,  len = 1, lower = 0)
  checkmate::assert_numeric(p$minPeakWidth,     len = 1, lower = 0)
  checkmate::assert_numeric(p$maxPeakWidth,     len = 1, lower = 0)
  checkmate::assert_numeric(p$minSN,            len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_FindChromPeaks_pracma <- function(x, engine = NULL) {
  chrom_results <- engine$Chromatograms
  if (is.null(chrom_results)) { warning("No Chromatograms results found."); return(FALSE) }
  chroms <- query_db(chrom_results, "SELECT * FROM Chromatograms")
  if (nrow(chroms) == 0) { warning("No chromatogram data found."); return(FALSE) }
  p <- x$parameters
  peaks <- rcpp_ms_integrate_chromatograms(
    chroms,
    merge              = p$merge,
    close_by_threshold = p$closeByThreshold,
    min_peak_height    = p$minPeakHeight,
    min_peak_distance  = p$minPeakDistance,
    min_peak_width     = p$minPeakWidth,
    max_peak_width     = p$maxPeakWidth,
    min_sn             = p$minSN
  )
  if (nrow(peaks) == 0) {
    message("\U2717 No peaks found by FindChromPeaks_pracma.")
    return(FALSE)
  }
  .update_chromatograms_peaks(chrom_results, data.table::as.data.table(peaks))
  invisible(TRUE)
}
