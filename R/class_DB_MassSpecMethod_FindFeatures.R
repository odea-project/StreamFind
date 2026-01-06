
# MARK: Native
# Native ------

#' @title DB_MassSpecMethod_FindFeatures_native class
#' @description Native StreamFind method for finding features (i.e., chromatographic peaks) in liquid chromatography coupled to high resolution mass spectrometry files.
#' @param rtWindows data.frame with rtmin and rtmax columns for retention time windows for data inclusion.
#' @param ppmThreshold integer(1) maximum allowed mass error in ppm for considering traces in a mass cluster.
#' @param minSNR numeric(1) minimum signal-to-noise ratio for considering a trace and chromatographic peak.
#' @param noiseThreshold numeric(1) lowest threshold to clean data (i.e., no trace with intensity below this level is kept).
#' @param minTraces numeric(1) minimum number of traces to consider a mass cluster and chromatographic peak.
#' @param baselineWindow numeric(1) retention time window to build a baseline in a mass cluster.
#' @param maxWidth numeric(1) expected maximum window for a chromatographic peak.
#' @param base_quantile numeric(1) quantile to estimate the baseline in a mass cluster.
#' @export
#'
DB_MassSpecMethod_FindFeatures_native <- function(
  rtWindows = data.frame(rtmin = numeric(), rtmax = numeric()),
  ppmThreshold = 15,
  noiseThreshold = 250,
  minSNR = 3,
  minTraces = 3,
  baselineWindow = 200,
  maxWidth = 100,
  base_quantile = 0.1,
  debug_mz = 0
) {
  rtWindows <- data.table::data.table(
    rtmin = as.numeric(rtWindows$rtmin),
    rtmax = as.numeric(rtWindows$rtmax)
  )
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "FindFeatures",
    required = NA_character_,
    algorithm = "native",
    input_class = "DB_MassSpecAnalyses",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_,
    parameters = list(
      rtWindows = rtWindows,
      ppmThreshold = as.numeric(ppmThreshold),
      noiseThreshold = as.numeric(noiseThreshold),
      minSNR = as.numeric(minSNR),
      minTraces = as.numeric(minTraces),
      baselineWindow = as.numeric(baselineWindow),
      maxWidth = as.numeric(maxWidth),
      base_quantile = as.numeric(base_quantile),
      debug_mz = as.numeric(debug_mz)
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_FindFeatures_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_FindFeatures_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "FindFeatures")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_data_frame(data.table::as.data.table(x$parameters$rtWindows))
  checkmate::assert_true(all(c("rtmin", "rtmax") %in% colnames(data.table::as.data.table(x$parameters$rtWindows))))
  checkmate::assert_numeric(x$parameters$rtWindows$rtmin)
  checkmate::assert_numeric(x$parameters$rtWindows$rtmax)
  checkmate::assert_numeric(x$parameters$ppmThreshold, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$noiseThreshold, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minSNR, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minTraces, len = 1, lower = 1)
  checkmate::assert_numeric(x$parameters$baselineWindow, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$maxWidth, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$base_quantile, len = 1, lower = 0, upper = 1)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_FindFeatures_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecAnalyses" %in% class(engine$Analyses)) {
    warning("Engine does not contain DB_MassSpecAnalyses.")
    return(FALSE)
  }

  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")

  if (nrow(analyses) == 0) {
    warning("No analyses found in the DB_MassSpecAnalyses.")
    return(FALSE)
  }

  headers <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")
  headers_list <- split(headers, headers$analysis)
  parameters <- x$parameters

  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses, parameters)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      fts <- load_cache(cache_manager, hash = hash)
      if (!is.null(fts)) {
        if (nrow(fts) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
          db <- file.path(engine$get_project_path(), "DB_MassSpecResults_NonTargetAnalysis.duckdb")
          DB_MassSpecResults_NonTargetAnalysis(db, analyses, headers, fts)
          return(invisible(TRUE))
        }
      }
    }
  }

  fts <- rcpp_nts_find_features2(
    info = analyses,
    spectra_headers = headers_list,
    rtWindowsMin = parameters$rtWindows$rtmin,
    rtWindowsMax = parameters$rtWindows$rtmax,
    ppmThreshold = parameters$ppmThreshold,
    noiseThreshold = parameters$noiseThreshold,
    minSNR = parameters$minSNR,
    minTraces = parameters$minTraces,
    baselineWindow = parameters$baselineWindow,
    maxWidth = parameters$maxWidth,
    base_quantile = parameters$base_quantile,
    debug_mz = parameters$debug_mz
  )

  if (is.null(fts) || length(fts) == 0) {
    warning("No features found.")
    return(FALSE)
  }

  names(fts) <- analyses$analysis
  fts <- data.table::rbindlist(fts, fill = TRUE, idcol = "analysis")

  save_cache(
    cache_manager,
    name = paste0("DB_FindFeatures_native"),
    hash = .make_hash(x, analyses, parameters),
    description = "Features found with DB_FindFeatures_native method",
    data = as.data.frame(fts)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  db <- file.path(engine$get_project_path(), "DB_MassSpecResults_NonTargetAnalysis.duckdb")
  invisible(DB_MassSpecResults_NonTargetAnalysis(db, analyses, headers, fts))
  invisible(TRUE)
}
