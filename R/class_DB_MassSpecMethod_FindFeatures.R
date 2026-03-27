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
#' @param baseQuantile numeric(1) quantile to estimate the baseline in a mass cluster.
#' @param debugAnalysis character(1) analysis name to enable debugging for (empty string to debug all).
#' @param debugMZ numeric(1) m/z value to enable debugging for specific mass traces (0 to disable).
#' @param debugSpecIdx integer(1) spectrum index to enable debugging for denoising (-1 to disable).
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
  baseQuantile = 0.1,
  debugAnalysis = "",
  debugMZ = 0,
  debugSpecIdx = -1
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
      baseQuantile = as.numeric(baseQuantile),
      debugAnalysis = as.character(debugAnalysis),
      debugMZ = as.numeric(debugMZ),
      debugSpecIdx = as.integer(debugSpecIdx)
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
  checkmate::assert_numeric(x$parameters$baseQuantile, len = 1, lower = 0, upper = 1)
  checkmate::assert_character(x$parameters$debugAnalysis, len = 1)
  checkmate::assert_numeric(x$parameters$debugMZ, len = 1, lower = 0)
  checkmate::assert_integerish(x$parameters$debugSpecIdx, len = 1, lower = -1)
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
  headers_split <- split(headers, headers$analysis)
  headers_list <- lapply(analyses$analysis, function(ana) {
    hd <- headers_split[[ana]]
    if (is.null(hd)) headers[0, ] else hd
  })
  names(headers_list) <- analyses$analysis
  parameters <- x$parameters

  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      fts <- load_cache(cache_manager, hash = hash)
      if (!is.null(fts)) {
        if (nrow(fts) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
          DB_MassSpecResults_NonTargetAnalysis(
            projectPath = engine$get_project_path(),
            features = fts
          )
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
    baseQuantile = parameters$baseQuantile,
    debugAnalysis = parameters$debugAnalysis,
    debugMZ = parameters$debugMZ,
    debugSpecIdx = parameters$debugSpecIdx
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
    hash = .make_hash(x, analyses, parameters, engine$Workflow),
    description = "Features found with DB_FindFeatures_native method",
    data = as.data.frame(fts)
  )
  message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  invisible(DB_MassSpecResults_NonTargetAnalysis(
    projectPath = engine$get_project_path(),
    features = fts
  ))
  invisible(TRUE)
}

#' @title Plot debug log for DB_MassSpecMethod_FindFeatures_native
#' @description Plots the debug log generated during the execution of the DB_MassSpecMethod_FindFeatures_native method.
#' @param x An object of class DB_MassSpecMethod_FindFeatures_native.
#' @param logFile Character(1) path to the debug log file.
#' @param plot3D Logical(1) indicating whether to create a 3D plot (default is FALSE).
#' @return A plot visualizing the debug information.
#' @export
#' @noRd
#'
plot_debug_log.DB_MassSpecMethod_FindFeatures_native <- function(x, logFile, plot3D = FALSE) {
  .plot_debug_DB_MassSpecMethod_FindFeatures_native(logFile = logFile, plot3D = plot3D)
}
