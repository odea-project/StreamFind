#' @title DB_MassSpecMethod_FillFeatures_native class
#' @description Native StreamFind method for filling missing features across samples by identifying gaps in feature groups and performing recursive integration.
#' @param withinReplicate Logical. If TRUE, fills features only within replicates. Default: FALSE.
#' @param filtered Logical. If TRUE, considers filtered features for gap filling. Default: FALSE.
#' @param rtExpand Numeric. Retention time window expansion in seconds for EIC extraction. Default: 10.
#' @param mzExpand Numeric. m/z window expansion for EIC extraction. Default: 0.01.
#' @param maxPeakWidth Numeric. Maximum peak width in seconds for building feature extraction targets. Default: 30.
#' @param minTracesIntensity Numeric. Minimum intensity to collect spectra data for extracted ion chromatograms. Default: 1000.
#' @param minNumberTraces Integer. Minimum number of traces to consider a feature. Default: 5.
#' @param minIntensity Numeric. Minimum intensity to consider a feature (maximum intensity of EIC). Default: 5000.
#' @param rtApexDeviation Numeric. Maximum RT deviation in seconds from target RT for apex location. Default: 5.
#' @param minSignalToNoiseRatio Numeric. Minimum signal to noise ratio to consider a feature. Default: 3.
#' @param minGaussianFit Numeric. Minimum Gaussian fit (R²) to consider a feature. Default: 0.2.
#' @param debugFG Character specifying feature group ID to debug (empty string to disable). Defaults to "".
#' @export
#'
DB_MassSpecMethod_FillFeatures_native <- function(
  withinReplicate = FALSE,
  filtered = FALSE,
  rtExpand = 10,
  mzExpand = 0.01,
  maxPeakWidth = 30,
  minTracesIntensity = 1000,
  minNumberTraces = 5,
  minIntensity = 5000,
  rtApexDeviation = 5,
  minSignalToNoiseRatio = 3,
  minGaussianFit = 0.2,
  debugFG = ""
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "FillFeatures",
    required = c("FindFeatures", "GroupFeatures"),
    algorithm = "native",
    input_class = "DB_MassSpecResults_NonTargetAnalysis",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_,
    parameters = list(
      withinReplicate = as.logical(withinReplicate),
      filtered = as.logical(filtered),
      rtExpand = as.numeric(rtExpand),
      mzExpand = as.numeric(mzExpand),
      maxPeakWidth = as.numeric(maxPeakWidth),
      minTracesIntensity = as.numeric(minTracesIntensity),
      minNumberTraces = as.integer(minNumberTraces),
      minIntensity = as.numeric(minIntensity),
      rtApexDeviation = as.numeric(rtApexDeviation),
      minSignalToNoiseRatio = as.numeric(minSignalToNoiseRatio),
      minGaussianFit = as.numeric(minGaussianFit),
      debugFG = as.character(debugFG)
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_FillFeatures_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_FillFeatures_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "FillFeatures")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_logical(x$parameters$withinReplicate, len = 1)
  checkmate::assert_logical(x$parameters$filtered, len = 1)
  checkmate::assert_numeric(x$parameters$rtExpand, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$mzExpand, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$maxPeakWidth, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minTracesIntensity, len = 1, lower = 0)
  checkmate::assert_integerish(x$parameters$minNumberTraces, len = 1, lower = 1)
  checkmate::assert_numeric(x$parameters$minIntensity, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$rtApexDeviation, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minSignalToNoiseRatio, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$minGaussianFit, len = 1, lower = 0, upper = 1)
  checkmate::assert_character(x$parameters$debugFG, len = 1, null.ok = FALSE)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_FillFeatures_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  analyses_info <- info(engine$Analyses)
  parameters <- x$parameters

  # Check if cache exists
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses_info, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      filled_features <- load_cache(cache_manager, hash = hash)
      if (!is.null(filled_features)) {
        if (nrow(filled_features) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")

          # Insert filled features into database
          conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
          on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

          # Insert filled features
          DBI::dbWriteTable(conn, "Features", filled_features, append = TRUE)

          message("\U2713 Filled features inserted into database (", nrow(filled_features), " features from cache).")

          DBI::dbDisconnect(conn, shutdown = TRUE)
          on.exit(NULL)

          return(invisible(TRUE))
        }
      }
    }
  }

  # Get features
  features <- get_features(
    x = engine$NonTargetAnalysis,
    analyses = NULL,
    filtered = parameters$filtered
  )

  if (nrow(features) == 0) {
    warning("No features found for gap filling.")
    return(FALSE)
  }

  # Check if feature groups exist
  if (all(features$feature_group == "" | is.na(features$feature_group))) {
    warning("No feature groups found. Run GroupFeatures first.")
    return(FALSE)
  }

  message("\U2139 Processing ", nrow(features), " features across ", length(unique(features$analysis)), " analyses for gap filling...")

  # Get analyses info and spectra headers
  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  spectra_headers <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")

  # Keep headers in the exact same analysis order used for info/feature_list.
  headers_split <- split(spectra_headers, spectra_headers$analysis)
  headers_list <- lapply(analyses$analysis, function(ana) {
    hd <- headers_split[[ana]]
    if (is.null(hd)) spectra_headers[0, ] else hd
  })
  names(headers_list) <- analyses$analysis

  # Convert features to list format expected by C++
  # Ensure all analyses are represented, even if they have no features
  feature_list <- lapply(analyses$analysis, function(ana) {
    ana_features <- features[features$analysis == ana, ]
    if (nrow(ana_features) == 0) {
      # Return empty data frame with correct columns
      return(features[0, ])
    }
    ana_features
  })
  names(feature_list) <- analyses$analysis

  # Get database connection for later updates
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  # Call C++ function for gap filling
  filled_features_list <- rcpp_nts_fill_features(
    info = analyses,
    spectra_headers = headers_list,
    feature_list = feature_list,
    withinReplicate = parameters$withinReplicate,
    filtered = parameters$filtered,
    rtExpand = parameters$rtExpand,
    mzExpand = parameters$mzExpand,
    maxPeakWidth = parameters$maxPeakWidth,
    minTracesIntensity = parameters$minTracesIntensity,
    minNumberTraces = parameters$minNumberTraces,
    minIntensity = parameters$minIntensity,
    rtApexDeviation = parameters$rtApexDeviation,
    minSignalToNoiseRatio = parameters$minSignalToNoiseRatio,
    minGaussianFit = parameters$minGaussianFit,
    debugFG = parameters$debugFG
  )

  if (is.null(filled_features_list) || length(filled_features_list) == 0) {
    warning("Feature gap filling failed.")
    DBI::dbDisconnect(conn, shutdown = TRUE)
    on.exit(NULL)
    return(FALSE)
  }

  names(filled_features_list) <- analyses$analysis
  filled_features <- data.table::rbindlist(filled_features_list, fill = TRUE, idcol = "analysis")

  # Filter only filled features (those newly added)
  filled_features <- data.table::as.data.table(filled_features)
  filled_features_new <- filled_features[filled_features$filled == TRUE, ]

  if (nrow(filled_features_new) == 0) {
    message("\U2139 No gaps were filled.")
    DBI::dbDisconnect(conn, shutdown = TRUE)
    on.exit(NULL)
    return(TRUE)
  }

  # Insert filled features into database
  DBI::dbWriteTable(conn, "Features", as.data.frame(filled_features_new), append = TRUE)

  message("\U2713 Gap filling complete. ", nrow(filled_features_new), " features filled.")

  DBI::dbDisconnect(conn, shutdown = TRUE)
  on.exit(NULL)

  # Prepare result for caching
  result <- filled_features_new

  # Cache results
  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_FillFeatures_native"),
      hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
      description = "Filled features from DB_FillFeatures_native method",
      data = as.data.frame(result)
    )
    message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  }

  invisible(TRUE)
}
