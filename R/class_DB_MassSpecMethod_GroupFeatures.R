# MARK: Native
# Native ------

#' @title DB_MassSpecMethod_GroupFeatures_native class
#' @description Native StreamFind method for grouping features across samples by aligning retention times and matching mass values.
#' @param method Character. Method for retention time alignment: "internal_standards" uses internal standards for RT correction (requires internal standards to be present), "obi_warp" uses Dynamic Time Warping alignment. Default: "internal_standards".
#' @param rtDeviation Numeric. Retention time tolerance in seconds for grouping features. Default: 5.
#' @param ppm Numeric. Mass tolerance in parts-per-million for grouping features. Default: 10.
#' @param minSamples Integer. Minimum number of samples a feature must appear in to be grouped. Default: 1.
#' @param binSize Numeric. RT bin size in seconds for harmonizing RT dimension across analyses.
#' Shifts are calculated per bin instead of per feature. Default: 5.
#' @param filtered Logical. If TRUE, includes filtered features in grouping. Default: FALSE.
#' @param debug Logical. If TRUE, creates a debug log file. Default: FALSE.
#' @param debugRT Numeric. RT value to focus debugging on (logs features within rtDeviation of this value). Default: 0 (disabled).
#' @export
#'
DB_MassSpecMethod_GroupFeatures_native <- function(
  method = "internal_standards",
  rtDeviation = 5,
  ppm = 10,
  minSamples = 1,
  binSize = 5,
  filtered = FALSE,
  debug = FALSE,
  debugRT = 0
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "GroupFeatures",
    required = c("FindFeatures"),
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
      method = as.character(method),
      rtDeviation = as.numeric(rtDeviation),
      ppm = as.numeric(ppm),
      minSamples = as.integer(minSamples),
      binSize = as.numeric(binSize),
      filtered = as.logical(filtered),
      debug = as.logical(debug),
      debugRT = as.numeric(debugRT)
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_GroupFeatures_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_GroupFeatures_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "GroupFeatures")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_choice(x$parameters$method, c("internal_standards", "obi_warp"))
  checkmate::assert_numeric(x$parameters$rtDeviation, len = 1, lower = 0)
  checkmate::assert_numeric(x$parameters$ppm, len = 1, lower = 0)
  checkmate::assert_integerish(x$parameters$minSamples, len = 1, lower = 1)
  checkmate::assert_numeric(x$parameters$binSize, len = 1, lower = 0)
  checkmate::assert_logical(x$parameters$filtered, len = 1)
  checkmate::assert_logical(x$parameters$debug, len = 1)
  checkmate::assert_numeric(x$parameters$debugRT, len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_GroupFeatures_native <- function(x, engine = NULL) {
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
      grouped_features <- load_cache(cache_manager, hash = hash)
      if (!is.null(grouped_features)) {
        if (nrow(grouped_features) > 0) {
          message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")

          # Update Features table with new feature_group assignments using bulk operations
          conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
          on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

          # Create a temporary table with the updates
          temp_table_name <- paste0("temp_feature_groups_", sample.int(1e9, 1))
          DBI::dbWriteTable(conn, temp_table_name, grouped_features, temporary = TRUE)

          # Perform bulk update using JOIN
          update_query <- sprintf("
            UPDATE Features
            SET feature_group = %s.feature_group
            FROM %s
            WHERE Features.analysis = %s.analysis
              AND Features.feature = %s.feature
          ", temp_table_name, temp_table_name, temp_table_name, temp_table_name)

          DBI::dbExecute(conn, update_query)

          # Clean up temporary table
          DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", temp_table_name))

          message("\U2713 Feature groups updated in database (", length(unique(grouped_features[grouped_features$feature_group != "", ]$feature_group)), " groups from cache).")

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
    warning("No features found for grouping.")
    return(FALSE)
  }

  message("\U2139 Processing ", nrow(features), " features across ", length(unique(features$analysis)), " analyses...")

  # Get analyses info and spectra headers
  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  spectra_headers <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")

  # Keep per-analysis lists in the exact analysis order passed to Rcpp.
  headers_split <- split(spectra_headers, spectra_headers$analysis)
  headers_list <- lapply(analyses$analysis, function(ana) {
    hd <- headers_split[[ana]]
    if (is.null(hd)) spectra_headers[0, ] else hd
  })
  names(headers_list) <- analyses$analysis

  features_split <- split(features, features$analysis)
  feature_list <- lapply(analyses$analysis, function(ana) {
    fts <- features_split[[ana]]
    if (is.null(fts)) features[0, ] else fts
  })
  names(feature_list) <- analyses$analysis

  # Get database connection for later updates
  conn <- DBI::dbConnect(duckdb::duckdb(), engine$NonTargetAnalysis$db)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  # Get internal standards if needed
  internal_standards_list <- list()
  if (parameters$method == "internal_standards") {
    internal_standards <- tryCatch(
      {
        get_internal_standards(engine$NonTargetAnalysis)
      },
      error = function(e) {
        NULL
      }
    )

    if (!is.null(internal_standards) && nrow(internal_standards) > 0) {
      # Split by analysis for C++ and align with analyses order.
      istd_split <- split(internal_standards, internal_standards$analysis)
      internal_standards_list <- lapply(analyses$analysis, function(ana) {
        istd <- istd_split[[ana]]
        if (is.null(istd)) internal_standards[0, ] else istd
      })
      names(internal_standards_list) <- analyses$analysis
      message("\U2139 Using ", nrow(internal_standards), " internal standards for alignment.")
    } else {
      warning("Internal standards not found but required for grouping with internal_standards method.")
      return(FALSE)
    }
  }

  # Call C++ function for grouping
  grouped_features_list <- rcpp_nts_group_features_2(
    info = analyses,
    spectra_headers = headers_list,
    feature_list = feature_list,
    method = parameters$method,
    internal_standards_list = internal_standards_list,
    rtDeviation = parameters$rtDeviation,
    ppm = parameters$ppm,
    minSamples = parameters$minSamples,
    binSize = parameters$binSize,
    debug = parameters$debug,
    debugRT = parameters$debugRT
  )

  if (is.null(grouped_features_list) || length(grouped_features_list) == 0) {
    warning("Feature grouping failed.")
    return(FALSE)
  }

  names(grouped_features_list) <- analyses$analysis
  grouped_features <- data.table::rbindlist(grouped_features_list, fill = TRUE, idcol = "analysis")

  # Prepare result for caching (only necessary columns)
  result <- grouped_features[, .(analysis, feature, feature_group)]

  # Cache results
  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_GroupFeatures_native"),
      hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
      description = "Feature groups from DB_GroupFeatures_native method",
      data = as.data.frame(result)
    )
    message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  }

  # Update Features table with new feature_group assignments using bulk operations
  # Create a temporary table with the updates
  temp_table_name <- paste0("temp_feature_groups_", sample.int(1e9, 1))

  DBI::dbWriteTable(conn, temp_table_name, result, temporary = TRUE)

  # Perform bulk update using JOIN
  update_query <- sprintf("
    UPDATE Features
    SET feature_group = %s.feature_group
    FROM %s
    WHERE Features.analysis = %s.analysis
      AND Features.feature = %s.feature
  ", temp_table_name, temp_table_name, temp_table_name, temp_table_name)

  DBI::dbExecute(conn, update_query)

  # Clean up temporary table
  DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", temp_table_name))

  message("\U2713 Feature groups updated in database (", length(unique(result[feature_group != ""]$feature_group)), " groups created).")

  DBI::dbDisconnect(conn, shutdown = TRUE)
  on.exit(NULL)

  invisible(TRUE)
}
