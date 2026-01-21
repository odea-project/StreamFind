# MARK: Native
# Native ------

#' @title DB_MassSpecMethod_LoadFeaturesMS1_native class
#' @description Native StreamFind method for loading MS1 traces into features stored in a database.
#' @param rtWindow Numeric length-2 vector of retention time offsets (seconds) to expand left/right feature windows. Note that left offset should be negative.
#' @param mzWindow Numeric length-2 vector of m/z offsets (Da) to expand left/right feature windows. Note that left offset should be negative.
#' @param mzClust Numeric(1) m/z tolerance used when clustering traces.
#' @param presence Numeric(1) minimum fraction (0-1) of scans required to keep a cluster.
#' @param minIntensity Numeric(1) minimum trace intensity to extract.
#' @param filtered Logical(1) whether to include features already marked as filtered.
#' @export
#'
DB_MassSpecMethod_LoadFeaturesMS1_native <- function(
  rtWindow = c(-2, 2),
  mzWindow = c(-1, 6),
  mzClust = 0.005,
  presence = 0.8,
  minIntensity = 250,
  filtered = FALSE
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "LoadFeaturesMS1",
    required = "FindFeatures",
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
      rtWindow = rtWindow,
      mzWindow = mzWindow,
      mzClust = mzClust,
      presence = presence,
      minIntensity = minIntensity,
      filtered = filtered
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_LoadFeaturesMS1_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_LoadFeaturesMS1_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "LoadFeaturesMS1")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_double(as.numeric(x$parameters$rtWindow), max.len = 2)
  checkmate::assert_double(as.numeric(x$parameters$mzWindow), max.len = 2)
  checkmate::assert_number(x$parameters$mzClust)
  checkmate::assert_number(x$parameters$presence)
  checkmate::assert_number(x$parameters$minIntensity)
  checkmate::assert_logical(x$parameters$filtered, max.len = 1)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_LoadFeaturesMS1_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecAnalyses" %in% class(engine$Analyses)) {
    warning("Engine does not contain DB_MassSpecAnalyses.")
    return(FALSE)
  }

  db <- file.path(engine$get_project_path(), "DB_MassSpecResults_NonTargetAnalysis.duckdb")
  if (!file.exists(db)) {
    warning("DB_MassSpecResults_NonTargetAnalysis database not found. Run FindFeatures first.")
    return(FALSE)
  }

  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  headers <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")

  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  features <- DBI::dbReadTable(conn, "Features")

  if (nrow(features) == 0) {
    warning("No features found in DB_MassSpecResults_NonTargetAnalysis. Not done.")
    return(FALSE)
  }

  headers_list <- split(headers, headers$analysis)
  features_list <- split(features, features$analysis)

  parameters <- x$parameters

  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      fts <- load_cache(cache_manager, hash = hash)
      if (!is.null(fts) && nrow(fts) > 0) {
        message("\U2139 Results from ", x$method, " using ", x$algorithm, " loaded from cache!")
        DB_MassSpecResults_NonTargetAnalysis(
          projectPath = engine$get_project_path(),
          features = fts
        )
        return(invisible(TRUE))
      }
    }
  }

  fts <- rcpp_nts_load_features_ms1_2(
    info = analyses,
    spectra_headers = headers_list,
    feature_list = features_list,
    filtered = parameters$filtered,
    rtWindow = parameters$rtWindow,
    mzWindow = parameters$mzWindow,
    minTracesIntensity = parameters$minIntensity,
    mzClust = parameters$mzClust,
    presence = parameters$presence
  )

  if (is.null(fts) || length(fts) == 0) {
    warning("No MS1 traces loaded.")
    return(FALSE)
  }

  names(fts) <- analyses$analysis
  fts <- data.table::rbindlist(fts, fill = TRUE, idcol = "analysis")

  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_LoadFeaturesMS1_native"),
      hash = .make_hash(x, analyses, parameters, engine$Workflow),
      description = "MS1 loaded with DB_LoadFeaturesMS1_native method",
      data = as.data.frame(fts)
    )
    message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  }

  DB_MassSpecResults_NonTargetAnalysis(
    projectPath = engine$get_project_path(),
    features = fts
  )
  invisible(TRUE)
}
