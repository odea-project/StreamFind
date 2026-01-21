# MARK: Native
# Native ------

#' @title DB_MassSpecMethod_LoadFeaturesMS2_native class
#' @description Native StreamFind method for loading MS2 spectra into features stored in a database.
#' @param isolationWindow Numeric(1) isolation window (Da) around precursor m/z for MS2 extraction.
#' @param mzClust Numeric(1) m/z tolerance used when clustering traces.
#' @param presence Numeric(1) minimum fraction (0-1) of scans required to keep a cluster.
#' @param minIntensity Numeric(1) minimum trace intensity to extract.
#' @param filtered Logical(1) whether to include features already marked as filtered.
#' @export
#'
DB_MassSpecMethod_LoadFeaturesMS2_native <- function(
  isolationWindow = 1.3,
  mzClust = 0.005,
  presence = 0.8,
  minIntensity = 10,
  filtered = FALSE
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "LoadFeaturesMS2",
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
      isolationWindow = isolationWindow,
      mzClust = mzClust,
      presence = presence,
      minIntensity = minIntensity,
      filtered = filtered
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_LoadFeaturesMS2_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_LoadFeaturesMS2_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "LoadFeaturesMS2")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_number(x$parameters$isolationWindow)
  checkmate::assert_number(x$parameters$mzClust)
  checkmate::assert_number(x$parameters$presence)
  checkmate::assert_number(x$parameters$minIntensity)
  checkmate::assert_logical(x$parameters$filtered, max.len = 1)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_LoadFeaturesMS2_native <- function(x, engine = NULL) {
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

  fts <- rcpp_nts_load_features_ms2_2(
    info = analyses,
    spectra_headers = headers_list,
    feature_list = features_list,
    filtered = parameters$filtered,
    minTracesIntensity = parameters$minIntensity,
    isolationWindow = parameters$isolationWindow,
    mzClust = parameters$mzClust,
    presence = parameters$presence
  )

  if (is.null(fts) || length(fts) == 0) {
    warning("No MS2 spectra loaded.")
    return(FALSE)
  }

  names(fts) <- analyses$analysis
  fts <- data.table::rbindlist(fts, fill = TRUE, idcol = "analysis")

  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_LoadFeaturesMS2_native"),
      hash = .make_hash(x, analyses, parameters, engine$Workflow),
      description = "MS2 loaded with DB_LoadFeaturesMS2_native method",
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
