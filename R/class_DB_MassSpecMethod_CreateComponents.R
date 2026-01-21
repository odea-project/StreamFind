# MARK: Native
# Native ------

#' @title DB_MassSpecMethod_CreateComponents_native class
#' @description Native StreamFind method to assign component identifiers by clustering features that overlap in retention time using their FWHM windows, with optional EIC correlation-based sub-clustering.
#' @param rtWindow Numeric length-2 vector of retention time offsets (seconds) applied left/right to the FWHM window when checking overlaps. Defaults to no offset.
#' @param minCorrelation Numeric value (0-1) for minimum Pearson correlation between EIC profiles to keep features in the same component. Features with correlation below this threshold are separated into different components. Set to 0 to disable EIC correlation filtering. Defaults to 0.8.
#' @param debugRT Numeric retention time (seconds) to debug. Components containing features within debugRT ± rtWindow will be logged to log. Set to 0 to disable debugging. Defaults to 0.
#' @param debugAnalysis Character string specifying the analysis name to debug. When specified, only debug logging for the matching analysis is generated. Empty string logs all analyses. Defaults to "".
#' @export
#'
DB_MassSpecMethod_CreateComponents_native <- function(
  rtWindow = c(0, 0),
  minCorrelation = 0.8,
  debugRT = 0,
  debugAnalysis = ""
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "CreateComponents",
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
      minCorrelation = minCorrelation,
      debugRT = debugRT,
      debugAnalysis = debugAnalysis
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_CreateComponents_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_CreateComponents_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "CreateComponents")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$rtWindow, max.len = 2, null.ok = FALSE)
  checkmate::assert_number(x$parameters$minCorrelation, lower = 0, upper = 1, null.ok = FALSE)
  checkmate::assert_number(x$parameters$debugRT, lower = 0, null.ok = FALSE)
  checkmate::assert_string(x$parameters$debugAnalysis, null.ok = FALSE)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_CreateComponents_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecAnalyses" %in% class(engine$Analyses)) {
    warning("Engine does not contain DB_MassSpecAnalyses.")
    return(FALSE)
  }

  analyses <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  if (nrow(analyses) == 0) {
    warning("No analyses found in DB_MassSpecAnalyses.")
    return(FALSE)
  }

  db <- file.path(engine$get_project_path(), "DB_MassSpecResults_NonTargetAnalysis.duckdb")
  if (!file.exists(db)) {
    warning("DB_MassSpecResults_NonTargetAnalysis database not found. Run FindFeatures first.")
    return(FALSE)
  }

  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  headers <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")
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

  fts <- rcpp_nts_create_components(
    info = analyses,
    spectra_headers = headers_list,
    feature_list = features_list,
    rtWindow = parameters$rtWindow,
    minCorrelation = parameters$minCorrelation,
    debugRT = parameters$debugRT,
    debugAnalysis = parameters$debugAnalysis
  )

  if (is.null(fts) || length(fts) == 0) {
    warning("No components created.")
    return(FALSE)
  }

  names(fts) <- analyses$analysis
  fts <- data.table::rbindlist(fts, fill = TRUE, idcol = "analysis")

  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_CreateComponents_native"),
      hash = .make_hash(x, analyses, parameters, engine$Workflow),
      description = "Components created with DB_CreateComponents_native method",
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
