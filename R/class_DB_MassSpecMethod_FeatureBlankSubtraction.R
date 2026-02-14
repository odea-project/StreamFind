
# MARK: native
# native ------

#' @title DB_MassSpecMethod_FeatureBlankSubtraction_native class
#' @description Native StreamFind method for blank subtraction of features.
#' @param blankThreshold numeric(1) threshold multiplier for blank feature intensities to consider a feature as present in blanks. To be present a features must be at least blankThreshold times more intense in samples than in blanks.
#' @param rtExpand numeric(1) retention time expansion window in seconds around feature RT for EIC extraction.
#' @param mzExpand numeric(1) m/z expansion window in Da around feature m/z for EIC extraction.
#' @export
#'
DB_MassSpecMethod_FeatureBlankSubtraction_native <- function(
  blankThreshold = 5,
  rtExpand = 10,
  mzExpand = 0.005
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "FeatureBlankSubtraction",
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
      blankThreshold = 5,
      rtExpand = rtExpand,
      mzExpand = mzExpand
    )
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid parameters for DB_MassSpecMethod_FeatureBlankSubtraction_native.")
  }
}

#' @export
#' @noRd
validate_object.DB_MassSpecMethod_FeatureBlankSubtraction_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "FeatureBlankSubtraction")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$blankThreshold, len = 1, lower = 0)
  NULL
}

#' @export
#' @noRd
run.DB_MassSpecMethod_FeatureBlankSubtraction_native <- function(x, engine = NULL) {
  if (!"DB_MassSpecResults_NonTargetAnalysis" %in% class(engine$NonTargetAnalysis)) {
    warning("Engine does not contain DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  nts <- engine$NonTargetAnalysis
  analyses <- info(engine$Analyses)

  if (nrow(analyses) == 0) {
    warning("No analyses found in the DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  blks <- unique(analyses$blank)
  if (length(blks) == 0 || all(is.na(blks))) {
    warning("No blanks defined in the DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

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

  fts <- query_db(nts, "SELECT * FROM Features")
  if (nrow(fts) == 0) {
    warning("No features found in DB_MassSpecResults_NonTargetAnalysis.")
    return(FALSE)
  }

  analyses_db <- query_db(engine$Analyses, "SELECT * FROM Analyses")
  spectra_headers <- query_db(engine$Analyses, "SELECT * FROM SpectraHeaders")
  headers_list <- split(spectra_headers, spectra_headers$analysis)

  feature_list <- lapply(analyses_db$analysis, function(ana) {
    ana_features <- fts[fts$analysis == ana, ]
    if (nrow(ana_features) == 0) {
      return(fts[0, ])
    }
    ana_features
  })
  names(feature_list) <- analyses_db$analysis

  fts_list <- rcpp_nts_blank_subtraction_2(
    info = analyses_db,
    spectra_headers = headers_list,
    feature_list = feature_list,
    blankThreshold = parameters$blankThreshold,
    rtExpand = parameters$rtExpand,
    mzExpand = parameters$mzExpand
  )

  if (is.null(fts_list) || length(fts_list) == 0) {
    warning("Blank subtraction failed.")
    return(FALSE)
  }

  names(fts_list) <- analyses_db$analysis
  fts <- data.table::rbindlist(fts_list, fill = TRUE, idcol = "analysis")

  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_FeatureBlankSubtraction_native"),
      hash = .make_hash(x, analyses, parameters, engine$Workflow),
      description = "Features found with DB_FeatureBlankSubtraction_native method",
      data = as.data.frame(fts)
    )
    message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  }
  invisible(DB_MassSpecResults_NonTargetAnalysis(
    projectPath = engine$get_project_path(),
    features = fts
  ))
  invisible(TRUE)
}
