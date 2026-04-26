#' DB_MassSpecMethod_FilterFeatures_native Class
#'
#' @description Settings for filtering features in DB_MassSpecResults_NonTargetAnalysis objects based on feature properties.
#'
#' @param minSN Numeric (length 1) with the minimum signal-to-noise ratio.
#' @param minIntensity Numeric (length 1) with the minimum intensity.
#' @param minArea Numeric (length 1) with the minimum peak area.
#' @param minWidth Numeric (length 1) with the minimum peak width in seconds.
#' @param maxWidth Numeric (length 1) with the maximum peak width in seconds.
#' @param maxPPM Numeric (length 1) with the maximum ppm deviation.
#' @param minFwhmRT Numeric (length 1) with the minimum FWHM in retention time (seconds).
#' @param maxFwhmRT Numeric (length 1) with the maximum FWHM in retention time (seconds).
#' @param minFwhmMZ Numeric (length 1) with the minimum FWHM in m/z.
#' @param maxFwhmMZ Numeric (length 1) with the maximum FWHM in m/z.
#' @param minGaussianA Numeric (length 1) with the minimum Gaussian amplitude.
#' @param minGaussianMu Numeric (length 1) with the minimum Gaussian mu (retention time).
#' @param maxGaussianMu Numeric (length 1) with the maximum Gaussian mu (retention time).
#' @param minGaussianSigma Numeric (length 1) with the minimum Gaussian sigma.
#' @param maxGaussianSigma Numeric (length 1) with the maximum Gaussian sigma.
#' @param minGaussianR2 Numeric (length 1) with the minimum Gaussian fit R-squared (0-1).
#' @param maxJaggedness Numeric (length 1) with the maximum jaggedness (smoothness, lower is better).
#' @param minSharpness Numeric (length 1) with the minimum sharpness (higher is better).
#' @param minAsymmetry Numeric (length 1) with the minimum asymmetry factor.
#' @param maxAsymmetry Numeric (length 1) with the maximum asymmetry factor (1.0 = symmetric).
#' @param maxModality Integer (length 1) with the maximum number of local maxima (1 = single peak).
#' @param minPlates Numeric (length 1) with the minimum theoretical plates (chromatographic efficiency).
#' @param onlyFilled Logical (length 1) with `TRUE` to keep only filled features, `FALSE` to keep only non-filled, `NA` for no filter.
#' @param removeFilled Logical (length 1) with `TRUE` to remove filled features.
#' @param minSizeEIC Integer (length 1) with the minimum number of EIC data points.
#' @param minSizeMS1 Integer (length 1) with the minimum number of MS1 data points.
#' @param minSizeMS2 Integer (length 1) with the minimum number of MS2 data points.
#' @param minRelPresenceReplicate Numeric (length 1) minimum relative presence of a feature group within a replicate,
#' computed as the fraction of analyses in that replicate where the feature group is present.
#' @param removeIsotopes Logical (length 1) with `TRUE` to remove features where adduct contains 'isotope'.
#' @param removeAdducts Logical (length 1) with `TRUE` to remove features where adduct contains 'adduct'.
#' @param removeLosses Logical (length 1) with `TRUE` to remove features where adduct contains 'loss'.
#'
#' @return A `DB_MassSpecMethod_FilterFeatures_native` object.
#'
#' @export
#'
DB_MassSpecMethod_FilterFeatures_native <- function(
  minSN = NA_real_,
  minIntensity = NA_real_,
  minArea = NA_real_,
  minWidth = NA_real_,
  maxWidth = NA_real_,
  maxPPM = NA_real_,
  minFwhmRT = NA_real_,
  maxFwhmRT = NA_real_,
  minFwhmMZ = NA_real_,
  maxFwhmMZ = NA_real_,
  minGaussianA = NA_real_,
  minGaussianMu = NA_real_,
  maxGaussianMu = NA_real_,
  minGaussianSigma = NA_real_,
  maxGaussianSigma = NA_real_,
  minGaussianR2 = NA_real_,
  maxJaggedness = NA_real_,
  minSharpness = NA_real_,
  minAsymmetry = NA_real_,
  maxAsymmetry = NA_real_,
  maxModality = NA_integer_,
  minPlates = NA_real_,
  onlyFilled = NA,
  removeFilled = FALSE,
  minSizeEIC = NA_integer_,
  minSizeMS1 = NA_integer_,
  minSizeMS2 = NA_integer_,
  minRelPresenceReplicate = NA_real_,
  removeIsotopes = FALSE,
  removeAdducts = FALSE,
  removeLosses = FALSE
) {
  x <- ProcessingStep(
    type = "DB_MassSpec",
    method = "FilterFeatures",
    required = "FindFeatures",
    algorithm = "native",
    input_class = "DB_MassSpecResults_NonTargetAnalysis",
    output_class = "DB_MassSpecResults_NonTargetAnalysis",
    parameters = list(
      minSN = as.numeric(minSN),
      minIntensity = as.numeric(minIntensity),
      minArea = as.numeric(minArea),
      minWidth = as.numeric(minWidth),
      maxWidth = as.numeric(maxWidth),
      maxPPM = as.numeric(maxPPM),
      minFwhmRT = as.numeric(minFwhmRT),
      maxFwhmRT = as.numeric(maxFwhmRT),
      minFwhmMZ = as.numeric(minFwhmMZ),
      maxFwhmMZ = as.numeric(maxFwhmMZ),
      minGaussianA = as.numeric(minGaussianA),
      minGaussianMu = as.numeric(minGaussianMu),
      maxGaussianMu = as.numeric(maxGaussianMu),
      minGaussianSigma = as.numeric(minGaussianSigma),
      maxGaussianSigma = as.numeric(maxGaussianSigma),
      minGaussianR2 = as.numeric(minGaussianR2),
      maxJaggedness = as.numeric(maxJaggedness),
      minSharpness = as.numeric(minSharpness),
      minAsymmetry = as.numeric(minAsymmetry),
      maxAsymmetry = as.numeric(maxAsymmetry),
      maxModality = as.integer(maxModality),
      minPlates = as.numeric(minPlates),
      onlyFilled = onlyFilled,
      removeFilled = as.logical(removeFilled),
      minSizeEIC = as.integer(minSizeEIC),
      minSizeMS1 = as.integer(minSizeMS1),
      minSizeMS2 = as.integer(minSizeMS2),
      minRelPresenceReplicate = as.numeric(minRelPresenceReplicate),
      removeIsotopes = as.logical(removeIsotopes),
      removeAdducts = as.logical(removeAdducts),
      removeLosses = as.logical(removeLosses)
    ),
    number_permitted = Inf,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid DB_MassSpecMethod_FilterFeatures_native object!")
  }
}

#' @export
#' @noRd
#'
validate_object.DB_MassSpecMethod_FilterFeatures_native <- function(x) {
  checkmate::assert_choice(x$type, "DB_MassSpec")
  checkmate::assert_choice(x$method, "FilterFeatures")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_numeric(x$parameters$minSN, len = 1)
  checkmate::assert_numeric(x$parameters$minIntensity, len = 1)
  checkmate::assert_numeric(x$parameters$minArea, len = 1)
  checkmate::assert_numeric(x$parameters$minWidth, len = 1)
  checkmate::assert_numeric(x$parameters$maxWidth, len = 1)
  checkmate::assert_numeric(x$parameters$maxPPM, len = 1)
  checkmate::assert_numeric(x$parameters$minFwhmRT, len = 1)
  checkmate::assert_numeric(x$parameters$maxFwhmRT, len = 1)
  checkmate::assert_numeric(x$parameters$minFwhmMZ, len = 1)
  checkmate::assert_numeric(x$parameters$maxFwhmMZ, len = 1)
  checkmate::assert_numeric(x$parameters$minGaussianA, len = 1)
  checkmate::assert_numeric(x$parameters$minGaussianMu, len = 1)
  checkmate::assert_numeric(x$parameters$maxGaussianMu, len = 1)
  checkmate::assert_numeric(x$parameters$minGaussianSigma, len = 1)
  checkmate::assert_numeric(x$parameters$maxGaussianSigma, len = 1)
  checkmate::assert_numeric(x$parameters$minGaussianR2, len = 1)
  checkmate::assert_numeric(x$parameters$maxJaggedness, len = 1)
  checkmate::assert_numeric(x$parameters$minSharpness, len = 1)
  checkmate::assert_numeric(x$parameters$minAsymmetry, len = 1)
  checkmate::assert_numeric(x$parameters$maxAsymmetry, len = 1)
  checkmate::assert_integerish(x$parameters$maxModality, len = 1)
  checkmate::assert_numeric(x$parameters$minPlates, len = 1)
  checkmate::assert_logical(x$parameters$removeFilled, len = 1)
  checkmate::assert_integerish(x$parameters$minSizeEIC, len = 1)
  checkmate::assert_integerish(x$parameters$minSizeMS1, len = 1)
  checkmate::assert_integerish(x$parameters$minSizeMS2, len = 1)
  checkmate::assert_numeric(x$parameters$minRelPresenceReplicate, len = 1)
  checkmate::assert_logical(x$parameters$removeIsotopes, len = 1)
  checkmate::assert_logical(x$parameters$removeAdducts, len = 1)
  checkmate::assert_logical(x$parameters$removeLosses, len = 1)
  NULL
}

#' @export
#' @noRd
#'
run.DB_MassSpecMethod_FilterFeatures_native <- function(
  x,
  engine = NULL
) {
  if (!is(engine, "DB_MassSpecEngine")) {
    warning("Engine is not a DB_MassSpecEngine object!")
    return(FALSE)
  }

  if (is.null(engine$NonTargetAnalysis)) {
    warning("No DB_MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$NonTargetAnalysis
  analyses_info <- info(engine$Analyses)
  parameters <- x$parameters

  # Check cache
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, analyses_info, parameters, engine$Workflow)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      fts <- load_cache(cache_manager, hash = hash)
      if (!is.null(fts) && is.data.frame(fts)) {
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

  # Query all features from database
  fts <- query_db(nts, "SELECT * FROM Features")

  if (nrow(fts) == 0) {
    warning("No features found in DB_MassSpecResults_NonTargetAnalysis! Not done.")
    return(FALSE)
  }

  # Count features before filtering
  n_before <- sum(!fts$filtered, na.rm = TRUE)

  if (n_before == 0) {
    warning("DB_MassSpecResults_NonTargetAnalysis object does not have unfiltered features! Not done.")
    return(FALSE)
  }

  analyses_db <- query_db(engine$Analyses, "SELECT * FROM Analyses")

  feature_list <- lapply(analyses_db$analysis, function(ana) {
    ana_features <- fts[fts$analysis == ana, ]
    if (nrow(ana_features) == 0) {
      return(fts[0, ])
    }
    ana_features
  })
  names(feature_list) <- analyses_db$analysis

  fts_list <- rcpp_nts_filter_features(
    info = analyses_db,
    feature_list = feature_list,
    minSN = parameters$minSN,
    minIntensity = parameters$minIntensity,
    minArea = parameters$minArea,
    minWidth = parameters$minWidth,
    maxWidth = parameters$maxWidth,
    maxPPM = parameters$maxPPM,
    minFwhmRT = parameters$minFwhmRT,
    maxFwhmRT = parameters$maxFwhmRT,
    minFwhmMZ = parameters$minFwhmMZ,
    maxFwhmMZ = parameters$maxFwhmMZ,
    minGaussianA = parameters$minGaussianA,
    minGaussianMu = parameters$minGaussianMu,
    maxGaussianMu = parameters$maxGaussianMu,
    minGaussianSigma = parameters$minGaussianSigma,
    maxGaussianSigma = parameters$maxGaussianSigma,
    minGaussianR2 = parameters$minGaussianR2,
    maxJaggedness = parameters$maxJaggedness,
    minSharpness = parameters$minSharpness,
    minAsymmetry = parameters$minAsymmetry,
    maxAsymmetry = parameters$maxAsymmetry,
    maxModality = parameters$maxModality,
    minPlates = parameters$minPlates,
    onlyFilled = parameters$onlyFilled,
    removeFilled = parameters$removeFilled,
    minSizeEIC = parameters$minSizeEIC,
    minSizeMS1 = parameters$minSizeMS1,
    minSizeMS2 = parameters$minSizeMS2,
    minRelPresenceReplicate = parameters$minRelPresenceReplicate,
    removeIsotopes = parameters$removeIsotopes,
    removeAdducts = parameters$removeAdducts,
    removeLosses = parameters$removeLosses
  )

  if (is.null(fts_list) || length(fts_list) == 0) {
    warning("Feature filtering failed.")
    return(FALSE)
  }

  names(fts_list) <- analyses_db$analysis
  fts <- data.table::rbindlist(fts_list, fill = TRUE, idcol = "analysis")

  # Count features after filtering
  n_after <- sum(!fts$filtered, na.rm = TRUE)
  n_filtered <- n_before - n_after

  message(sprintf("\u2713 FilterFeatures complete: %d features filtered, %d remaining", n_filtered, n_after))

  # Save to cache
  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_FilterFeatures_native"),
      hash = .make_hash(x, analyses_info, parameters, engine$Workflow),
      description = "Features filtered with DB_FilterFeatures_native method",
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
