#' DB_MassSpecMethod_FilterFeatures_native Class
#'
#' @description Settings for filtering features in DB_MassSpecResults_NonTargetAnalysis objects based on feature properties.
#'
#' @param minSN Numeric (length 1) with the minimum signal-to-noise ratio.
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

  parameters <- x$parameters

  # Check cache
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, nts$db, parameters)
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

  # Build SQL WHERE clauses for each filter
  filter_to_apply <- list()

  # Numeric filters (minimum thresholds)
  if (!is.na(parameters$minSN)) {
    filter_to_apply[["minSN"]] <- function(x) x$sn < parameters$minSN
  }

  if (!is.na(parameters$minArea)) {
    filter_to_apply[["minArea"]] <- function(x) x$area < parameters$minArea
  }

  if (!is.na(parameters$minWidth)) {
    filter_to_apply[["minWidth"]] <- function(x) x$width < parameters$minWidth
  }

  if (!is.na(parameters$maxWidth)) {
    filter_to_apply[["maxWidth"]] <- function(x) x$width > parameters$maxWidth
  }

  if (!is.na(parameters$maxPPM)) {
    filter_to_apply[["maxPPM"]] <- function(x) x$ppm > parameters$maxPPM
  }

  if (!is.na(parameters$minFwhmRT)) {
    filter_to_apply[["minFwhmRT"]] <- function(x) x$fwhm_rt < parameters$minFwhmRT
  }

  if (!is.na(parameters$maxFwhmRT)) {
    filter_to_apply[["maxFwhmRT"]] <- function(x) x$fwhm_rt > parameters$maxFwhmRT
  }

  if (!is.na(parameters$minFwhmMZ)) {
    filter_to_apply[["minFwhmMZ"]] <- function(x) x$fwhm_mz < parameters$minFwhmMZ
  }

  if (!is.na(parameters$maxFwhmMZ)) {
    filter_to_apply[["maxFwhmMZ"]] <- function(x) x$fwhm_mz > parameters$maxFwhmMZ
  }

  if (!is.na(parameters$minGaussianA)) {
    filter_to_apply[["minGaussianA"]] <- function(x) x$gaussian_A < parameters$minGaussianA
  }

  if (!is.na(parameters$minGaussianMu)) {
    filter_to_apply[["minGaussianMu"]] <- function(x) x$gaussian_mu < parameters$minGaussianMu
  }

  if (!is.na(parameters$maxGaussianMu)) {
    filter_to_apply[["maxGaussianMu"]] <- function(x) x$gaussian_mu > parameters$maxGaussianMu
  }

  if (!is.na(parameters$minGaussianSigma)) {
    filter_to_apply[["minGaussianSigma"]] <- function(x) x$gaussian_sigma < parameters$minGaussianSigma
  }

  if (!is.na(parameters$maxGaussianSigma)) {
    filter_to_apply[["maxGaussianSigma"]] <- function(x) x$gaussian_sigma > parameters$maxGaussianSigma
  }

  if (!is.na(parameters$minGaussianR2)) {
    filter_to_apply[["minGaussianR2"]] <- function(x) x$gaussian_r2 < parameters$minGaussianR2
  }

  if (!is.na(parameters$maxJaggedness)) {
    filter_to_apply[["maxJaggedness"]] <- function(x) x$jaggedness > parameters$maxJaggedness
  }

  if (!is.na(parameters$minSharpness)) {
    filter_to_apply[["minSharpness"]] <- function(x) x$sharpness < parameters$minSharpness
  }

  if (!is.na(parameters$minAsymmetry)) {
    filter_to_apply[["minAsymmetry"]] <- function(x) x$asymmetry < parameters$minAsymmetry
  }

  if (!is.na(parameters$maxAsymmetry)) {
    filter_to_apply[["maxAsymmetry"]] <- function(x) x$asymmetry > parameters$maxAsymmetry
  }

  if (!is.na(parameters$maxModality)) {
    filter_to_apply[["maxModality"]] <- function(x) x$modality > parameters$maxModality
  }

  if (!is.na(parameters$minPlates)) {
    filter_to_apply[["minPlates"]] <- function(x) x$plates < parameters$minPlates
  }

  # Boolean filters
  if (!is.na(parameters$onlyFilled)) {
    if (parameters$onlyFilled) {
      filter_to_apply[["onlyFilled"]] <- function(x) !x$filled
    } else {
      filter_to_apply[["notFilled"]] <- function(x) x$filled
    }
  }

  if (parameters$removeFilled) {
    filter_to_apply[["removeFilled"]] <- function(x) x$filled
  }

  # Integer filters
  if (!is.na(parameters$minSizeEIC)) {
    filter_to_apply[["minSizeEIC"]] <- function(x) x$eic_size < parameters$minSizeEIC
  }

  if (!is.na(parameters$minSizeMS1)) {
    filter_to_apply[["minSizeMS1"]] <- function(x) x$ms1_size < parameters$minSizeMS1
  }

  if (!is.na(parameters$minSizeMS2)) {
    filter_to_apply[["minSizeMS2"]] <- function(x) x$ms2_size < parameters$minSizeMS2
  }

  # Isotope, adduct, and loss filters
  if (parameters$removeIsotopes) {
    filter_to_apply[["removeIsotopes"]] <- function(x) grepl("isotope", x$adduct, fixed = TRUE)
  }

  if (parameters$removeAdducts) {
    filter_to_apply[["removeAdducts"]] <- function(x) grepl("adduct", x$adduct, fixed = TRUE)
  }

  if (parameters$removeLosses) {
    filter_to_apply[["removeLosses"]] <- function(x) grepl("loss", x$adduct, fixed = TRUE)
  }

  # Apply filters
  if (length(filter_to_apply) > 0) {
    for (filter_name in names(filter_to_apply)) {
      filter_func <- filter_to_apply[[filter_name]]
      to_filter <- !fts$filtered & filter_func(fts)
      to_filter[is.na(to_filter)] <- FALSE
      n_updated <- sum(to_filter)

      if (n_updated > 0) {
        fts$filtered[to_filter] <- TRUE
        fts$filter[to_filter] <- ifelse(
          is.na(fts$filter[to_filter]) | fts$filter[to_filter] == "",
          filter_name,
          paste(fts$filter[to_filter], filter_name)
        )
        message(sprintf("\u2713 Filtered %d features by %s", n_updated, filter_name))
      }
    }
  }

  # Count features after filtering
  n_after <- sum(!fts$filtered, na.rm = TRUE)
  n_filtered <- n_before - n_after

  message(sprintf("\u2713 FilterFeatures complete: %d features filtered, %d remaining", n_filtered, n_after))

  # Save to cache
  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_FilterFeatures_native"),
      hash = .make_hash(x, nts$db, parameters),
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
