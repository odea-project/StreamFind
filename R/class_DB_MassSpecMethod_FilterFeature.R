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
  minSizeMS2 = NA_integer_
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
      minSizeMS2 = as.integer(minSizeMS2)
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

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Analyses$results[["DB_MassSpecResults_NonTargetAnalysis"]])) {
    warning("No DB_MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$DB_MassSpecResults_NonTargetAnalysis

  conn <- DBI::dbConnect(duckdb::duckdb(), nts$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  parameters <- x$parameters

  # Check cache
  cache_manager <- engine$Cache
  if (!is.null(cache_manager)) {
    hash <- .make_hash(x, nts$db, parameters)
    cache_info <- get_cache_info(cache_manager)
    if (nrow(cache_info) > 0) {
      cached_result <- load_cache(cache_manager, hash = hash)
      if (!is.null(cached_result)) {
        if (is.logical(cached_result) && cached_result) {
          message("\U2139 FilterFeatures results using ", x$algorithm, " loaded from cache!")
          return(invisible(TRUE))
        }
      }
    }
  }

  # Count features before filtering
  n_before <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM Features WHERE NOT filtered")$n

  if (n_before == 0) {
    warning("DB_MassSpecResults_NonTargetAnalysis object does not have unfiltered features! Not done.")
    return(FALSE)
  }

  # Build SQL WHERE clauses for each filter
  filter_conditions <- character(0)
  filter_names <- character(0)

  # Numeric filters (minimum thresholds)
  if (!is.na(parameters$minSN)) {
    filter_conditions <- c(filter_conditions, sprintf("sn < %f", parameters$minSN))
    filter_names <- c(filter_names, "minSN")
  }

  if (!is.na(parameters$minArea)) {
    filter_conditions <- c(filter_conditions, sprintf("area < %f", parameters$minArea))
    filter_names <- c(filter_names, "minArea")
  }

  if (!is.na(parameters$minWidth)) {
    filter_conditions <- c(filter_conditions, sprintf("width < %f", parameters$minWidth))
    filter_names <- c(filter_names, "minWidth")
  }

  if (!is.na(parameters$maxWidth)) {
    filter_conditions <- c(filter_conditions, sprintf("width > %f", parameters$maxWidth))
    filter_names <- c(filter_names, "maxWidth")
  }

  if (!is.na(parameters$maxPPM)) {
    filter_conditions <- c(filter_conditions, sprintf("ppm > %f", parameters$maxPPM))
    filter_names <- c(filter_names, "maxPPM")
  }

  if (!is.na(parameters$minFwhmRT)) {
    filter_conditions <- c(filter_conditions, sprintf("fwhm_rt < %f", parameters$minFwhmRT))
    filter_names <- c(filter_names, "minFwhmRT")
  }

  if (!is.na(parameters$maxFwhmRT)) {
    filter_conditions <- c(filter_conditions, sprintf("fwhm_rt > %f", parameters$maxFwhmRT))
    filter_names <- c(filter_names, "maxFwhmRT")
  }

  if (!is.na(parameters$minFwhmMZ)) {
    filter_conditions <- c(filter_conditions, sprintf("fwhm_mz < %f", parameters$minFwhmMZ))
    filter_names <- c(filter_names, "minFwhmMZ")
  }

  if (!is.na(parameters$maxFwhmMZ)) {
    filter_conditions <- c(filter_conditions, sprintf("fwhm_mz > %f", parameters$maxFwhmMZ))
    filter_names <- c(filter_names, "maxFwhmMZ")
  }

  if (!is.na(parameters$minGaussianA)) {
    filter_conditions <- c(filter_conditions, sprintf("gaussian_A < %f", parameters$minGaussianA))
    filter_names <- c(filter_names, "minGaussianA")
  }

  if (!is.na(parameters$minGaussianMu)) {
    filter_conditions <- c(filter_conditions, sprintf("gaussian_mu < %f", parameters$minGaussianMu))
    filter_names <- c(filter_names, "minGaussianMu")
  }

  if (!is.na(parameters$maxGaussianMu)) {
    filter_conditions <- c(filter_conditions, sprintf("gaussian_mu > %f", parameters$maxGaussianMu))
    filter_names <- c(filter_names, "maxGaussianMu")
  }

  if (!is.na(parameters$minGaussianSigma)) {
    filter_conditions <- c(filter_conditions, sprintf("gaussian_sigma < %f", parameters$minGaussianSigma))
    filter_names <- c(filter_names, "minGaussianSigma")
  }

  if (!is.na(parameters$maxGaussianSigma)) {
    filter_conditions <- c(filter_conditions, sprintf("gaussian_sigma > %f", parameters$maxGaussianSigma))
    filter_names <- c(filter_names, "maxGaussianSigma")
  }

  if (!is.na(parameters$minGaussianR2)) {
    filter_conditions <- c(filter_conditions, sprintf("gaussian_r2 < %f", parameters$minGaussianR2))
    filter_names <- c(filter_names, "minGaussianR2")
  }

  if (!is.na(parameters$maxJaggedness)) {
    filter_conditions <- c(filter_conditions, sprintf("jaggedness > %f", parameters$maxJaggedness))
    filter_names <- c(filter_names, "maxJaggedness")
  }

  if (!is.na(parameters$minSharpness)) {
    filter_conditions <- c(filter_conditions, sprintf("sharpness < %f", parameters$minSharpness))
    filter_names <- c(filter_names, "minSharpness")
  }

  if (!is.na(parameters$minAsymmetry)) {
    filter_conditions <- c(filter_conditions, sprintf("asymmetry < %f", parameters$minAsymmetry))
    filter_names <- c(filter_names, "minAsymmetry")
  }

  if (!is.na(parameters$maxAsymmetry)) {
    filter_conditions <- c(filter_conditions, sprintf("asymmetry > %f", parameters$maxAsymmetry))
    filter_names <- c(filter_names, "maxAsymmetry")
  }

  if (!is.na(parameters$maxModality)) {
    filter_conditions <- c(filter_conditions, sprintf("modality > %d", parameters$maxModality))
    filter_names <- c(filter_names, "maxModality")
  }

  if (!is.na(parameters$minPlates)) {
    filter_conditions <- c(filter_conditions, sprintf("plates < %f", parameters$minPlates))
    filter_names <- c(filter_names, "minPlates")
  }

  # Boolean filters
  if (!is.na(parameters$onlyFilled)) {
    if (parameters$onlyFilled) {
      filter_conditions <- c(filter_conditions, "NOT filled")
      filter_names <- c(filter_names, "onlyFilled")
    } else {
      filter_conditions <- c(filter_conditions, "filled")
      filter_names <- c(filter_names, "notFilled")
    }
  }

  if (parameters$removeFilled) {
    filter_conditions <- c(filter_conditions, "filled")
    filter_names <- c(filter_names, "removeFilled")
  }

  # Integer filters
  if (!is.na(parameters$minSizeEIC)) {
    filter_conditions <- c(filter_conditions, sprintf("eic_size < %d", parameters$minSizeEIC))
    filter_names <- c(filter_names, "minSizeEIC")
  }

  if (!is.na(parameters$minSizeMS1)) {
    filter_conditions <- c(filter_conditions, sprintf("ms1_size < %d", parameters$minSizeMS1))
    filter_names <- c(filter_names, "minSizeMS1")
  }

  if (!is.na(parameters$minSizeMS2)) {
    filter_conditions <- c(filter_conditions, sprintf("ms2_size < %d", parameters$minSizeMS2))
    filter_names <- c(filter_names, "minSizeMS2")
  }

  # Apply filters
  if (length(filter_conditions) > 0) {
    for (i in seq_along(filter_conditions)) {
      condition <- filter_conditions[i]
      filter_name <- filter_names[i]

      # Update features that match the condition
      update_query <- sprintf(
        "UPDATE Features
         SET filtered = TRUE,
             filter = CASE
               WHEN filter IS NULL OR filter = '' THEN '%s'
               ELSE filter || ' %s'
             END
         WHERE NOT filtered AND (%s)",
        filter_name, filter_name, condition
      )

      n_updated <- DBI::dbExecute(conn, update_query)

      if (n_updated > 0) {
        message(sprintf("\u2713 Filtered %d features by %s", n_updated, filter_name))
      }
    }
  }

  # Count features after filtering
  n_after <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM Features WHERE NOT filtered")$n
  n_filtered <- n_before - n_after

  message(sprintf("\u2713 FilterFeatures complete: %d features filtered, %d remaining", n_filtered, n_after))

  # Save to cache
  if (!is.null(cache_manager)) {
    save_cache(
      cache_manager,
      name = paste0("DB_FilterFeatures_native"),
      hash = .make_hash(x, nts$db, parameters),
      description = "Features filtered with DB_FilterFeatures_native method",
      data = TRUE
    )
    message("\U1f5ab Results from ", x$method, " using ", x$algorithm, " cached!")
  }

  # Update the engine results
  engine$Results$DB_MassSpecResults_NonTargetAnalysis <- nts

  return(TRUE)
}
