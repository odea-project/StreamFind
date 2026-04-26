#' Mass Spectrometry Method to Calculate Features Quality (StreamFind algorithm)
#'
#' @description Settings for calculating quality parameters of features (e.g., signal-to-noise (sn) ratio).
#'
#' @template arg-ms-filtered
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @param minPeakWidth Numeric of length one with the minimum peak width centered on the maximum
#' for extracting the feature EIC.
#' @param maxPeakWidth Numeric of length one with the maximum peak width centered on the maximum
#' for reconstructing the feature EIC.
#' @param minNumberTraces Numeric of length 1 with the minimum number traces for calculating feature
#' quality.
#' @param minTracesIntensity Numeric of length 1 with the minimum intensity of spectra traces for
#' calculating feature quality.
#' @param baseCut Numeric of length 1 with the base cut for calculating feature Gaussian fit.
#'
#' @return A `MassSpecMethod_CalculateFeaturesQuality_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_CalculateFeaturesQuality_StreamFind <- function(
  filtered = FALSE,
  rtExpand = 0,
  mzExpand = 0,
  minPeakWidth = 6,
  maxPeakWidth = 30,
  minTracesIntensity = 0,
  minNumberTraces = 6,
  baseCut = 0
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "CalculateFeaturesQuality",
    required = "FindFeatures",
    algorithm = "StreamFind",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      "filtered" = as.logical(filtered),
      "rtExpand" = as.numeric(rtExpand),
      "mzExpand" = as.numeric(mzExpand),
      "minPeakWidth" = as.numeric(minPeakWidth),
      "maxPeakWidth" = as.numeric(maxPeakWidth),
      "minTracesIntensity" = as.numeric(minTracesIntensity),
      "minNumberTraces" = as.numeric(minNumberTraces),
      "baseCut" = as.numeric(baseCut)
    ),
    number_permitted = 1,
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
    stop("Invalid MassSpecMethod_CalculateFeaturesQuality_StreamFind object!")
  }
}

#' @export
#' @noRd
#'
validate_object.MassSpecMethod_CalculateFeaturesQuality_StreamFind <- function(
  x
) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "CalculateFeaturesQuality")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_logical(x$parameters$filtered, max.len = 1)
  checkmate::assert_number(x$parameters$rtExpand)
  checkmate::assert_number(x$parameters$mzExpand)
  checkmate::assert_number(x$parameters$minPeakWidth)
  checkmate::assert_number(x$parameters$maxPeakWidth)
  checkmate::assert_integer(as.integer(x$parameters$minNUmberTraces))
  checkmate::assert_number(x$parameters$minTracesIntensity)
  checkmate::assert_number(x$parameters$baseCut)
  NULL
}

#' @export
#' @noRd
#'
run.MassSpecMethod_CalculateFeaturesQuality_StreamFind <- function(
  x,
  engine = NULL
) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Analyses$results[["MassSpecResults_NonTargetAnalysis"]])) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  if (
    sum(vapply(nts$features, function(z) nrow(z), 0)) == 0
  ) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  feature_list <- nts$features

  feature_list <- lapply(feature_list, function(z) {
    if (!"quality" %in% colnames(z)) {
      z$quality <- rep(data.table::data.table(), nrow(z))
    }
    if (!"eic" %in% colnames(z)) {
      z$eic <- rep(data.table::data.table(), nrow(z))
    }
    z
  })

  parameters <- x$parameters
  ana_info <- nts$info
  headers <- nts$headers

  feature_list <- rcpp_nts_calculate_features_quality(
    ana_info,
    headers,
    feature_list,
    parameters$filtered,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minPeakWidth,
    parameters$maxPeakWidth,
    parameters$minTracesIntensity,
    parameters$minNumberTraces,
    parameters$baseCut
  )

  nts$features <- feature_list

  tryCatch(
    {
      engine$Results <- nts
      return(TRUE)
    },
    error = function(e) {
      warning(e)
      return(FALSE)
    }
  )
}
