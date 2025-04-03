#' **MassSpecMethod_CalculateFeaturesQuality_StreamFind**
#'
#' @description Settings for calculating quality parameters of features (e.g., signal-to-noise (sn)
#' ratio).
#'
#' @template arg-ms-filtered
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @param minPeakWidth Numeric of length one with the minimum peak width centered on the maximum
#' for extracting the feature EIC.
#' @param maxPeakWidth Numeric of length one with the maximum peak width centered on the maximum
#' for reconstructing the feature EIC.
#' @param minTraces Numeric of length 1 with the minimum number traces for calculating feature
#' quality.
#' @param minIntensity Numeric of length 1 with the minimum intensity of spectra traces for
#' calculating feature quality.
#' @param baseCut Numeric of length 1 with the base cut for calculating feature Gaussian fit.
#'
#' @return A `MassSpecMethod_CalculateFeaturesQuality_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_CalculateFeaturesQuality_StreamFind <- S7::new_class(
  name = "MassSpecMethod_CalculateFeaturesQuality_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(filtered = FALSE,
                         rtExpand = 0,
                         mzExpand = 0,
                         minPeakWidth = 6,
                         maxPeakWidth = 30,
                         minTracesIntensity = 0,
                         minNumberTraces = 6,
                         baseCut = 0) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "CalculateFeaturesQuality",
        required = "FindFeatures",
        algorithm = "StreamFind",
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
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "CalculateFeaturesQuality")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_logical(self@parameters$filtered, max.len = 1)
    checkmate::assert_number(self@parameters$rtExpand)
    checkmate::assert_number(self@parameters$mzExpand)
    checkmate::assert_number(self@parameters$minPeakWidth)
    checkmate::assert_number(self@parameters$maxPeakWidth)
    checkmate::assert_integer(as.integer(self@parameters$minNUmberTraces))
    checkmate::assert_number(self@parameters$minTracesIntensity)
    checkmate::assert_number(self@parameters$baseCut)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_CalculateFeaturesQuality_StreamFind) <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$has_results_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }

  NTS <- engine$NTS

  if (!NTS@has_features) {
    warning("NTS object does not have features! Not done.")
    return(FALSE)
  }

  feature_list <- NTS$feature_list

  feature_list <- lapply(feature_list, function(z) {
    if (!"quality" %in% colnames(z)) z$quality <- rep(data.table::data.table(), nrow(z))
    if (!"eic" %in% colnames(z)) z$eic <- rep(data.table::data.table(), nrow(z))
    z
  })

  parameters <- x$parameters
  
  ana_info <- engine$NTS$analyses_info
  headers <- engine$NTS$spectra_headers
  
  feature_list <- rcpp_ms_calculate_features_quality_v2(
    ana_info$analysis,
    ana_info$file,
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
  
  browser()
  browser()
  browser()
  

  tryCatch(
    {
      engine$NTS$feature_list <- feature_list
      return(TRUE)
    },
    error = function(e) {
      warning(e)
      return(FALSE)
    }
  )
}
