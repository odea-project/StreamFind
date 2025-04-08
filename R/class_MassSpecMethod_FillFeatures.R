#' **MassSpecMethod_FillFeatures_StreamFind**
#'
#' @description Settings for filling missing values in features.
#'
#' @param withinReplicate Logical of length one to fill within replicates not global.
#' @param filtered Logical of length one to consider filtered features or not.
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @param minPeakWidth Numeric of length one with the minimum peak width for building feature
#' extraction targets.
#' @param maxPeakWidth Numeric of length one with the maximum peak width for building feature
#' extraction targets.
#' @param minTracesIntensity Numeric of length one with the minimum intensity to collect spectra
#' data for extracted ion chromatograms.
#' @param baseCut Numeric of length one with the base cut for building Gaussian model.
#' @param maxSearchWindow Numeric of length one with maximum time search window (seconds) from the
#' averaged group retention time. Default to 5 seconds.
#' @param minNumberTraces Integer of length one with the minimum number of traces to consider a
#' feature.
#' @param minIntensity Numeric of length one with the minimum intensity to consider a feature.
#' Meaning that the maximum intensity of the extracted ion chromatogram must be greater than the
#' defined value.
#' @param minSignalToNoiseRatio Numeric of length one with the minimum signal to noise ratio to
#' consider a feature.
#' @param minGaussianFit Numeric of length one with the minimum Gaussian fit to consider a feature.
#'
#' @return A MassSpecMethod_FillFeatures_StreamFind class object.
#'
#' @export
#'
MassSpecMethod_FillFeatures_StreamFind <- S7::new_class(
  name = "MassSpecMethod_FillFeatures_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(withinReplicate = TRUE,
                         filtered = TRUE,
                         rtExpand = 0,
                         mzExpand = 0,
                         minPeakWidth = 6,
                         maxPeakWidth = 30,
                         minTracesIntensity = 1000,
                         minNumberTraces = 5,
                         minIntensity = 5000,
                         baseCut = 0.3,
                         maxSearchWindow = 5,
                         minSignalToNoiseRatio = 3,
                         minGaussianFit = 0.2) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "FillFeatures",
        required = c("FindFeatures", "GroupFeatures"),
        algorithm = "StreamFind",
        parameters = list(
          withinReplicate = as.logical(withinReplicate),
          filtered = as.logical(filtered),
          rtExpand = as.numeric(rtExpand),
          mzExpand = as.numeric(mzExpand),
          minPeakWidth = as.numeric(minPeakWidth),
          maxPeakWidth = as.numeric(maxPeakWidth),
          minTracesIntensity = as.numeric(minTracesIntensity),
          minNumberTraces = as.numeric(minNumberTraces),
          minIntensity = as.numeric(minIntensity),
          baseCut = as.numeric(baseCut),
          maxSearchWindow = as.numeric(maxSearchWindow),
          minSignalToNoiseRatio = as.numeric(minSignalToNoiseRatio),
          minGaussianFit = as.numeric(minGaussianFit)
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
    checkmate::assert_choice(self@method, "FillFeatures")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_logical(self@parameters$withinReplicate, len = 1)
    checkmate::assert_logical(self@parameters$filtered, len = 1)
    checkmate::assert_numeric(self@parameters$rtExpand, len = 1)
    checkmate::assert_numeric(self@parameters$mzExpand, len = 1)
    checkmate::assert_numeric(self@parameters$minPeakWidth, len = 1)
    checkmate::assert_numeric(self@parameters$maxPeakWidth, len = 1)
    checkmate::assert_integer(as.integer(self@parameters$minNumberTraces), len = 1)
    checkmate::assert_numeric(self@parameters$minTracesIntensity, len = 1)
    checkmate::assert_numeric(self@parameters$minIntensity, len = 1)
    checkmate::assert_numeric(self@parameters$baseCut, len = 1)
    checkmate::assert_numeric(self@parameters$maxSearchWindow, len = 1)
    checkmate::assert_numeric(self@parameters$minSignalToNoiseRatio, len = 1)
    checkmate::assert_numeric(self@parameters$minGaussianFit, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_FillFeatures_StreamFind) <- function(x, engine = NULL) {
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

  if (!NTS@has_groups) {
    warning("NTS object does not have feature groups! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters
  
  feature_list <- rcpp_ms_fill_features(
    engine$NTS$analyses_info,
    engine$NTS$spectra_headers,
    engine$NTS$feature_list,
    parameters$withinReplicate,
    parameters$filtered,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minPeakWidth,
    parameters$maxPeakWidth,
    parameters$minTracesIntensity,
    as.integer(parameters$minNumberTraces),
    parameters$minIntensity,
    parameters$baseCut,
    parameters$maxSearchWindow,
    parameters$minSignalToNoiseRatio,
    parameters$minGaussianFit
  )
  
  NTS$feature_list <- feature_list
  engine$NTS <- NTS
  TRUE
}
