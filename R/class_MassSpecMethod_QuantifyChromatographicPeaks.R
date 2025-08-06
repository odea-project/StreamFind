#' MassSpecMethod_QuantifyChromatographicPeaks_native S7 class
#'
#' @description Quantifies chromatographic peaks based on a calibration model.
#' 
#' @param calibration Numeric with the calibration/concentration values for each analysis. When
#' concentration is not know use NA_real_.
#' @param value Character with the value to quantify. Possible values are *intensity* or *area*.
#' @param model Character with the model to use for calibration. Possible values are *linear*,
#' *quadratic*, or *cubic*.
#'
#' @return A MassSpecMethod_QuantifyChromatographicPeaks_native object.
#'
#' @export
#'
MassSpecMethod_QuantifyChromatographicPeaks_native <- S7::new_class(
  name = "MassSpecMethod_QuantifyChromatographicPeaks_native",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  
  constructor = function(calibration = 0, value = "area", model = "linear") {
    
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "QuantifyChromatographicPeaks",
        required = c("LoadChromatograms", "IntegrateChromatograms"),
        algorithm = "native",
        parameters = list(
          calibration = as.numeric(calibration),
          value = as.character(value),
          model = "linear"
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
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "QuantifyChromatographicPeaks")
    checkmate::assert_choice(self@algorithm, "native")
    checkmate::assert_numeric(self@parameters$calibration)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_QuantifyChromatographicPeaks_native) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_chromatograms()) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  
  chromatograms <- engine$Chromatograms
  
  if (!chromatograms$has_peaks) {
    warning("No chromatograms peaks available! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  peaks <- chromatograms$peaks
  
  calibration_values <- parameters$calibration
  
  if (length(calibration_values) != length(peaks)) {
    calibration_values <- engine$Analyses$concentrations
  } else {
    names(calibration_values) <- names(peaks)
  }
  
  if (length(calibration_values) != length(peaks)) {
    warning("The number of calibration values does not match the number of chromatograms!")
    return(FALSE)
  }
  
  intensity <- NULL
  area <- NULL
  concentration <- NULL
  
  peaks <- data.table::rbindlist(peaks, idcol = "analysis", fill = TRUE)
  
  peaks$calibration <- calibration_values[peaks$analysis]
  
  calibration <- peaks[
    !is.na(peaks$calibration),
    .(
      analysis,
      calibration = mean(calibration),
      intensity = mean(intensity),
      area = mean(area)
    ),
    by = .(analysis)
  ]
  
  calibration_values <- calibration$calibration
  values <- calibration[[parameters$value]]
  
  switch(parameters$model,
    linear = {
      calibration_fit <- stats::lm(calibration_values ~ values)
      concentrations <- stats::predict(calibration_fit, newdata = data.frame(values = peaks[[parameters$value]]))
    },
    quadratic = {
      calibration_fit <- stats::lm(calibration_values ~ values + I(values^2))
      concentrations <- stats::vpredict(calibration_fit, newdata = data.frame(values = peaks[[parameters$value]]))
    },
    cubic = {
      calibration_fit <- stats::lm(calibration_values ~ values + I(values^2) + I(values^3))
      concentrations <- stats::predict(calibration_fit, newdata = data.frame(values = peaks[[parameters$value]]))
    }
  )
  
  peaks$concentration <- concentrations
  
  split_vec <- peaks$analysis
  peaks$analysis <- NULL
  peaks_list <- split(peaks, split_vec)
  chromatograms$peaks[names(peaks_list)] <- peaks_list
  chromatograms$calibration_model <- calibration_fit
  engine$Chromatograms <- chromatograms
  message(paste0("\U2713 ", "Chromatographic peaks quantified!"))
  TRUE
}
