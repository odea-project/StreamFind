
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_QuantifyChromatographicPeaks_StreamFind**
#'
#' @description Quantifies chromatographic peaks based on a calibration model.
#' 
#' @param calibration Numeric with the calibration/concentration values for each analysis. When concentration is not 
#' know use NA_real_.
#' @param value Character with the value to quantify. Possible values are *intensity* or *area*.
#' @param model Character with the model to use for calibration. Possible values are *linear*, *quadratic*, or *cubic*.
#'
#' @return A MassSpecSettings_QuantifyChromatographicPeaks_StreamFind object.
#'
#' @export
#'
MassSpecSettings_QuantifyChromatographicPeaks_StreamFind <- S7::new_class("MassSpecSettings_QuantifyChromatographicPeaks_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(calibration = 0, value = "area", model = "linear") {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "QuantifyChromatographicPeaks",
      required = c("LoadChromstograms", "IntegrateChromatograms"),
      algorithm = "StreamFind",
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
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "QuantifyChromatographicPeaks")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_numeric(self@parameters$calibration)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_QuantifyChromatographicPeaks_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_chromatograms()) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  
  chromatograms <- engine$chromatograms
  
  if (!chromatograms$has_peaks) {
    warning("No chromatograms peaks available! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  peaks <- chromatograms$peaks
  
  calibration_values <- parameters$calibration
  
  if (length(calibration_values) != length(peaks)) {
    calibration_values <- engine$analyses$concentrations
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
  chromatograms$peaks[names(peaks)] <- peaks_list
  chromatograms$peaks <- peaks_list
  chromatograms$calibration_model <- calibration_fit
  engine$chromatograms <- chromatograms
  message(paste0("\U2713 ", "Chromatographic peaks quantified!"))
  TRUE
}
