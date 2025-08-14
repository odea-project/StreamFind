#' @title MassSpecMethod_QuantifyChromatographicPeaks_native Class
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
MassSpecMethod_QuantifyChromatographicPeaks_native <-function(calibration = 0, value = "area", model = "linear") {
    
    x <- ProcessingStep(
        type = "MassSpec",
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
}

#' @describeIn MassSpecMethod_QuantifyChromatographicPeaks_native Validate the MassSpecMethod_QuantifyChromatographicPeaks_native object, returning NULL if valid.
#' @param x A MassSpecMethod_QuantifyChromatographicPeaks_native object.
#' @export
#' 
validate_object.MassSpecMethod_QuantifyChromatographicPeaks_native <- function(x) {
    checkmate::assert_choice(x$type, "MassSpec")
    checkmate::assert_choice(x$method, "QuantifyChromatographicPeaks")
    checkmate::assert_choice(x$algorithm, "native")
    checkmate::assert_numeric(x$parameters$calibration)
  NextMethod()
    NULL
}

#' @export
#' @noRd
run.MassSpecMethod_QuantifyChromatographicPeaks_native <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["MassSpecResults_Chromatograms"]])) {
    warning("No chromatograms results object available! Not done.")
    return(FALSE)
  }
  chrom_obj <- engine$Results[["MassSpecResults_Chromatograms"]]
  if (length(chrom_obj$peaks) == 0) {
    warning("No chromatograms peaks available! Not done.")
    return(FALSE)
  }
  parameters <- x$parameters
  peaks <- chrom_obj$peaks
  calibration_values <- parameters$calibration
  if (length(calibration_values) != length(peaks)) {
    calibration_values <- vapply(engine$Analyses$analyses, function(a) a$concentration, NA_real_)
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
  chrom_obj$peaks[names(peaks_list)] <- peaks_list
  chrom_obj$calibration_model <- calibration_fit
  engine$Results <- chrom_obj
  message(paste0("\U2713 ", "Chromatographic peaks quantified!"))
  TRUE
}
