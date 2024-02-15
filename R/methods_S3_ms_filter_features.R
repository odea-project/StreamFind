
#' @title .s3_ms_bin_spectra.Settings_filter_features_patRoon
#'
#' @description Filter features and feature groups using the algorithm patRoon.
#'
#' @noRd
#'
.s3_ms_filter_features.Settings_filter_features_patRoon <- function(settings, self) {
  
  if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  
  if (self$has_modules_data("patRoon")) {
    module_pat <- self$get_modules_data("patRoon")[["patRoon"]]
    
  } else {
    return(FALSE)
  }
  
  parameters <- settings$parameters
  
  possible_only_in_features <- c(
    "absMinIntensity", "relMinIntensity", "retentionRange", "mzRange", 
    "mzDefectRange", "chromWidthRange", "qualityRange", "negate"
  )
  
  if ("features" %in% is(module_pat$data)) {
    parameters <- parameters[names(parameters) %in% possible_only_in_features]
  }
  
  filter_fun <- patRoon::filter
  
  pat <- do.call(filter_fun, c(list("obj" = module_pat$data), parameters))
  
  # TODO patRoon, keep filtered features?! 
  
  self$add_modules_data(
    list("patRoon" = list(
      "data" = pat,
      "software" = "patRoon",
      "version" = as.character(packageVersion("patRoon"))
    ))
  )
  
  TRUE
}
