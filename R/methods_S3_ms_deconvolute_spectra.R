
#' @title .s3_ms_deconvolute_spectra.Settings_deconvolute_spectra_StreamFind
#'
#' @description Deconvolutes spectra according to calculated charges from multi-charged compounds.
#'
#' @noRd
#'
.s3_ms_deconvolute_spectra.Settings_deconvolute_spectra_StreamFind <- function(settings, self, private) {
  
  parameters <- settings$parameters
  
  # roundVal <- parameters$roundVal
  # relLowCut <- parameters$relLowCut
  # absLowCut <- parameters$absLowCut
  
  if (!self$has_spectra_charges()) {
    warning("Spectra charges not found for deconvolution! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  charges <- self$spectra_charges
  
  browser()
  
  
  
  
  
  
  
  
}
