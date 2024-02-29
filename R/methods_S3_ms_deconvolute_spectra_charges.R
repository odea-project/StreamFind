
#' @title .s3_deconvolute_spectra_charges.Settings_deconvolute_spectra_charges_StreamFind
#'
#' @description Deconvolutes spectra charges from multi-charged compounds.
#'
#' @noRd
#'
.s3_deconvolute_spectra_charges.Settings_deconvolute_spectra_charges_StreamFind <- function(settings, self) {
  
  
  rtmin = 314 - 5
  rtmax = 314 + 5
  mzmin = 2500
  mzmax = 3600
  presence = 0.1 # presence can be adjusted to remove noise
  mzClust = 0.001 # critical for resolution, in Da
  minIntensity = 50
  roundVal = 30
  lowCut = 0.2
  plotLevel = 2
  xlab = expression(italic("m/z"))
  ylab = "Intensity / counts"
  title = "Charges and mass annotation"

  ms1 <- self$get_ms1(
    mz = data.table("rtmin" = rtmin, "rtmax" = rtmax, "mzmin" = mzmin, "mzmax" = mzmax),
    presence = presence,
    mzClust = mzClust,
    minIntensity = minIntensity
  )
  
  # .plot_spectra_interactive(ms1)
  
  browser()
  
  
  
  
  
  
  
  
  
  
  
  
  
}
