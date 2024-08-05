
#' @noRd
.process.MassSpecSettings_CentroidSpectra_qCentroids <- function(settings, self) {

  message("Centroiding spectra with qCentroids...", appendLF = TRUE)
  
  if (self$get_number_analyses() == 0) {
    warning("There are no analyses! Add MS analyses as mzML or mzXML files!")
    return(FALSE)
  }
  
  if (!all(self$get_spectra_mode() %in% "profile")) {
    warning("MS analyses must be all in profile mode for centroiding! Not done.")
    return(FALSE)
  }

  if (!any(self$has_loaded_spectra())) self$load_spectra()

  zero_spectra <- all(self$get_spectra_number() == 0)

  if (zero_spectra) {
    warning("All analyses must have spectra! Analyses not centroided.")
    return(FALSE)
  }

  all_profile <- all(self$get_spectra_mode() %in% "profile")

  if (!all_profile) {
    warning("Spectra must be in profile mode for centroiding! Analyses not centroided.")
    return(FALSE)
  }

  spectra_list <- lapply(self$get_analyses(), function(x) x$spectra)

  parameters <- settings$parameters
  
  message("\U2699 Centroiding spectra for ", length(self$get_analyses()), " analyses using qCentroids...", appendLF = FALSE)

  centroided_spectra_list <- lapply(spectra_list, function(i) {
    do.call("rcpp_centroid_spectra_qCentroids", list("spectra" = i, maxScale = parameters$maxScale, mode = parameters$mode))
  })

  names(centroided_spectra_list) <- self$get_analysis_names()

  self$add_spectra(centroided_spectra_list, replace = TRUE)

  message(" Done!")

  TRUE
}
