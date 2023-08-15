
#' @title .s3_ms_bin_spectra.Settings_bin_spectra_qBinning
#'
#' @description Bins spectra using the algorithm qBinning.
#'
#' @noRd
#'
.s3_ms_bin_spectra.Settings_bin_spectra_qBinning <- function(settings, self) {

  message("Binning spectra with qBinning...", appendLF = TRUE)

  if (!any(self$has_loaded_spectra())) self$load_spectra()

  if (!any(self$has_loaded_spectra())) {
    warning("Spectra not found in MS analyses.")
    return(FALSE)
  }

  spectra <- self$get_spectra()

  # TODO Max Implementation qBinning method

  # self$add_features_eic(eics, replace = TRUE)

  FALSE
}
