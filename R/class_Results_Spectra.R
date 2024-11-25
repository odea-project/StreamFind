#' @export
#' @noRd
Spectra <- S7::new_class("Spectra", package = "StreamFind", parent = Results,
  
  properties = list(
    
    # MARK: Spectra
    ## __spectra -----
    spectra = S7::new_property(S7::class_list, default = list()),
    
    # MARK: is_averaged
    ## __is_averaged -----
    is_averaged = S7::new_property(S7::class_logical, default = FALSE),
    
    # MARK: is_neutralized
    ## __is_neutralized -----
    is_neutralized = S7::new_property(S7::class_logical, default = FALSE),
    
    # MARK: Peaks
    ## __peaks -----
    peaks = S7::new_property(S7::class_list, default = list()),
    
    # MARK: has_peaks
    ## __has_peaks -----
    has_peaks = S7::new_property(S7::class_logical, getter = function(self) length(self@peaks) > 0),
    
    # MARK: Charges
    ## __charges -----
    charges = S7::new_property(S7::class_list, default = list())
  ),
  
  constructor = function(spectra = list(),
                         is_averaged = FALSE,
                         is_neutralized = FALSE,
                         peaks = list(),
                         charges = list()) {
    S7::new_object(
      Results(), 
      name = "Spectra",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      spectra = spectra,
      is_averaged = is_averaged,
      is_neutralized = is_neutralized,
      peaks = peaks,
      charges = charges
    )
  },
  
  validator = function(self) {
    checkmate::assert_true(self@name == "Spectra")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_list(self@spectra)
    checkmate::assert_logical(self@is_averaged, max.len = 1)
    checkmate::assert_logical(self@is_neutralized, max.len = 1)
    if (length(self@spectra) > 0) {
      for (spectrum in self@spectra) {
        checkmate::assert_data_frame(spectrum)
      }
    }
    if (length(self@peaks) > 0) {
      for (peak in self@peaks) {
        checkmate::assert_data_frame(peak)
      }
    }
    if (length(self@charges) > 0) {
      for (charge in self@charges) {
        checkmate::assert_data_frame(charge)
      }
    }
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, Spectra) <- function(x) {
  if (length(x@spectra) > 0) {
    cat("Number spectra: ", length(x@spectra), "\n")
    cat("Averaged: ", x@is_averaged, "\n")
    cat("Neutralized: ", x@is_neutralized, "\n")
    if (x@has_peaks) {
      cat("Number peaks: ", vapply(x@peaks, nrow, 0), "\n")
    } else {
      cat("Number peaks: ", 0, "\n")
    }
    if (length(x@charges) > 0) {
      cat("Number charges: ", vapply(x@charges, nrow, 0), "\n")
    }
  } else {
    cat("Number spectra: ", 0, "\n")
  }
}
