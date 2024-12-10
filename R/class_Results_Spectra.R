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
    
    # MARK: Peaks
    ## __peaks -----
    peaks = S7::new_property(S7::class_list, default = list()),
    
    # MARK: has_peaks
    ## __has_peaks -----
    has_peaks = S7::new_property(S7::class_logical, getter = function(self) length(self@peaks) > 0)
  ),
  
  constructor = function(spectra = list(),
                         is_averaged = FALSE,
                         is_neutralized = FALSE,
                         peaks = list()) {
    S7::new_object(
      Results(), 
      name = "Spectra",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      spectra = spectra,
      is_averaged = is_averaged,
      peaks = peaks
    )
  },
  
  validator = function(self) {
    checkmate::assert_true(self@name == "Spectra")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_list(self@spectra)
    checkmate::assert_logical(self@is_averaged, max.len = 1)
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
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, Spectra) <- function(x) {
  if (length(x@spectra) > 0) {
    cat("Number spectra: ", length(x@spectra), "\n")
    cat("Averaged: ", x@is_averaged, "\n")
    if (x@has_peaks) {
      cat("Number peaks: ", vapply(x@peaks, nrow, 0), "\n")
    } else {
      cat("Number peaks: ", 0, "\n")
    }
  } else {
    cat("Number spectra: ", 0, "\n")
  }
}
