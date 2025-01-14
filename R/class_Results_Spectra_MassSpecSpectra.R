#' @export
#' @noRd
MassSpecSpectra <- S7::new_class(
  name = "MassSpecSpectra",
  package = "StreamFind",
  parent = Spectra,
  properties = list(
    is_neutralized = S7::new_property(S7::class_logical, default = FALSE),
    charges = S7::new_property(S7::class_list, default = list())
  ),
  constructor = function(spectra = list(),
                         is_averaged = FALSE,
                         is_neutralized = FALSE,
                         peaks = list(),
                         charges = list()) {
    S7::new_object(
      Spectra(), 
      name = "MassSpecSpectra",
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
    checkmate::assert_logical(self@is_neutralized, max.len = 1)
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
S7::method(show, MassSpecSpectra) <- function(x) {
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
