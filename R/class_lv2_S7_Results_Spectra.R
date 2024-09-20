#' @export
#' @noRd
Spectra <- S7::new_class("Spectra", package = "StreamFind", parent = Results,
  
  properties = list(
    ## __spectra -----
    spectra = S7::new_property(S7::class_list, default = list()),
    ## __averaged -----
    is_averaged = S7::new_property(S7::class_logical, default = FALSE),
    ## __is_neutralized -----
    is_neutralized = S7::new_property(S7::class_logical, default = FALSE),
    ## __peaks -----
    peaks = S7::new_property(S7::class_list, default = list()),
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
    valid <- all(
      checkmate::test_true(self@name == "Spectra"),
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_list(self@spectra),
      checkmate::test_logical(self@is_averaged, max.len = 1),
      checkmate::test_logical(self@is_neutralized, max.len = 1)
    ) && if (length(self@spectra) > 0) {
      all(vapply(self@spectra, function(x) checkmate::test_data_frame(x), FALSE))
    } else {
      TRUE
    } && if (length(self@peaks) > 0) {
      all(vapply(self@peaks, function(x) checkmate::test_data_frame(x), FALSE))
    } else {
      TRUE
    } && if (length(self@charges) > 0) {
      all(vapply(self@charges, function(x) checkmate::test_data_frame(x), FALSE))
    } else {
      TRUE
    }
    if (!valid) return(FALSE)
    NULL
  }
)
