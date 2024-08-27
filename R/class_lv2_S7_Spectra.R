#' @export
#' @noRd
Spectra <- S7::new_class("Spectra", package = "StreamFind", parent = Results,
  
  properties = list(
    ## __features -----
    spectra = S7::new_property(S7::class_list, default = list()),
    ## __averaged -----
    is_averaged = S7::new_property(S7::class_logical, default = FALSE)
  ),
  
  constructor = function(spectra = list(), is_averaged = FALSE) {
    S7::new_object(
      Results(), 
      name = "Spectra",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      spectra = spectra,
      is_averaged = is_averaged
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "Spectra"),
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_list(self@spectra),
      checkmate::test_logical(self@is_averaged, max.len = 1)
    ) && if (length(self@spectra) > 0) {
      all(vapply(self@spectra, function(x) checkmate::test_data_frame(x), FALSE))
    } else {
      TRUE
    }
    if (!valid) return(FALSE)
    NULL
  }
)
