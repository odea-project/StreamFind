#' @export
#' @noRd
Chromatograms <- S7::new_class("Chromatograms", package = "StreamFind", parent = Results,
  
  properties = list(
    ## __chromatograms -----
    chromatograms = S7::new_property(S7::class_list, default = list()),
    ## __averaged -----
    is_averaged = S7::new_property(S7::class_logical, default = FALSE)
  ),
  
  constructor = function(chromatograms = list(), is_averaged = FALSE) {
    S7::new_object(
      Results(), 
      name = "Chromatograms",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      chromatograms = chromatograms,
      is_averaged = is_averaged
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "Chromatograms"),
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_list(self@chromatograms),
      checkmate::test_logical(self@is_averaged, max.len = 1)
    ) && if (length(self@chromatograms) > 0) {
      all(vapply(self@chromatograms, function(x) checkmate::test_data_frame(x), FALSE))
    } else {
      TRUE
    }
    if (!valid) return(FALSE)
    NULL
  }
)
