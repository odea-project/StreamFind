#' @export
#' @noRd
Matrix <- S7::new_class("Matrix", package = "StreamFind", parent = Results,
  
  properties = list(
    ## __spectra -----
    matrix = S7::new_property(S7::class_data.frame, default = data.frame())
  ),
  
  constructor = function(matrix = data.frame()) {
    S7::new_object(
      Results(), 
      name = "Matrix",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      matrix = matrix
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "Model"),
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_data_frame(self@matrix)
    )
    if (!valid) return(FALSE)
    NULL
  }
)
