#' @export
#' @noRd
DataFrame <- S7::new_class("DataFrame", package = "StreamFind", parent = Results,
  
  properties = list(
    ## __spectra -----
    data = S7::new_property(S7::class_data.frame, default = data.frame())
  ),
  
  constructor = function(data = data.frame()) {
    S7::new_object(
      Results(), 
      name = "DataFrame",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      data = as.data.frame(data)
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "DataFrame"),
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_data_frame(self@data)
    )
    if (!valid) return(FALSE)
    NULL
  }
)
