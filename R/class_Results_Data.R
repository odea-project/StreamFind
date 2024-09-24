#' @export
#' @noRd
Data <- S7::new_class("Data", package = "StreamFind", parent = Results,
  
  properties = list(
    ## __spectra -----
    data = S7::new_property(S7::class_data.frame, default = data.frame())
  ),
  
  constructor = function(data = data.frame()) {
    S7::new_object(
      Results(), 
      name = "Data",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      data = as.data.frame(data)
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "Data"),
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_data_frame(self@data)
    )
    if (!valid) return(FALSE)
    NULL
  }
)
