#' @export
#' @noRd
Model <- S7::new_class("Model", package = "StreamFind", parent = Results,
  
  properties = list(
    ## __spectra -----
    model = S7::new_property(S7::class_list, default = list())
  ),
  
  constructor = function(model = list()) {
    S7::new_object(
      Results(), 
      name = "Model",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "Model"),
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_list(self@model)
    )
    if (!valid) return(FALSE)
    NULL
  }
)
