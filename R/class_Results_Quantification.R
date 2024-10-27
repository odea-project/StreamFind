#' @export
#' @noRd
Quantification <- S7::new_class("Quantification", package = "StreamFind", parent = Results,
  
  properties = list(
    ## __compound -----
    compounds = S7::new_property(S7::class_character, default = character()),
    
    ## __model -----
    models = S7::new_property(S7::class_list, default = list()),
    
    ## __quantity -----
    quantities = S7::new_property(S7::class_list, default = list())
    
  ),
  
  constructor = function(compounds = character(), models = list(), quantities = list()) {
    S7::new_object(
      Results(), 
      name = "Quantification",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      compounds = compounds,
      models = models,
      quantities = quantities
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "Quantification"),
      checkmate::test_true(self@software == "StreamFind"),
      checkmate::test_character(self@compounds),
      checkmate::test_list(self@models),
      checkmate::test_list(self@quantities)
    )
    if (!valid) return(FALSE)
    NULL
  }
)
