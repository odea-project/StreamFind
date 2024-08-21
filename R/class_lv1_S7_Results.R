
#' @export
#' @noRd
Results <- S7::new_class("Results", package = "StreamFind",
  
  properties = list(
    
    engine = S7::new_property(S7::class_character, default = NA_character_),
    
    name = S7::new_property(S7::class_character, default = NA_character_),
    
    software = S7::new_property(S7::class_character, default = NA_character_),
    
    version = S7::new_property(S7::class_character, default = NA_character_)
  ),
  
  validator = function(self) {
    valid <- all(
      checkmate::test_character(self@engine, len = 1),
      checkmate::test_choice(self@engine, choices = c(NA_character_, .get_available_engines())),
      checkmate::test_character(self@name, len = 1),
      checkmate::test_character(self@software, len = 1),
      checkmate::test_character(self@version, len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)
