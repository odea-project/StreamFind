#' @export
#' @noRd
Results <- S7::new_class("Results", package = "StreamFind",
  
  properties = list(
    
    name = S7::new_property(S7::class_character, default = NA_character_),
    
    software = S7::new_property(S7::class_character, default = NA_character_),
    
    version = S7::new_property(S7::class_character, default = NA_character_)
    
  ),
  
  validator = function(self) {
    valid <- all(
      checkmate::test_character(self@name, len = 1),
      checkmate::test_character(self@software, len = 1),
      checkmate::test_character(self@version, len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(`$`, Results) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`$<-`, Results) <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}
