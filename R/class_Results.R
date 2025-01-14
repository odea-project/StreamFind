#' @export
#' @noRd
Results <- S7::new_class(
  name = "Results",
  package = "StreamFind",
  properties = list(
    name = S7::new_property(S7::class_character, default = NA_character_),
    software = S7::new_property(S7::class_character, default = NA_character_),
    version = S7::new_property(S7::class_character, default = NA_character_)
  ),
  
  validator = function(self) {
    checkmate::assert_character(self@name)
    checkmate::assert_character(self@software)
    checkmate::assert_character(self@version)
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
