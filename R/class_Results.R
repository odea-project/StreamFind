#' @title Generic Results
#' 
#' @description The Results class is used to store results of data processing in
#' [StreamFind::Analyses] child classes. Child classes of Results are implemented for diverse types
#' of results for a given type of data.
#' 
#' @slot data_type A character string indicating the type of data.
#' @slot name A character string representing the name of the results.
#' @slot software A character string representing the name of the software used to generate the results.
#' @slot version A character string representing the version of the software used to generate the results.
#' 
#' @export
#' 
Results <- S7::new_class(
  name = "Results",
  package = "StreamFind",
  properties = list(
    data_type = S7::new_property(S7::class_character, default = NA_character_),
    name = S7::new_property(S7::class_character, default = NA_character_),
    software = S7::new_property(S7::class_character, default = NA_character_),
    version = S7::new_property(S7::class_character, default = NA_character_)
  ),
  
  validator = function(self) {
    checkmate::assert_character(self@data_type)
    checkmate::assert_character(self@name)
    checkmate::assert_character(self@software)
    checkmate::assert_character(self@version)
    NULL
  }
)

#' @export
#' @noRd
`$.StreamFind::Results` <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
`$<-.StreamFind::Results` <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}
