#' @title Results Class
#' 
#' @description The Results class is used to store results of data processing in
#' [StreamFind::Analyses] child classes. Child classes of Results are implemented for diverse types
#' of results.
#' 
#' @param name A character string representing the name of the results.
#' @param software A character string representing the name of the software used to generate the results.
#' @param version A character string representing the version of the software used to generate the results. 
#' 
#' @export
#' 
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
`$.Results` <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
`$<-.Results` <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}
