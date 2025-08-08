#' @title Generic Results Class
#' @description The `Results` class is used to store results of data processing in [StreamFind::Analyses] child classes. Child classes of `Results` are implemented for diverse types of results for a given type of data.
#' @slot type A character string indicating the type of data.
#' @slot name A character string representing the name of the results.
#' @slot software A character string representing the name of the software used to generate the results.
#' @slot version A character string representing the version of the software used to generate the results.
#' @export
#' 
Results <- function(type = NA_character_, name = "Results", software = "StreamFind", version = NA_character_) {
  structure(
    list(
      type = type,
      name = name,
      software = software,
      version = version
    ),
    class = c("Results"),
  )
}

#' @describeIn Results Validate the Results object, returning `NULL` if valid.
#' @param x An object of class `Results`.
#' @export
#' 
validate_object.Results <- function(x) {
  checkmate::assert_character(x$type)
  checkmate::assert_character(x$name)
  checkmate::assert_character(x$software)
  checkmate::assert_character(x$version)
  NULL
}
