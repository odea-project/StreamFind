#' @title Generic Results Class
#' @description The `Results` class is used to store results of data processing in [StreamFind::Analyses] child classes. Child classes of `Results` are implemented for diverse types of results for a given type of data.
#' @param type A character string indicating the type of data.
#' @param name A character string representing the name of the results.
#' @param software A character string representing the name of the software used to generate the results.
#' @param version A character string representing the version of the software used to generate the results.
#' @return A `Results` S3 class object which is a list with the elements `type`, `name`, `software`, and `version`. Other elements are added by child class constructors (e.g. `MassSpecSpectra`).
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
