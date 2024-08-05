
#' @noRd
.process <- function(settings, self, private) {
  UseMethod(".process")
}

#' @title Validate
#' 
#' @description Validates an object.
#' 
#' @param x An object.
#' 
#' @export
#' 
validate <- function(x) {
  UseMethod("validate")
}
