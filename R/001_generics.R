#' @noRd
validate <- function(x) {
  UseMethod("validate")
}

#' @noRd
asJSON <- function(x) {
  UseMethod("asJSON")
}

#' @noRd
export <- function(x) {
  UseMethod("export")
}
