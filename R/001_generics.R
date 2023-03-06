#' @export
#' @noRd
validate <- function(x, ...) {
  UseMethod("validate", x)
}
