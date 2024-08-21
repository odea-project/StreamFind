
#' @export
#' @noRd
show <- S7::new_generic("show", "x")

#' @export
#' @noRd
run <- S7::new_generic("run", "x")

#' @export
#' @noRd
save <- S7::new_generic("save", "x")

#' @export
#' @noRd
read <- S7::new_generic("read", "x")

#' @export
#'@noRd
add <- S7::new_generic("add", "x")

#' @export
#'@noRd
remove <- S7::new_generic("remove", "x")

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
