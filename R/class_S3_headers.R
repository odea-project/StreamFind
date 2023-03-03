
#' Headers S3 Class Constructor
#'
#' @description
#' Creates a headers S3 class object.
#'
#' @param call Character of length one with the name of the method where the
#' settings are to be applied.
#' @param algorithm Character of length one with the name of the algorithm to
#' be used.
#' @param parameters A list with parameter settings specific for the `call`
#' method and used `algorithm`.
#'
#' @details See the method documentation for more information about possible
#' algorithms and parameters to be used.
#'
#' @return A settings S3 class
#'
#' @export
#'
headers <- function(name = NA_character_,
                    path = NA_character_,
                    date = NULL, ...) {
  x <- list(
    "call" = call,
    "algorithm" = algorithm,
    "parameters" = parameters
  )

  if (validate_settings(x)) {
    structure(x, class = "settings")
  } else {
    NULL
  }
}

#' @rdname settings
#' @export
as.settings <- function(settings) {
  settings(settings$call, settings$algorithm, settings$parameters)
}
