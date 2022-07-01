

#' createSettings
#'
#' @description Creates a \linkS4class{settings} object for a given processing step.
#'
#' @param call The name of the function where the algorithm and settings are used.
#' @param algorithm A character string with the name of the algorithm to be used.
#' @param settings A list of settings dependent on the algorithm used.
#'
#' @return A \linkS4class{settings} object.
#'
#' @export
#'
createSettings <- function(call = NA_character_, algorithm = NA_character_, settings = list()) {

  ns <- new("settings")

  ns@call <- call
  ns@algorithm <- algorithm

  if (!checkmate::testClass(settings, "list")) settings <- list(settings)

  ns@settings <- settings

  return(ns)

}
