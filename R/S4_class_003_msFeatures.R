

#### validity ------------------------------------------------------------

msFeatures_validity <- function(object) {

  valid <- TRUE

  if (nrow(object@metadata) > 0) {
    must_have_names <- c("id", "index", "rt", "mz", "drt", "rtmin", "rtmax", "dppm", "mzmin", "mzmax")
    valid <- !FALSE %in% (must_have_names %in% colnames(object@metadata))
  }

  # if (nrow(object@features) > 0) {
  #   must_have_names <- paste0("id")
  #   valid <- !FALSE %in% (must_have_names %in% colnames(object@features))
  # }

  return(valid)
}


### msFeatures ----------------------------------------------------------------------------------------------

#' msFeatures-class
#'
#' @description An S4 class representing an MS sample/file within the \pkg{streamFind} package.
#' The \code{msFeatures} is used to store and manage MS data and the respective methods can be used
#' for inspection, processing and evaluation.
#'
#' @template slot-msAnalysis
#'
#' @references
#' \insertRef{patroon01}{streamFind}
#'
#' \insertRef{proteo01}{streamFind}
#'
#' \insertRef{proteo02}{streamFind}
#'
#' \insertRef{mzr01}{streamFind}
#'
#' \insertRef{mzr02}{streamFind}
#'
#' \insertRef{mzr03}{streamFind}
#'
#' \insertRef{mzr04}{streamFind}
#'
#' @export
#'
#' @importFrom data.table data.table
#'
#' @md
setClass("msFeatures",
         slots = c(
           analyses = "data.table",
           intensity = "data.table",
           metadata = "data.table",
           annotation = "list",
           #IS = "data.table",
           parameters = "list"
         ),
         prototype = list(
           analyses = data.table(),
           intensity = data.table(),
           metadata = data.table(),
           annotation = list(),
           #IS = data.table(),
           parameters = list()
         ),
         validity = msFeatures_validity
)


### S4 methods ----------------------------------------------------------------------------------------------


