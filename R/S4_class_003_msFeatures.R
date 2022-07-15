

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

#### [ sub-setting analyses ----------------------------------------------

#' @describeIn msFeatures subset on analyses, using analysis index or name.
#'
#' @param i The indice/s or name/s of the analyses to keep in the \code{x} object.
#'
#' @export
#'
setMethod("[", c("msFeatures", "ANY", "missing", "missing"), function(x, i, ...) {

  if (!missing(i)) {
    if (nrow(x@analyses) > 0) {
      if (!is.character(i)) {
        sname <- x@analyses$analysis[i]
        sidx <- i
      } else {
        if (FALSE %in% (i %in% x@analyses$analysis[i])) {
          warning("Given analysis name/s not found in the msFeatures object.")
          return(x)
        }
        sname <- i
        sidx <- which(x@analyses$analysis %in% sname)
      }

      cols_rem <- which(!x@analyses$analysis %in% sname)

      cols_rem <- x@analyses$analysis[cols_rem]

      if (length(cols_rem) > 0) {

        x@analyses <- x@analyses[sidx, ]

        temp_int <- copy(x@intensity)

        temp_int[, (cols_rem) := NULL]

        check_null_intensity <- apply(temp_int[, 2:ncol(temp_int)], 1, function(z) max(z))
        check_null_intensity <- check_null_intensity == 0

        x@intensity <- temp_int[!check_null_intensity, ]

        x@metadata <- x@metadata[!check_null_intensity, ]

        x@metadata$peaks <- lapply(x@metadata$peaks, function(p, sname) {
          p <- p[, !names(p) %in% sname]
          return(p)
        }, sname = sname)

        # TODO check is is really necessary to have the components object, maybe a method to produce the components object can be added
        # if (length(x@annotation) > 0) {
        #   x@annotation[[1]] <- x@annotation[[1]][, which(check_null_intensity)]
        # }
      }
    }
  }
  return(x)
})
