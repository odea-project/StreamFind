
#' @title checkOverlapRanges
#'
#' @description Helper function to check overlap ranges in a data.frame for
#' a given numeric vector.
#'
#' @param vals A numeric vector.
#' @param ranges A two columns data.frame with min and max ranges.
#'
#' @return A logical vector with \code{TRUE} for values within the ranges.
#'
#' @export
#'
checkOverlapRanges <- function(vals, ranges) {
  return(rowSums(mapply(function(a, b) between(vals, a, b),
                 ranges$min, ranges$max)) > 0)
}
