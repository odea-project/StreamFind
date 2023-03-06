#' @param plotTargetMark Logical, set to \code{TRUE} to plot a target mark.
#' @param targetsMark A two columns \link[data.table]{data.table} or \link{data.frame} with
#' \emph{m/z} and time targets. The column must be named with "mz" and "rt" for
#' \emph{m/z} and time values, respectively.
#' @param ppmMark A numeric vector of length one to define the mass deviation, in ppm,
#' of the target mark.
#' @param secMark A numeric vector of length one to define the time deviation, in seconds,
#' of the target mark.
#' @param numberRows A numeric vector of length one to define
#' the number of rows to grid the plots.
