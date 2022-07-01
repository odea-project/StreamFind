#' @param mz A vector with target \emph{m/z} values or
#' a two columns data.table or data.frame with minimum and maximum \emph{m/z} values.
#' Alternatively, \emph{m/z} and retention time values can be given as one data.table/data.frame
#' and the deviations given as \code{ppm} and \code{sec} are used to calculate the ranges.
#' The same also works for min and max values of \emph{m/z} and retention time targets.
#' Note that when mass/time ranges are given, \code{ppm} and \code{sec} are not used.
#' @param rt A vector with target retention time values or
#' a two columns data.table/data.frame with minimum and maximum retention time values.
#' @param ppm A numeric vector of length one with the mass deviation, in ppm.
#' @param sec A numeric vector of length one with the time deviation, in seconds.
#' @param id An id vector with target identifiers. When not given
#' is added as a combination of the \emph{m/z} and retention time ranges or values.
