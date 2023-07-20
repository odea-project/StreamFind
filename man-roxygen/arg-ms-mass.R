#' @param mass A vector with target neutral mass value/s (in Da) or a two columns
#' data.table or data.frame named `min` and `max` with minimum and maximum
#' neutral mass values (in Da), respectively. Alternatively, neutral mass (in Da)
#' and retention time values (in seconds) can be given as one data.table or
#' data.frame with columns named `mass` and `rt`. Then, the deviations given in
#' the \code{ppm} and \code{sec} arguments are used to calculate the ranges.
#' Also works with a data.table or data.frame with minimum and maximum values of
#' neutral mass and retention time targets. In this case, the column names must
#' be `min`, `max`, `rtmin` and `rtmax`. Note that when mass/time ranges
#'  are given, the \code{ppm} and \code{sec} arguments are not used.
