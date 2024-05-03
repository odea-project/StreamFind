#' @param mass A vector with target neutral mass value/s (in Da) or a two columns data.table or data.frame named `min` 
#' and `max` with minimum and maximum neutral mass values (in Da), respectively. Alternatively, neutral mass (in Da)
#' and retention time (in seconds) and/or drift time values (in milliseconds) can be given as one data.table or 
#' data.frame with columns named `mass` and `rt` and/or `drift`. Then, the deviations given in the \code{ppm}, \code{sec}
#' and \code{millisec}  arguments are used to calculate the ranges. Also works with a data.table or data.frame with 
#' minimum and maximum values of neutral mass, retention time and drift time targets. In this case, the column names 
#' must be `min`, `max`, `rtmin`, `rtmax`, `driftmin` and `driftmax`. Note that when mass/time ranges are given, the 
#' \code{ppm}, \code{sec} and \code{millisec}
#' arguments are not used.
