#' @param mz A vector with target \emph{m/z} value/s (in Da) or a two columns data.table or data.frame named `mzmin` and
#' `mzmax` with minimum and maximum \emph{m/z} values (in Da), respectively. Alternatively, \emph{m/z} (in Da) and 
#' retention time values (in seconds) can be given as one data.table or data.frame with columns named `mz` and `rt` 
#' and/or `drift`. Then, the deviations given in the \code{ppm}, \code{sec} and \code{millisec} arguments are used to 
#' calculate the ranges. Also works with a data.table or data.frame with minimum and maximum values of \emph{m/z}, 
#' retention time and drift time targets. In this case, the column names must be `mzmin`, `mzmax`, `rtmin`, `rtmax`, 
#' `driftmin` and `driftmax`. Note that when mass/time ranges are given, the \code{ppm}, \code{sec} and \code{millisec} 
#' arguments are not used.
