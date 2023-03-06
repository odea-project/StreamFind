#' @param file A character vector with the full path of the file to create
#' the analysis object. Alternatively, a \link{data.frame} or
#' \link[data.table]{data.table} with the columns file, replicate and
#' blank corresponding to the file full path, analysis replicate name and
#' respective blank analysis replicate name. When a table is given with
#' replicate and blank, the arguments \emph{replicate} and  \emph{blank}
#' are ignored. For file, only formats \emph{mzML} and \emph{mzXML} are
#' currently supported.
