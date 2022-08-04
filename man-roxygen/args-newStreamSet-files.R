#' @param files A character vector with the full path of files to create
#' the analyses set. Alternatively, a \link{data.frame} or
#' \link[data.table]{data.table} with the columns file, replicate and blank
#' corresponding to the file full path, analysis replicate names and
#' respective blank analysis replicate name. When a table is given with
#' replicates and blanks, the arguments \emph{replicates} and  \emph{blanks} are ignored.
#' For files, only formats \emph{mzML} and \emph{mzXML} are currently supported.
