#' @param convert Logical, set to \code{TRUE} for converting vendor files to open source formats (e.g., mzML).
#' The default is \code{FALSE} for no conversion even if vendor files are present.
#' @param convert_from The name of the vendor format to be found and converted.
#' Note that MS files are converted to \emph{mzML}.
#' Possible values are: \emph{thermo}, \emph{bruker}, \emph{agilent}, \emph{ab} (from AB Sciex) and \emph{waters}.
#' @param centroid Logical, set to \code{TRUE} to centroid profile data.
#' The default is \code{TRUE}. Note that centroid is only applicable for MS files.
# TODO Make convert files function, independent of the project creation.
