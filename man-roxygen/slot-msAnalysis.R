#' @slot analysis A character string with the MS file name the without extension.
#' @slot file A character string with the complete path of the MS file.
#' @slot replicate A character string with the respective sample replicate group name.
#' @slot metadata A list with other information queried from the MS files.
#' The info corresponds to the merged output of \link[mzR]{runInfo} and \link[mzR]{instrumentInfo}
#' from the \pkg{mzR} package.
#' @slot spectra A \link[data.table]{data.table} obtained by
#' the \link[mzR]{peaks} function of the \pkg{mzR} package.
#' Although the function \link[mzR]{peaks} returns a \code{list},
#' the output is collapsed to a \link[data.table]{data.table}.
#' @slot chromatograms A \link[data.table]{data.table} obtained by
#' the \link[mzR]{chromatogram} function of the \pkg{mzR} package.
#' Although the function \link[mzR]{chromatogram} returns a \code{list},
#' the output is collapsed to a \link[data.table]{data.table}.
#' @slot parameters A list with parameter \linkS4class{msSettings} for data processing, such as peak picking.
#' @slot peaks A \link[data.table]{data.table} with feature details, containing the following columns:
#' \enumerate{
#'  \item \strong{id}: character, the identifier of the peak;
#'  \item \strong{rt}: numeric, the calculated retention time (in seconds);
#'  \item \strong{mz}: numeric, the calculated \emph{m/z} value;
#'  \item \strong{intensity}: numeric, the calculated intensity or height (in counts);
#'  \item \strong{area}: numeric, the calculated area of the peak;
#'  \item \strong{rtmin}: numeric, the minimum retention time (in seconds);
#'  \item \strong{rtmax}: numeric, the maximum retention time, (in seconds);
#'  \item \strong{mzmin}: numeric, the minimum \emph{m/z} value;
#'  \item \strong{mzmax}: numeric, the maximum \emph{m/z} value;
#'  \item \strong{adduct}: character, the annotated adduct ion representation of the peak;
#'  \item \strong{isotope}: numeric, the annotated isotopic number;
#'  \item \strong{monoIsotope}: character, the identifier of the mono isotopic ion;
#'  \item \strong{scans}: numeric vector, the scans used for the feature integration;
#'  \item \strong{feature}: character, the identifier of the peaks group (i.e., feature) after
#'  alignment and grouping of corresponding peaks across samples;
#'  \item \strong{...} other columns added from various functions.
#' }
