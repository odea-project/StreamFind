#' @slot analyses A \linkS4class{data.table} with the information about the
#' analyses, analysis file paths and  replicates and associated replicate blanks
#' used to created the features.
#' @slot intensity A \linkS4class{data.table} with the a row for each feature
#' with the intensity of each peak in each analyses of the set.
#' @slot metadata A \linkS4class{data.table} with metadata for each feature.
#' @slot annotation ...
#' @slot parameters An ordered list of \linkS4class{settings} for each
#' processing step.
