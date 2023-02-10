#'
#' @importFrom Rcpp sourceCpp
#'
#' @importFrom utils packageVersion
#'
#' @importFrom Rdpack reprompt
#'
#' @importFrom gtools getDependencies
#'
#' @import R6
#'
#' @importFrom data.table is.data.table rbindlist as.data.table setnames
#' @importFrom data.table setorder copy fread data.table `:=` setcolorder
#' @importFrom data.table between setDTthreads CJ setnafill
#'
#' @importFrom parallelly supportsMulticore
#'
#' @importFrom parallel makeCluster clusterExport detectCores stopCluster
#'
#' @importFrom doParallel registerDoParallel
#'
#' @importFrom foreach foreach `%dopar%` registerDoSEQ
#'
#' @importFrom mzR openMSfile header peaks close chromatogramHeader
#' @importFrom mzR instrumentInfo runInfo nChrom chromatograms
#'
#' @importFrom tools file_ext
#'
#' @importFrom jsonlite toJSON fromJSON
#'
#' @importFrom stats sd
#'
#' @importFrom graphics axis legend lines points polygon
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @importFrom grDevices colorRampPalette colorRamp
#'
#' @importFrom plotly toRGB plot_ly add_trace layout hide_colorbar subplot
#' @importFrom plotly add_segments add_annotations hide_legend add_lines
#' @importFrom plotly group_by
#'
#' @importFrom xml2 read_xml xml_find_first xml_attr xml_find_all xml_name
#' @importFrom xml2 xml_text xml_child
#'
#' @noRd
NULL
