#'
#' @importFrom Rcpp sourceCpp
#'
#' @importFrom utils packageVersion head
#'
#' @importFrom Rdpack reprompt
#'
#' @importFrom gtools getDependencies
#'
#' @import R6
#'
#' @importFrom data.table is.data.table rbindlist as.data.table setnames
#' @importFrom data.table setorder copy fread data.table `:=` setcolorder
#' @importFrom data.table between setDTthreads CJ setnafill setkey
#'
#' @importFrom parallelly supportsMulticore
#'
#' @importFrom parallel makeCluster clusterExport detectCores stopCluster
#'
#' @importFrom doParallel registerDoParallel
#'
#' @importFrom foreach foreach `%dopar%` registerDoSEQ
#'
#' @importFrom tools file_ext
#'
#' @importFrom dplyr count
#'
#' @importFrom plyr round_any
#' 
#' @importFrom tidyr spread
#'
#' @importFrom jsonlite toJSON fromJSON
#'
#' @importFrom stats sd cor coef nls
#'
#' @importFrom graphics axis legend lines points polygon rect text
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @importFrom methods is
#'
#' @importFrom grDevices colorRampPalette colorRamp
#'
#' @importFrom plotly toRGB plot_ly add_trace layout hide_colorbar subplot
#' @importFrom plotly add_segments add_annotations hide_legend add_lines
#' @importFrom plotly group_by
#'
#' @importFrom checkmate assert_count assert_double assert_choice assert_logical assert_vector assert_number
#' @importFrom checkmate test_count test_double test_choice test_logical test_vector test_number
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' 
#' @importFrom patRoon featureTable
#' 
#' @importFrom baseline baseline
#' 
#' @importFrom pracma findpeaks
#'
#' @noRd
NULL
