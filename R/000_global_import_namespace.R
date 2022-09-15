#'
#' @importFrom Rcpp sourceCpp
#'
#' @importFrom utils packageVersion
#'
#' @importFrom Rdpack reprompt
#'
#' @importFrom gtools getDependencies
#'
#' @importFrom data.table is.data.table rbindlist as.data.table setnames
#' @importFrom data.table setorder copy fread data.table `:=` setcolorder
#'
#' @importFrom checkmate testClass assertClass testChoice
#'
#' @importClassesFrom patRoon features featureGroups
#' @importClassesFrom patRoon featuresOpenMS featureGroupsOpenMS
#' @importClassesFrom patRoon featureGroupsXCMS3
#' @importClassesFrom patRoon featuresSIRIUS featureGroupsSIRIUS
#' @importClassesFrom patRoon components
#' @importFrom patRoon findFeatures featureTable groupFeatures
#' @importMethodsFrom patRoon as.data.table groupTable getXCMSnExp
#' @importMethodsFrom patRoon generateComponents componentTable
#' @importFrom patRoon importFeatureGroupsXCMS3
#'
#' @importFrom dplyr select left_join full_join semi_join anti_join
#' @importFrom dplyr between everything count
#'
#' @importClassesFrom xcms XCMSnExp PeakGroupsParam ChromPeakAreaParam
#' @importClassesFrom xcms ChromPeakAreaParam FillChromPeaksParam
#' @importMethodsFrom xcms chromPeaks hasFilledChromPeaks fillChromPeaks
#' @importMethodsFrom xcms adjustedRtime processHistory
#' @importMethodsFrom xcms peakGroupsMatrix hasAdjustedRtime
#'
#' @importFrom stringr str_detect str_extract str_split
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
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
#' @importFrom plotly add_segments add_annotations hide_legend
#'
#' @importFrom plyr round_any
#'
#' @importFrom mzR openMSfile header peaks close copyWriteMSData writeMSData
#'
#' @importFrom tools file_ext
#'
#' @noRd
NULL
