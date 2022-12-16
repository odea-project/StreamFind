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
#' @importFrom data.table between
#'
#' @importFrom checkmate testClass assertClass testChoice
#'
#' @importFrom dplyr select left_join full_join semi_join anti_join right_join
#' @importFrom dplyr filter everything count bind_rows
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
#' @importFrom tools file_ext file_path_sans_ext
#'
#' @importFrom xml2 read_xml xml_find_first xml_attr xml_find_all xml_name
#' @importFrom xml2 xml_text xml_child
#'
#' @importFrom future plan supportsMulticore
#' @importFrom future.apply future_lapply
#'
#' @importFrom progressr handlers handler_progress  with_progress progressor
#'
#' @importFrom parallelly availableWorkers availableCores
#'
#' @noRd
NULL
