
#' @noRd
.process.StatisticSettings_PrepareData_autoscale <- function(settings, self, private) {
  
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  
  mat <- self$data
  
  center = settings$parameters$center
  scale = settings$parameters$scale
  
  mat <- mdatools::prep.autoscale(mat, center = center, scale = scale)
  
  self$data <- mat
  
  message(paste0("\U2713 ", "Data auto scaled!"))
  
  TRUE
}
