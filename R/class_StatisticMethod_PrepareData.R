#' StatisticMethod_PrepareData_autoscale S7 class
#'
#' @title StatisticMethod_PrepareData_autoscale
#' 
#' @description Auto scale and centers data using the \code{prep.autoscale} function from the \pkg{mdatools} package.
#' 
#' @param center Logical (length 1) indicating if the data should be centered.
#' @param scale Logical (length 1) indicating if the data should be scaled.
#' 
#' @return A StatisticMethod_PrepareData_autoscale object.
#'
#' @export
#' 
StatisticMethod_PrepareData_autoscale <- S7::new_class("StatisticMethod_PrepareData_autoscale",
  parent = S7::new_S3_class("ProcessingStep"),
  package = "StreamFind",
  
  constructor = function(center = TRUE, scale = TRUE) {
    
    S7::new_object(
      ProcessingStep(
        data_type = "Statistic",
        method = "PrepareData",
        algorithm = "autoscale",
        parameters = list(
          center = center,
          scale = scale
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "mdatools",
        developer = "Sergey Kucheryavskiy",
        contact = "svk@bio.aau.dk",
        link = "https://github.com/svkucheryavski/mdatools",
        doi = "10.1016/j.chemolab.2020.103937"
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "Statistic")
    checkmate::assert_choice(self@method, "PrepareData")
    checkmate::assert_choice(self@algorithm, "autoscale")
    checkmate::assert_logical(self@parameters$center, max.len = 1)
    checkmate::assert_logical(self@parameters$scale, max.len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, StatisticMethod_PrepareData_autoscale) <- function(x, engine = NULL) {
  
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  
  if (!is(engine, "StatisticEngine")) {
    warning("Engine is not a StatisticEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  mat <- engine$Analyses$analyses
  
  center = x$parameters$center
  scale = x$parameters$scale
  
  mat <- mdatools::prep.autoscale(mat, center = center, scale = scale)
  
  engine$data <- mat
  message(paste0("\U2713 ", "Data auto scaled!"))
  TRUE
}
