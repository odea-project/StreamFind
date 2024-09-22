
# ______________________________________________________________________________________________________________________
# autoscale -----
# ______________________________________________________________________________________________________________________

#' **StatisticSettings_PrepareData_autoscale**
#'
#' @title StatisticSettings_PrepareData_autoscale
#' 
#' @description Auto scale and centers data using the \code{prep.autoscale} function from the \pkg{mdatools} package.
#' 
#' @param center Logical (length 1) indicating if the data should be centered.
#' @param scale Logical (length 1) indicating if the data should be scaled.
#' 
#' @return A StatisticSettings_PrepareData_autoscale object.
#'
#' @export
#' 
StatisticSettings_PrepareData_autoscale <- S7::new_class("StatisticSettings_PrepareData_autoscale",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(center = TRUE, scale = TRUE) {
    
    S7::new_object(ProcessingSettings(
      engine = "Statistic",
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
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Statistic"),
      checkmate::test_choice(self@method, "PrepareData"),
      checkmate::test_choice(self@algorithm, "autoscale"),
      checkmate::test_logical(self@parameters$center, max.len = 1),
      checkmate::test_logical(self@parameters$scale, max.len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, StatisticSettings_PrepareData_autoscale) <- function(x, engine = NULL) {
  
  if (!is(engine, "StatisticEngine")) {
    warning("Engine is not a StatisticEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
 
  
  
  
}
