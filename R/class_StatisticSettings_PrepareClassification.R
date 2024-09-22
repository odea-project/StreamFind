
# ______________________________________________________________________________________________________________________
# knn -----
# ______________________________________________________________________________________________________________________

#' **StatisticSettings_PrepareClassification_knn**
#'
#' @description Prepares a classification model using the k-nearest neighbors (knn) algorithm from package \pkg{class}.
#' 
#' @param k Integer (length 1) with the number of neighbors to be used.
#' @param l Integer (length 1) with the minimum vote for definite decision, otherwise doubt.
#' (More precisely, less than k-l dissenting votes are allowed, even if k is increased by ties.)
#' 
#' @references
#' \insertRef{class01}{StreamFind}
#' 
#' @return A StatisticSettings_PrepareClassification_knn object.
#'
#' @export
#' 
StatisticSettings_PrepareClassification_knn <- S7::new_class("StatisticSettings_PrepareClassification_knn",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(k = 3, l = 0) {
    
    S7::new_object(ProcessingSettings(
      engine = "Statistic",
      method = "PrepareClassification",
      algorithm = "knn",
      parameters = list(
        k = k,
        l = l,
        prob = TRUE
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "class",
      developer = "Brian D. Ripley",
      contact = "ripley@stats.ox.ac.uk",
      link = "https://cran.r-project.org/web/packages/class/index.html",
      doi = "ISBN 0-387-95457-0"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Statistic"),
      checkmate::test_choice(self@method, "PrepareClassification"),
      checkmate::test_choice(self@algorithm, "knn"),
      checkmate::test_number(self@parameters$k),
      checkmate::test_number(self@parameters$l)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, StatisticSettings_PrepareClassification_knn) <- function(x, engine = NULL) {
  
  if (!is(engine, "StatisticEngine")) {
    warning("Engine is not a StatisticEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
 
  
  
  
}
