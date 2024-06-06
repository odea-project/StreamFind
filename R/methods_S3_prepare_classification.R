
#' @title .s3_prepare_classification.Settings_prepare_classification_knn
#'
#' @description Performs a classification model based on k-Nearest Neighbors using the package \pkg{class}.
#'
#' @noRd
#'
.s3_prepare_classification.Settings_prepare_classification_knn <- function(settings, self, private) {
  
  if (!requireNamespace("class", quietly = TRUE)) {
    warning("The package 'class' is not available! Not done.")
    return(FALSE)
  }
  
  conditions <- list(train_var = "train", label_var = "cl", test_var = "test", args = settings$parameters)
  
  # In this case the fun can be applied as is, but for other algorithms it may be necessary to create a wrapper function
  # For Example in  Hierarchical Clustering
  
  func <- class::knn
  
  self$add_results(list("classification" = list("func" = func, "conditions" = conditions)))
  
  message(paste0("\U2713 ", "KNN classification instructions added!"))
  
  TRUE
}
