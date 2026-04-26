#' @title StatisticResults_Model Class
#' @description The `StatisticResults_Model` class is used to store results of statistical models, including model parameters, test results, and predictions.
#' @param model A list containing the model parameters.
#' @return An object of class `StatisticResults_Model` which inherits from `Results`.
#' @export
#'
StatisticResults_Model <- function(model = list()) {
  x <- structure(
    list(
      type = "Statistic",
      name = "StatisticResults_Model",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model
    ),
    class = c("StatisticResults_Model", "Results")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid StatisticResults_Model object!")
  }
}

#' @describeIn StatisticResults_Model Validate the StatisticResults_Model object, returns NULL if valid.
#' @param x A `StatisticResults_Model` object.
#' @export
#'
validate_object.StatisticResults_Model <- function(x) {
  checkmate::assert_true(
    all(c("StatisticResults_Model", "Results") %in% class(x))
  )
  checkmate::assert_true(x$software == "StreamFind")
  checkmate::assert_list(x$model)
  NextMethod()
  NULL
}

#' @describeIn StatisticResults_Model Show the StatisticResults_Model object.
#' @param x A `StatisticResults_Model` object.
#' @export
#'
show.StatisticResults_Model <- function(x) {
  cat("Model: ", x$name, "\n")
  cat("Has test :", length(x$test) > 0, "\n")
  cat("Has prediction :", length(x$prediction) > 0, "\n")
}
