# MARK StatisticResults_KNN_class
#' @title StatisticResults_KNN_class Class
#' @description The `StatisticResults_KNN_class` class is used to store the results of a KNN classification model.
#' @param model A list containing the KNN model and its conditions.
#' @export
#'
StatisticResults_KNN_class <- function(model = list()) {
  if (!requireNamespace("class", quietly = TRUE)) {
    warning("The package 'class' is not available! Not done.")
    return(FALSE)
  }
  x <- structure(
    list(
      type = "Statistic",
      name = "StatisticResults_KNN_class",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model
    ),
    class = c(
      "StatisticResults_KNN_class",
      "StatisticResults_Model",
      "Results"
    )
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid StatisticResults_KNN_class object!")
  }
}

#' @describeIn StatisticResults_KNN_class Validate the StatisticResults_KNN_class object, returns NULL if valid.
#' @param x A `StatisticResults_KNN_class` object.
#' @export
#' 
validate_object.StatisticResults_KNN_class <- function(x) {
  checkmate::assert_true(identical(
    class(x),
    c("StatisticResults_KNN_class", "StatisticResults_Model", "Results")
  ))
  checkmate::assert_true(x$name == "StatisticResults_KNN_class")
  checkmate::assert_true(x$software == "StreamFind")
  NextMethod()
  NULL
}

#' @describeIn StatisticResults_KNN_class Get the model data.
#' @param x A StatisticResults_KNN_class object.
#' @export
#'
get_model_data.StatisticResults_KNN_class <- function(x) {
  classif <- data.table::data.table(
    "result" = "model",
    "analysis" = rownames(x$model$conditions$train),
    "class" = x$model$conditions$cl,
    "probability" = 1
  )

  if (length(x$test) > 0) {
    test <- x$test$results
    classif <- data.table::rbindlist(list(classif, test), fill = TRUE)
  }

  if (length(x$prediction) > 0) {
    prediction <- x$prediction$results
    classif <- data.table::rbindlist(list(classif, prediction), fill = TRUE)
  }
  list(
    "classification" = classif
  )
}

#' @describeIn StatisticResults_KNN_class Test new data using the KNN model.
#' @param x A `StatisticResults_KNN_class` object.
#' @param data A data frame containing the new data to test or predict.
#' @export
#' 
test.StatisticResults_KNN_class <- function(x, data) {
  res <- do.call(
    x$model$func,
    c(
      list(
        "train" = x$model$conditions$train,
        "test" = data,
        "cl" = x$model$conditions$cl
      ),
      x$model$conditions$args
    )
  )

  if (!is.null(attr(res, "prob"))) {
    prob <- round(attr(res, "prob"), digits = 1)
  } else {
    prob <- NULL
  }

  test <- data.table::data.table(
    result = "test",
    analysis = rownames(data),
    class = res
  )
  test$probability <- prob
  x$test <- list("results" = test, "data" = data)
  x
}

#' @describeIn StatisticResults_KNN_class Predict new data using the KNN model.
#' @param x A `StatisticResults_KNN_class` object.
#' @param data A data frame containing the new data to test or predict.
#' @export
#' 
predict.StatisticResults_KNN_class <- function(x, data) {
  res <- do.call(
    x$model$func,
    c(
      list(
        "train" = x$model$conditions$train,
        "test" = data,
        "cl" = x$model$conditions$cl
      ),
      x$model$conditions$args
    )
  )

  if (!is.null(attr(res, "prob"))) {
    prob <- round(attr(res, "prob"), digits = 1)
  } else {
    prob <- NULL
  }

  prediction <- data.table::data.table(
    result = "prediction",
    analysis = rownames(data),
    class = res
  )
  prediction$probability <- prob
  x$prediction <- list("results" = prediction, "data" = data)
  x
}
