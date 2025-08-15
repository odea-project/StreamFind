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

#' @export
#' @noRd
plot_explained_variance.StatisticResults_Model <- function(
  x,
  interactive = TRUE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  showText = TRUE,
  showLegend = TRUE
) {
  model_data <- get_model_data(x)

  if (is.null(model_data)) {
    return(NULL)
  }

  variance <- model_data$explained_variance

  if (is.null(xLab)) {
    xLab <- "Principle Components"
  }
  if (is.null(yLab)) {
    yLab <- "Explained Variance (%)"
  }

  if (!interactive) {
    if (!requireNamespace("mdatools", quietly = TRUE)) {
      warning("The package 'mdatools' is not available! Not done.")
      return(FALSE)
    }

    mdatools::plotVariance(
      x$model,
      res = x$model$res,
      show.labels = showText,
      show.legend = showLegend
    )
  } else {
    cl <- .get_colors(unique(variance$result))

    fig <- plot_ly()

    fig <- fig %>%
      plotly::add_bars(
        x = variance$pc,
        y = variance$expvar,
        marker = list(size = 10, color = cl[variance$result]),
        name = variance$result,
        showlegend = showLegend
      )

    xaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      dtick = 1
    )

    yaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, 120)
    )

    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)

    fig
  }
}

#' @export
#' @noRd
plot_cumulative_explained_variance.StatisticResults_Model <- function(
  x,
  interactive = TRUE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  showText = TRUE,
  showLegend = TRUE
) {
  model_data <- get_model_data(x)

  if (is.null(model_data)) {
    return(NULL)
  }

  variance <- model_data$cumulative_explained_variance

  if (is.null(xLab)) {
    xLab <- "Principle Components"
  }
  if (is.null(yLab)) {
    yLab <- "Explained Variance (%)"
  }

  if (!interactive) {
    if (!requireNamespace("mdatools", quietly = TRUE)) {
      warning("The package 'mdatools' is not available! Not done.")
      return(FALSE)
    }

    mdatools::plotCumVariance(
      x$model,
      res = x$model$res,
      show.labels = showText,
      show.legend = showLegend
    )
  } else {
    cl <- .get_colors(unique(variance$result))

    fig <- plot_ly()

    fig <- fig %>%
      add_trace(
        x = variance$pc,
        y = variance$expvar,
        type = "scatter",
        mode = "lines+markers",
        line = list(width = 2, color = cl[variance$result]),
        marker = list(size = 10, color = cl[variance$result]),
        name = variance$result,
        showlegend = showLegend
      )

    xaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      dtick = 1
    )

    yaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, 120)
    )

    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)

    fig
  }
}



# MARK: MRCALS
# MCRALS -----
#' @export
#' @noRd
MCRALS <- S7::new_class(
  name = "MCRALS",
  package = "StreamFind",
  parent = S7::new_S3_class("StatisticResults_Model"),
  constructor = function(model = list()) {
    S7::new_object(
      StatisticResults_Model(),
      name = "MCRALS",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model
    )
  },
  validator = function(self) {
    checkmate::assert_true(x$name == "MCRALS")
    checkmate::assert_true(x$software == "StreamFind")
    NULL
  }
)

#' @noRd
S7::method(summary, MCRALS) <- function(object, ...) {
  summary(object$model)
}

#' @noRd
S7::method(predict, MCRALS) <- function(x, data) {
  res <- stats::predict(x$model, data)
  res <- list("rescont" = res)
  colnames(res$rescont) <- colnames(x$model$rescont)
  rownames(res$rescont) <- unique(rownames(data))
  res$data <- data
  x$prediction <- res
  x
}

#' @noRd
S7::method(test, MCRALS) <- function(x, data) {
  res <- stats::predict(x$model, data)
  res <- list("rescont" = res)
  colnames(res$rescont) <- colnames(x$model$rescont)
  rownames(res$rescont) <- unique(rownames(data))
  res$data <- data
  x$test <- res
  x
}

#' @noRd
S7::method(get_model_data, MCRALS) <- function(x) {
  if (length(x$model) == 0) {
    warning("Model not found!")
    return(NULL)
  }

  model <- x$model

  var_expvar <- as.data.frame(model$variance[1, ])
  colnames(var_expvar) <- "expvar"
  var_expvar$pc <- rownames(var_expvar)
  var_expvar$result <- "model"
  var_expvar <- data.table::as.data.table(var_expvar)

  var_expcumvar <- as.data.frame(model$variance[2, ])
  colnames(var_expcumvar) <- "expvar"
  var_expcumvar$pc <- rownames(var_expcumvar)
  var_expcumvar$result <- "model"
  var_expcumvar <- data.table::as.data.table(var_expcumvar)

  resolved <- as.data.frame(model$resspec)
  resolved$result <- "model"

  # attempt check is attr(model$resspec, "features") can be changed to numeric if not keep character
  features <- attr(model$resspec, "features")
  features <- suppressWarnings(as.numeric(features))
  if (any(is.na(features))) {
    features <- seq_len(length(features))
  }
  resolved$feature <- features
  resolved <- data.table::as.data.table(resolved)

  contributions <- as.data.frame(model$rescont)
  contributions$result <- "model"
  contributions$analysis <- attr(model$rescont, "analyses")
  contributions <- data.table::as.data.table(contributions)

  prediction <- model$res$prediction
  if (length(prediction) > 0) {
    prediction_contributions <- as.data.frame(prediction$rescont)
    prediction_contributions$result <- "prediction"
    prediction_contributions$analysis <- rownames(prediction$rescont)
    prediction_contributions <- data.table::as.data.table(
      prediction_contributions
    )
    contributions <- data.table::rbindlist(
      list(contributions, prediction_contributions),
      fill = TRUE
    )
  }

  test <- model$res$test
  if (length(test) > 0) {
    test_contributions <- as.data.frame(test$rescont)
    test_contributions$result <- "test"
    test_contributions$analysis <- rownames(test$rescont)
    test_contributions <- data.table::as.data.table(test_contributions)
    contributions <- data.table::rbindlist(
      list(contributions, test_contributions),
      fill = TRUE
    )
  }

  list(
    "ncomp" = x$model$ncomp,
    "explained_variance" = var_expvar,
    "cumulative_explained_variance" = var_expcumvar,
    "resolved" = resolved,
    "contribution" = contributions
  )
}

#' @export
#' @noRd
S7::method(plot_resolved_spectra, MCRALS) <- function(
  x,
  interactive = TRUE,
  pcs = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  showText = TRUE,
  showLegend = TRUE
) {
  model_data <- get_model_data(x)

  if (is.null(model_data)) {
    return(NULL)
  }

  dt <- model_data$resolved

  if (!is.null(pcs)) {
    checkmate::assert_integerish(pcs)
    dt <- dt[pcs, , drop = FALSE]
  }

  dt <- as.data.frame(dt)

  if (!interactive) {
    if (!requireNamespace("mdatools", quietly = TRUE)) {
      warning("The package 'mdatools' is not available! Not done.")
      return(FALSE)
    }

    mdatools::plotSpectra(x$model)
  } else {
    cl <- .get_colors(colnames(dt)[seq_len(ncol(dt) - 2)])

    fig <- plot_ly()

    xVal <- dt$feature

    if (is.null(xLab)) {
      xLab <- "Variable"
    }
    if (is.null(xLab)) {
      yLab <- "Intensity"
    }

    for (i in seq_len(length(cl))) {
      fig <- fig %>%
        add_trace(
          x = xVal,
          y = dt[[i]],
          type = "scatter",
          mode = "markers+lines",
          line = list(size = 0.3, color = cl[i]),
          marker = list(size = 2, color = cl[i]),
          text = attr(x$model$resspec, "features"),
          hoverinfo = "text",
          name = names(cl[i]),
          legendgroup = names(cl[i]),
          showlegend = TRUE
        )
    }

    xaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )

    yaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )

    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)

    fig
  }
}

#' @export
#' @noRd
S7::method(plot_contributions, MCRALS) <- function(
  x,
  interactive = TRUE,
  pcs = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL
) {
  model_data <- get_model_data(x)

  if (is.null(model_data)) {
    return(NULL)
  }

  dt <- model_data$contribution

  dt <- as.data.frame(dt)

  if (!interactive) {
    if (!requireNamespace("mdatools", quietly = TRUE)) {
      warning("The package 'mdatools' is not available! Not done.")
      return(FALSE)
    }

    mdatools::plotContributions(x$model)
  } else {
    cl <- .get_colors(colnames(dt)[seq_len(ncol(dt) - 2)])

    fig <- plot_ly()

    x = dt$analysis

    if (is.null(xLab)) {
      xLab = "Analysis"
    }

    if (is.null(yLab)) {
      yLab = "Contribution"
    }

    for (i in seq_len(length(cl))) {
      fig <- fig %>%
        add_trace(
          x = x,
          y = dt[[i]],
          type = "scatter",
          mode = "markers+lines",
          line = list(size = 0.3, color = cl[i], dash = 'dash'),
          marker = list(size = 5, color = cl[i]),
          name = names(cl[i]),
          legendgroup = names(cl[i]),
          showlegend = TRUE
        )
    }

    xaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )

    yaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )

    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)

    fig
  }
}

#' @noRd
S7::method(plot, MCRALS) <- function(x, ...) {
  dots <- list(...)

  if ("iteractive" %in% names(dots)) {
    interactive <- dots$interactive
  } else {
    interactive <- TRUE
  }

  if (!interactive) {
    plot(x$model, ...)
  } else {
    plotList <- list()

    plotList[[1]] <- plot_resolved_spectra(x, ...)

    plotList[[2]] <- plot_contributions(x, ...)

    plotList[[3]] <- plot_explained_variance(x, ...)

    plotList[[4]] <- plot_cumulative_explained_variance(x, ...)

    annotations <- list(
      list(
        x = 0.2,
        y = 1.0,
        text = "Resolved Spectra",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.8,
        y = 1,
        text = "Resolved Contributions",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.2,
        y = 0.4,
        text = "Explained Variance",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.8,
        y = 0.4,
        text = "Cumulative Explained Variance",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      )
    )

    fig <- plotly::subplot(
      plotList,
      nrows = 2,
      margin = 0.07,
      titleY = TRUE,
      titleX = TRUE,
      which_layout = "merge"
    )

    fig <- fig %>% plotly::layout(annotations = annotations)

    fig
  }
}

# MARK KNN
# KNN -----
#' @export
#' @noRd
KNN <- S7::new_class(
  name = "KNN",
  package = "StreamFind",
  parent = S7::new_S3_class("StatisticResults_Model"),
  properties = list(),
  constructor = function(model = list()) {
    S7::new_object(
      StatisticResults_Model(),
      name = "KNN",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model
    )
  },
  validator = function(self) {
    checkmate::assert_true(x$name == "KNN")
    checkmate::assert_true(x$software == "StreamFind")
    NULL
  }
)

#' @noRd
S7::method(summary, KNN) <- function(object, ...) {
  NULL
}

#' @noRd
S7::method(plot, KNN) <- function(x, ...) {
  NULL
}

#' @noRd
S7::method(get_model_data, KNN) <- function(x) {
  classif <- data.table::data.table(
    "result" = "model",
    "analysis" = rownames(x$model$conditions$train),
    "class" = x$model$conditions$cl,
    "probability" = 1
  )

  if (x$has_test) {
    test <- x$test$results
    classif <- data.table::rbindlist(list(classif, test), fill = TRUE)
  }

  if (x$has_prediction) {
    prediction <- x$prediction$results
    classif <- data.table::rbindlist(list(classif, prediction), fill = TRUE)
  }

  list(
    "classification" = classif
  )
}

#' @noRd
S7::method(test, KNN) <- function(x, data) {
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

#' @noRd
S7::method(predict, KNN) <- function(x, data) {
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
