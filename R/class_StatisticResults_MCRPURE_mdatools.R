# MARK: MCRPURE
#' @title StatisticResults_MCRPURE_mdatools Class
#' @description Class for storing results of MCR-PURE models created with the \pkg{mdatools} package.
#' @param model A `mcrpure` object created with the \pkg{mdatools} package.
#' @export
#' 
StatisticResults_MCRPURE_mdatools <- function(model = list()) {
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  x <- structure(
    list(
      type = "Statistic",
      name = "StatisticResults_MCRPURE_mdatools",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model
    ),
    class = c(
      "StatisticResults_MCRPURE_mdatools",
      "StatisticResults_Model",
      "Results"
    )
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid StatisticResults_MCRPURE_mdatools object!")
  }
}

#' @describeIn StatisticResults_MCRPURE_mdatools Validate the object, return NULL if valid.
#' @param x A `StatisticResults_MCRPURE_mdatools` object.
#' @export
#' 
validate_object.StatisticResults_MCRPURE_mdatools <- function(x) {
  checkmate::assert_true(identical(
    class(x),
    c("StatisticResults_MCRPURE_mdatools", "StatisticResults_Model", "Results")
  ))
  checkmate::assert_true(x$name == "StatisticResults_MCRPURE_mdatools")
  checkmate::assert_true(x$software == "StreamFind")
  checkmate::assert_class(x$model, "mcrpure")
  NextMethod()
  NULL
}

#' @describeIn StatisticResults_MCRPURE_mdatools Print a summary of the model.
#' @param object A `StatisticResults_MCRPURE_mdatools` object.
#' @export
#'
summary.StatisticResults_MCRPURE_mdatools <- function(object, ...) {
  summary(object$model)
}

#' @describeIn StatisticResults_MCRPURE_mdatools Plot the explained variance of the MCRPURE model.
#' @param x A `StatisticResults_MCRPURE_mdatools` object.
#' @param ... Additional arguments passed to the plot function.
#' @export
#' 
plot_overview.StatisticResults_MCRPURE_mdatools <- function(x, ...) {
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

#' @describeIn StatisticResults_MCRPURE_mdatools Get the model data as a list of data frames.
#' @param x A StatisticResults_MCRPURE_mdatools object.
#' @export
#' 
get_model_data.StatisticResults_MCRPURE_mdatools <- function(x) {
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
  suppressWarnings(
    resolved$feature <- as.numeric(attr(model$resspec, "features"))
  )
  if (any(is.na(resolved$feature))) {
    resolved$feature <- seq_len(length(attr(model$resspec, "features")))
  }
  resolved <- data.table::as.data.table(resolved)

  contributions <- as.data.frame(model$rescont)
  contributions$result <- "model"
  contributions$analysis <- attr(model$rescont, "analyses")
  contributions <- data.table::as.data.table(contributions)

  purevars <- as.data.frame(model$purevars)
  purevars$vals <- as.numeric(model$purevals)
  colnames(purevars) <- c("vars", "vals")
  purevars$feature <- attr(model$resspec, "features")[purevars$vars]
  purevars$result <- "model"
  purevars <- data.table::as.data.table(purevars)

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
    "contribution" = contributions,
    "purity" = purevars
  )
}

#' @describeIn StatisticResults_MCRPURE_mdatools Predict new data using the MCRPURE model.
#' @param x A `StatisticResults_MCRPURE_mdatools` object.
#' @param data A data frame containing the new data to test or predict.
#' @export
#' 
predict.StatisticResults_MCRPURE_mdatools <- function(x, data) {
  res <- stats::predict(x$model, data)
  res <- list("rescont" = res)
  colnames(res$rescont) <- colnames(x$model$rescont)
  rownames(res$rescont) <- unique(rownames(data))
  res$data <- data
  x$model$res$prediction <- res
  x
}

#' @describeIn StatisticResults_MCRPURE_mdatools Test new data using the MCRPURE model.
#' @param x A `StatisticResults_MCRPURE_mdatools` object.
#' @param data A data frame containing the new data to test or predict.
#' @export
#' 
test.StatisticResults_MCRPURE_mdatools <- function(x, data) {
  res <- stats::predict(x$model, data)
  res <- list("rescont" = res)
  colnames(res$rescont) <- colnames(x$model$rescont)
  rownames(res$rescont) <- unique(rownames(data))
  res$data <- data
  x$model$res$test <- res
  x
}

#' @describeIn StatisticResults_MCRPURE_mdatools Plot the explained variance of the model.
#' @param x A StatisticResults_MCRPURE_mdatools object.
#' @template arg-interactive
#' @template arg-labs
#' @template arg-title
#' @template arg-showText
#' @template arg-showLegend
#' @export
#' 
plot_explained_variance.StatisticResults_MCRPURE_mdatools <- function(
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

#' @describeIn StatisticResults_MCRPURE_mdatools Plot the cumulative explained variance of the model.
#' @param x A StatisticResults_MCRPURE_mdatools object.
#' @template arg-interactive
#' @template arg-labs
#' @template arg-title
#' @template arg-showText
#' @template arg-showLegend
#' @export
#' 
plot_cumulative_explained_variance.StatisticResults_MCRPURE_mdatools <- function(
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

#' @describeIn StatisticResults_MCRPURE_mdatools Plot the resolved spectra of the MCRPURE model.
#' @param x A `StatisticResults_MCRPURE_mdatools` object.
#' @template arg-interactive
#' @param pcs A vector of integers specifying the principal components to plot. If NULL, all components are plotted.
#' @template arg-labs
#' @template arg-showText
#' @template arg-showLegend
#' @export
#' 
plot_resolved_spectra.StatisticResults_MCRPURE_mdatools <- function(
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

#' @describeIn StatisticResults_MCRPURE_mdatools Plot the contributions of the resolved spectra.
#' @param x A `StatisticResults_MCRPURE_mdatools` object.
#' @template arg-interactive
#' @param pcs A vector of integers specifying the principal components to plot. If NULL, all components are plotted.
#' @template arg-labs
#' @template arg-title
#' @export
#' 
plot_contributions.StatisticResults_MCRPURE_mdatools <- function(
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
