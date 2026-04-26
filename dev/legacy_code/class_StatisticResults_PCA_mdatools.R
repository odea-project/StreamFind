# MARK: StatisticResults_PCA_mdatools
#' @title StatisticResults_PCA_mdatools Class
#' @description The `StatisticResults_PCA_mdatools` class is used to store results of PCA model created with the \pkg{mdatools} package.
#' @param model An object of class `pca` from the \pkg{mdatools} package.
#' @export
#'
StatisticResults_PCA_mdatools <- function(model = list()) {
  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("The package 'mdatools' is not available! Not done.")
    return(FALSE)
  }
  x <- structure(
    list(
      type = "Statistic",
      name = "StatisticResults_PCA_mdatools",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      model = model
    ),
    class = c(
      "StatisticResults_PCA_mdatools",
      "StatisticResults_Model",
      "Results"
    )
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid StatisticResults_PCA_mdatools object!")
  }
}

#' @describeIn StatisticResults_PCA_mdatools Validate the object, returning NULL if valid.
#' @param x A StatisticResults_PCA_mdatools object.
#' @export
#' 
validate_object.StatisticResults_PCA_mdatools <- function(x) {
  checkmate::assert_true(identical(
    class(x),
    c("StatisticResults_PCA_mdatools", "StatisticResults_Model", "Results")
  ))
  checkmate::assert_true(x$name == "StatisticResults_PCA_mdatools")
  checkmate::assert_true(x$software == "StreamFind")
  checkmate::assert_class(x$model, "pca")
  NextMethod()
  NULL
}

#' @describeIn StatisticResults_PCA_mdatools Print a summary of the model.
#' @param object A StatisticResults_PCA_mdatools object.
#' @export
#' 
summary.StatisticResults_PCA_mdatools <- function(object, ...) {
  summary(object$model)
}

#' @describeIn StatisticResults_PCA_mdatools Get the model data as a list of data frames.
#' @param x A StatisticResults_PCA_mdatools object.
#' @export
#' 
get_model_data.StatisticResults_PCA_mdatools <- function(x) {
  if (length(x$model) == 0) {
    warning("Model not found!")
    return(NULL)
  }
  cal <- x$model$res$cal
  var_scores <- as.data.frame(cal$scores)
  var_scores$analysis <- rownames(var_scores)
  var_scores$result <- "model"
  var_scores <- data.table::as.data.table(var_scores)
  var_expvar <- as.data.frame(cal$expvar)
  colnames(var_expvar) <- "expvar"
  var_expvar$pc <- rownames(var_expvar)
  var_expvar$result <- "model"
  var_expvar <- data.table::as.data.table(var_expvar)
  var_expcumvar <- as.data.frame(cal$cumexpvar)
  colnames(var_expcumvar) <- "expvar"
  var_expcumvar$pc <- rownames(var_expcumvar)
  var_expcumvar$result <- "model"
  var_expcumvar <- data.table::as.data.table(var_expcumvar)
  var_residuals <- as.data.frame(cal$residuals)
  var_residuals$analysis <- rownames(var_residuals)
  var_residuals$result <- "model"
  var_residuals <- data.table::as.data.table(var_residuals)
  var_T2 <- as.data.frame(cal$T2)
  var_T2$analysis <- rownames(var_T2)
  var_T2$result <- "model"
  var_T2 <- data.table::as.data.table(var_T2)
  var_Q <- as.data.frame(cal$Q)
  var_Q$analysis <- rownames(var_Q)
  var_Q$result <- "model"
  var_Q <- data.table::as.data.table(var_Q)
  test <- x$model$res$test
  if (length(test) > 0) {
    test_scores <- as.data.frame(test$scores)
    test_scores$analysis <- rownames(test_scores)
    test_scores$result <- "test"
    test_scores <- data.table::as.data.table(test_scores)
    var_scores <- data.table::rbindlist(
      list(var_scores, test_scores),
      fill = TRUE
    )
    test_expvar <- as.data.frame(test$expvar)
    colnames(test_expvar) <- "expvar"
    test_expvar$pc <- rownames(test_expvar)
    test_expvar$result <- "test"
    test_expvar <- data.table::as.data.table(test_expvar)
    var_expvar <- data.table::rbindlist(
      list(var_expvar, test_expvar),
      fill = TRUE
    )
    test_expcumvar <- as.data.frame(test$cumexpvar)
    colnames(test_expcumvar) <- "expvar"
    test_expcumvar$pc <- rownames(test_expcumvar)
    test_expcumvar$result <- "test"
    test_expcumvar <- data.table::as.data.table(test_expcumvar)
    var_expcumvar <- data.table::rbindlist(
      list(var_expcumvar, test_expcumvar),
      fill = TRUE
    )
    test_residuals <- as.data.frame(test$residuals)
    test_residuals$analysis <- rownames(test_residuals)
    test_residuals$result <- "test"
    test_residuals <- data.table::as.data.table(test_residuals)
    var_residuals <- data.table::rbindlist(
      list(var_residuals, test_residuals),
      fill = TRUE
    )
    test_T2 <- as.data.frame(test$T2)
    test_T2$analysis <- rownames(test_T2)
    test_T2$result <- "test"
    test_T2 <- data.table::as.data.table(test_T2)
    var_T2 <- data.table::rbindlist(list(var_T2, test_T2), fill = TRUE)
    test_Q <- as.data.frame(test$Q)
    test_Q$analysis <- rownames(test_Q)
    test_Q$result <- "test"
    test_Q <- data.table::as.data.table(test_Q)
    var_Q <- data.table::rbindlist(list(var_Q, test_Q), fill = TRUE)
  }
  prediction <- x$model$res$prediction
  if (length(prediction) > 0) {
    prediction_scores <- as.data.frame(prediction$scores)
    prediction_scores$analysis <- rownames(prediction_scores)
    prediction_scores$result <- "prediction"
    prediction_scores <- data.table::as.data.table(prediction_scores)
    var_scores <- data.table::rbindlist(
      list(var_scores, prediction_scores),
      fill = TRUE
    )
    prediction_expvar <- as.data.frame(prediction$expvar)
    colnames(prediction_expvar) <- "expvar"
    prediction_expvar$pc <- rownames(prediction_expvar)
    prediction_expvar$result <- "prediction"
    prediction_expvar <- data.table::as.data.table(prediction_expvar)
    var_expvar <- data.table::rbindlist(
      list(var_expvar, prediction_expvar),
      fill = TRUE
    )
    prediction_expcumvar <- as.data.frame(prediction$cumexpvar)
    colnames(prediction_expcumvar) <- "expvar"
    prediction_expcumvar$pc <- rownames(prediction_expcumvar)
    prediction_expcumvar$result <- "prediction"
    prediction_expcumvar <- data.table::as.data.table(prediction_expcumvar)
    var_expcumvar <- data.table::rbindlist(
      list(var_expcumvar, prediction_expcumvar),
      fill = TRUE
    )
    prediction_residuals <- as.data.frame(prediction$residuals)
    prediction_residuals$analysis <- rownames(prediction_residuals)
    prediction_residuals$result <- "prediction"
    prediction_residuals <- data.table::as.data.table(prediction_residuals)
    var_residuals <- data.table::rbindlist(
      list(var_residuals, prediction_residuals),
      fill = TRUE
    )
    prediction_T2 <- as.data.frame(prediction$T2)
    prediction_T2$analysis <- rownames(prediction_T2)
    prediction_T2$result <- "prediction"
    prediction_T2 <- data.table::as.data.table(prediction_T2)
    var_T2 <- data.table::rbindlist(list(var_T2, prediction_T2), fill = TRUE)
    prediction_Q <- as.data.frame(prediction$Q)
    prediction_Q$analysis <- rownames(prediction_Q)
    prediction_Q$result <- "prediction"
    prediction_Q <- data.table::as.data.table(prediction_Q)
    var_Q <- data.table::rbindlist(list(var_Q, prediction_Q), fill = TRUE)
  }
  loadings <- as.data.frame(x$model$loadings)
  loadings$result <- "model"
  loadings$feature <- rownames(loadings)
  loadings <- data.table::as.data.table(loadings)
  list(
    "ncomp" = x$model$ncomp.selected,
    "loadings" = loadings,
    "scores" = var_scores,
    "explained_variance" = var_expvar,
    "cumulative_explained_variance" = var_expcumvar,
    "residuals" = var_residuals,
    "T2" = var_T2,
    "Q" = var_Q
  )
}

#' @describeIn StatisticResults_PCA_mdatools Predict new data using the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @param data A data frame containing the new data to test or predict.
#' @export
#' 
predict.StatisticResults_PCA_mdatools <- function(x, data) {
  res <- stats::predict(x$model, data)
  res$categories <- mdatools::categorize(x$model, res, x$model$ncomp.selected)
  res$data <- data
  x$model$res$prediction <- res
  x
}

#' @describeIn StatisticResults_PCA_mdatools Test new data using the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @param data A data frame containing the new data to test or predict.
#' @export
#' 
test.StatisticResults_PCA_mdatools <- function(x, data) {
  res <- stats::predict(x$model, data)
  res$categories <- mdatools::categorize(x$model, res, x$model$ncomp.selected)
  res$data <- data
  x$model$res$test <- res
  x
}

#' @describeIn StatisticResults_PCA_mdatools Plot the explained variance of the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @template arg-interactive
#' @template arg-labs
#' @template arg-title
#' @template arg-showText
#' @template arg-showLegend
#' @export
#' 
plot_explained_variance.StatisticResults_PCA_mdatools <- function(
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

#' @describeIn StatisticResults_PCA_mdatools Plot the cumulative explained variance of the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @template arg-interactive
#' @template arg-labs
#' @template arg-title
#' @template arg-showText
#' @template arg-showLegend
#' @export
#' 
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

#' @describeIn StatisticResults_PCA_mdatools Plot the scores of the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @template arg-analyses
#' @template arg-interactive
#' @param pcs A vector of integers specifying which principal components to plot (default is 1:2).
#' @template arg-title
#' @template arg-statistic-colorGroups
#' @template arg-showText
#' @template arg-showLegend
#' @template arg-colorBy
#' @export
#' 
plot_scores.StatisticResults_PCA_mdatools <- function(
  x,
  analyses = NULL,
  interactive = TRUE,
  pcs = 1:2,
  title = NULL,
  colorGroups = NULL,
  showText = TRUE,
  showLegend = TRUE,
  colorBy = "results"
) {
  model_data <- get_model_data(x)
  if (is.null(model_data)) {
    return(NULL)
  }
  if (length(pcs) > 2) {
    warning(
      "The number of principle components cannot be larger than 2! Not done."
    )
    return(NULL)
  }
  scores <- model_data$scores
  if (!is.null(analyses)) {
    scores <- scores[scores$analysis %in% analyses, ]
  }
  if (nrow(scores) == 0) {
    warning("No analyses found! Not done.")
    return(NULL)
  }
  if (any(pcs < 1) || any(pcs > ncol(scores) - 1)) {
    warning(
      "The principle components must be in the range ",
      "of the number of components in the model! Not done."
    )
    return(NULL)
  }
  var <- model_data$explained_variance
  if (!interactive) {
    if (!requireNamespace("mdatools", quietly = TRUE)) {
      warning("The package 'mdatools' is not available! Not done.")
      return(FALSE)
    }
    mdatools::plotScores(
      x$model,
      res = x$model$res,
      show.labels = showText,
      show.legend = showLegend,
      cgroup = colorGroups
    )
  } else {
    if (!is.null(colorGroups)) {
      if (length(colorGroups) != nrow(scores)) {
        warning(
          "The color groups must have the same length ",
          "as the number of analyses in the scores! Not done."
        )
        return(NULL)
      }
      colorGroups <- gsub(" ", "_", colorGroups)
      if (
        all(names(colorGroups) %in% scores$analysis) &&
          length(names(colorGroups)) > 0
      ) {
        scores$var_name <- colorGroups[scores$analysis]
        scores$var_name <- factor(
          scores$var_name,
          levels = unique(scores$var_name)
        )
      } else {
        scores$var_name <- as.character(colorGroups)
        scores$var_name <- factor(
          scores$var_name,
          levels = unique(scores$var_name)
        )
      }
      cl <- .get_colors(unique(scores$var_name))
      text <- paste0(scores$analysis, "\n", scores$var_name)
      names(text) <- scores$var_name
    } else {
      if (grepl("analyses", colorBy)) {
        scores$var_name <- paste0(scores$analysis, " - ", scores$result)
      } else {
        scores$var_name <- scores$result
      }
      cl <- .get_colors(unique(scores$var_name))
      text <- paste0(scores$analysis, "\n", scores$result)
      names(text) <- scores$var_name
    }
    if (ncol(scores) == 2) {
      x_val <- seq_len(nrow(scores))
      y_val <- as.numeric(scores[[1]])
      xLab <- "Analysis Index"
      if (!is.null(var)) {
        yLab <- paste0("PC", pcs, " (", round(var[pcs, 1], digits = 0), "%)")
      } else {
        yLab <- paste0("PC", pcs)
      }
    } else {
      x_val <- as.numeric(scores[[pcs[1]]])
      y_val <- as.numeric(scores[[pcs[2]]])
      if (!is.null(var)) {
        xLab <- paste0(
          "PC",
          pcs[1],
          " (",
          round(var[pcs[1], 1], digits = 0),
          "%)"
        )
        yLab <- paste0(
          "PC",
          pcs[2],
          " (",
          round(var[pcs[2], 1], digits = 0),
          "%)"
        )
      } else {
        xLab <- paste0("PC", pcs[1])
        yLab <- paste0("PC", pcs[2])
      }
    }
    if (!showText) {
      text <- NULL
    }
    scores$x_val <- x_val
    scores$y_val <- y_val
    scores[["text"]] <- text
    fig <- plot_ly(
      data = scores,
      x = ~x_val,
      y = ~y_val,
      type = "scatter",
      mode = "markers+text",
      color = ~var_name,
      colors = cl,
      marker = list(size = 10),
      text = scores$text,
      textfont = list(size = 14),
      textposition = "top",
      showlegend = showLegend
    )
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

#' @describeIn StatisticResults_PCA_mdatools Plot the loadings of the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @template arg-interactive
#' @param pcs A vector of integers specifying which principal components to plot (default is 1:2).
#' @param colorKey A vector of colors to use for the loadings (optional).
#' @template arg-title
#' @template arg-showText
#' @template arg-showLegend
#' @export
#' 
plot_loadings.StatisticResults_PCA_mdatools <- function(
  x,
  interactive = TRUE,
  pcs = 1:2,
  colorKey = NULL,
  title = NULL,
  showText = TRUE,
  showLegend = TRUE
) {
  model_data <- get_model_data(x)
  if (is.null(model_data)) {
    return(NULL)
  }
  dt <- model_data$loadings
  if (is.null(dt)) {
    warning("Loadings not found! Not done.")
    return(NULL)
  }
  dt <- as.data.frame(dt)
  if (length(pcs) > 2) {
    warning(
      "The number of principle components cannot be larger than 2! Not done."
    )
    return(NULL)
  }
  if (any(pcs < 1) || any(pcs > ncol(dt) - 1)) {
    warning(
      "The principle components must be in the range of ",
      "the number of components in the model! Not done."
    )
    return(NULL)
  }
  var <- model_data$explained_variance
  if (!interactive) {
    if (!requireNamespace("mdatools", quietly = TRUE)) {
      warning("The package 'mdatools' is not available! Not done.")
      return(FALSE)
    }
    mdatools::plotLoadings(
      x$model,
      res = x$model$res,
      show.labels = showText,
      show.legend = showLegend
    )
  } else {
    if (!is.null(colorKey)) {
      if (length(colorKey) != nrow(dt)) {
        warning(
          "The color key must have the same length as the ",
          "number of variables in the loadings! Not done."
        )
        return(NULL)
      }
      dt$var_name <- as.character(colorKey)
      cl <- .get_colors(colorKey)
    } else {
      dt$var_name <- dt$result
      cl <- .get_colors(unique(dt$result))
    }
    if (ncol(dt) == 1) {
      x <- seq_len(nrow(dt))
      y <- dt[, 1]
      xLab <- "Analysis Index"
      if (!is.null(var)) {
        yLab <- paste0("PC", pcs, "(", round(var[pcs], digits = 0), "%)")
      } else {
        yLab <- paste0("PC", pcs)
      }
    } else {
      x <- dt[[pcs[1]]]
      y <- dt[[pcs[2]]]

      if (!is.null(var)) {
        xLab <- paste0(
          "PC",
          pcs[1],
          " (",
          round(var[pcs[1], 1], digits = 0),
          "%)"
        )
        yLab <- paste0(
          "PC",
          pcs[2],
          " (",
          round(var[pcs[2], 1], digits = 0),
          "%)"
        )
      } else {
        xLab <- paste0("PC", pcs[1])
        yLab <- paste0("PC", pcs[2])
      }
    }
    if (showText) {
      text <- dt$feature
    } else {
      text <- NULL
    }
    fig <- plot_ly()
    fig <- fig %>%
      add_trace(
        x = x,
        y = y,
        type = "scatter",
        mode = "markers+text",
        text = text,
        textfont = list(size = 14, color = cl[dt$var_name]),
        textposition = "top",
        marker = list(size = 10, color = cl[dt$var_name]),
        name = dt$var_name,
        legendgroup = dt$var_name,
        showlegend = showLegend
      )
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

#' @describeIn StatisticResults_PCA_mdatools Plot the residuals of the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @template arg-analyses
#' @template arg-interactive
#' @template arg-labs
#' @template arg-title
#' @template arg-statistic-colorGroups
#' @template arg-showText
#' @template arg-showLegend
#' @template arg-colorBy
#' @export
#' 
plot_residuals.StatisticResults_PCA_mdatools <- function(
  x,
  analyses = NULL,
  interactive = TRUE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorGroups = NULL,
  showText = TRUE,
  showLegend = TRUE,
  colorBy = "results"
) {
  model_data <- get_model_data(x)
  if (is.null(model_data)) {
    return(NULL)
  }
  dt <- model_data$residuals
  if (is.null(dt)) {
    warning("Residuals not found! Not done.")
    return(NULL)
  }
  dt <- as.data.frame(dt)
  if (!is.null(analyses)) {
    dt <- dt[dt$analysis %in% analyses, ]
  }
  if (nrow(dt) == 0) {
    warning("No analyses found! Not done.")
    return(NULL)
  }
  if (is.null(xLab)) {
    xLab <- "Variable Index"
  }
  if (is.null(yLab)) {
    yLab <- "Intensity"
  }
  if (!interactive) {
    NULL
  } else {
    if (!is.null(colorGroups)) {
      if (length(colorGroups) != nrow(dt)) {
        warning(
          "The color groups must have the same length as ",
          "the number of analyses in the scores! Not done."
        )
        return(NULL)
      }
      colorGroups <- gsub(" ", "_", colorGroups)
      dt$var_name <- as.character(colorGroups)
      cl <- .get_colors(unique(colorGroups))
    } else {
      dt$var_name <- paste0(dt$analysis, " - ", dt$result)
      cl <- .get_colors(unique(dt$var_name))
    }
    fig <- plot_ly()
    xVal <- seq_len(ncol(dt) - 3)
    showVarList <- list()
    for (i in seq_len(nrow(dt))) {
      showVar <- is.null(showVarList[[dt$var_name[i]]])
      fig <- fig %>%
        add_trace(
          x = xVal,
          y = unlist(dt[i, xVal]),
          type = "scatter",
          mode = "lines",
          line = list(width = 0.5, color = unname(cl[dt$var_name[i]])),
          hoverinfo = "text",
          text = paste(
            "</br> analysis:  ",
            dt$analysis[i],
            "</br> variable:  ",
            colnames(dt[, xVal]),
            "</br> intensity: ",
            "%{y}"
          ),
          name = dt$var_name[i],
          legendgroup = dt$var_name[i],
          showlegend = showVar
        )
      showVarList[[dt$var_name[i]]] <- FALSE
    }
    xaxis <- list(
      linecolor = plotly::toRGB("black"),
      linewidth = 2,
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = plotly::toRGB("black"),
      linewidth = 2,
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )
    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    fig
  }
}

#' @describeIn StatisticResults_PCA_mdatools Plot residual distances of the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @export
#' 
plot_residual_distance.StatisticResults_PCA_mdatools <- function(x, ...) {
  pc <- NULL
  interactive <- TRUE
  title <- NULL
  showText <- TRUE
  showLegend <- TRUE
  colorBy <- "results"

  dots <- list(...)

  if (length(dots) > 0) {
    if ("pc" %in% names(dots)) {
      pc <- dots$pc
    }
    if ("interactive" %in% names(dots)) {
      interactive <- dots$interactive
    }
    if ("title" %in% names(dots)) {
      title <- dots$title
    }
    if ("showText" %in% names(dots)) {
      showText <- dots$showText
    }
    if ("showLegend" %in% names(dots)) {
      showLegend <- dots$showLegend
    }
  }

  model <- x$model

  if (is.null(model)) {
    warning("Model not found! Not done.")
    return(NULL)
  }

  if (!interactive) {
    if (!requireNamespace("mdatools", quietly = TRUE)) {
      warning("The package 'mdatools' is not available! Not done.")
      return(FALSE)
    }

    return(mdatools::plotResiduals(model, res = model$res))
  }

  res <- model$res

  if (!is.null(pc)) {
    if (length(pc) != 1) {
      warning("The principle component must be a single integer! Not done.")
      return(NULL)
    }

    if (pc < 1 || pc > model$ncomp) {
      warning(
        "The principle component must be in the range of ",
        "the number of components in the model! Not done."
      )
      return(NULL)
    }
  } else {
    pc <- model$ncomp.selected
  }

  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("Package mdatools not found but required! Not done.")
    return(invisible(self))
  }

  for (i in seq_len(length(res))) {
    res[[i]]$categories <- mdatools::categorize(model, res[[i]], pc)
  }

  Qlim <- model$Qlim

  T2lim <- model$T2lim

  lim_data <- mdatools::ldecomp.getLimitsCoordinates(
    Qlim,
    T2lim,
    ncomp = pc,
    norm = TRUE,
    log = FALSE
  )

  plot_data <- lapply(res, function(z) {
    mdatools::plotResiduals(
      z,
      ncomp = pc,
      norm = TRUE,
      log = FALSE,
      show.plot = FALSE
    )
  })

  cat <- lapply(res, function(z) z$categories)

  data <- Map(
    function(i, j, n) {
      if (n == "cal") {
        n <- "model"
      }
      a <- rownames(j)
      j <- data.table::as.data.table(j)
      colnames(j) <- c("h", "q")
      j$result <- n
      j$cat <- i
      j$analysis <- a
      j
    },
    cat,
    plot_data,
    names(plot_data)
  )

  data <- data.table::rbindlist(data)

  if (grepl("analyses", colorBy)) {
    data$var_name <- paste0(data$analysis, " - ", data$result)
  } else {
    data$var_name <- data$result
  }

  cl <- .get_colors(unique(data$var_name))

  if (showText) {
    text <- paste0(data$analysis, "\n", data$result, "\n", data$cat)
  } else {
    NULL
  }

  fig <- plot_ly()

  fig <- fig %>%
    add_trace(
      x = lim_data[[1]][, 1],
      y = lim_data[[1]][, 2],
      type = "scatter",
      mode = "lines",
      line = list(width = 1.5, color = toRGB("orange"), dash = "dash"),
      name = "Extreme Limit",
      legendgroup = "Extreme",
      showlegend = showLegend
    )

  fig <- fig %>%
    add_trace(
      x = lim_data[[2]][, 1],
      y = lim_data[[2]][, 2],
      type = "scatter",
      mode = "lines",
      line = list(width = 1.5, color = toRGB("darkred")),
      name = "Outlier Limit",
      legendgroup = "Outlier Limit",
      showlegend = showLegend
    )

  fig <- fig %>%
    add_trace(
      x = data$h,
      y = data$q,
      type = "scatter",
      mode = "markers+text",
      text = text,
      textfont = list(size = 14, color = cl[data$var_name]),
      textposition = "top",
      marker = list(size = 10, color = cl[data$var_name]),
      name = data$var_name,
      legendgroup = data$var_name,
      showlegend = showLegend
    )

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Score distance (h/h0)",
    titlefont = list(size = 12, color = "black")
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Orthogonal distance (q/q0)",
    titlefont = list(size = 12, color = "black")
  )

  fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
  fig
}

#' @describeIn StatisticResults_PCA_mdatools Plot the explained variance of the PCA model.
#' @param x A StatisticResults_PCA_mdatools object.
#' @param ... Additional arguments passed to the plot function.
#' @export
#' 
plot_overview.StatisticResults_PCA_mdatools <- function(x, ...) {
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

    plotList[[1]] <- plot_scores(x, ...)

    plotList[[2]] <- plot_loadings(x, ...)

    plotList[[3]] <- plot_residual_distance(x, ...)

    plotList[[4]] <- plot_explained_variance(x, ...)

    annotations <- list(
      list(
        x = 0.2,
        y = 1.0,
        text = "Scores",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.8,
        y = 1,
        text = "Loadings",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.2,
        y = 0.4,
        text = "Residual Distances",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.8,
        y = 0.4,
        text = "Explained Variance",
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
