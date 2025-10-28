# MARK: StatisticAnalyses
#' @title Analyses class and methods for handling statistical analysis data
#' @description The `StatisticAnalyses` class is used for tabular data statistical analysis.
#' @param analyses A `character` vector with full file path to `.csv` file with variable names as first row and analyses names as first column or a `data.frame` or `matrix` object, where the row names are the analyses names and the column names are the variable names.
#' @param classes A `character` vector with the classes of the analyses.
#' @param concentrations A `numeric` vector with the concentrations of the analyses.
#' @template arg-statistic-analyses-dots
#' @return A `StatisticAnalyses` object, which is a list with the following components:
#' \itemize{
#'   \item `analyses`: A `data.frame` or `matrix` with the analyses data, where the row names are the analyses names and the column names are the variable names.
#'   \item `classes`: A `character` vector with the classes of the analyses.
#'   \item `concentrations`: A `numeric` vector with the concentrations of the analyses.
#'   \item `results`: A list with the results of the analyses.
#'   \item `type`: A `character` vector with the type of the analyses, which is `"Statistic"`.
#'   \item `formats`: A `character` vector with the file formats of the analyses, which is `"csv"`.
#' }
#' @export
#'
StatisticAnalyses <- function(
  analyses = NULL,
  classes = character(),
  concentrations = numeric(),
  ...
) {
  if (is.null(analyses)) {
    analyses <- data.frame()
  }
  if (is.character(analyses)) {
    if (tools::file_ext(analyses) == "csv") {
      analyses <- read.csv(analyses, row.names = 1, ...)
    }
  }
  if (is.data.frame(analyses) || is.matrix(analyses)) {
    attr_analyses <- attributes(analyses)[
      -which(names(attributes(analyses)) %in% c("dimnames", "dim"))
    ]
    analyses <- as.data.frame(analyses)
    attributes(analyses) <- c(attributes(analyses), attr_analyses)
    if (nrow(analyses) > 0) {
      if (length(rownames(analyses)) == 0) {
        rownames(analyses) <- paste0("analysis_", seq_len(nrow(analyses)))
      }
      analyses <- analyses[order(rownames(analyses)), ]
    }
  }
  x <- structure(
    list(
      analyses = analyses,
      classes = classes,
      concentrations = concentrations,
      results = list(),
      type = "Statistic",
      formats = DataTypes()$file_formats$Statistic
    ),
    class = c("StatisticAnalyses", "Analyses")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecAnalyses object!")
  }
}

#' @describeIn StatisticAnalyses Validate the StatisticAnalyses object, returns `NULL` if valid.
#' @template arg-statistic-x
#' @export
#'
validate_object.StatisticAnalyses = function(x) {
  checkmate::assert_true(identical(
    x$formats,
    DataTypes()$file_formats$Statistic
  ))
  checkmate::assert_data_frame(x$analyses)
  checkmate::assert_character(rownames(x$analyses), len = nrow(x$analyses))
  checkmate::assert_numeric(as.matrix(x$analyses))
  if (length(x$classes) > 0) {
    checkmate::assert_true(length(x$classes) == length(x))
  }
  if (length(x$concentrations) > 0) {
    checkmate::assert_true(length(x$concentrations) == length(x))
  }
  NextMethod()
  NULL
}

# MARK: Methods
# Methods -----

#' @describeIn StatisticAnalyses Get the names of the analyses, which is the row names of the `analyses` data frame.
#' @template arg-statistic-x
#' @export
#'
get_analysis_names.StatisticAnalyses <- function(x) {
  out <- rownames(x$analyses)
  if (is(x$results[["model"]], "StreamFind::StatisticModel")) {
    has_test <- !is.null(x$results[["model"]]$model$res$test)
    has_prediction <- !is.null(x$results[["model"]]$model$res$prediction)
    if (has_test) {
      out <- c(out, rownames(x$results[["model"]]$model$res$test$data))
    }
    if (has_prediction) {
      out <- c(out, rownames(x$results[["model"]]$model$res$prediction$data))
    }
  }
  out
}

#' @describeIn StatisticAnalyses Get the classes assigned to each analysis.
#' @template arg-statistic-x
#' @export
#'
get_analysis_classes.StatisticAnalyses <- function(x) {
  if (length(x$classes) == 0) {
    warning("No classes found! Returning NULL.")
    return(NULL)
  }
  x$classes
}

#' @describeIn StatisticAnalyses Set the classes of the analyses, where the argument `value` must be a character vector with the same length as the number of analyses.
#' @template arg-statistic-x
#' @template arg-value
#' @export
#'
set_analysis_classes.StatisticAnalyses <- function(x, value) {
  if (length(value) != nrow(x$analyses)) {
    warning(
      "The length of the classes must be equal to the number of analyses! Not done."
    )
    return(x)
  }
  if (!is.character(value)) {
    warning("The classes must be character! Not done.")
    return(x)
  }
  x$classes <- value
  x
}

#' @describeIn StatisticAnalyses Get the concentration values of each analysis.
#' @template arg-statistic-x
#' @export
#'
get_concentrations.StatisticAnalyses <- function(x) {
  if (length(x$concentrations) == 0) {
    warning("No concentrations found! Returning NULL.")
    return(NULL)
  }
  x$concentrations
}

#' @describeIn StatisticAnalyses Set the concentrations of the analyses, where the argument `value` must be a numeric vector with the same length as the number of analyses.
#' @template arg-statistic-x
#' @template arg-value
#' @export
#'
set_concentrations.StatisticAnalyses <- function(x, value) {
  if (length(value) != nrow(x$analyses)) {
    warning(
      "The length of the concentrations must be equal to the number of analyses! Not done."
    )
    return(x)
  }
  if (!is.numeric(value)) {
    warning("The concentrations must be numeric! Not done.")
    return(x)
  }
  x$concentrations <- value
  x
}

# MARK: info
#' @describeIn StatisticAnalyses Get a data frame with the analyses, features, classes and concentrations.
#' @template arg-statistic-x
#' @export
#'
info.StatisticAnalyses <- function(x) {
  if (length(x$analyses) > 0) {
    analyses_names <- get_analysis_names(x)
    df <- data.frame(
      "analysis" = analyses_names,
      "features" = ncol(x$analyses)
    )
    if (length(x$classes) > 0) {
      df <- cbind(
        df,
        "class" = c(
          x$classes,
          rep(NA_character_, nrow(x$analyses) - length(x$classes))
        )
      )
    }
    if (length(x$concentrations) > 0) {
      df <- cbind(
        df,
        "concentration" = c(
          x$concentrations,
          rep(NA_real_, nrow(x$analyses) - length(x$concentrations))
        )
      )
    }
    row.names(df) <- seq_len(length(analyses_names))
    df
  } else {
    data.frame()
  }
}

# MARK: add
#' @describeIn StatisticAnalyses Add new analyses to the `StatisticAnalyses` object, where the argument `value` must be a csv file or directly a data frame with the same column names as the existing analyses.
#' @template arg-statistic-x
#' @template arg-value
#' @template arg-statistic-analyses-dots
#' @export
#'
add.StatisticAnalyses <- function(x, value, ...) {
  if (is.character(value)) {
    if (tools::file_ext(value) == "csv") {
      value <- read.csv(value, row.names = 1, ...)
    }
  }
  if (is.data.frame(value) || is.matrix(value)) {
    value <- as.data.frame(value)
    if (ncol(x$analyses) != ncol(value)) {
      warning("Number of columns must be equal to analyses present!")
      return(x)
    }
    if (!identical(colnames(x$analyses), colnames(value))) {
      warning("Column names must be equal to analyses present!")
      return(x)
    }
    if (nrow(value) > 0) {
      if (length(rownames(value)) == 0) {
        rownames(value) <- paste0(
          "analysis_",
          seq(nrow(x$analyses) + 1, nrow(x$analyses) + nrow(value), by = 1)
        )
      }
    }
    if (any(rownames(value) %in% rownames(x$analyses))) {
      warning("Some analysis names are already in the analyses! Not done.")
      return(x)
    }
    x$analyses <- rbind(x$analyses, value)
    x$analyses <- x$analyses[order(rownames(x$analyses)), ]
    if (length(x$results) > 0) {
      warning("Results removed as new analyses were added!")
      x$results <- list()
    }
  }
  if (is.null(validate_object(x))) {
    message(paste0("\U2713 ", "Analyses added!"))
    return(x)
  } else {
    stop("Invalid StatisticAnalyses object!")
  }
}

# MARK: remove
#' @describeIn StatisticAnalyses Remove analyses from the `StatisticAnalyses` object, where the argument `value` can be a character vector with the names of the analyses or a numeric vector with the indices of the analyses to remove.
#' @template arg-statistic-x
#' @template arg-value
#' @export
#'
remove.StatisticAnalyses <- function(x, value) {
  if (is.character(value)) {
    if (value %in% names(x)) {
      x$analyses <- x$analyses[-which(rownames(x$analyses) == value), ]
      if (length(x$results) > 0) {
        warning("Results removed as analyses were removed!")
        x$results <- list()
      }
    }
  }
  if (is.numeric(value)) {
    if (all(value <= nrow(x$analyses))) {
      x$analyses <- x$analyses[-value, ]
      if (length(x$results) > 0) {
        warning("Results removed as analyses were removed!")
        x$results <- list()
      }
    }
  }
  x
}

# MARK: `[<-`
#' @describeIn StatisticAnalyses Add new analyses to the `StatisticAnalyses` object, where the argument `value` must be a csv file or directly a data frame with the same column names as the existing analyses.
#' @template arg-statistic-x
#' @template arg-i
#' @template arg-value
#' @export
#'
`[<-.StatisticAnalyses` <- function(x, i, value) {
  x <- add(x, value)
  x
}

# MARK: `[[<-`
#' @describeIn StatisticAnalyses Add new analyses to the `StatisticAnalyses` object, where the argument `value` must be a csv file or directly a data frame with the same column names as the existing analyses.
#' @template arg-statistic-x
#' @template arg-i
#' @template arg-value
#' @export
#'
`[[<-.StatisticAnalyses` <- function(x, i, value) {
  x <- add(x, value)
  x
}

# MARK: plot_data
#' @describeIn StatisticAnalyses Plot the data of the `StatisticAnalyses` object.
#' @template arg-statistic-x
#' @template arg-analyses
#' @template arg-statistic-features
#' @template arg-statistic-transpose
#' @template arg-interactive
#' @template arg-labs
#' @template arg-title
#' @template arg-statistic-colorGroups
#' @template arg-statistic-xTickLabelsShow
#' @export
#'
plot_data.StatisticAnalyses <- function(
  x,
  analyses = NULL,
  features = NULL,
  transpose = FALSE,
  interactive = TRUE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorGroups = NULL,
  xTickLabelsShow = TRUE
) {
  mat <- x$analyses
  if (!is.null(analyses)) mat <- mat[analyses, ]
  if (nrow(mat) == 0) {
    warning("Analyses not found! Not done.")
    return(NULL)
  }
  if (!is.null(features)) {
    if (!is.numeric(features)) {
      warning("The features must be numeric! Not done.")
      return(NULL)
    }
    mat <- mat[, features, drop = FALSE]
  }
  if (transpose) {
    mat <- as.matrix(t(mat))
    if (is.null(xLab)) {
      xLab <- "Analysis Index"
    }
    if (is.null(yLab)) yLab <- "Intensity"
  } else {
    if (is.null(xLab)) {
      xLab <- "Variable Index"
    }
    if (is.null(yLab)) yLab <- "Intensity"
  }

  if (!interactive) {
    cl <- .get_colors(rownames(mat))
    plot(
      seq_len(ncol(mat)),
      mat[1, ],
      type = "l",
      col = unname(cl[1]),
      xlab = xLab,
      ylab = yLab,
      main = title,
      ylim = range(mat)
    )
    for (i in 2:nrow(mat)) {
      lines(seq_len(ncol(mat)), mat[i, ], col = unname(cl[i]))
    }
    legend(
      "topright",
      legend = rownames(mat),
      col = unname(cl),
      lty = 1,
      cex = 0.8
    )
  } else {
    if (!is.null(colorGroups)) {
      if (length(colorGroups) != nrow(mat)) {
        warning(
          "The color groups must have the same length as the number of analyses!"
        )
        return(NULL)
      }
      colorGroups <- gsub(" ", "_", colorGroups)
      var_name <- colorGroups
      cl <- .get_colors(unique(colorGroups))
    } else {
      var_name <- rownames(mat)
      cl <- .get_colors(unique(rownames(mat)))
    }
    how_leg <- rep(TRUE, length(cl))
    names(how_leg) <- names(cl)
    fig <- plotly::plot_ly()
    if (is.null(attr(mat, "xValues"))) {
      xVal <- seq_len(ncol(mat))
    } else {
      xVal <- attr(mat, "xValues")
      xVal <- factor(xVal, levels = xVal)
    }
    for (i in seq_len(nrow(mat))) {
      fig <- fig %>%
        plotly::add_trace(
          x = xVal,
          y = unlist(mat[i, ]),
          type = "scatter",
          mode = "lines",
          line = list(width = 0.5, color = unname(cl[var_name[i]])),
          text = paste0(
            "Analysis: ",
            rownames(mat)[i],
            "<br>",
            "Variable: ",
            xVal,
            "<br>",
            "Intensity: ",
            unlist(mat[i, ])
          ),
          hoverinfo = "text",
          name = var_name[i],
          legendgroup = var_name[i],
          showlegend = how_leg[var_name[i]]
        )
      how_leg[var_name[i]] <- FALSE
    }
    xaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      showticklabels = xTickLabelsShow
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
