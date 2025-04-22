# MARK: StatisticAnalyses
# StatisticAnalyses -----
#' @title Statistical Analyses (Tabular Data Input)
#' 
#' @description The StatisticAnalyses class is used for statistical analysis of tabular data.
#' 
#' @param analyses A `character` vector with full file path to `.csv` file with variable names as
#' first row and analyses names as first column or a `data.frame` or `matrix` object.
#' @param classes A `character` vector with the classes of the analyses.
#' @param concentrations A `numeric` vector with the concentrations of the analyses.
#' @param ... Additional arguments passed to the constructor.
#' 
#' @slot analyses A `data.frame` with the analyses data.
#' @slot type A `character` vector with the type of analyses.
#' @slot classes A `character` vector with the classes of the analyses.
#' @slot concentrations A `numeric` vector with the concentrations of the analyses.
#' @slot info (getter) A `data.frame` with the information of the analyses.
#' @slot number_variables (getter) A `numeric` with the number of variables in the analyses.
#' @slot has_data (getter) A `logical` indicating if the analyses has data.
#' @slot data (getter/setter) A `list` with the data of the analyses.
#' @slot has_model (getter) A `logical` indicating if the analyses has a model.
#' @slot model (getter/setter) A `list` with the model of the analyses.
#' @slot has_test (getter) A `logical` indicating if the analyses has a test.
#' @slot test (getter) A `list` with the test of the analyses.
#' @slot has_prediction (getter) A `logical` indicating if the analyses has a prediction.
#' @slot prediction (getter) A `list` with the prediction of the analyses.
#' @slot has_quantification (getter) A `logical` indicating if the analyses has a quantification.
#' @slot quantification (getter/setter) A `list` with the quantification of the analyses.
#' 
#' @export
#' 
StatisticAnalyses <- S7::new_class(
  name = "StatisticAnalyses",
  package = "StreamFind",
  parent = Analyses,
  properties = list(

    # MARK: analyses
    ## analyses -----
    analyses = S7::new_property(S7::class_data.frame, default = data.frame()),

    # MARK: type
    ## type -----
    type = S7::new_property(S7::class_character, getter = function(self) {
      out <- rep("model", nrow(self$analyses))
      if (is(self$results[["model"]], "StreamFind::StatisticModel")) {
        has_test <- !is.null(self$results[["model"]]$model$res$test$data)
        has_prediction <- !is.null(self$results[["model"]]$model$res$prediction$data)
        if (has_test) out <- c(out, rep("test", nrow(self$results[["model"]]$model$res$test$data)))
        if (has_prediction) {
          out <- c(out, rep("prediction", nrow(self$results[["model"]]$model$res$prediction$data)))
        }
      }
      names(out) <- names(self)
      out
    }),

    # MARK: classes
    ## classes -----
    classes = S7::new_property(S7::class_character, default = character()),

    # MARK: concentrations
    ## concentrations -----
    concentrations = S7::new_property(S7::class_numeric, default = numeric(0)),

    # MARK: info
    ## info -----
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (length(self) > 0) {
        analyses_names <- names(self)
        df <- data.frame(
          "analysis" = analyses_names,
          "type" = self@type,
          "features" = ncol(self@analyses)
        )

        if (length(self@classes) > 0) {
          df <- cbind(
            df,
            "class" = c(self@classes, rep(NA_character_, length(self@type) - length(self@classes)))
          )
        }

        if (length(self@concentrations) > 0) {
          df <- cbind(
            df,
            "concentration" = c(
              self@concentrations,
              rep(NA_real_, length(self@type) - length(self@concentrations))
            )
          )
        }
        row.names(df) <- seq_len(length(analyses_names))
        df
      } else {
        data.frame()
      }
    }, default = data.frame()),

    # MARK: number_variables
    ## number_variables -----
    number_variables = S7::new_property(S7::class_numeric, getter = function(self) {
      if (length(self) == 0) {
        return(0)
      }
      ncol(self@analyses)
    }),

    # MARK: has_data
    ## has_data -----
    has_data = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) {
        return(FALSE)
      }
      if (is.null(self@results[["DataFrame"]])) {
        if (nrow(self$analyses) == 0) {
          return(FALSE)
        }
      }
      if (!is(self@results[["DataFrame"]], "StreamFind::DataFrame")) {
        return(FALSE)
      }
      TRUE
    }),

    # MARK: data
    ## data -----
    data = S7::new_property(S7::class_list,
      getter = function(self) {
        if (!is.null(self@results[["DataFrame"]])) {
          data <- self@results[["DataFrame"]]
        } else {
          data <- StreamFind::DataFrame(data = self$analyses)
        }
        if (self$has_model) {
          if (self$has_test) data$data <- rbind(data$data, self$test$data)
          if (self$has_prediction) data$data <- rbind(data$data, self$prediction$data)
        }
        data
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::DataFrame")) {
          self@results[["results"]] <- list()
          self@results[[value@name]] <- value
        } else {
          warning("Value must be a Data results object! Not done.")
        }
        self
      }
    ),

    # MARK: has_model
    ## has_model -----
    has_model = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) {
        return(FALSE)
      }
      if (length(self@results) == 0) {
        return(FALSE)
      }
      if (!any(vapply(self@results, function(x) is(x, "StreamFind::StatisticModel"), FALSE))) {
        return(FALSE)
      }
      TRUE
    }),

    # MARK: model
    ## model -----
    model = S7::new_property(S7::class_list,
      getter = function(self) {
        if (self$has_model) {
          model_sel <- vapply(self@results, function(x) is(x, "StreamFind::StatisticModel"), FALSE)
          model <- self@results[model_sel]
          if (length(model) > 1) {
            warning("More than one model found, returning the first one!")
          }
          return(model[[1]])
        }
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::StatisticModel")) {
          self@results[[value@name]] <- value
        } else {
          warning("Value must be a Model results object! Not done.")
        }
        self
      }
    ),

    # MARK: has_test
    ## has_test -----
    has_test = S7::new_property(S7::class_logical, getter = function(self) {
      if (!self$has_model) {
        return(FALSE)
      }
      if (is.null(self@results[["model"]]$model$res$test)) {
        return(FALSE)
      }
      TRUE
    }),

    # MARK: test
    ## test -----
    test = S7::new_property(S7::class_list, getter = function(self) {
      if (self$has_test) {
        return(self@results[["model"]]$model$res$test)
      }
      NULL
    }),

    # MARK: has_prediction
    ## has_prediction -----
    has_prediction = S7::new_property(S7::class_logical, getter = function(self) {
      if (!self$has_model) {
        return(FALSE)
      }
      if (is.null(self@results[["model"]]$model$res$prediction)) {
        return(FALSE)
      }
      TRUE
    }),

    # MARK: prediction
    ## prediction -----
    prediction = S7::new_property(S7::class_list, getter = function(self) {
      if (self$has_prediction) {
        return(self@results[["model"]]$model$res$prediction)
      }
      NULL
    }),

    # MARK: has_quantification
    ## has_quantification -----
    has_quantification = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) {
        return(FALSE)
      }
      if (is.null(self@results[["quantification"]])) {
        return(FALSE)
      }
      if (!is(self@results[["quantification"]], "StreamFind::Quantification")) {
        return(FALSE)
      }
      TRUE
    }),

    # MARK: quantification
    ## quantification -----
    quantification = S7::new_property(S7::class_list,
      getter = function(self) {
        if (self$has_quantification) {
          return(self@results[["quantification"]])
        }
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::Quantification")) {
          self@results[["quantification"]] <- value
        } else {
          warning("Value must be a Quantification results object! Not done.")
        }
        self
      }
    )
  ),

  # MARK: constructor
  ## constructor -----
  constructor = function(analyses = NULL, classes = character(), concentrations = numeric(), ...) {
    if (is.null(analyses)) analyses <- data.frame()

    if (is.character(analyses)) {
      if (tools::file_ext(analyses) == "csv") {
        analyses <- read.csv(analyses, row.names = 1, ...)
      }
    }

    if (is.data.frame(analyses) || is.matrix(analyses)) {
      attr_analyses <- attributes(analyses)[-which(names(attributes(analyses)) %in% c("dimnames", "dim"))]
      analyses <- as.data.frame(analyses)
      attributes(analyses) <- c(attributes(analyses), attr_analyses)
      if (nrow(analyses) > 0) {
        if (length(rownames(analyses)) == 0) {
          rownames(analyses) <- paste0("analysis_", seq_len(nrow(analyses)))
        }

        analyses <- analyses[order(rownames(analyses)), ]
      }
    }

    S7::new_object(
      Analyses(),
      possible_formats = ".csv",
      analyses = analyses,
      classes = classes,
      concentrations = concentrations
    )
  },

  # MARK: validator
  ## validator -----
  validator = function(self) {
    checkmate::assert_true(identical(self@possible_formats, ".csv"))
    checkmate::assert_data_frame(self@analyses)
    checkmate::assert_character(rownames(self@analyses), len = nrow(self@analyses))
    checkmate::assert_numeric(as.matrix(self@analyses))
    if (length(self@classes) > 0) checkmate::assert_true(length(self@classes) == length(self))
    if (length(self@concentrations) > 0) checkmate::assert_true(length(self@concentrations) == length(self))
    NULL
  }
)

# MARK: Methods
# Methods -----

# MARK: names
## names -----
#' @noRd
S7::method(names, StatisticAnalyses) <- function(x) {
  out <- rownames(x$analyses)
  if (is(x$results[["model"]], "StreamFind::StatisticModel")) {
    has_test <- !is.null(x$results[["model"]]$model$res$test)
    has_prediction <- !is.null(x$results[["model"]]$model$res$prediction)
    if (has_test) out <- c(out, rownames(x$results[["model"]]$model$res$test$data))
    if (has_prediction) out <- c(out, rownames(x$results[["model"]]$model$res$prediction$data))
  }
  out
}

# MARK: length
## length -----
#' @export
#' @noRd
S7::method(length, StatisticAnalyses) <- function(x) {
  nrow(x@analyses)
}

# MARK: add
## add -----
#' @export
#' @noRd
S7::method(add, StatisticAnalyses) <- function(x, value) {
  if (is.character(value)) {
    if (tools::file_ext(value) == "csv") {
      value <- read.csv(value, row.names = 1, ...)
    }
  }

  if (is.data.frame(value) || is.matrix(value)) {
    value <- as.data.frame(value)

    if (ncol(x@analyses) != ncol(value)) {
      warning("Number of columns must be equal to analyses present!")
      return(x)
    }

    if (!identical(colnames(x@analyses), colnames(value))) {
      warning("Column names must be equal to analyses present!")
      return(x)
    }

    if (nrow(value) > 0) {
      if (length(rownames(value)) == 0) {
        rownames(value) <- paste0(
          "analysis_", seq(nrow(x@analyses) + 1, nrow(x@analyses) + nrow(value), by = 1)
        )
      }
    }

    x@analyses <- rbind(x@analyses, value)
    x@analyses <- x@analyses[order(rownames(x@analyses)), ]
    x@results <- list()
  }
  x
}

# MARK: remove
## remove -----
#' @export
#' @noRd
S7::method(remove, StatisticAnalyses) <- function(x, value) {
  if (is.character(value)) {
    if (value %in% names(x)) {
      x@analyses <- x@analyses[-which(rownames(x@analyses) == value), ]
      x@results <- list()
    }
  }
  if (is.numeric(value)) {
    if (all(value <= nrow(x@analyses))) {
      x@analyses <- x@analyses[-value, ]
      x@results <- list()
    }
  }
  x
}

# MARK: `[`
## `[` -----

#' @export
#' @noRd
`[.StreamFind::StatisticAnalyses` <- function(x, i) {
  if (is.character(i)) {
    x@analyses <- x@analyses[rownames(x@analyses) %in% i, ]
    x@results <- list()
  } else if (is.numeric(i)) {
    x@analyses <- x@analyses[i, ]
    x@results <- list()
  } else {
    warning("Index must be character or numeric!")
  }
  x
}

# MARK: `[<-`
## `[<-` -----
#' @export
#' @noRd
`[<-.StreamFind::StatisticAnalyses` <- function(x, i, value) {
  x <- add(x, value)
  x
}

# MARK: `[[`
## `[[` -----
#' @export
#' @noRd
`[[.StreamFind::StatisticAnalyses` <- function(x, i) {
  if (is.character(i)) {
    x@analyses <- x@analyses[rownames(x@analyses) %in% i, ]
    x@results <- list()
  } else if (is.numeric(i)) {
    x@analyses <- x@analyses[i, ]
    x@results <- list()
  } else {
    warning("Index must be character or numeric!")
  }
  x
}

# MARK: `[[<-`
## `[[<-` -----
#' @export
#' @noRd
`[[<-.StreamFind::StatisticAnalyses` <- function(x, i, value) {
  x <- add(x, value)
  x
}

# MARK: predict
## predict -----
#' @export
#' @noRd
S7::method(predict, StatisticAnalyses) <- function(x, data) {
  if (!is.data.frame(data) && !is(data, "data.table") && !is.matrix(data)) {
    warning("The data must be a data.frame, data.table or matrix! Not done.")
    return(x)
  }

  if (nrow(data) == 0) {
    warning("The data must not be empty! Not done.")
    return(x)
  }

  if (!all(vapply(data, is.numeric, FALSE))) {
    warning("The data must be numeric! Not done.")
    return(x)
  }

  if (is(data, "data.table") || is(data, "data.frame")) data <- as.data.frame(data)

  names <- rownames(data)

  if (is.null(names)) {
    names <- paste0("analysis_", seq_len(nrow(data)) + length(x))
  } else {
    if (any(names %in% names(x))) {
      warning("Some analysis names are already in the analyses! Not done.")
      return(x)
    }
  }

  if (is.null(x$model)) {
    warning("Model not found! Not done.")
    return(x)
  }

  if (ncol(data) != x$number_variables) {
    warning(
      "The number of variables in the data must be equal to the
      number of variables in the model! Not done."
    )
    return(x)
  }

  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("Package mdatools not found but required! Not done.")
    return(x)
  }

  x$model <- predict(x$model, data)
  message(paste0("\U2713 ", "Predicted results added!"))
  x
}

# MARK: test
## test -----
#' @export
#' @noRd
S7::method(test, StatisticAnalyses) <- function(x, data) {
  if (!is.data.frame(data) && !is(data, "data.table") && !is.matrix(data)) {
    warning("The data must be a data.frame, data.table or matrix! Not done.")
    return(x)
  }

  if (nrow(data) == 0) {
    warning("The data must not be empty! Not done.")
    return(x)
  }

  if (!all(vapply(data, is.numeric, FALSE))) {
    warning("The data must be numeric! Not done.")
    return(x)
  }

  if (is(data, "data.table") || is(data, "data.frame")) data <- as.data.frame(data)

  names <- rownames(data)

  if (is.null(names)) {
    names <- paste0("analysis_", seq_len(nrow(data)) + length(x))
  } else {
    if (any(names %in% names(x))) {
      warning("Some analysis names are already in the analyses! Not done.")
      return(x)
    }
  }

  if (is.null(x$model)) {
    warning("Model not found! Not done.")
    return(x)
  }

  if (ncol(data) != x$number_variables) {
    warning(
      "The number of variables in the data must be equal
      to the number of variables in the model! Not done."
    )
    return(x)
  }

  if (!requireNamespace("mdatools", quietly = TRUE)) {
    warning("Package mdatools not found but required! Not done.")
    return(x)
  }

  x$model <- test(x$model, data)
  message(paste0("\U2713 ", "Test results added!"))
  x
}

# MARK: Get Methods
# Get Methods -----

# MARK: get_model_data
## get_model_data -----
#' @export
#' @noRd
S7::method(get_model_data, StatisticAnalyses) <- function(x) {
  if (!x$has_model) {
    warning("Model not found! Not done.")
    return(NULL)
  }
  get_model_data(x$model)
}

# MARK: Plot Methods
# Plot Methods -----

# MARK: plot_data
## plot_data -----
#' @export
#' @noRd
S7::method(plot_data, StatisticAnalyses) <- function(x,
                                                     analyses = NULL,
                                                     features = NULL,
                                                     transpose = FALSE,
                                                     interactive = TRUE,
                                                     xLab = NULL,
                                                     yLab = NULL,
                                                     title = NULL,
                                                     colorGroups = NULL,
                                                     xTickLabelsShow = TRUE) {
  mat <- x$data$data

  if (!is.null(analyses)) mat <- mat[analyses, , drop = FALSE]

  if (nrow(mat) == 0) {
    warning("Analyses not found! Not done.")
    return(NULL)
  }

  if (x$has_model) {
    new_analyses_names <- paste0(x$type, "_", names(x))
    names(new_analyses_names) <- names(x)
    rownames(mat) <- new_analyses_names[rownames(mat)]
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
    if (is.null(xLab)) xLab <- "Analysis Index"
    if (is.null(yLab)) yLab <- "Intensity"
  } else {
    if (is.null(xLab)) xLab <- "Variable Index"
    if (is.null(yLab)) yLab <- "Intensity"
  }

  if (!interactive) {
    cl <- .get_colors(rownames(mat))
    plot(
      seq_len(ncol(mat)),
      mat[1, ], type = "l",
      col = unname(cl[1]),
      xlab = xLab,
      ylab = yLab,
      main = title,
      ylim = range(mat)
    )
    for (i in 2:nrow(mat)) lines(seq_len(ncol(mat)), mat[i, ], col = unname(cl[i]))
    legend("topright", legend = rownames(mat), col = unname(cl), lty = 1, cex = 0.8)
  } else {
    
    if (!is.null(colorGroups)) {
      if (length(colorGroups) != nrow(mat)) {
        warning("The color groups must have the same length as the number of analyses!")
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
      fig <- fig %>% plotly::add_trace(
        x = xVal,
        y = unlist(mat[i, ]),
        type = "scatter", mode = "lines",
        line = list(width = 0.5, color = unname(cl[var_name[i]])),
        text = paste0(
          "Analysis: ", rownames(mat)[i], "<br>",
          "Variable: ", xVal, "<br>",
          "Intensity: ", unlist(mat[i, ])
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

# MARK: plot_residual_distance
## plot_residual_distance -----
#' @export
#' @noRd
S7::method(plot_residual_distance, StatisticAnalyses) <- function(x, ...) {
  plot_residual_distance(x$model, ...)
}

# MARK: plot_explained_variance
## plot_explained_variance -----
#' @export
#' @noRd
S7::method(plot_explained_variance, StatisticAnalyses) <- function(x, ...) {
  plot_explained_variance(x$model, ...)
}

# MARK: plot_scores
## plot_scores -----
#' @export
#' @noRd
S7::method(plot_scores, StatisticAnalyses) <- function(x, ...) {
  dots <- list(...)
  if ("analyses" %in% names(dots)) dots$analyses <- .check_analyses_argument(x, dots$analyses)
  plot_scores(x$model, ...)
}

# MARK: plot_loadings
## plot_loadings -----
#' @export
#' @noRd
S7::method(plot_loadings, StatisticAnalyses) <- function(x, ...) {
  plot_loadings(x$model, ...)
}

# MARK: plot_residuals
## plot_residuals -----
#' @export
#' @noRd
S7::method(plot_residuals, StatisticAnalyses) <- function(x, ...) {
  dots <- list(...)
  if ("analyses" %in% names(dots)) dots$analyses <- .check_analyses_argument(x, dots$analyses)
  plot_residuals(x$model, ...)
}

# MARK: plot_resolved_spectra
## plot_resolved_spectra -----
#' @export
#' @noRd
S7::method(plot_resolved_spectra, StatisticAnalyses) <- function(x, ...) {
  plot_resolved_spectra(x$model, ...)
}
