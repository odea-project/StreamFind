
#' @export
#' @noRd
StatisticAnalyses <- S7::new_class("StatisticAnalyses", package = "StreamFind", parent = Analyses,

  properties = list(
    
    ## __analyses -----
    analyses = S7::new_property(S7::class_data.frame, default = data.frame()),
    
    ## __names -----
    names = S7::new_property(S7::class_character,
      getter = function(self) {
        rownames(self@analyses)
      }
    ),
    
    ## __classes -----
    classes = S7::new_property(S7::class_character, default = character()),
    
    ## __concentrations -----
    concentrations = S7::new_property(S7::class_numeric, default = numeric(0)),
    
    ## __info -----
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (length(self) > 0) {
        df <- data.frame(
          "analysis" = self@names,
          "features" = ncol(self@analyses)
        )
        
        if (length(self@classes) > 0) {
          df <- cbind(df, "class" = self@classes)
        }
        
        if (length(self@concentrations) > 0) {
          df <- cbind(df, "concentration" = self@concentrations)
        }
        
       row.names(df) <- seq_len(nrow(df))
       df
      } else {
        data.frame()
      }
    }, default = data.frame()),
    
    ## __number_variables -----
    number_variables = S7::new_property(S7::class_numeric, getter = function(self) {
      if (length(self) == 0) return(0)
      ncol(self@analyses)
    }),
    
    ## __has_data -----
    has_data = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["data"]])) {
        if (nrow(self$analyses) == 0) return(FALSE)
      }
      if (!is(self@results[["data"]], "StreamFind::DataFrame")) return(FALSE)
      TRUE
    }),
    
    ## __data -----
    data = S7::new_property(S7::class_list,
      getter = function(self) {
        if (!is.null(self@results[["data"]])) return(self@results[["data"]])
        StreamFind::DataFrame(data = self$analyses)
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::DataFrame")) {
          self@results[["data"]] <- value
        } else {
          warning("Value must be a Data results object! Not done.")
        }
        self
      }
    ),
    
    ## __has_model -----
    has_model = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["model"]])) return(FALSE)
      if (!is(self@results[["model"]], "StreamFind::StatisticModel")) return(FALSE)
      TRUE
    }),
    
    ## __model -----
    model = S7::new_property(S7::class_list,
      getter = function(self) {
        if (self$has_model) return(self@results[["model"]])
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::StatisticModel")) {
          self@results[["model"]] <- value
        } else {
          warning("Value must be a Model results object! Not done.")
        }
        self
      }
    ),
    
    ## has_prediction -----
    has_prediction = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["model"]])) return(FALSE)
      if (!is(self@results[["model"]], "StreamFind::StatisticModel")) return(FALSE)
      if (length(self@results[["model"]]$prediction) == 0) return(FALSE)
      TRUE
    }),
    
    ## __prediction -----
    prediction = S7::new_property(S7::class_list, getter = function(self) {
      if (self$has_prediction) return(self@results[["model"]]$prediction)
      NULL
    })
  ),
  
  constructor = function(analyses = NULL, classes = character(), concentrations = numeric(), ...) {
    
    if (is.null(analyses)) analyses <- data.frame()
    
    if (is.character(analyses)) {
      if (tools::file_ext(analyses) == "csv") {
        analyses <- read.csv(analyses, row.names = 1, ...)
      }
    }
    
    if (is.data.frame(analyses) || is.matrix(analyses)) {
      analyses <- as.data.frame(analyses)
      if (nrow(analyses) > 0) {
        if (length(rownames(analyses)) == 0) {
          rownames(analyses) <- paste0("analysis_", seq_len(nrow(analyses)))
        }
        
        analyses <- analyses[order(rownames(analyses)), ]
      }
    }
    
    S7::new_object(Analyses(), possible_formats = ".csv", analyses = analyses, classes = classes, concentrations = concentrations)
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(identical(self@possible_formats, ".csv")),
      checkmate::test_data_frame(self@analyses),
      checkmate::test_character(rownames(self@analyses), len = nrow(self@analyses)),
      checkmate::test_numeric(as.matrix(self@analyses))
    ) && if (length(self@classes) > 0) {
      length(self@classes) == nrow(self@analyses)
    } else {
      TRUE
    } && if (length(self@concentrations) > 0) {
      length(self@concentrations) == nrow(self@analyses)
    } else {
      TRUE
    }
    if (!valid) return(FALSE)
    NULL
  }
)

# Methods -----

#' @noRd
S7::method(names,  StatisticAnalyses) <- function(x) {
  rownames(x@analyses)
}

#' @export
#' @noRd
S7::method(length, StatisticAnalyses) <- function(x) {
  nrow(x@analyses)
}

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
        rownames(value) <- paste0("analysis_", seq(nrow(x@analyses) + 1, nrow(x@analyses) + nrow(value), by = 1))
      }
    }
    
    x@analyses <- rbind(x@analyses, value)
    
    x@analyses <- x@analyses[order(rownames(x@analyses)), ]
  }
  return(x)
}

#' @export
#' @noRd
S7::method(remove, StatisticAnalyses) <- function(x, value) {
  if (is.character(value)) {
    if (value %in% x@names) {
      x@analyses <- x@analyses[-which(rownames(x@analyses) == value), ]
    }
  }
  if (is.numeric(value)) {
    if (all(value <= nrow(x@analyses))) {
      x@analyses <- x@analyses[-value, ]
    }
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`[`, StatisticAnalyses) <- function(x, i) {
  if (is.character(i)) {
    x@analyses <- x@analyses[rownames(x@analyses) %in% i, ]
  } else if (is.numeric(i)) {
    x@analyses <- x@analyses[i, ]
  } else {
    warning("Index must be character or numeric!")
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`[<-`, StatisticAnalyses) <- function(x, i, value) {
  x <- add(x, value)
  return(x)
}

#' @export
#' @noRd
S7::method(`[[`, StatisticAnalyses) <- function(x, i) {
  if (is.character(i)) {
    x@analyses <- x@analyses[rownames(x@analyses) %in% i, ]
  } else if (is.numeric(i)) {
    x@analyses <- x@analyses[i, ]
  } else {
    warning("Index must be character or numeric!")
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`[[<-`, StatisticAnalyses) <- function(x, i, value) {
  x <- add(x, value)
  return(x)
}

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
    warning("The number of variables in the data must be equal to the number of variables in the model! Not done.")
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

# Get Methods -----

#' @export
#' @noRd
S7::method(get_model_data, StatisticAnalyses) <- function(x) {
  if (!x$has_model) {
    warning("Model not found! Not done.")
    return(NULL)
  }
  get_model_data(x$model)
}

# Plot Methods -----

#' @export
#' @noRd
S7::method(plot_data, StatisticAnalyses) <- function(x,
                                                     analyses = NULL,
                                                     features = NULL,
                                                     transpose = FALSE,
                                                     interactive = TRUE,
                                                     xLab = NULL,
                                                     yLab = NULL,
                                                     title = NULL) {
  
  analyses <- .check_analyses_argument(x, analyses)
  
  mat <- x$data$data
  
  mat <- mat[analyses, , drop = FALSE]
  
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
    if (is.null(xLab)) xLab <- "Analysis Index"
    if (is.null(yLab)) yLab <- "Intensity"
  } else {
    if (is.null(xLab)) xLab <- "Variable Index"
    if (is.null(yLab)) yLab <- "Intensity"
  }
  
  if (!interactive) {
    cl <- .get_colors(rownames(mat))
    plot(1:ncol(mat), mat[1, ], type = "l", col = unname(cl[1]), xlab = xLab, ylab = yLab, main = title, ylim = range(mat))
    for (i in 2:nrow(mat)) lines(1:ncol(mat), mat[i, ], col = unname(cl[i]))
    legend("topright", legend = rownames(mat), col = unname(cl), lty = 1, cex = 0.8)
    
  } else {
    
    cl <- .get_colors(rownames(mat))
    fig <- plotly::plot_ly()
    xVal <- seq_len(ncol(mat))
    
    for (i in seq_len(nrow(mat))) {
      fig <- fig %>% plotly::add_trace(
        x = xVal,
        y = unlist(mat[i, ]),
        type = "scatter", mode = "lines",
        line = list(width = 0.5, color = unname(cl[i])),
        text = paste0(
          rep(rownames(mat)[i], length(xVal))
          # "\n",
          # rep(colnames(mat), each = nrow(mat))
        ),
        hoverinfo = "text",
        name = names(cl)[i],
        legendgroup = names(cl)[i],
        showlegend = TRUE
      )
    }
    
    xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
    
    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    
    fig
  }
}

#' @export
#' @noRd
S7::method(plot_prediction, StatisticAnalyses) <- function(x, ...) {
  if (!x$has_prediction) {
    warning("Prediction not found! Not done.")
    return(NULL)
  }
  plot_prediction(x$model, ...)
}

#' @export
#' @noRd
S7::method(plot_explained_variance, StatisticAnalyses) <- function(x,
                                                                   interactive = TRUE,
                                                                   xLab = NULL,
                                                                   yLab = NULL,
                                                                   title = NULL) {
  
  if (!x$has_model) {
    warning("Model not found! Not done.")
    return(NULL)
  }
  
  variance <- get_model_data(x$model)$explained_variance
  
  if (is.null(variance)) {
    warning("Explained variance not found! Not done.")
    return(NULL)
  }
  
  variance <- cumsum(variance)
  
  if (is.null(xLab)) xLab <- "Principle Components"
  if (is.null(yLab)) yLab <- "Explained Variance (%)"
  
  if (!interactive) {
    plot(variance, type = "b", xlab = xLab, ylab = yLab, main = "Explained Variance")
    
  } else {
    
    fig <- plot_ly()
    
    fig <- fig %>% add_trace(
      x = seq_along(variance),
      y = variance,
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 2),
      marker = list(size = 10),
      name = "Explained Variance",
      showlegend = TRUE
    )
    
    xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"), dtick = 1)
    yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
    
    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    
    fig
  }
}

#' @export
#' @noRd
S7::method(plot_scores, StatisticAnalyses) <- function(x,
                                                       analyses = NULL,
                                                       interactive = TRUE,
                                                       pcs = 1:2,
                                                       title = NULL,
                                                       colorGroups = NULL,
                                                       showText = TRUE,
                                                       showLegend = TRUE) {
  
  if (!x$has_model) {
    warning("Model not found! Not done.")
    return(NULL)
  }
  
  analyses <- .check_analyses_argument(x, analyses)
  dt <- get_model_data(x$model)$scores
  
  if (is.null(dt)) {
    warning("Scores not found! Not done.")
    return(NULL)
  }
  
  dt <- dt[analyses, , drop = FALSE]
  dt <- as.data.frame(dt)
  
  if (length(pcs) > 2) {
    warning("The number of principle components cannot be larger than 2! Not done.")
    return(NULL)
  }
  
  if (any(pcs < 1) || any(pcs > ncol(dt))) {
    warning("The principle components must be in the range of the number of components in the model! Not done.")
    return(NULL)
  }
  
  var <- get_model_data(x$model)$explained_variance
  
  if (!interactive) {
    
    NULL
    
  } else {
    
    if (!is.null(colorGroups)) {
      
      if (length(colorGroups) != nrow(dt)) {
        warning("The color groups must have the same length as the number of analyses in the scores! Not done.")
        return(NULL)
      }
      
      colorGroups <- gsub(" ", "_", colorGroups)
      dt$var_name <- as.character(colorGroups)
      cl <- .get_colors(unique(colorGroups))
      text <- paste0(rownames(dt), "\n", dt$var_name)
    } else {
      dt$var_name <- rownames(dt)
      cl <- .get_colors(rownames(dt))
      text <- rownames(dt)
    }
    
    dt <- dt[order(dt$var_name), ]
    
    if (ncol(dt) == 1) {
      x_val = seq_len(nrow(dt))
      y_val = dt[[1]]
      xLab = "Analysis Index"
      if (!is.null(var)) {
        yLab = paste0("PC", pcs, "(", round(var[pcs], digits = 0) ,"%)")
      } else {
        yLab = paste0("PC", pcs)
      }
    } else {
      x_val = dt[[pcs[1]]]
      y_val = dt[[pcs[2]]]
      
      if (!is.null(var)) {
        xLab = paste0("PC", pcs[1], "(", round(var[pcs[1]], digits = 0) ,"%)")
        yLab = paste0("PC", pcs[2], "(", round(var[pcs[2]], digits = 0) ,"%)")
      } else {
        xLab = paste0("PC", pcs[1])
        yLab = paste0("PC", pcs[2])
      }
    }
    
    if (!showText) text <- NULL
    
    fig <- plot_ly()
    
    fig <- fig %>% add_trace(
      x = x_val,
      y = y_val,
      type = "scatter",
      mode = "markers+text",
      name = dt$var_name,
      legendgroup = dt$var_name,
      marker = list(size = 10, color = cl[dt$var_name]),
      text = text,
      textfont = list(size = 14, color = cl[dt$var_name]),
      textposition = "top",
      showlegend = showLegend
    )
    
    xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
    
    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    
    fig
  }
}

#' @export
#' @noRd
S7::method(plot_loadings, StatisticAnalyses) <- function(x,
                                                         interactive = TRUE,
                                                         pcs = 1:2,
                                                         colorKey = NULL,
                                                         title = NULL,
                                                         showText = TRUE,
                                                         showLegend = TRUE) {
  
  if (!x$has_model) {
    warning("Model not found! Not done.")
    return(NULL)
  }
  
  dt <- get_model_data(x$model)$loadings
  
  if (is.null(dt)) {
    warning("Loadings not found! Not done.")
    return(NULL)
  }
  
  dt <- as.data.frame(dt)
  
  if (length(pcs) > 2) {
    warning("The number of principle components cannot be larger than 2! Not done.")
    return(NULL)
  }
  
  if (any(pcs < 1) || any(pcs > ncol(dt))) {
    warning("The principle components must be in the range of the number of components in the model! Not done.")
    return(NULL)
  }
  
  dt <- dt[, pcs, drop = FALSE]
  
  var <- get_model_data(x$model)$explained_variance
  
  if (!interactive) {
    
    NULL
    
  } else {
    
    if (!is.null(colorKey)) {
      
      if (length(colorKey) != nrow(dt)) {
        warning("The color key must have the same length as the number of variables in the loadings! Not done.")
        return(NULL)
      }
      
      cl <- .get_colors(colorKey)
      
    } else {
      cl <- .get_colors(1)
    }
    
    if (length(cl) == 1) showLegend <- FALSE
    
    fig <- plot_ly()
    
    if (ncol(dt) == 1) {
      x = seq_len(nrow(dt))
      y = dt[, 1]
      xLab = "Analysis Index"
      if (!is.null(var)) {
        yLab = paste0("PC", pcs, "(", round(var[pcs], digits = 0) ,"%)")
      } else {
        yLab = paste0("PC", pcs)
      }
      
    } else {
      x = dt[, 1]
      y = dt[, 2]
      
      if (!is.null(var)) {
        xLab = paste0("PC", pcs[1], "(", round(var[pcs[1]], digits = 0) ,"%)")
        yLab = paste0("PC", pcs[2], "(", round(var[pcs[2]], digits = 0) ,"%)")
      } else {
        xLab = paste0("PC", pcs[1])
        yLab = paste0("PC", pcs[2])
      }
    }
    
    if (showText) text <- rownames(dt) else text <- NULL
    
    fig <- fig %>% add_trace(
      x = x,
      y = y,
      type = "scatter",
      mode = "markers+text",
      color = names(cl),
      colors = cl,
      text = text,
      textfont = list(size = 14, color = cl),
      textposition = "top",
      marker = list(size = 10, color = cl),
      name = names(cl),
      legendgroup = names(cl),
      showlegend = showLegend
    )
    
    xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
    
    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    
    fig
  }
}

#' @export
#' @noRd
S7::method(plot_residuals, StatisticAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          interactive = TRUE,
                                                          xLab = NULL,
                                                          yLab = NULL,
                                                          title = NULL) {
  
  if (!x$has_model) {
    warning("Model not found! Not done.")
    return(NULL)
  }
  
  analyses <- .check_analyses_argument(x, analyses)
  dt <- get_model_data(x$model)$residuals
  
  if (is.null(dt)) {
    warning("Residuals not found! Not done.")
    return(NULL)
  }
  
  dt <- dt[analyses, , drop = FALSE]
  dt <- as.data.frame(dt)
  
  if (is.null(dt)) return(NULL)
  if (is.null(xLab)) xLab <- "Variable Index"
  if (is.null(yLab)) yLab <- "Intensity"
  
  if (!interactive) {
    
    NULL
    
  } else {
    
    cl <- .get_colors(rownames(dt))
    
    fig <- plot_ly()
    
    xVal <- seq_len(ncol(dt))
    
    for (i in seq_len(nrow(dt))) {
      
      fig <- fig %>% add_trace(
        x = xVal,
        y = unlist(dt[i, ]),
        type = "scatter", mode = "lines",
        line = list(width = 0.5, color = unname(cl[i])),
        hoverinfo = "text",
        text = paste(
          "</br> analysis:  ", rownames(dt)[i],
          "</br> variable:  ", colnames(dt),
          "</br> intensity: ", "%{y}"
        ),
        name = names(cl)[i],
        legendgroup = names(cl)[i],
        showlegend = TRUE
      )
    }
    
    xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
    
    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    
    fig
  }
}

#' @export
#' @noRd
S7::method(plot_resolved_spectra, StatisticAnalyses) <- function(x,
                                                                 interactive = TRUE,
                                                                 pcs = NULL,
                                                                 original = TRUE,
                                                                 title = NULL,
                                                                 showText = TRUE,
                                                                 showLegend = TRUE) {
  
  if (!x$has_model) {
    warning("Model not found! Not done.")
    return(NULL)
  }
  
  dt <- get_model_data(x$model)$resolved
  
  if (is.null(dt)) {
    warning("Loadings not found! Not done.")
    return(NULL)
  }
  
  if (!is.null(pcs)) {
    checkmate::assert_integerish(pcs)
    dt <- dt[pcs, , drop = FALSE]
  }
  
  dt <- as.data.frame(dt)
  
  if (original) {
    data <- t(x$data$data)
    dt <- cbind(data, dt)
    for (i in seq_len(ncol(dt))) dt[, i] <- dt[, i] / max(dt[, i])
  }
  
  if (!interactive) {
    
    NULL
    
  } else {
    
    cl <- .get_colors(colnames(dt))
    
    fig <- plot_ly()
    
    x = seq_len(nrow(dt))
    
    xLab = "Var Index"
    
    yLab = "Intensity"
    
    for (i in seq_len(length(cl))) {
      
      fig <- fig %>% add_trace(
        x = x,
        y = dt[, i],
        type = "scatter",
        mode = "markers+lines",
        line = list(size = 0.3, color = cl[i]),
        marker = list(size = 2, color = cl[i]),
        name = names(cl[i]),
        legendgroup = names(cl[i]),
        showlegend = TRUE
      )
    }
    
    xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = yLab, titlefont = list(size = 12, color = "black"))
    
    fig <- fig %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    
    fig
  }
}
