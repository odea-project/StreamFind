
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
    classes = S7::new_property(S7::class_character, default = character(0)),
    
    ## __concentrations -----
    concentrations = S7::new_property(S7::class_numeric, default = numeric(0)),
    
    ## __info -----
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (self@length > 0) {
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
    
    ## __has_matrix -----
    has_matrix = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["matrix"]])) {
        if (nrow(self$analyses) == 0) return(FALSE)
      }
      if (!is(self@results[["matrix"]], "StreamFind::Matrix")) return(FALSE)
      TRUE
    }),
    
    ## __matrix -----
    matrix = S7::new_property(S7::class_list,
      getter = function(self) {
        if (!is.null(self@results[["matrix"]])) return(self@results[["matrix"]])
        StreamFind::Matrix(self$analyses)
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::Matrix")) {
          self@results[["matrix"]] <- value
        } else {
          warning("Value must be a Matrix results object! Not done.")
        }
        self
      }
    ),
    
    ## __has_model -----
    has_model = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["model"]])) return(FALSE)
      if (!is(self@results[["model"]], "StreamFind::Model")) return(FALSE)
      TRUE
    }),
    
    ## __spectra -----
    model = S7::new_property(S7::class_list,
      getter = function(self) {
        if (self$has_model) return(self@results[["model"]])
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::Model")) {
          self@results[["model"]] <- value
        } else {
          warning("Value must be a Model results object! Not done.")
        }
        self
      }
    )
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
  rownames(self@analyses)
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

# Plot Methods -----

#' @export
#' @noRd
S7::method(plot_matrix, Analyses) <- function(x,
                                              analyses = NULL,
                                              features = NULL,
                                              transpose = FALSE,
                                              interactive = TRUE,
                                              xLab = NULL,
                                              yLab = NULL,
                                              title = NULL) {
  
  analyses <- .check_analyses_argument(x, analyses)
  
  mat <- x$analyses
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
    
    fig <- plot_ly()
    
    xVal <- seq_len(ncol(mat))
    
    for (i in seq_len(nrow(mat))) {
      
      fig <- fig %>% add_trace(
        x = xVal,
        y = mat[i, ],
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

