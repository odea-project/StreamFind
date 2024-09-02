
#' @export
#' @noRd
StatisticAnalyses <- S7::new_class("StatisticAnalyses", package = "StreamFind", parent = Analyses,

  properties = list(
   
    analyses = S7::new_property(S7::class_data.frame, default = data.frame()),
    
    names = S7::new_property(S7::class_character, getter = function(self) {
      rownames(self@analyses)
    }, default = character(0)),
    
    classes = S7::new_property(S7::class_character, default = character(0)),
    
    concentrations = S7::new_property(S7::class_numeric, default = numeric(0)),
    
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
    }, default = data.frame())
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
S7::method(`[[<-`, Analyses) <- function(x, i, value) {
  x <- add(x, value)
  return(x)
}
