
#' @export
#' @noRd
Analyses <- S7::new_class("Analyses", package = "StreamFind",
  
  properties = list(
    
    # ___ possible_formats -----
    possible_formats = S7::new_property(S7::class_character, default = NA_character_),
    
    # ___ analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),
    
    # ___ results -----
    results = S7::new_property(S7::class_list, default = list()),
    
    # ___ info -----
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (self@length > 0) {
        df <- data.table::data.table(
          "analysis" = self@names,
          "class" = vapply(self@analyses, function(x) class(x)[1], "")
        )
        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    }, default = data.frame())
  ),
  
  constructor = function(analyses = list(), results = list()) {
    S7::new_object(S7::S7_object(), possible_formats = NA_character_, analyses = analyses, results = results)
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_character(self@possible_formats),
      checkmate::test_list(self@analyses),
      checkmate::test_list(self@results)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, Analyses) <- function(x, ...) {
  cat("\n")
  cat("Analyses")
  if (length(x) > 0) {
    cat("\n")
    overview <- x@info
    row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
    print(overview)
  } else {
    cat(" empty \n")
  }
  cat("\n")
  cat("Results")
  if (length(x@results) > 0) {
    cat("\n")
    names_results <- names(x@results)
    cat(
      paste0(" ", seq_len(length(names_results)), ": ", names_results),
      sep = "\n"
    )
  } else {
    cat(" empty \n")
  }
}

#' @export
#' @noRd
S7::method(print, Analyses) <- function(x, ...) {
  show(x)
}

#' @export
#' @noRd
S7::method(length, Analyses) <- function(x) {
  length(x@analyses)
}

#' @export
#' @noRd
S7::method(names, Analyses) <- function(x) {
  names(x@analyses)
}

#' @export
#' @noRd
S7::method(`[`, Analyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  return(x)
}

#' @export
#' @noRd
S7::method(`[<-`, Analyses) <- function(x, i, value) {
  if (is(value, "list")) {
    x@analyses[names(value)] <- value
    if (length(x@results) > 0) {
      warning("All results removed!")
      x@results <- list()
    }
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`[[`, Analyses) <- function(x, i) {
  x@analyses <- x@analyses[[i]]
  return(x)
}

#' @export
#' @noRd
S7::method(`[[<-`, Analyses) <- function(x, i, value) {
  if (is(value, "list")) {
    x@analyses[[names(value)]] <- value
    if (length(x@results) > 0) {
      warning("All results removed!")
      x@results <- list()
    }
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`$`, Analyses) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`$<-`, Analyses) <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}

#' @export
#' @noRd
S7::method(as.list, Analyses) <- function(x) {
  list("analyses" = x@analyses, "results" = x@results)
}

#' @export
#' @noRd
S7::method(save, Analyses) <- function(x, format = "json", name = "settings", path = getwd()) {
  if (format %in% "json") x <- .convert_to_json(as.list(x))
  .save_data_to_file(x, format, name, path)
}

#' @export
#' @noRd
S7::method(add, Analyses) <- function(x, analyses) {
  if (!is(analyses, "list")) {
    warning("Analysis must be a list!")
    return(x)
  }
  
  analyses_names <- names(analyses)
  
  if (length(analyses_names) == 0) {
    warning("Analysis names must be provided!")
    return(x)
  }
  
  if (any(vapply(analyses_names, function(a) a %in% x@names, FALSE))) {
    warning("Analysis names already exist!")
    return(x)
  }
  
  analyses <- c(x@analyses, analyses)
  analyses <- analyses[order(names(analyses))]
  x@analyses <- analyses
  
  if (length(x@results) > 0) {
    warning("All results removed!")
    x@results <- list()
  }
  
  x
}

#' @export
#' @noRd
S7::method(read, Analyses) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(Analyses(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::Analyses")) return(res)
  }
  NULL
}
