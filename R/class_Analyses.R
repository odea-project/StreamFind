#' @title Generic Analyses
#' 
#' @description The Analyses class is used harmonize the interface to data entries or links to raw data files. Analyses child classes are used to enable a specific structure and methods for each type of data.
#' 
#' @param analyses A list of analyses, where each element is a data entry or a connection to a raw data file.
#' @param results A list of results, where each element is specific \code{\link{Results}} child class.
#' 
#' @slot data_type (getter/setter) A character string indicating the type of data.
#' @slot possible_formats (getter/setter) A character vector of possible formats for the raw data.
#' @slot analyses (getter/setter) A list of analyses, where each element is a data entry or a connection to a raw data file.
#' @slot results (getter/setter) A list of results, where each element is specific \code{\link{Results}} child class.
#' @slot info (getter) A data frame containing information about the analyses.
#' 
#' @export
#' 
Analyses <- S7::new_class(
  name = "Analyses",
  package = "StreamFind",
  properties = list(
    
    # data_type -----
    data_type = S7::new_property(S7::class_character, default = NA_character_),

    # possible_formats -----
    possible_formats = S7::new_property(S7::class_character, default = NA_character_),

    # analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),

    # results -----
    results = S7::new_property(S7::class_list, default = list()),

    # info -----
    info = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        if (length(self) > 0) {
          df <- data.table::data.table(
            "analysis" = names(self),
            "class" = vapply(self@analyses, function(x) class(x)[1], "")
          )
          row.names(df) <- seq_len(nrow(df))
          df
        } else {
          data.frame()
        }
      }
    )
  ),
  constructor = function(analyses = list(), results = list()) {
    S7::new_object(
      S7::S7_object(),
      data_type = NA_character_,
      possible_formats = NA_character_,
      analyses = analyses,
      results = results
    )
  },
  validator = function(self) {
    checkmate::assert_true(grepl(self@data_type, is(self)) || is.na(self@data_type))
    checkmate::assert_character(self@possible_formats)
    checkmate::assert_true(
      checkmate::test_list(self@analyses) || checkmate::test_data_frame(self@analyses)
    )
    checkmate::assert_list(self@results)
    NULL
  }
)

#' @export
#' @noRd
`$.StreamFind::Analyses` <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
`$<-.StreamFind::Analyses` <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}

#' @export
#' @noRd
S7::method(show, Analyses) <- function(x, ...) {
  if (length(x) > 0) {
    overview <- x@info
    row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
    print(overview)
  } else {
    cat("empty \n")
  }

  if (length(x@results) > 0) {
    cat("\n")
    results_class <- vapply(x@results, is, "")
    cat(paste0("Result ", seq_len(length(results_class)), ": ", results_class), sep = "\n")
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
`[.StreamFind::Analyses` <- function(x, i) {
  x@analyses[i]
}

#' @export
#' @noRd
`[<-.StreamFind::Analyses` <- function(x, i, value) {
  if (is(value, "list")) {
    x@analyses[names(value)] <- value
    if (length(x@results) > 0) {
      warning("All results removed!")
      x@results <- list()
    }
  }
  x
}

#' @export
#' @noRd
`[[.StreamFind::Analyses` <- function(x, i) {
  x@analyses[[i]]
}

#' @export
#' @noRd
`[[<-.StreamFind::Analyses` <- function(x, i, value) {
  if (is(value, "list")) {
    x@analyses[[names(value)]] <- value
    if (length(x@results) > 0) {
      warning("All results removed!")
      x@results <- list()
    }
  }
  x
}

#' @export
#' @noRd
S7::method(as.list, Analyses) <- function(x, ...) {
  list("analyses" = x@analyses, "results" = x@results)
}

#' @export
#' @noRd
S7::method(save, Analyses) <- function(x, file = "analyses.json") {
  format <- tools::file_ext(file)
  if (format %in% "json") {
    x <- .convert_to_json(as.list(x))
    write(x, file)
  } else if (format %in% "rds") {
    saveRDS(x, file)
  } else {
    warning("Invalid format!")
  }
  invisible(NULL)
}

#' @export
#' @noRd
S7::method(add, Analyses) <- function(x, value) {
  if (!is(value, "list")) {
    warning("Analysis must be a list!")
    return(x)
  }
  value_names <- names(value)
  if (length(value_names) == 0) {
    warning("Analysis names must be provided!")
    return(x)
  }
  if (any(vapply(value_names, function(a) a %in% names(x), FALSE))) {
    warning("Analysis names already exist!")
    return(x)
  }
  analyses <- c(x@analyses, value)
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
    if (is(res, "StreamFind::Analyses")) {
      return(res)
    }
  }
  NULL
}
