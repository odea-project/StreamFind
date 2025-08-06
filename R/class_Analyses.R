#' @title Generic Analyses
#' 
#' @description The `Analyses` class is used to harmonize the interface to data or links to 
#' raw data files. `Analyses` child classes are used to enable a specific structure and methods 
#' for each type of data.
#' 
#' @param analyses A list of analyses, where each element is a data entry or a connection to a
#' raw data file.
#' @param results A list of results, where each element is specific \code{\link{Results}}
#' child class.
#' 
#' @export
#' 
Analyses <- function(analyses = list(), results = list()) {
  x <- structure(
    list(
      analyses = analyses,
      results = results
    ),
    class = c("Analyses"),
    data_type = NA_character_,
    possible_formats = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid Analyses object!")
  }
}

#' @describeIn Analyses Validate the Analyses object, returning `NULL` if valid or an error if not.
#' @param x An `Analyses` object.
#' @export
#'
validate_object.Analyses <- function(x) {
  checkmate::assert_true(grepl(attr(x, "data_type"), class(x)) || is.na(attr(x, "data_type")))
  checkmate::assert_character(attr(x, "possible_formats"))
  checkmate::assert_true(
    checkmate::test_list(x$analyses) || checkmate::test_data_frame(x$analyses)
  )
  checkmate::assert_list(x$results)
  NULL
}

#' @export
#' @noRd
info.Analyses <- function(x) {
  if (length(x) > 0) {
    df <- data.table::data.table(
      "analysis" = names(x),
      "class" = vapply(x$analyses, function(z) class(z)[1], "")
    )
    row.names(df) <- seq_len(nrow(df))
    df
  } else {
    data.frame()
  }
}

#' @export
#' @noRd
show.Analyses <- function(x, ...) {
  if (length(x) > 0) {
    overview <- info(x)
    row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
    print(overview)
  } else {
    cat("empty \n")
  }
  if (length(x$results) > 0) {
    cat("\n")
    results_class <- vapply(x$results, is, "")
    cat(paste0("Result ", seq_len(length(results_class)), ": ", results_class), sep = "\n")
  }
}

#' @export
#' @noRd
length.Analyses <- function(x) {
  length(x$analyses)
}

#' @export
#' @noRd
names.Analyses <- function(x) {
  names(x$analyses)
}

#' @export
#' @noRd
`[.Analyses` <- function(x, i) {
  x$analyses[i]
}

#' @export
#' @noRd
`[<-.Analyses` <- function(x, i, value) {
  if (is(value, "list")) {
    x$analyses[names(value)] <- value
    if (length(x@results) > 0) {
      warning("All results removed!")
      x$results <- list()
    }
  }
  x
}

#' @export
#' @noRd
`[[.Analyses` <- function(x, i) {
  x$analyses[[i]]
}

#' @export
#' @noRd
`[[<-.Analyses` <- function(x, i, value) {
  if (is(value, "list")) {
    x$analyses[[names(value)]] <- value
    if (length(x$results) > 0) {
      warning("All results removed!")
      x$results <- list()
    }
  }
  x
}

#' @export
#' @noRd
as.list.Analyses <- function(x, ...) {
  list("analyses" = x$analyses, "results" = x$results)
}

#' @export
#' @noRd
save.Analyses <- function(x, file = "analyses.json") {
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
add.Analyses <- function(x, value) {
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
  analyses <- c(x$analyses, value)
  analyses <- analyses[order(names(analyses))]
  x$analyses <- analyses
  if (length(x$results) > 0) {
    warning("All results removed!")
    x$results <- list()
  }
  x
}

#' @export
#' @noRd
read.Analyses <- function(x, file) {
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
