#' @title Generic Analyses
#' 
#' @description The `Analyses` class is used to harmonize the interface to data or links to raw data files and results across different types of data. `Analyses` child classes are used for specific data types, providing dedicated methods.
#' 
#' @param analyses A list of analyses, where each element is a data entry or a connection to a raw data file.
#' @param results A list of results, where each element is a specific \code{\link{Results}} child class.
#' 
#' @return
#' An object of class `Analyses`, which is a list containing:
#' \itemize{
#'  \item `analyses`: A list of analyses, where each element is a data entry or a connection to a raw data file.
#'  \item `results`: A list of results, where each element is a specific \code{\link{Results}} child class.
#'  \item `type`: A character string indicating the type of data contained in the analyses.
#'  \item `formats`: A character vector indicating the possible formats of the analyses.
#' }
#' 
#' @export
#' 
Analyses <- function(analyses = list(), results = list()) {
  x <- structure(
    list(
      analyses = analyses,
      results = results,
      type = NA_character_,
      formats = NA_character_
    ),
    class = c("Analyses")
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
  checkmate::assert_class(x, "Analyses")
  checkmate::assert_names(names(x), must.include = c("analyses", "results", "type", "formats"))
  checkmate::assert_character(x$type, len = 1, null.ok = FALSE)
  checkmate::assert_character(x$formats, null.ok = FALSE)
  checkmate::assert_true(grepl(x$type, class(x)[1]) || is.na(x$type))
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
show.Analyses <- function(x, ...) {
  if (length(x$analyses) > 0) {
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
