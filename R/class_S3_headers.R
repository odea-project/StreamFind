#' **Headers** S3 class constructor, methods and functions
#'
#' @description
#' Creates a Headers S3 class object.
#'
#' @template arg-headers-ellipsis
#'
#' @note If an argument or element name is given, it must be type character.
#' If an argument or element path is given, it must be type character and exist.
#' If an argument or element date is given, it must be class POSIXct or POSIXt.
#' If given date is character, conversion to class POSIXct or POSIXt is
#' attempted.
#'
#' @return A Headers S3 class object.
#'
#' @export
#'
Headers <- function(...) {

  x <- list(...)

  if (length(x) == 1) if (is.list(x[[1]])) x <- x[[1]]

  x_names <- names(x)

  x <- lapply(x, function(h) {
    if (any(is.null(h))) h <- NA_character_
    if (any(is.na(h))) h <- NA_character_
    h
  })

  if ("date" %in% x_names) {
    x$date <- as.POSIXct(x$date)
    attr(x$date, "tzone") <- NULL
  }

  if (!"name" %in% x_names) x$name <- NA_character_
  if (!"path" %in% x_names) x$path <- getwd()
  if (!"date" %in% x_names) x$date <- Sys.time()

  if (validate.Headers(x)) {
    structure(x, class = "Headers")
  } else {
    NULL
  }
}

#' @describeIn Headers
#' Validates a Headers S3 class object, returning a logical value of length one.
#'
#' @param x A Headers S3 class object.
#'
#' @export
#'
validate.Headers <- function(x) {

  if (missing(x)) x <- NULL

  valid <- FALSE

  if (is.list(x)) {
    valid <- TRUE

    if (!all(vapply(x, function(x) length(x) == 1, FALSE))) {
      warning("All headers must be of length 1!")
      valid <- FALSE
    }

    if (length(unique(names(x))) != length(x)) {
      warning("Headers must have names and not permitted duplicated names!")
      valid <- FALSE
    }

    if ("name" %in% names(x)) {
      if (!is.character(x$name)) {
        warning("Header list entry name must be character!")
        valid <- FALSE
      }
    }

    if ("path" %in% names(x)) {
      if (!dir.exists(x$path)) {
        warning("Header list entry path must exist!")
        valid <- FALSE
      }
    }

    if ("date" %in% names(x)) {
      if (!all(grepl("POSIXct|POSIXt", class(x$date)))) {
        warning("Header list entry date class must be POSIXct or POSIXt!")
        valid <- FALSE
      }
    }
  }

  valid
}

#' @describeIn Headers
#' Converts a Headers S3 class object to a JSON string.
#'
#' @export
asJSON.Headers <- function(x) {
  toJSON(
    x,
    dataframe = "columns",
    Date = "ISO8601",
    POSIXt = "string",
    factor = "string",
    complex = "string",
    null = "null",
    na = "null",
    auto_unbox = FALSE,
    digits = 8,
    pretty = TRUE,
    force = TRUE
  )
}

#' @describeIn Headers
#' Converts the argument value into a Headers S3 class object.
#'
#' @param value List to be checked and converted to Headers S3 class.
#'
#' @export
as.Headers <- function(value) {
  Headers(value = value)
}
