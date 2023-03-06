#' Headers S3 Class Constructor
#'
#' @description
#' Creates a headers S3 class object.
#'
#' @template arg-headers-ellipsis
#'
#' @note If an argument or element name is given, it must be type character.
#' If an argument or element path is given, it must be type character and exist.
#' If an argument or element date is given, it must be class POSIXct or POSIXt.
#' If given date is character, conversion to class POSIXct or POSIXt is
#' attempted.
#'
#' @return A headers S3 class object.
#'
#' @export
#'
headers <- function(...) {

  x <- list(...)

  if (length(x) == 1 & is.list(x[[1]])) x <- x[[1]]

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

  if (validate.headers(x)) {
    structure(x, class = "headers")
  } else {
    NULL
  }
}

#' @describeIn headers
#' Validates a headers S3 class object, returning a logical value of length one.
#'
#' @param headers A headers S3 class object.
#'
#' @export
#'
validate.headers <- function(headers) {

  if (missing(headers)) headers <- NULL

  valid <- FALSE

  if (is.list(headers)) {
    valid <- TRUE

    if (!all(vapply(headers, function(headers) length(headers) == 1, FALSE))) {
      warning("All headers must be of length 1!")
      valid <- FALSE
    }

    if (length(unique(names(headers))) != length(headers)) {
      warning("Headers must have names and not permitted duplicated names!")
      valid <- FALSE
    }

    if ("name" %in% names(headers)) {
      if (!is.character(headers$name)) {
        warning("Header list entry name must be character!")
        valid <- FALSE
      }
    }

    if ("path" %in% names(headers)) {
      if (!dir.exists(headers$path)) {
        warning("Header list entry path must exist!")
        valid <- FALSE
      }
    }

    if ("date" %in% names(headers)) {
      if (!all(grepl("POSIXct|POSIXt", class(headers$date)))) {
        warning("Header list entry date class must be POSIXct or POSIXt!")
        valid <- FALSE
      }
    }
  }

  valid
}

#' @describeIn headers
#' Converts the argument value into a headers S3 class object.
#'
#' @param value List to be checked and converted to headers S3 class.
#'
#' @export
as.headers <- function(value) {
  headers(value = value)
}
