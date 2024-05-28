#' **ProjectHeaders** S3 class constructor, methods and functions
#'
#' @description Creates a ProjectHeaders S3 class object.
#'
#' @template arg-headers-ellipsis
#'
#' @note If an argument or element name is given, it must be type character.
#' If an argument or element file is given, it must be type character and exist on disk.
#' If an argument or element date is given, it must be class POSIXct or POSIXt.
#' If given date is character, conversion to class POSIXct or POSIXt is attempted.
#'
#' @return A ProjectHeaders S3 class object.
#'
#' @export
#'
ProjectHeaders <- function(...) {

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
  if (!"author" %in% x_names) x$author <- NA_character_
  if (!"file" %in% x_names) x$file <- NA_character_
  if (!"date" %in% x_names) x$date <- Sys.time()
  
  if (!is.na(x$file)) {
    
    if (!grepl(".sqlite", x$file)) {
      warning("ProjectHeaders file must have extension .sqlite!")
      x$file <- NA_character_
      
    } else {
      if (!file.exists(x$file)) file.create(x$file)
    }
  }

  if (validate.ProjectHeaders(x)) {
    structure(x, class = "ProjectHeaders")
  } else {
    NULL
  }
}

#' @describeIn validate Validates a ProjectHeaders S3 object.
#' 
#' @param x A ProjectHeaders S3 object.
#' 
#' @export
#' 
validate.ProjectHeaders <- function(x) {

  if (missing(x)) x <- NULL

  valid <- FALSE

  if (is.list(x)) {
    valid <- TRUE

    if (!all(vapply(x, function(x) length(x) == 1, FALSE))) {
      warning("All headers must be of length 1!")
      valid <- FALSE
    }

    if (length(unique(names(x))) != length(x)) {
      warning("ProjectHeaders must have names and not permitted duplicated names!")
      valid <- FALSE
    }

    if (!all(c("name", "author", "file", "date") %in% names(x))) {
      warning("ProjectHeaders must contain at least entries name, author, file and date!")
      valid <- FALSE
    }

    if ("name" %in% names(x)) {
      if (!is.character(x$name)) {
        warning("ProjectHeaders entry name must be character length 1!")
        valid <- FALSE
      }
    }

    if ("author" %in% names(x)) {
      if (!is.character(x$author)) {
        warning("ProjectHeaders entry author must be character length 1!")
        valid <- FALSE
      }
    }

    if ("file" %in% names(x)) {
      if (!is.na(x$file)) {
        if (!file.exists(x$file)) {
          warning("ProjectHeaders entry file must exist!")
          valid <- FALSE
        }
      }
    }

    if ("date" %in% names(x)) {
      if (!all(grepl("POSIXct|POSIXt", class(x$date)))) {
        warning("ProjectHeaders entry date class must be POSIXct or POSIXt length 1!")
        valid <- FALSE
      }
    }
  }

  valid
}

#' @export
#' @noRd
#'
print.ProjectHeaders <- function(x, ...) {
  cat("\n")
  cat(
    " ", class(x), "\n"
  )
  names <- names(x)
  for (n in names) {
    cat("  ", n, ": ", as.character(x[[n]]), "\n", sep = "")
  }
}

#' @noRd
as.ProjectHeaders <- function(value) {
  ProjectHeaders(value = value)
}
