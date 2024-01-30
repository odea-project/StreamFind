#' **Analysis** S3 class constructor, methods and functions
#'
#' @description
#' Creates an Analysis S3 class object.
#'
#' @param name Character of length one with the name of the analysis.
#' @param replicate Character of length one with the analysis replicate group name.
#' @param blank Character of length one with the associated blank replicate group name.
#' @param file Character of length one (optional) with the full file path including extension.
#' @param version Character of length one with the version. It should match with
#' the version of the StreamFind package when created the `Analysis` object.
#'
#' @return An Analysis S3 class object.
#'
#' @export
#'
Analysis <- function(name = NA_character_,
                     replicate = NA_character_,
                     blank = NA_character_,
                     file = NA_character_,
                     version = NA_character_) {

  x <- list(
    "name" = name,
    "replicate" = replicate,
    "blank" = blank,
    "file" = file,
    "version" = version
  )

  x$name <- as.character(x$name)
  x$replicate <- as.character(x$replicate)
  x$blank <- as.character(x$blank)
  if (is.na(x$blank)) x$blank <- NA_character_
  x$file <- as.character(x$file)
  x$version <- as.character(x$version)

  if (validate.Analysis(x)) {
    x <- structure(x, class = "Analysis")
    if (is.na(x$version)) x$version <- as.character(packageVersion("StreamFind"))
    x
  } else {
    NULL
  }
}

#' @describeIn Analysis
#' Validates an Analysis S3 class object, returning a logical value of
#' length one.
#'
#' @param x An Analysis S3 class object.
#'
#' @export
#'
validate.Analysis <- function(x = NULL) {
  valid <- FALSE
  name <- NA_character_

  if (is.list(x)) {
    valid <- TRUE

    if (length(x$name) != 1 || !is.character(x$name)) {
      warning("Analysis name not conform!")
      valid <- FALSE
    } else {
      name <- x$name
    }

    if (length(x$replicate) != 1 || !is.character(x$replicate)) {
      warning("Analysis replicate name not conform!")
      valid <- FALSE
    }

    if (length(x$blank) != 1 || !is.character(x$blank)) {
      warning("Analysis blank name not conform!")
      valid <- FALSE
    }

    if (length(x$file) != 1 || !is.character(x$file)) {
      warning("Analysis file path entry not conform!")
      valid <- FALSE
    } else if (!is.na(x$file) && !file.exists(x$file)) {
      warning(paste0(x$file, " does not exist!"))
      valid <- FALSE
    }

    if (length(x$version) != 1 || !is.character(as.character(x$version))) {
      warning("Analysis version entry not conform!")
      valid <- FALSE
    }
  }

  if (!valid) warning("Issue/s found with analysis ", x$name, "!")

  valid
}

#' @describeIn Analysis
#' Prints the Analysis S3 class object in the console.
#'
#' @param ... Not used.
#'
#' @export
print.Analysis <- function(x, ...) {
  cat("\n")
  cat(
    " ", class(x), "\n"
  )
  cat(
    "  name: ", x$name, "\n",
    "  replicate: ", x$replicate, "\n",
    "  blank: ", x$blank, "\n",
    "  file: ", x$file, "\n",
    "  version: ", x$version, "\n",
    sep = ""
  )
}

#' @describeIn Analysis
#' Converts a Analysis S3 class object to a JSON string.
#'
#' @export
asJSON.Analysis <- function(x) {
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

#' @describeIn Analysis
#' Converts the argument value in an Analysis S3 class object.
#'
#' @param value A list to be checked and/or converted to Analysis S3 class.
#'
#' @export
as.Analysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(Analysis, value)
}
