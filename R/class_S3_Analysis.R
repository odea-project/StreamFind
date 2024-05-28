#' *Analysis* S3 class constructor, methods and functions
#'
#' @description
#' Creates an Analysis S3 class object.
#'
#' @param name Character of length one with the name of the analysis.
#' @param replicate Character of length one with the analysis replicate group name.
#' @param blank Character of length one with the associated blank replicate group name.
#'
#' @return An *Analysis* S3 class object.
#'
#' @export
#'
Analysis <- function(name = NA_character_,
                     replicate = NA_character_,
                     blank = NA_character_) {

  x <- list(
    "name" = name,
    "replicate" = replicate,
    "blank" = blank
  )

  x$name <- as.character(x$name)
  
  x$replicate <- as.character(x$replicate)
  
  x$blank <- as.character(x$blank)
  
  if (is.na(x$blank)) x$blank <- NA_character_

  if (validate.Analysis(x)) {
    x <- structure(x, class = "Analysis")
    x
  } else {
    NULL
  }
}

#' @describeIn validate description Validates an Analysis S3 object.
#' 
#' @param x An Analysis S3 object.
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
  }

  if (!valid) warning("Issue/s found with analysis ", x$name, "!")

  valid
}

#' @export
#' @noRd
#'
print.Analysis <- function(x, ...) {
  cat("\n")
  cat(
    " ", class(x), "\n"
  )
  cat(
    "  name: ", x$name, "\n",
    "  replicate: ", x$replicate, "\n",
    "  blank: ", x$blank, "\n",
    sep = ""
  )
}

#' @noRd
as.Analysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(Analysis, value)
}
