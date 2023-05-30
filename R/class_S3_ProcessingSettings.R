#' **ProcessingSettings** S3 class constructor, methods and functions
#'
#' @description
#' Creates a ProcessingSettings S3 class object. The ProcessingSettings are used
#' in \pkg{streamFind} for processing data in a given data class method.
#'
#' @param call Character of length one with the name of the method where the
#' processing settings are to be applied.
#' @param algorithm Character of length one with the name of the algorithm to
#' be used.
#' @param parameters List with parameters specific for the method `call` and
#' `algorithm`.
#' @param software Character of length one with the name of the software or
#' package.
#' @param developer Character of length one with the name of the developer/s.
#' @param contact Character of length one with the email of the developer.
#' @param link Character of length one with the documentation web link.
#' @param doi Character of length one with the DOI of algorithm.
#'
#' @details See the documentation of the method where the processing settings
#' are to be applied for more information about applicable algorithms and
#' parameters.
#'
#' @return A ProcessingSettings S3 class
#'
#' @export
#'
ProcessingSettings <- function(call = NA_character_,
                               algorithm = NA_character_,
                               parameters = NULL,
                               software = NA_character_,
                               developer = NA_character_,
                               contact = NA_character_,
                               link = NA_character_,
                               doi = NA_character_) {

  x <- list(
    "call" = call,
    "algorithm" = algorithm,
    "parameters" = parameters,
    "software" = software,
    "developer" = developer,
    "contact" = contact,
    "link" = link,
    "doi" = doi
  )

  if (is.data.frame(x$parameters)) {
    x$parameters <- as.list(x$parameters)
  }

  if ("class" %in% names(x$parameters)) {
    x$parameters[["Class"]] <- x$parameters$class
    x$parameters[["class"]] <- NULL
    x$parameters <- lapply(x$parameters, function(z) {
      if (is.list(z) & length(z) > 0) {
        z[[1]]
      } else {
        z
      }
    })

    if (x$parameters$Class %in% "CentWaveParam") {
      x$parameters$roiScales <- as.double()
    }

    if (x$parameters$Class %in% "PeakGroupsParam") {
      x$parameters$peakGroupsMatrix <- as.matrix(x$parameters$peakGroupsMatrix)
    }

    if (x$parameters$Class %in% "PeakGroupsParam") {
      x$parameters$subset <- as.integer(x$parameters$subset)
    }

    x$parameters <- do.call("new", x$parameters)
  }

  if (validate.ProcessingSettings(x)) {
    structure(x, class = "ProcessingSettings")
  } else {
    NULL
  }
}

#' @describeIn ProcessingSettings
#' Validates a ProcessingSettings S3 class object, returning a logical value of
#' length one.
#'
#' @param x A ProcessingSettings S3 class object.
#'
#' @export
#'
validate.ProcessingSettings <- function(x = NULL) {
  valid <- FALSE

  if (is.list(x)) {
    if (all(c("call", "algorithm", "parameters") %in% names(x))) {
      valid <- TRUE

      if (!length(x$call) == 1 && !all(is.character(x$algorithm))) {
        warning("Call entry must be of length 1!")
        valid <- FALSE
      }

      if (length(x$algorithm) != 1 && !all(is.character(x$algorithm))) {
        warning("Algorithm entry must be of length 1 and type character!")
        valid <- FALSE
      }

      if (!(is.list(x$parameters) || isS4(x$parameters))) {
        warning("Parameters entry must be a list or an S4 class!")
        valid <- FALSE
      }

    } else {
      warning("Settings elements must be named call, algorithm and parameters!")
    }
  }
  valid
}

#' @describeIn ProcessingSettings
#' Converts a ProcessingSettings S3 class object to a JSON string.
#'
#' @export
asJSON.ProcessingSettings <- function(x) {
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

#' @describeIn ProcessingSettings
#' Converts the argument in a ProcessingSettings S3 class object.
#'
#' @param value A list to be checked and/or converted to ProcessingSettings S3
#' class.
#'
#' @export
as.ProcessingSettings <- function(value) {
  must_have_elements <- c("call", "algorithm", "parameters")
  if (!all(must_have_elements %in% names(value))) return(NULL)
  ProcessingSettings(
    value$call,
    value$algorithm,
    value$parameters,
    value$software,
    value$developer,
    value$contact,
    value$link,
    value$doi
  )
}
