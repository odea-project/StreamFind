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
                               parameters = NULL) {

  x <- list(
    "call" = call,
    "algorithm" = algorithm,
    "parameters" = parameters
  )

  if (is.data.frame(x$parameters)) x$parameters <- list(x$parameters)

  x$parameters <- lapply(x$parameters, function(par) {

    if (is.data.frame(par)) par <- as.list(par)

    if ("class" %in% names(par)) {
      par[["Class"]] <- par$class
      par[["class"]] <- NULL
      par <- lapply(par, function(z) {
        if (is.list(z) & length(z) > 0) {
          z[[1]]
        } else {
          z
        }
      })

      if (par$Class %in% "CentWaveParam") par$roiScales <- as.double()

      if (par$Class %in% "PeakGroupsParam") {
        par$peakGroupsMatrix <- as.matrix(par$peakGroupsMatrix)
      }

      if (par$Class %in% "PeakGroupsParam") par$subset <- as.integer(par$subset)

      do.call("new", par)

    } else {
      par
    }
  })

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

      if (!length(x$call) == 1 & !all(is.character(x$algorithm))) {
        warning("Call entry must be of length 1!")
        valid <- FALSE
      }

      if (length(x$algorithm) != 1 & !all(is.character(x$algorithm))) {
        warning("Algorithm entry must be of length 1 and type character!")
        valid <- FALSE
      }

      if (!is.list(x$parameters)) {
        warning("Parameters entry must be a list!")
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
  ProcessingSettings(value$call, value$algorithm, value$parameters)
}
