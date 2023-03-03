#' validate_settings
#'
#' @description
#' Validates a settings S3 class object for data processing within the
#' streamFind package.
#'
#' @param x A settings S3 class object.
#'
#' @return Logical of length 1.
#'
#' @export
#'
validate_settings <- function(x = NULL) {
  valid <- FALSE

  if (is.list(x)) {
    if (all(c("call", "algorithm", "parameters") %in% names(x))) {
      valid <- TRUE

      if (!length(x$call) == 1) {
        warning("Call entry must be of length 1!")
        valid <- FALSE
      }

      if (length(x$algorithm) != 1 & !is.character(x$algorithm)) {
        warning("Algorithm entry must be of length 1 and type character!")
        valid <- FALSE
      }

      if (!is.list(x$parameters)) {
        warning("Parameters entry must be a list!")
        valid <- FALSE
      }

      if (valid) {

        processingFunctionCalls <- c(
          "find_features", "annotate_features",
          "load_features_ms1", "load_features_ms2",
          "load_groups_ms1", "load_groups_ms2",
          "group_features", "fill_features",
          "filter_features"
        )

        if (!any(processingFunctionCalls %in% x$call)) {
          warning("Call name not present in msData class processing methods!")
          valid <- FALSE
        }

        if (valid) {
          if ("find_features" %in% x$call) {
            ff_algorithm <- c(
              "openms", "xcms", "xcms3", "envipick",
              "sirius", "kpic2", "safd"
            )

            if (!any(ff_algorithm %in% x$algorithm)) {
              warning("Algorithm not viable for find_feature call!")
              valid <- FALSE
            }
          }

          if ("group_features" %in% x$call) {
            fg_algorithm <- c("openms", "xcms", "xcms3", "kpic2", "sirius")

            if (!any(fg_algorithm %in% x$algorithm)) {
              warning("Algorithm not viable for group_feature call!")
              valid <- FALSE
            }
          }
        }
      }
    } else {
      warning("Settings elements must be named call, algorithm and parameters!")
    }
  }
  valid
}

#' Settings S3 Class Constructor
#'
#' @description
#' Creates a settings S3 class.
#'
#' @param call Character of length one with the name of the method where the
#' settings are to be applied.
#' @param algorithm Character of length one with the name of the algorithm to
#' be used.
#' @param parameters A list with parameter settings specific for the `call`
#' method and used `algorithm`.
#'
#' @details See the method documentation for more information about possible
#' algorithms and parameters to be used.
#'
#' @return A settings S3 class
#'
#' @export
#'
settings <- function(call = NA_character_,
                     algorithm = NA_character_,
                     parameters = NULL) {
  x <- list(
    "call" = call,
    "algorithm" = algorithm,
    "parameters" = parameters
  )

  if (validate_settings(x)) {
    structure(x, class = "settings")
  } else {
    NULL
  }
}

#' @rdname settings
#' @export
as.settings <- function(settings) {
  settings(settings$call, settings$algorithm, settings$parameters)
}


