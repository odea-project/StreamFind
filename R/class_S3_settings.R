#' Settings S3 Class Constructor
#'
#' @description
#' Creates a settings S3 class.
#'
#' @param call Character of length one with the name of the method where the
#' settings are to be applied.
#' @param algorithm Character of length one with the name of the algorithm to
#' be used.
#' @param parameters List with parameter settings specific for the `call`
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

  if (validate.settings(x)) {
    structure(x, class = "settings")
  } else {
    NULL
  }
}

#' @describeIn settings
#' Validates a settings S3 class object, returning a logical value of length
#' one.
#'
#' @param settings A settings S3 class object.
#'
#' @export
#'
validate.settings <- function(settings) {
  valid <- FALSE

  if (is.list(settings)) {
    if (all(c("call", "algorithm", "parameters") %in% names(settings))) {
      valid <- TRUE

      if (!length(settings$call) == 1) {
        warning("Call entry must be of length 1!")
        valid <- FALSE
      }

      if (length(settings$algorithm) != 1 & !is.character(settings$algorithm)) {
        warning("Algorithm entry must be of length 1 and type character!")
        valid <- FALSE
      }

      if (!is.list(settings$parameters)) {
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

        if (!any(processingFunctionCalls %in% settings$call)) {
          warning("Call name not present in msData class processing methods!")
          valid <- FALSE
        }

        if (valid) {
          if ("find_features" %in% settings$call) {
            ff_algorithm <- c(
              "openms", "xcms", "xcms3", "envipick",
              "sirius", "kpic2", "safd"
            )

            if (!any(ff_algorithm %in% settings$algorithm)) {
              warning("Algorithm not viable for find_feature call!")
              valid <- FALSE
            }
          }

          if ("group_features" %in% settings$call) {
            fg_algorithm <- c("openms", "xcms", "xcms3", "kpic2", "sirius")

            if (!any(fg_algorithm %in% settings$algorithm)) {
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

#' @describeIn settings
#' Converts the argument in a settings S3 class object
#'
#' @param value A list to be checked and/or converted to settings S3 class.
#'
#' @export
as.settings <- function(value) {
  must_have_elements <- c("call", "algorithm", "parameters")
  if (!all(must_have_elements %in% names(value))) return(NULL)
  settings(value$call, value$algorithm, value$parameters)
}


