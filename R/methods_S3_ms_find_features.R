
#' @title .s3_ms_find_features.patRoon
#'
#' @description Finds features using the package patRoon.
#'
#' @noRd
#'
.s3_ms_find_features.patRoon <- function(settings, self, private) {
  
  anaInfo <- self$analysisInfo
  
  if (nrow(anaInfo) == 0) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  algorithm <- settings$algorithm

  if (grepl("_", algorithm, fixed = FALSE)) algorithm <- gsub("^(.*?)_.*$", "\\1", algorithm)

  if ("xcms" %in% algorithm || "xcms3" %in% algorithm) {
    if (!requireNamespace("xcms")) {
      warning("xcms package is not installed!")
      return(FALSE)
    }
  }

  parameters <- settings$parameters

  if (any(grepl("class|Class", names(parameters)))) {
    parameters[["Class"]] <- parameters$class
    parameters[["class"]] <- NULL

    parameters <- lapply(parameters, function(z) {
      if (is.list(z) & length(z) > 0) {
        z[[1]]
      } else {
        z
      }
    })

    if (parameters$Class %in% "CentWaveParam") {
      parameters$roiScales <- as.double()
      parameters$integrate <- as.integer(parameters$integrate)
    }

    parameters <- do.call("new", parameters)

  } else if (is.list(parameters)) {

    parameters <- lapply(parameters, function(par) {
      if (is.list(par)) {
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

          if (par$Class %in% "CentWaveParam") {
            par$roiScales <- as.double()
            par$integrate <- as.integer(par$integrate)
          }

          par <- do.call("new", par)
        }
      }
      par
    })
  }

  if (isS4(parameters)) parameters <- list("param" = parameters)

  anaInfo$algorithm <- algorithm

  ag <- list(analysisInfo = anaInfo, algorithm = algorithm)

  pp_fun <- patRoon::findFeatures
  
  if (!"verbose" %in% names(parameters)) parameters[["verbose"]] <- TRUE
  
  pat <- do.call(pp_fun, c(ag, parameters))
  
  for (x in patRoon::analyses(pat)) {
    pat@features[[x]]$filtered <- FALSE
    pol <- self$get_polarities(x)
    if ("positive" %in% pol) adduct_val <- -1.007276
    if ("negative" %in% pol) adduct_val <- 1.007276
    pat@features[[x]]$mass <- pat@features[[x]]$mz + adduct_val
  }
  
  self$features <- pat
  
  TRUE
}

#' @title .s3_ms_find_features.Settings_find_features_qPeaks
#'
#' @description Finds features using the algorithm qPeaks.
#'
#' @noRd
#'
.s3_ms_find_features.Settings_find_features_qPeaks <- function(settings, self, private) {
  
  message("Finding features with qPeaks...", appendLF = TRUE)
  
  if (!any(self$has_features_eic())) {
    warning("Feature EICs not found! Run bin_spectra to build feature EICs first.")
    return(FALSE)
  }
  
  eics <- self$get_features_eic()
  
  # TODO Max Implementation qPeaks method
  
  # self$add_features(features)
  
  FALSE
}
