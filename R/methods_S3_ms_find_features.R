
#' @title .s3_ms_find_features.Settings_find_features_qPeaks
#'
#' @description Finds features using the algorithm qPeaks.
#'
#' @noRd
#'
.s3_ms_find_features.Settings_find_features_qPeaks <- function(settings, self) {

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

#' @title .s3_ms_find_features.patRoon
#'
#' @description Finds features using the package patRoon.
#'
#' @noRd
#'
.s3_ms_find_features.patRoon <- function(settings, self) {

  if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  algorithm <- settings$algorithm

  if (grepl("_", algorithm, fixed = FALSE)) {
    algorithm <- gsub("^(.*?)_.*$", "\\1", algorithm)
  }

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

  anaInfo <- self$get_overview()

  anaInfo <- data.frame(
    "path" = dirname(anaInfo$file),
    "analysis" = anaInfo$analysis,
    "group" = anaInfo$replicate,
    "blank" = anaInfo$blank
  )

  anaInfo$blank[is.na(anaInfo$blank)] <- ""

  anaInfo$algorithm <- algorithm

  ag <- list(analysisInfo = anaInfo, algorithm = algorithm)

  pp_fun <- patRoon::findFeatures

  pat <- do.call(pp_fun, c(ag, parameters, verbose = FALSE))

  features <- .build_features_table_from_patRoon(pat, self)

  if (any(self$has_features())) self$remove_features()

  self$add_features(features, replace = TRUE)

  TRUE
}

#' @title .build_features_table_from_patRoon
#'
#' @param pat An object with class `features` or `featureGroups` from the
#' package \pkg{patRoon}.
#'
#' @param self A `MassSpecData` object. When applied within the R6, the self
#' object.
#'
#' @return A list with a features \linkS4class{data.table} for each analysis.
#'
#' @noRd
#'
.build_features_table_from_patRoon <- function(pat, self) {

  if ("features" %in% is(pat)) {
    anaInfo <- pat@analysisInfo
    isSet <- TRUE %in% grepl("Set", is(pat))
    features <- pat@features
    if ("featuresXCMS3" %in% is(pat)) {
      if (xcms::hasFilledChromPeaks(pat@xdata)) {
        extra <- xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE)
        extra$is_filled <- as.logical(extra$is_filled)
        extra$analysis <- anaInfo$analysis[extra$sample]
        extra <- split(extra, extra$analysis)
      } else {
        extra <- NULL
      }
    } else {
      extra <- NULL
    }
  }

  if ("featureGroups" %in% is(pat)) {
    anaInfo <- pat@analysisInfo
    features <- copy(pat@features@features)
    isSet <- TRUE %in% grepl("Set", is(pat))
    if ("featureGroupsXCMS3" %in% is(pat)) {
      if (xcms::hasFilledChromPeaks(pat@xdata)) {
        extra <- xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE)
        extra$is_filled <- as.logical(extra$is_filled)
        extra$analysis <- anaInfo$analysis[extra$sample]
        extra <- split(extra, extra$analysis)
      } else {
        extra <- NULL
      }
    } else {
      extra <- NULL
    }
  }

  analyses <- names(features)

  features <- lapply(analyses, function(x, extra, features, self, isSet) {
    temp <- features[[x]]

    valid = TRUE

    if (!is.data.frame(temp)) valid <- FALSE

    if (valid & nrow(temp) == 0) valid <- FALSE

    if (!valid) return(data.table())

    if (!is.null(extra)) {
      if (temp == nrow(extra[[x]]) & all(temp$mz == extra[[x]]$mz)) {
        temp$filled <- extra[[x]]$is_filled
      }
    }

    under_rt_max <- temp$rt <= temp$rtmax
    if (!all(under_rt_max)) {
      warning("Feature retention time value/s above the rtmax!")
    }

    under_rt_min <- temp$rt >= temp$rtmin
    if (!all(under_rt_min)) {
      warning("Feature retention time value/s under the rtmin!")
    }

    under_mz_max <- temp$mz <= temp$mzmax
    if (!all(under_rt_min)) {
      warning("Feature m/z value/s above the mzmax!")
    }

    under_mz_min <- temp$mz >= temp$mzmin
    if (!all(under_rt_min)) {
      warning("Feature m/z value/s under the mzmin!")
    }

    polarity <- self$get_polarities(x)

    if (polarity %in% "positive") {
      adduct <- "[M+H]+"
      adduct_val <- -1.007276
    }

    if (polarity %in% "negative") {
      adduct <- "[M-H]-"
      adduct_val <- 1.007276
    }

    # required as when is set the mz value is neutralized from patRoon
    if (isSet) {
      temp[temp$adduct %in% "[M-H]-", `:=`(
        mzmin = (temp$mz - 1.007276) - (temp$mz - temp$mzmin),
        mzmax = (temp$mz - 1.007276) + (temp$mzmax - temp$mz),
        mz = temp$mz - 1.007276
      )]
      temp[temp$adduct %in% "[M+H]+", `:=`(
        mzmin = (temp$mz + 1.007276) - (temp$mz - temp$mzmin),
        mzmax = (temp$mz + 1.007276) + (temp$mzmax - temp$mz),
        mz = temp$mz + 1.007276
      )]
    }

    if (!"adduct" %in% colnames(temp)) temp$adduct <- adduct
    if (!"mass" %in% colnames(temp)) temp$mass <- temp$mz + adduct_val
    if (!"filled" %in% colnames(temp)) {
      temp$filled <- FALSE
    } else {
      temp$filled <- as.logical(temp$filled)
    }
    if (!"filtered" %in% colnames(temp)) temp$filtered <- FALSE
    if (!"filter" %in% colnames(temp)) temp$filter <- NA_character_

    setnames(temp,
             c("ID", "ret", "retmin", "retmax"),
             c("feature", "rt", "rtmin", "rtmax"),
             skip_absent = TRUE
    )

    # when grouping features are removed from grouping conditions in patRoon
    # therefore, old features are retained and tagged with filter "grouping"
    temp_org <- self$get_features(x)
    build_feature_ids <- TRUE

    if (nrow(temp_org) > 0 && "featureGroups" %in% is(pat)) {

      temp_org$analysis <- NULL

      if (nrow(temp_org) != nrow(temp)) {

        build_feature_ids <- FALSE

        #modify the feature ids from original ids when numeric
        if (is.numeric(temp$feature)) {
          temp$feature <- temp_org$feature[temp$feature]
        }

        temp_org_not_grouped <- temp_org[!temp_org$feature %in% temp$feature, ]

        temp_list <- list(temp, temp_org_not_grouped)
        temp <- rbindlist(temp_list, fill = TRUE)
      }
    }

    if ("group" %in% colnames(temp)) {
      temp$filter[is.na(temp$group)] <- "grouping"
      temp$filtered[is.na(temp$group)] <- TRUE
    }

    temp <- temp[order(temp$mz), ]
    temp <- temp[order(temp$rt), ]
    temp <- temp[order(temp$filtered), ]

    if (build_feature_ids) {
      temp$index <- seq_len(nrow(temp))

      d_dig <- max(temp$mzmax - temp$mzmin)
      d_dig <- sub('.*\\.(0+)[1-9].*', '\\1', as.character(d_dig))
      d_dig <- nchar(d_dig) + 1

      temp$feature <- paste0(
        "mz",
        round(temp$mz, digits = d_dig),
        "_rt",
        round(temp$rt, digits = 0),
        "_f",
        temp$index
      )
    }

    setcolorder(
      temp,
      c(
        "feature", "index", "rt", "mz", "intensity", "area",
        "rtmin", "rtmax", "mzmin", "mzmax", "adduct", "mass",
        "filled", "filtered", "filter"
      )
    )

    temp$rt <- round(temp$rt, 3)
    temp$rtmin <- round(temp$rtmin, 3)
    temp$rtmax <- round(temp$rtmax, 3)

    temp$mz <- round(temp$mz, 8)
    temp$mzmin <- round(temp$mzmin, 8)
    temp$mzmax <- round(temp$mzmax, 8)

    temp
  }, extra = extra, features = features, self = self, isSet = isSet)

  names(features) <- analyses

  features
}
