#' @noRd
.run_group_features_patRoon <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$has_results_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }

  NTS <- engine$NTS

  if (!NTS@has_features) {
    warning("NTS object is empty! Not done.")
    return(FALSE)
  }

  pat_features <- get_patRoon_features(NTS, filtered = FALSE, featureGroups = FALSE)

  algorithm <- x$algorithm

  if (grepl("_", algorithm, fixed = FALSE)) algorithm <- gsub("^(.*?)_.*$", "\\1", algorithm)

  if ("xcms" %in% algorithm || "xcms3" %in% algorithm) {
    if (!requireNamespace("xcms")) {
      warning("xcms package is not installed!")
      return(FALSE)
    }
  }

  parameters <- x$parameters

  if ("class" %in% names(parameters)) {
    parameters[["Class"]] <- parameters$class
    parameters[["class"]] <- NULL

    parameters <- lapply(parameters, function(z) {
      if (is.list(z) & length(z) > 0) {
        z[[1]]
      } else {
        z
      }
    })

    if (parameters$Class %in% "PeakGroupsParam") {
      parameters$peakGroupsMatrix <- as.matrix(parameters$peakGroupsMatrix)
    }

    if (parameters$Class %in% "PeakGroupsParam") {
      parameters$subset <- as.integer(parameters$subset)
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

          if (par$Class %in% "PeakGroupsParam") {
            par$peakGroupsMatrix <- as.matrix(par$peakGroupsMatrix)
          }

          if (par$Class %in% "PeakGroupsParam") {
            par$subset <- as.integer(par$subset)
          }

          par <- do.call("new", par)
        }
      }
      par
    })
  }

  if (algorithm == "xcms3") {
    if ("Param" %in% is(parameters)) {
      parameters <- list("groupParam" = parameters)
    }

    parameters$groupParam@sampleGroups <- engine$Analyses@replicates

    if ("rtalign" %in% names(parameters)) {
      if (parameters$rtalign) {
        parameters$preGroupParam@sampleGroups <- engine$Analyses@replicates
      }
    }

    # when multiple polarities it makes setFeatureGroups, no rt alignment possible
    if (length(unique(engine$get_spectra_polarity())) > 1) {
      parameters <- parameters["groupParam"]
    }
  }

  ag <- list("obj" = pat_features, "algorithm" = algorithm)

  if (!"verbose" %in% names(parameters)) parameters[["verbose"]] <- TRUE

  pat <- do.call(patRoon::groupFeatures, c(ag, parameters))

  pat_fl <- pat@features@features

  pat_fl <- pat_fl[NTS@analyses_info$analysis]

  fl <- NTS@feature_list

  fl <- Map(function(z, y) {
    
    z_na <- z[!z$feature %in% y$ID, ]
    
    if (any(!z_na$filtered)) {
      z_na$filtered[!z_na$filtered] <- TRUE
      z_na$filter[!z_na$filtered] <- "GroupFeatures"
    }
    
    z <- z[z$feature %in% y$ID, ]
    
    ID_grps <- y$group
    names(ID_grps) <- y$ID
    
    z$group <- ID_grps[z$feature]
    
    z <- data.table::rbindlist(list(z, z_na))
    z
  }, fl, pat_fl)

  names(fl) <- NTS@analyses_info$analysis

  NTS@feature_list <- fl

  if (is(NTS, "StreamFind::NTS")) {
    engine$NTS <- NTS
    TRUE
  } else {
    FALSE
  }
}

# .extract_time_alignment <- function(pat, self) {
#   if ("featureGroupsXCMS3" %in% is(pat)) {
#
#     if (xcms::hasAdjustedRtime(pat@xdata)) {
#       rtAdj <- xcms::adjustedRtime(pat@xdata)
#       pkAdj <- xcms::processHistory(pat@xdata,
#                                     type = "Retention time correction"
#       )[[1]]
#       pkAdj <- pkAdj@param
#
#       addAdjPoints <- FALSE
#       if ("PeakGroupsParam" %in% is(pkAdj)) {
#         addAdjPoints <- TRUE
#         pkAdj <- xcms::peakGroupsMatrix(pkAdj)
#       }
#
#       # hasSpectra = all(self$has_loaded_spectra())
#       hasSpectra <- FALSE
#
#       if (!hasSpectra) {
#         rtOrg <- lapply(self$get_files(), function(x) {
#           file_link <- mzR::openMSfile(x, backend = "pwiz")
#           sH <- suppressWarnings(mzR::header(file_link))
#           suppressWarnings(mzR::close(file_link))
#           sH$retentionTime
#         })
#       }
#
#       alignment <- lapply(self$get_analysis_names(),
#         function(ana, rtOrg, rtAdj, addAdjPoints, pkAdj, all_ana) {
#           ana_idx <- which(all_ana %in% ana)
#           n_ana <- length(all_ana)
#
#           rts <- names(rtAdj)
#           ana_idx_string <- paste0(
#             "F",
#             paste(rep("0", nchar(n_ana) - nchar(ana_idx)), collapse = ""),
#             ana_idx
#           )
#           rts <- grepl(ana_idx_string, rts)
#           rts <- rtAdj[rts]
#
#           temp <- data.frame(
#             "rt_original" = rtOrg[[ana]],
#             "rt_adjusted" = rts
#           )
#
#           temp$adjustment <- temp$rt_original - temp$rt_adjusted
#
#           if (addAdjPoints) {
#             adjPoints <- unique(pkAdj[, ana_idx])
#             adjPoints <- adjPoints[adjPoints %in% temp$rt_original]
#             temp$adjPoints[temp$rt_original %in% adjPoints] <- adjPoints
#           }
#           row.names(temp) <- seq_len(nrow(temp))
#           temp
#         },
#         rtOrg = rtOrg,
#         rtAdj = rtAdj,
#         addAdjPoints = addAdjPoints,
#         pkAdj = pkAdj,
#         all_ana = self$get_analysis_names()
#       )
#
#       return(alignment)
#     }
#   }
#   NULL
# }

#' **MassSpecMethod_GroupFeatures_xcms3_peakdensity**
#'
#' @description Settings for grouping features (i.e., chromatographic peaks) across mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms} (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/groupChromPeaks-density.html}{peakDensity}.
#' The function uses the package \pkg{patRoon} in the background.
#'
#' @param bw numeric(1) defining the bandwidth (standard deviation of the smoothing kernel) to be used. This argument
#' is passed to the `density()` method.
#' @param minFraction numeric(1) defining the minimum fraction of analyses in at least one analysis replicate group in
#' which the features have to be present to be considered as a feature group.
#' @param minSamples numeric(1) with the minimum number of analyses in at least one analysis replicate group in which
#' the features have to be detected to be considered a feature group.
#' @param binSize numeric(1) defining the size of the overlapping slices in mz dimension.
#' @param maxFeatures numeric(1) with the maximum number of feature groups to be identified in a single mz slice.
#'
#' @details See the \link[patRoon]{groupFeaturesXCMS3} function from the \pkg{patRoon} package for more information and requirements.
#'
#' @return A `MassSpecMethod_GroupFeatures_xcms3_peakdensity` object.
#'
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' \insertRef{xcms01}{StreamFind}
#'
#' \insertRef{xcms02}{StreamFind}
#'
#' \insertRef{xcms03}{StreamFind}
#'
#' @export
#'
MassSpecMethod_GroupFeatures_xcms3_peakdensity <- S7::new_class(
  name = "MassSpecMethod_GroupFeatures_xcms3_peakdensity",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(bw = 5,
                         minFraction = 1,
                         minSamples = 1,
                         binSize = 0.008,
                         maxFeatures = 100) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "GroupFeatures",
        required = "FindFeatures",
        algorithm = "xcms3_peakdensity",
        parameters = list(
          bw = bw,
          minFraction = minFraction,
          minSamples = minSamples,
          binSize = binSize,
          maxFeatures = maxFeatures
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "xcms",
        developer = "Colin Smith, Johannes Rainer",
        contact = "siuzdak@scripps.edu",
        link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
        doi = "https://doi.org/10.1021/ac051437y"
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "GroupFeatures")
    checkmate::assert_choice(self@algorithm, "xcms3_peakdensity")
    checkmate::assert_numeric(self@parameters$bw, len = 1)
    checkmate::assert_numeric(self@parameters$minFraction, len = 1)
    checkmate::assert_numeric(self@parameters$minSamples, len = 1)
    checkmate::assert_numeric(self@parameters$binSize, len = 1)
    checkmate::assert_numeric(self@parameters$maxFeatures, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_GroupFeatures_xcms3_peakdensity) <- function(x, engine = NULL) {
  settings <- list()

  settings[["algorithm"]] <- x$algorithm

  parameters <- list(
    "rtalign" = FALSE,
    "groupParam" = list(
      class = "PeakDensityParam",
      sampleGroups = "holder",
      bw = x$parameters$bw,
      minFraction = x$parameters$minFraction,
      minSamples = x$parameters$minSamples,
      binSize = x$parameters$binSize,
      maxFeatures = x$parameters$maxFeatures
    )
  )

  settings[["parameters"]] <- parameters

  .run_group_features_patRoon(settings, engine)
}

#' **MassSpecMethod_GroupFeatures_xcms3_peakdensity_peakgroups**
#'
#' @description Settings for aligning and grouping features (i.e., chromatographic peaks) across mzML/mzXML files using
#' the package \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms} (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/adjustRtime-peakGroups.html}{peakGroups} for retention time alignment and the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/groupChromPeaks-density.html}{peakdensity} for grouping. The function uses the
#' package \pkg{patRoon} in the background.
#'
#' @param bw numeric(1) defining the bandwidth (standard deviation of the
#' smoothing kernel) to be used. This argument is passed to the `density()`
#' method.
#' @param minFraction numeric(1) defining the minimum fraction of analyses in at
#' least one analysis replicate group in which the features have to be present
#' to be considered as a feature group.
#' @param minSamples numeric(1) with the minimum number of analyses in at least
#' one analysis replicate group in which the features have to be detected to be
#' considered a feature group.
#' @param binSize numeric(1) defining the size of the overlapping slices in mz
#' dimension.
#' @param pre_bw as `bw` but applied before retention time alignment.
#' @param pre_minFraction as `minFraction` but applied before retention time
#' alignment.
#' @param pre_minSamples as `minSamples` but applied before retention time
#' alignment.
#' @param pre_binSize as `binSize` but applied before retention time alignment.
#' @param maxFeatures numeric(1) with the maximum number of feature groups to be
#' identified in a single mz slice.
#' @param rtAlignMinFraction numeric(1) between 0 and 1 defining the minimum
#' required fraction of samples in which peaks for the peak group were identified.
#' Peak groups passing this criteria will aligned across samples and retention
#' times of individual spectra will be adjusted based on this alignment.
#' For minFraction = 1 the peak group has to contain peaks in all samples of
#' the experiment. Note that if subset is provided, the specified fraction is
#' relative to the defined subset of samples and not to the total number of
#' samples within the experiment (i.e. a peak has to be present in the specified
#' proportion of subset samples).
#' @param extraPeaks numeric(1) defining the maximal number of additional peaks
#' for all samples to be assigned to a peak group (i.e. feature) for retention
#' time correction. For a data set with 6 samples, extraPeaks = 1 uses all peak
#' groups with a total peak count <= 6 + 1. The total peak count is the total
#' number of peaks being assigned to a peak group and considers also multiple
#' peaks within a sample being assigned to the group.
#' @param smooth character defining the function to be used, to interpolate
#' corrected retention times for all peak groups. Either "loess" or "linear".
#' @param span numeric(1) defining the degree of smoothing (if smooth = "loess").
#' This parameter is passed to the internal call to loess.
#' @param family character defining the method to be used for loess smoothing.
#' Allowed values are "gaussian" and "symmetric".See loess for more information.
#' @param subset integer with the indices of samples within the experiment on
#' which the alignment models should be estimated. Samples not part of the subset
#' are adjusted based on the closest subset sample. See description above
#' for more details.
#' @param subsetAdjust character specifying the method with which non-subset
#' samples should be adjusted. Supported options are "previous" and "average"
#' (default). See description above for more information.
#'
#' @details See the \link[patRoon]{groupFeaturesXCMS3} function from the \pkg{patRoon} package for
#' more information and requirements.
#'
#' @return A `MassSpecMethod_GroupFeatures_xcms3_peakdensity_peakgroups` object.
#'
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' \insertRef{xcms01}{StreamFind}
#'
#' \insertRef{xcms02}{StreamFind}
#'
#' \insertRef{xcms03}{StreamFind}
#'
#' @export
#'
MassSpecMethod_GroupFeatures_xcms3_peakdensity_peakgroups <- S7::new_class(
  name = "MassSpecMethod_GroupFeatures_xcms3_peakdensity_peakgroups",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(bw = 5,
                         minFraction = 1,
                         minSamples = 1,
                         binSize = 0.008,
                         pre_bw = 5,
                         pre_minFraction = 1,
                         pre_minSamples = 1,
                         pre_binSize = 0.008,
                         maxFeatures = 100,
                         rtAlignMinFraction = 0.9,
                         extraPeaks = 1,
                         smooth = "loess",
                         span = 0.2,
                         family = "gaussian",
                         subset = integer(),
                         subsetAdjust = "average") {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "GroupFeatures",
        required = "FindFeatures",
        algorithm = "xcms3_peakdensity_peakgroups",
        parameters = list(
          bw = bw,
          minFraction = minFraction,
          minSamples = minSamples,
          binSize = binSize,
          pre_bw = pre_bw,
          pre_minFraction = pre_minFraction,
          pre_minSamples = pre_minSamples,
          pre_binSize = pre_binSize,
          maxFeatures = maxFeatures,
          rtAlignMinFraction = rtAlignMinFraction,
          extraPeaks = extraPeaks,
          smooth = smooth,
          span = span,
          family = family,
          subset = subset,
          subsetAdjust = subsetAdjust
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "xcms",
        developer = "Colin Smith, Johannes Rainer",
        contact = "siuzdak@scripps.edu",
        link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
        doi = "https://doi.org/10.1021/ac051437y"
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "GroupFeatures")
    checkmate::assert_choice(self@algorithm, "xcms3_peakdensity_peakgroups")
    checkmate::assert_numeric(self@parameters$bw, len = 1)
    checkmate::assert_numeric(self@parameters$minFraction, len = 1)
    checkmate::assert_numeric(self@parameters$minSamples, len = 1)
    checkmate::assert_numeric(self@parameters$binSize, len = 1)
    checkmate::assert_numeric(self@parameters$pre_bw, len = 1)
    checkmate::assert_numeric(self@parameters$pre_minFraction, len = 1)
    checkmate::assert_numeric(self@parameters$pre_minSamples, len = 1)
    checkmate::assert_numeric(self@parameters$pre_binSize, len = 1)
    checkmate::assert_numeric(self@parameters$maxFeatures, len = 1)
    checkmate::assert_numeric(self@parameters$rtAlignMinFraction, len = 1)
    checkmate::assert_numeric(self@parameters$extraPeaks, len = 1)
    checkmate::assert_choice(self@parameters$smooth, c("loess", "linear"))
    checkmate::assert_numeric(self@parameters$span, len = 1)
    checkmate::assert_choice(self@parameters$family, c("gaussian", "symmetric"))
    checkmate::assert_integer(self@parameters$subset)
    checkmate::assert_choice(self@parameters$subsetAdjust, c("previous", "average"))
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_GroupFeatures_xcms3_peakdensity_peakgroups) <- function(x, engine = NULL) {
  settings <- list()

  settings[["algorithm"]] <- x$algorithm

  parameters <- list(
    "rtalign" = TRUE,
    "groupParam" = list(
      class = "PeakDensityParam",
      sampleGroups = "holder",
      bw = x$parameters$bw,
      minFraction = x$parameters$minFraction,
      minSamples = x$parameters$minSamples,
      binSize = x$parameters$binSize,
      maxFeatures = x$parameters$maxFeatures
    ),
    "preGroupParam" = list(
      class = "PeakDensityParam",
      sampleGroups = "holder",
      bw = x$parameters$pre_bw,
      minFraction = x$parameters$pre_minFraction,
      minSamples = x$parameters$pre_minSamples,
      binSize = x$parameters$pre_binSize,
      maxFeatures = x$parameters$maxFeatures
    ),
    "retAlignParam" = list(
      class = "PeakGroupsParam",
      minFraction = x$parameters$rtAlignMinFraction,
      extraPeaks = x$parameters$extraPeaks,
      smooth = x$parameters$smooth,
      span = x$parameters$span,
      family = x$parameters$family,
      peakGroupsMatrix = matrix(nrow = 0, ncol = 0),
      subset = as.integer(x$parameters$subset),
      subsetAdjust = x$parameters$subsetAdjust
    )
  )

  settings[["parameters"]] <- parameters

  .run_group_features_patRoon(settings, engine)
}

#' **MassSpecMethod_GroupFeatures_openms**
#'
#' @description Settings for grouping features (i.e., chromatographic peaks) in mzML/mzXML files using the
#' \href{https://www.openms.org/}{OpenMS}(\url{https://abibuilder.cs.uni-tuebingen.de/archive/openms/}) software
#' with the algorithm \href{https://abibuilder.cs.uni-tuebingen.de/archive/openms/Documentation/release/3.0.0/html/TOPP_FeatureLinkerUnlabeled.html}{FeatureLinkerUnlabeled}.
#' The function uses the package \pkg{patRoon} in the background.
#'
#' @param rtalign Logical length one. Set to TRUE to enable retention time alignment.
#' @param QT Logical length one. When TRUE the FeatureLinkerUnlabeledQT is used
#' instead of FeatureLinkerUnlabeled for grouping features.
#' @param maxAlignRT Numeric length one. Maximum retention time (in seconds) for
#' feature pairing when performing retention time alignment.
#' @param maxAlignMZ Numeric length one. Maximum *m/z* (in Da) for
#' feature pairing when performing retention time alignment.
#' @param maxGroupRT Numeric length one. Maximum retention time (in seconds) for
#' feature pairing when performing grouping.
#' @param maxGroupMZ Numeric length one. Maximum *m/z* (in Da) for
#' feature pairing when performing grouping.
#' @param verbose Logical of length one. When TRUE adds processing information
#' to the console.
#'
#' @details See the \link[patRoon]{groupFeaturesOpenMS} function from the \pkg{patRoon} package for
#' more information and requirements.
#'
#' @return A `MassSpecMethod_GroupFeatures_openms` object.
#'
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' \insertRef{openms01}{StreamFind}
#'
#' @export
#'
MassSpecMethod_GroupFeatures_openms <- S7::new_class(
  name = "MassSpecMethod_GroupFeatures_openms",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(rtalign = FALSE,
                         QT = FALSE,
                         maxAlignRT = 5,
                         maxAlignMZ = 0.008,
                         maxGroupRT = 5,
                         maxGroupMZ = 0.008,
                         verbose = FALSE) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "GroupFeatures",
        required = "FindFeatures",
        algorithm = "openms",
        parameters = list(
          rtalign = rtalign,
          QT = QT,
          maxAlignRT = maxAlignRT,
          maxAlignMZ = maxAlignMZ,
          maxGroupRT = maxGroupRT,
          maxGroupMZ = maxGroupMZ,
          verbose = verbose
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "openms",
        developer = "Oliver Kohlbacher",
        contact = "oliver.kohlbacher@uni-tuebingen.de",
        link = "https://openms.de/",
        doi = "https://doi.org/10.1038/nmeth.3959"
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "GroupFeatures")
    checkmate::assert_choice(self@algorithm, "openms")
    checkmate::assert_logical(self@parameters$rtalign, len = 1)
    checkmate::assert_logical(self@parameters$QT, len = 1)
    checkmate::assert_numeric(self@parameters$maxAlignRT, len = 1)
    checkmate::assert_numeric(self@parameters$maxAlignMZ, len = 1)
    checkmate::assert_numeric(self@parameters$maxGroupRT, len = 1)
    checkmate::assert_numeric(self@parameters$maxGroupMZ, len = 1)
    checkmate::assert_logical(self@parameters$verbose, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_GroupFeatures_openms) <- function(x, engine = NULL) {
  .run_group_features_patRoon(x, engine)
}
