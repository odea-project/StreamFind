#' @noRd
.run_find_features_patRoon <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (length(engine$Analyses$analyses) == 0) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  anaInfo <- data.frame(
    "path" = dirname(vapply(engine$Analyses$analyses, function(z) z$file, NA_character_)),
    "analysis" = vapply(engine$Analyses$analyses, function(z) z$name, NA_character_),
    "group" = vapply(engine$Analyses$analyses, function(z) z$replicate, NA_character_),
    "blank" = vapply(engine$Analyses$analyses, function(z) z$blank, NA_character_)
  )

  anaInfo$blank[is.na(anaInfo$blank)] <- ""

  if (nrow(anaInfo) == 0) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  algorithm <- x$algorithm

  if (grepl("_", algorithm, fixed = FALSE)) {
    algorithm <- gsub("^(.*?)_.*$", "\\1", algorithm)
  }

  parameters <- x$parameters

  if ("xcms" %in% algorithm || "xcms3" %in% algorithm) {
    if (!requireNamespace("xcms")) {
      warning("xcms package is not installed!")
      return(FALSE)
    }
    if (x$method %in% "FindFeatures") {
      parameters <- do.call(xcms::CentWaveParam, x$parameters)
    }
  }

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

  if (isS4(parameters)) {
    parameters <- list("param" = parameters)
  }

  anaInfo$algorithm <- algorithm

  ag <- list(analysisInfo = anaInfo, algorithm = algorithm)

  pp_fun <- patRoon::findFeatures

  if (!"verbose" %in% names(parameters)) {
    parameters[["verbose"]] <- TRUE
  }

  pat <- do.call(pp_fun, c(ag, parameters))

  for (a in patRoon::analyses(pat)) {
    pol <- paste0(unique(engine$Analyses$analyses[[a]]$spectra_headers$polarity), collapse = ", ")

    if (grepl(",", pol)) {
      warning(
        "Multiple polarities in analysis ",
        a,
        " not supported by patRoon! Mass for features could not be estimated."
      )
      pat@features[[a]]$polarity <- 0
      pat@features[[a]]$mass <- NA_real_
      next
    }

    if (grepl("0", pol)) {
      warning(
        "Unknown polarity in analyses",
        a,
        "! Mass for features could not be estimated."
      )
      pat@features[[a]]$polarity <- 0
      pat@features[[a]]$mass <- NA_real_
      next
    }

    if ("1" %in% pol) {
      adduct_val <- -1.007276
      pat@features[[a]]$polarity <- 1
      pat@features[[a]]$mass <- pat@features[[a]]$mz + adduct_val
      pat@features[[a]]$adduct <- "[M+H]+"
    }

    if ("-1" %in% pol) {
      adduct_val <- 1.007276
      pat@features[[a]]$polarity <- -1
      pat@features[[a]]$mass <- pat@features[[a]]$mz + adduct_val
      pat@features[[a]]$adduct <- "[M-H]-"
    }

    n_features <- nrow(pat@features[[a]])
    empty_dt_list <- list(rep(data.table::data.table(), n_features))

    pat@features[[a]]$ID <- paste0(
      "F",
      seq_len(n_features),
      "_MZ",
      round(pat@features[[a]]$mz, digits = 0),
      "_RT",
      round(pat@features[[a]]$ret, digits = 0)
    )

    pat@features[[a]]$mz <- round(pat@features[[a]]$mz, 5)
    pat@features[[a]]$mzmin <- round(pat@features[[a]]$mzmin, 5)
    pat@features[[a]]$mzmax <- round(pat@features[[a]]$mzmax, 5)
    pat@features[[a]]$mass <- round(pat@features[[a]]$mass, 5)
    pat@features[[a]]$ret <- round(pat@features[[a]]$ret, 2)
    pat@features[[a]]$retmin <- round(pat@features[[a]]$retmin, 2)
    pat@features[[a]]$retmax <- round(pat@features[[a]]$retmax, 2)
    pat@features[[a]]$intensity <- round(pat@features[[a]]$intensity, 2)
    pat@features[[a]]$area <- round(pat@features[[a]]$area, 2)

    pat@features[[a]]$filtered <- FALSE
    pat@features[[a]]$filter <- ""
    pat@features[[a]]$filled <- FALSE
    pat@features[[a]]$correction <- 1
    pat@features[[a]]$group <- ""
    pat@features[[a]]$quality <- empty_dt_list
    pat@features[[a]]$annotation <- empty_dt_list
    pat@features[[a]]$eic <- empty_dt_list
    pat@features[[a]]$ms1 <- empty_dt_list
    pat@features[[a]]$ms2 <- empty_dt_list
    pat@features[[a]]$istd <- empty_dt_list
    pat@features[[a]]$suspects <- empty_dt_list
    pat@features[[a]]$formulas <- empty_dt_list
    pat@features[[a]]$compounds <- empty_dt_list
  }

  feature_list <- pat@features

  feature_list <- lapply(feature_list, function(z) {
    data.table::setnames(z, "ID", "feature", skip_absent = TRUE)
    data.table::setnames(z, "ret", "rt", skip_absent = TRUE)
    data.table::setnames(z, "retmin", "rtmin", skip_absent = TRUE)
    data.table::setnames(z, "retmax", "rtmax", skip_absent = TRUE)
  })

  names(feature_list) <- patRoon::analyses(pat)

  analyses_names <- get_names(engine$Analyses)

  pols <- vapply(
    engine$Analyses$analyses, function(a) {
      paste0(unique(a$spectra_headers$polarity), collapse = ", ")
    },
    "0"
  )

  analyses_info <- data.table::data.table(
    "analysis" = analyses_names,
    "replicate" = vapply(engine$Analyses$analyses, function(z) z$replicate, NA_character_),
    "blank" = vapply(engine$Analyses$analyses, function(z) z$blank, NA_character_),
    "polarity" = pols,
    "file" = vapply(engine$Analyses$analyses, function(z) z$file, NA_character_)
  )

  headers <- lapply(engine$Analyses$analyses, function(a) a$spectra_headers)

  feature_list <- feature_list[analyses_names]

  fp <- c(
    "feature",
    "group",
    "rt",
    "mz",
    "intensity",
    "area",
    "rtmin",
    "rtmax",
    "mzmin",
    "mzmax",
    "mass",
    "polarity",
    "adduct",
    "filtered",
    "filter",
    "filled",
    "correction",
    "eic",
    "ms1",
    "ms2",
    "quality",
    "annotation",
    "istd",
    "suspects",
    "formulas",
    "compounds"
  )

  feature_list <- lapply(
    feature_list,
    function(z, fp) {
      z[, fp, with = FALSE]
    },
    fp = fp
  )

  nts <- MassSpecResults_NonTargetAnalysis(analyses_info, headers, feature_list)

  if (is.null(validate_object(nts))) {
    engine$Analyses$results[["MassSpecResults_NonTargetAnalysis"]] <- nts
    TRUE
  } else {
    FALSE
  }
}

#' MassSpecMethod_FindFeatures_xcms3_centwave class
#'
#' @description Method for finding features (i.e., chromatographic peaks) in mass spectrometry files
#' using the package \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
#' (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/findChromPeaks-centWave.html}{centWave}. The function uses
#' the package \pkg{patRoon} in the background.
#'
#' @param ppm numeric(1) defining the maximal tolerated m/z deviation in consecutive scans in parts
#' per million (ppm) for the initial ROI definition.
#' @param peakwidth numeric(2) with the expected approximate feature width in chromatographic space.
#' Given as a range (min, max) in seconds.
#' @param snthresh numeric(1) defining the signal to noise ratio cutoff.
#' @param prefilter numeric(2): c(k, I) specifying the prefilter step for the first analysis step
#' (ROI detection). Mass traces are only retained if they contain at least k peaks with
#' intensity >= I.
#' @param mzCenterFun Name of the function to calculate the m/z center of the
#' chromatographic peak (feature). Allowed are: "wMean": intensity weighted mean
#' of the peak's m/z values, "mean": mean of the peak's m/z values, "apex": use the
#' m/z value at the peak apex, "wMeanApex3": intensity weighted mean of the m/z
#' value at the peak apex and the m/z values left and right of it and
#' "meanApex3": mean of the m/z value of the peak apex and the m/z values
#' left and right of it.
#' @param integrate Integration method. For integrate = 1 peak limits are found
#' through descent on the mexican hat filtered data, for integrate = 2 the
#' descent is done on the real data. The latter method is more accurate but
#' prone to noise, while the former is more robust, but less exact.
#' @param mzdiff numeric(1) representing the minimum difference in m/z dimension
#' required for peaks with overlapping retention times; can be negative to
#' allow overlap. During peak post-processing, peaks defined to be overlapping
#' are reduced to the one peak with the largest signal.
#' @param fitgauss logical(1) whether or not a Gaussian should be fitted to each
#' peak. This affects mostly the retention time position of the peak.
#' @param noise numeric(1) allowing to set a minimum intensity required for
#' centroids to be considered in the first analysis step (centroids with
#' intensity < noise are omitted from ROI detection).
#' @param verboseColumns logical(1) whether additional peak meta data columns
#' should be returned.
#' @param firstBaselineCheck logical(1). If TRUE continuous data within regions
#' of interest is checked to be above the first baseline.
#' @param extendLengthMSW Option to force centWave to use all scales when
#' running centWave rather than truncating with the EIC length. Uses the
#' "open" method to extend the EIC to a integer base-2 length prior to being
#' passed to convolve rather than the default "reflect" method.
#' See https://github.com/sneumann/xcms/issues/445 for more information.
#'
#' @details See the \link[patRoon]{findFeaturesXCMS3} function from the \pkg{patRoon} package for
#' more information and requirements.
#'
#' @return A `MassSpecMethod_FindFeatures_xcms3_centwave` object.
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
MassSpecMethod_FindFeatures_xcms3_centwave <- function(
  ppm = 12,
  peakwidth = c(5, 60),
  snthresh = 15,
  prefilter = c(5, 1500),
  mzCenterFun = "wMean",
  integrate = 1,
  mzdiff = -0.0002,
  fitgauss = TRUE,
  noise = 500,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = FALSE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FindFeatures",
    required = NA_character_,
    algorithm = "xcms3_centwave",
    input_class = NA_character_,
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      ppm = as.numeric(ppm),
      peakwidth = as.numeric(peakwidth),
      snthresh = as.numeric(snthresh),
      prefilter = as.numeric(prefilter),
      mzCenterFun = as.character(mzCenterFun),
      integrate = as.numeric(integrate),
      mzdiff = as.numeric(mzdiff),
      fitgauss = as.logical(fitgauss),
      noise = as.numeric(noise),
      verboseColumns = as.logical(verboseColumns),
      firstBaselineCheck = as.logical(firstBaselineCheck),
      extendLengthMSW = as.logical(extendLengthMSW)
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "xcms",
    developer = "Ralf Tautenhahn, Johannes Rainer",
    contact = "rtautenh@ipb-halle.de",
    link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
    doi = "https://doi.org/10.1186/1471-2105-9-504"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid parameters for MassSpecMethod_FindFeatures_xcms3_centwave.")
  }
}

#' @describeIn MassSpecMethod_FindFeatures_xcms3_centwave Validate the object, returning `NULL` if valid.
#' @param x A `MassSpecMethod_FindFeatures_xcms3_centwave` object.
#' @export
#'
validate_object.MassSpecMethod_FindFeatures_xcms3_centwave <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FindFeatures")
  checkmate::assert_choice(x$algorithm, "xcms3_centwave")
  checkmate::assert_numeric(x$parameters$ppm, len = 1)
  checkmate::assert_numeric(x$parameters$peakwidth, len = 2)
  checkmate::assert_numeric(x$parameters$snthresh, len = 1)
  checkmate::assert_numeric(x$parameters$prefilter, len = 2)
  checkmate::assert_choice(
    x$parameters$mzCenterFun,
    c("wMean", "mean", "apex", "wMeanApex3", "meanApex3")
  )
  checkmate::assert_numeric(x$parameters$integrate, len = 1)
  checkmate::assert_numeric(x$parameters$mzdiff, len = 1)
  checkmate::assert_logical(x$parameters$fitgauss, len = 1)
  checkmate::assert_numeric(x$parameters$noise, len = 1)
  checkmate::assert_logical(x$parameters$verboseColumns, len = 1)
  checkmate::assert_logical(x$parameters$firstBaselineCheck, len = 1)
  checkmate::assert_logical(x$parameters$extendLengthMSW, len = 1)
  NULL
}


#' @export
#' @noRd
run.MassSpecMethod_FindFeatures_xcms3_centwave <- function(x, engine = NULL) {
  .run_find_features_patRoon(x, engine)
}

#' MassSpecMethod_FindFeatures_xcms3_matchedfilter class
#'
#' @description Settings for finding features (i.e., chromatographic peaks) in mzML/mzXML files
#' using the package \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
#' (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/findChromPeaks-Chromatogram-MatchedFilter.html}{MatchedFilter},
#' which is optimal/preferred for low resolution LC-MS data. The function uses the package
#' \pkg{patRoon} in the background.
#'
#' @param binSize numeric(1) specifying the width of the bins/slices in m/z dimension.
#' @param impute Character string specifying the method to be used for missing
#' value imputation. Allowed values are "none" (no linear interpolation), "lin"
#' (linear interpolation), "linbase" (linear interpolation within a certain
#' bin-neighborhood) and "intlin".
#' @param baseValue The base value to which empty elements should be set.
#' This is only considered for `impute` as "linbase" and corresponds to the
#' profBinLinBase's `baselevel` argument.
#' @param distance For `impute` as "linbase": number of non-empty neighboring
#' element of an empty element that should be considered for linear
#' interpolation. See details section for more information.
#' @param fwhm numeric(1) specifying the full width at half maximum of matched
#' filtration gaussian model peak. Only used to calculate the actual sigma,
#' see below.
#' @param max numeric(1) representing the maximum number of peaks that are
#' expected/will be identified per slice.
#' @param snthresh numeric(1) defining the signal to noise ratio cutoff.
#' @param steps numeric(1) defining the number of bins to be merged before
#' filtration (i.e. the number of neighboring bins that will be joined to the
#' slice in which filtration and peak detection will be performed).
#' @param mzdiff numeric(1) representing the minimum difference in m/z dimension
#' required for peaks with overlapping retention times; can be negative to allow
#' overlap. During peak post-processing, peaks defined to be overlapping are
#' reduced to the one peak with the largest signal.
#' @param index logical(1) specifying whether indices should be returned instead of values for m/z
#' and retention times.
#'
#' @details See the \link[patRoon]{findFeaturesXCMS3} function from the \pkg{patRoon} package for
#' more information and requirements.
#'
#' @return A `MassSpecMethod_FindFeatures_xcms3_matchedfilter` object.
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
MassSpecMethod_FindFeatures_xcms3_matchedfilter <- function(
  binSize = 0.5,
  impute = "none",
  baseValue = 0,
  distance = 0,
  fwhm = 30,
  max = 5,
  snthresh = 20,
  steps = 2,
  mzdiff = 0.5,
  index = FALSE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FindFeatures",
    required = NA_character_,
    algorithm = "xcms3_matchedfilter",
    input_class = NA_character_,
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      class = as.character("MatchedFilterParam"),
      binSize = as.numeric(binSize),
      impute = as.character(impute),
      baseValue = as.numeric(baseValue),
      distance = as.numeric(distance),
      fwhm = as.numeric(fwhm),
      sigma = as.numeric(fwhm / 2.3548),
      max = as.numeric(max),
      snthresh = as.numeric(snthresh),
      steps = as.numeric(steps),
      mzdiff = as.numeric(mzdiff),
      index = as.logical(index)
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "xcms",
    developer = "Ralf Tautenhahn, Johannes Rainer",
    contact = "rtautenh@ipb-halle.de",
    link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
    doi = "https://doi.org/10.1186/1471-2105-9-504"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop(
      "Invalid parameters for MassSpecMethod_FindFeatures_xcms3_matchedfilter."
    )
  }
}

#' @describeIn MassSpecMethod_FindFeatures_xcms3_matchedfilter Validate the object, returning `NULL` if valid.
#' @param x A `MassSpecMethod_FindFeatures_xcms3_matchedfilter` object.
#' @export
#'
validate_object.MassSpecMethod_FindFeatures_xcms3_matchedfilter = function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FindFeatures")
  checkmate::assert_choice(x$algorithm, "xcms3_matchedfilter")
  checkmate::assert_numeric(x$parameters$binSize, len = 1)
  checkmate::assert_choice(
    x$parameters$impute,
    c("none", "lin", "linbase", "intlin")
  )
  checkmate::assert_numeric(x$parameters$baseValue, len = 1)
  checkmate::assert_numeric(x$parameters$distance, len = 1)
  checkmate::assert_numeric(x$parameters$fwhm, len = 1)
  checkmate::assert_numeric(x$parameters$max, len = 1)
  checkmate::assert_numeric(x$parameters$snthresh, len = 1)
  checkmate::assert_numeric(x$parameters$steps, len = 1)
  checkmate::assert_numeric(x$parameters$mzdiff, len = 1)
  checkmate::assert_logical(x$parameters$index, len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_FindFeatures_xcms3_matchedfilter <- function(
  x,
  engine = NULL
) {
  .run_find_features_patRoon(x, engine)
}

#' MassSpecMethod_FindFeatures_openms class
#'
#' @description Settings for finding features (i.e., chromatographic peaks) in mzML/mzXML files
#' using the \href{https://www.openms.org/}{OpenMS}
#' (\url{https://abibuilder.cs.uni-tuebingen.de/archive/openms/}) software with the algorithm
#' \href{https://abibuilder.cs.uni-tuebingen.de/archive/openms/Documentation/release/latest/html/TOPP_FeatureFinderMetabo.html}{FeatureFinderMetabo}.
#' The function uses the package \pkg{patRoon} in the background.
#'
#' @param noiseThrInt Intensity threshold below which peaks are regarded as noise.
#' @param chromSNR Minimum signal-to-noise a mass trace should have.
#' @param chromFWHM Expected chromatographic peak width (in seconds).
#' @param mzPPM Allowed mass deviation (in ppm).
#' @param reEstimateMTSD Enables dynamic re-estimation of m/z variance during
#' mass trace collection stage.
#' @param traceTermCriterion Termination criterion for the extension of mass
#' traces. In 'outlier' mode, trace extension cancels if a predefined number of
#' consecutive outliers are found (see trace_termination_outliers parameter).
#' In 'sample_rate' mode, trace extension in both directions stops if ratio of
#' found peaks versus visited spectra falls below the 'min_sample_rate' threshold.
#' @param traceTermOutliers Mass trace extension in one direction cancels if
#' this number of consecutive spectra with no detectable peaks is reached.
#' @param minSampleRate Minimum fraction of scans along the mass trace that must
#' contain a peak.
#' @param minTraceLength Minimum expected length of a mass trace (in seconds).
#' @param maxTraceLength Maximum expected length of a mass trace (in seconds).
#' Set to a negative value to disable maximal length check during mass trace
#' detection.
#' @param widthFiltering Enable filtering of unlikely peak widths. The fixed
#' setting filters out mass traces outside the `min_fwhm`, `max_fwhm` interval
#' (set parameters accordingly!). The auto setting filters with the 5 and 95%
#' quantiles of the peak width distribution.
#' @param minFWHM Minimum full-width-at-half-maximum of chromatographic peaks
#' (in seconds). Ignored if parameter width_filtering is off or auto.
#' @param maxFWHM Maximum full-width-at-half-maximum of chromatographic peaks
#' (in seconds). Ignored if parameter width_filtering is off or auto.
#' @param traceSNRFiltering Apply post-filtering by signal-to-noise ratio after
#' smoothing.
#' @param localRTRange RT range where to look for coeluting mass traces.
#' @param localMZRange MZ range where to look for isotopic mass traces.
#' @param isotopeFilteringModel Remove/score candidate assemblies based on
#' isotope intensities. SVM isotope models for metabolites were trained with
#' either 2% or 5% RMS error. For peptides, an averagine cosine scoring is used.
#' Select the appropriate noise model according to the quality of measurement
#' or MS device.
#' @param MZScoring13C Use the 13C isotope peak position (~1.003355 Da) as the
#' expected shift in m/z for isotope mass traces (highly recommended for
#' lipidomics!). Disable for general metabolites
#' (as described in Kenar et al. 2014, MCP.).
#' @param useSmoothedInts Use LOWESS intensities instead of raw intensities.
#' @param intSearchRTWindow Retention time window (in seconds, +/- feature
#' retention time) that is used to find the closest data point to the retention
#' time to obtain the intensity of a feature (this is needed since OpenMS does
#' not provide this data).
#' @param useFFMIntensities If TRUE then peak intensities are directly loaded
#' from FeatureFinderMetabo output. Otherwise, intensities are loaded afterwards
#' from the input ‘mzML’ files, which is potentially much slower, especially
#' with many analyses files. However, useFFMIntensities=TRUE is still somewhat
#' experimental, may be less accurate and requires a recent version of OpenMS
#' (>=2.7).
#' @param verbose Logical of length one. When TRUE adds processing information to the console.
#'
#' @details See the \link[patRoon]{findFeaturesOpenMS} function from the \pkg{patRoon} package for
#' more information and requirements.
#'
#' @return A `MassSpecMethod_FindFeatures_openms` object.
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
MassSpecMethod_FindFeatures_openms <- function(
  noiseThrInt = 1000,
  chromSNR = 3,
  chromFWHM = 7,
  mzPPM = 15,
  reEstimateMTSD = TRUE,
  traceTermCriterion = "sample_rate",
  traceTermOutliers = 5,
  minSampleRate = 1,
  minTraceLength = 4,
  maxTraceLength = 70,
  widthFiltering = "fixed",
  minFWHM = 4,
  maxFWHM = 35,
  traceSNRFiltering = TRUE,
  localRTRange = 0,
  localMZRange = 0,
  isotopeFilteringModel = "none",
  MZScoring13C = FALSE,
  useSmoothedInts = FALSE,
  intSearchRTWindow = 3,
  useFFMIntensities = FALSE,
  verbose = FALSE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FindFeatures",
    required = NA_character_,
    algorithm = "openms",
    input_class = NA_character_,
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      noiseThrInt = noiseThrInt,
      chromSNR = chromSNR,
      chromFWHM = chromFWHM,
      mzPPM = mzPPM,
      reEstimateMTSD = reEstimateMTSD,
      traceTermCriterion = traceTermCriterion,
      traceTermOutliers = traceTermOutliers,
      minSampleRate = minSampleRate,
      minTraceLength = minTraceLength,
      maxTraceLength = maxTraceLength,
      widthFiltering = widthFiltering,
      minFWHM = minFWHM,
      maxFWHM = maxFWHM,
      traceSNRFiltering = traceSNRFiltering,
      localRTRange = localRTRange,
      localMZRange = localMZRange,
      isotopeFilteringModel = isotopeFilteringModel,
      MZScoring13C = MZScoring13C,
      useSmoothedInts = useSmoothedInts,
      intSearchRTWindow = intSearchRTWindow,
      useFFMIntensities = useFFMIntensities,
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
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid parameters for MassSpecMethod_FindFeatures_openms.")
  }
}

#' @describeIn MassSpecMethod_FindFeatures_openms Validate the `MassSpecMethod_FindFeatures_openms` object, returning `NULL` if valid.
#' @param x A `MassSpecMethod_FindFeatures_openms` object.
#' @export
#'
validate_object.MassSpecMethod_FindFeatures_openms <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FindFeatures")
  checkmate::assert_choice(x$algorithm, "openms")
  checkmate::assert_numeric(x$parameters$noiseThrInt, len = 1)
  checkmate::assert_numeric(x$parameters$chromSNR, len = 1)
  checkmate::assert_numeric(x$parameters$chromFWHM, len = 1)
  checkmate::assert_numeric(x$parameters$mzPPM, len = 1)
  checkmate::assert_logical(x$parameters$reEstimateMTSD, len = 1)
  checkmate::assert_choice(
    x$parameters$traceTermCriterion,
    c("outlier", "sample_rate")
  )
  checkmate::assert_numeric(x$parameters$traceTermOutliers, len = 1)
  checkmate::assert_numeric(x$parameters$minSampleRate, len = 1)
  checkmate::assert_numeric(x$parameters$minTraceLength, len = 1)
  checkmate::assert_numeric(x$parameters$maxTraceLength, len = 1)
  checkmate::assert_choice(x$parameters$widthFiltering, c("fixed", "auto"))
  checkmate::assert_numeric(x$parameters$minFWHM, len = 1)
  checkmate::assert_numeric(x$parameters$maxFWHM, len = 1)
  checkmate::assert_logical(x$parameters$traceSNRFiltering, len = 1)
  checkmate::assert_numeric(x$parameters$localRTRange, len = 1)
  checkmate::assert_numeric(x$parameters$localMZRange, len = 1)
  checkmate::assert_choice(
    x$parameters$isotopeFilteringModel,
    c("none", "2%", "5%", "averagine")
  )
  checkmate::assert_logical(x$parameters$MZScoring13C, len = 1)
  checkmate::assert_logical(x$parameters$useSmoothedInts, len = 1)
  checkmate::assert_numeric(x$parameters$intSearchRTWindow, len = 1)
  checkmate::assert_logical(x$parameters$useFFMIntensities, len = 1)
  checkmate::assert_logical(x$parameters$verbose, len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_FindFeatures_openms <- function(x, engine = NULL) {
  .run_find_features_patRoon(x, engine)
}

#' MassSpecMethod_FindFeatures_kpic2 class
#'
#' @description Settings for finding features (i.e., chromatographic peaks) in mzML/mzXML files
#' using the package \href{https://github.com/hcji/KPIC2}{KPIC}. The function uses the package
#' \pkg{patRoon} in thebackground.
#'
#' @param level Mass traces are only retained if their maximum values are over `level`.
#' @param mztol The initial m/z tolerance.
#' @param gap The number of gap points of a mass trace.
#' @param width The minimum length of a mass trace.
#' @param min_snr Minimum signal to noise ratio.
#' @param kmeans If `TRUE`, \link[KPIC]{getPIC.kmeans} is used to obtain
#' PICs (i.e., features). If `FALSE`, \link[KPIC]{getPIC} is used.
#' @param alpha If `kmeans` is `TRUE`, alpha is the parameter of forecasting.
#' If `kmeans` is `FALSE`, alpha is not used.
#'
#' @details See the \link[patRoon]{findFeaturesKPIC2} function from the \pkg{patRoon} package for
#' more information and requirements.
#'
#' @return A `MassSpecMethod_FindFeatures_kpic2` object.
#'
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' \insertRef{kpic01}{StreamFind}
#'
#' @export
#'
MassSpecMethod_FindFeatures_kpic2 <- function(
  level = 500,
  mztol = 0.01,
  gap = 2,
  width = 5,
  min_snr = 4,
  kmeans = TRUE,
  alpha = 0.3
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FindFeatures",
    required = NA_character_,
    algorithm = "kpic2",
    input_class = NA_character_,
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      kmeans = kmeans,
      level = level,
      mztol = mztol,
      gap = gap,
      width = width,
      min_snr = min_snr,
      alpha = alpha
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "kpic2",
    developer = "Hongchao Ji",
    contact = "ji.hongchao@foxmail.com",
    link = NA_character_,
    doi = "10.1021/acs.analchem.7b01547"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid parameters for MassSpecMethod_FindFeatures_kpic2.")
  }
}

#' @describeIn MassSpecMethod_FindFeatures_kpic2 Validate the object, returning `NULL` if valid.
#' @param x A `MassSpecMethod_FindFeatures_kpic2` object.
#' @export
#'
validate_object.MassSpecMethod_FindFeatures_kpic2 <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FindFeatures")
  checkmate::assert_choice(x$algorithm, "kpic2")
  checkmate::assert_numeric(x$parameters$level, len = 1)
  checkmate::assert_numeric(x$parameters$mztol, len = 1)
  checkmate::assert_numeric(x$parameters$gap, len = 1)
  checkmate::assert_numeric(x$parameters$width, len = 1)
  checkmate::assert_numeric(x$parameters$min_snr, len = 1)
  checkmate::assert_logical(x$parameters$kmeans, len = 1)
  if (x$parameters$kmeans) {
    checkmate::assert_numeric(x$parameters$alpha, len = 1)
  }
  NULL
}

#' @export
#' @noRd
#' 
run.MassSpecMethod_FindFeatures_kpic2 <- function(x, engine = NULL) {
  .run_find_features_patRoon(x, engine)
}

#' MassSpecMethod_FindFeatures_qalgorithms class
#'
#' @description The qAlgorithms uses a comprehensive peak model developed by
#' \href{https://doi.org/10.1021/acs.analchem.4c00494}{Renner et al.} to
#' identify peaks within LC-MS data. More information can be found in the
#' GitHub repository of the \href{https://github.com/odea-project/qAlgorithms}{qAlgorithms} project.
#' The qAlgorithms is best used with profile data but centroid data is also possible. Yet, a mass
#' uncertainty should by supplied in parts per million (ppm) to account for the m/z deviation in
#' centroid data.
#'
#' @param ppm numeric(1) defining the maximal tolerated m/z deviation in parts per million (ppm)
#' only applicable for centroid data. For profile data, the ppm value is ignored.
#'
#' @export
#'
MassSpecMethod_FindFeatures_qalgorithms <- function(ppm = 5) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FindFeatures",
    required = NA_character_,
    algorithm = "qalgorithms",
    input_class = NA_character_,
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(ppm = ppm),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "qAlgorithms",
    developer = "Gerrit Renner",
    contact = "gerrit.renner@uni-due.de",
    link = "https://github.com/odea-project/qAlgorithms",
    doi = "10.1021/acs.analchem.4c00494"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid parameters for MassSpecMethod_FindFeatures_qalgorithms.")
  }
}

#' @describeIn MassSpecMethod_FindFeatures_qalgorithms Validate the object, returning `NULL` if valid.
#' @param x A `MassSpecMethod_FindFeatures_qalgorithms` object.
#' @export
#' 
validate_object.MassSpecMethod_FindFeatures_qalgorithms <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FindFeatures")
  checkmate::assert_choice(x$algorithm, "qalgorithms")
  checkmate::assert_numeric(x$parameters$ppm, len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_FindFeatures_qalgorithms <- function(x, engine = NULL) {
  .run_find_features_patRoon(x, engine)
  FALSE
}
