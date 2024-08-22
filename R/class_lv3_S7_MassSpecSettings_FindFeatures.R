
# ______________________________________________________________________________________________________________________
# utility functions -----
# ______________________________________________________________________________________________________________________

#' @noRd
.run_find_features_patRoon <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (length(engine$analyses) == 0) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  anaInfo <- data.frame(
    "path" = dirname(engine$analyses$files),
    "analysis" = engine$analyses$names,
    "group" = engine$analyses$replicates,
    "blank" = engine$analyses$blanks
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
  
  if (grepl("_", algorithm, fixed = FALSE)) algorithm <- gsub("^(.*?)_.*$", "\\1", algorithm)
  
  if ("xcms" %in% algorithm || "xcms3" %in% algorithm) {
    if (!requireNamespace("xcms")) {
      warning("xcms package is not installed!")
      return(FALSE)
    }
  }
  
  parameters <- x$parameters
  
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
  
  for (a in patRoon::analyses(pat)) {
    pol <- engine$get_spectra_polarity(a)
    if (grepl("postive", pol) && grepl("negative", pol)) {
      warning("Multiple polarities in analyses", i, "! Mass for features could not be estimated.")
      pat@features[[a]]$mass <- NA_real_
      next
    }
    if (grepl("unkown", pol)) {
      warning("Unknown polarity in analyses", i, "! Mass for features could not be estimated.")
      pat@features[[a]]$mass <- NA_real_
      next
    }
    if ("positive" %in% pol) adduct_val <- -1.007276
    if ("negative" %in% pol) adduct_val <- 1.007276
    pat@features[[a]]$mass <- pat@features[[a]]$mz + adduct_val
    pat@features[[a]]$filtered <- FALSE
  }
  
  pols <- engine$get_spectra_polarity()
  
  if (length(unique(pols)) > 1 && !("featuresSet" %in% is(pat))) {
    pat <- patRoon::makeSet(
      pat[pols %in% "positive"],
      pat[pols %in% "negative"],
      adducts = list("[M+H]+", "[M-H]-")
    )
  }
  
  filtered <- lapply(patRoon::analyses(pat), function(a) pat@features[[a]][0, ])
  names(filtered) <- patRoon::analyses(pat)
  
  nts <- NTS(features = pat, filtered = filtered)
  
  if (is(nts, "StreamFind::NTS")) {
    engine$NTS <- nts
    TRUE
    
  } else {
    FALSE
  }
}

# ______________________________________________________________________________________________________________________
# xcms_centwave -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FindFeatures_xcms3_centwave**
#'
#' @description Settings for finding features (i.e., chromatographic peaks) in mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms} (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/findChromPeaks-centWave.html}{centWave}. The function uses the package 
#' \pkg{patRoon} in the background.
#'
#' @param ppm numeric(1) defining the maximal tolerated m/z deviation in consecutive scans in parts per million (ppm) 
#' for the initial ROI definition.
#' @param peakwidth numeric(2) with the expected approximate feature width in chromatographic space. Given as a range 
#' (min, max) in seconds.
#' @param snthresh numeric(1) defining the signal to noise ratio cutoff.
#' @param prefilter numeric(2): c(k, I) specifying the prefilter step for the first analysis step (ROI detection). 
#' Mass traces are only retained if they contain at least k peaks with intensity >= I.
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
#' @details See the \link[patRoon]{findFeaturesXCMS3} function from the \pkg{patRoon} package for more information and requirements.
#'
#' @return A `MassSpecSettings_FindFeatures_xcms3_centwave` object.
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
MassSpecSettings_FindFeatures_xcms3_centwave <- S7::new_class("MassSpecSettings_FindFeatures_xcms3_centwave",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(ppm = 12,
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
                         extendLengthMSW = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FindFeatures",
      algorithm = "xcms3_centwave",
      parameters = list(
        ppm = ppm,
        peakwidth = peakwidth,
        snthresh = snthresh,
        prefilter = prefilter,
        mzCenterFun = mzCenterFun,
        integrate = integrate,
        mzdiff = mzdiff,
        fitgauss = fitgauss,
        noise = noise,
        verboseColumns = verboseColumns,
        firstBaselineCheck = firstBaselineCheck,
        extendLengthMSW = extendLengthMSW
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "xcms",
      developer = "Ralf Tautenhahn, Johannes Rainer",
      contact = "rtautenh@ipb-halle.de",
      link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
      doi = "https://doi.org/10.1186/1471-2105-9-504"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "FindFeatures"),
      checkmate::test_choice(self@algorithm, "xcms3_centwave"),
      checkmate::test_numeric(self@parameters$ppm, len = 1),
      checkmate::test_numeric(self@parameters$peakwidth, len = 2),
      checkmate::test_numeric(self@parameters$snthresh, len = 1),
      checkmate::test_numeric(self@parameters$prefilter, len = 2),
      checkmate::test_choice(self@parameters$mzCenterFun, c("wMean", "mean", "apex", "wMeanApex3", "meanApex3")),
      checkmate::test_integer(self@parameters$integrate, len = 1),
      checkmate::test_numeric(self@parameters$mzdiff, len = 1),
      checkmate::test_logical(self@parameters$fitgauss, len = 1),
      checkmate::test_numeric(self@parameters$noise, len = 1),
      checkmate::test_logical(self@parameters$verboseColumns, len = 1),
      checkmate::test_logical(self@parameters$firstBaselineCheck, len = 1),
      checkmate::test_logical(self@parameters$extendLengthMSW, len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FindFeatures_xcms3_centwave) <- function(x, engine = NULL) {
  .run_find_features_patRoon(x, engine)
}

# ______________________________________________________________________________________________________________________
# xcms3_matchedfilter -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FindFeatures_xcms3_matchedfilter**
#'
#' @description Settings for finding features (i.e., chromatographic peaks) in mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms} (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/findChromPeaks-Chromatogram-MatchedFilter.html}{MatchedFilter},
#' which is optimal/preferred for low resolution LC-MS data. The function uses the package \pkg{patRoon} in the background.
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
#' @param index logical(1) specifying whether indices should be returned instead of values for m/z and retention times.
#'
#' @details See the \link[patRoon]{findFeaturesXCMS3} function from the \pkg{patRoon} package for more information and requirements.
#'
#' @return A `MassSpecSettings_FindFeatures_xcms3_matchedfilter` object.
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
MassSpecSettings_FindFeatures_xcms3_matchedfilter <- S7::new_class("MassSpecSettings_FindFeatures_xcms3_matchedfilter",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(binSize = 0.5,
                         impute = "none",
                         baseValue = 0,
                         distance = 0,
                         fwhm = 30,
                         max = 5,
                         snthresh = 20,
                         steps = 2,
                         mzdiff = 0.5,
                         index = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FindFeatures",
      algorithm = "xcms3_matchedfilter",
      parameters = list(
        class = "MatchedFilterParam",
        binSize = binSize,
        impute = impute,
        baseValue = baseValue,
        distance = distance,
        fwhm = fwhm,
        sigma = fwhm / 2.3548,
        max = max,
        snthresh = snthresh,
        steps = steps,
        mzdiff = mzdiff,
        index = index
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "xcms",
      developer = "Ralf Tautenhahn, Johannes Rainer",
      contact = "rtautenh@ipb-halle.de",
      link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
      doi = "https://doi.org/10.1186/1471-2105-9-504"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "FindFeatures"),
      checkmate::test_choice(self@algorithm, "xcms3_matchedfilter"),
      checkmate::test_numeric(self@parameters$binSize, len = 1),
      checkmate::test_choice(self@parameters$impute, c("none", "lin", "linbase", "intlin")),
      checkmate::test_numeric(self@parameters$baseValue, len = 1),
      checkmate::test_numeric(self@parameters$distance, len = 1),
      checkmate::test_numeric(self@parameters$fwhm, len = 1),
      checkmate::test_numeric(self@parameters$max, len = 1),
      checkmate::test_numeric(self@parameters$snthresh, len = 1),
      checkmate::test_numeric(self@parameters$steps, len = 1),
      checkmate::test_numeric(self@parameters$mzdiff, len = 1),
      checkmate::test_logical(self@parameters$index, len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FindFeatures_xcms3_matchedfilter) <- function(x, engine = NULL) {
  .run_find_features_patRoon(x, engine)
}

# ______________________________________________________________________________________________________________________
# openms -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FindFeatures_openms**
#'
#' @description Settings for finding features (i.e., chromatographic peaks) in mzML/mzXML files using the \href{https://www.openms.org/}{OpenMS}
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
#' @param extraOpts = NULL,
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
#' @details See the \link[patRoon]{findFeaturesOpenMS} function from the \pkg{patRoon} package for more information and requirements.
#'
#' @return A `MassSpecSettings_FindFeatures_openms` object.
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
MassSpecSettings_FindFeatures_openms <- S7::new_class("MassSpecSettings_FindFeatures_openms",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(noiseThrInt = 1000,
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
                         extraOpts = NULL,
                         intSearchRTWindow = 3,
                         useFFMIntensities = FALSE,
                         verbose = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FindFeatures",
      algorithm = "openms",
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
        extraOpts = extraOpts,
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
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "FindFeatures"),
      checkmate::test_choice(self@algorithm, "openms"),
      checkmate::test_numeric(self@parameters$noiseThrInt, len = 1),
      checkmate::test_numeric(self@parameters$chromSNR, len = 1),
      checkmate::test_numeric(self@parameters$chromFWHM, len = 1),
      checkmate::test_numeric(self@parameters$mzPPM, len = 1),
      checkmate::test_logical(self@parameters$reEstimateMTSD, len = 1),
      checkmate::test_choice(self@parameters$traceTermCriterion, c("outlier", "sample_rate")),
      checkmate::test_numeric(self@parameters$traceTermOutliers, len = 1),
      checkmate::test_numeric(self@parameters$minSampleRate, len = 1),
      checkmate::test_numeric(self@parameters$minTraceLength, len = 1),
      checkmate::test_numeric(self@parameters$maxTraceLength, len = 1),
      checkmate::test_choice(self@parameters$widthFiltering, c("fixed", "auto")),
      checkmate::test_numeric(self@parameters$minFWHM, len = 1),
      checkmate::test_numeric(self@parameters$maxFWHM, len = 1),
      checkmate::test_logical(self@parameters$traceSNRFiltering, len = 1),
      checkmate::test_numeric(self@parameters$localRTRange, len = 1),
      checkmate::test_numeric(self@parameters$localMZRange, len = 1),
      checkmate::test_choice(self@parameters$isotopeFilteringModel, c("none", "2%", "5%", "averagine")),
      checkmate::test_logical(self@parameters$MZScoring13C, len = 1),
      checkmate::test_logical(self@parameters$useSmoothedInts, len = 1),
      checkmate::test_list(self@parameters$extraOpts, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$intSearchRTWindow, len = 1),
      checkmate::test_logical(self@parameters$useFFMIntensities, len = 1),
      checkmate::test_logical(self@parameters$verbose, len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FindFeatures_openms) <- function(x, engine = NULL) {
  .run_find_features_patRoon(x, engine)
}

# ______________________________________________________________________________________________________________________
# kpic2 -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FindFeatures_kpic2**
#'
#' @description Settings for finding features (i.e., chromatographic peaks) in mzML/mzXML files using the package 
#' \href{https://github.com/hcji/KPIC2}{KPIC}. The function uses the package \pkg{patRoon} in the background.
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
#' @details See the \link[patRoon]{findFeaturesKPIC2} function from the \pkg{patRoon} package for more information and requirements.
#'
#' @return A `MassSpecSettings_FindFeatures_kpic2` object.
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
MassSpecSettings_FindFeatures_kpic2 <- S7::new_class("MassSpecSettings_FindFeatures_kpic2",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(level = 500,
                         mztol = 0.01,
                         gap = 2,
                         width = 5,
                         min_snr = 4,
                         kmeans = TRUE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FindFeatures",
      algorithm = "kpic2",
      parameters = list(
        kmeans = kmeans,
        level = level,
        mztol = mztol,
        gap = gap,
        width = width,
        min_snr = min_snr
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "kpic2",
      developer = "Hongchao Ji",
      contact = "ji.hongchao@foxmail.com",
      link = NA_character_,
      doi = "10.1021/acs.analchem.7b01547"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "FindFeatures"),
      checkmate::test_choice(self@algorithm, "kpic2"),
      checkmate::test_numeric(self@parameters$level, len = 1),
      checkmate::test_numeric(self@parameters$mztol, len = 1),
      checkmate::test_numeric(self@parameters$gap, len = 1),
      checkmate::test_numeric(self@parameters$width, len = 1),
      checkmate::test_numeric(self@parameters$min_snr, len = 1),
      checkmate::test_logical(self@parameters$kmeans, len = 1),
      if (self@parameters$kmeans) checkmate::test_numeric(self@parameters$alpha, len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FindFeatures_kpic2) <- function(x, engine = NULL) {
  .run_find_features_patRoon(x, engine)
}

# ______________________________________________________________________________________________________________________
# qPeaks -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FindFeatures_qPeaks**
#'
#' @description Not yet implemented.
#'
#' @noRd
#'
MassSpecSettings_FindFeatures_qPeaks <- S7::new_class("MassSpecSettings_FindFeatures_qPeaks",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
   
   S7::new_object(ProcessingSettings(
     engine = "MassSpec",
     method = "FindFeatures",
     algorithm = "qPeaks",
     parameters = list(),
     number_permitted = 1,
     version = as.character(packageVersion("StreamFind")),
     software = "qAlgorithms",
     developer = "Max, Gerrit",
     contact = "max@email.de",
     link = "https://github.com/odea-project/qAlgorithms",
     doi = NA_character_
   ))
  },
  
  validator = function(self) {
   valid <- all(
     checkmate::test_choice(self@engine, "MassSpec"),
     checkmate::test_choice(self@call, "FindFeatures"),
     checkmate::test_choice(self@algorithm, "qPeaks")
   )
   if (!valid) return(FALSE)
   NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FindFeatures_qPeaks) <- function(x, engine = NULL) {
  # .run_find_features_patRoon(x, engine)
  FALSE
}
