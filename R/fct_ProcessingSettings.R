
# ______________________________________________________________________________________________________________________
# centroid_spectra -----
# ______________________________________________________________________________________________________________________

#' @title Settings_centroid_spectra_qCentroids
#'
#' @description Centroids profile spectra using the \href{https://link.springer.com/article/10.1007/s00216-022-04224-y}{qCentroids}
#' algorithm, which is part of the \href{https://github.com/odea-project/qAlgorithms}{qAlgorithms} library.
#'
#' @param maxScale Integer of length one. Maximum scale as integer (default is 5) for defining the scale limit for the peak model.
#' @param mode Integer of length one. `0` for debugging, `1` for silent mode (the default) and `2` for progress bar mode.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_centroid_spectra_qCentroids.
#'
#' @references
#' \insertRef{qcentroids01}{StreamFind}
#'
#' @export
#'
Settings_centroid_spectra_qCentroids <- function(maxScale = 5, mode = 1, runParallel = FALSE) {
  
  settings <- list(
    call = "centroid_spectra",
    algorithm = "qCentroids",
    parameters = list(
      maxScale = maxScale,
      mode = mode
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "qAlgorithms",
    developer = "Gerrit Renner",
    contact = "gerrit.renner@uni-due.de",
    link = "https://github.com/odea-project/qAlgorithms",
    doi = "https://doi.org/10.1007/s00216-022-04224-y"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_centroid_spectra_qCentroids <- function(x) {
  all(
    checkmate::test_choice(x$call, "centroid_spectra"),
    checkmate::test_choice(x$algorithm, "qCentroids"),
    checkmate::test_int(x$parameters$maxScale),
    checkmate::test_int(x$parameters$mode)
  )
}





# ______________________________________________________________________________________________________________________
# bin_spectra -----
# ______________________________________________________________________________________________________________________

#' @title Settings_bin_spectra_qBinning
#'
#' @description Not yet implemented.
#'
#' @export
#'
Settings_bin_spectra_qBinning <- function() {
  
  # TODO Max - update settings info
  
  settings <- list(
    call = "bin_spectra",
    algorithm = "qBinning",
    parameters = list(),
    version = as.character(packageVersion("StreamFind")),
    software = "q",
    developer = "Max, Gerrit",
    contact = "max@email.de",
    link = "https://github.com/odea-project/qAlgorithms",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_bin_spectra_qBinning <- function(x) {
  all(
    checkmate::test_choice(x$call, "bin_spectra"),
    checkmate::test_choice(x$algorithm, "qBinning")
  )
}



#' @title Settings_bin_spectra_StreamFind
#'
#' @description Bins spectral data according to units of a given variable (e.g., 5 retention time values) or based on 
#' bins given as a named list of numeric values, where the names are the bin labels (i.e. the name of the column) and 
#' the values are the bin dimensions (e.g. 5 seconds).
#' 
#' @param unitsVal Character of length one with the column name of the variable to be used for binning.
#' @param unitsNumber Integer of length one with the number of units to be used for binning.
#' @param bins Named list of numeric values with the bin dimensions.
#' @param refBinAnalysis The analysis index to use a reference for creating the bins.
#' 
#' @returns A ProcessingSettings S3 class object with subclass Settings_bin_spectra_StreamFind.
#'
#' @export 
#'
Settings_bin_spectra_StreamFind <- function(unitsVal = NULL,
                                            unitsNumber = NULL,
                                            bins = NULL,
                                            refBinAnalysis = NULL) {
  
  settings <- list(
    call = "bin_spectra",
    algorithm = "StreamFind",
    parameters = list(
      unitsVal = unitsVal,
      unitsNumber = unitsNumber,
      bins = bins,
      refBinAnalysis = refBinAnalysis
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_bin_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "bin_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_character(x$parameters$unitsVal, len = 1, null.ok = TRUE),
    checkmate::test_integer(x$parameters$unitsNumber, len = 1, null.ok = TRUE),
    checkmate::test_list(x$parameters$bins, null.ok = TRUE),
    checkmate::test_integer(x$parameters$refBinAnalysis, len = 1, null.ok = TRUE)
  )
}





# ______________________________________________________________________________________________________________________
# find_features -----
# ______________________________________________________________________________________________________________________

#' @title Settings_find_features_qPeaks
#'
#' @description Not yet implemented.
#'
#' @export
#'
Settings_find_features_qPeaks <- function() {
  
  # TODO Max - update settings info
  
  settings <- list(
    call = "find_features",
    algorithm = "qPeaks",
    parameters = list(),
    version = as.character(packageVersion("StreamFind")),
    software = "q",
    developer = "Max, Gerrit",
    contact = "max@email.de",
    link = "https://github.com/odea-project/qAlgorithms",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_find_features_qPeaks <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "qPeaks")
  )
}



#' @title Settings_find_features_xcms3_centwave
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
#' @return A ProcessingSettings S3 class object with subclass Settings_find_features_xcms3_centwave.
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
Settings_find_features_xcms3_centwave <- function(ppm = 12,
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
  
  settings <- list(
    call = "find_features",
    algorithm = "xcms3_centwave",
    parameters = list(
      class = "CentWaveParam",
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
      roiList = list(),
      firstBaselineCheck = firstBaselineCheck,
      roiScales = numeric(),
      extendLengthMSW = extendLengthMSW
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "xcms",
    developer = "Ralf Tautenhahn, Johannes Rainer",
    contact = "rtautenh@ipb-halle.de",
    link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
    doi = "https://doi.org/10.1186/1471-2105-9-504"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_find_features_xcms3_centwave <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "xcms3_centwave"),
    checkmate::test_numeric(x$parameters$ppm, len = 1),
    checkmate::test_numeric(x$parameters$peakwidth, len = 2),
    checkmate::test_numeric(x$parameters$snthresh, len = 1),
    checkmate::test_numeric(x$parameters$prefilter, len = 2),
    checkmate::test_choice(x$parameters$mzCenterFun, c("wMean", "mean", "apex", "wMeanApex3", "meanApex3")),
    checkmate::test_integer(x$parameters$integrate, len = 1),
    checkmate::test_numeric(x$parameters$mzdiff, len = 1),
    checkmate::test_logical(x$parameters$fitgauss, len = 1),
    checkmate::test_numeric(x$parameters$noise, len = 1),
    checkmate::test_logical(x$parameters$verboseColumns, len = 1),
    checkmate::test_logical(x$parameters$firstBaselineCheck, len = 1),
    checkmate::test_logical(x$parameters$extendLengthMSW, len = 1)
  )
}



#' @title Settings_find_features_xcms3_matchedfilter
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
#' @return A ProcessingSettings S3 class object with subclass Settings_find_features_xcms3_matchedfilter.
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
Settings_find_features_xcms3_matchedfilter <- function(binSize = 0.5,
                                                       impute = "none",
                                                       baseValue = 0,
                                                       distance = 0,
                                                       fwhm = 30,
                                                       max = 5,
                                                       snthresh = 20,
                                                       steps = 2,
                                                       mzdiff = 0.5,
                                                       index = FALSE) {
  
  settings <- list(
    call = "find_features",
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
    version = as.character(packageVersion("StreamFind")),
    software = "xcms",
    developer = "Ralf Tautenhahn, Johannes Rainer",
    contact = "rtautenh@ipb-halle.de",
    link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
    doi = "https://doi.org/10.1186/1471-2105-9-504"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_find_features_xcms3_matchedfilter <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "xcms3_matchedfilter"),
    checkmate::test_numeric(x$parameters$binSize, len = 1),
    checkmate::test_choice(x$parameters$impute, c("none", "lin", "linbase", "intlin")),
    checkmate::test_numeric(x$parameters$baseValue, len = 1),
    checkmate::test_numeric(x$parameters$distance, len = 1),
    checkmate::test_numeric(x$parameters$fwhm, len = 1),
    checkmate::test_numeric(x$parameters$max, len = 1),
    checkmate::test_numeric(x$parameters$snthresh, len = 1),
    checkmate::test_numeric(x$parameters$steps, len = 1),
    checkmate::test_numeric(x$parameters$mzdiff, len = 1),
    checkmate::test_logical(x$parameters$index, len = 1)
  )
}



#' @title Settings_find_features_openms
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
#' @return A ProcessingSettings S3 class object with subclass Settings_find_features_openms.
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
Settings_find_features_openms <- function(noiseThrInt = 1000,
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
  
  settings <- list(
    call = "find_features",
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
    version = as.character(packageVersion("StreamFind")),
    software = "openms",
    developer = "Oliver Kohlbacher",
    contact = "oliver.kohlbacher@uni-tuebingen.de",
    link = "https://openms.de/",
    doi = "https://doi.org/10.1038/nmeth.3959"
  )
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @export
#' @noRd
#'
validate.Settings_find_features_openms <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "openms"),
    checkmate::test_numeric(x$parameters$noiseThrInt, len = 1),
    checkmate::test_numeric(x$parameters$chromSNR, len = 1),
    checkmate::test_numeric(x$parameters$chromFWHM, len = 1),
    checkmate::test_numeric(x$parameters$mzPPM, len = 1),
    checkmate::test_logical(x$parameters$reEstimateMTSD, len = 1),
    checkmate::test_choice(x$parameters$traceTermCriterion, c("outlier", "sample_rate")),
    checkmate::test_numeric(x$parameters$traceTermOutliers, len = 1),
    checkmate::test_numeric(x$parameters$minSampleRate, len = 1),
    checkmate::test_numeric(x$parameters$minTraceLength, len = 1),
    checkmate::test_numeric(x$parameters$maxTraceLength, len = 1),
    checkmate::test_choice(x$parameters$widthFiltering, c("fixed", "auto")),
    checkmate::test_numeric(x$parameters$minFWHM, len = 1),
    checkmate::test_numeric(x$parameters$maxFWHM, len = 1),
    checkmate::test_logical(x$parameters$traceSNRFiltering, len = 1),
    checkmate::test_numeric(x$parameters$localRTRange, len = 1),
    checkmate::test_numeric(x$parameters$localMZRange, len = 1),
    checkmate::test_choice(x$parameters$isotopeFilteringModel, c("none", "2%", "5%", "averagine")),
    checkmate::test_logical(x$parameters$MZScoring13C, len = 1),
    checkmate::test_logical(x$parameters$useSmoothedInts, len = 1),
    checkmate::test_list(x$parameters$extraOpts, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$intSearchRTWindow, len = 1),
    checkmate::test_logical(x$parameters$useFFMIntensities, len = 1),
    checkmate::test_logical(x$parameters$verbose, len = 1)
  )
}



#' @title Settings_find_features_kpic2
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
#' @return A ProcessingSettings S3 class object with subclass Settings_find_features_kpic2.
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
Settings_find_features_kpic2 <- function(level = 500,
                                         mztol = 0.01,
                                         gap = 2,
                                         width = 5,
                                         min_snr = 4,
                                         kmeans = TRUE,
                                         alpha = 0.3) {
  
  if (!requireNamespace("KPIC", quietly = TRUE)) warning("KPIC package required but not installed!")
  
  settings <- list(
    call = "find_features",
    algorithm = "kpic2",
    parameters = list(
      kmeans = kmeans,
      level = level,
      mztol = mztol,
      gap = gap,
      width = width,
      min_snr = min_snr
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "kpic2",
    developer = "Hongchao Ji",
    contact = "ji.hongchao@foxmail.com",
    link = NA_character_,
    doi = "10.1021/acs.analchem.7b01547"
  )
  
  if (kmeans) settings$parameters$alpha <- alpha
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_find_features_kpic2 <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "kpic2"),
    checkmate::test_numeric(x$parameters$level, len = 1),
    checkmate::test_numeric(x$parameters$mztol, len = 1),
    checkmate::test_numeric(x$parameters$gap, len = 1),
    checkmate::test_numeric(x$parameters$width, len = 1),
    checkmate::test_numeric(x$parameters$min_snr, len = 1),
    checkmate::test_logical(x$parameters$kmeans, len = 1),
    if (x$parameters$kmeans) checkmate::test_numeric(x$parameters$alpha, len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# group_features -----
# ______________________________________________________________________________________________________________________

#' @title Settings_group_features_xcms3_peakdensity
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
#' @return A ProcessingSettings S3 class object with subclass Settings_group_features_xcms3_peakdensity.
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
Settings_group_features_xcms3_peakdensity <- function(bw = 5,
                                                      minFraction = 1,
                                                      minSamples = 1,
                                                      binSize = 0.008,
                                                      maxFeatures = 100) {
  
  settings <- list(
    call = "group_features",
    algorithm = "xcms3_peakdensity",
    parameters = list(
      "rtalign" = FALSE,
      "groupParam" = list(
        class = "PeakDensityParam",
        sampleGroups = "holder",
        bw = bw,
        minFraction = minFraction,
        minSamples = minSamples,
        binSize = binSize,
        maxFeatures = maxFeatures
      )
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "xcms",
    developer = "Colin Smith, Johannes Rainer",
    contact = "siuzdak@scripps.edu",
    link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
    doi = "https://doi.org/10.1021/ac051437y"
  )
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @export
#' @noRd
#'
validate.Settings_group_features_xcms3_peakdensity <- function(x) {
  all(
    checkmate::test_choice(x$call, "group_features"),
    checkmate::test_choice(x$algorithm, "xcms3_peakdensity"),
    checkmate::test_logical(x$parameters$rtalign, len = 1),
    checkmate::test_list(x$parameters$groupParam, len = 1),
    checkmate::test_choice(x$parameters$groupParam$class, "PeakDensityParam"),
    checkmate::test_list(x$parameters$groupParam$sampleGroups, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$bw, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$minFraction, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$minSamples, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$binSize, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$maxFeatures, len = 1)
  )
}



#' @title Settings_group_features_xcms3_peakdensity_peakgroups
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
#' @param peakGroupsMatrix optional matrix of (raw) retention times for the peak
#' groups on which the alignment should be performed. Each column represents a
#' sample, each row a feature/peak group. Such a matrix is for example returned
#' by the adjustRtimePeakGroups method.
#' @param subset integer with the indices of samples within the experiment on
#' which the alignment models should be estimated. Samples not part of the subset
#' are adjusted based on the closest subset sample. See description above
#' for more details.
#' @param subsetAdjust character specifying the method with which non-subset
#' samples should be adjusted. Supported options are "previous" and "average"
#' (default). See description above for more information.
#'
#' @details See the \link[patRoon]{groupFeaturesXCMS3} function from the \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_group_features_xcms3_peakdensity_peakgroups.
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
Settings_group_features_xcms3_peakdensity_peakgroups <- function(bw = 5,
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
                                                                 peakGroupsMatrix = matrix(nrow = 0, ncol = 0),
                                                                 subset = integer(),
                                                                 subsetAdjust = "average") {
  
  settings <- list(
    call = "group_features",
    algorithm = "xcms3_peakdensity_peakgroups",
    parameters = list(
      "rtalign" = TRUE,
      "groupParam" = list(
        class = "PeakDensityParam",
        sampleGroups = "holder",
        bw = bw,
        minFraction = minFraction,
        minSamples = minSamples,
        binSize = binSize,
        maxFeatures = maxFeatures
      ),
      "preGroupParam" = list(
        class = "PeakDensityParam",
        sampleGroups = "holder",
        bw = pre_bw,
        minFraction = pre_minFraction,
        minSamples = pre_minSamples,
        binSize = pre_binSize,
        maxFeatures = maxFeatures
      ),
      "retAlignParam" = list(
        class = "PeakGroupsParam",
        minFraction = rtAlignMinFraction,
        extraPeaks = extraPeaks,
        smooth = smooth,
        span = span,
        family = family,
        peakGroupsMatrix = peakGroupsMatrix,
        subset = as.integer(subset),
        subsetAdjust = "average"
      )
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "xcms",
    developer = "Colin Smith, Johannes Rainer",
    contact = "siuzdak@scripps.edu",
    link = "https://bioconductor.org/packages/release/bioc/html/xcms.html",
    doi = "https://doi.org/10.1021/ac051437y"
  )
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @export
#' @noRd
#'
validate.Settings_group_features_xcms3_peakdensity_peakgroups <- function(x) {
  all(
    checkmate::test_choice(x$call, "group_features"),
    checkmate::test_choice(x$algorithm, "xcms3_peakdensity_peakgroups"),
    checkmate::test_logical(x$parameters$rtalign, len = 1),
    checkmate::test_list(x$parameters$groupParam, len = 1),
    checkmate::test_choice(x$parameters$groupParam$class, "PeakDensityParam"),
    checkmate::test_list(x$parameters$groupParam$sampleGroups, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$bw, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$minFraction, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$minSamples, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$binSize, len = 1),
    checkmate::test_numeric(x$parameters$groupParam$maxFeatures, len = 1),
    checkmate::test_list(x$parameters$preGroupParam, len = 1),
    checkmate::test_choice(x$parameters$preGroupParam$class, "PeakDensityParam"),
    checkmate::test_list(x$parameters$preGroupParam$sampleGroups, len = 1),
    checkmate::test_numeric(x$parameters$preGroupParam$bw, len = 1),
    checkmate::test_numeric(x$parameters$preGroupParam$minFraction, len = 1),
    checkmate::test_numeric(x$parameters$preGroupParam$minSamples, len = 1),
    checkmate::test_numeric(x$parameters$preGroupParam$binSize, len = 1),
    checkmate::test_numeric(x$parameters$preGroupParam$maxFeatures, len = 1),
    checkmate::test_list(x$parameters$retAlignParam, len = 1),
    checkmate::test_choice(x$parameters$retAlignParam$class, "PeakGroupsParam"),
    checkmate::test_numeric(x$parameters$retAlignParam$minFraction, len = 1),
    checkmate::test_numeric(x$parameters$retAlignParam$extraPeaks, len = 1),
    checkmate::test_choice(x$parameters$retAlignParam$smooth, c("loess", "linear")),
    checkmate::test_numeric(x$parameters$retAlignParam$span, len = 1),
    checkmate::test_choice(x$parameters$retAlignParam$family, c("gaussian", "symmetric")),
    checkmate::test_matrix(x$parameters$retAlignParam$peakGroupsMatrix),
    checkmate::test_integer(x$parameters$retAlignParam$subset),
    checkmate::test_choice(x$parameters$retAlignParam$subsetAdjust, c("previous", "average"))
  )
}



#' @title Settings_group_features_openms
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
#' @param extraOptsRT Named list containing extra options that will be passed
#' to MapAlignerPoseClustering.
#' @param extraOptsGroup Named list containing extra options that will be passed
#' to FeatureLinkerUnlabeledQT/FeatureLinkerUnlabeled.
#' @param verbose Logical of length one. When TRUE adds processing information
#' to the console.
#'
#' @details See the \link[patRoon]{groupFeaturesOpenMS} function from the \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_group_features_openms.
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
Settings_group_features_openms <- function(rtalign = FALSE,
                                           QT = FALSE,
                                           maxAlignRT = 5,
                                           maxAlignMZ = 0.008,
                                           maxGroupRT = 5,
                                           maxGroupMZ = 0.008,
                                           extraOptsRT = NULL,
                                           extraOptsGroup = NULL,
                                           verbose = FALSE) {
  
  settings <- list(
    call = "group_features",
    algorithm = "openms",
    parameters = list(
      rtalign = rtalign,
      QT = QT,
      maxAlignRT = maxAlignRT,
      maxAlignMZ = maxAlignMZ,
      maxGroupRT = maxGroupRT,
      maxGroupMZ = maxGroupMZ,
      extraOptsRT = extraOptsRT,
      extraOptsGroup = extraOptsGroup,
      verbose = verbose
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "openms",
    developer = "Oliver Kohlbacher",
    contact = "oliver.kohlbacher@uni-tuebingen.de",
    link = "https://openms.de/",
    doi = "https://doi.org/10.1038/nmeth.3959"
  )
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @export
#' @noRd
#'
validate.Settings_group_features_openms <- function(x) {
  all(
    checkmate::test_choice(x$call, "group_features"),
    checkmate::test_choice(x$algorithm, "openms"),
    checkmate::test_logical(x$parameters$rtalign, len = 1),
    checkmate::test_logical(x$parameters$QT, len = 1),
    checkmate::test_numeric(x$parameters$maxAlignRT, len = 1),
    checkmate::test_numeric(x$parameters$maxAlignMZ, len = 1),
    checkmate::test_numeric(x$parameters$maxGroupRT, len = 1),
    checkmate::test_numeric(x$parameters$maxGroupMZ, len = 1),
    checkmate::test_list(x$parameters$extraOptsRT, null.ok = TRUE),
    checkmate::test_list(x$parameters$extraOptsGroup, null.ok = TRUE),
    checkmate::test_logical(x$parameters$verbose, len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# load_features -----
# ______________________________________________________________________________________________________________________

#' @title Settings_load_features_ms1_StreamFind
#'
#' @description Settings for loading MS1 spectra for features.
#'
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_load_features_ms1_StreamFind.
#'
#' @export
#'
Settings_load_features_ms1_StreamFind <- function(rtWindow = c(-2, 2),
                                                  mzWindow = c(-1, 6),
                                                  mzClust = 0.005,
                                                  presence = 0.8,
                                                  minIntensity = 250,
                                                  filtered = FALSE) {
  
  settings <- list(
    call = "load_features_ms1",
    algorithm = "StreamFind",
    parameters = list(
      "rtWindow" = rtWindow,
      "mzWindow" = mzWindow,
      "mzClust" = mzClust,
      "presence" = presence,
      "minIntensity" = minIntensity,
      "filtered" = filtered
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_load_features_ms1_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_features_ms1"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_double(as.numeric(x$parameters$rtWindow), max.len = 2),
    checkmate::test_double(as.numeric(x$parameters$mzWindow), max.len = 2),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1)
  )
}



#' @title Settings_load_features_ms2_StreamFind
#'
#' @description Settings for loading MS2 spectra for features.
#'
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_load_features_ms2_StreamFind.
#'
#' @export
#'
Settings_load_features_ms2_StreamFind <- function(isolationWindow = 1.3,
                                                  mzClust = 0.005,
                                                  presence = 0.8,
                                                  minIntensity = 10,
                                                  filtered = FALSE) {
  
  settings <- list(
    call = "load_features_ms2",
    algorithm = "StreamFind",
    parameters = list(
      "isolationWindow" = isolationWindow,
      "mzClust" = mzClust,
      "presence" = presence,
      "minIntensity" = minIntensity,
      "filtered" = filtered
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
 as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_load_features_ms2_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_features_ms2"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$isolationWindow),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1)
  )
}



#' @title Settings_load_features_eic_StreamFind
#'
#' @description Settings for loading spectra EIC for feature groups.
#'
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_load_features_eic_StreamFind.
#'
#' @export
#'
Settings_load_features_eic_StreamFind <- function(rtExpand = 120, mzExpand = 0, filtered = FALSE) {
  
  settings <- list(
    call = "load_features_eic",
    algorithm = "StreamFind",
    parameters = list(
      "rtExpand" = rtExpand,
      "mzExpand" = mzExpand,
      "filtered" = filtered
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @export
#' @noRd
#'
validate.Settings_load_features_eic_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_features_eic"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$rtExpand),
    checkmate::test_number(x$parameters$mzExpand),
    checkmate::test_logical(x$parameters$filtered, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# load_MSPeakLists -----
# ______________________________________________________________________________________________________________________

#' @title Settings_load_MSPeakLists_patRoon
#'
#' @description Settings for loading MS2 and MS1 spectra for feature groups.
#'
#' @param maxMSRtWindow Maximum chromatographic peak window used for spectrum 
#' averaging (in seconds, +/- retention time). If NULL all spectra from a feature 
#' will be taken into account. Lower to decrease processing time.
#' @param precursorMzWindow The m/z window (in Da) to find MS/MS spectra of a precursor. 
#' This is typically used for Data-Dependent like MS/MS data and should correspond to the 
#' isolation m/z window (i.e. +/- the precursor m/z) that was used to collect the data. 
#' For Data-Independent MS/MS experiments, where precursor ions are not isolated prior to 
#' fragmentation (e.g. bbCID, MSe, all-ion, ...) the value should be NULL.
#' @param clusterMzWindow m/z window (in Da) used for clustering m/z values
#' when spectra are averaged. For method="hclust" this corresponds to the
#' cluster height, while for method="distance" this value is used to find
#' nearby masses (+/- window). Too small windows will prevent clustering
#' m/z values (thus erroneously treating equal masses along spectra as
#' different), whereas too big windows may cluster unrelated m/z values
#' from different or even the same spectrum together.
#' @param topMost Only retain this maximum number of MS peaks when generating
#' averaged spectra. Lowering this number may exclude more irrelevant (noisy)
#' MS peaks and decrease processing time, whereas higher values may avoid
#' excluding lower intense MS peaks that may still be of interest.
#' @param minIntensityPre MS peaks with intensities below this value will
#' be removed (applied prior to selection by `topMost`) before averaging.
#' @param minIntensityPost MS peaks with intensities below this value will
#' be removed after averaging.
#' @param avgFun Function that is used to calculate average m/z values.
#' @param method Method used for producing averaged MS spectra. Valid
#' values are "hclust", used for hierarchical clustering (using the
#' fastcluster package), and "distance", to use the between peak distance.
#' The latter method may reduces processing time and memory requirements,
#' at the potential cost of reduced accuracy.
#' @param pruneMissingPrecursorMS For MS data only: if TRUE then peak lists
#' without a precursor peak are removed. Note that even when this is set to
#' FALSE, functionality that relies on MS (not MS/MS) peak lists (e.g.
#' formulae calculation) will still skip calculation if a precursor is not
#' found.
#' @param retainPrecursorMSMS For MS/MS data only: if TRUE then always
#' retain the precursor mass peak even if is not among the `topMost` peaks.
#' Note that MS precursor mass peaks are always kept. Furthermore, note
#' that precursor peaks in both MS and MS/MS data may still be removed by
#' intensity thresholds (this is unlike the filter method function).
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_load_MSPeakLists_patRoon.
#'
#' @export
#'
Settings_load_MSPeakLists_patRoon <- function(maxMSRtWindow = 5,
                                              precursorMzWindow = 4,
                                              clusterMzWindow = 0.005,
                                              topMost = 100,
                                              minIntensityPre = 50,
                                              minIntensityPost = 50,
                                              avgFun = mean,
                                              method = "hclust",
                                              retainPrecursorMSMS = TRUE) {
  
  settings <- list(
    call = "load_MSPeakLists",
    algorithm = "patRoon",
    parameters = list(
      maxMSRtWindow = maxMSRtWindow,
      precursorMzWindow = precursorMzWindow,
      clusterMzWindow = clusterMzWindow,
      topMost = topMost,
      minIntensityPre = minIntensityPre,
      minIntensityPost = minIntensityPost,
      avgFun = avgFun,
      method = method,
      retainPrecursorMSMS = retainPrecursorMSMS
    ),
    version = as.character(packageVersion("patRoon")),
    software = "patRoon",
    developer = "Rick Helmus",
    contact = "r.helmus@uva.nl",
    link = "https://github.com/rickhelmus/patRoon",
    doi = "10.21105/joss.04029"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_load_MSPeakLists_patRoon <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_MSPeakLists"),
    checkmate::test_choice(x$algorithm, "patRoon"),
    checkmate::test_numeric(x$parameters$maxMSRtWindow, len = 1),
    checkmate::test_numeric(x$parameters$precursorMzWindow, len = 1),
    checkmate::test_numeric(x$parameters$clusterMzWindow, len = 1),
    checkmate::test_numeric(x$parameters$topMost, len = 1),
    checkmate::test_numeric(x$parameters$minIntensityPre, len = 1),
    checkmate::test_numeric(x$parameters$minIntensityPost, len = 1),
    checkmate::test_function(x$parameters$avgFun),
    checkmate::test_choice(x$parameters$method, c("hclust", "distance")),
    checkmate::test_logical(x$parameters$retainPrecursorMSMS, len = 1)
  )
}



#' @title Settings_load_MSPeakLists_StreamFind
#'
#' @description Settings for converting loaded MS2 and MS1 spectra into a `MSPeakLists` object from patRoon.
#'
#' @param clusterMzWindow m/z window (in Da) used for clustering m/z values
#' when spectra are averaged. For method="hclust" this corresponds to the
#' cluster height, while for method="distance" this value is used to find
#' nearby masses (+/- window). Too small windows will prevent clustering
#' m/z values (thus erroneously treating equal masses along spectra as
#' different), whereas too big windows may cluster unrelated m/z values
#' from different or even the same spectrum together.
#' @param topMost Only retain this maximum number of MS peaks when generating
#' averaged spectra. Lowering this number may exclude more irrelevant (noisy)
#' MS peaks and decrease processing time, whereas higher values may avoid
#' excluding lower intense MS peaks that may still be of interest.
#' @param minIntensityPre MS peaks with intensities below this value will
#' be removed (applied prior to selection by `topMost`) before averaging.
#' @param minIntensityPost MS peaks with intensities below this value will
#' be removed after averaging.
#' @param avgFun Function that is used to calculate average m/z values.
#' @param method Method used for producing averaged MS spectra. Valid
#' values are "hclust", used for hierarchical clustering (using the
#' fastcluster package), and "distance", to use the between peak distance.
#' The latter method may reduces processing time and memory requirements,
#' at the potential cost of reduced accuracy.
#' @param pruneMissingPrecursorMS For MS data only: if TRUE then peak lists
#' without a precursor peak are removed. Note that even when this is set to
#' FALSE, functionality that relies on MS (not MS/MS) peak lists (e.g.
#' formulae calculation) will still skip calculation if a precursor is not
#' found.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_load_MSPeakLists_StreamFind.
#'
#' @export
#'
Settings_load_MSPeakLists_StreamFind <- function(clusterMzWindow = 0.005,
                                                 topMost = 100,
                                                 minIntensityPre = 50,
                                                 minIntensityPost = 50,
                                                 avgFun = mean,
                                                 method = "distance") {
  
  settings <- list(
    call = "load_MSPeakLists",
    algorithm = "StreamFind",
    parameters = list(
      clusterMzWindow = clusterMzWindow,
      topMost = topMost,
      minIntensityPre = minIntensityPre,
      minIntensityPost = minIntensityPost,
      avgFun = avgFun,
      method = method
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_load_MSPeakLists_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_MSPeakLists"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_numeric(x$parameters$clusterMzWindow, len = 1),
    checkmate::test_numeric(x$parameters$topMost, len = 1),
    checkmate::test_numeric(x$parameters$minIntensityPre, len = 1),
    checkmate::test_numeric(x$parameters$minIntensityPost, len = 1),
    checkmate::test_function(x$parameters$avgFun),
    checkmate::test_choice(x$parameters$method, c("hclust", "distance"))
  )
}





# ______________________________________________________________________________________________________________________
# filter_features -----
# ______________________________________________________________________________________________________________________

#' @title Settings_filter_features_StreamFind
#'
#' @description Settings for filtering of features and feature groups.
#'
#' @param minSnRatio Numeric (length 1) with the minimum signal-to-noise ratio.
#' @param excludeIsotopes Logical (length 1) with `TRUE` for filtering annotated isotopes (only prevails the monoisotopic features).
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_filter_features_StreamFind.
#'
#' @export
#'
Settings_filter_features_StreamFind <- function(minSnRatio = 3, excludeIsotopes = TRUE) {
  
  settings <- list(
    call = "filter_features",
    algorithm = "StreamFind",
    parameters = list(
      minSnRatio = minSnRatio,
      excludeIsotopes = excludeIsotopes
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_filter_features_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "filter_features"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_numeric(x$parameters$minSnRatio, len = 1),
    checkmate::test_logical(x$parameters$excludeIsotopes, len = 1)
  )
}



#' @title Settings_filter_features_patRoon
#'
#' @description Settings for filtering of features and feature groups. A full description of the filtering parameters is
#'  in \code{\link[patRoon]{replicateGroupSubtract}} from patRoon package.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_filter_features_patRoon.
#'
#' @details Note that when filters are applied to features or feature groups 
#' these require specific results from processing modules. For instance, 
#' subtracting the blank can only be done after grouping features. Also, some 
#' filters require. Thus, not all filters can be applied to features. 
#' See \code{\link[patRoon]{features-class}} and \code{\link[patRoon]{replicateGroupSubtract}} 
#' for further information.
#' 
#' @export
#'
Settings_filter_features_patRoon <- function(absMinIntensity = NULL,
                                             relMinIntensity = NULL,
                                             preAbsMinIntensity = NULL,
                                             preRelMinIntensity = NULL,
                                             absMinAnalyses = NULL,
                                             relMinAnalyses = NULL,
                                             absMinReplicates = NULL,
                                             relMinReplicates = NULL,
                                             absMinFeatures = NULL,
                                             relMinFeatures = NULL,
                                             absMinReplicateAbundance = NULL,
                                             relMinReplicateAbundance = NULL,
                                             absMinConc = NULL,
                                             relMinConc = NULL,
                                             absMaxTox = NULL,
                                             relMaxTox = NULL,
                                             absMinConcTox = NULL,
                                             relMinConcTox = NULL,
                                             maxReplicateIntRSD = NULL,
                                             blankThreshold = NULL,
                                             retentionRange = NULL,
                                             mzRange = NULL,
                                             mzDefectRange = NULL,
                                             chromWidthRange = NULL,
                                             featQualityRange = NULL,
                                             groupQualityRange = NULL,
                                             rGroups = NULL,
                                             results = NULL,
                                             removeBlanks = FALSE,
                                             removeISTDs = FALSE,
                                             checkFeaturesSession = NULL,
                                             # predAggrParams = patRoon::getDefPredAggrParams(),
                                             removeNA = FALSE,
                                             negate = FALSE) {
  
  settings <- list(
    call = "filter_features",
    algorithm = "patRoon",
    parameters = list(),
    version = as.character(packageVersion("patRoon")),
    software = "patRoon",
    developer = "Rick Helmus",
    contact = "r.helmus@uva.nl",
    link = "https://github.com/rickhelmus/patRoon",
    doi = "10.21105/joss.04029"
  )
  
  settings$parameters <- list(
    "absMinIntensity" = absMinIntensity,
    "relMinIntensity" = relMinIntensity,
    "preAbsMinIntensity" = preAbsMinIntensity,
    "preRelMinIntensity" = preRelMinIntensity,
    "absMinAnalyses" = absMinAnalyses,
    "relMinAnalyses" = relMinAnalyses,
    "absMinReplicates" = absMinReplicates,
    "relMinReplicates" = relMinReplicates,
    "absMinFeatures" = absMinFeatures,
    "relMinFeatures" = relMinFeatures,
    "absMinReplicateAbundance" = absMinReplicateAbundance,
    "relMinReplicateAbundance" = relMinReplicateAbundance,
    "absMinConc" = absMinConc,
    "relMinConc" = relMinConc,
    "absMaxTox" = absMaxTox,
    "relMaxTox" = relMaxTox,
    "absMinConcTox" = absMinConcTox,
    "relMinConcTox" = relMinConcTox,
    "maxReplicateIntRSD" = maxReplicateIntRSD,
    "blankThreshold" = blankThreshold,
    "retentionRange" = retentionRange,
    "mzRange" = mzRange,
    "mzDefectRange" = mzDefectRange,
    "chromWidthRange" = chromWidthRange,
    "featQualityRange" = featQualityRange,
    "groupQualityRange" = groupQualityRange,
    "rGroups" = rGroups,
    "results" = results,
    "removeBlanks" = removeBlanks,
    "removeISTDs" = removeISTDs,
    "checkFeaturesSession" = checkFeaturesSession,
    # "predAggrParams" = predAggrParams,
    "removeNA" = removeNA,
    "negate" = negate
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_filter_features_patRoon <- function(x) {
  all(
    checkmate::test_choice(x$call, "filter_features"),
    checkmate::test_choice(x$algorithm, "patRoon"),
    checkmate::test_numeric(x$parameters$absMinIntensity, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$relMinIntensity, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$preAbsMinIntensity, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$preRelMinIntensity, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$absMinAnalyses, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$relMinAnalyses, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$absMinReplicates, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$relMinReplicates, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$absMinFeatures, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$relMinFeatures, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$absMinReplicateAbundance, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$relMinReplicateAbundance, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$absMinConc, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$relMinConc, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$absMaxTox, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$relMaxTox, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$absMinConcTox, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$relMinConcTox, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$maxReplicateIntRSD, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$blankThreshold, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$retentionRange, len = 2, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$mzRange, len = 2, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$mzDefectRange, len = 2, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$chromWidthRange, len = 2, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$featQualityRange, len = 2, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$groupQualityRange, len = 2, null.ok = TRUE),
    checkmate::test_character(x$parameters$rGroups, null.ok = TRUE),
    checkmate::test_list(x$parameters$results, null.ok = TRUE),
    checkmate::test_logical(x$parameters$removeBlanks, len = 1),
    checkmate::test_logical(x$parameters$removeISTDs, len = 1),
    checkmate::test_list(x$parameters$checkFeaturesSession, null.ok = TRUE),
    checkmate::test_logical(x$parameters$removeNA, len = 1),
    checkmate::test_logical(x$parameters$negate, len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# annotate_features -----
# ______________________________________________________________________________________________________________________

#' @title Settings_annotate_features_StreamFind
#'
#' @description Settings for annotation of isotopic features. The method uses the `maxIsotopes` to define the maximum 
#' length of the isotopic chain. The list of candidate features is build with the `rtWindowAlignment` and the maximum 
#' mass increment to match the maximum chain length. Then, the mass difference  of the natural isotopes defined by 
#' `elements` and a given monoisotopic ion (i.e., feature) are targeted. Each candidate is then evaluated according to 
#' the mass error and the expected relative intensity range as defined by the `mode`.
#'
#' @param maxIsotopes Numeric (length 1) with the maximum number of isotopic steps.
#' @param elements Character vector with the elements to target the isotopic annotation.
#' Possible elements are C, H, N, O, S, Cl, Br.
#' @param mode Character (length 1) with the type of molecules to be targeted. For now, only "small molecules" are possible.
#' @param maxCharge Numeric (length 1) with the maximum charge that ions can be ionized to find isotopes.
#' @param rtWindowAlignment Numeric (length 1) with the proportion of the monoisotopic feature time window to be used 
#' for retrieving isotopic candidates.
#' @param maxGaps Numeric (length 1) with the maximum of allowed gaps in isotopic chains.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_annotate_features_StreamFind.
#'
#' @export
#'
Settings_annotate_features_StreamFind <- function(maxIsotopes = 5,
                                                  elements = c("C", "H", "N", "O", "S", "Cl", "Br"),
                                                  mode = "small molecules",
                                                  maxCharge = 1,
                                                  rtWindowAlignment = 0.3,
                                                  maxGaps = 1) {
  
  settings <- list(
    call = "annotate_features",
    algorithm = "StreamFind",
    parameters = list(
      "maxIsotopes" = maxIsotopes,
      "elements" = elements,
      "mode" = mode,
      "maxCharge" = maxCharge,
      "rtWindowAlignment" = rtWindowAlignment,
      "maxGaps" = maxGaps
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_annotate_features_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "annotate_features"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_count(x$parameters$maxIsotopes),
    checkmate::test_count(x$parameters$maxCharge),
    checkmate::test_count(x$parameters$maxGaps),
    checkmate::test_number(x$parameters$rtWindowAlignment),
    checkmate::test_choice(x$parameters$mode, "small molecules"),
    checkmate::test_vector(x$parameters$elements, any.missing = FALSE, min.len = 1),
    vapply(x$parameters$elements, function(i) {
      checkmate::test_choice(i, c("C", "H", "N", "O", "S", "Cl", "Br", "Si", "Ge"))
    }, FALSE),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# suspect_screening -----
# ______________________________________________________________________________________________________________________

#' @title Settings_suspect_screening_StreamFind
#'
#' @description Settings for performing suspect screening using a data.frame with target compounds.
#'
#' @param database A data.frame with at least the columns name and mass, indicating the name and neutral monoisotopic 
#' mass of the suspect targets.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-ppmMS2
#' @template arg-ms-minFragments
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_suspect_screening_StreamFind.
#'
#' @export
#'
Settings_suspect_screening_StreamFind <- function(database = NULL,
                                                  ppm = 5,
                                                  sec = 10,
                                                  ppmMS2 = 10,
                                                  minFragments = 3,
                                                  isolationWindow = 1.3,
                                                  mzClust = 0.003,
                                                  presence = 0.8,
                                                  minIntensity = 0,
                                                  filtered = FALSE) {
  
  settings <- list(
    call = "suspect_screening",
    algorithm = "StreamFind",
    parameters = list(
      "database" = database,
      "ppm" = ppm,
      "sec" = sec,
      "ppmMS2" = ppmMS2,
      "minFragments" = minFragments,
      "isolationWindow" = isolationWindow,
      "mzClust" = mzClust,
      "presence" = presence,
      "minIntensity" = minIntensity,
      "filtered" = filtered
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_suspect_screening_StreamFind <- function(x) {
  
  x$parameters$database <- as.data.table(x$parameters$database)
  
  all(
    checkmate::test_choice(x$call, "suspect_screening"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$ppm),
    checkmate::test_number(x$parameters$sec),
    checkmate::test_number(x$parameters$ppmMS2),
    checkmate::test_number(x$parameters$minFragments),
    checkmate::test_number(x$parameters$isolationWindow),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$presence),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$filtered, max.len = 1)
  ) && if (is.data.frame(x$parameters$database)) {
    all(c("name", "mass") %in% colnames(x$parameters$database)) ||
      all(c("name", "neutralMass") %in% colnames(x$parameters$database)) ||
      all(c("name", "mz") %in% colnames(x$parameters$database))
  } else {
    FALSE
  }
}



#' @title Settings_suspect_screening_forident
#'
#' @description Settings for performing suspect screening using the \href{https://water.for-ident.org/}{FOR-IDENT} platform.
#'
#' @param addMS2 Logical length 1. When `TRUE` and MS2 data is available, the
#' fragments pattern (i.e., MS2 averaged spectra) is added to the .txt file to
#' import in FOR-IDENT platform. Note that when `addMS2` is `TRUE` the \emph{m/z}
#' values are used instead of neutral mass even is `useNeutralMass` is set to `TRUE`.
#' @param useNeutralMass Logical length 1. When `TRUE` and neutral mass is
#' available, the neutral mass of features/feature groups is used instead of the
#' \emph{m/z}.
#' @param path Character length 1 with the path to save the .txt file with the
#' list of features for identification.
#' @param name Character length 1 with the name of the file (without extension)
#' to be saved in the `path`.
#'
#' @note
#' After processing, a .txt file as defined by name and path is created with the
#' list of features or feature groups to be imported in the FOR-IDENT platform
#' (\url{https://water.for-ident.org/}). Note that log in credentials are needed.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_suspect_screening_forident.
#'
#' @export
#'
Settings_suspect_screening_forident <- function(addMS2 = FALSE,
                                                useNeutralMass = TRUE,
                                                path = getwd(),
                                                name = "feature_list") {
  
  settings <- list(
    call = "suspect_screening",
    algorithm = "forident",
    parameters = list(
      "addMS2" = addMS2,
      "useNeutralMass" = useNeutralMass,
      "path" = path,
      "name" = name
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "forident",
    developer = "Sylvia Grosse, Thomas Letzel",
    contact = "support@for-ident.org",
    link = "https://water.for-ident.org/#!home",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_suspect_screening_forident <- function(x) {
  all(
    checkmate::test_choice(x$call, "suspect_screening"),
    checkmate::test_choice(x$algorithm, "forident"),
    dir.exists(x$parameters$path),
    is.character(x$parameters$name),
    length(x$parameters$name) == 1,
    checkmate::test_logical(x$parameters$addMS2, max.len = 1)
  )
}



#' @title Settings_suspect_screening_patRoon
#'
#' @description Settings for performing suspect screening using the function \link[patRoon]{screenSuspects} from the patRoon R package.
#'
#' @param suspects A data.frame with suspect information. See section Suspect list format in \link[patRoon]{screenSuspects} for more information.
#' @param rtWindow The retention time window (in seconds) that will be used for matching a suspect (+/- feature data).
#' @param mzWindow The m/z window that will be used for matching a suspect (+/- feature data)..
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_suspect_screening_patRoon.
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @export
#'
Settings_suspect_screening_patRoon <- function(suspects = NULL, rtWindow = 12, mzWindow = 0.005, filtered = FALSE) {

  settings <- list(
    call = "suspect_screening",
    algorithm = "patRoon",
    parameters = list(
      "suspects" = suspects,
      "rtWindow" = rtWindow,
      "mzWindow" = mzWindow,
      "filtered" = filtered
    ),
    version = as.character(packageVersion("patRoon")),
    software = "patRoon",
    developer = "Rick Helmus",
    contact = "r.helmus@uva.nl",
    link = "https://github.com/rickhelmus/patRoon",
    doi = "https://doi.org/10.1186/s13321-020-00477-w"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_suspect_screening_patRoon <- function(x) {
  
  x$parameters$database <- as.data.table(x$parameters$database)
  
  all(
    checkmate::test_choice(x$call, "suspect_screening"),
    checkmate::test_choice(x$algorithm, "patRoon"),
    checkmate::test_number(x$parameters$rtWindow),
    checkmate::test_number(x$parameters$mzWindow),
    checkmate::test_logical(x$parameters$filtered, max.len = 1)
  ) && if (is.data.frame(x$parameters$suspects)) {
    all(c("name", "neutralMass") %in% colnames(x$parameters$suspects)) ||
      all(c("name", "mz") %in% colnames(x$parameters$suspects))
  } else {
    FALSE
  }
}





# ______________________________________________________________________________________________________________________
# find_internal_standards -----
# ______________________________________________________________________________________________________________________

#' @title Settings_find_internal_standards_StreamFind
#'
#' @description Settings for finding internal standards using a data.frame.
#'
#' @param database A data.frame with at least the columns name, mass, and rt indicating the name, neutral monoisotopic 
#' mass and retention time of the internal standards, respectively.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_find_internal_standards_StreamFind.
#'
#' @export
#'
Settings_find_internal_standards_StreamFind <- function(database = NULL, ppm = 5, sec = 10) {
  
  settings <- list(
    call = "find_internal_standards",
    algorithm = "StreamFind",
    algorithm = "StreamFind",
    parameters = list(
      "database" = database,
      "ppm" = ppm,
      "sec" = sec
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_find_internal_standards_StreamFind <- function(x) {
  
  x$parameters$database <- as.data.table(x$parameters$database)
  
  all(
    checkmate::test_choice(x$call, "find_internal_standards"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$ppm),
    checkmate::test_number(x$parameters$sec)
  ) && if (is.data.frame(x$parameters$database)) {
    all(c("name", "mass", "rt") %in% colnames(x$parameters$database)) ||
      all(c("name", "mz", "rt") %in% colnames(x$parameters$database))
  } else {
    FALSE
  }
}





# ______________________________________________________________________________________________________________________
# calculate_quality -----
# ______________________________________________________________________________________________________________________

#' @title Settings_calculate_quality_StreamFind
#'
#' @description Settings for calculating quality parameters of features (e.g., signal-to-noise (sn) ratio).
#'
#' @template arg-ms-rtExpand 
#' @template arg-ms-mzExpand 
#' @param minTraces Numeric of length 1 with the minimum number traces for calculating feature quality.
#' @template arg-ms-filtered 
#' @template arg-runParallel 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_calculate_quality_StreamFind.
#'
#' @export
#'
Settings_calculate_quality_StreamFind <- function(rtExpand = 120,
                                                  mzExpand = 0.0003,
                                                  minTraces = 6,
                                                  filtered = FALSE,
                                                  runParallel = TRUE) {
  
  settings <- list(
    call = "calculate_quality",
    algorithm = "StreamFind",
    algorithm = "StreamFind",
    parameters = list(
      "rtExpand" = rtExpand,
      "mzExpand" = mzExpand,
      "minTraces" = minTraces,
      "filtered" = filtered,
      "runParallel" = runParallel
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_calculate_quality_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "calculate_quality"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$rtExpand),
    checkmate::test_number(x$parameters$mzExpand),
    checkmate::test_number(x$parameters$minTraces),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# generate_formulas -----
# ______________________________________________________________________________________________________________________

#' @title Settings_generate_formulas_genform
#'
#' @description Settings for generating formulas using the algorithm \href{https://sourceforge.net/projects/genform/}{GenForm}. 
#' The algorithm is used via the function \link[patRoon]{generateFormulas} from the package \pkg{patRoon}. Therefore, 
#' it is highly recommended to check the original documentation of the function in \pkg{patRoon} for more details.
#' 
#' @param relMzDev Numeric (length 1) with the relative mass deviation, in ppm.
#' @param elements Character vector with the elements to use for formulae annotation. Always try to work with a minimal 
#' set by excluding elements you don't expect.
#' @param hetero Logical (length 1) indicating if heteroatoms are allowed in the formulae.
#' @param oc Logical (length 1) indicating presence of at least one carbon in the formulae.
#' @param thrMS, thrMSMS, thrComb Numeric (length 1) Sets the thresholds for the GenForm MS score (isoScore), MS/MS score 
#' (MSMSScore) and combined score (combMatch). Sets the thms/thmsms/thcomb command line options, respectively. Set to 
#' NULL for no threshold.
#' @param maxCandidates Numeric (length 1) with the maximum number of candidates to be generated.
#' @param extraOpts Character (length 1) with extra CLI options to be passed to the GenForm algorithm.
#' @param calculateFeatures Logical (length 1) indicating if features should be calculated.
#' @param featThreshold Numeric (length 1). If `calculateFeatures` is TRUE the minimum presence (from 0 to 1) of features 
#' with formula annotation to be considered for the respective feature group. 
#' @param featThresholdAnn Numeric (length 1). As `featThreshold`, but only considers features with annotations.
#' @param absAlignMzDev Numeric (length 1). When the group formula annotation consensus is made from feature annotations, 
#' the \emph{m/z} values of annotated MS/MS fragments may slightly deviate from those of the corresponding group MS/MS 
#' peak list. The `absAlignMzDev` argument specifies the maximum \emph{m/z} window used to re-align the mass peaks.
#' @param MSMode Character (length 1) with the MS mode to be used. Possible values are "MS", "MSMS", or "both".
#' @param isolatePrec Settings used for isolation of precursor mass peaks and their isotopes. This isolation is highly 
#' important for accurate isotope scoring of candidates, as non-relevant mass peaks will dramatically decrease the score. 
#' The value of `isolatePrec` should either be a `list` with parameters (see the `filter` method for `MSPeakLists` for 
#' more details), `TRUE` for default parameters or `FALSE` for no isolation (e.g. when you already performed isolation 
#' with the filter method). The `z` parameter (charge) is automatically deduced from the adduct used for annotation 
#' (unless `isolatePrec` is FALSE), hence any custom `z` setting is ignored.
#' @param timeout Numeric (length 1) with the maximum time in seconds to wait for the GenForm algorithm to finish.
#' @param topMost Numeric (length 1) with the maximum number of top candidates to be returned.
#' @param batchSize Maximum number of `GenForm` commands that should be run sequentially in each parallel process.
#' 
#' @return A ProcessingSettings S3 class object with subclass Settings_generate_formulas_genform.
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#' 
#' \insertRef{genform}{StreamFind}
#'
#' @export
#'
Settings_generate_formulas_genform <- function(relMzDev = 5,
                                               elements = "CHNOP",
                                               hetero = TRUE,
                                               oc = FALSE,
                                               thrMS = NULL,
                                               thrMSMS = NULL,
                                               thrComb = NULL,
                                               maxCandidates = Inf,
                                               extraOpts = NULL,
                                               calculateFeatures = TRUE,
                                               featThreshold = 0,
                                               featThresholdAnn = 0.75,
                                               absAlignMzDev = 0.002,
                                               MSMode = "both",
                                               isolatePrec = TRUE,
                                               timeout = 120,
                                               topMost = 50,
                                               batchSize = 8) {
  
  settings <- list(
    call = "generate_formulas",
    algorithm = "genform",
    parameters = list(
      relMzDev = relMzDev,
      elements = elements,
      hetero = hetero,
      oc = oc,
      thrMS = thrMS,
      thrMSMS = thrMSMS,
      thrComb = thrComb,
      maxCandidates = maxCandidates,
      extraOpts = extraOpts,
      calculateFeatures = calculateFeatures,
      featThreshold = featThreshold,
      featThresholdAnn = featThresholdAnn,
      absAlignMzDev = absAlignMzDev,
      MSMode = MSMode,
      isolatePrec = isolatePrec,
      timeout = timeout,
      topMost = topMost,
      batchSize = batchSize
    ),
    version = as.character(packageVersion("patRoon")),
    software = "GenForm",
    developer = "Markus Meringer",
    contact = "Markus.Meringer@Uni-Bayreuth.De",
    link = "https://sourceforge.net/projects/genform/",
    doi = "MATCH Commun. Math. Comput. Chem 65.2 (2011): 259-290."
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_generate_formulas_genform <- function(x) {
  all(
    checkmate::test_choice(x$call, "generate_formulas"),
    checkmate::test_choice(x$algorithm, "genform"),
    checkmate::test_number(x$parameters$relMzDev),
    checkmate::test_character(x$parameters$elements, min.len = 1),
    checkmate::test_logical(x$parameters$hetero, len = 1),
    checkmate::test_logical(x$parameters$oc, len = 1),
    checkmate::test_numeric(x$parameters$thrMS, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$thrMSMS, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$thrComb, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$maxCandidates, len = 1),
    checkmate::test_character(x$parameters$extraOpts, null.ok = TRUE),
    checkmate::test_logical(x$parameters$calculateFeatures, len = 1),
    checkmate::test_numeric(x$parameters$featThreshold, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$featThresholdAnn, len = 1, null.ok = TRUE),
    checkmate::test_numeric(x$parameters$absAlignMzDev, len = 1, null.ok = TRUE),
    checkmate::test_choice(x$parameters$MSMode, c("MS", "MSMS", "both")),
    checkmate::test_choice(x$parameters$isolatePrec, c(TRUE, FALSE, list())),
    checkmate::test_numeric(x$parameters$timeout, len = 1),
    checkmate::test_numeric(x$parameters$topMost, len = 1),
    checkmate::test_numeric(x$parameters$batchSize, len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# generate_compounds -----
# ______________________________________________________________________________________________________________________

#' @title Settings_generate_compounds_metfrag
#'
#' @description Settings for generating compounds using \href{https://ipb-halle.github.io/MetFrag/}{MetFrag}. 
#' The algorithm is used via the function \link[patRoon]{generateCompounds} from the package \pkg{patRoon}. Therefore, 
#' it is highly recommended to check the original documentation of the function in \pkg{patRoon} for more details.
#' 
#' @param method Character (length 1) with the method to be used for MetFrag execution: "CL" for MetFragCL and "R" for MetFragR.
#' @param timeout Numeric (length 1) with the maximum time (in seconds) before a MetFrag query for a feature group is stopped.
#' @param timeoutRetries Numeric (length 1) with the maximum number of retries after reaching a timeout before completely 
#' skipping the MetFrag query for a feature group.
#' @param errorRetries Numeric (length 1) with the maximum number of retries after an error occurred.
#' @param topMost Numeric (length 1) with the maximum number of top candidates to be returned.
#' @param dbRelMzDev Numeric (length 1) with the relative mass deviation, in ppm, for the database search.
#' @param fragRelMzDev Numeric (length 1) with the relative mass deviation, in ppm, for the fragment search.
#' @param fragAbsMzDev Numeric (length 1) with the absolute mass deviation, in Da, for the fragment search.
#' @param adduct Character (length 1) with the adduct to be used for the MetFrag annotation.
#' @param database Character (length 1) with the database to be used for the MetFrag annotation. Valid values are: 
#' "pubchem", "chemspider", "for-ident", "comptox", "pubchemlite", "kegg", "sdf", "psv" and "csv".
#' @param extendedPubChem Extended PubChem database is used for the MetFrag annotation when `database` is "pubchem".
#' Valid values are: FALSE (never use it), TRUE (always use it) or "auto" (default, use if specified scorings demand it).
#' @param chemSpiderToken Character (length 1) with the ChemSpider token to be used for the MetFrag annotation when
#' `database` is "chemspider".
#' @param scoreTypes Character vector with the score types to be used for the MetFrag annotation. 
#' @param scoreWeights Numeric vector with the score weights to be used for the MetFrag annotation.
#' @param preProcessingFilters Character vector with the pre-processing filters to be used for the MetFrag annotation.
#' @param postProcessingFilters Character vector with the post-processing filters to be used for the MetFrag annotation.
#' @param maxCandidatesToStop Numeric (length 1) with the maximum number of candidates to be returned before stopping the
#' MetFrag query for a feature group.
#' @param identifiers A list containing for each feature group a character vector with database identifiers that should 
#' be used to find candidates for a feature group (the list should be named by feature group names). Can be `NULL`.
#' @param extraOpts A named list containing further settings MetFrag.
#' 
#' @details Detailed documentation can be found in \link[patRoon]{generateCompoundsMetFrag}.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_generate_compounds_metfrag.
#' 
#' @references
#' 
#' \insertRef{metfrag01}{StreamFind}
#' 
#' \insertRef{metfrag02}{StreamFind}
#' 
#' \insertRef{metfrag03}{StreamFind}
#' 
#' \insertRef{metfrag04}{StreamFind}
#' 
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @export
#'
Settings_generate_compounds_metfrag <- function(method = "CL",
                                                timeout = 300,
                                                timeoutRetries = 5,
                                                errorRetries = 5,
                                                topMost = 5,
                                                dbRelMzDev = 8,
                                                fragRelMzDev = 10,
                                                fragAbsMzDev = 0.002,
                                                adduct = NULL,
                                                database = "comptox",
                                                extendedPubChem = "auto",
                                                chemSpiderToken = "",
                                                scoreTypes = patRoon::compoundScorings("metfrag", "comptox", onlyDefault = TRUE)$name,
                                                scoreWeights = 1,
                                                preProcessingFilters = c("UnconnectedCompoundFilter", "IsotopeFilter"),
                                                postProcessingFilters = c("InChIKeyFilter"),
                                                maxCandidatesToStop = 100,
                                                identifiers = NULL,
                                                extraOpts = NULL) {
  
  settings <- list(
    call = "generate_compounds",
    algorithm = "metfrag",
    parameters = list(
      method = method,
      timeout = timeout,
      timeoutRetries = timeoutRetries,
      errorRetries = errorRetries,
      topMost = topMost,
      dbRelMzDev = dbRelMzDev,
      fragRelMzDev = fragRelMzDev,
      fragAbsMzDev = fragAbsMzDev,
      adduct = adduct,
      database = database,
      extendedPubChem = extendedPubChem,
      chemSpiderToken = chemSpiderToken,
      scoreTypes = scoreTypes,
      scoreWeights = scoreWeights,
      preProcessingFilters = preProcessingFilters,
      postProcessingFilters = postProcessingFilters,
      maxCandidatesToStop = maxCandidatesToStop,
      identifiers = identifiers,
      extraOpts = extraOpts
    ),
    version = as.character(packageVersion("patRoon")),
    software = "MetFrag",
    developer = "Christoph Ruttkies and Emma L. Schymanski",
    contact = "cruttkie@ipb-halle.de",
    link = "https://ipb-halle.github.io/MetFrag/",
    doi = "https://doi.org/10.1186/s13321-016-0115-9"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_generate_compounds_metfrag <- function(x) {
  all(
    checkmate::test_choice(x$call, "generate_compounds"),
    checkmate::test_choice(x$algorithm, "metfrag"),
    checkmate::test_choice(x$parameters$method, c("CL", "R")),
    checkmate::test_number(x$parameters$timeout),
    checkmate::test_number(x$parameters$timeoutRetries),
    checkmate::test_number(x$parameters$errorRetries),
    checkmate::test_number(x$parameters$topMost),
    checkmate::test_number(x$parameters$dbRelMzDev),
    checkmate::test_number(x$parameters$fragRelMzDev),
    checkmate::test_number(x$parameters$fragAbsMzDev),
    checkmate::test_character(x$parameters$adduct, null.ok = TRUE),
    checkmate::test_choice(x$parameters$database, c("pubchem", "chemspider", "for-ident", "comptox", "pubchemlite", "kegg", "sdf", "psv", "csv")),
    checkmate::test_choice(x$parameters$extendedPubChem, c("auto", TRUE, FALSE)),
    checkmate::test_character(x$parameters$chemSpiderToken),
    checkmate::test_character(x$parameters$scoreTypes, min.len = 1),
    checkmate::test_numeric(x$parameters$scoreWeights, min.len = 1),
    checkmate::test_character(x$parameters$preProcessingFilters, min.len = 1),
    checkmate::test_character(x$parameters$postProcessingFilters, min.len = 1),
    checkmate::test_number(x$parameters$maxCandidatesToStop),
    checkmate::test_list(x$parameters$identifiers, len = 1, null.ok = TRUE),
    checkmate::test_list(x$parameters$extraOpts, len = 1, null.ok = TRUE)
  )
}





# ______________________________________________________________________________________________________________________
# integrate_chromatograms -----
# ______________________________________________________________________________________________________________________

#' @title Settings_integrate_chromatograms_StreamFind
#'
#' @description Integrates chromatograms using the function `findpeaks` from the package \pkg{pracma} with natively 
#' added peak exclusion and evaluation steps.
#' 
#' @param merge Logical (length 1) indicating if the nearby peaks should be merged.
#' @param closeByThreshold Numeric (length 1) with the maximum distance between peaks to be merged.
#' @param minPeakHeight Numeric (length 1) with the minimum peak height to be considered.
#' @param minPeakDistance Numeric (length 1) with the minimum distance between peaks.
#' @param minPeakWidth Numeric (length 1) with the minimum peak width.
#' @param maxPeakWidth Numeric (length 1) with the maximum peak width.
#' @param minSN Numeric (length 1) with the minimum signal-to-noise ratio.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_integrate_chromatograms_StreamFind.
#'
#' @export
#'
Settings_integrate_chromatograms_StreamFind <- function(merge = TRUE,
                                                        closeByThreshold = 45,
                                                        minPeakHeight = 0,
                                                        minPeakDistance = 10,
                                                        minPeakWidth = 5,
                                                        maxPeakWidth = 120,
                                                        minSN = 10) {
  
  settings <- list(
    call = "integrate_chromatograms",
    algorithm = "StreamFind",
    parameters = list(
      merge = merge,
      closeByThreshold = closeByThreshold,
      minPeakHeight = minPeakHeight,
      minPeakDistance = minPeakDistance,
      minPeakWidth = minPeakWidth,
      maxPeakWidth = maxPeakWidth,
      minSN = minSN
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_integrate_chromatograms_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "integrate_chromatograms"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_logical(x$parameters$merge, max.len = 1),
    checkmate::test_number(x$parameters$closeByThreshold),
    checkmate::test_number(x$parameters$minPeakHeight),
    checkmate::test_number(x$parameters$minPeakDistance),
    checkmate::test_number(x$parameters$minPeakWidth),
    checkmate::test_number(x$parameters$maxPeakWidth),
    checkmate::test_number(x$parameters$minSN)
  )
}





# ______________________________________________________________________________________________________________________
# calculate_spectra_charges -----
# ______________________________________________________________________________________________________________________

#' @title Settings_calculate_spectra_charges_StreamFind
#'
#' @description Calculates spectral charges from multi-charged compounds (e.g. proteins and monoclonal antibodies) for
#' mass deconvolution.
#' 
#' @param roundVal Numeric (length 1) with the rounding value for the m/z values before applying charge clustering.
#' @param relLowCut Numeric (length 1) with the relative low cut for the charge clustering.
#' @param absLowCut Numeric (length 1) with the absolute low cut for the charge clustering.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_calculate_spectra_charges_StreamFind.
#'
#' @export
#'
Settings_calculate_spectra_charges_StreamFind <- function(roundVal = 35, relLowCut = 0.2, absLowCut = 300) {
  
  settings <- list(
    call = "calculate_spectra_charges",
    algorithm = "StreamFind",
    parameters = list(
      roundVal = roundVal,
      relLowCut = relLowCut,
      absLowCut = absLowCut
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_calculate_spectra_charges_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "calculate_spectra_charges"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$roundVal),
    checkmate::test_number(x$parameters$relLowCut),
    checkmate::test_number(x$parameters$absLowCut)
  )
}





# ______________________________________________________________________________________________________________________
# deconvolute_spectra -----
# ______________________________________________________________________________________________________________________

#' @title Settings_deconvolute_spectra_StreamFind
#'
#' @description Deconvolutes the spectral mass-to-charge ratio (\emph{m/z}) to mass (Da) after assignment of charges.
#' 
#' @param clustVal Numeric (length 1) with the clustering value for the charge deconvolution.
#' @param window Optional numeric (length 1) with the window in \emph{m/z} for collecting traces of a given charge.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_deconvolute_spectra_StreamFind.
#'
#' @export
#'
Settings_deconvolute_spectra_StreamFind <- function(clustVal = 0.1, window = 20) {
  
  settings <- list(
    call = "deconvolute_spectra",
    algorithm = "StreamFind",
    parameters = list(
      clustVal = clustVal,
      window = window
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_deconvolute_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "deconvolute_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$clustVal),
    checkmate::test_number(x$parameters$window, null.ok = TRUE)
  )
}

# ______________________________________________________________________________________________________________________
# average_spectra -----
# ______________________________________________________________________________________________________________________

#' @title Settings_average_spectra_StreamFind
#'
#' @description Averages spectra based on analysis replicate groups.
#' 
#' @param collapseTime Logical (length 1). When `TRUE` the spectra are averaged, reducing the time variable.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_average_spectra_StreamFind.
#'
#' @export
#'
Settings_average_spectra_StreamFind <- function(collapseTime = FALSE) {
  
  settings <- list(
    call = "average_spectra",
    algorithm = "StreamFind",
    parameters = list(
      collapseTime = collapseTime
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_average_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "average_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_logical(x$parameters$collapseTime, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# cluster_spectra -----
# ______________________________________________________________________________________________________________________

#' @title Settings_cluster_spectra_StreamFind
#'
#' @description Clusters spectra based on a variable (i.e. column name).
#' 
#' @param val Character (length 1) with the variable to be used for clustering.
#' @param clustVal Numeric (length 1) with the clustering value.
#' @param presence Numeric (length 1) with the minimum presence of traces in a cluster to be considered.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_cluster_spectra_StreamFind.
#'
#' @export
#'
Settings_cluster_spectra_StreamFind <- function(val = "mz", clustVal = 0.001, presence = 0.1) {
  
  settings <- list(
    call = "cluster_spectra",
    algorithm = "StreamFind",
    parameters = list(
      val = val,
      clustVal = clustVal,
      presence = presence
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_cluster_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "cluster_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_character(x$parameters$val, min.len = 1),
    checkmate::test_number(x$parameters$clustVal),
    checkmate::test_number(x$parameters$presence)
  )
}





# ______________________________________________________________________________________________________________________
# subtract_blank_spectra -----
# ______________________________________________________________________________________________________________________

#' @title Settings_subtract_blank_spectra_StreamFind
#'
#' @description Subtracts the blank spectra to each analysis according to the blank assignment.
#' 
#' @param negativeToZero Logical (length 1) indicating if negative values should be set to zero.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_subtract_blank_spectra_StreamFind.
#'
#' @export
#'
Settings_subtract_blank_spectra_StreamFind <- function(negativeToZero = FALSE) {
  
  settings <- list(
    call = "subtract_blank_spectra",
    algorithm = "StreamFind",
    parameters = list(negativeToZero = negativeToZero),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_subtract_blank_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "subtract_blank_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_logical(x$parameters$negativeToZero, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# correct_chromatograms_baseline -----
# ______________________________________________________________________________________________________________________

#' @title Settings_correct_chromatograms_baseline_baseline
#'
#' @description Performs baseline correction to chromatograms using the \pkg{baseline} package.
#' 
#' @param method Character (length 1) with the method to be used for baseline correction. Possible values are "als",
#' "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", "peakDetection", "rfbaseline", "rollingBall", "shirley" 
#' and "TAP".
#' @param args List with additional arguments for the selected method. See the documentation of the \pkg{baseline} package
#' for more details.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_correct_chromatograms_baseline_baseline.
#'
#' @export
#'
Settings_correct_chromatograms_baseline_baseline <- function(method = "als", args = list(lambda = 5, p = 0.05, maxit = 10)) {
  
  settings <- list(
    call = "correct_chromatograms_baseline",
    algorithm = "baseline",
    parameters = list(
      method = method,
      args = args
    ),
    version = as.character(packageVersion("baseline")),
    software = "baseline",
    developer = "Kristian Hovde Liland and Bjørn-Helge Mevik",
    contact = "kristian.liland@nmbu.no",
    link = "https://github.com/khliland/baseline/",
    doi = "10.1366/000370210792434350"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_correct_chromatograms_baseline_baseline <- function(x) {
  all(
    checkmate::test_choice(x$call, "correct_chromatograms_baseline"),
    checkmate::test_choice(x$algorithm, "baseline"),
    checkmate::test_choice(x$parameters$method, c("als", "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", 
                                                  "peakDetection", "rfbaseline", "rollingBall", "shirley", "TAP")),
    checkmate::test_list(x$parameters$args, len = 1)
  )
}

#' @title Settings_correct_chromatograms_baseline_airpls
#'
#' @description Performs baseline correction using adaptive iteratively reweighted Penalized Least Squares (airPLS) 
#' based on the algorithm from Zhi-Min Zhang.
#' 
#' @param lambda Numeric (length 1) with the smoothing intensity. the higher the `lambda` the higher the smoothing.
#' @param differences Integer (length 1) indicating the order of the difference of penalties
#' @param itermax Integer (length 1) with the maximum number of iterations.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_correct_chromatograms_baseline_airpls.
#' 
#' @references
#' 
#' \insertRef{airpls01}{StreamFind}
#'
#' @export
#'
Settings_correct_chromatograms_baseline_airpls <- function(lambda = 10, differences = 1, itermax = 20) {
  
  settings <- list(
    call = "correct_chromatograms_baseline",
    algorithm = "airpls",
    parameters = list(lambda = lambda, differences = differences, itermax = itermax),
    version = NA_character_,
    software = "airPLS",
    developer = "Zhi-Min Zhang",
    contact = "zmzhang@csu.edu.cn",
    link = "https://github.com/zmzhang/airPLS",
    doi = "10.1039/b922045c"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_correct_chromatograms_baseline_airpls <- function(x) {
  all(
    checkmate::test_choice(x$call, "correct_chromatograms_baseline"),
    checkmate::test_choice(x$algorithm, "airpls"),
    checkmate::test_number(x$parameters$lambda),
    checkmate::test_integer(as.integer(x$parameters$differences)),
    checkmate::test_integer(as.integer(x$parameters$itermax))
  )
}





# ______________________________________________________________________________________________________________________
# correct_spectra_baseline -----
# ______________________________________________________________________________________________________________________

#' @title Settings_correct_spectra_baseline_baseline
#'
#' @description Performs baseline correction to spectra using the \pkg{baseline} package.
#' 
#' @param method Character (length 1) with the method to be used for baseline correction. Possible values are "als",
#' "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", "peakDetection", "rfbaseline", "rollingBall", "shirley" 
#' and "TAP".
#' @param args List with additional arguments for the selected method. See the documentation of the \pkg{baseline} package
#' for more details.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_correct_spectra_baseline_baseline.
#'
#' @export
#'
Settings_correct_spectra_baseline_baseline <- function(method = "als", args = list(lambda = 5, p = 0.05, maxit = 10)) {
  
  settings <- list(
    call = "correct_spectra_baseline",
    algorithm = "baseline",
    parameters = list(method = method, args = args),
    version = as.character(packageVersion("baseline")),
    software = "baseline",
    developer = "Kristian Hovde Liland and Bjørn-Helge Mevik",
    contact = "kristian.liland@nmbu.no",
    link = "https://github.com/khliland/baseline/",
    doi = "10.1366/000370210792434350"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_correct_spectra_baseline_baseline <- function(x) {
  all(
    checkmate::test_choice(x$call, "correct_spectra_baseline"),
    checkmate::test_choice(x$algorithm, "baseline"),
    checkmate::test_choice(x$parameters$method, c("als", "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", 
                                                  "peakDetection", "rfbaseline", "rollingBall", "shirley", "TAP")),
    checkmate::test_list(x$parameters$args, len = 1)
  )
}



#' @title Settings_correct_spectra_baseline_airpls
#'
#' @description Performs baseline correction using adaptive iteratively reweighted Penalized Least Squares (airPLS) 
#' based on the algorithm from Zhi-Min Zhang.
#' 
#' @param lambda Numeric (length 1) with the smoothing intensity. the higher the `lambda` the higher the smoothing.
#' @param differences Integer (length 1) indicating the order of the difference of penalties
#' @param itermax Integer (length 1) with the maximum number of iterations.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_correct_spectra_baseline_airpls.
#' 
#' @references
#' 
#' \insertRef{airpls01}{StreamFind}
#'
#' @export
#'
Settings_correct_spectra_baseline_airpls <- function(lambda = 10, differences = 1, itermax = 20) {
  
  settings <- list(
    call = "correct_spectra_baseline",
    algorithm = "airpls",
    parameters = list(
      lambda = lambda, differences = differences, itermax = itermax
    ),
    version = NA_character_,
    software = "airPLS",
    developer = "Zhi-Min Zhang",
    contact = "zmzhang@csu.edu.cn",
    link = "https://github.com/zmzhang/airPLS",
    doi = "10.1039/b922045c"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_correct_spectra_baseline_airpls <- function(x) {
  all(
    checkmate::test_choice(x$call, "correct_spectra_baseline"),
    checkmate::test_choice(x$algorithm, "airpls"),
    checkmate::test_number(x$parameters$lambda),
    checkmate::test_integer(as.integer(x$parameters$differences)),
    checkmate::test_integer(as.integer(x$parameters$itermax))
  )
}





# ______________________________________________________________________________________________________________________
# smooth_chromatograms -----
# ______________________________________________________________________________________________________________________

#' @title Settings_smooth_chromatograms_movingaverage
#'
#' @description Smooths chromatograms using the moving average algorithm.
#' 
#' @param windowSize Numeric (length 1) with the window size for the moving average.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_smooth_chromatograms_movingaverage.
#'
#' @export
#'
Settings_smooth_chromatograms_movingaverage <- function(windowSize = 5, xValWindow = NULL) {
  
  settings <- list(
    call = "smooth_chromatograms",
    algorithm = "movingaverage",
    parameters = list(windowSize = windowSize),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_smooth_chromatograms_movingaverage <- function(x) {
  all(
    checkmate::test_choice(x$call, "smooth_chromatograms"),
    checkmate::test_choice(x$algorithm, "movingaverage"),
    checkmate::test_number(x$parameters$windowSize)
  )
}



#' @title Settings_smooth_chromatograms_savgol
#'
#' @description Smooths chromatograms using the Savitzky-Golay algorithm from the \pkg{pracma} package.
#' 
#' @param fl Numeric (length 1) with the filter length (for instance fl = 51..151), has to be odd.
#' @param forder Numeric (length 1) with the order of the filter (2 = quadratic filter, 4 = quartic).
#' @param dorder Numeric (length 1) with the order of the derivative (0 = smoothing, 1 = first derivative, etc.).
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_smooth_chromatograms_savgol.
#'
#' @export
#'
Settings_smooth_chromatograms_savgol <- function(fl = 11, forder = 4, dorder = 0) {
  
  settings <- list(
    call = "smooth_chromatograms",
    algorithm = "savgol",
    parameters = list(
      fl = fl,
      forder = forder,
      dorder = dorder
    ),
    version = as.character(packageVersion("pracma")),
    software = "pracma",
    developer = "Hans W. Borchers",
    contact = NA_character_,
    link = "https://cran.r-project.org/web/packages/pracma/index.html",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_smooth_chromatograms_savgol <- function(x) {
  all(
    checkmate::test_choice(x$call, "smooth_chromatograms"),
    checkmate::test_choice(x$algorithm, "savgol"),
    checkmate::test_number(x$parameters$fl),
    checkmate::test_number(x$parameters$forder),
    checkmate::test_number(x$parameters$dorder)
  )
}

# ______________________________________________________________________________________________________________________
# smooth_spectra -----
# ______________________________________________________________________________________________________________________

#' @title Settings_smooth_spectra_movingaverage
#'
#' @description Smooths spectra using the moving average algorithm.
#' 
#' @param windowSize Numeric (length 1) with the window size for the moving average. 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_smooth_spectra_movingaverage.
#'
#' @export
#'
Settings_smooth_spectra_movingaverage <- function(windowSize = 5, xValWindow = NULL) {
  
  settings <- list(
    call = "smooth_spectra",
    algorithm = "movingaverage",
    parameters = list(windowSize = windowSize),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_smooth_spectra_movingaverage <- function(x) {
  all(
    checkmate::test_choice(x$call, "smooth_spectra"),
    checkmate::test_choice(x$algorithm, "movingaverage"),
    checkmate::test_number(x$parameters$windowSize)
  )
}

#' @title Settings_smooth_spectra_savgol
#'
#' @description Smooths spectra using the Savitzky-Golay algorithm from the \pkg{pracma} package.
#' 
#' @param fl Numeric (length 1) with the filter length (for instance fl = 51..151), has to be odd.
#' @param forder Numeric (length 1) with the order of the filter (2 = quadratic filter, 4 = quartic).
#' @param dorder Numeric (length 1) with the order of the derivative (0 = smoothing, 1 = first derivative, etc.).
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_smooth_spectra_savgol.
#'
#' @export
#'
Settings_smooth_spectra_savgol <- function(fl = 11, forder = 4, dorder = 0) {
  
  settings <- list(
    call = "smooth_spectra",
    algorithm = "savgol",
    parameters = list(
      fl = fl,
      forder = forder,
      dorder = dorder
    ),
    version = as.character(packageVersion("pracma")),
    software = "pracma",
    developer = "Hans W. Borchers",
    contact = NA_character_,
    link = "https://cran.r-project.org/web/packages/pracma/index.html",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_smooth_spectra_savgol <- function(x) {
  all(
    checkmate::test_choice(x$call, "smooth_spectra"),
    checkmate::test_choice(x$algorithm, "savgol"),
    checkmate::test_number(x$parameters$fl),
    checkmate::test_number(x$parameters$forder),
    checkmate::test_number(x$parameters$dorder)
  )
}





# ______________________________________________________________________________________________________________________
# normalize_spectra -----
# ______________________________________________________________________________________________________________________

#' @title Settings_normalize_spectra_minmax
#'
#' @description Normalizes spectra using the min-max algorithm.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_normalize_spectra_minmax.
#'
#' @export
#'
Settings_normalize_spectra_minmax <- function() {
  
  settings <- list(
    call = "normalize_spectra",
    algorithm = "minmax",
    parameters = list(),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_normalize_spectra_minmax <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "minmax")
  )
}



#' @title Settings_normalize_spectra_snv
#'
#' @description Normalizes spectra using the Standard Normal Variate (SNV) algorithm.
#' 
#' @param liftTozero Logical (length 1) indicating if the spectra should be lifted to zero.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_normalize_spectra_snv.
#'
#' @export
#'
Settings_normalize_spectra_snv <- function(liftTozero = FALSE) {
  
  settings <- list(
    call = "normalize_spectra",
    algorithm = "snv",
    parameters = list(liftTozero = liftTozero),
    version = NA_character_,
    software = NA_character_,
    developer = "Jürgen Schram",
    contact = "schram@hsnr.de",
    link = NA_character_,
    doi = "10.1016/j.trac.2018.12.004"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_normalize_spectra_snv <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "snv"),
    checkmate::test_logical(x$parameters$liftTozero, max.len = 1)
  )
}



#' @title Settings_normalize_spectra_scale
#'
#' @description Normalizes spectra using scaling based on the standard deviation.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_normalize_spectra_scale.
#'
#' @export
#'
Settings_normalize_spectra_scale <- function() {
  
  settings <- list(
    call = "normalize_spectra",
    algorithm = "scale",
    parameters = list(),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_normalize_spectra_scale <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "scale")
  )
}



#' @title Settings_normalize_spectra_blockweight
#'
#' @description Normalizes spectra using block weighting for downstream data evaluation.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_normalize_spectra_blockweight.
#'
#' @export
#'
Settings_normalize_spectra_blockweight <- function() {
  
  settings <- list(
    call = "normalize_spectra",
    algorithm = "blockweight",
    parameters = list(),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_normalize_spectra_blockweight <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "blockweight")
  )
}



#' @title Settings_normalize_spectra_meancenter
#'
#' @description Normalizes spectra using mean centering.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_normalize_spectra_meancenter.
#'
#' @export
#'
Settings_normalize_spectra_meancenter <- function() {
  
  settings <- list(
    call = "normalize_spectra",
    algorithm = "meancenter",
    parameters = list(),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_normalize_spectra_meancenter <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "meancenter")
  )
}





# ______________________________________________________________________________________________________________________
# subtract_spectra_section -----
# ______________________________________________________________________________________________________________________

#' @title Settings_subtract_spectra_section_StreamFind
#'
#' @description Subtracts a section of the spectra based on a variable (i.e. column name).
#' 
#' @param sectionVal Character (length 1) with the variable to be used for sectioning.
#' @param sectionWindow Numeric (length 2) with the window for the sectioning.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_subtract_spectra_section_StreamFind.
#'
#' @export
#'
Settings_subtract_spectra_section_StreamFind <- function(sectionVal = "rt", sectionWindow = c(10, 200)) {
  
  settings <- list(
    call = "subtract_spectra_section",
    algorithm = "StreamFind",
    parameters = list(
      sectionVal = sectionVal,
      sectionWindow = sectionWindow
    ),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_subtract_spectra_section_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "subtract_spectra_section"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_character(x$parameters$sectionVal, min.len = 1),
    checkmate::test_numeric(x$parameters$sectionWindow, len = 2)
  )
}





# ______________________________________________________________________________________________________________________
# delete_spectra_section -----
# ______________________________________________________________________________________________________________________

#' @title Settings_delete_spectra_section_StreamFind
#'
#' @description Deletes a section of the spectra based on a named list of data ranges for a given variable (i.e. column name).
#' 
#' @param section Named list with the variable to be used for sectioning and the window for the sectioning. The names 
#' should match column names in the data.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_delete_spectra_section_StreamFind.
#'
#' @export
#'
Settings_delete_spectra_section_StreamFind <- function(section = list()) {
  
  settings <- list(
    call = "delete_spectra_section",
    algorithm = "StreamFind",
    parameters = list(section = section),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_delete_spectra_section_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "delete_spectra_section"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_named_list(x$parameters$section)
  )
}





# ______________________________________________________________________________________________________________________
# merge_spectra_time_series -----
# ______________________________________________________________________________________________________________________

#' @title Settings_merge_spectra_time_series_StreamFind
#'
#' @description Merges Raman spectra based on time series data. It collapses data files into a single file.
#' 
#' @param preCut The number of pre Raman scans to exclude when merging.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_merge_spectra_time_series_StreamFind.
#'
#' @export
#'
Settings_merge_spectra_time_series_StreamFind <- function(preCut = 2) {
  
  settings <- list(
    call = "merge_spectra_time_series",
    algorithm = "StreamFind",
    parameters = list(preCut = preCut),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.Settings_merge_spectra_time_series_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "merge_spectra_time_series"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$preCut)
  )
}
