
# Default Settings -----

#' @title get_default_ProcessingSettings
#'
#' @description Saves on disk a ProcessingSettings S3 class object as the
#' defined `format`, `path` and `name`.
#'
#' @param call Character (length 1) with the method call name.
#' @param algorithm Character (length 1) with the algorithm name.
#'
#' @return A ProcessingSettings S3 class object with subclass as defined by
#' `call` and `algorithm`.
#'
#' @export
#'
get_default_ProcessingSettings <- function(call = NA_character_,
                                           algorithm = NA_character_) {

  class_string <- paste0("Settings_", call, "_", algorithm)

  do.call(class_string, list())
}

#' @title save_default_ProcessingSettings
#'
#' @description Saves on disk a ProcessingSettings S3 class object as the
#' defined `format`, `path` and `name`.
#'
#' @param call Character (length 1) with the method call name.
#' @param algorithm Character (length 1) with the algorithm name.
#' @param format Character (length 1) with the format of the saved file.
#' Possible are "json" and "rds".
#' @param name Character (length 1) with the name of the file without extension.
#' @param path Character (length 1) with the saving path.
#' The default is the `getwd()` path.
#'
#' @return Creates a json/rds files on the defined path.
#'
#' @export
#'
save_default_ProcessingSettings <- function(call = NA_character_,
                                            algorithm = NA_character_,
                                            format = "json",
                                            name = "settings",
                                            path = getwd()) {

  settings <- get_default_ProcessingSettings(call, algorithm)

  if (format %in% "json") {
    settings_js <- toJSON(
      settings,
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
    write(settings_js, file = paste0(path, "/", name, ".json"))
  }

  if (format %in% "rds") {
    saveRDS(settings, file = paste0(path, "/", name, ".rds"))
  }
}

# centroid_spectra -----

#' @title Settings_centroid_spectra_qCentroids
#'
#' @description Centroids profile spectra using the
#' \href{https://link.springer.com/article/10.1007/s00216-022-04224-y}{qCentroids}
#' algorithm, which is part of the
#' \href{https://github.com/odea-project/qAlgorithms}{qAlgorithms} library.
#'
#' @param maxScale Integer of length one. Maximum scale as integer (default is 
#' 5) for defining the scale limit for the peak model.
#' @param mode Integer of length one. `0` for debugging, `1` for silent mode 
#' (the default) and `2` for progressbar mode.
#' @template arg-runParallel
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_centroid_spectra_qCentroids.
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
      mode = mode,
      runParallel = runParallel
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

#' @describeIn Settings_centroid_spectra_qCentroids
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_centroid_spectra_qCentroids S3 class object.
#'
#' @export
#'
validate.Settings_centroid_spectra_qCentroids <- function(x) {
  all(
    checkmate::test_choice(x$call, "centroid_spectra"),
    checkmate::test_choice(x$algorithm, "qCentroids"),
    checkmate::test_int(x$parameters$maxScale),
    checkmate::test_int(x$parameters$mode),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1)
  )
}

# bin_spectra -----

#' @title Settings_bin_spectra_qBinning
#'
#' @description Not yet implemented.
#'
#' @return X.
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

#' @describeIn Settings_bin_spectra_qBinning
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_bin_spectra_qBinning S3 class object.
#'
#' @export
#'
validate.Settings_bin_spectra_qBinning <- function(x) {
  all(
    checkmate::test_choice(x$call, "bin_spectra"),
    checkmate::test_choice(x$algorithm, "qBinning")
  )
}

#' @title Settings_bin_spectra_StreamFind
#'
#' @description Not yet implemented.
#' 
#' @param unitsVal 
#' @param unitsNumber 
#' @param bins 
#'
#' @return X.
#'
#' @export
#'
Settings_bin_spectra_StreamFind <- function(unitsVal = NULL,
                                            unitsNumber = NULL,
                                            bins = NULL) {
  
  settings <- list(
    call = "bin_spectra",
    algorithm = "StreamFind",
    parameters = list(
      unitsVal = unitsVal,
      unitsNumber = unitsNumber,
      bins = bins
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

#' @describeIn Settings_bin_spectra_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_bin_spectra_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_bin_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "bin_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# find_features -----

#' @title Settings_find_features_qPeaks
#'
#' @description Not yet implemented.
#'
#' @return X.
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

#' @describeIn Settings_find_features_qPeaks
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_find_features_qPeaks S3 class object.
#'
#' @export
#'
validate.Settings_find_features_qPeaks <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "qPeaks")
  )
}

#' @title Settings_find_features_xcms3_centwave
#'
#' @description Settings for finding features (i.e., chromatographic peaks)
#' in mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
#' (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/findChromPeaks-centWave.html}{centWave}.
#' The function uses the package \pkg{patRoon} in the background.
#'
#' @param ppm numeric(1) defining the maximal tolerated m/z deviation in
#' consecutive scans in parts per million (ppm) for the initial ROI definition.
#' @param peakwidth numeric(2) with the expected approximate feature width in
#' chromatographic space. Given as a range (min, max) in seconds.
#' @param snthresh numeric(1) defining the signal to noise ratio cutoff.
#' @param prefilter numeric(2): c(k, I) specifying the prefilter step for the
#' first analysis step (ROI detection). Mass traces are only retained if they
#' contain at least k peaks with intensity >= I.
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
#' @details See the \link[patRoon]{findFeaturesXCMS3} function from the
#' \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_find_features_xcms3_centwave.
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
Settings_find_features_xcms3_centwave <- function(
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

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_find_features_xcms3_centwave
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_find_features_xcms3_centwave S3 class object.
#'
#' @export
#'
validate.Settings_find_features_xcms3_centwave <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "xcms3_centwave")
  )
}

#' @title Settings_find_features_xcms3_matchedfilter
#'
#' @description Settings for finding features (i.e., chromatographic peaks)
#' in mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
#' (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/findChromPeaks-Chromatogram-MatchedFilter.html}{MatchedFilter},
#' which is optimal/preferred for low resolution LC-MS data.
#' The function uses the package \pkg{patRoon} in the background.
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
#' @param index logical(1) specifying whether indicies should be returned
#' instead of values for m/z and retention times.
#'
#' @details See the \link[patRoon]{findFeaturesXCMS3} function from the
#' \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_find_features_xcms3_matchedfilter.
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
Settings_find_features_xcms3_matchedfilter <- function(
    binSize = 0.5,
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

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_find_features_xcms3_matchedfilter
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_find_features_xcms3_matchedfilter S3 class object.
#'
#' @export
#'
validate.Settings_find_features_xcms3_matchedfilter <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "xcms3_matchedfilter")
  )
}

#' @title Settings_find_features_openms
#'
#' @description Settings for finding features (i.e., chromatographic peaks)
#' in mzML/mzXML files using the \href{https://www.openms.org/}{OpenMS}
#' (\url{https://abibuilder.cs.uni-tuebingen.de/archive/openms/}) software
#' with the algorithm
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
#' @param verbose Logical of length one. When TRUE adds processing information
#' to the console.
#'
#' @details See the \link[patRoon]{findFeaturesOpenMS} function from the
#' \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_find_features_openms.
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
Settings_find_features_openms <- function(
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

#' @describeIn Settings_find_features_openms
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_find_features_openms S3 class object.
#'
#' @export
#'
validate.Settings_find_features_openms <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "openms")
  )
}

#' @title Settings_find_features_kpic2
#'
#' @description Settings for finding features (i.e., chromatographic peaks)
#' in mzML/mzXML files using the package \href{https://github.com/hcji/KPIC2}{KPIC}.
#' The function uses the package \pkg{patRoon} in the background.
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
#' @details See the \link[patRoon]{findFeaturesKPIC2} function from the
#' \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_find_features_kpic2.
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
Settings_find_features_kpic2 <- function(
    level = 500,
    mztol = 0.01,
    gap = 2,
    width = 5,
    min_snr = 4,
    kmeans = TRUE,
    alpha = 0.3) {

  if (!requireNamespace("KPIC", quietly = TRUE)) {
    warning("KPIC package required but not installed!")
    return(NULL)
  }

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

  if (kmeans) {
    settings$parameters$alpha <- alpha
  }

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_find_features_kpic2
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_find_features_kpic2 S3 class object.
#'
#' @export
#'
validate.Settings_find_features_kpic2 <- function(x) {
  all(
    checkmate::test_choice(x$call, "find_features"),
    checkmate::test_choice(x$algorithm, "kpic2")
  )
}

# group_features -----

#' @title Settings_group_features_xcms3_peakdensity
#'
#' @description Settings for grouping features (i.e., chromatographic peaks)
#' across mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
#' (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/groupChromPeaks-density.html}{peakDensity}.
#' The function uses the package \pkg{patRoon} in the background.
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
#' @param maxFeatures numeric(1) with the maximum number of feature groups to be
#' identified in a single mz slice.
#'
#' @details See the \link[patRoon]{groupFeaturesXCMS3} function from the
#' \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_group_features_xcms3_peakdensity.
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
Settings_group_features_xcms3_peakdensity <- function(
    bw = 5,
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

#' @describeIn Settings_group_features_xcms3_peakdensity
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_group_features_xcms3_peakdensity S3 class object.
#'
#' @export
#'
validate.Settings_group_features_xcms3_peakdensity <- function(x) {
  all(
    checkmate::test_choice(x$call, "group_features"),
    checkmate::test_choice(x$algorithm, "xcms3_peakdensity")
  )
}

#' @title Settings_group_features_xcms3_peakdensity_peakgroups
#'
#' @description Settings for aligning and grouping features (i.e.,
#' chromatographic peaks) across mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
#' (version 3) with the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/adjustRtime-peakGroups.html}{peakGroups}
#' for retention time alignment and the algorithm
#' \href{https://rdrr.io/bioc/xcms/man/groupChromPeaks-density.html}{peakdensity}
#' for grouping. The function uses the package \pkg{patRoon} in the background.
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
#' @details See the \link[patRoon]{groupFeaturesXCMS3} function from the
#' \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_group_features_xcms3_peakdensity_peakgroups.
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
Settings_group_features_xcms3_peakdensity_peakgroups <- function(
    bw = 5,
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

#' @describeIn Settings_group_features_xcms3_peakdensity_peakgroups
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_group_features_xcms3_peakdensity_peakgroups S3 class object.
#'
#' @export
#'
validate.Settings_group_features_xcms3_peakdensity_peakgroups <- function(x) {
  all(
    checkmate::test_choice(x$call, "group_features"),
    checkmate::test_choice(x$algorithm, "xcms3_peakdensity_peakgroups")
  )
}

#' @title Settings_group_features_openms
#'
#' @description Settings for grouping features (i.e., chromatographic peaks)
#' in mzML/mzXML files using the \href{https://www.openms.org/}{OpenMS}
#' (\url{https://abibuilder.cs.uni-tuebingen.de/archive/openms/}) software
#' with the algorithm
#' \href{https://abibuilder.cs.uni-tuebingen.de/archive/openms/Documentation/release/3.0.0/html/TOPP_FeatureLinkerUnlabeled.html}{FeatureLinkerUnlabeled}.
#' The function uses the package \pkg{patRoon} in the background.
#'
#' @param rtalign Logical length one. Set to TRUE to enable retention time
#' alignment.
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
#' @details See the \link[patRoon]{groupFeaturesOpenMS} function from the
#' \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_group_features_openms.
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
Settings_group_features_openms <- function(
    rtalign = FALSE,
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

#' @describeIn Settings_group_features_openms
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_group_features_openms S3 class object.
#'
#' @export
#'
validate.Settings_group_features_openms <- function(x) {
  all(
    checkmate::test_choice(x$call, "group_features"),
    checkmate::test_choice(x$algorithm, "openms")
  )
}

# load_features -----

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
#' @template arg-runParallel
#' @template arg-verbose
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_load_features_ms1_StreamFind.
#'
#' @export
#'
Settings_load_features_ms1_StreamFind <- function(
    rtWindow = c(-2, 2),
    mzWindow = c(-1, 6),
    mzClust = 0.005,
    presence = 0.8,
    minIntensity = 250,
    filtered = FALSE,
    runParallel = TRUE,
    verbose = FALSE) {

  settings <- list(
    call = "load_features_ms1",
    algorithm = "StreamFind",
    parameters = list(
      "rtWindow" = rtWindow,
      "mzWindow" = mzWindow,
      "mzClust" = mzClust,
      "presence" = presence,
      "minIntensity" = minIntensity,
      "filtered" = filtered,
      "runParallel" = runParallel,
      "verbose" = verbose
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

#' @describeIn Settings_load_features_ms1_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_load_features_ms1_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_load_features_ms1_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_features_ms1"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_double(as.numeric(x$parameters$rtWindow), max.len = 2),
    checkmate::test_double(as.numeric(x$parameters$mzWindow), max.len = 2),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
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
#' @template arg-runParallel
#' @template arg-verbose
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_load_features_ms2_StreamFind.
#'
#' @export
#'
Settings_load_features_ms2_StreamFind <- function(
    isolationWindow = 1.3,
    mzClust = 0.005,
    presence = 0.8,
    minIntensity = 10,
    filtered = FALSE,
    runParallel = TRUE,
    verbose = FALSE) {

  settings <- list(
    call = "load_features_ms2",
    algorithm = "StreamFind",
    parameters = list(
      "isolationWindow" = isolationWindow,
      "mzClust" = mzClust,
      "presence" = presence,
      "minIntensity" = minIntensity,
      "filtered" = filtered,
      "runParallel" = runParallel,
      "verbose" = verbose
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

#' @describeIn Settings_load_features_ms2_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_load_features_ms2_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_load_features_ms2_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_features_ms2"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$isolationWindow),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

#' @title Settings_load_features_eic_StreamFind
#'
#' @description Settings for loading MS2 spectra for feature groups.
#'
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @template arg-runParallel
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_load_features_eic_StreamFind.
#'
#' @export
#'
Settings_load_features_eic_StreamFind <- function(
    rtExpand = 120,
    mzExpand = 0,
    filtered = FALSE,
    runParallel = TRUE) {

  settings <- list(
    call = "load_features_eic",
    algorithm = "StreamFind",
    parameters = list(
      "rtExpand" = rtExpand,
      "mzExpand" = mzExpand,
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

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_load_features_eic_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_load_features_eic_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_load_features_eic_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_features_eic"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$rtExpand),
    checkmate::test_number(x$parameters$mzExpand),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1)
  )
}

# load_MSPeakLists -----

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
Settings_load_MSPeakLists_patRoon <- function(
    maxMSRtWindow = 5,
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
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @describeIn Settings_load_MSPeakLists_patRoon
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_load_MSPeakLists_patRoon S3 class object.
#'
#' @export
#'
validate.Settings_load_MSPeakLists_patRoon <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_MSPeakLists"),
    checkmate::test_choice(x$algorithm, "patRoon")
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
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @describeIn Settings_load_MSPeakLists_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_load_MSPeakLists_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_load_MSPeakLists_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_MSPeakLists"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# filter_features -----

#' @title Settings_filter_features_StreamFind
#'
#' @description Settings for filtering of features and feature groups.
#'
#' @param ... Ordered filters to be applied. Possible filter arguments are:
#' \itemize{
#'  \item{**minIntensity**}{  Numeric (length 1) with the minimum intensity.}
#'  \item{**minSnRatio**}{  Numeric (length 1) with the minimum signal-to-noise
#'  ratio.}
#'  \item{**maxGroupSd**}{  Numeric (length 1) with the maximum intensity
#'  deviation within each analysis replicate (in percentage).}
#'  \item{**blank**}{  Numeric (length 1) with the intensity threshold for blank
#'  subtraction. All features/feature groups not higher then the `blank` * its
#'  intensity are filtered.}
#'  \item{**minGroupAbundance**}{  Numeric (length 1) with the minimum presence
#'  of a feature is a given analysis replicate.}
#'  \item{**excludeIsotopes**}{  Logical (length 1) with `TRUE` for filtering
#'  annotated isotopes (only prevails the monoisotopic features).}
#'  \item{**rtFilter**}{  Numeric (length 2) with the min and max retention time
#'  (in seconds) values to filter features. Features within the retention time
#'  range are filtered out.}
#'  \item{**massFilter**}{  Numeric (length 2) with the min and max mass
#'  (in Da) values to filter features. Features within the mass range are
#'  filtered out.}
#'   \item{**onlySuspects**}{  Logical (length 1) with `TRUE` for only keeping 
#'   features annotated with a suspect.}
#' }
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_filter_features_StreamFind.
#'
#' @details
#' When feature groups exist the filtering is done on feature groups, meaning
#' that the filters are applied per feature group not individual features. For
#' instance, if feature groups exist the minimum intensity is applied on the
#' maximum intensity observed for a feature group and not the intensity of each
#' individual feature.
#'
#' @export
#'
Settings_filter_features_StreamFind <- function(...) {

  dots <- list(...)

  settings <- list(
    call = "filter_features",
    algorithm = "StreamFind",
    parameters = list(),
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )

  possible_feature_filters <- c(
    "minIntensity",
    "minSnRatio",
    "blank",
    "maxGroupSd",
    "minGroupAbundance",
    "excludeIsotopes",
    "excludeAdducts",
    "rtFilter",
    "massFilter",
    "onlySuspects"
  )

  if (!all(names(dots) %in% possible_feature_filters)) {
    warning("Added filter arguments not recognized!")
  }

  if ("minIntensity" %in% names(dots)) {
    checkmate::assert_number(dots[["minIntensity"]])
  }

  if ("minSnRatio" %in% names(dots)) {
    checkmate::assert_number(dots[["minSnRatio"]])
  }

  if ("maxGroupSd" %in% names(dots)) {
    checkmate::assert_number(dots[["maxGroupSd"]])
  }

  if ("blank" %in% names(dots)) {
    checkmate::assert_number(dots[["blank"]])
  }

  if ("minGroupAbundance" %in% names(dots)) {
    checkmate::assert_count(dots[["minGroupAbundance"]])
  }

  if ("excludeIsotopes" %in% names(dots)) {
    checkmate::assert_logical(dots[["excludeIsotopes"]], max.len = 1)
  }

  if ("rtFilter" %in% names(dots)) {
    checkmate::assert_double(as.numeric(dots[["rtFilter"]]), len = 2)
  }

  if ("massFilter" %in% names(dots)) {
    checkmate::assert_double(as.numeric(dots[["massFilter"]]), len = 2)
  }
  
  if ("onlySuspects" %in% names(dots)) {
    checkmate::assert_logical(dots[["onlySuspects"]], max.len = 1)
  }

  settings$parameters <- dots

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_filter_features_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_filter_features_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_filter_features_StreamFind <- function(x) {

  filters <- names(x$parameters)

  all(
    checkmate::test_choice(x$call, "filter_features"),
    checkmate::test_choice(x$algorithm, "StreamFind"),

    if ("minIntensity" %in% filters) {
      checkmate::test_number(x$parameters$minIntensity)
    } else {
      TRUE
    },

    if ("minSnRatio" %in% filters) {
      checkmate::test_number(x$parameters$minSnRatio)
    } else {
      TRUE
    },

    if ("maxGroupSd" %in% filters) {
      checkmate::test_number(x$parameters$maxGroupSd)
    } else {
      TRUE
    },

    if ("blank" %in% filters) {
      checkmate::test_number(x$parameters$blank)
    } else {
      TRUE
    },

    if ("minGroupAbundance" %in% filters) {
      checkmate::test_count(x$parameters$minGroupAbundance)
    } else {
      TRUE
    },

    if ("excludeIsotopes" %in% filters) {
      checkmate::test_logical(x$parameters$excludeIsotopes, max.len = 1)
    } else {
      TRUE
    },

    if ("rtFilter" %in% filters) {
      checkmate::test_double(as.numeric(x$parameters$rtFilter), len = 2)
    } else {
      TRUE
    },

    if ("massFilter" %in% filters) {
      checkmate::test_double(as.numeric(x$parameters$massFilter), len = 2)
    } else {
      TRUE
    },
    
    if ("onlySuspects" %in% filters) {
      checkmate::test_logical(x$parameters$onlySuspects, max.len = 1)
    } else {
      TRUE
    }
  )
}

#' @title Settings_filter_features_patRoon
#'
#' @description Settings for filtering of features and feature groups. A full 
#' description of the filtering parameters is described in 
#' \code{\link[patRoon]{replicateGroupSubtract}} from patRoon package.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_filter_features_patRoon.
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
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @describeIn Settings_filter_features_patRoon
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_filter_features_patRoon S3 class object.
#'
#' @export
#'
validate.Settings_filter_features_patRoon <- function(x) {
  
  # filters <- names(x$parameters)
  
  all(
    checkmate::test_choice(x$call, "filter_features"),
    checkmate::test_choice(x$algorithm, "patRoon"),
  )
}

# annotate_features -----

#' @title Settings_annotate_features_StreamFind
#'
#' @description Settings for annotation of isotopic features. The method uses
#' the `maxIsotopes` to define the maximum length of the isotopic chain.
#' The list of candidate features is build with the `rtWindowAlignment` and the
#' maximum mass increment to match the maximum chain length. Then, the mass
#' difference  of the natural isotopes defined by `elements` and a given
#' monoisotopic ion (i.e., feature) are targeted. Each candidate is then
#' evaluated according to the mass error and the expected relative intensity
#' range as defined by the `mode`.
#'
#' @param maxIsotopes Numeric (length 1) with the maximum number of isotopic steps.
#' @param elements Character vector with the elements to target the isotopic annotation.
#' Possible elements are C, H, N, O, S, Cl, Br.
#' @param mode Character (length 1) with the type of molecules to be targeted.
#' For now, only "small molecules" are possible.
#' @param maxCharge Numeric (length 1) with the maximum charge that ions can be
#' ionized to find isotopes.
#' @param rtWindowAlignment Numeric (length 1) with the proportion of the
#' monoisotopic feature time window to be used for retrieving isotopic candidates.
#' @param maxGaps Numeric (length 1) with the maximum of allowed gaps in isotopic chains.
#' @template arg-runParallel
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_annotate_features_StreamFind.
#'
#' @export
#'
Settings_annotate_features_StreamFind <- function(
    maxIsotopes = 5,
    elements = c("C", "H", "N", "O", "S", "Cl", "Br"),
    mode = "small molecules",
    maxCharge = 1,
    rtWindowAlignment = 0.3,
    maxGaps = 1,
    runParallel = FALSE) {

  checkmate::assert_count(maxIsotopes)
  checkmate::assert_count(maxCharge)
  checkmate::assert_count(maxGaps)
  checkmate::assert_number(rtWindowAlignment)
  checkmate::assert_choice(mode, "small molecules")
  checkmate::assert_vector(elements, any.missing = FALSE, min.len = 1)
  lapply(elements, function(i) checkmate::assert_choice(i, c("C", "H", "N", "O", "S", "Cl", "Br", "Si", "Ge")))
  checkmate::assert_logical(runParallel, max.len = 1)

  settings <- list(
    call = "annotate_features",
    algorithm = "StreamFind",
    parameters = list(
      "maxIsotopes" = maxIsotopes,
      "elements" = elements,
      "mode" = mode,
      "maxCharge" = maxCharge,
      "rtWindowAlignment" = rtWindowAlignment,
      "maxGaps" = maxGaps,
      "runParallel" = runParallel
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

#' @describeIn Settings_annotate_features_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_annotate_features_StreamFind S3 class object.
#'
#' @export
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

# suspect_screening -----

#' @title Settings_suspect_screening_StreamFind
#'
#' @description
#' Settings for performing suspect screening using a data.frame with target
#' compounds.
#'
#' @param database A data.frame with at least the columns name and mass,
#' indicating the name and neutral monoisotopic mass of the suspect targets.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-ppmMS2
#' @template arg-ms-minFragments
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-runParallel
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_suspect_screening_StreamFind.
#'
#' @export
#'
Settings_suspect_screening_StreamFind <- function(
    database = NULL,
    ppm = 5,
    sec = 10,
    ppmMS2 = 10,
    minFragments = 3,
    isolationWindow = 1.3,
    mzClust = 0.003,
    presence = 0.8,
    minIntensity = 0,
    runParallel = FALSE,
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
      "runParallel" = runParallel,
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

#' @describeIn Settings_suspect_screening_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_suspect_screening_StreamFind S3 class object.
#'
#' @export
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
#' @description
#' Settings for performing suspect screening using the
#' \href{https://water.for-ident.org/}{FOR-IDENT} platform.
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
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_suspect_screening_forident.
#'
#' @export
#'
Settings_suspect_screening_forident <- function(
    addMS2 = FALSE,
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

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_suspect_screening_forident
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_suspect_screening_forident S3 class object.
#'
#' @export
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
#' @description
#' Settings for performing suspect screening using the function 
#' \link[patRoon]{screenSuspects} from the patRoon R package.
#'
#' @param suspects A data.frame with suspect information. See section Suspect
#' list format in \link[patRoon]{screenSuspects} for more information.
#' @param rtWindow The retention time window (in seconds) that 
#' will be used for matching a suspect (+/- feature data).
#' @param mzWindow The m/z window that will be used for matching a suspect 
#' (+/- feature data)..
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_suspect_screening_patRoon.
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @export
#'
Settings_suspect_screening_patRoon <- function(
    suspects = NULL,
    rtWindow = 12,
    mzWindow = 0.005,
    filtered = FALSE) {
  
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
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @describeIn Settings_suspect_screening_patRoon
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_suspect_screening_patRoon S3 class object.
#'
#' @export
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

# find_internal_standards -----

#' @title Settings_find_internal_standards_StreamFind
#'
#' @description
#' Settings for finding internal standards using a data.frame.
#'
#' @param database A data.frame with at least the columns name, mass, and rt
#' indicating the name, neutral monoisotopic mass and retention time of the
#' internal standards, respectively.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_find_internal_standards_StreamFind.
#'
#' @export
#'
Settings_find_internal_standards_StreamFind <- function(
    database = NULL,
    ppm = 5,
    sec = 10) {

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

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_find_internal_standards_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_find_internal_standards_StreamFind S3 class object.
#'
#' @export
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

# calculate_quality -----

#' @title Settings_calculate_quality_StreamFind
#'
#' @description Settings for calculating quality parameters of features (e.g., signal-to-noise (sn) ratio).
#'
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @param minTraces Numeric of length 1 with the minimum number traces for 
#' calculating feature quality.
#' @template arg-ms-filtered
#' @template arg-runParallel
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_calculate_quality_StreamFind.
#'
#' @export
#'
Settings_calculate_quality_StreamFind <- function(
    rtExpand = 120,
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

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_calculate_quality_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_calculate_quality_StreamFind S3 class object.
#'
#' @export
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

# generate_formulas -----

#' @title Settings_generate_formulas_genform
#'
#' @description Settings for generating formulas using the algorithm 
#' \href{https://sourceforge.net/projects/genform/}{GenForm}. 
#' The algorithm is used via the function \link[patRoon]{generateFormulas} 
#' from the package \pkg{patRoon}.
#' 
#' @param relMzDev 
#' @param elements 
#' @param hetero 
#' @param oc 
#' @param thrMS 
#' @param thrMSMS 
#' @param thrComb 
#' @param maxCandidates 
#' @param extraOpts 
#' @param calculateFeatures 
#' @param featThreshold 
#' @param featThresholdAnn 
#' @param absAlignMzDev 
#' @param MSMode 
#' @param isolatePrec 
#' @param timeout 
#' @param topMost 
#' @param batchSize 
#' 
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
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @describeIn Settings_generate_formulas_genform
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_generate_formulas_genform S3 class object.
#'
#' @export
#'
validate.Settings_generate_formulas_genform <- function(x) {
  all(
    checkmate::test_choice(x$call, "generate_formulas"),
    checkmate::test_choice(x$algorithm, "genform")
  )
}

# generate_compounds -----

#' @title Settings_generate_compounds_metfrag
#'
#' @description Settings for generating compounds using \href{https://ipb-halle.github.io/MetFrag/}{MetFrag}. 
#' The algorithm is used via the function \link[patRoon]{generateCompounds} from the package \pkg{patRoon}.
#' 
#' @param method 
#' @param timeout 
#' @param timeoutRetries 
#' @param errorRetries 
#' @param topMost 
#' @param dbRelMzDev 
#' @param fragRelMzDev 
#' @param fragAbsMzDev 
#' @param adduct 
#' @param database 
#' @param extendedPubChem 
#' @param chemSpiderToken 
#' @param scoreTypes 
#' @param scoreWeights 
#' @param preProcessingFilters 
#' @param postProcessingFilters 
#' @param maxCandidatesToStop 
#' @param identifiers 
#' @param extraOpts 
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
  
  settings <- as.ProcessingSettings(settings)
  
  return(settings)
}

#' @describeIn Settings_generate_compounds_metfrag
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_generate_compounds_metfrag S3 class object.
#'
#' @export
#'
validate.Settings_generate_compounds_metfrag <- function(x) {
  all(
    checkmate::test_choice(x$call, "generate_compounds"),
    checkmate::test_choice(x$algorithm, "metfrag")
  )
}


# integrate_chromatograms -----

#' @title Settings_integrate_chromatograms_StreamFind
#'
#' @description Prototype.
#' 
#' @param merge 
#' @param closeByThreshold 
#' @param minPeakHeight 
#' @param minPeakDistance 
#' @param minPeakWidth 
#' @param maxPeakWidth 
#' @param minSN 
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

#' @describeIn Settings_integrate_chromatograms_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_integrate_chromatograms_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_integrate_chromatograms_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "integrate_chromatograms"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# calculate_spectra_charges -----

#' @title Settings_calculate_spectra_charges_StreamFind
#'
#' @description Prototype.
#' 
#' @param roundVal 
#' @param relLowCut 
#' @param absLowCut 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_calculate_spectra_charges_StreamFind.
#'
#' @export
#'
Settings_calculate_spectra_charges_StreamFind <- function(roundVal = 35,
                                                            relLowCut = 0.2,
                                                            absLowCut = 300) {
  
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

#' @describeIn Settings_calculate_spectra_charges_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_calculate_spectra_charges_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_calculate_spectra_charges_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "calculate_spectra_charges"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# deconvolute_spectra -----

#' @title Settings_deconvolute_spectra_StreamFind
#'
#' @description Prototype.
#' 
#' @param clustVal 
#' @param window 
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

#' @describeIn Settings_deconvolute_spectra_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_deconvolute_spectra_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_deconvolute_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "deconvolute_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# average_spectra -----

#' @title Settings_average_spectra_StreamFind
#'
#' @description Prototype.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_average_spectra_StreamFind.
#'
#' @export
#'
Settings_average_spectra_StreamFind <- function() {
  
  settings <- list(
    call = "average_spectra",
    algorithm = "StreamFind",
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

#' @describeIn Settings_average_spectra_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_average_spectra_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_average_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "average_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# cluster_spectra -----

#' @title Settings_cluster_spectra_StreamFind
#'
#' @description Prototype.
#' 
#' @param val 
#' @param clustVal 
#' @param presence 
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

#' @describeIn Settings_cluster_spectra_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_cluster_spectra_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_cluster_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "cluster_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# subtract_blank_spectra -----

#' @title Settings_subtract_blank_spectra_StreamFind
#'
#' @description Prototype.
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_subtract_blank_spectra_StreamFind.
#'
#' @export
#'
Settings_subtract_blank_spectra_StreamFind <- function() {
  
  settings <- list(
    call = "subtract_blank_spectra",
    algorithm = "StreamFind",
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

#' @describeIn Settings_subtract_blank_spectra_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_subtract_blank_spectra_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_subtract_blank_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "subtract_blank_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# correct_chromatograms_baseline -----

#' @title Settings_correct_chromatograms_baseline_baseline
#'
#' @description Prototype.
#' 
#' @param method 
#' @param args 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_correct_chromatograms_baseline_baseline.
#'
#' @export
#'
Settings_correct_chromatograms_baseline_baseline <- function(method = "als",
                                                       args = list(lambda = 5, p = 0.05, maxit = 10)) {
  
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

#' @describeIn Settings_correct_chromatograms_baseline_baseline
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_correct_chromatograms_baseline_baseline S3 class object.
#'
#' @export
#'
validate.Settings_correct_chromatograms_baseline_baseline <- function(x) {
  all(
    checkmate::test_choice(x$call, "correct_chromatograms_baseline"),
    checkmate::test_choice(x$algorithm, "baseline")
  )
}

#' @title Settings_correct_chromatograms_baseline_airpls
#'
#' @description Prototype.
#' 
#' @param lambda 
#' @param differences 
#' @param itermax 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_correct_chromatograms_baseline_airpls.
#'
#' @export
#'
Settings_correct_chromatograms_baseline_airpls <- function(lambda = 10, differences = 1, itermax = 20) {
  
  settings <- list(
    call = "correct_chromatograms_baseline",
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

#' @describeIn Settings_correct_chromatograms_baseline_airpls
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_correct_chromatograms_baseline_airpls S3 class object.
#'
#' @export
#'
validate.Settings_correct_chromatograms_baseline_airpls <- function(x) {
  all(
    checkmate::test_choice(x$call, "correct_chromatograms_baseline"),
    checkmate::test_choice(x$algorithm, "airpls")
  )
}

# correct_spectra_baseline -----

#' @title Settings_correct_spectra_baseline_baseline
#'
#' @description Prototype.
#' 
#' @param method 
#' @param args 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_correct_spectra_baseline_baseline.
#'
#' @export
#'
Settings_correct_spectra_baseline_baseline <- function(method = "als",
                                                       args = list(lambda = 5, p = 0.05, maxit = 10)) {
  
  settings <- list(
    call = "correct_spectra_baseline",
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

#' @describeIn Settings_correct_spectra_baseline_baseline
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_correct_spectra_baseline_baseline S3 class object.
#'
#' @export
#'
validate.Settings_correct_spectra_baseline_baseline <- function(x) {
  all(
    checkmate::test_choice(x$call, "correct_spectra_baseline"),
    checkmate::test_choice(x$algorithm, "baseline")
  )
}

#' @title Settings_correct_spectra_baseline_airpls
#'
#' @description Prototype.
#' 
#' @param lambda 
#' @param differences 
#' @param itermax 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_correct_spectra_baseline_airpls.
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

#' @describeIn Settings_correct_spectra_baseline_airpls
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_correct_spectra_baseline_airpls S3 class object.
#'
#' @export
#'
validate.Settings_correct_spectra_baseline_airpls <- function(x) {
  all(
    checkmate::test_choice(x$call, "correct_spectra_baseline"),
    checkmate::test_choice(x$algorithm, "airpls")
  )
}

# smooth_chromatograms -----

#' @title Settings_smooth_chromatograms_movingaverage
#'
#' @description Prototype.
#' 
#' @param windowSize 
#' @param xValWindow 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_smooth_chromatograms_movingaverage.
#'
#' @export
#'
Settings_smooth_chromatograms_movingaverage <- function(windowSize = 5, xValWindow = NULL) {
  
  settings <- list(
    call = "smooth_chromatograms",
    algorithm = "movingaverage",
    parameters = list(
      windowSize = windowSize,
      xValWindow = xValWindow
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

#' @describeIn Settings_smooth_chromatograms_movingaverage
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_smooth_chromatograms_movingaverage S3 class object.
#'
#' @export
#'
validate.Settings_smooth_chromatograms_movingaverage <- function(x) {
  all(
    checkmate::test_choice(x$call, "smooth_chromatograms"),
    checkmate::test_choice(x$algorithm, "movingaverage")
  )
}

#' @title Settings_smooth_chromatograms_savgol
#'
#' @description Prototype.
#' 
#' @param fl 
#' @param forder 
#' @param dorder 
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

#' @describeIn Settings_smooth_chromatograms_savgol
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_smooth_chromatograms_savgol S3 class object.
#'
#' @export
#'
validate.Settings_smooth_chromatograms_savgol <- function(x) {
  all(
    checkmate::test_choice(x$call, "smooth_chromatograms"),
    checkmate::test_choice(x$algorithm, "savgol")
  )
}

# smooth_spectra -----

#' @title Settings_smooth_spectra_movingaverage
#'
#' @description Prototype.
#' 
#' @param windowSize 
#' @param xValWindow 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_smooth_spectra_movingaverage.
#'
#' @export
#'
Settings_smooth_spectra_movingaverage <- function(windowSize = 5, xValWindow = NULL) {
  
  settings <- list(
    call = "smooth_spectra",
    algorithm = "movingaverage",
    parameters = list(
      windowSize = windowSize,
      xValWindow = xValWindow
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

#' @describeIn Settings_smooth_spectra_movingaverage
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_smooth_spectra_movingaverage S3 class object.
#'
#' @export
#'
validate.Settings_smooth_spectra_movingaverage <- function(x) {
  all(
    checkmate::test_choice(x$call, "smooth_spectra"),
    checkmate::test_choice(x$algorithm, "movingaverage")
  )
}

#' @title Settings_smooth_spectra_savgol
#'
#' @description Prototype.
#' 
#' @param fl 
#' @param forder 
#' @param dorder 
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

#' @describeIn Settings_smooth_spectra_savgol
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_smooth_spectra_savgol S3 class object.
#'
#' @export
#'
validate.Settings_smooth_spectra_savgol <- function(x) {
  all(
    checkmate::test_choice(x$call, "smooth_spectra"),
    checkmate::test_choice(x$algorithm, "savgol")
  )
}

# normalize_spectra -----

#' @title Settings_normalize_spectra_StreamFind
#'
#' @description Prototype.
#' 
#' @param liftTozero 
#' @param xName 
#' @param xVal 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_normalize_spectra_StreamFind.
#'
#' @export
#'
Settings_normalize_spectra_StreamFind <- function(liftTozero = FALSE,
                                                  xName = NULL,
                                                  xVal = NULL) {
  
  settings <- list(
    call = "normalize_spectra",
    algorithm = "StreamFind",
    parameters = list(
      liftTozero = liftTozero,
      xName = xName,
      xVal = xVal
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

#' @describeIn Settings_normalize_spectra_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_normalize_spectra_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_normalize_spectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

#' @title Settings_normalize_spectra_minmax
#'
#' @description Prototype.
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

#' @describeIn Settings_normalize_spectra_minmax
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_normalize_spectra_minmax S3 class object.
#'
#' @export
#'
validate.Settings_normalize_spectra_minmax <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "minmax")
  )
}

#' @title Settings_normalize_spectra_snv
#'
#' @description Prototype.
#' 
#' @param liftTozero 
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

#' @describeIn Settings_normalize_spectra_snv
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_normalize_spectra_snv S3 class object.
#'
#' @export
#'
validate.Settings_normalize_spectra_snv <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "snv")
  )
}

#' @title Settings_normalize_spectra_scale
#'
#' @description Prototype.
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
    version = NA_character_,
    software = NA_character_,
    developer = NA_character_,
    contact = NA_character_,
    link = NA_character_,
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @describeIn Settings_normalize_spectra_scale
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_normalize_spectra_scale S3 class object.
#'
#' @export
#'
validate.Settings_normalize_spectra_scale <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "scale")
  )
}

#' @title Settings_normalize_spectra_blockweight
#'
#' @description Prototype.
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
    version = NA_character_,
    software = NA_character_,
    developer = NA_character_,
    contact = NA_character_,
    link = NA_character_,
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @describeIn Settings_normalize_spectra_blockweight
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_normalize_spectra_blockweight S3 class object.
#'
#' @export
#'
validate.Settings_normalize_spectra_blockweight <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "blockweight")
  )
}

#' @title Settings_normalize_spectra_meancenter
#'
#' @description Prototype.
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
    version = NA_character_,
    software = NA_character_,
    developer = NA_character_,
    contact = NA_character_,
    link = NA_character_,
    doi = NA_character_
  )
  
  as.ProcessingSettings(settings)
}

#' @describeIn Settings_normalize_spectra_meancenter
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_normalize_spectra_meancenter S3 class object.
#'
#' @export
#'
validate.Settings_normalize_spectra_meancenter <- function(x) {
  all(
    checkmate::test_choice(x$call, "normalize_spectra"),
    checkmate::test_choice(x$algorithm, "meancenter")
  )
}

# subtract_spectra_section -----

#' @title Settings_subtract_spectra_section_StreamFind
#'
#' @description Prototype.
#' 
#' @param sectionVal 
#' @param sectionWindow 
#'
#' @return A ProcessingSettings S3 class object with subclass Settings_subtract_spectra_section_StreamFind.
#'
#' @export
#'
Settings_subtract_spectra_section_StreamFind <- function(sectionVal = "rt",
                                                         sectionWindow = c(10, 200)) {
  
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

#' @describeIn Settings_subtract_spectra_section_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_subtract_spectra_section_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_subtract_spectra_section_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "subtract_spectra_section"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# delete_spectra_section -----

#' @title Settings_delete_spectra_section_StreamFind
#'
#' @description Prototype.
#' 
#' @param section 
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

#' @describeIn Settings_delete_spectra_section_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_delete_spectra_section_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_delete_spectra_section_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "delete_spectra_section"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}

# merge_spectra_time_series -----

#' @title Settings_merge_spectra_time_series_StreamFind
#'
#' @description Prototype.
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

#' @describeIn Settings_merge_spectra_time_series_StreamFind
#' Validates the object structure, returning a logical value of length one.
#'
#' @param x A Settings_merge_spectra_time_series_StreamFind S3 class object.
#'
#' @export
#'
validate.Settings_merge_spectra_time_series_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "merge_spectra_time_series"),
    checkmate::test_choice(x$algorithm, "StreamFind")
  )
}
