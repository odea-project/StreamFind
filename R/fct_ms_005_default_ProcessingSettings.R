
# Default Settings -----

#' get_default_ProcessingSettings
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

#' save_default_ProcessingSettings
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

## centroid_spectra -----

#' @title Settings_centroid_spectra_qCentroids
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_centroid_spectra_qCentroids <- function() {

  # TODO Gerrit - update settings info

  settings <- list(
    call = "centroid_spectra",
    algorithm = "qCentroids",
    parameters = list(),
    software = "q",
    developer = "Max, Gerrit",
    contact = "gerrit@email.de",
    link = "",
    doi = ""
  )

  as.ProcessingSettings(settings)
}

## bin_spectra -----

#' @title Settings_bin_spectra_qBinning
#'
#' @description X.
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
    software = "q",
    developer = "Max, Gerrit",
    contact = "max@email.de",
    link = "",
    doi = ""
  )

  as.ProcessingSettings(settings)
}

## find_features -----

#' @title Settings_find_features_qPeaks
#'
#' @description X.
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
    software = "q",
    developer = "Max, Gerrit",
    contact = "max@email.de",
    link = "",
    doi = ""
  )

  as.ProcessingSettings(settings)

}

#' Settings_find_features_xcms3_centwave
#'
#' @description Settings for finding features (i.e., chromatographic peaks)
#' in mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
#' (version 3) with the algorithm centwave
#' (\url{https://rdrr.io/bioc/xcms/man/findChromPeaks-centWave.html}).
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
#' \insertRef{patroon01}{streamFind}
#'
#' \insertRef{patroon02}{streamFind}
#'
#' \insertRef{xcms01}{streamFind}
#'
#' \insertRef{xcms02}{streamFind}
#'
#' \insertRef{xcms03}{streamFind}
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
    software = "xcms",
    developer = "Ralf Tautenhahn, Johannes Rainer",
    contact = "rtautenh@ipb-halle.de",
    link = "https://rdrr.io/bioc/xcms/man/findChromPeaks-centWave.html",
    doi = "https://doi.org/10.1186/1471-2105-9-504"
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' Settings_find_features_openms
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
#'
#' @details See the \link[patRoon]{findFeaturesOpenMS} function from the
#' \pkg{patRoon} package for more information and requirements.
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_find_features_openms.
#'
#' @references
#' \insertRef{patroon01}{streamFind}
#'
#' \insertRef{patroon02}{streamFind}
#'
#' \insertRef{openms01}{streamFind}
#'
#' @export
#'
Settings_find_features_openms <- function(
    noiseThrInt = 500,
    chromSNR = 3,
    chromFWHM = 10,
    mzPPM = 12,
    reEstimateMTSD = FALSE,
    traceTermCriterion = "sample_rate",
    traceTermOutliers = 5,
    minSampleRate = 1,
    minTraceLength = 5,
    maxTraceLength = -1,
    widthFiltering = "fixed",
    minFWHM = 5,
    maxFWHM = 40,
    traceSNRFiltering = TRUE,
    localRTRange = 10,
    localMZRange = 6.5,
    isotopeFilteringModel = "metabolites (5% RMS)",
    MZScoring13C = FALSE,
    useSmoothedInts = FALSE,
    extraOpts = NULL,
    intSearchRTWindow = 3,
    useFFMIntensities = FALSE) {

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
      useFFMIntensities = useFFMIntensities
    ),
    software = "openms",
    developer = "Rost HL, Sachsenberg T, Aiche S, Bielow C et al.",
    contact = "oliver.kohlbacher@uni-tuebingen.de",
    link = "https://abibuilder.cs.uni-tuebingen.de/archive/openms/Documentation/release/latest/html/index.html",
    doi = "https://doi.org/10.1038/nmeth.3959"
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
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
#' \insertRef{patroon01}{streamFind}
#'
#' \insertRef{patroon02}{streamFind}
#'
#' \insertRef{kpic01}{streamFind}
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
    software = "kpic2",
    developer = "Hongchao Ji",
    contact = "ji.hongchao@foxmail.com",
    link = "https://github.com/hcji/KPIC2",
    doi = "10.1021/acs.analchem.7b01547"
  )

  if (kmeans) {
    settings$parameters$alpha <- alpha
  }

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

## group_features -----

#' Settings_group_features_xcms3_peakdensity
#'
#' @description Settings for grouping features (i.e., chromatographic peaks)
#' across mzML/mzXML files using the package
#' \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
#' (version 3) with the algorithm peakdensity
#' (\url{https://rdrr.io/bioc/xcms/man/groupChromPeaks-density.html}).
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
#' \insertRef{patroon01}{streamFind}
#'
#' \insertRef{patroon02}{streamFind}
#'
#' \insertRef{xcms01}{streamFind}
#'
#' \insertRef{xcms02}{streamFind}
#'
#' \insertRef{xcms03}{streamFind}
#'
#' @export
#'
Settings_group_features_xcms3_peakdensity <- function(
    bw = 5,
    minFraction = 0.5,
    minSamples = 1,
    binSize = 0.008,
    maxFeatures = 100) {

  settings <- list(
    call = "group_features",
    algorithm = "xcms3_peakdensity",
    parameters = list(
      class = "PeakDensityParam",
      sampleGroups = "holder",
      bw = bw,
      minFraction = minFraction,
      minSamples = minSamples,
      binSize = binSize,
      maxFeatures = maxFeatures
    ),
    software = "xcms",
    developer = "Colin Smith, Johannes Rainer",
    contact = "siuzdak@scripps.edu",
    link = "https://rdrr.io/bioc/xcms/man/groupChromPeaks-density.html",
    doi = "https://doi.org/10.1021/ac051437y"
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

## load_features -----

#' @title Settings_load_features_ms1_streamFind
#'
#' @description Settings for loading MS1 spectra for features.
#'
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-mzClust
#' @template arg-ms-isInAllSpectra
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#' @template arg-runParallel
#' @template arg-verbose
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_load_features_ms1_streamFind.
#'
#' @export
#'
Settings_load_features_ms1_streamFind <- function(
    rtWindow = c(-2, 2),
    mzWindow = c(-1, 6),
    mzClust = 0.003,
    isInAllSpectra = TRUE,
    minIntensity = 250,
    filtered = FALSE,
    runParallel = FALSE,
    verbose = FALSE) {

  settings <- list(
    call = "load_features_ms1",
    algorithm = "streamFind",
    parameters = list(
      "rtWindow" = rtWindow,
      "mzWindow" = mzWindow,
      "mzClust" = mzClust,
      "isInAllSpectra" = isInAllSpectra,
      "minIntensity" = minIntensity,
      "filtered" = filtered,
      "runParallel" = runParallel,
      "verbose" = verbose
    ),
    software = "streamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://github.com/ricardobachertdacunha/streamFind",
    doi = NA_character_
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_load_features_ms1_streamFind
#' Validates the Settings_load_features_ms1_streamFind S3 class object,
#' returning a logical value of length one.
#'
#' @param x A Settings_load_features_ms1_streamFind S3 class object.
#'
#' @export
#'
validate.Settings_load_features_ms1_streamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_features_ms1"),
    checkmate::test_choice(x$algorithm, "streamFind"),
    checkmate::test_double(x$parameters$rtWindow, max.len = 2),
    checkmate::test_double(x$parameters$mzWindow, max.len = 2),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

#' @title Settings_load_features_ms2_streamFind
#'
#' @description Settings for loading MS2 spectra for features.
#'
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-isInAllSpectra
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#' @template arg-runParallel
#' @template arg-verbose
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_load_features_ms2_streamFind.
#'
#' @export
#'
Settings_load_features_ms2_streamFind <- function(
    isolationWindow = 1.3,
    mzClust = 0.01,
    isInAllSpectra = TRUE,
    minIntensity = 0,
    filtered = FALSE,
    runParallel = FALSE,
    verbose = FALSE) {

  settings <- list(
    call = "load_features_ms2",
    algorithm = "streamFind",
    parameters = list(
      "isolationWindow" = isolationWindow,
      "mzClust" = mzClust,
      "isInAllSpectra" = isInAllSpectra,
      "minIntensity" = minIntensity,
      "filtered" = filtered,
      "runParallel" = runParallel,
      "verbose" = verbose
    ),
    software = "streamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://github.com/ricardobachertdacunha/streamFind",
    doi = NA_character_
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_load_features_ms2_streamFind
#' Validates the Settings_load_features_ms1_streamFind S3 class object,
#' returning a logical value of length one.
#'
#' @param x A Settings_load_features_ms2_streamFind S3 class object.
#'
#' @export
#'
validate.Settings_load_features_ms2_streamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_features_ms2"),
    checkmate::test_choice(x$algorithm, "streamFind"),
    checkmate::test_number(x$parameters$isolationWindow),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

#' @title Settings_load_groups_ms1_streamFind
#'
#' @description Settings for loading MS1 spectra for feature groups.
#'
#' @template arg-ms-mzClust
#' @template arg-ms-isInAllSpectra
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#' @template arg-runParallel
#' @template arg-verbose
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_load_groups_ms1_streamFind.
#'
#' @export
#'
Settings_load_groups_ms1_streamFind <- function(
    mzClust = 0.003,
    isInAllSpectra = TRUE,
    minIntensity = 1000,
    verbose = FALSE,
    filtered = FALSE,
    runParallel = FALSE) {

  settings <- list(
    call = "load_groups_ms1",
    algorithm = "streamFind",
    parameters = list(
      "mzClust" = mzClust,
      "isInAllSpectra" = isInAllSpectra,
      "minIntensity" = minIntensity,
      "filtered" = filtered,
      "runParallel" = runParallel,
      "verbose" = verbose
    ),
    software = "streamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://github.com/ricardobachertdacunha/streamFind",
    doi = NA_character_
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_load_groups_ms1_streamFind
#' Validates the Settings_load_groups_ms1_streamFind S3 class object,
#' returning a logical value of length one.
#'
#' @param x A Settings_load_groups_ms1_streamFind S3 class object.
#'
#' @export
#'
validate.Settings_load_groups_ms1_streamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_groups_ms1"),
    checkmate::test_choice(x$algorithm, "streamFind"),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

#' @title Settings_load_groups_ms2_streamFind
#'
#' @description Settings for loading MS2 spectra for feature groups.
#'
#' @template arg-ms-mzClust
#' @template arg-ms-isInAllSpectra
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#' @template arg-runParallel
#' @template arg-verbose
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_load_groups_ms2_streamFind.
#'
#' @export
#'
Settings_load_groups_ms2_streamFind <- function(
    mzClust = 0.01,
    isInAllSpectra = TRUE,
    minIntensity = 250,
    filtered = FALSE,
    runParallel = FALSE,
    verbose = FALSE) {

  settings <- list(
    call = "load_groups_ms2",
    algorithm = "streamFind",
    parameters = list(
      "mzClust" = mzClust,
      "isInAllSpectra" = isInAllSpectra,
      "minIntensity" = minIntensity,
      "filtered" = filtered,
      "runParallel" = runParallel,
      "verbose" = verbose
    ),
    software = "streamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://github.com/ricardobachertdacunha/streamFind",
    doi = NA_character_
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_load_groups_ms2_streamFind
#' Validates the Settings_load_groups_ms2_streamFind S3 class object,
#' returning a logical value of length one.
#'
#' @param x A Settings_load_groups_ms2_streamFind S3 class object.
#'
#' @export
#'
validate.Settings_load_groups_ms2_streamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "load_groups_ms2"),
    checkmate::test_choice(x$algorithm, "streamFind"),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

## filter_features -----

#' Settings_filter_features_streamFind
#'
#' @description Settings for filtering of features and feature groups.
#'
#' @param minIntensity Numeric (length 1) with the minimum intensity.
#' @param minSnRatio Numeric (length 1) with the minimum signal-to-noise ratio.
#' The filter is only applied if the features data.table contains the column "sn".
#' @param maxGroupSd Numeric (length 1) with the maximum intensity deviation
#' within each analysis replicate (in percentage).
#' @param blank Numeric (length 1) with the intensity threshold for blank
#' subtraction. All features/feature groups not higher then the `blank` * its
#' intensity are filtered.
#' @param minGroupAbundance Numeric (length 1) with the minimum presence of a
#' feature is a given analysis replicate.
#' @param excludeIsotopes Logical (length 1) with `TRUE` for filtering
#' annotated isotopes (only prevails the monoisotopic features).
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_filter_features_streamFind.
#'
#' @export
#'
Settings_filter_features_streamFind <- function(
    minIntensity = NULL,
    minSnRatio = NULL,
    maxGroupSd = NULL,
    blank = NULL,
    minGroupAbundance = NULL,
    excludeIsotopes = NULL) {

  settings <- list(
    call = "filter_features",
    algorithm = "streamFind",
    parameters = list(),
    software = "streamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://github.com/ricardobachertdacunha/streamFind",
    doi = NA_character_
  )

  if (!is.null(minIntensity)) {
    checkmate::assert_double(minIntensity, max.len = 1)
    settings$parameters[["minIntensity"]] <- minIntensity
  }

  if (!is.null(minSnRatio)) {
    checkmate::assert_double(minSnRatio, max.len = 1)
    settings$parameters[["minSnRatio"]] <- minSnRatio
  }

  if (!is.null(maxGroupSd)) {
    checkmate::assert_double(maxGroupSd, max.len = 1)
    settings$parameters[["maxGroupSd"]] <- maxGroupSd
  }

  if (!is.null(blank)) {
    checkmate::assert_double(blank, max.len = 1)
    settings$parameters[["blank"]] <- blank
  }

  if (!is.null(minGroupAbundance)) {
    checkmate::assert_count(minGroupAbundance)
    settings$parameters[["minGroupAbundance"]] <- minGroupAbundance
  }

  if (!is.null(excludeIsotopes)) {
    checkmate::assert_logical(excludeIsotopes, max.len = 1)
    settings$parameters[["excludeIsotopes"]] <- excludeIsotopes
  }

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_filter_features_streamFind
#' Validates the Settings_filter_features_streamFind S3 class object, returning
#' a logical value of length one.
#'
#' @param x A Settings_filter_features_streamFind S3 class object.
#'
#' @export
#'
validate.Settings_filter_features_streamFind <- function(x) {

  filters <- names(x$parameters)

  all(
    checkmate::test_choice(x$call, "filter_features"),
    checkmate::test_choice(x$algorithm, "streamFind"),

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
    }
    # checkmate::test_number(x$parameters$minIntensity),
    # checkmate::test_number(x$parameters$minSnRatio),
    # checkmate::test_number(x$parameters$maxGroupSd),
    # checkmate::test_number(x$parameters$blank),
    # checkmate::test_count(x$parameters$minGroupAbundance),
    # checkmate::test_logical(x$parameters$excludeIsotopes, max.len = 1)
  )
}

## annotate_features -----

#' Settings_annotate_features_streamFind
#'
#' @description
#' Settings for annotation of isotopic features. The method uses the `maxIsotopes`
#' to define the maximum length of the isotopic chain. The list of candidate features is
#' build with the `rtWindowAlignment` and the maximum mass increment to match the
#' maximum chain length. Then, the mass difference  of the natural isotopes defined
#' by `elements` and a given monoisotopic ion (i.e., feature) are targeted.
#' Each candidate is then evaluated according to the mass error and the expected
#' relative intensity range as defined by the `mode`.
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
#' Settings_annotate_features_streamFind.
#'
#' @export
#'
Settings_annotate_features_streamFind <- function(
    maxIsotopes = 5,
    elements = c("C","H", "N", "O", "S", "Cl", "Br"),
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
  lapply(elements, function(i) checkmate::assert_choice(i, c("C","H", "N", "O", "S", "Cl", "Br")))
  checkmate::assert_logical(runParallel, max.len = 1)

  settings <- list(
    call = "annotate_features",
    algorithm = "streamFind",
    parameters = list(
      "maxIsotopes" = maxIsotopes,
      "elements" = elements,
      "mode" = mode,
      "maxCharge" = maxCharge,
      "rtWindowAlignment" = rtWindowAlignment,
      "maxGaps" = maxGaps,
      "runParallel" = runParallel
    ),
    software = "streamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://github.com/ricardobachertdacunha/streamFind",
    doi = NA_character_
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_annotate_features_streamFind
#' Validates the Settings_annotate_features_streamFind S3 class object, returning a logical
#' value of length one.
#'
#' @param x A Settings_annotate_features_streamFind S3 class object.
#'
#' @export
#'
validate.Settings_annotate_features_streamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "annotate_features"),
    checkmate::test_choice(x$algorithm, "streamFind"),
    checkmate::test_count(x$parameters$maxIsotopes),
    checkmate::test_count(x$parameters$maxCharge),
    checkmate::test_count(x$parameters$maxGaps),
    checkmate::test_double(x$parameters$rtWindowAlignment, max.len = 1),
    checkmate::test_choice(x$parameters$mode, "small molecules"),
    checkmate::test_vector(x$parameters$elements, any.missing = FALSE, min.len = 1),
    vapply(x$parameters$elements, function(i) checkmate::test_choice(i, c("C","H", "N", "O", "S", "Cl", "Br")), FALSE),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1)
  )
}

## suspect_screening -----

#' Settings_suspect_screening_streamFind
#'
#' @description
#' Settings for performing suspect screening using a data.frame with target
#' compounds.
#'
#' @param database A data.frame with at least the columns name and mass,
#' indicating the name and neutral monoisotopic mass of the suspect targets.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#'
#' @return A ProcessingSettings S3 class object with subclass
#' Settings_suspect_screening_streamFind.
#'
#' @export
#'
Settings_suspect_screening_streamFind <- function(
    database = NULL,
    ppm = 4,
    sec = 10) {

  settings <- list(
    call = "suspect_screening",
    algorithm = "streamFind",
    parameters = list(
      "database" = database,
      "ppm" = ppm,
      "sec" = sec
    ),
    software = "streamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://github.com/ricardobachertdacunha/streamFind",
    doi = NA_character_
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @describeIn Settings_suspect_screening_streamFind
#' Validates the Settings_suspect_screening_streamFind S3 class object,
#' returning a logical value of length one.
#'
#' @param x A Settings_suspect_screening_streamFind S3 class object.
#'
#' @export
#'
validate.Settings_suspect_screening_streamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "suspect_screening"),
    checkmate::test_choice(x$algorithm, "streamFind"),
    checkmate::test_double(x$parameters$ppm, max.len = 1),
    checkmate::test_double(x$parameters$sec, max.len = 1)
  )
}






