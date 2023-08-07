
# Default Settings -----

#' @title get_default_ProcessingSettings
#'
#' @description X.
#'
#' @param call X.
#' @param software X.
#' @param algorithm X.
#'
#' @return A ProcessingSettings S3 class object.
#'
#' @export
#'
get_default_ProcessingSettings <- function(call = NA_character_,
                                           software = NA_character_,
                                           algorithm = NA_character_) {

  settings <- NULL

  if ("find_features" %in% call) {

    if ("centwave" %in% algorithm) {
      settings <- settings_find_features_xcms3_centwave()
    }

    if ("openms" %in% algorithm) {
      settings <- settings_find_features_openms()
    }

    if ("kpic2" %in% algorithm) {
      settings <- settings_find_features_kpic2()
    }
  }

  if ("group_features" %in% call) {

    if ("peakdensity" %in% algorithm) {
      settings <- settings_group_features_xcms3_peakdensity()
    }

    if ("openms" %in% algorithm) {

    }
  }

  if ("load_features_ms1" %in% call) {
    settings <- settings_load_features_ms1_streamFind()
  }

  if ("load_features_ms2" %in% call) {
    settings <- settings_load_features_ms2_streamFind()
  }

  if ("load_groups_ms1" %in% call) {
    settings <- settings_load_groups_ms1_streamFind()
  }

  if ("load_groups_ms2" %in% call) {
    settings <- settings_load_groups_ms2_streamFind()
  }

  if ("filter_features" %in% call) {
    settings <- settings_filter_features_streamFind()
  }

  if ("annotate_features" %in% call) {
    settings <- settings_annotate_features_streamFind()
  }

  return(settings)
}

#' @title save_default_ProcessingSettings
#'
#' @description X.
#'
#' @param call X.
#' @param software X.
#' @param algorithm X.
#' @param format X.
#' @param name X.
#' @param path X.
#'
#' @return Creates a json/rds files on the defined path.
#'
#' @export
#'
save_default_ProcessingSettings <- function(call = NA_character_,
                                            software = NA_character_,
                                            algorithm = NA_character_,
                                            format = "json",
                                            name = "settings",
                                            path = getwd()) {

  settings <- get_default_ProcessingSettings(call, software, algorithm)

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

## find -----

#' @title settings_find_features_xcms3_centwave
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_find_features_xcms3_centwave <- function() {

  if (!requireNamespace("xcms", quietly = TRUE)) {
    warning("xcms package required but not installed!")
    return(NULL)
  }

  settings <- list(
    call = "find_features",
    algorithm = "xcms3",
    parameters = xcms::CentWaveParam(
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
      roiList = list(),
      firstBaselineCheck = FALSE,
      roiScales = numeric(),
      extendLengthMSW = FALSE
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

#' @title settings_find_features_openms
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_find_features_openms <- function() {

  settings <- list(
    call = "find_features",
    algorithm = "openms",
    parameters = list(
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
      useFFMIntensities = FALSE
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

#' @title settings_find_features_kpic2
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_find_features_kpic2 <- function() {

  if (!requireNamespace("KPIC", quietly = TRUE)) {
    warning("KPIC package required but not installed!")
    return(NULL)
  }

  settings <- list(
    call = "find_features",
    algorithm = "kpic2",
    parameters = list(
      kmeans = TRUE,
      level = 500, # Mass traces are only retained if their maximum values are over level
      mztol = 0.01, # The initial m/z tolerance.
      gap = 2, # The number of gap points of a mass trace.
      width = 5, # The minimum length of a mass trace.
      min_snr = 4 # Minimum signal to noise ratio.
    ),
    software = "kpic2",
    developer = "Hongchao Ji",
    contact = "ji.hongchao@foxmail.com",
    link = "https://github.com/hcji/KPIC2",
    doi = "10.1021/acs.analchem.7b01547"
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

## group -----

#' @title settings_group_features_xcms3_peakdensity
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_group_features_xcms3_peakdensity <- function() {

  if (!requireNamespace("xcms", quietly = TRUE)) {
    warning("xcms package required but not installed!")
    return(NULL)
  }

  settings <- list(
    call = "group_features",
    algorithm = "xcms3",
    parameters = xcms::PeakDensityParam(
      sampleGroups = "holder",
      bw = 5,
      minFraction = 0.5,
      minSamples = 1,
      binSize = 0.008,
      maxFeatures = 100
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

## load -----

#' @title settings_load_features_ms1_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_load_features_ms1_streamFind <- function() {

  settings <- list(
    call = "load_features_ms1",
    algorithm = "streamFind",
    parameters = list(
      rtWindow = c(-2, 2),
      mzWindow = c(-1, 6),
      mzClust = 0.003,
      minIntensity = 250,
      filtered = FALSE,
      runParallel = FALSE,
      verbose = FALSE
    ),
    software = "streamFind",
    developer = "",
    contact = "cunha@iuta.de",
    link = "",
    doi = ""
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @title settings_load_features_ms2_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_load_features_ms2_streamFind <- function() {

  settings <- list(
    call = "load_features_ms2",
    algorithm = "streamFind",
    parameters = list(
      isolationWindow = 1.3,
      mzClust = 0.003,
      minIntensity = 0,
      filtered = FALSE,
      runParallel = FALSE,
      verbose = FALSE
    ),
    software = "streamFind",
    developer = "",
    contact = "cunha@iuta.de",
    link = "",
    doi = ""
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @title settings_load_groups_ms1_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_load_groups_ms1_streamFind <- function() {

  settings <- list(
    call = "load_groups_ms1",
    algorithm = "streamFind",
    parameters = list(
      mzClust = 0.003,
      minIntensity = 1000,
      verbose = FALSE,
      filtered = FALSE,
      runParallel = FALSE
    ),
    software = "streamFind",
    developer = "",
    contact = "cunha@iuta.de",
    link = "",
    doi = ""
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

#' @title settings_load_groups_ms2_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_load_groups_ms2_streamFind <- function() {

  settings <- list(
    call = "load_groups_ms2",
    algorithm = "streamFind",
    parameters = list(
      mzClust = 0.003,
      minIntensity = 250,
      filtered = FALSE,
      runParallel = FALSE,
      verbose = FALSE
    ),
    software = "streamFind",
    developer = "",
    contact = "cunha@iuta.de",
    link = "",
    doi = ""
  )

  settings <- as.ProcessingSettings(settings)

  return(settings)
}

## filter -----

#' @title settings_filter_features_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_filter_features_streamFind <- function() {

  settings <- list(
    call = "filter_features",
    algorithm = "streamFind",
    parameters = list(
      "minIntensity" = 5000,
      "minSnRatio" = 25,
      "maxGroupSd" = 30,
      "blank" = 5,
      "minGroupAbundance" = 3
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

## annotate -----

#' @title settings_annotate_features_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @noRd
#'
settings_annotate_features_streamFind <- function(maxIsotopes = 5,
                                                  maxCharge = 1,
                                                  rtWindowAlignment = 0.2,
                                                  maxGaps = 1) {

  settings <- list(
    call = "annotate_features",
    algorithm = "streamFind",
    parameters = list(
      "maxIsotopes" = maxIsotopes,
      "maxCharge" = maxCharge,
      "rtWindowAlignment" = rtWindowAlignment,
      "maxGaps" = maxGaps
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
