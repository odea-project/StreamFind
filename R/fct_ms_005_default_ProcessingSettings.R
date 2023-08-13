
# Default Settings -----

#' @title get_default_ProcessingSettings
#'
#' @description X.
#'
#' @param call X.
#' @param algorithm X.
#'
#' @return A ProcessingSettings S3 class object.
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

## find_features -----

#' @title Settings_find_features_xcms3_centwave
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_find_features_xcms3_centwave <- function() {

  if (!requireNamespace("xcms", quietly = TRUE)) {
    warning("xcms package required but not installed!")
    return(NULL)
  }

  settings <- list(
    call = "find_features",
    algorithm = "xcms3_centwave",
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

#' @title Settings_find_features_openms
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_find_features_openms <- function() {

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

#' @title Settings_find_features_kpic2
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_find_features_kpic2 <- function() {

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

## group_features -----

#' @title Settings_group_features_xcms3_peakdensity
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_group_features_xcms3_peakdensity <- function() {

  if (!requireNamespace("xcms", quietly = TRUE)) {
    warning("xcms package required but not installed!")
    return(NULL)
  }

  settings <- list(
    call = "group_features",
    algorithm = "xcms3_peakdensity",
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

## load_features -----

#' @title Settings_load_features_ms1_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_load_features_ms1_streamFind <- function() {

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
    checkmate::test_double(x$parameters$mzClust, max.len = 1),
    checkmate::test_double(x$parameters$minIntensity, max.len = 1),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

#' @title Settings_load_features_ms2_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_load_features_ms2_streamFind <- function() {

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
    checkmate::test_double(x$parameters$isolationWindow, max.len = 1),
    checkmate::test_double(x$parameters$mzClust, max.len = 1),
    checkmate::test_double(x$parameters$minIntensity, max.len = 1),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

#' @title Settings_load_groups_ms1_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_load_groups_ms1_streamFind <- function() {

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
    checkmate::test_double(x$parameters$mzClust, max.len = 1),
    checkmate::test_double(x$parameters$minIntensity, max.len = 1),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

#' @title Settings_load_groups_ms2_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_load_groups_ms2_streamFind <- function() {

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
    checkmate::test_double(x$parameters$mzClust, max.len = 1),
    checkmate::test_double(x$parameters$minIntensity, max.len = 1),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1),
    checkmate::test_logical(x$parameters$verbose, max.len = 1)
  )
}

## filter_features -----

#' Settings_filter_features_streamFind
#'
#' @description X.
#'
#' @return X.
#'
#' @export
#'
Settings_filter_features_streamFind <- function(
    minIntensity = 5000,
    minSnRatio = 25,
    maxGroupSd = 30,
    blank = 5,
    minGroupAbundance = 3,
    excludeIsotopes = TRUE) {

  checkmate::assert_double(minIntensity, max.len = 1)
  checkmate::assert_double(minSnRatio, max.len = 1)
  checkmate::assert_double(maxGroupSd, max.len = 1)
  checkmate::assert_double(blank, max.len = 1)
  checkmate::assert_count(minGroupAbundance)
  checkmate::assert_logical(excludeIsotopes, max.len = 1)

  settings <- list(
    call = "filter_features",
    algorithm = "streamFind",
    parameters = list(
      "minIntensity" = minIntensity,
      "minSnRatio" = minSnRatio,
      "maxGroupSd" = maxGroupSd,
      "blank" = blank,
      "minGroupAbundance" = minGroupAbundance,
      "excludeIsotopes" = excludeIsotopes
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

#' @describeIn Settings_filter_features_streamFind
#' Validates the Settings_filter_features_streamFind S3 class object, returning
#' a logical value of length one.
#'
#' @param x A Settings_filter_features_streamFind S3 class object.
#'
#' @export
#'
validate.Settings_filter_features_streamFind <- function(x) {
  all(
  checkmate::test_choice(x$call, "filter_features"),
  checkmate::test_choice(x$algorithm, "streamFind"),
  checkmate::test_double(x$parameters$minIntensity, max.len = 1),
  checkmate::test_double(x$parameters$minSnRatio, max.len = 1),
  checkmate::test_double(x$parameters$maxGroupSd, max.len = 1),
  checkmate::test_double(x$parameters$blank, max.len = 1),
  checkmate::test_count(x$parameters$minGroupAbundance),
  checkmate::test_logical(x$parameters$excludeIsotopes, max.len = 1)
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
  checkmate::assert_double(rtWindowAlignment, max.len = 1)
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
