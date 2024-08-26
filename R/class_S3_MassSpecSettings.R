
# ______________________________________________________________________________________________________________________
# LoadFeatures -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_LoadFeaturesMS1_StreamFind
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
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_LoadFeaturesMS1_StreamFind.
#'
#' @export
#'
MassSpecSettings_LoadFeaturesMS1_StreamFind <- function(rtWindow = c(-2, 2),
                                                        mzWindow = c(-1, 6),
                                                        mzClust = 0.005,
                                                        presence = 0.8,
                                                        minIntensity = 250,
                                                        filtered = FALSE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "LoadFeaturesMS1",
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
validate.MassSpecSettings_LoadFeaturesMS1_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$call, "LoadFeaturesMS1"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_double(as.numeric(x$parameters$rtWindow), max.len = 2),
    checkmate::test_double(as.numeric(x$parameters$mzWindow), max.len = 2),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1)
  )
}



#' @title MassSpecSettings_LoadFeaturesMS2_StreamFind
#'
#' @description Settings for loading MS2 spectra for features.
#'
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_LoadFeaturesMS2_StreamFind.
#'
#' @export
#'
MassSpecSettings_LoadFeaturesMS2_StreamFind <- function(isolationWindow = 1.3,
                                                        mzClust = 0.005,
                                                        presence = 0.8,
                                                        minIntensity = 10,
                                                        filtered = FALSE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "LoadFeaturesMS2",
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
validate.MassSpecSettings_LoadFeaturesMS2_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "LoadFeaturesMS2"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$isolationWindow),
    checkmate::test_number(x$parameters$mzClust),
    checkmate::test_number(x$parameters$minIntensity),
    checkmate::test_logical(x$parameters$filtered, max.len = 1)
  )
}



#' @title MassSpecSettings_LoadFeaturesEIC_StreamFind
#'
#' @description Settings for loading spectra EIC for feature groups.
#'
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_LoadFeaturesEIC_StreamFind.
#'
#' @export
#'
MassSpecSettings_LoadFeaturesEIC_StreamFind <- function(rtExpand = 120, mzExpand = 0, filtered = FALSE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "LoadFeaturesEIC",
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
validate.MassSpecSettings_LoadFeaturesEIC_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "LoadFeaturesEIC"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$rtExpand),
    checkmate::test_number(x$parameters$mzExpand),
    checkmate::test_logical(x$parameters$filtered, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# LoadMSPeakLists -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_LoadMSPeakLists_patRoon
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
#' @param retainPrecursorMSMS For MS/MS data only: if TRUE then always
#' retain the precursor mass peak even if is not among the `topMost` peaks.
#' Note that MS precursor mass peaks are always kept. Furthermore, note
#' that precursor peaks in both MS and MS/MS data may still be removed by
#' intensity thresholds (this is unlike the filter method function).
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_LoadMSPeakLists_patRoon.
#'
#' @export
#'
MassSpecSettings_LoadMSPeakLists_patRoon <- function(maxMSRtWindow = 5,
                                                     precursorMzWindow = 4,
                                                     clusterMzWindow = 0.005,
                                                     topMost = 100,
                                                     minIntensityPre = 50,
                                                     minIntensityPost = 50,
                                                     avgFun = mean,
                                                     method = "hclust",
                                                     retainPrecursorMSMS = TRUE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "LoadMSPeakLists",
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
validate.MassSpecSettings_LoadMSPeakLists_patRoon <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "LoadMSPeakLists"),
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



#' @title MassSpecSettings_LoadMSPeakLists_StreamFind
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
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_LoadMSPeakLists_StreamFind.
#'
#' @export
#'
MassSpecSettings_LoadMSPeakLists_StreamFind <- function(clusterMzWindow = 0.005,
                                                        topMost = 100,
                                                        minIntensityPre = 50,
                                                        minIntensityPost = 50,
                                                        avgFun = mean,
                                                        method = "distance") {
  
  settings <- list(
    engine = "MassSpec",
    call = "LoadMSPeakLists",
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
validate.MassSpecSettings_LoadMSPeakLists_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "LoadMSPeakLists"),
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
# CentroidSpectra -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_CentroidSpectra_qCentroids
#'
#' @description Centroids profile spectra using the \href{https://link.springer.com/article/10.1007/s00216-022-04224-y}{qCentroids}
#' algorithm, which is part of the \href{https://github.com/odea-project/qAlgorithms}{qAlgorithms} library.
#'
#' @param maxScale Integer of length one. Maximum scale as integer (default is 5) for defining the scale limit for the peak model.
#' @param mode Integer of length one. `0` for debugging, `1` for silent mode (the default) and `2` for progress bar mode.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_CentroidSpectra_qCentroids.
#'
#' @references
#' \insertRef{qcentroids01}{StreamFind}
#'
#' @export
#'
MassSpecSettings_CentroidSpectra_qCentroids <- function(maxScale = 5, mode = 1) {
  
  settings <- list(
    engine = "MassSpec",
    call = "CentroidSpectra",
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
validate.MassSpecSettings_CentroidSpectra_qCentroids <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "CentroidSpectra"),
    checkmate::test_choice(x$algorithm, "qCentroids"),
    checkmate::test_int(x$parameters$maxScale),
    checkmate::test_int(x$parameters$mode)
  )
}





# ______________________________________________________________________________________________________________________
# BinSpectra -----
# ______________________________________________________________________________________________________________________


#' @title MassSpecSettings_BinSpectra_StreamFind
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
#' @returns A ProcessingSettings S3 class object with subclass MassSpecSettings_BinSpectra_StreamFind.
#'
#' @export 
#'
MassSpecSettings_BinSpectra_StreamFind <- function(unitsVal = NULL,
                                                   unitsNumber = NULL,
                                                   bins = NULL,
                                                   refBinAnalysis = NULL) {
  
  settings <- list(
    engine = "MassSpec",
    call = "BinSpectra",
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
validate.MassSpecSettings_BinSpectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "BinSpectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_character(x$parameters$unitsVal, len = 1, null.ok = TRUE),
    checkmate::test_integer(x$parameters$unitsNumber, len = 1, null.ok = TRUE),
    checkmate::test_list(x$parameters$bins, null.ok = TRUE),
    checkmate::test_integer(x$parameters$refBinAnalysis, len = 1, null.ok = TRUE)
  )
}

# ______________________________________________________________________________________________________________________
# SuspectScreening -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_SuspectScreening_StreamFind
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
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_SuspectScreening_StreamFind.
#'
#' @export
#'
MassSpecSettings_SuspectScreening_StreamFind <- function(database = NULL,
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
    engine = "MassSpec",
    call = "SuspectScreening",
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
validate.MassSpecSettings_SuspectScreening_StreamFind <- function(x) {
  
  x$parameters$database <- as.data.table(x$parameters$database)
  
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "SuspectScreening"),
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



#' @title MassSpecSettings_SuspectScreening_forident
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
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_SuspectScreening_forident.
#'
#' @export
#'
MassSpecSettings_SuspectScreening_forident <- function(addMS2 = FALSE,
                                                       useNeutralMass = TRUE,
                                                       path = getwd(),
                                                       name = "feature_list") {
  
  settings <- list(
    engine = "MassSpec",
    call = "SuspectScreening",
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
validate.MassSpecSettings_SuspectScreening_forident <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "SuspectScreening"),
    checkmate::test_choice(x$algorithm, "forident"),
    dir.exists(x$parameters$path),
    is.character(x$parameters$name),
    length(x$parameters$name) == 1,
    checkmate::test_logical(x$parameters$addMS2, max.len = 1)
  )
}



#' @title MassSpecSettings_SuspectScreening_patRoon
#'
#' @description Settings for performing suspect screening using the function \link[patRoon]{screenSuspects} from the patRoon R package.
#'
#' @param suspects A data.frame with suspect information. See section Suspect list format in \link[patRoon]{screenSuspects} for more information.
#' @param rtWindow The retention time window (in seconds) that will be used for matching a suspect (+/- feature data).
#' @param mzWindow The m/z window that will be used for matching a suspect (+/- feature data)..
#' @template arg-ms-filtered
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_SuspectScreening_patRoon.
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @export
#'
MassSpecSettings_SuspectScreening_patRoon <- function(suspects = NULL, rtWindow = 12, mzWindow = 0.005, filtered = FALSE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "SuspectScreening",
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
validate.MassSpecSettings_SuspectScreening_patRoon <- function(x) {
  
  x$parameters$database <- as.data.table(x$parameters$database)
  
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "SuspectScreening"),
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
# CalculateQuality -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_CalculateQuality_StreamFind
#'
#' @description Settings for calculating quality parameters of features (e.g., signal-to-noise (sn) ratio).
#'
#' @template arg-ms-rtExpand 
#' @template arg-ms-mzExpand 
#' @param minTraces Numeric of length 1 with the minimum number traces for calculating feature quality.
#' @template arg-ms-filtered 
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_CalculateQuality_StreamFind.
#'
#' @export
#'
MassSpecSettings_CalculateQuality_StreamFind <- function(rtExpand = 120, mzExpand = 0.0003, minTraces = 6, filtered = FALSE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "CalculateQuality",
    algorithm = "StreamFind",
    algorithm = "StreamFind",
    parameters = list(
      "rtExpand" = rtExpand,
      "mzExpand" = mzExpand,
      "minTraces" = minTraces,
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
validate.MassSpecSettings_CalculateQuality_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "CalculateQuality"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$rtExpand),
    checkmate::test_number(x$parameters$mzExpand),
    checkmate::test_number(x$parameters$minTraces),
    checkmate::test_logical(x$parameters$filtered, max.len = 1),
    checkmate::test_logical(x$parameters$runParallel, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# GenerateFormulas -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_GenerateFormulas_genform
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
#' @param thrMS Numeric (length 1) Sets the thresholds for the GenForm MS score (isoScore). Sets the thms command line 
#' options, respectively. Set to NULL for no threshold.
#' @param thrMSMS Numeric (length 1) Sets the thresholds for the GenForm MS/MS score (MSMSScore). Sets the thmsms 
#' command line options, respectively. Set to NULL for no threshold.
#' @param thrComb Numeric (length 1) Sets the thresholds for the GenForm combined score (combMatch). Sets the thcomb 
#' command line options, respectively. Set to NULL for no threshold.
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
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_GenerateFormulas_genform.
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
MassSpecSettings_GenerateFormulas_genform <- function(relMzDev = 5,
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
    engine = "MassSpec",
    call = "GenerateFormulas",
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
validate.MassSpecSettings_GenerateFormulas_genform <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "GenerateFormulas"),
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
# GenerateCompounds -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_GenerateCompounds_metfrag
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
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_GenerateCompounds_metfrag.
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
MassSpecSettings_GenerateCompounds_metfrag <- function(method = "CL",
                                                       timeout = 300,
                                                       timeoutRetries = 5,
                                                       errorRetries = 5,
                                                       topMost = 5,
                                                       dbRelMzDev = 8,
                                                       fragRelMzDev = 10,
                                                       fragAbsMzDev = 0.005,
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
    engine = "MassSpec",
    call = "GenerateCompounds",
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
validate.MassSpecSettings_GenerateCompounds_metfrag <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "GenerateCompounds"),
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
# IntegrateChromatograms -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_IntegrateChromatograms_StreamFind
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
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_IntegrateChromatograms_StreamFind.
#'
#' @export
#'
MassSpecSettings_IntegrateChromatograms_StreamFind <- function(merge = TRUE,
                                                               closeByThreshold = 45,
                                                               minPeakHeight = 0,
                                                               minPeakDistance = 10,
                                                               minPeakWidth = 5,
                                                               maxPeakWidth = 120,
                                                               minSN = 10) {
  
  settings <- list(
    engine = "MassSpec",
    call = "IntegrateChromatograms",
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
validate.MassSpecSettings_IntegrateChromatograms_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "IntegrateChromatograms"),
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
# CalculateSpectraCharges -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_CalculateSpectraCharges_StreamFind
#'
#' @description Calculates spectral charges from multi-charged compounds (e.g. proteins and monoclonal antibodies) for
#' mass deconvolution.
#' 
#' @param roundVal Numeric (length 1) with the rounding value for the m/z values before applying charge clustering.
#' @param relLowCut Numeric (length 1) with the relative low cut for the charge clustering.
#' @param absLowCut Numeric (length 1) with the absolute low cut for the charge clustering.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_CalculateSpectraCharges_StreamFind.
#'
#' @export
#'
MassSpecSettings_CalculateSpectraCharges_StreamFind <- function(roundVal = 35, relLowCut = 0.2, absLowCut = 300) {
  
  settings <- list(
    engine = "MassSpec",
    call = "CalculateSpectraCharges",
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
validate.MassSpecSettings_CalculateSpectraCharges_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "CalculateSpectraCharges"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$roundVal),
    checkmate::test_number(x$parameters$relLowCut),
    checkmate::test_number(x$parameters$absLowCut)
  )
}





# ______________________________________________________________________________________________________________________
# DeconvoluteSpectra -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_DeconvoluteSpectra_StreamFind
#'
#' @description Deconvolutes the spectral mass-to-charge ratio (\emph{m/z}) to mass (Da) after assignment of charges.
#' 
#' @param clustVal Numeric (length 1) with the clustering value for the charge deconvolution.
#' @param window Optional numeric (length 1) with the window in \emph{m/z} for collecting traces of a given charge.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_DeconvoluteSpectra_StreamFind.
#'
#' @export
#'
MassSpecSettings_DeconvoluteSpectra_StreamFind <- function(clustVal = 0.1, window = 20) {
  
  settings <- list(
    engine = "MassSpec",
    call = "DeconvoluteSpectra",
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
validate.MassSpecSettings_DeconvoluteSpectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "DeconvoluteSpectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$clustVal),
    checkmate::test_number(x$parameters$window, null.ok = TRUE)
  )
}

# ______________________________________________________________________________________________________________________
# AverageSpectra -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_AverageSpectra_StreamFind
#'
#' @description Averages spectra based on analysis replicate groups.
#' 
#' @param collapseTime Logical (length 1). When `TRUE` the spectra are averaged, reducing the time variable.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_AverageSpectra_StreamFind.
#'
#' @export
#'
MassSpecSettings_AverageSpectra_StreamFind <- function(collapseTime = FALSE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "AverageSpectra",
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
validate.MassSpecSettings_AverageSpectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "AverageSpectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_logical(x$parameters$collapseTime, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# ClusterSpectra -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_ClusterSpectra_StreamFind
#'
#' @description Clusters spectra based on a variable (i.e. column name).
#' 
#' @param val Character (length 1) with the variable to be used for clustering.
#' @param clustVal Numeric (length 1) with the clustering value.
#' @param presence Numeric (length 1) with the minimum presence of traces in a cluster to be considered.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_ClusterSpectra_StreamFind.
#'
#' @export
#'
MassSpecSettings_ClusterSpectra_StreamFind <- function(val = "mz", clustVal = 0.001, presence = 0.1) {
  
  settings <- list(
    engine = "MassSpec",
    call = "ClusterSpectra",
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
validate.MassSpecSettings_ClusterSpectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "ClusterSpectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_character(x$parameters$val, min.len = 1),
    checkmate::test_number(x$parameters$clustVal),
    checkmate::test_number(x$parameters$presence)
  )
}





# ______________________________________________________________________________________________________________________
# SubtractBlankSpectra -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_SubtractBlankSpectra_StreamFind
#'
#' @description Subtracts the blank spectra to each analysis according to the blank assignment.
#' 
#' @param negativeToZero Logical (length 1) indicating if negative values should be set to zero.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_SubtractBlankSpectra_StreamFind.
#'
#' @export
#'
MassSpecSettings_SubtractBlankSpectra_StreamFind <- function(negativeToZero = FALSE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "SubtractBlankSpectra",
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
validate.MassSpecSettings_SubtractBlankSpectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "SubtractBlankSpectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_logical(x$parameters$negativeToZero, max.len = 1)
  )
}





# ______________________________________________________________________________________________________________________
# CorrectChromatogramsBaseline -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_CorrectChromatogramsBaseline_baseline
#'
#' @description Performs baseline correction to chromatograms using the \pkg{baseline} package.
#' 
#' @param method Character (length 1) with the method to be used for baseline correction. Possible values are "als",
#' "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", "peakDetection", "rfbaseline", "rollingBall", "shirley" 
#' and "TAP".
#' @param args List with additional arguments for the selected method. See the documentation of the \pkg{baseline} package
#' for more details.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_CorrectChromatogramsBaseline_baseline.
#'
#' @export
#'
MassSpecSettings_CorrectChromatogramsBaseline_baseline <- function(method = "als", args = list(lambda = 5, p = 0.05, maxit = 10)) {
  
  settings <- list(
    engine = "MassSpec",
    call = "CorrectChromatogramsBaseline",
    algorithm = "baseline",
    parameters = list(
      method = method,
      args = args
    ),
    version = as.character(packageVersion("baseline")),
    software = "baseline",
    developer = "Kristian Hovde Liland",
    contact = "kristian.liland@nmbu.no",
    link = "https://github.com/khliland/baseline/",
    doi = "10.1366/000370210792434350"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.MassSpecSettings_CorrectChromatogramsBaseline_baseline <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "CorrectChromatogramsBaseline"),
    checkmate::test_choice(x$algorithm, "baseline"),
    checkmate::test_choice(x$parameters$method, c("als", "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", 
                                                  "peakDetection", "rfbaseline", "rollingBall", "shirley", "TAP")),
    checkmate::test_list(x$parameters$args, len = 1)
  )
}

#' @title MassSpecSettings_CorrectChromatogramsBaseline_airpls
#'
#' @description Performs baseline correction using adaptive iteratively reweighted Penalized Least Squares (airPLS) 
#' based on the algorithm from Zhi-Min Zhang.
#' 
#' @param lambda Numeric (length 1) with the smoothing intensity. the higher the `lambda` the higher the smoothing.
#' @param differences Integer (length 1) indicating the order of the difference of penalties
#' @param itermax Integer (length 1) with the maximum number of iterations.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_CorrectChromatogramsBaseline_airpls.
#' 
#' @references
#' 
#' \insertRef{airpls01}{StreamFind}
#'
#' @export
#'
MassSpecSettings_CorrectChromatogramsBaseline_airpls <- function(lambda = 10, differences = 1, itermax = 20) {
  
  settings <- list(
    engine = "MassSpec",
    call = "CorrectChromatogramsBaseline",
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
validate.MassSpecSettings_CorrectChromatogramsBaseline_airpls <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "CorrectChromatogramsBaseline"),
    checkmate::test_choice(x$algorithm, "airpls"),
    checkmate::test_number(x$parameters$lambda),
    checkmate::test_integer(as.integer(x$parameters$differences)),
    checkmate::test_integer(as.integer(x$parameters$itermax))
  )
}





# ______________________________________________________________________________________________________________________
# CorrectSpectraBaseline -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_CorrectSpectraBaseline_baseline
#'
#' @description Performs baseline correction to spectra using the \pkg{baseline} package.
#' 
#' @param method Character (length 1) with the method to be used for baseline correction. Possible values are "als",
#' "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", "peakDetection", "rfbaseline", "rollingBall", "shirley" 
#' and "TAP".
#' @param args List with additional arguments for the selected method. See the documentation of the \pkg{baseline} package
#' for more details.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_CorrectSpectraBaseline_baseline.
#'
#' @export
#'
MassSpecSettings_CorrectSpectraBaseline_baseline <- function(method = "als", args = list(lambda = 5, p = 0.05, maxit = 10)) {
  
  settings <- list(
    engine = "MassSpec",
    call = "CorrectSpectraBaseline",
    algorithm = "baseline",
    parameters = list(method = method, args = args),
    version = as.character(packageVersion("baseline")),
    software = "baseline",
    developer = "Kristian Hovde Liland",
    contact = "kristian.liland@nmbu.no",
    link = "https://github.com/khliland/baseline/",
    doi = "10.1366/000370210792434350"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.MassSpecSettings_CorrectSpectraBaseline_baseline <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "CorrectSpectraBaseline"),
    checkmate::test_choice(x$algorithm, "baseline"),
    checkmate::test_choice(x$parameters$method, c("als", "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", 
                                                  "peakDetection", "rfbaseline", "rollingBall", "shirley", "TAP")),
    checkmate::test_list(x$parameters$args, len = 1)
  )
}



#' @title MassSpecSettings_CorrectSpectraBaseline_airpls
#'
#' @description Performs baseline correction using adaptive iteratively reweighted Penalized Least Squares (airPLS) 
#' based on the algorithm from Zhi-Min Zhang.
#' 
#' @param lambda Numeric (length 1) with the smoothing intensity. the higher the `lambda` the higher the smoothing.
#' @param differences Integer (length 1) indicating the order of the difference of penalties
#' @param itermax Integer (length 1) with the maximum number of iterations.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_CorrectSpectraBaseline_airpls.
#' 
#' @references
#' 
#' \insertRef{airpls01}{StreamFind}
#'
#' @export
#'
MassSpecSettings_CorrectSpectraBaseline_airpls <- function(lambda = 10, differences = 1, itermax = 20) {
  
  settings <- list(
    engine = "MassSpec",
    call = "CorrectSpectraBaseline",
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
validate.MassSpecSettings_CorrectSpectraBaseline_airpls <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "CorrectSpectraBaseline"),
    checkmate::test_choice(x$algorithm, "airpls"),
    checkmate::test_number(x$parameters$lambda),
    checkmate::test_integer(as.integer(x$parameters$differences)),
    checkmate::test_integer(as.integer(x$parameters$itermax))
  )
}





# ______________________________________________________________________________________________________________________
# SmoothChromatograms -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_SmoothChromatograms_movingaverage
#'
#' @description Smooths chromatograms using the moving average algorithm.
#' 
#' @param windowSize Numeric (length 1) with the window size for the moving average.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_SmoothChromatograms_movingaverage.
#'
#' @export
#'
MassSpecSettings_SmoothChromatograms_movingaverage <- function(windowSize = 5) {
  
  settings <- list(
    engine = "MassSpec",
    call = "SmoothChromatograms",
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
validate.MassSpecSettings_SmoothChromatograms_movingaverage <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "SmoothChromatograms"),
    checkmate::test_choice(x$algorithm, "movingaverage"),
    checkmate::test_number(x$parameters$windowSize)
  )
}



#' @title MassSpecSettings_SmoothChromatograms_savgol
#'
#' @description Smooths chromatograms using the Savitzky-Golay algorithm from the \pkg{pracma} package.
#' 
#' @param fl Numeric (length 1) with the filter length (for instance fl = 51..151), has to be odd.
#' @param forder Numeric (length 1) with the order of the filter (2 = quadratic filter, 4 = quartic).
#' @param dorder Numeric (length 1) with the order of the derivative (0 = smoothing, 1 = first derivative, etc.).
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_SmoothChromatograms_savgol.
#'
#' @export
#'
MassSpecSettings_SmoothChromatograms_savgol <- function(fl = 11, forder = 4, dorder = 0) {
  
  settings <- list(
    engine = "MassSpec",
    call = "SmoothChromatograms",
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
validate.MassSpecSettings_SmoothChromatograms_savgol <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "SmoothChromatograms"),
    checkmate::test_choice(x$algorithm, "savgol"),
    checkmate::test_number(x$parameters$fl),
    checkmate::test_number(x$parameters$forder),
    checkmate::test_number(x$parameters$dorder)
  )
}

# ______________________________________________________________________________________________________________________
# SmoothSpectra -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_SmoothSpectra_movingaverage
#'
#' @description Smooths spectra using the moving average algorithm.
#' 
#' @param windowSize Numeric (length 1) with the window size for the moving average. 
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_SmoothSpectra_movingaverage.
#'
#' @export
#'
MassSpecSettings_SmoothSpectra_movingaverage <- function(windowSize = 5) {
  
  settings <- list(
    engine = "MassSpec",
    call = "SmoothSpectra",
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
validate.MassSpecSettings_SmoothSpectra_movingaverage <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "SmoothSpectra"),
    checkmate::test_choice(x$algorithm, "movingaverage"),
    checkmate::test_number(x$parameters$windowSize)
  )
}

#' @title MassSpecSettings_SmoothSpectra_savgol
#'
#' @description Smooths spectra using the Savitzky-Golay algorithm from the \pkg{pracma} package.
#' 
#' @param fl Numeric (length 1) with the filter length (for instance fl = 51..151), has to be odd.
#' @param forder Numeric (length 1) with the order of the filter (2 = quadratic filter, 4 = quartic).
#' @param dorder Numeric (length 1) with the order of the derivative (0 = smoothing, 1 = first derivative, etc.).
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_SmoothSpectra_savgol.
#'
#' @export
#'
MassSpecSettings_SmoothSpectra_savgol <- function(fl = 11, forder = 4, dorder = 0) {
  
  settings <- list(
    engine = "MassSpec",
    call = "SmoothSpectra",
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
validate.MassSpecSettings_SmoothSpectra_savgol <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "SmoothSpectra"),
    checkmate::test_choice(x$algorithm, "savgol"),
    checkmate::test_number(x$parameters$fl),
    checkmate::test_number(x$parameters$forder),
    checkmate::test_number(x$parameters$dorder)
  )
}





# ______________________________________________________________________________________________________________________
# NormalizeSpectra -----
# ______________________________________________________________________________________________________________________

#' @title MassSpecSettings_NormalizeSpectra_minmax
#'
#' @description Normalizes spectra using the min-max algorithm.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_NormalizeSpectra_minmax.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_minmax <- function() {
  
  settings <- list(
    engine = "MassSpec",
    call = "NormalizeSpectra",
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
validate.MassSpecSettings_NormalizeSpectra_minmax <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "minmax")
  )
}



#' @title MassSpecSettings_NormalizeSpectra_snv
#'
#' @description Normalizes spectra using the Standard Normal Variate (SNV) algorithm.
#' 
#' @param liftTozero Logical (length 1) indicating if the spectra should be lifted to zero.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_NormalizeSpectra_snv.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_snv <- function(liftTozero = FALSE) {
  
  settings <- list(
    engine = "MassSpec",
    call = "NormalizeSpectra",
    algorithm = "snv",
    parameters = list(liftTozero = liftTozero),
    version = NA_character_,
    software = NA_character_,
    developer = "J\u00FCrgen Schram",
    contact = "schram@hsnr.de",
    link = NA_character_,
    doi = "10.1016/j.trac.2018.12.004"
  )
  
  as.ProcessingSettings(settings)
}

#' @export
#' @noRd
#'
validate.MassSpecSettings_NormalizeSpectra_snv <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "snv"),
    checkmate::test_logical(x$parameters$liftTozero, max.len = 1)
  )
}



#' @title MassSpecSettings_NormalizeSpectra_scale
#'
#' @description Normalizes spectra using scaling based on the standard deviation.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_NormalizeSpectra_scale.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_scale <- function() {
  
  settings <- list(
    engine = "MassSpec",
    call = "NormalizeSpectra",
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
validate.MassSpecSettings_NormalizeSpectra_scale <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "scale")
  )
}



#' @title MassSpecSettings_NormalizeSpectra_blockweight
#'
#' @description Normalizes spectra using block weighting for downstream data evaluation.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_NormalizeSpectra_blockweight.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_blockweight <- function() {
  
  settings <- list(
    engine = "MassSpec",
    call = "NormalizeSpectra",
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
validate.MassSpecSettings_NormalizeSpectra_blockweight <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "blockweight")
  )
}



#' @title MassSpecSettings_NormalizeSpectra_meancenter
#'
#' @description Normalizes spectra using mean centering.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_NormalizeSpectra_meancenter.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_meancenter <- function() {
  
  settings <- list(
    engine = "MassSpec",
    call = "NormalizeSpectra",
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
validate.MassSpecSettings_NormalizeSpectra_meancenter <- function(x) {
  all(
    checkmate::test_choice(x$engine, "MassSpec"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "meancenter")
  )
}
