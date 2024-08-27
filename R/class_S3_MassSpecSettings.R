
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
