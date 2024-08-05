
# ______________________________________________________________________________________________________________________
# BinSpectra -----
# ______________________________________________________________________________________________________________________


#' @title RamanSettings_BinSpectra_StreamFind
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
#' @returns A ProcessingSettings S3 class object with subclass RamanSettings_BinSpectra_StreamFind.
#'
#' @export 
#'
RamanSettings_BinSpectra_StreamFind <- function(unitsVal = NULL,
                                                   unitsNumber = NULL,
                                                   bins = NULL,
                                                   refBinAnalysis = NULL) {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_BinSpectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "BinSpectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_character(x$parameters$unitsVal, len = 1, null.ok = TRUE),
    checkmate::test_integer(x$parameters$unitsNumber, len = 1, null.ok = TRUE),
    checkmate::test_list(x$parameters$bins, null.ok = TRUE),
    checkmate::test_integer(x$parameters$refBinAnalysis, len = 1, null.ok = TRUE)
  )
}






# ______________________________________________________________________________________________________________________
# MergeSpectraTimeSeries -----
# ______________________________________________________________________________________________________________________

#' @title RamanSettings_MergeSpectraTimeSeries_StreamFind
#'
#' @description Merges Raman spectra based on time series data. It collapses data files into a single file.
#' 
#' @param preCut The number of pre Raman scans to exclude when merging.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_MergeSpectraTimeSeries_StreamFind.
#'
#' @export
#'
RamanSettings_MergeSpectraTimeSeries_StreamFind <- function(preCut = 2) {
  
  settings <- list(
    engine = "Raman",
    call = "MergeSpectraTimeSeries",
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
validate.RamanSettings_MergeSpectraTimeSeries_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "MergeSpectraTimeSeries"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_number(x$parameters$preCut)
  )
}





# ______________________________________________________________________________________________________________________
# AverageSpectra -----
# ______________________________________________________________________________________________________________________

#' @title RamanSettings_AverageSpectra_StreamFind
#'
#' @description Averages spectra based on analysis replicate groups.
#' 
#' @param collapseTime Logical (length 1). When `TRUE` the spectra are averaged, reducing the time variable.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_AverageSpectra_StreamFind.
#'
#' @export
#'
RamanSettings_AverageSpectra_StreamFind <- function(collapseTime = FALSE) {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_AverageSpectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "AverageSpectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_logical(x$parameters$collapseTime, max.len = 1)
  )
}






# ______________________________________________________________________________________________________________________
# SubtractBlankSpectra -----
# ______________________________________________________________________________________________________________________

#' @title RamanSettings_SubtractBlankSpectra_StreamFind
#'
#' @description Subtracts the blank spectra to each analysis according to the blank assignment.
#' 
#' @param negativeToZero Logical (length 1) indicating if negative values should be set to zero.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_SubtractBlankSpectra_StreamFind.
#'
#' @export
#'
RamanSettings_SubtractBlankSpectra_StreamFind <- function(negativeToZero = FALSE) {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_SubtractBlankSpectra_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "SubtractBlankSpectra"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_logical(x$parameters$negativeToZero, max.len = 1)
  )
}






# ______________________________________________________________________________________________________________________
# CorrectSpectraBaseline -----
# ______________________________________________________________________________________________________________________

#' @title RamanSettings_CorrectSpectraBaseline_baseline
#'
#' @description Performs baseline correction to spectra using the \pkg{baseline} package.
#' 
#' @param method Character (length 1) with the method to be used for baseline correction. Possible values are "als",
#' "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", "peakDetection", "rfbaseline", "rollingBall", "shirley" 
#' and "TAP".
#' @param args List with additional arguments for the selected method. See the documentation of the \pkg{baseline} package
#' for more details.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_CorrectSpectraBaseline_baseline.
#'
#' @export
#'
RamanSettings_CorrectSpectraBaseline_baseline <- function(method = "als", args = list(lambda = 5, p = 0.05, maxit = 10)) {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_CorrectSpectraBaseline_baseline <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "CorrectSpectraBaseline"),
    checkmate::test_choice(x$algorithm, "baseline"),
    checkmate::test_choice(x$parameters$method, c("als", "fillPeaks", "irls", "lowpass", "medianWindow", "modpolyfit", 
                                                  "peakDetection", "rfbaseline", "rollingBall", "shirley", "TAP")),
    checkmate::test_list(x$parameters$args, len = 1)
  )
}



#' @title RamanSettings_CorrectSpectraBaseline_airpls
#'
#' @description Performs baseline correction using adaptive iteratively reweighted Penalized Least Squares (airPLS) 
#' based on the algorithm from Zhi-Min Zhang.
#' 
#' @param lambda Numeric (length 1) with the smoothing intensity. the higher the `lambda` the higher the smoothing.
#' @param differences Integer (length 1) indicating the order of the difference of penalties
#' @param itermax Integer (length 1) with the maximum number of iterations.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_CorrectSpectraBaseline_airpls.
#' 
#' @references
#' 
#' \insertRef{airpls01}{StreamFind}
#'
#' @export
#'
RamanSettings_CorrectSpectraBaseline_airpls <- function(lambda = 10, differences = 1, itermax = 20) {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_CorrectSpectraBaseline_airpls <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "CorrectSpectraBaseline"),
    checkmate::test_choice(x$algorithm, "airpls"),
    checkmate::test_number(x$parameters$lambda),
    checkmate::test_integer(as.integer(x$parameters$differences)),
    checkmate::test_integer(as.integer(x$parameters$itermax))
  )
}






# ______________________________________________________________________________________________________________________
# SubtractSpectraSection -----
# ______________________________________________________________________________________________________________________

#' @title RamanSettings_SubtractSpectraSection_StreamFind
#'
#' @description Subtracts a section of the spectra based on a variable (i.e. column name).
#' 
#' @param sectionVal Character (length 1) with the variable to be used for sectioning.
#' @param sectionWindow Numeric (length 2) with the window for the sectioning.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_SubtractSpectraSection_StreamFind.
#'
#' @export
#'
RamanSettings_SubtractSpectraSection_StreamFind <- function(sectionVal = "rt", sectionWindow = c(10, 200)) {
  
  settings <- list(
    engine = "Raman",
    call = "SubtractSpectraSection",
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
validate.RamanSettings_SubtractSpectraSection_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "SubtractSpectraSection"),
    checkmate::test_choice(x$algorithm, "StreamFind"),
    checkmate::test_character(x$parameters$sectionVal, min.len = 1),
    checkmate::test_numeric(x$parameters$sectionWindow, len = 2)
  )
}





# ______________________________________________________________________________________________________________________
# DeleteSpectraSection -----
# ______________________________________________________________________________________________________________________

#' @title RamanSettings_DeleteSpectraSection_StreamFind
#'
#' @description Deletes a section of the spectra based on a named list of data ranges for a given variable (i.e. column name).
#' 
#' @param section Named list with the variable to be used for sectioning and the window for the sectioning. The names 
#' should match column names in the data.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_DeleteSpectraSection_StreamFind.
#'
#' @export
#'
RamanSettings_DeleteSpectraSection_StreamFind <- function(section = list()) {
  
  settings <- list(
    engine = "Raman",
    call = "DeleteSpectraSection",
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
validate.RamanSettings_DeleteSpectraSection_StreamFind <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "DeleteSpectraSection"),
    checkmate::test_choice(x$algorithm, "StreamFind")
    # TODO add section checks in validation of RamanSettings_DeleteSpectraSection_StreamFind
  )
}






# ______________________________________________________________________________________________________________________
# SmoothSpectra -----
# ______________________________________________________________________________________________________________________

#' @title RamanSettings_SmoothSpectra_movingaverage
#'
#' @description Smooths spectra using the moving average algorithm.
#' 
#' @param windowSize Numeric (length 1) with the window size for the moving average. 
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_SmoothSpectra_movingaverage.
#'
#' @export
#'
RamanSettings_SmoothSpectra_movingaverage <- function(windowSize = 5) {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_SmoothSpectra_movingaverage <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "SmoothSpectra"),
    checkmate::test_choice(x$algorithm, "movingaverage"),
    checkmate::test_number(x$parameters$windowSize)
  )
}

#' @title RamanSettings_SmoothSpectra_savgol
#'
#' @description Smooths spectra using the Savitzky-Golay algorithm from the \pkg{pracma} package.
#' 
#' @param fl Numeric (length 1) with the filter length (for instance fl = 51..151), has to be odd.
#' @param forder Numeric (length 1) with the order of the filter (2 = quadratic filter, 4 = quartic).
#' @param dorder Numeric (length 1) with the order of the derivative (0 = smoothing, 1 = first derivative, etc.).
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_SmoothSpectra_savgol.
#'
#' @export
#'
RamanSettings_SmoothSpectra_savgol <- function(fl = 11, forder = 4, dorder = 0) {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_SmoothSpectra_savgol <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
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

#' @title RamanSettings_NormalizeSpectra_minmax
#'
#' @description Normalizes spectra using the min-max algorithm.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_NormalizeSpectra_minmax.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_minmax <- function() {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_NormalizeSpectra_minmax <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "minmax")
  )
}



#' @title RamanSettings_NormalizeSpectra_snv
#'
#' @description Normalizes spectra using the Standard Normal Variate (SNV) algorithm.
#' 
#' @param liftTozero Logical (length 1) indicating if the spectra should be lifted to zero.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_NormalizeSpectra_snv.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_snv <- function(liftTozero = FALSE) {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_NormalizeSpectra_snv <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "snv"),
    checkmate::test_logical(x$parameters$liftTozero, max.len = 1)
  )
}



#' @title RamanSettings_NormalizeSpectra_scale
#'
#' @description Normalizes spectra using scaling based on the standard deviation.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_NormalizeSpectra_scale.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_scale <- function() {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_NormalizeSpectra_scale <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "scale")
  )
}



#' @title RamanSettings_NormalizeSpectra_blockweight
#'
#' @description Normalizes spectra using block weighting for downstream data evaluation.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_NormalizeSpectra_blockweight.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_blockweight <- function() {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_NormalizeSpectra_blockweight <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "blockweight")
  )
}



#' @title RamanSettings_NormalizeSpectra_meancenter
#'
#' @description Normalizes spectra using mean centering.
#'
#' @return A ProcessingSettings S3 class object with subclass RamanSettings_NormalizeSpectra_meancenter.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_meancenter <- function() {
  
  settings <- list(
    engine = "Raman",
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
validate.RamanSettings_NormalizeSpectra_meancenter <- function(x) {
  all(
    checkmate::test_choice(x$engine, "Raman"),
    checkmate::test_choice(x$call, "NormalizeSpectra"),
    checkmate::test_choice(x$algorithm, "meancenter")
  )
}
