
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






