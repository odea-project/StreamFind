

### fillingSettingsDefaultXCMS -----

#' @title fillingSettingsDefaultXCMS
#'
#' @return A \linkS4class{settings} object containing parameters for
#' recursive integration of peaks from analyses not represented in a given feature.
#'
#' @export
#'
fillingSettingsDefaultXCMS <- function() {

  return(
    new(
      "settings",
      call = "peakFilling",
      algorithm = "xcms",
      settings = list(
        xcms::ChromPeakAreaParam()
      )
    )
  )
}

#' @title peakFilling
#'
#' @description Recursive integration for filling missing peaks within each feature.
#'
#' @param object An \linkS4class{msData} object containing features.
#' @param settings A \linkS4class{settings} object with parameter for filling peaks.
#'
#'
#' @details The function \code{\link[xcms]{fillChromPeaks}}
#' from the \pkg{xcms} package can be used,
#' giving the respective parameters with the argument \code{settings}.
#' list with parameters according to the defined algorithm.
#' When the algorithm is set to \emph{xcms3}, the settings are the S4 class objects
#' \linkS4class{FillChromPeaksParam} or \linkS4class{ChromPeakAreaParam}.
#' See \code{?\link[xcms]{fillChromPeaks}} for more information.
#'
#' @return An \linkS4class{msData} object including filled missing peaks in analyses.
#'
#' @references
#' \insertRef{xcms01}{streamFind}
#' \insertRef{xcms02}{streamFind}
#' \insertRef{xcms03}{streamFind}
#'
#' @importMethodsFrom patRoon getXCMSnExp
#' @importFrom patRoon importFeatureGroupsXCMS3
#' @importMethodsFrom xcms fillChromPeaks
#' @importClassesFrom xcms ChromPeakAreaParam FillChromPeaksParam XCMSnExp
#' @importClassesFrom patRoon featuresSIRIUS featureGroupsSIRIUS featureGroups
#' @importFrom checkmate assertClass testClass
#'
peakFilling <- function(object, settings = NULL) {

  checkmate::assertClass(object, "msData")

  noPeaks <- sapply(object@analyses, function(x) nrow(x@peaks))
  noPeaks <- TRUE %in% (0 %in% noPeaks)

  noFeatures <- nrow(object@features@intensity) == 0

  if (noPeaks | noFeatures) {
    warning("Object does not have peaks/features!")
    return(object)
  }

  pat <- as.featureGroups(object)

  if (is.null(settings)) {
    prs <- getParameters(object, where = "features", call = "peakFilling")
    if (length(prs) > 0) {
      algorithm = getAlgorithm(prs)
      settings = getSettings(prs)
    } else {
      algorithm <- NA_character_
    }
  } else {
    prs <- fillingSettingsDefaultXCMS()
    algorithm = getAlgorithm(prs)
    settings = getSettings(prs)
  }

  if (is.na(algorithm)) {
    warning("Peak filling algorihtm not defined!")
    return(object)
  }

  if (algorithm == "xcms") {

    if (checkmate::testClass(settings, "list")) settings <- settings[[1]]

    Exp <- patRoon::getXCMSnExp(pat, loadRawData = TRUE)

    if (TRUE %in% hasAdjustedRetentionTime(object)) {
      adjRT <- lapply(object@analyses, function(x) {
        # TODO add check for rtAdjusted and change for normal rt when not there
        # the function loadBasicRawSpectraHeaderMZR() can be used to query the rt data if is not in analysis
        x@spectra$rtAdjusted
      })
      adjRT <- lapply(seq_len(length(adjRT)), function(x, adjRT) {
        temp <- adjRT[[x]]
        lenSeq <- seq_len(length(temp))
        names(temp) <- paste0(
          "F",
          x,
          ".S",
          sprintf(paste0("%0.", nchar(max(lenSeq)), "d"), lenSeq)
        )
        return(temp)
      }, adjRT = adjRT)

      names(adjRT) <- as.character(seq_len(length(adjRT)))

      xFD <- new("MsFeatureData")
      xFD$adjustedRtime <- adjRT
      xFD$chromPeakData <- Exp@msFeatureData$chromPeakData
      xFD$chromPeaks <- Exp@msFeatureData$chromPeaks
      xFD$featureDefinitions <- Exp@msFeatureData$featureDefinitions

      Exp@msFeatureData <- xFD
    }

    Exp <- xcms::fillChromPeaks(Exp, param = settings)

    pat <- patRoon::importFeatureGroupsXCMS3(Exp, analysisInfo(object))

    object <- buildPeaksTable(object, pat)

    object <- buildFeatures(object, pat)

  }

  object <- addParameters(object, where = "features", settings = prs)

  return(object)
}
