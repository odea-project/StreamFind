

#' @title peakPicking
#'
#' @description Finds chromatographic peaks from MS data in an \linkS4class{msAnalysis}
#' object or in MS analyses a given \linkS4class{msData} object.
#' The peak picking uses the \pkg{patRoon} package for data processing.
#'
#' @param object An \linkS4class{msData} or \linkS4class{msAnalysis} object.
#' @template args-single-settings
#'
#' @details See \link[patRoon]{findFeatures} for more information.
#' the following algorithms are available:
#' "xcms3", "xcms", "openms", "envipick", "sirius", "kpic2", "safd".
#' The parameters depend on the algorithm chosen.
#' See ?\pkg{patRoon} for further information.
#'
#' @return An \linkS4class{msAnalysis}/\linkS4class{msData} object
#' with peaks added.
#'
#' @seealso \link[patRoon]{findFeatures}
#'
#' @references
#' \insertRef{patroon01}{streamFind}
#'
#' @export
#'
#' @importFrom checkmate testClass
#' @importClassesFrom patRoon features
#' @importFrom patRoon findFeatures featureTable
#' @importFrom dplyr left_join
#'
peakPicking <- function(object = NULL, settings = NULL) {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msAnalysis"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(object)
  }

  #check in object for parameters
  if (is.null(settings)) {

    prs <- getParameters(object, call = "peakPicking")
    prs[sapply(prs, is.null)] <- NULL #remove NULLs from search

    if (length(unique(prs)) == 1 & length(prs) > 0) {
      prs <- unique(prs)
      algorithm <- getAlgorithm(prs[[1]])#all equal or samples with NULL, takes from the first first
      params <- getSettings(prs[[1]])

    } else if (length(prs) > 0) {
      algorithm <- sapply(prs, function(x) getAlgorithm(x)) #different in samples

      if (length(algorithm) != length(analyses(object))) {
        warning(paste0("Settings not present for the analyses: ",
          paste(analyses(object)[!analyses(object) %in% names(algorithm)], collapse = "; ")))
        return(object)
      }
    }
  } else if (checkmate::testClass(settings, "settings")) {

    algorithm <- getAlgorithm(settings)
    params <- getSettings(settings)

  } else {

    algorithm <- NA_character_
  }


  if (TRUE %in% is.null(algorithm)) {
    warning("Peak picking algorihtm not defined!")
    return(object)
  }


  sinfo <- analysisInfo(object)
  sinfo$blank[is.na(sinfo$blank)] <- ""


  if (length(algorithm) == 1) {

    sinfo$algorithm <- algorithm
    ag <- list(analysisInfo = sinfo, algorithm = algorithm)
    pat <- do.call("findFeatures", c(ag, params, verbose = TRUE))

    stgs <- createSettings(call = "peakPicking", algorithm = algorithm, settings = params)

    object <- addParameters(object, stgs)


  } else {

    params <- sapply(prs, function(x) getSettings(x))
    sinfo$algorithm <- algorithm

    sinfo <- split(sinfo, f = sinfo$algorithm)

    #check if parameter settings are different in each algorithm
    sinfo <- lapply(sinfo, function(x, params) {
      stgs_temp <- unique(settings[x$analysis])
      if (length(stgs_temp) > 1) x <- split(x, f = x$analysis)
      return(x)
    }, params = params)

    sinfo_new <- list()

    for (l in seq_len(length(sinfo))) {
      if (!is.data.frame(sinfo[[l]])) {
        sinfo_new <- c(sinfo_new, sinfo[[l]])
      } else {
        sinfo_new <- c(sinfo_new, sinfo[l])
      }
    }

    pat_list <- lapply(sinfo_new, function(x, params) {
      par_tmp <- unique(params[x$analysis])[[1]]
      ags <- list(analysisInfo = x, algorithm = unique(x$algorithm))
      pat_temp <- do.call("findFeatures", c(ags, par_tmp, verbose = TRUE))
      return(pat_temp)
    }, params = params)


    #bring all to one features object
    combAnaInfo <-  rbindlist(lapply(pat_list, function(x) x@analysisInfo))
    combAnaInfo <- combAnaInfo[order(combAnaInfo$analysis), ]

    combFeatures <- lapply(pat_list, function(x) x@features)
    combFeatures_new <- list()
    for (cf in seq_len(length(combFeatures))) {
      if (!is.data.frame(combFeatures[[cf]])) {
        combFeatures_new <- c(combFeatures_new, combFeatures[[cf]])
      } else {
        combFeatures_new <- c(combFeatures_new, combFeatures[cf])
      }
    }

    combFeatures_new <- combFeatures_new[order(names(combFeatures_new))]

    pat <- new("featuresSIRIUS", analysisInfo = combAnaInfo, features = combFeatures_new)
  }


  object <- buildPeaksTable(object, pat)

  validObject(object)

  return(object)
}


#' @title buildPeaksTable
#'
#' @param object A \linkS4class{msData} or \linkS4class{msAnalysis} object.
#' @param pat A \linkS4class{features} or \linkS4class{featureGroups} object from the package \pkg{patRoon}.
#'
#' @return A \link[data.table]{data.table} containing
#' information for peaks for each sample.
#'
#' @importClassesFrom patRoon features
#' @importFrom dplyr select
#' @importMethodsFrom xcms chromPeaks
#' @importFrom data.table rbindlist as.data.table setnames setorder
#' @importFrom checkmate testClass
#'
buildPeaksTable <- function(object, pat) {

  cat("Building peaks table... ")

  valid <- FALSE

  if (checkmate::testClass(pat, "features")) {
    valid <- TRUE
    peaks <- pat@features
    anaInfo <- pat@analysisInfo
  }

  if (checkmate::testClass(pat, "featureGroups")) {
    valid <- TRUE
    peaks <- pat@features@features
    anaInfo <- pat@analysisInfo
  }


  if (!valid) warning("Peaks table not build as the input from peak picking
                      is not a features or featureGroups class object!")

  if (checkmate::testClass(pat, "featuresXCMS3") | checkmate::testClass(pat, "featureGroupsXCMS3")) {
    if (xcms::hasFilledChromPeaks(pat@xdata)) {
      extra <- as.data.table(xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE))
      extra[, analysis := anaInfo$analysis[extra$sample]]
      extra[, analysis := factor(analysis, levels = anaInfo$analysis)]
      extra <- split(extra, extra$analysis)

      # extra <- as.data.table(xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE))
      # #extra <- setnames(extra, c("maxo", "into"), c("intensity", "area"))
      # extra[, analysis := anaInfo$analysis[extra$sample]]
      # extra[, analysis := factor(analysis, levels = anaInfo$analysis)]
      # extra <- split(extra, extra$analysis)

      for (a in names(peaks)) {
        if (nrow(peaks[[a]]) == nrow(extra[[a]]) & all(peaks[[a]]$mz == extra[[a]]$mz)) {
          #newCols <- colnames(extra)[!colnames(extra) %in% colnames(peaks)]
          #peaks <- cbind(peaks, extra[, newCols, with = FALSE])
          #peaks[, sample := NULL]
          peaks[[a]][, is_filled := extra[[a]]$is_filled]
        }
      }
    }
  }

  peaks <- lapply(peaks, function(x, object) {
    temp <- copy(x)
    setnames(temp, c("ID", "ret", "retmin", "retmax"), c("id", "rt", "rtmin", "rtmax"), skip_absent = TRUE)
    setnames(temp, "group", "feature", skip_absent = TRUE)
    temp[, dppm := round((mzmax - mzmin) / mzmin * 1E6, digits = 0)]
    temp[, drt := round(rtmax - rtmin, digits = 0)]

    temp <- temp[order(mz), ]
    temp <- temp[order(rt), ]

    temp$index <- seq_len(nrow(temp))

    temp <- select(
      temp,
      id,
      index,
      rt, mz,
      intensity, area,
      drt, rtmin, rtmax,
      dppm, mzmin, mzmax,
      everything()
    )

    temp$id <- paste0(
      "m",
      round(temp$mz, digits = 3),
      "_d",
      temp$dppm,
      "_r",
      round(temp$rt, digits = 0),
      "_t",
      temp$drt,
      "_p",
      temp$index
    )

    return(temp)
  })


  if (checkmate::testClass(object, "msAnalysis")) {
    object@peaks <- copy(peaks[[1]])

  } else {
    object@analyses <- lapply(analyses(object), function(x, object, peaks) {
      temp <- object@analyses[[x]]
      temp@peaks <- copy(peaks[[x]])
      return(temp)
    }, object = object, peaks = peaks)
  }

  # TODO implement multiple polarities to amend the peak id accordingly
  # pols <- polarities(object)
  # peaks$adduct <- NA_character_
  # for (anl in analyses(object)) {
  #   if ("positive" %in% pols[anl]) peaks[analysis %in% anl, adduct := "[M+H]+"]
  #   if ("negative" %in% pols[anl]) peaks[analysis %in% anl, adduct := "[M-H]-"]
  #
  #   #TODO implement polarity switching when both is output of polarities
  # }

  cat("Done! \n")
  return(object)
}
