

#' peakPicking
#'
#' @description Finds chromatographic peaks from centroided MS data in the
#' analyses of a given \linkS4class{msData} object.
#' The peak picking uses the \pkg{patRoon} package and
#' the following algorithms are available:
#' "xcms3", "xcms", "openms", "envipick", "sirius", "kpic2", "safd".
#' The parameters depend on the algorithm chosen.
#' See ?\pkg{patRoon} for further information.
#'
#' @param object An \linkS4class{msData} or \linkS4class{msAnalysis} object.
#' @param algorithm A character string with the algorithm to use for peak picking.
#' @param settings List of parameter settings for the specified algorithm.
#' See \link[patRoon]{findFeatures} for more information.
#'
#' @return An \linkS4class{msData} object with peaks added per analyses.
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
peakPicking <- function(object = NULL,
                        algorithm = NA_character_,
                        settings = NULL) {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msAnalysis"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(object)
  }

  #check in object for parameters
  if (is.na(algorithm)) {

    prs <- getParameters(object, call = "peakPicking")
    prs[sapply(prs, is.null)] <- NULL #remove NULLs from search

    if (length(unique(prs)) == 1 & length(prs) > 0) {
      prs <- unique(prs)
      algorithm <- getAlgorithm(prs[[1]])#all equal or samples with NULL, takes from the first first
      settings <- getSettings(prs[[1]])

    } else if (length(prs) > 0) {
      algorithm <- sapply(prs, function(x) getAlgorithm(x)) #different in samples

      if (length(algorithm) != length(analyses(object))) {
        warning(paste0("Settings not present for the analyses: ",
          paste(analyses(object)[!analyses(object) %in% names(algorithm)], collapse = "; ")))
        return(object)
      }
    }
  }

  if (TRUE %in% is.na(algorithm)) {
    warning("Peak picking algorihtm not defined!")
    return(object)
  }


  sinfo <- analysisInfo(object)
  sinfo$blank[is.na(sinfo$blank)] <- ""


  if (length(algorithm) == 1) {

    sinfo$algorithm <- algorithm
    ag <- list(analysisInfo = sinfo, algorithm = algorithm)
    pat <- do.call("findFeatures", c(ag, settings, verbose = TRUE))

    stgs <- createSettings(call = "peakPicking", algorithm = algorithm, settings = settings)
    object <- addParameters(object, stgs)


  } else {

    settings <- sapply(prs, function(x) getSettings(x))
    sinfo$algorithm <- algorithm

    sinfo <- split(sinfo, f = sinfo$algorithm)

    #check if parameter settings are different in each algorithm
    sinfo <- lapply(sinfo, function(x, settings) {
      stgs_temp <- unique(settings[x$analysis])
      if (length(stgs_temp) > 1) x <- split(x, f = x$analysis)
      return(x)
    }, settings = settings)

    sinfo_new <- list()

    for (l in seq_len(length(sinfo))) {
      if (!is.data.frame(sinfo[[l]])) {
        sinfo_new <- c(sinfo_new, sinfo[[l]])
      } else {
        sinfo_new <- c(sinfo_new, sinfo[l])
      }
    }

    pat_list <- lapply(sinfo_new, function(x, settings) {
      stgs <- unique(settings[x$analysis])[[1]]
      ags <- list(analysisInfo = x, algorithm = unique(x$algorithm))
      pat_temp <- do.call("findFeatures", c(ags, stgs, verbose = TRUE))
      return(pat_temp)
    }, settings = settings)


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


  peaks <- rbindlist(peaks, idcol = "analysis", fill = TRUE)

  peaks <- setnames(
    peaks,
    c("ID", "ret", "retmin", "retmax"),
    c("id", "rt", "rtmin", "rtmax"),
  )

  # if (checkmate::testClass(pat, "featuresXCMS3") | checkmate::testClass(pat, "featureGroupsXCMS3")) {
  #   extra <- as.data.table(xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE))
  #   extra <- setnames(extra, c("maxo", "into"), c("intensity", "area"))
  #   extra[, analysis := anaInfo$analysis[extra$sample]]
  #   extra[, analysis := factor(analysis, levels = anaInfo$analysis)]
  #   setorder(extra, analysis)
  #
  #   if (nrow(peaks) == nrow(extra) & all(peaks$mz == extra$mz)) {
  #     newCols <- colnames(extra)[!colnames(extra) %in% colnames(peaks)]
  #     peaks <- cbind(peaks, extra[, newCols, with = FALSE])
  #     peaks[, sample := NULL]
  #   }
  # }

  peaks <- setnames(peaks, "group", "feature", skip_absent = TRUE)

  rpl <- anaInfo$group
  names(rpl) <- anaInfo$analysis
  peaks[, replicate := rpl[peaks$analysis]][]
  peaks[, dppm := round((mzmax - mzmin) / mzmin * 1E6, digits = 0)]
  peaks[, drt := round(rtmax - rtmin, digits = 0)]
  peaks$index <- seq_len(nrow(peaks))

  peaks$id <- paste0(
    "m",
    round(peaks$mz, digits = 3),
    "_d",
    peaks$dppm,
    "_r",
    round(peaks$rt, digits = 0),
    "_t",
    peaks$drt,
    "_p",
    peaks$index
  )

  # TODO implement multiple polarities to ammend the peak id accordingly
  # pols <- polarities(object)
  # peaks$adduct <- NA_character_
  # for (anl in analyses(object)) {
  #   if ("positive" %in% pols[anl]) peaks[analysis %in% anl, adduct := "[M+H]+"]
  #   if ("negative" %in% pols[anl]) peaks[analysis %in% anl, adduct := "[M-H]-"]
  #
  #   #TODO implement polarity switching when both is output of polarities
  # }

  peaks <- select(peaks,
    id,
    index,
    analysis,
    replicate,
    rt, mz,
    intensity, area,
    drt, rtmin, rtmax,
    dppm, mzmin, mzmax,
    everything()
  )

  peaks <- peaks[order(mz), ]
  peaks <- peaks[order(rt), ]
  peaks <- peaks[order(analysis), ]

  if (checkmate::testClass(object, "msAnalysis")) {
    object@peaks <- peaks[, `:=`(analysis = NULL, replicate = NULL)]

  } else {
    object@analyses <- lapply(object@analyses, function(x, peaks) {
      x@peaks <- peaks[analysis %in% x@analysis, ]
      x@peaks[, `:=`(analysis = NULL, replicate = NULL)]
      return(x)
    }, peaks = peaks)
  }

  cat("Done! \n")
  return(object)
}
