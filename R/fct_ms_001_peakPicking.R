
#' @title peakPicking
#'
#' @description Finds chromatographic peaks from MS data in an
#' \linkS4class{msAnalysis} or in MS analyses of a given \linkS4class{msData}.
#' The peak picking uses the \pkg{patRoon} package for data processing,
#' enabling the use of several algorithms (see details).
#'
#' @param object An \linkS4class{msData} or \linkS4class{msAnalysis} object.
#' @template args-single-settings
#'
#' @note The \linkS4class{settings} call must be set to "peakPicking".
#'
#' @details See the \pkg{patRoon}'s \code{\link[patRoon]{findFeatures}} or the
#' \href{https://rickhelmus.github.io/patRoon/reference/findFeatures.html}{guide}
#' for more information. The following algorithms are available via
#' \pkg{patRoon}: "xcms3", "xcms", "openms", "envipick", "sirius", "kpic2",
#' "safd". The \linkS4class{settings} depend on the algorithm chosen.
#'
#' @return An \linkS4class{msAnalysis} or \linkS4class{msData} object
#' with peaks added to the respective slot.
#'
#' @seealso \code{\link[patRoon]{findFeatures}}
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

  if (testClass(object, "msData") | testClass(object, "msAnalysis"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(object)
  }


  if (is.null(settings)) {

    prs <- getParameters(object, call = "peakPicking")
    prs[sapply(prs, is.null)] <- NULL

    if (length(unique(prs)) == 1 & length(prs) > 0) {
      prs <- unique(prs)
      algorithm <- getAlgorithm(prs[[1]])
      params <- getSettings(prs[[1]])

    } else if (length(prs) > 0) {

      algorithm <- sapply(prs, function(x) getAlgorithm(x))

      if (length(algorithm) != length(analysisNames(object))) {

        warning(
          paste0("Settings not present for the analyses: ",
            paste(analysisNames(object)[!analysisNames(object) %in%
              names(algorithm)],
              collapse = "; "
            )
          )
        )

        return(object)
      }
    }
  } else if (testClass(settings, "settings")) {

    algorithm <- getAlgorithm(settings)
    params <- getSettings(settings)

  } else {

    algorithm <- NA_character_
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
    pat <- do.call("findFeatures", c(ag, params, verbose = TRUE))

    stgs <- createSettings(
      call = "peakPicking",
      algorithm = algorithm,
      settings = params
    )

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

    pat <- new("featuresSIRIUS",
      analysisInfo = combAnaInfo,
      features = combFeatures_new
    )
  }

  object <- buildPeaksTable(object, pat)

  validObject(object)

  return(object)
}

#' @title buildPeaksTable
#'
#' @param object A \linkS4class{msData} or \linkS4class{msAnalysis} object.
#' @param pat A \linkS4class{features} or \linkS4class{featureGroups} object
#' from the package \pkg{patRoon}.
#'
#' @return A \linkS4class{data.table} containing information of peaks found
#' in each sample.
#'
#' @importClassesFrom patRoon features
#' @importFrom dplyr select left_join full_join semi_join anti_join
#' @importMethodsFrom xcms chromPeaks hasFilledChromPeaks
#' @importFrom data.table rbindlist as.data.table setnames setorder copy
#' @importFrom checkmate testClass
#'
buildPeaksTable <- function(object, pat) {

  cat("Building peaks table... ")

  if (testClass(pat, "features")) {
    valid <- TRUE
    peaks <- pat@features
    anaInfo <- pat@analysisInfo

    if (testClass(pat, "featuresXCMS3")) {
      if (hasFilledChromPeaks(pat@xdata)) {
        extra <- as.data.table(chromPeaks(pat@xdata, isFilledColumn = TRUE))
        extra[, analysis := anaInfo$analysis[extra$sample]]
        extra[, analysis := factor(analysis, levels = anaInfo$analysis)]
        extra <- split(extra, extra$analysis)
      } else { extra <- NULL }
    } else { extra <- NULL }

    peaks <- lapply(names(peaks), function(x, extra, peaks) {
      temp <- peaks[[x]]

      if (!is.null(extra)) {
        if (temp == nrow(extra[[x]]) & all(temp$mz == extra[[x]]$mz)) {
          temp[, is_filled := extra[[x]]$is_filled]
        }
      }

      if (!"is_filled" %in% colnames(temp)) temp$is_filled <- 0
      if (!"filtered" %in% colnames(temp)) temp$filtered <- FALSE
      if (!"filter" %in% colnames(temp)) temp$filter <- NA_character_

      setnames(temp,
               c("ID", "ret", "retmin", "retmax"),
               c("id", "rt", "rtmin", "rtmax"), skip_absent = TRUE)

      setnames(temp, "group", "feature", skip_absent = TRUE)

      return(temp)
    }, extra = extra, peaks = peaks)
  }

  if (testClass(pat, "featureGroups")) {
    valid <- TRUE
    peaks <- pat@features@features
    anaInfo <- pat@analysisInfo

    if (testClass(pat, "featureGroupsXCMS3")) {
      if (hasFilledChromPeaks(pat@xdata)) {
        extra <- as.data.table(chromPeaks(pat@xdata, isFilledColumn = TRUE))
        extra[, analysis := anaInfo$analysis[extra$sample]]
        extra[, analysis := factor(analysis, levels = anaInfo$analysis)]
        extra <- split(extra, extra$analysis)
      } else { extra <- NULL }
    } else { extra <- NULL }

    peaks <- lapply(analysisNames(object), function(x, extra, peaks) {
      temp <- peaks[[x]]

      if (!is.null(extra)) {
        if (nrow(temp) == nrow(extra[[x]]) & all(temp$mz == extra[[x]]$mz)) {
          temp[, is_filled := extra[[x]]$is_filled]
        }
      }

      setnames(temp,
               c("ID", "ret", "retmin", "retmax"),
               c("id", "rt", "rtmin", "rtmax"), skip_absent = TRUE)

      setnames(temp, "group", "feature", skip_absent = TRUE)

      return(temp)

    }, extra = extra, peaks = peaks)

    names(peaks) <- analysisNames(object)

    peaks_org <- lapply(object@analyses, function(x) x@peaks)

    # amending existing peaks as patRoon removes peaks not grouped
    peaks <- lapply(analysisNames(object), function(x, peaks, peaks_org) {

      temp <- copy(peaks[[x]])

      temp_org <- copy(peaks_org[[x]])

      temp_rem <- anti_join(
        temp_org, temp, by = c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax"))

      temp_old <- semi_join(
        temp, temp_org, by = c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax"))

      temp_new <- anti_join(
        temp, temp_org, by = c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax"))

      temp_list <- list(
        temp_old,
        temp_new,
        temp_rem
      )

      temp <- rbindlist(temp_list, fill = TRUE)

      if (!"is_filled" %in% colnames(temp)) temp$is_filled <- 0
      if (!"filtered" %in% colnames(temp)) temp$filtered <- FALSE
      if (!"filter" %in% colnames(temp)) temp$filter <- NA_character_

      temp$filtered[is.na(temp$filtered)] <- FALSE
      temp$is_filled[is.na(temp$is_filled)] <- 0

      return(temp)

    }, peaks = peaks, peaks_org = peaks_org)
  }

  peaks <- lapply(peaks, function(x, object) {
    temp <- copy(x)

    if ("feature" %in% colnames(temp)) {
      temp[is.na(feature), filter := "grouping"]
      temp[is.na(feature), filtered := TRUE]
    }

    temp[, dppm := round((mzmax - mzmin) / mzmin * 1E6, digits = 0)]
    temp[, drt := round(rtmax - rtmin, digits = 0)]

    temp <- temp[order(mz), ]
    temp <- temp[order(rt), ]
    temp <- temp[order(filtered), ]

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

  names(peaks) <- analysisNames(object)

  if (testClass(object, "msAnalysis")) {
    object@peaks <- copy(peaks[[1]])

  } else {
    object@analyses <- lapply(analysisNames(object), function(x, object, peaks) {
      temp <- object@analyses[[x]]
      temp@peaks <- copy(peaks[[x]])
      return(temp)
    }, object = object, peaks = peaks)
  }

  # TODO implement multiple polarities to amend the peak id accordingly
  # pols <- polarities(object)
  # peaks$adduct <- NA_character_
  # for (anl in analysisNames(object)) {
  #   if ("positive" %in% pols[anl]) peaks[analysis %in% anl, adduct := "[M+H]+"]
  #   if ("negative" %in% pols[anl]) peaks[analysis %in% anl, adduct := "[M-H]-"]
  #
  #   #TODO implement polarity switching when both is output of polarities
  # }

  cat("Done! \n")

  return(object)
}
