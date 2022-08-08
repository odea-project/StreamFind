
#' @title peakGrouping
#'
#' @description Grouping and alignment of peaks across samples.
#' The peak grouping uses the \pkg{patRoon} package.
#'
#' @param object A \linkS4class{msData} object with
#' \linkS4class{msAnalysis} objects containing peaks.
#' @template args-single-settings
#'
#' @note The call in the \linkS4class{settings} must be set to "peakGrouping".
#'
#' @return An \linkS4class{msData} object containing a features S4 class.
#'
#' @details The following algorithms are possible: "xcms3", "xcms", "openms".
#' See ?\pkg{patRoon} for further information.
#' See \code{\link[patRoon]{groupFeatures}} for more information.
#'
#' @seealso \code{\link[patRoon]{groupFeatures}}
#'
#' @references
#' \insertRef{patroon01}{streamFind}
#'
#' @export
#'
#' @importFrom checkmate assertClass testClass
#' @importClassesFrom patRoon featureGroups
#' @importFrom patRoon groupFeatures
#' @importMethodsFrom patRoon as.data.table
#'
peakGrouping <- function(object = NULL, settings = NULL) {

  assertClass(object, "msData")

  noPeaks <- sapply(object@analyses, function(x) nrow(x@peaks))
  noPeaks <- TRUE %in% (0 %in% noPeaks)

  if (noPeaks) {
    warning("Object does not have peaks to group!")
    return(object)
  }

  # TODO implement as.featuresSet for multiple polarities
  # TODO check if all analyses have peaks before grouping
  pat <- as.features(object)

  if (is.null(settings)) {

    prs <- getParameters(object, where = "features", call = "peakGrouping")
    if (length(prs) > 0) {
      algorithm = getAlgorithm(prs)
      params = getSettings(prs)
    }

  } else if (testClass(settings, "settings")) {

    algorithm <- getAlgorithm(settings)
    params <- getSettings(settings)

  } else {

    algorithm <- NA_character_
  }


  if (is.na(algorithm)) {
    warning("Peak grouping algorihtm not defined!")
    return(object)
  }


  if (algorithm == "xcms3") {
      params$groupParam@sampleGroups <- replicateNames(object)
    if (params$rtalign) {
      params$preGroupParam@sampleGroups <- replicateNames(object)
    }
  }

  ag <- list(obj = pat, algorithm = algorithm)

  pat <- do.call(groupFeatures, c(ag, params, verbose = TRUE))

  stgs <- createSettings(call = "peaksGrouping",
    algorithm = algorithm, settings = params)

  object <- addParameters(object, settings = stgs, where = "features")

  object <- buildPeaksTable(object, pat)

  object <- buildFeatures(object, pat)

  object <- addAdjustedRetentionTime(object, pat)

  validObject(object)

  return(object)
}

#' buildFeatures
#'
#' @param object An \linkS4class{msData} object.
#' @param pat A \linkS4class{featureGroups} object from the package \pkg{patRoon}
#'
#' @return An \linkS4class{msData} object with updated features slot.
#'
#' @importMethodsFrom patRoon as.data.table groupTable
#' @importFrom data.table setnames setorder copy
#' @importFrom dplyr select everything left_join
#'
buildFeatures <- function(object, pat) {

  cat("Building table with features... ")

  feat <- patRoon::as.data.table(pat, average = FALSE)
  feat[, `:=`(ret = NULL, mz = NULL)]

  mtd <- patRoon::as.data.table(pat, average = TRUE)
  setnames(mtd, "ret", "rt")
  mtd <- mtd[, .(group, mz, rt)]

  pk <- peaks(object)
  index <- lapply(mtd$group, function(x, pk) {
    return(which(pk$feature == x))
  }, pk = pk)

  mtd$mzmin <- unlist(lapply(index, function(x) min(pk[x, mz])))
  mtd$mzmax <- unlist(lapply(index, function(x) max(pk[x, mz])))
  mtd$rtmin <- unlist(lapply(index, function(x) min(pk[x, rt])))
  mtd$rtmax <- unlist(lapply(index, function(x) max(pk[x, rt])))

  mtd$dppm <- round(((mtd$mzmax - mtd$mzmin) / mtd$mz) * 1E6, digits = 1)
  mtd$drt <- round(mtd$rtmax - mtd$rtmin, digits = 0)

  pk$replicate <- factor(pk$replicate, levels = unique(replicateNames(object)))

  tp_pks <- setNames(
    data.frame(
      matrix(ncol = length(analysisNames(object)),
             nrow = 1)), analysisNames(object)
  )

  tp_pks[1, ] <- 0

  mtd$peaks <- lapply(index, function(x, pk, tp_pks) {
    temp <- copy(tp_pks)
    temp[1, colnames(temp) %in% pk$analysis[x]] <- pk$index[x]
    return(temp)
  }, pk = pk, tp_pks = tp_pks)

  mtd$index <- as.numeric(sub(".*_", "", mtd$group))

  if ("is_filled" %in% colnames(pk)) {
    mtd$hasFilled <- unlist(lapply(index, function(x) 1 %in% pk$is_filled[x]))
  } else {
    mtd$hasFilled <- FALSE
  }

  if (!"filtered" %in% colnames(mtd)) {
    mtd$filtered <- FALSE
    mtd$filter <- NA_character_
  }

  new_id <- paste0(
    "m",
    round(mtd$mz, digits = 3),
    "_d",
    mtd$dppm,
    "_r",
    round(mtd$rt, digits = 0),
    "_t",
    mtd$drt,
    "_f",
    mtd$index
  )

  new_id <- data.table(group = mtd$group , id = new_id)

  mtd <- left_join(mtd, new_id, by = "group")
  mtd[, group := NULL]
  mtd <- select(
    mtd, id, index, rt, mz, drt, rtmin, rtmax, dppm, mzmin, mzmax, everything())

  feat <- left_join(feat, new_id, by = "group")
  feat[, group := NULL]
  feat <- select(feat, id, everything())

  #updates feature id in peaks table of each analysis
  setnames(new_id, c("group", "id"), c("feature", "new_feature"))
  object@analyses <- lapply(object@analyses, function(x, new_id) {
    x@peaks <- left_join(x@peaks, new_id, by = "feature")
    x@peaks[, feature := NULL]
    setnames(x@peaks, "new_feature", "feature")
    return(x)
  }, new_id = new_id)

  anaInfo <- as.data.table(analysisInfo(object))
  anaInfo <- anaInfo[, .(file, analysis, group, blank, class)]
  setnames(anaInfo, "group", "replicate")
  # TODO add set column to define the polarity of each sample

  object@features@analyses <- anaInfo
  object@features@intensity <- feat
  object@features@metadata <- mtd

  cat("Done! \n")

  return(object)
}

#' @title addAdjustedRetentionTime
#'
#' @description Function to add adjusted retention time information
#' to the spectra slot of each \linkS4class{msAnalysis} in the
#' \linkS4class{msData} object.
#'
#' @param object An \linkS4class{msData} object.
#' @param pat A \linkS4class{featureGroups} object from \pkg{patRoon}.
#'
#' @importFrom checkmate testClass assertClass
#' @importMethodsFrom xcms adjustedRtime processHistory peakGroupsMatrix hasAdjustedRtime
#' @importClassesFrom xcms XCMSnExp PeakGroupsParam
#' @importClassesFrom patRoon featureGroupsXCMS3
#' @importFrom stringr str_detect
#'
addAdjustedRetentionTime <- function(object, pat) {

  assertClass(object, "msData")

  if (testClass(pat, "featureGroupsXCMS3") & hasAdjustedRtime(pat@xdata)) {

    cat("Adding adjusted retention time values... ")

    rtAdj <- adjustedRtime(pat@xdata)

    pkAdj <- processHistory(
      pat@xdata,
      type = "Retention time correction"
    )[[1]]
    pkAdj <- pkAdj@param

    addAdjPoints <- FALSE
    if (testClass(pkAdj, "PeakGroupsParam")) {
      addAdjPoints <- TRUE
      pkAdj <- peakGroupsMatrix(pkAdj)
    }

    hasSpectra <- sapply(object@analyses, function(x) nrow(x@spectra))
    hasSpectra <- hasSpectra > 0
    names(hasSpectra) <- analysisNames(object)

    object@analyses <- lapply(object@analyses, function(x, hasSpectra) {
      if (!hasSpectra[analysisNames(x)]) {
        x@spectra <- loadBasicRawSpectraHeaderMZR(filePaths(x))
      }
      return(x)
    }, hasSpectra = hasSpectra)

    object@analyses <- lapply(object@analyses,
      function(x, object, rtAdj, addAdjPoints, pkAdj) {

      ana_idx <- which(analysisNames(object) %in% analysisNames(x))

      rts <- names(rtAdj)
      rts <- str_detect(rts, paste0("F", ana_idx))
      rts <- rtAdj[rts]

      temp <- x@spectra
      temp[, rtAdjusted := rts]
      temp[, adjustment := rtAdjusted - rt]

      if (addAdjPoints) {
        pk_rts <- unique(pkAdj[, ana_idx])
        pk_rts <- pk_rts[pk_rts %in% temp$rt]
        temp[rt %in% pk_rts, adjPoints := pk_rts]
      }

      x@spectra <- copy(temp)

      return(x)
    },
    object = object,
    rtAdj = rtAdj,
    addAdjPoints = addAdjPoints,
    pkAdj = pkAdj)

    cat("Done! \n")
    return(object)
  }

  return(object)
}
