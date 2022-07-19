

#' @title peakGrouping
#'
#' @description Grouping and alignment of peaks across samples.
#' The peak grouping uses the \pkg{patRoon} package.
#'
#' @param object A \linkS4class{msData} object with
#' \linkS4class{msAnalysis} objects containing peaks.
#' @template args-single-settings
#'
#'
#' @return An \linkS4class{msData} object
#' containing a features S4 class.
#'
#' @details and
#' the following algorithms are possible: "xcms3", "xcms", "openms".
#' See ?\pkg{patRoon} for further information.
#' See \link[patRoon]{groupFeatures} for more information.
#'
#' @seealso \link[patRoon]{groupFeatures}
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

  checkmate::assertClass(object, "msData")

  noPeaks <- sapply(object@analyses, function(x) nrow(x@peaks))
  noPeaks <- TRUE %in% (0 %in% noPeaks)

  if (noPeaks) {
    warning("Object does not have peaks to group!")
    return(object)
  }

  # TODO implement as.featuresSet for multiple polarities
  pat <- as.features(object)

  if (is.null(settings)) {

    prs <- getParameters(object, where = "features", call = "peakGrouping")
    if (length(prs) > 0) {
      algorithm = getAlgorithm(prs)
      params = getSettings(prs)
    }

  } else if (checkmate::testClass(settings, "settings")) {

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
      params$groupParam@sampleGroups <- replicates(object)
    if (params$rtalign) {
      params$preGroupParam@sampleGroups <- replicates(object)
    }
  }

  ag <- list(obj = pat, algorithm = algorithm)

  pat <- do.call(groupFeatures, c(ag, params, verbose = TRUE))

  stgs <- createSettings(call = "peaksGrouping", algorithm = algorithm, settings = params)

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

  pk$replicate <- factor(pk$replicate, levels = unique(replicates(object)))

  tp_pks <- setNames(data.frame(matrix(ncol = length(analyses(object)), nrow = 1)), analyses(object))
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

  mtd <- dplyr::left_join(mtd, new_id, by = "group")
  mtd[, group := NULL]
  mtd <- select(mtd, id, index, rt, mz, drt, rtmin, rtmax, dppm, mzmin, mzmax, everything())

  feat <- dplyr::left_join(feat, new_id, by = "group")
  feat[, group := NULL]
  feat <- select(feat, id, everything())

  #updates feature id in peaks table of each analysis
  setnames(new_id, c("group", "id"), c("feature", "new_feature"))
  object@analyses <- lapply(object@analyses, function(x, new_id) {
    x@peaks <- dplyr::left_join(x@peaks, new_id, by = "feature")
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
#'
#' @importFrom checkmate testClass assertClass
#' @importMethodsFrom xcms adjustedRtime processHistory peakGroupsMatrix hasAdjustedRtime
#' @importClassesFrom xcms XCMSnExp PeakGroupsParam
#' @importClassesFrom patRoon featureGroupsXCMS3
#'
addAdjustedRetentionTime <- function(object, pat) {

  checkmate::assertClass(object, "msData")

  if (checkmate::testClass(pat, "featureGroupsXCMS3") & xcms::hasAdjustedRtime(pat@xdata)) {

    cat("Adding adjusted retention time values... ")

    rtAdj <- xcms::adjustedRtime(pat@xdata)

    pkAdj <- xcms::processHistory(
      pat@xdata,
      type = "Retention time correction"
    )[[1]]
    pkAdj <- pkAdj@param

    addAdjPoints <- FALSE
    if (checkmate::testClass(pkAdj, "PeakGroupsParam")) {
      addAdjPoints <- TRUE
      pkAdj <- xcms::peakGroupsMatrix(pkAdj)
    }

    hasSpectra <- sapply(object@analyses, function(x) nrow(x@spectra))
    hasSpectra <- hasSpectra > 0
    names(hasSpectra) <- analyses(object)

    object@analyses <- lapply(object@analyses, function(x, hasSpectra) {
      if (!hasSpectra[analyses(x)]) {
        x@spectra <- loadBasicRawSpectraHeaderMZR(files(x))
      }
      return(x)
    }, hasSpectra = hasSpectra)

    object@analyses <- lapply(object@analyses, function(x, object, rtAdj, addAdjPoints, pkAdj) {

      ana_idx <- which(analyses(object) %in% analyses(x))

      rts <- names(rtAdj)
      rts <- stringr::str_detect(rts, paste0("F", ana_idx))
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


#' @title updateFeatureTable
#'
#' @description Function to update feature table from
#' peaks remaining in the \linkS4class{msData} object.
#'
#' @param object An \linkS4class{msData} object.
#' @param fast Logical, set to \code{TRUE} for a lazy update of features.
#' When \code{TRUE}, only the mz, rt, and intensity are updated.
#' When \code{FALSE}, the feature table is completly updated.
#' Note that UFIs, quality data and annotation info are lost.
#'
#' @return An \linkS4class{msData} object with updated features slot.
#'
#' @export
#'
#' @importMethodsFrom patRoon as.data.table groupNames
#' @importFrom data.table setnames setorder copy
#' @importFrom dplyr select everything
#'
updateFeatureTable <- function(object, fast = TRUE) {

  cat("Updating features... ")

  pat <- object@pat

  feat <- patRoon::as.data.table(pat, average = TRUE)

  feat <- setnames(feat, c("ret", "group"), c("rt", "id"))

  rpl <- unique(replicates(object))

  hasRemoved <- FALSE

  feat_org <- features(object)

  feat_org <- feat_org[id %in% feat$id, ]

  feat2org <- feat[id %in% feat_org$id, ]

  feat_org[, rt := feat2org$rt]
  feat_org[, mz := feat2org$mz]

  if (!TRUE %in% all.equal(feat_org$id, feat2org$id)) {
    warning("Mistach of features order during update
      of feature table! Feature table not updated."
    )
    return(object)
  }

  feat_org[, (rpl) := feat2org[, rpl, with = FALSE]]

  colRem <- colnames(feat_org)
  colRem <- c(which(colRem == "d_sec") + 1, which(colRem == "mzmin") - 1)
  colRem <- colnames(feat_org)[colRem[1]:colRem[2]]
  colRem <- colRem[!grepl(rpl, colRem, fixed = TRUE)]

  if (length(colRem) > 0) feat_org[, (colRem) := NULL]

  if (nrow(object@removed) > 0) {
    hasRemoved <- TRUE
    feat_rem <- object@removed
    feat_rem <- feat_rem[id %in% feat$id, ]
    feat2rem <- feat[id %in% feat_rem$id, ]
    feat_rem[, rt := feat2rem$rt]
    feat_rem[, mz := feat2rem$mz]
    feat_rem[, (rpl) := feat2rem[, rpl, with = FALSE]]
    if (length(colRem) > 0) feat_rem[, (colRem) := NULL]
  }

  if (fast) {

    object@features <- copy(feat_org)
    if (hasRemoved) object@removed <- copy(feat_rem)

    cat("Done with the fast method! \n")

    return(object)
  }

  #not fast
  if (hasRemoved) feat_org <- rbind(feat_org, feat_rem)

  ID <- patRoon::groupNames(object@pat)
  if (length(ID) != nrow(feat_org)) {
    warning("There is a mismatch in the number of features between
      ntsData and the patRoon object! Features not updated."
    )
    return(object)
  }
  feat_org <- feat_org[data.table::data.table(id = ID), on = "id"]

  rpl_sp <- lapply(rpl, function(x, st) {
    st$sample[st$replicate == x]
  }, st = samplesTable(object))

  feat_b <- patRoon::as.data.table(pat, average = FALSE)

  #update intensities sd
  feat_sd <- lapply(rpl_sp, function(x, feat_b) {
    temp <- feat_b[, x, with = FALSE]
    temp <- apply(temp, 1, function(x) sd(x) / mean(x) * 100)
    temp[is.nan(temp)] <- 0
    temp <- round(temp, digits = 0)
    return(temp)
  }, feat_b = feat_b)

  names(feat_sd) <- paste0(rpl, "_sd")
  feat_sd <- as.data.table(feat_sd)
  feat_sd[, id := feat$id]

  sd_cols <- paste0(rpl, "_sd")
  feat_org[, (sd_cols) := feat_sd[which(id %in% feat_org$id), sd_cols, with = FALSE]]

  #update mz and rt ranges
  pk <- peaks(object)

  pk$index <- seq_len(nrow(pk))
  index <- lapply(feat_org$id, function(x) pk[feature == x, index])
  feat_org$mzmin <- unlist(lapply(index, function(x) min(pk[x, mz])))
  feat_org$mzmax <- unlist(lapply(index, function(x) max(pk[x, mz])))
  feat_org$rtmin <- unlist(lapply(index, function(x) min(pk[x, rt])))
  feat_org$rtmax <- unlist(lapply(index, function(x) max(pk[x, rt])))

  feat_org$d_ppm <- round(((feat_org$mzmax - feat_org$mzmin) / feat_org$mz) * 1E6, digits = 1)

  feat_org$d_sec <- round(feat_org$rtmax - feat_org$rtmin, digits = 0)

  feat_org$p_id <- I(lapply(feat_org$id, function(x) pk[feature == x, id]))

  pk$replicate <- factor(pk$replicate, levels = rpl)

  feat_org$npeaks <- lapply(index, function(x) {
    as.data.frame(table(pk$replicate[x]))$Freq
  })

  if ("is_filled" %in% colnames(pk)) {
    feat_org$hasFilled <- unlist(lapply(index, function(x) 1 %in% pk$is_filled[x]))
  } else {
    feat_org$hasFilled <- FALSE
  }

  object@features <- feat_org

  if (hasRemoved) object <- removeFilteredFeatures(object)

  cat("Done with complete updating method! \n")

  return(object)
}
