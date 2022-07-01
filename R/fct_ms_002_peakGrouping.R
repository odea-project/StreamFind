

#' @title peakGrouping
#'
#' @description Grouping and alignment of peaks across samples.
#' The peak grouping uses the \pkg{patRoon} package and
#' the following algorithms are possible: "xcms3", "xcms", "openms".
#' See ?\pkg{patRoon} for further information.
#'
#' @param object A \linkS4class{msData} object with
#' \linkS4class{msAnalysis} objects containing peaks.
#' @param algorithm The algorithm to use for peak alignment and grouping.
#' @param settings The respective list of parameter settings.
#' See \link[patRoon]{groupFeatures} for more information.
#'
#' @return An \linkS4class{msData} object
#' containing a features S4 class.
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
peakGrouping <- function(object = NULL,
                         algorithm = NA_character_,
                         settings = NULL) {

  checkmate::assertClass(object, "msData")

  noPeaks <- sapply(object@analyses, function(x) nrow(x@peaks))
  noPeaks <- TRUE %in% (0 %in% noPeaks)

  if (noPeaks) {
    warning("Object does not have peaks to group!")
    return(object)
  }

  # TODO implement as.featuresSet for multiple polarities

  pat <- as.features(object)

  if (is.na(algorithm)) {
    prs <- getParameters(object, where = "features", call = "peakGrouping")
    if (length(prs) > 0) {
      algorithm = getAlgorithm(prs)
      settings = getSettings(prs)
    }
  }

  if (is.na(algorithm)) {
    warning("Peak grouping algorihtm not defined!")
    return(object)
  }

  if (algorithm == "xcms3") {
      settings$groupParam@sampleGroups <- replicates(object)
    if (settings$rtalign) {
      settings$preGroupParam@sampleGroups <- replicates(object)
    }
  }

  ag <- list(obj = pat, algorithm = algorithm)

  pat <- do.call(groupFeatures, c(ag, settings, verbose = TRUE))

  stgs <- createSettings(call = "peaksGrouping", algorithm = algorithm, settings = settings)
  object <- addParameters(object, settings = stgs, where = "features")

  object <- buildPeaksTable(object, pat)

  object <- buildFeatures(object, pat)


  # TODO implement adjusted retention time as in ntsIUTA
  #object <- addAdjustedRetentionTime(object)

  # if (simplify) {
  #   newFeats <- new("featuresSIRIUS",
  #     analysisInfo = pat@features@analysisInfo,
  #     features = pat@features@features
  #   )
  #
  #   newFeatGroups <- new("featureGroupsSIRIUS",
  #     groups = pat@groups,
  #     groupInfo = pat@groupInfo,
  #     analysisInfo = pat@analysisInfo,
  #     features = newFeats,
  #     ftindex = pat@ftindex
  #   )
  #
  #   object@pat <- newFeatGroups
  # }

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


  feat <- patRoon::as.data.table(pat, average = TRUE)
  feat <- setnames(feat, "ret", "rt")

  feat_b <- patRoon::as.data.table(pat, average = FALSE)

  rpl <- unique(replicates(object))

  rpl_ana <- lapply(rpl, function(x, st) {
    st$analysis[st$group == x]
  }, st = analysisInfo(object))

  feat_sd <- lapply(rpl_ana, function(x, feat_b) {
    temp <- feat_b[, x, with = FALSE]
    temp <- apply(temp, 1, function(x) sd(x) / mean(x) * 100)
    temp[is.nan(temp)] <- 0
    temp <- round(temp, digits = 0)
    return(temp)
  }, feat_b = feat_b)

  names(feat_sd) <- paste0(rpl, "_sd")
  feat <- cbind(feat, as.data.table(feat_sd))


  mtd <- copy(feat)
  setnames(mtd, "ret", "rt")
  mtd <- mtd[, .(group, mz, rt)]

  pk <- peaks(object)
  if (!"index" %in% colnames(pk)) pk$index <- seq_len(nrow(pk))

  index <- lapply(mtd$group, function(x) pk[feature == x, index])
  mtd$mzmin <- unlist(lapply(index, function(x) min(pk[x, mz])))
  mtd$mzmax <- unlist(lapply(index, function(x) max(pk[x, mz])))
  mtd$rtmin <- unlist(lapply(index, function(x) min(pk[x, rt])))
  mtd$rtmax <- unlist(lapply(index, function(x) max(pk[x, rt])))

  mtd$dppm <- round(((mtd$mzmax - mtd$mzmin) / mtd$mz) * 1E6, digits = 1)
  mtd$drt <- round(mtd$rtmax - mtd$rtmin, digits = 0)

  pk$replicate <- factor(pk$replicate, levels = unique(replicates(object)))

  mtd$npeaks <- lapply(index, function(x) {
    as.data.frame(table(pk$replicate[x]))$Freq
  })

  mtd$index <- as.numeric(sub(".*_", "", mtd$group))

  # TODO update after implementing filling peaks
  # if ("is_filled" %in% colnames(pk)) {
  #   mtd$hasFilled <- unlist(lapply(index, function(x) 1 %in% pk$is_filled[x]))
  # } else {
  #   mtd$hasFilled <- FALSE
  # }

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
  feat[, `:=`(group = NULL, mz = NULL, rt = NULL)]
  feat <- select(feat, id, everything())

  feat_b <- dplyr::left_join(feat_b, new_id, by = "group")
  feat_b[, `:=`(group = NULL, mz = NULL, rt = NULL)]
  feat_b <- select(feat_b, id, everything())






  pksIdx <- max(pat@ftindex)
  pksIdx <- t(pksIdx)
  colnames(pksIdx) <- mtd$id








  #updates feature id in peaks table of each analysis
  setnames(new_id, c("group", "id"), c("feature", "new_feature"))
  object@analyses <- lapply(object@analyses, function(x, new_id) {
    x@peaks <- dplyr::left_join(x@peaks, new_id, by = "feature")
    x@peaks[, feature := NULL]
    setnames(x@peaks, "new_feature", "feature")
    return(x)
  }, new_id = new_id)

  object@features@metadata <- mtd
  object@features@replicates <- feat
  object@features@analyses <- feat_b
  object@features@peaks

  cat("Done! \n")
  return(object)
}


#' @title addAdjustedRetentionTime
#'
#' @description Function to add adjusted retention time information
#' to the scans slot of the \linkS4class{msData} object.
#'
#' @param object An \linkS4class{msData} object.
#'
#' @importFrom checkmate testClass assertClass
#' @importMethodsFrom xcms adjustedRtime processHistory peakGroupsMatrix hasAdjustedRtime
#' @importClassesFrom xcms XCMSnExp PeakGroupsParam
#' @importClassesFrom patRoon featureGroupsXCMS3
#'
addAdjustedRetentionTime <- function(object) {

  checkmate::assertClass(object, "msData")

  pat <- object@pat

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

    scans <- object@scans
    scans <- lapply(seq_len(length(scans)), function(x, scans, rtAdj, addAdjPoints, pkAdj) {

      rts <- names(rtAdj)
      rts <- stringr::str_detect(rts, paste0("F", x))
      rts <- rtAdj[rts]

      temp <- scans[[x]]
      temp[, adjustedRetentionTime := rts]
      temp[, adjustment := adjustedRetentionTime - retentionTime]

      if (addAdjPoints) {
        pk_rts <- unique(pkAdj[, x])
        pk_rts <- pk_rts[pk_rts %in% temp$retentionTime]
        temp[retentionTime %in% pk_rts, adjPoints := pk_rts]
      }

      return(temp)
    },
    scans = scans,
    rtAdj = rtAdj,
    addAdjPoints = addAdjPoints,
    pkAdj = pkAdj)

    names(scans) <- samples(object)

    object@scans <- scans

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
