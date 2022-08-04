

#' @title calculateSNR
#'
#' @description Calculates the signal-to-noise (sn) ratio for peaks in a given \linkS4class{msData}
#' or \linkS4class{msAnalysis} object. If features are present in the \linkS4class{msData},
#' the minimum sn of the corresponding peaks group are returned in the metadata slot.
#' The argument \code{targetsID} can be used to specify for which peaks/features
#' the sn is to be calculated.
#'
#' @param object An \linkS4class{msData} object.
#' @param targetsID Optionally, a character vector with the \emph{id} of the features to calculate the sn ratio.
#' @param rtExpand The time, in seconds,
#' expansion before and after the time window of each feature
#' to extract centroids for estimation of the noise level.
#' @param filtered Logical, set to \code{TRUE} for calculating the sn of
#' peaks/features filtered. The default is \code{FALSE}.
#'
#' @details The noise is estimated by the maximum intensity of the background mass traces
#' (i.e., traces belonging to the same mass bin as the peak but taken from a wider and predefined
#' time window (\code{rtExpand}) before and after the peak limit. The sn is then calculated by
#' dividing the maximum intensity of the peak with the estimated noise.
#' Mass traces that belong to other peaks within the same mass and time windows are excluded before noise estimation.
#'
#' @return An \linkS4class{msData} or \linkS4class{msAnalysis} object with the
#' noise and sn columns amended to the peaks slot. When features are present, the
#' columns noise, noise_sd, sn and sn_sd are added to the slot metadata of the
#' \linkS4class{msFeatures} object in the given \linkS4class{msData}.
#'
#' @export
#'
#' @importFrom checkmate testClass
#' @importFrom stats sd
#' @importFrom data.table copy rbindlist
#'
calculateSNR <-  function(object, targetsID = NULL, rtExpand = 200, filtered = FALSE) {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msAnalysis"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(object)
  }

  if (checkmate::testClass(object, "msData") & nrow(object@features@metadata) > 0) {
    peaks_org <- peaks(object)
    feats_org <- object@features@metadata
    with_features <- TRUE
  } else {
    peaks_org <- peaks(object)
    if (checkmate::testClass(object, "msAnalysis")) peaks_org$analysis <- analysisNames(object)
    with_features <- FALSE
  }

  if (!("sn_pN" %in% colnames(peaks_org))) peaks_org[, sn_pN := NA_real_]
  if (!("sn_nN" %in% colnames(peaks_org))) peaks_org[, sn_nN := NA_real_]
  if (!("sn_noise" %in% colnames(peaks_org))) peaks_org[, sn_noise := NA_real_]
  if (!("sn_noise_sd" %in% colnames(peaks_org))) peaks_org[, sn_noise_sd := NA_real_]
  if (!("sn_value" %in% colnames(peaks_org))) peaks_org[, sn_value := NA_real_]

  if (with_features) {
    if (!("sn_pN" %in% colnames(feats_org))) feats_org[, sn_pN := NA_real_]
    if (!("sn_nN" %in% colnames(feats_org))) feats_org[, sn_nN := NA_real_]
    if (!("sn_noise" %in% colnames(feats_org))) feats_org[, sn_noise := NA_real_]
    if (!("sn_noise_sd" %in% colnames(feats_org))) feats_org[, sn_noise_sd := NA_real_]
    if (!("sn_value" %in% colnames(feats_org))) feats_org[, sn_value := NA_real_]
  }

  peaks_sn <- copy(peaks_org)

  if (!filtered) {
    peaks_sn <- peaks_sn[!peaks_sn$filtered, ]
  }

  if (!is.null(targetsID)) {
    if (with_features) {
      peaks_sn <- peaks_sn[id %in% targetsID | feature %in% targetsID, ]
    } else {
      peaks_sn <- peaks_sn[id %in% targetsID, ]
    }
  }

  #extract centroids from each peak in each sample, expanding the rt
  ana <- analysisNames(object)
  pks <- peaks_sn[, .(id, mzmin, mzmax, rtmin, rtmax, analysis)]
  pks <- pks[, `:=`(rtmin = rtmin - rtExpand, rtmax = rtmax + rtExpand)]

  cat("Extracting mass traces... ")
  eic <- lapply(ana, function(x, pks, object) {
    temp <- extractEICs(
      object,
      analyses =  x,
      mz = pks[analysis %in% x, ]
    )
  }, pks = pks, object = object)
  eic <- rbindlist(eic)
  ncent <- split(eic, by = "id")
  cat("Done! \n")

  cat(paste0("Calculating signal-to-noise ratio for ", nrow(peak_sn), " peaks... "))

  int <- lapply(peaks_sn$id, function(x) peaks_sn[id == x, intensity])
  names(int) <- peaks_sn$id

  noise_data_raw <- lapply(peaks_sn$id, function(x, ncent, peaks_sn) {

    temp <- peaks_sn[id %in% x, ]
    temp2 <- ncent[[x]]
    temp2 <- temp2[!(temp2$rt >= temp$rtmin & temp2$rt <= temp$rtmax), ]

    if (nrow(temp2) > 1) {
      #remove other peaks within the same mass and time deviation
      others <- peak_sn[
        analysis %in% temp$analysis &
          rt >= min(temp2$rt) &
          rt <= max(temp2$rt) &
          mz >= temp$mzmin &
          mz <= temp$mzmax &
          !(id %in% x),
      ]

      if (nrow(others) > 0) {
        for (oo in seq_len(nrow(others))) {
          temp2 <- temp2[!(rt > others$rtmin[oo] & rt < others$rtmax[oo]), ]
        }
      }
    }

    temp2 <- temp2$intensity

    return(temp2)
  }, peaks_sn = peaks_sn, ncent = ncent)

  names(noise_data_raw) <- peaks_sn$id

  noise_data_sd <- lapply(noise_data_raw, function(x) sd(x, na.rm = TRUE))

  noise_data <- lapply(noise_data_raw, function(x) ifelse(length(x) > 0, max(x, na.rm = TRUE), 0))

  for (pp in peaks_sn$id) {

    if (noise_data[[pp]] != 0) {
      peaks_sn[id %in% pp, sn_value := round(int[[pp]] / noise_data[[pp]], digits = 0)]
    } else {
      peaks_sn[id %in% pp, sn_value := NA]
    }

    peakcentN <- nrow(eic[id %in% pp & rt >= peaks_sn[id %in% pp, rtmin] & rt <= peaks_sn[id %in% pp, rtmax], ])
    noisecentN <- sapply(noise_data_raw, function(x) length(x))
    peaks_sn[id %in% pp, sn_pN := peakcentN]
    peaks_sn[id %in% pp, sn_nN := noisecentN[pp]]

    peaks_sn[id %in% pp, sn_noise_sd := round(noise_data_sd[[pp]], digits = 0)]
    peaks_sn[id %in% pp, sn_noise := round(noise_data[[pp]], digits = 0)]
  }

  peaks_org[id %in% peaks_sn$id, c("sn_pN", "sn_nN", "sn_noise", "sn_noise_sd", "sn_value")] <- peaks_sn[, .(sn_pN, sn_nN, sn_noise, sn_noise_sd, sn_value)]

  if (checkmate::testClass(object, "msData")) {
    object@analyses <- lapply(object@analyses, function(x, peaks_org) {
      ana <- analysisNames(x)
      temp <- peaks_org[analysis %in% ana, ]
      temp[, `:=`(analysis = NULL, replicate = NULL)]
      x@peaks <- copy(temp)
      return(x)
    }, peaks_org = peaks_org)

  } else {
    peaks_org[, analysis := NULL]
    object@peaks <- copy(peaks_org)
  }

  cat("Done! \n")

  if (with_features) {
    for (ff in unique(peak_sn$feature)) {
      toFeat <- peak_sn[feature %in% ff, .(sn_pN, sn_nN, sn_noise, sn_noise_sd, sn_value)]
      toFeat <- toFeat[sn_value == max(sn_value, na.rm = TRUE), ]
      toFeat <- toFeat[1, ]
      feat_org[id %in% ff, c("sn_pN", "sn_nN", "sn_noise", "sn_noise_sd", "sn_value")] <- toFeat
    }
    object@features@metadata <- copy(feat_org)
  }

  validObject(object)

  return(object)
}


#' @title calculateFeaturesMetadata
#'
#' @description Function to calculate feature quality indicators
#' using the \pkg{MetaClean} package via the \pkg{patRoon} interface.
#' See \code{?MetaClean} for further details.
#'
#' @param object An \linkS4class{ntsData} object containing features.
#' @param targets Optional character vector with features \emph{id}/s
#' for calculating metadata for the specified features.
#'
#' @return An \linkS4class{msData} object containing features and peaks
#' with amended quality indicators.
#'
#' @export
#'
#' @importFrom checkmate assertClass
#' @importMethodsFrom patRoon calculatePeakQualities featureTable
#' @importFrom data.table rbindlist
#'
calculateFeaturesMetadata <- function(object, targets = NULL) {

  checkmate::assertClass(object, "msData")

  object2 <- object

  #runs calculation for peaks of certain features
  if (!is.null(targets)) object2 <- object2[, targets]

  pat <- object2@pat
  patpeaks <- pat@features

  patpeaks <- patRoon::calculatePeakQualities(
    patpeaks,
    weights = NULL,
    flatnessFactor = 0.05, #Passed to MetaClean as the flatness.factor argument to calculateJaggedness and calculateModality.
    #avgFunc = mean, #not used for features object, --- mean additional parameter for handling featureGroups
    parallel = TRUE
  )

  pkq <- patRoon::featureTable(patpeaks)
  pkq <- rbindlist(pkq, idcol = "sample")
  newCols <- colnames(pkq)[c(which(colnames(pkq) == "ApexBoundaryRatio"):ncol(pkq))]

  pks <- peaks(object2)
  pks <- pks[, colnames(pks)[!colnames(pks) %in% newCols], with = FALSE]

  if (FALSE %in% (all.equal(pks$feature, pkq$group) & all.equal(pks$mz, pkq$mz))) {
    warning("Peaks table do not match between patRoon and ntsIUTA!")
    return(object)
  }

  pks <- cbind(pks, pkq[, newCols, with = FALSE])

  pks2 <- copy(pks)

  pks2 <- pks2[, .(
    ApexBoundaryRatio = min(ApexBoundaryRatio, na.rm = TRUE),
    FWHM2Base = max(FWHM2Base, na.rm = TRUE),
    Jaggedness = min(Jaggedness, na.rm = TRUE),
    Modality = min(Modality, na.rm = TRUE),
    Symmetry = max(Symmetry, na.rm = TRUE),
    GaussianSimilarity = max(GaussianSimilarity, na.rm = TRUE),
    Sharpness = max(Sharpness, na.rm = TRUE),
    TPASR = max(TPASR, na.rm = TRUE),
    ZigZag = min(ZigZag, na.rm = TRUE),
    ApexBoundaryRatioScore = max(ApexBoundaryRatioScore, na.rm = TRUE),
    FWHM2BaseScore = max(FWHM2BaseScore, na.rm = TRUE),
    JaggednessScore = max(JaggednessScore, na.rm = TRUE),
    ModalityScore = max(ModalityScore, na.rm = TRUE),
    SymmetryScore = max(SymmetryScore, na.rm = TRUE),
    GaussianSimilarityScore = max(GaussianSimilarityScore, na.rm = TRUE),
    SharpnessScore = max(SharpnessScore, na.rm = TRUE),
    TPASRScore = max(TPASRScore, na.rm = TRUE),
    ZigZagScore = max(ZigZagScore, na.rm = TRUE),
    totalScore = max(totalScore, na.rm = TRUE)
  ), by = feature]

  #update peaks newCols
  peaks_org <- copy(object@peaks)

  if (!TRUE %in% newCols %in% colnames(peaks_org)) {
    peaks_org[, (newCols) := as.numeric(NA)]
  }

  peaks_org[which(id %in% pks$id), (newCols) := pks[, newCols, with = FALSE]]

  object@peaks <- copy(peaks_org)

  #update features newCols
  feats_org <- copy(object@features)

  if (!TRUE %in% newCols %in% colnames(feats_org)) {
    feats_org[, (newCols) := as.numeric(NA)]
  }

  feats_org[which(feats_org$id %in% pks2$feature), (newCols) := pks2[, newCols, with = FALSE]]

  object@features <- copy(feats_org)

  return(object)
}
