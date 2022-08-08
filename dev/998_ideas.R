
#' @title calculateFeaturesMetadata
#'
#' @description Function to calculate feature quality indicators
#' using the \pkg{MetaClean} package via the \pkg{patRoon} interface.
#' See \code{?MetaClean} for further details.
#'
#' @param object An \linkS4class{msData} object containing features.
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

  assertClass(object, "msData")

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
