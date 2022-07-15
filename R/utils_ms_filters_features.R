

#' minIntensity
#'
#' @param obj An \linkS4class{ntsData} object.
#' @param value A numerical value with the desired minimum intensity for features.
#'
minIntensity <- function(obj, value = 5000) {


  feat_mtd <- obj@features@metadata

  rpl <- unique(replicates(obj)[!blanks(obj) == replicates(obj)])

  check <- apply(feat_int[, rpl, with = FALSE], MARGIN = 1, function(x) {
    all(x < value, na.rm = TRUE)
  })

  feats_org[is.na(filterTag) & check, filterTag := "minIntensity"]

  obj@features <- feats_org

  return(obj)
}




#' blankThreshold
#'
#' @param obj An \linkS4class{msData} object.
#' @param value A numerical value to multiply the assigned blank intensity.
#'
blankThreshold <- function(obj, value = 3) {

  feats_org <- features(obj)

  if (!"filterTag" %in% colnames(feats_org)) feats_org[, filterTag := NA_character_]

  blk <- blanks(obj)[!blanks(obj) == replicates(obj)]
  rpl <- replicates(obj)[!blanks(obj) == replicates(obj)]
  names(rpl) <- blk
  rpl <- rpl[!duplicated(rpl)]

  allRpl <- unique(replicates(obj))

  temp <- feats_org[, allRpl, with = FALSE]

  for (r in seq_len(length(rpl))) {
    rp <- rpl[r]
    bl <- names(rpl)[r]
    temp[, (rp) := temp[, rp, with = FALSE] < temp[, bl, with = FALSE] * value]
  }

  check <- apply(temp[, rpl, with = FALSE], MARGIN = 1, function(x) all(x, na.rm = TRUE))

  feats_org[is.na(filterTag) & check, filterTag := "blankThreshold"]

  obj@features <- feats_org

  return(obj)
}




#' maxReplicateIntensityDeviation
#'
#' @param obj An \linkS4class{msData} object.
#' @param value A numerical value set at the desired sd percentage maximum.
#'
maxReplicateIntensityDeviation <- function(obj, value = 30) {

  feats_org <- features(obj)

  if (!"filterTag" %in% colnames(feats_org)) feats_org[, filterTag := NA_character_]

  rpl <- unique(replicates(obj)[!blanks(obj) == replicates(obj)])
  rplSD <- paste0(rpl, "_sd")

  check <- apply(feats_org[, rplSD, with = FALSE], MARGIN = 1, function(x) {
    all(x > value, na.rm = TRUE)
  })

  feats_org[is.na(filterTag) & check, filterTag := "maxReplicateIntensityDeviation"]

  obj@features <- feats_org

  return(obj)
}




#' minReplicateAbundance
#'
#' @param obj An \linkS4class{msData} object.
#' @param value A numerical value set at the desired minimum representation in replicates.
#'
minReplicateAbundance <- function(obj, value = 3) {

  feats_org <- features(obj)

  if (!"filterTag" %in% colnames(feats_org)) feats_org[, filterTag := NA_character_]

  check <- feats_org$npeaks
  check <- unlist(lapply(check, function(x) all(x < value)))

  feats_org[is.na(filterTag) & check, filterTag := "minReplicateAbundance"]

  obj@features <- feats_org

  return(obj)
}




#' snRatio
#'
#' @param obj An \linkS4class{msData} object.
#' @param snRatio A numerical value set at the desired signal-to-noise ratio for features.
#'
snRatio <- function(obj, value = 10) {

  feats_org <- features(obj)

  if (!("sn_value" %in% colnames(feats_org))) {
    warning("Signal-to-noise ratio values not found!")
    return(obj)
  }

  if (!"filterTag" %in% colnames(feats_org)) feats_org[, filterTag := NA_character_]

  check <- feats_org$sn_value > value | is.na(feats_org$sn_value)

  feats_org[is.na(filterTag) & !check, filterTag := "snRatio"]

  obj@features <- feats_org

  return(obj)
}
