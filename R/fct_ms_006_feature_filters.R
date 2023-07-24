
#' minIntensityFeatures
#'
#' @param features The feature data.table.
#' @param value A numerical value with the desired threshold.
#'
#' @noRd
#'
minIntensityFeatures <- function(features, value = 5000) {
  sel <- features$intensity <= value & !features$filtered
  features$filtered[sel] <- TRUE
  features$filter[sel] <- "minIntensity"
  features
}

#' minSnRationFeatures
#'
#' @param fts The feature data.table.
#' @param value A numerical value with the desired threshold.
#'
#' @noRd
#'
minSnRationFeatures <- function(fts, value = 3) {
  if ("sn" %in% colnames(fts)) {
    sel <- fts$sn <= value & !fts$filtered
    fts$filtered[sel] <- TRUE
    fts$filter[sel] <- "minSnRatio"
  }
  fts
}

#' maxReplicateIntensityDeviationFeatures
#'
#' @param fts The feature data.table.
#' @param value A numerical value with the desired threshold.
#'
maxReplicateIntensityDeviationFeatures <- function(features, groups_sd, value = 40) {

  if (nrow(groups_sd) == 0) {
    warning("Feature groups not present to filter replicate intensities!")
    return(features)
  }




  browser()

  feats_int <- features(obj, average = TRUE)
  #
  # rpl <- unique(replicateNames(obj)[!blankReplicateNames(obj) == replicateNames(obj)])
  # rplSD <- paste0(rpl, "_sd")
  #
  # check <- apply(feats_int[, rplSD, with = FALSE], MARGIN = 1, function(x, value) {
  #   all(x > value, na.rm = TRUE)
  # }, value = value)
  #
  # temp_mtd <- copy(obj@metadata)
  # temp_mtd[is.na(filter) & check, `:=`(filtered = TRUE,
  #                                      filter = "maxReplicateIntensityDeviation")]
  # obj@metadata <- copy(temp_mtd)
  #
  # return(obj)
}
