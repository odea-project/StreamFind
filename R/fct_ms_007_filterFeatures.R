

### filterFeatures ------------------------------------------------------------------------------------------

#' @title filterFeatures
#'
#' @description Filter features in an \linkS4class{msData} or \linkS4class{msFeatures} object.
#'
#' @details Filters are added as an \linkS4class{settings} object with
#' the slot call defined as "filterFeatures", the slot algorithm set to "filter"
#' and the filtering parameters are given as an ordered list in the settings slot.
#' The available filters are as follows:
#' \itemize{
#'  \item \code{minIntensity}: features below a minimum intensity threshold.
#' For example, minIntensity = 3000, removes features with maximum
#' peak representation below 3000 counts;
#'  \item \code{blankThreshold}: features that are not more intense than a defined
#' threshold multiplier of the assigned blank intensity.
#' For example, blankThreshold = 3, features with maximum peak representation
#' that are not higher than 3 times the blank intensity;
#'  \item \code{maxReplicateIntensityDeviation} features based on a
#' maximum standard deviation (SD), in percentage, among replicate analyses.
#' For example, maxReplicateIntensityDeviation = 30, filters features that do not have
#' the SD below 30% in at least one analysis replicate group.
#'  \item \code{minReplicateAbundance}: features that are not present with at least
#' a specified frequency in one analysis replicate group.
#' For example, minReplicateAbundance = 2, filters features that are not represented
#' in at least two analyses within a replicate.
#'  \item \code{excludeIsotopes} features annotated as isotopes are excluded (i.e., filtered).
#'  \item \code{excludeAdducts} features annotated as adducts are filtered when the corresponding
#'  protonated/deprotonated ion is present.
#'  \item \code{snRatio} features below a minimum signal-to-noise (s/n) ratio threshold.
#' For example, snRatio = 3, filters features with a maximum s/n in corresponding peaks below a 3.
#' }
#'
#' @note The \linkS4class{settings} object with the filters can be added
#' directly to the \code{object}. When \code{settings} is \code{NULL}, the filters
#' in the \code{object} are used instead.
#'
#' @param object An \linkS4class{msData} or \linkS4class{msFeatures} object.
#' @template args-single-settings
#'
#' @export
#'
filterFeatures <- function(object, settings = NULL) {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msFeatures"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(object)
  }

  if (checkmate::testClass(object, "msData")) {
    obj <- object@features
  } else {
    obj <- object
  }


  if (is.null(settings)) {
    prs <- getParameters(obj, call = "filterFeatures")

    if (length(prs) > 0) {
      if (length(prs) > 1) {
        # TODO add merge filter settings for more than 1 settings object
      }

      algorithm = getAlgorithm(prs)
      filterList = getSettings(prs)

    } else {

      algorithm <- NA_character_
    }

  } else if (checkmate::testClass(settings, "settings")) {

    algorithm <- getAlgorithm(settings)
    filterList <- getSettings(settings)

  } else {

    algorithm <- NA_character_
  }

  if (TRUE %in% is.null(algorithm)) {
    warning("Filter settings not defined or not recognized")
    return(object)
  }

  if (length(filterList) == 0) {
    warning("No filters selected or recognized.")
    return(object)
  }

  name_filters <- names(filterList)

  listOfViableFilters <- c(
    "minIntensity",
    "blankThreshold",
    "maxReplicateIntensityDeviation",
    "minReplicateAbundance",
    "excludeIsotopes",
    "excludeAdducts",
    "snRatioThreshold"
  )

  if (!all(name_filters %in% listOfViableFilters)) {
    warning("At least one filters is not recognized.")
    return(object)
  }

  for (i in seq_len(length(name_filters))) {
    switch(names(filterList)[i],
           minIntensity = (obj <- minIntensityFeatures(obj, value = unlist(filterList[i]))),
           blankThreshold = (obj <- blankThresholdFeatures(obj, value = unlist(filterList[i]))),
           maxReplicateIntensityDeviation = (obj <- maxReplicateIntensityDeviationFeatures(obj, value = unlist(filterList[i]))),
           minReplicateAbundance = (obj <- minReplicateAbundanceFeatures(obj, value = unlist(filterList[i]))),
           excludeIsotopes = (obj <- excludeIsotopesFeatures(obj, value = unlist(filterList[i]))),
           excludeAdducts = (obj <- excludeAdductsFeatures(obj, value = unlist(filterList[i]))),
           snRatioThreshold = (obj <- snRatioThresholdFeatures(obj, value = unlist(filterList[i])))
    )
  }

  # TODO when msData add possible section to amend peaks for features filtered out.

  object@features <- obj

  return(object)
}

### removeFilteredFeatures ----------------------------------------------------------------------------------

#' @title removeFilteredFeatures
#'
#' @description Function to remove permanently filtered features from a
#' \linkS4class{msData} or \linkS4class{msFeatures} object.
#'
#' @param object An \linkS4class{msFeatures} object.
#' @param which A character vector with the filter tag/s of features to be removed.
#' The default is \emph{all} to remove all filtered features.
#'
#' @export
#'
#' @importFrom data.table copy
#'
removeFilteredFeatures <- function(object, which = "all") {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msFeatures"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(object)
  }

  if (checkmate::testClass(object, "msData")) {
    temp_mtd <- copy(object@features@metadata)
  } else {
    temp_mtd <- copy(object@metadata)
  }

  if ("all" %in% which) which <- unique(temp_mtd$filter)

  which <- which[!is.na(which)]

  keep_id <- temp_mtd$id[!temp_mtd$filter %in% which]

  object <- object[, keep_id]

  return(object)
}

### filters -------------------------------------------------------------------------------------------------

### minIntensity ---------------------------------------------------------

#' minIntensityFeatures
#'
#' @param obj An \linkS4class{msFeatures} object.
#' @param value A numerical value with the desired minimum intensity for features.
#'
#' @importFrom data.table copy
#'
minIntensityFeatures <- function(obj, value = 5000) {

  feat_int <- copy(obj@intensity)

  check <- apply(feat_int[, id := NULL], MARGIN = 1, function(x, value) {
    all(x < value, na.rm = TRUE)
  }, value = value)

  temp_mtd <- copy(obj@metadata)
  temp_mtd[is.na(filter) & check, `:=`(filtered = TRUE, filter = "minIntensity")]
  obj@metadata <- copy(temp_mtd)

  return(obj)
}

### blankThreshold -------------------------------------------------------

#' blankThresholdFeatures
#'
#' @param obj An \linkS4class{msFeatures} object.
#' @param value A numerical value to multiply the assigned blank intensity.
#'
#' @importFrom data.table copy
#'
blankThresholdFeatures <- function(obj, value = 3) {

  feats_int <- features(obj, average = TRUE)

  blk <- blanks(obj)[!blanks(obj) == replicates(obj)]
  rpl <- replicates(obj)[!blanks(obj) == replicates(obj)]

  names(rpl) <- blk
  rpl <- rpl[!duplicated(rpl)]

  allRpl <- unique(replicates(obj))

  temp <- feats_int[, allRpl, with = FALSE]

  for (r in seq_len(length(rpl))) {
    rp <- rpl[r]
    bl <- names(rpl)[r]
    temp[, (rp) := temp[, rp, with = FALSE] < temp[, bl, with = FALSE] * value]
  }

  check <- apply(temp[, rpl, with = FALSE], MARGIN = 1, function(x) all(x, na.rm = TRUE))

  temp_mtd <- copy(obj@metadata)
  temp_mtd[is.na(filter) & check, `:=`(filtered = TRUE, filter = "blank")]
  obj@metadata <- copy(temp_mtd)

  return(obj)
}

### maxReplicateIntensityDeviation ---------------------------------------

#' maxReplicateIntensityDeviationFeatures
#'
#' @param obj An \linkS4class{msFeatures} object.
#' @param value A numerical value set at the desired sd percentage maximum.
#'
#' @importFrom data.table copy
#'
maxReplicateIntensityDeviationFeatures <- function(obj, value = 40) {

  feats_int <- features(obj, average = TRUE)

  rpl <- unique(replicates(obj)[!blanks(obj) == replicates(obj)])
  rplSD <- paste0(rpl, "_sd")

  check <- apply(feats_int[, rplSD, with = FALSE], MARGIN = 1, function(x, value) {
    all(x > value, na.rm = TRUE)
  }, value = value)

  temp_mtd <- copy(obj@metadata)
  temp_mtd[is.na(filter) & check, `:=`(filtered = TRUE, filter = "maxReplicateIntensityDeviation")]
  obj@metadata <- copy(temp_mtd)

  return(obj)
}

### minReplicateAbundance ------------------------------------------------

#' minReplicateAbundanceFeatures
#'
#' @param obj An \linkS4class{msFeatures} object.
#' @param value A numerical value set at the desired minimum representation in replicates.
#'
#' @importFrom data.table copy
#'
minReplicateAbundanceFeatures <- function(obj, value = 3) {

  feats_org <- copy(obj@metadata)

  rpl <- replicates(obj)
  ana <- analysisNames(obj)
  names(rpl) <- ana

  check <- feats_org$id
  check <- unlist(lapply(check, function(x, feats_org, rpl, value, bl) {
    fts <- feats_org[id %in% x, ]
    if (FALSE %in% fts$filtered) {
      temp <- unlist(fts$peaks)
      temp <- data.table(idx = temp, rep = rpl)
      temp <- temp[, .(idx = sum(idx > 0)), by = rep]
      temp <- temp[!rep %in% bl, ]
      return(!(TRUE %in% (temp$idx >= value)))
    } else {
      return(FALSE)
    }
  }, feats_org = feats_org, rpl = rpl, value = value, bl = unique(blanks(obj))))

  temp_mtd <- copy(obj@metadata)
  temp_mtd[is.na(filter) & check, `:=`(filtered = TRUE, filter = "minReplicateAbundance")]
  obj@metadata <- copy(temp_mtd)

  return(obj)
}

### excludeIsotopes ------------------------------------------------

#' excludeIsotopesFeatures
#'
#' @param obj An \linkS4class{msFeatures} object.
#' @param value Logical, when \code{TRUE} isotopes are filtered.
#'
#' @importFrom data.table copy
#'
excludeIsotopesFeatures <- function(obj, value = TRUE) {

  feats_org <- copy(obj@metadata)

  if ("isonr" %in% colnames(feats_org)) {
    feats_org[is.na(filter) & isonr > 0, `:=`(filtered = TRUE, filter = "isotope")]
  }

  obj@metadata <- copy(feats_org)

  return(obj)
}

### excludeAdducts -------------------------------------------------------

#' excludeAdductsFeatures
#'
#' @param obj An \linkS4class{msFeatures} object.
#' @param value Logical, when \code{TRUE}, adducts with protonated/deprotonated ions present are filtered.
#'
#' @importFrom data.table copy
#'
excludeAdductsFeatures <- function(obj, value = TRUE) {

  feats_org <- copy(obj@metadata)

  if ("neutralMass" %in% colnames(feats_org) & "adduct_ion" %in% colnames(feats_org)) {

    adduct_names <- getParameters(obj, "peakAnnotation")
    adduct_names <- getSettings(adduct_names)
    adduct_names <- adduct_names$extraOpts$rules
    adduct_ion <- adduct_names[massdiff < 1.2, name]
    adduct_names <- adduct_names[massdiff > 1.2, ]

    # TODO improve manner to retrieve adduct names used for features, see below option
    # adduct_names_pos <- data.table::fread(system.file("rules/primary_adducts_pos.csv", package = "CAMERA"), header = TRUE)
    # adduct_names_pos <- adduct_names_pos[massdiff > 1.5, ]
    # adduct_names_neg <- data.table::fread(system.file("rules/primary_adducts_neg.csv", package = "CAMERA"), header = TRUE)
    # adduct_names_neg <- adduct_names_neg[massdiff > 0, ]

    adducts <- feats_org$id[feats_org$adduct_ion %in% adduct_names$name]

    check <- lapply(adducts, function(x, feats_org, adduct_ion) {
      temp <- feats_org[id %in% x, neutralMass]
      temp <- feats_org[neutralMass == temp, ]
      temp <- adduct_ion %in% temp$adduct_ion
      return(temp)
    }, feats_org = feats_org, adduct_ion = adduct_ion)

    names(check) <- adducts
    check <- unlist(check)
    check <- check[check]

    feats_org[is.na(filter) & id %in% names(check), `:=`(filtered = TRUE, filter = "adduct")]
  }

  obj@metadata <- copy(feats_org)

  return(obj)
}

# TODO improve filter sn after final sn calculation
### snRatioThreshold -----------------------------------------------------

#' snRatioThresholdFeatures
#'
#' @param obj An \linkS4class{msFeatures} object.
#' @param snRatio A numerical value set at the desired signal-to-noise ratio for features.
#'
snRatioThresholdFeatures <- function(obj, value = 10) {

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
