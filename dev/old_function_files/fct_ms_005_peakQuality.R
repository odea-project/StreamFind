

#' @title calculateSNR
#'
#' @description Calculates the signal-to-noise (sn) ratio for peaks in a
#' given \linkS4class{msData} or \linkS4class{msAnalysis} object.
#' If features are present in the \linkS4class{msData}, the minimum sn of
#' the corresponding peaks group are returned in the metadata slot.
#' The argument \code{targetsID} can be used to specify for which peaks/features
#' the sn is to be calculated.
#'
#' @param object An \linkS4class{msData} object.
#' @param targetsID Optionally, a character vector with the \emph{id} of
#' the features to calculate the sn ratio.
#' @param rtExpand The time, in seconds,
#' expansion before and after the time window of each feature
#' to extract centroids for estimation of the noise level.
#' @param filtered Logical, set to \code{TRUE} for calculating the sn of
#' peaks/features filtered. The default is \code{FALSE}.
#'
#' @details The noise is estimated by the maximum intensity of the background
#' mass traces (i.e., traces belonging to the same mass bin as the peak but
#' taken from a wider and predefined time window (\code{rtExpand}) before
#' and after the peak limit. The sn is then calculated by dividing the maximum
#' intensity of the peak with the estimated noise. Mass traces that belong to
#' other peaks within the same mass and time windows are excluded before
#' noise estimation.
#'
#' @return An \linkS4class{msData} or \linkS4class{msAnalysis} object with the
#' noise and sn columns amended to the peaks slot. When features are present,
#' the columns noise, noise_sd, sn and sn_sd are added to the slot metadata of
#' the \linkS4class{msFeatures} object in the given \linkS4class{msData}.
#'
#' @export
#'
calculateSNR <-  function(object,
                          targetsID = NULL,
                          rtExpand = 200,
                          filtered = FALSE,
                          run_parallel = FALSE) {

  valid <- FALSE

  if (testClass(object, "msData") | testClass(object, "msAnalysis"))
    valid = TRUE

  if (!valid) {
    warning("Invalid class object used as argument!")
    return(object)
  }

  if (testClass(object, "msData") & nrow(object@features@metadata) > 0) {
    peaks_org <- peaks(object)
    feats_org <- object@features@metadata
    with_features <- TRUE
  } else {
    peaks_org <- peaks(object)

    if (testClass(object, "msAnalysis")) {
      peaks_org$analysis <- analysisNames(object)
    }

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
    TRUE %in% !peaks_sn$filtered
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
  pks <- peaks_sn[, .(id, mz, rt, mzmin, mzmax, rtmin, rtmax, analysis)]

  cat("Extracting mass traces... ")

  eic <- peakEICs(object,
    analyses = ana,
    targetsID = pks$id,
    rtExpand = rtExpand,
    run_parallel = run_parallel
  )

  ncent <- copy(eic)

  cat("Done! \n")

  cat(paste0("Calculating signal-to-noise ratio for ",
             nrow(peaks_sn), " peaks... "))

  peaks_sn$unique_id <- paste0(peaks_sn$id, "/", peaks_sn$analysis)

  int <- peaks_sn$intensity
  names(int) <- peaks_sn$unique_id

  noise_data_raw <- lapply(peaks_sn$unique_id, function(x, ncent, peaks_sn) {

    temp <- peaks_sn[unique_id %in% x, ]
    temp2 <- ncent[(analysis %in% temp$analysis) & (id %in% temp$id), ]

    temp2 <- temp2[!((temp2$rt >= temp$rtmin) & (temp2$rt <= temp$rtmax)), ]

    if (nrow(temp2) > 1) {
      #remove other peaks within the same mass and time deviation
      others <- peaks_sn[
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

  names(noise_data_raw) <- peaks_sn$unique_id

  noise_data_sd <- lapply(noise_data_raw, function(x) sd(x, na.rm = TRUE))

  noise_data <- lapply(noise_data_raw, function(x) {
    ifelse(length(x) > 0, max(x, na.rm = TRUE), 0)
  })

  for (pp in peaks_sn$unique_id) {

    ana <- 

    if (noise_data[[pp]] != 0) {
      peaks_sn[unique_id %in% pp, sn_value :=
                 round(int[[pp]] / noise_data[[pp]], digits = 0)]
    } else {
      peaks_sn[unique_id %in% pp, sn_value := NA]
    }

    peakcentN <- nrow(eic[
      id %in% peaks_sn[unique_id %in% pp, id] &
      analysis %in% peaks_sn[unique_id %in% pp, analysis] & 
      rt >= peaks_sn[unique_id %in% pp, rtmin] &
      rt <= peaks_sn[unique_id %in% pp, rtmax], ])

    noisecentN <- sapply(noise_data_raw, function(x) length(x))
    peaks_sn[unique_id %in% pp, sn_pN := peakcentN]
    peaks_sn[unique_id %in% pp, sn_nN := noisecentN[pp]]

    peaks_sn[unique_id %in% pp, sn_noise_sd := round(noise_data_sd[[pp]], digits = 0)]
    peaks_sn[unique_id %in% pp, sn_noise := round(noise_data[[pp]], digits = 0)]
  }

  org_unique_ids <- paste0(peaks_org$id, "/", peaks_org$analysis)
  peaks_org$unique_id <- org_unique_ids

  peaks_org[unique_id %in% peaks_sn$unique_id,
    c("sn_pN", "sn_nN", "sn_noise", "sn_noise_sd", "sn_value")] <-
    peaks_sn[, .(sn_pN, sn_nN, sn_noise, sn_noise_sd, sn_value)]

  if (testClass(object, "msData")) {
    object@analyses <- lapply(object@analyses, function(x, peaks_org) {
      ana <- analysisNames(x)
      temp <- peaks_org[analysis %in% ana, ]
      temp[, `:=`(unique_id = NULL, analysis = NULL, replicate = NULL)]
      x@peaks <- copy(temp)
      return(x)
    }, peaks_org = peaks_org)

  } else {
    peaks_org[, analysis := NULL]
    peaks_org[, unique_id := NULL]
    object@peaks <- copy(peaks_org)
  }

  cat("Done! \n")

  if (with_features) {
    
    feat_org <- copy(object@features@metadata)

    for (ff in unique(peaks_sn$feature)) {

      toFeat <- peaks_sn[feature %in% ff,
                        .(sn_pN, sn_nN, sn_noise, sn_noise_sd, sn_value)]

      toFeat <- toFeat[sn_value == max(sn_value, na.rm = TRUE), ]
      toFeat <- toFeat[1, ]
      feat_org[id %in% ff,
        c("sn_pN", "sn_nN", "sn_noise", "sn_noise_sd", "sn_value")] <-
        toFeat
    }
    object@features@metadata <- copy(feat_org)
  }

  validObject(object)

  return(object)
}
