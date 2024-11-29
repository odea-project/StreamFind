
# ______________________________________________________________________________________________________________________
# patRoon -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_LoadMSPeakLists_patRoon**
#'
#' @description Settings for loading MS2 and MS1 spectra for feature groups.
#'
#' @param maxMSRtWindow Maximum chromatographic peak window used for spectrum 
#' averaging (in seconds, +/- retention time). If NULL all spectra from a feature 
#' will be taken into account. Lower to decrease processing time.
#' @param precursorMzWindow The m/z window (in Da) to find MS/MS spectra of a precursor. 
#' This is typically used for Data-Dependent like MS/MS data and should correspond to the 
#' isolation m/z window (i.e. +/- the precursor m/z) that was used to collect the data. 
#' For Data-Independent MS/MS experiments, where precursor ions are not isolated prior to 
#' fragmentation (e.g. bbCID, MSe, all-ion, ...) the value should be NULL.
#' @param clusterMzWindow m/z window (in Da) used for clustering m/z values
#' when spectra are averaged. For method="hclust" this corresponds to the
#' cluster height, while for method="distance" this value is used to find
#' nearby masses (+/- window). Too small windows will prevent clustering
#' m/z values (thus erroneously treating equal masses along spectra as
#' different), whereas too big windows may cluster unrelated m/z values
#' from different or even the same spectrum together.
#' @param topMost Only retain this maximum number of MS peaks when generating
#' averaged spectra. Lowering this number may exclude more irrelevant (noisy)
#' MS peaks and decrease processing time, whereas higher values may avoid
#' excluding lower intense MS peaks that may still be of interest.
#' @param minIntensityPre MS peaks with intensities below this value will
#' be removed (applied prior to selection by `topMost`) before averaging.
#' @param minIntensityPost MS peaks with intensities below this value will
#' be removed after averaging.
#' @param avgFun Function that is used to calculate average m/z values.
#' @param method Method used for producing averaged MS spectra. Valid
#' values are "hclust", used for hierarchical clustering (using the
#' fastcluster package), and "distance", to use the between peak distance.
#' The latter method may reduces processing time and memory requirements,
#' at the potential cost of reduced accuracy.
#' @param retainPrecursorMSMS For MS/MS data only: if TRUE then always
#' retain the precursor mass peak even if is not among the `topMost` peaks.
#' Note that MS precursor mass peaks are always kept. Furthermore, note
#' that precursor peaks in both MS and MS/MS data may still be removed by
#' intensity thresholds (this is unlike the filter method function).
#'
#' @return A `MassSpecSettings_LoadMSPeakLists_patRoon` object.
#'
#' @export
#'
MassSpecSettings_LoadMSPeakLists_patRoon <- S7::new_class("MassSpecSettings_LoadMSPeakLists_patRoon",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(maxMSRtWindow = 5,
                         precursorMzWindow = 4,
                         clusterMzWindow = 0.005,
                         topMost = 100,
                         minIntensityPre = 50,
                         minIntensityPost = 50,
                         avgFun = "mean",
                         method = "hclust",
                         retainPrecursorMSMS = TRUE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "LoadMSPeakLists",
      required = c("FindFeatures", "GroupFeatures"),
      algorithm = "patRoon",
      parameters = list(
        maxMSRtWindow = maxMSRtWindow,
        precursorMzWindow = precursorMzWindow,
        clusterMzWindow = clusterMzWindow,
        topMost = topMost,
        minIntensityPre = minIntensityPre,
        minIntensityPost = minIntensityPost,
        avgFun = avgFun,
        method = method,
        retainPrecursorMSMS = retainPrecursorMSMS
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "patRoon",
      developer = "Rick Helmus",
      contact = "r.helmus@uva.nl",
      link = "https://github.com/rickhelmus/patRoon",
      doi = "10.21105/joss.04029"
    ))
  },
  
  validator = function(self) {
      checkmate::assert_choice(self@engine, "MassSpec")
      checkmate::assert_choice(self@method, "LoadMSPeakLists")
      checkmate::assert_choice(self@algorithm, "patRoon")
      checkmate::assert_numeric(self@parameters$maxMSRtWindow, len = 1)
      checkmate::assert_numeric(self@parameters$precursorMzWindow, len = 1)
      checkmate::assert_numeric(self@parameters$clusterMzWindow, len = 1)
      checkmate::assert_numeric(self@parameters$topMost, len = 1)
      checkmate::assert_numeric(self@parameters$minIntensityPre, len = 1)
      checkmate::assert_numeric(self@parameters$minIntensityPost, len = 1)
      checkmate::assert_character(self@parameters$avgFun)
      checkmate::assert_choice(self@parameters$method, c("hclust", "distance"))
      checkmate::assert_logical(self@parameters$retainPrecursorMSMS, len = 1)
    
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_LoadMSPeakLists_patRoon) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }
  
  nts <- engine$nts
  
  if (!nts@has_groups) {
    warning("NTS object does not have feature groups! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  parameters$avgFun <- get(parameters$avgFun)
  
  av_args <- list(
    clusterMzWindow =  parameters$clusterMzWindow,
    topMost = parameters$topMost,
    minIntensityPre = parameters$minIntensityPre,
    minIntensityPost = parameters$minIntensityPost,
    avgFun = parameters$avgFun,
    method = parameters$method,
    pruneMissingPrecursorMS = TRUE,
    retainPrecursorMSMS = parameters$retainPrecursorMSMS
  )
  
  mspl <- patRoon::generateMSPeakLists(
    nts$features,
    algorithm = "mzr",
    maxMSRtWindow = parameters$maxMSRtWindow,
    precursorMzWindow = parameters$precursorMzWindow,
    topMost = parameters$topMost,
    avgFeatParams = av_args,
    avgFGroupParams = av_args
  )
  
  nts$mspl <- mspl
  engine$nts <- nts
  message("\U2713 MSPeakLists loaded!")
  TRUE
}

# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_LoadMSPeakLists_StreamFind**
#'
#' @description Settings for converting loaded MS2 and MS1 spectra into a `MSPeakLists` object from patRoon.
#'
#' @param clusterMzWindow m/z window (in Da) used for clustering m/z values
#' when spectra are averaged. For method="hclust" this corresponds to the
#' cluster height, while for method="distance" this value is used to find
#' nearby masses (+/- window). Too small windows will prevent clustering
#' m/z values (thus erroneously treating equal masses along spectra as
#' different), whereas too big windows may cluster unrelated m/z values
#' from different or even the same spectrum together.
#' @param topMost Only retain this maximum number of MS peaks when generating
#' averaged spectra. Lowering this number may exclude more irrelevant (noisy)
#' MS peaks and decrease processing time, whereas higher values may avoid
#' excluding lower intense MS peaks that may still be of interest.
#' @param minIntensityPre MS peaks with intensities below this value will
#' be removed (applied prior to selection by `topMost`) before averaging.
#' @param minIntensityPost MS peaks with intensities below this value will
#' be removed after averaging.
#' @param avgFun Character with the function name that is used to calculate average m/z values.
#' @param method Method used for producing averaged MS spectra. Valid
#' values are "hclust", used for hierarchical clustering (using the
#' fastcluster package), and "distance", to use the between peak distance.
#' The latter method may reduces processing time and memory requirements,
#' at the potential cost of reduced accuracy.
#'
#' @return A `MassSpecSettings_LoadMSPeakLists_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_LoadMSPeakLists_StreamFind <- S7::new_class("MassSpecSettings_LoadMSPeakLists_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(clusterMzWindow = 0.005,
                         topMost = 100,
                         minIntensityPre = 50,
                         minIntensityPost = 50,
                         avgFun = "mean",
                         method = "distance") {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "LoadMSPeakLists",
      required = c("FindFeatures", "GroupFeatures", "LoadFeaturesMS1", "LoadFeaturesMS2"),
      algorithm = "StreamFind",
      parameters = list(
        clusterMzWindow = clusterMzWindow,
        topMost = topMost,
        minIntensityPre = minIntensityPre,
        minIntensityPost = minIntensityPost,
        avgFun = avgFun,
        method = method
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "LoadMSPeakLists")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_numeric(self@parameters$clusterMzWindow, len = 1)
    checkmate::assert_numeric(self@parameters$topMost, len = 1)
    checkmate::assert_numeric(self@parameters$minIntensityPre, len = 1)
    checkmate::assert_numeric(self@parameters$minIntensityPost, len = 1)
    checkmate::assert_character(self@parameters$avgFun)
    checkmate::assert_choice(self@parameters$method, c("hclust", "distance"))
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_LoadMSPeakLists_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }
  
  nts <- engine$nts
  
  if (!nts@has_groups) {
    warning("NTS object does not have feature groups! Not done.")
    return(FALSE)
  }
  
  if (!(nts$has_features_ms1 && nts$has_features_ms2)) {
    warning("Features MS1 and/or MS2 not loaded! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  parameters$avgFun <- get(parameters$avgFun)
  
  mspl <- .convert_ms1_ms2_columns_to_MSPeakLists(engine, parameters)

  nts$mspl <- mspl
  engine$nts <- nts
  message("\U2713 MSPeakLists loaded!")
  TRUE
}

#' @noRd
.convert_ms1_ms2_columns_to_MSPeakLists <- function(engine, parameters) {
  
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  
  half_clustWindow <- parameters$clusterMzWindow / 2
  
  correct_spectrum <- function(s, t, out) {
    
    if (length(s) > 1) s <- s[1]
    
    names(s) <- t
    
    if (length(s[[1]]) > 0) {
      n_traces <- nrow(s[[1]])
      
      if (n_traces > 0) {
        s[[1]]$id <- seq_len(n_traces)
        if (!"is_pre" %in% colnames(s[[1]])) s[[1]]$is_pre <- rep(FALSE, n_traces)
        cols_to_keep <- c("id", "mz", "intensity", "is_pre")
        s[[1]] <- s[[1]][, cols_to_keep, with = FALSE]
        colnames(s[[1]]) <- c("ID", "mz", "intensity", "precursor")
      } else {
        s <- NULL
      }
    } else {
      s <- NULL
    }
    
    out <- c(out, s)
    out
  }
  
  feature_list <- engine$nts$feature_list
  
  plist <- lapply(feature_list, function(x, correct_spectrum) {
    
    features <- x[!x$filtered, ]
    
    groups <- unique(features$group)
    
    groups <- groups[!is.na(groups)]
    
    if (length(groups) == 0) return(NULL)
    
    glist <- lapply(groups, function(x2, features, correct_spectrum) {
      
      out <- list()
      
      MS <- features$ms1[features$group %in% x2]
      
      if (length(MS) > 1) MS <- MS[1]
      
      if (!is.null(MS[[1]])) {
        
        if (!"is_pre" %in% colnames(MS[[1]])) MS[[1]]$is_pre <- rep(FALSE, nrow(MS[[1]]))
        
        t_mz_min <- features$mzmin[features$group %in% x2]
        t_mz_max <- features$mzmax[features$group %in% x2]
        
        MS[[1]]$is_pre <- vapply(MS[[1]]$mz, function(x3, t_mz_min, t_mz_max) {
            x3 >= t_mz_min - half_clustWindow & x3 <= t_mz_max + half_clustWindow
          }, t_mz_min = t_mz_min, t_mz_max = t_mz_max, FALSE
        )
      }
      
      MSMS <- features$ms2[features$group %in% x2]
      
      out <- correct_spectrum(MS, "MS", out)
      
      out <- correct_spectrum(MSMS, "MSMS", out)
      
      out
      
    }, features = features, correct_spectrum = correct_spectrum)
    
    names(glist) <- groups
    
    glist = glist[order(names(glist))]
    
    glist
    
  }, correct_spectrum = correct_spectrum)
  
  names(plist) <- engine$get_analysis_names()
  
  plist <- plist[vapply(plist, function(x) length(x) > 0, FALSE)]
  
  run_list <- lapply(engine$get_analysis_names(), function(x) engine$get_spectra_headers(x))
  
  mlist <- Map(function(x, y) {
    
    features <- x[!x$filtered, ]
    
    groups <- unique(features$group)
    
    groups <- groups[!is.na(groups)]
    
    pol_col <- as.character(y$polarity)
    
    pol_key = c(1, 0, -1)
    
    names(pol_key) <- c("1", "-1", "0")
    
    y$polarity <- pol_key[pol_col]
    
    setnames(y, c("index", "level", "ce", "pre_mz"), c("seqNum", "msLevel", "collisionEnergy", "precursorMZ"), skip_absent = TRUE)
    
    glist <- lapply(groups, function(x2, features, y) {
      
      out <- list()
      
      ft <- features[features$group %in% x2, ]
      
      if (nrow(ft) > 0) {
        MS <- y[y$rt >= ft$rtmin & y$rt <= ft$rtmax & y$msLevel == 1, ]
        
        if (nrow(MS) > 0) out[["MS"]] <- MS
        
        MSMS <- y[y$rt >= ft$rtmin & y$rt <= ft$rtmax & y$precursorMZ >= ft$mzmin - 1.3/2 & y$precursorMZ <= ft$mzmax + 1.3/2 & y$msLevel == 2, ]
        
        if (nrow(MSMS) > 0) out[["MSMS"]] <- MSMS
      }
      
      out
      
    }, features = features, y = y)
    
    names(glist) <- groups
    
    glist = glist[order(names(glist))]
    
    glist
    
  }, feature_list, run_list)
  
  names(mlist) <- engine$get_analysis_names()
  
  mlist <- mlist[vapply(mlist, function(x) length(x) > 0, FALSE)]
  
  groups <- lapply(feature_list, function(x) x$group[!x$filtered])
  
  groups <- unique(unlist(groups))
  
  groups <- groups[!is.na(groups)]
  
  pat_param <- list(
    "clusterMzWindow" = parameters$clusterMzWindow,
    "topMost" = parameters$topMost,
    "minIntensityPre" = parameters$minIntensityPre,
    "minIntensityPost" = parameters$minIntensityPost,
    "avgFun" = parameters$avgFun,
    "method" = parameters$method,
    "pruneMissingPrecursorMS" = FALSE,
    "retainPrecursorMSMS" = TRUE
  )
  
  pol <- engine$get_spectra_polarity()[names(plist)]
  
  if (length(unique(pol)) > 1) {
    
    plist_pos <- plist[pol %in% "positive"]
    
    mlist_pos <- mlist[pol %in% "positive"]
    
    groups_pos <- unique(unlist(lapply(plist_pos, function(x) names(x))))
    
    pl_pos <- new("MSPeakLists",
                  peakLists = plist_pos,
                  metadata = mlist_pos,
                  avgPeakListArgs = pat_param,
                  origFGNames = groups_pos,
                  algorithm = "mzr"
    )
    
    plist_neg <- plist[pol %in% "negative"]
    mlist_neg <- mlist[pol %in% "negative"]
    groups_neg <- unique(unlist(lapply(plist_neg, function(x) names(x))))
    
    pl_neg <- new("MSPeakLists",
                  peakLists = plist_neg,
                  metadata = mlist_neg,
                  avgPeakListArgs = pat_param,
                  origFGNames = groups_neg,
                  algorithm = "mzr"
    )
    
    plfinal <- new("MSPeakListsSet",
                   analysisInfo = engine$nts$analysisInfo,
                   peakLists = plist,
                   metadata = mlist,
                   avgPeakListArgs = pat_param,
                   origFGNames = unique(groups),
                   algorithm = "mzr-set",
                   setObjects = list("positive" = pl_pos, "negative" = pl_neg)
    )
    
  } else {
    plfinal <- new("MSPeakLists",
                   peakLists = plist,
                   metadata = mlist,
                   avgPeakListArgs = pat_param,
                   origFGNames = groups,
                   algorithm = "mzr"
    )
  }
  
  plfinal
}
