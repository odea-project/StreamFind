
#' @title .s3_ms_load_MSPeakLists.Settings_load_MSPeakLists_patRoon
#'
#' @description Creates a MSPeakLists object from patRoon.
#'
#' @noRd
#'
.s3_ms_load_MSPeakLists.Settings_load_MSPeakLists_patRoon <- function(settings, self, private) {
  
  if (!self$has_groups()) {
    warning("Feature groups not found! Not loaded.")
    return(FALSE)
  }
  
  parameters <- settings$parameters
  
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
    self$featureGroups,
    algorithm = "mzr",
    maxMSRtWindow = parameters$maxMSRtWindow,
    precursorMzWindow = parameters$precursorMzWindow,
    topMost = parameters$topMost,
    avgFeatParams = av_args,
    avgFGroupParams = av_args
  )
  
  if ("MSPeakLists" %in% is(mspl)) {
    
    private$.results$patRoon$mspl <- mspl
    
    message("\U2713 MSPeakLists loaded!")
    
    TRUE
    
  } else {
    warning("MSPeakLists not returned from patRoon! Not loaded.")
    FALSE
  }
}

#' @title .s3_ms_load_MSPeakLists.Settings_load_MSPeakLists_StreamFind
#'
#' @description Converts loaded ms1 ans ms2 spectra into a MSPeakLists object from patRoon.
#'
#' @noRd
#'
.s3_ms_load_MSPeakLists.Settings_load_MSPeakLists_StreamFind <- function(settings, self, private) {
  
  if (!self$has_groups()) {
    warning("Feature groups not found! Not loaded.")
    return(FALSE)
  }
  
  if (!(self$has_loaded_features_ms1() && self$has_loaded_features_ms2())) {
    warning("Features MS1 and/or MS2 not loaded! Not done.")
    return(FALSE)
  }
  
  parameters <- settings$parameters
  
  mspl <- .convert_ms1_ms2_columns_to_MSPeakLists(self, parameters)
  
  if ("MSPeakLists" %in% is(mspl)) {
    
    private$.results$patRoon$mspl <- mspl

    message("\U2713 MSPeakLists loaded!")
    
    TRUE
    
  } else {
    warning("MSPeakLists not returned! Not loaded.")
    FALSE
  }
}

#' .convert_ms1_ms2_columns_to_MSPeakLists
#'
#' @description Helper function to Convert loaded ms1 ans ms2 spectra into a MSPeakLists object from patRoon.
#'
#' @noRd
#'
.convert_ms1_ms2_columns_to_MSPeakLists <- function(self, parameters) {
  
  half_clustWindow <- parameters$clusterMzWindow / 2
  
  correct_spectrum <- function(s, t, out) {
    
    if (length(s) > 1) s <- s[1]
    
    names(s) <- t
    
    if (!is.null(s[[1]])) {
      n_traces <- nrow(s[[1]])
      
      if (n_traces > 0) {
        s[[1]][["id"]] <- seq_len(n_traces)
        
        if (!"is_pre" %in% colnames(s[[1]])) {
          s[[1]][["is_pre"]] <- rep(FALSE, n_traces)
        }
        
        cols_to_keep <- c("id", "mz", "intensity", "is_pre")
        s[[1]] <- s[[1]][, cols_to_keep, with = FALSE]
        
        colnames(s[[1]]) <- c("ID", "mz", "intensity", "precursor")
      }
    }
    
    out <- c(out, s)
    
    out
  }
  
  plist <- lapply(self$feature_list, function(x, correct_spectrum) {
    
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
          
        MS[[1]][["is_pre"]] <- vapply(MS[[1]][["mz"]], function(x, t_mz_min, t_mz_max) {
            x >= t_mz_min - half_clustWindow  & x <= t_mz_max + half_clustWindow
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
  
  names(plist) <- self$get_analysis_names()
  
  plist <- plist[vapply(plist, function(x) length(x) > 0, FALSE)]
  
  run_list <- lapply(self$get_analysis_names(), function(x) self$get_spectra_headers(x))
  
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
    
  }, self$feature_list, run_list)
  
  names(mlist) <- self$get_analysis_names()
  
  mlist <- mlist[vapply(mlist, function(x) length(x) > 0, FALSE)]
  
  groups <- lapply(self$feature_list, function(x) x$group)
  
  groups <- unique(unlist(groups))
  
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
  
  pol <- self$get_spectra_polarity()[names(plist)]
  
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
      analysisInfo = self$analysisInfo,
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
