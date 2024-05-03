
#' @title .s3_ms_load_features_ms1.Settings_load_features_ms1_StreamFind
#'
#' @description Loads the ms1 spectra for each feature.
#'
#' @noRd
#'
.s3_ms_load_features_ms1.Settings_load_features_ms1_StreamFind <- function(settings, self, private) {
  
  if (!any(self$has_features())) {
    warning("Features not found! Not loaded.")
    return(invisible(self))
  }
  
  parameters <- settings$parameters
  
  cached_ms1 <- FALSE
  
  if (.caches_data()) {
    ana_feats <- self$get_features(filtered = TRUE)
    ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
    hash <- patRoon::makeHash(ana_feats, parameters)
    ms1 <- patRoon::loadCacheData("load_features_ms1", hash)
    
    if (!is.null(ms1)) {
      if (all(ms1$id %in% ana_feats$feature)) {
        message("\U2139 Features MS1 spectra loaded from cache!")
        cached_ms1 <- TRUE
      } else {
        ms1 <- NULL
      }
    } else {
      ms1 <- NULL
    }
    
  } else {
    hash <- NULL
    ms1 <- NULL
  }
  
  if (is.null(ms1)) {
    ms1 <- self$get_features_ms1(
      rtWindow = parameters$rtWindow,
      mzWindow = parameters$mzWindow,
      mzClust = parameters$mzClust,
      presence = parameters$presence,
      minIntensity = parameters$minIntensity,
      filtered = parameters$filtered,
      loaded = FALSE
    )
  }
  
  analyses <- self$get_analysis_names()
  
  features <- self$feature_list
  
  ms1_col <- lapply(analyses, function(x, ms1, features) {
    
    ana_ms1 <- ms1[ms1$analysis %in% x, ]
    
    fts_all <- features[[x]]$feature
    
    fts_ms1 <- lapply(fts_all, function(z, ana_ms1) {
      
      ft_ms1 <- ana_ms1[ana_ms1$feature %in% z, ]
      
      if (nrow(ft_ms1) > 0) {
        ft_ms1[["group"]] <- NULL
        ft_ms1[["analysis"]] <- NULL
        ft_ms1[["feature"]] <- NULL
        ft_ms1
        
      } else {
        NULL
      }
    }, ana_ms1 = ana_ms1)
    
    fts_ms1
    
  }, ms1 = ms1, features = features)
  
  if (private$.add_features_column("ms1", ms1_col)) {
    
    message("\U2713 MS1 spectra added to features!")
    
    TRUE
    
  } else {
    FALSE
  }
}
