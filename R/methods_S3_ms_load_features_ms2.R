
#' @title .s3_ms_load_features_ms2.Settings_load_features_ms2_StreamFind
#'
#' @description Loads the ms2 spectra for each feature.
#'
#' @noRd
#'
.s3_ms_load_features_ms2.Settings_load_features_ms2_StreamFind <- function(settings, self, private) {
  
  if (!any(self$has_features())) {
    warning("Features not found! Not loaded.")
    return(invisible(self))
  }
  
  parameters <- settings$parameters
  
  cached_ms2 <- FALSE
  
  if (.caches_data()) {
    ana_feats <- self$get_features(filtered = TRUE)
    ana_feats <- ana_feats[, c("analysis", "feature"), with = FALSE]
    hash <- patRoon::makeHash(ana_feats, parameters)
    ms2 <- patRoon::loadCacheData("load_features_ms2", hash)
    
    if (!is.null(ms2)) {
      if (all(ms2$id %in% ana_feats$feature)) {
        message("\U2139 Features MS2 spectra loaded from cache!")
        cached_ms2 <- TRUE
      } else {
        ms2 <- NULL
      }
    } else {
      ms2 <- NULL
    }
    
  } else {
    hash <- NULL
    ms2 <- NULL
  }
  
  if (is.null(ms2)) {
    ms2 <- self$get_features_ms2(
      isolationWindow =  parameters$isolationWindow,
      mzClust = parameters$mzClust,
      presence = parameters$presence,
      minIntensity = parameters$minIntensity,
      verbose = parameters$verbose,
      filtered = parameters$filtered,
      loadedMS2 = FALSE
    )
  }
  
  analyses <- self$get_analysis_names()
  
  features <- self$feature_list
  
  ms2_col <- lapply(analyses, function(x, ms2, features) {
    
    ana_ms2 <- ms2[ms2$analysis %in% x, ]
    
    fts_all <- features[[x]]$feature
    
    fts_ms2 <- lapply(fts_all, function(z, ana_ms2) {
      
      ft_ms2 <- ana_ms2[ana_ms2$feature %in% z, ]
      
      if (nrow(ft_ms2) > 0) {
        ft_ms2[["group"]] <- NULL
        ft_ms2[["analysis"]] <- NULL
        ft_ms2[["feature"]] <- NULL
        ft_ms2
        
      } else {
        NULL
      }
    }, ana_ms2 = ana_ms2)
    
    fts_ms2
    
  }, ms2 = ms2, features = features)
  
  if (private$.add_features_column("ms2", ms2_col)) {
    
    message("\U2713 MS2 spectra added to features!")
    
    TRUE
    
  } else {
    FALSE
  }
}
