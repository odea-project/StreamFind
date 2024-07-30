
#' @title .s3_LoadFeaturesEIC.MassSpecSettings_LoadFeaturesEIC_StreamFind
#'
#' @description Loads the extracted ion chromatogram for each feature.
#'
#' @noRd
#'
.s3_LoadFeaturesEIC.MassSpecSettings_LoadFeaturesEIC_StreamFind <- function(settings, self, private) {
  
  if (!any(self$has_features())) {
    warning("Features not found! Not loaded.")
    return(FALSE)
  }
  
  parameters <- settings$parameters
  
  feat_eics <- self$get_features_eic(
    rtExpand = parameters$rtExpand,
    mzExpand = parameters$mzExpand,
    filtered = parameters$filtered,
    loaded = FALSE
  )
  
  feat_eics_list <- split(feat_eics, feat_eics$analysis)
  
  settings$parameters$filtered
  
  features <- self$get_feature_list(filtered = TRUE)
  
  eic_col <- lapply(names(features), function(x, features, feat_eics_list) {
    
    eics <- feat_eics_list[[x]]
    
    fts <- features[[x]]
    
    if (!is.null(eics)) {
      
      eics[["analysis"]] <- NULL
      
      eics[["group"]] <- NULL
      
      eics[["name"]] <- NULL
      
      # eics_l <- split(eics, eics$feature)
      # 
      # eics_l <- lapply(eics_l, function(z) {
      #   z[["feature"]] <- NULL
      #   z
      # })
      
      eics_l <- lapply(fts$feature, function(x) {
        temp <- eics[eics$feature %in% x, ]
        
        if (nrow(temp) > 0) {
          temp$feature <- NULL
          temp
          
        } else {
          NULL
        }
        
      })
      
      eics_l
      
    } else {
      lapply(fts$feature, function(x) NULL)
    }
    
  }, features = features, feat_eics_list = feat_eics_list)
  
  names(eic_col) <- names(features)
  
  if (private$.add_features_column("eic", eic_col)) {
    
    message("\U2713 Feature EICs loaded!")
    
    TRUE
    
  } else {
    FALSE
  }
}
