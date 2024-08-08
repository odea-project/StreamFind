
#' @noRd
.process.MassSpecSettings_FindInternalStandards_StreamFind <- function(settings, self, private) {
  
  if (!any(self$has_features())) {
    warning("There are no features! Run find_features first!")
    return(FALSE)
  }
  
  if (!validate(settings)) return(FALSE)
  
  cache <- .load_chache("find_internal_standards", self$featureGroups, settings)
  
  if (!is.null(cache$data)) {
    if (private$.add_features_column("istd", cache$data)) {
      message("\U2139 Internal standards annotation loaded from cache!")
      return(TRUE)
    }
  }
  
  database <- settings$parameters$database
  
  database <- data.table::as.data.table(database)
  
  internal_standards <- self$get_suspects(
    database = database,
    ppm = settings$parameters$ppm,
    sec = settings$parameters$sec,
    filtered = TRUE,
    onGroups = FALSE
  )
  
  if (nrow(internal_standards) == 0) {
    warning("Internal standards were not found!")
    return(FALSE)
  }
  
  if ("intensity" %in% colnames(database)) {
    intensity <- database$intensity
    names(intensity) <- database$name
    
    internal_standards$rec <- round((internal_standards$intensity / intensity[internal_standards$istd_name]) * 100, digits = 1)
    
  } else if ("area" %in% colnames(database)) {
    area <- database$area
    names(area) <- database$name
    
    internal_standards$rec <- round(
      (internal_standards$area / area[internal_standards$istd_name]) * 100, 
      digits = 1
    )
    
  } else {
    
    blks <- self$get_blank_names()
    
    if (any(!is.na(blks)) & self$has_groups()) {
      
      rpls <- self$get_replicate_names()
      
      internal_standards$replicate <- rpls[internal_standards$analysis]
      
      internal_standards$rec <- vapply(seq_len(nrow(internal_standards)),
        function(x, internal_standards, blks) {
        
          feat <- internal_standards[x, ]
          
          if (feat$replicate %in% blks) return(1)
          
          feat_area <- feat$area
          
          blk <- blks[feat$analysis]
          
          blk_feats <- internal_standards[
            internal_standards$replicate %in% blk &
              internal_standards$group %in% feat$group, ]
          
          if (nrow(blk_feats) > 0) {
            blk_area <- mean(blk_feats$area, na.rm = TRUE)
            return(round(feat_area / blk_area, digits = 2))
            
          } else {
            NA_real_
          }
        
        }, 
        internal_standards = internal_standards,
        blks = blks,
        NA_real_
      )
      
    } else {
      internal_standards$rec <- NA_real_  
    }
  }
  
  if (nrow(internal_standards) > 0) {
    internal_standards_l <- split(internal_standards, internal_standards$analysis)
    
    if (!any(self$has_features())) return(FALSE)
    
    features <- self$get_feature_list(filtered = TRUE)

    istd_col <- lapply(names(features), function(x, features, internal_standards_l) {
      
      istd <- internal_standards_l[[x]]
      
      fts <- features[[x]]
      
      if (!is.null(istd)) {

        istd_l <- lapply(fts$feature, function(z, istd) {

          istd_idx <- which(istd$feature %in% z)

          if (length(istd_idx) > 0) {
            istd_temp <- istd[istd_idx, ]
            istd_temp <- istd_temp[, c("name", "formula", "error_mass", "error_rt", "rec"), with = FALSE]

            if (nrow(istd_temp) > 0) {
              istd_temp
            } else {
              NULL
            }
          } else {
            NULL
          }

        }, istd = istd)

        istd_l

      } else {
        lapply(fts$feature, function(x) NULL)
      }
      
    }, features = features, internal_standards_l = internal_standards_l)
    
    names(istd_col) <- names(features)
    
    if (private$.add_features_column("istd", istd_col)) {
      
      if (!is.null(cache$hash)) {
        .save_cache("find_internal_standards", istd_col, cache$hash)
        message("\U1f5ab Internal standards annotation cached!")
      }
      
      message("\U2713 ", length(unique(internal_standards$name)), " internal standards found and tagged!")
      TRUE
      
    } else {
      FALSE
    }
    
  } else {
    FALSE
  }
}
