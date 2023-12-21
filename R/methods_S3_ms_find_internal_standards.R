
#' @title .s3_ms_find_internal_standards.Settings_find_internal_standards_StreamFind
#'
#' @description Finds internal standards in the features/groups.
#'
#' @noRd
#'
.s3_ms_find_internal_standards.Settings_find_internal_standards_StreamFind <- function(settings, self) {
  
  if (!validate(settings)) return(FALSE)
  
  database <- settings$parameters$database
  
  database <- as.data.table(database)

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
    
    internal_standards$rec <- round(
      (internal_standards$intensity / intensity[internal_standards$istd_name]) * 100, 
      digits = 1
    )
    
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
    
    analyses <- self$get_analyses()

    analyses <- lapply(analyses, function(x, internal_standards_l) {
      
      istd <- internal_standards_l[[x$name]]
      
      if (!is.null(istd)) {

        istd_l <- lapply(x$features$feature, function(z, istd) {

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

        x$features$istd <- istd_l

      } else {
        x$features$istd <- lapply(x$features$feature, function(x) NULL)
      }
      
      
      # x$features[["istd_name"]] <- NA_character_
      # x$features[["istd_rte"]] <- NA_real_
      # x$features[["istd_mze"]] <- NA_real_
      # x$features[["istd_rec"]] <- NA_real_
      # 
      # 
      # if (!is.null(istd)) {
      #   for (i in seq_len(nrow(istd))) {
      #     ft_idx <- which(x$features$feature %in% istd$feature[i])
      #     x$features$istd_name[ft_idx] <- istd$name[i]
      #     x$features$istd_rte[ft_idx] <- istd$error_rt[i]
      #     x$features$istd_mze[ft_idx] <- istd$error_mass[i]
      #     x$features$istd_rec[ft_idx] <- istd$rec[i] 
      #   }
      # }
      
      x
      
    }, internal_standards_l = internal_standards_l)
    
    features <- lapply(analyses, function(x) x$features)
    
    suppressMessages(self$add_features(features, replace = TRUE))
    
    if ("istd" %in% colnames(self$get_features())) {
      
      # self$InternalStandards <- .module_methods_InternalStandards(self$.__enclos_env__)
      
      message("\U2713 ", length(unique(internal_standards$name)), " internal standards found and tagged!")
      
      TRUE
      
    } else {
      FALSE
    }
    
  } else {
    FALSE
  }
}

# .module_methods_InternalStandards <- function(env) {
# 
#   out <- list()
# 
#   out[["get"]] <- function() {
#     istd <- self$get_features(filtered = TRUE)
# 
#     if ("istd_name" %in% colnames(istd)) {
# 
#       istd <- istd[!is.na(istd$istd_name), ]
# 
#       if (nrow(istd) > 0) {
# 
#         setnames(istd,
#                  c("istd_name", "istd_rte", "istd_mze", "istd_rec"),
#                  c("name", "rte", "mze","rec")
#         )
# 
#         cols <- c(
#           "name",
#           "intensity",
#           "area",
#           "rte",
#           "mze",
#           "rec",
#           "analysis",
#           "feature",
#           "group"
#         )
# 
#         istd <- istd[, cols, with = FALSE]
# 
#         setorder(istd, "name")
# 
#         istd
# 
#       } else {
#         warning("Internal standards not found!")
#       }
# 
#     } else {
#       warning("Not present! Run find_internal_standards method to tag the internal standards!")
#     }
#   }
# 
#   environment(out[["get"]]) <- env
# 
#   
#   
#   out
# }
