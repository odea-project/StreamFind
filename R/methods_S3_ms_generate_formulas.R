#' @title .s3_ms_suspect_screening.Settings_generate_formulas_patRoon
#'
#' @description Generate formulas for feature groups.
#'
#' @noRd
#'
.s3_ms_suspect_screening.Settings_generate_formulas_patRoon <- function(settings, self) {
  
  if (!validate(settings)) return(FALSE)
  
  parameters <- settings$parameters
  
  fg <- ms$as_patRoon_featureGroups(filtered = parameters$filtered)
  
  if (is.null(fg)) return(FALSE)
  
  res <- patRoon::screenSuspects(
    fGroups = fg,
    suspects = parameters$suspects,
    rtWindow = parameters$rtWindow,
    mzWindow = parameters$mzWindow,
    skipInvalid = TRUE,
    prefCalcChemProps = TRUE,
    neutralChemProps = TRUE,
    onlyHits = TRUE,
    adduct = NULL
  )
  
  # suspect_list <- res@screenInfo
  # 
  # if (!any(self$has_features())) return(FALSE)
  # 
  # analyses <- self$get_analyses()
  # 
  # features <- lapply(analyses, function(x, suspect_list) {
  #   
  #   fts <- x$features
  #   
  #   has_suspect_features <- any(suspect_list$group %in% fts$group)
  #   
  #   if (has_suspect_features) {
  #     
  #     suspects_l <- lapply(seq_len(nrow(fts)), function(z, suspect_list) {
  #       
  #       ft <- fts[z, ]
  #       
  #       sus_idx <- which(suspect_list$group %in% ft$group)
  #       
  #       if (length(sus_idx) > 0) {
  #         sus_temp <- suspect_list[sus_idx, ]
  #         
  #         if ("rt" %in% colnames(sus_temp)) {
  #           if (!is.na(sus_temp$rt)) {
  #             sus_temp$d_rt <- sus_temp$rt - ft$rt
  #             sus_temp$d_rt <- round(sus_temp$d_rt, digits = 1)
  #           }
  #           setnames(sus_temp, "rt", "exp_rt")
  #           setnames(sus_temp, "d_rt", "error_rt")
  #         }
  #         
  #         if ("neutralMass" %in% colnames(sus_temp)) {
  #           sus_temp$d_mz <- (sus_temp$neutralMass - ft$mass) / ft$mass * 1E6
  #           sus_temp$d_mz <- round(sus_temp$d_mz, digits = 1)
  #           setnames(sus_temp, "neutralMass", "exp_mass")
  #           setnames(sus_temp, "d_mz", "error_mass")
  #         }
  #          # TODO make case for mz column
  #         
  #         sus_temp[["group"]] <- NULL
  #         sus_temp[["sets"]] <- NULL
  #         sus_temp[["molNeutralized-negative"]] <- NULL
  #         sus_temp[["molNeutralized-positive"]] <- NULL
  #         sus_temp[["molNeutralized"]] <- NULL
  # 
  #         if (nrow(sus_temp) > 0) {
  #           sus_temp  
  #         } else {
  #           NULL
  #         }
  #       } else {
  #         NULL
  #       }
  #       
  #     }, suspect_list = suspect_list)
  #     
  #     fts$suspects <- suspects_l
  # 
  #   } else {
  #     fts$suspects <- lapply(fts$feature, function(x) NULL)
  #   }
  #   
  #   fts
  #   
  # }, suspect_list = suspect_list)
  
  # suppressMessages(self$add_features(features, replace = TRUE))
  
  TRUE
}
