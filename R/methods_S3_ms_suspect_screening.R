#' @title .s3_ms_suspect_screening.Settings_suspect_screening_StreamFind
#'
#' @description Makes suspect screening on features.
#'
#' @noRd
#'
.s3_ms_suspect_screening.Settings_suspect_screening_StreamFind <- function(settings, self) {

  if (!validate(settings)) return(FALSE)

  suspect_features <- self$get_suspects(
    database = settings$parameters$database,
    ppm = settings$parameters$ppm,
    sec = settings$parameters$sec,
    filtered = settings$parameters$filtered,
    onGroups = FALSE
  )
  
  suspect_cols <- colnames(suspect_features)
  suspect_cols <- c(suspect_cols[1:which(suspect_cols %in% "analysis") - 1])

  if (nrow(suspect_features) > 0) {
    
    suspect_features_l <- split(suspect_features, suspect_features$analysis)
    
    if (!any(self$has_features())) return(FALSE)
    
    analyses <- self$get_analyses()
    
    analyses <- lapply(analyses, function(x, suspect_features_l, suspect_cols) {
      
      suspects <- suspect_features_l[[x$name]]
      
      if (!is.null(suspects)) {
        
        suspects_l <- lapply(x$features$feature, function(z, suspects, suspect_cols) {
          
          sus_idx <- which(suspects$feature %in% z)
          
          if (length(sus_idx) > 0) {
            sus_temp <- suspects[sus_idx, ]
            sus_temp <- sus_temp[, suspect_cols, with = FALSE]
            
            if (nrow(sus_temp) > 0) {
              sus_temp  
            } else {
              NULL
            }
          } else {
            NULL
          }
          
        }, suspects = suspects, suspect_cols = suspect_cols)
        
        x$features$suspects <- suspects_l
        

      } else {
        x$features$suspects <- lapply(x$features$feature, function(x) NULL)
      }

      x
      
    }, suspect_features_l = suspect_features_l, suspect_cols = suspect_cols)
    
    features <- lapply(analyses, function(x) x$features)
    
    suppressMessages(self$add_features(features, replace = TRUE))
    
    
    # suspects_data <- list(
    #   "features" = suspect_features,
    #   "groups" = suspect_groups
    # )
    # 
    # output <- list(suspects_data)
    # names(output) <- settings$call
    # 
    # self$add_modules_data(output)

    TRUE
  } else {
    FALSE
  }
}

#' @title .s3_ms_suspect_screening.Settings_suspect_screening_forident
#'
#' @description Makes suspect screening on features.
#'
#' @noRd
#'
.s3_ms_suspect_screening.Settings_suspect_screening_forident <- function(settings, self) {

  if (!validate(settings)) return(FALSE)

  if (self$has_groups()) {

    polarities <- unique(self$get_polarities())

    if (length(polarities) > 1 & settings$parameters$addMS2) {
      warning("Using ms2 data of feature groups from multiple polarities is not possible! Using features of each analysis instead.")

      out_list <- self$get_features()
      out_list$rt <- out_list$rt / 60
      out_list$label <- paste0(out_list$analysis, "_" , out_list$feature)
    }

    out_list <- self$get_groups()
    out_list$rt <- out_list$rt / 60
    out_list$label <- out_list$group

  } else {
    out_list <- self$get_features()
    out_list$rt <- out_list$rt / 60
    out_list$label <- paste0(out_list$analysis, "_" , out_list$feature)
  }

  if (settings$parameters$useNeutralMass & "mass" %in% colnames(out_list)) {
    out_list$Mass <- out_list$mass

  } else if ("mz" %in% colnames(out_list)) {
    out_list$Mass <- out_list$mz

  } else {
    out_list$Mass <- out_list$mass
  }

  if (!settings$parameters$addMS2) {

    sink(paste0(settings$parameters$path,"/", settings$parameters$name, ".txt"))
    cat("\n")
    cat("\n")

    for (i in seq_len(nrow(out_list))) {
      cat("NAME: ")
      cat(out_list$label[i])
      cat("\n")
      cat("RETENTIONTIME: ")
      cat(round(out_list$rt[i], digits = 3))
      cat("\n")
      cat("Mass: ")
      cat(round(out_list$Mass[i], digits = 4))
      cat("\n")
      cat("Formula: ")
      cat("\n")
      cat("//")
      cat("\n")
      cat("\n")
    }
    sink()

    message("\U2713 List saved as ", paste0(settings$parameters$path,"/", settings$parameters$name, ".txt"))

  } else {

    if ("ms2" %in% colnames(out_list)) {

      if (!"mz" %in% colnames(out_list)) {
        warning("m/z values not found in the feature groups data.table but it is required for inclusion of fragments! .txt files not exported.")
        # return(FALSE)
      } else {
        out_list$Mass <- out_list$mz
      }

      sink(paste0(settings$parameters$path,"/", settings$parameters$name, ".txt"))
      cat("\n")
      cat("\n")

      for (i in seq_len(nrow(out_list))) {
        cat("NAME: ")
        cat(out_list$label[i])
        cat("\n")
        cat("RETENTIONTIME: ")
        cat(round(out_list$rt[i], digits = 3))
        cat("\n")
        cat("PRECURSORMZ: ")
        cat(round(out_list$Mass[i], digits = 4))
        cat("\n")
        cat("Formula: ")
        cat("\n")
        if (is.data.frame(out_list$ms2[[i]])) {
          temp_ms2 <- out_list$ms2[[i]]
          for (j in seq_len(nrow(temp_ms2))) {
            cat(
              paste(
                round(temp_ms2$mz[j], digits = 4),
                round(temp_ms2$intensity[j], digits = 0),
                sep = " "
              )
            )
            cat(" ")
          }
          rm(j, temp_ms2)
        } else {
          cat("N/A")
        }
        cat("\n")
        cat("//")
        cat("\n")
        cat("\n")
      }
      sink()

      message("\U2713 List saved as ", paste0(settings$parameters$path,"/", settings$parameters$name, ".txt"))

    } else {
      warning("MS2 data not found! Load features and/or feature groups MS2 data.")
      return(FALSE)
    }
  }

  TRUE
}

#' @title .s3_ms_suspect_screening.Settings_suspect_screening_patRoon
#'
#' @description Makes suspect screening on feature groups.
#'
#' @noRd
#'
.s3_ms_suspect_screening.Settings_suspect_screening_patRoon <- function(settings, self) {
  
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
  
  suspect_list <- res@screenInfo
  
  if (!any(self$has_features())) return(FALSE)
  
  analyses <- self$get_analyses()
  
  features <- lapply(analyses, function(x, suspect_list) {
    
    fts <- x$features
    
    has_suspect_features <- any(suspect_list$group %in% fts$group)
    
    if (has_suspect_features) {
      
      suspects_l <- lapply(seq_len(nrow(fts)), function(z, suspect_list) {
        
        ft <- fts[z, ]
        
        sus_idx <- which(suspect_list$group %in% ft$group)
        
        if (length(sus_idx) > 0) {
          sus_temp <- suspect_list[sus_idx, ]
          
          if ("rt" %in% colnames(sus_temp)) {
            if (!is.na(sus_temp$rt)) {
              sus_temp$d_rt <- sus_temp$rt - ft$rt
              sus_temp$d_rt <- round(sus_temp$d_rt, digits = 1)
            }
            setnames(sus_temp, "rt", "exp_rt")
            setnames(sus_temp, "d_rt", "error_rt")
          }
          
          if ("neutralMass" %in% colnames(sus_temp)) {
            sus_temp$d_mz <- (sus_temp$neutralMass - ft$mass) / ft$mass * 1E6
            sus_temp$d_mz <- round(sus_temp$d_mz, digits = 1)
            setnames(sus_temp, "neutralMass", "exp_mass")
            setnames(sus_temp, "d_mz", "error_mass")
          }
           # TODO make case for mz column
          
          sus_temp[["group"]] <- NULL
          sus_temp[["sets"]] <- NULL
          sus_temp[["molNeutralized-negative"]] <- NULL
          sus_temp[["molNeutralized-positive"]] <- NULL
          sus_temp[["molNeutralized"]] <- NULL

          if (nrow(sus_temp) > 0) {
            sus_temp  
          } else {
            NULL
          }
        } else {
          NULL
        }
        
      }, suspect_list = suspect_list)
      
      fts$suspects <- suspects_l

    } else {
      fts$suspects <- lapply(fts$feature, function(x) NULL)
    }
    
    fts
    
  }, suspect_list = suspect_list)
  
  suppressMessages(self$add_features(features, replace = TRUE))
  
  TRUE
}
