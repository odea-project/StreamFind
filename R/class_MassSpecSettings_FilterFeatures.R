
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FilterFeatures_StreamFind**
#'
#' @description Settings for filtering of features and feature groups.
#'
#' @param minSnRatio Numeric (length 1) with the minimum signal-to-noise ratio.
#' @param excludeIsotopes Logical (length 1) with `TRUE` for filtering annotated isotopes 
#' (only prevails the monoisotopic features).
#' @param excludeAdducts Logical (length 1) with `TRUE` for filtering annotated adducts.
#' @param minIntensity Numeric (length 1) with the minimum intensity threshold.
#' @param onlyWithMS2 Logical (length 1) with `TRUE` for filtering features without MS2 spectra.
#'
#' @return A `MassSpecSettings_FilterFeatures_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_FilterFeatures_StreamFind <- S7::new_class("MassSpecSettings_FilterFeatures_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(minSnRatio = NA_real_,
                         excludeIsotopes = FALSE,
                         excludeAdducts = FALSE,
                         minIntensity = NA_real_,
                         onlyWithMS2 = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FilterFeatures",
      algorithm = "StreamFind",
      parameters = list(
        minSnRatio = as.numeric(minSnRatio),
        excludeIsotopes = as.logical(excludeIsotopes),
        excludeAdducts = as.logical(excludeAdducts),
        minIntensity = as.numeric(minIntensity),
        onlyWithMS2 = as.logical(onlyWithMS2)
      ),
      number_permitted = Inf,
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
    checkmate::assert_choice(self@method, "FilterFeatures")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_numeric(self@parameters$minSnRatio, len = 1)
    checkmate::assert_logical(self@parameters$excludeIsotopes, len = 1)
    checkmate::assert_logical(self@parameters$excludeAdducts, len = 1)
    checkmate::assert_numeric(self@parameters$minIntensity, len = 1)
    checkmate::assert_logical(self@parameters$onlyWithMS2, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FilterFeatures_StreamFind) <- function(x, engine = NULL) {
  
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
  
  if (!engine$nts$has_features) {
    warning("There are no features! Run find_features first!")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  filters <- names(parameters)
  
  n_features <- engine$nts$number_features
  
  .filter_excludeIsotopes = function(value = NULL, engine) {
    
    if (engine$nts$has_features && is.logical(value) && length(value) == 1) {
      
      if (!value) return()
      
      features <- engine$nts$feature_list
      
      features <- lapply(features, function(x) {
        if ("annotation" %in% colnames(x)) {
          iso <- vapply(x$annotation, function(z) {
            if (length(z) == 0) {
              NA_integer_
            } else {
              z[["iso_step"]]
            }
          }, NA_integer_)
          iso[is.na(iso)] <- 0
          sel <- iso > 0
          x$filtered[sel] <- TRUE
        }
        x
      })
      
      engine$nts$feature_list <- features
      
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_excludeAdducts = function(value = NULL, engine) {
    
    if (engine$nts$has_features && is.logical(value) && length(value) == 1) {
      
      if (!value) return()
      
      features <- engine$nts$feature_list
      
      features <- lapply(features, function(x) {
        if ("annotation" %in% colnames(x)) {
          res <- vapply(x$annotation, function(z) {
            if (length(z) == 0) {
              NA_character_
            } else {
              z[["adduct_cat"]]
            }
          }, NA_character_)
          res[is.na(res)] <- ""
          sel <- res != ""
          x$filtered[sel] <- TRUE
        }
        x
      })
      
      engine$nts$feature_list <- features
      
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_minSnRatio <- function(value = 3, engine) {
    
    if (engine$nts$has_features && is.numeric(value) && length(value) == 1) {
      
      features <- engine$nts$feature_list
      
      features <- lapply(features, function(x) { 
        if ("quality" %in% colnames(x)) {
          qlt <- vapply(x$quality, function(z) {
            if (length(z) == 0) {
              NA_real_
            } else {
              z[["sn"]]
            }
          }, NA_real_)
          qlt[is.na(qlt)] <- 0
          sel <- qlt <= value
          x$filtered[sel] <- TRUE
        }
        
        x
      })
      
      engine$nts$feature_list <- features
      
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_minIntensity <- function(value = 1000, engine) {
    
    if (engine$nts$has_features && is.numeric(value) && length(value) == 1) {
      
      if (engine$nts@has_groups) {
        rpl <- unique(engine$analyses$replicates)
        groups <- engine$get_groups(filtered = FALSE, intensities = TRUE, average = TRUE, metadata = FALSE)
        groups_sel <- apply(groups[, rpl, with = FALSE], MARGIN = 1,function(x) max(x) <= value)
        groups <- groups$group[groups_sel]
        
        feature_list <- engine$nts$feature_list
        
        feature_list <- lapply(feature_list, function(x, groups) {
          sel <- x$group %in% groups
          x$filtered[sel] <- TRUE
          x
        }, groups = groups)
        
        engine$nts$feature_list <- feature_list
        
      } else {
        
        features <- engine$nts$feature_list
        
        feature_list <- lapply(feature_list, function(x) {
          sel <- x$intensity <= value
          x$filtered[sel] <- TRUE
          x
        })
        
        engine$nts$feature_list <- features
      }
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_onlyWithMS2 <- function(value = NULL, engine) {
    
    if (engine$nts$has_features && is.logical(value) && length(value) == 1) {
      
      if (!value) return()
      
      features <- engine$nts$feature_list
      
      features <- lapply(features, function(x) { 
        if ("ms2" %in% colnames(x)) {
          sel <- vapply(x$ms2, function(z) {
            if (length(z) == 0) {
              TRUE
            } else if (nrow(z) == 0) {
              TRUE
            } else {
              FALSE
            }
          }, FALSE)
          x$filtered[sel] <- TRUE
        }
        x
      })
      
      engine$nts$feature_list <- features
      
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  
  # 
  # # Filters features with max replicate group intensity deviation.
  # #
  # .filter_maxGroupSd = function(value = 30) {
  # 
  #   if (engine$has_groups()) {
  # 
  #     if (is.numeric(value) & length(value) == 1) {
  # 
  #       message("\U2699 Filtering by maxGroupSd...", appendLF = FALSE)
  # 
  #       rpl <- engine$get_replicate_names()
  #       rpl <- paste(rpl, "_sd", sep = '')
  #       names(rpl) <- engine$get_analysis_names()
  # 
  #       groups <- engine$get_groups(
  #         filtered = FALSE, intensities = TRUE,
  #         average = TRUE, sdValues = TRUE,
  #         metadata = FALSE
  #       )
  # 
  #       rpl <- rpl[rpl %in% colnames(groups)]
  # 
  #       lapply(1:nrow(groups), function(x, value, rpl, groups) {
  # 
  #         g <- groups$group[x]
  #         vec <- as.numeric(groups[x, unique(rpl), with = FALSE])
  #         sel <- vec >= value & !is.na(vec)
  # 
  #         for (i in 1:length(vec)) {
  # 
  #           if (sel[i]) {
  #             rp <- unique(rpl)[i]
  #             anas <- names(rpl)[rpl %in% rp]
  # 
  #             for (a in anas) {
  #               sel2 <- private$.analyses[[a]]$features$group %in% g
  # 
  #               if (TRUE %in% sel2) {
  #                 if (!private$.analyses[[a]]$features$filtered[sel2]) {
  #                   private$.analyses[[a]]$features$filtered[sel2] <- TRUE
  #                   private$.analyses[[a]]$features$filter[sel2] <- "maxGroupSd"
  #                 }
  #               }
  #             }
  #           }
  #         }
  #       }, value = value, rpl = rpl, groups = groups)
  # 
  #       # groups_sel <- apply(groups[, rpl, with = FALSE], MARGIN = 1,
  #       #   function(x, value) {
  #       #     all(x >= value | x == 0, na.rm = TRUE)
  #       #   }, value = value
  #       # )
  #       #
  #       # groups <- groups$group[groups_sel]
  #       #
  #       # private$.tag_filtered(groups, "maxGroupSd")
  # 
  #       private$.register(
  #         "filter_features",
  #         "features",
  #         "maxGroupSd",
  #         "StreamFind",
  #         as.character(packageVersion("StreamFind")),
  #         paste0(value, "%")
  #       )
  # 
  #       message(" Done!")
  # 
  #     } else {
  #       warning("The value for maxGroupSd filtering must be numeric and of length one!")
  #     }
  #   } else {
  #     warning("There are no feature groups in the MassSpecEngine!")
  #   }
  # },
  # 
  # # Filters feature with min replicate group abundance.
  # #
  # .filter_minGroupAbundance = function(value = 3) {
  # 
  #   if (engine$has_groups()) {
  # 
  #     if (is.numeric(value) & length(value) == 1) {
  # 
  #       message("\U2699 Filtering by minGroupAbundance...", appendLF = FALSE)
  # 
  #       rpl <- engine$get_replicate_names()
  # 
  #       groups <- engine$get_groups(filtered = FALSE, intensities = TRUE, average = FALSE, metadata = FALSE)
  # 
  #       features <- engine$get_features(filtered = FALSE)
  # 
  #       lapply(1:nrow(groups), function(x, value, rpl, groups) {
  # 
  #         g <- groups$group[x]
  #         which_fts <- which(features$group %in% g)
  #         analyses <- features$analysis[which_fts]
  # 
  #         r <- rpl[analyses]
  #         r <- table(r)
  # 
  #         for (i in 1:length(r)) {
  # 
  #           if (r[i] < value) {
  #             rp <- names(r[i])
  #             anas <- names(rpl)[rpl %in% rp]
  # 
  #             for (a in anas) {
  # 
  #               if (a %in% analyses) {
  #                 sel2 <- private$.analyses[[a]]$features$group %in% g
  #                 private$.analyses[[a]]$features$filtered[sel2] <- TRUE
  #                 private$.analyses[[a]]$features$filter[sel2] <- "minGroupAbundance"
  #               }
  #             }
  #           }
  #         }
  #       }, value = value, rpl = rpl, groups = groups)
  # 
  #       # # TODO add abundance to group metadata output
  #       #
  #       # groups_sel <- vapply(groups$group,
  #       #   function(x, features, rpl, value) {
  #       #     which_fts <- which(features$group %in% x)
  #       #     analyses <- features$analysis[which_fts]
  #       #     r <- rpl[analyses]
  #       #     r <- table(r)
  #       #     !any(apply(r, 1, function(x) max(x) >= value))
  #       #   },
  #       #   features = features,
  #       #   rpl = engine$get_replicate_names(),
  #       #   value = value,
  #       #   FALSE
  #       # )
  #       #
  #       # groups <- groups$group[groups_sel]
  #       #
  #       # private$.tag_filtered(groups, "minGroupAbundance")
  # 
  #       private$.register(
  #         "filter_features",
  #         "features",
  #         "minGroupAbundance",
  #         "StreamFind",
  #         as.character(packageVersion("StreamFind")),
  #         value
  #       )
  # 
  #       message(" Done!")
  # 
  #     } else {
  #       warning("The value for minGroupAbundance filtering must be numeric and of length one!")
  #     }
  #   } else {
  #     warning("There are no feature groups in the MassSpecEngine!")
  #   }
  # },
  # 
  # # Filters feature groups which not higher then the defined threshold of the
  # # corresponding blank replicate group.
  # #
  # .filter_blank = function(value = 30) {
  # 
  #   if (engine$has_groups()) {
  # 
  #     if (is.numeric(value) & length(value) == 1) {
  # 
  #       blk <- engine$get_blank_names()
  # 
  #       rpl <- engine$get_replicate_names()
  # 
  #       names(blk) <- rpl
  # 
  #       blk <- blk[!rpl %in% unique(blk)]
  # 
  #       blk <- blk[!duplicated(names(blk))]
  # 
  #       rpl <- rpl[!rpl %in% blk]
  # 
  #       if (length(blk) != 0) {
  # 
  #         message("\U2699 Subtracting blank...", appendLF = FALSE)
  # 
  #         groups <- engine$get_groups(filtered = TRUE, intensities = TRUE, average = TRUE, sdValues = FALSE, metadata = FALSE)
  # 
  #         for (r in seq_len(length(blk))) {
  #           rp <- names(blk)[r]
  #           bl <- blk[r]
  #           groups[, (rp) := groups[[rp]] <= (groups[[bl]] * value)][]
  #         }
  # 
  #         lapply(1:nrow(groups), function(x, value, rpl, groups) {
  # 
  #           g <- groups$group[x]
  #           sel <- as.logical(groups[x, unique(rpl), with = FALSE])
  # 
  #           for (i in 1:length(sel)) {
  # 
  #             if (sel[i]) {
  #               rp <- unique(rpl)[i]
  #               anas <- names(rpl)[rpl %in% rp]
  # 
  #               for (a in anas) {
  #                 sel2 <- private$.analyses[[a]]$features$group %in% g
  # 
  #                 if (TRUE %in% sel2) {
  #                   if (!private$.analyses[[a]]$features$filtered[sel2]) {
  #                     private$.analyses[[a]]$features$filtered[sel2] <- TRUE
  #                     private$.analyses[[a]]$features$filter[sel2] <- "blank"
  #                   }
  #                 }
  #               }
  #             }
  #           }
  #         }, value = value, rpl = rpl, groups = groups)
  # 
  #         blk_anas <- unique(names(engine$get_replicate_names()[engine$get_replicate_names() %in% blk]))
  # 
  #         for (b in blk_anas) {
  #           private$.analyses[[b]]$features$filtered <- TRUE
  #           private$.analyses[[b]]$features$filter <- "blank"
  #         }
  # 
  #         # groups_sel <- apply(groups[, names(blk), with = FALSE], MARGIN = 1,
  #         #   function(x) { all(x, na.rm = TRUE) }
  #         # )
  #         #
  #         # groups <- groups$group[groups_sel]
  #         #
  #         # private$.tag_filtered(groups, "blank")
  # 
  #         private$.register(
  #           "filter_features",
  #           "features",
  #           "blank",
  #           "StreamFind",
  #           as.character(packageVersion("StreamFind")),
  #           paste0("multiplier ", value)
  #         )
  # 
  #         message(" Done!")
  # 
  # 
  #       } else {
  #         warning("There are no blank analysis replicates!")
  #       }
  #     } else {
  #       warning("The value for blank filtering must be numeric and of length one!")
  #     }
  #   } else {
  #     warning("There are no feature groups in the MassSpecEngine!")
  #   }
  # },
  # 
  # # Filters features and feature groups within a retention time range.
  # #
  # .filter_rtFilter = function(value = c(0, 0)) {
  # 
  #   if (any(engine$has_features())) {
  # 
  #     if (is.numeric(value) & length(value) == 2) {
  # 
  #       message("\U2699 Filtering by rtFilter...", appendLF = FALSE)
  # 
  #       value <- sort(value)
  # 
  #       # if (engine$has_groups()) {
  #       #   rpl <- engine$get_replicate_names()
  #       #
  #       #   groups <- engine$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = TRUE)
  #       #
  #       #   groups_sel <- (groups$rt <= value[2]) & (groups$rt >= value[1])
  #       #
  #       #   groups <- groups$group[groups_sel]
  #       #
  #       #   private$.tag_filtered(groups, "rtFilter")
  #       #
  #       # } else {
  #         private$.analyses <- lapply(private$.analyses, function(x) {
  #           sel <- (x$features$rt <= value[2]) & (x$features$rt >= value[1]) & (!x$features$filtered)
  #           x$features$filtered[sel] <- TRUE
  #           x$features$filter[sel] <- "rtFilter"
  #           x
  #         })
  #       # }
  # 
  #       private$.register(
  #         "filter_features",
  #         "features",
  #         "rtFilter",
  #         "StreamFind",
  #         as.character(packageVersion("StreamFind")),
  #         paste(value, collapse = "; ")
  #       )
  # 
  #       message(" Done!")
  # 
  #     } else {
  #       warning("The value for rt filtering must be numeric and of length two!")
  #     }
  #   } else {
  #     warning("There are no features in the MassSpecEngine!")
  #   }
  # },
  # 
  # # Filters features and feature groups within a mass range.
  # #
  # .filter_massFilter = function(value = c(0, 0)) {
  # 
  #   if (any(engine$has_features())) {
  # 
  #     if (is.numeric(value) & length(value) == 2) {
  # 
  #       message("\U2699 Filtering by massFilter...", appendLF = FALSE)
  # 
  #       value <- sort(value)
  # 
  #       # if (engine$has_groups()) {
  #       #   rpl <- engine$get_replicate_names()
  #       #
  #       #   groups <- engine$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = TRUE)
  #       #
  #       #   groups_sel <- (groups$mass <= value[2]) & (groups$mass >= value[1])
  #       #
  #       #   groups <- groups$group[groups_sel]
  #       #
  #       #   private$.tag_filtered(groups, "massFilter")
  #       #
  #       # } else {
  #         private$.analyses <- lapply(private$.analyses, function(x) {
  #           sel <- (x$features$mass <= value[2]) &
  #             (x$features$mass >= value[1]) &
  #             (!x$features$filtered)
  #           x$features$filtered[sel] <- TRUE
  #           x$features$filter[sel] <- "massFilter"
  #           x
  #         })
  #       # }
  # 
  #       private$.register(
  #         "filter_features",
  #         "features",
  #         "massFilter",
  #         "StreamFind",
  #         as.character(packageVersion("StreamFind")),
  #         paste(value, collapse = "; ")
  #       )
  # 
  #       message(" Done!")
  # 
  #     } else {
  #       warning("The value for neutral mass filtering must be numeric and of length two!")
  #     }
  #   } else {
  #     warning("There are no features in the MassSpecEngine!")
  #   }
  # },
  # 
  # # Filters features and feature groups within a mass range.
  # #
  # .filter_onlySuspects = function(value = NULL) {
  # 
  #   if (any(engine$has_suspects())) {
  # 
  #     if (is.logical(value) & length(value) == 1) {
  # 
  #       message("\U2699 Filtering by onlySuspects...", appendLF = FALSE)
  # 
  #       if (engine$has_groups()) {
  # 
  #         sus <- engine$get_suspects(onGroups = TRUE)
  # 
  #         groups <- engine$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE)
  # 
  #         groups_sel <- !groups$group %in% sus$group
  # 
  #         groups <- groups$group[groups_sel]
  # 
  #         private$.tag_filtered(groups, "onlySuspects")
  # 
  #       } else {
  #         private$.analyses <- lapply(private$.analyses, function(x) {
  #           sel <- vapply(x$features$suspects, function(x) is.null(x), FALSE) & !x$features$filtered
  #           x$features$filtered[sel] <- TRUE
  #           x$features$filter[sel] <- "onlySuspects"
  #           x
  #         })
  #       }
  # 
  #       private$.register(
  #         "filter_features",
  #         "features",
  #         "onlySuspects",
  #         "StreamFind",
  #         as.character(packageVersion("StreamFind")),
  #         paste(value, collapse = "; ")
  #       )
  # 
  #       message(" Done!")
  # 
  #     } else {
  #       warning("The value for onlySuspects must be logical and of length one!")
  #     }
  #   } else {
  #     warning("There are no suspects in the MassSpecEngine!")
  #   }
  # }
  
  # MARK: Switch Loop
  # __Switch Loop ----
  
  for (i in seq_len(length(filters))) {
    
    if (is.na(parameters[[filters[i]]]) || length(parameters[[filters[i]]]) == 0) next
    
    switch(filters[i],
           minIntensity = .filter_minIntensity(parameters[[filters[i]]], engine),
           
           minSnRatio = .filter_minSnRatio(parameters[[filters[i]]], engine),
           
           # maxGroupSd = (private$.filter_maxGroupSd(parameters[[filters[i]]])),
           
           # blank = (private$.filter_blank(parameters[[filters[i]]])),
           
           # minGroupAbundance = (private$.filter_minGroupAbundance(parameters[[filters[i]]])),
           
           excludeIsotopes = .filter_excludeIsotopes(parameters[[filters[i]]], engine),
           
           excludeAdducts = .filter_excludeAdducts(parameters[[filters[i]]], engine),
           
           onlyWithMS2 = .filter_onlyWithMS2(parameters[[filters[i]]], engine)
           
           # rtFilter = private$.filter_rtFilter(parameters[[filters[i]]]),
           
           # massFilter = private$.filter_massFilter(parameters[[filters[i]]]),
           
           # onlySuspects = .filter_onlySuspects(parameters[[filters[i]]], engine)
           
           # TODO add more filters
    )
  }
  
  n_features_after <- engine$nts$number_features
  
  n_features_filtered <- n_features - n_features_after
  
  if (n_features_filtered < 0) n_features_filtered <- 0
  
  message(paste0("\U2713 ", n_features_filtered, " features filtered!"))
  
  TRUE
}

# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FilterFeatures_patRoon**
#'
#' @description Settings for filtering of features and feature groups. A full description of the filtering parameters is
#'  in \code{\link[patRoon]{replicateGroupSubtract}} from patRoon package.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_FilterFeatures_patRoon.
#' 
#' @param absMinIntensity Numeric length one. Minimum absolute intensity for a feature.
#' @param relMinIntensity Numeric length one. Minimum relative intensity for a feature.
#' @param preAbsMinIntensity Numeric length one. Minimum absolute intensity for a feature before grouping.
#' @param preRelMinIntensity Numeric length one. Minimum relative intensity for a feature before grouping.
#' @param absMinAnalyses Numeric length one. Minimum number of analyses a feature must be present in.
#' @param relMinAnalyses Numeric length one. Minimum relative number of analyses a feature must be present in.
#' @param absMinReplicates Numeric length one. Minimum number of replicates a feature must be present in.
#' @param relMinReplicates Numeric length one. Minimum relative number of replicates a feature must be present in.
#' @param absMinFeatures Numeric length one. Minimum number of features a feature group must contain.
#' @param relMinFeatures Numeric length one. Minimum relative number of features a feature group must contain.
#' @param absMinReplicateAbundance Numeric length one. Minimum absolute abundance of a replicate.
#' @param relMinReplicateAbundance Numeric length one. Minimum relative abundance of a replicate.
#' @param absMinConc Numeric length one. Minimum absolute concentration of a feature.
#' @param relMinConc Numeric length one. Minimum relative concentration of a feature.
#' @param absMaxTox Numeric length one. Maximum absolute toxicity of a feature.
#' @param relMaxTox Numeric length one. Maximum relative toxicity of a feature.
#' @param absMinConcTox Numeric length one. Minimum absolute concentration of a feature to be considered toxic.
#' @param relMinConcTox Numeric length one. Minimum relative concentration of a feature to be considered toxic.
#' @param maxReplicateIntRSD Numeric length one. Maximum relative standard deviation of intensities within a replicate.
#' @param blankThreshold Numeric length one. Maximum intensity of a feature to be considered a blank.
#' @param retentionRange Numeric length two. Retention time range (in seconds) for a feature.
#' @param mzRange Numeric length two. m/z range (in Da) for a feature.
#' @param mzDefectRange Numeric length two. m/z defect range (in Da) for a feature.
#' @param chromWidthRange Numeric length two. Chromatographic width range (in seconds) for a feature.
#' @param featQualityRange Numeric length two. Feature quality range for a feature.
#' @param groupQualityRange Numeric length two. Group quality range for a feature group.
#' @param rGroups Character with the replicate group names to filter.
#' @param removeBlanks Logical length one. Remove blank samples.
#' @param removeISTDs Logical length one. Remove internal standards.
#' @param removeNA Logical length one. Remove NA values.
#' @param negate Logical length one. Negate the filter.
#'
#' @details Note that when filters are applied to features or feature groups 
#' these require specific results from processing modules. For instance, 
#' subtracting the blank can only be done after grouping features. Also, some 
#' filters require. Thus, not all filters can be applied to features. 
#' See \code{\link[patRoon]{features-class}} and \code{\link[patRoon]{replicateGroupSubtract}} 
#' for further information.
#' 
#' @export
#'
MassSpecSettings_FilterFeatures_patRoon <- S7::new_class("MassSpecSettings_FilterFeatures_patRoon",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(absMinIntensity = NULL,
                         relMinIntensity = NULL,
                         preAbsMinIntensity = NULL,
                         preRelMinIntensity = NULL,
                         absMinAnalyses = NULL,
                         relMinAnalyses = NULL,
                         absMinReplicates = NULL,
                         relMinReplicates = NULL,
                         absMinFeatures = NULL,
                         relMinFeatures = NULL,
                         absMinReplicateAbundance = NULL,
                         relMinReplicateAbundance = NULL,
                         absMinConc = NULL,
                         relMinConc = NULL,
                         absMaxTox = NULL,
                         relMaxTox = NULL,
                         absMinConcTox = NULL,
                         relMinConcTox = NULL,
                         maxReplicateIntRSD = NULL,
                         blankThreshold = NULL,
                         retentionRange = NULL,
                         mzRange = NULL,
                         mzDefectRange = NULL,
                         chromWidthRange = NULL,
                         featQualityRange = NULL,
                         groupQualityRange = NULL,
                         rGroups = NULL,
                         removeBlanks = FALSE,
                         removeISTDs = FALSE,
                         removeNA = FALSE,
                         negate = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FilterFeatures",
      algorithm = "patRoon",
      parameters = list(
        "absMinIntensity" = as.numeric(absMinIntensity),
        "relMinIntensity" = as.numeric(relMinIntensity),
        "preAbsMinIntensity" = as.numeric(preAbsMinIntensity),
        "preRelMinIntensity" = as.numeric(preRelMinIntensity),
        "absMinAnalyses" = as.numeric(absMinAnalyses),
        "relMinAnalyses" = as.numeric(relMinAnalyses),
        "absMinReplicates" = as.numeric(absMinReplicates),
        "relMinReplicates" = as.numeric(relMinReplicates),
        "absMinFeatures" = as.numeric(absMinFeatures),
        "relMinFeatures" = as.numeric(relMinFeatures),
        "absMinReplicateAbundance" = as.numeric(absMinReplicateAbundance),
        "relMinReplicateAbundance" = as.numeric(relMinReplicateAbundance),
        "absMinConc" = as.numeric(absMinConc),
        "relMinConc" = as.numeric(relMinConc),
        "absMaxTox" = as.numeric(absMaxTox),
        "relMaxTox" = as.numeric(relMaxTox),
        "absMinConcTox" = as.numeric(absMinConcTox),
        "relMinConcTox" = as.numeric(relMinConcTox),
        "maxReplicateIntRSD" = as.numeric(maxReplicateIntRSD),
        "blankThreshold" = as.numeric(blankThreshold),
        "retentionRange" = as.numeric(retentionRange),
        "mzRange" = as.numeric(mzRange),
        "mzDefectRange" = as.numeric(mzDefectRange),
        "chromWidthRange" = as.numeric(chromWidthRange),
        "featQualityRange" = as.numeric(featQualityRange),
        "groupQualityRange" = as.numeric(groupQualityRange),
        "rGroups" = as.character(rGroups),
        "removeBlanks" = as.logical(removeBlanks),
        "removeISTDs" = as.logical(removeISTDs),
        "removeNA" = as.logical(removeNA),
        "negate" = as.logical(negate)
      ),
      number_permitted = Inf,
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
    checkmate::assert_choice(self@method, "FilterFeatures")
    checkmate::assert_choice(self@algorithm, "patRoon")
    checkmate::assert_numeric(self@parameters$absMinIntensity)
    checkmate::assert_numeric(self@parameters$relMinIntensity)
    checkmate::assert_numeric(self@parameters$preAbsMinIntensity)
    checkmate::assert_numeric(self@parameters$preRelMinIntensity)
    checkmate::assert_numeric(self@parameters$absMinAnalyses)
    checkmate::assert_numeric(self@parameters$relMinAnalyses)
    checkmate::assert_numeric(self@parameters$absMinReplicates)
    checkmate::assert_numeric(self@parameters$relMinReplicates)
    checkmate::assert_numeric(self@parameters$absMinFeatures)
    checkmate::assert_numeric(self@parameters$relMinFeatures)
    checkmate::assert_numeric(self@parameters$absMinReplicateAbundance)
    checkmate::assert_numeric(self@parameters$relMinReplicateAbundance)
    checkmate::assert_numeric(self@parameters$absMinConc)
    checkmate::assert_numeric(self@parameters$relMinConc)
    checkmate::assert_numeric(self@parameters$absMaxTox)
    checkmate::assert_numeric(self@parameters$relMaxTox)
    checkmate::assert_numeric(self@parameters$absMinConcTox)
    checkmate::assert_numeric(self@parameters$relMinConcTox)
    checkmate::assert_numeric(self@parameters$maxReplicateIntRSD)
    checkmate::assert_numeric(self@parameters$blankThreshold)
    checkmate::assert_numeric(self@parameters$retentionRange)
    checkmate::assert_numeric(self@parameters$mzRange)
    checkmate::assert_numeric(self@parameters$mzDefectRange)
    checkmate::assert_numeric(self@parameters$chromWidthRange)
    checkmate::assert_numeric(self@parameters$featQualityRange)
    checkmate::assert_numeric(self@parameters$groupQualityRange)
    checkmate::assert_character(self@parameters$rGroups)
    checkmate::assert_logical(self@parameters$removeBlanks)
    checkmate::assert_logical(self@parameters$removeISTDs)
    checkmate::assert_logical(self@parameters$removeNA)
    checkmate::assert_logical(self@parameters$negate)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FilterFeatures_patRoon) <- function(x, engine = NULL) {
  
  if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
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
  
  if (engine$nts$has_features) {
    nts <- engine$nts
    pat <- nts$features
    
  } else {
    warning("No features found! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  possible_only_in_features <- c(
    "absMinIntensity", "relMinIntensity", "retentionRange", "mzRange",
    "mzDefectRange", "chromWidthRange", "qualityRange", "negate"
  )
  
  if ("features" %in% is(pat$data)) parameters <- parameters[names(parameters) %in% possible_only_in_features]
  
  parameters <- lapply(parameters, function(z) if (is.na(z) || length(z) == 0) return(NULL) else z)
  
  filter_fun <- patRoon::filter
  
  pat <- do.call(filter_fun, c(list("obj" = pat), parameters))
  
  filtered <- nts$feature_list
  
  nts$features <- pat
  
  feature_list <- nts@feature_list
  
  filtered <- Map(
    function(x, y) {
      x <- x[!x$feature %in% y$feature, ]
      if (nrow(x) > 0) x$filtered <- TRUE
      x
    },
    filtered, feature_list
  )
  
  feature_list <- Map(
    function(x, y) {
      data.table::rbindlist(list(x, y), fill = TRUE)
    },
    feature_list, filtered
  )
  
  nts$feature_list <- feature_list
  
  engine$nts <- nts
  
  TRUE
}
