
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FilterFeatures_StreamFind**
#'
#' @description Settings for filtering of features and feature groups.
#'
#' @param minSnRatio Numeric (length 1) with the minimum signal-to-noise ratio.
#' @param excludeIsotopes Logical (length 1) with `TRUE` for filtering annotated isotopes (only prevails the monoisotopic features).
#'
#' @return A `MassSpecSettings_FilterFeatures_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_FilterFeatures_StreamFind <- S7::new_class("MassSpecSettings_FilterFeatures_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(minSnRatio = NULL, excludeIsotopes = NULL) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FilterFeatures",
      algorithm = "StreamFind",
      parameters = list( minSnRatio = minSnRatio, excludeIsotopes = excludeIsotopes),
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
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "FilterFeatures"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_numeric(self@parameters$minSnRatio, len = 1, null.ok = TRUE),
      checkmate::test_logical(self@parameters$excludeIsotopes, len = 1, null.ok = TRUE)
    )
    if (!valid) return(FALSE)
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
  
  if (length(engine$analyses) == 0) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_features()) {
    warning("There are no features! Run find_features first!")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  filters <- names(parameters)
  
  possible_feature_filters <- c(
    "minSnRatio",
    # "blank",
    # "maxGroupSd",
    # "minGroupAbundance",
    "excludeIsotopes",
    # "excludeAdducts",
    # "rtFilter",
    # "massFilter",
    "onlySuspects"
  )
  
  if (!all(filters %in% possible_feature_filters)) {
    warning("At least one of the filters is not recognized.")
    return(FALSE)
  }
  
  n_features <- engine$NTS$number_features
  
  # Filters features annotated as isotopes when groups are present the
  # isotopes are filtered if present in the all samples of a replicate.
  #
  .filter_excludeIsotopes = function(value = NULL, engine) {
    
    if (any(engine$has_features()) && is.logical(value) && length(value) == 1) {
      
      features <- engine$NTS$feature_list
      
      features <- lapply(features, function(x) {
        if ("isotope" %in% colnames(x)) {
          iso <- vapply(x$isotope, function(z) {
            if (length(z) == 0) {
              NA_integer_
            } else {
              z[["step"]]
            }
          }, NA_integer_)
          iso[is.na(iso)] <- 0
          sel <- iso > 0
          x$filtered[sel] <- TRUE
        }
        x
      })
      
      engine$NTS$feature_list <- features
      
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  # Filters features and feature groups with minimum signal-to-noise ratio.
  #
  .filter_minSnRatio <- function(value = 3, engine) {
    
    if (any(engine$has_features()) && is.numeric(value) && length(value) == 1) {
      
      features <- engine$NTS$feature_list
      
      features <- lapply(features, function(x) {
        
        if ("quality" %in% colnames(x)) {
          qlt <- vapply(x$quality, function(z) if (!is.null(z)) z[["sn"]] else NA_real_, NA_real_)
          qlt[is.na(qlt)] <- 0
          sel <- qlt <= value
          x$filtered[sel] <- TRUE
        }
        
        x
      })
      
      engine$NTS$feature_list <- features
      
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  # # Filters features and feature groups with minimum intensity.
  # #
  # .filter_minIntensity = function(value = 5000) {
  # 
  #   if (any(engine$has_features())) {
  # 
  #     if (is.numeric(value) & length(value) == 1) {
  # 
  #       message("\U2699 Filtering by minIntensity...", appendLF = FALSE)
  # 
  #       # if (engine$has_groups()) {
  #       #   rpl <- unique(engine$get_replicate_names())
  #       #   groups <- engine$get_groups(filtered = FALSE, intensities = TRUE, average = TRUE, metadata = FALSE)
  #       #   groups_sel <- apply(groups[, rpl, with = FALSE], MARGIN = 1,function(x) max(x) <= value)
  #       #   groups <- groups$group[groups_sel]
  #       #   private$.tag_filtered(groups, "minIntensity")
  #       #
  #       # } else {
  #         private$.analyses <- lapply(private$.analyses, function(x) {
  #           sel <- (x$features$intensity <= value) & (!x$features$filtered)
  #           x$features$filtered[sel] <- TRUE
  #           x$features$filter[sel] <- "minIntensity"
  #           x
  #         })
  #       # }
  # 
  #       private$.register(
  #         "filter_features",
  #         "features",
  #         "minIntensity",
  #         "StreamFind",
  #         as.character(packageVersion("StreamFind")),
  #         paste0(value, " counts")
  #       )
  # 
  #       message(" Done!")
  # 
  #     } else {
  #       stop("The value for minimum intensity filtering must be numeric and of length one!")
  #     }
  #   } else {
  #     warning("There are no features in the MassSpecEngine!")
  #   }
  # },
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
  
  for (i in seq_len(length(filters))) {
    
    if (is.null(parameters[[filters[i]]])) next
    
    switch(filters[i],
           # minIntensity = (private$.filter_minIntensity(parameters[[filters[i]]])),
           
           minSnRatio = .filter_minSnRatio(parameters[[filters[i]]], engine),
           
           # maxGroupSd = (private$.filter_maxGroupSd(parameters[[filters[i]]])),
           
           # blank = (private$.filter_blank(parameters[[filters[i]]])),
           
           # minGroupAbundance = (private$.filter_minGroupAbundance(parameters[[filters[i]]])),
           
           excludeIsotopes = .filter_excludeIsotopes(parameters[[filters[i]]], engine)
           
           # rtFilter = private$.filter_rtFilter(parameters[[filters[i]]]),
           
           # massFilter = private$.filter_massFilter(parameters[[filters[i]]]),
           
           # onlySuspects = .filter_onlySuspects(parameters[[filters[i]]], engine)
           
           # TODO add more filters
    )
  }
  
  n_features_after <- engine$NTS$number_features
  
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
#' @param rGroups List of replicate groups.
#' @param results Only keep feature groups that have results in the object specified by results. See 
#' \code{\link[patRoon]{replicateGroupSubtract}} for further information.
#' @param removeBlanks Logical length one. Remove blank samples.
#' @param removeISTDs Logical length one. Remove internal standards.
#' @param checkFeaturesSession Check features session.
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
                         results = NULL,
                         removeBlanks = FALSE,
                         removeISTDs = FALSE,
                         checkFeaturesSession = NULL,
                         # predAggrParams = patRoon::getDefPredAggrParams(),
                         removeNA = FALSE,
                         negate = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FilterFeatures",
      algorithm = "patRoon",
      parameters = list(
        "absMinIntensity" = absMinIntensity,
        "relMinIntensity" = relMinIntensity,
        "preAbsMinIntensity" = preAbsMinIntensity,
        "preRelMinIntensity" = preRelMinIntensity,
        "absMinAnalyses" = absMinAnalyses,
        "relMinAnalyses" = relMinAnalyses,
        "absMinReplicates" = absMinReplicates,
        "relMinReplicates" = relMinReplicates,
        "absMinFeatures" = absMinFeatures,
        "relMinFeatures" = relMinFeatures,
        "absMinReplicateAbundance" = absMinReplicateAbundance,
        "relMinReplicateAbundance" = relMinReplicateAbundance,
        "absMinConc" = absMinConc,
        "relMinConc" = relMinConc,
        "absMaxTox" = absMaxTox,
        "relMaxTox" = relMaxTox,
        "absMinConcTox" = absMinConcTox,
        "relMinConcTox" = relMinConcTox,
        "maxReplicateIntRSD" = maxReplicateIntRSD,
        "blankThreshold" = blankThreshold,
        "retentionRange" = retentionRange,
        "mzRange" = mzRange,
        "mzDefectRange" = mzDefectRange,
        "chromWidthRange" = chromWidthRange,
        "featQualityRange" = featQualityRange,
        "groupQualityRange" = groupQualityRange,
        "rGroups" = rGroups,
        "results" = results,
        "removeBlanks" = removeBlanks,
        "removeISTDs" = removeISTDs,
        "checkFeaturesSession" = checkFeaturesSession,
        # "predAggrParams" = predAggrParams,
        "removeNA" = removeNA,
        "negate" = negate
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
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "FilterFeatures"),
      checkmate::test_choice(self@algorithm, "patRoon"),
      checkmate::test_numeric(self@parameters$absMinIntensity, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$relMinIntensity, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$preAbsMinIntensity, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$preRelMinIntensity, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$absMinAnalyses, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$relMinAnalyses, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$absMinReplicates, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$relMinReplicates, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$absMinFeatures, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$relMinFeatures, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$absMinReplicateAbundance, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$relMinReplicateAbundance, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$absMinConc, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$relMinConc, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$absMaxTox, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$relMaxTox, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$absMinConcTox, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$relMinConcTox, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$maxReplicateIntRSD, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$blankThreshold, len = 1, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$retentionRange, len = 2, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$mzRange, len = 2, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$mzDefectRange, len = 2, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$chromWidthRange, len = 2, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$featQualityRange, len = 2, null.ok = TRUE),
      checkmate::test_numeric(self@parameters$groupQualityRange, len = 2, null.ok = TRUE),
      checkmate::test_character(self@parameters$rGroups, null.ok = TRUE),
      checkmate::test_list(self@parameters$results, null.ok = TRUE),
      checkmate::test_logical(self@parameters$removeBlanks, len = 1),
      checkmate::test_logical(self@parameters$removeISTDs, len = 1),
      checkmate::test_list(self@parameters$checkFeaturesSession, null.ok = TRUE),
      checkmate::test_logical(self@parameters$removeNA, len = 1),
      checkmate::test_logical(self@parameters$negate, len = 1)
    )
    if (!valid) return(FALSE)
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
  
  if (engine$has_features()) {
    nts <- engine$NTS
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
  
  if ("features" %in% is(pat$data)) {
    parameters <- parameters[names(parameters) %in% possible_only_in_features]
  }
  
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
  
  engine$NTS <- nts
  
  TRUE
}
