
#' @title .s3_ms_bin_spectra.Settings_filter_features_StreamFind
#'
#' @description Filter features and feature groups using the algorithm StreamFind.
#'
#' @noRd
#'
.s3_ms_filter_features.Settings_filter_features_StreamFind <- function(settings, self) {
  
  parameters <- settings$parameters
  
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
  
  n_features <- length(self$features)
  
  # Filters features annotated as isotopes when groups are present the
  # isotopes are filtered if present in the all samples of a replicate.
  #
  .filter_excludeIsotopes = function(value = NULL, self) {
    
    if (any(self$has_features()) && is.logical(value) && length(value) == 1) {
      
      features <- self$features@features
      
      features <- lapply(features, function(x) {
        
        if ("isotope" %in% colnames(x)) {
          iso <- vapply(x$isotope, function(z) z$step, NA_integer_)
          iso[is.na(iso)] <- 0
          sel <- iso > 0
          x$filtered[sel] <- TRUE
        }
        
        x
      })

      feature_list <- lapply(features, function(x) {
        x <- x[!x$filtered]
        x
      })
      
      self$feature_list <- feature_list
      
      filtered_feature_list <- lapply(features, function(x) {
        x <- x[x$filtered]
        x
      })
      
      self$filtered_features <- filtered_feature_list
      
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  # Filters features and feature groups with minimum signal-to-noise ratio.
  #
  .filter_minSnRatio <- function(value = 3, self) {
    
    if (any(self$has_features()) && is.numeric(value) && length(value) == 1) {
      
      features <- self$features@features
      
      features <- lapply(features, function(x) {
        
        if ("quality" %in% colnames(x)) {
          qlt <- vapply(x$quality, function(z) if (!is.null(z)) z$sn else NA_real_, NA_real_)
          qlt[is.na(qlt)] <- 0
          sel <- qlt <= value
          x$filtered[sel] <- TRUE
        }
        
        x
      })
      
      feature_list <- lapply(features, function(x) {
        x <- x[!x$filtered]
        x
      })
      
      self$feature_list <- feature_list
      
      filtered_feature_list <- lapply(features, function(x) {
        x <- x[x$filtered]
        x
      })
      
      self$filtered_features <- filtered_feature_list
      
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  # # Filters features and feature groups with minimum intensity.
  # #
  # .filter_minIntensity = function(value = 5000) {
  # 
  #   if (any(self$has_features())) {
  # 
  #     if (is.numeric(value) & length(value) == 1) {
  # 
  #       message("\U2699 Filtering by minIntensity...", appendLF = FALSE)
  # 
  #       # if (self$has_groups()) {
  #       #   rpl <- unique(self$get_replicate_names())
  #       #   groups <- self$get_groups(filtered = FALSE, intensities = TRUE, average = TRUE, metadata = FALSE)
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
  #   if (self$has_groups()) {
  # 
  #     if (is.numeric(value) & length(value) == 1) {
  # 
  #       message("\U2699 Filtering by maxGroupSd...", appendLF = FALSE)
  # 
  #       rpl <- self$get_replicate_names()
  #       rpl <- paste(rpl, "_sd", sep = '')
  #       names(rpl) <- self$get_analysis_names()
  # 
  #       groups <- self$get_groups(
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
  #   if (self$has_groups()) {
  # 
  #     if (is.numeric(value) & length(value) == 1) {
  # 
  #       message("\U2699 Filtering by minGroupAbundance...", appendLF = FALSE)
  # 
  #       rpl <- self$get_replicate_names()
  # 
  #       groups <- self$get_groups(filtered = FALSE, intensities = TRUE, average = FALSE, metadata = FALSE)
  # 
  #       features <- self$get_features(filtered = FALSE)
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
  #       #   rpl = self$get_replicate_names(),
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
  #   if (self$has_groups()) {
  # 
  #     if (is.numeric(value) & length(value) == 1) {
  # 
  #       blk <- self$get_blank_names()
  # 
  #       rpl <- self$get_replicate_names()
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
  #         groups <- self$get_groups(filtered = TRUE, intensities = TRUE, average = TRUE, sdValues = FALSE, metadata = FALSE)
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
  #         blk_anas <- unique(names(self$get_replicate_names()[self$get_replicate_names() %in% blk]))
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
  #   if (any(self$has_features())) {
  # 
  #     if (is.numeric(value) & length(value) == 2) {
  # 
  #       message("\U2699 Filtering by rtFilter...", appendLF = FALSE)
  # 
  #       value <- sort(value)
  # 
  #       # if (self$has_groups()) {
  #       #   rpl <- self$get_replicate_names()
  #       #
  #       #   groups <- self$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = TRUE)
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
  #   if (any(self$has_features())) {
  # 
  #     if (is.numeric(value) & length(value) == 2) {
  # 
  #       message("\U2699 Filtering by massFilter...", appendLF = FALSE)
  # 
  #       value <- sort(value)
  # 
  #       # if (self$has_groups()) {
  #       #   rpl <- self$get_replicate_names()
  #       #
  #       #   groups <- self$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = TRUE)
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
  #   if (any(self$has_suspects())) {
  # 
  #     if (is.logical(value) & length(value) == 1) {
  # 
  #       message("\U2699 Filtering by onlySuspects...", appendLF = FALSE)
  # 
  #       if (self$has_groups()) {
  # 
  #         sus <- self$get_suspects(onGroups = TRUE)
  # 
  #         groups <- self$get_groups(filtered = FALSE, intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE)
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
    
    switch(filters[i],
           # minIntensity = (private$.filter_minIntensity(parameters[[filters[i]]])),
           
           minSnRatio = .filter_minSnRatio(parameters[[filters[i]]], self),
           
           # maxGroupSd = (private$.filter_maxGroupSd(parameters[[filters[i]]])),
           
           # blank = (private$.filter_blank(parameters[[filters[i]]])),
           
           # minGroupAbundance = (private$.filter_minGroupAbundance(parameters[[filters[i]]])),
           
           excludeIsotopes = .filter_excludeIsotopes(parameters[[filters[i]]], self)
           
           # rtFilter = private$.filter_rtFilter(parameters[[filters[i]]]),
           
           # massFilter = private$.filter_massFilter(parameters[[filters[i]]]),
           
           # onlySuspects = .filter_onlySuspects(parameters[[filters[i]]], self)
           
           # TODO add more filters
    )
  }
  
  n_features_after <- length(self$features)
  
  n_features_filtered <- n_features - n_features_after
  
  if (n_features_filtered < 0) n_features_filtered <- 0
  
  message(paste0("\U2713 ", n_features_filtered, " features filtered!"))
  
  TRUE
}

#' @title .s3_ms_bin_spectra.Settings_filter_features_patRoon
#'
#' @description Filter features and feature groups using the algorithm patRoon.
#'
#' @noRd
#'
.s3_ms_filter_features.Settings_filter_features_patRoon <- function(settings, self) {
  
  if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  
  if (self$has_modules_data("patRoon")) {
    module_pat <- self$get_modules_data("patRoon")[["patRoon"]]
    
  } else {
    warning("No features found! Not done.")
    return(FALSE)
  }
  
  parameters <- settings$parameters
  
  possible_only_in_features <- c(
    "absMinIntensity", "relMinIntensity", "retentionRange", "mzRange",
    "mzDefectRange", "chromWidthRange", "qualityRange", "negate"
  )
  
  if ("features" %in% is(module_pat$data)) {
    parameters <- parameters[names(parameters) %in% possible_only_in_features]
  }
  
  filter_fun <- patRoon::filter
  
  pat <- do.call(filter_fun, c(list("obj" = module_pat$data), parameters))
  
  filtered <- self$features@features
  
  if ("features" %in% is(pat)) {
    self$features <- pat
    feature_list <- pat@features
    
  } else if ("featureGroups" %in% is(pat)) {
    self$featureGroups <- pat
    feature_list <- pat@features@features
    
  } else {
    warning("Filter output must be a features or featureGroups class object! Not done.")
    return(FALSE)
  }
  
  filtered <- Map(
    function(x, y) {
      x <- x[!x$ID %in% y$ID, ]
      if (nrow(x) > 0) x$filtered <- TRUE
      x
    },
    filtered, feature_list
  )
  
  self$filtered_features <- filtered
  
  TRUE
}
