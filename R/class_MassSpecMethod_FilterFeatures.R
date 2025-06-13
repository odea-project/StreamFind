#' MassSpecMethod_FilterFeatures_StreamFind S7 class
#'
#' @description Settings for filtering of features and feature groups.
#'
#' @template arg-ms-correctIntensity
#' @param minSnRatio Numeric (length 1) with the minimum signal-to-noise ratio.
#' @param minGaussianFit Numeric (length 1) with the minimum Gaussian fit.
#' @param excludeIsotopes Logical (length 1) with `TRUE` for filtering annotated isotopes
#' (only prevails the monoisotopic features).
#' @param excludeAdducts Logical (length 1) with `TRUE` for filtering annotated adducts.
#' @param minIntensity Numeric (length 1) with the minimum intensity threshold.
#' @param maxDeviationInReplicate Numeric (length 1) with the maximum standard deviation of the
#' intensity in each replicate group. The value must be a percentage from 0 to 100. The filter is
#' applied if the minimum standard deviation from all replicates is above the value.
#' @param minAbundanceInReplicate Numeric (length 1) with the minimum abundance in each replicate
#' group. The filter is applied if the maximum abundance from all replicates is below the value.
#' @param blankThreshold Numeric (length 1) with the intensity multiplier to set the blank
#' threshold. The filter is applied if the maximum mean intensity from all non-blank replicates
#' is below the correspondent blank intensity times the multiplier.
#' @param conservative Logical (length 1) with `TRUE` for conservative filtering. The filters are
#' applied on a feature group basis. If `FALSE`, the filters are applied on a feature basis. This 
#' means that when `TRUE` a feature is only filtered if all features within a feature group comply
#' with the filter.
#' @param onlyWithMS2 Logical (length 1) with `TRUE` for filtering features without MS2 spectra.
#'
#' @return A `MassSpecMethod_FilterFeatures_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_FilterFeatures_StreamFind <- S7::new_class(
  name = "MassSpecMethod_FilterFeatures_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(correctIntensity = TRUE,
                         minSnRatio = NA_real_,
                         minGaussianFit = NA_real_,
                         excludeIsotopes = FALSE,
                         excludeAdducts = FALSE,
                         minIntensity = NA_real_,
                         maxDeviationInReplicate = NA_real_,
                         minAbundanceInReplicate = NA_real_,
                         blankThreshold = NA_real_,
                         conservative = TRUE,
                         onlyWithMS2 = FALSE) {
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "FilterFeatures",
        required = "FindFeatures",
        algorithm = "StreamFind",
        parameters = list(
          correctIntensity = as.logical(correctIntensity),
          minSnRatio = as.numeric(minSnRatio),
          minGaussianFit = as.numeric(minGaussianFit),
          excludeIsotopes = as.logical(excludeIsotopes),
          excludeAdducts = as.logical(excludeAdducts),
          minIntensity = as.numeric(minIntensity),
          maxDeviationInReplicate = as.numeric(maxDeviationInReplicate),
          minAbundanceInReplicate = as.numeric(minAbundanceInReplicate),
          blankThreshold = as.numeric(blankThreshold),
          conservative = as.logical(conservative),
          onlyWithMS2 = as.logical(onlyWithMS2)
        ),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "FilterFeatures")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_logical(self@parameters$correctIntensity, len = 1)
    checkmate::assert_numeric(self@parameters$minSnRatio, len = 1)
    checkmate::assert_numeric(self@parameters$minGaussianFit, len = 1)
    checkmate::assert_logical(self@parameters$excludeIsotopes, len = 1)
    checkmate::assert_logical(self@parameters$excludeAdducts, len = 1)
    checkmate::assert_numeric(self@parameters$minIntensity, len = 1)
    checkmate::assert_numeric(
      self@parameters$maxDeviationInReplicate,
      len = 1,
      lower = 0,
      upper = 100
    )
    checkmate::assert_numeric(self@parameters$minAbundanceInReplicate, len = 1)
    checkmate::assert_numeric(self@parameters$blankThreshold, len = 1)
    checkmate::assert_logical(self@parameters$conservative, len = 1)
    checkmate::assert_logical(self@parameters$onlyWithMS2, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_FilterFeatures_StreamFind) <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$has_results_nts()) {
    warning("No NonTargetAnalysisResults object available! Not done.")
    return(FALSE)
  }

  if (!engine$NonTargetAnalysisResults$has_features) {
    warning("There are no features! Run find_features first!")
    return(FALSE)
  }

  parameters <- x$parameters
  correctIntensity <- parameters$correctIntensity
  parameters$correctIntensity <- NULL
  conservative <- parameters$conservative
  parameters$conservative <- NULL
  
  filters <- names(parameters)
  n_features <- sum(vapply(engine$NonTargetAnalysisResults$feature_list, function(x) sum(!x$filtered), 0))

  .filter_excludeIsotopes <- function(value = NULL, engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.logical(value) && length(value) == 1) {
      if (!value) {
        return()
      }
      
      features <- engine$NonTargetAnalysisResults$feature_list

      features <- lapply(features, function(x) {
        if ("annotation" %in% colnames(x)) {
          iso <- vapply(x$annotation, function(z) {
            if (nrow(z) == 0) {
              0
            } else {
              z[["iso_step"]]
            }
          }, 0)
          sel <- iso > 0 & !x$filtered
          x$filtered[sel] <- TRUE
          x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " excludeIsotopes"))
        }
        x
      })

      engine$NonTargetAnalysisResults$feature_list <- features
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }

  .filter_excludeAdducts <- function(value = NULL, engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.logical(value) && length(value) == 1) {
      if (!value) {
        return()
      }

      features <- engine$NonTargetAnalysisResults$feature_list

      features <- lapply(features, function(x) {
        if ("annotation" %in% colnames(x)) {
          res <- vapply(x$annotation, function(z) {
            if (nrow(z) == 0) {
              ""
            } else {
              z[["adduct_cat"]]
            }
          }, "")
          sel <- res != ""  & !x$filtered
          x$filtered[sel] <- TRUE
          x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " excludeAdducts"))
        }
        x
      })

      engine$NonTargetAnalysisResults$feature_list <- features
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }

  .filter_minSnRatio <- function(value = 3, conservative = FALSE, engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.numeric(value) && length(value) == 1) {
      
      if (is.na(value) || value == 0) {
        return()
      }
      
      if (engine$NonTargetAnalysisResults@has_groups && conservative) {
        rpl <- unique(engine$Analyses$replicates)
        groups <- get_groups(
          engine$NonTargetAnalysisResults,
          filtered = FALSE,
          intensities = FALSE,
          average = TRUE,
          metadata = TRUE,
          correctIntensity = FALSE
        )
        if (any(!is.na(groups$sn))) {
          groups_sel <- groups$sn < value
          groups <- groups$group[groups_sel]
          feature_list <- engine$NonTargetAnalysisResults$feature_list
          feature_list <- lapply(feature_list, function(x, groups) {
            sel <- x$group %in% groups & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minSnRatio"))
            x
          }, groups = groups)
          engine$NonTargetAnalysisResults$feature_list <- feature_list
        } else {
          warning("There are no signal-to-noise ratio values in features!")
        }
      } else {
        features <- engine$NonTargetAnalysisResults$feature_list
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
            sel <- qlt <= value & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minSnRatio"))
          }
          x
        })
        engine$NonTargetAnalysisResults$feature_list <- features
      }
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_minGaussianFit <- function(value = 0.3, conservative = FALSE, engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.numeric(value) && length(value) == 1) {
      
      if (is.na(value) || value == 0) {
        return()
      }
      
      if (engine$NonTargetAnalysisResults@has_groups && conservative) {
        rpl <- unique(engine$Analyses$replicates)
        groups <- get_groups(
          engine$NonTargetAnalysisResults,
          filtered = FALSE,
          intensities = FALSE,
          average = TRUE,
          metadata = TRUE
        )
        if (any(!is.na(groups$gauss_f))) {
          groups_sel <- groups$gauss_f < value
          groups <- groups$group[groups_sel]
          feature_list <- engine$NonTargetAnalysisResults$feature_list
          feature_list <- lapply(feature_list, function(x, groups) {
            sel <- x$group %in% groups & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minGaussianFit"))
            x
          }, groups = groups)
          engine$NonTargetAnalysisResults$feature_list <- feature_list
        } else {
          warning("There are no signal-to-noise ratio values in features!")
        }
      } else {
        features <- engine$NonTargetAnalysisResults$feature_list
        features <- lapply(features, function(x) {
          if ("quality" %in% colnames(x)) {
            qlt <- vapply(x$quality, function(z) {
              if (length(z) == 0) {
                NA_real_
              } else {
                z[["gauss_f"]]
              }
            }, NA_real_)
            qlt[is.na(qlt)] <- 0
            sel <- qlt <= value & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minGaussianFit"))
          }
          x
        })
        engine$NonTargetAnalysisResults$feature_list <- features
      }
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  # .filter_minRtMuDistance <- function(value = 2, conservative = FALSE, engine) {
  #   if (engine$NonTargetAnalysisResults$has_features && is.numeric(value) && length(value) == 1) {
  #     
  #     if (is.na(value) || value == 0) {
  #       return()
  #     }
  #     
  #     if (engine$NonTargetAnalysisResults@has_groups && conservative) {
  #       rpl <- unique(engine$Analyses$replicates)
  #       groups <- get_groups(
  #         engine$NonTargetAnalysisResults,
  #         filtered = FALSE,
  #         intensities = FALSE,
  #         average = TRUE,
  #         metadata = TRUE
  #       )
  #       if (any(!is.na(groups$gauss_f))) {
  #         groups_sel <- groups$gauss_f < value
  #         groups <- groups$group[groups_sel]
  #         feature_list <- engine$NonTargetAnalysisResults$feature_list
  #         feature_list <- lapply(feature_list, function(x, groups) {
  #           sel <- x$group %in% groups & !x$filtered
  #           x$filtered[sel] <- TRUE
  #           x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minRtMuDistance"))
  #           x
  #         }, groups = groups)
  #         engine$NonTargetAnalysisResults$feature_list <- feature_list
  #       } else {
  #         warning("There are no signal-to-noise ratio values in features!")
  #       }
  #     } else {
  #       features <- engine$NonTargetAnalysisResults$feature_list
  #       features <- lapply(features, function(x) {
  #         if ("quality" %in% colnames(x)) {
  #           mu <- vapply(x$quality, function(z) {
  #             if (length(z) == 0) {
  #               NA_real_
  #             } else {
  #               z[["gauss_u"]]
  #             }
  #           }, NA_real_)
  #           mu[is.na(mu)] <- 0
  #           distance_mu <- abs(mu - x$rt)
  #           sel <- distance_mu > value & !x$filtered
  #           x$filtered[sel] <- TRUE
  #           x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minRtMuDistance"))
  #         }
  #         x
  #       })
  #       engine$NonTargetAnalysisResults$feature_list <- features
  #     }
  #   } else {
  #     warning("There are no features in the MassSpecEngine!")
  #   }
  # }
  
  .filter_minIntensity <- function(value = 0, correctIntensity, conservative, engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.numeric(value) && length(value) == 1) {
      
      if (is.na(value) || value == 0) {
        return()
      }
      
      if (engine$NonTargetAnalysisResults@has_groups && conservative) {
        rpl <- unique(engine$Analyses$replicates)
        groups <- get_groups(
          engine$NonTargetAnalysisResults,
          filtered = FALSE,
          intensities = TRUE,
          average = TRUE,
          metadata = FALSE,
          correctIntensity = correctIntensity
        )
        rpl <- rpl[rpl %in% colnames(groups)]
        groups_sel <- apply(groups[, rpl, with = FALSE], MARGIN = 1, function(x) max(x) <= value)
        groups <- groups$group[groups_sel]
        feature_list <- engine$NonTargetAnalysisResults$feature_list
        feature_list <- lapply(feature_list, function(x, groups) {
          sel <- x$group %in% groups & !x$filtered
          x$filtered[sel] <- TRUE
          x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minIntensity"))
          x
        }, groups = groups)
        engine$NonTargetAnalysisResults$feature_list <- feature_list
      } else {
        feature_list <- engine$NonTargetAnalysisResults$feature_list
        feature_list <- lapply(feature_list, function(x, correctIntensity) {
          
          intensity_vector <- x$intensity
          
          if (correctIntensity) {
            if ("suppression_factor" %in% colnames(x)) {
              intensity_vector <- intensity_vector * x$suppression_factor
            }
          }
          sel <- intensity_vector <= value & !x$filtered
          x$filtered[sel] <- TRUE
          x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minIntensity"))
          x
        }, correctIntensity = correctIntensity)
        engine$NonTargetAnalysisResults$feature_list <- feature_list
      }
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_maxDeviationInReplicate <- function(value = 100, correctIntensity, conservative, engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.numeric(value) && length(value) == 1) {
      
      if (is.na(value) || value == 100) {
        return()
      }
      
      if (engine$NonTargetAnalysisResults@has_groups) {
        rpl <- unique(engine$Analyses$replicates)
        rpl <- paste(rpl, "_sd", sep = "")
        names(rpl) <- unique(engine$Analyses$replicates)
        
        groups <- get_groups(
          engine$NonTargetAnalysisResults,
          filtered = FALSE,
          intensities = TRUE,
          average = TRUE,
          sdValues = TRUE,
          metadata = FALSE,
          correctIntensity = correctIntensity
        )
        
        rpl <- rpl[rpl %in% colnames(groups)]
        
        if (conservative) {
          groups_sel <- apply(groups[, rpl, with = FALSE], MARGIN = 1, function(x) min(x) > value)
          groups <- groups$group[groups_sel]
          feature_list <- engine$NonTargetAnalysisResults$feature_list
          feature_list <- lapply(feature_list, function(x, groups) {
            sel <- x$group %in% groups & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " maxDeviationInReplicate"))
            x
          }, groups = groups)
          engine$NonTargetAnalysisResults$feature_list <- feature_list
          
        } else {
          groups_sel <- lapply(groups[, rpl, with = FALSE], function(x) x > value)
          groups_sel <- lapply(groups_sel, function(x) {
            names(x) <- groups$group
            x
          })
          names(groups_sel) <- names(rpl)
          
          feature_list <- engine$NonTargetAnalysisResults$feature_list
          
          feature_list <- Map(function(x, y) {
            if (nrow(x) > 0) {
              x$replicate <- y
            }
            x
          }, feature_list, engine$Analyses$replicates)
            
          feature_list <- lapply(feature_list, function(x, groups_sel) {
            sel <- groups_sel[[x$replicate[1]]]
            sel <- sel[sel]
            sel <- x$group %in% names(sel) & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " maxDeviationInReplicate"))
            x$replicate <- NULL
            x
          }, groups_sel = groups_sel)
          engine$NonTargetAnalysisResults$feature_list <- feature_list
        }
      } else {
        warning("There are no feature groups but needed for the maxDeviationInReplicate filter!")
      }
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_minAbundanceInReplicate <- function(value = 0, conservative, engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.numeric(value) && length(value) == 1) {
      
      if (is.na(value) || value == 0) {
        return()
      }
      
      if (engine$NonTargetAnalysisResults@has_groups) {
        rpl <- unique(engine$Analyses$replicates)
        rpl <- paste(rpl, "_n", sep = "")
        names(rpl) <- unique(engine$Analyses$replicates)
        
        groups <- get_groups(
          engine$NonTargetAnalysisResults,
          filtered = FALSE,
          intensities = TRUE,
          average = TRUE,
          sdValues = TRUE,
          metadata = FALSE
        )
        
        rpl <- rpl[rpl %in% colnames(groups)]
        
        if (conservative) {
          groups_sel <- apply(groups[, rpl, with = FALSE], MARGIN = 1, function(x) max(x) < value)
          groups <- groups$group[groups_sel]
          feature_list <- engine$NonTargetAnalysisResults$feature_list
          feature_list <- lapply(feature_list, function(x, groups) {
            sel <- x$group %in% groups & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " minAbundanceInReplicate"))
            x
          }, groups = groups)
          engine$NonTargetAnalysisResults$feature_list <- feature_list
          
        } else {
          groups_sel <- lapply(groups[, rpl, with = FALSE], function(x) x < value)
          groups_sel <- lapply(groups_sel, function(x) {
            names(x) <- groups$group
            x
          })
          names(groups_sel) <- names(rpl)
          
          feature_list <- engine$NonTargetAnalysisResults$feature_list
          
          feature_list <- Map(function(x, y) {
            if (nrow(x) > 0) {
              x$replicate <- y
            }
            x
          }, feature_list, engine$Analyses$replicates)
          
          feature_list <- lapply(feature_list, function(x, groups_sel) {
            sel <- groups_sel[[x$replicate[1]]]
            sel <- sel[sel]
            sel <- x$group %in% names(sel) & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " maxDeviationInReplicate"))
            x$replicate <- NULL
            x
          }, groups_sel = groups_sel)
          engine$NonTargetAnalysisResults$feature_list <- feature_list
        }
      } else {
        warning("There are no feature groups but needed for the minAbundanceInReplicate filter!")
      }
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_blankThreshold <- function(value = 3,
                                     correctIntensity,
                                     conservative,
                                     engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.numeric(value) && length(value) == 1) {
      
      if (is.na(value) || value == 0) {
        return()
      }
      
      if (engine$NonTargetAnalysisResults@has_groups) {
        info <- engine$Analyses$info[, c("replicate", "blank"), with = FALSE]
        info <- unique(info)
        
        if (all(is.na(info$blank) | info$blank %in% "")) {
          warning("There are no blank replicates!")
          return()
        }
        
        rpl <- unique(info$replicate)
        
        groups <- get_groups(
          engine$NonTargetAnalysisResults,
          filtered = FALSE,
          intensities = TRUE,
          average = TRUE,
          sdValues = FALSE,
          metadata = FALSE,
          correctIntensity = correctIntensity
        )
        
        rpl <- rpl[rpl %in% colnames(groups)]
        
        groups_rpl <- groups[, rpl, with = FALSE]
        
        groups_blk <- groups[, unique(info$blank), with = FALSE]
        
        groups_list <- lapply(colnames(groups_rpl), function(x,
                                                             value,
                                                             info,
                                                             groups_rpl,
                                                             groups_blk) {
          blk <- unique(info$blank[info$replicate %in% x])
          if (length(blk) == 0) {
            warning("There is no blank for replicate ", x, "!")
            return(rep(FALSE, nrow(groups_rpl)))
          }
          blk_ints <- groups_blk[, blk, with = FALSE]
          blk_ints <- apply(blk_ints, MARGIN = 1, FUN = function(z) max(z) * value)
          rpl_ints <- groups_rpl[, x, with = FALSE]
          rpl_ints <- apply(rpl_ints, MARGIN = 1, FUN = function(z) min(z))
          rpl_ints <= blk_ints
        }, value = value, info = info, groups_rpl = groups_rpl, groups_blk = groups_blk)
        names(groups_list) <- colnames(groups_rpl)
        groups_list <- data.table::as.data.table(groups_list)
        feature_list <- engine$NonTargetAnalysisResults$feature_list
        if (conservative) {
          groups_sel <- apply(groups_list, MARGIN = 1, function(x) all(x))
          groups <- groups$group[groups_sel]
          feature_list <- lapply(feature_list, function(x, groups) {
            sel <- x$group %in% groups & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " blankThreshold"))
            x
          }, groups = groups)
        } else {
          info2 <- engine$Analyses$info
          analyses <- names(feature_list)
          feature_groups_list <- lapply(analyses, function(x, info2, groups, groups_list) {
            rp <- info2$replicate[info2$analysis %in% x]
            if (!rp %in% colnames(groups_list)) {
              groups$group
            } else {
              sel <- groups_list[[rp]]
              groups$group[sel]
            }
          }, info2 = info2, groups = groups, groups_list = groups_list)
          
          names(feature_groups_list) <- analyses
          
          feature_list <- Map(function(x, y) {
            sel <- x$group %in% y & !x$filtered
            x$filtered[sel] <- TRUE
            x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " blankThreshold"))
            x
          }, feature_list, feature_groups_list)
          
          names(feature_list) <- analyses
        }
        engine$NonTargetAnalysisResults$feature_list <- feature_list
      } else {
        warning("There are no feature groups but needed for the blankThreshold filter!")
      }
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }
  
  .filter_onlyWithMS2 <- function(value = NULL, engine) {
    if (engine$NonTargetAnalysisResults$has_features && is.logical(value) && length(value) == 1) {
      if (!value) {
        return()
      }
      features <- engine$NonTargetAnalysisResults$feature_list
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
          sel <- sel & !x$filtered
          x$filtered[sel] <- TRUE
          x$filter[sel] <- gsub("NA ", "", paste0(x$filter[sel], " onlyWithMS2"))
        }
        x
      })
      engine$NonTargetAnalysisResults$feature_list <- features
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }

  # MARK: Switch Loop
  # __Switch Loop ----
  
  for (i in seq_len(length(filters))) {
    if (is.na(parameters[[filters[i]]]) || length(parameters[[filters[i]]]) == 0) next
    
    switch(
      filters[i],
      excludeIsotopes = .filter_excludeIsotopes(parameters[[filters[i]]], engine),
      excludeAdducts = .filter_excludeAdducts(parameters[[filters[i]]], engine),
      minIntensity = .filter_minIntensity(
        parameters[[filters[i]]],
        correctIntensity,
        conservative,
        engine
      ),
      blankThreshold = .filter_blankThreshold(
        parameters[[filters[i]]],
        correctIntensity,
        conservative,
        engine
      ),
      maxDeviationInReplicate = .filter_maxDeviationInReplicate(
        parameters[[filters[i]]],
        correctIntensity,
        conservative,
        engine
      ),
      minSnRatio = .filter_minSnRatio(parameters[[filters[i]]], conservative, engine),
      minGaussianFit = .filter_minGaussianFit(parameters[[filters[i]]], conservative, engine),
      minAbundanceInReplicate = .filter_minAbundanceInReplicate(
        parameters[[filters[i]]],
        conservative,
        engine
      ),
      onlyWithMS2 = .filter_onlyWithMS2(parameters[[filters[i]]], engine)
    )
  }
  
  n_features_after <- sum(vapply(engine$NonTargetAnalysisResults$feature_list, function(x) sum(!x$filtered), 0))
  
  n_features_filtered <- n_features - n_features_after
  
  if (n_features_filtered < 0) n_features_filtered <- 0
  
  message(paste0("\U2713 ", n_features_filtered, " features filtered!"))
  
  TRUE
}

#' MassSpecMethod_FilterFeatures_patRoon S7 class
#'
#' @description Settings for filtering of features and feature groups. A full description of the
#' filtering parameters is in \code{\link[patRoon]{replicateGroupSubtract}} from patRoon package.
#'
#' @return A MassSpecMethod_FilterFeatures_patRoon object.
#'
#' @param absMinIntensity Numeric length one. Minimum absolute intensity for a feature.
#' @param relMinIntensity Numeric length one. Minimum relative intensity for a feature.
#' @param preAbsMinIntensity Numeric length one. Minimum absolute intensity for a feature before
#' grouping.
#' @param preRelMinIntensity Numeric length one. Minimum relative intensity for a feature before
#' grouping.
#' @param absMinAnalyses Numeric length one. Minimum number of analyses a feature must be present
#' in.
#' @param relMinAnalyses Numeric length one. Minimum relative number of analyses a feature must be
#' present in.
#' @param absMinReplicates Numeric length one. Minimum number of replicates a feature must be
#' present in.
#' @param relMinReplicates Numeric length one. Minimum relative number of replicates a feature must
#' be present in.
#' @param absMinFeatures Numeric length one. Minimum number of features a feature group must contain.
#' @param relMinFeatures Numeric length one. Minimum relative number of features a feature group
#' must contain.
#' @param absMinReplicateAbundance Numeric length one. Minimum absolute abundance of a replicate.
#' @param relMinReplicateAbundance Numeric length one. Minimum relative abundance of a replicate.
#' @param absMinConc Numeric length one. Minimum absolute concentration of a feature.
#' @param relMinConc Numeric length one. Minimum relative concentration of a feature.
#' @param absMaxTox Numeric length one. Maximum absolute toxicity of a feature.
#' @param relMaxTox Numeric length one. Maximum relative toxicity of a feature.
#' @param absMinConcTox Numeric length one. Minimum absolute concentration of a feature to be
#' considered toxic.
#' @param relMinConcTox Numeric length one. Minimum relative concentration of a feature to be
#' considered toxic.
#' @param maxReplicateIntRSD Numeric length one. Maximum relative standard deviation of intensities
#' within a replicate.
#' @param blankThreshold Numeric length one. Maximum intensity of a feature to be considered a
#' blank.
#' @param retentionRange Numeric length two. Retention time range (in seconds) for a feature.
#' @param mzRange Numeric length two. m/z range (in Da) for a feature.
#' @param mzDefectRange Numeric length two. m/z defect range (in Da) for a feature.
#' @param chromWidthRange Numeric length two. Chromatographic width range (in seconds) for a
#' feature.
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
MassSpecMethod_FilterFeatures_patRoon <- S7::new_class(
  "MassSpecMethod_FilterFeatures_patRoon",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(absMinIntensity = NA_real_,
                         relMinIntensity = NA_real_,
                         preAbsMinIntensity = NA_real_,
                         preRelMinIntensity = NA_real_,
                         absMinAnalyses = NA_real_,
                         relMinAnalyses = NA_real_,
                         absMinReplicates = NA_real_,
                         relMinReplicates = NA_real_,
                         absMinFeatures = NA_real_,
                         relMinFeatures = NA_real_,
                         absMinReplicateAbundance = NA_real_,
                         relMinReplicateAbundance = NA_real_,
                         absMinConc = NA_real_,
                         relMinConc = NA_real_,
                         absMaxTox = NA_real_,
                         relMaxTox = NA_real_,
                         absMinConcTox = NA_real_,
                         relMinConcTox = NA_real_,
                         maxReplicateIntRSD = NA_real_,
                         blankThreshold = NA_real_,
                         retentionRange = NA_real_,
                         mzRange = NA_real_,
                         mzDefectRange = NA_real_,
                         chromWidthRange = NA_real_,
                         featQualityRange = NA_real_,
                         groupQualityRange = NA_real_,
                         rGroups = NA_real_,
                         removeBlanks = FALSE,
                         removeISTDs = FALSE,
                         removeNA = FALSE,
                         negate = FALSE) {
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "FilterFeatures",
        required = "FindFeatures",
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
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
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
S7::method(run, MassSpecMethod_FilterFeatures_patRoon) <- function(x, engine = NULL) {
  if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_nts()) {
    warning("No NonTargetAnalysisResults object available! Not done.")
    return(FALSE)
  }
  
  if (engine$NonTargetAnalysisResults$has_features) {
    NonTargetAnalysisResults <- engine$NonTargetAnalysisResults
    pat <- get_patRoon_features(NonTargetAnalysisResults, featureGroups = TRUE)
  } else {
    warning("No features found! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  possible_only_in_features <- c(
    "absMinIntensity", "relMinIntensity", "retentionRange", "mzRange",
    "mzDefectRange", "chromWidthRange", "qualityRange", "negate"
  )

  if ("features" %in% is(pat)) {
    parameters <- parameters[names(parameters) %in% possible_only_in_features]
  }

  parameters <- lapply(parameters, function(z) {
    if (all(is.na(z)) || length(z) == 0) {
      return(NULL)
    } else {
      z
    }
  })

  filter_fun <- patRoon::filter
  
  pat <- do.call(filter_fun, c(list("obj" = pat), parameters))

  pat_features <- pat@features
  
  if ("features" %in% is(pat_features)) {
    pat_features <- pat_features@features
  }

  feature_list <- NonTargetAnalysisResults$feature_list
  
  pat_features <- pat_features[names(pat_features) %in% names(feature_list)]
  pat_features <- pat_features[match(names(feature_list), names(pat_features))]

  feature_list <- Map(function(x, y) {
    if (nrow(y) == 0) {
      if (nrow(x) > 0) {
        x$filtered[!x$filtered] <- TRUE
        x$filter[!x$filtered] <- "patRoon"
      }
    } else {
      if (nrow(x) > 0) {
        x$filtered[!x$filtered & !x$feature %in% y$ID] <- TRUE
        x$filter[!x$filtered & !x$feature %in% y$ID] <- "patRoon"
      }
    }
    x
  }, feature_list, pat_features)

  NonTargetAnalysisResults$feature_list <- feature_list
  engine$NonTargetAnalysisResults <- NonTargetAnalysisResults
  TRUE
}
