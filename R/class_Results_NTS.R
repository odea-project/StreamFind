#' @export
#' @noRd
NTS <- S7::new_class("NTS",
  package = "StreamFind", parent = Results,
  properties = list(
    
    # MARK: features_properties
    ## __features_properties -----
    features_properties = S7::new_property(S7::class_character),
    
    # MARK: features
    ## __features -----
    features = S7::new_property(S7::class_list, default = list()),
    
    # MARK: number_features
    ## __number_features -----
    number_features = S7::new_property(S7::class_integer,
      getter = function(self) {
        if (length(self@features) > 0) {
          length(self@features)
        } else {
          0
        }
      }
    ),
    
    # MARK: has_features
    ## __has_features -----
    has_features = S7::new_property(S7::class_logical, getter = function(self) self@number_features > 0),
    
    # MARK: has_groups
    ## __has_groups -----
    has_groups = S7::new_property(S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          if ("featureGroups" %in% is(self@features)) {
            if (length(self@features) > 0) {
              return(TRUE)
            }
            return(FALSE)
          }
        }
        FALSE
      }
    ),
    
    # MARK: group_names
    ## __group_names -----
    group_names = S7::new_property(S7::class_character, getter = function(self) {
      if (self@has_groups) {
        return(names(self@features[[1]]))
      }
      NA_character_
    }),
    
    # MARK: filtered
    ## __filtered -----
    filtered = S7::new_property(S7::class_list, default = list()),
    
    # MARK: feature_list
    ## __feature_list -----
    feature_list = S7::new_property(S7::class_list,
      getter = function(self) {
        if (!self@has_features) return(self@features)
        
        if ("featureGroups" %in% is(self@features[[1]])) {
          pat <- self@features@features[[1]]
        } else {
          pat <- self@features[[1]]
        }
        
        f_list <- pat@features
        
        filtered_list <- self$filtered
        
        f_list <- Map(
          function(x, y) {
            if (nrow(x) > 0) x <- x[!(x$ID %in% y$ID), ]
            y <- data.table::rbindlist(list(y, x), fill = TRUE)
            y
          },
          filtered_list, f_list
        )
        
        f_list <- lapply(f_list, function(x) {
          z <- data.table::copy(x)
          data.table::setnames(z, "ID", "feature", skip_absent = TRUE)
          data.table::setnames(z, "ret", "rt", skip_absent = TRUE)
          data.table::setnames(z, "retmin", "rtmin", skip_absent = TRUE)
          data.table::setnames(z, "retmax", "rtmax", skip_absent = TRUE)
        })
        
        if ("featuresSet" %in% is(pat)) {
          for (x in names(f_list)) {
            adduct_val <- f_list[[x]]$polarity * 1.007276
            f_list[[x]]$mz <- f_list[[x]]$mz + adduct_val
            f_list[[x]]$mzmin <- f_list[[x]]$mzmin + adduct_val
            f_list[[x]]$mzmax <- f_list[[x]]$mzmax + adduct_val
          }
        }
        
        f_list
      },
      
      setter = function(self, value) {
        if (!is.list(value)) {
          warning("The argument value must be a list!")
          return(self)
        }
        if (!self@has_features) {
          warning("No features object found! Not done.")
          return(self)
        }
        if (length(value) != self@number_analyses) {
          warning("The argument value must have the same length as the number of analyses!")
          return(self)
        }
        if (!all(vapply(value, is.data.frame, FALSE))) {
          warning("The argument value must be a list of data.frames!")
          return(self)
        }
        value <- lapply(value, function(x) {
          data.table::setnames(x, "feature", "ID", skip_absent = TRUE)
          data.table::setnames(x, "rt", "ret", skip_absent = TRUE)
          data.table::setnames(x, "rtmin", "retmin", skip_absent = TRUE)
          data.table::setnames(x, "rtmax", "retmax", skip_absent = TRUE)
        })
        
        if ("features" %in% is(self$features[[1]])) {
          if (identical(patRoon::analyses(self@features[[1]]), names(value))) {
            if ("featuresSet" %in% is(self$features[[1]])) {
              for (x in names(value)) {
                adduct_val <- value[[x]]$polarity * -1.007276
                value[[x]]$mz <- value[[x]]$mz + adduct_val
                value[[x]]$mzmin <- value[[x]]$mzmin + adduct_val
                value[[x]]$mzmax <- value[[x]]$mzmax + adduct_val
              }
            }
            self@features[[1]]@features <- lapply(value, function(x) x[!x$filtered, ])
            self@filtered <- lapply(value, function(x) x[x$filtered, ])
            self
          } else {
            warning("Feature list names not matching analysis names! Not done.")
            return(self)
          }
        } else if ("featureGroups" %in% is(self$features[[1]])) {
          if (identical(patRoon::analyses(self@features[[1]]), names(value))) {
            if (all(vapply(value, function(x) "group" %in% colnames(x), FALSE))) {
              if ("featureGroupsSet" %in% is(self$features[[1]])) {
                for (x in names(value)) {
                  adduct_val <- value[[x]]$polarity * -1.007276
                  value[[x]]$mz <- value[[x]]$mz + adduct_val
                  value[[x]]$mzmin <- value[[x]]$mzmin + adduct_val
                  value[[x]]$mzmax <- value[[x]]$mzmax + adduct_val
                }
              }
              fg_left <- unique(unlist(lapply(value, function(x) x$group[!x$filtered])))
              fg_left <- fg_left[!is.na(fg_left)]
              if (length(fg_left) > 0) {
                fg <- self$features
                fg <- fg[, fg_left]
                self@features <- fg
              }
              self@features[[1]]@features@features <- lapply(value, function(x) x[!x$filtered, ])
              self@filtered <- lapply(value, function(x) x[x$filtered, ])
              self
            } else {
              warning("Feature groups not present in features! Not done.")
              return(self)
            }
          } else {
            warning("Feature list names not matching analysis names! Not done.")
            return(self)
          }
        } else {
          warning("Features not found! Not done.")
          return(self)
        }
      }
    ),
    
    # MARK: mspl
    ## __mspl -----
    mspl = S7::new_property(S7::class_list, default = list()),
    
    # MARK: formulas
    ## __formulas -----
    formulas = S7::new_property(S7::class_list, default = list()),
    
    # MARK: compounds
    ## __compounds -----
    compounds = S7::new_property(S7::class_list, default = list()),
    
    # MARK: analysisInfo
    ## __analysisInfo -----
    analysisInfo = S7::new_property(S7::class_data.frame,
      getter = function(self) {
        if (self@has_features) return(self@features@analysisInfo)
        data.table::data.table()
      }
    ),
    
    # MARK: number_analyses
    ## __number_analyses -----
    number_analyses = S7::new_property(S7::class_integer,
      getter = function(self) {
        if (self@has_features) length(self@features[[1]]@analysisInfo$analysis)
        0
      }
    ),
    
    # MARK: has_features_ms1
    ## __has_features_ms1 -----
    has_features_ms1 = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@number_features > 0) {
        return(any(vapply(self$feature_list, function(x) {
          if ("ms1" %in% colnames(x)) {
            any(vapply(x$ms1, function(z) length(z) > 0, FALSE))
          } else {
            FALSE
          }
        }, FALSE)))
      }
      FALSE
    }),
    
    # MARK: has_features_ms2
    ## __has_features_ms2 -----
    has_features_ms2 = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@number_features > 0) {
        return(any(vapply(self$feature_list, function(x) {
          if ("ms2" %in% colnames(x)) {
            any(vapply(x$ms2, function(z) length(z) > 0, FALSE))
          } else {
            FALSE
          }
        }, FALSE)))
      }
      FALSE
    }),
    
    # MARK: has_features_eic
    ## __has_features_eic -----
    has_features_eic = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@number_features > 0) {
        return(any(vapply(self$feature_list, function(x) {
          if ("eic" %in% colnames(x)) {
            any(vapply(x$eic, function(z) length(z) > 0, FALSE))
          } else {
            FALSE
          }
        }, FALSE)))
      }
      FALSE
    }),
    
    # MARK: has_features_suspects
    ## __has_features_suspects -----
    has_features_suspects = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@number_features > 0) {
        return(any(vapply(self$feature_list, function(x) {
          if ("suspects" %in% colnames(x)) {
            any(vapply(x$suspects, function(z) length(z) > 0, FALSE))
          } else {
            FALSE
          }
        }, FALSE)))
      }
      FALSE
    })
  ),
  constructor = function(
    features = list(),
    filtered = list(),
    mspl = list(),
    formulas = list(),
    compounds = list()) {
    
    if (!is.list(features)) {
      if (!("features" %in% is(features) || "featureGroups" %in% is(features))) {
        warning("The argument features must be of class features or featureGroups from patRoon")
        features <- list()
      }
    } else {
      warning("Empty NTS object created!")
      features <- list()
    }
    
    if (length(features) > 0) {
      mspl <- list(new("MSPeakLists", algorithm = NA_character_))
      formulas <- list(new("formulas", algorithm = NA_character_))
      compounds <- list(new("compounds", algorithm = NA_character_))
      if (("featuresSet" %in% is(features) || "featureGroupsSet" %in% is(features))) {
        mspl <- new("MSPeakListsSet", algorithm = NA_character_)
        formulas <- new("formulasSet", algorithm = NA_character_)
        compounds <- new("compoundsSet", algorithm = NA_character_)
      }
    }

    features_properties <- c(
      "ID", "ret", "mz", "area", "intensity", "retmin", "retmax", "mzmin", "mzmax", "mass", "polarity", "adduct",
      "filtered", "filled", "quality", "annotation", "istd", "ms1", "ms2", "eic", "suspects"
    )

    if ("features" %in% is(features)) {
      features@features <- lapply(features@features, function(x) {
        n_features <- nrow(x)
        if (n_features > 0) {
          if (!"polarity" %in% colnames(x)) warning("Polarity column not found in features but required!")
          if (!"mass" %in% colnames(x)) warning("Mass column not found in features but required!")
          if (!"filtered" %in% colnames(x)) x$filtered <- FALSE
          if (!"filter" %in% colnames(x)) x$filtered <- NA_character_
          if (!"filled" %in% colnames(x)) x$filled <- FALSE
          if (!"quality" %in% colnames(x)) x$quality <- list(rep(data.table::data.table(), n_features))
          if (!"annotation" %in% colnames(x)) x$annotation <- list(rep(data.table::data.table(), n_features))
          if (!"eic" %in% colnames(x)) x$eic <- list(rep(data.table::data.table(), n_features))
          if (!"ms1" %in% colnames(x)) x$ms1 <- list(rep(data.table::data.table(), n_features))
          if (!"ms2" %in% colnames(x)) x$ms2 <- list(rep(data.table::data.table(), n_features))
          if (!"istd" %in% colnames(x)) x$istd <- list(rep(data.table::data.table(), n_features))
          if (!"suspects" %in% colnames(x)) x$suspects <- list(rep(data.table::data.table(), n_features))
        }
        x
      })
      
      features <- list(features)
      
    } else if ("featureGroups" %in% is(features)) {
      features@features@features <- lapply(features@features@features, function(x) {
        if (nrow(x) > 0) {
          if (!"polarity" %in% colnames(x)) warning("Polarity column not found in features but required!")
          if (!"mass" %in% colnames(x)) warning("Mass column not found in features but required!")
          if (!"filtered" %in% colnames(x)) x$filtered <- FALSE
          if (!"filter" %in% colnames(x)) x$filtered <- NA_character_
          if (!"filled" %in% colnames(x)) x$filled <- FALSE
          if (!"quality" %in% colnames(x)) x$quality <- list(rep(data.table::data.table(), nrow(x)))
          if (!"annotation" %in% colnames(x)) x$annotation <- list(rep(data.table::data.table(), nrow(x)))
          if (!"eic" %in% colnames(x)) x$eic <- list(rep(data.table::data.table(), nrow(x)))
          if (!"ms1" %in% colnames(x)) x$ms1 <- list(rep(data.table::data.table(), nrow(x)))
          if (!"ms2" %in% colnames(x)) x$ms2 <- list(rep(data.table::data.table(), nrow(x)))
          if (!"istd" %in% colnames(x)) x$istd <- list(rep(data.table::data.table(), nrow(x)))
          if (!"suspects" %in% colnames(x)) x$suspects <- list(rep(data.table::data.table(), nrow(x)))
        }
        x
      })
      
      features <- list(features)
    }

    S7::new_object(
      Results(),
      name = "nts",
      software = "patRoon",
      version = as.character(packageVersion("patRoon")),
      features_properties = features_properties,
      features = features,
      filtered = filtered,
      mspl = mspl,
      formulas = formulas,
      compounds = compounds
    )
  },
  validator = function(self) {
    checkmate::assert_true(self@name == "nts")
    checkmate::assert_true(self@software == "patRoon")
    checkmate::assert_character(self@version, len = 1)
    if (length(self@features) > 0) {
      checkmate::assert_true(("features" %in% is(self@features[[1]])) || ("featureGroups" %in% is(self@features[[1]])))
      checkmate::assert_list(self@filtered)
      checkmate::assert_true("MSPeakLists" %in% is(self@mspl[[1]]))
      checkmate::assert_true("formulas" %in% is(self@formulas[[1]]))
      checkmate::assert_true("compounds" %in% is(self@compounds[[1]])) 
    }
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, NTS) <- function(x) {
  cat("\n")
  cat(is(x))
  cat("\n")
  cat("  Number of analyses: ", x@number_analyses, "\n")
  if (x@has_groups) {
    cat("  Number of groups: ", x@number_features, "\n")
  } else {
    cat("  Number of features: ", x@number_features, "\n")
  }
  if (x$has_features) {
    cat("\n")
    print(x@features[[1]])
    cat("\n")
    cat("\n")
    cat("  Filtered features: ", "\n")
    print(vapply(x@filtered, function(z) nrow(z), 0))
    cat("\n")
    if (length(x@mspl[[1]]) > 0) {
      cat("\n")
      print(x@mspl[[1]])
      cat("\n")
    }
    if (length(x@formulas[[1]]) > 0) {
      cat("\n")
      print(x@formulas[[1]])
      cat("\n")
    }
    if (length(x@compounds[[1]]) > 0) {
      cat("\n")
      print(x@compounds[[1]])
      cat("\n")
    }
  }
}

#' @export
#' @noRd
S7::method(print, NTS) <- function(x, ...) {
  show(x)
}

#' @export
#' @noRd
S7::method(`[`, NTS) <- function(x, i, j) {
  
  if (!x$has_features) {
    warning("No features found to subset!")
    return(x)
  }
  
  if (missing(j)) {
    x@features[[1]] <- x@features[[1]][i]
    x@filtered <- x@filtered[i]
    if (length(x@mspl[[1]]) > 0) x@mspl[[1]] <- x@mspl[[1]][i]
    if (length(x@formulas[[1]]) > 0) {
      x@formulas[[1]] <- x@formulas[[1]][x$group_names]
      if ("featureFormulas" %in% slotNames(x@formulas[[1]])) {
        x@formulas[[1]]@featureFormulas <- x@formulas[[1]]@featureFormulas[i]
      }
    }
    if (length(x@compounds[[1]]) > 0) x@compounds[[1]] <- x@compounds[[1]][x$group_names]
    return(x)
  } else if (missing(i)) {
    x@features[[1]] <- x@features[[1]][, j]
    if (length(x@mspl[[1]]) > 0) x@mspl[[1]] <- x@mspl[[1]][, j]
    if (length(x@formulas[[1]]) > 0) x@formulas[[1]] <- x@formulas[[1]][j]
    if (length(x@compounds[[1]]) > 0) x@compounds[[1]] <- x@compounds[[1]][j]
    return(x)
  } else {
    x@features[[1]] <- x@features[[1]][i, j]
    x@filtered <- x@filtered[i]
    if (length(x@mspl[[1]]) > 0) x@mspl <- x@mspl[i, j]
    if (length(x@formulas[[1]]) > 0) {
      x@formulas <- x@formulas[[1]][j]
      if ("featureFormulas" %in% slotNames(x@formulas[[1]])) {
        x@formulas[[1]]@featureFormulas <- x@formulas[[1]]@featureFormulas[i]
      }
    }
    if (length(x@compounds[[1]]) > 0) x@compounds[[1]] <- x@compounds[[1]][j]
    return(x)
  }
}

#' @export
#' @noRd
S7::method(`[[`, NTS) <- function(x, i) {
  x@features[[1]] <- x@features[[1]][[i]]
  x@filtered <- x@filtered[[i]]
  if (length(x@mspl[[1]]) > 0) x@mspl[[1]] <- x@mspl[[1]][[i]]
  if (length(x@formulas[[1]]) > 0) {
    if ("featureFormulas" %in% slotNames(x@formulas[[1]])) {
      x@formulas[[1]]@featureFormulas <- x@formulas[[1]]@featureFormulas[i]
      x@formulas[[1]] <- x@formulas[[1]][x$group_names]
    }
  }
  if (length(x@compounds[[1]]) > 0) x@compounds[[1]] <- x@compounds[[1]][x$group_names]
  return(x)
}

#' @export
#' @noRd
S7::method(report, NTS) <- function(x,
                                    path = paste0(getwd(), "/report"),
                                    filtered = FALSE,
                                    settingsFile = system.file("report", "settings.yml", package = "patRoon"),
                                    eicRtWindow = 30,
                                    eicTopMost = 1,
                                    eicTopMostByRGroup = TRUE,
                                    eicOnlyPresent = TRUE,
                                    eicMzExpWindow = 0.001,
                                    adductPos = "[M+H]+",
                                    adductNeg = "[M-H]-",
                                    specSimMethod = "cosine",
                                    specSimRemovePrecursor = FALSE,
                                    specSimMzWeight = 0,
                                    specSimIntWeight = 1,
                                    specSimAbsMzDev = 0.005,
                                    specSimRelMinIntensity = 0.05,
                                    specSimMinPeaks = 1,
                                    specSimShift = "none",
                                    specSimCombineMethod = "mean",
                                    clearPath = FALSE,
                                    openReport = TRUE,
                                    parallel = TRUE) {
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found!")
    return(NULL)
  }

  if (!x$has_groups) {
    warning("No feature groups found to report!")
    return(NULL)
  }

  patRoon::report(
    x$features[[1]],
    x$mspl[[1]],
    formulas = x$formulas[[1]],
    compounds = x$compounds[[1]],
    compsCluster = NULL,
    components = NULL,
    TPs = NULL,
    settingsFile = settingsFile,
    path = path,
    EICParams = list(
      rtWindow = eicRtWindow,
      topMost = eicTopMost,
      topMostByRGroup = eicTopMostByRGroup,
      onlyPresent = eicOnlyPresent,
      mzExpWindow = eicMzExpWindow,
      setsAdductPos = adductPos,
      setsAdductNeg = adductNeg
    ),
    specSimParams = list(
      method = specSimMethod,
      removePrecursor = specSimRemovePrecursor,
      mzWeight = specSimMzWeight,
      intWeight = specSimIntWeight,
      absMzDev = specSimAbsMzDev,
      relMinIntensity = specSimRelMinIntensity,
      minPeaks = specSimMinPeaks,
      shift = specSimShift,
      setCombineMethod = specSimCombineMethod
    ),
    clearPath = clearPath,
    openReport = openReport,
    parallel = parallel,
    overrideSettings = list()
  )

  message("\U2713 Report generated!")
}

# Utility functions -----

#' @noRd
.add_features_column <- function(nts = NULL, name = NULL, data = NULL) {
  if (!is(nts, "StreamFind::NTS")) {
    warning("NTS object is not of class NTS! Not done.")
    return(nts)
  }
  if (nts@number_features > 0) {
    feature_list <- nts@feature_list
    feature_list <- Map(function(x, y) {
      if (nrow(x) == length(y)) x[[name]] <- y
      x
    }, feature_list, data)
    nts$feature_list <- feature_list
  } else {
    warning("No features found! Not done.")
  }
  nts
}
