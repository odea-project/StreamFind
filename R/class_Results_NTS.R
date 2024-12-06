#' @export
#' @noRd
NTS <- S7::new_class("NTS",
  package = "StreamFind", parent = Results,
  properties = list(

    # MARK: feature_properties
    ## __feature_properties -----
    feature_properties = S7::new_property(S7::class_character, default = NA_character_),

    # MARK: analyses_info
    ## __analyses_info -----
    analyses_info = S7::new_property(S7::class_data.frame, default = data.table::data.table()),

    # MARK: number_analyses
    ## __number_analyses -----
    number_analyses = S7::new_property(S7::class_integer,
      getter = function(self) {
        nrow(self@analyses_info)
      }
    ),

    # MARK: feature_list
    ## __feature_list -----
    feature_list = S7::new_property(S7::class_list, default = list()),

    # MARK: number_features
    ## __number_features -----
    number_features = S7::new_property(S7::class_integer,
      getter = function(self) {
        if (length(self@feature_list) > 0) {
          vapply(self@feature_list, function(x) {
            if (nrow(x) == 0) return(0)
            nrow(x[!x$filtered, ])
          }, 0)
        } else {
          0
        }
      }
    ),

    # MARK: has_features
    ## __has_features -----
    has_features = S7::new_property(S7::class_logical, getter = function(self) any(self@number_features > 0)),

    # MARK: has_filtered_features
    ## __has_filtered_features -----
    has_filtered_features = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          any(vapply(self@feature_list, function(x) any(x$filtered), FALSE))
        } else {
          FALSE
        }
      }
    ),

    # MARK: number_filtered_features
    ## __number_filtered_features -----
    number_filtered_features = S7::new_property(
      S7::class_integer,
      getter = function(self) {
        if (self@has_features) {
          vapply(self@feature_list, function(x) sum(x$filtered), 0)
        } else {
          0
        }
      }
    ),
    
    # MARK: has_groups
    ## __has_groups -----
    has_groups = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          any(vapply(self@feature_list, function(x) any(!is.na(x$group)), FALSE))
        } else {
          FALSE
        }
      }
    ),
    
    # MARK: number_groups
    ## __number_groups -----
    number_groups = S7::new_property(S7::class_integer,
      getter = function(self) {
        if (self@has_groups) {
          vapply(self@feature_list, function(x) {
            if (nrow(x) == 0) return(0)
            length(x$group[!x$filtered & !is.na(x$group)])
          }, 0)
        } else {
          0
        }
      }
    ),
    
    # MARK: group_names
    ## __group_names -----
    group_names = S7::new_property(S7::class_character,
      getter = function(self) {
        if (self@has_groups) {
          return(unique(unlist(lapply(self@feature_list, function(x) ifelse(nrow(x) > 0, unique(x$group), 0)))))
        }
        NA_character_
      }
    ),
    
    # MARK: has_features_ms1
    ## __has_features_ms1 -----
    has_features_ms1 = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@has_features) {
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
      if (self@has_features) {
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
      if (self@has_features) {
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
      if (self@has_features) {
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
  
  # MARK: constructor
  ## __constructor -----
  constructor = function(analyses_info = data.table::data.table(), feature_list = list()) {
    fp <- c(
      "feature", "rt", "mz", "area", "intensity", "rtmin", "rtmax", "mzmin", "mzmax", "mass", "polarity", "adduct",
      "filtered", "filter", "filled", "group", "quality", "annotation", "istd", "ms1", "ms2", "eic", "suspects",
      "formulas", "compounds"
    )
    
    S7::new_object(
      Results(),
      name = "nts",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      feature_properties = fp,
      analyses_info = analyses_info,
      feature_list = feature_list
    )
  },
  
  # MARK: validator
  ## __validator -----
  validator = function(self) {
    checkmate::assert_true(self@name == "nts")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_character(self@version, len = 1)
    if (length(self@has_features) > 0) {
      checkmate::assert_true(identical(self$analyses_info$analysis, names(self@feature_list)))
      for (x in self@feature_list) {
        checkmate::assert_data_table(x)
        checkmate::assert_true(all(self@feature_properties %in% colnames(x)))
      }
    }
    NULL
  }
)

# MARK: show
## __show -----
#' @export
#' @noRd
S7::method(show, NTS) <- function(x) {
  cat("\n")
  cat(is(x))
  cat("\n")
  if (!x$has_features) {
    cat("No features found!")
    return()
  }
  info <- data.table::data.table(
    "analysis" = x@analyses_info$analysis,
    "features" = x@number_features,
    "filtered" = x@number_filtered_features,
    "groups" = x@number_groups
  )
  print(info)
}

# MARK: `[`
## __`[` -----
#' @export
#' @noRd
S7::method(`[`, NTS) <- function(x, i, j) {
  if (!x$has_features) {
    warning("No features found to subset!")
    return(x)
  }
  
  if (!missing(j)) {
    x@analyses_info <- x@analyses_info[i, ]
    x@feature_list <- x@feature_list[i]
    return(x)
  }
  
  if (!missing(j)) {
    if (!x$has_groups) {
      warning("No feature groups found to subset!")
    } else {
      x@feature_list <- lapply(x@feature_list, function(z) {
        z <- z[z$group %in% j, ]
        z
      })
    }
  }
  
  x
}

# MARK: `[[`
## __`[[` -----
#' @export
#' @noRd
S7::method(`[[`, NTS) <- function(x, i) {
  if (!missing(j)) {
    if (!x$has_groups) {
      warning("No feature groups found to subset!")
    } else if (length(i) == 1) {
      x@feature_list <- lapply(x@feature_list, function(z) {
        z <- z[z$group %in% j, ]
        z
      })
    } else {
      warning("Only one group can be selected!")
    }
  }
  x
}

# MARK: get_patRoon_features
## __get_patRoon_features -----
#' @export
#' @noRd
S7::method(get_patRoon_features, NTS) <- function(x, filtered = FALSE, featureGroups = TRUE) {
  if (!x$has_features) {
    warning("No features found to get!")
    return(NULL)
  }
  
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  
  feature_list <- x$feature_list
  
  feature_list <- lapply(feature_list, function(z) {
    if (!filtered) z <- z[!z$filtered, ]
    data.table::setnames(z, "feature", "ID", skip_absent = TRUE)
    data.table::setnames(z, "rt", "ret", skip_absent = TRUE)
    data.table::setnames(z, "rtmin", "retmin", skip_absent = TRUE)
    data.table::setnames(z, "rtmax", "retmax", skip_absent = TRUE)
    z
  })
  
  feature_list <- feature_list[vapply(feature_list, nrow, 0) > 0]
  
  ana_info <- x$analyses_info
  ana_info <- ana_info[ana_info$analysis %in% names(feature_list), ]
  pols <- x$analyses_info$polarity
  ana_info$path <- dirname(ana_info$file)
  data.table::setnames(ana_info, "replicate", "group", skip_absent = TRUE)
  data.table::setcolorder(ana_info, c("path", "analysis", "group", "blank", "polarity"))
  
  make_set <- FALSE
  
  if (length(unique(pols)) > 1) {
    make_set <- TRUE
  }
  
  if (x$has_groups && featureGroups) {
    
    feature_list <- lapply(feature_list, function(z) {
      z <- z[!is.na(z$group), ]
      z$index <- seq_len(nrow(z))
      z
    })
    
    pat <- new("featuresOpenMS", features = feature_list, analysisInfo = ana_info)
    
    pat@features <- lapply(pat@features, function(z) {
      if ("name" %in% colnames(z)) z$name <- NULL
      if ("index" %in% colnames(z)) z$index <- NULL
      z
    })
    
    if (make_set) {
      pat_set <- patRoon::makeSet(
        pat[pols %in% "negative"],
        pat[pols %in% "positive"],
        adducts = list("[M-H]-", "[M+H]+")
      )
    }
    
    fts <- data.table::rbindlist(feature_list, idcol = "analysis")
    
    groups <- fts[, .(intensity = intensity), by = c("group", "analysis")]
    groups <- data.table::dcast(groups, analysis ~ group, value.var = "intensity", fill = 0)
    if (make_set) {
      groups <- groups[match(pat_set@analysisInfo$analysis, groups$analysis), ]
    } else {
      groups <- groups[match(pat@analysisInfo$analysis, groups$analysis), ]
    }
    groups$analysis <- NULL
    
    ftindex <- fts[, .(index = index), by = c("group", "analysis")]
    ftindex <- data.table::dcast(ftindex, analysis ~ group, value.var = "index", fill = 0)
    if (make_set) {
      ftindex <- ftindex[match(pat_set@analysisInfo$analysis, ftindex$analysis), ]
    } else {
      ftindex <- ftindex[match(pat@analysisInfo$analysis, ftindex$analysis), ]
    }
    ftindex$analysis <- NULL
    
    groups_info <- fts[, .(mass = round(mean(mass), digits = 4), ret = round(mean(ret), digits = 0)), by = c("group")]
    groups_info_rows <- groups_info$group
    groups_info[["group"]] <- NULL
    groups_info <- as.data.frame(groups_info)
    rownames(groups_info) <- groups_info_rows
    colnames(groups_info) <- c("mzs", "rts") # Note that here the mzs is still neutral mass
    
    data.table::setcolorder(groups, groups_info_rows)
    data.table::setcolorder(ftindex, groups_info_rows)
    
    if (make_set) {
      fg <- new(
        "featureGroupsOpenMS",
        groups = groups,
        analysisInfo = pat_set@analysisInfo,
        groupInfo = groups_info,
        features = pat_set,
        ftindex = ftindex
      )
      
      fg_set <- patRoon::featureGroupsSet(
        groupAlgo = "openms",
        groupArgs = list(),
        groupVerbose = FALSE,
        groups = patRoon::groupTable(fg),
        groupInfo = patRoon::groupInfo(fg),
        analysisInfo = patRoon::analysisInfo(fg),
        features = patRoon::getFeatures(fg),
        ftindex = patRoon::groupFeatIndex(fg),
        algorithm = "openms-set"
      )
      
      fg_set@annotations <- patRoon:::getAnnotationsFromSetFeatures(fg_set)

      return(fg_set)
      
    } else {
      if (unique(pols) %in% "positive") {
        groups_info$mzs <- groups_info$mzs + 1.007276
      } else if (unique(pols) %in% "negative") {
        groups_info$mzs <- groups_info$mzs - 1.007276
      } else {
        stop("Polarity should be defined as positive or negative!")
      }
      
      fg <- new(
        "featureGroupsOpenMS",
        groups = groups,
        analysisInfo = pat@analysisInfo,
        groupInfo = groups_info,
        features = pat,
        ftindex = ftindex,
        groupAlgo = "openms"
      )
      
      return(fg)
    }
  } else {
    pat <- new("featuresOpenMS", features = feature_list, analysisInfo = ana_info)
    
    if (make_set) {
      pat <- patRoon::makeSet(
        pat[pols %in% "negative"],
        pat[pols %in% "positive"],
        adducts = list("[M-H]-", "[M+H]+")
      )
      
      pat@analysisInfo <- pat@analysisInfo[order(pat@analysisInfo$analysis), ]
      
      pat@features <- pat@features[pat@analysisInfo$analysis]
    }
    
    pat@features <- lapply(pat@features, function(z) {
      if ("name" %in% colnames(z)) z$name <- NULL
      if ("index" %in% colnames(z)) z$index <- NULL
      z
    })
    
    return(pat)
  }
}

# MARK: get_patRoon_MSPeakLists
## __get_patRoon_MSPeakLists -----
#' @export
#' @noRd
S7::method(get_patRoon_MSPeakLists, NTS) <- function(x,
                                                     clusterMzWindow = 0.005,
                                                     topMost = 100,
                                                     minIntensityPre = 50,
                                                     minIntensityPost = 50,
                                                     avgFun = "mean",
                                                     method = "distance") {
  if (!x$has_features) {
    warning("No features found to get!")
    return(NULL)
  }
  
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  
  parameters <- list(
    clusterMzWindow = clusterMzWindow,
    topMost = topMost,
    minIntensityPre = minIntensityPre,
    minIntensityPost = minIntensityPost,
    avgFun = avgFun,
    method = method
  )
  
  parameters$avgFun <- get(parameters$avgFun)
  
  mspl <- .convert_ms1_ms2_columns_to_MSPeakLists(x, parameters)
  
  mspl
}

# MARK: report
## __report -----
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

  pat <- x$get_patRoon_features(filtered = filtered, featureGroups = TRUE)

  patRoon::report(
    x$features[[1]],
    MSPeakLists = NULL,
    formulas = NULL,
    compounds = NULL,
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

# MARK: .add_features_column
## __.add_features_column -----
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
