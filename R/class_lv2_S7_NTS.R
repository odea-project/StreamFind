#' @export
#' @noRd
NTS <- S7::new_class("NTS", package = "StreamFind", parent = Results,
  
  properties = list(
    
    ## __features -----
    features = S7::new_property(S7::as_class(methods::getClassDef("workflowStep", package = "patRoon"))),
    
    ## __filtered -----
    filtered = S7::new_property(S7::class_list, default = list()),
    
    ## __mspl -----
    mspl = S7::new_property(S7::as_class(methods::getClassDef("workflowStep", package = "patRoon"))),
    
    ## __formulas -----
    formulas = S7::new_property(S7::as_class(methods::getClassDef("formulas", package = "patRoon"))),
    
    ## __compounds -----
    compounds = S7::new_property(S7::as_class(methods::getClassDef("compounds", package = "patRoon"))),
    
    ## __analysisInfo -----
    analysisInfo = S7::new_property(S7::class_data.frame, getter = function(self) self@features@analysisInfo),
    
    ## __number_analyses -----
    number_analyses = S7::new_property(S7::class_integer, getter = function(self) length(patRoon::analyses(self@features))),
    
    ## __number_features -----
    number_features = S7::new_property(S7::class_integer, getter = function(self) length(self@features)),
    
    ## __feature_list -----
    feature_list = S7::new_property(S7::class_list,
      
      getter = function(self) {
        if ("featureGroups" %in% is(self@features)) {
          pat <- self@features@features
        } else {
          pat <- self@features
        }
        
        f_list <- pat@features
        filtered_list <- self$filtered
        f_list <- Map(
          function(x, y) {
            if (nrow(x) > 0) x <- x[!(x$ID %in% y$ID), ]
            y <- rbindlist(list(y, x), fill = TRUE)
            y
          },
          filtered_list, f_list
        )
        
        f_list <- lapply(f_list, function(x) {
          z <- copy(x)
          data.table::setnames(z, "ID", "feature", skip_absent = TRUE)
          data.table::setnames(z, "ret", "rt", skip_absent = TRUE)
          data.table::setnames(z, "retmin", "rtmin", skip_absent = TRUE)
          data.table::setnames(z, "retmax", "rtmax", skip_absent = TRUE)
        })
        
        if ("featuresSet" %in% is(pat)) {
          for (x in names(f_list)) {
            pol <- pat@analysisInfo[[x]]$set
            if ("positive" %in% pol) adduct_val <- -1.007276
            if ("negative" %in% pol) adduct_val <- 1.007276
            sel_to_change <- round(f_list[[x]]$mz, 0) == round(f_list[[x]]$mass, 0)
            f_list[[x]]$mz[sel_to_change] <- f_list[[x]]$mz - adduct_val
            f_list[[x]]$mzmin[sel_to_change] <- f_list[[x]]$mzmin - adduct_val
            f_list[[x]]$mzmax[sel_to_change] <- f_list[[x]]$mzmax - adduct_val
          }
        }
        
        f_list
      },
      
      setter = function(self, value) {
        if (!is.list(value)) {
          warning("The argument value must be a list!")
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
        
        if ("features" %in% is(self$features)) {
          if (identical(patRoon::analyses(self@features), names(value))) {
            if ("featuresSet" %in% is(self$features)) {
              for (x in names(value)) {
                pol <- self$features@analysisInfo[[x]]$set
                if ("positive" %in% pol) adduct_val <- -1.007276
                if ("negative" %in% pol) adduct_val <- 1.007276
                sel_to_change <- round(value[[x]]$mz, 0) != round(value[[x]]$mass, 0)
                value[[x]]$mz[sel_to_change] <- value[[x]]$mz + adduct_val
                value[[x]]$mzmin[sel_to_change] <- value[[x]]$mzmin + adduct_val
                value[[x]]$mzmax[sel_to_change] <- value[[x]]$mzmax + adduct_val
              }
            }
            self@features@features <- lapply(value, function(x) x[!x$filtered, ])
            self@filtered <- lapply(value, function(x) x[x$filtered, ])
            self
          } else {
            warning("Feature list names not matching analysis names! Not done.")
            return(self)
          }
        } else if ("featureGroups" %in% is(self$features)) {
          if (identical(patRoon::analyses(self@features), names(value))) {
            if (all(vapply(value, function(x) "group" %in% colnames(x), FALSE))) {
              self@features@features@features <- value
              fg <- self$features
              fg_left <- unique(unlist(lapply(value, function(x) x$group[!x$filtered])))
              fg_left <- fg_left[!is.na(fg_left)]
              if (length(fg_left) > 0) {
                fg <- fg[, fg_left]
                self@features <- fg
              }
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
    
    ## __has_features -----
    has_features = S7::new_property(S7::class_logical, getter = function(self) self@number_features > 0),
    
    ## __has_groups -----
    has_groups = S7::new_property(S7::class_logical, getter = function(self) {
      if ("featureGroups" %in% is(self@features)) {
        if (length(self@features) > 0) return(TRUE)
        return(FALSE)
      }
      FALSE
    }),
    
    ## __has_features_ms1 -----
    has_features_ms1 = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@number_features > 0) {
        any(vapply(self$feature_list, function(x) {
          if ("ms2" %in% colnames(x)) {
            any(vapply(x$ms2, is.data.frame, FALSE))
          } else {
            FALSE
          }
        }, FALSE))
      }
      FALSE
    }),
    
    ## __has_features_ms2 -----
    has_features_ms2 = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@number_features > 0) {
        any(vapply(self$feature_list, function(x) {
          if ("ms2" %in% colnames(x)) {
            any(vapply(x$ms2, is.data.frame, FALSE))
          } else {
            FALSE
          }
        }, FALSE))
      }
      FALSE
    }),
    
    ## __has_features_eic -----
    has_features_eic = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@number_features > 0) {
        any(vapply(self$feature_list, function(x) {
          if ("eic" %in% colnames(x)) {
            any(vapply(x$eic, is.data.frame, FALSE))
          } else {
            FALSE
          }
        }, FALSE))
      }
      FALSE
    }),
    
    ## __has_features_suspects -----
    has_features_suspects = S7::new_property(S7::class_logical, getter = function(self) {
      if (self@number_features > 0) {
        any(vapply(self$feature_list, function(x) {
          if ("suspects" %in% colnames(x)) {
            any(vapply(x$suspects, is.data.frame, FALSE))
          } else {
            FALSE
          }
        }, FALSE))
      }
      FALSE
    })
  ),
  
  constructor = function(
    features = new("featuresOpenMS"),
    filtered = list(),
    mspl = new("MSPeakLists", algorithm = NA_character_),
    formulas = new("formulas", algorithm = NA_character_),
    compounds = new("compounds", algorithm = NA_character_)) {
    
    if (!requireNamespace("patRoon", quietly = TRUE)) {
      warning("patRoon package not found! Install it for finding features.")
      return(NULL)
    }
    
    if (!("features" %in% is(features) || "featureGroups" %in% is(features))) {
      warning("The argument features must be of class features or featureGroups from patRoon")
      return(NULL)
    }
    
    S7::new_object(
      Results(), 
      name = "NTS",
      software = "patRoon",
      version = as.character(packageVersion("patRoon")),
      features = features,
      filtered = filtered,
      mspl = mspl,
      formulas = formulas,
      compounds = compounds
    )
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_true(self@name == "NTS"),
      checkmate::test_true(self@software == "patRoon"),
      checkmate::test_character(self@version, len = 1),
      checkmate::test_true(("features" %in% is(self@features)) || ("featureGroups" %in% is(self@features))),
      checkmate::test_list(self@filtered),
      checkmate::test_true("MSPeakLists" %in% is(self@mspl)),
      checkmate::test_true("formulas" %in% is(self@formulas)),
      checkmate::test_true("compounds" %in% is(self@compounds)),
      if (length(self@mspl) > 0) checkmate::test_true(length(self@mspl) == self@length),
      if (length(self@formulas) > 0) checkmate::test_true(length(self@formulas) == self@length),
      if (length(self@compounds) > 0) checkmate::test_true(length(self@compounds) == self@length)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, NTS) <- function(x) {
  
  cat("\n")
  cat("NTS\n")
  cat("\n")
  cat("  Number of analyses: ", x@number_analyses, "\n")
  if (x@has_groups) {
    cat("  Number of groups: ", x@number_features, "\n")
  } else {
    cat("  Number of features: ", x@number_features, "\n")
  }
  cat("\n")
  print(x@features)
  cat("\n")
  
  cat("\n")
  cat("  Filtered features: ", "\n")
  print(vapply(x@filtered, function(z) nrow(z), 0))
  cat("\n")
  
  if (length(x@mspl) > 0) {
    cat("\n")
    print(x@mspl)
    cat("\n")
  }
  
  if (length(x@formulas) > 0) {
    cat("\n")
    print(x@formulas)
    cat("\n")
  }
  
  if (length(x@compounds) > 0) {
    cat("\n")
    print(x@compounds)
    cat("\n")
  }
}

#' @export
#' @noRd
S7::method(print, NTS) <- function(x, ...) {
  show(x)
}

#' @export
#' @noRd
S7::method(`$`, NTS) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`$<-`, NTS) <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}

#' @export
#' @noRd
S7::method(`[`, NTS) <- function(x, i, j) {
  if (missing(j)) {
    x@features <- x@features[i]
    x@filtered <- x@filtered[i]
    if (length(x@mspl) > 0) x@mspl <- x@mspl[i]
    if (length(x@formulas) > 0) x@formulas <- x@formulas[i]
    if (length(x@compounds) > 0) x@compounds <- x@compounds[i]
    return(x)
  } else if (missing(i)) {
    x@features <- x@features[, j]
    if (length(x@mspl) > 0) x@mspl <- x@mspl[, j]
    if (length(x@formulas) > 0) x@formulas <- x@formulas[, j]
    if (length(x@compounds) > 0) x@compounds <- x@compounds[, j]
    return(x)
  } else {
    x@features <- x@features[i, j]
    x@filtered <- x@filtered[i]
    if (length(x@mspl) > 0) x@mspl <- x@mspl[i, j]
    if (length(x@formulas) > 0) x@formulas <- x@formulas[i, j]
    if (length(x@compounds) > 0) x@compounds <- x@compounds[i, j]
    return(x)
  }
}

#' @export
#' @noRd
S7::method(`[[`, NTS) <- function(x, i) {
  x@features <- x@features[[i]]
  x@filtered <- x@filtered[[i]]
  if (length(x@mspl) > 0) x@mspl <- x@mspl[[i]]
  if (length(x@formulas) > 0) x@formulas <- x@formulas[[i]]
  if (length(x@compounds) > 0) x@compounds <- x@compounds[[i]]
  x
}

#' Adds an extra column to data.frame object in features.
#' @noRd
.add_features_column = function(nts = NULL, name = NULL, data = NULL) {
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
