#' @export
#' @noRd
NTS <- S7::new_class("NTS", package = "StreamFind", parent = Results,
  
  properties = list(
    
    features = S7::new_property(S7::as_class(methods::getClassDef("workflowStep", package = "patRoon"))),
    
    filtered = S7::new_property(S7::class_list, default = list()),
    
    mspl = S7::new_property(S7::as_class(methods::getClassDef("workflowStep", package = "patRoon"))),
    
    formulas = S7::new_property(S7::as_class(methods::getClassDef("formulas", package = "patRoon"))),
    
    compounds = S7::new_property(S7::as_class(methods::getClassDef("compounds", package = "patRoon"))),
    
    analysisInfo = S7::new_property(S7::class_data.frame, getter = function(self) self@features@analysisInfo),
    
    number_analyses = S7::new_property(S7::class_integer, getter = function(self) length(patRoon::analyses(self@features))),
    
    number_features = S7::new_property(S7::class_integer, getter = function(self) length(self@features)),
    
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
    )
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
      engine = "MassSpecEngine",
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
      checkmate::test_character(self@engine, len = 1),
      checkmate::test_true(self@engine == "MassSpecEngine"),
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
S7::method(`[`, NTS) <- function(x, i) {
  if (x@length > 0) {
    x@features[i]
    x@filtered[i]
    if (length(x@mspl) > 0) x@mspl[i]
    if (length(x@formulas) > 0) x@formulas[i]
    if (length(x@compounds) > 0) x@compounds[i]
  }
  x
}

#' @export
#' @noRd
S7::method(`[[`, NTS) <- function(x, i) {
  
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
