
#' @noRd
MassSpecAnalyses <- S7::new_class("MassSpecAnalyses", package = "StreamFind", parent = Analyses,

  properties = list(
    
    ## __analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),
    
    ## __names -----
    names = S7::new_property(S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) x$name, NA_character_)
      }
    ),
    
    ## __replicates -----
    replicates = S7::new_property(S7::class_character,
      getter = function(self) vapply(self@analyses, function(x) x$replicate, NA_character_),
      setter = function(self, value) {
        if (length(value) != length(self)) {
          warning("Length of replicates not conform!")
          return(self)
        }
        if (!is.character(value)) {
          warning("Replicates must be character!")
          return(self)
        }
        for (i in seq_len(length(self))) self@analyses[[i]]$replicate <- value[i]
        
        if (self$has_nts) {
          if (self@results$nts$has_features) {
            self@results$nts$features@analysisInfo$group <- value
            if (self@results$nts$has_groups) {
              self@results$nts$features@features@analysisInfo$group <- value
            }
          }
        }
        
        self
      }
    ),
    
    ## __blanks -----
    blanks = S7::new_property(S7::class_character,
      getter = function(self) vapply(self@analyses, function(x) x$blank, NA_character_),
      setter = function(self, value) {
        if (length(value) != length(self)) {
          warning("Length of blanks not conform!")
          return(self)
        }
        if (!is.character(value)) {
          warning("Blanks must be character!")
          return(self)
        }
        if (!all(value %in% self@replicates)) {
          warning("Blank names must be in replicate names!")
          return(self)
        }
        for (i in seq_len(length(self))) self@analyses[[i]]$blank <- value[i]
        if (self$has_nts) {
          if (self@results$nts$has_features) {
            self@results$nts$features@analysisInfo$blank <- value
            if (self@results$nts$has_groups) {
              self@results$nts$features@features@analysisInfo$blank <- value
            }
          }
        }
        self
      }
    ),
    
    ## __types -----
    types = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$type, NA_character_)
    }),
    
    ## __files -----
    files = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$file, NA_character_)
    }),
    
    ## __formats -----
    formats = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$format, NA_character_)
    }),
    
    ## __instruments -----
    instruments = S7::new_property(S7::class_character, getter = function(self) {
      lapply(self@analyses, function(x) x$instrument)
    }),
    
    ## __software -----
    software = S7::new_property(S7::class_character, getter = function(self) {
      lapply(self@analyses, function(x) x$software)
    }),
    
    ## __spectra_number -----
    spectra_number = S7::new_property(S7::class_numeric, getter = function(self) {
      vapply(self@analyses, function(x) x$spectra_number, 0)
    }),
    
    ## __spectra_headers -----
    spectra_headers = S7::new_property(S7::class_list, getter = function(self) {
      lapply(self@analyses, function(x) {
        res <- x$spectra_headers
        res$replicate <- x$replicate
        data.table::setcolorder(res, c("replicate"))
        res
      })
    }),
    
    ## __spectra_mode -----
    spectra_mode = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) {
        if (x$spectra_number == 0) return(NA_character_)
        mode <- unique(x$spectra_headers$mode)
        mode[mode == 0] <- "unknown"
        mode[mode == 1] <- "profile"
        mode[mode == 2] <- "centroid"
        if (length(mode) > 1) mode <- paste(mode, collapse = ", ")
        mode
      }, NA_character_)
    }),
    
    ## __spectra_level -----
    spectra_level = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) {
        if (x$spectra_number == 0) return(NA_character_)
        levels <- unique(x$spectra_headers$level)
        if (length(levels) > 1) levels <- paste(levels, collapse = ", ")
        levels
      }, NA_character_)
    }),
    
    ## __spectra_lowest_mz -----
    spectra_lowest_mz = S7::new_property(S7::class_numeric, getter = function(self) {
      vapply(self@analyses, function(x) min(x$spectra_headers$lowmz), NA_real_)
    }),
    
    ## __spectra_highest_mz -----
    spectra_highest_mz = S7::new_property(S7::class_numeric, getter = function(self) {
      vapply(self@analyses, function(x) max(x$spectra_headers$highmz), NA_real_)
    }),
    
    ## __spectra_lowest_rt -----
    spectra_lowest_rt = S7::new_property(S7::class_numeric, getter = function(self) {
      vapply(self@analyses, function(x) min(x$spectra_headers$rt), NA_real_)
    }),
    
    ## __spectra_highest_rt -----
    spectra_highest_rt = S7::new_property(S7::class_numeric, getter = function(self) {
      vapply(self@analyses, function(x) max(x$spectra_headers$rt), NA_real_)
    }),
    
    ## __spectra_highest_mobility -----
    spectra_highest_mobility = S7::new_property(S7::class_numeric, getter = function(self) {
      vapply(self@analyses, function(x) max(x$spectra_headers$mobility), NA_real_)
    }),
    
    ## __spectra_lowest_mobility -----
    spectra_lowest_mobility = S7::new_property(S7::class_numeric, getter = function(self) {
      vapply(self@analyses, function(x) min(x$spectra_headers$mobility), NA_real_)
    }),
    
    ## __spectra_polarity -----
    spectra_polarity = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) {
        if (x$spectra_number == 0) return(NA_character_)
        polarity <- unique(x$spectra_headers$polarity)
        
        if (length(polarity) > 1) {
          polarities <- x$spectra_headers$polarity
          scans_pos <- length(polarities[polarities == 1])
          scans_neg <- length(polarities[polarities == -1])
          ratio <- scans_pos/scans_neg
          if (ratio < 1.2 & ratio > 0.8) {
            warning("Multiple polarities detected! Currently, find_features algorithms cannot handled multiple polarities properly.")
            return(NA_character_)
          } else if (ratio > 1.2) {
            per_pos_pol <- round((scans_pos / nrow(x$spectra_headers)) * 100, digits = 0)
            polarity <- 1
          } else {
            per_neg_pol <- round((scans_neg / nrow(x$spectra_headers)) * 100, digits = 0)
            polarity <- -1
          }
        }
        
        polarity[polarity == 0] <- "unkown"
        polarity[polarity == 1] <- "positive"
        polarity[polarity == -1] <- "negative"
        if (length(polarity) > 1) polarity <- paste(polarity, collapse = ", ")
        polarity
      }, NA_character_)
    }),
    
    ## __spectra_tic -----
    spectra_tic = S7::new_property(S7::class_numeric, getter = function(self) {
      lapply(self@analyses, function(x) {
        res <- x$spectra_headers[, c("polarity", "level", "rt", "tic"), with = FALSE] 
        res$replicate <- x$replicate
        colnames(res) <- c("polarity", "level", "rt", "intensity", "replicate")
        data.table::setcolorder(res, "replicate")
        res
      })
    }),
    
    ## __spectra_bpc -----
    spectra_bpc = S7::new_property(S7::class_numeric, getter = function(self) {
      lapply(self@analyses, function(x) {
        res <- x$spectra_headers[, c("polarity", "level", "rt", "bpmz", "bpint"), with = FALSE]
        res$replicate <- x$replicate
        colnames(res) <- c("polarity", "level", "rt", "mz", "intensity", "replicate")
        data.table::setcolorder(res, "replicate")
        res
      })
    }),
    
    ## __spectra_raw -----
    spectra_raw = S7::new_property(S7::class_list, getter = function(self) {
      Spectra(
        lapply(self@analyses, function(x) {
          if (nrow(x$spectra) > 0) {
            x$spectra
          } else {
            levels <- unique(self$spectra_level)
            levels <- as.numeric(unlist(strsplit(levels, ", ")))
            targets <- MassSpecTargets()@targets
            rcpp_parse_ms_spectra(x, levels, targets, 0, 0)
          }
        })
      )
    }),
    
    ## __chromatograms_number -----
    chromatograms_number = S7::new_property(S7::class_numeric, getter = function(self) {
      vapply(self@analyses, function(x) x$chromatograms_number, 0)
    }),
    
    ## __chromatograms_headers -----
    chromatograms_headers = S7::new_property(S7::class_data.frame, getter = function(self) {
      lapply(self@analyses, function(x) {
        res <- x$chromatograms_headers
        res$replicate <- x$replicate
        data.table::setcolorder(res, c("replicate"))
        res
      })
    }),
    
    ## chromatograms_raw -----
    chromatograms_raw = S7::new_property(S7::class_list, getter = function(self) {
      Chromatograms(
        lapply(self@analyses, function(x) {
          if (nrow(x$chromatograms) > 0) {
            x$chromatograms
          } else {
            levels <- unique(x$spectra_level)
            # levels <- as.numeric(unlist(strsplit(levels, ", ")))
            targets <- MassSpecTargets()@targets
            rcpp_parse_ms_chromatograms(x, idx = seq_along(x$number_chromatograms))
          }
        })
      )
    }),
    
    ## __info -----
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (length(self) > 0) {
        pols <- self@spectra_polarity
        pols <- unlist(pols)
        
        df <- data.table::data.table(
          "analysis" = vapply(self@analyses, function(x) x$name, ""),
          "replicate" = vapply(self@analyses, function(x) x$replicate, ""),
          "blank" = vapply(self@analyses, function(x) x$blank, ""),
          "type" = vapply(self@analyses, function(x) x$type, ""),
          "polarity" = pols,
          "spectra" = vapply(self@analyses, function(x) round(x$spectra_number, digits = 0), 0),
          "chromatograms" = vapply(self@analyses, function(x) round(x$chromatograms_number, digits = 0), 0)
        )
        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    }),
    
    ## __has_ion_mobility -----
    has_ion_mobility = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      vapply(self@analyses, function(x) any(x$spectra_headers$mobility > 0), FALSE)
    }),
    
    ## __has_loaded_spectra -----
    has_loaded_spectra = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      vapply(self@analyses, function(x) nrow(x$spectra) > 0, FALSE)
    }),
    
    ## __has_loaded_chromatograms -----
    has_loaded_chromatograms = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      vapply(self@analyses, function(x) nrow(x$chromatograms) > 0, FALSE)
    }),
    
    ## __has_nts -----
    has_nts = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["nts"]])) return(FALSE)
      if (!is(self@results[["nts"]], "StreamFind::NTS")) return(FALSE)
      TRUE
    }),
    
    ## __nts -----
    nts = S7::new_property(S7::class_list,
      getter = function(self) {
      if (self$has_nts) return(self@results[["nts"]])
      NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::NTS")) {
          if (value@number_analyses > 0) {
            analyses_names <- unname(names(self))
            value_analyses_names <- sort(value@features@analysisInfo$analysis)
            if (identical(analyses_names, value_analyses_names)) {
              self@results[["nts"]] <- value
            } else {
              # TODO check if some analyses are in engine and subset engine to match nts
              warning("Analysis names do not match! Not done.")
            }
          } else {
            warning("NTS results object is empty! Not done.")
          }
        } else {
          warning("Value must be an NTS results object! Not done.")
        }
        self
      }
    ),
    
    ## __has_spectra -----
    has_spectra = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["spectra"]])) return(FALSE)
      if (!is(self@results[["spectra"]], "StreamFind::Spectra")) return(FALSE)
      TRUE
    }),
    
    ## __spectra -----
    spectra = S7::new_property(S7::class_list,
      getter = function(self) {
        if (self$has_spectra) return(self@results[["spectra"]])
        if (any(self$spectra_number > 0)) return(Spectra(self$spectra_raw))
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::Spectra")) {
          if (!value$is_averaged) {
            analyses_names <- unname(names(self))
            value_analyses_names <- names(value$spectra)
            if (identical(analyses_names, value_analyses_names)) {
              self@results[["spectra"]] <- value
            } else {
              warning("Analysis names do not match! Not done.")
            }
          } else if (value$is_averaged) {
            replicate_names <- unname(self$replicates)
            value_analyses_names <- names(value$spectra)
            if (identical(replicate_names, value_analyses_names)) {
              self@results[["spectra"]] <- value
            } else {
              warning("Replicate names do not match! Not done.")
            }
          } else {
            warning("Spectra results object is not well defined! Not done.")
          }
        } else {
          warning("Value must be an Spectra results object! Not done.")
        }
        self
      }
    ),
    
    ## __has_chromatograms -----
    has_chromatograms = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["chromatograms"]])) return(FALSE)
      if (!is(self@results[["chromatograms"]], "StreamFind::Chromatograms")) return(FALSE)
      TRUE
    }),
    
    ## __chromatograms -----
    chromatograms = S7::new_property(S7::class_list,
      getter = function(self) {
        if (self$has_chromatograms) return(self@results[["chromatograms"]])
        if (any(self$chromatograms_number > 0)) return(Spectra(self$chromatograms_raw))
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::Chromatograms")) {
          if (!value$is_averaged) {
            analyses_names <- unname(names(self))
            value_analyses_names <- names(value$chromatograms)
            if (identical(analyses_names, value_analyses_names)) {
              self@results[["chromatograms"]] <- value
            } else {
              warning("Analysis names do not match! Not done.")
            }
          } else if (value$is_averaged) {
            replicate_names <- unname(self$replicates)
            value_analyses_names <- names(value$chromatograms)
            if (identical(replicate_names, value_analyses_names)) {
              self@results[["chromatograms"]] <- value
            } else {
              warning("Replicate names do not match! Not done.")
            }
          } else {
            warning("Chromatograms results object is not well defined! Not done.")
          }
        } else {
          warning("Value must be an Chromatograms results object! Not done.")
        }
      self
      }
    )
  ),

  constructor = function(files = NULL) {
    analyses <- .get_MassSpecAnalysis_from_files(files)
    S7::new_object(Analyses(), possible_formats = c(".mzML|.mzXML"), analyses = analyses)
  },

  validator = function(self) {
    valid <- all(
      checkmate::test_true(identical(self@possible_formats, c(".mzML|.mzXML"))),
      if (length(self) > 0) checkmate::test_true(identical(names(self@analyses), unname(self@names)))
    )
    if (!valid) return(FALSE)
    NULL
  }
)

# Methods -----


#' @noRd
S7::method(names,  MassSpecAnalyses) <- function(x) {
  vapply(x@analyses, function(x) x$name, NA_character_)
}


#' @noRd
S7::method(add, MassSpecAnalyses) <- function(x, value) {
  if (is.character(value)) {
    if (all(grepl(x@possible_formats, value))) {
      value <- .get_MassSpecAnalysis_from_files(value)
    } else {
      warning("File/s not valid!")
      return(x)
    }
  }
  if (!all(vapply(value, function(a) is(a, "MassSpecAnalysis"), FALSE))) {
    warning("Analysis/s not valid!")
    return(x)
  }
  if (any(vapply(value, function(a) a$name %in% x@names, FALSE))) {
    warning("Analysis names already exist!")
    return(x)
  }
  analyses <- c(x@analyses, value)
  analyses <- analyses[order(names(analyses))]
  if (length(analyses) > length(x@analyses)) {
    warning("All results removed!")
    x@results <- list()
  }
  x@analyses <- analyses
  x
}


#' @noRd
S7::method(remove, MassSpecAnalyses) <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!x$names %in% value]
    x@analyses <- x@analyses[order(names(x@analyses))]
    if (x@has_nts) x@results$nts <- x@results$nts[!x$names %in% value]
  } else if (is.numeric(value)) {
    x@analyses <- x@analyses[-value]
    x@analyses <- x@analyses[order(names(x@analyses))]
    if (x@has_nts) x@results$nts <- x@results$nts[-value]
  }
  x
}


#' @noRd
S7::method(`[`, MassSpecAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (x@has_nts) x@results$nts <- x@results$nts[i]
  x
}


#' @noRd
S7::method(`[<-`, MassSpecAnalyses) <- function(x, i, value) {
  x <- add(x, value)
  x
}


#' @noRd
S7::method(`[[`, MassSpecAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[[i]]
  if (x@has_nts) x@results$nts <- x@results$nts[[i]]
  x
}


#' @noRd
S7::method(`[[<-`, MassSpecAnalyses) <- function(x, i, value) {
  x <- add(x, value)
  x
}


#' @noRd
S7::method(as.list, MassSpecAnalyses) <- function(x) {
  
  if (x$has_nts) {
    
    browser()
    
  }
  
  list("analyses" = x@analyses, "results" = x@results)
}


#' @noRd
S7::method(read, MassSpecAnalyses) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      
      json <- jsonlite::fromJSON(file)
      
      browser()
      
      # TODO add read method for NTS
      
      return(Analyses(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::MassSpecAnalyses")) return(res)
  }
  NULL
}

### Get -----


#' @noRd
S7::method(get_spectra_tic, MassSpecAnalyses) <- function(x, analyses = NULL, levels = c(1, 2), rt = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  value <- x$spectra_tic[analyses]
  value <- data.table::rbindlist(value, idcol = "analysis", fill = TRUE)
  value <- value[value$level %in% levels, ]
  if (!is.null(rt)) {
    if (length(rt) == 2 && is.numeric(rt)) {
      rt <- sort(rt)
      sel <- value$rt >= rt[1] & value$rt <= rt[2]
      value <- value[sel, ]
    }
  }
  value
}


#' @noRd
S7::method(get_spectra_bpc, MassSpecAnalyses) <- function(x, analyses = NULL, levels = c(1, 2), rt = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  value <- x$spectra_bpc[analyses]
  value <- data.table::rbindlist(value, idcol = "analysis", fill = TRUE)
  value <- value[value$level %in% levels, ]
  if (!is.null(rt)) {
    if (length(rt) == 2 && is.numeric(rt)) {
      rt <- sort(rt)
      sel <- value$rt >= rt[1] & value$rt <= rt[2]
      value <- value[sel, ]
    }
  }
  value
}


#' @noRd
S7::method(get_spectra, MassSpecAnalyses) <- function(x,
                                                      analyses = NULL,
                                                      levels = NULL,
                                                      mass = NULL,
                                                      mz = NULL,
                                                      rt = NULL,
                                                      mobility = NULL,
                                                      ppm = 20,
                                                      sec = 60,
                                                      millisec = 5,
                                                      id = NULL,
                                                      allTraces = TRUE,
                                                      isolationWindow = 1.3,
                                                      minIntensityMS1 = 0,
                                                      minIntensityMS2 = 0,
                                                      useRawData = TRUE,
                                                      useLoadedData = TRUE) {
  
  analyses <- .check_analyses_argument(x, analyses)
  
  if (is.null(analyses)) return(data.table())
  
  if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) minIntensityMS1 <- 0
  
  if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) minIntensityMS2 <- 0
  
  polarities <- x$spectra_polarity[analyses]
  
  if (!is.null(mass)) {
    targets <- MassSpecTargets(mass, rt, mobility, ppm, sec, millisec, id, analyses, polarities)
  } else {
    targets <- MassSpecTargets(mz, rt, mobility, ppm, sec, millisec, id, analyses, polarities)
  }
  
  targets <- targets@targets
  
  if ("polarity" %in% colnames(targets) && nrow(targets) > 0) {
    for (i in seq_len(nrow(targets))) {
      if (targets$polarity[i] == "positive") targets$polarity[i] <- 1
      if (targets$polarity[i] == "negative") targets$polarity[i] <- -1
    }
  }
  
  num_cols <- c("mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax")
  
  if (all(apply(targets[, num_cols, with = FALSE], 1, function(z) sum(z, na.rm = TRUE)) != 0)) {
    
    if (TRUE %in% is.na(targets$mz)) targets$mz[is.na(targets$mz)] <- 0
    
    if (TRUE %in% is.na(targets$mzmax)) targets$mzmax[is.na(targets$mzmax)] <- max(x$spectra_highest_mz[analyses])
    
    if (TRUE %in% is.na(targets$mzmin)) targets$mzmin[is.na(targets$mzmin)] <- min(x$spectra_lowest_mz[analyses])
    
    if (TRUE %in% (targets$mzmax == 0)) targets$mzmax[targets$mzmax == 0] <- max(x$spectra_highest_mz[analyses])
    
    if (TRUE %in% is.na(targets$rt)) targets$rt[is.na(targets$rt)] <- 0
    
    if (TRUE %in% is.na(targets$rtmax)) targets$rtmax[is.na(targets$rtmax)] <- max(x$spectra_highest_rt[analyses])
    
    if (TRUE %in% is.na(targets$rtmin)) targets$rtmin[is.na(targets$rtmin)] <- min(x$spectra_lowest_rt[analyses])
    
    if (TRUE %in% (targets$rtmax == 0)) targets$rtmax[targets$rtmax == 0] <- max(x$spectra_highest_rt[analyses])
    
    if (TRUE %in% is.na(targets$mobility)) targets$mobility[is.na(targets$mobility)] <- 0
    
    if (TRUE %in% is.na(targets$mobilitymax) && any(x$has_ion_mobility[analyses])) targets$mobilitymax[is.na(targets$mobilitymax)] <- max(x$spectra_highest_mobility[analyses], na.rm = TRUE)
    
    if (TRUE %in% is.na(targets$mobilitymin) && any(x$has_ion_mobility[analyses])) targets$mobilitymin[is.na(targets$mobilitymin)] <- min(x$spectra_lowest_mobility[analyses], na.rm = TRUE)
    
    if (TRUE %in% (targets$mobilitymax == 0) && any(x$has_ion_mobility[analyses])) targets$mobilitymax[targets$mobilitymax == 0] <- max(x$spectra_highest_mobility[analyses], na.rm = TRUE)
    
  } else {
    targets$id <- targets$analysis
  }
  
  if (is.null(levels)) {
    levels <- unique(x$spectra_level[analyses])
    levels <- as.numeric(unlist(strsplit(levels, ", ")))
  }
  
  if (!2 %in% levels) allTraces <- TRUE
  
  if (!is.logical(allTraces)) allTraces <- TRUE
  
  if (nrow(targets) > 0) {
    
    if ("polarity" %in% colnames(targets)) targets$polarity <- as.numeric(targets$polarity)
    
    targets$precursor <- FALSE
    
    if (!allTraces) {
      
      if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) isolationWindow <- 0
      
      targets$precursor <- TRUE
      
      targets$mzmin <- targets$mzmin - (isolationWindow / 2)
      
      targets$mzmax <- targets$mzmax + (isolationWindow / 2)
      
      # TODO make case for DIA when pre_mz is not available
    }
  }
  
  #__________________________________________________________________
  # Extracts spectra results
  #__________________________________________________________________
  if ((!useRawData) && x$has_spectra) {
    
    if (x$spectra$is_averaged) {
      spec <- rbindlist(x$spectra$spectra, idcol = "replicate", fill = TRUE)
    } else {
      spec <- rbindlist(x$spectra$spectra, idcol = "analysis", fill = TRUE)
    }

    if ("analysis" %in% colnames(spec)) {
      spec <- spec[spec$analysis %in% analyses, ]

    } else if ("replicate" %in% colnames(spec)) {
      rpl <- x$replicates
      rpl <- rpl[analyses]
      spec <- spec[spec$replicate %in% unname(rpl)]

      if (!"analysis" %in% colnames(spec)) {
        spec$analysis <- spec$replicate
        setcolorder(spec, c("analysis", "replicate"))
      }
    }

    return(spec)
  
  #__________________________________________________________________
  # Extracts spectra from results
  #__________________________________________________________________
  } else if (useRawData && useLoadedData && any(x$has_loaded_spectra)) {
    
    spec_list <- lapply(x$analyses[analyses], function(a) {
      
      temp <- a$spectra
      
      with_im <- any(temp$mobility > 0)
      
      if (!is.null(levels)) temp <- temp[temp$level %in% levels, ]
      
      if (nrow(targets) > 0) {
        
        if ("analysis" %in% colnames(targets)) targets <- targets[targets$analysis %in% a$name, ]
        
        if (nrow(targets) > 0) {
          
          if ("polarity" %in% colnames(targets)) temp <- temp[temp$polarity == targets$polarity, ]
          
          if (nrow(targets) > 0) {
            temp <- .trim_spectra_targets(temp, targets, with_im)
            
          } else {
            temp <- data.table()
          }
        } else {
          temp <- data.table()
        }
      }
      
      if (with_im) temp$mobility <- NULL
      
      if (!"analysis" %in% colnames(temp)) temp$analysis <- a$name
      
      if (!"replicate" %in% colnames(temp)) temp$replicate <- a$replicate
      
      temp <- temp[!(temp$intensity <= minIntensityMS1 & temp$level == 1), ]
      
      temp <- temp[!(temp$intensity <= minIntensityMS2 & temp$level == 2), ]
      
      temp
      
    })
    
    #__________________________________________________________________
    # Extracts spectra from raw data 
    #__________________________________________________________________
  } else {
    
    spec_list <- lapply(x$analyses[analyses], function(a, levels, targets) {
      
      if ("analysis" %in% colnames(targets)) targets <- targets[targets$analysis %in% a$name, ]
      
      cache <- lapply(seq_len(nrow(targets)), function(i) {
        .load_chache(paste0("parsed_ms_spectra_", gsub("-|[/]|[.]", "", targets$id[i])), a$file, levels, targets[i, ], minIntensityMS1, minIntensityMS2)
      })
      
      names(cache) <- targets$id
      
      cached_targets_sel <- vapply(cache, function(z) !is.null(z$data), FALSE)
      
      cached_targets <- cache[cached_targets_sel]
      
      no_cached_targets <- targets[!cached_targets_sel, ]
      
      if (nrow(no_cached_targets) > 0) {
        
        message("\U2699 Parsing spectra from ", basename(a$file), "...", appendLF = FALSE)
        
        if (nrow(no_cached_targets) == 1) {
          num_cols <- c("mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax")
          if (apply(no_cached_targets[, num_cols, with = FALSE], 1, function(z) sum(z, na.rm = TRUE)) == 0) {
            no_cached_targets <- no_cached_targets[0, ]
          }
        }
        
        spec <- rcpp_parse_ms_spectra(a, levels, no_cached_targets, minIntensityMS1, minIntensityMS2)
        
        message(" Done!")
        
        if (nrow(spec) > 0) {
          if (!any(spec$mobility > 0)) spec$mobility <- NULL
          if (!"analysis" %in% colnames(spec)) spec$analysis <- a$name
          if (!"replicate" %in% colnames(spec)) spec$replicate <- a$replicate
          
          if ("id" %in% colnames(spec)) {
            spec <- split(spec, spec$id)
          } else {
            spec <- list(spec)
          }
          
          for (i in names(spec)) {
            if (nrow(spec[[i]]) > 0) {
              if (!is.null(cache[[i]]$hash)) {
                .save_cache(paste0("parsed_ms_spectra_", gsub("-|[/]|[.]", "", i)), spec[[i]], cache[[i]]$hash)
                # message("\U1f5ab Parsed spectra for ", i, " cached!")
              }
            }
          }
          
          spec <- data.table::rbindlist(spec, fill = TRUE)
          
        } else {
          spec <- data.table::data.table()
        }
      } else {
        spec <- data.table::data.table()
      }
      
      if (length(cached_targets) > 0) {
        cached_targets_dt <- data.table::rbindlist(lapply(cached_targets, function(z) as.data.table(z$data)), fill = TRUE)
        spec <- data.table::rbindlist(c(list(cached_targets_dt), list(spec)), fill = TRUE)
        message("\U2139 Spectra loaded from cache!")
      }
      
      if (nrow(spec) == 0) return(data.table::data.table())
      
      if ("id" %in% colnames(spec)) {
        data.table::setorder(spec, id, rt, mz)
      } else {
        data.table::setorder(spec, rt, mz)
      }
      
      gc()
      spec
      
    }, levels = levels, targets = targets)
  }
  
  if (length(spec_list) == length(analyses)) {
    
    spec <- data.table::rbindlist(spec_list, fill = TRUE)
    
    if (nrow(spec) > 0) data.table::setcolorder(spec, c("analysis", "replicate"))
    
    spec
    
  } else {
    warning("Defined analyses not found!")
    data.table::data.table()
  }
}


#' @noRd
S7::method(get_spectra_eic, MassSpecAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          mass = NULL,
                                                          mz = NULL,
                                                          rt = NULL,
                                                          mobility = NULL,
                                                          ppm = 20,
                                                          sec = 60,
                                                          millisec = 5,
                                                          id = NULL,
                                                          useRawData = TRUE,
                                                          useLoadedData = FALSE) {
  
  eic <- get_spectra(x,
    analyses, levels = 1,
    mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces = TRUE,
    isolationWindow = 1.3,
    minIntensityMS1 = 0,
    minIntensityMS2 = 0,
    useRawData = useRawData,
    useLoadedData = useLoadedData
  )
  
  if (nrow(eic) > 0) {
    intensity <- NULL
    eic <- as.data.table(eic)
    if (!"id" %in% colnames(eic)) eic$id <- NA_character_
    if (!"polarity" %in% colnames(eic)) eic$polarity <- 0
    eic <- eic[, `:=`(intensity = sum(intensity)), by = c("analysis", "polarity", "id", "rt")][]
    eic <- eic[, c("analysis", "polarity", "id", "rt", "intensity"), with = FALSE]
    eic <- unique(eic)
  }
  
  eic
}


#' @noRd
S7::method(get_spectra_ms1, MassSpecAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          mass = NULL,
                                                          mz = NULL,
                                                          rt = NULL,
                                                          mobility = NULL,
                                                          ppm = 20,
                                                          sec = 60,
                                                          millisec = 5,
                                                          id = NULL,
                                                          mzClust = 0.003,
                                                          presence = 0.8,
                                                          minIntensity = 1000,
                                                          useRawData = TRUE,
                                                          useLoadedData = FALSE) {
  
  ms1 <- get_spectra(x,
    analyses, levels = 1,
    mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces = TRUE,
    minIntensityMS1 = minIntensity,
    minIntensityMS2 = 0,
    useRawData = useRawData,
    useLoadedData = useLoadedData
  )
  
  if (nrow(ms1) == 0) return(ms1)
  
  if (!"id" %in% colnames(ms1)) {
    
    if (x$has_ion_mobility) {
      ms1$id <- paste(
        round(min(ms1$mz), 4),
        "-",
        round(max(ms1$mz), 4),
        "/",
        round(max(ms1$rt), 0),
        "-",
        round(min(ms1$rt), 0),
        "/",
        round(max(ms1$mobility), 0),
        "-",
        round(min(ms1$mobility), 0),
        sep = ""
      )
      
    } else {
      ms1$id <- paste(
        round(min(ms1$mz), 4),
        "-",
        round(max(ms1$mz), 4),
        "/",
        round(max(ms1$rt), 0),
        "-",
        round(min(ms1$rt), 0),
        sep = ""
      )
    }
  }
  
  if (!is.numeric(mzClust)) mzClust = 0.01
  
  ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id, "_", ms1$polarity)
  
  ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, FALSE)
  
  ms1_df <- rbindlist(ms1_list, fill = TRUE)
  
  ms1_df <- ms1_df[order(ms1_df$mz), ]
  
  ms1_df <- ms1_df[order(ms1_df$id), ]
  
  ms1_df <- ms1_df[order(ms1_df$analysis), ]
  
  ms1_df
}


#' @noRd
S7::method(get_spectra_ms2, MassSpecAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          mass = NULL,
                                                          mz = NULL,
                                                          rt = NULL,
                                                          mobility = NULL,
                                                          ppm = 20,
                                                          sec = 60,
                                                          millisec = 5,
                                                          id = NULL,
                                                          isolationWindow = 1.3,
                                                          mzClust = 0.005,
                                                          presence = 0.8,
                                                          minIntensity = 0,
                                                          useRawData = TRUE,
                                                          useLoadedData = FALSE) {
  
  ms2 <- get_spectra(x,
    analyses, levels = 2,
    mass, mz, rt, mobility, ppm, sec, millisec, id,
    isolationWindow = isolationWindow,
    allTraces = FALSE,
    minIntensityMS1 = 0,
    minIntensityMS2 = minIntensity,
    useRawData = useRawData,
    useLoadedData = useLoadedData
  )
  
  if (nrow(ms2) == 0) return(ms2)
  
  if (!"id" %in% colnames(ms2)) {
    if (x$has_ion_mobility) {
      ms2$id <- paste(
        round(min(ms2$mz), 4),
        "-",
        round(max(ms2$mz), 4),
        "/",
        round(max(ms2$rt), 0),
        "-",
        round(min(ms2$rt), 0),
        "/",
        round(max(ms2$mobility), 0),
        "-",
        round(min(ms2$mobility), 0),
        sep = ""
      )
      
    } else {
      ms2$id <- paste(
        round(min(ms2$mz), 4),
        "-",
        round(max(ms2$mz), 4),
        "/",
        round(max(ms2$rt), 0),
        "-",
        round(min(ms2$rt), 0),
        sep = ""
      )
    }
  }
  
  if (!is.numeric(mzClust)) mzClust = 0.01
  
  ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id, "_", ms2$polarity)
  
  ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, FALSE)
  
  ms2_df <- rbindlist(ms2_list, fill = TRUE)
  
  ms2_df <- ms2_df[order(ms2_df$mz), ]
  
  ms2_df <- ms2_df[order(ms2_df$id), ]
  
  ms2_df <- ms2_df[order(ms2_df$analysis), ]
  
  ms2_df
}


#' @noRd
S7::method(get_chromatograms, MassSpecAnalyses) <- function(x,
                                                            analyses = NULL,
                                                            chromatograms = NULL,
                                                            minIntensity = NULL,
                                                            useRawData = FALSE,
                                                            useLoadedData = TRUE) {
  
  analyses <- .check_analyses_argument(x, analyses)
  
  if (is.null(analyses)) return(data.table())
  
  #__________________________________________________________________
  # Extracts chromatograms from results via the active binding
  #__________________________________________________________________
  if ((!useRawData) && x$has_chromatograms) {
    
    if (x$chromatograms$is_averaged) {
      chroms <- rbindlist(x$chromatograms$chromatograms, idcol = "replicate", fill = TRUE)
    } else {
      chroms <- rbindlist(x$chromatograms$chromatograms, idcol = "analysis", fill = TRUE)
    }
    
    if ("analysis" %in% colnames(chroms)) {
      chroms <- chroms[chroms$analysis %in% analyses, ]
      
    } else if ("replicate" %in% colnames(chroms)) {
      rpl <- x$replicates
      rpl <- rpl[analyses]
      chroms <- chroms[chroms$replicate %in% unname(rpl)]
      
      if (!"analysis" %in% colnames(chroms)) {
        chroms$analysis <- chroms$replicate
        setcolorder(chroms, c("analysis", "replicate"))
      }
    }
    
    if (is.numeric(chromatograms)) {
      which_chroms <- chroms$index %in% chromatograms
      chroms <- chroms[which_chroms, ]
      
    } else if (is.character(chromatograms)) {
      which_chroms <- chroms$id %in% chromatograms
      chroms <- chroms[which_chroms, ]
      
    } else if (!is.null(chromatograms)) {
      return(data.table())
    }
    
    if (is.numeric(minIntensity)) chroms <- chroms[chroms$intensity > minIntensity, ]
    
    chroms
    
    #__________________________________________________________________
    # Extracts loaded chromatograms
    #__________________________________________________________________
  } else if (any(x$has_loaded_chromatograms[analyses]) && useLoadedData) {
    
    chroms <- lapply(x$analyses[analyses], function(z) z$chromatograms)
    chroms <- rbindlist(chroms, idcol = "analysis", fill = TRUE)
    
    if (is.numeric(chromatograms)) {
      which_chroms <- chroms$index %in% chromatograms
      chroms <- chroms[which_chroms, ]
      
    } else if (is.character(chromatograms)) {
      which_chroms <- chroms$id %in% chromatograms
      chroms <- chroms[which_chroms, ]
      
    } else if (!is.null(chromatograms)) {
      return(data.table())
    }
    
    if (is.numeric(minIntensity)) chroms <- chroms[chroms$intensity > minIntensity, ]
    
    chroms
    
    #__________________________________________________________________
    # Extracts loaded chromatograms
    #__________________________________________________________________
  } else {
    
    chroms_list <- lapply(x$analyses[analyses], function(z, chromatograms) {
      
      if (nrow(z$chromatograms_headers) == 0) return(data.frame())
      
      idx <- z$chromatograms_headers$index
      
      if (is.numeric(chromatograms)) {
        idx <- idx[chromatograms + 1]
        
      } else if (is.character(chromatograms)) {
        cid <- z$chromatograms_headers$id
        which_chroms <- cid %in% chromatograms
        idx <- idx[which_chroms]
        
      } else if (!is.null(chromatograms)) {
        return(data.table())
      }
      
      cache <- .load_chache("parsed_ms_chromatograms", z$file, idx)
      
      if (!is.null(cache$data)) {
        message("\U2139 Chromatograms loaded from cache!")
        return(cache$data)
      }
      
      message("\U2699 Parsing chromatograms from ", basename(z$file), "...", appendLF = FALSE)
      
      chrom <- rcpp_parse_ms_chromatograms(z, idx)
      
      message(" Done!")
      
      if (nrow(chrom) == 0) return(data.frame())
      
      if (!"analysis" %in% colnames(chrom)) chrom$analysis <- z$name
      
      if (!"replicate" %in% colnames(chrom)) chrom$replicate <- z$replicate
      
      if (!is.null(cache$hash)) {
        .save_cache("parsed_ms_chromatograms", chrom, cache$hash)
        message("\U1f5ab Parsed chromatograms cached!")
      }
      
      chrom
      
    }, chromatograms = chromatograms)
    
    if (length(chroms_list) == length(analyses)) {
      
      chroms <- rbindlist(chroms_list, fill = TRUE)
      
      if (nrow(chroms) > 0) setcolorder(chroms, c("analysis", "replicate"))
      
      if (is.numeric(minIntensity)) chroms <- chroms[chroms$intensity > minIntensity, ]
      
      chroms
      
    } else {
      warning("Defined analyses or chromatograms not found!")
      data.table()
    }
  }
}


#' @noRd
S7::method(get_features, MassSpecAnalyses) <- function(x,
                                                       analyses = NULL,
                                                       features = NULL,
                                                       mass = NULL,
                                                       mz = NULL,
                                                       rt = NULL,
                                                       mobility = NULL,
                                                       ppm = 20,
                                                       sec = 60,
                                                       millisec = 5,
                                                       filtered = FALSE) {
  
  analyses <- .check_analyses_argument(x, analyses)
  
  if (is.null(analyses)) return(data.table())
  
  fts <- NULL
  
  if (x$has_nts) fts <- x$nts$feature_list[analyses]
  
  if (is.null(fts)) return(data.table())
  
  fts <- rbindlist(fts, idcol = "analysis", fill = TRUE)
  
  if (nrow(fts) == 0) return(data.table())
  
  if (!filtered) fts <- fts[!fts$filtered, ]
  
  fts$feature <- as.character(fts$feature)
  
  if (!is.null(features)) {
    target_id <- features
    
    if (is.character(target_id)) {
      
      if ("group" %in% colnames(fts)) {
        fts <- fts[fts$feature %in% target_id | fts$group %in% target_id, ]
        
      } else {
        fts <- fts[fts$feature %in% target_id, ]
      }
      
      fts$replicate <- x$replicates[fts$analysis]
      
      return(fts)
      
    } else if (is.numeric(target_id)) {
      
      fts <- fts[target_id, ]
      
      fts$replicate <- x$replicates[fts$analysis]
      
      return(fts)
    }
    
    if (is.data.frame(target_id)) {
      
      if (all(colnames(fts) %in% colnames(target_id))) return(target_id)
      
      if ("analysis" %in% colnames(target_id)) {
        sel <- rep(FALSE, nrow(fts))
        
        for (i in seq_len(nrow(target_id))) {
          sel[(fts$feature %in% target_id$feature[i] &
                 fts$analysis %in% target_id$analysis[i]) |
                fts$group %in% target_id$group] <- TRUE
        }
        
        fts <- fts[sel, ]
        
        if ("name" %in% colnames(target_id)) {
          ids <- target_id$name
          names(ids) <- target_id$feature
          fts$name <- ids[fts$feature]
        }
        
        return(fts)
        
      } else if ("group" %in% colnames(target_id)) {
        sel <- rep(FALSE, nrow(fts))
        
        for (i in seq_len(nrow(target_id))) {
          sel[fts$feature %in% target_id$feature[i] |
                fts$group %in% target_id$group] <- TRUE
        }
        
        fts <- fts[sel, ]
        
        if ("name" %in% colnames(target_id)) {
          ids <- target_id$name
          names(ids) <- target_id$group
          ids <- ids[!duplicated(names(ids))]
          fts$name <- ids[fts$group]
        }
        
        fts$replicate <- x$replicates[fts$analysis]
        
        return(fts)
      }
    }
    
    return(data.table())
  }
  
  if (!is.null(mass)) {
    
    if (is.data.frame(mass)) {
      colnames(mass) <- gsub("mass", "mz", colnames(mass))
      colnames(mass) <- gsub("neutralMass", "mz", colnames(mass))
      colnames(mass) <- gsub("min", "mzmin", colnames(mass))
      colnames(mass) <- gsub("max", "mzmax", colnames(mass))
    }
    
    targets <- MassSpecTargets(mass, rt, mobility, ppm, sec, millisec)
    targets <- targets@targets
    
    for (i in seq_len(nrow(targets))) {
      
      if (targets$rtmax[i] == 0) targets$rtmax[i] <- max(fts$rtmax)
      
      if (targets$mzmax[i] == 0) targets$mzmax[i] <- max(fts$mass)
      
      if ("mobility" %in% colnames(fts)) {
        if (targets$mobilitymax[i] == 0) targets$mobilitymax[i] <- max(fts$mobility)
      }
    }
    
    sel <- rep(FALSE, nrow(fts))
    
    ids <- rep(NA_character_, nrow(fts))
    
    for (i in seq_len(nrow(targets))) {
      
      if ("mobility" %in% colnames(fts)) {
        sel[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
              between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
              between(fts$mobility, targets$mobilitymin[i], targets$mobilitymax[i])] <- TRUE
        
        ids[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
              between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
              between(fts$mobility, targets$mobilitymin[i], targets$mobilitymax[i])] <- targets$id[i]
        
      } else {
        sel[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
              between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
        
        ids[between(fts$mass, targets$mzmin[i], targets$mzmax[i]) &
              between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- targets$id[i]
      }
    }
    
    fts$name <- ids
    
    fts$replicate <- x$replicates[fts$analysis]
    
    return(fts[sel])
  }
  
  if (!is.null(mz)) {
    targets <- MassSpecTargets(mz, rt, mobility, ppm, sec, millisec)
    targets <- targets@targets
    
    for (i in seq_len(nrow(targets))) {
      
      if (targets$rtmax[i] == 0) targets$rtmax[i] <- max(fts$rtmax)
      
      if (targets$mzmax[i] == 0) targets$mzmax[i] <- max(fts$mzmax)
      
      if ("mobility" %in% colnames(fts)) {
        if (targets$mobilitymax[i] == 0) targets$mobilitymax[i] <- max(fts$mobility)
      }
    }
    
    sel <- rep(FALSE, nrow(fts))
    
    ids <- rep(NA_character_, nrow(fts))
    
    for (i in seq_len(nrow(targets))) {
      
      if ("mobility" %in% colnames(fts)) {
        sel[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
              between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
              between(fts$mobility, targets$mobilitymin[i], targets$mobilitymax[i])] <- TRUE
        
        ids[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
              between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
              between(fts$mobility, targets$mobilitymin[i], targets$mobilitymax[i])] <- targets$id[i]
        
      } else {
        sel[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
              between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
        
        ids[between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
              between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- targets$id[i]
      }
    }
    
    fts$name <- ids
    
    fts$replicate <- x$replicates[fts$analysis]
    
    return(fts[sel])
  }
  
  fts$replicate <- x$replicates[fts$analysis]
  
  fts
}


#' @noRd
S7::method(get_features_eic, MassSpecAnalyses) <- function(x,
                                                           analyses = NULL,
                                                           features = NULL,
                                                           mass = NULL,
                                                           mz = NULL,
                                                           rt = NULL,
                                                           mobility = NULL,
                                                           ppm = 20,
                                                           sec = 60,
                                                           millisec = 5,
                                                           rtExpand = NULL,
                                                           mzExpand = NULL,
                                                           filtered = FALSE,
                                                           useLoadedData = TRUE) {
  
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) return(data.table())
  
  if (useLoadedData) {
    if (x$has_nts) {
      if (x$nts$has_features_eic) {
        useLoadedData <- TRUE
      } else {
        useLoadedData <- FALSE
      }
    } else {
      useLoadedData <- FALSE
    }
  }
  
  if (is.null(rtExpand)) rtExpand <- 0
  if (is.null(mzExpand)) mzExpand <- 0
  
  if (useLoadedData) {
    
    eic_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
      temp <- fts[x, ]
      
      temp_ms <- temp[["eic"]][[1]]
      
      if (is.null(temp_ms)) return(data.table())
      
      temp_ms$analysis <- temp$analysis
      
      temp_ms$feature <- temp$feature
      
      temp_ms
    }, fts = fts)
    
    eic <- rbindlist(eic_list, fill = TRUE)
    
    add_filtered <- FALSE
    
    if (any(fts$filtered)) {
      if (nrow(eic) == 0) {
        add_filtered <- TRUE
      } else if (any(!(fts$feature[fts$filtered] %in% eic$feature))) {
        add_filtered <- TRUE
      }
    }
    
    if (add_filtered) {
      fts_filtered <- fts[fts$filtered, ]
      fts_filtered <- fts_filtered[!(fts_filtered$feature %in% eic$feature), ]
      fts_filtered$rtmin <- fts_filtered$rtmin - rtExpand
      fts_filtered$rtmax <- fts_filtered$rtmax + rtExpand
      fts_filtered$mzmin <- fts_filtered$mzmin - mzExpand
      fts_filtered$mzmax <- fts_filtered$mzmax + mzExpand
      
      eic_2 <- get_spectra(x, analyses = analyses, levels = 1, mz = fts_filtered, id = fts_filtered$feature)
      
      eic_2 <- eic_2[, c("analysis", "polarity", "id", "rt", "mz", "intensity"), with = FALSE]
      
      setnames(eic_2, "id", "feature")
      
      eic <- list(eic, eic_2)
      
      eic <- rbindlist(eic, fill = TRUE)
    }
    
    if (nrow(eic) == 0) return(data.table())
    
  } else {
    
    fts$rtmin <- fts$rtmin - rtExpand
    fts$rtmax <- fts$rtmax + rtExpand
    fts$mzmin <- fts$mzmin - mzExpand
    fts$mzmax <- fts$mzmax + mzExpand
    
    eic <- get_spectra(x, analyses = analyses, levels = 1, mz = data.table::copy(fts), id = fts$feature, useRawData = TRUE, useLoadedData = TRUE)
    
    eic <- eic[, c("analysis", "polarity", "id", "rt", "mz", "intensity"), with = FALSE]
    
    setnames(eic, "id", "feature")
  }
  
  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- fts$feature
    eic$group <- fgs[eic$feature]
  }
  
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- fts$feature
    eic$name <- tar_ids[eic$feature]
  }
  
  eic
}


#' @noRd
S7::method(get_features_ms1, MassSpecAnalyses) <- function(x,
                                                           analyses = NULL,
                                                           features = NULL,
                                                           mass = NULL,
                                                           mz = NULL,
                                                           rt = NULL,
                                                           mobility = NULL,
                                                           ppm = 20,
                                                           sec = 60,
                                                           millisec = 5,
                                                           rtWindow = c(-2, 2),
                                                           mzWindow = c(-5, 100),
                                                           mzClust = 0.003,
                                                           presence = 0.8,
                                                           minIntensity = 1000,
                                                           filtered = FALSE,
                                                           useLoadedData = TRUE) {
  
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) return(data.table())
  
  if (!is.null(rtWindow) & length(rtWindow) == 2 & is.numeric(rtWindow)) {
    fts$rtmin <- fts$rt + rtWindow[1]
    fts$rtmax <- fts$rt + rtWindow[2]
  }
  
  if (!is.null(mzWindow) & length(mzWindow) == 2 & is.numeric(mzWindow)) {
    fts$mzmin <- fts$mz + mzWindow[1]
    fts$mzmax <- fts$mz + mzWindow[2]
  }
  
  if (useLoadedData) {
    if (x$has_nts) {
      if (x$nts$has_features_ms1) {
        useLoadedData <- TRUE
      } else {
        useLoadedData <- FALSE
      }
    } else {
      useLoadedData <- FALSE
    }
  }
  
  if (useLoadedData) {
    
    ms1_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
      temp <- fts[x, ]
      
      temp_ms <- temp[["ms1"]][[1]]
      
      if (is.null(temp_ms)) return(data.table())
      
      temp_ms$analysis <- temp$analysis
      
      temp_ms$feature <- temp$feature
      
      temp_ms
    }, fts = fts)
    
    ms1 <- rbindlist(ms1_list, fill = TRUE)
    
    add_filtered <- FALSE
    
    if (any(fts$filtered)) {
      if (nrow(ms1) == 0) {
        add_filtered <- TRUE
      } else if (any(!(fts$feature[fts$filtered] %in% ms1$feature))) {
        add_filtered <- TRUE
      }
    }
    
    if (add_filtered) {
      fts_filtered <- fts[fts$filtered, ]
      fts_filtered <- fts_filtered[!(fts_filtered$feature %in% ms1$feature), ]
      
      ms1_2 <- get_spectra_ms1(x,
        analyses = unique(fts$analysis),
        mz = fts_filtered,
        id = fts_filtered$feature,
        mzClust = mzClust,
        presence = presence,
        minIntensity = minIntensity
      )
      
      setnames(ms1_2, "id", "feature")
      
      ms1 <- list(ms1, ms1_2)
      
      ms1 <- rbindlist(ms1, fill = TRUE)
    }
    
    if (nrow(ms1) == 0) return(data.table())
    
  } else {
    ms1 <- get_spectra_ms1(x,
      analyses = unique(fts$analysis),
      mz = data.table::copy(fts),
      id = fts$feature,
      mzClust = mzClust,
      presence = presence,
      minIntensity = minIntensity
    )
    
    setnames(ms1, "id", "feature")
  }
  
  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  
  unique_ms1_id <- paste0(ms1$analysis, "-", ms1$feature)
  
  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms1$group <- fgs[unique_ms1_id]
  }
  
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms1$name <- tar_ids[unique_ms1_id]
  }
  
  data.table::copy(ms1)
}


#' @noRd
S7::method(get_features_ms2, MassSpecAnalyses) <- function(x,
                                                           analyses = NULL,
                                                           features = NULL,
                                                           mass = NULL,
                                                           mz = NULL,
                                                           rt = NULL,
                                                           mobility = NULL,
                                                           ppm = 20,
                                                           sec = 60,
                                                           millisec = 5,
                                                           isolationWindow = 1.3,
                                                           mzClust = 0.003,
                                                           presence = 0.8,
                                                           minIntensity = 0,
                                                           filtered = FALSE,
                                                           useLoadedData = TRUE) {
  
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) return(data.table())
  
  if (useLoadedData) {
    if (x$has_nts) {
      if (x$nts$has_features_ms2) {
        useLoadedData <- TRUE
      } else {
        useLoadedData <- FALSE
      }
    } else {
      useLoadedData <- FALSE
    }
  }
  
  if (useLoadedData) {
    
    ms2_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
      temp <- fts[x, ]
      
      temp_ms <- temp[["ms2"]][[1]]
      
      if (is.null(temp_ms)) return(data.table())
      
      temp_ms$analysis <- temp$analysis
      
      temp_ms$feature <- temp$feature
      
      temp_ms
    }, fts = fts)
    
    ms2 <- rbindlist(ms2_list, fill = TRUE)
    
    add_filtered <- FALSE
    
    if (any(fts$filtered)) {
      if (nrow(ms2) == 0) {
        add_filtered <- TRUE
      } else if (any(!(fts$feature[fts$filtered] %in% ms2$feature))) {
        add_filtered <- TRUE
      }
    }
    
    if (add_filtered) {
      fts_filtered <- fts[fts$filtered, ]
      fts_filtered <- fts_filtered[!(fts_filtered$feature %in% ms2$feature), ]
      
      ms2_2 <- get_spectra_ms2(x,
        analyses = unique(fts$analysis),
        mz = fts_filtered,
        id = fts_filtered$feature,
        isolationWindow = isolationWindow,
        mzClust = mzClust,
        presence = presence,
        minIntensity = minIntensity
      )
      
      if (nrow(ms2_2) == 0) return(data.table())
      
      setnames(ms2_2, "id", "feature", skip_absent = TRUE)
      
      ms2 <- list(ms2, ms2_2)
      
      ms2 <- rbindlist(ms2, fill = TRUE)
    }
    
    if (nrow(ms2) == 0) return(data.table())
    
  } else {
    ms2 <- get_spectra_ms2(x,
      analyses = unique(fts$analysis),
      mz = data.table::copy(fts),
      id = fts$feature,
      isolationWindow = isolationWindow,
      mzClust = mzClust,
      presence = presence,
      minIntensity = minIntensity
    )
    
    if (nrow(ms2) == 0) return(data.table())
    
    setnames(ms2, "id", "feature", skip_absent = TRUE)
  }
  
  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  
  unique_ms2_id <- paste0(ms2$analysis, "-", ms2$feature)
  
  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms2$group <- fgs[unique_ms2_id]
  }
  
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms2$name <- tar_ids[unique_ms2_id]
  }
  
  data.table::copy(ms2)
}


#' @noRd
S7::method(get_groups, MassSpecAnalyses) <- function(x,
                                                     groups = NULL,
                                                     mass = NULL,
                                                     mz = NULL,
                                                     rt = NULL,
                                                     mobility = NULL,
                                                     ppm = 20,
                                                     sec = 60,
                                                     millisec = 5,
                                                     filtered = FALSE,
                                                     intensities = TRUE,
                                                     average = FALSE,
                                                     sdValues = FALSE,
                                                     metadata = FALSE) {
  
  if (!x$has_nts) return(data.table())
  
  if (!x$nts$has_groups) return(data.table())
  
  fts <- get_features(x,
    analyses = NULL,
    features = groups,
    mass, mz, rt, mobility, ppm, sec, millisec,
    filtered = filtered
  )
  
  if (nrow(fts) > 0) {
    g_ids <- unique(fts$group)
    
    fgroups <- data.table("group" = g_ids)
    
    if (intensities) {
      
      if (average) {
        intensity <- NULL
        rpls <- x$replicates
        fts_temp <- copy(fts)
        fts_temp$analysis <- rpls[fts_temp$analysis]
        fts_av <- fts_temp[, .(intensity = mean(intensity), sd = sd(intensity), n = length(intensity)), by = c("group", "analysis")]
        fts_av$sd[is.na(fts_av$sd)] <- 0 
        fts_av$sd <- round(fts_av$sd / fts_av$intensity * 100, digits = 0)
        
        fts_sd <- copy(fts_av)
        fts_n <- copy(fts_av)
        
        fts_sd$intensity <- NULL
        fts_sd$n <- NULL
        fts_sd$analysis <- paste(fts_sd$analysis, "_sd", sep = "")
        fts_sd <- data.table::dcast(fts_sd[, c("group", "analysis", "sd"), with = TRUE], group ~ analysis, value.var = "sd")
        fts_sd[is.na(fts_sd)] <- 0
        
        tbl_rpls <- table(rpls)
        fts_n$tn <- tbl_rpls[fts_n$analysis]
        fts_n$n <- round(fts_n$n / fts_n$tn * 100, digits = 0)
        fts_n$intensity <- NULL
        fts_n$tn <- NULL
        fts_n$sd <- NULL
        fts_n$analysis <- paste(fts_n$analysis, "_n", sep = "")
        fts_n <- data.table::dcast(fts_n[, c("group", "analysis", "n"), with = TRUE], group ~ analysis, value.var = "n")
        fts_n[is.na(fts_n)] <- 0
        
        fts_av$sd <- NULL
        fts_av$n <- NULL
        fts_av <- data.table::dcast(fts_av, group ~ analysis, value.var = "intensity")
        fts_av[is.na(fts_av)] <- 0
        
      } else {
        fts_av <- fts[, .(intensity = max(intensity)), by = c("group", "analysis")]
        fts_av <- data.table::dcast(fts_av, group ~ analysis, value.var = "intensity")
        fts_av[is.na(fts_av)] <- 0
      }
    }
    
    if ("name" %in% colnames(fts)) {
      g_names <- fts$name
      names(g_names) <- fts$group
      g_names <- g_names[!duplicated(names(g_names))]
      fgroups$name <- g_names[fgroups$group]
    }
    
    if (metadata) {
      cols <- colnames(fts)
      if (!"istd" %in% cols) fts[["istd"]] <- list(NULL)
      if (!"quality" %in% cols) fts[["quality"]] <- list(NULL)
      if (!"isotope" %in% cols) fts[["isotope"]] <- list(NULL)
      rtmin <- NULL
      rtmax <- NULL
      mzmin <- NULL
      mzmax <- NULL
      feature <- NULL
      quality <- NULL
      isotope <- NULL
      istd <- NULL
      
      fts_meta <- fts[, .(
        rt = round(mean(rt), digits = 0),
        mass = round(mean(mass), digits = 4),
        rtdev = round(max(rtmax - rtmin), digits = 0),
        massdev = round(max(mzmax - mzmin), digits = 4),
        presence = round(length(feature) / self$get_number_analyses() * 100, digits = 1),
        maxint = round(max(intensity), digits = 0),
        sn = round(max(vapply(quality, function(x) if (!is.null(x)) x$sn else 0, 0), na.rm = TRUE), digits = 1),
        iso = min(vapply(isotope, function(x) if (!is.null(x)) x$step else 0, 0)),
        istd = paste0(unique(vapply(istd, function(x) if (!is.null(x)) x$name else NA_character_, NA_character_)), collapse = "; "),
        filtered = all(filtered)
      ), by = "group"]
      
      fgroups <- fgroups[fts_meta, on = "group"]
    }
    
    if (intensities) fgroups <- fgroups[fts_av, on = "group"]
    
    if (average && sdValues) {
      fgroups <- fgroups[fts_sd, on = "group"]
      fgroups <- fgroups[fts_n, on = "group"]
    }
    
    fgroups
    
  } else {
    data.table()
  }
}


#' @noRd
S7::method(get_groups_ms1, MassSpecAnalyses) <- function(x,
                                                         groups = NULL,
                                                         mass = NULL,
                                                         mz = NULL,
                                                         rt = NULL,
                                                         mobility = NULL,
                                                         ppm = 20,
                                                         sec = 60,
                                                         millisec = 5,
                                                         rtWindow = c(-2, 2),
                                                         mzWindow = c(-5, 90),
                                                         mzClustFeatures = 0.003,
                                                         presenceFeatures = 0.8,
                                                         minIntensityFeatures = 1000,
                                                         useLoadedData = TRUE,
                                                         mzClust = 0.003,
                                                         presence = 0.8,
                                                         minIntensity = 1000,
                                                         groupBy = "groups",
                                                         filtered = FALSE) {
  
  fgs <- get_groups(x,
    groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered,
    intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
  )
  
  if (nrow(fgs) == 0) return(data.table())
  
  fts <- self$get_features(features = fgs$group)
  
  if (nrow(fts) == 0) return(data.table())
  
  ms1 <- get_features_ms1(x,
    analyses = unique(fts$analysis),
    features = fts$feature,
    rtWindow = rtWindow,
    mzWindow = mzWindow,
    mzClust = mzClustFeatures,
    presence = presenceFeatures,
    minIntensity = minIntensityFeatures,
    filtered = filtered,
    useLoadedData = useLoadedData
  )
  
  ms1 <- ms1[ms1$intensity > minIntensity, ]
  
  if (nrow(ms1) == 0) return(data.table())
  
  polarities <- unique(x$spectra_polarity[unique(ms1$analysis)])
  
  multiple_polarities <- FALSE
  
  # TODO check for polarity switching with comma
  
  if (length(polarities) > 1) multiple_polarities <- TRUE
  
  if ("groups" %in% groupBy) {
    
    if (multiple_polarities) {
      ms1$unique_id <- paste0(ms1$group, "_", ms1$polarity)
      ms1$analysis <- NA_character_
      
    } else {
      ms1$unique_id <- ms1$group
      ms1$analysis <- NA_character_
    }
    
  } else {
    rpls <- x$replicates
    ms1$analysis <- rpls[ms1$analysis]
    
    if (multiple_polarities) {
      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group, "", ms1$polarity)
    } else {
      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group)
    }
  }
  
  ms1$id <- ms1$group
  
  ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, verbose = FALSE)
  
  ms1_df <- rbindlist(ms1_list, fill = TRUE)
  
  ms1_df$group <- ms1_df$id
  
  ms1_df[["id"]] <- NULL
  
  ms1_df <- ms1_df[order(ms1_df$mz), ]
  
  ms1_df <- ms1_df[order(ms1_df$group), ]
  
  if ("groups" %in% groupBy) {
    ms1_df[["analysis"]] <- NULL
    
  } else {
    ms1_df <- ms1_df[order(ms1_df$analysis), ]
    setnames(ms1_df, "analysis", "replicate")
  }
  
  if ("name" %in% colnames(fgs)) {
    tar_ids <- fgs$name
    names(tar_ids) <- fgs$group
    ms1_df$name <- tar_ids[ms1_df$group]
  }
  
  copy(ms1_df)
}


#' @noRd
S7::method(get_groups_ms2, MassSpecAnalyses) <- function(x,
                                                         groups = NULL,
                                                         mass = NULL,
                                                         mz = NULL,
                                                         rt = NULL,
                                                         mobility = NULL,
                                                         ppm = 20,
                                                         sec = 60,
                                                         millisec = 5,
                                                         isolationWindow = 1.3,
                                                         mzClustFeatures = 0.003,
                                                         presenceFeatures = 0.8,
                                                         minIntensityFeatures = 100,
                                                         useLoadedData = TRUE,
                                                         mzClust = 0.003,
                                                         presence = 0.8,
                                                         minIntensity = 100,
                                                         groupBy = "groups",
                                                         filtered = FALSE) {
  
  fgs <- get_groups(x,
    groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered,
    intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
  )
  
  if (nrow(fgs) == 0) return(data.table())
  
  fts <- self$get_features(features = fgs$group, filtered = filtered)
  
  if (nrow(fts) == 0) return(data.table())
  
  ms2 <- get_features_ms2(x,
    analyses = unique(fts$analysis),
    features = fts$feature,
    isolationWindow = isolationWindow,
    mzClust = mzClustFeatures,
    presence = presenceFeatures,
    minIntensity = minIntensityFeatures,
    filtered = filtered,
    useLoadedData = useLoadedData
  )
  
  ms2 <- ms2[ms2$intensity > minIntensity, ]
  
  if (nrow(ms2) == 0) return(data.table())
  
  polarities <- unique(x$spectra_polarity[unique(ms2$analysis)])
  
  multiple_polarities <- FALSE
  
  # TODO check for polarity switching with comma
  
  if (length(polarities) > 1) multiple_polarities <- TRUE
  
  if ("groups" %in% groupBy) {
    if (multiple_polarities) {
      ms2$unique_id <- paste0(ms2$group, "_", ms2$polarity)
      ms2$analysis <- NA_character_
      
    } else {
      ms2$unique_id <- ms2$group
      ms2$analysis <- NA_character_
    }
    
  } else {
    rpls <- x$replicates
    ms2$analysis <- rpls[ms2$analysis]
    
    if (multiple_polarities) {
      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group, "", ms2$polarity)
    } else {
      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group)
    }
  }
  
  ms2$id <- ms2$group
  
  ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, verbose = FALSE)
  
  ms2_df <- rbindlist(ms2_list, fill = TRUE)
  
  ms2_df$group <- ms2_df$id
  
  ms2_df[["id"]] <- NULL
  
  ms2_df <- ms2_df[order(ms2_df$mz), ]
  
  ms2_df <- ms2_df[order(ms2_df$group), ]
  
  if ("groups" %in% groupBy) {
    ms2_df[["analysis"]] <- NULL
    
  } else {
    ms2_df <- ms2_df[order(ms2_df$analysis), ]
    setnames(ms2_df, "analysis", "replicate")
  }
  
  if ("name" %in% colnames(fgs)) {
    tar_ids <- fgs$name
    names(tar_ids) <- fgs$group
    ms2_df$name <- tar_ids[ms2_df$group]
  }
  
  copy(ms2_df)
}

### Plot -----


#' @noRd
S7::method(plot_spectra_tic, MassSpecAnalyses) <- function(x,
                                                           analyses = NULL,
                                                           levels = c(1, 2),
                                                           rt = NULL,
                                                           xLab = NULL,
                                                           yLab = NULL,
                                                           title = NULL,
                                                           colorBy = "analyses",
                                                           legendNames = NULL,
                                                           showLegend = TRUE,
                                                           xlim = NULL,
                                                           ylim = NULL,
                                                           cex = 0.6,
                                                           downsize = 1,
                                                           interactive = TRUE) {
  intensity <- NULL
  level <- NULL
  tic <- get_spectra_tic(x, analyses, levels, rt)
  tic <- tic[, .(intensity = mean(intensity)), by = .(rt = floor(rt / downsize) * downsize, analysis, level)]
  if (nrow(tic) == 0) {
    message("\U2717 TIC not found for the analyses!")
    return(NULL)
  }
  if (grepl("replicates", colorBy)) tic$replicate <- x@replicates[tic$analysis]
  if (!"id" %in% colnames(tic)) tic$id <- tic$analysis
  if (!interactive) {
    .plot_spectra_eic_static(tic, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_spectra_eic_interactive(tic, legendNames, colorBy, title, showLegend, xLab, yLab)
  }
}


#' @noRd
S7::method(plot_spectra_bpc, MassSpecAnalyses) <- function(x,
                                                           analyses = NULL,
                                                           levels = c(1, 2),
                                                           rt = NULL,
                                                           xLab = NULL,
                                                           yLab = NULL,
                                                           title = NULL,
                                                           colorBy = "analyses",
                                                           legendNames = NULL,
                                                           showLegend = TRUE,
                                                           xlim = NULL,
                                                           ylim = NULL,
                                                           cex = 0.6,
                                                           interactive = TRUE) {
  bpc <- get_spectra_bpc(x, analyses, levels, rt)
  if (nrow(bpc) == 0) {
    message("\U2717 BPC not found for the analyses!")
    return(NULL)
  }
  if (grepl("replicates", colorBy)) bpc$replicate <- x@replicates[bpc$analysis]
  if (!"id" %in% colnames(bpc)) bpc$id <- bpc$analysis
  if (!interactive) {
    .plot_spectra_eic_static(bpc, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_spectra_bpc_interactive(bpc, legendNames, colorBy, title, showLegend, xLab, yLab)
  }
}


#' @noRd
S7::method(plot_spectra_eic, MassSpecAnalyses) <- function(x,
                                                           analyses = NULL,
                                                           mass = NULL,
                                                           mz = NULL,
                                                           rt = NULL,
                                                           mobility = NULL,
                                                           ppm = 20,
                                                           sec = 60,
                                                           millisec = 5,
                                                           id = NULL,
                                                           legendNames = NULL,
                                                           xLab = NULL,
                                                           yLab = NULL,
                                                           title = NULL,
                                                           colorBy = "targets",
                                                           showLegend = TRUE,
                                                           xlim = NULL,
                                                           ylim = NULL,
                                                           cex = 0.6,
                                                           interactive = TRUE) {

  eic <- get_spectra_eic(x, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id)
  
  if (nrow(eic) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }
  
  if (grepl("replicates", colorBy)) eic$replicate <- x$replicates[eic$analysis]
  
  if (!interactive) {
    .plot_spectra_eic_static(eic, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_spectra_eic_interactive(eic, legendNames, colorBy, title, showLegend, xLab, yLab)
  }
}


#' @noRd
S7::method(plot_spectra_ms1, MassSpecAnalyses) <- function(x,
                                                           analyses = NULL,
                                                           mass = NULL,
                                                           mz = NULL,
                                                           rt = NULL,
                                                           mobility = NULL,
                                                           ppm = 20,
                                                           sec = 60,
                                                           millisec = 5,
                                                           id = NULL,
                                                           mzClust = 0.003,
                                                           presence = 0.8,
                                                           minIntensity = 1000,
                                                           legendNames = NULL,
                                                           xLab = NULL,
                                                           yLab = NULL,
                                                           title = NULL,
                                                           colorBy = "targets",
                                                           showText = FALSE,
                                                           interactive = TRUE) {
  
  ms1 <- get_spectra_ms1(x, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, mzClust, presence, minIntensity)
  
  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }
  
  if (grepl("replicates", colorBy)) ms1$replicate <- x$replicates[ms1$analysis]
  
  if (!interactive) {
    .plot_spectra_ms1_static(ms1, legendNames, colorBy, title, xLab, yLab, showText)
  } else {
    .plot_spectra_ms1_interactive(ms1, legendNames, colorBy, title, xLab, yLab, showText)
  }
}


#' @noRd
S7::method(plot_spectra_ms2, MassSpecAnalyses) <- function(x,
                                                           analyses = NULL,
                                                           mass = NULL,
                                                           mz = NULL,
                                                           rt = NULL,
                                                           mobility = NULL,
                                                           ppm = 20,
                                                           sec = 60,
                                                           millisec = 5,
                                                           id = NULL,
                                                           isolationWindow = 1.3,
                                                           mzClust = 0.005,
                                                           presence = 0.8,
                                                           minIntensity = 0,
                                                           legendNames = NULL,
                                                           xLab = NULL,
                                                           yLab = NULL,
                                                           title = NULL,
                                                           colorBy = "targets",
                                                           interactive = TRUE) {
  
  ms2 <- get_spectra_ms2(x, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, isolationWindow, mzClust, presence, minIntensity)
  
  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }
  
  if (grepl("replicates", colorBy)) ms2$replicate <- x$replicates[ms2$analysis]
  
  if (!interactive) {
    .plot_spectra_ms2_static(ms2, legendNames, colorBy, title, xLab, yLab)
  } else {
    .plot_spectra_ms2_interactive(ms2, legendNames, colorBy, title, xLab, yLab)
  }
}


#' @noRd
S7::method(plot_features, MassSpecAnalyses) <- function(x,
                                                        analyses = NULL,
                                                        features = NULL,
                                                        mass = NULL,
                                                        mz = NULL,
                                                        rt = NULL,
                                                        mobility = NULL,
                                                        ppm = 20,
                                                        sec = 60,
                                                        millisec = 5,
                                                        rtExpand = 120,
                                                        mzExpand = NULL,
                                                        useLoadedData = TRUE,
                                                        filtered = FALSE,
                                                        legendNames = NULL,
                                                        xLab = NULL,
                                                        yLab = NULL,
                                                        title = NULL,
                                                        colorBy = "targets",
                                                        showLegend = TRUE,
                                                        xlim = NULL,
                                                        ylim = NULL,
                                                        cex = 0.6,
                                                        interactive = TRUE) {
  
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  eic <- get_features_eic(x,
    analyses = unique(fts$analysis),
    features = fts,
    rtExpand = rtExpand,
    mzExpand = mzExpand,
    filtered = filtered,
    useLoadedData = useLoadedData
  )
  
  intensity <- NULL
  
  eic <- eic[, `:=`(intensity = sum(intensity)), by = c("analysis", "polarity", "feature", "rt")][]
  
  if (nrow(eic) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }
  
  if (grepl("replicates", colorBy)) eic$replicate <- x$replicates[eic$analysis]
  
  if (!interactive) {
    .plot_features_static(eic, fts, legendNames, colorBy, xLab, yLab, title, showLegend, xlim, ylim, cex)
  } else {
    .plot_features_interactive(eic, fts, legendNames, colorBy, xLab, yLab, title, showLegend)
  }
}


#' @noRd
S7::method(map_features, MassSpecAnalyses) <- function(x,
                                                       analyses = NULL,
                                                       features = NULL,
                                                       mass = NULL,
                                                       mz = NULL,
                                                       rt = NULL,
                                                       mobility = NULL,
                                                       ppm = 20,
                                                       sec = 60,
                                                       millisec = 5,
                                                       filtered = FALSE,
                                                       legendNames = NULL,
                                                       xLab = NULL,
                                                       yLab = NULL,
                                                       title = NULL,
                                                       colorBy = "targets",
                                                       showLegend = TRUE,
                                                       xlim = 30,
                                                       ylim = 0.05,
                                                       cex = 0.6,
                                                       interactive = TRUE) {
  
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  if (grepl("replicates", colorBy)) fts$replicate <- x$replicates[fts$analysis]
  
  if (!interactive) {
    .map_features_static(fts, colorBy, legendNames, xLab, yLab, title, showLegend, xlim, ylim, cex)
  } else {
    .map_features_interactive(fts, colorBy, legendNames, xlim, ylim, xLab, yLab, title)
  }
}


#' @noRd
S7::method(plot_features_ms1, MassSpecAnalyses) <- function(x,
                                                            analyses = NULL,
                                                            features = NULL,
                                                            mass = NULL,
                                                            mz = NULL,
                                                            rt = NULL,
                                                            mobility = NULL,
                                                            ppm = 20,
                                                            sec = 60,
                                                            millisec = 5,
                                                            rtWindow = c(-2, 2),
                                                            mzWindow = c(-5, 100),
                                                            mzClust = 0.003,
                                                            presence = 0.8,
                                                            minIntensity = 1000,
                                                            filtered = FALSE,
                                                            useLoadedData = TRUE,
                                                            legendNames = NULL,
                                                            xLab = NULL,
                                                            yLab = NULL,
                                                            title = NULL,
                                                            colorBy = "targets",
                                                            interactive = TRUE) {
  
  ms1 <- get_features_ms1(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, 
                          rtWindow, mzWindow, mzClust, presence, minIntensity, filtered, useLoadedData)
  
  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }
  
  if (grepl("replicates", colorBy)) {
    ms1$replicate <- x$replicates[ms1$analysis]
  }
  
  if (!interactive) {
    .plot_spectra_ms1_static(ms1, legendNames, colorBy, title, xLab, yLab)
  } else {
    .plot_spectra_ms1_interactive(ms1, legendNames, colorBy, title, xLab, yLab)
  }
}


#' @noRd
S7::method(plot_features_ms2, MassSpecAnalyses) <- function(x,
                                                            analyses = NULL,
                                                            features = NULL,
                                                            mass = NULL,
                                                            mz = NULL,
                                                            rt = NULL,
                                                            mobility = NULL,
                                                            ppm = 20,
                                                            sec = 60,
                                                            millisec = 5,
                                                            isolationWindow = 1.3,
                                                            mzClust = 0.005,
                                                            presence = 0.8,
                                                            minIntensity = 0,
                                                            filtered = FALSE,
                                                            useLoadedData = TRUE,
                                                            legendNames = NULL,
                                                            xLab = NULL,
                                                            yLab = NULL,
                                                            title = NULL,
                                                            colorBy = "targets",
                                                            interactive = TRUE) {
  
  ms2 <- get_features_ms2(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, 
                          isolationWindow, mzClust, presence, minIntensity, filtered, useLoadedData)
  
  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }
  if (grepl("replicates", colorBy)) {
    ms2$replicate <- x$replicates[ms2$analysis]
  }
  
  if (!interactive) {
    .plot_spectra_ms2_static(ms2, legendNames, colorBy, title, xLab, yLab)
  } else {
    .plot_spectra_ms2_interactive(ms2, legendNames, colorBy, title, xLab, yLab)
  }
}


#' @noRd
S7::method(plot_groups, MassSpecAnalyses) <- function(x,
                                                      groups = NULL,
                                                      mass = NULL,
                                                      mz = NULL,
                                                      rt = NULL,
                                                      mobility = NULL,
                                                      ppm = 20,
                                                      sec = 60,
                                                      millisec = 5,
                                                      rtExpand = 15,
                                                      mzExpand = 0.005,
                                                      filtered = FALSE,
                                                      legendNames = NULL,
                                                      xLab = NULL,
                                                      yLab = NULL,
                                                      title = NULL,
                                                      colorBy = "targets",
                                                      showLegend = TRUE,
                                                      xlim = NULL,
                                                      ylim = NULL,
                                                      cex = 0.6,
                                                      interactive = TRUE) {
  
  fts <- get_features(x, analyses = NULL, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (grepl("targets", colorBy) & !isTRUE(legendNames)) {
    fts$name <- fts$group
    if (is.null(legendNames)) legendNames <- TRUE
  }
  
  plot_features(x,
    features = fts,
    rtExpand = rtExpand,
    mzExpand = mzExpand,
    filtered = filtered,
    legendNames = legendNames,
    xLab = xLab,
    yLab = yLab,
    title = title,
    colorBy = colorBy,
    showLegend = showLegend,
    xlim = xlim,
    ylim = ylim,
    cex = cex,
    interactive = interactive
  )
}


#' @noRd
S7::method(plot_groups_ms1, MassSpecAnalyses) <- function(x,
                                                          groups = NULL,
                                                          mass = NULL,
                                                          mz = NULL,
                                                          rt = NULL,
                                                          mobility = NULL,
                                                          ppm = 20,
                                                          sec = 60,
                                                          millisec = 5,
                                                          rtWindow = c(-2, 2),
                                                          mzWindow = c(-5, 90),
                                                          mzClustFeatures = 0.005,
                                                          presenceFeatures = 0.8,
                                                          minIntensityFeatures = 1000,
                                                          useLoadedData = TRUE,
                                                          mzClust = 0.005,
                                                          presence = 0.8,
                                                          minIntensity = 1000,
                                                          groupBy = "groups",
                                                          filtered = FALSE,
                                                          legendNames = NULL,
                                                          xLab = NULL,
                                                          yLab = NULL,
                                                          title = NULL,
                                                          colorBy = "targets",
                                                          interactive = TRUE) {
  
  if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
    groupBy <- "groups"
  } else {
    groupBy <- "replicates"
  }
  
  ms1 <- get_groups_ms1(x, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtWindow, mzWindow, mzClustFeatures, 
    presenceFeatures, minIntensityFeatures, useLoadedData, mzClust, presence, minIntensity, groupBy, filtered
  )
  
  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }
  
  if ("analyses" %in% colorBy) colorBy <- "replicates"
  
  if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"
  
  if (!interactive) {
    .plot_spectra_ms1_static(ms1, legendNames, colorBy, title, xLab, yLab)
  } else {
    .plot_spectra_ms1_interactive(ms1, legendNames, colorBy, title, xLab, yLab)
  }
}


#' @noRd
S7::method(plot_groups_ms2, MassSpecAnalyses) <- function(x,
                                                          groups = NULL,
                                                          mass = NULL,
                                                          mz = NULL,
                                                          rt = NULL,
                                                          mobility = NULL,
                                                          ppm = 20,
                                                          sec = 60,
                                                          millisec = 5,
                                                          isolationWindow = 1.3,
                                                          mzClustFeatures = 0.003,
                                                          presenceFeatures = 0.8,
                                                          minIntensityFeatures = 100,
                                                          useLoadedData = TRUE,
                                                          mzClust = 0.003,
                                                          presence = TRUE,
                                                          minIntensity = 100,
                                                          groupBy = "groups",
                                                          filtered = FALSE,
                                                          legendNames = NULL,
                                                          xLab = NULL,
                                                          yLab = NULL,
                                                          title = NULL,
                                                          colorBy = "targets",
                                                          interactive = TRUE) {
  
  if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
    groupBy <- "groups"
  } else {
    groupBy <- "replicates"
  }
  
  ms2 <- get_groups_ms2(x, groups, mass, mz, rt, mobility, ppm, sec, millisec, isolationWindow, mzClustFeatures,
    presenceFeatures, minIntensityFeatures, useLoadedData, mzClust, presence, minIntensity, groupBy, filtered
  )
  
  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }
  
  if ("analyses" %in% colorBy) colorBy <- "replicates"
  
  if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"
  
  if (!interactive) {
    .plot_spectra_ms2_static(ms2, legendNames, colorBy, title, xLab, yLab)
  } else {
    .plot_spectra_ms2_interactive(ms2, legendNames, colorBy, title, xLab, yLab)
  }
}


#' @noRd
S7::method(plot_groups_overview, MassSpecAnalyses) <- function(x,
                                                               analyses = NULL,
                                                               groups = NULL,
                                                               mass = NULL,
                                                               mz = NULL,
                                                               rt = NULL,
                                                               mobility = NULL,
                                                               ppm = 20,
                                                               sec = 60,
                                                               millisec = 5,
                                                               rtExpand = 120,
                                                               mzExpand = 0.005,
                                                               useLoadedData = TRUE,
                                                               filtered = FALSE,
                                                               legendNames = NULL,
                                                               title = NULL,
                                                               heights = c(0.35, 0.5, 0.15)) {
  
  fts <- get_features(x, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  eic <- get_features_eic(x, analyses = unique(fts$analysis), features = fts,
    rtExpand = rtExpand, mzExpand = mzExpand, filtered = filtered, useLoadedData = useLoadedData
  )
  
  intensity <- NULL
  
  eic <- eic[, `:=`(intensity = sum(intensity)), by = c("analysis", "polarity", "feature", "rt")][]
  
  if (nrow(eic) == 0) {
    message("\U2717 Traces and/or features not found for targets!")
    return(NULL)
  }
  
  if (is.character(legendNames) & length(legendNames) == length(unique(fts$group))) {
    leg <- legendNames
    names(leg) <- unique(fts$group)
    leg <- leg[fts$group]
  } else if (isTRUE(legendNames) & "name" %in% colnames(fts)) {
    leg <- fts$name
  } else {
    leg <- fts$group
  }
  
  names(leg) <- paste0(fts$feature, "_", fts$analysis)
  eic$uid <- paste0(eic$feature, "_", eic$analysis)
  fts$uid <- paste0(fts$feature, "_", fts$analysis)
  eic$var <- leg[eic$uid]
  fts$var <- leg
  
  analyses <- .check_analyses_argument(self$analyses, analyses)
  
  .plot_groups_overview_aux(fts, eic, heights, analyses)
}


#' @noRd
S7::method(plot_groups_profile, MassSpecAnalyses) <- function(x,
                                                              analyses = NULL,
                                                              groups = NULL,
                                                              mass = NULL,
                                                              mz = NULL,
                                                              rt = NULL,
                                                              mobility = NULL,
                                                              ppm = 20,
                                                              sec = 60,
                                                              millisec = 5,
                                                              filtered = FALSE,
                                                              normalized = TRUE,
                                                              legendNames = NULL,
                                                              yLab = NULL,
                                                              title = NULL) {
  
  fts <- get_features(x, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  if (!"polarity" %in% colnames(fts)) {
    polarities <- self$get_spectra_polarity()
    fts$polarity <- polarities[fts$analysis]
  }
  
  if (normalized && "intensity_rel" %in% colnames(fts)) fts$intensity <- as.numeric(fts$intensity_rel)
  
  if (is.character(legendNames) & length(legendNames) == length(unique(fts$group))) {
    leg <- legendNames
    names(leg) <- unique(fts$group)
    leg <- leg[fts$group]
    fts$var <- leg[fts$group]
  } else if (isTRUE(legendNames) & "name" %in% colnames(fts)) {
    leg <- fts$name
    fts$var <- fts$name
  } else {
    leg <- fts$group
    fts$var <- fts$group
  }
  
  u_leg <- unique(leg)
  
  colors <- .get_colors(u_leg)
  
  analyses <- .check_analyses_argument(self$analyses, analyses)
  
  showLeg <- rep(TRUE, length(u_leg))
  names(showLeg) <- u_leg
  
  rpls <- x$replicates
  
  plot <- plot_ly(fts, x = sort(unique(fts$analysis)))
  
  for (g in u_leg) {
    
    df <- fts[fts$var == g, ]
    
    if (!all(analyses %in% df$analysis)) {
      extra <- data.frame(
        "analysis" = analyses[!analyses %in% df$analysis],
        "polarity" = polarities[!analyses %in% df$analysis],
        "var" = g,
        "intensity" = 0
      )
      df <- rbind(df[, c("analysis", "var", "intensity", "polarity")], extra)
    }
    
    df <- df[order(df$analysis), ]
    
    if (normalized) {
      
      if (length(unique(df$polarity)) > 1) {
        
        for (p in unique(df$polarity)) {
          max_int <- max(df$intensity[df$polarity == p])
          if (max_int > 0) df$intensity[df$polarity == p] <- df$intensity[df$polarity == p] / max_int
        }
      } else {
        max_int <- max(df$intensity)
        if (max_int > 0) df$intensity <- df$intensity / max_int
      }
    }
    
    plot <- plot %>% add_trace(df,
      x = df$analysis,
      y = df$intensity,
      type = "scatter", mode = "lines",
      line = list(width = 0.5, color = colors[g], dash = "dash"),
      connectgaps = FALSE,
      name = g,
      legendgroup = g,
      showlegend = FALSE
    )
    
    df$replicate <- rpls[df$analysis]
    
    for (r in unique(df$replicate)) {
      df_r <- df[df$replicate %in%  r, ]
      
      plot <- plot %>% add_trace(df,
        x = df_r$analysis,
        y = df_r$intensity,
        type = "scatter", mode = "lines+markers",
        line = list(width = 1.5, color = colors[g]),
        marker = list(size = 5, color = colors[g]),
        connectgaps = FALSE,
        name = g,
        legendgroup = g,
        showlegend = showLeg[g]
      )
      
      showLeg[g] <- FALSE
    }
  }
  
  xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = NULL)
  
  if (!is.null(yLab)) {
    if (normalized) {
      yLab <- "Normalized intensity"
    } else {
      yLab <- "Intensity / counts"
    }
  }
  
  yaxis <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Normalized intensity",
    titlefont = list(size = 12, color = "black")
  )
  
  plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis)
  
  plot
}









#' @noRd
S7::method(plot_chromatograms, MassSpecAnalyses) <- function(x,
                                                             analyses = NULL,
                                                             chromatograms = NULL,
                                                             minIntensity = NULL,
                                                             useRawData = FALSE,
                                                             useLoadedData = TRUE,
                                                             xLab = NULL,
                                                             yLab = NULL,
                                                             title = NULL,
                                                             colorBy = "targets",
                                                             legendNames = NULL,
                                                             showLegend = TRUE,
                                                             xlim = NULL,
                                                             ylim = NULL,
                                                             cex = 0.6,
                                                             interactive = TRUE) {
  
  chromatograms <- get_chromatograms(x, analyses, chromatograms, minIntensity, useRawData, useLoadedData)
  
  if (nrow(chromatograms) == 0) {
    message("\U2717 Chromatograms not found for the analyses!")
    return(NULL)
  }
  
  if ("replicates" %in% colorBy) chromatograms$replicate <- x$replicates[chromatograms$analysis]
  
  pol_key <- c("positive", "negative", "nd")
  names(pol_key) <- c("1", "-1", "0")
  chromatograms$polarity <- as.character(chromatograms$polarity)
  chromatograms$polarity <- pol_key[chromatograms$polarity]
  
  if (!interactive) {
    .plot_spectra_eic_static(chromatograms, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_chromatograms_interactive(chromatograms, legendNames, colorBy, xLab, yLab, title, showLegend)
  }
}








#' @noRd
# S7::method(, MassSpecAnalyses) <- function(x, ) {
#   
# }


#' @noRd
# S7::method(, MassSpecAnalyses) <- function(x, ) {
#   
# }






# Utility functions -----

#' @noRd
.get_MassSpecAnalysis_from_files <- function(files = NULL) {
  if (!is.null(files)) {
    if (is.data.frame(files)) {
      if (all(c("path", "analysis") %in% colnames(files))) {
        files$file <- vapply(seq_len(nrow(files)), function(x) {
          list.files(files$path[x], pattern = files$analysis[x], full.names = TRUE, recursive = FALSE)
        }, "")
      }
      
      if ("file" %in% colnames(files)) {
        if ("replicate" %in% colnames(files)) {
          replicates <- as.character(files$replicate)
        } else if ("group" %in% colnames(files)) {
          replicates <- as.character(files$group)
        } else {
          replicates <- rep(NA_character_, nrow(files))
        }
        
        if ("blank" %in% colnames(files)) {
          blanks <- as.character(files$blank)
        } else {
          blanks <- rep(NA_character_, nrow(files))
        }
        
        files <- files$file
      } else {
        files <- NA_character_
      }
    } else {
      replicates <- rep(NA_character_, length(files))
      blanks <- rep(NA_character_, length(files))
    }
    
    possible_ms_file_formats <- ".mzML|.mzXML"
    
    valid_files <- vapply(files,
      FUN.VALUE = FALSE,
      function(x, possible_ms_file_formats) {
        if (!file.exists(x)) {
          return(FALSE)
        }
        if (FALSE %in% grepl(possible_ms_file_formats, x)) {
          return(FALSE)
        }
        TRUE
      }, possible_ms_file_formats = possible_ms_file_formats
    )
    
    if (!all(valid_files)) {
      warning("File/s not valid!")
      return(NULL)
    }
    
    names(replicates) <- as.character(files)
    names(blanks) <- as.character(files)
    
    analyses <- lapply(files, function(x) {
      cache <- .load_chache("parsed_ms_analyses", x)
      if (!is.null(cache$data)) {
        message("\U2139 ", basename(x), " analysis loaded from cache!")
        cache$data
      } else {
        message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
        ana <- rcpp_parse_ms_analysis(x)
        class_ana <- class(ana)[1]
        
        if (!class_ana %in% "MassSpecAnalysis") {
          message(" Not Done!")
          return(NULL)
        }
        
        message(" Done!")
        
        rpl <- replicates[x]
        
        if (is.na(rpl)) {
          rpl <- ana$name
          rpl <- sub("-[^-]+$", "", rpl)
        }
        
        ana$replicate <- rpl
        blk <- blanks[x]
        
        if (!is.na(blk)) ana$blank <- blk
        
        ana$blank <- blk
        
        if (!is.null(cache$hash)) {
          .save_cache("parsed_ms_analyses", ana, cache$hash)
          message("\U1f5ab Parsed analysis cached!")
        }
        
        ana
      }
    })
    
    names(analyses) <- vapply(analyses, function(x) x[["name"]], "")
    analyses <- analyses[order(names(analyses))]
    
    if (all(vapply(analyses, function(x) "MassSpecAnalysis" %in% is(x), FALSE))) {
      analyses
    } else {
      list()
    }
  } else {
    list()
  }
}
