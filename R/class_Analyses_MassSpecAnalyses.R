# MARK: MassSpecAnalyses
# MassSpecAnalyses -----
#' @export
#' @noRd
MassSpecAnalyses <- S7::new_class(
  "MassSpecAnalyses",
  package = "StreamFind",
  parent = Analyses,
  properties = list(

    # MARK: analyses
    ## analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),

    # MARK: replicates
    ## replicates -----
    replicates = S7::new_property(
      S7::class_character,
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
        for (i in seq_len(length(self))) {
          self@analyses[[i]]$replicate <- value[i]
        }
        if (self$has_results_nts) {
          if (self@results$NTS$has_features) {
            self@results$NTS$features@analysisInfo$group <- value
            if (self@results$NTS$has_groups) {
              self@results$NTS$features@features@analysisInfo$group <- value
            }
          }
        }
        self
      }
    ),

    # MARK: blanks
    ## blanks -----
    blanks = S7::new_property(
      S7::class_character,
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
        if (self$has_results_nts) {
          if (self@results$NTS$has_features) {
            self@results$NTS$features@analysisInfo$blank <- value
            if (self@results$NTS$has_groups) {
              self@results$NTS$features@features@analysisInfo$blank <- value
            }
          }
        }
        self
      }
    ),

    # MARK: concentrations
    ## concentrations -----
    concentrations = S7::new_property(
      S7::class_numeric,
      getter = function(self) vapply(self@analyses, function(x) x$concentration, 0),
      setter = function(self, value) {
        if (length(value) != length(self)) {
          warning("Length of concentrations not conform!")
          return(self)
        }
        if (!is.numeric(value)) {
          warning("Concentrations must be numeric!")
          return(self)
        }
        for (i in seq_len(length(self))) self@analyses[[i]]$concentration <- value[i]
        if (self$has_results_nts) {
          if (self@results$NTS$has_features) {
            self@results$NTS$features@analysisInfo$concentration <- value
            if (self@results$NTS$has_groups) {
              self@results$NTS$features@features@analysisInfo$concentration <- value
            }
          }
        }
        self
      }
    ),

    # MARK: types
    ## types -----
    types = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) x$type, NA_character_)
      }
    ),

    # MARK: files
    ## files -----
    files = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) x$file, NA_character_)
      }
    ),

    # MARK: formats
    ## formats -----
    formats = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) x$format, NA_character_)
      }
    ),

    # MARK: instruments
    ## instruments -----
    instruments = S7::new_property(
      S7::class_character,
      getter = function(self) {
        lapply(self@analyses, function(x) x$instrument)
      }
    ),

    # MARK: software
    ## software -----
    software = S7::new_property(
      S7::class_character,
      getter = function(self) {
        lapply(self@analyses, function(x) x$software)
      }
    ),

    # MARK: spectra_number
    ## spectra_number -----
    spectra_number = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) x$spectra_number, 0)
      }
    ),

    # MARK: spectra_headers
    ## spectra_headers -----
    spectra_headers = S7::new_property(
      S7::class_list,
      getter = function(self) {
        lapply(self@analyses, function(x) {
          res <- x$spectra_headers
          res$replicate <- x$replicate
          data.table::setcolorder(res, c("replicate"))
          res
        })
      }
    ),

    # MARK: spectra_mode
    ## spectra_mode -----
    spectra_mode = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) {
          if (x$spectra_number == 0) {
            return(NA_character_)
          }
          mode <- unique(x$spectra_headers$mode)
          mode[mode == 0] <- "unknown"
          mode[mode == 1] <- "profile"
          mode[mode == 2] <- "centroid"
          if (length(mode) > 1) mode <- paste(mode, collapse = ", ")
          mode
        }, NA_character_)
      }
    ),

    # MARK: spectra_level
    ## spectra_level -----
    spectra_level = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) {
          if (x$spectra_number == 0) {
            return(NA_character_)
          }
          levels <- unique(x$spectra_headers$level)
          if (length(levels) > 1) levels <- paste(levels, collapse = ", ")
          as.character(levels)
        }, NA_character_)
      }
    ),

    # MARK: spectra_lowest_mz
    ## spectra_lowest_mz -----
    spectra_lowest_mz = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) min(x$spectra_headers$lowmz), NA_real_)
      }
    ),

    # MARK: spectra_highest_mz
    ## spectra_highest_mz -----
    spectra_highest_mz = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) max(x$spectra_headers$highmz), NA_real_)
      }
    ),

    # MARK: spectra_lowest_rt
    ## spectra_lowest_rt -----
    spectra_lowest_rt = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) min(x$spectra_headers$rt), NA_real_)
      }
    ),

    # MARK: spectra_highest_rt
    ## spectra_highest_rt -----
    spectra_highest_rt = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) max(x$spectra_headers$rt), NA_real_)
      }
    ),

    # MARK: spectra_lowest_mobility
    ## spectra_highest_mobility -----
    spectra_highest_mobility = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) max(x$spectra_headers$mobility), NA_real_)
      }
    ),

    # MARK: spectra_lowest_mobility
    ## spectra_lowest_mobility -----
    spectra_lowest_mobility = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) min(x$spectra_headers$mobility), NA_real_)
      }
    ),

    # MARK: spectra_polarity
    ## spectra_polarity -----
    spectra_polarity = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) {
          if (x$spectra_number == 0) {
            return(NA_character_)
          }
          polarity <- unique(x$spectra_headers$polarity)
          if (length(polarity) > 1) {
            # tries to infer short polarity switching from scans
            polarities <- x$spectra_headers$polarity
            scans_pos <- length(polarities[polarities == 1])
            scans_neg <- length(polarities[polarities == -1])
            ratio <- scans_pos / scans_neg
            if (ratio > 1.5) {
              polarity <- 1
            } else if (ratio < 0.5) {
              polarity <- -1
            }
          }
          polarity[polarity == 0] <- "unkown"
          polarity[polarity == 1] <- "positive"
          polarity[polarity == -1] <- "negative"
          if (length(polarity) > 1) polarity <- paste(polarity, collapse = ", ")
          polarity
        }, NA_character_)
      }
    ),

    # MARK: spectra_tic
    ## spectra_tic -----
    spectra_tic = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        lapply(self@analyses, function(x) {
          res <- x$spectra_headers[, c("polarity", "level", "rt", "tic"), with = FALSE]
          res$replicate <- x$replicate
          colnames(res) <- c("polarity", "level", "rt", "intensity", "replicate")
          data.table::setcolorder(res, "replicate")
          res
        })
      }
    ),

    # MARK: spectra_bpc
    ## spectra_bpc -----
    spectra_bpc = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        lapply(self@analyses, function(x) {
          res <- x$spectra_headers[, c("polarity", "level", "rt", "bpmz", "bpint"), with = FALSE]
          res$replicate <- x$replicate
          colnames(res) <- c("polarity", "level", "rt", "mz", "intensity", "replicate")
          data.table::setcolorder(res, "replicate")
          res
        })
      }
    ),

    # MARK: chromatograms_number
    ## chromatograms_number -----
    chromatograms_number = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) x$chromatograms_number, 0)
      }
    ),
    
    # MARK: chromatograms_headers
    ## chromatograms_headers -----
    chromatograms_headers = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        lapply(self@analyses, function(x) {
          res <- x$chromatograms_headers
          res$replicate <- x$replicate
          data.table::setcolorder(res, c("replicate"))
          res
        })
      }
    ),

    # MARK: chromatograms_raw
    ## chromatograms_raw -----
    chromatograms_raw = S7::new_property(
      S7::class_list,
      getter = function(self) {
        StreamFind::Chromatograms(
          lapply(self@analyses, function(x) {
            if (nrow(x$chromatograms) > 0) {
              x$chromatograms
            } else {
              rcpp_parse_ms_chromatograms(x, idx = seq_along(x$number_chromatograms))
            }
          })
        )
      }
    ),

    # MARK: info
    ## info -----
    info = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
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
            "chromatograms" = vapply(self@analyses, function(x) round(x$chromatograms_number, digits = 0), 0),
            "concentration" = vapply(self@analyses, function(x) x$concentration, 0)
          )
          row.names(df) <- seq_len(nrow(df))
          df
        } else {
          data.frame()
        }
      }
    ),

    # MARK: has_ion_mobility
    ## has_ion_mobility -----
    has_ion_mobility = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self) == 0) {
          return(FALSE)
        }
        vapply(self@analyses, function(x) any(x$spectra_headers$mobility > 0), FALSE)
      }
    ),

    # MARK: has_results_nts
    ## has_results_nts -----
    has_results_nts = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self) == 0) {
          return(FALSE)
        }
        if (is.null(self@results[["NTS"]])) {
          return(FALSE)
        }
        if (!is(self@results[["NTS"]], "StreamFind::NTS")) {
          return(FALSE)
        }
        TRUE
      }
    ),
    
    # MARK: has_results_spectra
    ## has_results_spectra -----
    has_results_spectra = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self) == 0) {
          return(FALSE)
        }
        if (is.null(self@results[["MassSpecSpectra"]])) {
          return(FALSE)
        }
        if (!is(self@results[["MassSpecSpectra"]], "StreamFind::MassSpecSpectra")) {
          return(FALSE)
        }
        TRUE
      }
    ),
    
    # MARK: has_results_chromatograms
    ## has_results_chromatograms -----
    has_results_chromatograms = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self) == 0) {
          return(FALSE)
        }
        if (is.null(self@results[["Chromatograms"]])) {
          return(FALSE)
        }
        if (!is(self@results[["Chromatograms"]], "StreamFind::Chromatograms")) {
          return(FALSE)
        }
        TRUE
      }
    ),

    # MARK: NTS
    ## NTS -----
    NTS = S7::new_property(
      S7::class_list,
      getter = function(self) {
        if (self$has_results_nts) {
          return(self@results[["NTS"]])
        }
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::NTS")) {
          if (value@number_analyses > 0) {
            analyses_names <- unname(names(self))
            value_analyses_names <- sort(value@analyses_info$analysis)
            if (identical(analyses_names, value_analyses_names)) {
              self@results[[value@name]] <- value
            } else {
              # TODO check if some analyses are in engine and subset engine to match NTS
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

    # MARK: Spectra
    ## Spectra -----
    Spectra = S7::new_property(
      S7::class_list,
      getter = function(self) {
        if (self$has_results_spectra) {
          return(self@results[["MassSpecSpectra"]])
        }
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::MassSpecSpectra")) {
          if (!value$is_averaged) {
            analyses_names <- unname(names(self))
            value_analyses_names <- names(value$spectra)
            if (identical(analyses_names, value_analyses_names)) {
              self@results[[value@name]] <- value
            } else {
              warning("Analysis names do not match! Not done.")
            }
          } else if (value$is_averaged) {
            replicate_names <- unique(unname(self$replicates))
            value_analyses_names <- names(value$spectra)
            if (identical(replicate_names, value_analyses_names)) {
              self@results[[value@name]] <- value
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

    # MARK: Chromatograms
    ## Chromatograms -----
    Chromatograms = S7::new_property(
      S7::class_list,
      getter = function(self) {
        if (self$has_results_chromatograms) {
          return(self@results[["Chromatograms"]])
        }
        NULL
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::Chromatograms")) {
          if (!value$is_averaged) {
            analyses_names <- unname(names(self))
            value_analyses_names <- names(value$chromatograms)
            if (identical(analyses_names, value_analyses_names)) {
              self@results[["Chromatograms"]] <- value
            } else {
              warning("Analysis names do not match! Not done.")
            }
          } else if (value$is_averaged) {
            replicate_names <- unname(self$replicates)
            value_analyses_names <- names(value$chromatograms)
            if (identical(replicate_names, value_analyses_names)) {
              self@results[["Chromatograms"]] <- value
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

  # MARK: constructor
  ## constructor -----
  constructor = function(files = NULL, centroid = FALSE, levels = c(1, 2)) {
    analyses <- .get_MassSpecAnalysis_from_files(files, centroid, levels)
    S7::new_object(
      Analyses(),
      possible_formats = c("mzML", "mzXML", "d", "raw"),
      analyses = analyses
    )
  },

  # MARK: validator
  ## validator -----
  validator = function(self) {
    checkmate::assert_true(identical(self@possible_formats, c("mzML", "mzXML", "d", "raw")))
    if (length(self) > 0) {
      checkmate::assert_true(identical(names(self@analyses), unname(names(self))))
    }
    NULL
  }
)

# MARK: Methods
# Methods -----

# MARK: names
#' @export
#' @noRd
S7::method(names, MassSpecAnalyses) <- function(x) {
  vapply(x@analyses, function(x) x$name, NA_character_)
}

# MARK: add
#' @export
#' @noRd
S7::method(add, MassSpecAnalyses) <- function(x, value) {
  if (is.character(value)) {
    if (all(tools::file_ext(value) %in% x@possible_formats)) {
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
  if (any(vapply(value, function(a) a$name %in% names(x), FALSE))) {
    warning("Analysis names already exist!")
    return(x)
  }
  analyses <- c(x@analyses, value)
  analyses <- analyses[order(names(analyses))]
  if (length(analyses) > length(x@analyses)) {
    if (length(x$results) > 0) {
      warning("All results removed!")
      x@results <- list()
    }
  }
  x@analyses <- analyses
  x
}

# MARK: remove
#' @export
#' @noRd
S7::method(remove, MassSpecAnalyses) <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!names(x) %in% value]
    x@analyses <- x@analyses[order(names(x@analyses))]
    if (x@has_results_nts) x@results$NTS <- x@results$NTS[!names(x) %in% value]
    if (x@has_results_spectra) x$results$Spectra <- x$results$Spectra[!names(x) %in% value]
    if (x@has_results_chromatograms) {
      x$results$Chromatograms <- x$results$Chromatograms[!names(x) %in% value]
    }
  } else if (is.numeric(value)) {
    x@analyses <- x@analyses[-value]
    x@analyses <- x@analyses[order(names(x@analyses))]
    if (x@has_results_nts) x@results$NTS <- x@results$NTS[-value]
    if (x@has_results_spectra) x$results$Spectra <- x$results$Spectra[-value]
    if (x@has_results_chromatograms) {
      x$results$Chromatograms <- x$results$Chromatograms[-value]
    }
  }
  x
}

# MARK: `[`
#' @export
#' @noRd
S7::method(`[`, MassSpecAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (x@has_results_nts) x@results$NTS <- x@results$NTS[i]
  if (x@has_results_spectra) x@results$Spectra <- x@results$Spectra[i]
  if (x@has_results_chromatograms) x@results$Chromatograms <- x@results$Chromatograms[i]
  x
}

# MARK: `[<-`
#' @export
#' @noRd
S7::method(`[<-`, MassSpecAnalyses) <- function(x, i, value) {
  x <- add(x, value)
  x
}

# MARK: `[[`
#' @export
#' @noRd
S7::method(`[[`, MassSpecAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (x@has_results_nts) x@results$NTS <- x@results$NTS[i]
  if (x@has_results_spectra) x@results$Spectra <- x@results$Spectra[i]
  if (x@has_results_chromatograms) x@results$Chromatograms <- x@results$Chromatograms[i]
  x
}

# MARK: `[[<-`
#' @export
#' @noRd
S7::method(`[[<-`, MassSpecAnalyses) <- function(x, i, value) {
  x <- add(x, value)
  x
}

# MARK: as.list
#' @export
#' @noRd
S7::method(as.list, MassSpecAnalyses) <- function(x) {
  if (length(x@results) > 0) {
    # TODO make as.list method for each MassSpecAnalyses results
    browser()
  }
  list("analyses" = x@analyses, "results" = x@results)
}

# MARK: read
#' @export
#' @noRd
S7::method(read, MassSpecAnalyses) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      json <- jsonlite::fromJSON(file)
      browser()
      # TODO add read method for MassSpecAnalyses NTS and other results
      return(Analyses(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::MassSpecAnalyses")) {
      return(res)
    }
  }
  NULL
}

# MARK: get_spectra_tic
## get_spectra_tic -----
#' @export
#' @noRd
S7::method(get_spectra_tic, MassSpecAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          levels = c(1, 2),
                                                          rt = NULL) {
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
  value$replicate <- x$replicates[value$analysis]
  data.table::setcolorder(value, c("analysis", "replicate"))
  value
}

# MARK: get_spectra_bpc
## get_spectra_bpc -----
#' @export
#' @noRd
S7::method(get_spectra_bpc, MassSpecAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          levels = c(1, 2),
                                                          rt = NULL) {
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
  value$replicate <- x$replicates[value$analysis]
  data.table::setcolorder(value, c("analysis", "replicate"))
  value
}

# MARK: get_raw_spectra
## get_raw_spectra -----
#' @export
#' @noRd
S7::method(get_raw_spectra, MassSpecAnalyses) <- function(x,
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
                                                          minIntensityMS2 = 0) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }
  
  if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) minIntensityMS1 <- 0
  
  if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) minIntensityMS2 <- 0
  
  if (is.data.frame(mz)) {
    if ("analysis" %in% colnames(mz)) {
      analyses <- mz$analysis
    }
  }
  
  if (is.data.frame(mass)) {
    if ("analysis" %in% colnames(mass)) {
      analyses <- mass$analysis
    }
  }
  
  polarities <- x$spectra_polarity[analyses]
  
  targets <- MassSpecTargets(mass, mz, rt, mobility, ppm, sec, millisec, id, analyses, polarities)
  
  targets <- targets@targets
  
  if ("polarity" %in% colnames(targets) && nrow(targets) > 0) {
    for (i in seq_len(nrow(targets))) {
      if (targets$polarity[i] == "positive") targets$polarity[i] <- 1
      if (targets$polarity[i] == "negative") targets$polarity[i] <- -1
    }
  }
  
  num_cols <- c(
    "mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax"
  )
  
  if (all(apply(targets[, num_cols, with = FALSE], 1, function(z) sum(z, na.rm = TRUE)) != 0)) {
    if (TRUE %in% is.na(targets$mz)) {
      targets$mz[is.na(targets$mz)] <- 0
    }
    if (TRUE %in% is.na(targets$mzmax)) {
      targets$mzmax[is.na(targets$mzmax)] <- max(x$spectra_highest_mz[analyses])
    }
    if (TRUE %in% is.na(targets$mzmin)) {
      targets$mzmin[is.na(targets$mzmin)] <- min(x$spectra_lowest_mz[analyses])
    }
    if (TRUE %in% (targets$mzmax == 0)) {
      targets$mzmax[targets$mzmax == 0] <- max(x$spectra_highest_mz[analyses])
    }
    if (TRUE %in% is.na(targets$rt)) {
      targets$rt[is.na(targets$rt)] <- 0
    }
    if (TRUE %in% is.na(targets$rtmax)) {
      targets$rtmax[is.na(targets$rtmax)] <- max(x$spectra_highest_rt[analyses])
    }
    if (TRUE %in% is.na(targets$rtmin)) {
      targets$rtmin[is.na(targets$rtmin)] <- min(x$spectra_lowest_rt[analyses])
    }
    if (TRUE %in% (targets$rtmax == 0)) {
      targets$rtmax[targets$rtmax == 0] <- max(x$spectra_highest_rt[analyses])
    }
    if (TRUE %in% is.na(targets$mobility)) {
      targets$mobility[is.na(targets$mobility)] <- 0
    }
    if (TRUE %in% is.na(targets$mobilitymax) && any(x$has_ion_mobility[analyses])) {
      targets$mobilitymax[is.na(targets$mobilitymax)] <- max(
        x$spectra_highest_mobility[analyses], na.rm = TRUE
      )
    }
    if (TRUE %in% is.na(targets$mobilitymin) && any(x$has_ion_mobility[analyses])) {
      targets$mobilitymin[is.na(targets$mobilitymin)] <- min(
        x$spectra_lowest_mobility[analyses], na.rm = TRUE
      )
    }
    if (TRUE %in% (targets$mobilitymax == 0) && any(x$has_ion_mobility[analyses])) {
      targets$mobilitymax[targets$mobilitymax == 0] <- max(
        x$spectra_highest_mobility[analyses], na.rm = TRUE
      )
    }
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
  
  spec_list <- lapply(x$analyses[analyses], function(a, levels, targets) {
    if ("analysis" %in% colnames(targets)) {
      targets <- targets[targets$analysis %in% a$name, ]
    }
    
    cache <- lapply(seq_len(nrow(targets)), function(i) {
      .load_cache_sqlite(
        paste0("parsed_ms_spectra_", gsub("-|[/]|[.]|[() ]", "", targets$id[i])),
        a$file,
        levels,
        targets[i, ],
        minIntensityMS1,
        minIntensityMS2
      )
    })
    
    names(cache) <- targets$id
    cached_targets_sel <- vapply(cache, function(z) !is.null(z$data), FALSE)
    cached_targets <- cache[cached_targets_sel]
    no_cached_targets <- targets[!cached_targets_sel, ]
    
    if (nrow(no_cached_targets) > 0) {
      message("\U2699 Parsing spectra from ", basename(a$file), "...", appendLF = FALSE)
      
      if (nrow(no_cached_targets) == 1) {
        num_cols <- c(
          "mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax"
        )
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
              .save_cache_sqlite(paste0("parsed_ms_spectra_", gsub("-|[/]|[.]|[() ]", "", i)), spec[[i]], cache[[i]]$hash)
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
      cached_targets_dt <- data.table::rbindlist(
        lapply(cached_targets, function(z) data.table::as.data.table(z$data)),
        fill = TRUE
      )
      spec <- data.table::rbindlist(c(list(cached_targets_dt), list(spec)), fill = TRUE)
      message("\U2139 Spectra loaded from cache!")
    }
    if (nrow(spec) == 0) {
      return(data.table::data.table())
    }
    if ("id" %in% colnames(spec)) {
      data.table::setorder(spec, id, rt, mz)
    } else {
      data.table::setorder(spec, rt, mz)
    }
    gc()
    spec
  }, levels = levels, targets = targets)
  
  if (length(spec_list) == length(analyses)) {
    spec <- data.table::rbindlist(spec_list, fill = TRUE)
    if (nrow(spec) > 0) data.table::setcolorder(spec, c("analysis", "replicate"))
    spec
  } else {
    warning("Defined analyses not found!")
    data.table::data.table()
  }
}

# MARK: get_spectra_eic
## get_spectra_eic -----
#' @export
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
                                                          id = NULL) {
  eic <- get_raw_spectra(
    x,
    analyses,
    levels = 1,
    mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces = TRUE,
    isolationWindow = 1.3,
    minIntensityMS1 = 0,
    minIntensityMS2 = 0
  )
  
  if (nrow(eic) > 0) {
    intensity <- NULL
    eic <- data.table::as.data.table(eic)
    if (!"id" %in% colnames(eic)) eic$id <- NA_character_
    if (!"polarity" %in% colnames(eic)) eic$polarity <- 0
    cols_summary <- c("analysis", "replicate", "polarity", "id", "rt")
    intensity <- NULL
    mz = NULL
    eic <- eic[, .(intensity = max(intensity), mz = mean(mz)), by = cols_summary]
    sel_cols <- c("analysis", "replicate", "id", "polarity", "rt", "mz", "intensity")
    eic <- eic[, sel_cols, with = FALSE]
    eic <- unique(eic)
  }
  
  eic
}

# MARK: get_spectra_ms1
## get_spectra_ms1 -----
#' @export
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
                                                          minIntensity = 1000) {
  ms1 <- get_raw_spectra(
    x,
    analyses,
    levels = 1,
    mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces = TRUE,
    minIntensityMS1 = minIntensity,
    minIntensityMS2 = 0
  )
  
  if (nrow(ms1) == 0) {
    return(ms1)
  }
  
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
  
  if (!is.numeric(mzClust)) mzClust <- 0.01
  
  ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id, "_", ms1$polarity)
  
  ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, FALSE)
  
  ms1_df <- data.table::rbindlist(ms1_list, fill = TRUE)
  
  ms1_df <- ms1_df[order(ms1_df$mz), ]
  
  ms1_df <- ms1_df[order(ms1_df$id), ]
  
  ms1_df <- ms1_df[order(ms1_df$analysis), ]
  
  ms1_df$replicate <- x$replicates[ms1_df$analysis]
  
  data.table::setcolorder(ms1_df, c("analysis", "replicate"))
  
  ms1_df
}

# MARK: get_spectra_ms2
## get_spectra_ms2 -----
#' @export
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
                                                          minIntensity = 0) {
  ms2 <- get_raw_spectra(
    x,
    analyses,
    levels = 2,
    mass, mz, rt, mobility, ppm, sec, millisec, id,
    isolationWindow = isolationWindow,
    allTraces = FALSE,
    minIntensityMS1 = 0,
    minIntensityMS2 = minIntensity
  )
  
  if (nrow(ms2) == 0) {
    return(ms2)
  }
  
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
  
  if (!is.numeric(mzClust)) mzClust <- 0.01
  
  ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id, "_", ms2$polarity)
  
  ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, FALSE)
  
  ms2_df <- data.table::rbindlist(ms2_list, fill = TRUE)
  
  ms2_df <- ms2_df[order(ms2_df$mz), ]
  
  ms2_df <- ms2_df[order(ms2_df$id), ]
  
  ms2_df <- ms2_df[order(ms2_df$analysis), ]
  
  ms2_df$replicate <- x$replicates[ms2_df$analysis]
  
  data.table::setcolorder(ms2_df, c("analysis", "replicate"))
  
  ms2_df
}

# MARK: plot_spectra_tic
## plot_spectra_tic -----
#' @export
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
                                                           downsize = 1,
                                                           interactive = TRUE,
                                                           renderEngine = "webgl") {
  intensity <- NULL
  level <- NULL
  
  tic <- get_spectra_tic(x, analyses, levels, rt)
  tic$rt <- floor(tic$rt / downsize) * downsize
  groupCols <- c("rt", "analysis", "replicate", "polarity", "level")
  tic <- tic[, .(intensity = mean(intensity)), by = groupCols]
  
  if (nrow(tic) == 0) {
    message("\U2717 TIC not found for the analyses!")
    return(NULL)
  }
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  tic <- .make_colorBy_varkey(tic, colorBy, legendNames)
  
  tic$loop <- paste0(tic$analysis, tic$replicate, tic$id, tic$var)
  
  cl <- .get_colors(unique(tic$var))
  
  if (!interactive) {
    ggplot2::ggplot(tic, ggplot2::aes(x = rt, y = intensity, group = loop)) + 
      ggplot2::geom_line(ggplot2::aes(color = var)) + 
      ggplot2::scale_color_manual(values = cl) + 
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
    
    loop <- NULL
    
    plot <- tic %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~paste(
          "<br>analysis: ", analysis,
          "<br>replicate: ", replicate,
          "<br>id: ", id,
          "<br>polarity: ", polarity,
          "<br>level: ", level,
          "<br>rt: ", rt,
          "<br>intensity: ", intensity
        ),
        hoverinfo = "text"
      ) %>% plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    
    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }
    
    plot
  }
}

# MARK: plot_spectra_bpc
## plot_spectra_bpc -----
#' @export
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
                                                           interactive = TRUE,
                                                           renderEngine = "webgl") {
  bpc <- get_spectra_bpc(x, analyses, levels, rt)
  
  if (nrow(bpc) == 0) {
    message("\U2717 BPC not found for the analyses!")
    return(NULL)
  }
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  bpc <- .make_colorBy_varkey(bpc, colorBy, legendNames)
  
  bpc$loop <- paste0(bpc$analysis, bpc$replicate, bpc$id, bpc$var)
  
  cl <- .get_colors(unique(bpc$var))
  
  if (!interactive) {
    ggplot2::ggplot(bpc, ggplot2::aes(x = rt, y = intensity, group = loop)) + 
      ggplot2::geom_line(ggplot2::aes(color = var)) + 
      ggplot2::scale_color_manual(values = cl) + 
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
    
    loop <- NULL
    
    plot <- bpc %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~paste(
          "<br>analysis: ", analysis,
          "<br>replicate: ", replicate,
          "<br>id: ", id,
          "<br>polarity: ", polarity,
          "<br>level: ", level,
          "<br>mz: ", mz,
          "<br>rt: ", rt,
          "<br>intensity: ", intensity
        ),
        hoverinfo = "text"
      ) %>% plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    
    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }
    
    plot
  }
}

# MARK: plot_spectra_eic
## plot_spectra_eic -----
#' @export
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
                                                           interactive = TRUE,
                                                           renderEngine = "webgl") {
  eic <- get_spectra_eic(x, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id)
  
  if (nrow(eic) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  eic <- .make_colorBy_varkey(eic, colorBy, legendNames)
  
  eic$loop <- paste0(eic$analysis, eic$replicate, eic$id, eic$var)
  
  cl <- .get_colors(unique(eic$var))
  
  if (!interactive) {
    ggplot2::ggplot(eic, ggplot2::aes(x = rt, y = intensity, group = loop)) + 
      ggplot2::geom_line(ggplot2::aes(color = var)) +
      ggplot2::geom_point(ggplot2::aes(color = var), size = 1) +
      ggplot2::scale_color_manual(values = cl) + 
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
    
    loop <- NULL
    
    plot <- eic %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~paste(
          "<br>analysis: ", analysis,
          "<br>replicate: ", replicate,
          "<br>id: ", id,
          "<br>polarity: ", polarity,
          "<br>mz: ", mz,
          "<br>rt: ", rt,
          "<br>intensity: ", intensity
        ),
        hoverinfo = "text"
      ) %>% plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    
    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }
    
    plot
  }
}

# MARK: plot_spectra_xic
## plot_spectra_xic -----
#' @export
#' @noRd
S7::method(plot_spectra_xic, MassSpecAnalyses) <- function(x,
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
                                                           plotTargetMark = TRUE,
                                                           targetsMark = NULL,
                                                           ppmMark = 5,
                                                           secMark = 10,
                                                           numberRows = 1,
                                                           renderEngine = "webgl") {
  xic <- get_raw_spectra(
    x,
    analyses,
    levels = 1,
    mass,
    mz, rt, mobility, ppm, sec, millisec, id,
    allTraces = TRUE,
    isolationWindow = 1.3,
    minIntensityMS1 = 0,
    minIntensityMS2 = 0
  )
  
  if (nrow(xic) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }
  
  if (!"id" %in% colnames(xic)) xic$id <- NA_character_
  
  ids <- unique(xic$id)
  if (is.character(legendNames) & length(legendNames) == length(ids)) {
    names(legendNames) <- ids
    xic$id <- legendNames[xic$id]
  }
  
  if (plotTargetMark) {
    plotTargetMark <- FALSE
    if (is.data.frame(targetsMark)) {
      if (nrow(targetsMark) == length(ids) &&
          "mz" %in% colnames(targetsMark) &&
          "rt" %in% colnames(targetsMark)) {
        tgmMZ <- as.numeric(targetsMark$mz)
        names(tgmMZ) <- ids
        tgmRT <- as.numeric(targetsMark$rt)
        names(tgmRT) <- ids
        xic$mz_id <- tgmMZ[xic$id]
        xic$rt_id <- tgmRT[xic$id]
        
        plotTargetMark <- TRUE
      }
    }
  }
  
  ids <- unique(xic$id)
  
  xic <- split(xic, by = "id")
  
  colors <- colorRamp(c("#383E47", "#5E8CAA", "#16B9E5", "#16E5C9", "#16E54C"))
  
  line <- list(
    type = "line",
    line = list(color = "red", dash = "dash", width = 0.5),
    xref = "x",
    yref = "y"
  )
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Retention time / seconds",
    titlefont = list(size = 10, color = "black"),
    tickfont = list(size = 10)
  )
  
  yaxis1 <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 10, color = "black"),
    tickfont = list(size = 10)
  )
  
  yaxis2 <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = paste("<i>m/z</i> / Da"),
    titlefont = list(size = 10, color = "black"),
    tickfont = list(size = 10)
  )
  
  mainPlot <- list()
  
  for (t in ids) {
    plotList <- list()
    vline1 <- list()
    vline2 <- list()
    hline <- list()
    rect <- list()
    
    xic_s <- xic[[t]]
    
    rtmin <- min(xic_s$rt, na.rm = TRUE)
    rtmax <- max(xic_s$rt, na.rm = TRUE)
    mzmin <- min(xic_s$mz, na.rm = TRUE)
    mzmax <- max(xic_s$mz, na.rm = TRUE)
    maxInt <- max(xic_s$intensity, na.rm = TRUE) * 1.1
    
    xic_s <- split(xic_s, by = "analysis")
    
    if (any(unlist(lapply(xic_s, nrow)) > 20000)) {
      warning(paste0(
        "The MS area to be plotted seems rather large. ",
        "It is suggested to restrict the data first using",
        " narrow mz and rt deviations."
      ))
      return(NULL)
    }
    
    for (s in seq_len(length(xic_s))) {
      temp <- xic_s[[s]]
      
      if (plotTargetMark && is.numeric(temp$mz_id) && is.numeric(temp$rt_id)) {
        plotTargetMark_loop <- TRUE
        
        vline1 <- list(
          x0 = temp$rt_id[1],
          x1 = temp$rt_id[1],
          y0 = 0,
          y1 = max(temp$intensity, na.rm = TRUE)
        )
        
        vline2 <- list(
          x0 = temp$rt_id[1],
          x1 = temp$rt_id[1],
          y0 = mzmin,
          y1 = mzmax
        )
        
        hline <- list(
          x0 = min(temp$rt, na.rm = TRUE),
          x1 = max(temp$rt, na.rm = TRUE),
          y0 = temp$mz_id[1],
          y1 = temp$mz_id[1]
        )
        
        rect <- list(
          type = "rect",
          fillcolor = "red",
          line = list(color = "red"),
          opacity = 0.1,
          x0 = temp$rt_id[1] - secMark,
          x1 = temp$rt_id[1] + secMark,
          xref = "x",
          y0 = temp$mz_id[1] - ((ppmMark / 1E6) * temp$mz_id[1]),
          y1 = temp$mz_id[1] + ((ppmMark / 1E6) * temp$mz_id[1]),
          yref = "y"
        )
      } else {
        plotTargetMark_loop <- FALSE
      }
      
      p1 <- plot_ly(
        data = temp, x = temp$rt, y = temp$intensity,
        type = "scatter", mode = "markers",
        color = temp$intensity, colors = colors,
        marker = list(size = 8, line = list(color = "white", width = 0.5)),
        name = paste0(s, "p1")
      )
      
      if (plotTargetMark_loop) {
        p1 <- p1 %>% plotly::layout(shapes = c(vline1, line))
      }
      
      p1 <- p1 %>% add_annotations(
        text = paste(unique(temp$analysis), t, sep = " - "),
        x = 0.05, y = 1, yref = "paper", xref = "paper",
        xanchor = "left", yanchor = "bottom", align = "center",
        showarrow = FALSE, font = list(size = 10)
      )
      
      p2 <- plot_ly(
        temp,
        x = temp$rt, y = temp$mz,
        type = "scatter", mode = "markers",
        color = temp$intensity, colors = colors,
        marker = list(size = 8, line = list(color = "white", width = 0.5)),
        name = paste0(s, "p2")
      )
      
      if (plotTargetMark_loop) {
        p2 <- p2 %>% plotly::layout(
          shapes = list(c(vline2, line), c(hline, line), rect)
        )
      }
      
      p1 <- p1 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis1)
      p2 <- p2 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis2)
      
      plotList[[paste0("p1", s)]] <- p1
      plotList[[paste0("p2", s)]] <- p2
    }
    
    plotList <- plotList[order(names(plotList))]
    
    plot <- subplot(
      plotList,
      nrows = 2,
      margin = 0.01,
      shareX = TRUE,
      shareY = TRUE,
      which_layout = "merge"
    )
    
    plot <- hide_colorbar(plot)
    
    plot <- hide_legend(plot)
    
    mainPlot[[t]] <- plot
  }
  
  if (length(ids) > 1) {
    finalplot <- subplot(
      mainPlot,
      titleY = TRUE, titleX = TRUE,
      nrows = numberRows,
      margin = 0.05,
      shareY = FALSE,
      shareX = FALSE,
      which_layout = "merge"
    )
  } else {
    finalplot <- mainPlot[[1]]
  }
  
  if (renderEngine %in% "webgl") {
    finalplot <- finalplot %>% plotly::toWebGL()
  }
  
  finalplot
}

# MARK: plot_spectra_ms1
## plot_spectra_ms1 -----
#' @export
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
  ms1 <- get_spectra_ms1(
    x, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id, mzClust, presence, minIntensity
  )
  
  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }
  
  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)
  
  ms1$loop <- paste0(ms1$analysis, ms1$replicate, ms1$id, ms1$var)
  
  cl <- .get_colors(unique(ms1$var))
  
  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    plot <- ggplot2::ggplot(ms1, ggplot2::aes(x = mz, y = intensity, group = loop)) +
      ggplot2::geom_segment(ggplot2::aes(xend = mz, yend = 0, color = var), linewidth = 1)
    
    if (showText) {
      plot <- plot + ggplot2::geom_text(
        ggplot2::aes(label = round(mz, 4)),
        vjust = 0.2, hjust = -0.2, angle = 90, size = 2, show.legend = FALSE
      )
    }
    
    plot <- plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(ms1$intensity) * 1.5)) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
    plot
    
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)
    
    title <- list(text = title, font = list(size = 12, color = "black"))
    
    xaxis <- list(
      linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms1$mz) / 10), -1),
      ticks = "outside"
    )
    
    yaxis <- list(
      linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms1$intensity) * 1.5)
    )
    
    loop <- NULL
    
    plot <- ms1 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = 0.01)),
        text = ~paste0(round(mz, digits = 4), "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>% plotly::layout(
        bargap = 1,
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )
    
    plot
  }
}

# MARK: plot_spectra_ms2
## plot_spectra_ms2 -----
#' @export
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
                                                           showText = TRUE,
                                                           interactive = TRUE) {
  ms2 <- get_spectra_ms2(
    x, analyses, mass, mz, rt, mobility, ppm, sec, millisec, id,
    isolationWindow, mzClust, presence, minIntensity
  )
  
  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }
  
  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)
  
  loop <- NULL
  
  ms2$loop <- paste0(ms2$analysis, ms2$replicate, ms2$id, ms2$var)
  
  cl <- .get_colors(unique(ms2$var))
  
  if (showText) {
    ms2$text_string <- paste0(round(ms2$mz, 4))
    ms2$text_string[ms2$is_pre] <- paste0("Pre ", ms2$text_string[ms2$is_pre])
  } else {
    ms2$text_string <- ""
  }
  
  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ms2$linesize <- 1
    ms2$linesize[ms2$is_pre] <- 2
    
    plot <- ggplot2::ggplot(ms2, ggplot2::aes(x = mz, y = intensity, group = loop)) +
      ggplot2::geom_segment(ggplot2::aes(xend = mz, yend = 0, color = var, linewidth = linesize))
    
    if (showText) {
      plot <- plot + ggplot2::geom_text(
        ggplot2::aes(label = text_string),
        vjust = 0.2, hjust = -0.2, angle = 90, size = 2, show.legend = FALSE
      )
    }
    
    plot <- plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(ms2$intensity) * 1.5)) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_linewidth_continuous(range = c(1, 2), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
    plot
    
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ms2$linesize <- 0.01
    ms2$linesize[ms2$is_pre] <- 2
    
    ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)
    
    title <- list(text = title, font = list(size = 12, color = "black"))
    
    xaxis <- list(
      linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms2$mz) / 10), -1),
      ticks = "outside"
    )
    
    yaxis <- list(
      linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms2$intensity) * 1.5)
    )
    
    plot <- ms2 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = ~linesize)),
        text = ~paste0(text_string, "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>% plotly::layout(
        bargap = 1,
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )
    
    plot
  }
}

# MARK: get_raw_chromatograms
## get_raw_chromatograms -----
#' @export
#' @noRd
S7::method(get_raw_chromatograms, MassSpecAnalyses) <- function(x,
                                                                analyses = NULL,
                                                                chromatograms = NULL,
                                                                rtmin = 0,
                                                                rtmax = 0,
                                                                minIntensity = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }

  chroms_list <- lapply(x$analyses[analyses], function(z, chromatograms) {
    if (nrow(z$chromatograms_headers) == 0) {
      return(data.frame())
    }
    
    idx <- z$chromatograms_headers$index
    
    if (is.numeric(chromatograms)) {
      idx <- idx[chromatograms + 1]
    } else if (is.character(chromatograms)) {
      cid <- z$chromatograms_headers$id
      which_chroms <- cid %in% chromatograms
      idx <- idx[which_chroms]
    } else if (!is.null(chromatograms)) {
      return(data.table::data.table())
    }
    
    cache <- StreamFind:::.load_cache_sqlite("parsed_ms_chromatograms", z$file, idx)
    
    if (!is.null(cache$data)) {
      message("\U2139 Chromatograms loaded from cache!")
      return(cache$data)
    }
    
    message("\U2699 Parsing chromatograms from ", basename(z$file), "...", appendLF = FALSE)
    
    chrom <- rcpp_parse_ms_chromatograms(z, idx)
    
    message(" Done!")
    
    if (nrow(chrom) == 0) {
      warning("Parsing chromatograms failed!")
      return(data.table::data.table())
    }
    
    if (!"analysis" %in% colnames(chrom)) chrom$analysis <- z$name
    
    if (!"replicate" %in% colnames(chrom)) chrom$replicate <- z$replicate
    
    if (!is.null(cache$hash)) {
      StreamFind:::.save_cache_sqlite("parsed_ms_chromatograms", chrom, cache$hash)
      message("\U1f5ab Parsed chromatograms cached!")
    }
    
    chrom
  }, chromatograms = chromatograms)
  
  if (length(chroms_list) == length(analyses)) {
    chroms <- data.table::rbindlist(chroms_list, fill = TRUE)
    
    if (nrow(chroms) > 0) setcolorder(chroms, c("analysis", "replicate"))
    
    if (is.numeric(minIntensity)) chroms <- chroms[chroms$intensity > minIntensity, ]
    
    if (is.numeric(rtmin) && is.numeric(rtmax)) {
      if (rtmax > 0) chroms <- chroms[chroms$rt >= rtmin & chroms$rt <= rtmax]
    }
    
    chroms
  } else {
    warning("Defined analyses or chromatograms not found!")
    data.table::data.table()
  }
}

# MARK: Spectra Methods
# Spectra Methods ------

# MARK: load_spectra
## load_spectra -----
#' @export
#' @noRd
S7::method(load_spectra, MassSpecAnalyses) <- function(x,
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
                                                       minIntensityMS2 = 0) {
  cache <- .load_cache_sqlite(
    "load_spectra",
    x, analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces, isolationWindow, minIntensityMS1, minIntensityMS2
  )
  
  if (!is.null(cache$data)) {
    x$Spectra <- cache$data
    message("\U2139 Spectra loaded from cache!")
    return(x)
  }
  
  spec <- get_raw_spectra(
    x,
    analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces, isolationWindow, minIntensityMS1, minIntensityMS2
  )
  
  if (nrow(spec) > 0) {
    split_vector <- spec$analysis
    spec$analysis <- NULL
    spec$replicate <- NULL
    spec <- split(spec, split_vector)
    
    for (a in names(x)) {
      if (!a %in% names(spec)) {
        spec[[a]] <- data.table::data.table()
      }
    }
    
    spec <- spec[names(x)]
    
    spec <- StreamFind::MassSpecSpectra(
      spec,
      replicates = x$replicates,
      is_averaged = FALSE
    )
    if (!is.null(cache$hash)) {
      .save_cache_sqlite("load_spectra", spec, cache$hash)
      message("\U1f5ab Spectra cached!")
    }
    x$Spectra <- spec
  } else {
    warning("Not done! Spectra not found.")
  }
  x
}

# MARK: Chromatograms
# Chromatograms ------

# MARK: load_chromatograms
## load_chromatograms -----
#' @export
#' @noRd
S7::method(load_chromatograms, MassSpecAnalyses) <- function(x,
                                                             analyses = NULL,
                                                             chromatograms = NULL,
                                                             rtmin = 0,
                                                             rtmax = 0,
                                                             minIntensity = NULL) {
  chroms <- get_raw_chromatograms(
    x,
    analyses,
    chromatograms,
    rtmin,
    rtmax,
    minIntensity
  )
  
  if (nrow(chroms) > 0) {
    split_vector <- chroms$analysis
    chroms$analysis <- NULL
    chroms$replicate <- NULL
    chroms <- split(chroms, split_vector)
    chroms <- StreamFind::Chromatograms(
      chroms,
      replicates = x$replicates[names(chroms)],
      is_averaged = FALSE
    )
    x$Chromatograms <- chroms
  } else {
    warning("Not done! Chromatograms not found.")
  }
  
  x
}

# MARK: NTS
# NTS ------

# MARK: get_matrix_suppression
## get_matrix_suppression -----
#' @export
#' @noRd
S7::method(get_matrix_suppression, MassSpecAnalyses) <- function(x, rtWindow = 10) {
  mpList <- .calculate_tic_matrix_suppression(x, rtWindow)
  if (is.null(mpList)) {
    return(data.table::data.table())
  }
  data.table::rbindlist(mpList, fill = TRUE)
}

# MARK: plot_matrix_suppression
## plot_matrix_suppression -----
#' @export
#' @noRd
S7::method(plot_matrix_suppression, MassSpecAnalyses) <- function(x,
                                                                  analyses = NULL,
                                                                  rtWindow = 10,
                                                                  xLab = NULL,
                                                                  yLab = NULL,
                                                                  title = NULL,
                                                                  colorBy = "analyses",
                                                                  legendNames = NULL,
                                                                  downsize = 1,
                                                                  interactive = TRUE,
                                                                  renderEngine = "webgl") {
  mp <- get_matrix_suppression(x, rtWindow)
  if (nrow(mp) == 0) {
    message("\U2717 TIC matrix suppression not found for the analyses!")
    return(NULL)
  }
  if (!"id" %in% colnames(mp)) mp$id <- mp$analysis
  mp$intensity <- mp$mp
  if (is.null(yLab)) yLab <- "Supression Factor"
  if (is.null(xLab)) xLab <- "Retention time / seconds"

  mp <- .make_colorBy_varkey(mp, colorBy, legendNames)
  mp$loop <- paste0(mp$analysis, mp$replicate, mp$id, mp$var)
  cl <- .get_colors(unique(mp$var))
  
  if (!interactive) {
    ggplot2::ggplot(mp, ggplot2::aes(x = rt, y = intensity, group = loop)) + 
      ggplot2::geom_line(ggplot2::aes(color = var)) + 
      ggplot2::scale_color_manual(values = cl) + 
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
    
    loop <- NULL
    
    plot <- mp %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~paste(
          "<br>analysis: ", analysis,
          "<br>replicate: ", replicate,
          "<br>id: ", id,
          "<br>polarity: ", polarity,
          "<br>level: ", level,
          "<br>rt: ", rt,
          "<br>suppression: ", intensity
        ),
        hoverinfo = "text"
      ) %>% plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    
    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }
    
    plot
  }
}

# MARK: get_components
## get_components -----
#' @export
#' @noRd
S7::method(get_components, MassSpecAnalyses) <- function(x,
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
  if (!x$has_results_nts) {
    warning("No NTS results found!")
    return(data.table::data.table())
  }

  if (!x$NTS$has_features) {
    warning("Features not found!")
    return(data.table::data.table())
  }

  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for targets!")
    return(data.table::data.table())
  }

  fts$uid <- paste0(fts$analysis, "-", fts$feature)

  if ("name" %in% colnames(fts)) names_uid <- fts[, c("name", "uid"), with = FALSE]

  all_fts <- x$NTS$feature_list

  all_fts <- Map(function(x, y) {
    x$analysis <- y
    x
  }, all_fts, names(all_fts))

  all_fts <- lapply(all_fts, function(z, fts) {
    sel <- vapply(z[["annotation"]], function(k) nrow(k) > 0, FALSE)
    z <- z[sel, ]
    cf <- vapply(z[["annotation"]], function(k) k$component_feature, "")
    z$uid <- paste0(z$analysis, "-", cf)
    z <- z[z$uid %in% fts$uid | z$feature %in% fts$feature, ]
    update_fts_uid <- match(z$feature, fts$feature)
    update_fts_uid <- update_fts_uid[!is.na(update_fts_uid)]
    z$uid[z$feature %in% fts$feature] <- fts$uid[update_fts_uid]
    if ("name" %in% colnames(fts)) {
      z$name <- names_uid$name[match(z$uid, names_uid$uid)]
    }
    z$analysis <- NULL
    z
  }, fts = fts)

  all_fts <- lapply(all_fts, function(z) {
    if (nrow(z) == 0) {
      return(data.table::data.table())
    }
    annotation <- lapply(z[["annotation"]], function(k) data.table::as.data.table(k))
    annotation <- data.table::rbindlist(annotation)
    feature <- NULL
    z <- z[annotation, on = .(feature)]
    z$annotation <- NULL
    z <- z[order(z$component_feature), ]
    z
  })

  all_fts <- data.table::rbindlist(all_fts, idcol = "analysis", fill = TRUE)

  data.table::setnames(all_fts, "component_feature", "component")

  if ("group" %in% colnames(all_fts)) {
    groups <- fts$group
    names(groups) <- fts$uid
    groups <- groups[!is.na(groups)]
    all_fts$group <- groups[all_fts$uid]
    data.table::setcolorder(all_fts, c("analysis", "component", "feature", "group"))
  } else {
    data.table::setcolorder(all_fts, c("analysis", "component", "feature"))
  }

  all_fts$uid <- NULL
  all_fts
}

# MARK: get_suspects
## get_suspects -----
#' @export
#' @noRd
S7::method(get_suspects, MassSpecAnalyses) <- function(x,
                                                       analyses = NULL,
                                                       database = NULL,
                                                       features = NULL,
                                                       mass = NULL,
                                                       mz = NULL,
                                                       rt = NULL,
                                                       mobility = NULL,
                                                       ppm = 5,
                                                       sec = 10,
                                                       millisec = 5,
                                                       ppmMS2 = 10,
                                                       minFragments = 3,
                                                       isolationWindow = 1.3,
                                                       mzClust = 0.003,
                                                       presence = 0.8,
                                                       minIntensity = 0,
                                                       filtered = FALSE,
                                                       onGroups = TRUE) {
  if (!x$has_results_nts) {
    warning("No NTS results found!")
    return(data.table::data.table())
  }

  if (!x$NTS$has_features) {
    warning("Features not found!")
    return(data.table::data.table())
  }

  if (is.null(database)) {
    features <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)

    if (nrow(features) == 0) {
      message("\U2717 Features not found for targets!")
      return(data.table::data.table())
    }

    features[["name"]] <- NULL

    if ("suspects" %in% colnames(features)) {
      sel <- vapply(features$suspects, function(z) {
        if (length(z) > 0) {
          if (is.data.frame(z)) {
            if (nrow(z) > 0) {
              return(TRUE)
            }
          }
        }
        FALSE
      }, FALSE)

      if (any(sel)) {
        features <- features[sel, ]

        suspects_l <- features[["suspects"]]

        suspects <- lapply(seq_len(length(suspects_l)), function(z, suspects_l, features) {
          temp <- suspects_l[[z]]

          temp_ft <- features[z, ]
          temp_ft[["suspects"]] <- NULL
          temp_ft$rt <- NULL
          temp_ft$intensity <- NULL
          temp_ft$area <- NULL
          temp_ft$mass <- NULL

          if ("group" %in% colnames(temp)) {
            temp <- merge(temp, temp_ft, by = c("feature", "group"), all = TRUE)
          } else {
            temp <- merge(temp, temp_ft, by = "feature", all = TRUE)
          }

          data.table::setcolorder(temp, c("analysis", "replicate"))

          temp
        }, suspects_l = suspects_l, features = features)

        suspects <- data.table::rbindlist(suspects, fill = TRUE)
      } else {
        warning("Suspects were not found! Run suspect_screening or give a database for searching for suspects.")
        return(data.table::data.table())
      }
    } else {
      warning("Suspects were not found! Run suspect_screening or give a database.")
      return(data.table::data.table())
    }
  } else {
    database <- data.table::as.data.table(database)

    valid_db <- FALSE

    if (is.data.frame(database)) {
      database <- data.table::as.data.table(database)
      if (any(c("mass", "neutralMass") %in% colnames(database)) | "mz" %in% colnames(database)) {
        if ("name" %in% colnames(database)) {
          if ("neutralMass" %in% colnames(database)) {
            data.table::setnames(database, "neutralMass", "mass")
          }
          valid_db <- TRUE
        }
      }
    }

    if (!valid_db) {
      warning("Argument database must be a data.frame with at least the columns name and mass or mz!")
      return(data.table::data.table())
    }

    if (!"rt" %in% colnames(database)) {
      database$rt <- 0
    } else {
      database$rt[database$rt == ""] <- 0
    }

    database$rt <- as.numeric(database$rt)
    database$rt[is.na(rt)] <- 0

    if ("mass" %in% colnames(database)) {
      suspects <- get_features(x, analyses, mass = database, ppm = ppm, sec = sec, millisec = millisec, filtered = filtered)
    } else if ("mz" %in% colnames(database)) {
      suspects <- get_features(x, analyses, mz = database, ppm = ppm, sec = sec, millisec = millisec, filtered = filtered)
    } else {
      warning("Argument database must be a data.frame with at least the columns name and mass or mz!")
      return(data.table::data.table())
    }

    if (nrow(suspects) == 0) {
      message("\U2717 No suspects found!")
      return(data.table::data.table())
    }

    suspects <- split(suspects, suspects$analysis)

    suspects <- lapply(suspects, function(z, database) {
      out <- data.table::data.table()

      if (nrow(z) > 0) {
        for (i in seq_len(nrow(z))) {
          suspect_analysis <- z$analysis[i]
          suspect_feature <- z$feature[i]
          suspect_name <- z$name[i]
          suspect_mass <- z$mass[i]
          suspect_rt <- z$rt[i]
          suspect_intensity <- z$intensity[i]
          suspect_area <- z$area[i]

          suspect_db <- database[database$name == suspect_name][1, ]

          temp <- data.table::data.table("analysis" = suspect_analysis, "feature" = suspect_feature)
          if ("group" %in% colnames(z)) temp$group <- z$group[i]
          temp$name <- suspect_name

          if ("formula" %in% colnames(suspect_db)) temp$formula <- suspect_db$formula
          if ("SMILES" %in% colnames(suspect_db)) temp$SMILES <- suspect_db$SMILES

          temp$mass <- suspect_mass
          if ("mz" %in% suspect_db) {
            temp$exp_mass <- suspect_db$mz - (z$polarity[i] * 1.007276)
          } else {
            temp$exp_mass <- suspect_db$mass
          }
          temp$error_mass <- round(((temp$mass - temp$exp_mass) / temp$mass) * 1E6, digits = 1)

          temp$rt <- suspect_rt
          temp$exp_rt <- suspect_db$rt
          temp$error_rt <- round(temp$rt - temp$exp_rt, digits = 1)

          temp$id_level <- "4"

          temp$shared_fragments <- 0
          temp$fragments <- NA_character_

          temp$intensity <- suspect_intensity
          temp$area <- suspect_area

          if (temp$exp_rt > 0) temp$id_level <- "3b"

          if ("fragments" %in% colnames(suspect_db)) {
            fragments <- suspect_db$fragments

            if (!is.na(fragments)) {
              ms2 <- data.table::data.table()

              if ("ms2" %in% colnames(z)) {
                ms2 <- z$ms2[i][[1]]
                if (length(ms2) == 0) ms2 <- data.table::data.table()
              }

              if (nrow(ms2) == 0) {
                ms2 <- get_features_ms2(
                  x,
                  z$analysis[i],
                  z$feature[i],
                  isolationWindow = isolationWindow,
                  mzClust = mzClust,
                  presence = presence,
                  minIntensity = minIntensity
                )
              }

              if (nrow(ms2) > 0) {
                fragments <- unlist(strsplit(fragments, split = "; ", fixed = TRUE))
                fragments <- strsplit(fragments, " ")
                fragments <- data.table::data.table(
                  "mz" = vapply(fragments, function(x) as.numeric(x[1]), NA_real_),
                  "intensity" = vapply(fragments, function(x) as.numeric(x[2]), NA_real_)
                )

                mzr <- fragments$mz * ppm / 1E6
                fragments$mzmin <- fragments$mz - mzr
                fragments$mzmax <- fragments$mz + mzr

                fragments$shared <- apply(fragments, 1, function(x) {
                  any(ms2$mz >= x[3] & ms2$mz <= x[4])
                })

                temp$shared_fragments <- sum(fragments$shared)

                if (temp$shared_fragments > 3) {
                  temp$fragments <- suspect_db$fragments

                  if (temp$id_level == "3b") {
                    temp$id_level <- "1"
                  } else if (temp$id_level == "4") {
                    temp$id_level <- "2"
                  }
                }
              }
            }
          }
          out <- data.table::rbindlist(list(out, temp), fill = TRUE)
        }
      }
      out
    }, database = database)

    suspects <- data.table::rbindlist(suspects, fill = TRUE)
  }

  if (nrow(suspects) > 0 && !filtered && x$NTS$has_groups && onGroups) {
    if (all(!is.na(suspects$group))) {
      suspects$id_level <- factor(suspects$id_level, levels = c("1", "2", "3a", "3b", "4"), ordered = TRUE)

      id_level <- NULL
      error_mass <- NULL
      error_rt <- NULL
      shared_fragments <- NULL
      name <- NULL

      temp_vals <- suspects[, .(
        name = unique(name),
        id_level = min(id_level),
        error_mass = min(abs(error_mass)),
        error_rt = min(abs(error_rt)),
        shared_fragments = max(shared_fragments)
      ), by = "group"]

      temp_vals <- unique(temp_vals)
      groups_df <- get_groups(x, groups = unique(suspects$group), intensities = TRUE, average = TRUE, sdValues = FALSE, metadata = FALSE)
      group <- NULL
      groups_df <- groups_df[temp_vals, on = .(group)]
      data.table::setkey(groups_df, group)
      groups_df <- groups_df[unique(group), mult = "first"]
      data.table::setcolorder(groups_df, c("group", "name", "id_level", "error_mass", "error_rt", "shared_fragments"))
      data.table::setkey(groups_df, NULL)
      return(groups_df)
    }
  }

  suspects
}

# MARK: get_internal_standards
## get_internal_standards -----
#' @export
#' @noRd
S7::method(get_internal_standards, MassSpecAnalyses) <- function(x, average = TRUE) {
  istd <- get_features(x, filtered = TRUE)

  if ("istd" %in% colnames(istd)) {
    sel <- vapply(istd$istd, function(z) {
      if (length(z) > 0) {
        if (is.data.frame(z)) {
          if (nrow(z) > 0) {
            return(TRUE)
          }
        }
      }
      FALSE
    }, FALSE)

    istd <- istd[sel, ]

    if (nrow(istd) > 0) {
      istd_l <- istd[["istd"]]

      istd_l2 <- lapply(seq_len(length(istd_l)), function(x, istd_l, istd) {
        temp <- istd_l[[x]]
        temp_ft <- istd[x, ]
        temp <- cbind(temp, temp_ft)
        temp
      }, istd = istd, istd_l = istd_l)

      istd <- rbindlist(istd_l2, fill = TRUE)

      istd$rtr <- round(istd$rtmax - istd$rtmin, digits = 1)

      istd$mzr <- round(istd$mzmax - istd$mzmin, digits = 4)

      if ("annotation" %in% colnames(istd)) {
        istd$iso_n <- vapply(istd$annotation, function(x) {
          if (length(x) == 0) {
            NA_real_
          } else {
            x$iso_size
          }
        }, NA_real_)
        istd$iso_c <- vapply(istd$annotation, function(x) {
          if (length(x) == 0) {
            NA_real_
          } else {
            x$iso_number_carbons
          }
        }, NA_real_)
      } else {
        istd$iso_n <- NA_real_
        istd$iso_c <- NA_real_
      }

      if (x$NTS$has_groups && average) {
        rpl <- x$replicates

        istd$replicate <- rpl[istd$analysis]

        cols <- c(
          "name",
          "rt",
          "mass",
          "intensity",
          "area",
          "rtr",
          "mzr",
          "error_rt",
          "error_mass",
          "rec",
          "iso_n",
          "iso_c",
          "replicate",
          "group"
        )

        istd <- istd[, cols, with = FALSE]

        area <- NULL
        rt <- NULL
        mass <- NULL
        intensity <- NULL
        rtr <- NULL
        mzr <- NULL
        error_rt <- NULL
        error_mass <- NULL
        rec <- NULL
        iso_n <- NULL
        iso_c <- NULL

        istd <- istd[, `:=`(
          freq = length(area),
          rt = round(mean(rt, na.rm = TRUE), digits = 0),
          mass = round(mean(mass, na.rm = TRUE), digits = 4),
          intensity = round(mean(intensity, na.rm = TRUE), digits = 0),
          intensity_sd = round(sd(intensity, na.rm = TRUE), digits = 0),
          area = round(mean(area, na.rm = TRUE), digits = 0),
          area_sd = round(sd(area, na.rm = TRUE), digits = 0),
          rtr = round(mean(rtr, na.rm = TRUE), digits = 1),
          rtr_sd = round(sd(rtr, na.rm = TRUE), digits = 1),
          mzr = round(mean(mzr, na.rm = TRUE), digits = 4),
          mzr_sd = round(sd(mzr, na.rm = TRUE), digits = 4),
          error_rt = round(mean(error_rt, na.rm = TRUE), digits = 1),
          error_rt_sd = round(sd(error_rt, na.rm = TRUE), digits = 1),
          error_mass = round(mean(error_mass, na.rm = TRUE), digits = 1),
          error_mass_sd = round(sd(error_mass, na.rm = TRUE), digits = 1),
          rec = round(mean(rec, na.rm = TRUE), digits = 1),
          rec_sd = round(sd(rec, na.rm = TRUE), digits = 1),
          iso_n = round(mean(iso_n, na.rm = TRUE), digits = 0),
          iso_n_sd = round(sd(iso_n, na.rm = TRUE), digits = 0),
          iso_c = round(mean(iso_c, na.rm = TRUE), digits = 0),
          iso_c_sd = round(sd(iso_c, na.rm = TRUE), digits = 0)
        ),
        by = c("name", "group", "replicate")
        ][]

        istd <- unique(istd)

        istd$rec[is.nan(istd$rec)] <- NA_real_
      } else {
        cols <- c(
          "name",
          "rt",
          "mass",
          "intensity",
          "area",
          "rtr",
          "mzr",
          "error_rt",
          "error_mass",
          "rec",
          "iso_n",
          "iso_c",
          "analysis",
          "feature"
        )

        if (x$NTS$has_groups) cols <- c(cols, "group")

        istd <- istd[, cols, with = FALSE]
        istd$intensity <- round(istd$intensity, digits = 0)
        istd$area <- round(istd$area, digits = 0)
      }

      setorder(istd, "name")

      istd
    } else {
      warning("Internal standards not found!")
      data.table()
    }
  } else {
    warning("Not present! Run find_internal_standards method to tag the internal standards!")
    data.table()
  }
}

# MARK: get_compounds
## get_compounds -----
#' @export
#' @noRd
S7::method(get_compounds, MassSpecAnalyses) <- function(x,
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
                                                        averaged = TRUE) {
  if (!x$has_results_nts) {
    warning("No NTS results found!")
    return(data.table::data.table())
  }

  if (!x$NTS$has_features) {
    warning("Features not found!")
    return(data.table::data.table())
  }

  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for targets!")
    return(data.table::data.table())
  }

  compounds <- fts$compounds

  if (!averaged && x$NTS$has_groups) {
    compounds <- Map(function(z, y) {
      if (nrow(z) > 0) z$analysis <- y
      z
    }, compounds, fts$analysis)

    compounds <- Map(function(z, y) {
      if (nrow(z) > 0) z$feature <- y
      z
    }, compounds, fts$feature)
  }

  compounds <- Map(function(z, y) {
    if (nrow(z) > 0) z$polarity <- y
    z
  }, compounds, fts$polarity)

  compounds <- Map(function(z, y) {
    if (nrow(z) > 0) z$rt <- y
    z
  }, compounds, fts$rt)

  compounds <- Map(function(z, y) {
    if (nrow(z) > 0) z$mass <- y
    z
  }, compounds, fts$mass)

  compounds <- Map(function(z, y) {
    if (nrow(z) > 0) z$group <- y
    z
  }, compounds, fts$group)

  compounds <- compounds[vapply(compounds, function(z) nrow(z) > 0, FALSE)]

  compounds <- data.table::rbindlist(compounds, fill = TRUE)

  if (nrow(compounds) > 0) {
    if (averaged && x$NTS$has_groups) {
      data.table::setcolorder(compounds, c("group", "rt", "mass", "polarity", "compoundName"))
      compounds <- compounds[!duplicated(paste0(compounds$group, compounds$compoundName, compounds$polarity))]
    } else {
      data.table::setcolorder(compounds, c("analysis", "feature", "group", "rt", "mass", "polarity", "compoundName"))
    }
  }
  compounds
}

# MARK: get_fold_change
## get_fold_change -----
#' @export
#' @noRd
S7::method(get_fold_change, MassSpecAnalyses) <- function(x,
                                                          replicatesIn = NULL,
                                                          replicatesOut = NULL,
                                                          groups = NULL,
                                                          mass = NULL,
                                                          mz = NULL,
                                                          rt = NULL,
                                                          mobility = NULL,
                                                          ppm = 4,
                                                          sec = 10,
                                                          millisec = 5,
                                                          filtered = FALSE,
                                                          constantThreshold = 0.5,
                                                          eliminationThreshold = 0.2,
                                                          correctSuppression = FALSE,
                                                          fillZerosWithLowerLimit = FALSE,
                                                          lowerLimit = NA_real_) {
  if (!x$has_results_nts) {
    warning("\U2717 NTS resuts not found!")
    return(NULL)
  }

  if (!x$NTS$has_groups) {
    warning("\U2717 Feature groups not found!")
    return(NULL)
  }

  rpls <- x$replicates

  if (is.numeric(replicatesIn)) replicatesIn <- unique(rpls[replicatesIn])
  if (is.numeric(replicatesOut)) replicatesOut <- unique(rpls[replicatesOut])

  if (any(is.na(replicatesIn)) || any(is.na(replicatesOut))) {
    message("\U2717 Replicates not found!")
    return(NULL)
  }

  if (length(replicatesIn) == 1 && length(replicatesOut) > 1) {
    replicatesIn <- rep(replicatesIn, length(replicatesOut))
  }

  groups_dt <- get_groups(
    x,
    groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered,
    intensities = TRUE,
    average = FALSE,
    sdValues = FALSE,
    metadata = FALSE,
    correctSuppression
  )

  if (nrow(groups_dt) == 0) {
    message("\U2717 Feature groups not found for the targets!")
    return(NULL)
  }

  comb <- data.table::data.table()

  for (rep in seq_len(length(replicatesOut))) {
    out_temp <- names(x)[x$replicates %in% replicatesOut[rep]]
    in_temp <- names(x)[x$replicates %in% replicatesIn[rep]]
    comb_temp <- expand.grid(
      analysisIn = in_temp,
      analysisOut = out_temp,
      replicateIn = replicatesIn[rep],
      replicateOut = replicatesOut[rep]
    )
    comb <- data.table::rbindlist(list(comb, comb_temp), fill = TRUE)
  }

  if (nrow(comb) == 0) {
    warning("\U2717 Combinations could not be made, check replicates IN and OUT!")
    return(NULL)
  }

  fc <- lapply(seq_len(nrow(comb)), function(z, comb, groups_dt, fillZerosWithLowerLimit) {
    anaIn <- comb$analysisIn[z]
    anaOut <- comb$analysisOut[z]
    vecOut <- groups_dt[, colnames(groups_dt) %in% anaOut, with = FALSE][[1]]
    vecIn <- groups_dt[, colnames(groups_dt) %in% anaIn, with = FALSE][[1]]

    if (fillZerosWithLowerLimit) {
      if (is.na(lowerLimit)) {
        vecOut[vecOut == 0] <- min(vecOut[vecOut > 0])
        vecIn[vecIn == 0] <- min(vecIn[vecIn > 0])
      } else {
        vecOut[vecOut == 0] <- lowerLimit
        vecIn[vecIn == 0] <- lowerLimit
      }
    }

    fc_vec <- vecOut / vecIn
    res <- data.table::data.table(group = groups_dt$group, fc = fc_vec)
    res$analysis_in <- anaIn
    res$analysis_out <- anaOut
    res$replicate_in <- comb$replicateIn[z]
    res$replicate_out <- comb$replicateOut[z]
    res$combination <- z
    res
  }, comb = comb, groups_dt = groups_dt, fillZerosWithLowerLimit = fillZerosWithLowerLimit)

  fc <- data.table::rbindlist(fc)

  sel_nan <- is.nan(fc$fc)

  fc <- fc[!sel_nan, ]

  # fc_av <- fc[, .(fc = mean(fc, na.rm = TRUE)), by = c("combination", "group")]

  fc_category <- list(
    "Elimination" = c(0, eliminationThreshold),
    "Decrease" = c(eliminationThreshold, constantThreshold),
    "Constant" = c(constantThreshold, 1 / constantThreshold),
    "Increase" = c(1 / constantThreshold, 1 / eliminationThreshold),
    "Formation" = c(1 / eliminationThreshold, Inf)
  )

  fc_boundaries <- c(
    paste0("(", 0, "-", eliminationThreshold, ")"),
    paste0("(", eliminationThreshold, "-", constantThreshold, ")"),
    paste0("(", constantThreshold, "-", 1 / constantThreshold, ")"),
    paste0("(", 1 / constantThreshold, "-", 1 / eliminationThreshold, ")"),
    paste0("(", 1 / eliminationThreshold, "-Inf)")
  )

  names(fc_boundaries) <- names(fc_category)

  for (i in seq_along(fc_category)) {
    fc$category[fc$fc >= fc_category[[i]][1] & fc$fc < fc_category[[i]][2]] <- names(fc_category)[i]
  }

  sel_na_category <- is.na(fc$category)

  fc <- fc[!sel_na_category, ]
  fc$category <- factor(fc$category, levels = names(fc_category))
  fc$bondaries <- paste(fc$category, fc_boundaries[fc$category], sep = "\n")
  fc$bondaries <- factor(fc$bondaries, levels = paste(names(fc_category), fc_boundaries, sep = "\n"))
  fc
}

# Plot NTS -----



# MARK: plot_groups_overview
## plot_groups_overview -----
#' @export
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
                                                               correctSuppression = TRUE,
                                                               filtered = FALSE,
                                                               legendNames = NULL,
                                                               title = NULL,
                                                               heights = c(0.35, 0.5, 0.15)) {
  fts <- get_features(x, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered)

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  eic <- get_features_eic(
    x,
    analyses = unique(fts$analysis), features = fts,
    rtExpand = rtExpand, mzExpand = mzExpand, filtered = filtered, useLoadedData = useLoadedData
  )

  intensity <- NULL

  eic <- eic[, `:=`(intensity = max(intensity)), by = c("analysis", "polarity", "feature", "rt")][]

  eic <- unique(eic)

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

  analyses <- .check_analyses_argument(x, analyses)

  .plot_groups_overview_aux(fts, eic, heights, analyses, correctSuppression)
}

# MARK: plot_groups_profile
## plot_groups_profile -----
#' @export
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
                                                              correctSuppression = TRUE,
                                                              averaged = FALSE,
                                                              normalized = TRUE,
                                                              legendNames = NULL,
                                                              yLab = NULL,
                                                              title = NULL,
                                                              showLegend = TRUE) {
  fts <- get_features(x, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  analyses <- .check_analyses_argument(x, analyses)

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  polarities <- x$spectra_polarity

  if (!"polarity" %in% colnames(fts)) fts$polarity <- polarities[fts$analysis]

  if (correctSuppression) {
    if ("suppression_factor" %in% colnames(fts)) {
      fts$intensity <- fts$intensity * fts$suppression_factor
    }
  }

  if (normalized && "intensity_rel" %in% colnames(fts)) {
    fts$intensity <- as.numeric(fts$intensity_rel)
  }
  
  if (averaged && x$NTS$has_groups) {
    if (!"replicate" %in% colnames(fts)) fts$replicate <- x$replicates[fts$analysis]
    group_cols <- c("replicate", "group", "polarity")
    if ("name" %in% colnames(fts)) group_cols <- c(group_cols, "name")
    fts <- fts[, .(intensity = mean(intensity)), by = group_cols]
    data.table::setnames(fts, "replicate", "analysis")
    names(polarities) <- x$replicates[names(polarities)]
    polarities <- polarities[!duplicated(names(polarities))]
    analyses <- unique(x$replicates[analyses])
    
  }

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
  
  showLeg <- rep(showLegend, length(u_leg))
  names(showLeg) <- u_leg

  rpls <- x$replicates

  plot <- plot_ly(fts, x = sort(unique(fts$analysis)))

  for (g in u_leg) {
    df <- fts[fts$var == g, ]

    if (!all(analyses %in% df$analysis)) {
      extra <- data.frame(
        "analysis" = analyses[!analyses %in% df$analysis],
        "polarity" = polarities[!names(polarities) %in% df$analysis & names(polarities) %in% analyses],
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
      df_r <- df[df$replicate %in% r, ]

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

  if (is.null(yLab)) {
    if (normalized) {
      yLab <- "Normalized intensity"
      range_yaxis <- c(0, max(1.1))
    } else {
      yLab <- "Intensity / counts"
      range_yaxis <- c(min(fts$intensity), max(fts$intensity))
    }
  }

  yaxis <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = yLab,
    titlefont = list(size = 12, color = "black"),
    range = range_yaxis
  )

  plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis)

  plot
}

# MARK: map_components
## map_components -----
#' @export
#' @noRd
S7::method(map_components, MassSpecAnalyses) <- function(x,
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
                                                         xlim = 30,
                                                         ylim = 0.05,
                                                         showLegend = TRUE,
                                                         legendNames = NULL,
                                                         xLab = NULL,
                                                         yLab = NULL,
                                                         title = NULL,
                                                         colorBy = "targets",
                                                         interactive = TRUE) {
  components <- get_components(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  new_order <- order(abs(components$iso_step * 2))
  components <- components[new_order, ]

  if (nrow(components) == 0) {
    warning("\U2717 Components not found for the targets!")
    return(NULL)
  }

  if (grepl("replicates", colorBy) && !"replicate" %in% colnames(components)) {
    components$replicate <- x$replicates[components$analysis]
  }

  if (!interactive) {
    .map_components_static(components, colorBy, legendNames, xlim, ylim, xLab, yLab, title, showLegend)
  } else {
    .map_components_interactive(components, colorBy, legendNames, xlim, ylim, xLab, yLab, title, showLegend = TRUE)
  }
}

# MARK: plot_internal_standards
## plot_internal_standards -----
#' @export
#' @noRd
S7::method(plot_internal_standards, MassSpecAnalyses) <- function(x,
                                                                  analyses = NULL,
                                                                  presence = TRUE,
                                                                  recovery = TRUE,
                                                                  deviations = TRUE,
                                                                  widths = TRUE) {
  analyses <- .check_analyses_argument(x, analyses)

  if (!x$has_results_nts) {
    return(NULL)
  }

  if (x$NTS$has_groups) {
    istd <- get_internal_standards(x, average = TRUE)
    istd <- istd[istd$replicate %in% x$replicates[analyses], ]
    .plot_internal_standards_qc_interactive(istd, x$replicates[analyses], presence, recovery, deviations, widths)
  } else {
    istd <- get_internal_standards(x, average = FALSE)
    istd <- istd[istd$analysis %in% analyses, ]
    .plot_internal_standards_qc_interactive(istd, names(x)[analyses], presence, recovery, deviations, widths)
  }
}

# MARK: plot_suspects
## plot_suspects -----
#' @export
#' @noRd
S7::method(plot_suspects, MassSpecAnalyses) <- function(x,
                                                        analyses = NULL,
                                                        database = NULL,
                                                        features = NULL,
                                                        mass = NULL,
                                                        mz = NULL,
                                                        rt = NULL,
                                                        mobility = NULL,
                                                        ppm = 4,
                                                        sec = 10,
                                                        millisec = 5,
                                                        ppmMS2 = 10,
                                                        minFragments = 3,
                                                        isolationWindow = 1.3,
                                                        mzClust = 0.003,
                                                        presence = 0.8,
                                                        minIntensity = 0,
                                                        filtered = FALSE,
                                                        rtExpand = 120,
                                                        mzExpand = 0.005,
                                                        useLoadedData = TRUE,
                                                        colorBy = "targets",
                                                        interactive = TRUE) {
  if (!x$has_results_nts) {
    return(NULL)
  }

  if (!x$NTS$has_features_suspects) {
    return(NULL)
  }

  suspects <- get_suspects(
    x,
    analyses,
    database,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    ppmMS2,
    minFragments,
    isolationWindow,
    mzClust,
    presence,
    minIntensity,
    filtered,
    onGroups = FALSE
  )

  if (nrow(suspects) == 0) {
    return(NULL)
  }

  eic <- get_features_eic(
    x,
    analyses = unique(suspects$analysis),
    features = suspects$feature,
    rtExpand = rtExpand,
    mzExpand = mzExpand,
    filtered = filtered,
    useLoadedData = useLoadedData
  )

  intensity <- NULL

  eic <- eic[, `:=`(intensity = sum(intensity)), by = c("analysis", "polarity", "feature", "rt")][]

  if (nrow(eic) == 0) {
    message("\U2717 Traces and/or features not found for targets!")
    return(NULL)
  }

  if (grepl("replicates", colorBy)) {
    eic$replicate <- x$replicates[eic$analysis]
    suspects$replicate <- x$replicates[suspects$analysis]
  }

  suspects <- .make_colorBy_varkey(suspects, colorBy, TRUE)

  leg <- suspects$var
  names(leg) <- paste0(suspects$feature, "_", suspects$analysis)
  eic$uid <- paste0(eic$feature, "_", eic$analysis)
  suspects$uid <- paste0(suspects$feature, "_", suspects$analysis)
  eic$var <- leg[eic$uid]

  if (!interactive) {
    .plot_suspects_static(suspects, eic)
  } else {
    .plot_suspects_interactive(suspects, eic, heights = c(0.5, 0.5))
  }
}

# MARK: plot_fold_change
## plot_fold_change -----
#' @export
#' @noRd
S7::method(plot_fold_change, MassSpecAnalyses) <- function(x,
                                                           replicatesIn = NULL,
                                                           replicatesOut = NULL,
                                                           groups = NULL,
                                                           mass = NULL,
                                                           mz = NULL,
                                                           rt = NULL,
                                                           mobility = NULL,
                                                           ppm = 4,
                                                           sec = 10,
                                                           millisec = 5,
                                                           filtered = FALSE,
                                                           constantThreshold = 0.5,
                                                           eliminationThreshold = 0.2,
                                                           correctSuppression = FALSE,
                                                           fillZerosWithLowerLimit = FALSE,
                                                           lowerLimit = NA_real_,
                                                           normalized = TRUE,
                                                           yLab = NULL,
                                                           title = NULL,
                                                           interactive = TRUE,
                                                           showLegend = TRUE) {
  fc <- get_fold_change(
    x,
    replicatesIn,
    replicatesOut,
    groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered,
    constantThreshold,
    eliminationThreshold,
    correctSuppression,
    fillZerosWithLowerLimit,
    lowerLimit
  )

  if (is.null(fc)) {
    return(NULL)
  }

  if (nrow(fc) == 0) {
    return(NULL)
  }

  fc_summary_count <- fc[, .(count = .N), by = c("combination", "bondaries", "replicate_out")]

  if (is.null(yLab)) {
    yLab <- "Number of feature groups (out / in)"
  }

  if (!interactive) {
    fc_summary_count$bondaries <- paste(
      fc_summary_count$replicate_out,
      fc_summary_count$bondaries,
      sep = "\n"
    )

    fc_summary_count$bondaries <- factor(
      fc_summary_count$bondaries,
      levels = unique(fc_summary_count$bondaries)
    )

    fc_levels <- fc_summary_count[, .(replicate_out, bondaries)]
    fc_levels <- unique(fc_levels)

    colours <- .get_colors(unique(fc_levels$replicate_out))
    colours_key <- colours[fc_levels$replicate_out]
    
    if (normalized) {
      fc_summary_count$uid <- paste0(
        fc_summary_count$replicate_out, "_", fc_summary_count$combination
      )
      for (i in unique(fc_summary_count$uid)) {
        sel <- fc_summary_count$uid %in% i
        fc_summary_count$count[sel] <- fc_summary_count$count[sel] / sum(fc_summary_count$count[sel])
      }
    }

    graphics::boxplot(
      fc_summary_count$count ~ fc_summary_count$bondaries,
      data = fc_summary_count,
      col = paste0(colours_key, "50"),
      border = colours_key,
      main = title,
      xlab = NULL,
      ylab = yLab,
      outline = TRUE,
      ylim = c(0, max(fc_summary_count$count) + 1)
    )
    # outliers <- graphics::boxplot(
    #   fc_summary_count$count ~ fc_summary_count$bondaries,
    #   data = fc_summary_count,
    #   plot = FALSE
    # )
    # if (length(outliers$out) > 0) {
    #   points(outliers$group, outliers$out, col = "red", pch = 4)
    # }
    if (showLegend) {
      legend(
        "topright",
        legend = names(colours),
        fill = colours
      )
    }
    
  } else {
    
    if (normalized) {
      fc_summary_count$uid <- paste0(
        fc_summary_count$replicate_out, "_", fc_summary_count$combination
      )
      for (i in unique(fc_summary_count$uid)) {
        sel <- fc_summary_count$uid %in% i
        fc_summary_count$count[sel] <- fc_summary_count$count[sel] / sum(fc_summary_count$count[sel])
      }
    }
    
    fig <- plotly::plot_ly(
      data = fc_summary_count,
      x = ~bondaries,
      y = ~count,
      color = ~replicate_out,
      colors = .get_colors(unique(fc_summary_count$replicate_out)),
      type = "box",
      jitter = 0.03,
      showlegend = showLegend
    )
    fig <- fig %>% plotly::layout(
      title = title,
      xaxis = list(title = ""),
      yaxis = list(title = yLab, range = c(0, max(fc_summary_count$count) + 1))
    )
    fig
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



# MARK: Utility functions
# Utility functions -----

# MARK: .get_MassSpecAnalysis_from_files
#' @noRd
.get_MassSpecAnalysis_from_files <- function(files = NULL,
                                             centroid = FALSE,
                                             levels = c(1, 2)) {
  if (!is.null(files)) {
    if (is.data.frame(files)) {
      if (all(c("path", "analysis") %in% colnames(files))) {
        files$file <- vapply(seq_len(nrow(files)), function(x) {
          list.files(
            files$path[x],
            pattern = files$analysis[x],
            full.names = TRUE,
            recursive = FALSE
          )
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

    possible_ms_file_formats <- ".mzML$|.mzXML$|.d$"

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

    if (any(vapply(files, function(x) tools::file_ext(x) == "d", FALSE))) {
      files_to_convert <- files[tools::file_ext(files) == "d"]
      files_converted <- gsub(".d$", ".mzML", files_to_convert)

      for (i in seq_along(files_converted)) {
        if (file.exists(files_converted[i])) {
          if (files_converted[i] %in% files) {
            files <- files[!files %in% files_to_convert[i]]
          } else {
            files[files == files_to_convert[i]] <- files_converted[i]
          }
        }
      }

      files_to_convert <- files[tools::file_ext(files) == "d"]

      if (length(files_to_convert) > 0) {
        filter <- ""

        if (centroid) {
          filter <- "peakPicking vendor"
        }

        if (centroid && is.numeric(levels) && length(levels) > 0) {
          levels <- paste(levels, collapse = "-")
          levels <- paste("msLevel=", levels, collapse = "", sep = "")
          if (filter != "") {
            filter <- paste(filter, levels, sep = " ")
          } else {
            filter <- levels
          }
        }

        optList <- list()
        if (filter != "") {
          optList <- list(filter = filter)
        }

        tryCatch(
          {
            StreamFind::convert_ms_files(
              files = files_to_convert,
              outputFormat = "mzML",
              outputPath = NULL,
              optList = optList
            )

            files <- c(
              files[!files %in% files_to_convert],
              files_converted[!files_converted %in% files]
            )
            
            exist_files <- vapply(files, function(x) file.exists(x), FALSE)
            files <- files[exist_files]
          },
          error = function(e) {
            warning("Error converting files!")
            files <- files[!files %in% files_to_convert]
          },
          warning = function(w) {
            warning("Warning converting files!")
            files <- files[!files %in% files_to_convert]
          }
        )
      }
    }

    analyses <- lapply(files, function(x) {
      
      cache <- .load_chache_rds("parsed_ms_analyses", x)
      # cache <- .load_cache_sqlite("parsed_ms_analyses", x)
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

        concentration <- suppressWarnings(as.numeric(ana$name))

        if (is.na(concentration)) {
          ana$concentration <- NA_real_
        } else {
          ana$concentration <- concentration
        }

        if (!is.null(cache$hash)) {
          .save_cache_rds("parsed_ms_analyses", ana, cache$hash)
          # .save_cache_sqlite("parsed_ms_analyses", ana, cache$hash)
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
