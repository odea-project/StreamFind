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
                                                                  showLegend = TRUE,
                                                                  renderEngine = "webgl") {
  analyses <- .check_analyses_argument(x, analyses)
  mp <- get_matrix_suppression(x, rtWindow)
  if (nrow(mp) == 0) {
    message("\U2717 TIC matrix suppression not found for the analyses!")
    return(NULL)
  }
  sel <- mp$analysis %in% analyses
  mp <- mp[sel, ]
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
        mode = "lines", #+markers
        line = list(width = 2),
        # marker = list(size = 2.5),
        text = ~paste(
          "<br>analysis: ", analysis,
          "<br>replicate: ", replicate,
          "<br>id: ", id,
          "<br>polarity: ", polarity,
          "<br>level: ", level,
          "<br>rt: ", rt,
          "<br>suppression: ", intensity
        ),
        hoverinfo = "text",
        showlegend = showLegend
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
