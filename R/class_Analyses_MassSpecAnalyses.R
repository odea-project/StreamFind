#' @export
#' @noRd
MassSpecAnalyses <- S7::new_class(
  "MassSpecAnalyses",
  package = "StreamFind",
  parent = Analyses,
  properties = list(

    # MARK: analyses
    # analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),

    # MARK: replicates
    # replicates -----
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

    # MARK: blanks
    # blanks -----
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

    # MARK: concentrations
    # concentrations -----
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
        if (self$has_nts) {
          if (self@results$nts$has_features) {
            self@results$nts$features@analysisInfo$concentration <- value
            if (self@results$nts$has_groups) {
              self@results$nts$features@features@analysisInfo$concentration <- value
            }
          }
        }
        self
      }
    ),

    # MARK: types
    # types -----
    types = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) x$type, NA_character_)
      }
    ),

    # MARK: files
    # files -----
    files = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) x$file, NA_character_)
      }
    ),

    # MARK: formats
    # formats -----
    formats = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@analyses, function(x) x$format, NA_character_)
      }
    ),

    # MARK: instruments
    # instruments -----
    instruments = S7::new_property(
      S7::class_character,
      getter = function(self) {
        lapply(self@analyses, function(x) x$instrument)
      }
    ),

    # MARK: software
    # software -----
    software = S7::new_property(
      S7::class_character,
      getter = function(self) {
        lapply(self@analyses, function(x) x$software)
      }
    ),

    # MARK: spectra_number
    # spectra_number -----
    spectra_number = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) x$spectra_number, 0)
      }
    ),

    # MARK: spectra_headers
    # spectra_headers -----
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
    # spectra_mode -----
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
    # spectra_level -----
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
    # spectra_lowest_mz -----
    spectra_lowest_mz = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) min(x$spectra_headers$lowmz), NA_real_)
      }
    ),

    # MARK: spectra_highest_mz
    # spectra_highest_mz -----
    spectra_highest_mz = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) max(x$spectra_headers$highmz), NA_real_)
      }
    ),

    # MARK: spectra_lowest_rt
    # spectra_lowest_rt -----
    spectra_lowest_rt = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) min(x$spectra_headers$rt), NA_real_)
      }
    ),

    # MARK: spectra_highest_rt
    # spectra_highest_rt -----
    spectra_highest_rt = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) max(x$spectra_headers$rt), NA_real_)
      }
    ),

    # MARK: spectra_lowest_mobility
    # spectra_highest_mobility -----
    spectra_highest_mobility = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) max(x$spectra_headers$mobility), NA_real_)
      }
    ),

    # MARK: spectra_lowest_mobility
    # spectra_lowest_mobility -----
    spectra_lowest_mobility = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) min(x$spectra_headers$mobility), NA_real_)
      }
    ),

    # MARK: spectra_polarity
    # spectra_polarity -----
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
    # spectra_tic -----
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
    # spectra_bpc -----
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

    # MARK: spectra_raw
    # spectra_raw -----
    spectra_raw = S7::new_property(
      S7::class_list,
      getter = function(self) {
        StreamFind::MassSpecSpectra(
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
      }
    ),

    # MARK: chromatograms_number
    # chromatograms_number -----
    chromatograms_number = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        vapply(self@analyses, function(x) x$chromatograms_number, 0)
      }
    ),

    # chromatograms_headers -----
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
    # chromatograms_raw -----
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
    # info -----
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
    # has_ion_mobility -----
    has_ion_mobility = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self) == 0) {
          return(FALSE)
        }
        vapply(self@analyses, function(x) any(x$spectra_headers$mobility > 0), FALSE)
      }
    ),

    # MARK: has_loaded_spectra
    # has_loaded_spectra -----
    has_loaded_spectra = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self) == 0) {
          return(FALSE)
        }
        vapply(self@analyses, function(x) nrow(x$spectra) > 0, FALSE)
      }
    ),

    # MARK: has_loaded_chromatograms
    # has_loaded_chromatograms -----
    has_loaded_chromatograms = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self) == 0) {
          return(FALSE)
        }
        vapply(self@analyses, function(x) nrow(x$chromatograms) > 0, FALSE)
      }
    ),

    # MARK: has_nts
    # has_nts -----
    has_nts = S7::new_property(
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

    # MARK: nts
    # nts -----
    nts = S7::new_property(
      S7::class_list,
      getter = function(self) {
        if (self$has_nts) {
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

    # MARK: has_spectra
    # has_spectra -----
    has_spectra = S7::new_property(
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

    # MARK: spectra
    # spectra -----
    spectra = S7::new_property(
      S7::class_list,
      getter = function(self) {
        if (self$has_spectra) {
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

    # MARK: has_chromatograms
    # has_chromatograms -----
    has_chromatograms = S7::new_property(
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

    # MARK: chromatograms
    # chromatograms -----
    chromatograms = S7::new_property(
      S7::class_list,
      getter = function(self) {
        if (self$has_chromatograms) {
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
  # constructor -----
  constructor = function(files = NULL, centroid = FALSE, levels = c(1, 2)) {
    analyses <- .get_MassSpecAnalysis_from_files(files, centroid, levels)
    S7::new_object(Analyses(), possible_formats = c("mzML", "mzXML", "d"), analyses = analyses)
  },

  # MARK: validator
  # validator -----
  validator = function(self) {
    checkmate::assert_true(identical(self@possible_formats, c("mzML", "mzXML", "d")))
    if (length(self) > 0) checkmate::assert_true(identical(names(self@analyses), unname(names(self))))
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
  if (any(vapply(value, function(a) a$name %in% names(x), FALSE))) {
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

# MARK: remove
#' @export
#' @noRd
S7::method(remove, MassSpecAnalyses) <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!names(x) %in% value]
    x@analyses <- x@analyses[order(names(x@analyses))]
    if (x@has_nts) x@results$nts <- x@results$nts[!names(x) %in% value]
  } else if (is.numeric(value)) {
    x@analyses <- x@analyses[-value]
    x@analyses <- x@analyses[order(names(x@analyses))]
    if (x@has_nts) x@results$nts <- x@results$nts[-value]
  }
  x
}

# MARK: `[`
#' @export
#' @noRd
S7::method(`[`, MassSpecAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (x@has_nts) x@results$nts <- x@results$nts[i]
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
  if (x@has_nts) x@results$nts <- x@results$nts[i]
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

# MARK: Get Spectra
# Get Spectra ------

# MARK: get_spectra_tic
## get_spectra_tic -----
#' @export
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

# MARK: get_spectra_bpc
## get_spectra_bpc -----
#' @export
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

# MARK: get_spectra
## get_spectra -----
#' @export
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

  if (is.null(analyses)) {
    return(data.table())
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

  num_cols <- c("mz", "rt", "mobility", "mzmin", "mzmax", "rtmin", "rtmax", "mobilitymin", "mobilitymax")

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
      targets$mobilitymax[is.na(targets$mobilitymax)] <- max(x$spectra_highest_mobility[analyses], na.rm = TRUE)
    }
    if (TRUE %in% is.na(targets$mobilitymin) && any(x$has_ion_mobility[analyses])) {
      targets$mobilitymin[is.na(targets$mobilitymin)] <- min(x$spectra_lowest_mobility[analyses], na.rm = TRUE)
    }
    if (TRUE %in% (targets$mobilitymax == 0) && any(x$has_ion_mobility[analyses])) {
      targets$mobilitymax[targets$mobilitymax == 0] <- max(x$spectra_highest_mobility[analyses], na.rm = TRUE)
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

  # __________________________________________________________________
  # Extracts spectra results
  # __________________________________________________________________
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

    # __________________________________________________________________
    # Extracts spectra from results
    # __________________________________________________________________
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

    # __________________________________________________________________
    # Extracts spectra from raw data
    # __________________________________________________________________
  } else {
    spec_list <- lapply(x$analyses[analyses], function(a, levels, targets) {
      if ("analysis" %in% colnames(targets)) {
        targets <- targets[targets$analysis %in% a$name, ]
      }

      cache <- lapply(seq_len(nrow(targets)), function(i) {
        .load_chache(
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
                .save_cache(paste0("parsed_ms_spectra_", gsub("-|[/]|[.]|[() ]", "", i)), spec[[i]], cache[[i]]$hash)
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
          lapply(cached_targets, function(z) as.data.table(z$data)),
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

# MARK: get_spectra_matrix
## get_spectra_matrix -----
#' @export
#' @noRd
S7::method(get_spectra_matrix, MassSpecAnalyses) <- function(x, analyses = NULL) {
  if (!x$has_spectra) {
    warning("No spectra results object available!")
    return(matrix())
  }
  analyses <- .check_analyses_argument(x, analyses)
  spec_list <- x$spectra$spectra
  if (x$spectra$is_averaged) {
    rpl <- x$replicates
    rpl <- unique(rpl[analyses])
    spec_list <- spec_list[rpl]
  } else {
    spec_list <- spec_list[analyses]
  }
  intensity <- NULL
  spec_list <- spec_list[vapply(spec_list, function(z) nrow(z) > 0, FALSE)]

  spec_list <- lapply(spec_list, function(z) {
    if (!"bins" %in% colnames(z)) {
      if ("mass" %in% colnames(z)) z$mz <- z$mass
      if (!"level" %in% colnames(z)) z$level <- 1
      if ("mobility" %in% colnames(z)) {
        z$bins <- paste0("r", z$rt, "_m", z$mz, "_d", z$mobility, "_p", z$polarity, "_l", z$level)
      } else {
        z$bins <- paste0("r", z$rt, "_m", z$mz, "_p", z$polarity, "_l", z$level)
      }
    }
    z <- z[, .(intensity = mean(intensity)), by = c("bins")]
    z <- data.table::dcast(z, formula = 1 ~ bins, value.var = "intensity")[, -1]
    z
  })

  spec <- data.table::rbindlist(spec_list, fill = TRUE)
  spec[is.na(spec)] <- 0
  spec <- as.matrix(spec)
  rownames(spec) <- names(spec_list)
  spec
}

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
  cache <- .load_chache(
    "load_spectra",
    x, analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces, isolationWindow, minIntensityMS1, minIntensityMS2
  )
  if (!is.null(cache$data)) {
    x$spectra <- cache$data
    message("\U2139 Spectra loaded from cache!")
    return(x)
  }
  spec <- get_spectra(
    x,
    analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces, isolationWindow, minIntensityMS1, minIntensityMS2,
    useRawData = TRUE, useLoadedData = FALSE
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

    spec <- StreamFind::MassSpecSpectra(spec, is_averaged = FALSE)
    if (!is.null(cache$hash)) {
      .save_cache("load_spectra", spec, cache$hash)
      message("\U1f5ab Spectra cached!")
    }
    x$spectra <- spec
  } else {
    warning("Not done! Spectra not found.")
  }
  x
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
                                                          id = NULL,
                                                          useRawData = TRUE,
                                                          useLoadedData = FALSE) {
  eic <- get_spectra(
    x,
    analyses,
    levels = 1,
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
    eic <- eic[, `:=`(intensity = max(intensity), mz = mean(mz)), by = c("analysis", "polarity", "id", "rt")][]
    eic <- eic[, c("analysis", "polarity", "id", "rt", "mz", "intensity"), with = FALSE]
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
                                                          minIntensity = 1000,
                                                          useRawData = TRUE,
                                                          useLoadedData = FALSE) {
  ms1 <- get_spectra(
    x,
    analyses,
    levels = 1,
    mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces = TRUE,
    minIntensityMS1 = minIntensity,
    minIntensityMS2 = 0,
    useRawData = useRawData,
    useLoadedData = useLoadedData
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

  ms1_df <- rbindlist(ms1_list, fill = TRUE)

  ms1_df <- ms1_df[order(ms1_df$mz), ]

  ms1_df <- ms1_df[order(ms1_df$id), ]

  ms1_df <- ms1_df[order(ms1_df$analysis), ]

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
                                                          minIntensity = 0,
                                                          useRawData = TRUE,
                                                          useLoadedData = FALSE) {
  ms2 <- get_spectra(x,
    analyses,
    levels = 2,
    mass, mz, rt, mobility, ppm, sec, millisec, id,
    isolationWindow = isolationWindow,
    allTraces = FALSE,
    minIntensityMS1 = 0,
    minIntensityMS2 = minIntensity,
    useRawData = useRawData,
    useLoadedData = useLoadedData
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

  ms2_df <- rbindlist(ms2_list, fill = TRUE)

  ms2_df <- ms2_df[order(ms2_df$mz), ]

  ms2_df <- ms2_df[order(ms2_df$id), ]

  ms2_df <- ms2_df[order(ms2_df$analysis), ]

  ms2_df
}

# MARK: get_spectra_peaks
## get_spectra_peaks -----
#' @export
#' @noRd
S7::method(get_spectra_peaks, MassSpecAnalyses) <- function(x, analyses = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }

  if (!x$has_spectra) {
    return(data.table::data.table())
  }

  pks <- x$spectra$peaks
  if (length(pks) == 0) {
    return(data.table::data.table())
  }

  if (x$spectra$is_averaged) {
    pks <- data.table::rbindlist(x$spectra$peaks, idcol = "replicate", fill = TRUE)
  } else {
    pks <- data.table::rbindlist(x$spectra$peaks, idcol = "analysis", fill = TRUE)
  }

  if ("analysis" %in% colnames(pks)) {
    pks <- pks[pks$analysis %in% analyses, ]
  } else if ("replicate" %in% colnames(pks)) {
    rpl <- x$replicates
    rpl <- rpl[analyses]
    pks <- pks[pks$replicate %in% unname(rpl)]

    if (!"analysis" %in% colnames(pks)) {
      pks$analysis <- pks$replicate
      data.table::setcolorder(pks, c("analysis", "replicate"))
    }
  }

  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found for the targets!")
    return(data.table::data.table())
  }

  pks
}

# MARK: Plot Spectra
# Plot Spectra -----

# MARK: plot_spectra
## plot_spectra -----
#' @export
#' @noRd
S7::method(plot_spectra, MassSpecAnalyses) <- function(x,
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
                                                       useRawData = FALSE,
                                                       useLoadedData = TRUE,
                                                       legendNames = TRUE,
                                                       colorBy = "analyses",
                                                       xVal = "mz",
                                                       xLab = NULL,
                                                       yLab = NULL,
                                                       title = NULL,
                                                       cex = 0.6,
                                                       showLegend = TRUE,
                                                       interactive = TRUE) {
  spec <- get_spectra(x, analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces = allTraces, isolationWindow, minIntensityMS1, minIntensityMS2,
    useRawData, useLoadedData
  )

  if (nrow(spec) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }

  if (xVal == "mz" && (!"mz" %in% colnames(spec)) && "mass" %in% colnames(spec)) xVal <- "mass"

  if (!xVal %in% colnames(spec)) {
    message("\U2717 xVal not found in spectra data.table!")
    return(NULL)
  }

  spec$x <- spec[[xVal]]

  if ("feature" %in% colnames(spec)) spec$id <- spec$feature

  if ("replicates" %in% colorBy && !"replicate" %in% colnames(spec)) {
    spec$replicate <- x$replicates[spec$analysis]
  }

  spec <- .make_colorBy_varkey(spec, colorBy, legendNames = NULL)

  unique_key <- c("analysis", "var", "x")

  intensity <- NULL

  spec <- spec[, .(intensity = max(intensity)), by = c(unique_key)]

  spec <- unique(spec)

  if ("rt" %in% xVal) {
    if (is.null(xLab)) xLab <- "Retention time / seconds"
  } else if ("mz" %in% xVal) {
    if (is.null(xLab)) {
      if (interactive) {
        xLab <- "<i>m/z</i> / Da"
      } else {
        xLab <- expression(italic("m/z ") / " Da")
      }
    }
  } else if ("mass" %in% xVal) {
    if (is.null(xLab)) xLab <- "Mass / Da"
  } else if ("mobility" %in% xVal) {
    if (is.null(xLab)) xLab <- "mobility time / milliseconds"
  }

  if (is.null(yLab)) yLab <- "Intensity / counts"

  setorder(spec, var, x)

  if (!interactive) {
    return(.plot_x_spectra_static(spec, xLab, yLab, title, cex, showLegend))
  } else {
    return(.plot_x_spectra_interactive(spec, xLab, yLab, title, colorBy))
  }
}

# MARK: plot_spectra_3d
## plot_spectra_3d -----
#' @export
#' @noRd
S7::method(plot_spectra_3d, MassSpecAnalyses) <- function(x,
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
                                                          useRawData = FALSE,
                                                          useLoadedData = TRUE,
                                                          legendNames = TRUE,
                                                          colorBy = "analyses",
                                                          xVal = "rt",
                                                          yVal = "mz",
                                                          xLab = NULL,
                                                          yLab = NULL,
                                                          zLab = NULL) {
  spec <- get_spectra(
    x, analyses, levels, mass, mz, rt, mobility, ppm, sec, millisec, id,
    allTraces = allTraces, isolationWindow, minIntensityMS1, minIntensityMS2,
    useRawData = useRawData, useLoadedData = useLoadedData
  )

  if (nrow(spec) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }

  if ("mobility" %in% c(xVal, yVal)) {
    if (!"mobility" %in% colnames(spec)) {
      warning("mobility time values not found!")
      return(NULL)
    }
  }

  if ("feature" %in% colnames(spec)) spec$id <- spec$feature
  if (!"replicates" %in% colorBy) spec$replicate <- x$replicates[spec$analysis]
  .plot_spectra_3d_interactive(spec, colorBy, legendNames, xVal, yVal, xLab, yLab, zLab)
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
    .plot_spectra_eic_interactive(bpc, legendNames, colorBy, title, showLegend, xLab, yLab)
  }
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
                                                                  showLegend = TRUE,
                                                                  xlim = NULL,
                                                                  ylim = NULL,
                                                                  cex = 0.6,
                                                                  interactive = TRUE) {
  mp <- get_matrix_suppression(x, rtWindow)
  if (nrow(mp) == 0) {
    message("\U2717 TIC matrix suppression not found for the analyses!")
    return(NULL)
  }
  if (grepl("replicates", colorBy) && "replicate" %in% colnames(mp)) mp$replicate <- x@replicates[mp$analysis]
  if (!"id" %in% colnames(mp)) mp$id <- mp$analysis
  mp$intensity <- mp$mp
  if (is.null(yLab)) yLab <- "Supression Factor"
  if (!interactive) {
    .plot_spectra_eic_static(mp, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_spectra_eic_interactive(mp, legendNames, colorBy, title, showLegend, xLab, yLab)
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
                                                           numberRows = 1) {
  xic <- get_spectra(x,
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

  .plot_spectra_xic_interactive(
    xic,
    legendNames,
    plotTargetMark,
    targetsMark,
    ppmMark,
    secMark,
    numberRows
  )
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

# MARK: plot_spectra_baseline
## plot_spectra_baseline -----
#' @export
#' @noRd
S7::method(plot_spectra_baseline, MassSpecAnalyses) <- function(x,
                                                                analyses = NULL,
                                                                xVal = "mz",
                                                                xLab = NULL,
                                                                yLab = NULL,
                                                                title = NULL,
                                                                cex = 0.6,
                                                                showLegend = TRUE,
                                                                colorBy = "analyses",
                                                                interactive = TRUE) {
  spec <- get_spectra(x, analyses = analyses, useRawData = FALSE, useLoadedData = TRUE)

  if (nrow(spec) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }

  if (!("baseline" %in% colnames(spec) && "raw" %in% colnames(spec))) {
    warning("Baseline not found!")
    return(NULL)
  }

  if ("replicates" %in% colorBy && !"replicate" %in% colnames(spec)) {
    spec$replicate <- x$replicates[spec$analysis]
  }

  if (xVal == "mz" && (!"mz" %in% colnames(spec)) && "mass" %in% colnames(spec)) xVal <- "mass"

  if (!xVal %in% colnames(spec)) {
    message("\U2717 xVal not found in spectra data.table!")
    return(NULL)
  }

  spec$x <- spec[[xVal]]

  if ("feature" %in% colnames(spec)) spec$id <- spec$feature

  if ("replicates" %in% colorBy && !"replicate" %in% colnames(spec)) {
    spec$replicate <- x$replicates[spec$analysis]
  }

  spec <- .make_colorBy_varkey(spec, colorBy, legendNames = NULL)

  unique_key <- c("analysis", "var", "x")

  intensity <- NULL
  baseline <- NULL
  raw <- NULL

  spec <- spec[, .(intensity = max(intensity), baseline = max(baseline), raw = max(raw)), by = c(unique_key)]

  spec <- unique(spec)

  if ("rt" %in% xVal) {
    if (is.null(xLab)) xLab <- "Retention time / seconds"
  } else if ("mz" %in% xVal) {
    if (is.null(xLab)) {
      if (interactive) {
        xLab <- "<i>m/z</i> / Da"
      } else {
        xLab <- expression(italic("m/z ") / " Da")
      }
    }
  } else if ("mass" %in% xVal) {
    if (is.null(xLab)) xLab <- "Mass / Da"
  } else if ("mobility" %in% xVal) {
    if (is.null(xLab)) xLab <- "mobility time / milliseconds"
  }

  if (is.null(yLab)) yLab <- "Intensity / counts"

  setorder(spec, var, x)

  if (!interactive) {
    return(.plot_x_spectra_baseline_static(spec, xLab, yLab, title, cex, showLegend))
  } else {
    return(.plot_x_spectra_baseline_interactive(spec, xLab, yLab, title, colorBy))
  }
}

# MARK: plot_spectra_charges
## plot_spectra_charges -----
#' @export
#' @noRd
S7::method(plot_spectra_charges, MassSpecAnalyses) <- function(x,
                                                               analyses = NULL,
                                                               legendNames = NULL,
                                                               title = NULL,
                                                               colorBy = "analyses",
                                                               showLegend = TRUE,
                                                               xlim = NULL,
                                                               ylim = NULL,
                                                               cex = 0.6,
                                                               xLab = NULL,
                                                               yLab = NULL,
                                                               interactive = TRUE) {
  analyses <- .check_analyses_argument(x, analyses)

  if (is.null(analyses)) {
    return(NULL)
  }

  if (!x$has_spectra) {
    return(NULL)
  }

  if (length(x$spectra$charges) == 0) {
    message("\U2717 Spectra charges not found!")
    return(NULL)
  }

  res <- x$spectra$charges

  res <- rbindlist(res, idcol = "analysis", fill = TRUE)

  res <- res[res$analysis %in% analyses, ]

  if (nrow(res) == 0) {
    message("\U2717 Spectra charges not found for the targets!")
    return(NULL)
  }

  res$replicate <- x$replicates[res$analysis]

  if (!interactive) {
    .plot_spec_charges_static(res, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_spec_charges_interactive(res, legendNames, colorBy, title, showLegend, xLab, yLab)
  }
}

# MARK: plot_spectra_peaks
## plot_spectra_peaks -----
#' @export
#' @noRd
S7::method(plot_spectra_peaks, MassSpecAnalyses) <- function(x,
                                                             analyses = NULL,
                                                             legendNames = NULL,
                                                             title = NULL,
                                                             colorBy = "targets",
                                                             showLegend = TRUE,
                                                             xlim = NULL,
                                                             ylim = NULL,
                                                             cex = 0.6,
                                                             xLab = NULL,
                                                             yLab = NULL,
                                                             interactive = TRUE) {
  pks <- get_spectra_peaks(x, analyses = analyses)

  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found!")
    return(NULL)
  }

  chroms <- get_spectra(x, analyses = analyses, useRawData = FALSE, useLoadedData = TRUE)

  if (nrow(chroms) == 0) {
    message("\U2717 Chromatograms not found!")
    return(NULL)
  }

  if ("replicates" %in% colorBy && !"replicate" %in% colnames(pks)) {
    pks$replicate <- x$replicates[pks$analysis]
    chroms$replicate <- x$replicates[chroms$analysis]
  }

  if ("mass" %in% colnames(chroms)) {
    chroms$rt <- chroms$mass
    pks$rt <- pks$mass
    pks$rtmin <- pks$min
    pks$rtmax <- pks$max
    if (is.null(xLab)) xLab <- "Mass / Da"
  } else if ("mz" %in% colnames(chroms)) {
    chroms$rt <- chroms$mz
    pks$rt <- pks$mz
    pks$rtmin <- pks$min
    pks$rtmax <- pks$max
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
  }

  if (!interactive) {
    .plot_chrom_peaks_static(chroms, pks, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_chrom_peaks_interactive(chroms, pks, legendNames, colorBy, title, showLegend, xLab, yLab)
  }
}

# MARK: Chromatograms
# Chromatograms ------

# MARK: get_chromatograms
## get_chromatograms -----
#' @export
#' @noRd
S7::method(get_chromatograms, MassSpecAnalyses) <- function(x,
                                                            analyses = NULL,
                                                            chromatograms = NULL,
                                                            rtmin = 0,
                                                            rtmax = 0,
                                                            minIntensity = NULL,
                                                            useRawData = FALSE,
                                                            useLoadedData = TRUE) {
  analyses <- .check_analyses_argument(x, analyses)

  if (is.null(analyses)) {
    return(data.table())
  }

  # __________________________________________________________________
  # Extracts chromatograms from results via the active binding
  # __________________________________________________________________
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

    if (is.numeric(rtmin) && is.numeric(rtmax)) {
      if (rtmax > 0) chroms <- chroms[chroms$rt >= rtmin & chroms$rt <= rtmax]
    }

    chroms

    # __________________________________________________________________
    # Extracts loaded chromatograms
    # __________________________________________________________________
  } else if (any(x$has_loaded_chromatograms[analyses]) && useLoadedData && !useRawData) {
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

    if (is.numeric(rtmin) && is.numeric(rtmax)) {
      if (rtmax > 0) chroms <- chroms[chroms$rt >= rtmin & chroms$rt <= rtmax]
    }

    chroms

    # __________________________________________________________________
    # Extracts chromatograms from raw files
    # __________________________________________________________________
  } else {
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

      cache <- StreamFind:::.load_chache("parsed_ms_chromatograms", z$file, idx)

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
        StreamFind:::.save_cache("parsed_ms_chromatograms", chrom, cache$hash)
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
}

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
  analyses <- .check_analyses_argument(x, analyses)
  chroms <- get_chromatograms(
    x,
    analyses,
    chromatograms,
    rtmin,
    rtmax,
    minIntensity,
    useRawData = TRUE,
    useLoadedData = FALSE
  )
  if (nrow(chroms) > 0) {
    split_vector <- chroms$analysis
    chroms$analysis <- NULL
    chroms$replicate <- NULL
    chroms <- split(chroms, split_vector)
    chroms <- StreamFind::Chromatograms(chroms, is_averaged = FALSE)
    x$chromatograms <- chroms
  } else {
    warning("Not done! Chromatograms not found.")
  }
  return(x)
}

# MARK: get_chromatograms_peaks
## get_chromatograms_peaks -----
#' @export
#' @noRd
S7::method(get_chromatograms_peaks, MassSpecAnalyses) <- function(x, analyses = NULL, chromatograms = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }

  if (!x$has_chromatograms) {
    return(data.table::data.table())
  }

  pks <- x$chromatograms$peaks
  if (length(pks) == 0) {
    return(data.table::data.table())
  }

  if (x$chromatograms$is_averaged) {
    pks <- data.table::rbindlist(x$chromatograms$peaks, idcol = "replicate", fill = TRUE)
  } else {
    pks <- data.table::rbindlist(x$chromatograms$peaks, idcol = "analysis", fill = TRUE)
  }

  if ("analysis" %in% colnames(pks)) {
    pks <- pks[pks$analysis %in% analyses, ]
  } else if ("replicate" %in% colnames(pks)) {
    rpl <- x$replicates
    rpl <- rpl[analyses]
    pks <- pks[pks$replicate %in% unname(rpl)]

    if (!"analysis" %in% colnames(pks)) {
      pks$analysis <- pks$replicate
      data.table::setcolorder(pks, c("analysis", "replicate"))
    }
  }

  if (is.numeric(chromatograms)) {
    which_pks <- pks$index %in% chromatograms
    pks <- pks[which_pks, ]
  } else if (is.character(chromatograms)) {
    which_pks <- pks$id %in% chromatograms
    pks <- pks[which_pks, ]
  } else if (!is.null(chromatograms)) {
    return(data.table::data.table())
  }

  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found for the targets!")
    return(data.table::data.table())
  }

  pks
}

# MARK: plot_chromatograms
## plot_chromatograms -----
#' @export
#' @noRd
S7::method(plot_chromatograms, MassSpecAnalyses) <- function(x,
                                                             analyses = NULL,
                                                             chromatograms = NULL,
                                                             rtmin = 0,
                                                             rtmax = 0,
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
  chromatograms <- StreamFind::get_chromatograms(
    x, analyses,
    chromatograms,
    rtmin, rtmax,
    minIntensity,
    useRawData,
    useLoadedData
  )

  if (nrow(chromatograms) == 0) {
    message("\U2717 Chromatograms not found for the analyses!")
    return(NULL)
  }

  if (grepl("replicates", colorBy, fixed = FALSE)) {
    chromatograms$replicate <- x$replicates[chromatograms$analysis]
  }

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

# MARK: plot_chromatograms_peaks
## plot_chromatograms_peaks -----
#' @export
#' @noRd
S7::method(plot_chromatograms_peaks, MassSpecAnalyses) <- function(x,
                                                                   analyses = NULL,
                                                                   chromatograms = NULL,
                                                                   legendNames = NULL,
                                                                   title = NULL,
                                                                   colorBy = "targets",
                                                                   showLegend = TRUE,
                                                                   xlim = NULL,
                                                                   ylim = NULL,
                                                                   cex = 0.6,
                                                                   xLab = NULL,
                                                                   yLab = NULL,
                                                                   interactive = TRUE) {
  pks <- get_chromatograms_peaks(x, analyses = analyses, chromatograms = chromatograms)

  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found!")
    return(NULL)
  }

  chroms <- get_chromatograms(
    x,
    analyses = analyses,
    chromatograms = chromatograms,
    useRawData = FALSE,
    useLoadedData = TRUE
  )

  if (nrow(chroms) == 0) {
    message("\U2717 Chromatograms not found!")
    return(NULL)
  }

  if (grepl("replicates", colorBy, fixed = FALSE) && !"replicate" %in% colnames(pks)) {
    pks$replicate <- x$replicates[pks$analysis]
    chroms$replicate <- x$replicates[chroms$analysis]
  }

  if (!interactive) {
    .plot_chrom_peaks_static(chroms, pks, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_chrom_peaks_interactive(chroms, pks, legendNames, colorBy, title, showLegend, xLab, yLab)
  }
}

# MARK: plot_chromatograms_baseline
## plot_chromatograms_baseline -----
#' @export
#' @noRd
S7::method(plot_chromatograms_baseline, MassSpecAnalyses) <- function(x,
                                                                      analyses = NULL,
                                                                      chromatograms = NULL,
                                                                      xLab = NULL,
                                                                      yLab = NULL,
                                                                      title = NULL,
                                                                      cex = 0.6,
                                                                      showLegend = TRUE,
                                                                      colorBy = "analyses",
                                                                      interactive = TRUE) {
  chroms <- get_chromatograms(x, analyses, chromatograms, minIntensity = 0, useRawData = FALSE, useLoadedData = TRUE)

  if (!("baseline" %in% colnames(chroms) && "raw" %in% colnames(chroms))) {
    warning("Baseline not found!")
    return(NULL)
  }

  if ("replicates" %in% colorBy && !"replicate" %in% colnames(chroms)) {
    chroms$replicate <- x$replicates[chroms$analysis]
  }

  chroms <- .make_colorBy_varkey(chroms, colorBy, legendNames = NULL)

  setnames(chroms, "rt", "x")

  if (is.null(xLab)) xLab <- "Retention time / seconds"

  if (is.null(yLab)) yLab <- "Intensity / counts"

  if (!interactive) {
    return(.plot_x_spectra_baseline_static(chroms, xLab, yLab, title, cex, showLegend))
  } else {
    return(.plot_x_spectra_baseline_interactive(chroms, xLab, yLab, title, colorBy))
  }
}

# MARK: Get NTS
# Get NTS ------

# MARK: get_features_count
## get_features_count -----
#' @export
#' @noRd
S7::method(get_features_count, MassSpecAnalyses) <- function(x, analyses = NULL, filtered = FALSE) {
  analyses <- .check_analyses_argument(x, analyses)
  info <- data.table::data.table()
  if (x$has_nts) {
    if (x$nts$has_features) {
      info <- data.table::data.table(
        "analysis" = x@nts@analyses_info$analysis,
        "features" = x@nts@number_features,
        "filtered" = x@nts@number_filtered_features,
        "groups" = x@nts@number_groups
      )
      if (filtered) {
        info$features <- info$filtered + info$features
      }
      info$replicate <- x$replicates[info$analysis]
      info <- info[info$analysis %in% analyses, ]
    }
  }
  info
}

# MARK: get_features
## get_features -----
#' @export
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

  if (is.null(analyses)) {
    return(data.table::data.table())
  }

  fts <- NULL

  if (x$has_nts) fts <- x$nts$feature_list[analyses]

  if (is.null(fts)) {
    return(data.table::data.table())
  }

  fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

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
      if (all(colnames(fts) %in% colnames(target_id))) {
        return(target_id)
      }

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

    return(data.table::data.table())
  }

  polarities <- x$spectra_polarity[analyses]

  id <- NULL

  targets <- MassSpecTargets(mass, mz, rt, mobility, ppm, sec, millisec, id, analyses, polarities)

  targets <- targets@targets

  if (nrow(targets) > 0) {
    for (i in seq_len(nrow(targets))) {
      if (targets$rtmax[i] == 0) targets$rtmax[i] <- max(fts$rtmax)

      if (targets$mzmax[i] == 0) targets$mzmax[i] <- max(fts$mz)

      if ("mobility" %in% colnames(fts)) {
        if (targets$mobilitymax[i] == 0) targets$mobilitymax[i] <- max(fts$mobility)
      }
    }

    sel <- rep(FALSE, nrow(fts))

    ids <- rep(NA_character_, nrow(fts))

    if ("polarity" %in% colnames(targets) && nrow(targets) > 0) {
      for (i in seq_len(nrow(targets))) {
        if (targets$polarity[i] == "positive") targets$polarity[i] <- 1
        if (targets$polarity[i] == "negative") targets$polarity[i] <- -1
      }
    }

    for (i in seq_len(nrow(targets))) {
      if ("mobility" %in% colnames(fts)) {
        sel[fts$analysis == targets$analysis[i] & fts$polarity == targets$polarity[i] &
          data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
          data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
          data.table::between(fts$mobility, targets$mobilitymin[i], targets$mobilitymax[i])] <- TRUE

        ids[fts$analysis == targets$analysis[i] & fts$polarity == targets$polarity[i] &
          data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
          data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
          data.table::between(fts$mobility, targets$mobilitymin[i], targets$mobilitymax[i])] <- targets$id[i]
      } else {
        sel[fts$analysis == targets$analysis[i] & fts$polarity == targets$polarity[i] &
          data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
          data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE

        ids[fts$analysis == targets$analysis[i] & fts$polarity == targets$polarity[i] &
          data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
          data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- targets$id[i]
      }
    }

    fts$name <- ids

    fts$replicate <- x$replicates[fts$analysis]

    return(fts[sel])
  }

  fts$replicate <- x$replicates[fts$analysis]

  fts
}

# MARK: get_features_eic
## get_features_eic -----
#' @export
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
                                                           rtExpand = 0,
                                                           mzExpand = 0,
                                                           filtered = FALSE,
                                                           useLoadedData = TRUE) {
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)

  if (nrow(fts) == 0) {
    return(data.table())
  }

  if (useLoadedData) {
    if (x$nts$has_features_eic) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }

  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_list <- x$analyses[names(fts_list)]

    fts <- rcpp_ms_load_features_eic(
      analyses = ana_list,
      features = fts_list,
      filtered = filtered,
      rtExpand = rtExpand,
      mzExpand = mzExpand,
      minTracesIntensity = 0
    )

    fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  } else {
    sel <- vapply(fts$eic, function(z) {
      if (length(z) == 0) {
        return(FALSE)
      }
      if (is.data.frame(z)) if (nrow(z) == 0) {
        return(FALSE)
      }
      TRUE
    }, TRUE)
    fts_without_eic <- fts[!sel, ]
    fts_with_eic <- fts[sel, ]

    if (nrow(fts_without_eic) > 0) {
      fts_without_eic_ana_split_vector <- fts_without_eic$analysis
      fts_without_eic$analysis <- NULL
      fts_without_eic_list <- split(fts_without_eic, fts_without_eic_ana_split_vector)
      ana_list <- x$analyses[names(fts_without_eic_list)]

      fts_without_eic <- rcpp_ms_load_features_eic(
        analyses = ana_list,
        features = fts_without_eic_list,
        filtered = filtered,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        minTracesIntensity = 0
      )

      fts_without_eic <- data.table::rbindlist(fts_without_eic, idcol = "analysis", fill = TRUE)

      fts <- data.table::rbindlist(list(fts_without_eic, fts_with_eic), fill = TRUE)
    }
  }

  eic_list <- lapply(seq_len(nrow(fts)), function(z, fts) {
    temp <- fts[z, ]
    temp_ms <- temp[["eic"]][[1]]
    if (is.null(temp_ms)) {
      return(data.table::data.table())
    }
    if (!is.data.frame(temp_ms)) temp_ms <- data.table::as.data.table(temp_ms)
    temp_ms$analysis <- temp$analysis
    temp_ms$feature <- temp$feature
    temp_ms
  }, fts = fts)

  eic <- data.table::rbindlist(eic_list, fill = TRUE)
  data.table::setcolorder(eic, c("analysis", "feature"))

  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_eic_id <- paste0(eic$analysis, "-", eic$feature)

  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    eic$group <- fgs[unique_eic_id]
  }

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    eic$name <- tar_ids[unique_eic_id]
  }

  eic
}

# MARK: get_features_ms1
## get_features_ms1 -----
#' @export
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

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  if (useLoadedData) {
    if (x$nts$has_features_ms1) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }

  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_list <- x$analyses[names(fts_list)]

    fts <- rcpp_ms_load_features_ms1(
      analyses = ana_list,
      features = fts_list,
      filtered = filtered,
      rtWindow = rtWindow,
      mzWindow = mzWindow,
      minTracesIntensity = minIntensity,
      mzClust = mzClust,
      presence = presence
    )

    fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  }

  ms1_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
    temp <- fts[x, ]
    temp_ms <- temp[["ms1"]][[1]]
    if (is.null(temp_ms)) {
      return(data.table::data.table())
    }
    temp_ms$analysis <- temp$analysis
    temp_ms$feature <- temp$feature
    temp_ms
  }, fts = fts)

  ms1 <- data.table::rbindlist(ms1_list, fill = TRUE)
  data.table::setcolorder(ms1, c("analysis", "feature"))

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

  ms1
}

# MARK: get_features_ms2
## get_features_ms2 -----
#' @export
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

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  if (useLoadedData) {
    if (x$nts$has_features_ms1) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }

  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_list <- x$analyses[names(fts_list)]

    fts <- rcpp_ms_load_features_ms2(
      analyses = ana_list,
      features = fts_list,
      filtered = filtered,
      minTracesIntensity = minIntensity,
      isolationWindow = isolationWindow,
      mzClust = mzClust,
      presence = presence
    )

    fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  }

  ms2_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
    temp <- fts[x, ]
    temp_ms <- temp[["ms2"]][[1]]
    if (is.null(temp_ms)) {
      return(data.table::data.table())
    }
    temp_ms$analysis <- temp$analysis
    temp_ms$feature <- temp$feature
    temp_ms
  }, fts = fts)

  ms2 <- data.table::rbindlist(ms2_list, fill = TRUE)
  data.table::setcolorder(ms2, c("analysis", "feature"))

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

  ms2
}

# MARK: get_groups
## get_groups -----
#' @export
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
                                                     metadata = FALSE,
                                                     correctSuppression = FALSE) {
  if (!x$has_nts) {
    return(data.table::data.table())
  }

  if (!x$nts$has_groups) {
    return(data.table::data.table())
  }

  fts <- get_features(
    x,
    analyses = NULL,
    features = groups,
    mass, mz, rt, mobility, ppm, sec, millisec,
    filtered = filtered
  )

  if (correctSuppression) {
    if ("suppression_factor" %in% colnames(fts)) {
      fts$intensity <- fts$intensity * fts$suppression_factor
    }
  }

  if (nrow(fts) > 0) {
    g_ids <- unique(fts$group)

    fgroups <- data.table::data.table("group" = g_ids)

    if (intensities) {
      if (average) {
        intensity <- NULL
        rpls <- x$replicates
        fts_temp <- data.table::copy(fts)
        fts_temp$analysis <- rpls[fts_temp$analysis]
        fts_av <- fts_temp[, .(intensity = mean(intensity), sd = sd(intensity), n = length(intensity)), by = c("group", "analysis")]
        fts_av$sd[is.na(fts_av$sd)] <- 0
        fts_av$sd <- round(fts_av$sd / fts_av$intensity * 100, digits = 0)

        fts_sd <- data.table::copy(fts_av)
        fts_n <- data.table::copy(fts_av)

        fts_sd$intensity <- NULL
        fts_sd$n <- NULL
        fts_sd$analysis <- paste(fts_sd$analysis, "_sd", sep = "")
        fts_sd <- data.table::dcast(fts_sd[, c("group", "analysis", "sd"), with = TRUE], group ~ analysis, value.var = "sd")
        fts_sd[is.na(fts_sd)] <- 0

        tbl_rpls <- table(rpls)
        fts_n$n <- tbl_rpls[fts_n$analysis]
        # fts_n$n <- round(fts_n$n / fts_n$tn * 100, digits = 0)
        fts_n$intensity <- NULL
        # fts_n$tn <- NULL
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

      if (!"istd" %in% cols) fts[["istd"]] <- list(rep(list(), nrow(fts)))
      if (!"quality" %in% cols) fts[["quality"]] <- list(rep(list(), nrow(fts)))
      if (!"annotation" %in% cols) fts[["isotope"]] <- list(rep(list(), nrow(fts)))

      rtmin <- NULL
      rtmax <- NULL
      mzmin <- NULL
      mzmax <- NULL
      feature <- NULL
      quality <- NULL
      annotation <- NULL
      istd <- NULL

      fts_meta <- fts[, .(
        rt = round(mean(rt), digits = 0),
        mass = round(mean(mass), digits = 4),
        rtdev = round(max(rtmax - rtmin), digits = 0),
        massdev = round(max(mzmax - mzmin), digits = 4),
        presence = round(length(feature) / length(x) * 100, digits = 1),
        maxint = round(max(intensity), digits = 0),
        sn = round(max(vapply(quality, function(z) if (length(z) > 0) z$sn else 0, 0), na.rm = TRUE), digits = 1),
        iso = min(vapply(annotation, function(z) if (length(z) > 0) z$iso_step else 0, 0)),
        istd = paste0(unique(vapply(istd, function(z) if (length(z) > 0) z$name else NA_character_, NA_character_)), collapse = "; "),
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
    data.table::data.table()
  }
}

# MARK: get_groups_ms1
## get_groups_ms1 -----
#' @export
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

  if (nrow(fgs) == 0) {
    return(data.table::data.table())
  }

  fts <- get_features(x, features = fgs$group)

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

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

  if (nrow(ms1) == 0) {
    return(data.table::data.table())
  }

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

  ms1_df <- data.table::rbindlist(ms1_list, fill = TRUE)

  ms1_df$group <- ms1_df$id

  ms1_df[["id"]] <- NULL

  ms1_df <- ms1_df[order(ms1_df$mz), ]

  ms1_df <- ms1_df[order(ms1_df$group), ]

  if ("groups" %in% groupBy) {
    ms1_df[["analysis"]] <- NULL
  } else {
    ms1_df <- ms1_df[order(ms1_df$analysis), ]
    data.table::setnames(ms1_df, "analysis", "replicate")
  }

  if ("name" %in% colnames(fgs)) {
    tar_ids <- fgs$name
    names(tar_ids) <- fgs$group
    ms1_df$name <- tar_ids[ms1_df$group]
  }

  data.table::copy(ms1_df)
}

# MARK: get_groups_ms2
## get_groups_ms2 -----
#' @export
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

  if (nrow(fgs) == 0) {
    return(data.table::data.table())
  }

  fts <- get_features(x, features = fgs$group, filtered = filtered)

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

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

  if (nrow(ms2) == 0) {
    return(data.table::data.table())
  }

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

  ms2_df <- data.table::rbindlist(ms2_list, fill = TRUE)

  ms2_df$group <- ms2_df$id

  ms2_df[["id"]] <- NULL

  ms2_df <- ms2_df[order(ms2_df$mz), ]

  ms2_df <- ms2_df[order(ms2_df$group), ]

  if ("groups" %in% groupBy) {
    ms2_df[["analysis"]] <- NULL
  } else {
    ms2_df <- ms2_df[order(ms2_df$analysis), ]
    data.table::setnames(ms2_df, "analysis", "replicate")
  }

  if ("name" %in% colnames(fgs)) {
    tar_ids <- fgs$name
    names(tar_ids) <- fgs$group
    ms2_df$name <- tar_ids[ms2_df$group]
  }

  data.table::copy(ms2_df)
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
  if (!x$has_nts) {
    warning("No NTS results found!")
    return(data.table::data.table())
  }

  if (!x$nts$has_features) {
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

  all_fts <- x$nts$feature_list

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
  if (!x$has_nts) {
    warning("No NTS results found!")
    return(data.table::data.table())
  }

  if (!x$nts$has_features) {
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

  if (nrow(suspects) > 0 && !filtered && x$nts$has_groups && onGroups) {
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

      if (x$nts$has_groups && average) {
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

        if (x$nts$has_groups) cols <- c(cols, "group")

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
  if (!x$has_nts) {
    warning("No NTS results found!")
    return(data.table::data.table())
  }

  if (!x$nts$has_features) {
    warning("Features not found!")
    return(data.table::data.table())
  }

  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for targets!")
    return(data.table::data.table())
  }

  compounds <- fts$compounds

  if (!averaged && x$nts$has_groups) {
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
    if (averaged && x$nts$has_groups) {
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
  if (!x$has_nts) {
    warning("\U2717 NTS resuts not found!")
    return(NULL)
  }

  if (!x$nts$has_groups) {
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

# MARK: plot_features_count
## plot_features_count -----
#' @export
#' @noRd
S7::method(plot_features_count, MassSpecAnalyses) <- function(x,
                                                              analyses = NULL,
                                                              filtered = FALSE,
                                                              yLab = NULL,
                                                              title = NULL,
                                                              colorBy = "analyses",
                                                              showLegend = TRUE,
                                                              showHoverText = TRUE) {
  info <- get_features_count(x, analyses, filtered)

  if ("replicates" %in% colorBy) info$analysis <- info$replicate

  features <- NULL

  info <- info[, .(
    features = round(mean(features), digits = 0),
    features_sd = round(sd(features), digits = 0),
    n_analysis = length(features)
  ), by = c("analysis")]

  info$features_sd[is.na(info$features_sd)] <- 0

  info <- unique(info)
  
  if (showHoverText) {
    info$hover_text <- paste(
      info$analysis, "<br>",
      "N.: ", info$n_analysis, "<br>",
      "Features: ", info$features, " (SD: ", info$features_sd, ")"
    )
  } else {
    info$hover_text <- ""
  }

  info <- info[order(info$analysis), ]

  colors_tag <- StreamFind:::.get_colors(info$analysis)

  if (is.null(yLab)) yLab <- "Number of features"

  plotly::plot_ly(
    x = info$analysis,
    y = info$features,
    marker = list(color = unname(colors_tag)),
    type = "bar",
    text = info$hover_text,
    hoverinfo = "text",
    error_y = list(
      type = "data",
      array = info$features_sd,
      color = "darkred",
      symmetric = FALSE,
      visible = TRUE
    ),
    name = names(colors_tag),
    showlegend = showLegend
  ) %>% plotly::layout(
    xaxis = list(title = NULL),
    yaxis = list(title = yLab)
  )
}

# MARK: plot_features
## plot_features -----
#' @export
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
                                                        mzExpand = 0.001,
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

  eic <- eic[, `:=`(intensity = max(intensity)), by = c("analysis", "polarity", "feature", "rt")][]

  eic <- unique(eic)

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

# MARK: map_features
## map_features -----
#' @export
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
                                                       neutral_mass = TRUE,
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

  if (neutral_mass) {
    fts$mzmin <- fts$mass - (fts$mz - fts$mzmin)
    fts$mzmax <- fts$mass + (fts$mzmax - fts$mz)
    fts$mz <- fts$mass
  }

  if (!interactive) {
    .map_features_static(fts, colorBy, legendNames, xLab, yLab, title, showLegend, xlim, ylim, cex)
  } else {
    .map_features_interactive(fts, colorBy, legendNames, xlim, ylim, xLab, yLab, title)
  }
}

# MARK: map_features_intensity
## map_features_intensity -----
#' @export
#' @noRd
S7::method(map_features_intensity, MassSpecAnalyses) <- function(x,
                                                                 analyses = NULL,
                                                                 features = NULL,
                                                                 mass = NULL,
                                                                 mz = NULL,
                                                                 rt = NULL,
                                                                 mobility = NULL,
                                                                 ppm = 20,
                                                                 sec = 60,
                                                                 millisec = 5,
                                                                 neutral_mass = TRUE,
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

  fts <- .make_colorBy_varkey(fts, colorBy, legendNames)

  if (!interactive) {
    .map_features_static(fts, colorBy, legendNames, xLab, yLab, title, showLegend, xlim, ylim, cex)
  } else {
    plotly::plot_ly(
      data = fts,
      x = ~rt,
      y = ~intensity,
      color = ~var,
      type = "scatter",
      mode = "markers",
      colors = StreamFind:::.get_colors(unique(fts$var)),
      text = ~ paste(
        "<br>Analysis: ", analysis,
        "<br>Mass: ", mass,
        "<br>MZ: ", mz,
        "<br>RT: ", rt,
        "<br>Intensity: ", intensity
      ),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = NULL,
        xaxis = list(title = "Retention Time (Seconds)"),
        yaxis = list(title = "Intensity (Counts)"),
        legend = list(title = "Analysis")
      )
  }
}

# MARK: plot_features_ms1
## plot_features_ms1 -----
#' @export
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
  ms1 <- get_features_ms1(
    x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
    rtWindow, mzWindow, mzClust, presence, minIntensity, filtered, useLoadedData
  )

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

# MARK: plot_features_ms2
## plot_features_ms2 -----
#' @export
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
  ms2 <- get_features_ms2(
    x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
    isolationWindow, mzClust, presence, minIntensity, filtered, useLoadedData
  )

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

# MARK: plot_groups
## plot_groups -----
#' @export
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
                                                      mzExpand = 0.001,
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

  plot_features(
    x,
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

# MARK: plot_groups_ms1
## plot_groups_ms1 -----
#' @export
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

  ms1 <- get_groups_ms1(
    x, groups, mass, mz, rt, mobility, ppm, sec, millisec, rtWindow, mzWindow, mzClustFeatures,
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

# MARK: plot_groups_ms2
## plot_groups_ms2 -----
#' @export
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

  ms2 <- get_groups_ms2(
    x, groups, mass, mz, rt, mobility, ppm, sec, millisec, isolationWindow, mzClustFeatures,
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
  
  if (averaged && x$nts$has_groups) {
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

  if (!x$has_nts) {
    return(NULL)
  }

  if (x$nts$has_groups) {
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
  if (!x$has_nts) {
    return(NULL)
  }

  if (!x$nts$has_features_suspects) {
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
.get_MassSpecAnalysis_from_files <- function(files = NULL, centroid = FALSE, levels = c(1, 2)) {
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

            files <- c(files[!files %in% files_to_convert], files_converted[!files_converted %in% files])
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

        concentration <- suppressWarnings(as.numeric(ana$name))

        if (is.na(concentration)) {
          ana$concentration <- NA_real_
        } else {
          ana$concentration <- concentration
        }

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
