
#' @export
#' @noRd
MassSpecAnalyses <- S7::new_class("MassSpecAnalyses", package = "StreamFind", parent = Analyses,

  properties = list(
    
    ## __analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),
    
    ## __names -----
    names = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$name, NA_character_)
    }),
    
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
        
        if (self$has_NTS) {
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
        if (self$has_NTS) {
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
    
    ## __has_NTS -----
    has_NTS = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      !is.null(self@results[["NTS"]])
    })
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

#' @export
#' @noRd
S7::method(names, Analyses) <- function(x) {
  vapply(self@analyses, function(x) x$name, NA_character_)
}

#' @export
#' @noRd
S7::method(add, MassSpecAnalyses) <- function(x, value) {
  
  if (is.character(value)) {
    if (grepl(x@possible_formats, value)) {
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

#' @export
#' @noRd
S7::method(remove, MassSpecAnalyses) <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!x$names %in% value]
    x@analyses <- x@analyses[order(names(x@analyses))]
    if (x@has_NTS) x@results$NTS <- x@results$NTS[!x$names %in% value]
  } else if (is.numeric(value)) {
    x@analyses <- x@analyses[-value]
    x@analyses <- x@analyses[order(names(x@analyses))]
    if (x@has_NTS) x@results$NTS <- x@results$NTS[-value]
  }
  
  x
}

#' @export
#' @noRd
S7::method(`[`, MassSpecAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (x@has_NTS) x@results$NTS <- x@results$NTS[i]
  return(x)
}

#' @export
#' @noRd
S7::method(`[<-`, Analyses) <- function(x, i, value) {
  warning("Method not implemented in MassSpecAnalyses! Use add or remove methods instead.")
  return(x)
}

#' @export
#' @noRd
S7::method(`[[`, Analyses) <- function(x, i) {
  x@analyses <- x@analyses[[i]]
  if (x@has_NTS) x@results$NTS <- x@results$NTS[[i]]
  return(x)
}

#' @export
#' @noRd
S7::method(`[[<-`, Analyses) <- function(x, i, value) {
  warning("Method not implemented in MassSpecAnalyses! Use add or remove methods instead." )
  return(x)
}

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
