#' @export
#' @noRd
RamanAnalyses <- S7::new_class("RamanAnalyses", package = "StreamFind", parent = Analyses,

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
    
    ## __info -----
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (length(self) > 0) {
        df <- data.table::data.table(
          "analysis" = vapply(self@analyses, function(x) x$name, ""),
          "replicate" = vapply(self@analyses, function(x) x$replicate, ""),
          "blank" = vapply(self@analyses, function(x) x$blank, ""),
          "type" = vapply(self@analyses, function(x) x$type, ""),
          "spectra" = vapply(self@analyses, function(x) nrow(x$spectra), 0)
        )
        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    }),
    
    ## __raw_spectra -----
    raw_spectra = S7::new_property(S7::class_list, getter = function(self) {
      if (length(self) > 0) {
        lapply(self@analyses, function(x) x$spectra)
      } else {
        list()
      }
    }),
    
    ## __has_spectra -----
    has_spectra = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      if (is.null(self@results[["spectra"]])) {
        if (length(self$raw_spectra) == 0) return(FALSE)
      } else{
        if (!is(self@results[["spectra"]], "StreamFind::Spectra")) return(FALSE)
      }
      TRUE
    }),
    
    ## __spectra -----
    spectra = S7::new_property(S7::class_data.frame,
      getter = function(self) {
        if (!is.null(self@results[["spectra"]])) return(self@results[["spectra"]])
        StreamFind::Spectra(self$raw_spectra)
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
            replicate_names <- unique(unname(self$replicates))
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
        self
      }
    )
  ),

  constructor = function(files = NULL) {
    analyses <- .get_RamanAnalysis_from_files(files)
    S7::new_object(Analyses(), possible_formats = c(".asc"), analyses = analyses)
  },

  validator = function(self) {
    valid <- all(
      checkmate::test_true(identical(self@possible_formats, c(".asc"))),
      if (length(self) > 0) checkmate::test_true(identical(names(self@analyses), unname(self@names)))
    )
    if (!valid) return(FALSE)
    NULL
  }
)

# Methods -----

#' @export
#' @noRd
S7::method(names, RamanAnalyses) <- function(x) {
  vapply(x@analyses, function(x) x$name, NA_character_)
}

#' @export
#' @noRd
S7::method(add, RamanAnalyses) <- function(x, value) {
  
  if (is.character(value)) {
    if (grepl(x@possible_formats, value)) {
      value <- .get_RamanAnalysis_from_files(value)
    } else {
      warning("File/s not valid!")
      return(x)
    }
  }
  
  if (!all(vapply(value, function(a) is(a, "RamanAnalysis"), FALSE))) {
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
S7::method(remove, RamanAnalyses) <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!x$names %in% value]
    x@analyses <- x@analyses[order(names(x@analyses))]
  } else if (is.numeric(value)) {
    x@analyses <- x@analyses[-value]
    x@analyses <- x@analyses[order(names(x@analyses))]
  }
  if (x@has_processed_spectra) {
    Spectra_names <- names(x@results$Spectra)
    if (!x@results$Spectra$is_averaged) {
      x@results$Spectra <- x@results$Spectra[x$names %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    } else {
      x@results$Spectra <- x@results$Spectra[x$replicates %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    }
  }
  x
}

#' @export
#' @noRd
S7::method(`[`, RamanAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (!is.null(x$results$spectra)) {
    spectra_names <- names(x@results$spectra$spectra)
    if (!x@results$spectra$is_averaged) {
      x@results$spectra <- x@results$spectra[x$names %in% spectra_names]
      x@results$spectra <- x@results$spectra[order(names(x@results$Spectra))]
    } else {
      x@results$spectra <- x@results$spectra[x$replicates %in% spectra_names]
      x@results$spectra <- x@results$spectra[order(names(x@results$Spectra))]
    }
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`[<-`, RamanAnalyses) <- function(x, i, value) {
  warning("Method not implemented in RamanAnalyses! Use add or remove methods instead.")
  return(x)
}

#' @export
#' @noRd
S7::method(`[[`, RamanAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[[i]]
  if (x@has_processed_spectra) {
    Spectra_names <- names(x@results$Spectra)
    if (!x@results$Spectra$is_averaged) {
      x@results$Spectra <- x@results$Spectra[x$names %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    } else {
      x@results$Spectra <- x@results$Spectra[x$replicates %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    }
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`[[<-`, RamanAnalyses) <- function(x, i, value) {
  warning("Method not implemented in RamanAnalyses! Use add or remove methods instead." )
  return(x)
}

# Get Methods ------

#' @export
#' @noRd
S7::method(get_spectra, RamanAnalyses) <- function(x,
                                                   analyses = NULL,
                                                   rt = NULL,
                                                   shift = NULL,
                                                   minIntensity = 0,
                                                   useRawData = FALSE) {
  
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) return(data.frame())
  
  if (x$has_spectra && !useRawData) {
    spec <- x$spectra$spectra
    
    if (x$spectra$is_averaged) {
      spec <- rbindlist(spec, idcol = "replicate", fill = TRUE)
    } else {
      spec <- rbindlist(spec, idcol = "analysis", fill = TRUE)
    }
    
    if ("analysis" %in% colnames(spec)) {
      spec <- spec[spec$analysis %in% analyses, ]
      if (!"replicate" %in% colnames(spec)) spec$replicate <- x$replicates[spec$analysis]
      
    } else if ("replicate" %in% colnames(spec)) {
      rpl <- x$replicates
      rpl <- rpl[analyses]
      spec <- spec[spec$replicate %in% unname(rpl)]
      if (!"analysis" %in% colnames(spec)) spec$analysis <- spec$replicate
    }
    
  } else {
    spec <- x$raw_spectra[analyses]
    spec <- rbindlist(spec, idcol = "analysis", fill = TRUE)
    spec$replicate <- x$replicates[spec$analysis] 
  }
  
  if (nrow(spec) == 0) {
    warning("No spectra found!")
    return(spec)
  }
  
  if (!is.null(rt) && length(rt) == 2 && "rt" %in% colnames(spec)) {
    rt_range <- sort(rt)
    sel <- spec$rt >= rt_range[1] & spec$rt <= rt_range[2]
    spec <- spec[sel, ]
  }
  
  if (!is.null(shift) && length(shift) == 2) {
    shift_range <- sort(shift)
    sel <- spec$shift >= shift_range[1] & spec$shift <= shift_range[2]
    spec <- spec[sel, ]
  }
  
  if ("rt" %in% colnames(spec)) {
    setorder(spec, analysis, rt, shift)
  } else {
    setorder(spec, analysis, shift)
  }
  
  spec <- spec[spec$intensity >= minIntensity, ]
  
  setcolorder(spec, c("analysis", "replicate"))
  spec
}

#' @export
#' @noRd
S7::method(get_spectra_matrix, RamanAnalyses) <- function(x, analyses = NULL) {
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
  
  spec_list <- spec_list[vapply(spec_list, function(z) nrow(z) > 0, FALSE)]
  
  intensity <- NULL
  
  spec_list <- lapply(spec_list, function(z) {
    z <- z[, .(intensity = mean(intensity)), by = c("shift")]
    z <- data.table::dcast(z, formula = 1 ~ shift, value.var = "intensity")[, -1]
    z
  })
  
  spec <- as.matrix(rbindlist(spec_list, fill = TRUE))
  rownames(spec) <- names(spec_list)
  attr(spec, "xValues") <- as.numeric(colnames(spec))
  spec
}

# Plot Methods ------

#' @export
#' @noRd
S7::method(plot_spectra, RamanAnalyses) <- function(x,
                                                    analyses = NULL,
                                                    rt = NULL,
                                                    shift = NULL,
                                                    minIntensity = 0,
                                                    useRawData = FALSE,
                                                    xVal = "shift",
                                                    xLab = NULL,
                                                    yLab = NULL,
                                                    title = NULL,
                                                    cex = 0.6,
                                                    showLegend = TRUE,
                                                    colorBy = "analyses",
                                                    interactive = TRUE) {
  
  spectra <- get_spectra(x, analyses, rt, shift, minIntensity, useRawData)
  
  if (nrow(spectra) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  
  intensity <- NULL
  
  if ("rt" %in% xVal) {
    spectra <- spectra[, .(intensity = sum(intensity)), by = c("analysis", "rt")]
    if (is.null(xLab)) xLab = "Retention time / seconds"
    setnames(spectra, "rt", "shift")
    
  } else if ("shift" %in% xVal) {
    spectra <- spectra[, .(intensity = mean(intensity)), by = c("analysis", "shift")]
    if (is.null(xLab)) {
      if (interactive) {
        xLab = "Raman shift / cm<sup>-1</sup>"
      } else {
        xLab = expression("Raman shift / cm"^"-1")
      }
    }
  }
  
  spectra <- unique(spectra)
  
  if (is.null(yLab)) yLab = "Raman intensity / A.U."
  
  if ("replicates" %in% colorBy) {
    if (!"replicate" %in% colnames(spectra)) {
      spectra$replicate <- x$replicates[spectra$analysis]
    }
  }
  
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
  
  setnames(spectra, "shift", "x")
  
  if (!interactive) {
    return(.plot_x_spectra_static(spectra, xLab, yLab, title, cex, showLegend))
    
  } else {
    return(.plot_x_spectra_interactive(spectra, xLab, yLab, title, colorBy))
  }
}

#' @noRd
S7::method(plot_spectra_baseline, RamanAnalyses) <- function(x,
                                                             analyses = NULL,
                                                             rt = NULL,
                                                             shift = NULL,
                                                             minIntensity = 0,
                                                             xVal = "shift",
                                                             xLab = NULL,
                                                             yLab = NULL,
                                                             title = NULL,
                                                             cex = 0.6,
                                                             showLegend = TRUE,
                                                             colorBy = "analyses",
                                                             interactive = TRUE) {
  
  spectra <- get_spectra(x, analyses, rt, shift, minIntensity, useRawData = FALSE)
  
  if (!("baseline" %in% colnames(spectra) && "raw" %in% colnames(spectra))) {
    warning("Baseline not found!")
    return(NULL)
  }
  
  baseline <- NULL
  
  if ("rt" %in% xVal) {
    spectra <- spectra[, .(baseline = sum(baseline), raw = sum(raw)), by = c("analysis", "rt")]
    
    if (is.null(xLab)) xLab = "Retention time / seconds"
    
    setnames(spectra, "rt", "shift")
    
  } else if ("shift" %in% xVal) {
    
    spectra <- spectra[, .(baseline = mean(baseline), raw = mean(raw)), by = c("analysis", "shift")]
    
    if (is.null(xLab)) {
      if (interactive) {
        xLab = "Raman shift / cm<sup>-1</sup>"
      } else {
        xLab = expression("Raman shift / cm"^"-1")
      }
    }
  }
  
  spectra <- unique(spectra)
  
  if (is.null(yLab)) yLab = "Raman intensity / A.U."
  
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
  
  setnames(spectra, "shift", "x")
  
  if (!interactive) {
    return(.plot_x_spectra_baseline_static(spectra, xLab, yLab, title, cex, showLegend))
    
  } else {
    return(.plot_x_spectra_baseline_interactive(spectra, xLab, yLab, title, colorBy))
  }
}

#' @export
#' @noRd
S7::method(plot_chromatograms, RamanAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          rt = NULL,
                                                          shift = NULL,
                                                          minIntensity = 0,
                                                          useRawData = FALSE,
                                                          xLab = NULL,
                                                          yLab = NULL,
                                                          title = NULL,
                                                          cex = 0.6,
                                                          showLegend = TRUE,
                                                          colorBy = "analyses",
                                                          interactive = TRUE) {
  
  spectra <- get_spectra(x, analyses, rt, shift, minIntensity, useRawData)
  
  if ("rt" %in% colnames(spectra)) {
    
    intensity <- NULL
    
    spectra[["shift"]] <- NULL
    
    spectra <- spectra[, .(intensity = sum(intensity)), by = c("analysis", "rt")]
    
    spectra <- unique(spectra)
    
    if (is.null(xLab)) xLab = "Retention time / seconds"
    
    spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
    
    setnames(spectra, "rt", "x")
    
    if (!interactive) {
      return(.plot_x_spectra_static(spectra, xLab, yLab, title, cex, showLegend))
      
    } else {
      return(.plot_x_spectra_interactive(spectra, xLab, yLab, title, colorBy))
    }
    
  } else {
    warning("Column rt not found in spectra data.table!")
    NULL
  }
}

# Utility functions ------

#' @noRd
.get_RamanAnalysis_from_files <- function(files = NULL) {
  if (!is.null(files)) {
    
    if (is.data.frame(files)) {
      
      if ("file" %in% colnames(files)) {
        
        if ("replicate" %in% colnames(files)) {
          replicates <- as.character(files$replicate)
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
        files <- ""
      }
      
    } else {
      replicates <- rep(NA_character_, length(files))
      blanks <- rep(NA_character_, length(files))
    }
    
    possible_ms_file_formats <- ".asc"
    
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
      return(list())
    }
    
    names(replicates) <- as.character(files)
    
    names(blanks) <- as.character(files)
    
    analyses <- lapply(files, function(x) {
      
      cache <- .load_chache("parsed_raman_analyses", x)
      
      if (!is.null(cache$data)) {
        message("\U2139 Analysis loaded from cache!")
        cache$data
        
      } else {
        
        message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
        
        ana <- rcpp_parse_asc_file(x)
        
        class_ana <- class(ana)[1]
        
        if (!class_ana %in% "RamanAnalysis") {
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
          .save_cache("parsed_raman_analyses", ana, cache$hash)
          message("\U1f5ab Parsed file cached!")
        }
        
        ana
      }
    })
    
    names(analyses) <- vapply(analyses, function(x) x[["name"]], "")
    
    analyses <- analyses[order(names(analyses))]
    
    if (all(vapply(analyses, function(x) "RamanAnalysis" %in% is(x), FALSE))) {
      analyses
    } else {
      warning("Not all added files could be converted as RamanAnalysis!")
      list()
    }
  } else {
    list()
  }
}
