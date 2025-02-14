# MARK: RamanAnalyses
# RamanAnalyses -----
#' @export
#' @noRd
RamanAnalyses <- S7::new_class("RamanAnalyses",
  package = "StreamFind", parent = Analyses,
  properties = list(

    # MARK: analyses
    ## analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),

    # MARK: replicates
    ## replicates -----
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

    # MARK: blanks
    ## blanks -----
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
        for (i in seq_len(length(self))) {
          self@analyses[[i]]$concentration <- value[i]
        }
        self
      }
    ),
    
    # MARK: references
    ## references -----
    references = S7::new_property(S7::class_character,
      getter = function(self) vapply(self@analyses, function(x) x$reference, NA_character_),
      setter = function(self, value) {
        if (length(value) != length(self)) {
          warning("Length of references not conform!")
          return(self)
        }
        if (!is.character(value)) {
          warning("References must be character!")
          return(self)
        }
        if (!all(value %in% self@replicates)) {
          warning("Reference names must be in replicate names!")
          return(self)
        }
        for (i in seq_len(length(self))) self@analyses[[i]]$reference <- value[i]
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

    # MARK: info
    ## info -----
    info = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        if (length(self) > 0) {
          df <- data.table::data.table(
            "analysis" = vapply(self@analyses, function(x) x$name, ""),
            "replicate" = vapply(self@analyses, function(x) x$replicate, ""),
            "blank" = vapply(self@analyses, function(x) x$blank, ""),
            "reference" = vapply(self@analyses, function(x) x$reference, ""),
            "type" = vapply(self@analyses, function(x) x$type, ""),
            "spectra" = vapply(self@analyses, function(x) nrow(x$spectra), 0),
            "concentration" = vapply(self@analyses, function(x) x$concentration, 0)
          )
          row.names(df) <- seq_len(nrow(df))
          df
        } else {
          data.frame()
        }
      }
    ),

    # MARK: has_spectra
    ## has_spectra -----
    has_spectra = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (length(self) == 0) {
          return(FALSE)
        }
        if (is.null(self@results[["RamanSpectra"]])) {
          if (sum(vapply(self$analyses, function(x) nrow(x$spectra), 0)) == 0) {
            return(FALSE)
          }
        } else {
          if (!is(self@results[["RamanSpectra"]], "StreamFind::RamanSpectra")) {
            return(FALSE)
          }
        }
        TRUE
      }
    ),

    # MARK: Spectra
    ## Spectra -----
    Spectra = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        if (!is.null(self@results[["RamanSpectra"]])) {
          return(self@results[["RamanSpectra"]])
        } else {
          if (length(self) > 0) {
            StreamFind::RamanSpectra(lapply(self@analyses, function(x) x$spectra))
          } else {
            StreamFind::RamanSpectra()
          }
        }
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::RamanSpectra")) {
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
          warning("Value must be an RamanSpectra results object! Not done.")
        }
        self
      }
    )
  ),

  # MARK: constructor
  ## constructor -----
  constructor = function(files = NULL) {
    analyses <- .get_RamanAnalysis_from_files(files)
    S7::new_object(
      Analyses(),
      possible_formats = c("asc", "sif", "json", "wdf", "sdf", "csv", "txt"),
      analyses = analyses
    )
  },

  # MARK: validator
  ## validator -----
  validator = function(self) {
    possible_formats <- c("asc", "sif", "json", "wdf", "sdf", "csv", "txt")
    checkmate::assert_true(identical(self@possible_formats, possible_formats))
    if (length(self) > 0) {
      checkmate::assert_true(identical(names(self@analyses), unname(names(self))))
      rpls <- self@replicates
      blks <- self@blanks
      refs <- self@references
      for (i in seq_len(length(rpls))) {
        checkmate::assert_true(blks[i] %in% rpls || is.na(blks[i]))
        checkmate::assert_true(refs[i] %in% rpls || is.na(refs[i]))
      }
    }
    NULL
  }
)

# MARK: Methods
# Methods -----

# MARK: names
## names -----
#' @export
#' @noRd
S7::method(names, RamanAnalyses) <- function(x) {
  vapply(x@analyses, function(x) x$name, NA_character_)
}

# MARK: add
## add -----
#' @export
#' @noRd
S7::method(add, RamanAnalyses) <- function(x, value) {
  if (is.character(value)) {
    if (all(vapply(value, function(z) tools::file_ext(z) %in% x@possible_formats, FALSE))) {
      value <- .get_RamanAnalysis_from_files(value)
    } else {
      warning("File/s not valid!")
      return(x)
    }
  }

  if (!all(vapply(value, function(a) {
      checkmate::test_character(a$name) &&
        checkmate::test_character(a$replicate) &&
          checkmate::test_character(a$blank) &&
            checkmate::test_character(a$reference) &&
              checkmate::test_character(a$type) &&
                checkmate::test_numeric(a$concentration) &&
                  checkmate::test_list(a$metadata) &&
                    checkmate::test_data_table(a$spectra)}, FALSE))) {
    warning("Analysis/s not valid!")
    return(x)
  }
  
  if (any(vapply(value, function(a) a$name %in% names(x), FALSE))) {
    warning("Analysis names already exist!")
    return(x)
  }
  
  value <- lapply(value, function(a) {
    if (!is(a, "RamanAnalysis")) {
      class(a) <- c("RamanAnalysis", "Analysis")
    }
    a
  })
  
  names(value) <- vapply(value, function(a) a$name, NA_character_)

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
## remove -----
#' @export
#' @noRd
S7::method(remove, RamanAnalyses) <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!names(x) %in% value]
    x@analyses <- x@analyses[order(names(x@analyses))]
  } else if (is.numeric(value)) {
    x@analyses <- x@analyses[-value]
    x@analyses <- x@analyses[order(names(x@analyses))]
  }
  if (!is.null(x@results[["RamanSpectra"]])) {
    spec <- x@results[["RamanSpectra"]]
    spectra_names <- names(spec)
    if (!spec$is_averaged) {
      spec <- spec[names(x) %in% spectra_names]
      spec <- spec[order(names(spec))]
    } else {
      spec <- spec[x$replicates %in% spectra_names]
      spec <- spec[order(names(spec))]
    }
    
    x@results[["RamanSpectra"]] <- spec
  }
  x
}

# MARK: `[`
## `[` -----
#' @export
#' @noRd
S7::method(`[`, RamanAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (!is.null(x$results[["RamanSpectra"]])) {
    spec <- x@results[["RamanSpectra"]]
    spectra_names <- names(spec)
    if (!spec$is_averaged) {
      spec <- spec[names(x) %in% spectra_names]
      spec <- spec[order(names(spec))]
    } else {
      spec <- spec[x$replicates %in% spectra_names]
      spec <- spec[order(names(spec))]
    }
    x@results[["RamanSpectra"]] <- spec
  }
  return(x)
}

# MARK: `[<-`
## `[<-` -----
#' @export
#' @noRd
S7::method(`[<-`, RamanAnalyses) <- function(x, i, value) {
  warning("Method not implemented in RamanAnalyses! Use add or remove methods instead.")
  return(x)
}

# MARK: `[[`
## `[[` -----
#' @export
#' @noRd
S7::method(`[[`, RamanAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[[i]]
  if (!is.null(x$results[["RamanSpectra"]])) {
    spec <- x@results[["RamanSpectra"]]
    spectra_names <- names(spec)
    if (!spec$is_averaged) {
      spec <- spec[names(x) %in% spectra_names]
      spec <- spec[order(names(spec))]
    } else {
      spec <- spec[x$replicates %in% spectra_names]
      spec <- spec[order(names(spec))]
    }
    x@results[["RamanSpectra"]] <- spec
  }
  return(x)
}

# MARK: `[[<-`
## `[[<-` -----
#' @export
#' @noRd
S7::method(`[[<-`, RamanAnalyses) <- function(x, i, value) {
  warning("Method not implemented in RamanAnalyses! Use add or remove methods instead.")
  return(x)
}

# MARK: `c`
## `c` -----
#' @export
#' @noRd
S7::method(`c`, RamanAnalyses) <- function(x, ...) {
  dots <- list(...)
  
  if (length(dots) == 0) {
    return(x)
  }
  
  dots <- dots[vapply(dots, function(z) is(z, "StreamFind::RamanAnalyses"), FALSE)]
  
  if (length(dots) == 0) {
    return(x)
  }
  
  comb_analyses <- x@analyses
  
  comb_results <- x@results[["RamanSpectra"]]
  
  for (i in seq_along(dots)) {
    comb_analyses <- c(comb_analyses, dots[[i]]@analyses)
    
    if (!is.null(comb_results)) {
      if (!is.null(dots[[i]]@results[["RamanSpectra"]])) {
        new_spectra <- dots[[i]]@results[["RamanSpectra"]]
        comb_results@spectra <- c(comb_results@spectra, new_spectra@spectra)
        comb_results@chrom_peaks <- c(comb_results@chrom_peaks, new_spectra@chrom_peaks)
        comb_results@peaks <- c(comb_results@peaks, new_spectra@peaks)
      }
    }
  }
  
  names(comb_analyses) <- vapply(comb_analyses, function(z) z$name, NA_character_)
  
  comb_analyses <- comb_analyses[!duplicated(comb_analyses)]
  comb_analyses <- comb_analyses[order(names(comb_analyses))]
  
  x@analyses <- comb_analyses
  
  if (!is.null(comb_results)) {
    comb_results@spectra <- comb_results@spectra[!duplicated(names(comb_results@spectra))]
    comb_results@chrom_peaks <- comb_results@chrom_peaks[!duplicated(names(comb_results@chrom_peaks))]
    comb_results@peaks <- comb_results@peaks[!duplicated(names(comb_results@peaks))]
    
    if (length(comb_results@spectra) > 0) {
      comb_results@spectra <- comb_results@spectra[order(names(comb_results@spectra))]
    }
    
    if (length(comb_results@chrom_peaks) > 0) {
      comb_results@chrom_peaks <- comb_results@chrom_peaks[order(names(comb_results@chrom_peaks))]
    }
    
    if (length(comb_results@peaks) > 0) {
      comb_results@peaks <- comb_results@peaks[order(names(comb_results@peaks))]
    }
    
    x@results[["RamanSpectra"]] <- comb_results
  }
  
  return(x)
}

# MARK: get_spectra
## get_spectra -----
#' @export
#' @noRd
S7::method(get_spectra, RamanAnalyses) <- function(x,
                                                   analyses = NULL,
                                                   targets = NULL,
                                                   rt = NULL,
                                                   shift = NULL,
                                                   minIntensity = NULL,
                                                   useRawData = FALSE) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(list())
  }
  
  if (x$has_spectra) {
    if (useRawData) {
      spec <- StreamFind::RamanSpectra(lapply(x@analyses, function(x) x$spectra))
      spec$spectra <- spec$spectra[analyses]
      
    } else {
      spec <- x$Spectra
      
      if (spec$is_averaged) {
        rpl <- x$replicates
        rpl <- rpl[analyses]
        spec$spectra <- spec$spectra[names(spec$spectra) %in% unname(rpl)]
        spec$spectra <- Map( function(z, y) {
          z$replicate <- y
          z
        }, spec$spectra, names(spec$spectra))
      } else {
        spec$spectra <- spec$spectra[analyses]
        spec$spectra <- Map( function(z, y) {
          z$analysis <- y
          z
        }, spec$spectra, names(spec$spectra))
      }
    }
    
    if (spec$has_chrom_peaks) {
      if (length(spec$spectra) == length(spec$chrom_peaks[names(spec$spectra)])) {
        spec$spectra <- Map(function(z, y) {
          z$id <- NA_character_
          for (i in seq_len(nrow(y))) {
            sel <- z$rt >= y$rtmin[i] & z$rt <= y$rtmax[i]
            if (sum(sel) > 0) {
              if ("group" %in% colnames(y)) {
                z$id[sel] <- y$group[i]
              } else {
                z$id[sel] <- y$peak[i]
              }
            }
          }
          z
        }, spec$spectra, spec$chrom_peaks[names(spec$spectra)])
      }
    }
    
  } else {
    warning("No spectra available!")
    return(list())
  }
  
  spec_list <- spec$spectra

  spec_list <- lapply(spec_list, function(z) {
    if (!is.null(targets) && ("group" %in% colnames(z) || "id" %in% colnames(z))) {
      if ("group" %in% colnames(z)) {
        z <- z[z$group %in% targets, ]
      } else {
        z <- z[z$id %in% targets, ]
      }
    }
    
    if (!is.null(rt) && length(rt) == 2 && "rt" %in% colnames(z)) {
      rt_range <- sort(rt)
      sel <- z$rt >= rt_range[1] & z$rt <= rt_range[2]
      z <- z[sel, ]
    }
    
    if (!is.null(shift) && length(shift) == 2) {
      shift_range <- sort(shift)
      sel <- z$shift >= shift_range[1] & z$shift <= shift_range[2]
      z <- z[sel, ]
    }
    
    if (!is.null(minIntensity)) z <- z[z$intensity >= minIntensity, ]
    
    z
  })
  
  spec_list
}

# MARK: get_spectra_matrix
## get_spectra_matrix -----
#' @export
#' @noRd
S7::method(get_spectra_matrix, RamanAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          targets = NULL,
                                                          rt = NULL,
                                                          shift = NULL,
                                                          minIntensity = NULL,
                                                          useRawData = FALSE) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(list())
  }
  
  if (!x$has_spectra) {
    warning("No spectra results object available!")
    return(matrix())
  }

  spec_list <- get_spectra(x, analyses, targets, rt, shift, minIntensity, useRawData)

  spec_list <- lapply(spec_list, function(z) {
    if ("id" %in% colnames(z)) {
      data.table::setorder(z, id, shift)
      z$var <- paste0(z$id, "_", z$shift)
      z$var <- factor(z$var, levels = unique(z$var))
    } else {
      z$var <- z$shift
    }
    
    intensity <- NULL
    z <- z[, .(intensity = mean(intensity)), by = c("var")]
    z <- data.table::dcast(z, formula = 1 ~ var, value.var = "intensity")[, -1]
    z
    
  })
  
  mat <- as.matrix(rbindlist(spec_list, fill = TRUE))
  rownames(mat) <- names(spec_list)
  attr(mat, "xValues") <- colnames(mat)
  mat
}

# MARK: plot_spectra
## plot_spectra -----
#' @export
#' @noRd
S7::method(plot_spectra, RamanAnalyses) <- function(x,
                                                    analyses = NULL,
                                                    targets = NULL,
                                                    rt = NULL,
                                                    shift = NULL,
                                                    minIntensity = NULL,
                                                    useRawData = FALSE,
                                                    xLab = NULL,
                                                    yLab = NULL,
                                                    title = NULL,
                                                    showLegend = TRUE,
                                                    colorBy = "analyses",
                                                    interactive = TRUE,
                                                    cex = 0.6,
                                                    renderEngine = "webgl") {
  spectra <- get_spectra(x, analyses, targets, rt, shift, minIntensity, useRawData)

  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  
  spectra <- data.table::rbindlist(spectra, fill = TRUE)
  
  groupCols <- c("shift")
  if ("analysis" %in% colnames(spectra)) groupCols <- c("analysis", groupCols)
  if ("replicate" %in% colnames(spectra)) groupCols <- c("replicate", groupCols)
  if ("id" %in% colnames(spectra)) groupCols <- c("id", groupCols)
  groupCols <- groupCols[groupCols %in% colnames(spectra)]
  intensity <- NULL
  spectra <- spectra[, .(intensity = mean(intensity)), by = groupCols]
  
  spectra <- unique(spectra)
  
  if (is.null(xLab)) {
    if (interactive) {
      xLab <- "Raman shift / cm<sup>-1</sup>"
    } else {
      xLab <- expression("Raman shift / cm"^"-1")
    }
  }

  if (is.null(yLab)) yLab <- "Raman intensity / A.U."

  if (grepl("replicates", colorBy)) {
    if (!"replicate" %in% colnames(spectra)) {
      spectra$replicate <- x$replicates[spectra$analysis]
    }
  }
  
  colorBy <- gsub("chrom_peaks", "targets", colorBy)

  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)

  setnames(spectra, "shift", "x")

  if (!interactive) {
    return(.plot_x_spectra_static(spectra, xLab, yLab, title, cex, showLegend))
  } else {
    return(.plot_lines_interactive(spectra, xLab, yLab, title, colorBy, renderEngine))
  }
}

# MARK: plot_spectra_3d
## plot_spectra_3d -----
#' @export
#' @noRd
S7::method(plot_spectra_3d, RamanAnalyses) <- function(x,
                                                       analyses = NULL,
                                                       targets = NULL,
                                                       rt = NULL,
                                                       shift = NULL,
                                                       minIntensity = NULL,
                                                       useRawData = FALSE,
                                                       legendNames = TRUE,
                                                       colorBy = "analyses",
                                                       xLab = NULL,
                                                       yLab = NULL,
                                                       zLab = NULL,
                                                       renderEngine = "webgl") {
  
  spectra <- get_spectra(x, analyses, targets, rt, shift, minIntensity, useRawData)
  
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  
  spectra <- data.table::rbindlist(spectra, fill = TRUE)
  
  xlab <- "Shift / cm<sup>-1</sup>"
  ylab <- "Retention time / seconds"
  zlab <- "Intensity / counts"
  if (!is.null(xLab)) xlab <- xLab
  if (!is.null(yLab)) ylab <- yLab
  if (!is.null(zLab)) zlab <- zLab
  
  colorBy <- gsub("chrom_peaks", "targets", colorBy)

  if (grepl("targets", colorBy)) {
    if (!"id" %in% colnames(spectra)) {
      warning("No targets found!")
      return(NULL)
    }
  }
  
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames)
  
  spectra$shiftrt <- paste(
    spectra$id,
    spectra$rt,
    spectra$shift,
    sep = ""
  )
  
  colors_var <- .get_colors(unique(spectra$var))
  
  hover_text <- paste0(
    "<br>id: ", spectra$id,
    "<br>analysis: ", spectra$analysis,
    "<br>replicate: ", spectra$replicate,
    "<br>shift: ", spectra$shift,
    "<br>rt: ", spectra$rt,
    "<br>intensity: ", spectra$intensity
  )
  
  fig <- plotly::plot_ly(spectra, x = ~shift, y = ~rt, z = ~intensity) %>%
    group_by(spectra$rt) %>%
      plotly::add_lines(
        color = ~var,
        colors = colors_var,
        hoverinfo = "text",
        text = hover_text,
        line = list(width = 4)
      )
  
  fig <- fig %>% plotly::layout(scene = list(
    xaxis = list(title = xlab),
    yaxis = list(title = ylab),
    zaxis = list(title = zlab)
  ))
  
  if (renderEngine %in% "webgl") {
    fig <- fig %>% plotly::toWebGL()
  }
  
  fig
}

# MARK: plot_spectra_baseline
## plot_spectra_baseline -----
#' @noRd
S7::method(plot_spectra_baseline, RamanAnalyses) <- function(x,
                                                             analyses = NULL,
                                                             targets = NULL,
                                                             rt = NULL,
                                                             shift = NULL,
                                                             minIntensity = NULL,
                                                             xLab = NULL,
                                                             yLab = NULL,
                                                             title = NULL,
                                                             showLegend = TRUE,
                                                             colorBy = "analyses",
                                                             interactive = TRUE,
                                                             cex = 0.6,
                                                             renderEngine = "webgl") {
  
  spectra <- get_spectra(x, analyses, targets, rt, shift, minIntensity, useRawData = FALSE)
  
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  
  has_baseline <- any(vapply(spectra, function(z) "baseline" %in% colnames(z), FALSE))
  
  if (!has_baseline) {
    warning("Baseline not found!")
    return(NULL)
  }
  
  baseline <- NULL
  
  if (x$Spectra$is_averaged) {
    spectra <- data.table::rbindlist(spectra, idcol = "replicate", fill = TRUE)
  } else {
    spectra <- data.table::rbindlist(spectra, idcol = "analysis", fill = TRUE)
  }
  
  groupCols <- c("analysis", "replicate", "shift")
  if ("id" %in% colnames(spectra)) groupCols <- c("id", groupCols)
  if ("group" %in% colnames(spectra)) groupCols <- c("group", groupCols)
  groupCols <- groupCols[groupCols %in% colnames(spectra)]
  
  spectra <- spectra[, .(baseline = mean(baseline), raw = mean(raw)), by = groupCols]
  
  spectra <- unique(spectra)
  
  if (is.null(xLab)) {
    if (interactive) {
      xLab <- "Raman shift / cm<sup>-1</sup>"
    } else {
      xLab <- expression("Raman shift / cm"^"-1")
    }
  }
  
  if (is.null(yLab)) yLab <- "Raman intensity / A.U."
  
  if ("replicates" %in% colorBy) {
    if (!"replicate" %in% colnames(spectra)) {
      spectra$replicate <- x$replicates[spectra$analysis]
    }
  }
  
  colorBy <- gsub("chrom_peaks", "targets", colorBy)
  
  if (grepl("targets", colorBy)) {
    if ("group" %in% colnames(spectra)) {
      spectra$id <- spectra$group
    }
  }
  
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
  
  setnames(spectra, "shift", "x")

  if (!interactive) {
    return(.plot_x_spectra_baseline_static(spectra, xLab, yLab, title, cex, showLegend))
  } else {
    return(.plot_lines_baseline_interactive(spectra, xLab, yLab, title, colorBy, renderEngine))
  }
}

# MARK: plot_chromatograms
## plot_chromatograms -----
#' @export
#' @noRd
S7::method(plot_chromatograms, RamanAnalyses) <- function(x,
                                                          analyses = NULL,
                                                          targets = NULL,
                                                          rt = NULL,
                                                          shift = NULL,
                                                          minIntensity = NULL,
                                                          useRawData = FALSE,
                                                          xLab = NULL,
                                                          yLab = NULL,
                                                          title = NULL,
                                                          showLegend = TRUE,
                                                          colorBy = "analyses",
                                                          interactive = TRUE,
                                                          cex = 0.6,
                                                          renderEngine = "webgl") {
  spectra <- get_spectra(x, analyses, targets, rt, shift, minIntensity, useRawData)
  
  if (!all(vapply(spectra, function(z) "rt" %in% colnames(z), FALSE))) {
    warning("Column rt not found in spectra data.table for plotting chromatograms!")
    return(NULL)
  }
  
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  
  intensity <- NULL
  
  if (x$Spectra$is_averaged && !useRawData) {
    spectra <- data.table::rbindlist(spectra, idcol = "replicate", fill = TRUE)
  } else {
    spectra <- data.table::rbindlist(spectra, idcol = "analysis", fill = TRUE)
  }
  
  groupCols <- c("analysis", "replicate", "rt")
  if ("id" %in% colnames(spectra)) groupCols <- c("id", groupCols)
  if ("group" %in% colnames(spectra)) groupCols <- c("group", groupCols)
  groupCols <- groupCols[groupCols %in% colnames(spectra)]
  
  spectra <- spectra[, .(intensity = sum(intensity)), by = groupCols]
  
  spectra <- unique(spectra)
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Cumulative Raman intensity / A.U."
  
  if ("replicates" %in% colorBy) {
    if (!"replicate" %in% colnames(spectra)) {
      spectra$replicate <- x$replicates[spectra$analysis]
    }
  }
  
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
  
  setnames(spectra, "rt", "x")
  
  if (!interactive) {
    return(.plot_x_spectra_static(spectra, xLab, yLab, title, cex, showLegend))
  } else {
    return(.plot_lines_interactive(spectra, xLab, yLab, title, colorBy, renderEngine))
  }
}

# MARK: get_chromatograms_peaks
## get_chromatograms_peaks -----
#' @export
#' @noRd
S7::method(get_chromatograms_peaks, RamanAnalyses) <- function(x,
                                                               analyses = NULL,
                                                               targets = NULL,
                                                               rt = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }
  
  if (!x$has_spectra) {
    return(data.table::data.table())
  }
  
  pks <- x$Spectra$chrom_peaks
  if (length(pks) == 0) {
    return(data.table::data.table())
  }
  
  if (x$Spectra$is_averaged) {
    pks <- data.table::rbindlist(x$Spectra$chrom_peaks, idcol = "replicate", fill = TRUE)
  } else {
    pks <- data.table::rbindlist(x$Spectra$chrom_peaks, idcol = "analysis", fill = TRUE)
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
  
  if (!is.null(targets)) {
    if ("group" %in% colnames(pks)) {
      pks <- pks[pks$group %in% targets, ]
    } else if ("id" %in% colnames(pks)) {
      pks <- pks[pks$id %in% targets, ]
    }
  }
  
  if (is.numeric(rt)) {
    pks <- pks[pks$rt >= rt[1] & pks$rt <= rt[2], ]
  }
  
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found for the targets!")
    return(data.table::data.table())
  }
  
  pks
}

# MARK: plot_chromatograms_peaks
## plot_chromatograms_peaks -----
#' @export
#' @noRd
S7::method(plot_chromatograms_peaks, RamanAnalyses) <- function(x,
                                                                analyses = NULL,
                                                                targets = NULL,
                                                                rt = NULL,
                                                                title = NULL,
                                                                legendNames = TRUE,
                                                                colorBy = "targets",
                                                                showLegend = TRUE,
                                                                xlim = NULL,
                                                                ylim = NULL,
                                                                xLab = NULL,
                                                                yLab = NULL,
                                                                interactive = TRUE,
                                                                cex = 0.6,
                                                                renderEngine = "webgl") {
  pks <- get_chromatograms_peaks(x, analyses, targets, rt)
  
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found!")
    return(NULL)
  }
  
  spectra <- get_spectra(x, analyses = analyses, targets = targets, useRawData = FALSE)
  
  if (!all(vapply(spectra, function(z) "rt" %in% colnames(z), FALSE))) {
    warning("Column rt not found in spectra data.table for plotting chromatograms!")
    return(NULL)
  }
  
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  
  intensity <- NULL
  
  if (x$Spectra$is_averaged) {
    spectra <- data.table::rbindlist(spectra, idcol = "replicate", fill = TRUE)
  } else {
    spectra <- data.table::rbindlist(spectra, idcol = "analysis", fill = TRUE)
  }
  
  groupCols <- c("analysis", "replicate", "rt")
  if ("id" %in% colnames(spectra)) groupCols <- c("id", groupCols)
  if ("group" %in% colnames(spectra)) groupCols <- c("group", groupCols)
  groupCols <- groupCols[groupCols %in% colnames(spectra)]
  
  spectra <- spectra[, .(intensity = sum(intensity)), by = groupCols]
  
  spectra <- unique(spectra)
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Cumulative Raman intensity / A.U."
  
  if ("replicates" %in% colorBy) {
    if (!"replicate" %in% colnames(pks)) {
      pks$replicate <- x$replicates[pks$analysis]
    }
    if (!"replicate" %in% colnames(spectra)) {
      spectra$replicate <- x$replicates[spectra$analysis]
    }
  }
  
  data.table::setnames(spectra, "rt", "x")
  data.table::setnames(pks, c("rt", "rtmin", "rtmax"), c("x", "min", "max"))
  
  if (!interactive) {
    .plot_chrom_peaks_static(
      spectra, pks, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab
    )
  } else {
    .plot_lines_peaks_interactive(
      spectra, pks, legendNames, colorBy, title, showLegend, xLab, yLab, renderEngine
    )
  }
}

# MARK: Utility functions
# Utility functions ------

# MARK: .get_RamanAnalysis_from_files
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
        if ("concentration" %in% colnames(files)) {
          concentrations <- as.numeric(files$concentration)
        } else {
          concentrations <- rep(NA_real_, nrow(files))
        }
        if ("reference" %in% colnames(files)) {
          references <- as.character(files$reference)
        } else {
          references <- rep(NA_character_, nrow(files))
        }
        files <- files$file
      } else {
        files <- ""
      }
    } else {
      replicates <- rep(NA_character_, length(files))
      blanks <- rep(NA_character_, length(files))
      concentrations <- rep(NA_real_, length(files))
      references <- rep(NA_character_, length(files))
    }

    possible_file_formats <- c("asc", "sif", "json", "wdf", "sdf", "csv", "txt")

    valid_files <- vapply(files,
      FUN.VALUE = FALSE,
      function(x, possible_file_formats) {
        if (!file.exists(x)) {
          return(FALSE)
        }
        if (!tools::file_ext(x) %in% possible_file_formats) {
          return(FALSE)
        }
        TRUE
      }, possible_file_formats = possible_file_formats
    )

    if (!all(valid_files)) {
      warning("File/s not valid!")
      return(list())
    }

    names(replicates) <- as.character(files)
    names(blanks) <- as.character(files)
    names(concentrations) <- as.character(files)
    names(references) <- as.character(files)

    analyses <- lapply(files, function(x) {
      cache <- .load_chache("parsed_raman_analyses", x)

      if (!is.null(cache$data)) {
        message("\U2139 Analysis loaded from cache!")
        cache$data
      } else {
        message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
        format <- tools::file_ext(x)
        
        switch(format,

          "asc" = {
            ana <- rcpp_parse_asc_file(x)
          },

          "sif" = {
            if (!reticulate::py_module_available("orpl")) {
              warning("Python module 'orpl' not available for reading .sif files!")
              return(NULL)
            }

            tryCatch(
              {
                orpl_module <- reticulate::import("orpl")
                orlp_file_io <- orpl_module$file_io
                sif_file <- orlp_file_io$load_sif(x)
                sif_file_name <- basename(tools::file_path_sans_ext(x))
                
                spectra <- sif_file$accumulations
                detector_dimension <- nrow(spectra)
                pixels <- seq_len(detector_dimension)
                calibration_data <- sif_file$metadata$details[["Calibration_data"]]
                ex_wavelength <- sif_file$metadata$details[["RamanExWavelength"]]
                calibration_nm <- rep(NA_real_, detector_dimension)
                
                if (calibration_data[1] == 0) {
                  calibration_data[1] <- ex_wavelength
                }

                for (i in seq_len(detector_dimension)) {
                  calibration_nm[i] <- calibration_data[1]
                  for (j in 2:length(calibration_data)) {
                    if (calibration_data[j] > 0) {
                      calibration_nm[i] <- calibration_nm[i] + calibration_data[j] * pixels[i]^(j - 1)
                    }
                  }
                }

                shifts <- ((1 / ex_wavelength) - (1 / calibration_nm)) * 1e7
                shifts <- round(shifts, digits = 0)

                exposure_time <- as.numeric(sif_file$metadata$exposure_time)

                spectra <- lapply(seq_len(ncol(spectra)), function(x, shifts, exposure_time) {
                  data.table::data.table(
                    "rt" = x * exposure_time,
                    "shift" = shifts,
                    "intensity" = rev(spectra[, x])
                  )
                }, shifts = shifts, exposure_time = exposure_time)

                spectra <- data.table::rbindlist(spectra)

                metadata <- reticulate::py_to_r(sif_file$metadata)

                ana <- list(
                  "name" = sif_file_name,
                  "replicate" = sif_file_name,
                  "blank" = NA_character_,
                  "concentration" = NA_real_,
                  "reference" = NA_character_,
                  "file" = x,
                  "type" = "Raman",
                  "metadata" = metadata,
                  "spectra" = spectra
                )

                class(ana) <- c("RamanAnalysis", "Analysis")
              },
              error = function(e) {
                warning("Error loading .sif file! The error is ", e)
                return(NULL)
              }
            )
          },

          "default" = {
            if (!reticulate::py_module_available("orpl")) {
              warning("Python module 'orpl' not available for reading data files!")
              return(NULL)
            }
            
            tryCatch(
              {
                orpl_module <- reticulate::import("orpl")
                orlp_file_io <- orpl_module$file_io
                sif_file <- orlp_file_io$load_file(x)
                sif_file_name <- basename(tools::file_path_sans_ext(x))
                
                spectra <- sif_file$accumulations
                detector_dimension <- nrow(spectra)
                pixels <- seq_len(detector_dimension)
                calibration_data <- sif_file$metadata$details[["Calibration_data"]]
                ex_wavelength <- sif_file$metadata$details[["RamanExWavelength"]]
                calibration_nm <- rep(NA_real_, detector_dimension)
                
                if (calibration_data[1] == 0) {
                  calibration_data[1] <- ex_wavelength
                }
                
                for (i in seq_len(detector_dimension)) {
                  calibration_nm[i] <- calibration_data[1]
                  for (j in 2:length(calibration_data)) {
                    if (calibration_data[j] > 0) {
                      calibration_nm[i] <- calibration_nm[i] + calibration_data[j] * pixels[i]^(j - 1)
                    }
                  }
                }
                
                shifts <- ((1 / ex_wavelength) - (1 / calibration_nm)) * 1e7
                shifts <- round(shifts, digits = 0)
                
                exposure_time <- as.numeric(sif_file$metadata$exposure_time)
                
                spectra <- lapply(seq_len(ncol(spectra)), function(x, shifts, exposure_time) {
                  data.table::data.table(
                    "rt" = x * exposure_time,
                    "shift" = shifts,
                    "intensity" = rev(spectra[, x])
                  )
                }, shifts = shifts, exposure_time = exposure_time)
                
                spectra <- data.table::rbindlist(spectra)
                
                metadata <- reticulate::py_to_r(sif_file$metadata)
                
                ana <- list(
                  "name" = sif_file_name,
                  "replicate" = sif_file_name,
                  "blank" = NA_character_,
                  "concentration" = NA_real_,
                  "reference" = NA_character_,
                  "file" = x,
                  "type" = "Raman",
                  "metadata" = metadata,
                  "spectra" = spectra
                )
                
                class(ana) <- c("RamanAnalysis", "Analysis")
              },
              error = function(e) {
                warning("Error loading file! \n", e)
                return(NULL)
              }
            )
          }
        )

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
        ana$blank <- blanks[x]
        ana$concentration <- concentrations[x]
        ana$reference <- references[x]
        
        if ("rt" %in% colnames(ana$spectra)) ana$type <- "LC-Raman"
        
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
