# MARK: RamanAnalyses
#' @export
#' @noRd
RamanAnalyses <- S7::new_class("RamanAnalyses",
  package = "StreamFind", parent = Analyses,
  properties = list(

    # MARK: analyses
    ## __analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),

    # MARK: replicates
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

    # MARK: blanks
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

    # MARK: types
    ## __types -----
    types = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$type, NA_character_)
    }),

    # MARK: files
    ## __files -----
    files = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$file, NA_character_)
    }),

    # MARK: info
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

    # MARK: raw_spectra
    ## __raw_spectra -----
    raw_spectra = S7::new_property(S7::class_list, getter = function(self) {
      if (length(self) > 0) {
        lapply(self@analyses, function(x) x$spectra)
      } else {
        list()
      }
    }),

    # MARK: has_spectra
    ## __has_spectra -----
    has_spectra = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) {
        return(FALSE)
      }
      if (is.null(self@results[["spectra"]])) {
        if (length(self$raw_spectra) == 0) {
          return(FALSE)
        }
      } else {
        if (!is(self@results[["spectra"]], "StreamFind::RamanSpectra")) {
          return(FALSE)
        }
      }
      TRUE
    }),

    # MARK: spectra
    ## __spectra -----
    spectra = S7::new_property(S7::class_data.frame,
      getter = function(self) {
        if (!is.null(self@results[["spectra"]])) {
          return(self@results[["spectra"]])
        }
        StreamFind::RamanSpectra(self$raw_spectra)
      },
      setter = function(self, value) {
        if (is(value, "StreamFind::RamanSpectra")) {
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
          warning("Value must be an RamanSpectra results object! Not done.")
        }
        self
        self
      }
    )
  ),

  # MARK: constructor
  ## __constructor -----
  constructor = function(files = NULL) {
    analyses <- .get_RamanAnalysis_from_files(files)
    S7::new_object(Analyses(), possible_formats = c("asc", "sif"), analyses = analyses)
  },

  # MARK: validator
  ## __validator -----
  validator = function(self) {
    checkmate::assert_true(identical(self@possible_formats, c("asc", "sif")))
    if (length(self) > 0) checkmate::assert_true(identical(names(self@analyses), unname(names(self))))
    NULL
  }
)

# MARK: Methods
# Methods -----

# MARK: names
## __names -----
#' @export
#' @noRd
S7::method(names, RamanAnalyses) <- function(x) {
  vapply(x@analyses, function(x) x$name, NA_character_)
}

# MARK: add
## __add -----
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

  if (!all(vapply(value, function(a) is(a, "RamanAnalysis"), FALSE))) {
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
    message("All results removed!")
    x@results <- list()
  }

  x@analyses <- analyses
  x
}

# MARK: remove
## __remove -----
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
  if (!is.null(x@results[["spectra"]])) {
    spectra_names <- names(x@results$spectra)
    if (!x@results$spectra$is_averaged) {
      x@results$spectra <- x@results$spectra[names(x) %in% spectra_names]
      x@results$spectra <- x@results$spectra[order(names(x@results$spectra))]
    } else {
      x@results$spectra <- x@results$spectra[x$replicates %in% spectra_names]
      x@results$spectra <- x@results$spectra[order(names(x@results$spectra))]
    }
  }
  x
}

# MARK: `[`
## __`[` -----
#' @export
#' @noRd
S7::method(`[`, RamanAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (!is.null(x$results$spectra)) {
    spectra_names <- names(x@results$spectra$spectra)
    if (!x@results$spectra$is_averaged) {
      x@results$spectra <- x@results$spectra[names(x) %in% spectra_names]
      x@results$spectra <- x@results$spectra[order(names(x@results$spectra))]
    } else {
      x@results$spectra <- x@results$spectra[x$replicates %in% spectra_names]
      x@results$spectra <- x@results$spectra[order(names(x@results$spectra))]
    }
  }
  return(x)
}

# MARK: `[<-`
## __`[<-` -----
#' @export
#' @noRd
S7::method(`[<-`, RamanAnalyses) <- function(x, i, value) {
  warning("Method not implemented in RamanAnalyses! Use add or remove methods instead.")
  return(x)
}

# MARK: `[[`
## __`[[` -----
#' @export
#' @noRd
S7::method(`[[`, RamanAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[[i]]
  if (x@has_processed_spectra) {
    spectra_names <- names(x@results$spectra)
    if (!x@results$spectra$is_averaged) {
      x@results$spectra <- x@results$spectra[names(x) %in% spectra_names]
      x@results$spectra <- x@results$spectra[order(names(x@results$spectra))]
    } else {
      x@results$spectra <- x@results$spectra[x$replicates %in% spectra_names]
      x@results$spectra <- x@results$spectra[order(names(x@results$spectra))]
    }
  }
  return(x)
}

# MARK: `[[<-`
## __`[[<-` -----
#' @export
#' @noRd
S7::method(`[[<-`, RamanAnalyses) <- function(x, i, value) {
  warning("Method not implemented in RamanAnalyses! Use add or remove methods instead.")
  return(x)
}

# MARK: get_spectra
## __get_spectra -----
#' @export
#' @noRd
S7::method(get_spectra, RamanAnalyses) <- function(x,
                                                   analyses = NULL,
                                                   rt = NULL,
                                                   shift = NULL,
                                                   minIntensity = 0,
                                                   useRawData = FALSE) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.frame())
  }

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
    if (x$spectra$is_averaged) {
      spec$replicate <- spec$analysis
    } else {
      spec$replicate <- x$replicates[spec$analysis]
    }
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

# MARK: get_spectra_matrix
## __get_spectra_matrix -----
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

# MARK: plot_spectra
## __plot_spectra -----
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
    groupCols <- c("analysis", "replicate", "rt")
    if ("id" %in% colnames(spectra)) groupCols <- c("id", groupCols)
    spectra <- spectra[, .(intensity = sum(intensity)), by = groupCols]
    if (is.null(xLab)) xLab <- "Retention time / seconds"
    setnames(spectra, "rt", "shift")
  } else if ("shift" %in% xVal) {
    groupCols <- c("analysis", "replicate", "shift")
    if ("id" %in% colnames(spectra)) groupCols <- c("id", groupCols)
    spectra <- spectra[, .(intensity = mean(intensity)), by = groupCols]
    if (is.null(xLab)) {
      if (interactive) {
        xLab <- "Raman shift / cm<sup>-1</sup>"
      } else {
        xLab <- expression("Raman shift / cm"^"-1")
      }
    }
  }

  spectra <- unique(spectra)

  if (is.null(yLab)) yLab <- "Raman intensity / A.U."

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

# MARK: plot_spectra_3d
## __plot_spectra_3d -----
#' @export
#' @noRd
S7::method(plot_spectra_3d, RamanAnalyses) <- function(x,
                                                       analyses = NULL,
                                                       rt = NULL,
                                                       shift = NULL,
                                                       minIntensity = 0,
                                                       useRawData = FALSE,
                                                       legendNames = TRUE,
                                                       colorBy = "analyses",
                                                       xVal = "shift",
                                                       yVal = "rt",
                                                       xLab = NULL,
                                                       yLab = NULL,
                                                       zLab = NULL) {
  
  spectra <- get_spectra(x, analyses, rt, shift, minIntensity, useRawData)
  
  if (nrow(spectra) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }
  
  checkmate::assert_choice(xVal, c("shift", "rt"))
  checkmate::assert_choice(yVal, c("shift", "rt"))
  
  if (any(duplicated(c(xVal, yVal)))) {
    stop("Duplicated x and y values are not possible!")
  }
  
  xlab <- switch(
    xVal,
    "shift" = "Shift / cm<sup>-1</sup>",
    "rt" = "Elution time / seconds",
  )
  
  ylab <- switch(
    yVal,
    "shift" = "Shift / cm<sup>-1</sup>",
    "rt" = "Elution time / seconds",
  )
  
  zlab <- "Intensity / counts"
  
  if (!is.null(xLab)) xlab <- xLab
  if (!is.null(yLab)) ylab <- yLab
  if (!is.null(zLab)) zlab <- zLab
  
  if (grepl("chrom_peaks", colorBy, fixed = FALSE)) {
    if (!x$spectra$has_chrom_peaks) {
      warning("No chromatographic peaks available!")
      return(NULL)
    } else {
      chrom_peaks <- x$spectra$chrom_peaks
      chrom_peaks <- Map(function(z, y) {
        z$analysis <- y
        z
      }, chrom_peaks, names(chrom_peaks))
      if (length(chrom_peaks) > 0) {
        chrom_spectra <- lapply(chrom_peaks, function(z, spectra) {
          temp <- spectra[spectra$analysis %in% z$analysis, ]
          temp$id <- NA_character_
          for (i in seq_len(nrow(z))) {
            temp$id[temp$rt >= z$rtmin[i] & temp$rt <= z$rtmax[i]] <- paste0(z$peak[i], "_", z$rt[i])
          }
          temp <- temp[!is.na(temp$id), ]
          temp
        }, spectra = spectra)
        spectra <- data.table::rbindlist(chrom_spectra)
        colorBy <- gsub("chrom_peaks", "targets", colorBy)
      }
    }
  }
  
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames)
  
  spectra$shiftrt <- paste(
    spectra$id,
    spectra$rt,
    spectra$shift,
    sep = ""
  )
  
  spectra[["x"]] <- spectra[[xVal[1]]]
  
  spectra[["y"]] <- spectra[[yVal[1]]]
  
  colors_var <- .get_colors(unique(spectra$var))
  
  hover_text <- paste0(
    "<br>id: ", spectra$id,
    "<br>analysis: ", spectra$analysis,
    "<br>replicate: ", spectra$replicate,
    "<br>shift: ", spectra$shift,
    "<br>rt: ", spectra$rt,
    "<br>intensity: ", spectra$intensity
  )
  
  fig <- plotly::plot_ly(spectra, x = ~x, y = ~y, z = ~intensity) %>%
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
  
  fig
}

# MARK: plot_spectra_baseline
## __plot_spectra_baseline -----
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

    if (is.null(xLab)) xLab <- "Retention time / seconds"

    setnames(spectra, "rt", "shift")
  } else if ("shift" %in% xVal) {
    spectra <- spectra[, .(baseline = mean(baseline), raw = mean(raw)), by = c("analysis", "shift")]

    if (is.null(xLab)) {
      if (interactive) {
        xLab <- "Raman shift / cm<sup>-1</sup>"
      } else {
        xLab <- expression("Raman shift / cm"^"-1")
      }
    }
  }

  spectra <- unique(spectra)

  if (is.null(yLab)) yLab <- "Raman intensity / A.U."

  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)

  setnames(spectra, "shift", "x")

  if (!interactive) {
    return(.plot_x_spectra_baseline_static(spectra, xLab, yLab, title, cex, showLegend))
  } else {
    return(.plot_x_spectra_baseline_interactive(spectra, xLab, yLab, title, colorBy))
  }
}

# MARK: plot_chromatograms
## __plot_chromatograms -----
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

    if (is.null(xLab)) xLab <- "Retention time / seconds"

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

# MARK: get_chromatograms_peaks
## __get_chromatograms_peaks -----
#' @export
#' @noRd
S7::method(get_chromatograms_peaks, RamanAnalyses) <- function(x, analyses = NULL, rt = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }
  
  if (!x$has_spectra) {
    return(data.table::data.table())
  }
  
  pks <- x$spectra$chrom_peaks
  if (length(pks) == 0) {
    return(data.table::data.table())
  }
  
  if (x$spectra$is_averaged) {
    pks <- data.table::rbindlist(x$spectra$chrom_peaks, idcol = "replicate", fill = TRUE)
  } else {
    pks <- data.table::rbindlist(x$spectra$chrom_peaks, idcol = "analysis", fill = TRUE)
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
## __plot_chromatograms_peaks -----
#' @export
#' @noRd
S7::method(plot_chromatograms_peaks, RamanAnalyses) <- function(x,
                                                                analyses = NULL,
                                                                rt = NULL,
                                                                title = NULL,
                                                                legendNames = TRUE,
                                                                colorBy = "targets",
                                                                showLegend = TRUE,
                                                                xlim = NULL,
                                                                ylim = NULL,
                                                                cex = 0.6,
                                                                xLab = NULL,
                                                                yLab = NULL,
                                                                interactive = TRUE) {
  pks <- get_chromatograms_peaks(x, analyses = analyses, rt = rt)
  
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found!")
    return(NULL)
  }
  
  spectra <- get_spectra(x, analyses = analyses, useRawData = FALSE)
  
  if ("rt" %in% colnames(spectra)) {
    intensity <- NULL
    spectra[["shift"]] <- NULL
    spectra <- spectra[, .(intensity = sum(intensity)), by = c("analysis", "rt")]
    spectra <- unique(spectra)
    if (is.null(xLab)) xLab <- "Retention time / seconds"
    spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
  } else {
    warning("Column rt not found in spectra data.table!")
    NULL
  }
  
  if (nrow(spectra) == 0) {
    message("\U2717 Chromatograms not found!")
    return(NULL)
  }
  
  if ("replicates" %in% colorBy && !"replicate" %in% colnames(pks)) {
    pks$replicate <- x$replicates[pks$analysis]
    spectra$replicate <- x$replicates[spectra$analysis]
  }
  
  pks$id <- pks$analysis
  spectra$id <- spectra$analysis
  
  if (!interactive) {
    .plot_chrom_peaks_static(spectra, pks, legendNames, colorBy, title, showLegend, xlim, ylim, cex, xLab, yLab)
  } else {
    .plot_chrom_peaks_interactive(spectra, pks, legendNames, colorBy, title, showLegend, xLab, yLab)
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

        files <- files$file
      } else {
        files <- ""
      }
    } else {
      replicates <- rep(NA_character_, length(files))
      blanks <- rep(NA_character_, length(files))
    }

    possible_ms_file_formats <- c("asc", "sif")

    valid_files <- vapply(files,
      FUN.VALUE = FALSE,
      function(x, possible_ms_file_formats) {
        if (!file.exists(x)) {
          return(FALSE)
        }
        if (!tools::file_ext(x) %in% possible_ms_file_formats) {
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
                pixels <- seq_len(detector_dimension) - 1
                calibration_data <- sif_file$metadata$details[["Calibration_data"]]
                ex_wavelength <- sif_file$metadata$details[["RamanExWavelength"]]
                calibration_nm <- rep(NA_real_, detector_dimension)

                for (i in seq_len(detector_dimension)) {
                  calibration_nm[i] <- calibration_data[1]
                  for (j in 2:length(calibration_data)) {
                    calibration_nm[i] <- calibration_nm[i] + calibration_data[j] * pixels[i]^(j - 1)
                  }
                }

                shifts <- ((1 / ex_wavelength) - (1 / calibration_nm)) * 1e7

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
                  "file" = x,
                  "type" = "raman",
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
            warning("File format not supported!")
            return(NULL)
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
