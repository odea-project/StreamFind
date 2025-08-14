# MARK: RamanAnalyses
# RamanAnalyses -----
#' @title RamanAnalyses Class
#' @description The RamanAnalyses class is used to store a list of Raman spectra.
#' @param files A `character` vector with full file paths to "asc", "sif", "json", "wdf", "sdf", "csv" and/or "txt" raman files or a `data.frame` with colnames `file`, `replicate` and `blank`. The "replicate" column is used to group the analyses and the "blank" column is used to identify the blank samples. The "file" column is the full to the raman files.
#' @return A `RamanAnalyses` object which is fundamentally a list with each element representing a Raman analysis. Each analysis is a list with the following elements:
#' - `name`: The name of the analysis.
#' - `replicate`: The replicate identifier for the analysis.
#' - `blank`: The blank sample identifier for the analysis.
#' - `concentration`: The concentration of the sample in the analysis.
#' - `reference`: The reference sample identifier for the analysis.
#' - `file`: The file path to the Raman data file.
#' - `type`: The type of analysis, which is "Raman".
#' - `metadata`: A list containing metadata for the analysis.
#' - `spectra`: A `data.table` containing the Raman spectra data, with columns for shift, intensity, and optionally other parameters like rt and id.
#' The `results` element is a list that can hold results. Possible results is currently only [StreamFind::RamanResults_Spectra].
#'
#' @export
#'
RamanAnalyses <- function(files = NULL) {
  analyses <- .get_RamanAnalysis_from_files(files)
  x <- structure(
    list(
      analyses = analyses,
      results = list(),
      type = "Raman",
      formats = DataTypes()$file_formats$Raman
    ),
    class = c("RamanAnalyses", "Analyses")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid RamanAnalyses object!")
  }
}

# MARK: validate_object
#' @describeIn RamanAnalyses Validate the RamanAnalyses object, returning `NULL` if valid.
#' @template arg-raman-x
#' @export
#'
validate_object.RamanAnalyses <- function(x) {
  checkmate::assert_true(identical(class(x), c("RamanAnalyses", "Analyses")))
  checkmate::assert_true(identical(x$formats, DataTypes()$file_formats$Raman))
  if (length(x$analyses) > 0) {
    lapply(x$analyses, function(a) {
      if (!is.list(a)) {
        stop("All analyses must be list objects!")
      }
      checkmate::assert_names(
        names(a),
        must.include = c(
          "name",
          "replicate",
          "blank",
          "concentration",
          "reference",
          "file",
          "type",
          "metadata",
          "spectra"
        )
      )
    })
    checkmate::assert_true(all(
      get_analysis_names(x) %in%
        vapply(x$analyses, function(a) a$name, NA_character_)
    ))
    rpls <- vapply(x$analyses, function(a) a$replicate, NA_character_)
    blks <- vapply(x$analyses, function(a) a$blank, NA_character_)
    refs <- vapply(x$analyses, function(a) a$reference, NA_character_)
    for (i in seq_along(rpls)) {
      checkmate::assert_true(blks[i] %in% rpls || is.na(blks[i]))
      checkmate::assert_true(refs[i] %in% rpls || is.na(refs[i]))
    }
  }
  NextMethod()
  NULL
}

# MARK: Methods
# Methods -----

#' @describeIn RamanAnalyses Get the names of the analyses in the `RamanAnalyses` object.
#' @template arg-raman-x
#' @export
#'
get_analysis_names.RamanAnalyses <- function(x) {
  vapply(x$analyses, function(z) z$name, NA_character_)
}

# MARK: get_replicate_names
#' @describeIn RamanAnalyses Get the replicates of the analyses in the `RamanAnalyses` object.
#' @template arg-raman-x
#' @export
#'
get_replicate_names.RamanAnalyses <- function(x) {
  vapply(x$analyses, function(x) x$replicate, NA_character_)
}

# MARK: set_replicates
#' @describeIn RamanAnalyses Set the replicates of the analyses in the `RamanAnalyses` object. The argument `value` must be a character vector with the same length as the number of analyses in the object.
#' @template arg-raman-x
#' @template arg-value
#' @export
#'
set_replicates.RamanAnalyses <- function(x, value) {
  if (length(value) != length(x$analyses)) {
    stop("Length of value must be equal to the number of analyses!")
  }
  for (i in seq_along(x$analyses)) {
    x$analyses[[i]]$replicate <- value[i]
  }
  x
}

# MARK: get_blank_names
#' @describeIn RamanAnalyses Get the blanks of the analyses in the `RamanAnalyses` object.
#' @template arg-raman-x
#' @export
#'
get_blank_names.RamanAnalyses <- function(x) {
  vapply(x$analyses, function(x) x$blank, NA_character_)
}

# MARK: set_blanks
#' @describeIn RamanAnalyses Set the blanks of the analyses in the `RamanAnalyses` object. The argument `value` must be a character vector with the same length as the number of analyses in the object.
#' @template arg-raman-x
#' @template arg-value
#' @export
#'
set_blanks.RamanAnalyses <- function(x, value) {
  if (length(value) != length(x$analyses)) {
    stop("Length of value must be equal to the number of analyses!")
  }
  for (i in seq_along(x$analyses)) {
    x$analyses[[i]]$blank <- value[i]
  }
  x
}

# MARK: get_concentrations
#' @describeIn RamanAnalyses Get the concentrations of the analyses in the `RamanAnalyses` object.
#' @template arg-raman-x
#' @export
#'
get_concentrations.RamanAnalyses <- function(x) {
  vapply(x$analyses, function(x) x$concentration, NA_real_)
}

# MARK: set_concentrations
#' @describeIn RamanAnalyses Set the concentrations of the analyses in the `RamanAnalyses` object. The argument `value` must be a numeric vector with the same length as the number of analyses in the object.
#' @template arg-raman-x
#' @template arg-value
#' @export
#'
set_concentrations.RamanAnalyses <- function(x, value) {
  if (length(value) != length(x$analyses)) {
    stop("Length of value must be equal to the number of analyses!")
  }
  if (!is.numeric(value)) {
    stop("Value must be a numeric vector!")
  }
  for (i in seq_along(x$analyses)) {
    x$analyses[[i]]$concentration <- value[i]
  }
  x
}

# MARK: info
#' @describeIn RamanAnalyses Get a summary `data.table` of the `RamanAnalyses` object.
#' @template arg-raman-x
#' @export
#'
info.RamanAnalyses <- function(x) {
  if (length(x$analyses) > 0) {
    df <- data.table::data.table(
      "analysis" = vapply(x$analyses, function(x) x$name, NA_character_),
      "replicate" = vapply(x$analyses, function(x) x$replicate, NA_character_),
      "blank" = vapply(x$analyses, function(x) x$blank, NA_character_),
      "concentration" = vapply(
        x$analyses,
        function(x) x$concentration,
        NA_real_
      ),
      "reference" = vapply(x$analyses, function(x) x$reference, NA_character_),
      "spectra" = vapply(
        x$analyses,
        function(x) {
          if (is.null(x$spectra)) {
            return(0)
          } else {
            return(nrow(x$spectra))
          }
        },
        0
      )
    )
    row.names(df) <- seq_len(nrow(df))
    df
  } else {
    data.table::data.table()
  }
}

#' @describeIn RamanAnalyses Add Raman analyses to the `RamanAnalyses` object. The argument `value` can be a character vector with full file paths to "asc", "sif", "json", "wdf", "sdf", "csv" and/or "txt" raman files or a list of `RamanAnalysis` objects.
#' @template arg-raman-x
#' @template arg-value
#' @export
#'
add.RamanAnalyses <- function(x, value) {
  if (is.character(value)) {
    if (
      all(vapply(
        value,
        function(z) tools::file_ext(z) %in% x$formats,
        FALSE
      ))
    ) {
      value <- .get_RamanAnalysis_from_files(value)
    } else {
      warning("File/s not valid!")
      return(x)
    }
  }
  if (
    !all(vapply(
      value,
      function(a) {
        checkmate::test_character(a$name) &&
          checkmate::test_character(a$replicate) &&
          checkmate::test_character(a$blank) &&
          checkmate::test_character(a$reference) &&
          checkmate::test_character(a$type) &&
          checkmate::test_numeric(a$concentration) &&
          checkmate::test_list(a$metadata) &&
          checkmate::test_data_table(a$spectra)
      },
      FALSE
    ))
  ) {
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
  analyses <- c(x$analyses, value)
  analyses <- analyses[order(names(analyses))]
  if (length(analyses) > length(x$analyses)) {
    if (length(x$results) > 0) {
      warning("All results removed!")
      x$results <- list()
    }
  }
  x$analyses <- analyses
  x
}

#' @describeIn RamanAnalyses Remove Raman analyses from the `RamanAnalyses` object. The argument `value` can be a character vector with the names of the analyses or a numeric vector with the indices of the analyses to remove.
#' @template arg-raman-x
#' @template arg-value
#' @export
#'
remove.RamanAnalyses <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!get_analysis_names(x) %in% value]
    x$analyses <- x$analyses[order(names(x$analyses))]
    if (!is.null(x$results[["RamanResults_Spectra"]])) {
      x$results$RamanResults_Spectra <- x$results$RamanResults_Spectra[
        !get_analysis_names(x) %in% value
      ]
    }
  } else if (is.numeric(value)) {
    x$analyses <- x$analyses[-value]
    x$analyses <- x$analyses[order(names(x$analyses))]
    if (!is.null(x$results[["RamanResults_Spectra"]])) {
      x$results$RamanResults_Spectra <- x$results$RamanResults_Spectra[-value]
    }
  }
  x
}

#' @describeIn RamanAnalyses Subset the `RamanAnalyses` object by analyses. The argument `i` can be a character vector with the names of the analyses or a numeric vector with the indices of the analyses to keep.
#' @template arg-raman-x
#' @template arg-i
#' @export
#'
`[.RamanAnalyses` <- function(x, i) {
  x$analyses <- x$analyses[i]
  if (!is.null(x$results[["RamanResults_Spectra"]])) {
    x$results$RamanResults_Spectra <- x$results$RamanResults_Spectra[i]
  }
  x
}

#' @export
#' @noRd
`[<-.RamanAnalyses` <- function(x, i, value) {
  x <- add(x, value)
  x
}

#' @export
#' @noRd
`[[.RamanAnalyses` <- function(x, i) {
  x$analyses <- x$analyses[[i]]
  if (!is.null(x$results[["RamanResults_Spectra"]])) {
    x$results$RamanResults_Spectra <- x$results$RamanResults_Spectra[[i]]
  }
  x
}

#' @export
#' @noRd
`[[<-.RamanAnalyses` <- function(x, i, value) {
  x <- add(x, value)
  x
}

#' @export
#' @noRd
`c.RamanAnalyses` <- function(x, ...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(x)
  }
  dots <- dots[vapply(
    dots,
    function(z) {
      if (is(z, "RamanAnalyses")) {
        is.null(validate_object(z))
      } else {
        FALSE
      }
    },
    FALSE
  )]
  if (length(dots) == 0) {
    return(x)
  }
  comb_analyses <- x$analyses
  comb_results <- x$results[["RamanResults_Spectra"]]

  for (i in seq_along(dots)) {
    comb_analyses <- c(comb_analyses, dots[[i]]$analyses)
    if (!is.null(comb_results)) {
      if (!is.null(dots[[i]]$results[["RamanResults_Spectra"]])) {
        new_spectra <- dots[[i]]$results[["RamanResults_Spectra"]]
        comb_results$spectra <- c(comb_results$spectra, new_spectra$spectra)
        comb_results$chrom_peaks <- c(
          comb_results$chrom_peaks,
          new_spectra$chrom_peaks
        )
        comb_results$peaks <- c(comb_results$peaks, new_spectra$peaks)
      }
    }
  }

  names(comb_analyses) <- vapply(
    comb_analyses,
    function(z) z$name,
    NA_character_
  )

  comb_analyses <- comb_analyses[!duplicated(comb_analyses)]
  comb_analyses <- comb_analyses[order(names(comb_analyses))]
  x$analyses <- comb_analyses

  if (!is.null(comb_results)) {
    comb_results$spectra <- comb_results$spectra[
      !duplicated(names(comb_results$spectra))
    ]
    comb_results$chrom_peaks <- comb_results$chrom_peaks[
      !duplicated(names(comb_results$chrom_peaks))
    ]
    comb_results$peaks <- comb_results$peaks[
      !duplicated(names(comb_results$peaks))
    ]

    if (length(comb_results$spectra) > 0) {
      comb_results$spectra <- comb_results$spectra[order(names(
        comb_results$spectra
      ))]
    }

    if (length(comb_results$chrom_peaks) > 0) {
      comb_results$chrom_peaks <- comb_results$chrom_peaks[order(names(
        comb_results$chrom_peaks
      ))]
    }

    if (length(comb_results$peaks) > 0) {
      comb_results$peaks <- comb_results$peaks[order(names(comb_results$peaks))]
    }

    if (!is.null(validate_object(comb_results))) {
      stop("Invalid RamanResults_Spectra object after combining!")
    }

    x$results[["RamanResults_Spectra"]] <- comb_results
  }
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid RamanAnalyses object after combining!")
  }
}

# MARK: get_spectra
#' @describeIn RamanAnalyses Get the spectra from the `RamanAnalyses` object.
#' @template arg-raman-x
#' @template arg-analyses
#' @template arg-raman-targets
#' @template arg-raman-target
#' @template arg-raman-minIntensity
#' @template arg-useRawData
#' @export
#'
get_spectra.RamanAnalyses <- function(
  x,
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE
) {
  analyses <- .check_analyses_argument(x$analyses, analyses)
  if (is.null(analyses)) {
    return(list())
  }
  if (is.null(x$results[["RamanResults_Spectra"]]) && !useRawData) {
    x$results[["RamanResults_Spectra"]] <- StreamFind::RamanResults_Spectra(
      lapply(x$analyses, function(z) z$spectra)
    )
  }
  if (useRawData) {
    spec <- StreamFind::RamanResults_Spectra(
      lapply(x$analyses, function(z) z$spectra)
    )
    spec$spectra <- spec$spectra[analyses]
  } else if (!is.null(x$results[["RamanResults_Spectra"]])) {
    spec <- x$results$RamanResults_Spectra
    if (spec$is_averaged) {
      rpl <- get_replicate_names(x)
      rpl <- rpl[analyses]
      spec$spectra <- spec$spectra[names(spec$spectra) %in% unname(rpl)]
      spec$spectra <- Map(
        function(z, y) {
          if (nrow(z) > 0) {
            z$replicate <- y
          }
          z
        },
        spec$spectra,
        names(spec$spectra)
      )
    } else {
      rpl <- get_replicate_names(x)
      spec$spectra <- spec$spectra[analyses]
      spec$spectra <- Map(
        function(z, y) {
          if (nrow(z) > 0) {
            z$analysis <- y
            z$replicate <- rpl[y]
          }
          z
        },
        spec$spectra,
        names(spec$spectra)
      )
    }
  } else {
    warning("No spectra results object available!")
    return(list())
  }
  if (length(spec$chrom_peaks) > 0) {
    if (length(spec$spectra) == length(spec$chrom_peaks)) {
      spec$spectra <- Map(
        function(z, y) {
          if (nrow(z) > 0) {
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
          }
          z
        },
        spec$spectra,
        spec$chrom_peaks[names(spec$spectra)]
      )
    }
  }
  spec_list <- spec$spectra
  spec_list <- lapply(spec_list, function(z) {
    if (
      !is.null(targets) && ("group" %in% colnames(z) || "id" %in% colnames(z))
    ) {
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
    if (!is.null(minIntensity)) {
      z <- z[z$intensity >= minIntensity, ]
    }
    z
  })
  spec_list
}

# MARK: get_spectra_matrix
#' @describeIn RamanAnalyses Get the spectra matrix from the `RamanAnalyses` object.
#' @template arg-raman-x
#' @template arg-analyses
#' @template arg-raman-targets
#' @template arg-raman-target
#' @template arg-raman-minIntensity
#' @template arg-useRawData
#' @export
#'
get_spectra_matrix.RamanAnalyses <- function(
  x,
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE
) {
  analyses <- .check_analyses_argument(x$analyses, analyses)
  if (is.null(analyses)) {
    return(list())
  }
  spec_list <- get_spectra(
    x,
    analyses,
    targets,
    rt,
    shift,
    minIntensity,
    useRawData
  )
  if (length(spec_list) == 0) {
    return(NULL)
  }
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
#' @describeIn RamanAnalyses Plot the spectra from the `RamanAnalyses` object.
#' @template arg-raman-x
#' @template arg-analyses
#' @template arg-raman-targets
#' @template arg-raman-target
#' @template arg-raman-minIntensity
#' @template arg-useRawData
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_spectra.RamanAnalyses <- function(
  x,
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  interactive = TRUE,
  renderEngine = "webgl"
) {
  spectra <- get_spectra(
    x,
    analyses,
    targets,
    rt,
    shift,
    minIntensity,
    useRawData
  )
  if (length(spectra) == 0) {
    return(NULL)
  }
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  spectra <- data.table::rbindlist(spectra, fill = TRUE)
  groupCols <- c("shift")
  if ("analysis" %in% colnames(spectra)) {
    groupCols <- c("analysis", groupCols)
  }
  if ("replicate" %in% colnames(spectra)) {
    groupCols <- c("replicate", groupCols)
  }
  if ("id" %in% colnames(spectra)) {
    groupCols <- c("id", groupCols)
  }
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
  if (is.null(yLab)) {
    yLab <- "Raman intensity / A.U."
  }
  colorBy <- gsub("chrom_peaks", "targets", colorBy)
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
  spectra$loop <- paste0(
    spectra$analysis,
    spectra$replicate,
    spectra$id,
    spectra$var
  )
  cl <- .get_colors(unique(spectra$var))
  if (!interactive) {
    ggplot2::ggplot(
      spectra,
      ggplot2::aes(x = shift, y = intensity, group = loop)
    ) +
      ggplot2::geom_line(ggplot2::aes(color = var)) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )
    loop <- NULL
    plot <- spectra %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~shift,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines",
        line = list(width = 0.5),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>id: ",
          id,
          "<br>shift: ",
          shift,
          "<br>intensity: ",
          intensity
        ),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
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

# MARK: plot_spectra_3d
#' @describeIn RamanAnalyses Plot the spectra in 3D from the `RamanAnalyses` object.
#' @template arg-raman-x
#' @template arg-analyses
#' @template arg-raman-targets
#' @template arg-raman-target
#' @template arg-raman-minIntensity
#' @template arg-useRawData
#' @template arg-LegendNames
#' @template arg-colorBy
#' @template arg-labs
#' @template arg-zLab
#' @template arg-renderEngine
#' @export
#'
plot_spectra_3d.RamanAnalyses <- function(
  x,
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
  renderEngine = "webgl"
) {
  spectra <- get_spectra(
    x,
    analyses,
    targets,
    rt,
    shift,
    minIntensity,
    useRawData
  )
  if (length(spectra) == 0) {
    return(NULL)
  }
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  spectra <- data.table::rbindlist(spectra, fill = TRUE)
  xlab <- "Shift / cm<sup>-1</sup>"
  ylab <- "Retention time / seconds"
  zlab <- "Intensity / counts"
  if (!is.null(xLab)) {
    xlab <- xLab
  }
  if (!is.null(yLab)) {
    ylab <- yLab
  }
  if (!is.null(zLab)) {
    zlab <- zLab
  }
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
    "<br>analysis: ",
    spectra$analysis,
    "<br>replicate: ",
    spectra$replicate,
    "<br>id: ",
    spectra$id,
    "<br>rt: ",
    spectra$rt,
    "<br>shift: ",
    spectra$shift,
    "<br>intensity: ",
    spectra$intensity
  )
  fig <- plotly::plot_ly(spectra, x = ~shift, y = ~rt, z = ~intensity) %>%
    group_by(spectra$rt) %>%
    plotly::add_lines(
      color = ~var,
      colors = colors_var,
      line = list(width = 1),
      hoverinfo = "text",
      text = hover_text,
      line = list(width = 4)
    )
  fig <- fig %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab),
        zaxis = list(title = zlab)
      )
    )
  if (renderEngine %in% "webgl") {
    fig <- fig %>% plotly::toWebGL()
  }
  fig
}

# MARK: plot_spectra_baseline
#' @describeIn RamanAnalyses Plot the baseline-corrected spectra from the `RamanAnalyses` object.
#' @template arg-raman-x
#' @template arg-analyses
#' @template arg-raman-targets
#' @template arg-raman-target
#' @template arg-raman-minIntensity
#' @template arg-labs
#' @template arg-title
#' @template arg-colorBy
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_spectra_baseline.RamanAnalyses <- function(
  x,
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  interactive = TRUE,
  renderEngine = "webgl"
) {
  spectra <- get_spectra(
    x,
    analyses,
    targets,
    rt,
    shift,
    minIntensity,
    useRawData = FALSE
  )
  if (length(spectra) == 0) {
    return(NULL)
  }
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  spectra <- data.table::rbindlist(spectra, fill = TRUE)
  if (!"baseline" %in% colnames(spectra)) {
    warning("Baseline not found!")
    return(NULL)
  }
  groupCols <- c("analysis", "replicate", "shift")
  if ("id" %in% colnames(spectra)) {
    groupCols <- c("id", groupCols)
  }
  if ("group" %in% colnames(spectra)) {
    groupCols <- c("group", groupCols)
  }
  groupCols <- groupCols[groupCols %in% colnames(spectra)]
  baseline = NULL
  spectra <- spectra[,
    .(baseline = mean(baseline), raw = mean(raw)),
    by = groupCols
  ]
  spectra <- unique(spectra)
  if (is.null(xLab)) {
    if (interactive) {
      xLab <- "Raman shift / cm<sup>-1</sup>"
    } else {
      xLab <- expression("Raman shift / cm"^"-1")
    }
  }
  if (is.null(yLab)) {
    yLab <- "Raman intensity / A.U."
  }
  colorBy <- gsub("chrom_peaks", "targets", colorBy)
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
  spectra$loop <- paste0(
    spectra$analysis,
    spectra$replicate,
    spectra$id,
    spectra$var
  )
  cl <- .get_colors(unique(spectra$var))
  if (!interactive) {
    ggplot2::ggplot(spectra, ggplot2::aes(x = shift, group = loop)) +
      ggplot2::geom_line(ggplot2::aes(y = raw, color = var)) +
      ggplot2::geom_line(
        ggplot2::aes(y = baseline, color = var),
        linetype = "dashed"
      ) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )
    loop <- NULL
    plot <- spectra %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~shift,
        y = ~raw,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines",
        line = list(width = 0.5),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>id: ",
          id,
          "<br>shift: ",
          shift,
          "<br>intensity: ",
          raw
        ),
        hoverinfo = "text",
        name = ~var,
        legendgroup = ~var
      ) %>%
      plotly::add_trace(
        x = ~shift,
        y = ~baseline,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines",
        line = list(dash = "dash", width = 0.5),
        name = ~var,
        legendgroup = ~var,
        showlegend = FALSE
      ) %>%
      plotly::layout(
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

# MARK: plot_chromatograms
#' @describeIn RamanAnalyses Plot the chromatograms from the `RamanAnalyses` object when the spectra have a time dimension.
#' @template arg-raman-x
#' @template arg-analyses
#' @template arg-raman-targets
#' @template arg-raman-target
#' @template arg-raman-minIntensity
#' @template arg-useRawData
#' @template arg-labs
#' @template arg-title
#' @template arg-colorBy
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#' 
plot_chromatograms.RamanAnalyses <- function(
  x,
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  interactive = TRUE,
  renderEngine = "webgl"
) {
  spectra <- get_spectra(
    x,
    analyses,
    targets,
    rt,
    shift,
    minIntensity,
    useRawData
  )
  if (length(spectra) == 0) {
    return(NULL)
  }
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  spectra <- data.table::rbindlist(spectra, fill = TRUE)
  if (!"rt" %in% colnames(spectra)) {
    warning(
      "Column rt not found in spectra data.table for plotting chromatograms!"
    )
    return(NULL)
  }
  intensity <- NULL
  groupCols <- c("analysis", "replicate", "rt")
  if ("id" %in% colnames(spectra)) {
    groupCols <- c("id", groupCols)
  }
  if ("group" %in% colnames(spectra)) {
    groupCols <- c("group", groupCols)
  }
  groupCols <- groupCols[groupCols %in% colnames(spectra)]
  spectra <- spectra[, .(intensity = sum(intensity)), by = groupCols]
  spectra <- unique(spectra)
  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  if (is.null(yLab)) {
    yLab <- "Cumulative Raman intensity / A.U."
  }
  colorBy <- gsub("chrom_peaks", "targets", colorBy)
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
  spectra$loop <- paste0(
    spectra$analysis,
    spectra$replicate,
    spectra$id,
    spectra$var
  )
  cl <- .get_colors(unique(spectra$var))
  if (!interactive) {
    ggplot2::ggplot(
      spectra,
      ggplot2::aes(x = rt, y = intensity, group = loop)
    ) +
      ggplot2::geom_line(ggplot2::aes(color = var)) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )
    loop <- NULL
    plot <- spectra %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines",
        line = list(width = 0.5),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>id: ",
          id,
          "<br>rt: ",
          rt,
          "<br>intensity: ",
          intensity
        ),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
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

# MARK: get_chromatograms_peaks
#' @describeIn RamanAnalyses Get the chromatograms peaks from the `RamanAnalyses` object.
#' @template arg-raman-x
#' @template arg-analyses
#' @template arg-raman-targets
#' @template arg-raman-rt
#' @export
#' 
get_chromatograms_peaks.RamanAnalyses <- function(
  x,
  analyses = NULL,
  targets = NULL,
  rt = NULL
) {
  analyses <- .check_analyses_argument(x$analyses, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }
  if(is.null(x$results[["RamanResults_Spectra"]])) {
    message("\U2717 No RamanResults_Spectra object found!")
    return(data.table::data.table())
  }
  pks <- x$results[["RamanResults_Spectra"]]$chrom_peaks
  if (length(pks) == 0) {
    return(data.table::data.table())
  }
  if (x$results[["RamanResults_Spectra"]]$is_averaged) {
    pks <- data.table::rbindlist(
      pks,
      idcol = "replicate",
      fill = TRUE
    )
  } else {
    pks <- data.table::rbindlist(
      pks,
      idcol = "analysis",
      fill = TRUE
    )
  }
  if ("analysis" %in% colnames(pks)) {
    pks <- pks[pks$analysis %in% analyses, ]
  } else if ("replicate" %in% colnames(pks)) {
    rpl <- get_replicate_names(x)
    rpl <- rpl[analyses]
    pks <- pks[pks$replicate %in% unname(rpl)]
    if (!"analysis" %in% colnames(pks)) {
      pks$analysis <- pks$replicate
      data.table::setcolorder(pks, c("analysis", "replicate"))
    }
  }
  if (!"replicate" %in% colnames(pks)) {
    rpl <- get_replicate_names(x)
    pks$replicate <- rpl[pks$analysis]
  }
  if (!is.null(targets)) {
    if ("group" %in% colnames(pks)) {
      pks <- pks[pks$group %in% targets, ]
    } else if ("id" %in% colnames(pks)) {
      pks <- pks[pks$id %in% targets, ]
    }
  }
  if (is.numeric(rt) && length(rt) == 2) {
    rt <- sort(rt)
    sel <- pks$rt >= rt[1] & pks$rt <= rt[2]
    pks <- pks[sel, ]
  }
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found for the targets!")
    return(data.table::data.table())
  }
  pks
}

# MARK: plot_chromatograms_peaks
#' @describeIn RamanAnalyses Plot the chromatograms peaks from the `RamanAnalyses` object.
#' @template arg-raman-x
#' @template arg-analyses
#' @template arg-raman-targets
#' @template arg-raman-rt
#' @template arg-title
#' @template arg-legendNames
#' @template arg-colorBy
#' @template arg-xlim-ylim
#' @template arg-labs
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#' 
plot_chromatograms_peaks.RamanAnalyses <- function(
  x,
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  title = NULL,
  legendNames = TRUE,
  colorBy = "targets",
  xlim = NULL,
  ylim = NULL,
  xLab = NULL,
  yLab = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  pks <- get_chromatograms_peaks(x, analyses, targets, rt)
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found!")
    return(NULL)
  }
  spectra <- get_spectra(
    x,
    analyses,
    targets,
    rt,
    shift = NULL,
    minIntensity = 0,
    useRawData = FALSE
  )
  if (length(spectra) == 0) {
    return(NULL)
  }
  if (sum(vapply(spectra, nrow, 0)) == 0) {
    warning("No spectra found for the defined targets!")
    return(NULL)
  }
  spectra <- data.table::rbindlist(spectra, fill = TRUE)
  if (!"rt" %in% colnames(spectra)) {
    warning(
      "Column rt not found in spectra data.table for plotting chromatograms!"
    )
    return(NULL)
  }
  intensity <- NULL
  groupCols <- c("analysis", "replicate", "rt")
  if ("id" %in% colnames(spectra)) {
    groupCols <- c("id", groupCols)
  }
  if ("group" %in% colnames(spectra)) {
    groupCols <- c("group", groupCols)
  }
  groupCols <- groupCols[groupCols %in% colnames(spectra)]
  spectra <- spectra[, .(intensity = sum(intensity)), by = groupCols]
  spectra <- unique(spectra)
  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  if (is.null(yLab)) {
    yLab <- "Cumulative Raman intensity / A.U."
  }
  colorBy <- gsub("chrom_peaks", "targets", colorBy)
  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames)
  if ("group" %in% colnames(pks)) {
    pks$id <- pks$group
  } else {
    pks$id <- pks$peak
  }
  pks <- .make_colorBy_varkey(pks, colorBy, legendNames)
  cl <- .get_colors(unique(pks$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)
  if (!interactive) {
    plot <- ggplot2::ggplot(spectra, ggplot2::aes(x = rt))
    for (i in seq_len(nrow(pks))) {
      pk_analysis <- pks[["analysis"]][i]
      pk_replicate <- pks[["replicate"]][i]
      pk_id <- pks[["id"]][i]
      pk_var <- pks[["var"]][i]
      temp <- dplyr::filter(
        spectra,
        analysis %in% pk_analysis & replicate %in% pk_replicate
      )
      temp$var <- pk_var
      plot <- plot +
        ggplot2::geom_line(
          data = temp,
          ggplot2::aes(y = intensity, color = var)
        )
      temp <- temp[temp$id %in% pk_id, ]
      plot <- plot +
        ggplot2::geom_ribbon(
          data = temp,
          ggplot2::aes(
            ymin = rep(min(intensity), length(intensity)),
            ymax = intensity,
            fill = var
          )
        )
    }
    plot <- plot +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_fill_manual(values = cl50, guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
    plot
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )
    show_legend <- rep(TRUE, length(cl))
    names(show_legend) <- names(cl)
    plot <- plot_ly(spectra, x = ~rt)
    for (i in seq_len(nrow(pks))) {
      pk_analysis <- pks[["analysis"]][i]
      pk_replicate <- pks[["replicate"]][i]
      pk_id <- pks[["id"]][i]
      pk_var <- pks[["var"]][i]
      plot <- plot %>%
        add_trace(
          data = dplyr::filter(
            spectra,
            analysis %in%
              pk_analysis &
              replicate %in% pk_replicate &
              id %in% pk_id
          ),
          x = ~rt,
          y = ~intensity,
          type = "scatter",
          mode = "markers",
          marker = list(color = cl[pk_var], size = 5),
          text = ~ paste(
            "<br>analysis: ",
            pk_analysis,
            "<br>replicate: ",
            pk_replicate,
            "<br>id: ",
            pk_id,
            "<br>rt: ",
            round(rt, 2),
            "<br>intensity: ",
            round(intensity, 0)
          ),
          hoverinfo = "text",
          name = pk_var,
          legendgroup = pk_var,
          showlegend = FALSE
        )
      plot <- plot %>%
        plotly::add_ribbons(
          data = dplyr::filter(
            spectra,
            analysis %in%
              pk_analysis &
              replicate %in% pk_replicate &
              id %in% pk_id
          ),
          x = ~rt,
          ymin = ~ min(intensity),
          ymax = ~intensity,
          line = list(color = cl[pk_var], width = 1.5),
          fillcolor = cl50[pk_var],
          text = ~ paste(
            "<br>analysis: ",
            pk_analysis,
            "<br>replicate: ",
            pk_replicate,
            "<br>id: ",
            pk_id,
            "<br>rt: ",
            round(rt, 2),
            "<br>intensity: ",
            round(intensity, 0)
          ),
          hoverinfo = "text",
          name = pk_var,
          legendgroup = pk_var,
          showlegend = show_legend[pk_var]
        )

      show_legend[pk_var] <- FALSE
    }
    for (i in seq_len(nrow(pks))) {
      pk_analysis <- pks[["analysis"]][i]
      pk_replicate <- pks[["replicate"]][i]
      pk_id <- pks[["id"]][i]
      pk_var <- pks[["var"]][i]

      plot <- plot %>%
        add_trace(
          data = dplyr::filter(
            spectra,
            analysis %in% pk_analysis & replicate %in% pk_replicate
          ),
          x = ~rt,
          y = ~intensity,
          type = "scatter",
          mode = "lines",
          line = list(color = cl[pk_var], width = 0.5),
          name = pk_var,
          legendgroup = pk_var,
          showlegend = FALSE
        )
    }
    plot <- plot %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    if (renderEngine %in% "webgl") {
      # Fix for warnings with hoveron when webgl is used
      plot$x$attrs <- lapply(plot$x$attrs, function(x) {
        if (!is.null(x[["hoveron"]])) {
          x[["hoveron"]] <- NULL
        }
        x
      })
      plot <- plot %>% plotly::toWebGL()
    }
    plot
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

    valid_files <- vapply(
      files,
      FUN.VALUE = FALSE,
      function(x, possible_file_formats) {
        if (!file.exists(x)) {
          return(FALSE)
        }
        if (!tools::file_ext(x) %in% possible_file_formats) {
          return(FALSE)
        }
        TRUE
      },
      possible_file_formats = possible_file_formats
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
      cache <- .load_cache_sqlite("parsed_raman_analyses", x)

      if (!is.null(cache$data)) {
        message("\U2139 Analysis loaded from cache!")
        cache$data
      } else {
        message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
        format <- tools::file_ext(x)

        if (format %in% c("sif", "json", "wdf", "sdf", "csv", "txt")) {
          format <- "orpl_format"
        }

        switch(
          format,

          "asc" = {
            ana <- rcpp_parse_asc_file(x)
          },

          "orpl_format" = {
            if (!reticulate::py_module_available("orpl")) {
              if (!reticulate::virtualenv_exists("r-StreamFind")) {
                warning("Python virtual environment 'r-StreamFind' not found!")
                return(NULL)
              }

              tryCatch(
                {
                  reticulate::py_install("orpl", envname = "r-StreamFind")
                },
                error = function(e) {
                  warning(
                    "Error installing Python module 'orpl'! The error is ",
                    e
                  )
                  return(NULL)
                }
              )

              if (!reticulate::py_module_available("orpl")) {
                warning(
                  "Python module 'orpl' not available for reading .sif files!"
                )
                return(NULL)
              }
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
                calibration_data <- sif_file$metadata$details[[
                  "Calibration_data"
                ]]
                ex_wavelength <- sif_file$metadata$details[[
                  "RamanExWavelength"
                ]]
                calibration_nm <- rep(NA_real_, detector_dimension)

                if (calibration_data[1] == 0) {
                  calibration_data[1] <- ex_wavelength
                }

                for (i in seq_len(detector_dimension)) {
                  calibration_nm[i] <- calibration_data[1]
                  for (j in 2:length(calibration_data)) {
                    if (calibration_data[j] > 0) {
                      calibration_nm[i] <- calibration_nm[i] +
                        calibration_data[j] * pixels[i]^(j - 1)
                    }
                  }
                }

                shifts <- ((1 / ex_wavelength) - (1 / calibration_nm)) * 1e7
                shifts <- round(shifts, digits = 0)

                exposure_time <- as.numeric(sif_file$metadata$exposure_time)

                spectra <- lapply(
                  seq_len(ncol(spectra)),
                  function(x, shifts, exposure_time) {
                    data.table::data.table(
                      "rt" = x * exposure_time,
                      "shift" = shifts,
                      "intensity" = rev(spectra[, x])
                    )
                  },
                  shifts = shifts,
                  exposure_time = exposure_time
                )

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
        ana$blank <- blanks[x]
        ana$concentration <- concentrations[x]
        ana$reference <- references[x]

        if ("rt" %in% colnames(ana$spectra)) {
          ana$type <- "LC-Raman"
        }

        if (!is.null(cache$hash)) {
          .save_cache_sqlite("parsed_raman_analyses", ana, cache$hash)
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
