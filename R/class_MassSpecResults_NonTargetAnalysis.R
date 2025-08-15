# MARK: MassSpecResults_NonTargetAnalysis
#' @title MassSpecResults_NonTargetAnalysis S3 Class
#' @description The `MassSpecResults_NonTargetAnalysis` class is a child of the [StreamFind::Results] class and is used to store results from non-target analysis (NTA) workflows for mass spectrometry data ("MassSpec").
#' @param info A data frame containing information about the analyses.
#' @param headers A list of data frames containing information about the spectra headers.
#' @param features A list of data frames containing information about the features.
#' @return An object of class `MassSpecResults_NonTargetAnalysis` with the following structure:
#' \itemize{
#'   \item `type`: The type of the results, which is "MassSpec".
#'   \item `name`: The name of the results, which is "MassSpecResults_NonTargetAnalysis".
#'   \item `software`: The software used for the analysis, which is "StreamFind".
#'   \item `version`: The version of the software, as a character string.
#'   \item `info`: A data frame containing information about the analyses.
#'   \item `headers`: A list of data frames containing information about the spectra headers.
#'   \item `features`: A list of data frames containing information about the features.
#' }
#' The `info` data.table contains the following columns: analysis, replicate, blank, polarity and file. Each `features` data frame contains the following columns: feature, group, rt, mz, intensity, area, rtmin, rtmax, mzmin, mzmax, mass, polarity, adduct, filtered, filter, filled, correction, eic, ms1, ms2, quality, annotation, istd, suspects, formulas and compounds.
#' @export
#'
MassSpecResults_NonTargetAnalysis <- function(
  info = data.table::data.table(),
  headers = list(),
  features = list()
) {
  x <- structure(
    list(
      type = "MassSpec",
      name = "MassSpecResults_NonTargetAnalysis",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      info = info,
      headers = headers,
      features = features
    ),
    class = c("MassSpecResults_NonTargetAnalysis", "Results")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecResults_NonTargetAnalysis object.")
  }
}

#' @describeIn MassSpecResults_NonTargetAnalysis Validates the MassSpecResults_NonTargetAnalysis object, returning NULL if valid.
#' @template arg-nts-x
#' @export
#'
validate_object.MassSpecResults_NonTargetAnalysis = function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_true(x$name == "MassSpecResults_NonTargetAnalysis")
  checkmate::assert_true(x$software == "StreamFind")
  checkmate::assert_character(x$version, len = 1)
  if (length(x$features) > 0) {
    checkmate::assert_true(identical(x$info$analysis, names(x$features)))
    checkmate::assert_true(identical(x$info$analysis, names(x$headers)))
    fp <- c(
      "feature",
      "group",
      "rt",
      "mz",
      "intensity",
      "area",
      "rtmin",
      "rtmax",
      "mzmin",
      "mzmax",
      "mass",
      "polarity",
      "adduct",
      "filtered",
      "filter",
      "filled",
      "correction",
      "eic",
      "ms1",
      "ms2",
      "quality",
      "annotation",
      "istd",
      "suspects",
      "formulas",
      "compounds"
    )
    lapply(x$features, function(z) {
      checkmate::assert_data_table(z)
      if (nrow(z) > 0) {
        checkmate::assert_true(all(fp %in% colnames(z)))
      }
    })
  }
  NextMethod()
  NULL
}

# MARK: show
#' @describeIn MassSpecResults_NonTargetAnalysis Prints a summary of the MassSpecResults_NonTargetAnalysis object.
#' @template arg-nts-x
#' @export
#'
show.MassSpecResults_NonTargetAnalysis <- function(x) {
  cat("\n")
  cat(is(x))
  cat("\n")
  if (length(x$features) == 0) {
    cat("No features found!")
    return()
  }
  info <- data.table::data.table(
    "analysis" = x$info$analysis,
    "replicate" = x$info$replicate,
    "features" = vapply(x$features, nrow, integer(1)),
    "filtered" = vapply(
      x$features,
      function(z) {
        sum(z$filtered)
      },
      integer(1)
    ),
    "groups" = vapply(
      x$features,
      function(z) {
        grs <- unique(z$group)
        grs <- grs[!is.na(grs) & grs != ""]
        length(grs)
      },
      integer(1)
    ),
    "polarity" = x$info$polarity
  )
  print(info)
}

#' @describeIn MassSpecResults_NonTargetAnalysis Converts a list object to a `MassSpecResults_NonTargetAnalysis` object if it is compatible.
#' @template arg-value
#' @export
#' 
as.MassSpecResults_NonTargetAnalysis <- function(value) {
  if (is(value, "MassSpecResults_NonTargetAnalysis")) {
    if(is.null(validate_object(value))) {
      return(value)
    } else {
      stop("Invalid MassSpecResults_NonTargetAnalysis object.")
    }
  }
  if (is.list(value)) {
    if (all(c("info", "headers", "features") %in% names(value))) {
      value$info <- data.table::as.data.table(value$info)
      checkmate::assert_data_table(value$info)
      checkmate::assert_true(all(c("analysis", "replicate", "blank", "polarity", "file") %in% colnames(value$info)))
      value$headers <- lapply(value$headers, data.table::as.data.table)
      checkmate::assert_list(value$headers)
      lapply(value$headers, function(z) checkmate::assert_data_table(z))
      checkmate::assert_true(
        identical(
          value$info$analysis,
          names(value$headers)
        )
      )
      checkmate::assert_list(value$features)
      value$features <- lapply(value$features, function(z) {
        z <- data.table::as.data.table(z)
        if (nrow(z) == 0) return(z)
        checkmate::assert_true(
          all(
            c(
              "feature", "group", "rt", "mz", "intensity", "area",
              "rtmin", "rtmax", "mzmin", "mzmax", "mass", "polarity",
              "adduct", "filtered", "filter", "filled", "correction",
              "eic", "ms1", "ms2", "quality", "annotation",
              "istd", "suspects", "formulas", "compounds"
            ) %in% colnames(z)
          )
        )
        z$eic <- lapply(z$eic, data.table::as.data.table)
        z$ms1 <- lapply(z$ms1, data.table::as.data.table)
        z$ms2 <- lapply(z$ms2, data.table::as.data.table)
        z$quality <- lapply(z$quality, data.table::as.data.table)
        z$annotation <- lapply(z$annotation, data.table::as.data.table)
        z$istd <- lapply(z$istd, data.table::as.data.table)
        z$suspects <- lapply(z$suspects, data.table::as.data.table)
        z$formulas <- lapply(z$formulas, data.table::as.data.table)
        z$compounds <- lapply(z$compounds, data.table::as.data.table)
        z
      })
      checkmate::assert_true(
        identical(
          value$info$analysis,
          names(value$features)
        )
      )
      nts <- MassSpecResults_NonTargetAnalysis(
        info = value$info,
        headers = value$headers,
        features = value$features
      )
      if (is.null(validate_object(nts))) {
        return(nts)
      } else {
        stop("Invalid MassSpecResults_NonTargetAnalysis object.")
      }
    } else {
      stop("List does not contain required components for MassSpecResults_NonTargetAnalysis.")
    }
  } else {
    stop("Value is not a list or MassSpecResults_NonTargetAnalysis object.")
  }
}

# MARK: names
#' @export
#' @noRd
names.MassSpecResults_NonTargetAnalysis <- function(x) {
  names(x$features)
}

# MARK: `[`
#' @describeIn MassSpecResults_NonTargetAnalysis Subsets the MassSpecResults_NonTargetAnalysis object by analyses (with `i`) or feature groups (`j`).
#' @template arg-nts-x
#' @template arg-i
#' @template arg-j
#' @export
#'
`[.MassSpecResults_NonTargetAnalysis` <- function(x, i, j) {
  if (length(x$features) == 0) {
    warning("No features found to subset!")
    return(x)
  }
  if (!missing(i)) {
    x$info <- x$info[i, ]
    x$headers <- x$headers[i]
    x$features <- x$features[i]
  }
  if (!missing(j)) {
    if (all(vapply(x$features, function(z) all(is.na(z$group) | z$group %in% ""), FALSE))) {
      warning("No feature groups found to subset!")
    } else if (is.character(j) || is.numeric(j)) {
      x$features <- lapply(x$features, function(z) {
        z <- z[z$group %in% j, ]
        z
      })
    } else if (data.table::is.data.table(j)) {
      if (all(c("feature", "analysis") %in% colnames(j))) {
        fts <- x$features
        fts_names <- names(fts)

        fts <- Map(
          function(z, k) {
            if (nrow(z) > 0) {
              z$analysis <- k
            }
            z
          },
          fts,
          fts_names
        )

        fts <- lapply(
          fts,
          function(z, j) {
            if (nrow(z) > 0) {
              z <- z[z$feature %in% j$feature & z$analysis %in% j$analysis, ]
            }
            z
          },
          j = j
        )

        fts <- lapply(fts, function(z) {
          if (nrow(z) > 0) {
            z$analysis <- NULL
          }
          z
        })

        names(fts) <- fts_names

        x$features <- fts
      }
    }
  }
  x
}

# MARK: `[[`
#' @describeIn MassSpecResults_NonTargetAnalysis Subsets the MassSpecResults_NonTargetAnalysis object by feature groups. The argument `value` should be a character vector with the group names.
#' @template arg-value
#' @export
#'
`[[.MassSpecResults_NonTargetAnalysis` <- function(x, value) {
  if (!missing(value)) {
    if (length(x$features) == 0) {
      warning("No feature groups found to subset!")
    } else {
      if (!any(vapply(x$features, function(z) any(!is.na(z$group), FALSE)))) {
        warning("No feature groups found to subset!")
      } else {
        x$features <- lapply(x$features, function(z) {
          z <- z[z$group %in% value, ]
          z
        })
      }
    }
  }
  x
}

# MARK: get_features_count
#' @describeIn MassSpecResults_NonTargetAnalysis Returns a data table with the number of features for each analysis.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-filtered
#' @export
#'
get_features_count.MassSpecResults_NonTargetAnalysis <- function(
  x,
  analyses = NULL,
  filtered = FALSE
) {
  analyses <- .check_analyses_argument(x, analyses)
  info <- data.table::data.table()
  if (length(x$features) > 0) {
    info <- data.table::data.table(
      "analysis" = x$info$analysis,
      "replicate" = x$info$replicate,
      "features" = vapply(x$features, function(z, filtered) {
        if (filtered) return(nrow(z))
        return(nrow(z[!z$filtered, ]))
      }, integer(1), filtered = filtered),
      "filtered" = vapply(
        x$features,
        function(z) {
          sum(z$filtered)
        },
        integer(1)
      ),
      "groups" = vapply(
        x$features,
        function(z, filtered) {
          if (filtered) {
            z <- z[!z$filtered, ]
            zg <- unique(z$group)
            zg <- zg[!is.na(zg) & zg != ""]
            return(length(zg))
          } else {
            zg <- unique(z$group[!z$filtered])
            zg <- zg[!is.na(zg) & zg != ""]
            return(length(zg))
          }
        },
        integer(1), filtered = filtered
      )
    )
    info <- info[info$analysis %in% analyses, ]
  }
  info
}

# MARK: plot_features_count
#' @describeIn MassSpecResults_NonTargetAnalysis Plots the number of features for each analysis as a bar plot.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-filtered
#' @template arg-yLab
#' @template arg-title
#' @template arg-colorBy
#' @template arg-showLegend
#' @template arg-showHoverText
#' @export
#'
plot_features_count.MassSpecResults_NonTargetAnalysis <- function(
  x,
  analyses = NULL,
  filtered = FALSE,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  showLegend = TRUE,
  showHoverText = TRUE
) {
  info <- get_features_count(x, analyses, filtered)

  if ("replicates" %in% colorBy) {
    info$analysis <- info$replicate
  }

  features <- NULL

  info <- info[,
    .(
      features = round(mean(features), digits = 0),
      features_sd = round(sd(features), digits = 0),
      n_analysis = length(features)
    ),
    by = c("analysis")
  ]

  info$features_sd[is.na(info$features_sd)] <- 0

  info <- unique(info)

  if (showHoverText) {
    info$hover_text <- paste(
      info$analysis,
      "<br>",
      "N.: ",
      info$n_analysis,
      "<br>",
      "Features: ",
      info$features,
      " (SD: ",
      info$features_sd,
      ")"
    )
  } else {
    info$hover_text <- ""
  }

  info <- info[order(info$analysis), ]

  colors_tag <- .get_colors(info$analysis)

  if (is.null(yLab)) {
    yLab <- "Number of features"
  }

  plot <- plotly::plot_ly(
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
  ) %>%
    plotly::layout(
      xaxis = list(title = NULL, tickfont = list(size = 14)),
      yaxis = list(
        title = yLab,
        tickfont = list(size = 14),
        titlefont = list(size = 18)
      )
    )

  plot
}

# MARK: get_features
#' @describeIn MassSpecResults_NonTargetAnalysis Returns a data table with the features for the specified analyses and targets.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @export
#'
get_features.MassSpecResults_NonTargetAnalysis <- function(
  x,
  analyses = NULL,
  features = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  filtered = FALSE
) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }

  fts <- NULL

  if (length(x$features) > 0) {
    fts <- x$features[analyses]
  }

  if (is.null(fts)) {
    return(data.table::data.table())
  }

  fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  if (!filtered) {
    fts <- fts[!fts$filtered, ]
  }

  fts$feature <- as.character(fts$feature)

  if (!is.null(features)) {
    target_id <- features

    if (is.character(target_id)) {
      if ("group" %in% colnames(fts)) {
        fts <- fts[fts$feature %in% target_id | fts$group %in% target_id, ]
      } else {
        fts <- fts[fts$feature %in% target_id, ]
      }
      rpls <- x$info$replicate
      names(rpls) <- x$info$analysis
      fts$replicate <- rpls[fts$analysis]
      return(fts)
    } else if (is.numeric(target_id)) {
      fts <- fts[target_id, ]
      rpls <- x$info$replicate
      names(rpls) <- x$info$analysis
      fts$replicate <- rpls[fts$analysis]
      return(fts)
    }

    if (is.data.frame(target_id)) {
      if (all(colnames(fts) %in% colnames(target_id))) {
        return(target_id)
      }
      if ("analysis" %in% colnames(target_id)) {
        sel <- rep(FALSE, nrow(fts))
        for (i in seq_len(nrow(target_id))) {
          sel[
            (fts$feature %in%
              target_id$feature[i] &
              fts$analysis %in% target_id$analysis[i]) |
              fts$group %in% target_id$group
          ] <- TRUE
        }
        fts <- fts[sel, ]
        if ("name" %in% colnames(target_id)) {
          ids <- target_id$name
          names(ids) <- target_id$feature
          fts$name <- ids[fts$feature]
        }
        rpls <- x$info$replicate
        names(rpls) <- x$info$analysis
        fts$replicate <- rpls[fts$analysis]
        return(fts)
      } else if ("group" %in% colnames(target_id)) {
        sel <- rep(FALSE, nrow(fts))
        for (i in seq_len(nrow(target_id))) {
          sel[
            fts$feature %in%
              target_id$feature[i] |
              fts$group %in% target_id$group
          ] <- TRUE
        }
        fts <- fts[sel, ]
        if ("name" %in% colnames(target_id)) {
          ids <- target_id$name
          names(ids) <- target_id$group
          ids <- ids[!duplicated(names(ids))]
          fts$name <- ids[fts$group]
        }
        rpls <- x$info$replicate
        names(rpls) <- x$info$analysis
        fts$replicate <- rpls[fts$analysis]
        return(fts)
      }
    }
    return(data.table::data.table())
  }
  polarities <- vapply(
    x$headers[analyses],
    function(a) {
      paste0(unique(a$polarity), collapse = ", ")
    },
    NA_character_
  )
  id <- NULL
  targets <- MassSpecTargets(
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    analyses,
    polarities
  )
  if (nrow(targets) > 0) {
    for (i in seq_len(nrow(targets))) {
      if (targets$rtmax[i] == 0) {
        targets$rtmax[i] <- max(fts$rtmax)
      }
      if (targets$mzmax[i] == 0) {
        targets$mzmax[i] <- max(fts$mzmax)
      }
      if ("mobility" %in% colnames(fts)) {
        if (targets$mobilitymax[i] == 0) {
          targets$mobilitymax[i] <- max(fts$mobility)
        }
      }
    }
    sel <- rep(FALSE, nrow(fts))
    ids <- rep(NA_character_, nrow(fts))
    for (i in seq_len(nrow(targets))) {
      if ("mobility" %in% colnames(fts)) {
        sel[
          fts$analysis == targets$analysis[i] &
            fts$polarity == targets$polarity[i] &
            data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
            data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
            data.table::between(
              fts$mobility,
              targets$mobilitymin[i],
              targets$mobilitymax[i]
            )
        ] <- TRUE
        ids[
          fts$analysis == targets$analysis[i] &
            fts$polarity == targets$polarity[i] &
            data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
            data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
            data.table::between(
              fts$mobility,
              targets$mobilitymin[i],
              targets$mobilitymax[i]
            )
        ] <- targets$id[i]
      } else {
        sel[
          fts$analysis == targets$analysis[i] &
            fts$polarity == targets$polarity[i] &
            data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
            data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i])
        ] <- TRUE
        ids[
          fts$analysis == targets$analysis[i] &
            fts$polarity == targets$polarity[i] &
            data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
            data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i])
        ] <- targets$id[i]
      }
    }
    fts$name <- ids
    rpls <- x$info$replicate
    names(rpls) <- x$info$analysis
    fts$replicate <- rpls[fts$analysis]
    return(fts[sel])
  }
  fts$replicate <- x$info$replicate[fts$analysis]
  fts
}

# MARK: map_features
#' @describeIn MassSpecResults_NonTargetAnalysis Maps features from the MassSpecResults_NonTargetAnalysis object.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-neutral_mass
#' @template arg-ms-filtered
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-showLegend
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
map_features.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  colorBy = "replicates+targets",
  showLegend = TRUE,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  fts <- .make_colorBy_varkey(fts, colorBy, legendNames)

  cl <- .get_colors(unique(fts$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- "Retention time / seconds"
    }
    if (is.null(yLab)) {
      if (neutral_mass) {
        yLab <- expression(italic("m/z ") / " Da")
      } else {
        yLab <- "Mass / Da"
      }
    }

    if (neutral_mass) {
      fts$mzmin <- fts$mass - (fts$mz - fts$mzmin)
      fts$mzmax <- fts$mass + (fts$mzmax - fts$mz)
      fts$mz <- fts$mass
    }

    ggplot2::ggplot(fts, aes(x = rt, y = mz)) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = rtmin,
          xmax = rtmax,
          ymin = mzmin,
          ymax = mzmax,
          fill = factor(var)
        ),
        alpha = 0.7
      ) +
      ggplot2::geom_point(ggplot2::aes(color = factor(var)), size = 2) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_fill_manual(values = cl, guide = FALSE) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    if (is.null(xLab)) {
      xLab <- "Retention time / seconds"
    }
    if (is.null(yLab)) {
      if (neutral_mass) {
        yLab <- "<i>m/z</i> / Da"
      } else {
        yLab <- "Mass / Da"
      }
    }

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

    plotlegend <- rep(TRUE, length(cl))

    names(plotlegend) <- names(cl)

    plot <- plot_ly()

    for (i in seq_len(nrow(fts))) {
      ft <- fts[i, ]

      if (neutral_mass) {
        ft$mzmin <- ft$mass - (ft$mz - ft$mzmin)
        ft$mzmax <- ft$mass + (ft$mzmax - ft$mz)
        ft$mz <- ft$mass
      }

      x0 <- ft$rtmin
      x1 <- ft$rtmax
      y0 <- ft$mzmin
      y1 <- ft$mzmax
      ft_var <- ft$var

      plot <- plot %>%
        add_trace(
          x = c(x0, x1, x1, x0, x0),
          y = c(y0, y0, y1, y1, y0),
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = paste0(cl[ft_var], "70"),
          line = list(color = cl[ft_var]),
          opacity = 0.2,
          name = ft_var,
          legendgroup = ft_var,
          showlegend = FALSE
        )
    }

    for (i in seq_len(nrow(fts))) {
      ft <- fts[i, ]

      hT <- .make_features_hover_string(ft)

      if (neutral_mass) {
        ft$mz <- ft$mass
      }

      plot <- plot %>%
        add_trace(
          x = ft$rt,
          y = ft$mz,
          type = "scatter",
          mode = "markers",
          marker = list(size = 8, color = cl[ft$var]),
          name = ft$var,
          legendgroup = ft$var,
          showlegend = plotlegend[ft$var],
          text = hT,
          hoverinfo = "text"
        )

      if (isTRUE(plotlegend[ft$var])) {
        plotlegend[ft$var] <- FALSE
      }
    }

    if (showLegend) {
      plot <- plot %>%
        plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    } else {
      plot <- plot %>%
        plotly::layout(
          legend = NULL,
          xaxis = xaxis,
          yaxis = yaxis,
          title = title
        )
    }

    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }

    plot
  }
}

# MARK: map_features_intensity
#' @describeIn MassSpecResults_NonTargetAnalysis Maps features intensity from the MassSpecResults_NonTargetAnalysis object.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @template arg-ms-correctIntensity
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-renderEngine
#' @export
#'
map_features_intensity.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  correctIntensity = FALSE,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "replicates+targets",
  renderEngine = "webgl"
) {
  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  if (correctIntensity) {
    if ("correction" %in% colnames(fts)) {
      fts$intensity <- fts$intensity * fts$correction
    }
  }

  fts <- .make_colorBy_varkey(fts, colorBy, legendNames)

  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  if (is.null(yLab)) {
    yLab <- "Intensity (Counts)"
  }

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

  hT <- .make_features_hover_string(fts)

  plot <- plotly::plot_ly(
    data = fts,
    x = ~rt,
    y = ~intensity,
    color = ~var,
    type = "scatter",
    mode = "markers",
    colors = .get_colors(unique(fts$var)),
    text = ~hT,
    hoverinfo = "text"
  ) %>%
    plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)

  if (renderEngine %in% "webgl") {
    plot <- plot %>% plotly::toWebGL()
  }

  plot
}

# MARK: get_features_eic
#' @describeIn MassSpecResults_NonTargetAnalysis Returns a data table with the extracted ion chromatograms (EIC) for features in the specified analyses and targets.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @template arg-useLoadedData
#' @export
#'
get_features_eic.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  useLoadedData = TRUE
) {
  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  if (useLoadedData) {
    if (any(vapply(fts$eic, function(z) length(z) > 0, FALSE))) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }

  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_info <- x$info[x$info$analysis %in% names(fts_list), ]

    fts <- rcpp_ms_load_features_eic(
      analyses_names = ana_info$analysis,
      analyses_files = ana_info$file,
      headers = x$headers[names(fts_list)],
      features = fts_list,
      filtered = filtered,
      rtExpand = rtExpand,
      mzExpand = mzExpand,
      minTracesIntensity = 0
    )

    fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  } else {
    sel <- vapply(
      fts$eic,
      function(z) {
        if (length(z) == 0) {
          return(FALSE)
        }
        if (is.data.frame(z)) {
          if (nrow(z) == 0) {
            return(FALSE)
          }
        }
        TRUE
      },
      TRUE
    )
    fts_without_eic <- fts[!sel, ]
    fts_with_eic <- fts[sel, ]
    if (nrow(fts_without_eic) > 0) {
      fts_without_eic_ana_split_vector <- fts_without_eic$analysis
      fts_without_eic$analysis <- NULL
      fts_without_eic_list <- split(
        fts_without_eic,
        fts_without_eic_ana_split_vector
      )
      ana_info <- x$info[
        x$info$analysis %in% names(fts_without_eic_list),
      ]
      fts_without_eic <- rcpp_ms_load_features_eic(
        analyses_names = ana_info$analysis,
        analyses_files = ana_info$file,
        headers = x$headers[names(fts_without_eic_list)],
        features = fts_without_eic_list,
        filtered = filtered,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        minTracesIntensity = 0
      )
      fts_without_eic <- data.table::rbindlist(
        fts_without_eic,
        idcol = "analysis",
        fill = TRUE
      )
      fts <- data.table::rbindlist(
        list(fts_without_eic, fts_with_eic),
        fill = TRUE
      )
    }
  }
  eic_list <- lapply(
    seq_len(nrow(fts)),
    function(z, fts) {
      temp <- fts[z, ]
      temp_ms <- temp[["eic"]][[1]]
      if (is.null(temp_ms)) {
        return(data.table::data.table())
      }
      if (!is.data.frame(temp_ms)) {
        temp_ms <- data.table::as.data.table(temp_ms)
      }
      temp_ms$analysis <- temp$analysis
      temp_ms$feature <- temp$feature
      temp_ms
    },
    fts = fts
  )
  eic <- data.table::rbindlist(eic_list, fill = TRUE)
  rpls <- x$info$replicate
  names(rpls) <- x$info$analysis
  eic$replicate <- rpls[eic$analysis]
  data.table::setcolorder(eic, c("analysis", "replicate", "feature"))
  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_eic_id <- paste0(eic$analysis, "-", eic$feature)
  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    eic$group <- fgs[unique_eic_id]
    data.table::setcolorder(eic, c("analysis", "replicate", "group"))
  }
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    eic$name <- tar_ids[unique_eic_id]
    data.table::setcolorder(eic, c("analysis", "replicate", "name"))
  }
  eic
}

# MARK: plot_features
#' @describeIn MassSpecResults_NonTargetAnalysis Plots features from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_features.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  interactive = TRUE,
  renderEngine = "webgl"
) {
  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  eic <- get_features_eic(
    x,
    analyses = unique(fts$analysis),
    features = fts,
    rtExpand = rtExpand,
    mzExpand = mzExpand,
    filtered = filtered,
    useLoadedData = useLoadedData
  )

  if (nrow(eic) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }

  intensity <- NULL
  cols_by <- c("analysis", "replicate", "polarity", "feature", "rt")
  eic <- eic[, `:=`(intensity = max(intensity)), by = cols_by]
  eic <- unique(eic)

  fts <- .make_colorBy_varkey(fts, colorBy, legendNames)

  cl <- .get_colors(unique(fts$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)

  if (!interactive) {
    plot <- ggplot2::ggplot(eic, ggplot2::aes(x = rt))

    for (i in seq_len(nrow(fts))) {
      ft_analysis <- fts[["analysis"]][i]
      ft_replicate <- fts[["replicate"]][i]
      ft_id <- fts[["feature"]][i]
      ft_var <- fts[["var"]][i]
      ft_min <- fts[["min"]][i]
      ft_max <- fts[["max"]][i]

      temp <- dplyr::filter(
        eic,
        analysis %in%
          ft_analysis &
          replicate %in% ft_replicate &
          feature %in% ft_id
      )

      temp$var <- ft_var

      plot <- plot +
        ggplot2::geom_line(
          data = temp,
          ggplot2::aes(y = intensity, color = var)
        )

      temp <- temp[temp$rt >= ft_min & temp$rt <= ft_max, ]

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

    plot <- plot_ly()

    for (i in seq_len(nrow(fts))) {
      pk <- fts[i, ]

      ft_var <- pk$var

      hT <- .make_features_hover_string(pk)

      temp <- dplyr::filter(
        eic,
        analysis %in%
          pk$analysis &
          replicate %in% pk$replicate &
          feature %in% pk$feature &
          rt >= pk$rtmin &
          rt <= pk$rtmax
      )

      plot <- plot %>%
        add_trace(
          data = temp,
          x = ~rt,
          y = ~intensity,
          type = "scatter",
          mode = "markers",
          marker = list(color = cl[ft_var], size = 5),
          text = paste(hT, "<br>x: ", temp$rt, "<br>y: ", temp$intensity),
          hoverinfo = "text",
          name = ft_var,
          legendgroup = ft_var,
          showlegend = FALSE
        )

      plot <- plot %>%
        plotly::add_ribbons(
          data = temp,
          x = ~rt,
          ymin = ~ min(intensity),
          ymax = ~intensity,
          line = list(color = cl[ft_var], width = 1.5),
          fillcolor = cl50[ft_var],
          text = paste(hT, "<br>x: ", temp$rt, "<br>y: ", temp$intensity),
          hoverinfo = "text",
          name = ft_var,
          legendgroup = ft_var,
          showlegend = show_legend[ft_var]
        )

      show_legend[ft_var] <- FALSE
    }

    for (i in seq_len(nrow(fts))) {
      pk <- fts[i, ]
      ft_var <- pk$var

      plot <- plot %>%
        add_trace(
          data = dplyr::filter(
            eic,
            analysis %in%
              pk$analysis &
              replicate %in% pk$replicate &
              feature %in% pk$feature
          ),
          x = ~rt,
          y = ~intensity,
          type = "scatter",
          mode = "lines",
          line = list(color = cl[ft_var], width = 0.5),
          name = ft_var,
          legendgroup = ft_var,
          showlegend = FALSE
        )
    }

    plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)

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

# MARK: get_features_ms1
#' @describeIn MassSpecResults_NonTargetAnalysis Returns a data table with MS1 features for the specified analyses and targets.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-normalized
#' @template arg-ms-filtered
#' @template arg-useLoadedData
#' @export
#'
get_features_ms1.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  normalized = TRUE,
  filtered = FALSE,
  useLoadedData = TRUE
) {
  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  if (useLoadedData) {
    if (any(vapply(fts$ms1, function(z) length(z) > 0, FALSE))) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }

  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_info <- x$info[x$info$analysis %in% names(fts_list), ]

    fts <- rcpp_ms_load_features_ms1(
      analyses_names = ana_info$analysis,
      analyses_files = ana_info$file,
      headers = x$headers[names(fts_list)],
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

  ms1_list <- lapply(
    seq_len(nrow(fts)),
    function(x, fts) {
      temp <- fts[x, ]
      temp_ms <- temp[["ms1"]][[1]]
      if (is.null(temp_ms)) {
        return(data.table::data.table())
      }
      if (!"mz" %in% colnames(temp_ms)) {
        return(data.table::data.table())
      }
      temp_ms$analysis <- temp$analysis
      temp_ms$feature <- temp$feature
      temp_ms
    },
    fts = fts
  )

  if (normalized) {
    ms1_list <- lapply(ms1_list, function(z) {
      if (!is.null(z)) {
        if (nrow(z) > 0) {
          max_intensity <- max(z$intensity)
          if (max_intensity > 0) {
            z$intensity <- z$intensity / max_intensity
          }
        }
      }
      z
    })
  }

  ms1 <- data.table::rbindlist(ms1_list, fill = TRUE)

  if (nrow(ms1) == 0) {
    return(data.table::data.table())
  }

  ms1$replicate <- x$info$replicate[ms1$analysis]
  data.table::setcolorder(ms1, c("analysis", "replicate", "feature"))

  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_ms1_id <- paste0(ms1$analysis, "-", ms1$feature)

  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms1$group <- fgs[unique_ms1_id]
    data.table::setcolorder(ms1, c("analysis", "replicate", "group"))
  }

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms1$name <- tar_ids[unique_ms1_id]
    data.table::setcolorder(ms1, c("analysis", "replicate", "name"))
  }

  ms1
}

# MARK: plot_features_ms1
#' @describeIn MassSpecResults_NonTargetAnalysis Plots MS1 features from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-normalized
#' @template arg-ms-filtered
#' @template arg-useLoadedData
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-showText
#' @template arg-interactive
#' @export
#'
plot_features_ms1.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  normalized = TRUE,
  filtered = FALSE,
  useLoadedData = TRUE,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "targets",
  showText = FALSE,
  interactive = TRUE
) {
  ms1 <- get_features_ms1(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    rtWindow,
    mzWindow,
    mzClust,
    presence,
    minIntensity,
    normalized,
    filtered,
    useLoadedData
  )

  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }

  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)

  ms1$loop <- paste0(ms1$analysis, ms1$replicate, ms1$id, ms1$var)

  cl <- .get_colors(unique(ms1$var))

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- expression(italic("m/z ") / " Da")
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    plot <- ggplot2::ggplot(
      ms1,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          xend = mz,
          yend = 0,
          color = var
        ),
        linewidth = 1
      )

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = round(mz, 4)),
          vjust = 0.2,
          hjust = -0.2,
          angle = 90,
          size = 2,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(ms1$intensity) * 1.5)
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)

    plot
  } else {
    if (is.null(xLab)) {
      xLab <- "<i>m/z</i> / Da"
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms1$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
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
        text = ~ paste0(round(mz, digits = 4), "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>%
      plotly::layout(
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

# MARK: get_features_ms2
#' @describeIn MassSpecResults_NonTargetAnalysis Returns a data table with MS2 features for the specified analyses and targets.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-normalized
#' @template arg-ms-filtered
#' @template arg-useLoadedData
#' @export
#'
get_features_ms2.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  normalized = TRUE,
  filtered = FALSE,
  useLoadedData = TRUE
) {
  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  if (useLoadedData) {
    if (any(vapply(fts$ms2, function(z) length(z) > 0, FALSE))) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }

  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_info <- x$info[x$info$analysis %in% names(fts_list), ]

    fts <- rcpp_ms_load_features_ms2(
      analyses_names = ana_info$analysis,
      analyses_files = ana_info$file,
      headers = x$headers[names(fts_list)],
      features = fts_list,
      filtered = filtered,
      minTracesIntensity = minIntensity,
      isolationWindow = isolationWindow,
      mzClust = mzClust,
      presence = presence
    )

    fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  }

  ms2_list <- lapply(
    seq_len(nrow(fts)),
    function(x, fts) {
      temp <- fts[x, ]
      temp_ms <- temp[["ms2"]][[1]]
      if (is.null(temp_ms)) {
        return(data.table::data.table())
      }
      if (!"mz" %in% colnames(temp_ms)) {
        return(data.table::data.table())
      }
      temp_ms$analysis <- temp$analysis
      temp_ms$feature <- temp$feature
      temp_ms
    },
    fts = fts
  )

  if (normalized) {
    ms2_list <- lapply(ms2_list, function(z) {
      if (!is.null(z)) {
        if (nrow(z) > 0) {
          max_intensity <- max(z$intensity)
          if (max_intensity > 0) {
            z$intensity <- z$intensity / max_intensity
          }
        }
      }
      z
    })
  }

  ms2 <- data.table::rbindlist(ms2_list, fill = TRUE)

  if (nrow(ms2) == 0) {
    return(data.table::data.table())
  }

  ms2$replicate <- x$info$replicate[ms2$analysis]
  data.table::setcolorder(ms2, c("analysis", "replicate", "feature"))

  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_ms2_id <- paste0(ms2$analysis, "-", ms2$feature)

  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms2$group <- fgs[unique_ms2_id]
    data.table::setcolorder(ms2, c("analysis", "replicate", "group"))
  }

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms2$name <- tar_ids[unique_ms2_id]
    data.table::setcolorder(ms2, c("analysis", "replicate", "name"))
  }

  ms2
}

# MARK: plot_features_ms2
#' @describeIn MassSpecResults_NonTargetAnalysis Plots MS2 features from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-normalized
#' @template arg-ms-filtered
#' @template arg-useLoadedData
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-showText
#' @template arg-interactive
#' @export
#'
plot_features_ms2.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  normalized = TRUE,
  filtered = FALSE,
  useLoadedData = TRUE,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "targets",
  showText = TRUE,
  interactive = TRUE
) {
  ms2 <- get_features_ms2(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    isolationWindow,
    mzClust,
    presence,
    minIntensity,
    normalized,
    filtered,
    useLoadedData
  )

  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }

  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)

  ms2$loop <- paste0(ms2$analysis, ms2$replicate, ms2$id, ms2$var)

  cl <- .get_colors(unique(ms2$var))

  if (showText) {
    ms2$text_string <- paste0(round(ms2$mz, 4))
    ms2$text_string[ms2$is_pre] <- paste0("Pre ", ms2$text_string[ms2$is_pre])
  } else {
    ms2$text_string <- ""
  }

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- expression(italic("m/z ") / " Da")
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ms2$linesize <- 1
    ms2$linesize[ms2$is_pre] <- 2

    plot <- ggplot2::ggplot(
      ms2,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(ggplot2::aes(
        xend = mz,
        yend = 0,
        color = var,
        linewidth = linesize
      ))

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = text_string),
          vjust = 0.2,
          hjust = -0.2,
          angle = 90,
          size = 2,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(ms2$intensity) * 1.5)
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_linewidth_continuous(range = c(1, 2), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)

    plot
  } else {
    if (is.null(xLab)) {
      xLab <- "<i>m/z</i> / Da"
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ms2$linesize <- 0.01
    ms2$linesize[ms2$is_pre] <- 2

    ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms2$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms2$intensity) * 1.5)
    )

    loop <- NULL

    plot <- ms2 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = ~linesize)),
        text = ~ paste0(text_string, "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>%
      plotly::layout(
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

# MARK: get_groups
#' @describeIn MassSpecResults_NonTargetAnalysis Returns a data table with group information for the specified analyses and targets.
#' @template arg-nts-x
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @template arg-ms-intensities
#' @template arg-ms-average
#' @template arg-ms-sdValues
#' @template arg-ms-metadata
#' @template arg-ms-correctIntensity
#' @export
#'
get_groups.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  correctIntensity = FALSE
) {
  if (length(x$features) == 0) {
    return(data.table::data.table())
  }
  if (!any(vapply(x$features, function(z) any(!(is.na(z$group) | z$group %in% "")), FALSE))) {
    return(data.table::data.table())
  }
  fts <- get_features(
    x,
    analyses = NULL,
    features = groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered = filtered
  )

  if (correctIntensity) {
    if ("correction" %in% colnames(fts)) {
      fts$intensity <- fts$intensity * fts$correction
    }
  }

  if (nrow(fts) > 0) {
    g_ids <- unique(fts$group)
    fgroups <- data.table::data.table("group" = g_ids)

    if (intensities) {
      if (average) {
        intensity <- NULL
        rpls <- x$info$replicate
        names(rpls) <- x$info$analysis
        fts_temp <- data.table::copy(fts)
        fts_temp$analysis <- rpls[fts_temp$analysis]
        fts_av <- fts_temp[,
          .(
            intensity = mean(intensity),
            sd = sd(intensity),
            n = length(intensity)
          ),
          by = c("group", "analysis")
        ]
        fts_av$sd[is.na(fts_av$sd)] <- 0
        fts_av$sd <- round(fts_av$sd / fts_av$intensity * 100, digits = 0)
        fts_sd <- data.table::copy(fts_av)
        fts_n <- data.table::copy(fts_av)
        fts_sd$intensity <- NULL
        fts_sd$n <- NULL
        fts_sd$analysis <- paste(fts_sd$analysis, "_sd", sep = "")
        fts_sd <- data.table::dcast(
          fts_sd[, c("group", "analysis", "sd"), with = TRUE],
          group ~ analysis,
          value.var = "sd"
        )
        fts_sd[is.na(fts_sd)] <- 0
        #tbl_rpls <- table(rpls)
        #fts_n$n <- tbl_rpls[fts_n$analysis]
        fts_n$intensity <- NULL
        fts_n$sd <- NULL
        fts_n$analysis <- paste(fts_n$analysis, "_n", sep = "")
        fts_n <- data.table::dcast(
          fts_n[, c("group", "analysis", "n"), with = TRUE],
          group ~ analysis,
          value.var = "n"
        )
        fts_n[is.na(fts_n)] <- 0
        fts_av$sd <- NULL
        fts_av$n <- NULL
        fts_av <- data.table::dcast(
          fts_av,
          group ~ analysis,
          value.var = "intensity"
        )
        fts_av[is.na(fts_av)] <- 0
      } else {
        fts_av <- fts[,
          .(intensity = max(intensity)),
          by = c("group", "analysis")
        ]
        fts_av <- data.table::dcast(
          fts_av,
          group ~ analysis,
          value.var = "intensity"
        )
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
      rtmin <- NULL
      rtmax <- NULL
      mzmin <- NULL
      mzmax <- NULL
      feature <- NULL
      quality <- NULL
      annotation <- NULL
      istd <- NULL

      max_presence <- nrow(x$info)

      fts_meta <- fts[,
        .(
          rt = round(mean(rt), digits = 0),
          mass = round(mean(mass), digits = 4),
          rtdev = round(max(rtmax - rtmin), digits = 0),
          massdev = round(max(mzmax - mzmin), digits = 4),
          presence = round(length(feature) / max_presence * 100, digits = 1),
          maxint = round(max(intensity), digits = 0),
          sn = round(
            max(
              vapply(
                quality,
                function(z) {
                  if (length(z) > 0) {
                    z$sn
                  } else {
                    0
                  }
                },
                0
              ),
              na.rm = TRUE
            ),
            digits = 1
          ),
          gauss_f = round(
            max(
              vapply(
                quality,
                function(z) {
                  if (length(z) > 0) {
                    z$gauss_f
                  } else {
                    0
                  }
                },
                0
              ),
              na.rm = TRUE
            ),
            digits = 4
          ),
          iso = min(
            vapply(
              annotation,
              function(z) {
                if (length(z) > 0) {
                  z$iso_step
                } else {
                  0
                }
              },
              0
            ),
            na.rm = TRUE
          ),
          istd = paste0(
            unique(
              vapply(
                istd,
                function(z) {
                  if (length(z) > 0) {
                    z$name
                  } else {
                    NA_character_
                  }
                },
                NA_character_
              )
            ),
            collapse = "; "
          ),
          filtered = all(filtered)
        ),
        by = "group"
      ]

      fgroups <- fgroups[fts_meta, on = "group"]
    }

    if (intensities) {
      fgroups <- fgroups[fts_av, on = "group"]
    }

    if (average && sdValues) {
      fgroups <- fgroups[fts_sd, on = "group"]
      fgroups <- fgroups[fts_n, on = "group"]
    }

    fgroups
  } else {
    data.table::data.table()
  }
}

# MARK: plot_groups
#' @describeIn MassSpecResults_NonTargetAnalysis Plots groups from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_groups.MassSpecResults_NonTargetAnalysis <- function(
  x,
  analyses = NULL,
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
  interactive = TRUE,
  renderEngine = "webgl"
) {
  fts <- get_features(
    x,
    analyses,
    groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (grepl("targets", colorBy) & !isTRUE(legendNames)) {
    fts$name <- fts$group
    if (is.null(legendNames)) {
      legendNames <- TRUE
    }
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
    interactive = interactive,
    renderEngine = renderEngine
  )
}

# MARK: plot_groups_overview
#' @describeIn MassSpecResults_NonTargetAnalysis Plots an overview of groups from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @template arg-legendNames
#' @template arg-title
#' @template arg-nts-heights
#' @template arg-renderEngine
#' @export
#'
plot_groups_overview.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  correctIntensity = TRUE,
  filtered = FALSE,
  legendNames = NULL,
  title = NULL,
  heights = c(0.35, 0.5, 0.15),
  renderEngine = "webgl"
) {
  fts <- get_features(
    x,
    analyses,
    groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  eic <- get_features_eic(
    x,
    analyses = unique(fts$analysis),
    features = fts,
    rtExpand = rtExpand,
    mzExpand = mzExpand,
    filtered = filtered,
    useLoadedData = useLoadedData
  )

  intensity <- NULL
  cols_by <- c("analysis", "polarity", "feature", "rt")
  eic <- eic[, `:=`(intensity = max(intensity)), by = cols_by]
  eic <- unique(eic)

  if (nrow(eic) == 0) {
    message("\U2717 Traces and/or features not found for targets!")
    return(NULL)
  }

  if (
    is.character(legendNames) &
      length(legendNames) == length(unique(fts$group))
  ) {
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

  leg <- unique(eic$var)
  colors <- .get_colors(leg)
  showleg <- rep(TRUE, length(leg))
  names(showleg) <- names(leg)
  plot <- plot_ly()

  for (g in leg) {
    uid <- unique(eic$uid[eic$var == g])

    for (u in uid) {
      ft <- fts[fts$uid == u, ]
      if (nrow(ft) == 0) {
        next
      }
      df <- eic[eic$uid == u, ]

      plot <- plot %>%
        add_trace(
          df,
          x = df$rt,
          y = df$intensity,
          type = "scatter",
          mode = "lines",
          line = list(width = 0.5, color = colors[g]),
          connectgaps = TRUE,
          name = g,
          legendgroup = g,
          showlegend = FALSE
        )

      df <- df[df$rt >= ft$rtmin & df$rt <= ft$rtmax, ]
      df$mz <- as.numeric(df$mz)

      plot <- plot %>%
        add_trace(
          df,
          x = df$rt,
          y = df$intensity,
          type = "scatter",
          mode = "lines+markers",
          fill = "tozeroy",
          connectgaps = TRUE,
          fillcolor = paste(color = colors[g], 50, sep = ""),
          line = list(width = 0.1, color = colors[g]),
          marker = list(size = 3, color = colors[g]),
          name = g,
          legendgroup = g,
          showlegend = showleg[which(leg %in% g)],
          hoverinfo = "text",
          hoverlabel = list(bgcolor = colors[g]),
          text = paste(
            if ("var" %in% colnames(ft)) {
              paste("</br> ", ft$var)
            } else {
              ""
            },
            # "</br> name: ", g,
            "</br> group: ",
            ft$group,
            "</br> feature: ",
            ft$feature,
            "</br> analysis: ",
            ft$analysis,
            "</br> replicate: ",
            ft$replicate,
            "</br> <i>m/z</i>: ",
            round(ft$mz, digits = 4),
            "</br> rt: ",
            round(df$rt, digits = 0),
            "</br> intensity: ",
            round(df$intensity, digits = 0)
          )
        )
      showleg[which(leg %in% g)] <- FALSE
    }
  }

  plot2 <- plot_ly()

  for (g in leg) {
    ft2 <- fts[fts$var == g, ]
    if (!"filled" %in% colnames(ft2)) {
      ft2$filled <- FALSE
    }

    ft_nf <- ft2[!ft2$filled, ]

    if (nrow(ft_nf) > 0) {
      hT <- .make_features_hover_string(ft_nf)

      plot2 <- plot2 %>%
        add_trace(
          x = ft_nf$rt,
          y = ft_nf$analysis,
          type = "scatter",
          mode = "markers",
          marker = list(
            line = list(color = colors[g], width = 3),
            color = "#000000",
            size = 10
          ),
          error_x = list(
            type = "data",
            symmetric = FALSE,
            arrayminus = ft_nf$rt - ft_nf$rtmin,
            array = ft_nf$rtmax - ft_nf$rt,
            color = colors[g],
            width = 5
          ),
          name = g,
          legendgroup = g,
          showlegend = FALSE,
          hoverinfo = "text",
          hoverlabel = list(bgcolor = colors[g]),
          text = hT
        )
    }

    ft_f <- ft2[ft2$filled, ]

    if (nrow(ft_f) > 0) {
      hT <- .make_features_hover_string(ft_f)

      plot2 <- plot2 %>%
        add_trace(
          x = ft_f$rt,
          y = ft_f$analysis,
          type = "scatter",
          mode = "markers",
          marker = list(
            line = list(color = colors[g], width = 3),
            color = "#f8f8f8",
            size = 10
          ),
          error_x = list(
            type = "data",
            symmetric = FALSE,
            arrayminus = ft_f$rt - ft_f$rtmin,
            array = ft_f$rtmax - ft_f$rt,
            color = colors[g],
            width = 5
          ),
          name = g,
          legendgroup = g,
          showlegend = FALSE,
          hoverinfo = "text",
          hoverlabel = list(bgcolor = colors[g]),
          text = hT
        )
    }
  }
  plot2 <- hide_colorbar(plot2)

  plot3 <- plot_ly(fts, x = sort(unique(fts$analysis)))

  for (g in leg) {
    df_3 <- fts[fts$var == g, ]

    if (correctIntensity) {
      if ("correction" %in% colnames(df_3)) {
        df_3$intensity <- df_3$intensity * df_3$correction
      }
    }

    if (!all(analyses %in% df_3$analysis)) {
      extra <- data.frame(
        "analysis" = analyses[!analyses %in% df_3$analysis],
        "var" = g,
        "intensity" = 0
      )
      df_3 <- rbind(df_3[, c("analysis", "var", "intensity")], extra)
    }

    df_3 <- df_3[order(df_3$analysis), ]

    plot3 <- plot3 %>%
      add_trace(
        df_3,
        x = df_3$analysis,
        y = df_3$intensity / max(df_3$intensity),
        type = "scatter",
        mode = "lines",
        line = list(width = 1, color = colors[g]),
        connectgaps = FALSE,
        name = g,
        legendgroup = g,
        showlegend = FALSE
      )
  }

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Retention time / seconds",
    titlefont = list(size = 12, color = "black"),
    range = c(min(eic$rt), max(eic$rt)),
    autotick = TRUE,
    ticks = "outside"
  )

  yaxis1 <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )

  yaxis2 <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "",
    titlefont = list(size = 12, color = "black"),
    tick0 = 0,
    dtick = 1
  )

  xaxis3 <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = NULL
  )

  yaxis3 <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Normalized intensity",
    titlefont = list(size = 12, color = "black"),
    tick0 = 0,
    dtick = 0.25,
    range = c(0, 1.5)
  )

  plotList <- list()

  plot <- plot %>% plotly::layout(xaxis = xaxis3, yaxis = yaxis1)
  plotList[["plot"]] <- plot

  plot2 <- plot2 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis2)
  plotList[["plot2"]] <- plot2

  plot3 <- plot3 %>% plotly::layout(xaxis = xaxis3, yaxis = yaxis3)

  plotf <- subplot(
    plotList,
    nrows = 2,
    titleY = TRUE,
    titleX = TRUE,
    heights = heights[1:2],
    margin = 0.01,
    shareX = TRUE,
    which_layout = "merge"
  )

  plotf_2 <- subplot(
    list(plotf, plot3),
    nrows = 2,
    titleY = TRUE,
    titleX = TRUE,
    heights = c(sum(heights[1:2]), heights[3]),
    margin = 0.01,
    shareX = FALSE,
    which_layout = "merge"
  )

  # if (renderEngine == "webgl") {
  #   #plotf_2 <- plotly::config(plotf_2, displayModeBar = TRUE)
  # }
  #
  # if (renderEngine %in% "webgl") {
  #   # Fix for warnings with hoveron when webgl is used
  #   plotf_2$x$attrs <- lapply(plotf_2$x$attrs, function(x) {
  #     if (!is.null(x[["hoveron"]])) {
  #       x[["hoveron"]] <- NULL
  #     }
  #     x
  #   })
  #
  #   plotf_2 <- plotf_2 %>% plotly::toWebGL()
  # }

  plotf_2
}

# MARK: plot_groups_profile
#' @describeIn MassSpecResults_NonTargetAnalysis Plots the profile of groups from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @template arg-ms-correctIntensity
#' @template arg-averaged
#' @template arg-normalized
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-showLegend
#' @template arg-renderEngine
#' @export
#'
plot_groups_profile.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  correctIntensity = TRUE,
  averaged = FALSE,
  normalized = TRUE,
  legendNames = NULL,
  yLab = NULL,
  title = NULL,
  showLegend = TRUE,
  renderEngine = "webgl"
) {
  fts <- get_features(
    x,
    analyses,
    groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  analyses <- .check_analyses_argument(x, analyses)

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  polarities <- vapply(
    x$headers[analyses],
    function(a) {
      paste0(unique(a$polarity), collapse = ", ")
    },
    NA_character_
  )

  if (!"polarity" %in% colnames(fts)) {
    fts$polarity <- polarities[fts$analysis]
  }

  if (correctIntensity) {
    if ("correction" %in% colnames(fts)) {
      fts$intensity <- fts$intensity * fts$correction
    }
  }

  if (normalized && "intensity_rel" %in% colnames(fts)) {
    fts$intensity <- as.numeric(fts$intensity_rel)
  }

  if (
    averaged && any(vapply(x$features, function(x) any(!is.na(x$group)), FALSE))
  ) {
    group_cols <- c("replicate", "group", "polarity")
    if ("name" %in% colnames(fts)) {
      group_cols <- c(group_cols, "name")
    }
    intensity <- NULL
    fts <- fts[,
      .(intensity = mean(intensity), intensity_sd = sd(intensity)),
      by = group_cols
    ]
    names(polarities) <- x$info$replicate[names(polarities)]
    polarities <- polarities[!duplicated(names(polarities))]
    data.table::setnames(fts, "replicate", "analysis")
    analyses <- unique(x$info$replicate[analyses])
  }

  if (
    is.character(legendNames) &
      length(legendNames) == length(unique(fts$group))
  ) {
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

  if (showLegend) {
    showLeg <- rep(TRUE, length(u_leg))
  } else {
    showLeg <- rep(FALSE, length(u_leg))
  }

  names(showLeg) <- u_leg

  rpls <- x$info$replicate

  plot <- plot_ly(fts, x = sort(unique(fts$analysis)))

  for (g in u_leg) {
    df <- fts[fts$var == g, ]

    if (!all(analyses %in% df$analysis)) {
      extra <- data.frame(
        "analysis" = analyses[!analyses %in% df$analysis],
        "polarity" = polarities[
          !names(polarities) %in% df$analysis &
            names(polarities) %in% analyses
        ],
        "var" = g,
        "intensity" = 0
      )

      if (averaged && any(!is.na(df$group))) {
        extra$intensity_sd <- 0
        df <- rbind(
          df[, c("analysis", "var", "intensity", "intensity_sd", "polarity")],
          extra
        )
      } else {
        df <- rbind(df[, c("analysis", "var", "intensity", "polarity")], extra)
      }
    }

    df <- df[order(df$analysis), ]

    if (normalized) {
      if (length(unique(df$polarity)) > 1) {
        for (p in unique(df$polarity)) {
          max_int <- max(df$intensity[df$polarity == p])
          if (max_int > 0) {
            df$intensity[df$polarity == p] <- df$intensity[df$polarity == p] /
              max_int
            if (averaged && any(!is.na(df$group))) {
              df$intensity_sd[df$polarity == p] <- df$intensity_sd[
                df$polarity == p
              ] /
                max_int
            }
          }
        }
      } else {
        max_int <- max(df$intensity)
        if (max_int > 0) {
          df$intensity <- df$intensity / max_int
          if (averaged && any(!is.na(df$group))) {
            df$intensity_sd <- df$intensity_sd / max_int
          }
        }
      }
    }

    plot <- plot %>%
      add_trace(
        df,
        x = df$analysis,
        y = df$intensity,
        type = "scatter",
        mode = "lines",
        line = list(
          width = 0.5,
          color = colors[g],
          dash = "dash"
        ),
        connectgaps = FALSE,
        name = g,
        legendgroup = g,
        showlegend = FALSE
      )

    df$replicate <- rpls[df$analysis]

    for (r in unique(df$replicate)) {
      df_r <- df[df$replicate %in% r, ]

      if (averaged && any(!is.na(df_r$group))) {
        plot <- plot %>%
          add_trace(
            df_r,
            x = df_r$analysis,
            y = df_r$intensity,
            error_y = list(
              type = "data",
              array = df_r$intensity_sd,
              visible = TRUE,
              color = colors[g],
              width = 1
            ),
            type = "scatter",
            mode = "lines",
            line = list(width = 1.5, color = colors[g]),
            connectgaps = FALSE,
            name = g,
            legendgroup = g,
            showlegend = showLeg[g]
          )
      } else {
        plot <- plot %>%
          add_trace(
            df,
            x = df_r$analysis,
            y = df_r$intensity,
            type = "scatter",
            mode = "lines+markers",
            line = list(width = 1.5, color = colors[g]),
            marker = list(size = 5, color = colors[g]),
            connectgaps = FALSE,
            name = g,
            legendgroup = g,
            showlegend = showLeg[g]
          )
      }
      showLeg[g] <- FALSE
    }
  }

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = NULL
  )

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
    linecolor = toRGB("black"),
    linewidth = 2,
    title = yLab,
    titlefont = list(size = 12, color = "black"),
    range = range_yaxis
  )

  plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis)

  if (renderEngine %in% "webgl") {
    plot <- plot %>% plotly::toWebGL()
  }

  plot
}

# MARK: get_groups_ms1
#' @describeIn MassSpecResults_NonTargetAnalysis Extracts MS1 data for groups from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-mzClustFeatures
#' @template arg-ms-presenceFeatures
#' @template arg-ms-minIntensityFeatures
#' @template arg-useLoadedData
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-top
#' @template arg-normalized
#' @template arg-ms-groupBy
#' @template arg-ms-filtered
#' @export
#'
get_groups_ms1.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  top = 25,
  normalized = TRUE,
  groupBy = "groups",
  filtered = FALSE
) {
  fgs <- get_groups(
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
    intensities = FALSE,
    average = FALSE,
    sdValues = FALSE,
    metadata = FALSE
  )

  if (nrow(fgs) == 0) {
    return(data.table::data.table())
  }

  fts <- get_features(x, features = fgs$group)

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  ms1 <- get_features_ms1(
    x,
    analyses = unique(fts$analysis),
    features = fts$feature,
    rtWindow = rtWindow,
    mzWindow = mzWindow,
    mzClust = mzClustFeatures,
    presence = presenceFeatures,
    minIntensity = minIntensityFeatures,
    normalized = normalized,
    filtered = filtered,
    useLoadedData = useLoadedData
  )

  ms1 <- ms1[ms1$intensity > minIntensity, ]

  if (nrow(ms1) == 0) {
    return(data.table::data.table())
  }

  polarities <- unique(fts$polarity)
  polarities[polarities == 0] <- "unkown"
  polarities[polarities == 1] <- "positive"
  polarities[polarities == -1] <- "negative"

  multiple_polarities <- FALSE

  # TODO check for polarity switching with comma

  if (length(polarities) > 1) {
    multiple_polarities <- TRUE
  }

  if ("groups" %in% groupBy) {
    if (multiple_polarities) {
      ms1$unique_id <- paste0(ms1$group, "_", ms1$polarity)
      ms1$analysis <- NA_character_
    } else {
      ms1$unique_id <- ms1$group
      ms1$analysis <- NA_character_
    }
  } else {
    rpls <- x$info$replicate
    ms1$analysis <- rpls[ms1$analysis]

    if (multiple_polarities) {
      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group, "", ms1$polarity)
    } else {
      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group)
    }
  }

  ms1$id <- ms1$group

  ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, verbose = FALSE)

  ms1_list <- lapply(ms1_list, function(z) {
    if (!is.null(z)) {
      if (nrow(z) > 0) {
        z <- z[order(z$intensity, decreasing = TRUE), ]
        z <- z[seq_len(min(nrow(z), top)), ]
        z <- z[order(z$mz), ]
      }
    }
    z
  })

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

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- fts$group
    ms1_df$name <- tar_ids[ms1_df$group]
  }

  data.table::copy(ms1_df)
}

# MARK: get_groups_ms2
#' @describeIn MassSpecResults_NonTargetAnalysis Extracts MS2 data for groups from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClustFeatures
#' @template arg-ms-presenceFeatures
#' @template arg-ms-minIntensityFeatures
#' @template arg-useLoadedData
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-top
#' @template arg-normalized
#' @template arg-ms-groupBy
#' @template arg-ms-filtered
#' @export
#'
get_groups_ms2.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  top = 25,
  normalized = TRUE,
  groupBy = "groups",
  filtered = FALSE
) {
  fgs <- get_groups(
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
    intensities = FALSE,
    average = FALSE,
    sdValues = FALSE,
    metadata = FALSE
  )

  if (nrow(fgs) == 0) {
    return(data.table::data.table())
  }

  fts <- get_features(x, features = fgs$group)

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  ms2 <- get_features_ms2(
    x,
    analyses = unique(fts$analysis),
    features = fts$feature,
    isolationWindow = isolationWindow,
    mzClust = mzClustFeatures,
    presence = presenceFeatures,
    minIntensity = minIntensityFeatures,
    normalized = normalized,
    filtered = filtered,
    useLoadedData = useLoadedData
  )

  ms2 <- ms2[ms2$intensity > minIntensity, ]

  if (nrow(ms2) == 0) {
    return(data.table::data.table())
  }

  polarities <- unique(fts$polarity)
  polarities[polarities == 0] <- "unkown"
  polarities[polarities == 1] <- "positive"
  polarities[polarities == -1] <- "negative"

  multiple_polarities <- FALSE

  # TODO check for polarity switching with comma

  if (length(polarities) > 1) {
    multiple_polarities <- TRUE
  }

  if ("groups" %in% groupBy) {
    if (multiple_polarities) {
      ms2$unique_id <- paste0(ms2$group, "_", ms2$polarity)
      ms2$analysis <- NA_character_
    } else {
      ms2$unique_id <- ms2$group
      ms2$analysis <- NA_character_
    }
  } else {
    rpls <- x$info$replicate
    ms2$analysis <- rpls[ms2$analysis]

    if (multiple_polarities) {
      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group, "", ms2$polarity)
    } else {
      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group)
    }
  }

  ms2$id <- ms2$group

  ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, verbose = FALSE)

  ms2_list <- lapply(ms2_list, function(z) {
    if (!is.null(z)) {
      if (nrow(z) > 0) {
        z <- z[order(z$intensity, decreasing = TRUE), ]
        z <- z[seq_len(min(nrow(z), top)), ]
        z <- z[order(z$mz), ]
      }
    }
    z
  })

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

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- fts$group
    ms2_df$name <- tar_ids[ms2_df$group]
  }

  data.table::copy(ms2_df)
}

# MARK: plot_groups_ms1
#' @describeIn MassSpecResults_NonTargetAnalysis Plots MS1 traces for groups from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtWindow
#' @template arg-ms-mzWindow
#' @template arg-ms-mzClustFeatures
#' @template arg-ms-presenceFeatures
#' @template arg-ms-minIntensityFeatures
#' @template arg-useLoadedData
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-top
#' @template arg-normalized
#' @template arg-ms-groupBy
#' @template arg-ms-filtered
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-showText
#' @template arg-interactive
#' @export
#'
plot_groups_ms1.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  top = 25,
  normalized = TRUE,
  groupBy = "groups",
  filtered = FALSE,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "targets",
  showText = FALSE,
  interactive = TRUE
) {
  if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
    groupBy <- "groups"
  } else {
    groupBy <- "replicates"
  }

  ms1 <- get_groups_ms1(
    x,
    groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    rtWindow,
    mzWindow,
    mzClustFeatures,
    presenceFeatures,
    minIntensityFeatures,
    useLoadedData,
    mzClust,
    presence,
    minIntensity,
    top,
    normalized,
    groupBy,
    filtered
  )

  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }

  if ("analyses" %in% colorBy) {
    colorBy <- "replicates"
  }
  if (
    grepl("analyses", colorBy) &&
      grepl("targets", colorBy)
  ) {
    colorBy <- "replicates+targets"
  }

  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)

  ms1$loop <- paste0(ms1$analysis, ms1$replicate, ms1$id, ms1$var)

  cl <- .get_colors(unique(ms1$var))

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- expression(italic("m/z ") / " Da")
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    plot <- ggplot2::ggplot(
      ms1,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          xend = mz,
          yend = 0,
          color = var
        ),
        linewidth = 1
      )

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = round(mz, 4)),
          vjust = 0.2,
          hjust = -0.2,
          angle = 90,
          size = 2,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(ms1$intensity) * 1.5)
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)

    plot
  } else {
    if (is.null(xLab)) {
      xLab <- "<i>m/z</i> / Da"
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms1$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
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
        text = ~ paste0(round(mz, digits = 4), "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>%
      plotly::layout(
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

# MARK: plot_groups_ms2
#' @describeIn MassSpecResults_NonTargetAnalysis Plots MS2 traces for groups from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClustFeatures
#' @template arg-ms-presenceFeatures
#' @template arg-ms-minIntensityFeatures
#' @template arg-useLoadedData
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @template arg-ms-top
#' @template arg-normalized
#' @template arg-ms-groupBy
#' @template arg-ms-filtered
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-showText
#' @template arg-interactive
#' @export
#'
plot_groups_ms2.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  top = 25,
  normalized = TRUE,
  groupBy = "groups",
  filtered = FALSE,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "targets",
  showText = TRUE,
  interactive = TRUE
) {
  if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
    groupBy <- "groups"
  } else {
    groupBy <- "replicates"
  }

  ms2 <- get_groups_ms2(
    x,
    groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    isolationWindow,
    mzClustFeatures,
    presenceFeatures,
    minIntensityFeatures,
    useLoadedData,
    mzClust,
    presence,
    minIntensity,
    top,
    normalized,
    groupBy,
    filtered
  )

  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }

  if ("analyses" %in% colorBy) {
    colorBy <- "replicates"
  }
  if (
    grepl("analyses", colorBy) &&
      grepl("targets", colorBy)
  ) {
    colorBy <- "replicates+targets"
  }

  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)

  ms2$loop <- paste0(ms2$analysis, ms2$replicate, ms2$id, ms2$var)

  cl <- .get_colors(unique(ms2$var))

  if (showText) {
    ms2$text_string <- paste0(round(ms2$mz, 4))
    ms2$text_string[ms2$is_pre] <- paste0("Pre ", ms2$text_string[ms2$is_pre])
  } else {
    ms2$text_string <- ""
  }

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- expression(italic("m/z ") / " Da")
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ms2$linesize <- 1
    ms2$linesize[ms2$is_pre] <- 2

    plot <- ggplot2::ggplot(
      ms2,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(ggplot2::aes(
        xend = mz,
        yend = 0,
        color = var,
        linewidth = linesize
      ))

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = text_string),
          vjust = 0.2,
          hjust = -0.2,
          angle = 90,
          size = 2,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(ms2$intensity) * 1.5)
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_linewidth_continuous(range = c(1, 2), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)

    plot
  } else {
    if (is.null(xLab)) {
      xLab <- "<i>m/z</i> / Da"
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    ms2$linesize <- 0.01
    ms2$linesize[ms2$is_pre] <- 2

    ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms2$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms2$intensity) * 1.5)
    )

    loop <- NULL

    plot <- ms2 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = ~linesize)),
        text = ~ paste0(text_string, "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>%
      plotly::layout(
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

# MARK: get_components
#' @describeIn MassSpecResults_NonTargetAnalysis Extracts components based on annotation from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @export
#'
get_components.MassSpecResults_NonTargetAnalysis <- function(
  x,
  analyses = NULL,
  features = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  filtered = FALSE
) {
  if (length(x$features) == 0) {
    warning("Features not found!")
    return(data.table::data.table())
  }

  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for targets!")
    return(data.table::data.table())
  }

  fts <- split(fts, fts$analysis)
  fts_sel <- lapply(
    x$info$analysis,
    function(z, fts) {
      if (z %in% names(fts)) {
        return(fts[[z]])
      } else {
        data.table::data.table()
      }
    },
    fts = fts
  )

  names(fts_sel) <- x$info$analysis

  all_fts <- x$features

  all_fts <- Map(
    function(z, k) {
      if (nrow(z) == 0) {
        return(data.table::data.table())
      }
      if (nrow(k) == 0) {
        return(data.table::data.table())
      }
      sel <- vapply(
        z[["annotation"]],
        function(i) {
          nrow(i) > 0
        },
        FALSE
      )
      z <- z[sel, ]
      cf <- vapply(
        z[["annotation"]],
        function(i) {
          i$component_feature
        },
        ""
      )
      z$component_feature <- cf
      z <- z[cf %in% k$feature | z$feature %in% k$feature, ]
      z
    },
    all_fts,
    fts_sel
  )

  all_fts <- lapply(all_fts, function(z) {
    if (nrow(z) == 0) {
      return(data.table::data.table())
    }
    annotation <- lapply(z[["annotation"]], function(k) {
      data.table::as.data.table(k)
    })
    annotation <- data.table::rbindlist(annotation)
    z <- z[annotation, on = "feature"]
    z <- z[order(z$component_feature), ]
    z
  })

  all_fts <- data.table::rbindlist(all_fts, idcol = "analysis", fill = TRUE)
  all_fts$replicate <- x$info$replicate[all_fts$analysis]
  data.table::setnames(all_fts, "component_feature", "component")
  data.table::setcolorder(
    all_fts,
    c("analysis", "replicate", "component", "feature", "group")
  )

  all_fts
}

# MARK: map_components
#' @describeIn MassSpecResults_NonTargetAnalysis Maps components based on annotation from the MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-interactive
#' @template arg-showLegend
#' @template arg-renderEngine
#' @export
#'
map_components.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  interactive = TRUE,
  showLegend = TRUE,
  renderEngine = "webgl"
) {
  components <- get_components(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  new_order <- order(abs(components$iso_step * 2))
  components <- components[new_order, ]

  if (nrow(components) == 0) {
    warning("\U2717 Components not found for the targets!")
    return(NULL)
  }

  if (
    grepl("groups", colorBy) &&
      "group" %in% colnames(components)
  ) {
    components$id <- components$group
    colorBy <- sub("groups", "targets", colorBy)
  } else {
    components$id <- components$component
  }

  components <- .make_colorBy_varkey(components, colorBy, legendNames)

  cl <- .get_colors(unique(components$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- "Retention time / seconds"
    }
    if (is.null(yLab)) {
      yLab <- expression(italic("m/z ") / " Da")
    }

    ggplot2::ggplot(components, aes(x = rt, y = mz)) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = rtmin,
          xmax = rtmax,
          ymin = mzmin,
          ymax = mzmax,
          fill = factor(var)
        ),
        alpha = 0.7
      ) +
      ggplot2::geom_point(ggplot2::aes(color = factor(var)), size = 2) +
      geom_text(
        aes(
          x = rt,
          y = mz,
          label = paste0(iso_cat, " ", iso_isotope, " ", adduct_cat),
          color = factor(var)
        ),
        vjust = 0.2,
        hjust = -0.2,
        angle = 90,
        size = 2,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_fill_manual(values = cl) +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
  } else {
    if (is.null(xLab)) {
      xLab <- "Retention time / seconds"
    }
    if (is.null(yLab)) {
      yLab <- "<i>m/z</i> / Da"
    }
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

    plotlegend <- rep(TRUE, length(cl))
    names(plotlegend) <- names(cl)
    plot <- plot_ly()

    for (i in seq_len(nrow(components))) {
      ft <- components[i, ]

      x0 <- ft$rtmin
      x1 <- ft$rtmax
      y0 <- ft$mzmin
      y1 <- ft$mzmax
      ft_var <- ft$var

      plot <- plot %>%
        add_trace(
          x = c(x0, x1, x1, x0, x0),
          y = c(y0, y0, y1, y1, y0),
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = paste0(cl[ft_var], "70"),
          line = list(color = cl[ft_var]),
          opacity = 0.2,
          name = ft_var,
          legendgroup = ft_var,
          showlegend = FALSE
        )
    }

    for (i in seq_len(nrow(components))) {
      ft <- components[i, ]

      hT <- .make_features_hover_string(ft)

      plot <- plot %>%
        add_trace(
          x = ft$rt,
          y = ft$mz,
          type = "scatter",
          mode = "markers+text",
          marker = list(size = 8, color = cl[ft$var]),
          name = ft$var,
          legendgroup = ft$var,
          showlegend = plotlegend[ft$var],
          text = paste0(ft$iso_cat, " ", ft$iso_isotope, " ", ft$adduct_cat),
          textposition = "midle right",
          textfont = list(size = 9, color = cl[ft$var]),
          hovertemplate = hT
        )

      if (isTRUE(plotlegend[ft$var])) {
        plotlegend[ft$var] <- FALSE
      }
    }

    if (showLegend) {
      plot <- plot %>%
        plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    } else {
      plot <- plot %>%
        plotly::layout(
          legend = NULL,
          xaxis = xaxis,
          yaxis = yaxis,
          title = title
        )
    }

    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }

    plot
  }
}

# MARK: get_suspects
#' @describeIn MassSpecResults_NonTargetAnalysis Extracts feature suspects based on a database. The database should be a data.frame with at least the columns name and mass, indicating the name and neutral monoisotopic mass of the suspect targets. If a database is not provided, it will extract suspects from the MassSpecResults_NonTargetAnalysis object if they were previously annotated.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-ppmMS2
#' @template arg-ms-mzrMS2
#' @template arg-ms-minCusiness
#' @template arg-ms-minFragments
#' @template arg-ms-filtered
#' @export
#'
get_suspects.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  mzrMS2 = 0.008,
  minCusiness = 0.7,
  minFragments = 3,
  filtered = FALSE
) {
  if (length(x$features) == 0) {
    warning("Features not found!")
    return(data.table::data.table())
  }

  if (is.null(database)) {
    features <- get_features(
      x,
      analyses,
      features,
      mass,
      mz,
      rt,
      mobility,
      ppm,
      sec,
      millisec,
      filtered
    )

    if (nrow(features) == 0) {
      message("\U2717 Features not found for targets!")
      return(data.table::data.table())
    }

    features[["name"]] <- NULL

    if ("suspects" %in% colnames(features)) {
      sel <- vapply(
        features$suspects,
        function(z) {
          if (length(z) > 0) {
            if (is.data.frame(z)) {
              if (nrow(z) > 0) {
                return(TRUE)
              }
            }
          }
          FALSE
        },
        FALSE
      )

      if (any(sel)) {
        features <- features[sel, ]
        suspects_l <- features[["suspects"]]

        suspects <- lapply(
          seq_len(length(suspects_l)),
          function(z, suspects_l, features) {
            temp <- suspects_l[[z]]
            temp_ft <- features[z, ]
            temp_ft[["suspects"]] <- NULL
            temp_ft$rt <- NULL
            temp_ft$intensity <- NULL
            temp_ft$area <- NULL
            temp_ft$mass <- NULL

            if ("group" %in% colnames(temp)) {
              temp <- merge(
                temp,
                temp_ft,
                by = c("feature", "group"),
                all = TRUE
              )
            } else {
              temp <- merge(temp, temp_ft, by = "feature", all = TRUE)
            }

            data.table::setcolorder(temp, c("analysis", "replicate"))

            temp
          },
          suspects_l = suspects_l,
          features = features
        )

        suspects <- data.table::rbindlist(suspects, fill = TRUE)
      } else {
        warning(
          "Suspects were not found! Run SuspectScreening Method or give a database."
        )
        return(data.table::data.table())
      }
    } else {
      warning(
        "Suspects were not found! Run SuspectScreening Method or give a database."
      )
      return(data.table::data.table())
    }
  } else {
    database <- data.table::as.data.table(database)
    valid_db <- FALSE

    if (is.data.frame(database)) {
      database <- data.table::as.data.table(database)
      if (
        any(c("mass", "neutralMass") %in% colnames(database)) |
          "mz" %in% colnames(database)
      ) {
        if ("name" %in% colnames(database)) {
          if ("neutralMass" %in% colnames(database)) {
            data.table::setnames(database, "neutralMass", "mass")
          }
          valid_db <- TRUE
        }
      }
    }

    if (!valid_db) {
      warning(
        "Argument database must be a data.frame with at least the columns name and mass or mz!"
      )
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
      suspects <- get_features(
        x,
        analyses,
        mass = database,
        ppm = ppm,
        sec = sec,
        millisec = millisec,
        filtered = filtered
      )
    } else if ("mz" %in% colnames(database)) {
      suspects <- get_features(
        x,
        analyses,
        mz = database,
        ppm = ppm,
        sec = sec,
        millisec = millisec,
        filtered = filtered
      )
    } else {
      warning(
        "Argument database must be a data.frame with at least the columns name and mass or mz!"
      )
      return(data.table::data.table())
    }

    if (nrow(suspects) == 0) {
      message("\U2717 No suspects found!")
      return(data.table::data.table())
    }

    suspects <- split(suspects, suspects$analysis)

    suspects <- lapply(
      suspects,
      function(z, database) {
        out <- data.table::data.table()

        if (nrow(z) > 0) {
          for (i in seq_len(nrow(z))) {
            suspect_analysis <- z$analysis[i]
            suspect_replicate <- z$replicate[i]
            suspect_feature <- z$feature[i]
            suspect_name <- z$name[i]
            suspect_mass <- z$mass[i]
            suspect_rt <- z$rt[i]
            suspect_intensity <- z$intensity[i]
            suspect_area <- z$area[i]

            suspect_db <- database[vapply(
              database$name,
              function(j) {
                grepl(j, suspect_name)
              },
              FALSE
            )]
            suspect_db <- suspect_db[1, ]

            temp <- data.table::data.table(
              "analysis" = suspect_analysis,
              "feature" = suspect_feature
            )

            if ("group" %in% colnames(z)) {
              temp$group <- z$group[i]
            }
            temp$name <- suspect_name

            if ("formula" %in% colnames(suspect_db)) {
              temp$formula <- suspect_db$formula
            }
            if ("SMILES" %in% colnames(suspect_db)) {
              temp$SMILES <- suspect_db$SMILES
            }

            temp$mass <- suspect_mass

            if ("mz" %in% suspect_db) {
              temp$exp_mass <- suspect_db$mz - (z$polarity[i] * 1.007276)
            } else {
              temp$exp_mass <- suspect_db$mass
            }

            temp$error_mass <- round(
              ((temp$mass - temp$exp_mass) / temp$mass) * 1E6,
              digits = 1
            )

            temp$rt <- suspect_rt
            temp$exp_rt <- suspect_db$rt
            temp$error_rt <- round(temp$rt - temp$exp_rt, digits = 1)
            temp$id_level <- "4"
            temp$shared_fragments <- 0
            temp$cusiness <- 0
            temp$fragments <- list(data.table::data.table())
            temp$intensity <- suspect_intensity
            temp$area <- suspect_area

            if (temp$exp_rt > 0) {
              temp$id_level <- "3b"
            }

            if (
              "fragments" %in%
                colnames(suspect_db) ||
                "fragments_mz" %in% colnames(suspect_db)
            ) {
              if ("fragments" %in% colnames(suspect_db)) {
                fragments <- suspect_db$fragments
              } else {
                fragments <- suspect_db$fragments_mz
              }

              if (!is.na(fragments)) {
                ms2 <- data.table::data.table()

                if ("ms2" %in% colnames(z)) {
                  ms2 <- z$ms2[i][[1]]
                  if (length(ms2) == 0) {
                    ms2 <- data.table::data.table()
                  }
                }

                if (nrow(ms2) > 0) {
                  if ("fragments" %in% colnames(suspect_db)) {
                    fragments <- unlist(strsplit(
                      fragments,
                      split = "; ",
                      fixed = TRUE
                    ))
                    fragments <- strsplit(fragments, " ")
                    fragments <- data.table::data.table(
                      "formula" = vapply(
                        fragments,
                        function(x) {
                          x[1]
                        },
                        NA_character_
                      ),
                      "mz" = vapply(
                        fragments,
                        function(x) {
                          as.numeric(x[1])
                        },
                        NA_real_
                      ),
                      "intensity" = vapply(
                        fragments,
                        function(x) {
                          as.numeric(x[2])
                        },
                        NA_real_
                      )
                    )
                  } else {
                    fragments <- unlist(strsplit(
                      fragments,
                      split = ";",
                      fixed = TRUE
                    ))
                    fragments_int <- unlist(strsplit(
                      suspect_db$fragments_int,
                      split = ";",
                      fixed = TRUE
                    ))
                    if ("fragments_formula" %in% colnames(suspect_db)) {
                      fragments_formula <- unlist(
                        strsplit(
                          suspect_db$fragments_formula,
                          split = ";",
                          fixed = TRUE
                        )
                      )
                    } else {
                      fragments_formula <- rep(NA_character_, length(fragments))
                    }

                    fragments <- data.table::data.table(
                      "formula" = fragments_formula,
                      "mz" = as.numeric(fragments),
                      "intensity" = as.numeric(fragments_int)
                    )
                  }

                  mzr <- fragments$mz * ppm / 1E6
                  mzr[mzr < mzrMS2] <- mzrMS2
                  fragments$mzmin <- fragments$mz - mzr
                  fragments$mzmax <- fragments$mz + mzr

                  fragments$exp_idx <- vapply(
                    seq_len(nrow(fragments)),
                    function(z, ms2, fragments) {
                      idx <- which(
                        ms2$mz >= fragments$mzmin[z] &
                          ms2$mz <= fragments$mzmax[z]
                      )
                      if (length(idx) == 0) {
                        NA_integer_
                      } else {
                        if (length(idx) > 1) {
                          candidates <- ms2$mz[idx]
                          mz_error <- abs(candidates - fragments$mz[z])
                          idx <- idx[which.min(mz_error)]
                          idx <- idx[1]
                        }
                        as.integer(idx)
                      }
                    },
                    ms2 = ms2,
                    fragments = fragments,
                    integer(1)
                  )

                  number_shared_fragments <- length(fragments$exp_idx[
                    !is.na(fragments$exp_idx)
                  ])

                  if (number_shared_fragments > 0) {
                    fragments$exp_mz <- ms2$mz[fragments$exp_idx]
                    fragments$mass_error <- round(
                      fragments$mz - fragments$exp_mz,
                      digits = 4
                    )
                    fragments$exp_intensity <- ms2$intensity[fragments$exp_idx]
                    fragments$exp_intensity[is.na(fragments$exp_intensity)] <- 0
                    sel <- fragments$exp_intensity > 0
                    intensity <- fragments$intensity[sel]
                    intensity <- intensity / max(intensity)
                    intensity_exp <- fragments$exp_intensity[sel]
                    intensity_exp <- intensity_exp / max(intensity_exp)
                    dot_pro <- intensity * intensity_exp
                    dot_pro <- sum(dot_pro)
                    mag_int <- sqrt(sum(intensity^2))
                    mag_exp_int <- sqrt(sum(intensity_exp^2))
                    cusiness <- round(
                      dot_pro / (mag_int * mag_exp_int),
                      digits = 4
                    )

                    ms2_unknown <- ms2[
                      unique(-fragments$exp_idx[!is.na(fragments$exp_idx)]),
                      c("mz", "intensity"),
                      with = FALSE
                    ]
                    if (nrow(ms2_unknown) > 1) {
                      ms2_unknown$formula <- "unkown"
                      data.table::setnames(
                        ms2_unknown,
                        c("mz", "intensity"),
                        c("exp_mz", "exp_intensity")
                      )
                      fragments <- data.table::rbindlist(
                        list(fragments, ms2_unknown),
                        fill = TRUE
                      )
                    }
                  } else {
                    cusiness <- 0
                  }

                  temp$shared_fragments <- number_shared_fragments
                  temp$cusiness <- cusiness
                  temp$epx_ms2_size <- nrow(ms2)

                  if (
                    number_shared_fragments >= minFragments ||
                      cusiness >= minCusiness
                  ) {
                    temp$fragments <- list(fragments)
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
      },
      database = database
    )

    suspects <- data.table::rbindlist(suspects, fill = TRUE)
  }
  suspects
}

# MARK: plot_suspects
#' @describeIn MassSpecResults_NonTargetAnalysis Plots feature suspects based on a database. The database should be a data.frame with at least the columns name and mass, indicating the name and neutral monoisotopic mass of the suspect targets. If a database is not provided, it will extract suspects from the MassSpecResults_NonTargetAnalysis object if they were previously annotated.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-ppmMS2
#' @template arg-ms-mzrMS2
#' @template arg-ms-minCusiness
#' @template arg-ms-minFragments
#' @template arg-ms-filtered
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-useLoadedData
#' @template arg-colorBy
#' @template arg-nts-heights
#' @template arg-interactive
#' @export
#'
plot_suspects.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  mzrMS2 = 0.008,
  minCusiness = 0.7,
  minFragments = 3,
  filtered = FALSE,
  rtExpand = 120,
  mzExpand = 0.005,
  useLoadedData = TRUE,
  legendNames = NULL,
  colorBy = "replicates+targets",
  heights = c(0.5, 0.5),
  interactive = TRUE
) {
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
    mzrMS2,
    minCusiness,
    minFragments,
    filtered
  )

  if (nrow(suspects) == 0) {
    warning("\U2717 Suspects not found!")
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

  if (nrow(eic) == 0) {
    message("\U2717 Traces and/or features not found for targets!")
    return(NULL)
  }

  suspects <- .make_colorBy_varkey(suspects, colorBy, TRUE)

  eic$uid <- paste0(eic$feature, "_", eic$analysis)
  suspects$uid <- paste0(suspects$feature, "_", suspects$analysis)

  leg <- suspects$var
  names(leg) <- paste0(suspects$feature, "_", suspects$analysis)

  eic$var <- leg[eic$uid]

  leg <- unique(leg)
  cl <- .get_colors(leg)

  if (!interactive) {
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = eic,
        ggplot2::aes(
          x = rt,
          y = intensity,
          group = uid,
          color = var
        ),
        linewidth = 0.5
      ) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::labs(x = "Retention time / seconds", y = "Intensity / counts") +
      ggplot2::theme_classic()

    for (g in leg) {
      uid_list <- unique(eic$uid[eic$var == g])

      for (u in uid_list) {
        df <- dplyr::filter(eic, uid == u)
        ft <- dplyr::filter(suspects, uid == u)

        if (nrow(ft) > 0) {
          df_filled <- dplyr::filter(df, rt >= ft$rtmin & rt <= ft$rtmax)

          p1 <- p1 +
            ggplot2::geom_ribbon(
              data = df_filled,
              ggplot2::aes(
                x = rt,
                ymin = 0,
                ymax = intensity
              ),
              fill = cl[g],
              alpha = 0.5
            )
        }
      }
    }

    data <- suspects$ms2
    names(data) <- suspects$uid
    data <- Map(
      function(i, j) {
        if (nrow(i) > 0) {
          i$var <- j
          i$intensity <- i$intensity / max(i$intensity, na.rm = TRUE)
        }
        i
      },
      data,
      suspects$var
    )
    data <- data.table::rbindlist(data, idcol = "uid", fill = TRUE)

    fragments <- suspects$fragments
    names(fragments) <- suspects$uid

    fragments <- lapply(fragments, function(z) {
      if (!is.na(z)) {
        z <- unlist(strsplit(z, split = "; ", fixed = TRUE))
        z <- strsplit(z, " ")
        z <- data.table::data.table(
          "mz" = vapply(
            z,
            function(x) {
              as.numeric(x[1])
            },
            NA_real_
          ),
          "intensity" = vapply(
            z,
            function(x) {
              as.numeric(x[2])
            },
            NA_real_
          )
        )

        z$intensity <- z$intensity / max(z$intensity, na.rm = TRUE)
        z$intensity <- -z$intensity
      } else {
        z <- data.table::data.table()
      }
      z
    })

    fragments <- Map(
      function(i, j) {
        if (nrow(i) > 0) {
          i$var <- j
        }
        i
      },
      fragments,
      suspects$var
    )

    fragments <- data.table::rbindlist(fragments, idcol = "uid", fill = TRUE)

    all_data <- data.table::rbindlist(list(data, fragments), fill = TRUE)

    all_data$vpos <- -0.2
    all_data$vpos[all_data$intensity < 0] <- 1.2

    p2 <- ggplot2::ggplot(
      all_data,
      ggplot2::aes(x = mz, y = intensity, fill = var)
    ) +
      ggplot2::geom_col(width = 0.5, position = "identity") +
      ggplot2::scale_fill_manual(values = cl) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::guides(fill = "none") +
      ggplot2::geom_text(
        ggplot2::aes(label = round(mz, 4), color = var),
        vjust = 0.2,
        hjust = all_data$vpos,
        angle = 90,
        size = 2,
        show.legend = FALSE
      ) +
      ggplot2::labs(
        x = expression(italic("m/z ") / " Da"),
        y = "Normalized intensity (Exp vs -Database)"
      ) +
      ggplot2::theme_classic() +
      ggplot2::geom_hline(yintercept = 0, linetype = "solid") +
      ggplot2::annotate(
        "text",
        x = max(all_data$mz, na.rm = TRUE) - 0.1,
        y = 1.25,
        label = "Experimental spectra",
        hjust = 1
      ) +
      ggplot2::annotate(
        "text",
        x = max(all_data$mz, na.rm = TRUE) - 0.1,
        y = -1.25,
        label = "Database spectra",
        hjust = 1
      ) +
      ggplot2::scale_y_continuous(
        limits = c(
          min(all_data$intensity, na.rm = TRUE) * 1.3,
          max(all_data$intensity, na.rm = TRUE) * 1.3
        )
      )

    gridExtra::grid.arrange(p1, p2, ncol = 1)
  } else {
    showleg <- rep(TRUE, length(leg))
    names(showleg) <- leg

    plot <- plot_ly()

    for (g in leg) {
      uid <- unique(suspects$uid[suspects$var %in% g])

      for (u in uid) {
        ft <- suspects[suspects$uid == u, ]
        if (nrow(ft) == 0) {
          next
        }
        df <- eic[eic$uid == u, ]

        plot <- plot %>%
          add_trace(
            x = df$rt,
            y = df$intensity,
            type = "scatter",
            mode = "lines",
            line = list(width = 0.5, color = cl[g]),
            connectgaps = TRUE,
            name = g,
            legendgroup = g,
            showlegend = FALSE
          )

        df <- df[df$rt >= ft$rtmin & df$rt <= ft$rtmax, ]
        hT <- .make_features_hover_string(ft)
        hT <- paste(
          "</br> suspect: ",
          g,
          "</br> id_level: ",
          ft$id_level,
          "</br> error_mass: ",
          ft$error_mass,
          "</br> error_rt: ",
          ft$error_rt,
          "</br> shared_fragments: ",
          ft$shared_fragments,
          hT
        )

        plot <- plot %>%
          add_trace(
            x = df$rt,
            y = df$intensity,
            type = "scatter",
            mode = "lines+markers",
            fill = "tozeroy",
            connectgaps = TRUE,
            fillcolor = paste(color = cl[g], 50, sep = ""),
            line = list(width = 0.1, color = cl[g]),
            marker = list(size = 3, color = cl[g]),
            text = hT,
            hoverinfo = "text",
            name = g,
            legendgroup = g,
            showlegend = showleg[g]
          )

        showleg[g] <- FALSE
      }
    }

    max_mz <- 0

    plot2 <- plot_ly()

    for (g in leg) {
      uid <- unique(suspects$uid[suspects$var %in% g])

      for (u in uid) {
        data <- suspects$ms2[suspects$uid == u][[1]]
        fragments <- suspects$fragments[suspects$uid == u][[1]]

        if (!is.null(data) && !is.null(fragments)) {
          bar_widths <- rep(0.2, nrow(data))

          if (nrow(data) > 0) {
            data$intensity <- data$intensity / max(data$intensity, na.rm = TRUE)

            temp_max_mz <- max(data$mz, na.rm = TRUE)
            if (temp_max_mz > max_mz) {
              max_mz <- temp_max_mz
            }

            plot2 <- plot2 %>%
              add_trace(
                x = data$mz,
                y = data$intensity,
                type = "bar",
                width = 0.05,
                marker = list(
                  color = cl[g],
                  line = list(color = cl[g], width = bar_widths)
                ),
                text = paste0(
                  round(
                    as.numeric(
                      data$mz
                    ),
                    4
                  ),
                  "  "
                ),
                textposition = "outside",
                textangle = 90,
                textfont = list(size = 9, color = cl[g]),
                name = g,
                legendgroup = g,
                hovertemplate = paste(
                  "Exp:",
                  "<br><i>m/z</i>: %{x:.4f}",
                  "<br>intensity: %{y:.0f}"
                ),
                showlegend = FALSE
              )
          }

          if (nrow(fragments) > 0) {
            fragments <- fragments[!is.na(fragments$intensity), ]
            fragments$intensity <- fragments$intensity /
              max(fragments$intensity, na.rm = TRUE)
            fragments$intensity <- -fragments$intensity

            temp_max_mz <- max(fragments$mz, na.rm = TRUE)
            if (temp_max_mz > max_mz) {
              max_mz <- temp_max_mz
            }

            plot2 <- plot2 %>%
              add_trace(
                x = fragments$mz,
                y = fragments$intensity,
                type = "bar",
                width = 0.05,
                marker = list(
                  color = cl[g],
                  line = list(color = cl[g], width = bar_widths)
                ),
                text = paste0(
                  round(
                    as.numeric(
                      fragments$mz
                    ),
                    4
                  ),
                  "  "
                ),
                textposition = "outside",
                textangle = 90,
                textfont = list(size = 9, color = cl[g]),
                name = g,
                legendgroup = g,
                hovertemplate = paste(
                  "Database:",
                  "<br><i>m/z</i>: %{x:.4f}",
                  "<br>intensity: %{y:.0f}"
                ),
                showlegend = FALSE
              )

            # add annotation text to the plot2 in the max y and max x the text "experimental spectra" and "database spectra" in the max x and min y of the plot region
          }
        }
      }
    }

    plot2 <- plot2 %>%
      add_trace(
        x = max_mz,
        y = 1.2,
        type = "scatter",
        mode = "text",
        text = "<i>Experimental spectra</i>",
        textposition = "top left",
        textfont = list(color = "black"),
        showlegend = FALSE
      )

    plot2 <- plot2 %>%
      add_trace(
        x = max_mz,
        y = -1.2,
        type = "scatter",
        mode = "text",
        text = "<i>Database spectra</i>",
        textposition = "bottom left",
        textfont = list(color = "black"),
        showlegend = FALSE
      )

    xaxis1 <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = "Retention time / seconds",
      titlefont = list(size = 12, color = "black"),
      autotick = TRUE,
      ticks = "outside"
    )

    xaxis2 <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = "<i>m/z</i> / Da",
      titlefont = list(size = 12, color = "black"),
      autotick = TRUE,
      ticks = "outside"
    )

    yaxis1 <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = "Intensity / counts",
      titlefont = list(size = 12, color = "black")
    )

    yaxis2 <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = "Normalized intensity (Exp vs -Database)",
      range = c(-1.4, 1.4),
      titlefont = list(size = 12, color = "black")
    )

    plotList <- list()

    plot <- plot %>% plotly::layout(xaxis = xaxis1, yaxis = yaxis1)
    plotList[["plot"]] <- plot

    plot2 <- plot2 %>%
      plotly::layout(
        xaxis = xaxis2,
        yaxis = yaxis2,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )
    plotList[["plot2"]] <- plot2

    plotf <- subplot(
      plotList,
      nrows = 2,
      titleY = TRUE,
      titleX = TRUE,
      heights = heights[1:2],
      margin = 0.03,
      shareX = FALSE,
      which_layout = "merge"
    )

    plotf
  }
}

# MARK: get_internal_standards
#' @describeIn MassSpecResults_NonTargetAnalysis Extracts internal standards from the MassSpecResults_NonTargetAnalysis object. If the MassSpecResults_NonTargetAnalysis object has groups, it averages the internal standards across replicates, when `average = TRUE`.
#' @template arg-nts-x
#' @template arg-ms-average
#' @export
#'
get_internal_standards.MassSpecResults_NonTargetAnalysis <- function(
  x,
  average = TRUE
) {
  istd <- get_features(x, filtered = TRUE)
  if ("istd" %in% colnames(istd)) {
    sel <- vapply(
      istd$istd,
      function(z) {
        if (length(z) > 0) {
          if (is.data.frame(z)) {
            if (nrow(z) > 0) {
              return(TRUE)
            }
          }
        }
        FALSE
      },
      FALSE
    )
    istd <- istd[sel, ]
    if (nrow(istd) > 0) {
      istd_l <- istd[["istd"]]
      istd_l2 <- lapply(
        seq_len(length(istd_l)),
        function(z, istd_l, istd) {
          temp <- istd_l[[z]]
          temp_ft <- istd[z, ]
          temp <- cbind(temp, temp_ft)
          temp
        },
        istd = istd,
        istd_l = istd_l
      )
      istd <- data.table::rbindlist(istd_l2, fill = TRUE)
      istd$rtr <- round(istd$rtmax - istd$rtmin, digits = 1)
      istd$mzr <- round(istd$mzmax - istd$mzmin, digits = 4)
      if ("annotation" %in% colnames(istd)) {
        istd$iso_n <- vapply(
          istd$annotation,
          function(z) {
            if (length(z) == 0) {
              NA_real_
            } else {
              z$iso_size
            }
          },
          NA_real_
        )
        istd$iso_c <- vapply(
          istd$annotation,
          function(z) {
            if (length(z) == 0) {
              NA_real_
            } else {
              z$iso_number_carbons
            }
          },
          NA_real_
        )
      } else {
        istd$iso_n <- NA_real_
        istd$iso_c <- NA_real_
      }
      if (
        any(vapply(x$features, function(x) any(!is.na(x$group) | x$group %in% ""), FALSE)) &&
          average
      ) {
        rpl <- x$info$replicate
        names(rpl) <- x$info$analysis
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

        istd <- istd[,
          `:=`(
            freq = length(intensity),
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
        if (
          any(vapply(
            x$features,
            function(x) any(!(is.na(x$group) | x$group == "")),
            FALSE
          ))
        ) {
          cols <- c(cols, "group")
        }

        istd <- istd[, cols, with = FALSE]
        istd$intensity <- round(istd$intensity, digits = 0)
        istd$area <- round(istd$area, digits = 0)
      }

      setorder(istd, "name")

      istd
    } else {
      warning("Internal standards not found!")
      data.table::data.table()
    }
  } else {
    warning(
      "Not present! Run FindInternalStandards method to tag the internal standards!"
    )
    data.table::data.table()
  }
}

# MARK: plot_internal_standards
#' @describeIn MassSpecResults_NonTargetAnalysis Plots internal standards from the MassSpecResults_NonTargetAnalysis object. If the MassSpecResults_NonTargetAnalysis object has groups, it averages the internal standards across replicates, when `average = TRUE`.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-showPresence
#' @template arg-ms-showRecovery
#' @template arg-ms-showDeviations
#' @template arg-ms-showWidths
#' @template arg-renderEngine
#' @export
#'
plot_internal_standards.MassSpecResults_NonTargetAnalysis <- function(
  x,
  analyses = NULL,
  showPresence = TRUE,
  showRecovery = TRUE,
  showDeviations = TRUE,
  showWidths = TRUE,
  renderEngine = "webgl"
) {
  analyses <- .check_analyses_argument(x$features, analyses)
  if (any(vapply(x$features[analyses], function(x) any(!(is.na(x$group) | x$group %in% "")), FALSE))) {
    istd <- get_internal_standards(x, average = TRUE)
    if (nrow(istd) == 0) {
      warning("Internal standards not found!")
      return(NULL)
    }
    rpls <- x$info$replicate
    names(rpls) <- x$info$analysis
    istd <- istd[istd$replicate %in% rpls[analyses], ]
    if (nrow(istd) == 0) {
      warning("Internal standards not found!")
      return(NULL)
    }
  } else {
    istd <- get_internal_standards(x, average = FALSE)
    if (nrow(istd) == 0) {
      warning("Internal standards not found!")
      return(NULL)
    }
    istd <- istd[istd$analysis %in% analyses, ]
    if (nrow(istd) == 0) {
      warning("Internal standards not found!")
      return(NULL)
    }
  }
  if (
    !("analysis" %in% colnames(istd)) &
      "replicate" %in% colnames(istd)
  ) {
    istd$analysis <- istd$replicate
  }
  analyses <- unique(istd$analysis)
  leg <- unique(istd$name)
  colors <- .get_colors(leg)
  showLegend <- TRUE
  showLegendPresence <- FALSE
  showLegendRecovery <- FALSE
  showLegendDeviations <- FALSE
  showLegendWidths <- FALSE

  if (showPresence && "freq" %in% colnames(istd)) {
    plot_presence <- plot_ly(istd, x = analyses)
    showLegendPresence <- TRUE
    showLegend <- FALSE
    max_freq <- max(istd$freq, na.rm = TRUE)
  }

  if (showRecovery && !all(is.na(istd$rec))) {
    plot_recovery <- plot_ly(istd, x = analyses)
    if (showLegend) {
      showLegendRecovery <- TRUE
      showLegend <- FALSE
    }
  }

  if (showDeviations) {
    plot_rtr <- plot_ly(istd, x = analyses)
    plot_mzr <- plot_ly(istd, x = analyses)
    if (showLegend) {
      showLegendDeviations <- TRUE
      showLegend <- FALSE
    }
  }

  if (showWidths) {
    plot_rtw <- plot_ly(istd, x = analyses)
    plot_mzw <- plot_ly(istd, x = analyses)
    if (showLegend) {
      showLegendWidths <- TRUE
      showLegend <- FALSE
    }
  }

  freq_template <- rep(0, length(analyses))
  names(freq_template) <- analyses

  for (i in unique(istd$name)) {
    df <- istd[istd$name == i, ]

    if ("freq" %in% colnames(istd) && showPresence) {
      freq <- freq_template
      for (j in analyses) {
        freq[j] <- sum(df$freq[df$analysis == j])
      }
      freq <- freq / max_freq * 100

      plot_presence <- plot_presence %>%
        add_trace(
          df,
          x = analyses,
          y = freq,
          type = "scatter",
          mode = "markers",
          marker = list(size = 5, color = colors[i]),
          connectgaps = FALSE,
          name = i,
          legendgroup = i,
          showlegend = showLegendPresence
        )
    }

    if ("rec" %in% colnames(istd) && showRecovery) {
      df_rec <- df[!is.na(df$rec), ]

      if (nrow(df_rec) > 0) {
        if (!"rec_sd" %in% colnames(df_rec)) {
          error_rec <- NULL
        } else {
          df_rec$rec_sd[is.na(df_rec$rec_sd)] <- 0

          error_rec <- list(
            type = "data",
            symmetric = FALSE,
            arrayminus = df_rec$rec_sd,
            array = df_rec$rec_sd,
            color = colors[i],
            width = 5
          )
        }

        plot_recovery <- plot_recovery %>%
          add_trace(
            df_rec,
            x = df_rec$analysis,
            y = df_rec$rec * 100,
            type = "scatter",
            mode = "markers",
            marker = list(size = 5, color = colors[i]),
            error_y = error_rec,
            connectgaps = TRUE,
            name = i,
            legendgroup = i,
            showlegend = showLegendRecovery
          )
      }
    }

    if (showDeviations) {
      df_rtr <- df[!is.na(df$error_rt), ]

      if (nrow(df_rtr) > 0) {
        if (!"error_rt_sd" %in% colnames(df_rtr)) {
          error_error_rt <- NULL
        } else {
          df_rtr$error_rt_sd[is.na(df_rtr$error_rt_sd)] <- 0

          error_error_rt <- list(
            type = "data",
            symmetric = FALSE,
            arrayminus = df_rtr$error_rt_sd,
            array = df_rtr$error_rt_sd,
            color = colors[i],
            width = 5
          )
        }

        plot_rtr <- plot_rtr %>%
          add_trace(
            df_rtr,
            x = df_rtr$analysis,
            y = df_rtr$error_rt,
            type = "scatter",
            mode = "markers",
            marker = list(size = 5, color = colors[i]),
            error_y = error_error_rt,
            connectgaps = FALSE,
            name = i,
            legendgroup = i,
            showlegend = showLegendDeviations
          )
      }

      df_mzr <- df[!is.na(df$error_mass), ]

      if (nrow(df_mzr) > 0) {
        if (!"error_mass_sd" %in% colnames(df_mzr)) {
          error_error_mass <- NULL
        } else {
          df_mzr$error_mass_sd[is.na(df_mzr$error_mass_sd)] <- 0

          error_error_mass <- list(
            type = "data",
            symmetric = FALSE,
            arrayminus = df_mzr$error_mass_sd,
            array = df_mzr$error_mass_sd,
            color = colors[i],
            width = 5
          )
        }

        plot_mzr <- plot_mzr %>%
          add_trace(
            df_mzr,
            x = df_mzr$analysis,
            y = df_mzr$error_mass,
            type = "scatter",
            mode = "markers",
            marker = list(size = 5, color = colors[i]),
            error_y = error_error_mass,
            connectgaps = FALSE,
            name = i,
            legendgroup = i,
            showlegend = FALSE
          )
      }
    }

    if (showWidths) {
      df_rtw <- df[!is.na(df$rtr), ]

      if (nrow(df_rtw) > 0) {
        if (!"rtr_sd" %in% colnames(df_rtw)) {
          error_rtr <- NULL
        } else {
          df_rtw$rtr_sd[is.na(df_rtw$rtr_sd)] <- 0

          error_rtr <- list(
            type = "data",
            symmetric = FALSE,
            arrayminus = df_rtw$rtr_sd,
            array = df_rtw$rtr_sd,
            color = colors[i],
            width = 5
          )
        }

        plot_rtw <- plot_rtw %>%
          add_trace(
            df_rtw,
            x = df_rtw$analysis,
            y = df_rtw$rtr,
            type = "scatter",
            mode = "markers",
            marker = list(size = 5, color = colors[i]),
            error_y = error_rtr,
            connectgaps = TRUE,
            name = i,
            legendgroup = i,
            showlegend = showLegendWidths
          )
      }

      df_mzw <- df[!is.na(df$mzr), ]

      if (nrow(df_mzw) > 0) {
        if (!"mzr_sd" %in% colnames(df_mzw)) {
          error_mzr <- NULL
        } else {
          df_mzw$mzr_sd[is.na(df_mzw$mzr_sd)] <- 0

          error_mzr <- list(
            type = "data",
            symmetric = FALSE,
            arrayminus = df_mzw$mzr_sd,
            array = df_mzw$mzr_sd,
            color = colors[i],
            width = 5
          )
        }

        plot_mzw <- plot_mzw %>%
          add_trace(
            df_mzw,
            x = df_mzw$analysis,
            y = df_mzw$mzr,
            type = "scatter",
            mode = "markers",
            marker = list(size = 5, color = colors[i]),
            error_y = error_mzr,
            connectgaps = TRUE,
            name = i,
            legendgroup = i,
            showlegend = FALSE
          )
      }
    }
  }

  if ("group" %in% colnames(istd)) {
    rt_error <- c(
      (min(istd$error_rt, na.rm = TRUE) - max(istd$error_rt_sd, na.rm = TRUE)) *
        0.9,
      (max(istd$error_rt, na.rm = TRUE) + max(istd$error_rt_sd, na.rm = TRUE)) *
        1.1
    )

    mz_error <- c(
      (min(istd$error_mass, na.rm = TRUE) -
        max(istd$error_mass_sd, na.rm = TRUE)) *
        0.9,
      (max(istd$error_mass, na.rm = TRUE) +
        max(istd$error_mass_sd, na.rm = TRUE)) *
        1.1
    )

    time_range <- c(
      0,
      (max(istd$rtr, na.rm = TRUE) + max(istd$rtr_sd, na.rm = TRUE)) * 1.1
    )

    mass_range <- c(
      0,
      (max(istd$mzr, na.rm = TRUE) + max(istd$mzr_sd, na.rm = TRUE)) * 1.1
    )
  } else {
    rt_error <- c(
      min(istd$error_rt, na.rm = TRUE) * 0.9,
      max(istd$error_rt, na.rm = TRUE) * 1.1
    )
    mz_error <- c(
      min(istd$error_mass, na.rm = TRUE) * 0.9,
      max(istd$error_mass, na.rm = TRUE) * 1.1
    )
    time_range <- c(0, max(istd$rtr, na.rm = TRUE) * 1.1)
    mass_range <- c(0, max(istd$mzr, na.rm = TRUE) * 1.1)
  }

  if (rt_error[1] >= -20) {
    rt_error <- c(-20, rt_error[2])
  }
  if (rt_error[2] <= 20) {
    rt_error <- c(rt_error[1], 20)
  }
  if (mz_error[1] >= -15) {
    mz_error <- c(-15, mz_error[2])
  }
  if (mz_error[2] <= 15) {
    mz_error <- c(mz_error[1], 15)
  }
  if (time_range[2] <= 30) {
    time_range <- c(0, 30)
  }
  if (mass_range[2] <= 0.01) {
    mass_range <- c(0, 0.01)
  }

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = NULL
  )

  yaxis_presence <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Presence / %",
    titlefont = list(size = 12, color = "black"),
    range = c(-10, 200)
  )

  yaxis_recovery <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Recovery / %",
    titlefont = list(size = 12, color = "black"),
    range = c(-10, 200)
  )

  yaxis_deviation_rt <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "RT / s",
    titlefont = list(size = 12, color = "black"),
    range = rt_error
  )

  yaxis_deviation_mz <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Mass / ppm",
    titlefont = list(size = 12, color = "black"),
    range = mz_error
  )

  yaxis_width_rt <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Width / s",
    titlefont = list(size = 12, color = "black"),
    range = time_range
  )

  yaxis_width_mz <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Width / Da",
    titlefont = list(size = 12, color = "black"),
    range = mass_range
  )

  plotList <- list()

  hrect <- function(y0 = 0, y1 = 1, fillcolor = "lightgreen", opacity = 0.2) {
    list(
      type = "rect",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y0,
      y1 = y1,
      line_width = 0,
      fillcolor = fillcolor,
      opacity = opacity,
      layer = "below"
    )
  }

  if ("freq" %in% colnames(istd) && showPresence) {
    plot_presence <- plot_presence %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis_presence,
        shapes = hrect(90, 110)
      )
    plotList[["plot_presence"]] <- plot_presence
  }

  if ("rec" %in% colnames(istd) && showRecovery) {
    plot_recovery <- plot_recovery %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis_recovery,
        shapes = hrect(50, 150)
      )
    plotList[["plot_recovery"]] <- plot_recovery
  }

  if (showDeviations) {
    plot_rtr <- plot_rtr %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis_deviation_rt,
        shapes = hrect(-15, 15)
      )
    plotList[["plot_rtr"]] <- plot_rtr
    plot_mzr <- plot_mzr %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis_deviation_mz,
        shapes = hrect(-10, 10)
      )
    plotList[["plot_mzr"]] <- plot_mzr
  }

  if (showWidths) {
    plot_rtw <- plot_rtw %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis_width_rt,
        shapes = hrect(5, 25)
      )
    plotList[["plot_rtw"]] <- plot_rtw
    plot_mzw <- plot_mzw %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis_width_mz,
        shapes = hrect(0, 0.005)
      )
    plotList[["plot_mzw"]] <- plot_mzw
  }

  if (length(plotList) == 0) {
    return(NULL)
  } else if (length(plotList) == 1) {
    final_plot <- plotList[[1]]
  } else {
    final_plot <- subplot(
      plotList,
      nrows = length(plotList),
      titleY = TRUE,
      titleX = TRUE,
      shareX = TRUE,
      which_layout = "merge"
    )
  }

  if (renderEngine %in% "webgl") {
    final_plot <- final_plot %>% plotly::toWebGL()
  }

  final_plot
}

# MARK: get_compounds
#' @describeIn MassSpecResults_NonTargetAnalysis Extracts compounds from the MassSpecResults_NonTargetAnalysis object. If the MassSpecResults_NonTargetAnalysis object has groups, it averages the compounds across replicates, when `averaged = TRUE`.
#' @template arg-nts-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @template arg-averaged
#' @export
#'
get_compounds.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  averaged = TRUE
) {
  if (length(x$features) == 0) {
    warning("Features not found!")
    return(data.table::data.table())
  }

  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for targets!")
    return(data.table::data.table())
  }

  compounds <- fts$compounds

  if (!averaged && any(vapply(x$features, function(x) any(!is.na(x$group))))) {
    compounds <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$analysis <- y
        }
        z
      },
      compounds,
      fts$analysis
    )

    compounds <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$feature <- y
        }
        z
      },
      compounds,
      fts$feature
    )
  }

  compounds <- Map(
    function(z, y) {
      if (nrow(z) > 0) {
        z$polarity <- y
      }
      z
    },
    compounds,
    fts$polarity
  )

  compounds <- Map(
    function(z, y) {
      if (nrow(z) > 0) {
        z$rt <- y
      }
      z
    },
    compounds,
    fts$rt
  )

  compounds <- Map(
    function(z, y) {
      if (nrow(z) > 0) {
        z$mass <- y
      }
      z
    },
    compounds,
    fts$mass
  )

  compounds <- Map(
    function(z, y) {
      if (nrow(z) > 0) {
        z$group <- y
      }
      z
    },
    compounds,
    fts$group
  )

  compounds <- compounds[vapply(
    compounds,
    function(z) {
      nrow(z) > 0
    },
    FALSE
  )]

  compounds <- data.table::rbindlist(compounds, fill = TRUE)

  if (nrow(compounds) > 0) {
    if (averaged && any(vapply(x$features, function(z) !all(is.na(z$group) | z$group %in% ""), FALSE))) {
      data.table::setcolorder(
        compounds,
        c("group", "rt", "mass", "polarity", "compoundName")
      )
      duplos <- duplicated(paste0(
        compounds$group,
        compounds$compoundName,
        compounds$polarity
      ))
      compounds <- compounds[!duplos]
    } else {
      cols_order <- c(
        "analysis",
        "feature",
        "group",
        "rt",
        "mass",
        "polarity",
        "compoundName"
      )
      data.table::setcolorder(compounds, cols_order)
    }
  }
  compounds
}

# MARK: get_fold_change
#' @describeIn MassSpecResults_NonTargetAnalysis Gets a data.table with fold-change analysis between the `replicatesIn` and `replicatesOut`.
#'
#' @param replicatesIn Character vector with the names of the replicates to be considered as
#' the denominator.
#' @param replicatesOut Character vector with the names of the replicates to be considered as
#' the numerator.
#' @param constantThreshold Numeric of length one. The threshold to consider a feature as
#' constant.
#' @param eliminationThreshold Numeric of length one. The threshold to consider a feature as
#' eliminated.
#' @template arg-ms-correctIntensity
#' @param fillZerosWithLowerLimit Logical of length one. When `TRUE` the zero values are filled
#' with the lower limit.
#' @param lowerLimit Numeric of length one. The lower limit to fill the zero values.
#'
#' @export
#'
get_fold_change.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  correctIntensity = FALSE,
  fillZerosWithLowerLimit = FALSE,
  lowerLimit = NA_real_
) {
  if (!any(vapply(x$features, function(x) any(!is.na(x$group) | x$group %in% ""), FALSE))) {
    warning("\U2717 Feature groups not found!")
    return(NULL)
  }

  rpls <- x$info$replicate

  if (is.numeric(replicatesIn)) {
    replicatesIn <- unique(rpls[replicatesIn])
  }
  if (is.numeric(replicatesOut)) {
    replicatesOut <- unique(rpls[replicatesOut])
  }

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
    correctIntensity = correctIntensity
  )

  if (nrow(groups_dt) == 0) {
    message("\U2717 Feature groups not found for the targets!")
    return(NULL)
  }

  comb <- data.table::data.table()

  for (rep in seq_len(length(replicatesOut))) {
    out_temp <- names(x)[x$info$replicate %in% replicatesOut[rep]]
    in_temp <- names(x)[x$info$replicate %in% replicatesIn[rep]]
    comb_temp <- expand.grid(
      analysisIn = in_temp,
      analysisOut = out_temp,
      replicateIn = replicatesIn[rep],
      replicateOut = replicatesOut[rep]
    )
    comb <- data.table::rbindlist(list(comb, comb_temp), fill = TRUE)
  }

  if (nrow(comb) == 0) {
    warning(
      "\U2717 Combinations could not be made, check replicates IN and OUT!"
    )
    return(NULL)
  }

  fc <- lapply(
    seq_len(nrow(comb)),
    function(z, comb, groups_dt, fillZerosWithLowerLimit) {
      anaIn <- comb$analysisIn[z]
      anaOut <- comb$analysisOut[z]

      selOut <- colnames(groups_dt) %in% as.character(anaOut)
      vecOut <- groups_dt[, selOut, with = FALSE][[1]]

      selIn <- colnames(groups_dt) %in% as.character(anaIn)
      vecIn <- groups_dt[, selIn, with = FALSE][[1]]

      if (fillZerosWithLowerLimit) {
        if (is.na(lowerLimit)) {
          vecOut[vecOut == 0] <- min(vecOut[vecOut > 0])
          vecIn[vecIn == 0] <- min(vecIn[vecIn > 0])
        } else {
          vecOut[vecOut == 0] <- lowerLimit
          vecIn[vecIn == 0] <- lowerLimit
        }
      }

      fc_vec <- as.numeric(vecOut) / as.numeric(vecIn)

      res <- data.table::data.table("group" = groups_dt$group, "fc" = fc_vec)
      res$analysis_in <- anaIn
      res$analysis_out <- anaOut
      res$replicate_in <- comb$replicateIn[z]
      res$replicate_out <- comb$replicateOut[z]
      res$combination <- z
      res
    },
    comb = comb,
    groups_dt = groups_dt,
    fillZerosWithLowerLimit = fillZerosWithLowerLimit
  )

  fc <- data.table::rbindlist(fc)
  sel_nan <- is.nan(fc$fc)
  fc <- fc[!sel_nan, ]
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
    fc$category[
      fc$fc >= fc_category[[i]][1] &
        fc$fc <= fc_category[[i]][2]
    ] <- names(fc_category)[i]
  }
  sel_na_category <- is.na(fc$category)
  fc <- fc[!sel_na_category, ]
  fc$category <- factor(fc$category, levels = names(fc_category))
  fc$bondaries <- paste(fc$category, fc_boundaries[fc$category], sep = "\n")
  fc$bondaries <- factor(
    fc$bondaries,
    levels = paste(names(fc_category), fc_boundaries, sep = "\n")
  )
  fc
}

# MARK: plot_fold_change
#' @describeIn MassSpecResults_NonTargetAnalysis Plots the fold-change analysis between the `replicatesIn` and `replicatesOut`.
#'
#' #' @param replicatesIn Character vector with the names of the replicates to be considered as
#' the denominator.
#' @param replicatesOut Character vector with the names of the replicates to be considered as
#' the numerator.
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @param constantThreshold Numeric of length one. The threshold to consider a feature as
#' constant.
#' @param eliminationThreshold Numeric of length one. The threshold to consider a feature as
#' eliminated.
#' @template arg-ms-correctIntensity
#' @param fillZerosWithLowerLimit Logical of length one. When `TRUE` the zero values are filled
#' with the lower limit.
#' @param lowerLimit Numeric of length one. The lower limit to fill the zero values.
#' @template arg-normalized
#' @template arg-yLab
#' @template arg-title
#' @template arg-interactive
#' @template arg-showLegend
#' @export
#'
plot_fold_change.MassSpecResults_NonTargetAnalysis <- function(
  x,
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
  correctIntensity = FALSE,
  fillZerosWithLowerLimit = FALSE,
  lowerLimit = NA_real_,
  normalized = TRUE,
  yLab = NULL,
  title = NULL,
  interactive = TRUE,
  showLegend = TRUE
) {
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
    correctIntensity,
    fillZerosWithLowerLimit,
    lowerLimit
  )

  if (is.null(fc)) {
    return(NULL)
  }

  if (nrow(fc) == 0) {
    return(NULL)
  }

  fc_summary_count <- fc[,
    .(count = .N),
    by = c("combination", "bondaries", "replicate_out", "replicate_in")
  ]

  all_fts <- get_features_count(x)
  all_fts <- all_fts[all_fts$replicate %in% replicatesIn, ]

  unique_combinations_max <- unique(fc_summary_count[,
    c("combination", "replicate_out", "replicate_in"),
    with = FALSE
  ])

  unique_combinations_min <- unique_combinations_max

  unique_combinations_max$count <- vapply(
    unique_combinations_max$replicate_in,
    function(z, all_fts) {
      max(all_fts$groups[all_fts$replicate == z])
    },
    all_fts = all_fts,
    0
  )

  unique_combinations_min$count <- vapply(
    unique_combinations_min$replicate_in,
    function(z, all_fts) {
      min(all_fts$groups[all_fts$replicate == z])
    },
    all_fts = all_fts,
    0
  )

  unique_combinations_max$bondaries <- "Total\nfeatures in"
  unique_combinations_min$bondaries <- "Total\nfeatures in"

  fc_summary_count <- data.table::rbindlist(
    list(
      unique_combinations_max,
      unique_combinations_min,
      fc_summary_count
    ),
    use.names = TRUE
  )

  if (is.null(yLab)) {
    yLab <- "Number of feature groups"

    if (normalized) {
      yLab <- "Relative number of feature groups"
    }
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
    replicate_out <- NULL
    bondaries <- NULL
    fc_levels <- fc_summary_count[, .(replicate_out, bondaries)]
    fc_levels <- unique(fc_levels)

    colours <- .get_colors(unique(fc_levels$replicate_out))
    colours_key <- colours[fc_levels$replicate_out]

    if (normalized) {
      fc_summary_count$uid <- paste0(
        fc_summary_count$replicate_out,
        "_",
        fc_summary_count$combination
      )

      for (i in unique(fc_summary_count$uid)) {
        sel <- fc_summary_count$uid %in% i
        fc_summary_count$count[sel] <- fc_summary_count$count[sel] /
          max(fc_summary_count$count[sel])
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
      legend("topright", legend = names(colours), fill = colours)
    }
  } else {
    if (normalized) {
      fc_summary_count$uid <- paste0(
        fc_summary_count$replicate_out,
        "_",
        fc_summary_count$combination
      )
      for (i in unique(fc_summary_count$uid)) {
        sel <- fc_summary_count$uid %in% i
        fc_summary_count$count[sel] <- fc_summary_count$count[sel] /
          max(fc_summary_count$count[sel])
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
    fig <- fig %>%
      plotly::layout(
        title = title,
        xaxis = list(title = ""),
        yaxis = list(
          title = yLab,
          range = c(
            0,
            max(
              fc_summary_count$count
            ) *
              1.1
          )
        )
      )
    fig
  }
}

# MARK: patRoon Objects
# patRoon Objects -----

# MARK: get_patRoon_features
#' @describeIn MassSpecResults_NonTargetAnalysis  Creates an S4 class `features` or `featureGroups`from the \pkg{patRoon} package.
#' @template arg-nts-x
#' @template arg-ms-filtered
#' @param featureGroups Logical of length one. When `TRUE` the `featureGroups` class is returned.
#' @export
#'
get_patRoon_features.MassSpecResults_NonTargetAnalysis <- function(
  x,
  filtered = FALSE,
  featureGroups = TRUE
) {
  if (length(x$features) == 0) {
    warning("No features found to get!")
    return(NULL)
  }

  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  feature_list <- x$features

  feature_list <- lapply(feature_list, function(z) {
    if (!filtered) {
      z <- z[!z$filtered, ]
    }
    data.table::setnames(z, "feature", "ID", skip_absent = TRUE)
    data.table::setnames(z, "rt", "ret", skip_absent = TRUE)
    data.table::setnames(z, "rtmin", "retmin", skip_absent = TRUE)
    data.table::setnames(z, "rtmax", "retmax", skip_absent = TRUE)
    z
  })

  feature_list <- feature_list[vapply(feature_list, nrow, 0) > 0]

  ana_info <- x$info
  ana_info <- ana_info[ana_info$analysis %in% names(feature_list), ]
  pols <- x$info$polarity
  pols[pols == "1"] <- "positive"
  pols[pols == "-1"] <- "negative"
  ana_info$path <- dirname(ana_info$file)
  data.table::setnames(ana_info, "replicate", "group", skip_absent = TRUE)
  data.table::setcolorder(
    ana_info,
    c("path", "analysis", "group", "blank", "polarity")
  )

  make_set <- FALSE

  if (length(unique(pols)) > 1) {
    make_set <- TRUE
  }

  if (
    any(vapply(x$features, function(x) any(!is.na(x$group)), FALSE)) &&
      featureGroups
  ) {
    feature_list <- lapply(feature_list, function(z) {
      z <- z[!is.na(z$group), ]
      z$index <- seq_len(nrow(z))
      z
    })

    pat <- new(
      "featuresOpenMS",
      features = feature_list,
      analysisInfo = ana_info
    )

    pat@features <- lapply(pat@features, function(z) {
      if ("name" %in% colnames(z)) {
        z$name <- NULL
      }
      if ("index" %in% colnames(z)) {
        z$index <- NULL
      }
      z
    })

    if (make_set) {
      pat_set <- patRoon::makeSet(
        pat[pols %in% "negative"],
        pat[pols %in% "positive"],
        adducts = list("[M-H]-", "[M+H]+")
      )
    }

    fts <- data.table::rbindlist(feature_list, idcol = "analysis")

    intensity <- NULL
    groups <- fts[, .(intensity = intensity), by = c("group", "analysis")]
    groups <- data.table::dcast(
      groups,
      analysis ~ group,
      value.var = "intensity",
      fill = 0
    )
    if (make_set) {
      groups <- groups[match(pat_set@analysisInfo$analysis, groups$analysis), ]
    } else {
      groups <- groups[match(pat@analysisInfo$analysis, groups$analysis), ]
    }
    groups$analysis <- NULL

    index <- NULL
    ftindex <- fts[, .(index = index), by = c("group", "analysis")]
    ftindex <- data.table::dcast(
      ftindex,
      analysis ~ group,
      value.var = "index",
      fill = 0
    )
    if (make_set) {
      ftindex <- ftindex[
        match(pat_set@analysisInfo$analysis, ftindex$analysis),
      ]
    } else {
      ftindex <- ftindex[match(pat@analysisInfo$analysis, ftindex$analysis), ]
    }
    ftindex$analysis <- NULL

    mass <- NULL
    ret <- NULL
    groups_info <- fts[,
      .(
        mass = round(mean(mass), digits = 4),
        ret = round(mean(ret), digits = 0)
      ),
      by = c("group")
    ]
    groups_info_rows <- groups_info$group
    groups_info[["group"]] <- NULL
    groups_info <- as.data.frame(groups_info)
    rownames(groups_info) <- groups_info_rows
    colnames(groups_info) <- c("mzs", "rts") # Note that here the mzs is still neutral mass

    data.table::setcolorder(groups, groups_info_rows)
    data.table::setcolorder(ftindex, groups_info_rows)

    if (make_set) {
      fg <- new(
        "featureGroupsOpenMS",
        groups = groups,
        analysisInfo = pat_set@analysisInfo,
        groupInfo = groups_info,
        features = pat_set,
        ftindex = ftindex
      )

      fg_set <- patRoon::featureGroupsSet(
        groupAlgo = "openms",
        groupArgs = list(),
        groupVerbose = FALSE,
        groups = patRoon::groupTable(fg),
        groupInfo = patRoon::groupInfo(fg),
        analysisInfo = patRoon::analysisInfo(fg),
        features = patRoon::getFeatures(fg),
        ftindex = patRoon::groupFeatIndex(fg),
        algorithm = "openms-set"
      )

      fg_set@annotations <- patRoon:::getAnnotationsFromSetFeatures(fg_set)

      return(fg_set)
    } else {
      if (unique(pols) %in% "positive") {
        groups_info$mzs <- groups_info$mzs + 1.007276
      } else if (unique(pols) %in% "negative") {
        groups_info$mzs <- groups_info$mzs - 1.007276
      } else {
        stop("Polarity should be defined as positive or negative!")
      }

      fg <- new(
        "featureGroupsOpenMS",
        groups = groups,
        analysisInfo = pat@analysisInfo,
        groupInfo = groups_info,
        features = pat,
        ftindex = ftindex
      )

      return(fg)
    }
  } else {
    pat <- new(
      "featuresOpenMS",
      features = feature_list,
      analysisInfo = ana_info
    )

    if (make_set) {
      pat <- patRoon::makeSet(
        pat[pols %in% "negative"],
        pat[pols %in% "positive"],
        adducts = list("[M-H]-", "[M+H]+")
      )

      pat@analysisInfo <- pat@analysisInfo[order(pat@analysisInfo$analysis), ]

      pat@features <- pat@features[pat@analysisInfo$analysis]
    }

    pat@features <- lapply(pat@features, function(z) {
      if ("name" %in% colnames(z)) {
        z$name <- NULL
      }
      if ("index" %in% colnames(z)) {
        z$index <- NULL
      }
      z
    })

    return(pat)
  }
}

# MARK: get_patRoon_MSPeakLists
#' @describeIn MassSpecResults_NonTargetAnalysis Creates S4 class `MSPeakLists`. Note that feature groups are required. The MS and MSMS spectra of each feature are then average by \pkg{patRoon} to produce the feature group spectra.
#' @template arg-nts-x
#' @template arg-ms-mzClust
#' @template arg-ms-minIntensity
#' @template arg-ms-presence
#' @template arg-ms-top
#' @template arg-normalized
#' @export
#'
get_patRoon_MSPeakLists.MassSpecResults_NonTargetAnalysis <- function(
  x,
  mzClust = 0.005,
  minIntensity = 0,
  presence = 0.7,
  top = 25,
  normalized = FALSE
) {
  if (length(x$features) == 0) {
    warning("No features found to get!")
    return(NULL)
  }

  if (
    !any(vapply(
      x$features,
      function(x) {
        if ("ms2" %in% colnames(x)) {
          any(vapply(
            x$ms2,
            function(z) {
              length(z) > 0
            },
            FALSE
          ))
        } else {
          FALSE
        }
      },
      FALSE
    ))
  ) {
    warning("No MS2 features found to get!")
    return(NULL)
  }

  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  parameters <- patRoon::getDefAvgPListParams()
  parameters$clusterMzWindow <- mzClust
  parameters$minIntensityPre <- minIntensity
  parameters$minIntensityPost <- minIntensity
  parameters$topMost <- top

  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  correct_spectrum <- function(s, t, out) {
    if (length(s) > 1) {
      s <- s[1]
    }

    names(s) <- t

    if (length(s[[1]]) > 0) {
      n_traces <- nrow(s[[1]])

      if (n_traces > 0) {
        s[[1]]$id <- seq_len(n_traces)
        if (!"is_pre" %in% colnames(s[[1]])) {
          s[[1]]$is_pre <- rep(FALSE, n_traces)
        }
        cols_to_keep <- c("id", "mz", "intensity", "is_pre")
        s[[1]] <- s[[1]][, cols_to_keep, with = FALSE]
        colnames(s[[1]]) <- c("ID", "mz", "intensity", "precursor")
      } else {
        s <- NULL
      }
    } else {
      s <- NULL
    }

    out <- c(out, s)
    out
  }

  feature_list <- x$features

  plist <- lapply(
    feature_list,
    function(z, correct_spectrum) {
      features <- z[!z$filtered, ]
      groups <- unique(features$group)
      groups <- groups[!is.na(groups)]
      if (length(groups) == 0) {
        return(NULL)
      }
      glist <- lapply(
        groups,
        function(z2, features, correct_spectrum) {
          out <- list()
          MS <- features$ms1[features$group %in% z2]
          if (length(MS) > 1) {
            MS <- MS[1]
          }
          if (!is.null(MS[[1]])) {
            if (!"is_pre" %in% colnames(MS[[1]])) {
              MS[[1]]$is_pre <- rep(FALSE, nrow(MS[[1]]))
            }
            t_mz_min <- features$mzmin[features$group %in% z2]
            t_mz_max <- features$mzmax[features$group %in% z2]

            MS[[1]]$is_pre <- vapply(
              MS[[1]]$mz,
              function(z3, t_mz_min, t_mz_max) {
                z3 >= t_mz_min - (mzClust / 2) & z3 <= t_mz_max + (mzClust / 2)
              },
              t_mz_min = t_mz_min,
              t_mz_max = t_mz_max,
              FALSE
            )
          }
          MSMS <- features$ms2[features$group %in% z2]
          out <- correct_spectrum(MS, "MS", out)
          out <- correct_spectrum(MSMS, "MSMS", out)
          out
        },
        features = features,
        correct_spectrum = correct_spectrum
      )

      names(glist) <- groups
      glist = glist[order(names(glist))]
      glist
    },
    correct_spectrum = correct_spectrum
  )

  names(plist) <- x$info$analysis
  plist <- plist[vapply(
    plist,
    function(x) {
      length(x) > 0
    },
    FALSE
  )]

  run_list <- lapply(x$info$file, function(z) {
    rcpp_parse_ms_spectra_headers(z)
  })

  mlist <- Map(
    function(z, y) {
      features <- z[!z$filtered, ]
      groups <- unique(features$group)
      groups <- groups[!is.na(groups)]
      pol_col <- as.character(y$polarity)
      pol_key = c(1, 0, -1)
      names(pol_key) <- c("1", "-1", "0")
      y$polarity <- pol_key[pol_col]
      setnames(
        y,
        c("index", "level", "ce", "pre_mz"),
        c("seqNum", "msLevel", "collisionEnergy", "precursorMZ"),
        skip_absent = TRUE
      )

      glist <- lapply(
        groups,
        function(z2, features, y) {
          out <- list()
          ft <- features[features$group %in% z2, ]
          if (nrow(ft) > 0) {
            MS <- y[y$rt >= ft$rtmin & y$rt <= ft$rtmax & y$msLevel == 1, ]
            if (nrow(MS) > 0) {
              out[["MS"]] <- MS
            }
            MSMS <- y[
              y$rt >= ft$rtmin &
                y$rt <= ft$rtmax &
                y$precursorMZ >= ft$mzmin - 1.3 / 2 &
                y$precursorMZ <= ft$mzmax + 1.3 / 2 &
                y$msLevel == 2,
            ]
            if (nrow(MSMS) > 0) {
              out[["MSMS"]] <- MSMS
            }
          }
          out
        },
        features = features,
        y = y
      )

      names(glist) <- groups
      glist = glist[order(names(glist))]
      glist
    },
    feature_list,
    run_list
  )

  names(mlist) <- x$info$analysis
  mlist <- mlist[vapply(
    mlist,
    function(x) {
      length(x) > 0
    },
    FALSE
  )]
  mlist <- mlist[names(plist)]

  groups <- lapply(feature_list[names(plist)], function(z) {
    z$group[!z$filtered]
  })
  groups <- unique(unlist(groups))
  groups <- groups[!is.na(groups)]

  av_ms1 <- get_groups_ms1(
    x,
    groups = groups,
    useLoadedData = TRUE,
    mzClust = mzClust,
    presence = presence,
    minIntensity = minIntensity,
    top = top,
    normalized = normalized
  )

  av_ms2 <- get_groups_ms2(
    x,
    groups = groups,
    useLoadedData = TRUE,
    mzClust = mzClust,
    presence = presence,
    minIntensity = minIntensity,
    top = top,
    normalized = normalized
  )

  av_plist <- lapply(
    groups,
    function(z, av_ms2) {
      out <- list()

      if (nrow(av_ms1) == 0) {
        out[["MS"]] <- NULL
      } else {
        temp_ms1 <- av_ms1[av_ms1$group %in% z, ]
        if (nrow(temp_ms1) == 0) {
          out[["MS"]] <- NULL
        } else {
          temp_ms1$ID <- seq_len(nrow(temp_ms1))
          data.table::setnames(temp_ms1, "is_pre", "precursor")
          temp_ms1 <- temp_ms1[,
            c("ID", "mz", "intensity", "precursor"),
            with = FALSE
          ]
          out[["MS"]] <- temp_ms1
        }
      }

      if (nrow(av_ms2) == 0) {
        out[["MSMS"]] <- NULL
      } else {
        temp_ms2 <- av_ms2[av_ms2$group %in% z, ]
        if (nrow(temp_ms2) == 0) {
          out[["MSMS"]] <- NULL
        } else {
          temp_ms2$ID <- seq_len(nrow(temp_ms2))
          data.table::setnames(temp_ms2, "is_pre", "precursor")
          temp_ms2 <- temp_ms2[,
            c("ID", "mz", "intensity", "precursor"),
            with = FALSE
          ]
          out[["MSMS"]] <- temp_ms2
        }
      }

      out
    },
    av_ms2 = av_ms2
  )

  names(av_plist) <- groups

  pat_param <- list(
    "clusterMzWindow" = parameters$clusterMzWindow,
    "topMost" = parameters$topMost,
    "minIntensityPre" = parameters$minIntensityPre,
    "minIntensityPost" = parameters$minIntensityPost,
    "avgFun" = parameters$avgFun,
    "method" = parameters$method,
    "pruneMissingPrecursorMS" = TRUE,
    "retainPrecursorMSMS" = TRUE
  )

  ana_info <- x$info[x$info$analysis %in% names(plist), ]
  pol <- ana_info$polarity
  ana_info$path <- dirname(ana_info$file)
  data.table::setnames(ana_info, "replicate", "group", skip_absent = TRUE)
  ana_info$set <- ana_info$polarity
  ana_info$polarity <- NULL
  data.table::setcolorder(
    ana_info,
    c("path", "analysis", "group", "blank", "set")
  )

  if (length(unique(pol)) > 1) {
    warning(
      "Different polarities found in the analyses. Conversion not yet implemented!"
    )
    return(NULL)

    plist_pos <- plist[pol %in% "positive"]
    mlist_pos <- mlist[pol %in% "positive"]
    groups_pos <- unique(unlist(lapply(plist_pos, function(x) {
      names(x)
    })))

    pl_pos <- new(
      "MSPeakLists",
      peakLists = plist_pos,
      metadata = mlist_pos,
      avgPeakListArgs = pat_param,
      origFGNames = groups_pos,
      algorithm = "mzr"
    )

    plist_neg <- plist[pol %in% "negative"]
    mlist_neg <- mlist[pol %in% "negative"]
    groups_neg <- unique(unlist(lapply(plist_neg, function(x) {
      names(x)
    })))

    pl_neg <- new(
      "MSPeakLists",
      peakLists = plist_neg,
      metadata = mlist_neg,
      avgPeakListArgs = pat_param,
      origFGNames = groups_neg,
      algorithm = "mzr"
    )

    plistComb <- Reduce(
      modifyList,
      lapply(
        list("positive" = pl_pos, "negative" = pl_neg),
        patRoon::peakLists
      )
    )

    plfinal <- new(
      "MSPeakListsSet",
      analysisInfo = ana_info,
      peakLists = plist,
      metadata = mlist,
      avgPeakListArgs = pat_param,
      origFGNames = unique(groups),
      algorithm = "mzr-set",
      setObjects = list("positive" = pl_pos, "negative" = pl_neg)
    )
  } else {
    plfinal <- new(
      "MSPeakLists",
      doAverage = FALSE,
      peakLists = plist,
      metadata = mlist,
      averagedPeakLists = av_plist,
      avgPeakListArgs = pat_param,
      origFGNames = groups,
      algorithm = "mzr"
    )
  }

  plfinal
}

# MARK: get_patRoon_compounds
#' @describeIn MassSpecResults_NonTargetAnalysis Creates an S4 class `Compounds` from the \pkg{patRoon} package.
#' @template arg-nts-x
#' @template arg-ms-filtered
#' @export
#'
get_patRoon_compounds.MassSpecResults_NonTargetAnalysis <- function(x) {
  comp <- get_compounds(x, filtered = FALSE)

  if (nrow(comp) == 0) {
    warning("No compounds found to get!")
    return(NULL)
  }

  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  comp_split <- comp$group
  comp$group <- NULL
  comp$rt <- NULL
  comp$polarity <- NULL
  comp$mass <- NULL
  comp <- split(comp, comp_split)

  scoreRanges <- lapply(comp, function(z) {
    scores <- lapply(z, function(k) {
      if (is.numeric(k)) {
        return(c(min(k), max(k)))
      } else {
        NULL
      }
    })

    scores <- scores[!sapply(scores, is.null)]
  })

  pat_comp <- patRoon::compounds(
    MS2QuantMeta = list(),
    groupAnnotations = comp,
    scoreTypes = names(scoreRanges[[1]]),
    scoreRanges = scoreRanges,
    algorithm = "metfrag"
  )

  pat_comp
}

# MARK: report
#' @export
#'
report.MassSpecResults_NonTargetAnalysis <- function(
  x,
  filtered = FALSE,
  mzClust = 0.005,
  minIntensity = 10,
  presence = 0.7,
  top = 25,
  normalized = TRUE,
  path = paste0(getwd(), "/report"),
  settingsFile = system.file("report", "settings.yml", package = "patRoon"),
  eicRtWindow = 30,
  eicTopMost = 1,
  eicTopMostByRGroup = TRUE,
  eicOnlyPresent = TRUE,
  eicMzExpWindow = 0.001,
  adductPos = "[M+H]+",
  adductNeg = "[M-H]-",
  specSimMethod = "cosine",
  specSimRemovePrecursor = FALSE,
  specSimMzWeight = 0,
  specSimIntWeight = 1,
  specSimAbsMzDev = 0.005,
  specSimRelMinIntensity = 0.05,
  specSimMinPeaks = 1,
  specSimShift = "none",
  specSimCombineMethod = "mean",
  clearPath = FALSE,
  openReport = TRUE,
  parallel = TRUE
) {
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found!")
    return(NULL)
  }

  if (
    !any(vapply(
      x$features,
      function(x) {
        if ("group" %in% colnames(x)) {
          any(!is.na(x$group))
        } else {
          FALSE
        }
      },
      FALSE
    ))
  ) {
    warning("No feature groups found to report!")
    return(NULL)
  }

  pat <- get_patRoon_features(x, filtered = filtered, featureGroups = TRUE)

  mspl <- get_patRoon_MSPeakLists(
    x,
    mzClust = mzClust,
    minIntensity = minIntensity,
    presence = presence,
    top = top,
    normalized = normalized
  )

  comp <- get_patRoon_compounds(x)

  patRoon::report(
    pat,
    MSPeakLists = mspl,
    formulas = NULL,
    compounds = comp,
    compsCluster = NULL,
    components = NULL,
    TPs = NULL,
    settingsFile = settingsFile,
    path = path,
    EICParams = list(
      rtWindow = eicRtWindow,
      topMost = eicTopMost,
      topMostByRGroup = eicTopMostByRGroup,
      onlyPresent = eicOnlyPresent,
      mzExpWindow = eicMzExpWindow,
      setsAdductPos = adductPos,
      setsAdductNeg = adductNeg
    ),
    specSimParams = list(
      method = specSimMethod,
      removePrecursor = specSimRemovePrecursor,
      mzWeight = specSimMzWeight,
      intWeight = specSimIntWeight,
      absMzDev = specSimAbsMzDev,
      relMinIntensity = specSimRelMinIntensity,
      minPeaks = specSimMinPeaks,
      shift = specSimShift,
      setCombineMethod = specSimCombineMethod
    ),
    clearPath = clearPath,
    openReport = openReport,
    parallel = parallel,
    overrideSettings = list()
  )

  message("\U2713 Report generated!")
}

# MARK: Utility functions
# Utility functions -----

# MARK: .add_features_column
#' @noRd
.add_features_column <- function(
  MassSpecResults_NonTargetAnalysis = NULL,
  name = NULL,
  data = NULL
) {
  if (!is(MassSpecResults_NonTargetAnalysis, "StreamFind::MassSpecResults_NonTargetAnalysis")) {
    warning(
      "MassSpecResults_NonTargetAnalysis object is not of class MassSpecResults_NonTargetAnalysis! Not done."
    )
    return(MassSpecResults_NonTargetAnalysis)
  }
  if (MassSpecResults_NonTargetAnalysis@has_features) {
    feature_list <- MassSpecResults_NonTargetAnalysis@feature_list
    feature_list <- Map(
      function(x, y) {
        if (nrow(x) == length(y)) {
          x[[name]] <- y
        }
        x
      },
      feature_list,
      data
    )
    MassSpecResults_NonTargetAnalysis$feature_list <- feature_list
  } else {
    warning("No features found! Not done.")
  }
  MassSpecResults_NonTargetAnalysis
}

# MARk: .make_features_hover_string
#' @noRd
.make_features_hover_string <- function(pk = data.table::data.table()) {
  if (nrow(pk) == 0) {
    return("")
  }
  has_quality <- any(vapply(
    pk[["quality"]],
    function(z) {
      nrow(z) > 0
    },
    logical(1)
  ))
  has_annotation <- any(vapply(
    pk[["annotation"]],
    function(z) {
      nrow(z) > 0
    },
    logical(1)
  ))

  hT <- paste(
    if ("var" %in% colnames(pk)) {
      paste("</br>", pk[["var"]])
    } else {
      ""
    },
    "</br> feature: ",
    pk[["feature"]],
    if ("group" %in% colnames(pk)) {
      paste("</br> group: ", pk[["group"]])
    } else {
      ""
    },
    "</br> analysis: ",
    pk[["analysis"]],
    "</br> replicate: ",
    pk[["replicate"]],
    "</br> mass: ",
    round(pk[["mass"]], digits = 4),
    "</br> <i>m/z</i>: ",
    round(pk[["mz"]], digits = 4),
    "</br> dppm: ",
    round(((pk[["mzmax"]] - pk[["mzmin"]]) / pk[["mz"]]) * 1E6, digits = 0),
    "</br> rt: ",
    round(pk[["rt"]], digits = 0),
    "</br> drt: ",
    round(pk[["rtmax"]] - pk[["rtmin"]], digits = 0),
    "</br> intensity: ",
    round(pk[["intensity"]], digits = 0),
    "</br> area: ",
    round(pk[["area"]], digits = 0),
    "</br> correction: ",
    round(pk[["correction"]], digits = 2),
    "</br> filtered: ",
    pk[["filtered"]],
    "</br> filter: ",
    pk[["filter"]],
    "</br> filled: ",
    pk[["filled"]],
    if (has_quality) {
      paste(
        "</br></br> Quality: ",
        "</br>   noise: ",
        vapply(
          pk[["quality"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["noise"]], digits = 0)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   sn: ",
        vapply(
          pk[["quality"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["sn"]], digits = 1)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   gaufit: ",
        vapply(
          pk[["quality"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["gauss_f"]], digits = 4)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   A: ",
        vapply(
          pk[["quality"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["gauss_a"]], digits = 2)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   mu: ",
        vapply(
          pk[["quality"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["gauss_u"]], digits = 2)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   sigma: ",
        vapply(
          pk[["quality"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["gauss_s"]], digits = 2)
            } else {
              NA_real_
            }
          },
          NA_real_
        )
      )
    } else {
      ""
    },
    if (has_annotation) {
      paste(
        "</br></br> Annotation: ",
        "</br>   component: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            if (nrow(z) > 0) {
              z[["component_feature"]]
            } else {
              NA_character_
            }
          },
          NA_character_
        ),
        "</br>   isotope: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            if (nrow(z) > 0) {
              z[["iso_cat"]]
            } else {
              NA_character_
            }
          },
          NA_character_
        ),
        "</br>   elements: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            z[["iso_isotope"]]
          },
          NA_character_
        ),
        "</br>   number_carbons: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["iso_number_carbons"]], digits = 0)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   iso_mass_error: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["iso_mass_distance_error"]], digits = 5)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   iso_time_error: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["iso_time_error"]], digits = 1)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   adduct: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            if (nrow(z) > 0) {
              z[["adduct_cat"]]
            } else {
              NA_character_
            }
          },
          NA_character_
        ),
        "</br>   adduct_mass_error: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["adduct_mass_error"]], digits = 5)
            } else {
              NA_real_
            }
          },
          NA_real_
        ),
        "</br>   adduct_time_error: ",
        vapply(
          pk[["annotation"]],
          function(z) {
            if (nrow(z) > 0) {
              round(z[["adduct_time_error"]], digits = 1)
            } else {
              NA_real_
            }
          },
          NA_real_
        )
      )
    } else {
      ""
    }
  )

  hT
}
