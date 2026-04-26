# MARK: MassSpecResults_Spectra S3 Class
#' @title Constructor and methods to handle mass spectrometry spectra results
#' @description The `MassSpecResults_Spectra` class is a child of [StreamFind::Results] and is used to store mass spectrometry spectra results.
#' @param spectra A list of spectra data.table objects.
#' @param replicates A character vector of replicate names.
#' @param is_averaged Logical indicating if the spectra are averaged.
#' @param is_neutralized Logical indicating if the spectra are neutralized.
#' @param peaks A list of data.table objects containing peak information for each spectrum.
#' @param charges A list of data.table objects containing charge information for each spectrum
#' @export
#' @seealso [StreamFind::Results]
#'
MassSpecResults_Spectra <- function(
  spectra = list(),
  replicates = character(),
  is_averaged = FALSE,
  is_neutralized = FALSE,
  peaks = list(),
  charges = list()
) {
  x <- structure(
    list(
      type = "MassSpec",
      name = "MassSpecResults_Spectra",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      spectra = spectra,
      is_averaged = is_averaged,
      peaks = peaks,
      replicates = replicates,
      is_neutralized = is_neutralized,
      charges = charges
    ),
    class = c("MassSpecResults_Spectra", "Results")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecResults_Spectra object!")
  }
}

#' @describeIn MassSpecResults_Spectra Validate the MassSpecResults_Spectra object, returning NULL if valid.
#' @template arg-ms-spec-x
#' @export
#'
validate_object.MassSpecResults_Spectra <- function(x) {
  checkmate::assert_logical(x$is_neutralized, max.len = 1)
  if (length(x$peaks) > 0) {
    for (peak in x$peaks) {
      checkmate::assert_data_frame(peak)
    }
  }
  if (length(x$charges) > 0) {
    for (charge in x$charges) {
      checkmate::assert_data_frame(charge)
    }
  }
  NextMethod()
  NULL
}

# MARK: Methods
# Methods ------

# MARK: show
#' @describeIn MassSpecResults_Spectra Show the MassSpecResults_Spectra object.
#' @template arg-ms-spec-x
#' @export
#'
show.MassSpecResults_Spectra <- function(x) {
  if (length(x$spectra) > 0) {
    cat("Number spectra: ", length(x$spectra), "\n")
    cat("Averaged: ", x$is_averaged, "\n")
    cat("Neutralized: ", x$is_neutralized, "\n")
    if (length(x$peaks) > 0) {
      cat("Number peaks: ", vapply(x$peaks, nrow, 0), "\n")
    } else {
      cat("Number peaks: ", 0, "\n")
    }
    if (length(x$charges) > 0) {
      cat("Number charges: ", vapply(x$charges, nrow, 0), "\n")
    }
  } else {
    cat("Number spectra: ", 0, "\n")
  }
}

# MARK: `[`
#' @describeIn MassSpecResults_Spectra Subset the MassSpecResults_Spectra object.
#' @template arg-ms-spec-x
#' @export
#'
`[.MassSpecResults_Spectra` <- function(x, i) {
  x$spectra <- x$spectra[i]
  if (length(x$peaks) > 0) {
    x$peaks <- x$peaks[i]
  }
  if (length(x$charges) > 0) {
    x$charges <- x$charges[i]
  }
  if (x$is_averaged) {
    x$replicates <- x$replicates[i]
  } else {
    x$replicates <- x$replicates[names(x$spectra)]
  }
  x
}

# MARK: get_spectra
#' @describeIn MassSpecResults_Spectra Get spectra from the MassSpecResults_Spectra object, returning a list of data.table objects.
#' @template arg-ms-spec-x
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @export
#'
get_spectra.MassSpecResults_Spectra <- function(
  x,
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
  minIntensityMS2 = 0
) {
  if (length(x$spectra) == 0) {
    warning("No spectra results available!")
    return(list())
  }

  analyses <- .check_analyses_argument(x$spectra, analyses)
  if (is.null(analyses)) {
    return(list())
  }

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

  if (x$is_averaged) {
    rpl <- unique(x$replicates)
    rpl <- rpl[rpl %in% analyses]
    x$spectra <- x$spectra[names(x$spectra) %in% rpl]
    x$spectra <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$analysis <- y
          z$replicate <- y
          data.table::setcolorder(z, c("analysis", "replicate"))
        }
        z
      },
      x$spectra,
      names(x$spectra)
    )
  } else {
    rpl <- x$replicates[analyses]
    x$spectra <- x$spectra[analyses]
    x$spectra <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$analysis <- y
          z$replicate <- rpl[y]
          data.table::setcolorder(z, c("analysis", "replicate"))
        }
        z
      },
      x$spectra,
      names(x$spectra)
    )
  }

  if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) {
    minIntensityMS1 <- 0
  }

  if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) {
    minIntensityMS2 <- 0
  }

  polarities <- vapply(
    x$spectra,
    function(z) {
      if ("polarity" %in% colnames(z)) {
        pol <- unique(z$polarity)
        pol <- paste(pol, collapse = ", ")
        pol
      } else {
        "0"
      }
    },
    "0"
  )

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

  num_cols <- c(
    "mz",
    "rt",
    "mobility",
    "mzmin",
    "mzmax",
    "rtmin",
    "rtmax",
    "mobilitymin",
    "mobilitymax"
  )

  if (
    all(
      apply(targets[, num_cols, with = FALSE], 1, function(z) {
        sum(z, na.rm = TRUE)
      }) !=
        0
    )
  ) {
    if (TRUE %in% is.na(targets$mz)) {
      targets$mz[is.na(targets$mz)] <- 0
    }
    if (TRUE %in% is.na(targets$mzmax)) {
      targets$mzmax[is.na(targets$mzmax)] <- max(vapply(
        x$spectra,
        function(z) max(z$mz),
        0
      ))
    }
    if (TRUE %in% is.na(targets$mzmin)) {
      targets$mzmin[is.na(targets$mzmin)] <- min(vapply(
        x$spectra,
        function(z) min(z$mz),
        0
      ))
    }
    if (TRUE %in% (targets$mzmax == 0)) {
      targets$mzmax[targets$mzmax == 0] <- max(vapply(
        x$spectra,
        function(z) max(z$mz),
        0
      ))
    }
    if (TRUE %in% is.na(targets$rt)) {
      targets$rt[is.na(targets$rt)] <- 0
    }
    if (TRUE %in% is.na(targets$rtmax)) {
      targets$rtmax[is.na(targets$rtmax)] <- max(vapply(
        x$spectra,
        function(z) max(z$rt),
        0
      ))
    }
    if (TRUE %in% is.na(targets$rtmin)) {
      targets$rtmin[is.na(targets$rtmin)] <- min(vapply(
        x$spectra,
        function(z) min(z$rt),
        0
      ))
    }
    if (TRUE %in% (targets$rtmax == 0)) {
      targets$rtmax[targets$rtmax == 0] <- max(vapply(
        x$spectra,
        function(z) max(z$rt),
        0
      ))
    }
    if (TRUE %in% is.na(targets$mobility)) {
      targets$mobility[is.na(targets$mobility)] <- 0
    }

    if (
      any(vapply(x$spectra, function(z) "mobility" %in% colnames(z), FALSE))
    ) {
      if (TRUE %in% is.na(targets$mobilitymax)) {
        targets$mobilitymax[is.na(targets$mobilitymax)] <- max(
          vapply(x$spectra, function(z) max(z$mobility), 0)
        )
      }
      if (TRUE %in% is.na(targets$mobilitymin)) {
        targets$mobilitymin[is.na(targets$mobilitymin)] <- min(
          vapply(x$spectra, function(z) min(z$mobility), 0)
        )
      }
      if (TRUE %in% (targets$mobilitymax == 0)) {
        targets$mobilitymax[targets$mobilitymax == 0] <- max(
          vapply(x$spectra, function(z) max(z$mobility), 0)
        )
      }
    }
  } else {
    targets <- data.table::data.table()
  }

  if (is.null(levels)) {
    levels <- vapply(
      x$spectra,
      function(z) {
        if ("level" %in% colnames(z)) {
          unique(z$level)
        } else {
          0
        }
      },
      0
    )
  }

  if (!2 %in% levels) {
    allTraces <- TRUE
  }

  if (!is.logical(allTraces)) {
    allTraces <- TRUE
  }

  if (nrow(targets) > 0) {
    if ("polarity" %in% colnames(targets)) {
      targets$polarity <- as.numeric(targets$polarity)
    }
    targets$precursor <- FALSE
    if (!allTraces) {
      if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) {
        isolationWindow <- 0
      }
      targets$precursor <- TRUE
      targets$mzmin <- targets$mzmin - (isolationWindow / 2)
      targets$mzmax <- targets$mzmax + (isolationWindow / 2)
      # TODO make case for DIA when pre_mz is not available
    }
  }

  spec_list <- lapply(
    x$spectra,
    function(temp, rpl, analyses) {
      with_im <- any(temp$mobility > 0)
      if (!is.null(levels) && "level" %in% colnames(temp)) {
        temp <- temp[temp$level %in% levels, ]
      }

      if (nrow(targets) > 0) {
        if ("analysis" %in% colnames(targets)) {
          if ("analysis" %in% colnames(temp)) {
            targets <- targets[targets$analysis %in% temp$analysis, ]
          } else {
            targets <- targets[rpl[targets$analysis] %in% temp$replicate, ]
          }
        }

        if (nrow(targets) > 0) {
          if (
            "polarity" %in% colnames(targets) && "polarity" %in% colnames(temp)
          ) {
            temp <- temp[temp$polarity %in% targets$polarity, ]
          }
          temp <- .trim_spectra_targets(temp, targets, with_im)
        } else {
          return(data.table::data.table())
        }
      }

      if (with_im) {
        temp$mobility <- NULL
      }
      if ("level" %in% colnames(temp)) {
        temp <- temp[!(temp$intensity <= minIntensityMS1 & temp$level == 1), ]
        temp <- temp[!(temp$intensity <= minIntensityMS2 & temp$level == 2), ]
      } else {
        temp <- temp[
          !(temp$intensity <= max(c(minIntensityMS1, minIntensityMS2))),
        ]
      }

      temp
    },
    rpl = rpl,
    analyses = analyses
  )

  names(spec_list) <- names(x$spectra)

  spec_list
}

# MARK: plot_spectra
#' @describeIn MassSpecResults_Spectra Plot spectra from the MassSpecResults_Spectra object.
#' @template arg-ms-spec-x
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @template arg-legendNames
#' @template arg-colorBy
#' @param xVal A character string indicating the x-axis variable. Options are "rt", "mz", "mass", or "mobility".
#' @template arg-labs
#' @template arg-title
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_spectra.MassSpecResults_Spectra <- function(
  x,
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
  legendNames = TRUE,
  colorBy = "analyses",
  xVal = "mz",
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  spec <- get_spectra(
    x,
    analyses,
    levels,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces = allTraces,
    isolationWindow,
    minIntensityMS1,
    minIntensityMS2
  )

  spec <- data.table::rbindlist(spec, fill = TRUE)

  if (nrow(spec) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }

  if (
    xVal == "mz" && (!"mz" %in% colnames(spec)) && "mass" %in% colnames(spec)
  ) {
    xVal <- "mass"
  }

  if (!xVal %in% colnames(spec)) {
    message("\U2717 xVal not found in spectra data.table!")
    return(NULL)
  }

  spec$xval <- spec[[xVal]]

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
  } else {
    stop(
      "Invalid xVal specified! It must be one of 'rt', 'mz', 'mass', or 'mobility'."
    )
  }

  if (is.null(yLab)) {
    yLab <- "Intensity / counts"
  }

  spec <- .make_colorBy_varkey(spec, colorBy, legendNames)

  spec$loop <- paste0(
    spec$analysis,
    spec$replicate,
    spec$id,
    spec$polarity,
    spec$level,
    spec$var
  )

  unique_key <- c(
    "analysis",
    "replicate",
    "id",
    "polarity",
    "level",
    "loop",
    "var",
    "xval"
  )
  unique_key <- unique_key[unique_key %in% colnames(spec)]
  intensity <- NULL
  spec <- spec[, .(intensity = max(intensity)), by = c(unique_key)]

  if (!"level" %in% colnames(spec)) {
    spec$level <- NA_integer_
  }

  cl <- .get_colors(unique(spec$var))

  data.table::setorder(spec, loop, xval)

  if (!interactive) {
    ggplot2::ggplot(spec, ggplot2::aes(x = xval, y = intensity, group = loop)) +
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

    plot <- spec %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~xval,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>id: ",
          id,
          # "<br>polarity: ", polarity,
          # "<br>level: ", level,
          "<br>",
          xVal,
          ": ",
          xval,
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
#' @describeIn MassSpecResults_Spectra Plot 3D spectra from the MassSpecResults_Spectra object.
#' @template arg-ms-spec-x
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @template arg-legendNames
#' @template arg-colorBy
#' @param xVal A character string indicating the x-axis variable. Options are "rt", "mz", "mass", or "mobility".
#' @param yVal A character string indicating the y-axis variable. Options are "rt", "mz", "mass", or "mobility".
#' @template arg-labs
#' @param zLab A character string indicating the z-axis label. Default is "Intensity / counts".
#' @template arg-renderEngine
#' @export
#'
plot_spectra_3d.MassSpecResults_Spectra <- function(
  x,
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
  legendNames = TRUE,
  colorBy = "analyses",
  xVal = "rt",
  yVal = "mz",
  xLab = NULL,
  yLab = NULL,
  zLab = NULL,
  renderEngine = "webgl"
) {
  spec <- get_spectra(
    x,
    analyses,
    levels,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces = allTraces,
    isolationWindow,
    minIntensityMS1,
    minIntensityMS2
  )

  spec <- data.table::rbindlist(spec, fill = TRUE)

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

  if (
    xVal == "mz" && (!"mz" %in% colnames(spec)) && "mass" %in% colnames(spec)
  ) {
    xVal <- "mass"
  }
  if (
    yVal == "mz" && (!"mz" %in% colnames(spec)) && "mass" %in% colnames(spec)
  ) {
    yVal <- "mass"
  }

  checkmate::assert_choice(xVal, c("rt", "mz", "mass", "mobility"))
  checkmate::assert_choice(yVal, c("rt", "mz", "mass", "mobility"))

  if (any(duplicated(c(xVal, yVal)))) {
    stop("Duplicated x and y values are not possible!")
  }

  xlab <- switch(
    xVal,
    "mz" = "<i>m/z</i> / Da",
    "rt" = "Retention time / seconds",
    "mobility" = "Mobility time / milliseconds"
  )

  ylab <- switch(
    yVal,
    "mz" = "<i>m/z</i> / Da",
    "rt" = "Retention time / seconds",
    "mobility" = "Mobility time / milliseconds"
  )

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

  spec <- .make_colorBy_varkey(spec, colorBy, legendNames)

  spec$rtmz <- paste(
    spec$id,
    spec$level,
    spec$polarity,
    spec$mz,
    spec$rt,
    spec$mobility,
    spec$analysis,
    sep = ""
  )

  spec_temp <- data.table::copy(spec)

  spec_temp$intensity <- 0

  spec <- rbind(spec, spec_temp)

  spec[["x"]] <- spec[[xVal[1]]]

  spec[["y"]] <- spec[[yVal[1]]]

  colors_var <- .get_colors(unique(spec$var))

  hover_text <- paste0(
    "<br>id: ",
    spec$id,
    "<br>analysis: ",
    spec$analysis,
    "<br>replicate: ",
    spec$replicate,
    "<br>level: ",
    spec$level,
    "<br>polarity: ",
    spec$polarity,
    "<br>rt: ",
    spec$rt,
    "<br>mz: ",
    spec$mz,
    "<br>mobility: ",
    spec$mobility,
    "<br>intensity: ",
    spec$intensity
  )

  fig <- plot_ly(spec, x = ~x, y = ~y, z = ~intensity) %>%
    group_by(spec$rtmz) %>%
    add_lines(
      color = ~var,
      colors = colors_var,
      hoverinfo = "text",
      text = hover_text
    )

  fig <- fig %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab),
        zaxis = list(title = zlab)
      )
    )

  # if (renderEngine %in% "webgl") {
  #   fig <- fig %>% plotly::toWebGL()
  # }

  fig
}

# MARK: plot_spectra_charges
#' @describeIn MassSpecResults_Spectra Plot spectra charges from the MassSpecResults_Spectra object.
#' @template arg-ms-spec-x
#' @template arg-analyses
#' @template arg-legendNames
#' @template arg-title
#' @template arg-colorBy
#' @template arg-labs
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_spectra_charges.MassSpecResults_Spectra <- function(
  x,
  analyses = NULL,
  legendNames = NULL,
  title = NULL,
  colorBy = "analyses",
  xLab = NULL,
  yLab = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  analyses <- .check_analyses_argument(x$spectra, analyses)
  if (is.null(analyses)) {
    return(NULL)
  }

  if (length(x$charges) == 0) {
    message("\U2717 Spectra charges not found!")
    return(NULL)
  }

  res <- x$charges

  if (x$is_averaged) {
    rpl <- x$replicates
    rpl <- rpl[rpl %in% analyses]
    res <- res[names(res) %in% names(rpl)]
    res <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$analysis <- y
          data.table::setcolorder(z, "analysis")
        }
        z
      },
      res,
      names(res)
    )
    res <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$replicate <- y
          data.table::setcolorder(z, c("analysis", "replicate"))
        }
        z
      },
      res,
      rpl
    )
  } else {
    rpl <- x$replicates[analyses]
    res <- res[analyses]
    res <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$analysis <- y
          z$replicate <- rpl[y]
          data.table::setcolorder(z, c("analysis", "replicate"))
        }
        z
      },
      res,
      names(res)
    )
  }

  res <- rbindlist(res, fill = TRUE)

  if (nrow(res) == 0) {
    message("\U2717 Spectra charges not found for the targets!")
    return(NULL)
  }

  res <- .make_colorBy_varkey(res, colorBy, legendNames)

  res$loop <- paste0(
    res$analysis,
    res$replicate,
    res$id,
    res$polarity,
    res$var
  )

  cl <- .get_colors(unique(res$var))

  if (!interactive) {
    if (is.null(xLab)) {
      xLab <- expression(italic("m/z ") / " Da")
    }
    if (is.null(yLab)) {
      yLab <- "Intensity / counts"
    }

    plot <- ggplot2::ggplot(
      res,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = mz, yend = 0, color = var),
        linewidth = 1
      )

    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(z, " - ", round(mz, digits = 3))),
        vjust = 0.2,
        hjust = -0.2,
        angle = 90,
        size = 2,
        show.legend = FALSE
      )

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(res$intensity) * 1.5)
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

    ticksMin <- plyr::round_any(min(res$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(res$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(res$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, max(res$intensity) * 1.5)
    )

    loop <- NULL

    plot <- res %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = 2)),
        text = ~ paste0(z, " - ", round(mz, digits = 3)),
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

# MARK: get_spectra_matrix
#' @describeIn MassSpecResults_Spectra Get spectra matrix from the MassSpecResults_Spectra object.
#' @template arg-ms-spec-x
#' @template arg-analyses
#' @export
#'
get_spectra_matrix.MassSpecResults_Spectra <- function(
  x,
  analyses = NULL
) {
  if (length(x$spectra) == 0) {
    warning("No spectra results object available!")
    return(matrix())
  }
  analyses <- .check_analyses_argument(x$spectra, analyses)
  spec_list <- x$spectra
  if (x$is_averaged) {
    rpl <- x$replicates
    rpl <- unique(rpl[rpl %in% analyses])
    spec_list <- spec_list[names(spec_list) %in% rpl]
  } else {
    spec_list <- spec_list[analyses]
  }
  intensity <- NULL
  spec_list <- spec_list[vapply(spec_list, function(z) nrow(z) > 0, FALSE)]

  spec_list <- lapply(spec_list, function(z) {
    if (!"bins" %in% colnames(z)) {
      if ("mass" %in% colnames(z)) {
        z$mz <- z$mass
      }
      if (!"level" %in% colnames(z)) {
        z$level <- 1
      }
      if ("mobility" %in% colnames(z)) {
        z$bins <- paste0(
          "r",
          z$rt,
          "_m",
          z$mz,
          "_d",
          z$mobility,
          "_p",
          z$polarity,
          "_l",
          z$level
        )
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

# MARK: get_spectra_peaks
#' @describeIn MassSpecResults_Spectra Get spectra peaks from the MassSpecResults_Spectra object.
#' @template arg-ms-spec-x
#' @template arg-analyses
#' @export
#'
get_spectra_peaks.MassSpecResults_Spectra <- function(
  x,
  analyses = NULL
) {
  analyses <- .check_analyses_argument(x$spectra, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }

  pks <- x$peaks
  if (length(pks) == 0) {
    return(data.table::data.table())
  }

  if (x$is_averaged) {
    rpl <- x$replicates
    rpl <- rpl[analyses]
    pks <- pks[names(pks) %in% unname(rpl)]
    pks <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$replicate <- y
          data.table::setcolorder(z, c("replicate"))
        }
        z
      },
      pks,
      names(pks)
    )
  } else {
    rpl <- x$replicates[analyses]
    pks <- pks[analyses]
    pks <- Map(
      function(z, y) {
        if (nrow(z) > 0) {
          z$analysis <- y
          z$replicate <- rpl[y]
          data.table::setcolorder(z, c("analysis", "replicate"))
        }
        z
      },
      pks,
      names(pks)
    )
  }

  pks <- data.table::rbindlist(pks, idcol = "analysis", fill = TRUE)

  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found for the analyses!")
    return(data.table::data.table())
  }

  pks
}

# MARK: plot_spectra_peaks
#' @describeIn MassSpecResults_Spectra Plot spectra peaks from the MassSpecResults_Spectra object.
#' @template arg-ms-spec-x
#' @template arg-analyses
#' @template arg-legendNames
#' @template arg-colorBy
#' @param xVal A character string indicating the x-axis variable. Options are "mz", "mass", or "mobility".
#' @template arg-labs
#' @template arg-title
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_spectra_peaks.MassSpecResults_Spectra <- function(
  x,
  analyses = NULL,
  legendNames = TRUE,
  colorBy = "analyses",
  xVal = "mz",
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  pks <- get_spectra_peaks(x, analyses)
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found!")
    return(NULL)
  }

  spec <- get_spectra(x, analyses)

  spec <- data.table::rbindlist(spec)

  if (nrow(spec) == 0) {
    message("\U2717 Spectra not found!")
    return(NULL)
  }

  pks$spec_id <- pks$id
  spec$spec_id <- spec$id

  if (grepl("targets", colorBy)) {
    pks$id <- pks$peak
  }

  checkmate::assert_choice(xVal, c("mz", "mass", "mobility"))

  if (
    xVal == "mz" && (!"mz" %in% colnames(spec)) && "mass" %in% colnames(spec)
  ) {
    xVal <- "mass"
  }

  if (!xVal %in% colnames(spec)) {
    message("\U2717 xVal not found in spectra data.table!")
    return(NULL)
  }

  pks$xval <- pks[[xVal]]
  spec$xval <- spec[[xVal]]

  if ("mz" %in% xVal) {
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

  if (is.null(yLab)) {
    yLab <- "Intensity / counts"
  }

  pks <- .make_colorBy_varkey(pks, colorBy, legendNames)

  cl <- .get_colors(unique(pks$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)

  if (!interactive) {
    plot <- ggplot2::ggplot(spec, ggplot2::aes(x = xval))

    for (i in seq_len(nrow(pks))) {
      pk_analysis <- pks[["analysis"]][i]
      pk_replicate <- pks[["replicate"]][i]
      pk_spec <- pks[["spec_id"]][i]
      pk_id <- pks[["peak"]][i]
      pk_var <- pks[["var"]][i]
      pk_min <- pks[["min"]][i]
      pk_max <- pks[["max"]][i]

      temp <- dplyr::filter(
        spec,
        analysis %in%
          pk_analysis &
          replicate %in% pk_replicate &
          spec_id %in% pk_spec
      )

      temp$var <- pk_var

      plot <- plot +
        ggplot2::geom_line(
          data = temp,
          ggplot2::aes(y = intensity, color = var)
        )

      temp <- temp[temp$xval >= pk_min & temp$xval <= pk_max, ]

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

    for (i in seq_len(nrow(pks))) {
      pk_analysis <- pks[["analysis"]][i]
      pk_replicate <- pks[["replicate"]][i]
      pk_spec <- pks[["spec_id"]][i]
      pk_id <- pks[["peak"]][i]
      pk_var <- pks[["var"]][i]
      pk_min <- pks[["min"]][i]
      pk_max <- pks[["max"]][i]
      pk_sn <- pks[["sn"]][i]

      temp <- dplyr::filter(
        spec,
        analysis %in%
          pk_analysis &
          replicate %in% pk_replicate &
          spec_id %in% pk_spec &
          xval >= pk_min &
          xval <= pk_max
      )

      plot <- plot %>%
        add_trace(
          data = temp,
          x = ~xval,
          y = ~intensity,
          type = "scatter",
          mode = "markers",
          marker = list(color = cl[pk_var], size = 5),
          text = ~ paste(
            "<br>analysis: ",
            pk_analysis,
            "<br>replicate: ",
            pk_replicate,
            "<br>spectrum: ",
            pk_spec,
            "<br>peak: ",
            pk_id,
            "<br>S/N: ",
            pk_sn,
            "<br>",
            xVal,
            ": ",
            round(xval, 3),
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
          data = temp,
          x = ~xval,
          ymin = ~ min(intensity),
          ymax = ~intensity,
          line = list(color = cl[pk_var], width = 1.5),
          fillcolor = cl50[pk_var],
          text = ~ paste(
            "<br>analysis: ",
            pk_analysis,
            "<br>replicate: ",
            pk_replicate,
            "<br>spectrum: ",
            pk_spec,
            "<br>peak: ",
            pk_id,
            "<br>S/N: ",
            pk_sn,
            "<br>",
            xVal,
            ": ",
            round(xval, 3),
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
      pk_var <- pks[["var"]][i]

      plot <- plot %>%
        add_trace(
          data = dplyr::filter(
            spec,
            analysis %in% pk_analysis & replicate %in% pk_replicate
          ),
          x = ~xval,
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

# MARK: plot_spectra_baseline
#' @export
#' @noRd
plot_spectra_baseline.MassSpecResults_Spectra <- function(
  x,
  analyses = NULL,
  legendNames = TRUE,
  colorBy = "analyses",
  xVal = "mz",
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  warning("Not yet implemented!")
  NULL
}
