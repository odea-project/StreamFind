#' @title plot_spectra_interactive
#'
#' @description 3D interactive plot for spectra using the \pkg{plotly} package.
#'
#' @param spectra A `data.table` with the at least columns *analysis*, *level*,
#' *rt*, *mz* and *intensity*.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#'
#' @return An interactive 3D plot.
#'
#' @export
#'
plot_spectra_interactive <- function(spectra = NULL, colorBy = "analyses") {
  if (!"id" %in% colnames(spectra)) spectra$id <- ""

  spectra$id <- factor(spectra$id)
  spectra$level <- paste("MS", spectra$level, sep = "")
  spectra$level <- factor(spectra$level)
  spectra$analysis <- factor(spectra$analysis)

  spectra$rtmz <- paste(
    spectra$id, spectra$level,
    spectra$mz, spectra$rt,
    spectra$analysis,
    sep = ""
  )

  spec_temp <- spectra
  spec_temp$intensity <- 0
  spectra <- rbind(spectra, spec_temp)

  if (colorBy == "levels") {
    spectra$var <- spectra$level
  } else if (colorBy == "targets") {
    spectra$var <- spectra$id
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(spectra)) {
    spectra$var <- spectra$replicate
  } else {
    spectra$var <- spectra$analysis
  }

  colors_var <- get_colors(unique(spectra$var))

  fig <- plot_ly(spectra, x = ~rt, y = ~mz, z = ~intensity) %>%
    group_by(spectra$rtmz) %>%
    add_lines(color = ~var, colors = colors_var)

  fig <- fig %>% plotly::layout(scene = list(
    xaxis = list(title = "Retention time / seconds"),
    yaxis = list(title = "<i>m/z</i>"),
    zaxis = list(title = "Intensity / counts")
  ))

  return(fig)
}

#' @title plot_xic_interactive
#'
#' @description Plot traces or profile data for targets using expected
#' \emph{m/z} and retention time pairs, including deviations.
#'
#' @param xic A data.table with at least columns: *mz*, *rt* and *intensity*.
#' @param legendNames A character vector with the same length as the unique ids
#' in the column *id* of the data.table `xic`.
#' @param plotTargetMark Logical, set to \code{TRUE} (the default) to plot a
#' target mark.
#' @param targetsMark A data.frame with columns "mz" and "rt", defining the
#' \emph{m/z} and retention time values of each targets (i.e., each unique id
#' in the `xic` table). Note that the number of rows should match with the
#' number of unique target ids.
#' @param ppmMark A numeric vector of length one to define the mass deviation,
#' in ppm, of the target mark. The default is 5 ppm.
#' @param secMark A numeric vector of length one to define the time deviation,
#' in seconds, of the target mark. The default is 10 ppm.
#' @param numberRows A numeric vector of length one to define the number of
#' rows to grid the plots. Note that each target is always plotted in one row
#' for all selected analyses.
#'
#' @return An iterative plot of the traces for the requested \emph{m/z} and
#' retention time pairs.
#'
#' @export
#'
plot_xic_interactive <- function(xic,
                                 legendNames = NULL,
                                 plotTargetMark = TRUE,
                                 targetsMark = NULL,
                                 ppmMark = 5,
                                 secMark = 10,
                                 numberRows = 1) {
  if (!"id" %in% colnames(xic)) xic$id <- NA_character_

  ids <- unique(xic$id)
  if (is.character(legendNames) & length(legendNames) == length(ids)) {
    names(legendNames) <- ids
    xic$id <- legendNames[xic$id]
  }

  if (plotTargetMark) {
    plotTargetMark <- FALSE
    if (is.data.frame(targetsMark)) {
      if (nrow(targetsMark) == length(ids) &
        "mz" %in% colnames(targetsMark) &
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
    title = paste("<i>m/z</i>"),
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

      if (plotTargetMark & is.numeric(temp$mz_id) & is.numeric(temp$rt_id)) {
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

  return(finalplot)
}

#' @title plot_eic_static
#'
#' @description Static plot of EIC using the \pkg{base} package.
#'
#' @param eic A data table with the analysis, id, rt, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @return An EIC static plot.
#'
#' @export
#'
plot_eic_static <- function(eic = NULL, legendNames = NULL, colorBy = "targets",
                            title = NULL) {
  if (!"id" %in% colnames(eic)) eic$id <- NA_character_

  if ("analyses" %in% colorBy) {
    leg <- unique(eic$analysis)
    varkey <- eic$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(eic)) {
    leg <- unique(eic$replicate)
    varkey <- eic$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(eic$id))) {
    leg <- legendNames
    names(leg) <- unique(eic$id)
    varkey <- leg[eic$id]
  } else {
    leg <- unique(eic$id)
    varkey <- eic$id
  }

  eic$var <- varkey

  cl <- get_colors(unique(eic$var))
  sp <- unique(eic$analysis)
  ids <- unique(eic$id)

  plot(eic$rt,
    type = "n",
    xlab = "Retention time / seconds",
    ylab = "Intensity / counts",
    xlim = c(min(eic$rt), max(eic$rt)),
    ylim = c(0, max(eic$intensity)),
    main = title
  )

  for (s in sp) {
    for (t in ids) {
      select_vector <- eic$analysis == s & eic$id == t
      lt <- unique(eic$var[select_vector])
      lines(
        x = eic$rt[select_vector],
        y = eic$intensity[select_vector],
        type = "l",
        pch = 19,
        cex = 0.5,
        col = cl[lt]
      )
      points(
        x = eic$rt[select_vector],
        y = eic$intensity[select_vector],
        type = "p",
        pch = 19,
        cex = 0.2,
        col = cl[lt]
      )
    }
  }

  legend(
    "topright",
    legend = names(cl),
    col = cl,
    lty = 1,
    cex = 0.8
  )
}

#' plot_eic_interactive
#'
#' @description Interactive plot of EIC using the \pkg{plotly} package.
#'
#' @param eic A data table with the analysis, id, rt, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @return An EIC interactive plot.
#'
#' @export
#'
plot_eic_interactive <- function(eic = NULL, legendNames = NULL,
                                 colorBy = "targets", title = NULL) {
  if (!"id" %in% colnames(eic)) eic$id <- NA_character_

  if ("analyses" %in% colorBy) {
    leg <- unique(eic$analysis)
    varkey <- eic$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(eic)) {
    leg <- unique(eic$replicate)
    varkey <- eic$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(eic$id))) {
    leg <- legendNames
    names(leg) <- unique(eic$id)
    varkey <- leg[eic$id]
  } else {
    leg <- unique(eic$id)
    varkey <- eic$id
  }

  eic$var <- varkey

  leg <- unique(eic$var)
  cl <- get_colors(leg)
  sp <- unique(eic$analysis)
  ids <- unique(eic$id)

  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "Retention time / seconds",
    titlefont = list(size = 12, color = "black")
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )

  plot <- plot_ly()
  showL <- rep(TRUE, length(leg))
  names(showL) <- leg

  for (s in sp) {
    for (t in ids) {
      select_vector <- eic$analysis == s & eic$id == t
      lt <- unique(eic$var[select_vector])
      y <- eic$intensity[select_vector]
      plot <- plot %>% add_trace(
        x = eic$rt[select_vector],
        y = y,
        type = "scatter", mode = "lines+markers",
        line = list(width = 0.5, color = unname(cl[lt])),
        marker = list(size = 2, color = unname(cl[lt])),
        name = lt,
        legendgroup = lt,
        showlegend = showL[lt],
        hovertemplate = paste("<br>rt: %{x}<br>", "intensity: %{y}")
      )
      if (length(y) >= 1) showL[lt] <- FALSE
    }
  }

  plot <- plot %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )

  return(plot)
}

#' @title plot_bpc_interactive
#'
#' @description Interactive plot of BPC using the \pkg{plotly} package.
#'
#' @param bpc A data table with the analysis, id, rt, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @return A BPC interactive plot.
#'
#' @export
#'
plot_bpc_interactive <- function(bpc = NULL, legendNames = NULL,
                                 colorBy = "targets", title = NULL) {
  if (!"id" %in% colnames(bpc)) bpc$id <- NA_character_

  if ("analyses" %in% colorBy) {
    leg <- unique(bpc$analysis)
    varkey <- bpc$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(bpc)) {
    leg <- unique(bpc$replicate)
    varkey <- bpc$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(bpc$id))) {
    leg <- legendNames
    names(leg) <- unique(bpc$id)
    varkey <- leg[bpc$id]
  } else {
    leg <- unique(bpc$id)
    varkey <- bpc$id
  }

  bpc$var <- varkey

  leg <- unique(bpc$var)
  cl <- get_colors(leg)
  sp <- unique(bpc$analysis)
  ids <- unique(bpc$id)

  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "Retention time / seconds",
    titlefont = list(size = 12, color = "black")
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )

  plot <- plot_ly()
  showL <- rep(TRUE, length(leg))
  names(showL) <- leg

  for (s in sp) {
    for (t in ids) {
      select_vector <- bpc$analysis == s & bpc$id == t
      lt <- unique(bpc$var[select_vector])
      y <- bpc$intensity[select_vector]
      plot <- plot %>% add_trace(
        x = bpc$rt[select_vector],
        y = y,
        type = "scatter", mode = "lines+markers",
        line = list(width = 0.5, color = unname(cl[lt])),
        marker = list(size = 2, color = unname(cl[lt])),
        name = lt,
        legendgroup = lt,
        showlegend = showL[lt],
        hovertemplate = paste(
          "<br>rt: %{x}<br>",
          "mz: ",
          round(bpc$mz[select_vector], digits = 4),
          "<br>", "intensity: %{y}"
        )
      )
      if (length(y) >= 1) showL[lt] <- FALSE
    }
  }

  plot <- plot %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )

  return(plot)
}

#' @title plot_ms2_static
#'
#' @description Static plot of MSn spectra using the \pkg{base} package.
#'
#' @param ms2 A data table with the id, mz, intensity, preMZ, isPre
#' and var (i.e., the plotting variable for each entry) as columns.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @return An MSn plot.
#'
#' @export
#'
plot_ms2_static <- function(ms2 = NULL, legendNames = NULL,
                            colorBy = "targets", title = NULL) {
  if ("analyses" %in% colorBy) {
    leg <- unique(ms2$analysis)
    varkey <- ms2$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(ms2)) {
    leg <- unique(ms2$replicate)
    varkey <- ms2$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(ms2$id))) {
    leg <- legendNames
    names(leg) <- unique(ms2$id)
    varkey <- leg[ms2$id]
  } else {
    leg <- unique(ms2$id)
    varkey <- ms2$id
  }

  ms2$var <- varkey

  cl <- get_colors(unique(ms2$var))

  ms2$var <- as.factor(ms2$var)

  ms2$color <- cl[ms2$var]

  ms2$text = paste0(round(ms2$mz, 4))

  plot(intensity ~ mz, ms2,
    type = "h",
    xlab = expression(italic("m/z")),
    ylab = "Intensity / counts",
    col = ms2$color,
    lwd = 2,
    ylim = c(0, max(ms2$intensity) * 1.5),
    main = title,
    yaxs = "i",
    xaxt = "n"
  )

  precursors <- ms2[ms2$isPre, ]

  for (s in seq_len(nrow(precursors))) {
    lines(
      x = rep(precursors$mz[s], 2),
      y = c(0, precursors$intensity[s]),
      type = "h",
      pch = 19,
      lwd = 5,
      cex = 0.5,
      col = precursors$color[s]
    )
  }

  text(
    x = ms2$mz, y = ms2$intensity, adj = c(-0.1, 0.25),
    labels = ms2$text, vfont = NULL,
    cex = 0.6, col = ms2$color, font = NULL, srt = 90
  )

  ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)

  axis(1, seq(ticksMin, ticksMax, 10), lwd = 1.5, cex.axis = 0.8)
  axis(1, seq(ticksMin, ticksMax, 5), labels = FALSE, lwd = 1, col = "darkgray")
  axis(1, seq(ticksMin, ticksMax, 2.5), labels = FALSE, lwd = 0.5, col = "darkgray")

  legend(
    "topright",
    legend = levels(ms2$var),
    col = cl,
    lty = 1,
    cex = 0.8
  )
}

#' @title plot_ms2_interactive
#'
#' @description Interactive plot of MS2 spectra using the \pkg{plotly} package.
#'
#' @param ms2 A data.table with the id, preMZ, mz, intensity,
#' isPre and var (i.e., the plotting variable for each entry) as columns.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @return An MS2 spectra interactive plot.
#'
#' @export
#'
plot_ms2_interactive <- function(ms2 = NULL, legendNames = NULL,
                                 colorBy = "targets", title = NULL) {
  if ("analyses" %in% colorBy) {
    leg <- unique(ms2$analysis)
    varkey <- ms2$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(ms2)) {
    leg <- unique(ms2$replicate)
    varkey <- ms2$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(ms2$id))) {
    leg <- legendNames
    names(leg) <- unique(ms2$id)
    varkey <- leg[ms2$id]
  } else {
    leg <- unique(ms2$id)
    varkey <- ms2$id
  }

  ms2$var <- varkey

  leg <- unique(ms2$var)

  cl <- get_colors(leg)

  plot <- plot_ly()

  for (v in leg) {
    data <- ms2[ms2$var == v, ]

    bar_widths <- rep(0.2, nrow(data))

    mz_text <- paste0(round(data$mz, digits = 4), "  ")

    if (TRUE %in% data$isPre) {
      mz_text[data$isPre] <- paste0("Pre ", mz_text[data$isPre])
      bar_widths[data$isPre] <- 4
    }

    plot <- plot %>% add_trace(
      data = data,
      x = data$mz,
      y = data$intensity,
      type = "bar",
      width = 0.05,
      marker = list(
        color = cl[v],
        line = list(color = cl[v], width = bar_widths)
      ),
      text = mz_text,
      textposition = "outside",
      textangle = 90,
      textfont = list(
        size = 9,
        color = rep(cl[v], length(data$mz))
      ),
      name = v,
      legendgroup = v,
      hovertemplate = paste("<i>m/z</i>: %{x:.4f}", "<br>intensity: %{y:.0f}")
    )
  }

  ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "<i>m/z<i>",
    titlefont = list(size = 12, color = "black"),
    range = c(ticksMin, ticksMax),
    dtick = round((max(ms2$mz) / 10), -1),
    ticks = "outside"
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12, color = "black"),
    range = c(0, max(ms2$intensity) * 1.5)
  )

  plot <- plot %>% plotly::layout(bargap = 1,
    title = title, xaxis = xaxis, yaxis = yaxis,
    barmode = "overlay", uniformtext = list(minsize = 6, mode = "show")
  )

  return(plot)
}

#' @title plot_ms1_static
#'
#' @description Static plot of spectra using the \pkg{base} package.
#'
#' @param ms1 A data.table with the analyses, id, mz, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @return An ms1 plot.
#'
#' @export
#'
plot_ms1_static <- function(ms1 = NULL, legendNames = NULL,
                            colorBy = "targets", title = NULL) {
  if ("analyses" %in% colorBy) {
    leg <- unique(ms1$analysis)
    varkey <- ms1$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(ms1)) {
    leg <- unique(ms1$replicate)
    varkey <- ms1$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(ms1$id))) {
    leg <- legendNames
    names(leg) <- unique(ms1$id)
    varkey <- leg[ms1$id]
  } else {
    leg <- unique(ms1$id)
    varkey <- ms1$id
  }

  ms1$var <- varkey

  cl <- get_colors(unique(ms1$var))

  ms1$var <- as.factor(ms1$var)

  ms1$color <- cl[ms1$var]

  ms1$text = paste0(round(ms1$mz, 4))

  plot(intensity ~ mz, ms1,
    type = "h",
    xlab = expression(italic("m/z")),
    ylab = "Intensity / counts",
    col = ms1$color,
    lwd = 2,
    ylim = c(0, max(ms1$intensity) * 1.5),
    main = title,
    yaxs = "i",
    xaxt = "n"
  )

  text(
    x = ms1$mz, y = ms1$intensity, adj = c(-0.1, 0.25),
    labels = ms1$text, vfont = NULL,
    cex = 0.6, col = ms1$color, font = NULL, srt = 90
  )

  ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)

  axis(1, seq(ticksMin, ticksMax, 10),
    lwd = 1.5, cex.axis = 0.8
  )

  axis(1, seq(ticksMin, ticksMax, 5),
    labels = FALSE, lwd = 1, col = "darkgray"
  )

  axis(1, seq(ticksMin, ticksMax, 2.5),
    labels = FALSE, lwd = 0.5, col = "darkgray"
  )

  legend(
    "topright",
    legend = levels(ms1$var),
    col = cl,
    lty = 1,
    cex = 0.8
  )
}

#' @title plot_ms1_interactive
#'
#' @description Interactive plot of MS1 ms1 using the \pkg{plotly} package.
#'
#' @param ms1 A data.table with the analyses, id, mz, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @return An ms1 interactive plot.
#'
#' @export
#'
plot_ms1_interactive <- function(ms1 = NULL, legendNames = NULL,
                                 colorBy = "targets", title = NULL) {
  if ("analyses" %in% colorBy) {
    leg <- unique(ms1$analysis)
    varkey <- ms1$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(ms1)) {
    leg <- unique(ms1$replicate)
    varkey <- ms1$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(ms1$id))) {
    leg <- legendNames
    names(leg) <- unique(ms1$id)
    varkey <- leg[ms1$id]
  } else {
    leg <- unique(ms1$id)
    varkey <- ms1$id
  }

  ms1$var <- varkey

  leg <- unique(ms1$var)

  cl <- get_colors(leg)

  plot <- plot_ly()

  for (v in leg) {
    data <- ms1[ms1$var == v, ]

    mz_text <- paste0(round(data$mz, digits = 4), "  ")

    plot <- plot %>% add_trace(
      x = data$mz,
      y = data$intensity,
      type = "bar",
      marker = list(
        color = cl[v],
        line = list(color = cl[v], width = 0.01)
      ),
      text = mz_text,
      textposition = "outside",
      textangle = 90,
      textfont = list(
        size = 9,
        color = rep(cl[v], length(data$mz))
      ),
      name = v,
      legendgroup = v,
      hovertemplate = paste("<i>m/z</i>: %{x:.4f}", "<br>intensity: %{y:.0f}")
    )
  }

  ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "<i>m/z<i>",
    titlefont = list(size = 12, color = "black"),
    range = c(ticksMin, ticksMax),
    dtick = round((max(ms1$mz) / 10), -1),
    ticks = "outside"
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12, color = "black"),
    range = c(0, max(ms1$intensity) * 1.5)
  )

  plot <- plot %>% plotly::layout(bargap = 1,
    title = title, xaxis = xaxis, yaxis = yaxis,
    barmode = "overlay", uniformtext = list(minsize = 6, mode = "show")
  )

  return(plot)
}

#' @title plot_features_static
#'
#' @description Static plot of features using the \pkg{base} package.
#'
#' @param eic A data table with the analysis, replicate,
#' id, rt, intensity and var (i.e., the plotting variables for each peak)
#' as columns.
#' @param features A data table with the individual features to plot.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @importFrom graphics axis legend lines points polygon
#'
#' @return A plot of chromatographic peaks.
#'
#' @export
#'
plot_features_static <- function(eic = NULL, features = NULL,
                                 legendNames = NULL, colorBy = "targets",
                                 title = NULL) {
  if ("analyses" %in% colorBy) {
    leg <- unique(eic$analysis)
    varkey <- eic$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(eic)) {
    leg <- unique(eic$replicate)
    varkey <- eic$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(eic$id))) {
    leg <- legendNames
    names(leg) <- unique(eic$id)
    varkey <- leg[eic$id]
  } else {
    leg <- unique(eic$id)
    varkey <- eic$id
  }

  eic$var <- varkey

  cl <- get_colors(unique(eic$var))

  ids <- unique(eic$id)

  plot(eic$rt,
    type = "n",
    xlab = "Retention time / seconds",
    ylab = "Intensity / counts",
    xlim = c(min(eic$rt), max(eic$rt)),
    ylim = c(0, max(eic$intensity)),
    main = title
  )

  for (t in ids) {
    select_vector <- eic$id == t
    lt <- unique(eic$var[select_vector])
    pk_eic <- eic[select_vector, ]
    pk_a <- features[features$feature == t, ]
    pk_eic_a <- pk_eic[
      pk_eic$rt >= pk_a$rtmin &
      pk_eic$rt <= pk_a$rtmax &
      pk_eic$id == t,
    ]
    points(
      x = pk_eic$rt,
      y = pk_eic$intensity,
      type = "p",
      pch = 19,
      cex = 0.2,
      col = cl[lt]
    )
    lines(
      x = pk_eic$rt,
      y = pk_eic$intensity,
      type = "l",
      pch = 19,
      cex = 0.3,
      col = cl[lt]
    )
    polygon(
      c(pk_eic_a$rt, rev(pk_eic_a$rt)),
      c(pk_eic_a$intensity, rep(0, length(pk_eic_a$intensity))),
      col = paste(color = unname(cl[lt]), 50, sep = ""),
      border = F
    )
    lines(
      x = rep(pk_a$rt, 2),
      y = c(0, pk_a$intensity),
      type = "l",
      pch = 19,
      cex = 0.6,
      col = cl[lt]
    )
  }

  legend(
    "topright",
    legend = names(cl),
    col = cl,
    lty = 1,
    cex = 0.8
  )
}

#' @title plot_features_interactive
#'
#' @description Plots chromatographic peaks with the package \pkg{plotly}.
#'
#' @param eic A data table with the analysis, replicate,
#' id, rt, intensity and var (i.e., the plotting variables for each peak)
#' as columns.
#' @param features A data table with the individual peaks to plot.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#' @param title A character vector to be used as title.
#'
#' @return A chromatographic peak plot through \pkg{plotly}.
#'
#' @export
#'
plot_features_interactive <- function(eic = NULL, features = NULL,
                                      legendNames = NULL, colorBy = "targets",
                                      title = NULL) {
  if ("analyses" %in% colorBy) {
    leg <- unique(eic$analysis)
    varkey <- eic$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(eic)) {
    leg <- unique(eic$replicate)
    varkey <- eic$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(eic$id))) {
    leg <- legendNames
    names(leg) <- unique(eic$id)
    varkey <- leg[eic$id]
  } else {
    leg <- unique(eic$id)
    varkey <- eic$id
  }

  eic$var <- varkey

  leg <- unique(eic$var)

  cl <- get_colors(leg)

  ids <- unique(eic$id)

  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "Retention time / seconds",
    range = c(min(eic$rt), max(eic$rt)),
    titlefont = list(size = 12, color = "black")
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )

  plot <- plot_ly()
  showL <- rep(TRUE, length(leg))
  names(showL) <- leg

  for (t in ids) {
    lt <- unique(eic$var[eic$id == t])
    y <- eic$intensity[eic$id == t]

    plot <- plot %>% add_trace(
      x = eic$rt[eic$id == t],
      y = y,
      type = "scatter", mode = "lines+markers",
      line = list(width = 0.3, color = unname(cl[lt])),
      marker = list(size = 2, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste("<br>rt: %{x}<br>", "int: %{y}")
    )
    if (length(y) >= 1) showL[lt] <- FALSE

    pk <- features[features$feature %in% t, ]
    pk_eic <- eic[eic$rt >= pk$rtmin & eic$rt <= pk$rtmax & eic$id == t, ]

    hT <- paste(
      "</br> feature: ", pk$feature,
      ifelse("group" %in% colnames(pk),
        paste("</br> group: ", pk$group), ""
      ),
      "</br> analysis: ", pk$analysis,
      "</br> <i>m/z</i>: ", round(pk$mz, digits = 4),
      "</br> dppm: ", round(((pk$mzmax - pk$mzmin) / pk$mz) * 1E6, digits = 0),
      "</br> rt: ", round(pk$rt, digits = 0),
      "</br> drt: ", round(pk$rtmax - pk$rtmin, digits = 0),
      "</br> intensity: ", round(pk$intensity, digits = 0),
      "</br> filled: ",
      if ("is_filled" %in% colnames(pk)) {
        ifelse(pk$is_filled == 1, TRUE, FALSE)
      } else {
        FALSE
      }
    )

    plot <- plot %>% add_trace(
      x = pk_eic$rt,
      y = pk_eic$intensity,
      type = "scatter", mode = "lines+markers",
      line = list(width = 0.6, color = unname(cl[lt])),
      fill = "tozeroy", connectgaps = TRUE,
      fillcolor = paste(color = unname(cl[lt]), 50, sep = ""),
      marker = list(size = 3, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = FALSE,
      hoverinfo = "text",
      text = hT
    )

    plot <- plot %>% add_segments(
      x = pk$rt,
      xend = pk$rt,
      y = 0,
      yend = pk$intensity,
      legendgroup = lt,
      showlegend = FALSE,
      line = list(color = unname(cl[lt]), size = 0.5),
      hoverinfo = "text",
      text = hT
    )
  }

  plot <- plot %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )

  return(plot)
}

#' @title map_features_static
#'
#' @description Function for plotting peak spaces.
#'
#' @param features A data table with the individual peak details to plot.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy Possible values are \code{"targets"} (the default),
#' \code{"analyses"} or \code{replicates}.
#' @param xlim A length one or two numeric vector for setting the \emph{x}
#' limits (in seconds) of the plot.
#' @param ylim A length one or two numeric vector for setting the \emph{m/z}
#' limits of the plot.
#' @param title An optional character vector to be used as title.
#' for coloring by target features, analyses or replicates, respectively.
#' @param showLegend Logical, set to \code{TRUE} to show legend.
#'
#' @return A peak/s map plot produced through \pkg{base} plot.
#'
#' @export
#'
map_features_static <- function(features, colorBy = "targets",
                                legendNames = NULL,
                                xlim = 60, ylim = 5,
                                title = NULL, showLegend = TRUE) {
  if ("analyses" %in% colorBy) {
    leg <- unique(features$analysis)
    varkey <- features$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(features)) {
    leg <- unique(features$replicate)
    varkey <- features$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(features$feature))) {
    leg <- legendNames
    names(leg) <- unique(features$feature)
    varkey <- leg[features$feature]
  } else {
    leg <- unique(features$feature)
    varkey <- features$feature
  }

  features$var <- varkey

  if (length(xlim) == 1) {
    rtr <- c(min(features$rtmin) - xlim, max(features$rtmax) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(features$rtmin), max(features$rtmax))
  }

  if (length(ylim) == 1) {
    mzr <- c(min(features$mzmin) - ylim, max(features$mzmax) + ylim)
  } else if (length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(features$mzmin), max(features$mzmax))
  }

  cl <- get_colors(unique(features$var))

  plot(features$rt,
    features$mz,
    type = "n",
    xlab = "Retention time / seconds",
    ylab = expression(italic("m/z")),
    xlim = rtr,
    ylim = mzr,
    main = title
  )

  rect(
    xleft = features$rtmin,
    xright = features$rtmax,
    ybottom = features$mzmin,
    ytop = features$mzmax,
    col = paste0(cl[features$var], "70"),
    border = paste0(cl[features$var], "70")
  )

  points(
    x = features$rt,
    y = features$mz,
    type = "p",
    pch = 19,
    cex = 1,
    col = cl[features$var]
  )

  if (showLegend) {
    legend(
      "topright",
      legend = names(cl),
      col = cl,
      pch = 19,
      lty = 1,
      cex = 0.8
    )
  }
}

#' @title map_features_interactive
#'
#' @description Function for plotting peak spaces.
#'
#' @param features A data table with the individual peak details to plot.
#' @param legendNames A character vector with the same length as the unique ids
#' in the table given in `eic`.
#' @param colorBy Possible values are \code{"targets"} (the default),
#' \code{"analyses"} or \code{replicates}.
#' @param xlim A length one or two numeric vector for setting the \emph{x}
#' limits (in seconds) of the plot.
#' @param ylim A length one or two numeric vector for setting the \emph{m/z}
#' limits of the plot.
#' @param title An optional character vector to be used as title.
#' @param showLegend Logical, set to \code{TRUE} to show legend.
#'
#' @return A peak/s map plot produced through \pkg{plotly}.
#'
#' @export
#'
map_features_interactive <- function(features, colorBy = "targets",
                                     legendNames = NULL,
                                     xlim = 60, ylim = 5,
                                     title = NULL, showLegend = TRUE) {
  if ("analyses" %in% colorBy) {
    leg <- unique(features$analysis)
    varkey <- features$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(features)) {
    leg <- unique(features$replicate)
    varkey <- features$replicate
  } else if (is.character(legendNames) &
    length(legendNames) == length(unique(features$feature))) {
    leg <- legendNames
    names(leg) <- unique(features$feature)
    varkey <- leg[features$feature]
  } else {
    leg <- unique(features$feature)
    varkey <- features$feature
  }

  features$var <- varkey

  if (length(xlim) == 1) {
    rtr <- c(min(features$rtmin) - xlim, max(features$rtmax) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(features$rtmin), max(features$rtmax))
  }

  if (length(ylim) == 1) {
    mzr <- c(min(features$mzmin) - ylim, max(features$mzmax) + ylim)
  } else if (length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(features$mzmin), max(features$mzmax))
  }

  cl <- get_colors(unique(features$var))

  plot <- plot_ly()

  plot <- plot %>% add_trace(
    x = features$rt, y = features$mz, color = features$var,
    type = "scatter", mode = "markers", colors = cl,
    marker = list(size = 8),
    hoverinfo = "text",
    text = paste(
      "</br> feature: ", features$feature,
      "</br> analysis: ", features$analysis,
      "</br> <i>m/z</i>: ", round(features$mz, digits = 4),
      "</br> dppm: ", round(((features$mzmax - features$mzmin) /
        features$mz) * 1E6, digits = 0),
      "</br> rt: ", round(features$rt, digits = 0),
      "</br> drt: ", round(features$rtmax - features$rtmin, digits = 0),
      "</br> intensity: ", round(features$intensity, digits = 0),
      "</br> filled: ",
      if ("is_filled" %in% colnames(features)) {
        ifelse(features$is_filled == 1, TRUE, FALSE)
      } else {
        FALSE
      }
    )
  )

  shapes <- list()

  for (i in seq_len(nrow(features))) {
    shapes[[i]] <- list(
      type = "rect",
      fillcolor = cl[names(cl) %in% features$var[i]],
      opacity = 0.2,
      line = list(color = cl[names(cl) %in% features$var[i]]),
      x0 = features$rtmin[i],
      x1 = features$rtmax[i],
      xref = "x",
      y0 = features$mzmin[i],
      y1 = features$mzmax[i],
      yref = "y"
    )
  }

  title <- list(
    text = title, x = 0.1, y = 0.98,
    font = list(size = 9, color = "black")
  )

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "Retention time / seconds",
    titlefont = list(size = 12, color = "black"),
    range = rtr,
    autotick = TRUE, ticks = "outside"
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "<i>m/z</i>",
    range = mzr,
    titlefont = list(size = 12, color = "black")
  )

  plot <- plot %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title,
    shapes = shapes
  )

  return(plot)
}

#' plot_groups_overview_aux
#'
#' @description Plots features for each feature group.
#'
#' @param features A data table with the individual peak details to plot.
#' @param eic A data table with the analysis, id, rt, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param heights A numeric vector of length two to control the height of
#' the first and second plot, respectively.
#' @param analyses X.
#'
#' @return plot.
#'
#' @export
#'
plot_groups_overview_aux <- function(features, eic, heights, analyses) {
  leg <- unique(eic$var)
  colors <- get_colors(leg)
  showleg <- rep(TRUE, length(leg))
  names(showleg) <- names(leg)

  plot <- plot_ly()

  for (g in leg) {
    uid <- unique(eic$uid[eic$var == g])

    for (u in uid) {
      df <- eic[eic$uid == u, ]
      ft <- features[features$uid == u, ]

      plot <- plot %>% add_trace(df,
        x = df$rt,
        y = df$intensity,
        type = "scatter", mode = "lines",
        line = list(width = 0.5, color = colors[g]),
        connectgaps = TRUE,
        name = g,
        legendgroup = g,
        showlegend = FALSE
      )

      df <- df[df$rt >= ft$rtmin & df$rt <= ft$rtmax, ]
      df$mz <- as.numeric(df$mz)

      plot <- plot %>% add_trace(df,
        x = df$rt,
        y = df$intensity,
        type = "scatter", mode = "lines+markers",
        fill = "tozeroy", connectgaps = TRUE,
        fillcolor = paste(color = colors[g], 50, sep = ""),
        line = list(width = 0.1, color = colors[g]),
        marker = list(size = 3, color = colors[g]),
        name = g,
        legendgroup = g,
        showlegend = showleg[which(leg %in% g)],
        hoverinfo = "text",
        hoverlabel = list(bgcolor = colors[g]),
        text = paste(
          "</br> name: ", g,
          "</br> group: ", ft$group,
          "</br> feature: ", ft$feature,
          "</br> analysis: ", ft$analysis,
          "</br> <i>m/z</i>: ", round(ft$mz, digits = 4),
          "</br> rt: ", round(df$rt, digits = 0),
          "</br> intensity: ", round(df$intensity, digits = 0)
        )
      )
      showleg[which(leg %in% g)] <- FALSE
    }
  }

  plot2 <- plot_ly()

  for (g in leg) {
    ft2 <- features[features$var == g, ]


    if (!"is_filled" %in% colnames(ft2)) ft2$is_filled <- FALSE

    ft_nf <- ft2[!ft2$is_filled, ]

    plot2 <- plot2 %>% add_trace(
      x = ft_nf$rt,
      y = ft_nf$analysis,
      type = "scatter",
      mode = "markers",
      marker = list(
        line = list(color = colors[g], width = 3),
        color = "#000000", size = 10
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
      text = paste(
        "</br> name: ", g,
        "</br> group: ", ft_nf$group,
        "</br> feature: ", ft_nf$feature,
        "</br> analysis: ", ft_nf$analysis,
        "</br> intensity: ", round(ft_nf$intensity, digits = 0),
        "</br> width: ", round(ft_nf$rtmax - ft_nf$rtmin, digits = 0),
        "</br> dppm: ", round(((ft_nf$mzmax - ft_nf$mzmin) / ft_nf$mz) *
          1E6, digits = 1),
        "</br> filled: ", ft_nf$is_filled
      )
    )

    ft_f <- ft2[ft2$is_filled, ]

    if (nrow(ft_f) > 0) {
      plot2 <- plot2 %>% add_trace(
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
        text = paste(
          "</br> name: ", g,
          "</br> group: ", ft_f$group,
          "</br> feature: ", ft_f$feature,
          "</br> analysis: ", ft_f$analysis,
          "</br> intensity: ", round(ft_f$intensity, digits = 0),
          "</br> width: ", round(ft_f$rtmax - ft_f$rtmin, digits = 0),
          "</br> dppm: ", round(((ft_f$mzmax - ft_f$mzmin) / ft_f$mz) *
            1E6, digits = 1),
          "</br> filled: ", ft_f$is_filled
        )
      )
    }
  }
  plot2 <- hide_colorbar(plot2)

  plot3 <- plot_ly(features, x = features$analysis)

  for (g in leg) {
    df_3 <- features[features$var == g, ]

    if (!all(analyses %in% df_3$analysis)) {
      extra <- data.frame(
        "analysis" = analyses[!analyses %in% df_3$analysis],
        "var" = g,
        "intensity" = 0
      )
      df_3 <- rbind(df_3[, c("analysis", "var", "intensity")], extra)
      df_3 <- df_3[order(df_3$analysis), ]
    }

    plot3 <- plot3 %>% add_trace(df_3,
      x = df_3$analysis,
      y = df_3$intensity / max(df_3$intensity),
      type = "scatter", mode = "lines",
      line = list(width = 1, color = colors[g]),
      connectgaps = FALSE,
      name = g,
      legendgroup = g,
      showlegend = FALSE
    )
  }

  xaxis <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Retention time / seconds",
    titlefont = list(size = 12, color = "black"),
    range = c(min(eic$rt), max(eic$rt)),
    autotick = TRUE, ticks = "outside"
  )

  yaxis1 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )

  yaxis2 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "",
    titlefont = list(size = 12, color = "black"),
    tick0 = 0, dtick = 1
  )

  xaxis3 <- list(linecolor = toRGB("black"), linewidth = 2, title = NULL)

  yaxis3 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Normalized intensity",
    titlefont = list(size = 12, color = "black"),
    tick0 = 0, dtick = 0.25, range = c(0, 1.2)
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
    titleY = TRUE, titleX = TRUE,
    heights = heights[1:2],
    margin = 0.01,
    shareX = TRUE,
    which_layout = "merge"
  )

  plotf_2 <- subplot(
    list(plotf, plot3),
    nrows = 2,
    titleY = TRUE, titleX = TRUE,
    heights = c(sum(heights[1:2]), heights[3]),
    margin = 0.01,
    shareX = FALSE,
    which_layout = "merge"
  )

  plotf_2 <- plotf_2 %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", "targets", "</b>")))
  )

  return(plotf_2)
}
