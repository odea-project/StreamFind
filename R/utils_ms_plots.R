
#' .get_colors
#'
#' @noRd
#'
.get_colors <- function(obj) {
  colors <- c(
    brewer.pal(8, "Greys")[6],
    brewer.pal(8, "Greens")[6],
    brewer.pal(8, "Blues")[6],
    brewer.pal(8, "Oranges")[6],
    brewer.pal(8, "Purples")[6],
    brewer.pal(8, "PuRd")[6],
    brewer.pal(8, "YlOrRd")[6],
    brewer.pal(8, "PuBuGn")[6],
    brewer.pal(8, "GnBu")[6],
    brewer.pal(8, "BuPu")[6],
    brewer.pal(8, "Dark2")
  )

  Ncol <- length(unique(obj))

  if (Ncol > 18) {
    colors <- colorRampPalette(colors)(Ncol)
  }

  if (length(unique(obj)) < length(obj)) {
    Vcol <- colors[seq_len(Ncol)]
    Ncol <- length(obj)
    char <- NULL
    count <- dplyr::count(data.frame(n = seq_len(Ncol), char = obj), char)
    Vcol <- rep(Vcol, times = count[, "n"])
    names(Vcol) <- obj
  } else {
    Vcol <- colors[seq_len(Ncol)]
    names(Vcol) <- obj
  }

  Vcol
}

#' .make_colorBy_varkey
#'
#' @noRd
#'
.make_colorBy_varkey <- function(data = NULL, colorBy = NULL, legendNames = NULL) {

  if (!"id" %in% colnames(data)) {

    if ("feature" %in% colnames(data)) {
      data$id <- data$feature

    } else if ("group" %in% colnames(data)) {
      data$id <- data$group

    } else {
      data$id <- ""
    }
  }

  if (!"analysis" %in% colnames(data)) data$analysis <- ""

  data$id <- factor(data$id)

  data$analysis <- factor(data$analysis)

  if ("level" %in% colnames(data)) {

    if (!is.character(data$level)) {
      data$level <- paste("MS", data$level, sep = "")
    }

    data$level <- factor(data$level)

  } else {
    data$level <- "not defined"
  }

  if ("polarity" %in% colnames(data)) {
    if (!is.character(data$polarity)) {
      pol_key <- c("positive", "negative", "not defined")
      names(pol_key) <- c("1", "-1", "0")
      data$polarity <- as.character(data$polarity)
      data$polarity <- pol_key[data$polarity]
    }

  } else {
    data$polarity <- "not defined"
  }

  if ("analyses" %in% colorBy) {
    varkey <- data$analysis

  } else if (("targets+analyses" %in% colorBy ||  "analyses+targets" %in% colorBy) && "analysis" %in% colnames(data)) {
    
    if ("name" %in% colnames(data) & isTRUE(legendNames)) {
      varkey <- paste0(data$name, " - ", data$analysis)
    } else {
      varkey <- paste0(data$id, " - ", data$analysis)
    }
    
  } else if ("replicates" %in% colorBy && "replicate" %in% colnames(data)) {
    varkey <- data$replicate

  } else if (("targets+replicates" %in% colorBy ||  "replicates+targets" %in% colorBy) && "replicate" %in% colnames(data)) {
    
    if ("name" %in% colnames(data) & isTRUE(legendNames)) {
      varkey <- paste0(data$name, " - ", data$replicate)
    } else {
      varkey <- paste0(data$id, " - ", data$replicate)
    }
    
  } else if ("polarities" %in% colorBy && "polarity" %in% colnames(data)) {
    varkey <- data$polarity

  } else if (("targets+polarities" %in% colorBy || "polarities+targets" %in% colorBy) && "polarity" %in% colnames(data)) {

    if ("name" %in% colnames(data) & isTRUE(legendNames)) {
      varkey <- paste0(data$name, " - ", data$polarity)
    } else {
      varkey <- paste0(data$id, " - ", data$polarity)
    }

  } else if ("levels" %in% colorBy && "level" %in% colnames(data)) {
    varkey <- data$level

  } else if (is.character(legendNames) && length(legendNames) == length(unique(data$id))) {
    leg <- legendNames
    names(leg) <- unique(data$id)
    varkey <- leg[data$id]

  } else if ("name" %in% colnames(data) && isTRUE(legendNames)) {
    varkey <- data$name

  } else {
    varkey <- data$id
  }

  data$var <- varkey

  data
}


#' .plot_spectra_interactive
#'
#' @noRd
#'
.plot_spectra_interactive <- function(spectra = NULL,
                                      colorBy = "analyses",
                                      legendNames = NULL,
                                      xVal = "rt",
                                      yVal = "mz",
                                      xLab = NULL,
                                      yLab = NULL,
                                      zLab = NULL) {

  checkmate::assert_choice(xVal, c("rt", "mz", "drift"))

  checkmate::assert_choice(yVal, c("rt", "mz", "drift"))

  if (any(duplicated(c(xVal, yVal)))) {
    stop("Duplicated x and y values are not possible!")
  }

  xlab <- switch(xVal,
    "mz" = "<i>m/z</i> / Da",
    "rt" = "Retention time / seconds",
    "drift" = "Drift time / milliseconds"
  )

  ylab <- switch(yVal,
    "mz" = "<i>m/z</i> / Da",
    "rt" = "Retention time / seconds",
    "drift" = "Drift time / milliseconds"
  )
  
  zlab <- "Intensity / counts"
  
  if (!is.null(xLab)) xlab <- xLab
  if (!is.null(yLab)) ylab <- yLab
  if (!is.null(zLab)) zlab <- zLab

  spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames)

  spectra$rtmz <- paste(
    spectra$id,
    spectra$level,
    spectra$polarity,
    spectra$mz,
    spectra$rt,
    spectra$drift,
    spectra$analysis,
    sep = ""
  )

  spec_temp <- spectra

  spec_temp$intensity <- 0

  spectra <- rbind(spectra, spec_temp)

  spectra[["x"]] <- spectra[[xVal[1]]]

  spectra[["y"]] <- spectra[[yVal[1]]]

  colors_var <- .get_colors(unique(spectra$var))

  fig <- plot_ly(spectra, x = ~x, y = ~y, z = ~intensity) %>%
    group_by(spectra$rtmz) %>%
    add_lines(color = ~var, colors = colors_var)

  fig <- fig %>% plotly::layout(scene = list(
    xaxis = list(title = xlab),
    yaxis = list(title = ylab),
    zaxis = list(title = zlab)
  ))

  fig
}

#' .plot_xic_interactive
#'
#' @noRd
#'
.plot_xic_interactive <- function(xic,
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

  finalplot
}

#' .plot_eic_static
#'
#' @noRd
#'
.plot_eic_static <- function(eic = NULL,
                            legendNames = NULL,
                            colorBy = "targets",
                            title = NULL,
                            showLegend = TRUE,
                            xlim = NULL,
                            ylim = NULL,
                            cex = 0.6) {

  eic <- .make_colorBy_varkey(eic, colorBy, legendNames)

  cl <- .get_colors(unique(eic$var))

  eic$loop <- paste0(eic$analysis, eic$id, eic$var)
  loop_key <- unique(eic$loop)

  if (is.numeric(xlim) & length(xlim) == 1) {
    rtr <- c(min(eic$rt) - xlim, max(eic$rt) + xlim)
  } else if (is.numeric(xlim) & length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(eic$rt), max(eic$rt))
    if (showLegend) {
      rtr[2] <- rtr[2] * 1.01
    }
  }

  if (is.numeric(ylim) & length(ylim) == 1) {
    intr <- c(min(eic$intensity) - ylim, (max(eic$intensity) + ylim))
  } else if (is.numeric(ylim) & length(ylim) == 2) {
    intr <- ylim
  } else {
    intr <- c(0, max(eic$intensity))
  }

  if (is.null(cex) || !is.numeric(cex)) cex <- 0.6

  plot(eic$rt,
    type = "n",
    xlab = "Retention time / seconds",
    ylab = "Intensity / counts",
    xlim = rtr,
    ylim = intr,
    main = title
  )

  for (t in loop_key) {
    select_vector <- eic$loop %in% t
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

  if (showLegend) {
    legend(
      x = "topright",
      legend = names(cl),
      col = cl,
      lwd = 2,
      lty = 1,
      cex = cex,
      bty = "n"
    )
  }
}

#' .plot_eic_interactive
#'
#' @noRd
#'
.plot_eic_interactive <- function(eic = NULL,
                                 legendNames = NULL,
                                 colorBy = "targets",
                                 title = NULL,
                                 showLegend = TRUE) {

  eic <- .make_colorBy_varkey(eic, colorBy, legendNames)

  leg <- unique(eic$var)
  cl <- .get_colors(leg)

  eic$loop <- paste0(eic$analysis, eic$id, eic$var)
  loop_key <- unique(eic$loop)


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

  if (showLegend) {
    showL <- rep(TRUE, length(leg))
  } else {
    showL <- rep(FALSE, length(leg))
  }

  names(showL) <- leg

  for (t in loop_key) {
    select_vector <- eic$loop %in% t
    lt <- unique(eic$var[select_vector])
    x <- eic$rt[select_vector]
    y <- eic$intensity[select_vector]

    plot <- plot %>% add_trace(
      x = x,
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

  if (showLegend) {
    plot <- plot %>% plotly::layout(
      legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  } else {
    plot <- plot %>% plotly::layout(
      legend = NULL,
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  }

  plot
}

#' .plot_bpc_interactive
#'
#' @noRd
#'
.plot_bpc_interactive <- function(bpc = NULL,
                                 legendNames = NULL,
                                 colorBy = "targets",
                                 title = NULL,
                                 showLegend = TRUE) {

  bpc <- .make_colorBy_varkey(bpc, colorBy, legendNames)

  leg <- unique(bpc$var)
  cl <- .get_colors(leg)

  bpc$loop <- paste0(bpc$analysis, bpc$id, bpc$var)
  loop_key <- unique(bpc$loop)

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

  if (showLegend) {
    showL <- rep(TRUE, length(leg))
  } else {
    showL <- rep(FALSE, length(leg))
  }

  names(showL) <- leg

  for (t in loop_key) {
    select_vector <- bpc$loop %in% t
    lt <- unique(bpc$var[select_vector])
    x <- bpc$rt[select_vector]
    y <- bpc$intensity[select_vector]
    ana <- bpc$analysis[select_vector]

    plot <- plot %>% add_trace(
      x = x,
      y = y,
      type = "scatter", mode = "lines+markers",
      line = list(width = 0.5, color = unname(cl[lt])),
      marker = list(size = 2, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste(
        "<br>analysis: ", ana,
        "<br>rt: %{x}<br>",
        "mz: ",
        round(bpc$mz[select_vector], digits = 4),
        "<br>", "intensity: %{y}"
      )
    )
    if (length(y) >= 1) showL[lt] <- FALSE
  }


  if (showLegend) {
    plot <- plot %>% plotly::layout(
      legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  } else {
    plot <- plot %>% plotly::layout(
      legend = NULL,
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  }

  plot
}

#' .plot_ms2_static
#'
#' @noRd
#'
.plot_ms2_static <- function(ms2 = NULL,
                            legendNames = NULL,
                            colorBy = "targets",
                            title = NULL) {

  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)

  cl <- .get_colors(unique(ms2$var))

  ms2$var <- as.factor(ms2$var)

  ms2$color <- cl[ms2$var]

  ms2$text = paste0(round(ms2$mz, 4))

  plot(intensity ~ mz, ms2,
    type = "h",
    xlab = expression(italic("m/z ") / " Da"),
    ylab = "Intensity / counts",
    col = ms2$color,
    lwd = 2,
    ylim = c(0, max(ms2$intensity) * 1.5),
    main = title,
    yaxs = "i",
    xaxt = "n"
  )

  precursors <- ms2[ms2$is_pre, ]

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
    "topleft",
    legend = levels(ms2$var),
    col = cl,
    lty = 1,
    cex = 0.8,
    bty = "n"
  )
}

#' .plot_ms2_interactive
#'
#' @noRd
#'
.plot_ms2_interactive <- function(ms2 = NULL, legendNames = NULL,
                                 colorBy = "targets", title = NULL) {

  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)

  leg <- unique(ms2$var)

  cl <- .get_colors(leg)

  plot <- plot_ly()

  for (v in leg) {
    data <- ms2[ms2$var == v, ]

    bar_widths <- rep(0.2, nrow(data))

    mz_text <- paste0(round(data$mz, digits = 4), "  ")

    if (TRUE %in% data$is_pre) {
      mz_text[data$is_pre] <- paste0("Pre ", mz_text[data$is_pre])
      bar_widths[data$is_pre] <- 4
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
    linewidth = 2, title = "<i>m/z</i> / Da",
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

#' .plot_ms1_static
#'
#' @noRd
#'
.plot_ms1_static <- function(ms1 = NULL,
                             legendNames = NULL,
                             colorBy = "targets",
                             title = NULL) {

  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)

  cl <- .get_colors(unique(ms1$var))

  ms1$var <- as.factor(ms1$var)

  ms1$color <- cl[ms1$var]

  ms1$text = paste0(round(ms1$mz, 4))

  plot(intensity ~ mz, ms1,
    type = "h",
    xlab = expression(italic("m/z ") / " Da"),
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
    "topleft",
    legend = levels(ms1$var),
    col = cl,
    lty = 1,
    cex = 0.8,
    bty = "n"
  )
}

#' .plot_ms1_interactive
#'
#' @noRd
#'
.plot_ms1_interactive <- function(ms1 = NULL,
                                 legendNames = NULL,
                                 colorBy = "targets",
                                 title = NULL,
                                 showText = TRUE) {

  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)

  leg <- unique(ms1$var)

  cl <- .get_colors(leg)

  plot <- plot_ly()

  for (v in leg) {
    data <- ms1[ms1$var == v, ]

    if (showText) {
      mz_text <- paste0(round(data$mz, digits = 4), "  ")
    } else {
      mz_text <- " "
    }

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
    linewidth = 2, title = "<i>m/z</i> / Da",
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

#' .plot_features_static
#'
#' @noRd
#'
.plot_features_static <- function(eic = NULL,
                                 features = NULL,
                                 legendNames = NULL,
                                 colorBy = "targets",
                                 title = NULL,
                                 showLegend = TRUE,
                                 xlim = NULL,
                                 ylim = NULL,
                                 cex = 0.6) {

  eic <- .make_colorBy_varkey(eic, colorBy, legendNames)

  leg <- unique(eic$var)

  cl <- .get_colors(leg)

  eic$unique_ids <- paste0(eic$id, eic$analysis)

  features$unique_ids <- paste0(features$feature, features$analysis)

  ids <- unique(eic$unique_ids)

  if (is.numeric(xlim) & length(xlim) == 1) {
    rtr <- c(min(eic$rt) - xlim, max(eic$rt) + xlim)
  } else if (is.numeric(xlim) & length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(eic$rt), max(eic$rt))
    if (showLegend) {
      rtr[2] <- rtr[2] * 1.01
    }
  }

  if (is.numeric(ylim) & length(ylim) == 1) {
    intr <- c(min(eic$intensity) - ylim, (max(eic$intensity) + ylim))
  } else if (is.numeric(ylim) & length(ylim) == 2) {
    intr <- ylim
  } else {
    intr <- c(0, max(eic$intensity))
  }

  if (is.null(cex) || !is.numeric(cex)) cex <- 0.6
  
  plot_qlt <- FALSE
  
  if ("quality" %in% colnames(features)) {
    plot_qlt <- TRUE
  }

  plot(eic$rt,
    type = "n",
    xlab = "Retention time / seconds",
    ylab = "Intensity / counts",
    xlim = rtr,
    ylim = intr,
    main = title
  )

  for (t in ids) {
    select_vector <- eic$unique_ids == t
    lt <- unique(eic$var[select_vector])
    pk_eic <- eic[select_vector, ]

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

    pk_a <- features[features$unique_ids == t, ]

    # pk_eic_a <- pk_eic[
    #   pk_eic$rt >= pk_a$rtmin[f] &
    #     pk_eic$rt <= pk_a$rtmax[f],
    # ]

    # polygon(
    #   c(pk_eic_a$rt, rev(pk_eic_a$rt)),
    #   c(pk_eic_a$intensity, rep(0, length(pk_eic_a$intensity))),
    #   col = paste(color = unname(cl[lt]), 50, sep = ""),
    #   border = F
    # )

    rect(
      xleft = pk_a$rtmin,
      xright = pk_a$rtmax,
      ybottom = 0,
      ytop = pk_a$intensity,
      border = cl[lt],
      lwd = 1,
      lty = 3
    )

    lines(
      x = rep(pk_a$rt, 2),
      y = c(0, pk_a$intensity),
      type = "l",
      pch = 19,
      cex = 0.6,
      col = cl[lt]
    )
    
    if (plot_qlt) {
      q_t <- pk_a$quality[[1]]
      
      if (!is.null(q_t$model)) {
        lines(
          x = q_t$rt,
          y = q_t$predicted,
          type = "l",
          pch = 19,
          cex = 0.3,
          lty = 2,
          col = cl[lt]
        )
        
        lines(
          x = q_t$rt,
          y = rep(q_t$noise, length(q_t$rt)),
          type = "l",
          pch = 19,
          cex = 0.3,
          lty = 2,
          col = cl[lt]
        )
      }
    }
  }

  if (showLegend) {

    # library(ggplot2)
    # plot <- ggplot2::ggplot(eic, aes(rt, intensity, color = var))
    # plot <- plot + ggplot2::geom_line(aes(group = eic$unique_ids))
    # plot <- plot + ggplot2::scale_color_manual(values = cl, labels = names(cl))
    # plot <- plot + ggplot2::theme_bw()
    # plot <- plot + theme(panel.grid = element_blank())
    # plot <- plot + labs(
    #   x = "Retention time / seconds",
    #   y = "Intensity / counts",
    #   title = title
    # )

    legend(
      x = "topright",
      legend = names(cl),
      col = cl,
      lwd = 2,
      lty = 1,
      cex = cex,
      bty = "n"
    )
  }
}

#' .plot_features_interactive
#'
#' @noRd
#'
.plot_features_interactive <- function(eic = NULL,
                                       features = NULL,
                                       legendNames = NULL,
                                       colorBy = "targets",
                                       title = NULL,
                                       showLegend = TRUE) {

  eic <- .make_colorBy_varkey(eic, colorBy, legendNames)

  leg <- unique(eic$var)

  cl <- .get_colors(leg)

  eic$unique_ids <- paste0(eic$id, eic$analysis)

  features$unique_ids <- paste0(features$feature, features$analysis)

  ids <- unique(eic$unique_ids)
  
  plot_qlt <- FALSE
  
  if ("quality" %in% colnames(features)) {
    plot_qlt <- TRUE
  }

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

  if (showLegend) {
    showL <- rep(TRUE, length(leg))
  } else {
    showL <- rep(FALSE, length(leg))
  }

  names(showL) <- leg

  for (t in ids) {
    lt <- unique(eic$var[eic$unique_ids == t])
    y <- eic$intensity[eic$unique_ids == t]

    plot <- plot %>% add_trace(
      x = eic$rt[eic$unique_ids == t],
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

    pk <- features[features$unique_ids %in% t, ]

    pk_eic <- eic[eic$rt >= pk$rtmin & eic$rt <= pk$rtmax & eic$unique_ids %in% t, ]

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
      },
      if (plot_qlt) {
        q_t <- pk$quality[[1]]
        
        if (!is.null(q_t$model)) {
          paste(
            "</br> noise: ", q_t$noise,
            "</br> sn: ", q_t$sn,
            "</br> gaufit: ", round(q_t$gaufit, digits = 4),
            "</br> A: ", q_t$A,
            "</br> mu: ", q_t$mu,
            "</br> sigma: ", q_t$sigma
          )
        }
      } else {
        ""
      }
    )
    
    if (plot_qlt) {
      
      if (!is.null(q_t$model)) {
        plot <- plot %>%  plotly::add_trace(
          x = q_t$rt,
          y = q_t$predicted,
          type = 'scatter',
          name = lt,
          legendgroup = lt,
          mode = 'lines',
          line = list(dash = 'dash', color = unname(cl[lt])),
          showlegend = FALSE
        )
        
        plot <- plot %>%  plotly::add_trace(
          x = q_t$rt,
          y = rep(q_t$noise, length(q_t$rt)),
          type = 'scatter',
          name = lt,
          legendgroup = lt,
          mode = 'lines',
          line = list(dash = 'dot', color = unname(cl[lt])),
          showlegend = FALSE
        )
      }
    }

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

  if (showLegend) {
    plot <- plot %>% plotly::layout(
      legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  } else {
    plot <- plot %>% plotly::layout(
      legend = NULL,
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  }

  plot
}

#' .map_features_static
#'
#' @noRd
#'
.map_features_static <- function(features,
                                colorBy = "targets",
                                legendNames = NULL,
                                title = NULL,
                                showLegend = TRUE,
                                xlim = NULL,
                                ylim = NULL,
                                cex = 0.6) {

  features <- .make_colorBy_varkey(features, colorBy, legendNames)

  leg <- unique(features$var)

  cl <- .get_colors(leg)

  if (length(xlim) == 1) {
    rtr <- c(min(features$rtmin) - xlim, max(features$rtmax) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(features$rtmin), max(features$rtmax))
  }

  if (is.numeric(xlim) & length(xlim) == 1) {
    rtr <- c(min(features$rtmin) - xlim, max(features$rtmax) + xlim)
  } else if (is.numeric(xlim) & length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(features$rtmin), max(features$rtmax))
    if (showLegend) {
      rtr[2] <- rtr[2] * 1.01
    }
  }

  if (is.numeric(ylim) & length(ylim) == 1) {
    mzr <- c(min(features$mzmin) - ylim, max(features$mzmax) + ylim)
  } else if (is.numeric(ylim) & length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(features$mzmin), max(features$mzmax))
  }

  if (is.null(cex) || !is.numeric(cex)) cex <- 0.6

  plot(features$rt,
    features$mz,
    type = "n",
    xlab = "Retention time / seconds",
    ylab = expression(italic("m/z ") / " Da"),
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
      x = "topright",
      legend = names(cl),
      col = cl,
      lwd = 2,
      lty = 1,
      cex = cex,
      bty = "n"
    )
  }
}

#' .map_features_interactive
#'
#' @noRd
#'
.map_features_interactive <- function(features,
                                     colorBy = "targets",
                                     legendNames = NULL,
                                     xlim = 60, ylim = 5,
                                     title = NULL,
                                     showLegend = TRUE) {

  features <- .make_colorBy_varkey(features, colorBy, legendNames)

  leg <- unique(features$var)

  cl <- .get_colors(leg)

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

  plotlegend <- rep(TRUE, length(cl))

  names(plotlegend) <- names(cl)

  plot <- plot_ly()

  for (i in seq_len(nrow(features))) {

    x0 <- features$rtmin[i]
    x1 <- features$rtmax[i]
    y0 <- features$mzmin[i]
    y1 <- features$mzmax[i]

    plot <- plot %>% add_trace(
      x = c(x0, x1, x1, x0, x0),
      y = c(y0, y0, y1, y1, y0),
      type = "scatter",
      mode = "lines",
      fill = "none",
      line = list(color = cl[features$var[i]]),
      opacity = 0.2,
      name = features$var[i],
      legendgroup = features$var[i],
      showlegend = FALSE
    )
  }


  for (i in seq_len(nrow(features))) {
    ft <- features[i, ]
    x <- ft$rt
    y <- ft$mz

    plot <- plot %>% add_trace(
      x = x, y = y,
      type = "scatter", mode = "markers",
      #color = cl[ft$var],
      marker = list(size = 8, color = cl[ft$var]),
      name = ft$var,
      legendgroup = ft$var,
      showlegend = plotlegend[ft$var],
      hoverinfo = "text",
      text = paste(
        "</br> feature: ", ft$feature,
        "</br> analysis: ", ft$analysis,
        "</br> <i>m/z</i>: ", round(y, digits = 4),
        "</br> dppm: ", round(((ft$mzmax - ft$mzmin) / y) * 1E6, digits = 0),
        "</br> rt: ", round(x, digits = 0),
        "</br> drt: ", round(ft$rtmax - ft$rtmin, digits = 0),
        "</br> intensity: ", round(ft$intensity, digits = 0),
        "</br> filled: ",
        if ("is_filled" %in% colnames(ft)) {
          ifelse(ft$is_filled == 1, TRUE, FALSE)
        } else {
          FALSE
        }
      )
    )

    if (isTRUE(plotlegend[ft$var])) {
      plotlegend[ft$var] <- FALSE
    }
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
    linewidth = 2, title = "<i>m/z</i> / Da",
    range = mzr,
    titlefont = list(size = 12, color = "black")
  )

  if (showLegend) {
    plot <- plot %>% plotly::layout(
      legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  } else {
    plot <- plot %>% plotly::layout(
      legend = NULL,
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  }

  plot
}

#' .plot_groups_overview_aux
#'
#' @noRd
#'
.plot_groups_overview_aux <- function(features, eic, heights, analyses) {

  leg <- unique(eic$var)

  colors <- .get_colors(leg)

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
        "</br> filled: ", ft_nf$is_filled,
        "</br> filtered: ", ft_nf$filtered,
        "</br> filter: ", ft_nf$filter,
        ifelse("isotope" %in% colnames(ft_nf),
          paste("</br> cluster: ", vapply(ft_nf$isotope, function(x) x$cluster, NA_real_),
                "</br>  - size: ", vapply(ft_nf$isotope, function(x) x$cluster_size, NA_real_),
                "</br>  - isotope: ", vapply(ft_nf$isotope, function(x) x$tag, NA_character_),
                "</br>  - carbons: ", vapply(ft_nf$isotope, function(x) x$carbons, NA_real_)), "")
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
    tick0 = 0, dtick = 0.25, range = c(0, 1.5)
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

#' .map_isotopes_interactive
#'
#' @noRd
#'
.map_isotopes_interactive <- function(isotopes, colorBy = "targets",
                                      legendNames = NULL,
                                      xlim = 60, ylim = 5,
                                      title = NULL, showLegend = TRUE) {

  if ("analyses" %in% colorBy) {
    leg <- unique(isotopes$analysis)
    varkey <- isotopes$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(isotopes)) {
    leg <- unique(isotopes$replicate)
    varkey <- isotopes$replicate
  } else if (is.character(legendNames) &
             length(legendNames) == length(unique(isotopes$iso_cluster))) {
    leg <- legendNames
    names(leg) <- unique(isotopes$iso_cluster)
    varkey <- leg[isotopes$iso_cluster]
  } else if ("name" %in% colnames(isotopes) & isTRUE(legendNames)) {
    leg <- unique(isotopes$name)
    varkey <- isotopes$name
  } else if ("features" %in% colorBy) {
    leg <- unique(isotopes$feature)
    varkey <- isotopes$feature
  } else {
    leg <- unique(isotopes$iso_cluster)
    varkey <- as.character(isotopes$iso_cluster)
  }

  isotopes$var <- varkey

  if (length(xlim) == 1) {
    rtr <- c(min(isotopes$rtmin) - xlim, max(isotopes$rtmax) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(isotopes$rtmin), max(isotopes$rtmax))
  }

  if (length(ylim) == 1) {
    mzr <- c(min(isotopes$mzmin) - ylim, max(isotopes$mzmax) + ylim)
  } else if (length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(isotopes$mzmin), max(isotopes$mzmax))
  }

  cl <- .get_colors(unique(isotopes$var))

  plotlegend <- rep(TRUE, length(cl))
  names(plotlegend) <- names(cl)

  plot <- plot_ly()

  for (i in seq_len(nrow(isotopes))) {

    x0 <- isotopes$rtmin[i]
    x1 <- isotopes$rtmax[i]
    y0 <- isotopes$mzmin[i]
    y1 <- isotopes$mzmax[i]

    plot <- plot %>% add_trace(
      x = c(x0, x1, x1, x0, x0),
      y = c(y0, y0, y1, y1, y0),
      type = "scatter",
      mode = "lines",
      fill = "none",
      line = list(color = cl[isotopes$var[i]]),
      opacity = 0.2,
      name = isotopes$var[i],
      legendgroup = isotopes$var[i],
      showlegend = FALSE
    )
  }

  for (i in seq_len(nrow(isotopes))) {
    ft <- isotopes[i, ]
    x <- ft$rt
    y <- ft$mz

    plot <- plot %>% add_trace(
      x = x, y = y,
      type = "scatter", mode = "markers+text",
      #color = cl[ft$var],
      marker = list(size = 15 * ft$iso_relative_intensity, color = cl[ft$var]),
      name = ft$var,
      legendgroup = ft$var,
      showlegend = ifelse(ft$iso_tag == "M", FALSE, plotlegend[ft$var]),
      text =  paste0(ft$iso_tag, " ", ft$iso_elements),
      textposition = "midle right",
      textfont = list(size = 12, color = cl[ft$var]),
      hovertext = paste(
        "</br> cluster: ", ft$iso_cluster,
        "</br> feature: ", ft$feature,
        "</br> analysis: ", ft$analysis,
        "</br> <i>m/z</i>: ", round(y, digits = 4),
        "</br> dppm: ", round(((ft$mzmax - ft$mzmin) / y) * 1E6, digits = 0),
        "</br> rt: ", round(x, digits = 0),
        "</br> drt: ", round(ft$rtmax - ft$rtmin, digits = 0),
        "</br> filled: ",
        if ("is_filled" %in% colnames(ft)) {
          ifelse(ft$is_filled == 1, TRUE, FALSE)
        } else {
          FALSE
        },
        "</br> intensity: ", round(ft$intensity, digits = 0),
        "</br> rel intensity (%): ", round(ft$iso_relative_intensity * 100, digits = 2),
        "</br> charge: ", ft$iso_charge,
        "</br> isotope: ", ft$iso_tag,
        "</br> element/s: ", ft$iso_elements,
        "</br> target mass defect: ", round(ft$iso_md_hit, digits = 4),
        "</br> exp mass defect: ", round(ft$iso_md_diff, digits = 4),
        "</br> estimated carbons: ", ft$iso_carbons
      )
    )

    if (isTRUE(plotlegend[ft$var]) & ft$iso_tag != "M") {
      plotlegend[ft$var] <- FALSE
    }
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
    linewidth = 2, title = "<i>m/z</i> / Da",
    range = mzr,
    titlefont = list(size = 12, color = "black")
  )

  plot <- plot %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )

  plot
}

#' .map_isotopes_static
#'
#' @noRd
#'
.map_isotopes_static <- function(isotopes, colorBy = "targets",
                                  legendNames = NULL,
                                  xlim = 60, ylim = 5,
                                  title = NULL, showLegend = TRUE) {
  if ("analyses" %in% colorBy) {
    leg <- unique(isotopes$analysis)
    varkey <- isotopes$analysis
  } else if ("replicates" %in% colorBy & "replicate" %in% colnames(isotopes)) {
    leg <- unique(isotopes$replicate)
    varkey <- isotopes$replicate
  } else if (is.character(legendNames) &
             length(legendNames) == length(unique(isotopes$iso_cluster))) {
    leg <- legendNames
    names(leg) <- unique(isotopes$iso_cluster)
    varkey <- leg[isotopes$iso_cluster]
  } else if ("name" %in% colnames(isotopes) & isTRUE(legendNames)) {
    leg <- unique(isotopes$name)
    varkey <- isotopes$name
  } else if ("features" %in% colorBy) {
    leg <- unique(isotopes$feature)
    varkey <- isotopes$feature
  } else {
    leg <- unique(isotopes$iso_cluster)
    varkey <- as.character(isotopes$iso_cluster)
  }

  isotopes$var <- varkey

  if (length(xlim) == 1) {
    rtr <- c(min(isotopes$rtmin) - xlim, max(isotopes$rtmax) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(isotopes$rtmin), max(isotopes$rtmax))
  }

  ylim_oufset <- 1 + (0.02 * length(unique(isotopes$var)))

  if (length(ylim) == 1) {
    mzr <- c(min(isotopes$mzmin) - ylim, (max(isotopes$mzmax) + ylim) * ylim_oufset)
  } else if (length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(isotopes$mzmin), max(isotopes$mzmax) * ylim_oufset)
  }

  cl <- .get_colors(unique(isotopes$var))

  plot(isotopes$rt,
       isotopes$mz,
       type = "n",
       xlab = "Retention time / seconds",
       ylab = expression(italic("m/z ") / " Da"),
       xlim = rtr,
       ylim = mzr,
       main = title
  )

  rect(
    xleft = isotopes$rtmin,
    xright = isotopes$rtmax,
    ybottom = isotopes$mzmin,
    ytop = isotopes$mzmax,
    col = paste0(cl[isotopes$var], "70"),
    border = paste0(cl[isotopes$var], "70")
  )

  points(
    x = isotopes$rt,
    y = isotopes$mz,
    type = "p",
    pch = 19,
    cex = 1.5 * isotopes$iso_rel_int,
    col = cl[isotopes$var]
  )

  for (i in seq_len(nrow(isotopes))) {
    text(
      isotopes$rt[i] + 0.2,
      isotopes$mz[i],
      paste0(isotopes$iso_tag, " ", isotopes$iso_elements)[i],
      pos = 4, col = cl[isotopes$var[i]], cex = 0.6
    )
  }

  if (showLegend) {
    legend(
      "topleft",
      legend = names(cl),
      col = cl,
      pch = 19,
      lty = 1,
      cex = 0.7,
      bty = "n"
    )
  }
}

.plot_internal_standards_qc_interactive <- function(istd, analyses) {

  if (!is.data.frame(istd)) {
    return(NULL)
  }

  if (nrow(istd) == 0) {
    return(NULL)
  }

  if (!("analysis" %in% colnames(istd)) & "replicate" %in% colnames(istd)) {
    istd$analysis <- istd$replicate
  }

  leg <- unique(istd$name)

  colors <- .get_colors(leg)

  if ("freq" %in% colnames(istd)) p0 <- plot_ly(istd, x = analyses)

  p1 <- plot_ly(istd, x = analyses)

  p2 <- plot_ly(istd, x = analyses)

  if (!all(is.na(istd$rec))) {
    p3 <- plot_ly(istd, x = analyses)
    do_rec <- TRUE
  } else {
    do_rec <- FALSE
  }

  p4 <- plot_ly(istd, x = analyses)

  p5 <- plot_ly(istd, x = analyses)

  for (i in unique(istd$name)) {
    df <- istd[istd$name == i, ]

    showLegendInSecond <- FALSE

    if ("freq" %in% colnames(istd)) {
      p0 <- p0 %>% add_trace(df,
        x = df$analysis,
        y = df$freq / max(istd$freq, na.rm = TRUE) * 100,
        type = "scatter", mode = "markers",
        marker = list(size = 5, color = colors[i]),
        connectgaps = FALSE,
        name = i,
        legendgroup = i,
        showlegend = TRUE
      )
    } else {
      showLegendInSecond <- TRUE
    }

    df1 <- df[!is.na(df$error_rt), ]

    if (nrow(df1) > 0) {

      if (!"error_rt_sd" %in% colnames(df1)) {
        error_error_rt <- NULL

      } else {
        df1$error_rt_sd[is.na(df1$error_rt_sd)] <- 0

        error_error_rt <- list(
          type = "data",
          symmetric = FALSE,
          arrayminus = df1$error_rt_sd,
          array = df1$error_rt_sd,
          color = colors[i],
          width = 5
        )
      }

      p1 <- p1 %>% add_trace(df1,
        x = df1$analysis,
        y = df1$error_rt,
        type = "scatter", mode = "markers",
        marker = list(size = 5, color = colors[i]),
        error_y = error_error_rt,
        connectgaps = FALSE,
        name = i,
        legendgroup = i,
        showlegend = showLegendInSecond
      )
    }

    df2 <- df[!is.na(df$error_mass), ]

    if (nrow(df2) > 0) {

      if (!"error_mass_sd" %in% colnames(df2)) {
        error_error_mass <- NULL

      } else {
        df2$error_mass_sd[is.na(df2$error_mass_sd)] <- 0

        error_error_mass <- list(
          type = "data",
          symmetric = FALSE,
          arrayminus = df2$error_mass_sd,
          array = df2$error_mass_sd,
          color = colors[i],
          width = 5
        )
      }

      p2 <- p2 %>% add_trace(df2,
        x = df2$analysis,
        y = df2$error_mass,
        type = "scatter", mode = "markers",
        marker = list(size = 5, color = colors[i]),
        error_y = error_error_mass,
        connectgaps = FALSE,
        name = i,
        legendgroup = i,
        showlegend = FALSE
      )
    }

    if (do_rec) {
      df3 <- df[!is.na(df$rec), ]

      if (nrow(df3) > 0) {

        if (!"rec_sd" %in% colnames(df3)) {
          error_rec <- NULL
        } else {
          df3$rec_sd[is.na(df3$rec_sd)] <- 0

          error_rec <- list(
            type = "data",
            symmetric = FALSE,
            arrayminus = df3$rec_sd,
            array = df3$rec_sd,
            color = colors[i],
            width = 5
          )
        }

        p3 <- p3 %>% add_trace(df3,
          x = df3$analysis,
          y = df3$rec * 100,
          type = "scatter", mode = "markers",
          marker = list(size = 5, color = colors[i]),
          error_y = error_rec,
          connectgaps = TRUE,
          name = i,
          legendgroup = i,
          showlegend = FALSE
        )
      }
    }

    df4 <- df[!is.na(df$rtr), ]

    if (nrow(df4) > 0) {

      if (!"rtr_sd" %in% colnames(df4)) {
        error_rtr <- NULL
      } else {
        df4$rtr_sd[is.na(df4$rtr_sd)] <- 0

        error_rtr <- list(
          type = "data",
          symmetric = FALSE,
          arrayminus = df4$rtr_sd,
          array = df4$rtr_sd,
          color = colors[i],
          width = 5
        )
      }

      p4 <- p4 %>% add_trace(df4,
        x = df4$analysis,
        y = df4$rtr,
        type = "scatter", mode = "markers",
        marker = list(size = 5, color = colors[i]),
        error_y = error_rtr,
        connectgaps = TRUE,
        name = i,
        legendgroup = i,
        showlegend = FALSE
      )
    }

    df5 <- df[!is.na(df$mzr), ]

    if (nrow(df5) > 0) {

      if (!"mzr_sd" %in% colnames(df5)) {
        error_mzr <- NULL
      } else {
        df5$mzr_sd[is.na(df5$mzr_sd)] <- 0

        error_mzr <- list(
          type = "data",
          symmetric = FALSE,
          arrayminus = df5$mzr_sd,
          array = df5$mzr_sd,
          color = colors[i],
          width = 5
        )
      }

      p5 <- p5 %>% add_trace(df5,
        x = df5$analysis,
        y = df5$mzr,
        type = "scatter", mode = "markers",
        marker = list(size = 5, color = colors[i]),
        error_y = error_mzr,
        connectgaps = TRUE,
        name = i,
        legendgroup = i,
        showlegend = FALSE
      )
    }
  }

  xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = NULL)

  yaxis0 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Presence / %",
    titlefont = list(size = 12, color = "black"),
    range = c(0, 110)
  )

  yaxis1 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "RT / s",
    titlefont = list(size = 12, color = "black"),
    range = c(-15, 15)
  )

  yaxis2 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Mass / ppm",
    titlefont = list(size = 12, color = "black"),
    range = c(-10, 10)
  )

  yaxis3 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Recovery / %",
    titlefont = list(size = 12, color = "black"),
    range = c(0, 200)
  )

  yaxis4 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Width / s",
    titlefont = list(size = 12, color = "black"),
    range = c(0, 60)
  )

  yaxis5 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Width / Da",
    titlefont = list(size = 12, color = "black"),
    range = c(0, 0.01)
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

  if ("freq" %in% colnames(istd)) {
    p0 <- p0 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis0, shapes = hrect(90, 110))
    plotList[["p0"]] <- p0
  }

  p1 <- p1 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis1, shapes = hrect(10, -10))
  plotList[["p1"]] <- p1

  p2 <- p2 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis2, shapes = hrect(5, -5))
  plotList[["p2"]] <- p2

  if (do_rec) {
    p3 <- p3 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis3, shapes = hrect(50, 150))
    plotList[["p3"]] <- p3
  }

  p4 <- p4 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis4,
    shapes = hrect(mean(istd$rtr) - sd(istd$rtr), mean(istd$rtr) + sd(istd$rtr)))
  plotList[["p4"]] <- p4

  p5 <- p5 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis5,
    shapes = hrect(mean(istd$mzr) - sd(istd$mzr), mean(istd$mzr) + sd(istd$mzr)))
  plotList[["p5"]] <- p5

  # plot3 <- plot3 %>% plotly::layout(xaxis = xaxis3, yaxis = yaxis3)

  plotf <- subplot(
    plotList,
    nrows = length(plotList),
    titleY = TRUE, titleX = TRUE,
    # heights = heights[1:2],
    # margin = 0.05,
    shareX = TRUE,
    which_layout = "merge"
  )


  # plotf_2 <- subplot(
  #   list(plotf, plot3),
  #   nrows = 2,
  #   titleY = TRUE, titleX = TRUE,
  #   heights = c(sum(heights[1:2]), heights[3]),
  #   margin = 0.01,
  #   shareX = FALSE,
  #   which_layout = "merge"
  # )
  #
  # plotf_2 <- plotf_2 %>% plotly::layout(
  #   legend = list(title = list(text = paste("<b>", "targets", "</b>")))
  # )




  plotf

}

#' .plot_chromatograms_interactive
#'
#' @noRd
#'
.plot_chromatograms_interactive <- function(chromatograms = NULL,
                                            legendNames = NULL,
                                            colorBy = "targets",
                                            title = NULL,
                                            showLegend = TRUE) {
  
  chromatograms <- .make_colorBy_varkey(chromatograms, colorBy, legendNames)
  
  leg <- unique(chromatograms$var)
  cl <- .get_colors(leg)
  
  chromatograms$loop <- paste0(chromatograms$analysis, chromatograms$id, chromatograms$var)
  loop_key <- unique(chromatograms$loop)
  
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
  
  if (showLegend) {
    showL <- rep(TRUE, length(leg))
  } else {
    showL <- rep(FALSE, length(leg))
  }
  
  names(showL) <- leg
  
  for (t in loop_key) {
    select_vector <- chromatograms$loop %in% t
    lt <- unique(chromatograms$var[select_vector])
    x <- chromatograms$rt[select_vector]
    y <- chromatograms$intensity[select_vector]
    
    t_pol <- unique(chromatograms$polarity[select_vector])
    t_pre_ce <- unique(chromatograms$pre_ce[select_vector])
    t_pre_mz <- unique(chromatograms$pre_mz[select_vector])
    t_pro_mz <- unique(chromatograms$pro_mz[select_vector])
    
    plot <- plot %>% add_trace(
      x = x,
      y = y,
      type = "scatter", mode = "lines+markers",
      line = list(width = 0.5, color = unname(cl[lt])),
      marker = list(size = 2, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste(
        "<br>id: ", lt,
        "<br>polarity: ", t_pol,
        "<br>pre_ce: ", t_pre_ce,
        "<br>pre_mz: ", t_pre_mz,
        "<br>pro_mz: ", t_pro_mz,
        "<br>rt: %{x}",
        "<br>", "intensity: %{y}"
      )
    )
    if (length(y) >= 1) showL[lt] <- FALSE
  }
  
  
  if (showLegend) {
    plot <- plot %>% plotly::layout(
      legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  } else {
    plot <- plot %>% plotly::layout(
      legend = NULL,
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
  }
  
  plot
}

#' .plot_suspects_interactive
#'
#' @noRd
#'
.plot_suspects_interactive <- function(suspects, eic, heights) {
  
  leg <- unique(eic$var)
  
  colors <- .get_colors(leg)
  
  showleg <- rep(TRUE, length(leg))
  
  names(showleg) <- names(leg)
  
  plot_qlt <- FALSE
  
  if ("quality" %in% colnames(suspects)) plot_qlt <- TRUE
  
  plot <- plot_ly()
  
  for (g in leg) {
    uid <- unique(eic$uid[eic$var == g])
    
    for (u in uid) {
      df <- eic[eic$uid == u, ]
      ft <- suspects[suspects$uid == u, ]
      
      plot <- plot %>% add_trace(
        df,
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
      
      plot <- plot %>% add_trace(
        df,
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
          "</br> suspect: ", g,
          "</br> id_level: ", ft$id_level,
          "</br> error_mass: ", ft$error_mass,
          "</br> error_rt: ", ft$error_rt,
          "</br> shared_fragments: ", ft$shared_fragments,
          "</br> group: ", ft$group,
          "</br> feature: ", ft$feature,
          "</br> analysis: ", ft$analysis,
          "</br> <i>m/z</i>: ", round(ft$mz, digits = 4),
          "</br> rt: ", round(df$rt, digits = 0),
          "</br> intensity: ", round(df$intensity, digits = 0),
          "</br> filled: ",
          if ("is_filled" %in% colnames(ft)) {
            ifelse(ft$is_filled == 1, TRUE, FALSE)
          } else {
            FALSE
          },
          if (plot_qlt) {
            q_t <- ft$quality[[1]]
            
            if (!is.null(q_t$model)) {
              paste(
                "</br> noise: ", q_t$noise,
                "</br> sn: ", q_t$sn,
                "</br> gaufit: ", round(q_t$gaufit, digits = 4),
                "</br> A: ", q_t$A,
                "</br> mu: ", q_t$mu,
                "</br> sigma: ", q_t$sigma
              )
            }
          } else {
            ""
          }
        )
      )
      showleg[which(leg %in% g)] <- FALSE
    }
  }
  
  plot2 <- plot_ly()
  
  for (g in leg) {
    uid <- unique(suspects$uid[suspects$var == g])
    
    for (u in uid) {
    
      data <- suspects$ms2[suspects$uid == u][[1]]
      
      fragments <- suspects$fragments[suspects$uid == u]
      
      if (!is.null(data) && !is.na(fragments)) {
        
        bar_widths <- rep(0.2, nrow(data))
        
        data$intensity <- data$intensity / max(data$intensity)

        plot2 <- plot2 %>% add_trace(
          data = data,
          x = data$mz,
          y = data$intensity,
          type = "bar",
          width = 0.05,
          marker = list(
            color = colors[g],
            line = list(color = colors[g], width = bar_widths)
          ),
          name = g,
          legendgroup = g,
          hovertemplate = paste("Exp:","<br><i>m/z</i>: %{x:.4f}", "<br>intensity: %{y:.0f}"),
          showlegend = FALSE
        )
        
        fragments <- unlist(strsplit(fragments, split = "; ", fixed = TRUE))
        fragments <- strsplit(fragments, " ")
        fragments <- data.table(
          "mz" = vapply(fragments, function(x) as.numeric(x[1]), NA_real_),
          "intensity" = vapply(fragments, function(x) as.numeric(x[2]), NA_real_)
        )
        
        fragments$intensity <- fragments$intensity / max(fragments$intensity)

        fragments$intensity <- -fragments$intensity
        
        plot2 <- plot2 %>% add_trace(
          data = fragments,
          x = fragments$mz,
          y = fragments$intensity,
          type = "bar",
          width = 0.05,
          marker = list(
            color = colors[g],
            line = list(color = colors[g], width = bar_widths)
          ),
          name = g,
          legendgroup = g,
          hovertemplate = paste("Database:","<br><i>m/z</i>: %{x:.4f}", "<br>intensity: %{y:.0f}"),
          showlegend = FALSE
        )
      }
    }
  }
  
  xaxis1 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Retention time / seconds",
    titlefont = list(size = 12, color = "black"),
    autotick = TRUE, ticks = "outside"
  )
  
  xaxis2 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "<i>m/z</i> / Da",
    titlefont = list(size = 12, color = "black"),
    autotick = TRUE, ticks = "outside"
  )
  
  yaxis1 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis2 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Normalized intensity",
    range = c(-1.3, 1.3),
    titlefont = list(size = 12, color = "black")
  )
  
  plotList <- list()
  
  plot <- plot %>% plotly::layout(xaxis = xaxis1, yaxis = yaxis1)
  plotList[["plot"]] <- plot
  
  plot2 <- plot2 %>% plotly::layout(xaxis = xaxis2, yaxis = yaxis2)
  plotList[["plot2"]] <- plot2
  
  plotf <- subplot(
    plotList,
    nrows = 2,
    titleY = TRUE, titleX = TRUE,
    heights = heights[1:2],
    margin = 0.03,
    shareX = FALSE,
    which_layout = "merge"
  )
  
  plotf <- plotf %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", "suspects", "</b>")))
  )
  
  return(plotf)
}
