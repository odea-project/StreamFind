
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
    df <- data.frame(n = seq_len(Ncol), char = obj)
    count <- table(df$char)
    count <- as.data.frame(count)
    Vcol <- rep(Vcol, times = count[, "Freq"])
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
    
  } else if (("analyses+polarities" %in% colorBy || "polarities+analyses" %in% colorBy) && "polarity" %in% colnames(data)) {
    varkey <- paste0(data$analysis, " - ", data$polarity)
    
  } else if (("replicates+polarities" %in% colorBy || "polarities+replicates" %in% colorBy) && "polarity" %in% colnames(data)) {
    varkey <- paste0(data$replicate, " - ", data$polarity)
    
  } else if ("levels" %in% colorBy && "level" %in% colnames(data)) {
    varkey <- data$level
    
  } else if (("levels+polarities" %in% colorBy || "polarities+levels" %in% colorBy) && "polarity" %in% colnames(data) && "level" %in% colnames(data)) {
    varkey <- paste0(data$level, " - ", data$polarity)
    
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

#' .plot_spectra_xic_interactive
#'
#' @noRd
#'
.plot_spectra_xic_interactive <- function(xic,
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

#' .plot_spectra_eic_static
#'
#' @noRd
#'
.plot_spectra_eic_static <- function(eic = NULL,
                                     legendNames = NULL,
                                     colorBy = "targets",
                                     title = NULL,
                                     showLegend = TRUE,
                                     xlim = NULL,
                                     ylim = NULL,
                                     cex = 0.6,
                                     xLab = NULL,
                                     yLab = NULL) {
  
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
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  plot(eic$rt,
       type = "n",
       xlab = xLab,
       ylab = yLab,
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

#' .plot_spectra_eic_interactive
#'
#' @noRd
#'
.plot_spectra_eic_interactive <- function(eic = NULL,
                                          legendNames = NULL,
                                          colorBy = "targets",
                                          title = NULL,
                                          showLegend = TRUE,
                                          xLab = NULL,
                                          yLab = NULL) {
  
  eic <- .make_colorBy_varkey(eic, colorBy, legendNames)
  
  leg <- unique(eic$var)
  cl <- .get_colors(leg)
  
  eic$loop <- paste0(eic$analysis, eic$id, eic$var)
  loop_key <- unique(eic$loop)
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
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
    plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
  } else {
    plot <- plot %>% plotly::layout(legend = NULL, xaxis = xaxis, yaxis = yaxis, title = title)
  }
  
  plot
}

#' .plot_spectra_bpc_interactive
#'
#' @noRd
#'
.plot_spectra_bpc_interactive <- function(bpc = NULL,
                                          legendNames = NULL,
                                          colorBy = "targets",
                                          title = NULL,
                                          showLegend = TRUE,
                                          xLab = NULL,
                                          yLab = NULL) {
  
  bpc <- .make_colorBy_varkey(bpc, colorBy, legendNames)
  
  leg <- unique(bpc$var)
  cl <- .get_colors(leg)
  
  bpc$loop <- paste0(bpc$analysis, bpc$id, bpc$var)
  loop_key <- unique(bpc$loop)
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
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
    plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
  } else {
    plot <- plot %>% plotly::layout(legend = NULL, xaxis = xaxis, yaxis = yaxis, title = title)
  }
  
  plot
}

#' .plot_spectra_ms2_static
#'
#' @noRd
#'
.plot_spectra_ms2_static <- function(ms2 = NULL, legendNames = NULL, colorBy = "targets", title = NULL, xLab = NULL, yLab = NULL) {
  
  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)
  
  cl <- .get_colors(unique(ms2$var))
  
  ms2$var <- as.factor(ms2$var)
  
  ms2$color <- cl[ms2$var]
  
  ms2$text = paste0(round(ms2$mz, 4))
  
  if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  plot(intensity ~ mz, ms2,
       type = "h",
       xlab = xLab,
       ylab = yLab,
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

#' .plot_spectra_ms2_interactive
#'
#' @noRd
#'
.plot_spectra_ms2_interactive <- function(ms2 = NULL, legendNames = NULL, colorBy = "targets", title = NULL, xLab = NULL, yLab = NULL) {
  
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
  
  if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black"),
    range = c(ticksMin, ticksMax),
    dtick = round((max(ms2$mz) / 10), -1),
    ticks = "outside"
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = yLab,
    titlefont = list(size = 12, color = "black"),
    range = c(0, max(ms2$intensity) * 1.5)
  )
  
  plot <- plot %>% plotly::layout(bargap = 1, title = title, xaxis = xaxis, yaxis = yaxis, barmode = "overlay", uniformtext = list(minsize = 6, mode = "show"))
  
  return(plot)
}

#' .plot_spectra_ms1_static
#'
#' @noRd
#'
.plot_spectra_ms1_static <- function(ms1 = NULL, legendNames = NULL, colorBy = "targets", title = NULL, xLab = NULL, yLab = NULL, showText = FALSE) {
  
  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)
  
  cl <- .get_colors(unique(ms1$var))
  
  ms1$var <- as.factor(ms1$var)
  
  ms1$color <- cl[ms1$var]
  
  ms1$text = paste0(round(ms1$mz, 4))
  
  if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  plot(intensity ~ mz, ms1,
       type = "h",
       xlab = xLab,
       ylab = yLab,
       col = ms1$color,
       lwd = 2,
       ylim = c(0, max(ms1$intensity) * 1.5),
       main = title,
       yaxs = "i",
       xaxt = "n"
  )
  
  if (showText) {
    text(
      x = ms1$mz, y = ms1$intensity, adj = c(-0.1, 0.25),
      labels = ms1$text, vfont = NULL,
      cex = 0.6, col = ms1$color, font = NULL, srt = 90
    )
  }
  
  ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)
  
  axis(1, seq(ticksMin, ticksMax, 10),lwd = 1.5, cex.axis = 0.8)
  
  axis(1, seq(ticksMin, ticksMax, 5), labels = FALSE, lwd = 1, col = "darkgray")
  
  axis(1, seq(ticksMin, ticksMax, 2.5), labels = FALSE, lwd = 0.5, col = "darkgray")
  
  legend("topleft", legend = levels(ms1$var), col = cl, lty = 1, cex = 0.8, bty = "n")
}

#' .plot_spectra_ms1_interactive
#'
#' @noRd
#'
.plot_spectra_ms1_interactive <- function(ms1 = NULL, legendNames = NULL, colorBy = "targets", title = NULL, xLab = NULL, yLab = NULL, showText = FALSE) {
  
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
  
  if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black"),
    range = c(ticksMin, ticksMax),
    dtick = round((max(ms1$mz) / 10), -1),
    ticks = "outside"
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = yLab,
    titlefont = list(size = 12, color = "black"),
    range = c(0, max(ms1$intensity) * 1.5)
  )
  
  plot <- plot %>% plotly::layout(bargap = 1, title = title, xaxis = xaxis, yaxis = yaxis, barmode = "overlay", uniformtext = list(minsize = 6, mode = "show"))
  
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
                                  xLab = NULL,
                                  yLab = NULL,
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
  
  ids <- unique(features$unique_ids)
  
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
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  plot(eic$rt,
       type = "n",
       xlab = xLab,
       ylab = yLab,
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
                                       xLab = NULL,
                                       yLab = NULL,
                                       title = NULL,
                                       showLegend = TRUE) {
  
  eic <- .make_colorBy_varkey(eic, colorBy, legendNames)
  
  leg <- unique(eic$var)
  
  cl <- .get_colors(leg)
  
  eic$unique_ids <- paste0(eic$id, eic$analysis)
  
  features$unique_ids <- paste0(features$feature, features$analysis)
  
  ids <- unique(features$unique_ids)
  
  plot_qlt <- FALSE
  
  if ("quality" %in% colnames(features)) {
    plot_qlt <- TRUE
  }
  
  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    range = c(min(eic$rt), max(eic$rt)),
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
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
      ifelse("group" %in% colnames(pk), paste("</br> group: ", pk$group), ""),
      "</br> analysis: ", pk$analysis,
      "</br> <i>m/z</i>: ", round(pk$mz, digits = 4),
      "</br> dppm: ", round(((pk$mzmax - pk$mzmin) / pk$mz) * 1E6, digits = 0),
      "</br> rt: ", round(pk$rt, digits = 0),
      "</br> drt: ", round(pk$rtmax - pk$rtmin, digits = 0),
      "</br> intensity: ", round(pk$intensity, digits = 0),
      "</br> filtered: ", pk$filtered,
      
      if ("dqsPeak" %in% colnames(pk)) {
        paste("</br> DQS: ", pk$dqsPeak)
      } else {
        ""
      },
      
      if ("filled" %in% colnames(pk)) {
        paste("</br> filled: ", pk$filled)
      } else {
        ""
      },
      
      if (plot_qlt) {
        q_t <- pk$quality[[1]]
        if (length(q_t) > 0) {
          paste(
            "</br> noise: ", round(q_t$noise, digits = 0),
            "</br> sn: ", round(q_t$sn, digits = 1),
            "</br> gaufit: ", round(q_t$gauss_f, digits = 4),
            "</br> A: ", round(q_t$gauss_a, digits = 2),
            "</br> mu: ", round(q_t$gauss_u, digits = 2),
            "</br> sigma: ", round(q_t$gauss_s, digits = 2)
          )
        } else {
          ""
        }
      } else {
        ""
      },
      
      if ("annotation" %in% colnames(pk)) {
        anno <- pk$annotation[[1]]
        if (length(anno) > 0) {
          paste(
            "</br> component: ", vapply(pk$annotation, function(x) x$component_feature, NA_character_),
            "</br> isotope: ", vapply(pk$annotation, function(x) x$iso_cat, NA_character_),
            "</br> iso_elements: ", vapply(pk$annotation, function(x) x$iso_isotope, NA_character_),
            "</br> iso_number_carbons: ", vapply(pk$annotation, function(x) round(x$iso_number_carbons, digits = 0), NA_real_),
            "</br> iso_mass_error: ", vapply(pk$annotation, function(x) round(x$iso_mass_error, digits = 5), NA_real_),
            "</br> iso_time_error: ", vapply(pk$annotation, function(x) round(x$iso_time_error, digits = 1), NA_real_),
            "</br> adduct: ", vapply(pk$annotation, function(x) x$adduct_cat, NA_character_),
            "</br> adduct_mass_error: ", vapply(pk$annotation, function(x) round(x$adduct_mass_error, digits = 5), NA_real_),
            "</br> adduct_time_error: ", vapply(pk$annotation, function(x) round(x$adduct_time_error, digits = 1), NA_real_)
          )
        } else {
          ""
        }
      } else {
        ""
      }
    )
    
    # if (plot_qlt) {
    #   
    #   if (length(q_t) > 0) {
    #     qlt_eic <- pk$eic[[1]]
    #     pred_eic <- vapply(qlt_eic$rt, function(x) q_t$gauss_a * exp(-((x - q_t$gauss_u)^2) / (2 * q_t$gauss_s^2)), 0)
    #     raise_val <- max(pk_eic$intensity) - max(pred_eic)
    #     pred_eic <- pred_eic + raise_val
    #     plot <- plot %>%  plotly::add_trace(
    #       x = qlt_eic$rt,
    #       y = pred_eic,
    #       type = 'scatter',
    #       name = lt,
    #       legendgroup = lt,
    #       mode = 'lines',
    #       line = list(dash = 'dash', color = unname(cl[lt])),
    #       showlegend = FALSE
    #     )
    #     
    #     plot <- plot %>%  plotly::add_trace(
    #       x = qlt_eic$rt,
    #       y = rep(q_t$noise, length(qlt_eic$rt)),
    #       type = 'scatter',
    #       name = lt,
    #       legendgroup = lt,
    #       mode = 'lines',
    #       line = list(dash = 'dot', color = unname(cl[lt])),
    #       showlegend = FALSE
    #     )
    #   }
    # }
    
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
                                 xLab = NULL,
                                 yLab = NULL, 
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
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- expression(italic("m/z ") / " Da")
  
  plot(features$rt,
       features$mz,
       type = "n",
       xlab = xLab,
       ylab = yLab,
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
                                      xlim = 60,
                                      ylim = 5,
                                      xLab = NULL,
                                      yLab = NULL,
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
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "<i>m/z</i> / Da"
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black"),
    range = rtr,
    autotick = TRUE, ticks = "outside"
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
    range = mzr,
    titlefont = list(size = 12, color = "black")
  )
  
  if (showLegend) {
    plot <- plot %>% plotly::layout(
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
      ft <- features[features$uid == u, ]
      if (nrow(ft) == 0) next
      df <- eic[eic$uid == u, ]
      
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
                                   "</br> replicate: ", ft$replicate,
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
        "</br> replicate: ", ft_nf$replicate,
        "</br> intensity: ", round(ft_nf$intensity, digits = 0),
        "</br> width: ", round(ft_nf$rtmax - ft_nf$rtmin, digits = 0),
        "</br> dppm: ", round(((ft_nf$mzmax - ft_nf$mzmin) / ft_nf$mz) *
                                1E6, digits = 1),
        "</br> filled: ", ft_nf$is_filled,
        "</br> filtered: ", ft_nf$filtered,
        "</br> filter: ", ft_nf$filter,
        if ("quality" %in% colnames(ft_nf)) {
          q_t <- ft_nf$quality
          q_t <- lapply(q_t, function(x) if (length(x) == 0) list(noise = 0, sn = 0, gauss_f = 0, gauss_a = 0, gauss_u = 0, gauss_s = 0) else x)
          if (length(q_t) > 0) {
            paste(
              "</br> noise: ", vapply(q_t, function(x) round(x$noise, digits = 0), 0),
              "</br> sn: ", vapply(q_t, function(x) round(x$sn, digits = 1), 0),
              "</br> gaufit: ", vapply(q_t, function(x) round(x$gauss_f, digits = 4), 0),
              "</br> A: ", vapply(q_t, function(x) round(x$gauss_a, digits = 2), 0),
              "</br> mu: ", vapply(q_t, function(x) round(x$gauss_u, digits = 2), 0),
              "</br> sigma: ", vapply(q_t, function(x) round(x$gauss_s, digits = 2), 0)
            )
          } else {
            ""
          }
        } else {
          ""
        },
        if ("annotation" %in% colnames(ft_nf)) {
          anno <- ft_nf$annotation[[1]]
          if (length(anno) > 0) {
            paste(
              "</br> component: ", vapply(ft_nf$annotation, function(x) x$component_feature, NA_character_),
              "</br> isotope: ", vapply(ft_nf$annotation, function(x) x$iso_cat, NA_character_),
              "</br> iso_elements: ", vapply(ft_nf$annotation, function(x) x$iso_isotope, NA_character_),
              "</br> iso_number_carbons: ", vapply(ft_nf$annotation, function(x) round(x$iso_number_carbons, digits = 0), NA_real_),
              "</br> iso_mass_error: ", vapply(ft_nf$annotation, function(x) round(x$iso_mass_error, digits = 5), NA_real_),
              "</br> iso_time_error: ", vapply(ft_nf$annotation, function(x) round(x$iso_time_error, digits = 1), NA_real_),
              "</br> adduct: ", vapply(ft_nf$annotation, function(x) x$adduct_cat, NA_character_),
              "</br> adduct_mass_error: ", vapply(ft_nf$annotation, function(x) round(x$adduct_mass_error, digits = 5), NA_real_),
              "</br> adduct_time_error: ", vapply(ft_nf$annotation, function(x) round(x$adduct_time_error, digits = 1), NA_real_)
            )
          } else {
            ""
          }
        } else {
          ""
        }
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
          "</br> replicate: ", ft_f$replicate,
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
  
  plot3 <- plot_ly(features, x = sort(unique(features$analysis)))
  
  for (g in leg) {
    df_3 <- features[features$var == g, ]
    
    if (!all(analyses %in% df_3$analysis)) {
      extra <- data.frame(
        "analysis" = analyses[!analyses %in% df_3$analysis],
        "var" = g,
        "intensity" = 0
      )
      df_3 <- rbind(df_3[, c("analysis", "var", "intensity")], extra)
    }
    
    df_3 <- df_3[order(df_3$analysis), ]
    
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

#' .map_isotopes_interactive
#'
#' @noRd
#'
.map_components_interactive <- function(components, colorBy = "targets", legendNames = NULL, xlim = 60, ylim = 5,
                                        xLab = NULL, yLab = NULL, title = NULL, showLegend = TRUE) {
  
  if (grepl("groups", colorBy) && "group" %in% colnames(components)) {
    components$id <- components$group
    colorBy <- sub("groups", "targets", colorBy)
  } else {
    components$id <- components$component
  }
  
  components <- .make_colorBy_varkey(components, colorBy, legendNames)
  
  if (length(xlim) == 1) {
    rtr <- c(min(components$rtmin) - xlim, max(components$rtmax) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(components$rtmin), max(components$rtmax))
  }
  
  if (length(ylim) == 1) {
    mzr <- c(min(components$mzmin) - ylim, max(components$mzmax) + ylim)
  } else if (length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(components$mzmin), max(components$mzmax))
  }
  
  cl <- .get_colors(unique(components$var))
  
  plotlegend <- rep(TRUE, length(cl))
  
  names(plotlegend) <- names(cl)

  plot_qlt <- FALSE
  
  if ("quality" %in% colnames(components)) {
    plot_qlt <- TRUE
  }
    
  plot <- plot_ly()
  
  for (i in seq_len(nrow(components))) {
    
    x0 <- components$rtmin[i]
    x1 <- components$rtmax[i]
    y0 <- components$mzmin[i]
    y1 <- components$mzmax[i]
    
    plot <- plot %>% add_trace(
      x = c(x0, x1, x1, x0, x0),
      y = c(y0, y0, y1, y1, y0),
      type = "scatter",
      mode = "lines",
      fill = "none",
      line = list(color = cl[components$var[i]]),
      opacity = 0.2,
      name = components$var[i],
      legendgroup = components$var[i],
      showlegend = FALSE
    )
  }
  
  for (i in seq_len(nrow(components))) {
    ft <- components[i, ]
    
    x <- ft$rt
    y <- ft$mz
    
    plot <- plot %>% add_trace(
      x = x, y = y,
      type = "scatter", mode = "markers+text",
      marker = list(size = 15 * ft$iso_relative_intensity, color = cl[ft$var]),
      name = ft$var,
      legendgroup = ft$var,
      showlegend = ifelse(ft$iso_cat == "M", FALSE, plotlegend[ft$var]),
      text =  paste0(ft$iso_cat, " ", ft$iso_isotope),
      textposition = "midle right",
      textfont = list(size = 12, color = cl[ft$var]),
      hovertext = paste(
        "</br> analysis: ", ft$analysis,
        "</br> component: ", ft$component,
        "</br> feature: ", ft$feature,
        "</br> <i>m/z</i>: ", round(y, digits = 4),
        "</br> dppm: ", round(((ft$mzmax - ft$mzmin) / y) * 1E6, digits = 0),
        "</br> rt: ", round(x, digits = 0),
        "</br> drt: ", round(ft$rtmax - ft$rtmin, digits = 0),
        
        if ("dqsPeak" %in% colnames(ft)) {
          paste("</br> DQS: ", ft$dqsPeak)
        } else {
          ""
        },
        
        if ("filled" %in% colnames(ft)) {
          paste("</br> filled: ", ft$filled)
        } else {
          ""
        },
        
        if (plot_qlt) {
          q_t <- ft$quality[[1]]
          if (length(q_t) > 0) {
            paste(
              "</br> noise: ", round(q_t$noise, digits = 0),
              "</br> sn: ", round(q_t$sn, digits = 1),
              "</br> gaufit: ", round(q_t$gauss_f, digits = 4),
              "</br> A: ", q_t$gauss_a,
              "</br> mu: ", q_t$gauss_u,
              "</br> sigma: ", q_t$gauss_s
            )
          } else {
            ""
          }
        } else {
          ""
        },
        "</br> intensity: ", round(ft$intensity, digits = 0),
        "</br> rel_intensity (%): ", round(ft$iso_relative_intensity * 100, digits = 2),
        "</br> isotope: ", ft$iso_cat,
        "</br> iso_elements: ", ft$iso_isotope,
        "</br> iso_number_carbons: ", round(ft$iso_number_carbons, digits = 0),
        "</br> iso_mass_error: ", round(ft$iso_mass_error, digits = 5),
        "</br> iso_time_error: ", round(ft$iso_time_error, digits = 1),
        "</br> adduct: ", ft$adduct_cat,
        "</br> adduct_mass_error: ", round(ft$adduct_mass_error, digits = 5),
        "</br> adduct_time_error: ", round(ft$adduct_time_error, digits = 1)
      )
    )
    
    if (isTRUE(plotlegend[ft$var]) & ft$iso_cat != "M") {
      plotlegend[ft$var] <- FALSE
    }
  }
  
  title <- list(
    text = title, x = 0.1, y = 0.98,
    font = list(size = 9, color = "black")
  )
  
  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  
  if (is.null(yLab)) {
    yLab <- "<i>m/z</i> / Da"
  }
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black"),
    range = rtr,
    autotick = TRUE, ticks = "outside"
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
    range = mzr,
    titlefont = list(size = 12, color = "black")
  )
  
  plot <- plot %>% plotly::layout(
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )
  
  plot
}

#' .map_components_static
#'
#' @noRd
#'
.map_components_static <- function(components, colorBy = "targets", legendNames = NULL, xlim = 60, ylim = 5,
                                   xLab = NULL, yLab = NULL, title = NULL, showLegend = TRUE) {
  
  if (grepl("groups", colorBy) && "group" %in% colnames(components)) {
    components$id <- components$group
    colorBy <- sub("groups", "targets", colorBy)
  } else {
    components$id <- components$component
  }
  
  components <- .make_colorBy_varkey(components, colorBy, legendNames)
  
  if (length(xlim) == 1) {
    rtr <- c(min(components$rtmin) - xlim, max(components$rtmax) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(components$rtmin), max(components$rtmax))
  }
  
  ylim_oufset <- 1 + (0.02 * length(unique(components$var)))
  
  if (length(ylim) == 1) {
    mzr <- c(min(components$mzmin) - ylim, (max(components$mzmax) + ylim) * ylim_oufset)
  } else if (length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(components$mzmin), max(components$mzmax) * ylim_oufset)
  }
  
  cl <- .get_colors(unique(components$var))
  
  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  
  if (is.null(yLab)) {
    yLab <- expression(italic("m/z ") / " Da")
  }
  
  plot(components$rt,
       components$mz,
       type = "n",
       xlab = xLab,
       ylab = yLab,
       xlim = rtr,
       ylim = mzr,
       main = title
  )
  
  rect(
    xleft = components$rtmin,
    xright = components$rtmax,
    ybottom = components$mzmin,
    ytop = components$mzmax,
    col = paste0(cl[components$var], "70"),
    border = paste0(cl[components$var], "70")
  )
  
  points(
    x = components$rt,
    y = components$mz,
    type = "p",
    pch = 19,
    cex = 1.5 * components$iso_relative_intensity,
    col = cl[components$var]
  )
  
  for (i in seq_len(nrow(components))) {
    text(
      components$rt[i] + 0.2,
      components$mz[i],
      paste0(components$iso_cat, " ", components$iso_istope)[i],
      pos = 4, col = cl[components$var[i]], cex = 0.6
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

.plot_internal_standards_qc_interactive <- function(istd, analyses, presence, recovery, deviations, widths) {
  
  if (!is.data.frame(istd)) return(NULL)
  
  if (nrow(istd) == 0) return(NULL)
  
  if (!("analysis" %in% colnames(istd)) & "replicate" %in% colnames(istd)) istd$analysis <- istd$replicate
  
  leg <- unique(istd$name)
  
  colors <- .get_colors(leg)
  
  showLegend <- TRUE
  showLegendPresence <- FALSE
  showLegendRecovery <- FALSE
  showLegendDeviations <- FALSE
  showLegendWidths <- FALSE
  
  if (presence && "freq" %in% colnames(istd)) {
    plot_presence <- plot_ly(istd, x = unique(analyses))
    showLegendPresence <- TRUE
    showLegend <- FALSE
    max_freq <- max(istd$freq, na.rm = TRUE)
  }
  
  if (recovery && !all(is.na(istd$rec))) {
    plot_recovery <- plot_ly(istd, x = unique(analyses))
    if (showLegend) {
      showLegendRecovery <- TRUE
      showLegend <- FALSE
    }
  }
  
  if (deviations) {
    plot_rtr <- plot_ly(istd, x = unique(analyses))
    plot_mzr <- plot_ly(istd, x = unique(analyses))
    if (showLegend) {
      showLegendDeviations <- TRUE
      showLegend <- FALSE
    }
  }
  
  if (widths) {
    plot_rtw <- plot_ly(istd, x = unique(analyses))
    plot_mzw <- plot_ly(istd, x = unique(analyses))
    if (showLegend) {
      showLegendWidths <- TRUE
      showLegend <- FALSE
    }
  }
  
  freq_template <- rep(0, length(unique(analyses)))
  names(freq_template) <- unique(analyses)
  
  for (i in unique(istd$name)) {
    
    df <- istd[istd$name == i, ]
    
    if ("freq" %in% colnames(istd) && presence) {
      
      freq <- freq_template
      for (j in analyses) freq[j] <- sum(df$freq[df$analysis == j])
      freq <- freq / max_freq * 100
      
      plot_presence <- plot_presence %>% 
        add_trace(
          df,
          x = unique(analyses),
          y = freq,
          type = "scatter", mode = "markers",
          marker = list(size = 5, color = colors[i]),
          connectgaps = FALSE,
          name = i,
          legendgroup = i,
          showlegend = showLegendPresence
        )
    }
    
    if ("rec" %in% colnames(istd) && recovery) {
      
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
          add_trace(df_rec,
            x = df_rec$analysis,
            y = df_rec$rec * 100,
            type = "scatter", mode = "markers",
            marker = list(size = 5, color = colors[i]),
            error_y = error_rec,
            connectgaps = TRUE,
            name = i,
            legendgroup = i,
            showlegend = showLegendRecovery
          )
      }
    }
    
    if (deviations) {
      
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
            type = "scatter", mode = "markers",
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
            type = "scatter", mode = "markers",
            marker = list(size = 5, color = colors[i]),
            error_y = error_error_mass,
            connectgaps = FALSE,
            name = i,
            legendgroup = i,
            showlegend = FALSE
        )
      }
    }
    
    if (widths) {
      
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
            type = "scatter", mode = "markers",
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
  }
  
  if ("group" %in% colnames(istd)) {
    
    rt_error <- c(
      (min(istd$error_rt, na.rm = TRUE) - max(istd$error_rt_sd, na.rm = TRUE)) * 0.9,
      (max(istd$error_rt, na.rm = TRUE) + max(istd$error_rt_sd, na.rm = TRUE)) * 1.1)
    
    mz_error <- c(
      (min(istd$error_mass, na.rm = TRUE) - max(istd$error_mass_sd, na.rm = TRUE)) * 0.9,
      (max(istd$error_mass, na.rm = TRUE) + max(istd$error_mass_sd, na.rm = TRUE)) * 1.1)
    
    time_range <- c(0, (max(istd$rtr, na.rm = TRUE) + max(istd$rtr_sd, na.rm = TRUE)) * 1.1)
    
    mass_range <- c(0, (max(istd$mzr, na.rm = TRUE) + max(istd$mzr_sd, na.rm = TRUE)) * 1.1)
    
  } else {
    rt_error <- c(min(istd$error_rt, na.rm = TRUE) * 0.9, max(istd$error_rt, na.rm = TRUE) * 1.1)
    mz_error <- c(min(istd$error_mass, na.rm = TRUE) * 0.9, max(istd$error_mass, na.rm = TRUE) * 1.1)
    time_range <- c(0, max(istd$rtr, na.rm = TRUE) * 1.1)
    mass_range <- c(0, max(istd$mzr, na.rm = TRUE) * 1.1)
  }
  
  if (rt_error[1] >= -20) rt_error <- c(-20, rt_error[2])
  if (rt_error[2] <= 20) rt_error <- c(rt_error[1], 20)
  if (mz_error[1] >= -15) mz_error <- c(-15, mz_error[2])
  if (mz_error[2] <= 15) mz_error <- c(mz_error[1], 15)
  if (time_range[2] <= 30) time_range <- c(0, 30)
  if (mass_range[2] <= 0.01) mass_range <- c(0, 0.01)
  
  xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = NULL)
  
  yaxis_presence <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Presence / %",
    titlefont = list(size = 12, color = "black"),
    range = c(-10, 200)
  )
  
  yaxis_recovery <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Recovery / %",
    titlefont = list(size = 12, color = "black"),
    range = c(-10, 200)
  )
  
  yaxis_deviation_rt <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "RT / s",
    titlefont = list(size = 12, color = "black"),
    range = rt_error
  )
  
  yaxis_deviation_mz <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Mass / ppm",
    titlefont = list(size = 12, color = "black"),
    range = mz_error
  )
  
  yaxis_width_rt <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Width / s",
    titlefont = list(size = 12, color = "black"),
    range = time_range
  )
  
  yaxis_width_mz <- list(
    linecolor = toRGB("black"), linewidth = 2,
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
  
  if ("freq" %in% colnames(istd) && presence) {
    plot_presence <- plot_presence %>% plotly::layout(xaxis = xaxis, yaxis = yaxis_presence, shapes = hrect(90, 110))
    plotList[["plot_presence"]] <- plot_presence
  }
  
  if ("rec" %in% colnames(istd) && recovery) {
    plot_recovery <- plot_recovery %>% plotly::layout(xaxis = xaxis, yaxis = yaxis_recovery, shapes = hrect(50, 150))
    plotList[["plot_recovery"]] <- plot_recovery
  }
  
  if (deviations) {
    plot_rtr <- plot_rtr %>% plotly::layout(xaxis = xaxis, yaxis = yaxis_deviation_rt, shapes = hrect(-15, 15))
    plotList[["plot_rtr"]] <- plot_rtr
    plot_mzr <- plot_mzr %>% plotly::layout(xaxis = xaxis, yaxis = yaxis_deviation_mz, shapes = hrect(-10, 10))
    plotList[["plot_mzr"]] <- plot_mzr
  }
  
  if (widths) {
    plot_rtw <- plot_rtw %>% plotly::layout(xaxis = xaxis, yaxis = yaxis_width_rt, shapes = hrect(5, 25))
    plotList[["plot_rtw"]] <- plot_rtw
    plot_mzw <- plot_mzw %>% plotly::layout(xaxis = xaxis, yaxis = yaxis_width_mz, shapes = hrect(0, 0.005))
    plotList[["plot_mzw"]] <- plot_mzw
  }
  
  if (length(plotList) == 0) {
    NULL
  } else if (length(plotList) == 1) {
    plotList[[1]]
  } else {
    subplot(
      plotList,
      nrows = length(plotList),
      titleY = TRUE, titleX = TRUE,
      shareX = TRUE,
      which_layout = "merge"
    )
  }
}

#' .plot_chromatograms_interactive
#'
#' @noRd
#'
.plot_chromatograms_interactive <- function(chromatograms = NULL,
                                            legendNames = NULL,
                                            colorBy = "targets",
                                            xLab = NULL,
                                            yLab = NULL,
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
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
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
  
  plot <- plot_ly()
  
  for (g in leg) {
    uid <- unique(eic$uid[eic$var == g])
    
    for (u in uid) {
      ft <- suspects[suspects$uid == u, ]
      if (nrow(ft) == 0) next
      df <- eic[eic$uid == u, ]
      
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
          if ("quality" %in% colnames(ft)) {
            paste(
              "</br> noise: ", vapply(ft$quality, function(x) x$noise, NA_real_),
              "</br> sn: ", vapply(ft$quality, function(x) x$sn, NA_real_),
              "</br> gaufit: ", round(vapply(ft$quality, function(x) x$gaufit, NA_real_), digits = 4),
              "</br> A: ", vapply(ft$quality, function(x) x$A, NA_real_),
              "</br> mu: ", vapply(ft$quality, function(x) x$mu, NA_real_),
              "</br> sigma: ", vapply(ft$quality, function(x) x$sigma, NA_real_)
            )
          } else {
            ""
          },
          if ("isotope" %in% colnames(ft)) {
            paste(
              "</br> iso_cluster: ", vapply(ft$isotope, function(x) x$cluster, NA_real_),
              "</br> iso_size: ", vapply(ft$isotope, function(x) x$cluster_size, NA_real_),
              "</br> isotope: ", vapply(ft$isotope, function(x) x$tag, NA_character_),
              "</br> carbons: ", vapply(ft$isotope, function(x) x$carbons, NA_real_)
            )
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
  
  return(plotf)
}

#' .plot_spec_charges_static
#'
#' @noRd
#'
.plot_spec_charges_static <- function(spec = NULL,
                                      res = NULL,
                                      legendNames = NULL,
                                      colorBy = "targets",
                                      title = NULL,
                                      showLegend = TRUE,
                                      xlim = NULL,
                                      ylim = NULL,
                                      cex = 0.6,
                                      xLab = NULL,
                                      yLab = NULL) {
  
  res$unique_ids <- paste0(res$analysis, "_", res$id)
  
  res <- .make_colorBy_varkey(res, colorBy, legendNames)
  
  leg <- unique(res$var)
  
  cl <- .get_colors(leg)
  
  ids <- unique(res$unique_ids)
  
  xlab <- expression(italic("m/z ") / " Da")
  ylab <- "Intensity / counts"
  if (!is.null(xLab)) xlab <- xLab
  if (!is.null(yLab)) ylab <- yLab
  
  if (is.numeric(xlim) & length(xlim) == 1) {
    rtr <- c(min(spec$mz) - xlim, max(spec$mz) + xlim)
  } else if (is.numeric(xlim) & length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(spec$mz), max(spec$mz))
    if (showLegend) {
      rtr[2] <- rtr[2] * 1.01
    }
  }
  
  if (is.numeric(ylim) & length(ylim) == 1) {
    intr <- c(min(spec$intensity) - ylim, (max(spec$intensity) + ylim))
  } else if (is.numeric(ylim) & length(ylim) == 2) {
    intr <- ylim
  } else {
    intr <- c(0, max(spec$intensity) * 1.4)
  }
  
  if (is.null(cex) || !is.numeric(cex)) cex <- 0.6
  
  plot(
    spec$mz,
    type = "n",
    xlab = xlab,
    ylab = ylab,
    xlim = rtr,
    ylim = intr,
    main = title
  )
  
  for (t in ids) {
    select_vector <- res$unique_ids == t
    
    lt <- unique(res$var[select_vector])
    
    pk <- res[select_vector, ]
    
    sp <- spec[spec$analysis %in% pk$analysis[1] & spec$id %in% pk$id[1], ]
    
    lines(x = sp$mz, y = sp$intensity, type = "l", pch = 19, cex = 0.3, col = cl[lt])
    
    text(x = pk$mz, y = pk$intensity, adj = c(-0.1, 0.25),
         labels = paste0(pk$z, " - " , round(pk$mz, digits = 3)),
         vfont = NULL, cex = 0.6, col = "darkgreen", font = NULL, srt = 90
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

#' .plot_spec_charges_interactive
#'
#' @noRd
#'
.plot_spec_charges_interactive <- function(res = NULL,
                                           legendNames = NULL,
                                           colorBy = "targets",
                                           title = NULL,
                                           showLegend = TRUE,
                                           xLab = NULL,
                                           yLab = NULL) {
  
  res$unique_ids <- paste0(res$analysis, "_", res$id)
  
  res <- .make_colorBy_varkey(res, colorBy, legendNames)
  
  setorder(res, var)
  
  leg <- unique(res$var)
  
  cl <- .get_colors(leg)
  
  ids <- unique(res$unique_ids)
  
  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )
  
  xlab <- "<i>m/z</i> / Da"
  ylab <- "Intensity / counts"
  if (!is.null(xLab)) xlab <- xLab
  if (!is.null(yLab)) ylab <- yLab
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xlab,
    range = c(min(res$mz), max(res$mz)),
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = ylab,
    range = c(min(res$intensity), max(res$intensity) * 1.3),
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
    select_vector <- res$unique_ids == t
    lt <- unique(res$var[select_vector])
    pk <- res[select_vector, ]
    
    plot <- plot %>% add_trace(
      x = pk$mz,
      y = pk$intensity,
      type = "bar",
      width = 0.05,
      marker = list(
        color = unname(cl[lt]),
        line = list(color = unname(cl[lt]), width = 0.05)
      ),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste("<br>mz: %{x}<br>", "int: %{y}")
    )
    
    if (nrow(pk) >= 1) showL[lt] <- FALSE
    
    plot <- plot %>% add_trace(
      x = pk$mz,
      y = pk$intensity,
      type = "scatter", mode = "text",
      # marker = list(color = unname(cl[lt])),
      text = paste0(pk$z, "\n" , round(pk$mz, digits = 3)),
      textposition = "outside",
      # textangle = 90,
      textfont = list(size = 9, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = FALSE,
      hovertemplate = paste("<i>m/z</i>: %{x:.4f}", "<br>intensity: %{y:.0f}")
    )
  }
  
  if (showLegend) {
    plot <- plot %>% plotly::layout(
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

#' .plot_chrom_peaks_static
#'
#' @noRd
#'
.plot_chrom_peaks_static <- function(chroms = NULL,
                                     peaks = NULL,
                                     legendNames = NULL,
                                     colorBy = "targets",
                                     title = NULL,
                                     showLegend = TRUE,
                                     xlim = NULL,
                                     ylim = NULL,
                                     cex = 0.6,
                                     xLab = NULL,
                                     yLab = NULL) {
  
  peaks$unique_ids <- paste0(peaks$analysis, "_", peaks$id, "_", peaks$peak)
  
  peaks <- .make_colorBy_varkey(peaks, colorBy, legendNames)
  
  leg <- unique(peaks$var)
  
  cl <- .get_colors(leg)
  
  ids <- unique(peaks$unique_ids)
  
  if (!"raw" %in% colnames(chroms)) chroms$raw <- chroms$intensity
  
  xlab <- "Retention time / seconds"
  ylab <- "Intensity / counts"
  if (!is.null(xLab)) xlab <- xLab
  if (!is.null(yLab)) ylab <- yLab
  
  if (is.numeric(xlim) & length(xlim) == 1) {
    rtr <- c(min(chroms$rt) - xlim, max(chroms$rt) + xlim)
  } else if (is.numeric(xlim) & length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(chroms$rt), max(chroms$rt))
    if (showLegend) {
      rtr[2] <- rtr[2] * 1.01
    }
  }
  
  if (is.numeric(ylim) & length(ylim) == 1) {
    intr <- c(min(chroms$raw) - ylim, (max(chroms$raw) + ylim))
  } else if (is.numeric(ylim) & length(ylim) == 2) {
    intr <- ylim
  } else {
    intr <- c(0, max(chroms$raw))
  }
  
  if (is.null(cex) || !is.numeric(cex)) cex <- 0.6
  
  plot(
    chroms$rt,
    type = "n",
    xlab = xlab,
    ylab = ylab,
    xlim = rtr,
    ylim = intr,
    main = title
  )
  
  for (t in ids) {
    select_vector <- peaks$unique_ids == t
    
    lt <- unique(peaks$var[select_vector])
    
    pk <- peaks[select_vector, ]
    
    chrom <- chroms[chroms$analysis %in% pk$analysis[1] & chroms$id %in% pk$id[1], ]
    
    sel_chrom <- chrom$rt >= pk$rtmin[1] & chrom$rt <= pk$rtmax[1]
    
    pk_chrom <- chrom[sel_chrom, ]
    
    points(
      x = pk_chrom$rt,
      y = pk_chrom$raw,
      type = "p",
      pch = 19,
      cex = 0.2,
      col = cl[lt]
    )
    
    lines(
      x = chrom$rt,
      y = chrom$raw,
      type = "l",
      pch = 19,
      cex = 0.3,
      col = cl[lt]
    )
    
    if (!"baseline" %in% colnames(chrom)) pk_chrom$baseline <- rep(min(pk_chrom$raw), nrow(pk_chrom))
    
    polygon(
      c(pk_chrom$rt, rev(pk_chrom$rt)),
      c(pk_chrom$raw, rev(pk_chrom$baseline)),
      col = paste(color = unname(cl[lt]), 50, sep = ""),
      border = FALSE
    )
    
    # text(
    #   x = pk$rt[1], y = pk$intensity[1], adj = c(0.5, -1),
    #   labels = pk$peak[1],
    #   vfont = NULL, cex = 0.8, col = "black", font = NULL, srt = 0
    # )
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

#' .plot_chrom_peaks_interactive
#'
#' @noRd
#'
.plot_chrom_peaks_interactive <- function(chroms = NULL,
                                          peaks = NULL,
                                          legendNames = NULL,
                                          colorBy = "targets",
                                          title = NULL,
                                          showLegend = TRUE,
                                          xLab = NULL,
                                          yLab = NULL) {
  
  peaks$unique_ids <- paste0(peaks$analysis, "_", peaks$id, "_", peaks$peak)
  
  peaks <- .make_colorBy_varkey(peaks, colorBy, legendNames)
  
  leg <- unique(peaks$var)
  
  cl <- .get_colors(leg)
  
  ids <- unique(peaks$unique_ids)
  
  if (!"raw" %in% colnames(chroms)) chroms$raw <- chroms$intensity
  
  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )
  
  xlab <- "Retention time / seconds"
  ylab <- "Intensity / counts"
  if (!is.null(xLab)) xlab <- xLab
  if (!is.null(yLab)) ylab <- yLab
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xlab,
    range = c(min(chroms$rt), max(chroms$rt)),
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = ylab,
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
    select_vector <- peaks$unique_ids == t
    
    lt <- unique(peaks$var[select_vector])
    
    pk <- peaks[select_vector, ]
    
    chrom <- chroms[chroms$analysis %in% pk$analysis[1] & chroms$id %in% pk$id[1], ]
    
    sel_chrom <- chrom$rt >= pk$rtmin[1] & chrom$rt <= pk$rtmax[1]
    
    pk_chrom <- chrom[sel_chrom, ]
    
    plot <- plot %>% add_trace(
      x = chrom$rt,
      y = chrom$raw,
      type = "scatter", mode = "lines",
      line = list(width = 0.3, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste("<br>rt: %{x}<br>", "int: %{y}")
    )
    
    if (nrow(pk_chrom) >= 1) showL[lt] <- FALSE
    
    hT <- paste(
      "</br> analysis: ", pk$analysis[1],
      "</br> id: ", pk$id[1],
      "</br> peak: ", pk$peak[1],
      "</br> x: ", round(pk$rt[1], digits = 0),
      "</br> dx: ", round(pk$rtmax[1] - pk$rtmin[1], digits = 0),
      "</br> intensity: ", round(pk$intensity[1], digits = 0)
    )
    
    if (!"baseline" %in% colnames(chrom)) pk_chrom$baseline <- rep(min(pk_chrom$raw), nrow(pk_chrom))
    
    plot <- plot %>% add_trace(
      x = c(pk_chrom$rt, rev(pk_chrom$rt)),
      y = c(pk_chrom$raw, rev(pk_chrom$baseline)),
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
  }
  
  if (showLegend) {
    plot <- plot %>% plotly::layout(
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

#' .plot_x_spectra_static
#' 
#' @noRd
#'
.plot_x_spectra_static <- function(spectra, xLab, yLab, title, cex, showLegend) {
  
  
  cl <- .get_colors(unique(spectra$var))
  
  spectra$loop <- paste0(spectra$analysis, spectra$id, spectra$var)
  
  loop_key <- unique(spectra$loop)
  
  xr <- c(min(spectra$x), max(spectra$x))
  if (showLegend) {
    xr[2] <- xr[2] * 1.01
  }
  
  intr <- c(0, max(spectra$intensity))
  
  if (is.null(cex) || !is.numeric(cex)) cex <- 1
  
  plot(spectra$x,
       type = "n",
       xlab = xLab,
       ylab = yLab,
       xlim = xr,
       ylim = intr,
       main = title
  )
  
  for (t in loop_key) {
    
    select_vector <- spectra$loop %in% t
    
    lt <- unique(spectra$var[select_vector])
    
    lines(
      x = spectra$x[select_vector],
      y = spectra$intensity[select_vector],
      type = "l",
      pch = 19,
      cex = 0.5,
      col = cl[lt]
    )
  }
  
  if (showLegend) {
    legend(
      x = "topright",
      inset = 0.01,
      y.intersp = 1.5,
      legend = names(cl),
      col = cl,
      lwd = 2,
      lty = 1,
      cex = cex,
      bty = "n"
    )
  }
}

#' .plot_x_spectra_interactive
#' 
#' @noRd
#'
.plot_x_spectra_interactive <- function(spectra, xLab, yLab, title, colorBy) {
  
  leg <- unique(spectra$var)
  
  cl <- .get_colors(leg)
  
  spectra$loop <- paste0(spectra$analysis, spectra$id, spectra$var)
  
  loop_key <- unique(spectra$loop)
  
  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
    titlefont = list(size = 12, color = "black")
  )
  
  plot <- plot_ly()
  
  showL <- rep(TRUE, length(leg))
  
  names(showL) <- leg
  
  for (t in loop_key) {
    select_vector <- spectra$loop %in% t
    lt <- unique(spectra$var[select_vector])
    x <- spectra$x[select_vector]
    y <- spectra$intensity[select_vector]
    
    plot <- plot %>% add_trace(
      x = x,
      y = y,
      type = "scatter", mode = "lines+markers",
      line = list(width = 0.5, color = unname(cl[lt])),
      marker = list(size = 2, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste("<br>x: %{x}<br>", "y: %{y}")
    )
    
    if (length(y) >= 1) showL[lt] <- FALSE
  }
  
  plot <- plot %>% plotly::layout(
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )
  
  plot
}

#' .plot_x_spectra_baseline_static
#' 
#' @noRd
#'
.plot_x_spectra_baseline_static <- function(spectra, xLab, yLab, title, cex, showLegend) {
  
  cl <- .get_colors(unique(spectra$var))
  
  spectra$loop <- paste0(spectra$analysis, spectra$id, spectra$var)
  
  loop_key <- unique(spectra$loop)
  
  xr <- c(min(spectra$x), max(spectra$x))
  if (showLegend) {
    xr[2] <- xr[2] * 1.01
  }
  
  intr <- c(0, max(spectra$raw))
  
  if (is.null(cex) || !is.numeric(cex)) cex <- 1
  
  plot(spectra$x,
       type = "n",
       xlab = xLab,
       ylab = yLab,
       xlim = xr,
       ylim = intr,
       main = title
  )
  
  for (t in loop_key) {
    
    select_vector <- spectra$loop %in% t
    
    lt <- unique(spectra$var[select_vector])
    
    lines(
      x = spectra$x[select_vector],
      y = spectra$raw[select_vector],
      type = "l",
      lty = 1,
      lwd = 1,
      pch = 19,
      cex = 0.5,
      col = cl[lt]
    )
    
    lines(
      x = spectra$x[select_vector],
      y = spectra$baseline[select_vector],
      type = "l",
      lty = 2,
      lwd = 2,
      pch = 19,
      cex = 0.5,
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

#' .plot_x_spectra_baseline_interactive
#' 
#' @noRd
#'
.plot_x_spectra_baseline_interactive <- function(spectra, xLab, yLab, title, colorBy) {
  
  leg <- unique(spectra$var)
  
  cl <- .get_colors(leg)
  
  spectra$loop <- paste0(spectra$analysis, spectra$id, spectra$var)
  
  loop_key <- unique(spectra$loop)
  
  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
    titlefont = list(size = 12, color = "black")
  )
  
  plot <- plot_ly()
  
  showL <- rep(TRUE, length(leg))
  
  names(showL) <- leg
  
  for (t in loop_key) {
    select_vector <- spectra$loop %in% t
    lt <- unique(spectra$var[select_vector])
    x <- spectra$x[select_vector]
    y <- spectra$raw[select_vector]
    y2 <- spectra$baseline[select_vector]
    
    plot <- plot %>% add_trace(
      x = x,
      y = y,
      type = "scatter", mode = "lines+markers",
      line = list(width = 0.5, color = unname(cl[lt])),
      marker = list(size = 2, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste("<br>x: %{x}<br>", "y: %{y}")
    )
    
    plot <- plot %>% add_trace(
      x = x,
      y = y2,
      type = "scatter", mode = "lines",
      line = list(width = 2, color = unname(cl[lt]), dash = 'dash'),
      name = lt,
      legendgroup = lt,
      showlegend = FALSE,
      hovertemplate = paste("<br>x: %{x}<br>", "y: %{y}")
    )
    
    if (length(y) >= 1) showL[lt] <- FALSE
  }
  
  plot <- plot %>% plotly::layout(
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )
  
  plot
}

#' .plot_x_spectra_baseline_interactive
#' 
#' @noRd
#'
.map_x_spectra <- function(spectra,
                           xLab = NULL, yLab = NULL, title = NULL,
                           colorBy = "analyses",
                           legendNames = NULL,
                           xlim = NULL, ylim = NULL) {
  
  spec <- .make_colorBy_varkey(spectra, colorBy, legendNames)
  
  leg <- unique(spec$var)
  
  cl <- .get_colors(leg)
  
  brighter_color <- function(color_hex, factor = 1.5) {
    # Convert hexadecimal color to RGB values
    color_rgb <- col2rgb(color_hex)
    
    # Calculate brighter RGB values
    brighter_rgb <- color_rgb * factor
    
    # Ensure RGB values are within [0, 255] range
    brighter_rgb[brighter_rgb > 255] <- 255
    
    # Convert RGB values back to hexadecimal format
    brighter_hex <- rgb(brighter_rgb[1, ], brighter_rgb[2, ], brighter_rgb[3, ], maxColorValue = 255)
    
    return(brighter_hex)
  }
  
  # for each element in cl return a brighter version of the color
  cl_l <- vapply(cl, function(x) brighter_color(x, factor = 2), "")
  
  cl_d <- vapply(cl, function(x) brighter_color(x, factor = 0.5), "")
  
  show_col(cl)
  
  cl_grad <- lapply(seq_len(length(cl)), function(x) col_numeric(palette = c(cl_l[x], cl_d[x]), domain = NULL))
  
  names(cl_grad) <- names(cl)
  
  spec$loop <- spec$var
  
  loop_key <- unique(spec$loop)
  
  if (length(xlim) == 1) {
    rtr <- c(min(spec$rt) - xlim, max(spec$rt) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(spec$rt), max(spec$rt))
  }
  
  if (length(ylim) == 1) {
    mzr <- c(min(spec$mz) - ylim, max(spec$mz) + ylim)
  } else if (length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(spec$mz), max(spec$mz))
  }
  
  title <- list(text = title, x = 0.13, y = 0.98, font = list(size = 12, color = "black"))
  
  if (is.null(xLab)) xLab = "Retention time / seconds"
  
  if (is.null(yLab)) yLab = "<i>m/z</i> / Da"
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
    titlefont = list(size = 12, color = "black")
  )
  
  showL <- rep(TRUE, length(leg))
  
  names(showL) <- leg
  
  plot <- plot_ly()
  
  for (t in loop_key) {
    select_vector <- spec$loop %in% t
    lt <- unique(spec$var[select_vector])
    x <- spec$rt[select_vector]
    y <- spec$mz[select_vector]
    z <- spec$intensity[select_vector]
    lt_colors <- cl_grad[[lt]]
    z_normalized <- (z - min(z)) / (max(z) - min(z))
    
    point_colors <- lt_colors(z_normalized)
    
    plot <- plot %>% add_trace(
      x = x,
      y = y,
      type = "scatter",
      mode = "markers",
      inherit = FALSE,
      marker = list(size = 1, color = point_colors),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste("<br>x: %{x}<br>", "y: %{y}")
    )
    
    if (length(y) >= 1) showL[lt] <- FALSE
  }
  
  plot <- plot %>% plotly::layout(
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  ) %>% hide_colorbar()
  
  plot
}
