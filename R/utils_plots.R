
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
    
    if (!"baseline" %in% colnames(chrom)) chrom$baseline <- rep(0, nrow(chrom))
    
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
      "</br> rt: ", round(pk$rt[1], digits = 0),
      "</br> drt: ", round(pk$rtmax[1] - pk$rtmin[1], digits = 0),
      "</br> intensity: ", round(pk$intensity[1], digits = 0)
    )
    
    if (!"baseline" %in% colnames(chrom)) chrom$baseline <- rep(0, nrow(chrom))
    
    plot <- plot %>% add_trace(
      # x = pk_chrom$rt,
      # y = pk_chrom$raw,
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
