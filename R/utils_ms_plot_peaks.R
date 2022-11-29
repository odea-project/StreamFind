
#' @title plotPeaksStatic
#'
#' @description Static plot of chromatographic peaks using the \pkg{base} package.
#'
#' @param eic A data table with the analysis, replicate,
#' id, rt, intensity and var (i.e., the plotting variables for each peak)
#' as columns.
#' @param peaks A data table with the individual peaks to plot.
#' @param title An optional character vector to be used as title.
#'
#' @importFrom graphics axis legend lines points polygon
#'
#' @return A plot of chromatographic peaks.
#'
plotPeaksStatic <- function(eic, peaks, title = NULL) {

  cl <- getColors(unique(eic$var))
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
    lt <- unique(eic[id == t, var])
    pk_eic <- eic[id == t, ]
    pk_a <- peaks[peaks$id == t, ]
    pk_eic_a <- pk_eic[rt >= pk_a$rtmin & rt <= pk_a$rtmax & id == t, ]
    points(
      x = pk_eic$rt,
      y = pk_eic$intensity,
      type = "p",
      pch = 19,
      cex = 0.2,
      col = cl[lt]
    )

    polygon(
      c(pk_eic_a$rt, rev(pk_eic_a$rt)),
      c(pk_eic_a$intensity, rep(0, length(pk_eic_a$intensity))),
      #pk_eic_a$rt[pk_eic_a$intensity > 0],
      #y = pk_eic_a$intensity[pk_eic_a$intensity > 0],
      col = paste(color = unname(cl[lt]), 50, sep = ""),
      border = F
    )
    lines(
      x = rep(pk_a$rt, 2),
      y = c(0, pk_a$intensity),
      type = "l",
      pch = 19,
      cex = 0.5,
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

#' @title plotPeaksInteractive
#'
#' @description Plots chromatographic peaks with the package \pkg{plotly}.
#'
#' @param eic A data table with the analysis, replicate,
#' id, rt, intensity and var (i.e., the plotting variables for each peak)
#' as columns.
#' @param peaks A data table with the individual peaks to plot.
#' @param title An optional character vector to be used as title.
#' @param colorBy Possible values are \code{"targets"} (the default),
#' \code{"analyses"} or \code{replicates},
#' for coloring by target peaks, analyses or replicates, respectively.
#'
#' @return A chromatographic peak plot through \pkg{plotly}.
#'
plotPeaksInteractive <- function(eic, peaks, title, colorBy) {

  leg <- unique(eic$var)
  cl <- getColors(leg)
  ids <- unique(eic$id)

  title <- list(text = title, x = 0.13, y = 0.98, font = list(size = 12, color = "black"))

  xaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Retention time / seconds",
                range = c(min(eic$rt), max(eic$rt)),
                titlefont = list(size = 12, color = "black"))

  yaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Intensity / counts",
                titlefont = list(size = 12, color = "black"))

  plot <- plot_ly()
  showL <- rep(TRUE, length(leg))
  names(showL) <- leg

  for (t in ids) {
    lt <- unique(eic[id == t, var])
    y <- eic[id == t, intensity]
    plot <- plot %>% add_trace(
      x = eic[id == t, rt],
      y = y,
      type = "scatter", mode = "markers",
      marker = list(size = 2, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste("<br>rt: %{x}<br>", "int: %{y}")
    )
    if (length(y) >= 1) showL[lt] <- FALSE

    pk <- peaks[peaks$id %in% t, ]
    pk_eic <- eic[rt >= pk$rtmin & rt <= pk$rtmax & id == t, ]

    hT <- paste(
      "</br> peak: ", pk$id,
      ifelse("feature" %in% colnames(pk), paste("</br> feature: ", pk$feature), ""),
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

    plot <- plot %>%  add_trace(
      x = pk_eic$rt,
      y = pk_eic$intensity,
      type = "scatter", mode =  "lines+markers",
      line = list(width = 0.5, color = unname(cl[lt])),
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

  plot <- plot %>% layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )

  return(plot)
}

#' @title mapPeaksInteractive
#'
#' @description Function for plotting peak spaces.
#'
#' @param peaks A data table with the individual peak details to plot.
#' @param xlim A length one or two numeric vector for setting the \emph{x} limits (in seconds) of the plot.
#' @param ylim A length one or two numeric vector for setting the \emph{m/z} limits of the plot.
#' @param title An optional character vector to be used as title.
#' @param colorBy Possible values are \code{"targets"} (the default),
#' \code{"analyses"} or \code{replicates},
#' for coloring by target peaks, analyses or replicates, respectively.
#'
#' @return A peak/s map plot produced through \pkg{plotly}.
#'
mapPeaksInteractive <- function(peaks, xlim = 60, ylim = 5, title, colorBy = "targets") {

  if (length(xlim) == 1) {
    rtr <- c(min(peaks$rtmin) - xlim, max(peaks$rtmax) + xlim)
  } else if (length(xlim) == 2) {
    rtr <- xlim
  } else {
    rtr <- c(min(peaks$rtmin), max(peaks$rtmax))
  }

  if (length(ylim) == 1) {
    mzr <- c(min(peaks$mzmin) - ylim, max(peaks$mzmax) + ylim)
  } else if (length(ylim) == 2) {
    mzr <- ylim
  } else {
    mzr <- c(min(peaks$mzmin), max(peaks$mzmax))
  }

  cl <- getColors(unique(peaks$var))

  plot <- plot_ly()

  plot <- plot %>% add_trace(
    x = peaks$rt, y = peaks$mz, color = peaks$var,
    type = "scatter", mode = "markers", colors = cl,
    marker = list(size = 8),
    hoverinfo = "text",
    text = paste(
      "</br> peak: ", peaks$id,
      "</br> analysis: ", peaks$analysis,
      "</br> <i>m/z</i>: ", round(peaks$mz, digits = 4),
      "</br> dppm: ", round(((peaks$mzmax - peaks$mzmin) / peaks$mz) * 1E6, digits = 0),
      "</br> rt: ", round(peaks$rt, digits = 0),
      "</br> drt: ", round(peaks$rtmax - peaks$rtmin, digits = 0),
      "</br> Int: ", round(peaks$intensity, digits = 0),
      "</br> Filled: ",
      if ("is_filled" %in% colnames(peaks)) {
        ifelse(peaks$is_filled == 1, TRUE, FALSE)
      } else {
        FALSE
      }
    )
  )

  shapes <- list()

  for (i in seq_len(nrow(peaks))) {
    shapes[[i]] <- list(
      type = "rect",
      fillcolor = cl[names(cl) %in% peaks$var[i]],
      opacity = 0.2,
      line = list(color = cl[names(cl) %in% peaks$var[i]]),
      x0 = peaks$rtmin[i],
      x1 = peaks$rtmax[i],
      xref = "x",
      y0 = peaks$mzmin[i],
      y1 = peaks$mzmax[i],
      yref = "y"
    )
  }

  title <- list(text = title, x = 0.1, y = 0.98,
                font = list(size = 9, color = "black"))

  xaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Retention time / seconds",
                titlefont = list(size = 12, color = "black"),
                range = rtr,
                autotick = TRUE, ticks = "outside")

  yaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "<i>m/z</i>",
                range = mzr,
                titlefont = list(size = 12, color = "black"))

  plot <- plot %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title,
    shapes = shapes
  )

  return(plot)
}
