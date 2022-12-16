

#' @title plotStaticMSn
#'
#' @description Static plot of MSn spectra using the \pkg{base} package.
#'
#' @param ms2 A data table with the id, mz, intensity, CE, preMZ, precursor and var (i.e., the plotting variable for each entry) as columns.
#' Optionally, sample and replicate can be in the table.
#' @param title A character vector to be used as title.
#'
#' @return A MSn plot.
#'
plotStaticMSn <- function(ms2, title = NULL) {

  cl <- getColors(unique(ms2$var))

  ms2[, color := cl[var]]

  plot(intensity~mz,  ms2,
    type = "h",
    xlab = expression(italic("m/z")),
    ylab = "Intensity / counts",
    col = color,
    lwd = 2,
    ylim = c(0, max(ms2$intensity) * 1.15),
    main = title,
    yaxs = "i"
  )

  precursors <- ms2[ms2$isPre, ]

  for (s in seq_len(nrow(precursors))) {

    lines(
      x = rep(precursors[s, mz], 2),
      y = c(0, precursors[s, intensity]),
      type = "h",
      pch = 19,
      lwd = 8,
      cex = 0.5,
      col = precursors[s, color]
    )

    text(x = precursors[s, mz], y = precursors[s, intensity],
         labels = "P", pos = 3, offset = 0.5, vfont = NULL,
         cex = 1, col = precursors[s, color], font = NULL)
  }

  ticksMin <- round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)
  axis(1, seq(ticksMin, ticksMax, 10), lwd = 1.5)
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




#' @title plotInteractiveMSn
#'
#' @description Interactive plot of EICs using the \pkg{plotly} package.
#'
#' @param ms2 A data table with the sample, replicate,
#' id, rt, intensity and var (i.e., the plotting variable for each entry)
#' as columns.
#' @param title A character vector to be used as title.
#'
#' @return A EIC interactive plot.
#'
plotInteractiveMSn <- function(ms2, title) {

  leg <- unique(ms2$var)

  cl <- getColors(leg)

  plot <- plot_ly()

  for (v in seq_len(length(leg))) {

    data <- ms2[var == leg[v], ]

    precursor <- data[data$isPre, ]

    plot <- plot %>% add_trace(
      x = data$mz,
      y = data$intensity,
      type = "bar",
      width = 0.05,
      marker = list(color = cl[v],
      line = list(color = cl[v], width = 0.2)),
      name = leg[v],
      legendgroup = leg[v],
      hoverinfo = "text", text = paste(
        "</br> mz: ", round(data$mz, digits = 4),
        "</br> intensity: ", round(data$intensity, digits = 0),
        "</br> CE: ", data$ce,
        "</br> precMZ: ", data$preMZ,
        "</br> isPrecursor: ", data$precursor
      )
    )

    if (nrow(precursor) > 0) {

      plot <- plot %>% add_trace(
        x = precursor$mz,
        y = precursor$intensity,
        type = "bar",
        marker = list(color = cl[v],
        line = list(color = cl[v], width = 1)),
        name = leg[v],
        legendgroup = leg[v],
        showlegend = FALSE
      )

      plot <- plot %>% plotly::add_text(x = precursor$mz,
                                y = precursor$intensity,
                                text = "P", textposition = "top",
                                textfont = list(color = rep(cl[v], length(precursor$mz))),
                                legendgroup = leg[v],
                                showlegend = FALSE)

    }
  }

  ticksMin <- round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "<i>m/z<i>",
    titlefont = list(size = 12, color = "black"),
    range = c(ticksMin, ticksMax),
    dtick = 5,
    ticks = "outside"
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12,
    color = "black")
  )

  plot <- plot %>% plotly::layout(title = title, xaxis = xaxis, yaxis = yaxis, barmode = "overlay")

  return(plot)
}
