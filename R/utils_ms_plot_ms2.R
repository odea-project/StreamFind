
#' @title plot_static_ms2
#'
#' @description Static plot of MSn spectra using the \pkg{base} package.
#'
#' @param ms2 A data table with the id, mz, intensity, preMZ, isPre
#' and var (i.e., the plotting variable for each entry) as columns.
#' @param title A character vector to be used as title.
#'
#' @return An MSn plot.
#'
plot_static_ms2 <- function(ms2, title = NULL) {

  cl <- get_colors(unique(ms2$var))

  ms2$var = as.factor(ms2$var)

  ms2[, color := cl[var]]

  plot(intensity~mz,  ms2,
    type = "h",
    xlab = expression(italic("m/z")),
    ylab = "Intensity / counts",
    col = color,
    lwd = 2,
    ylim = c(0, max(ms2$intensity) * 1.15),
    main = title,
    yaxs = "i",
    xaxt = "n"
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

#' @title plot_static_spectra
#'
#' @description Static plot of spectra using the \pkg{base} package.
#'
#' @param spectra A data.table with the analyses, id, mz, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param title A character vector to be used as title.
#'
#' @return An spectra plot.
#'
plot_static_spectra <- function(spectra, title = NULL) {

  cl <- get_colors(unique(spectra$var))

  spectra$var = as.factor(spectra$var)

  spectra[, color := cl[var]]

  plot(intensity~mz,  spectra,
       type = "h",
       xlab = expression(italic("m/z")),
       ylab = "Intensity / counts",
       col = color,
       lwd = 2,
       ylim = c(0, max(spectra$intensity) * 1.15),
       main = title,
       yaxs = "i",
       xaxt = "n"
  )

  ticksMin <- round_any(min(spectra$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- round_any(max(spectra$mz, na.rm = TRUE) * 1.1, 10)

  axis(1, seq(ticksMin, ticksMax, 10), lwd = 1.5, cex.axis = 0.8)
  axis(1, seq(ticksMin, ticksMax, 5), labels = FALSE, lwd = 1, col = "darkgray")
  axis(1, seq(ticksMin, ticksMax, 2.5), labels = FALSE, lwd = 0.5, col = "darkgray")

  legend(
    "topright",
    legend = levels(spectra$var),
    col = cl,
    lty = 1,
    cex = 0.8
  )
}

#' @title plot_interactive_ms2
#'
#' @description Interactive plot of MS2 spectra using the \pkg{plotly} package.
#'
#' @param ms2 A data.table with the id, preMZ, mz, intensity,
#' isPre and var (i.e., the plotting variable for each entry) as columns.
#' @param title A character vector to be used as title.
#'
#' @return An MS2 spectra interactive plot.
#'
plot_interactive_ms2 <- function(ms2, title) {

  leg <- unique(ms2$var)

  cl <- get_colors(leg)

  plot <- plot_ly()

  for (v in leg) {

    data <- ms2[var == v, ]

    bar_widths = rep(0.2, nrow(data))

    mz_text = paste0(round(data$mz, digits = 4), "  ")

    precursor <- data[data$isPre, ]

    if (TRUE %in% data$isPre) {
      mz_text[data$isPre] = paste0("Pre ", mz_text[data$isPre])
      bar_widths[data$isPre] = 2
    }

    notPrecursor <- data[!data$isPre, ]

    plot <- plot %>% add_trace(
      data = data,
      x = data$mz,
      y = data$intensity,
      type = "bar",
      width = 0.05,
      marker = list(color = cl[v],
      line = list(color = cl[v], width = bar_widths)),
      text = mz_text,
      textposition = "outside",
      textangle = 90,
      textfont = list(
        size = 9,
        color = rep(cl[v], length(data$mz))
      ),
      name = v,
      legendgroup = v,
      hovertemplate = paste('<i>m/z</i>: %{x:.4f}', '<br>intensity: %{y:.0f}')
    )
  }

  ticksMin <- round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "<i>m/z<i>",
    titlefont = list(size = 12, color = "black"),
    range = c(ticksMin, ticksMax),
    dtick = round((max(ms2$mz)/10), -1),
    ticks = "outside"
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )

  plot <- plot %>% plotly::layout(
    title = title, xaxis = xaxis, yaxis = yaxis,
    barmode = "overlay", uniformtext = list(minsize = 6, mode = 'show')
  )

  return(plot)
}

#' @title plot_interactive_spectra
#'
#' @description Interactive plot of MS1 spectra using the \pkg{plotly} package.
#'
#' @param ms1 A data.table with the analyses, id, mz, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param title A character vector to be used as title.
#'
#' @return An spectra interactive plot.
#'
plot_interactive_spectra <- function(spectra, title = NULL) {

  leg <- unique(spectra$var)

  cl <- get_colors(leg)

  plot <- plot_ly()

  for (v in leg) {

    data <- spectra[spectra$var == v, ]

    bar_widths = rep(0.0001, nrow(data))

    mz_text = paste0(round(data$mz, digits = 4), "  ")

    plot <- plot %>% add_trace(
      x = data$mz,
      y = data$intensity,
      type = "bar",
      #width = 0.05,
      marker = list(color = cl[v],
                    line = list(color = cl[v], width = bar_widths)),
      text = mz_text,
      textposition = "outside",
      textangle = 90,
      textfont = list(
        size = 9,
        color = rep(cl[v], length(data$mz))
      ),
      name = v,
      legendgroup = v,
      hovertemplate = paste('<i>m/z</i>: %{x:.4f}', '<br>intensity: %{y:.0f}')
    )
  }

  ticksMin <- round_any(min(spectra$mz, na.rm = TRUE) * 0.9, 10)
  ticksMax <- round_any(max(spectra$mz, na.rm = TRUE) * 1.1, 10)

  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = "<i>m/z<i>",
    titlefont = list(size = 12, color = "black"),
    range = c(ticksMin, ticksMax),
    dtick = round((max(spectra$mz)/10), -1),
    ticks = "outside"
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )

  plot <- plot %>% plotly::layout(
    title = title, xaxis = xaxis, yaxis = yaxis,
    barmode = "overlay", uniformtext = list(minsize = 6, mode = 'show')
  )

  return(plot)
}
