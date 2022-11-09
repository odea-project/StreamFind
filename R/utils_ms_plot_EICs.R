

#' @title plotStaticEICs
#'
#' @description Static plot of EICs using the \pkg{base} package.
#'
#' @param eic A data table with the analysis, replicate,
#' id, rt, intensity and var (i.e., the plotting variable for each entry)
#' as columns.
#' @param title A character vector to be used as title.
#'
#' @return A EIC plot.
#'
plotStaticEICs <- function(eic, title = NULL) {

  cl <- getColors(unique(eic$var))
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
      lt <- unique(eic[analysis == s & id == t, var])
      lines(
        x = eic[analysis == s & id == t, rt],
        y = eic[analysis == s  & id == t, intensity],
        type = "l",
        pch = 19,
        cex = 0.5,
        col = cl[lt]
      )
      points(
        x = eic[analysis == s & id == t, rt],
        y = eic[analysis == s  & id == t, intensity],
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




#' @title plotInteractiveEICs
#'
#' @description Interactive plot of EICs using the \pkg{plotly} package.
#'
#' @param eic A data table with the analysis, replicate,
#' id, rt, intensity and var (i.e., the plotting variable for each entry)
#' as columns.
#' @param title A character vector to be used as title.
#' @param colorBy A character vector to name the legend.
#'
#' @return A EIC interactive plot.
#'
plotInteractiveEICs <- function(eic, title, colorBy) {

  leg <- unique(eic$var)
  cl <- getColors(leg)
  sp <- unique(eic$analysis)
  ids <- unique(eic$id)

  title <- list(text = title, x = 0.13, y = 0.98, font = list(size = 12, color = "black"))

  xaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Retention time / seconds",
                titlefont = list(size = 12, color = "black"))

  yaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Intensity / counts",
                titlefont = list(size = 12, color = "black"))

  plot <- plot_ly()
  showL <- rep(TRUE, length(leg))
  names(showL) <- leg

  for (s in sp) {
    for (t in ids) {
      lt <- unique(eic[analysis == s & id == t, var])
      y <- eic[analysis == s & id == t, intensity]
      plot <- plot %>% add_trace(
        x = eic[analysis == s & id == t, rt],
        y = y,
        type = "scatter", mode = "lines+markers",
        line = list(width = 0.5, color = unname(cl[lt])),
        marker = list(size = 2, color = unname(cl[lt])),
        name = lt,
        legendgroup = lt,
        showlegend = showL[lt],
        hovertemplate = paste("<br>rt: %{x}<br>", "int: %{y}")
      )
      if (length(y) >= 1) showL[lt] <- FALSE
    }
  }

  plot <- plot %>% layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )

  return(plot)
}
