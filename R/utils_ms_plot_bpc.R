
#' @title plot_interactive_bpc
#'
#' @description Interactive plot of BPC using the \pkg{plotly} package.
#'
#' @param eic A data table with the analysis, id, rt, mz, intensity and var
#' (i.e., the plotting variable for each entry) as columns.
#' @param title A character vector to be used as title.
#' @param colorBy A character vector to name the legend.
#'
#' @return A BPC interactive plot.
#'
plot_interactive_bpc <- function(bpc, title, colorBy) {

  leg <- unique(bpc$var)
  cl <- getColors(leg)
  sp <- unique(bpc$analysis)
  ids <- unique(bpc$id)

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
      lt <- unique(bpc[analysis == s & id == t, var])
      y <- bpc[analysis == s & id == t, intensity]
      plot <- plot %>% add_trace(
        x = bpc[analysis == s & id == t, rt],
        y = y,
        type = "scatter", mode = "lines+markers",
        line = list(width = 0.5, color = unname(cl[lt])),
        marker = list(size = 2, color = unname(cl[lt])),
        name = lt,
        legendgroup = lt,
        showlegend = showL[lt],
        hovertemplate = paste("<br>rt: %{x}<br>",
                              "mz: ",
                              round(bpc[analysis == s & id == t, mz], digits = 4),
                              "<br>","intensity: %{y}")
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
