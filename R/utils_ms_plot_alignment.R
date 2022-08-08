

#' @title plotAlignment
#'
#' @description Plots the results from the retention time alignment across analyses.
#'
#' @param object An \linkS4class{msData} or \linkS4class{msAnalysis} object with adjusted retention time.
#' @param interactive Logical, set to \code{TRUE} for plotting with \pkg{plotly} package.
#'
#' @return A plot with the retention time alignment differences for each sample.
#'
#' @export
#'
#' @importFrom plotly toRGB plot_ly add_trace layout
#' @importFrom data.table copy
#'
plotAlignment <- function(object, interactive = TRUE) {

  valid <- FALSE

  if (checkmate::testClass(object, "msData") | checkmate::testClass(object, "msAnalysis"))
    valid = TRUE

  adj_rt <- hasAdjustedRetentionTime(object)

  if (!TRUE %in% adj_rt)
    valid <- FALSE

  if (!valid) {
    warning("Invalid class object or adjusted retention time not found!")
    return(NULL)
  }

  if (checkmate::testClass(object, "msAnalysis")) {
    ana <- list(object)
    names(ana) <- analyses(object)
  } else {
    ana <- object@analyses[names(adj_rt)[adj_rt]]
  }

  df <- lapply(ana, function(x) return(x@spectra))
  #df <- rbindlist(df, idcol = "analysis")

  colors <- getColors(names(df))

  if (interactive) {

    xaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = "Retention time (seconds)",
      titlefont = list(size = 12, color = "black")
    )

    yaxis <- list(
      linecolor = toRGB("black"),
      linewidth = 2,
      title = "RT<sub>Adjusted</sub> - RT<sub>Raw</sub> (seconds)",
      titlefont = list(size = 12, color = "black")
    )

    plot <- plot_ly()

    for (i in names(df)) {

      plot  <- plot %>% add_trace(
        x = df[[i]]$rt,
        y = df[[i]]$adjustment,
        type = "scatter",
        mode = "lines",
        line = list(
          shape = "spline", width = 0.5,
          color = colors[i]
        ),
        name = i,
        legendgroup = i,
        showlegend = TRUE
      )

      df_pt <- copy(df[[i]][!is.na(adjPoints), ])

      plot <- plot %>% add_trace(
        x = df_pt$adjPoints,
        y = df_pt$adjustment,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 5,
          color = colors[i]
        ),
        name = i,
        legendgroup = i,
        showlegend = FALSE
      )
    }

    plot <- plot %>% layout(
      legend = list(title = list(text = "<b> Analyses: </b>")),
      xaxis = xaxis, yaxis = yaxis
    )

    return(plot)
  }


}
