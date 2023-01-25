
#' @title plot_interactive_xic
#'
#' @description Plot traces or profile data for targets using expected
#' \emph{m/z} and retention time pairs, including deviations.
#'
#' @param xic A \linkS4class{data.table} from the method \code{\link{XICs}}.
#' @param plotTargetMark Logical, set to \code{TRUE} (the default) to plot a
#' target mark.
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
plot_interactive_xic <- function(xic,
                                 plotTargetMark = TRUE,
                                 ppmMark = 5,
                                 secMark = 10,
                                 numberRows = 1) {

  ids <- unique(xic$id)

  sNames <- unique(xic$analysis)

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
        "The MS area to be plotted for ", t, " seems rather large.",
        "It is suggested to restrict the data first using",
        " narrow mz and rt deviations."
      ))
      next
    }

    for (s in seq_len(length(xic_s))) {

      temp <- xic_s[[s]]

      if (plotTargetMark & class(temp$mz_id) == "numeric" & class(temp$rt_id) == "numeric") {

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
          x1 =  max(temp$rt, na.rm = TRUE),
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

      if (plotTargetMark_loop) p1 <- p1 %>% plotly::layout(shapes = c(vline1, line))

      p1 <- p1 %>% add_annotations(
        text = paste(unique(temp$analysis), t, sep = " - "),
        x = 0.05, y = 1, yref = "paper", xref = "paper",
        xanchor = "left", yanchor = "bottom", align = "center",
        showarrow = FALSE, font = list(size = 10)
      )

      p2 <- plot_ly(
        temp, x = temp$rt, y = temp$mz,
        type = "scatter", mode = "markers",
        color = temp$intensity, colors = colors,
        marker = list(size = 8, line = list(color = "white", width = 0.5)),
        name = paste0(s, "p2")
      )

      if (plotTargetMark_loop) p2 <- p2 %>% plotly::layout(shapes = list(c(vline2, line), c(hline, line), rect))

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

    plot <- plot %>% plotly::layout(
      xaxis = xaxis, xaxis2 = xaxis,
      xaxis3 = xaxis, xaxis4 = xaxis,
      xaxis5 = xaxis, xaxis6 = xaxis,
      yaxis = yaxis1, yaxis2 = yaxis2
    )

    mainPlot[[t]] <- plot
  }

  if (length(ids) > 1) {
    finalplot <- plotly::subplot(
      mainPlot,
      nrows = numberRows,
      margin = 0.05
    )

    # TODO Fix the issue with the axis labels
    # finalplot <- finalplot %>% plotly::layout(
    #   xaxis = xaxis, xaxis2 = xaxis,
    #   xaxis3 = xaxis, xaxis4 = xaxis,
    #   xaxis5 = xaxis, xaxis6 = xaxis,
    #   xaxis7 = xaxis, xaxis8 = xaxis,
    #   xaxis9 = xaxis, xaxis10 = xaxis,
    #   yaxis = yaxis1, yaxis2 = yaxis2
    # )


  } else {
    finalplot <- mainPlot[[1]]
  }

  return(finalplot)
}
