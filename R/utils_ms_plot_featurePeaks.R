
#' @title plotFeaturePeaks
#'
#' @description Plots peaks for each feature in an \linkS4class{msData} object.
#'
#' @param object An \linkS4class{msData} object.
#' @template args-single-analyses
#' @template args-single-targetsID
#' @template args-makeTargets
#' @param heights A numeric vector of length two to control the height of
#' the first and second plot, respectively.
#'
#' @return A double plot with peak chromatograms on the top part
#' and feature peak groups below.
#'
#' @export
#'
plotFeaturePeaks <- function(object,
                             analyses = NULL,
                             targetsID = NULL,
                             mz = NULL, ppm = 20,
                             rt = NULL, sec = 30, id = NULL,
                             heights = c(0.6, 0.4)) {

  assertClass(object, "msData")

  fts <- features(
    object = object,
    targetsID = targetsID,
    mz = mz, ppm = ppm,
    rt = rt, sec = sec
  )

  pks <- peaks(
    object = object,
    analyses = analyses,
    targets = fts$id
  )

  eic <- lapply(split(pks, pks$analysis), function(x, object) {
    return(
      EICs(
        object,
        analyses = unique(x$analysis),
        mz = x
      )
    )
  }, object = object)

  eic <- rbindlist(eic)

  if (nrow(eic) < 1) return(cat("Data was not found for any of the targets!"))

  eic$var <- sapply(eic$id, function(x, pks) {
    pks[id == x, feature]
  }, pks = pks)

  if (!is.null(id) & length(id) == length(unique(eic$var))) {
    leg <- id
    names(leg) <- unique(eic$var)
    eic$var <- sapply(eic$var, function(x) leg[x])
  } else {
    leg <- unique(eic$var)
    names(leg) <- unique(eic$var)
  }

  colors <- getColors(leg)

  showleg <- rep(TRUE, length(leg))
  names(showleg) <- names(leg)

  plot <- plot_ly()

  for (i in fts$id) {

    pk_temp <- pks[feature == i, ]

    for (z in pk_temp$id) {

      df <- eic[id == z, ]

      plot <- plot %>% add_trace(df,
        x = df$rt,
        y = df$intensity,
        type = "scatter", mode = "lines",
        line = list(width = 0.5,
                    color = colors[i]),
        connectgaps = TRUE,
        name = leg[i],
        legendgroup = leg[i],
        showlegend = FALSE
      )

      df <- df[rt >= pk_temp[id == z, rtmin] & rt <= pk_temp[id == z, rtmax], ]
      df$mz <- as.numeric(df$mz)

      plot <- plot %>%  add_trace(
        df,
        x = df$rt,
        y = df$intensity,
        type = "scatter", mode =  "lines+markers",
        fill = "tozeroy", connectgaps = TRUE,
        fillcolor = paste(color = colors[i], 50, sep = ""),
        line = list(width = 0.1, color = colors[i]),
        marker = list(size = 3, color = colors[i]),
        name = leg[i],
        legendgroup = leg[i],
        showlegend = showleg[i],
        hoverinfo = "text",
        hoverlabel = list(bgcolor = colors[i]),
        text = paste(
          "</br> name: ", leg[i],
          "</br> feature: ", i,
          "</br> peak: ", z,
          "</br> analysis: ", pk_temp[id == z, analysis],
          "</br> <i>m/z</i>: ", round(df$mz, digits = 4),
          "</br> rt: ", round(df$rt, digits = 0),
          "</br> Int: ", round(df$intensity, digits = 0)
        )
      )

      showleg[i] <- FALSE
    }
  }

  plot2 <- plot_ly()

  for (i in fts$id) {

    df2 <- pks[feature == i, ]

    if (!"is_filled" %in% colnames(df2)) df2$is_filled <- 0

    df_p <- df2[is_filled == 0, ]

    plot2 <- plot2 %>% add_trace(
      x = df_p$rt,
      y = df_p$analysis,
      type = "scatter",
      mode = "markers",
      marker = list(
        line = list(color = colors[i], width = 3),
        color = "#000000", size = 10
      ),
      error_x = list(
        type = "data",
        symmetric = FALSE,
        arrayminus = df_p$rt - df_p$rtmin,
        array = df_p$rtmax - df_p$rt,
        color = colors[i],
        width = 5
      ),
      name = leg[i],
      legendgroup = leg[i],
      showlegend = FALSE,
      hoverinfo = "text",
      hoverlabel = list(bgcolor = colors[i]),
      text = paste(
        "</br> name: ", leg[i],
        "</br> feature: ", i,
        "</br> peak: ", df_p$id,
        "</br> analysis: ", df_p$analysis,
        "</br> height: ", round(df_p$intensity, digits = 0),
        "</br> width: ", round(df_p$rtmax - df_p$rtmin, digits = 0),
        "</br> dppm: ", round(((df_p$mzmax - df_p$mzmin) / df_p$mz) * 1E6, digits = 1),
        "</br> filled: ", ifelse(df_p$is_filled == 1, "TRUE", "FALSE")
      )
    )

    df_f <- df2[is_filled == 1, ]

    if (nrow(df_f) > 0) {
      plot2 <- plot2 %>% add_trace(
        x = df_f$rt,
        y = df_f$analysis,
        type = "scatter",
        mode = "markers",
        marker = list(
          line = list(color = colors[i], width = 3),
          color = "#f8f8f8",
          size = 10
        ),
        error_x = list(
          type = "data",
          symmetric = FALSE,
          arrayminus = df_f$rt - df_f$rtmin,
          array = df_f$rtmax - df_f$rt,
          color = colors[i],
          width = 5
        ),
        name = leg[i],
        legendgroup = leg[i],
        showlegend = FALSE,
        hoverinfo = "text",
        hoverlabel = list(bgcolor = colors[i]),
        text = paste(
          "</br> name: ", leg[i],
          "</br> feature: ", i,
          "</br> peak: ", df_f$id,
          "</br> analysis: ", df_f$analysis,
          "</br> height: ", round(df_f$intensity, digits = 0),
          "</br> width: ", round(df_f$rtmax - df_f$rtmin, digits = 0),
          "</br> dppm: ", round(((df_f$mzmax - df_f$mzmin) / df_f$mz) * 1E6, digits = 1),
          "</br> filled: ", ifelse(df_f$is_filled == 1, "TRUE", "FALSE")
        )
      )
    }
  }

  plot2 <- hide_colorbar(plot2)

  plotList <- list()

  plotList[["plot"]] <- plot

  plotList[["plot2"]] <- plot2

  xaxis <- list(linecolor = toRGB("black"), linewidth = 2,
                title = "Retention time (seconds)",
                titlefont = list(size = 12, color = "black"),
                range = c(min(eic$rt), max(eic$rt)), autotick = TRUE, ticks = "outside")

  yaxis1 <- list(linecolor = toRGB("black"), linewidth = 2,
                title = "Intensity (counts)",
                titlefont = list(size = 12, color = "black"))

  yaxis2 <- list(linecolor = toRGB("black"), linewidth = 2,
                title = "",
                titlefont = list(size = 12, color = "black"),
                tick0 = 0, dtick = 1)

  plotf <- subplot(
    plotList,
    nrows = 2,
    heights = heights,
    margin = 0.01,
    shareX = TRUE,
    which_layout = "merge"
  )

  plotf <- plotf %>% layout(
    legend = list(title = list(text = paste("<b>", "targets", "</b>"))),
    xaxis = xaxis, yaxis = yaxis1, yaxis2 = yaxis2
  )

  return(plotf)
}
