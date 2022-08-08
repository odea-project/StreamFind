
#' @title plotAnnotationInteractive
#'
#' @description Function to plot features annotation.
#'
#' @param object An \linkS4class{msData} object.
#' @param comps A \linkS4class{data.table} as obtained by the
#' method \code{annotation} for \linkS4class{msData}.
#' @param colorBy A string value to define the plotting legend.
#' Possible values are "mass" and "isotopes".
#'
#' @return A dotted plot with features annotation.
#'
plotAnnotationInteractive <- function(object, comps, colorBy = "isotopes") {

  rpl <- unique(replicates(object))
  intTemp <- comps[, rpl, with = FALSE]
  intMean <- apply(intTemp, 1, FUN = mean)
  intSD <- apply(intTemp, 1, FUN = sd)
  comps[, intMean := intMean]
  comps[, intSD := round(intSD / intMean * 100, digits = 0)]

  if ("isotopes" %in% colorBy) {
    comps[, intensity := intMean / max(intMean), by = monoiso]
    vars <- unique(comps$monoiso)
    colorsplot <- getColors(vars)

  } else {
    comps[, intensity := intMean / max(intMean), by = neutralMass]
    vars <- unique(comps$neutralMass)
    colorsplot <- getColors(vars)

  }

  xaxis <- list(
    linecolor = toRGB("black"), linewidth = 2, title = "Retention time (seconds)",
    titlefont = list(size = 12, color = "black"),
    range = c(min(comps$rt) - 120, max(comps$rt) + 120)
  )

  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2,
    title = "<i>m/z<i>",
    titlefont = list(size = 12, color = "black"),
    range = c(min(comps$mz) - 5, max(comps$mz) + 5)
  )

  plot <- plot_ly()

  for (nm in vars) {

    if ("isotopes" %in% colorBy) {
      temp <- comps[monoiso %in% nm, ]
    } else {
      temp <- comps[neutralMass %in% nm, ]
    }

    tempCol <- colorsplot[names(colorsplot) %in% nm]

    plot <- plot %>% add_trace(
      x = temp$rt,
      y = temp$mz,
      type = "scatter", mode = "markers+text",
      marker = list(size = 20 * temp$intensity,
                    opacity = 0.6, color = tempCol,
                    line = list(color = tempCol)),
      text =  temp$adduct_ion,
      textposition = "midle right",
      textfont = list(size = 12, color = tempCol),
      hovertext = paste(
        "</br> neutral mass: ", temp$neutralMass,
        "</br> annotation: ", temp$adduct_ion,
        "</br> feature: ", temp$id,
        "</br> charge: ", temp$charge,
        "</br> rt: ", round(temp$rt, digits = 0),
        "</br> mz: ", round(temp$mz, digits = 4),
        "</br> d_sec: ", round(temp$drt, digits = 0),
        "</br> d_ppm: ", round(temp$dppm, digits = 0),
        "</br> intensity_rel: ", round(temp$intensity, digits = 3),
        "</br> intensity_av: ", round(temp$intMean, digits = 0),
        "</br> intensity_sd%: ", temp$intSD
      ),
      name = ifelse(is.numeric(nm), round(nm, digits = 4), nm),
      legendgroup = ifelse(is.numeric(nm), round(nm, digits = 4), nm),
      showlegend = TRUE
    )
  }

  plot <- plot %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", "targets", "</b>"))),
    xaxis = xaxis, yaxis = yaxis
  )

  return(plot)
}
