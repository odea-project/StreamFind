
#' @title plot_spectra_interactive
#'
#' @description 3D interactive plot for spectra using the \pkg{plotly} package.
#'
#' @param spectra A \code{data.table} with the analysis, level, rt, mz and
#' intensity.
#' @param colorBy A string (length 1). One of "analyses" (the default),
#' "levels", "targets" or "replicates". For "replicates", a column with
#' replicate names should be given.
#'
#' @return A 3D interactive plot.
#'
plot_spectra_interactive = function(spec = NULL, colorBy = "analyses") {

  if (!"id" %in% colnames(spec)) spec$id = ""

  spec$id = factor(spec$id)
  spec$level = paste("MS", spec$level, sep = "")
  spec$level = factor(spec$level)
  spec$analysis = factor(spec$analysis)

  spec$rtmz = paste(
    spec$id, spec$level,
    spec$mz, spec$rt,
    spec$analysis, sep = "")

  spec_temp = spec
  spec_temp$intensity = 0
  spec = rbind(spec, spec_temp)

  if (colorBy == "levels") {
    spec$var = spec$level
  } else if (colorBy == "targets"){
    spec$var = spec$id
  } else if ("replicates" %in% colorBy) {
    spec$replicate = self$get_replicate_names()[spec$analysis]
    spec$var = spec$replicate
  } else {
    spec$var = spec$analysis
  }

  colors_var = get_colors(unique(spec$var))

  fig = plot_ly(spec, x = ~rt, y = ~mz, z = ~intensity) %>%
    group_by(spec$rtmz) %>%
    add_lines(color = ~var,  colors = colors_var)

  fig = fig %>% layout(scene = list(
    xaxis = list(title = "Retention time / seconds"),
    yaxis = list(title = "<i>m/z</i>"),
    zaxis = list(title = "Intensity / counts")))

  return(fig)
}
