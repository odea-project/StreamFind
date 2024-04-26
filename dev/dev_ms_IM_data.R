
# path <- "F:/example_ms_files"
path <- "C:/Users/Ricardo Cunha/Documents/example_ms_files"
files <- list.files(path, pattern = "im_tof", full.names = TRUE)
cols <- c("name", "formula", "mz", "rt")
db <- paste0(path, "/qc_MS2_pos.csv")
db <- data.table::fread(db)
db <- db[, cols, with = FALSE]

# patRoon::clearCache("parsed_ms_analyses")
View(rcpp_parse_ms_analysis(files[3]))

patRoon::clearCache("parsed_ms_analyses")
ms <- MassSpecData$new(files[3])
ms

# ms$get_run()

# ms$has_ion_mobility()

patRoon::clearCache("parsed_ms_analyses")
patRoon::clearCache("parsed_ms_spectra")

# ms$get_spectra_eic(mz = db$mz, drift = 20, millisec = 5)

ms$plot_eic(mz = db$mz, drift = 20, millisec = 5)

ms$get_spectra_ms1(mz = db$mz, drift = 20, millisec = 5)

ms$get_spectra_ms2(mz = db$mz, drift = 20, millisec = 5)

ms$plot_eic(mz = db$mz, drift = 20, millisec = 5)

# ms$plot_spectra(levels = c(1, 2), colorBy = "levels")

patRoon::clearCache("parsed_ms_spectra")

ms$plot_spectra(
  # mz = data.table(mzmin = 0, mzmax = 300),
  rt = data.table(rtmin = 190, rtmax = 200),
  drift = data.table(driftmin = 15, driftmax = 25),
  levels = c(1, 2),
  colorBy = "levels",
  xVal = "mz", yVal = "drift"
)

ms$plot_spectra(
  # mz = data.table(mzmin = 232.5, mzmax = 238),
  # rt = data.table(rtmin = 920, rtmax = 960),
  drift = data.table(driftmin = 15, driftmax = 30),
  levels = c(1, 2),
  colorBy = "levels",
  xVal = "drift", yVal = "rt"
)

spectra <- ms$get_spectra()

make_ms_targets(mz = 10, rt = 100)

plot_im_map(spectra, xval = "mz")

plot_im_surface(spectra, xval = "mz")

plot_im_map <- function(spectra = NULL, xval = "rt") {
  
  intensity = NULL
  drift = NULL
  x = NULL
  
  if (!is.data.frame(spectra)) {
    warning("Spectra must be a data.frame or data.table!")
    return(NULL)
  }
  
  if (nrow(spectra) == 0) {
    warning("There are no spectra to plot!")
    return(NULL)
  }
  
  if (!"drift" %in% colnames(spectra)) {
    warning("Drift column not found in spectra data.table!")
    return(NULL)
  }
  
  checkmate::assert_choice(xval, c("rt","mz"))

  spectra <- as.data.table(spectra)
  
  dt_heat <- copy(spectra)
  
  dt_heat[["x"]] <- dt_heat[[xval]]
  
  dt_heat <- dt_heat[, c("x", "drift", "intensity"), with = FALSE]
  
  dt_heat$intensity <- log(dt_heat$intensity)
  
  dt_heat <- dt_heat[, .(intensity = sum(intensity)), by = c("x", "drift")]
  
  dt_heat <- tidyr::complete(dt_heat, x, tidyr::nesting(drift), fill = list(intensity = 0))
  
  dt_heat <- tidyr::pivot_wider(dt_heat, names_from = x, values_from = intensity)
  
  dt_heat_mat <- as.matrix(dt_heat)
  
  row.names(dt_heat_mat) <- dt_heat_mat[, 1]
  
  dt_heat_mat <- dt_heat_mat[, -1]
  
  plot <- plotly::plot_ly(x = colnames(dt_heat_mat), y = rownames(dt_heat_mat),
                          z = dt_heat_mat, colors = "RdYlBu", type = "heatmap",
                          colorbar = list(title = '<b> Intensity </b>'),
                          reversescale = TRUE)
  
  xaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Retention time / seconds",
                titlefont = list(size = 12, color = "black"))
  
  yaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Drift / miliseconds",
                titlefont = list(size = 12, color = "black"))
  
  plot <- plot %>% layout(
    xaxis = xaxis,
    yaxis = yaxis
  )
  
  plot
}




plot_im_surface <- function(spectra = NULL, xval = "rt") {
  
  intensity = NULL
  drift = NULL
  x = NULL
  
  if (!is.data.frame(spectra)) {
    warning("Spectra must be a data.frame or data.table!")
    return(NULL)
  }
  
  if (nrow(spectra) == 0) {
    warning("There are no spectra to plot!")
    return(NULL)
  }
  
  if (!"drift" %in% colnames(spectra)) {
    warning("Drift column not found in spectra data.table!")
    return(NULL)
  }
  
  checkmate::assert_choice(xval, c("rt","mz"))
  
  spectra <- as.data.table(spectra)
  
  dt_heat <- copy(spectra)
  
  dt_heat$x <- dt_heat[[xval]]
  
  dt_heat <- dt_heat[, c("x", "drift", "intensity"), with = FALSE]
  
  # dt_heat$intensity <- log(dt_heat$intensity)
  
  dt_heat <- dt_heat[, .(intensity = sum(intensity)), by = c("x", "drift")]
  
  dt_heat <- tidyr::complete(dt_heat, x, tidyr::nesting(drift), fill = list(intensity = 0))
  
  dt_heat <- tidyr::pivot_wider(dt_heat, names_from = x, values_from = intensity)
  
  dt_heat_mat <- as.matrix(dt_heat)
  
  row.names(dt_heat_mat) <- dt_heat_mat[, 1]
  
  dt_heat_mat <- dt_heat_mat[, -1]
  
  browser()
  
  fig <- plotly::plot_ly(
    x = as.numeric(colnames(dt_heat_mat)),
    y = as.numeric(rownames(dt_heat_mat)),
    z = dt_heat_mat) %>% plotly::add_surface(colors = "RdYlBu", reversescale = TRUE, showscale = FALSE,)
  
  xaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = '<i> m/z </i>',
                titlefont = list(size = 12, color = "black"))
  
  yaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Drift Time / miliseconds",
                titlefont = list(size = 12, color = "black"))
  
  zaxis <- list(linecolor = toRGB("black"),
                linewidth = 2, title = "Intensity / counts",
                titlefont = list(size = 12, color = "black"))
  
  fig <- fig %>% layout(
    scene = list(
      xaxis = list(title = '<i> m/z </i>'),
      yaxis = list(title = "Drift Time / miliseconds"),
      zaxis = list(title = "Intensity / counts")
    )
  )
  
  fig
  
  
  
}










