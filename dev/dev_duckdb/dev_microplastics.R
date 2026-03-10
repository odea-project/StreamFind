
library(StreamFind)

files <- list.files("D:\\microplastics", full.names = TRUE)

ms <- DB_MassSpecEngine$new(
  projectPath = "D:\\microplastics\\data",
  files = files[5]
)



fig_tic <- plot_spectra_tic(ms$Analyses, interactive = FALSE)


fig_111mz <- plot_spectra_eic(
  ms$Analyses,
  mz = data.frame(
    mz = c(111, 69, 154, 210),
    rt = c(19*60, 19*60, 19*60, 19*60)
  ),
  sec = 60, ppm = 2000,
  interactive = FALSE
)

plot_spectra_ms1(
  ms$Analyses,
  mz = data.frame(
    mz = c(111, 69, 154, 210),
    rt = c(19*60, 19*60, 19*60, 19*60)
  ),
  sec = 60, ppm = 2000
)

fig_208mz <- plot_spectra_eic(ms$Analyses, mz = 208, ppm = 1000, interactive = FALSE)

ms$run_app()