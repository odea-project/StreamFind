
wd <- "C:/Users/apoli/Documents/Dev_230830_Bevacizumab_Avastin_LotB8703H40_Raman_HRMS"
files <- list.files(paste0(wd, "/HRMS_2.5mgmL"), pattern = "mzML", full.names = TRUE)

# files <- StreamFindData::get_ms_file_paths()[29]

ms <- MassSpecEngine$new(files)

ps <- Settings_integrate_chromatograms_StreamFind(
  chromatograms = c(0),
  smoothing = TRUE,
  windowSize = 10,
  baseline = TRUE,
  baseline_method = "als",
  baseline_args = list(lambda = 6, p = 0.02, maxit = 10),
  minPeakHeight = 1000,
  minPeakDistance = 2,
  minPeakWidth = 5,
  maxPeakWidth = 120,
  minSN = 10
)
#
ms$integrate_chromatograms(ps)

ms$chrom_peaks

# ms$plot_chrom_peaks(colorBy = "targets+analyses")


ms$deconvolute_spectra_charges(Settings_deconvolute_spectra_charges_StreamFind())



