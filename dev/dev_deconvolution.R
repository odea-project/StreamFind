
wd2 <- "C:/Users/apoli/Documents/Dev_230830_Bevacizumab_Avastin_LotB8703H40_Raman_HRMS"

files <- list.files(paste0(wd2, "/HRMS_2.5mgmL"), pattern = "mzML", full.names = TRUE)

# files <- StreamFindData::get_ms_file_paths()[29]

ms <- MassSpecEngine$new(files)

ms$load_chromatograms(chromatograms = 0)

# ms$plot_chromatograms(colorBy = "analyses")

ms$smooth_chromatograms(Settings_smooth_chromatograms_movingaverage(windowSize = 5))

# ms$plot_chromatograms(colorBy = "analyses")

ms$correct_chromatograms_baseline(Settings_correct_chromatograms_baseline_airpls())

# ms$plot_chromatograms(colorBy = "analyses")

ps <- Settings_integrate_chromatograms_StreamFind(
  merge = TRUE,
  closeByThreshold = 2,
  minPeakHeight = 20000,
  minPeakDistance = 2,
  minPeakWidth = 5,
  maxPeakWidth = 120,
  minSN = 50
)

ms$integrate_chromatograms(ps)

ms$chromatograms_peaks

ms$plot_chromatograms_peaks(colorBy = "analyses+targets")

ms





# View(ms$get_results("chromatograms"))
# ms$get_chromatograms()
# ms$chromatograms


ms$plot_chromatograms_baseline(colorBy = "targets+analyses")








ms$chromatograms_peaks

# ms$plot_chromatograms_peaks(colorBy = "targets+analyses")

ms$deconvolute_spectra_charges(Settings_deconvolute_spectra_charges_StreamFind())

ms$plot_spectra()


View(ms$get_results("spectra"))

ms$spectra_charges

ms$plot_spectra_charges()

ms$averaged_spectra

mean(ms$spectra_peaks$mass)

sd(ms$spectra_peaks$mass)

ms$plot_spectra_peaks()

ms$plot_spectra_charges()

ms$chrom_peaks

View(ms$get_results("chromatograms"))

# patRoon::clearCache("all")



wd3 <- "C:/Users/apoli/Documents/iSoft/Bevacizumab_Zirabev_LotFR7476"
files2 <- list.files(wd3, pattern = "mzML", full.names = TRUE)

ms2 <- MassSpecEngine$new(files2)

ms2$integrate_chromatograms(ps)

ms2$plot_chrom_peaks(colorBy = "analyses")

ms2$chrom_peaks




ms2$plot_spectra_tic()



ms2$chrom_peaks



ms2$plot_chrom_peaks(colorBy = "targets+analyses")

ms2$plot_spectra_ms1(analyses = 6, rt = data.table(rtmin = 342 - 2.5, rtmax = 342 + 2.5), presence = 0.1, mzClust = 0.01, minIntensity = 50, interactive = FALSE)

dps <- Settings_deconvolute_spectra_charges_StreamFind(
  rtmin = 342 - 2.5,
  rtmax = 342 + 2.5
)


