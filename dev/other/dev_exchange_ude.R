
# resources --------------------------------------------------------------------

all_files <- StreamFindData::get_ms_file_paths()
db <- StreamFindData::get_ms_tof_spiked_chemicals()
db_cols <- c("name", "formula", "mass", "rt")
db <- db[, db_cols, with = FALSE]




# development ------------------------------------------------------------------

# MassSpecData with one TOF and Orbitrap MS file
ms <- MassSpecData$new(all_files[c(7)])
ms$load_spectra()

ms2 <- ms$clone()

nrow(ms2$get_spectra())


qCentroids <- Method_centroid_spectra_qCentroids(maxScale = 20, mode = 2, runParallel = FALSE)

ms2$centroid_spectra(qCentroids)

# qBinning <- Method_bin_spectra_qBinning()
# qPeaks <- Method_find_features_qPeaks()


ms$plot_spectra(analyses = 1, mass = db[16], ppm = 150)

patRoon::clearCache("parsed_ms_spectra")
patRoon::clearCache("parsed_ms_analyses")

ms2$plot_spectra(analyses = 1)

ms2$has_loaded_spectra()

anas <- ms2$get_analyses()

tof_spectra <- ms$get_spectra(1)
tof_spectra <- tof_spectra[, c("scan", "rt", "mz", "intensity")]

# write.csv(tof_spectra, "tof_spectra.csv")

orb_spectra <- ms$get_spectra(2)
orb_spectra <- orb_spectra[, c("scan", "rt", "mz", "intensity")]

cent_spectra <- rcpp_centroid_spectra_qCentroids(tof_spectra, maxScale = 5, mode = 2)

class(cent_spectra)



# ms$bin_spectra(qBinning)
# ms$find_features(qPeaks)




# spectra dt -------------------------------------------------------------------

# tof_spectra <- ms$get_spectra(1)
# tof_spectra <- tof_spectra[, c("scan", "rt", "mz", "intensity")]
#
# head(tof_spectra)
#
# orb_spectra <- ms$get_spectra(2)
# orb_spectra <- orb_spectra[, c("scan", "rt", "mz", "intensity")]
#
# head(orb_spectra)
