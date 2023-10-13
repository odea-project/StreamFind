
# resources --------------------------------------------------------------------

all_files <- StreamFindData::get_ms_file_paths()
db <- StreamFindData::get_ms_tof_spiked_chemicals()
db_cols <- c("name", "formula", "mass", "rt")
db <- db[, db_cols, with = FALSE]




# development ------------------------------------------------------------------

# MassSpecData with one TOF and Orbitrap MS file
ms <- MassSpecData$new(all_files[c(7, 31)])

qCentroids <- Settings_centroid_spectra_qCentroids(runParallel = FALSE)

# qBinning <- Settings_bin_spectra_qBinning()
# qPeaks <- Settings_find_features_qPeaks()

ms$centroid_spectra(qCentroids)









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
