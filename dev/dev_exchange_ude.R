
# resources --------------------------------------------------------------------

all_files <- StreamFindData::get_all_file_paths()
db <- StreamFindData::get_tof_spiked_chemicals()
files_mrm <- all_files[grepl("mrm", all_files)]
files <- all_files[1:3]
files1 <- all_files[grepl("influent|blank", all_files)]
files2 <- all_files[grepl("o3sw", all_files)]
db_cols <- c("name", "mass", "rt")

carbamazepin_d10 <- db[db$name %in% "Carbamazepin-d10", db_cols, with = FALSE]
diuron_d6 <- db[db$name %in% "Diuron-d6", db_cols, with = FALSE]
carb_pos <- carbamazepin_d10$mass + 1.007276
carb <- carbamazepin_d10$mass
carb_rt <- carbamazepin_d10$rt
diu_pos <- diuron_d6$mass + 1.007276
diu <- diuron_d6$mass
diu_rt <- diuron_d6$rt

sec_dev <- 30
ppm_dev <- 10

targets <- make_ms_targets(
  mz = data.frame(
    id = c("tg1", "tg2"),
    mz = c(carb_pos, diu_pos),
    rt = c(carb_rt, diu_rt)
  ),
  ppm = ppm_dev,
  sec = sec_dev
)

neutral_targets <- make_ms_targets(
  mz = data.frame(
    id = c("tg1", "tg2"),
    mz = c(carb, diu),
    rt = c(carb_rt, diu_rt)
  ),
  ppm = ppm_dev,
  sec = sec_dev
)

# development ------------------------------------------------------------------

# MassSpecData with one TOF and Orbitrap MS file
ms <- MassSpecData$new(all_files[c(7, 31)])

qCentroids <- Settings_centroid_spectra_qCentroids()

qBinning <- Settings_bin_spectra_qBinning()

qPeaks <- Settings_find_features_qPeaks()

ms$centroid_spectra(qCentroids)

ms$bin_spectra(qBinning)

ms$find_features(qPeaks)

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
