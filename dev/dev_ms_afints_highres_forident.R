
#path <- "F:/example_ms_files"
path <- "C:/Users/Ricardo Cunha/Documents/Work/example_ms_files"




# resources --------------------------------------------------------------------
cols <- c("name", "formula", "mz", "rt")

tof_db <- paste0(path, "/qc_MS2_pos.csv")
tof_db <- data.table::fread(tof_db)
tof_db <- tof_db[, cols, with = FALSE]

afin_db <- paste0(path, "/Composition_Mix-Fusion.csv")
afin_db <- data.table::fread(afin_db)
afin_db <- afin_db[, cols, with = FALSE]

ude_db <- paste0(path, "/mix1_orbitrap_ude.csv")
ude_db <- data.table::fread(ude_db)
ude_db <- ude_db[, cols, with = FALSE]

files <- list.files(path, pattern = ".mzML", full.names = TRUE)

# Settings for annotation of isotopes
afs <- Settings_annotate_features_StreamFind(
  maxIsotopes = 5,
  elements = c("C", "H", "N", "O", "S", "Cl", "Br"),
  mode = "small molecules",
  maxCharge = 1,
  rtWindowAlignment = 0.5,
  maxGaps = 1,
  runParallel = FALSE
)




# tof --------------------------------------------------------------------------

tof_fl <- files[18]

tof_ffs <- Settings_find_features_xcms3_centwave(
  ppm = 12,
  peakwidth = c(5, 40),
  snthresh = 5,
  prefilter = c(5, 1500),
  mzCenterFun = "wMean",
  integrate = 2,
  mzdiff = 0.0005,
  fitgauss = TRUE,
  noise = 500,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = FALSE
)

tof_ms <- MassSpecData$new(
  files = tof_fl,
  headers = list(name = "tof"),
  settings = list(tof_ffs, afs)
)

tof_ms$find_features()$annotate_features()

tof_suspects <- tof_ms$get_suspects(database = tof_db, ppm = 8, sec = 10)




# afin --------------------------------------------------------------------------

afin_fl <- files[5]

afin_ffs <- Settings_find_features_xcms3_centwave(
  ppm = 3,
  peakwidth = c(5, 80),
  snthresh = 10,
  prefilter = c(6, 150000),
  mzCenterFun = "wMean",
  integrate = 1,
  mzdiff = 0.0002,
  fitgauss = TRUE,
  noise = 50000,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = FALSE
)

afin_ms <- MassSpecData$new(
  files = afin_fl,
  headers = list(name = "afin"),
  settings = list(afin_ffs, afs)
)

afin_ms$find_features()$annotate_features()

afin_suspects <- afin_ms$get_suspects(database = afin_db, ppm = 5, sec = 10)

afin_ms$map_components(features = afin_suspects[c(11, 31), ], legendNames = TRUE)




# ude --------------------------------------------------------------------------

ude_fl <- files[8]

ude_ffs <- Settings_find_features_xcms3_centwave(
  ppm = 4,
  peakwidth = c(5, 80),
  snthresh = 10,
  prefilter = c(6, 50000),
  mzCenterFun = "wMean",
  integrate = 2,
  mzdiff = 0.0002,
  fitgauss = TRUE,
  noise = 50000 / 3,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = FALSE
)

ude_ms <- MassSpecData$new(
  files = ude_fl,
  headers = list(name = "ude"),
  settings = list(ude_ffs, afs)
)

ude_ms$find_features()$annotate_features()

ude_suspects <- ude_ms$get_suspects(database = ude_db, ppm = 5, sec = 10)




# forident showcase ------------------------------------------------------------

suspects_ms <- tof_ms$subset_features(features = tof_suspects)

slfms2 <- Settings_load_features_ms2_StreamFind(
  isolationWindow = 1.3,
  mzClust = 0.008,
  presence = 0.9,
  minIntensity = 200
)

suspects_ms$load_features_ms2(slfms2)

suspects_ms$plot_features_ms2(loadedMS2 = TRUE, colorBy = "targets")

sssfi <- Settings_suspect_screening_forident(
  addMS2 = TRUE,
  useNeutralMass = FALSE,
  path = getwd(),
  name = "feature_list_forident"
)

suspects_ms$suspect_screening(sssfi)

file.remove("feature_list_forident.txt")










