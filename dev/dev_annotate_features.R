# resources --------------------------------------------------------------------

all_files <- streamFindData::msFilePaths()
db <- streamFindData::msSpikedChemicals()
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

isos <- read.csv(paste0(getwd(), "/dev/isotopes.csv"))
isos <- isos[isos$element %in% c("C", "H", "N", "S", "Cl", "P", "O", "F"), ]

# settings ---------------------------------------------------------------------

settings_ff <- list(
  call = "find_features",
  algorithm = "xcms3",
  parameters = list(xcms::CentWaveParam(
    ppm = 12, peakwidth = c(5, 30),
    snthresh = 10, prefilter = c(5, 1000),
    mzCenterFun = "wMean", integrate = 1,
    mzdiff = -0.0005, fitgauss = TRUE,
    noise = 500, verboseColumns = TRUE,
    firstBaselineCheck = TRUE,
    extendLengthMSW = FALSE
  ))
)

# r6 test ----------------------------------------------------------------------
# patRoon::clearCache("parsed_ms_analyses")
# patRoon::clearCache("parsed_ms_spectra")

ms <- MassSpecData$new(files = all_files[1:3],
  headers = list(name = "Example 1"),
  settings = list(settings_ff)
)

ms$find_features()

# code dev ---------------------------------------------------------------------

fts <- ms$get_features(analyses = 1)
cols_keep <- c("feature", "mz", "rt", "intensity")
fts_tar <- ms$get_features(analyses = 1, mz = targets[2, ])
fts1 <- fts[fts$rt >= fts_tar$rtmin & fts$rt <= fts_tar$rtmax, cols_keep, with = FALSE]
fts1 <- fts1[order(fts1$mz), ]

plot_spectra_interactive(fts1)

239/3




na.omit(isos$i1 - isos$i0)
max(na.omit(isos$i1 - isos$i0)) - min(na.omit(isos$i1 - isos$i0))

na.omit(isos$i2 - isos$i0)
max(na.omit(isos$i2 - isos$i0)) - min(na.omit(isos$i2 - isos$i0))



