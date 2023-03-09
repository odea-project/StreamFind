
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
carb <- carbamazepin_d10$mass + 1.007276
carb_rt <- carbamazepin_d10$rt
diu_pos <- diuron_d6$mass + 1.007276
diu <- diuron_d6$mass + 1.007276
diu_rt <- diuron_d6$rt
sec_dev <- 30
ppm_dev <- 10
mz <- data.frame(
  id = c("tg1", "tg2"),
  mz = c(carb, diu),
  rt = c(carb_rt, diu_rt)
)
targets <- make_ms_targets(mz = mz, ppm = ppm_dev, sec = sec_dev)

settings_ff <- settings(
  call = "find_features",
  algorithm = "xcms3",
  parameters = list(xcms::CentWaveParam(
    ppm = 12, peakwidth = c(5, 30),
    snthresh = 10, prefilter = c(5, 1500),
    mzCenterFun = "mean", integrate = 2,
    mzdiff = -0.0001, fitgauss = TRUE,
    noise = 500, verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = TRUE
  ))
)

settings_gf <- settings(
  "call" = "group_features",
  "algorithm" = "xcms3",
  "parameters" = list(
    # rtalign = FALSE,
    groupParam = xcms::PeakDensityParam(
      sampleGroups = "holder",
      bw = 5,
      minFraction = 0.5,
      minSamples = 1,
      binSize = 0.008,
      maxFeatures = 100
    )
  )
)

settingsLoadFeaturesMS1 <- list(
  "call" = "load_features_ms1",
  "algorithm" = "streamFind",
  "parameters" = list(
    rtWindow = c(-2, 2),
    mzWindow = c(-1, 6),
    mzClust = 0.003,
    minIntensity = 250,
    filtered = FALSE,
    runParallel = FALSE,
    verbose = FALSE
  )
)

settingsLoadGroupsMS1 <- list(
  "call" = "load_groups_ms1",
  "algorithm" = "streamFind",
  "parameters" = list(
    mzClust = 0.003,
    minIntensity = 1000,
    verbose = FALSE,
    filtered = FALSE,
    runParallel = FALSE
  )
)

# code -------------------------------------------------------------------------

ms <- msData$new(files = all_files[10:21],
  headers = list(name = "Example 1"),
  settings = list(
    find = settings_ff,
    group = settings_gf,
    ms1ft = settingsLoadFeaturesMS1,
    ms1gp = settingsLoadGroupsMS1
  )
)

ms$find_features()
ms$group_features()

