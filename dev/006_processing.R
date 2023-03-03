
# resources --------------------------------------------------------------------
# ------------------------------------------------------------------------------
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

# code -------------------------------------------------------------------------
# ------------------------------------------------------------------------------

ms <- msData$new(files = files[1:3], header = list(name = "Example 1"))

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

class(settings_ff)

settings_ff <- list(
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

class(settings_ff)
class(as.settings(settings_ff))

ana <- make_ms_analyses(files[1:3])
ana <- lapply(ana, function(x) as.msAnalysis(x))


