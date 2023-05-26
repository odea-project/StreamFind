
all_files <- streamFindData::msFilePaths()
files <- all_files[1:3]
ms <- MassSpecData$new(files)


settings_ff <- list(
  call = "find_features",
  algorithm = "xcms3",
  parameters = xcms::CentWaveParam(
    ppm = 12,
    peakwidth = c(5, 30),
    snthresh = 10,
    prefilter = c(5, 1000),
    mzCenterFun = "wMean",
    integrate = 1,
    mzdiff = -0.0005,
    fitgauss = TRUE,
    noise = 500,
    verboseColumns = TRUE,
    roiList = list(),
    firstBaselineCheck = TRUE,
    roiScales = numeric(),
    extendLengthMSW = FALSE
  ),
  software = "xcms",
  developer = "Ralf Tautenhahn, Johannes Rainer",
  contact = "rtautenh@ipb-halle.de",
  link = "https://rdrr.io/bioc/xcms/man/findChromPeaks-centWave.html",
  doi = "https://doi.org/10.1186/1471-2105-9-504"
)


settings_ff <- as.ProcessingSettings(settings_ff)

ms$find_features(settings_ff)

ms$get_features()

do.call("ppm", list(settings_ff$parameters))

slot

library(xcms)
