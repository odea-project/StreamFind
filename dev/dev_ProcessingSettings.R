
all_files <- streamFindData::msFilePaths()
files <- all_files[1:3]
ms <- MassSpecData$new(files)
ms$import_settings("./settings.json")
ms$find_features()



settings_ff <- get_default_ProcessingSettings(
  call = "find_features",
  software = "xcms",
  algorithm = "centwave"
)

save_default_ProcessingSettings(
  call = "find_features",
  software = "xcms",
  algorithm = "centwave"
)

ms$find_features(settings_ff)


ps <- ms$get_settings("find_features")

ms$get_features(analyses = 3)
ms$plot_features(analyses = 3, features = 89)

do.call("prefilter", list(settings_ff$parameters))

ms$save_settings()

ms$import_settings("./settings.json")

ms$find_features()













settings_ff2 <- as.ProcessingSettings(
  list(
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
)
