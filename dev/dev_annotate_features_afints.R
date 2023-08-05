
path <- "C:/Users/Ricardo Cunha/Documents/Work/Dev_20230530_Orbitrap_AFINTS"
all_files <- list.files(path, pattern = ".mzML", full.names = TRUE)
file <- all_files[5]

# path <- "E:/Dev_20230530_Orbitrap_AFINTS/"
# file <- "E:/Dev_20230530_Orbitrap_AFINTS/230621_MixFusion_HR_02_10.mzML"

db <- paste0(path, "/Composition_Mix-Fusion.csv")
db <- data.table::fread(db)
cols <- c("name", "formula", "mz")
db <- db[, cols, with = FALSE]
db

ms <- MassSpecData$new(file)

ffs <- ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3",
  parameters = xcms::CentWaveParam(
    ppm = 3,
    peakwidth = c(5, 80),
    snthresh = 5,
    prefilter = c(6, 150000),
    mzCenterFun = "wMean",
    integrate = 2,
    mzdiff = 0.00005,
    fitgauss = TRUE,
    noise = 50000,
    verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = FALSE
  )
)

ms$find_features(ffs)

suspects <- ms$suspect_screening(db, ppm = 2)
#View(suspects)


ms$plot_features_ms1(
  features = c("mz273.127_rt365_f1767"),
  rtWindow = c(-0.5, 0.5),
  mzWindow = c(0, 6)
)

fts <- ms$get_features()
fts <- fts[order(fts$mz), ]
which(fts$feature %in% "mz267.07_rt1008_f51")

output <- rcpp_ms_annotation_isotopes(fts, maxGaps = 1)

suspects_res <- suspects$name
names(suspects_res) <- suspects$feature

suspects_for <- suspects$formula
names(suspects_for) <- suspects$feature

output$output$name <- suspects_res[output$output$iso_feat]
output$output$formula <- suspects_for[output$output$iso_feat]

# View(output)



View(output$output)







# ms$plot_bpc()
#
# ms$plot_eic(mz = db$mz[176], ppm = 4)
#
# ms$plot_spectra(levels = 1, mz = data.frame(
#   mzmin = 251,
#   mzmax = 260,
#   rtmin = 550,
#   rtmax = 650
# ))


# ms$plot_features(features = "mz195.123_rt293_f479")

# ms$plot_features(mz = 195.123, rt = 293, ppm = 50, sec = 30)

# ms$plot_spectra(levels = 1, mz = data.frame(
#   mzmin = 164,
#   mzmax = 170,
#   rtmin = 370,
#   rtmax = 380
# ))

# ms$plot_xic(mz = 195.123, rt = 293, ppm = 50, sec = 10)


fts <- ms$get_features()
fts <- fts[order(fts$mz), ]
which(fts$feature %in% "mz388.106_rt1165_f7725")

output <- rcpp_ms_annotation_isotopes(fts)
View(output)
View(output$output)

ms$plot_eic(mz = 197.1262, rt = 293, ppm = 5, sec = 30)

ms$map_features(features = output$output$feature[output$output$iso_gr == 1])

ms$plot_features_ms1(features = output$output$feature[output$output$iso_gr == 1],
                     mzClust = 0.0001, rtWindow = c(-1, 1), mzWindow = c(-1, 3))


ms$map_features(features = output$output$feature[output$output$iso_gr == 558])

ms$plot_features(features = output$output$feature[output$output$iso_gr == 558])
