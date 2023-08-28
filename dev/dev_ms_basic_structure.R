
all_files <- streamFindData::msFilePaths()

files_df <- data.frame(
  "file" = all_files[grepl("blank|influent|o3sw", all_files)],
  "replicate" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("in_neg", 3),
    rep("in_pos", 3),
    rep("out_neg", 3),
    rep("out_pos", 3)
  ),
  "blank" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3)
  )
)

ms <- MassSpecData$new(files = files_df)

j <- Settings_suspect_screening_streamFind()
class(j)
sloop::s3_dispatch(validate(j))

ffs <- get_default_ProcessingSettings(
  call = "find_features",
  algorithm = "xcms3_centwave"
)

gfs <- get_default_ProcessingSettings(
  call = "group_features",
  algorithm = "xcms3_peakdensity"
)

fls <- Settings_filter_features_streamFind(
  minIntensity = 5000,
  minSnRatio = 20,
  maxGroupSd = 30,
  # blank = 5,
  minGroupAbundance = 3,
  excludeIsotopes = TRUE
)

ms$add_settings(list(ffs, gfs, fls))

ms$find_features()$group_features()$filter_features()

ms

db <- streamFindData::msSpikedChemicals()
db <- db[grepl("S", db$tag), ]
cols <- c("name", "formula", "mass", "rt")
db <- db[, cols, with = FALSE]
db

sss <- Settings_suspect_screening_streamFind(
  database = db,
  ppm = 5,
  sec = 10
)


# implement export MS2 ------

slfms2 <- Settings_load_features_ms2_streamFind()
slfms2$parameters$minIntensity <- 100
slgms2 <- Settings_load_groups_ms2_streamFind()
slgms2$parameters$minIntensity <- 100
msbp <- ms$subset_analyses(4:6)
suspects <- msbp$get_suspects(database = db, ppm = 10, sec = 15)

msbp$remove_features(filtered = TRUE)
msbp <- msbp$subset_features(features = suspects)
msbp$load_features_ms2(slfms2)
msbp$load_groups_ms2(slgms2)

msbp$suspect_screening(sss)

View(msbp$get_modules_data("suspect_screening"))



ms2 <- ms$get_spectra(analyses = 4, mz = 239.0629, rt = 1157.414, ppm = 20, sec = 30, levels = 2, allTraces = FALSE)
ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id)


rcpp_ms_cluster_ms2(ms2, 0.005, TRUE, TRUE)



ms$get_ms2(analyses = 4, mz = 267.0702, rt = 1007.222, ppm = 20, sec = 30, mzClust = 0.01, isInAllSpectra = TRUE)

ms$get_ms2(analyses = 5, mz = 267.0702, rt = 1007.222, ppm = 20, sec = 30, mzClust = 0.01, isInAllSpectra = TRUE)


ms$plot_ms1(analyses = 4, mz = data.frame(mzmin = 239, mzmax = 245, rtmin = 1156, rtmax = 1158), isInAllSpectra = TRUE, verbose = TRUE)
ms1




View(suspects)

ms$plot_groups_ms2(groups = suspects$group[1],
  isolationWindow = 1.3,
  mzClustFeatures = 0.003,
  minIntensityFeatures = 150
)







# history ----------------------------------------------------------------------

ms$get_history()


# other ------------------------------------------------------------------------

# ms$save_settings(format = "json", name = "settings")

