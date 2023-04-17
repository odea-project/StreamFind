
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

# settings ---------------------------------------------------------------------

settings_ff <- settings(
  call = "find_features",
  algorithm = "xcms3",
  parameters = list(xcms::CentWaveParam(
    ppm = 12, peakwidth = c(5, 30),
    snthresh = 10, prefilter = c(5, 1500),
    mzCenterFun = "wMean", integrate = 1,
    mzdiff = -0.0005, fitgauss = TRUE,
    noise = 500, verboseColumns = TRUE,
    firstBaselineCheck = TRUE,
    extendLengthMSW = FALSE
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
    runParallel = TRUE,
    verbose = FALSE
  )
)

settingsLoadFeaturesMS2 <- list(
  "call" = "load_features_ms2",
  "algorithm" = "streamFind",
  "parameters" = list(
    isolationWindow = 1.3,
    mzClust = 0.003,
    minIntensity = 0,
    filtered = FALSE,
    runParallel = TRUE,
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
    runParallel = TRUE
  )
)

settingsLoadGroupsMS2 <- list(
  "call" = "load_groups_ms2",
  "algorithm" = "streamFind",
  "parameters" = list(
    mzClust = 0.003,
    minIntensity = 250,
    filtered = FALSE,
    runParallel = TRUE,
    verbose = FALSE
  )
)

# code -------------------------------------------------------------------------

patRoon::clearCache("parsed_ms_analyses")
patRoon::clearCache("parsed_ms_spectra")
patRoon::clearCache("load_features_ms1")
patRoon::clearCache("load_features_ms2")
patRoon::clearCache("load_groups_ms1")
patRoon::clearCache("load_groups_ms2")


ms <- msData$new(files = all_files[10:21],
  headers = list(name = "Example 1"),
  settings = list(
    find = settings_ff,
    group = settings_gf,
    ms1ft = settingsLoadFeaturesMS1,
    ms2ft = settingsLoadFeaturesMS2,
    ms1gp = settingsLoadGroupsMS1,
    ms2gp = settingsLoadGroupsMS2
  )
)

ms$get_files()
ms$get_spectra_levels(analyses = 1:2)
ms$get_polarities()
ms$get_run(1:2)

ms$get_spectra()

ms$get_bpc()
ms$get_tic()


big_file_test <- "E:\\Dev_20230126_IonMobilityDataFirstTraining\\WorklistData-0001.mzML"

big_file_test <- "E:\\20230126_DA_EDA_background_evaluation\\221118_DA-EDA_solid phase background_centrifuged\\mzml\\02_QC_pos-r001.mzML"

init <- Sys.time()
msz <- mzR::openMSfile(big_file_test)
# msz1 <- mzR::instrumentInfo(msz)
# msz2 <- mzR::runInfo(msz)
msz3 <- mzR::header(msz)
mzR::close(msz)
Sys.time() - init
init <- Sys.time()
ana <- rcpp_parse_msAnalysis(big_file_test)
Sys.time() - init
init <- Sys.time()
ana1 <- parse_msAnalysis(big_file_test)
Sys.time() - init



init <- Sys.time()
msz <- mzR::openMSfile(big_file_test)
msz4 <- mzR::peaks(msz)
mzR::close(msz)
Sys.time() - init
init <- Sys.time()
spectra <- rcpp_parse_spectra(big_file_test)
Sys.time() - init


init <- Sys.time()
msz <- mzR::openMSfile(all_files[4])
msz4 <- mzR::peaks(msz)
mzR::close(msz)
Sys.time() - init
init <- Sys.time()
spectra <- rcpp_parse_spectra(all_files[4])
Sys.time() - init


init <- Sys.time()
spectra <- rcpp_parse_xml(all_files[1])
Sys.time() - init
# spectra


all_files <- streamFindData::msFilePaths()
analysis <- rcpp_parse_msAnalysis(all_files[1])
spectra <- rcpp_parse_msAnalysis_spectra(analysis)
spectra

all_files <- streamFindData::msFilePaths()
rcpp_parse_spectra(all_files[4], which = c(1, 2))

rcpp_parse_run(all_files[10])



rcpp_parse_msAnalysis(all_files[11])

rcpp_parse_msAnalysis(all_files[28])
rcpp_parse_msAnalysis(all_files[4])
rcpp_parse_msAnalysis(all_files[7])
rcpp_parse_msAnalysis(all_files[31])
rcpp_parse_msAnalysis(all_files[32])




# ms$subset_analyses("ana")
# ms$subset_features(1:2)
# ms$subset_groups(1:2)
#
#
# ms$remove_analyses(1)
#
# ms$get_spectra(analyses = 4:6, mz = targets)

ms$find_features()

ms$group_features()

ms$load_features_ms1()
ms$load_features_ms2()

ms$load_groups_ms1()
ms$load_groups_ms2()


ms$get_features(mass = neutral_targets)

anas <- parse.msAnalysis(files)
ms$add_analyses(anas)

ms$has_loaded_features_ms1()
ms$has_loaded_groups_ms1()


fts <- ms$get_features(mz = targets)
gr <- ms$get_groups(groups = unique(fts$group))
# ms$remove_features(fts)
# ms$remove_analyses(1:3)



test <- ms$subset_features(fts)

test$has_loaded_features_ms1()
test$get_features_ms1()


test <- ms$subset_groups(gr$group)
test$get_features(filtered = TRUE)
test$has_loaded_groups_ms1()
test$get_groups(filtered = TRUE)

test$remove_features(filtered = TRUE)


test$load_groups_ms1()


test$get_groups()
test$get_features()
length(test$get_analyses())

test$get_analysis_names()


test$get_overview()

test <- ms$subset_analyses(1:2)

analyses = ms$get_analysis_names(1:2)

test$get_groups()



nrow(ms$get_features(filtered = TRUE))


newFeatures <- lapply(ms$get_analyses(), function(x) {
  # x$features$group <- NULL
  x$features
})

newFeatures[[3]] <- NULL


test <- rcpp_features_df_list_to_df(newFeatures)
unique(test$analysis)


all.equal(ms$get_features(filtered = TRUE), rcpp_features_df_list_to_df(newFeatures))

ms$get_features()
ms$get_groups()

View(ms$get_analyses())
View(ms$get_groups())

all(unique(ms$get_features()$group) %in% ms$get_groups()$group)
all(unique(ms$get_groups()$group %in% ms$get_features()$group))


any(is.na(ms$get_features()$group))

ms$check_correspondence()


rcpp_ms_feature_groups_correspondence(ms$get_groups(), ms$get_features(), TRUE)


test <- ms$get_features()
View(rcpp_ms_make_groups_update_features(test)$features)
View(test$intensities)


fts <- ms$get_features(features = "m234.174_rt902_g177")

ms$plot_features(features = "m234.174_rt902_g177", interactive = F)


"[M]" %in% rcpp_ms_get_feature_groups_ranges(ms$get_groups()$group, ms$get_features())$adduct

rcpp_ms_get_feature_groups_ranges(ms$get_groups()$group, ms$get_features())



self <- ms$clone()

all_groups <- self$get_groups()$group

all_features_as_list <- lapply(self$get_analyses(), function(x) x$features)

ana1 <- all_features_as_list[[1]]
ana1_groups <- ana1$group
ana1_groups <- ana1_groups[!is.na(ana1_groups)]
groups_missing <- all_groups[!all_groups %in% ana1_groups]
groups_missing <- self$get_groups(groups = groups_missing)























