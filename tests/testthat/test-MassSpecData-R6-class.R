library(StreamFind)
library(testthat)

test_that("suggested dependencies", {
  expect_true(requireNamespace("StreamFindData"))
  expect_true(requireNamespace("xcms"))
  expect_true(requireNamespace("patRoon"))
})

# resources -----

all_files <- StreamFindData::get_ms_file_paths()
db <- StreamFindData::get_ms_tof_spiked_chemicals()
files_mrm <- all_files[grepl("mrm", all_files)]
files <- all_files[grepl("influent|blank", all_files)]
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
mz <- data.frame(
  id = c("tg1", "tg2"),
  mz = c(carb_pos, diu_pos),
  rt = c(carb_rt, diu_rt)
)
mass <- data.frame(
  id = c("tg1", "tg2"),
  mz = c(carb, diu),
  rt = c(carb_rt, diu_rt)
)
targets <- make_ms_targets(mz = mz, ppm = ppm_dev, sec = sec_dev)
neutral_targets <- make_ms_targets(mz = mass, ppm = ppm_dev, sec = sec_dev)

# MassSpecData class tests -----

test_that("test empty MassSpecData", {
  expect_equal(class(MassSpecData$new()), c("MassSpecData", "R6"))
})

ms <- MassSpecData$new(files, runParallel = FALSE)

test_that("create MassSpecData", {
  expect_equal(class(ms), c("MassSpecData", "R6"))
})

test_that("getter for features with empty object", {
  expect_equal(nrow(ms$get_features()), 0)
  expect_equal(nrow(ms$get_features_ms1()), 0)
  expect_equal(nrow(ms$get_features_ms2()), 0)
  expect_equal(nrow(ms$get_features_eic()), 0)
  expect_equal(nrow(ms$get_groups()), 0)
  expect_equal(nrow(ms$get_groups_ms1()), 0)
  expect_equal(nrow(ms$get_groups_ms2()), 0)
})

test_that("getter for names and filePaths", {
  expect_equal(
    unname(ms$get_analysis_names()),
    gsub(".mzML|.mzXML", "", basename(files))
  )
  expect_equal(unname(ms$get_files()), files)
  expect_s3_class(ms$get_overview(), "data.frame")
})

rpl <- c(
  rep("blank_neg", 3),
  rep("blank_pos", 3),
  rep("influent_neg", 3),
  rep("influent_pos", 3)
)

blk <- c(
  rep("blank_neg", 3),
  rep("blank_pos", 3),
  rep("blank_neg", 3),
  rep("blank_pos", 3)
)

ms$add_replicate_names(rpl)
ms$add_blank_names(blk)

test_that("test setter and getter for replicates and blanks", {
  expect_equal(unname(ms$get_replicate_names()), rpl)
  expect_equal(unname(ms$get_blank_names()), blk)
  expect_equal(names(ms$get_replicate_names()), unname(ms$get_analysis_names()))
  expect_equal(names(ms$get_blank_names()), unname(ms$get_analysis_names()))
})

ms2 <- MassSpecData$new(files2, runParallel = FALSE)

test_that("getter analyses", {
  expect_equal(class(ms2$get_analyses(1:3)), "list")
  expect_equal(length(ms2$get_analyses(1:3)), 3)
})

ms2$add_analyses(ms$get_analyses())

test_that("add analyses", {
  expect_equal(ms2$get_number_analyses(), 18)
})

ms3 <- ms2$subset_analyses(4:6)

test_that("subset analyses", {
  expect_equal(ms3$get_number_analyses(), 3)
})

ms3$load_spectra()

test_that("loading spectra", {
  expect_true(all(ms3$has_loaded_spectra()))
})

test_that("getting spectra for targets", {
  expect_s3_class(
    ms$get_spectra(analyses = 1, mz = targets, levels = 1), "data.frame"
  )
  expect_s3_class(
    ms3$get_spectra(analyses = 1, mz = targets, levels = c(1, 2)), "data.frame"
  )
  expect_true("id" %in%
    colnames(ms3$get_spectra(analyses = 1, mz = targets, levels = 1)))
  expect_true(2 %in%
    ms3$get_spectra(analyses = 1, mz = targets, allTraces = F, levels = 2)$level)
})

ms_mrm <- MassSpecData$new(files = files_mrm)
ms_mrm$load_chromatograms()
test_that("get chromatograms", {
  expect_true(all(ms_mrm$has_loaded_chromatograms()))
  expect_s3_class(ms_mrm$get_chromatograms(analyses = 1), "data.frame")
})

test_that("get tic and bpc", {
  expect_s3_class(ms$get_tic(2), "data.frame")
  expect_true("intensity" %in% colnames(ms$get_tic(2:3)))
  expect_s3_class(ms$get_bpc(2), "data.frame")
  expect_true("mz" %in% colnames(ms$get_bpc(2:3)))
})

test_that("get EIC, MS1 and MS2 spectra", {
  expect_s3_class(ms$get_eic(4, mz = targets), "data.table")
  expect_true("rt" %in% colnames(ms$get_eic(4, mz = targets)))
  expect_s3_class(ms$get_ms1(4,
    mz = targets, minIntensity = 10000
  ), "data.table")
  expect_true("mz" %in% colnames(ms$get_ms1(4,
    mz = targets, minIntensity = 10000
  )))
  expect_s3_class(ms$get_ms2(4, mz = targets), "data.table")
  expect_true("is_pre" %in% colnames(ms$get_ms2(4, mz = targets)))
})

# ms$plot_tic(colorBy = "replicates")
# ms$plot_bpc(colorBy = "replicates")
# ms$plot_xic(analyses = 4:5, mz = targets, targetsMark = targets)
# ms$plot_eic(analyses = 4:5, mz = targets, title = "Test plot!")
# ms$plot_ms2(analyses = 4:5, mz = targets, minIntensity = 500)

settings_ff <- Settings_find_features_xcms3_centwave(
  ppm = 12,
  peakwidth = c(5, 40),
  snthresh = 10,
  prefilter = c(6, 5000),
  mzCenterFun = "mean",
  integrate = 2,
  mzdiff = 0.0005,
  fitgauss = TRUE,
  noise = 1500,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = TRUE
)


ms$add_settings(settings = settings_ff)

test_that("add and get settings", {
  expect_true(is.list(ms$get_settings(call = "find_features")))
  expect_true(ms$has_settings("find_features"))
})

suppressWarnings(ms$find_features(settings = settings_ff))

test_that("find and get features", {
  expect_s3_class(ms$get_features(mz = targets), "data.table")
  expect_true("mz" %in% colnames(ms$get_features(mz = targets[1, ])))
  expect_true(all(ms$has_features()))
})

ftar <- ms$get_features(analyses = 4, mz = targets)

test_that("get MS1 and MS2 for features", {
  expect_s3_class(ms$get_features_ms1(features = ftar$feature), "data.frame")
  expect_gt(nrow(ms$get_features_ms1(features = ftar$feature)), 0)
  expect_s3_class(ms$get_features_ms2(features = ftar$feature), "data.frame")
  expect_gt(nrow(ms$get_features_ms2(features = ftar$feature)), 0)
  expect_equal(nrow(ms$get_features_ms1(analyses = 1, features = ftar$feature[12])), 0)
  expect_equal(nrow(ms$get_features_ms1(analyses = 1, mz = ftar)), 0)
  expect_s3_class(ms$get_features_ms2(features = ftar$feature), "data.frame")
  expect_equal(nrow(ms$get_features_ms2(analyses = 1, features = ftar$feature)), 0)
  expect_equal(nrow(ms$get_features_ms2(analyses = 1, mz = ftar)), 0)
})

# ms$plot_features_ms1(features = ftar$feature)
# ms$plot_features_ms2(features = ftar$feature)
# ms$plot_features_ms1(features = ftar$feature, interactive = FALSE)
# ms$plot_features_ms2(features = ftar$feature, interactive = FALSE)

settings_gf <- Settings_group_features_xcms3_peakdensity(
  bw = 5,
  minFraction = 0.5,
  minSamples = 1,
  binSize = 0.008,
  maxFeatures = 100
)

ms$group_features(settings = settings_gf)

test_that("group features", {
  expect_s3_class(ms$get_groups(mass = neutral_targets), "data.table")
  expect_true("group" %in% colnames(ms$get_groups(mz = neutral_targets[1, ])))
  expect_true(all(ms$has_groups()))
})

# ms$plot_groups(mz = targets, legendNames = c("Target1", "Target2"))
# ms$plot_groups_overview(mz = targets)

settings_gf_alignment <- Settings_group_features_xcms3_peakdensity_peakgroups(
  bw = 3,
  minFraction = 0.6,
  minSamples = 2,
  binSize = 0.008,
  pre_bw = 5,
  pre_minFraction = 1,
  pre_minSamples = 3,
  pre_binSize = 0.008,
  maxFeatures = 100,
  rtAlignMinFraction = 0.3,
  extraPeaks = 0,
  smooth = "loess",
  span = 0.3,
  family = "gaussian"
)

ms4 <- ms$subset_analyses(analyses = 4:6)

ms4$group_features(settings = settings_gf_alignment)

test_that("alignment of features", {
  expect_gt(length(ms4$get_alignment()), 1)
  expect_true(all(ms4$has_alignment()))
})

# ms4$plot_alignment()

ms$save_headers()
ms$save_settings()
ms$save_analyses()
ms$save_groups()
ms$save()

test_that("save private fields as json", {
  expect_true(file.exists("headers.json"))
  expect_true(file.exists("settings.json"))
  expect_true(file.exists("analyses.json"))
  expect_true(file.exists("groups.json"))
  expect_true(file.exists("MassSpecData.json"))
})

ms5 <- MassSpecData$new()

test_that("import headers and settings from json file", {
  expect_invisible(ms5$import_headers("headers.json"))
  expect_invisible(ms5$import_settings("settings.json"))
  expect_equal(ms$get_headers(), ms5$get_headers())
  expect_equal(names(ms$get_settings()), names(ms5$get_settings()))
})

test_that("import analyses and groups from json file", {
  expect_invisible(ms5$import_analyses("analyses.json"))
  expect_invisible(ms5$import_groups("groups.json"))
  expect_equal(ms$get_analysis_names(), ms5$get_analysis_names())
  expect_equal(ms$get_groups()[["group"]], ms5$get_groups()[["group"]])
})

test_that("import MassSpecData object from json file", {
  expect_equal(ms5, import_MassSpecData("MassSpecData.json"))
  expect_equal(ms$get_groups()[["group"]], ms5$get_groups()[["group"]])
})

ms5$remove_analyses(c(1:3, 7:9))

test_that("remove analyses", {
  expect_equal(ms5$get_number_analyses(), 6)
  expect_lt(nrow(ms5$get_groups()), nrow(ms$get_groups()))
})

org_g_number <- nrow(ms5$get_groups())
ms5$remove_groups("not_a_group_name")

test_that("remove 0 groups (wrong name)", {
  expect_equal(nrow(ms5$get_groups()), org_g_number)
})

n_fts <- nrow(ms5$get_features(filtered = FALSE))
n_fts_total <- nrow(ms5$get_features(filtered = TRUE))
ms5$remove_groups(1:2)

test_that("remove 2 groups", {
  expect_lt(nrow(ms5$get_groups()), org_g_number)
  expect_equal(nrow(ms5$get_features(filtered = TRUE)), n_fts_total)
  expect_lt(nrow(ms5$get_features()), n_fts)
})

fts_to_rem <- ms5$get_features(mz = targets)
ms5$remove_features(fts_to_rem)

test_that("remove 12 features from targets", {
  expect_lt(nrow(ms5$get_features()), n_fts_total)
  expect_equal(nrow(ms5$get_groups(groups = unique(fts_to_rem$group))), 0)
  expect_equal(nrow(ms5$get_features(mz = targets)), 0)
})

ms5$remove_groups()

test_that("remove groups completely", {
  expect_false(ms5$has_groups())
  expect_false(any(ms5$get_features()[["filtered"]]))
})

ms5$remove_features()

test_that("remove features completely", {
  expect_false(any(ms5$has_features()))
})

ms5$remove_settings("group_features")

test_that("remove settings", {
  expect_null(suppressWarnings(ms5$get_settings("group_features"))[[1]])
})

file.remove(c("headers.json", "analyses.json", "groups.json", "settings.json"))

ms5 <- import_MassSpecData("MassSpecData.json")
fts_to_subset <- ms5$get_features(mz = targets)
ms5 <- ms5$subset_features(features = fts_to_subset)

test_that("subset features", {
  expect_equal(nrow(ms5$get_features()), 12)
  expect_equal(nrow(ms5$get_groups()), 2)
})

slfms1 <- Settings_load_features_ms1_StreamFind(
  rtWindow = c(-2, 2),
  mzWindow = c(-1, 6),
  mzClust = 0.003,
  presence = 0.8,
  minIntensity = 250,
  filtered = FALSE,
  runParallel = FALSE,
  verbose = FALSE
)


slfms2 <- Settings_load_features_ms2_StreamFind(
  isolationWindow = 1.3,
  mzClust = 0.003,
  presence = 0.8,
  minIntensity = 0,
  filtered = FALSE,
  runParallel = FALSE,
  verbose = FALSE
)

ms5$load_features_ms1(settings = slfms1)

test_that("load MS1 features", {
  expect_true(any(ms5$has_loaded_features_ms1()))
  expect_equal(
    unique(ms5$get_features_ms1(loadedMS1 = TRUE)[["feature"]]),
    unique(fts_to_subset$feature)
  )
})

ms5$remove_features_ms1()

test_that("remove loaded MS1 features", {
  expect_true(!any(ms5$has_loaded_features_ms1()))
})

ms5$load_features_ms2(settings = slfms2)

test_that("load MS2 features", {
  expect_true(any(ms5$has_loaded_features_ms2()))
  expect_equal(
      unique(ms5$get_features_ms2(loadedMS2 = TRUE)[["feature"]]),
      unique(fts_to_subset$feature)
  )
})

ms5$remove_features_ms2()

test_that("remove loaded MS2 features", {
  expect_true(!any(ms5$has_loaded_features_ms2()))
})

ms5 <- import_MassSpecData("MassSpecData.json")
groups_to_subset <- ms5$get_groups(mass = neutral_targets)
ms5 <- ms5$subset_groups(groups = groups_to_subset$group)

test_that("subset groups", {
  expect_equal(nrow(ms5$get_features()), 12)
  expect_equal(
    nrow(ms5$get_features(filtered = TRUE)),
    nrow(ms$get_features(filtered = TRUE))
  )
  expect_equal(nrow(ms5$get_groups()), 2)
})

ms5$remove_features(filtered = TRUE)

test_that("remove filtered features", {
  expect_equal(
    nrow(ms5$get_features(filtered = TRUE)),
    nrow(ms5$get_features(filtered = FALSE))
  )
  expect_equal(nrow(ms5$get_groups()), 2)
})

patRoon::clearCache("parsed_ms_spectra")
patRoon::clearCache("load_features_ms1")
patRoon::clearCache("load_features_ms2")
patRoon::clearCache("load_groups_ms1")
patRoon::clearCache("load_groups_ms2")

ms5$load_features_ms1(settings = slfms1)

ms5$load_features_ms2(settings = slfms2)


slfgms1 <- Settings_load_groups_ms1_StreamFind(
  mzClust = 0.003,
  presence = 0.8,
  minIntensity = 1000,
  verbose = FALSE,
  filtered = FALSE,
  runParallel = FALSE
)

slfgms2 <- Settings_load_groups_ms2_StreamFind(
  mzClust = 0.003,
  presence = 0.8,
  minIntensity = 250,
  filtered = FALSE,
  runParallel = FALSE,
  verbose = FALSE
)

ms6 <- ms5$subset_analyses(which(grepl("pos", ms5$get_analysis_names())))


ms6$load_groups_ms1(settings = slfgms1)

test_that("load MS1 groups", {
  expect_true(any(ms6$has_loaded_groups_ms1()))
  expect_equal(
    unique(ms6$get_groups_ms1()[["group"]]),
    groups_to_subset$group
  )
})

ms6$remove_groups_ms1()

test_that("remove loaded MS1 groups", {
  expect_false(ms6$has_loaded_groups_ms1())
})

ms6$load_groups_ms2(settings = slfgms2)

test_that("load MS2 groups", {
  expect_true(any(ms6$has_loaded_groups_ms2()))
  expect_equal(
    unique(ms6$get_groups_ms2()[["group"]]),
    groups_to_subset$group
  )
})

ms6$remove_groups_ms2()

test_that("remove loaded MS2 groups", {
  expect_false(ms6$has_loaded_groups_ms2())
})

file.remove("MassSpecData.json")

patRoon::clearCache("all")



# todos -----

# TODO Make a validation function for the validity of class content.
# To be used when importing from rds file.

# TODO Make check for single polarity when removing or sub-setting analyses

# TODO check for features with mzmax - mzmin = 0, why?

# TODO Improve methods for plotting already produced data.frames from
# class functions, similar to S4 implementation for data.table

# TODO Implement self filling function for missing features in groups

# TODO Transfer filters for feature groups from S4 to R6 and add filters for
# simple groups. Also, add method to filter a specific replicate other than
# assigned blank

# TODO annotation after feature finding

# TODO improved grouping based on annotation

# work Lines -----

# ms <- MassSpecData$new(files, runParallel = FALSE)
# rpl <- c(rep("blank_neg", 3),rep("blank_pos", 3),rep("influent_neg", 3),rep("influent_pos", 3))
# blk <- c(rep("blank_neg", 3),rep("blank_pos", 3),rep("blank_neg", 3),rep("blank_pos", 3))
# ms$add_replicate_names(rpl)
# ms$add_blank_names(blk)
# settings_ff <- list(
#   "call" = "find_features",
#   "algorithm" = "xcms3",
#   "parameters" = list(xcms::CentWaveParam(
#     ppm = 12, peakwidth = c(5, 30),
#     snthresh = 10, prefilter = c(5, 3000),
#     mzCenterFun = "mean", integrate = 2,
#     mzdiff = -0.0005, fitgauss = TRUE,
#     noise = 1000, verboseColumns = TRUE,
#     firstBaselineCheck = FALSE,
#     extendLengthMSW = TRUE
#   ))
# )
# settings_gf <- list(
#   "call" = "group_features",
#   "algorithm" = "xcms3",
#   "parameters" = list(
#     groupParam = xcms::PeakDensityParam(
#       sampleGroups = "holder",
#       bw = 5,
#       minFraction = 0.5,
#       minSamples = 1,
#       binSize = 0.008,
#       maxFeatures = 100
#     )
#   )
# )
# settings_gf_alignment <- list(
#   "call" = "group_features",
#   "algorithm" = "xcms3",
#   "parameters" = list(
#     rtalign = TRUE,
#     loadRawData = TRUE,
#     groupParam = xcms::PeakDensityParam(
#       sampleGroups = "holder",
#       bw = 3,
#       minFraction = 0.6,
#       minSamples = 2,
#       binSize = 0.008,
#       maxFeatures = 100
#     ),
#     preGroupParam = xcms::PeakDensityParam(
#       sampleGroups = "holder",
#       bw = 5,
#       minFraction = 1,
#       minSamples = 3,
#       binSize = 0.008,
#       maxFeatures = 100
#     ),
#     retAlignParam = xcms::PeakGroupsParam(
#       minFraction = 1,
#       extraPeaks = 0,
#       smooth = "loess",
#       span = 0.3,
#       family = "gaussian"
#     )
#   )
# )

# ms <- MassSpecData$new(files[c(4:5)], runParallel = FALSE) #, 10:12
# ms$find_features(settings = settings_ff)
# ms$group_features(settings = settings_gf)
# ms$group_features(settings = settings_gf_alignment)



# slfms1 <- list(
#   "call" = "load_features_ms1",
#   "algorithm" = "StreamFind",
#   "parameters" = list(
#     rtWindow = c(-2, 2),
#     mzWindow = c(-1, 6),
#     mzClust = 0.001,
#     minIntensity = 250,
#     filtered = FALSE,
#     runParallel = FALSE,
#     verbose = FALSE
#   )
# )

# slfms2 <- list(
#   "call" = "load_features_ms2",
#   "algorithm" = "StreamFind",
#   "parameters" = list(
#     isolationWindow = 1.3,
#     mzClust = 0.001,
#     minIntensity = 250,
#     filtered = FALSE,
#     runParallel = FALSE,
#     verbose = FALSE
#   )
# )

# ms$add_settings(slfms2)
# ms$get_settings("load_features_ms2")

# ms$load_features_ms1()







# make_ms_targets test -----

# case 1
mz1 <- c(carb_pos, diu_pos)
rt1 <- c(carb_rt, diu_rt)
id1 <- c("target1", "target2")
targets1 <- make_ms_targets(mz = mz1, rt = rt1, ppm = ppm_dev, sec = sec_dev, id = id1)

# case 2
ppm_carb <- ppm_dev / 1E6 * carb_pos
ppm_diu <- ppm_dev / 1E6 * diu_pos

mz2 <- data.frame(
  mzmin = c(carb_pos - ppm_carb, diu_pos - ppm_diu),
  mzmax = c(carb_pos + ppm_carb, diu_pos + ppm_diu)
)

rt2 <- data.frame(
  rtmin = c(carb_rt - sec_dev, diu_rt - sec_dev),
  rtmax = c(carb_rt + sec_dev, diu_rt + sec_dev)
)

targets2 <- make_ms_targets(mz = mz2, rt = rt2)

# case 3
mz3 <- data.frame(
  id = c("target1", "target2"),
  mz = c(carb_pos, diu_pos),
  rt = c(carb_rt, diu_rt)
)

targets3 <- make_ms_targets(mz = mz3, ppm = ppm_dev, sec = sec_dev)

# case 4
mz4 <- data.frame(
  id = c("target1", "target2"),
  mzmin = c(carb_pos - ppm_carb, diu_pos - ppm_diu),
  mzmax = c(carb_pos + ppm_carb, diu_pos + ppm_diu),
  rtmin = c(carb_rt - sec_dev, diu_rt - sec_dev),
  rtmax = c(carb_rt + sec_dev, diu_rt + sec_dev)
)

targets4 <- make_ms_targets(mz = mz4)

t1 <- rbind(targets1[1, ], targets2[1, ], targets3[1, ], targets4[1, ])
t2 <- rbind(targets1[2, ], targets2[2, ], targets3[2, ], targets4[2, ])

test_that("targets all equal", {
  expect_true(all(round(apply(t1[, 2:7], 2, sd), digits = 4) == 0))
  expect_true(all(round(apply(t2[, 2:7], 2, sd), digits = 4) == 0))
})
