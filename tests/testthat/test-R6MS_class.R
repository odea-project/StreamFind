
library(streamFind)
library(testthat)

test_that("suggested dependencies", {
  expect_true(requireNamespace("streamFindData"))
  expect_true(requireNamespace("xcms"))
  expect_true(requireNamespace("patRoon"))
})

# resources -----

all_files <- streamFindData::msFilePaths()
db <- streamFindData::msSpikedChemicals()
files_mrm <- all_files[grepl("mrm", all_files)]
files <- all_files[grepl("influent|blank", all_files)]
files2 <- all_files[grepl("o3sw", all_files)]

#Carbamazepin-d10 is ionized only in positive mode
carbamazepin_d10 <- db[name %in% "Carbamazepin-d10", .(name, mass, rt)]

#Diuron-d6 is ionized in both positive and negative modes
diuron_d6 <- db[name %in% "Diuron-d6", .(name, mass, rt)]

carb_pos <- carbamazepin_d10$mass + 1.0073
carb <- carbamazepin_d10$mass + 1.0073
carb_rt <- carbamazepin_d10$rt

diu_pos <- diuron_d6$mass + 1.0073
diu <- diuron_d6$mass + 1.0073
diu_rt <- diuron_d6$rt

sec_dev <- 60
ppm_dev <- 20

mz <- data.frame(id = c("tg1", "tg2"),mz = c(carb, diu),rt = c(carb_rt, diu_rt))
targets <- makeTargets(mz = mz, ppm = ppm_dev, sec = sec_dev)

# R6MS class tests -----

ms <- R6MS$new(files, run_parallel = FALSE)

test_that("create R6MS", {
  expect_equal(class(ms), c("R6MS", "R6"))
})

test_that("getter for names and filePaths", {
  expect_equal(unname(ms$get_analysis_names()),
    gsub(".mzML|.mzXML", "", basename(files)))
  expect_equal(unname(ms$get_file_paths()), files)
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

ms$set_replicate_names(rpl)
ms$set_blank_names(blk)

test_that("test setter and getter for replicates and blanks", {
  expect_equal(unname(ms$get_replicate_names()), rpl)
  expect_equal(unname(ms$get_blank_names()), blk)
  expect_equal(names(ms$get_replicate_names()), unname(ms$get_analysis_names()))
  expect_equal(names(ms$get_blank_names()), unname(ms$get_analysis_names()))
})

ms2 <- R6MS$new(files2, run_parallel = TRUE)

test_that("gets analyses", {
  expect_equal(class(ms2$get_analyses(1:3)), "list")
  expect_equal(length(ms2$get_analyses(1:3)), 3)
})

ms2$add_analyses(ms$get_analyses())

test_that("adds analyses", {
  expect_equal(ms2$number_analyses, 18)
})

ms3 = ms2$subset_analyses(4:6)

test_that("subset analyses", {
  expect_equal(ms3$number_analyses, 3)
})

ms3$load_spectra()

test_that("loading spectra", {
  expect_true(all(ms3$has_loaded_spectra()))
})

test_that("getting spectra for targets", {
  expect_s3_class(
    ms3$get_spectra(analyses = 1, mz = targets, level = 1),"data.frame")
  expect_true("id" %in%
    colnames(ms3$get_spectra(analyses = 1, mz = targets, level = 1)))
  expect_true(2 %in%
    ms3$get_spectra(analyses = 1, mz = targets, allTraces = F, level = 2)$level)
})

ms_mrm <- R6MS$new(files_mrm)
ms_mrm$load_chromatograms()
test_that("get chromatograms", {
  expect_true(all(ms_mrm$has_loaded_chromatograms()))
  expect_s3_class(ms_mrm$get_chromatograms(analyses = 1),"data.frame")
})

test_that("get tics and bpcs", {
  expect_s3_class(ms$get_tics(2),"data.frame")
  expect_true("intensity" %in% colnames(ms$get_tics(2:3)))
  expect_s3_class(ms$get_bpcs(2),"data.frame")
  expect_true("mz" %in% colnames(ms$get_bpcs(2:3)))
})

test_that("get EIC and MS2 spectra", {

  expect_s3_class(ms$get_eics(4, mz = targets), "data.table")
  expect_true("rt" %in% colnames(ms$get_eics(4, mz = targets)))

  expect_s3_class(ms$get_ms2(4, mz = targets), "data.table")
  expect_true("isPre" %in% colnames(ms$get_ms2(4, mz = targets)))

})

# ms$plot_tic(colorBy = "replicates")
# ms$plot_bpc(colorBy = "replicates")
# ms$plot_xic(analyses = 4:5, mz = targets, targetsMark = targets)
# ms$plot_eic(analyses = 4:5, mz = targets, title = "Test plot!")
# ms$plot_ms2(analyses = 4:5, mz = targets, minIntensity = 500)

settings_ff <- createSettings(
  call = "find_features",
  algorithm = "xcms3",
  parameters = xcms::CentWaveParam(
    ppm = 12, peakwidth = c(5, 40),
    snthresh = 10, prefilter = c(4, 800),
    mzCenterFun = "mean", integrate = 2,
    mzdiff = -0.0001, fitgauss = TRUE,
    noise = 250, verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = TRUE
  )
)

exportSettings(settings_ff, name = "settings", format = "json")

test_that("create settings S4 class", {
  expect_s4_class(settings_ff, "settings")

  expect_s4_class(importSettings(paste0(getwd(),
    "/settings.json")), "settings")
})

ms$add_settings(settings = settings_ff)

test_that("add and get settings", {
  expect_s4_class(ms$get_settings(call = "find_features"), "settings")
  expect_true(ms$has_settings("find_features"))
})

ms$find_features(settings = settings_ff)

test_that("find and get features", {
  expect_s3_class(ms$get_features(mz = targets), "data.table")
  expect_true("mz" %in% colnames(ms$get_features(mz = targets[1,])))
  expect_true(all(ms$has_features()))
})

settings_gf <- createSettings(
  call = "group_features",
  algorithm = "xcms3",
  parameters = list(
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

ms$group_features(settings = settings_gf)

test_that("group features", {
  expect_s3_class(ms$get_feature_groups(mz = targets), "data.table")
  expect_true("group" %in% colnames(ms$get_feature_groups(mz = targets[1,])))
  expect_true(all(ms$has_feature_groups()))
})






settings_gf_2 <- createSettings(
  call = "group_features",
  algorithm = "xcms3",
  parameters = list(
    rtalign = TRUE,
    loadRawData = TRUE,
    groupParam = xcms::PeakDensityParam(
      sampleGroups = "holder",
      bw = 3,
      minFraction = 0.6,
      minSamples = 2,
      binSize = 0.008,
      maxFeatures = 100),
    preGroupParam = xcms::PeakDensityParam(
      sampleGroups = "holder",
      bw = 5,
      minFraction = 1,
      minSamples = 3,
      binSize = 0.008,
      maxFeatures = 100),
    retAlignParam = xcms::PeakGroupsParam(
      minFraction = 1,
      extraPeaks = 0,
      smooth = "loess",
      span = 0.3,
      family = "gaussian")
  )
)

ms <- R6MS$new(files[c(4:6, 10:12)], run_parallel = FALSE)
#ms$add_settings(settings = settings_ff)
#object = ms$clone(deep = T)
ms$find_features(settings = settings_ff)
ms$group_features(settings = settings_gf_2)

ms$plot_feature_groups(mz = targets, colorBy = "analyses", interactive = FALSE)
ms$get_feature_groups(mass = diuron_d6, onlyIntensities = TRUE, average = TRUE)
ms$plot_feature_groups(mz = targets, legendNames = c("Tar1", "Tar2"))

ms$plot_alignment()
self = ms$clone(deep = T)

View(ms$get_features())

fts = ms$get_features(mass = diuron_d6)#[7226, ]

ms$plot_eic(analyses = ft$analysis, mz = fts)
ms$plot_features(analyses = ft$analysis, mz = fts)
ms$map_features(mz = targets, colorBy = "analyses")

ms$get_features(mass = diuron_d6)


ms$add_settings(settings = settings_pp)
ms$get_settings()





md_set <- peakPicking(md_set)

test_that("test_methods_for_peaks", {
  expect_s3_class(peaks(md_set), "data.table")
  expect_gt(nrow(peaks(md_set)), 1)
  expect_true(all(hasPeaks(md_set)))
  expect_s4_class(as.features(md_set), "features")

})





# spectra(md_set_2)
# chromatograms(md_set_2)
#
# EICs(md_set_2, mz = targets4)
#
# start <- Sys.time()
#
# plotEICs(md_set, mz = targets4, run_parallel = TRUE)
#
# Sys.time() - start
#
# start <- Sys.time()
#
# plotEICs(md_set, mz = targets4, run_parallel = FALSE)
#
# Sys.time() - start
#
#
# TICs(md_set_2)
# plotTICs(md_set_2, interactive = TRUE)
#
# BPCs(md_set_2)
# plotBPCs(md_set_2)
#
#
# XICs(md_set_2, mz = targets4)
# plotXICs(md_set_2[4:5], mz = targets4[1, ])
#
# MS2s(md_set[4:5], mz = targets4)
# plotMS2s(md_set[4:5], mz = targets4[1,], colorBy = "analyses")
#
#
#
# plotSpectra(md_set_2, analyses = 1, mz = targets4, colorBy = "levels")
# plotChromatograms(md_set_2, id = "TIC", colorBy = "replicates")
#
# plotPeaks(md_set, mass = diuron_d6, run_parallel = TRUE, interactive = TRUE)
# mapPeaks(md_set, mass = diuron_d6)






















# other tests -----

#case 1
mz1 <- c(carb_pos, diu_pos)
rt1 <- c(carb_rt, diu_rt)
id1 <- c("target1", "target2")
targets1 <- makeTargets(mz = mz1, rt = rt1, ppm = ppm_dev, sec = sec_dev, id = id1)

#case 2
ppm_carb <- ppm_dev / 1E6 * carb_pos
ppm_diu <- ppm_dev / 1E6 * diu_pos

mz2 <- data.frame(mzmin = c(carb_pos - ppm_carb, diu_pos - ppm_diu),
                  mzmax = c(carb_pos + ppm_carb, diu_pos + ppm_diu))

rt2 <- data.frame(rtmin = c(carb_rt - sec_dev, diu_rt - sec_dev),
                  rtmax = c(carb_rt + sec_dev, diu_rt + sec_dev))

targets2 <- makeTargets(mz = mz2, rt = rt2)

#case 3
mz3 <- data.frame(
  id = c("target1", "target2"),
  mz = c(carb_pos, diu_pos),
  rt = c(carb_rt, diu_rt)
)

targets3 <- makeTargets(mz = mz3, ppm = ppm_dev, sec = sec_dev)

#case 4
mz4 <- data.frame(
  id = c("target1", "target2"),
  mzmin = c(carb_pos - ppm_carb, diu_pos - ppm_diu),
  mzmax = c(carb_pos + ppm_carb, diu_pos + ppm_diu),
  rtmin = c(carb_rt - sec_dev, diu_rt - sec_dev),
  rtmax = c(carb_rt + sec_dev, diu_rt + sec_dev)
)

targets4 <- makeTargets(mz = mz4)

t1 <- rbind(targets1[1, ], targets2[1,], targets3[1, ], targets4[1, ])
t2 <- rbind(targets1[2, ], targets2[2,], targets3[2, ], targets4[2, ])

test_that("targets all equal", {
  expect_true(all(round(apply(t1[, 2:7], 2, sd), digits = 4) == 0))
  expect_true(all(round(apply(t2[, 2:7], 2, sd), digits = 4) == 0))
})
