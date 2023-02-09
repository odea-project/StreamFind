
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

sec_dev <- 30
ppm_dev <- 10

mz <- data.frame(id = c("tg1", "tg2"),mz = c(carb, diu),rt = c(carb_rt, diu_rt))
targets <- makeTargets(mz = mz, ppm = ppm_dev, sec = sec_dev)

# R6MS class tests -----

test_that("test empty R6MS", {
  expect_equal(class(R6MS$new()), c("R6MS", "R6"))
})

ms <- R6MS$new(files, run_parallel = FALSE)

test_that("create R6MS", {
  expect_equal(class(ms), c("R6MS", "R6"))
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

test_that("getter analyses", {
  expect_equal(class(ms2$get_analyses(1:3)), "list")
  expect_equal(length(ms2$get_analyses(1:3)), 3)
})

ms2$add_analyses(ms$get_analyses())

test_that("add analyses", {
  expect_equal(ms2$get_number_analyses(), 18)
})

ms3 = ms2$subset_analyses(4:6)

test_that("subset analyses", {
  expect_equal(ms3$get_number_analyses(), 3)
})

ms3$load_spectra()

test_that("loading spectra", {
  expect_true(all(ms3$has_loaded_spectra()))
})

test_that("getting spectra for targets", {
  expect_s3_class(
    ms$get_spectra(analyses = 1, mz = targets, level = 1),"data.frame")
  expect_s3_class(
    ms3$get_spectra(analyses = 1, mz = targets, level = c(1, 2)),"data.frame")
  expect_true("id" %in%
    colnames(ms3$get_spectra(analyses = 1, mz = targets, level = 1)))
  expect_true(2 %in%
    ms3$get_spectra(analyses = 1, mz = targets, allTraces = F, level = 2)$level)
})

ms_mrm <- R6MS$new(files = files_mrm)
ms_mrm$load_chromatograms()
test_that("get chromatograms", {
  expect_true(all(ms_mrm$has_loaded_chromatograms()))
  expect_s3_class(ms_mrm$get_chromatograms(analyses = 1),"data.frame")
})

test_that("get tic and bpc", {
  expect_s3_class(ms$get_tic(2),"data.frame")
  expect_true("intensity" %in% colnames(ms$get_tic(2:3)))
  expect_s3_class(ms$get_bpc(2),"data.frame")
  expect_true("mz" %in% colnames(ms$get_bpc(2:3)))
})

test_that("get EIC, MS1 and MS2 spectra", {
  expect_s3_class(ms$get_eic(4, mz = targets), "data.table")
  expect_true("rt" %in% colnames(ms$get_eic(4, mz = targets)))
  expect_s3_class(ms$get_ms1(4,
    mz = targets, minIntensity = 10000), "data.table")
  expect_true("mz" %in% colnames(ms$get_ms1(4,
    mz = targets, minIntensity = 10000)))
  expect_s3_class(ms$get_ms2(4, mz = targets), "data.table")
  expect_true("isPre" %in% colnames(ms$get_ms2(4, mz = targets)))
})

# ms$plot_tic(colorBy = "replicates")
# ms$plot_bpc(colorBy = "replicates")
# ms$plot_xic(analyses = 4:5, mz = targets, targetsMark = targets)
# ms$plot_eic(analyses = 4:5, mz = targets, title = "Test plot!")
# ms$plot_ms2(analyses = 4:5, mz = targets, minIntensity = 500)

settings_ff <- list(
  "call" = "find_features",
  "algorithm" = "xcms3",
  "parameters" = list(xcms::CentWaveParam(
    ppm = 12, peakwidth = c(5, 40),
    snthresh = 10, prefilter = c(4, 800),
    mzCenterFun = "mean", integrate = 2,
    mzdiff = -0.0001, fitgauss = TRUE,
    noise = 250, verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = TRUE
  ))
)

ms$add_settings(settings = settings_ff)

test_that("add and get settings", {
  expect_true(is.list(ms$get_settings(call = "find_features")))
  expect_true(ms$has_settings("find_features"))
})

ms$find_features(settings = settings_ff)

test_that("find and get features", {
  expect_s3_class(ms$get_features(mz = targets), "data.table")
  expect_true("mz" %in% colnames(ms$get_features(mz = targets[1,])))
  expect_true(all(ms$has_features()))
})

ftar = ms$get_features(analyses = 4, mz = targets)

test_that("get MS1 and MS2 for features", {
  expect_s3_class(ms$get_features_ms1(id = ftar$id), "data.frame")
  expect_gt(nrow(ms$get_features_ms1(id = ftar$id)), 0)
  expect_s3_class(ms$get_features_ms2(id = ftar$id), "data.frame")
  expect_gt(nrow(ms$get_features_ms2(id = ftar$id)), 0)
  expect_equal(nrow(ms$get_features_ms1(analyses = 1, id = ftar$id)), 0)
  expect_equal(nrow(ms$get_features_ms1(analyses = 1, mz = ftar)), 0)
  expect_s3_class(ms$get_features_ms2(id = ftar$id), "data.frame")
  expect_equal(nrow(ms$get_features_ms2(analyses = 1, id = ftar$id)), 0)
  expect_equal(nrow(ms$get_features_ms2(analyses = 1, mz = ftar)), 0)
})

# ms$plot_features_ms1(id = ftar$id)
# ms$plot_features_ms2(id = ftar$id)
# ms$plot_features_ms1(id = ftar$id, interactive = FALSE)
# ms$plot_features_ms2(id = ftar$id, interactive = FALSE)

settings_gf <- list(
  "call" = "group_features",
  "algorithm" = "xcms3",
  "parameters" = list(
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
  expect_s3_class(ms$get_groups(mz = targets), "data.table")
  expect_true("group" %in% colnames(ms$get_groups(mz = targets[1,])))
  expect_true(all(ms$has_groups()))
})

test_that("get feature groups MS1 and MS2", {
  expect_s3_class(ms$get_groups_ms1(mz = targets), "data.table")
  expect_gt(nrow(ms$get_groups_ms1(mz = targets)), 0)
  expect_s3_class(ms$get_groups_ms2(mz = targets), "data.table")
  expect_gt(nrow(ms$get_groups_ms2(mz = targets)), 0)
})

# ms$plot_groups(mz = targets, legendNames = c("Target1", "Target2"))
# ms$plot_groups_overview(mz = targets)

settings_gf_alignment <- list(
  "call" = "group_features",
  "algorithm" = "xcms3",
  "parameters" = list(
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

ms4 = ms$subset_analyses(4:6)

ms4$group_features(settings = settings_gf_alignment)

test_that("alignment of features", {
  expect_gt(length(ms4$get_alignment()), 1)
  expect_true(all(ms4$has_alignment()))
})

# ms4$plot_alignment()

ms$save_header()
ms$save_settings()
ms$save_analyses()
ms$save_groups()
ms$save()

test_that("save private fields as json", {
  expect_true(file.exists("header.json"))
  expect_true(file.exists("settings.json"))
  expect_true(file.exists("analyses.json"))
  expect_true(file.exists("groups.json"))
  expect_true(file.exists("msData.json"))
})

ms5 = R6MS$new()

test_that("import header and settings from json file", {
  expect_invisible(ms5$import_header("header.json"))
  expect_invisible(ms5$import_settings("settings.json"))
  expect_equal(ms$get_header(), ms5$get_header())
  expect_equal(ms$get_settings(), ms5$get_settings())
})

test_that("import analyses and groups from json file", {
  expect_invisible(ms5$import_analyses("analyses.json"))
  expect_invisible(ms5$import_groups("groups.json"))
  expect_equal(ms$get_analysis_names(), ms5$get_analysis_names())
  expect_equal(ms$get_groups()[["group"]], ms5$get_groups()[["group"]])
})

test_that("import R6MS object from json file", {
  expect_equal(ms5, import_R6MS("msData.json"))
  expect_equal(ms$get_groups()[["group"]], ms5$get_groups()[["group"]])
})

file.remove(c("header.json", "analyses.json", "groups.json", "msData.json"))
file.remove("settings.json")














# TODO Implement a field for storing MS lists for each feature/feature groups

# TODO Make a validation function for the validity of class content.
# To be used when importing from rds file.

# TODO Improve methods for plotting already produced data.frames from
# class functions, similar to S4 implementation for data.table

# TODO Implement sub-setting for features and feature groups

# TODO Implement self filling function for missing features in groups

# TODO Transfer filters for feature groups from S4 to R6 and add filters for
# simple groups. Also, add method to filter a specific replicate other than
# assigned blank

# TODO annotation after feature finding

# TODO improved grouping based on annotation

# Work Lines -----

ms <- R6MS$new(files[c(4:6, 10:12)], run_parallel = FALSE)
ms$find_features(settings = settings_ff)
ms$group_features(settings = settings_gf_alignment)
self = ms$clone(deep = T)

ms$save()
ms$save_header()

ms2 = import_R6MS(file = paste0(getwd(), "/msData.json"))

ms2 = import_R6MS(file = paste0(getwd(), "/groups.json"))

ms$import_header(file)

gr = ms$get_groups()[1:100, ]
gr = gr$group
test = ms$get_groups_ms1(groups = gr, run_parallel = TRUE, verbose = FALSE)










ms$save_settings()
ms$save_analyses()
ms$save_groups()
ms$save()

file.remove(c("header.json", "settings.json", "analyses.json", "groups.json", "msData.json"))


save_R6MS(ms)

js_ms = jsonlite::fromJSON(paste0(getwd(), "/ms.json"))

js_ms$analyses = lapply(js_ms$analyses, function(x) {
  x$name = as.character(x$name)
  x$replicate = as.character(x$replicate)
  x$blank = as.character(x$blank)
  if (is.na(x$blank)) x$blank = NA_character_
  x$file = as.character(x$file)
  x$type = as.character(x$type)
  x$time_stamp = as.character(x$time_stamp)
  x$spectra_number = as.integer(x$spectra_number)
  x$spectra_mode = as.character(x$spectra_mode)
  x$spectra_levels = as.integer(x$spectra_levels)
  x$mz_low = as.numeric(x$mz_low)
  x$mz_high = as.numeric(x$mz_high)
  x$rt_start = as.numeric(x$rt_start)
  x$rt_end = as.numeric(x$rt_end)
  x$polarity = as.character(x$polarity)
  x$chromatograms_number = as.integer(x$chromatograms_number)
  x$ion_mobility = as.logical(x$ion_mobility)
  x$tic = as.data.table(x$tic)
  x$bpc = as.data.table(x$bpc)
  x$spectra = as.data.table(x$spectra)
  x$chromatograms = as.data.table(x$chromatograms)
  x$features = as.data.table(x$features)
  return(x)
})

all(unlist(lapply(js_ms$analyses, validate_list_ms_analysis)))

js_ms$alignment = as.data.table(js_ms$alignment)

js_ms$groups = lapply(js_ms$groups, function(x) {
  features_temp = as.data.frame(x[["features"]])
  x[["features"]] = NULL
  x = as.data.table(x)
  x$features = list(features_temp)
  return(x)
})

js_ms$groups = rbindlist(js_ms$feature_groups)

js_ms$groups = js_ms$feature_groups[order(js_ms$feature_groups$index), ]

head(js_ms$groups)

ms$get_groups()


head(ms$get_groups())





















ms$plot_spectra(levels = c(1, 2), mz = targets[1, ], minIntensityMS2 = 1000,
                allTraces = FALSE, colorBy = "levels")

spectra = ms$get_spectra(levels = 2, mz = targets[1, ], allTraces = FALSE)
# spectra = spectra[spectra$mz > 78 & spectra$mz < 78.1, ]
# spectra = spectra[spectra$mz > 239 & spectra$mz < 239.5, ]
# spectra = spectra[spectra$mz > 52 & spectra$mz < 52.3, ]
# spectra = spectra[spectra$mz > 159.5 & spectra$mz < 160, ]
spectra = spectra[spectra$mz > 247.15 & spectra$mz < 247.2, ]
spectra = spectra[spectra$mz > 204.1 & spectra$mz < 204.16, ]
max(spectra$mz) - min(spectra$mz)
(max(spectra$mz) - min(spectra$mz))/max(spectra$mz)*1E6



ms$get_features()

gtar = ms$get_groups(mz = targets)

ms$get_groups_ms1(groups = gtar$group)
ms$get_groups_ms1(groups = gtar$group, groupBy = "replicates")
ms$plot_groups_ms1(groups = gtar$group)
ms$plot_groups_ms1(groups = gtar$group, colorBy = "replicates")

ms$get_groups_ms2(groups = gtar$group)
ms$plot_groups_ms2(groups = gtar$group)
ms$plot_groups_ms2(groups = gtar$group, colorBy = "replicates")

ms$plot_features_ms2(id = gtar$group[1], colorBy = "analyses")

ms$plot_groups_ms2(group = gtar$group[1])

correlate_spectra(ms$get_features_ms1(id = gtar$group[1]),
                  decimals = 3,
                  minIntensity = 1000,
                  method = "pearson")

correlate_analysis_spectra(
  spectra = ms$get_groups_ms2(groups = gtar$group,
                                      groupBy = "replicates"),
  splitSpectra = TRUE,
  byReplicates = TRUE,
  decimals = 3,
  minIntensity = 200,
  method = "pearson"
)

ms$plot_features_ms1(mz = targets[1, ], mzClust = 0.01,
                     rtWindow = c(-2, 2), colorBy = "analyses")
ms$plot_features_ms2(mz = targets[2, ], mzClust = 0.01, colorBy = "analyses")


#ms$plot_features_ms1(analyses = 1, mz = targets, interactive = T)
ms$plot_groups_overview(mz = targets)

ms$plot_ms2(1, mz = targets)
ms$plot_ms1(1, mz = targets)

ftar = ms$get_features(mz = targets)
ms$get_features_ms1(id = ftar$id)
ms$get_features_ms2(id = ftar$id)

ms$plot_features(mz = targets)





ftar = ms$get_features(mz = targets)




get_ms1()
























































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
