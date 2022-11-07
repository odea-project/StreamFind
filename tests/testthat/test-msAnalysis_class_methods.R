
test_that("suggested dependencies", {

  expect_true(requireNamespace("streamFindData"))

  expect_true(requireNamespace("xcms"))

  expect_true(requireNamespace("patRoon"))

})

### resources -----------------------------------------------------------------

files <- streamFindData::msFilePaths()
db <- streamFindData::msSpikedChemicals()



### preparation ---------------------------------------------------------------

file <- files[grepl("00_hrms_s_is_pos_cent-r001.mzML", files)]

fl_mzML_pos <- files[grepl("01_ww_is_pos_blank-r001.mzML", files)]
fl_mzML_neg <- files[grepl("01_ww_is_neg_blank-r001.mzML", files)]
fl_mzXML_pos <- files[grepl("00_hrms_s_is_pos_mzxml_cent-r001", files)]
fl_mzML_profile <- files[grepl("00_hrms_s_is_pos_prof-r001.mzML", files)]
fl_mrm_pos <- files[grepl("04_ms_mrm_pos_nitrosamines_9ngml.mzML", files)]
fl_mrm_neg <- files[grepl("04_ms_mrm_neg_hormones_9ngml.mzML", files)]

ana_empty <- new("msAnalysis")

ana_mzML_pos <- newAnalysis(fl_mzML_pos)
ana_mzML_neg <- newAnalysis(fl_mzML_neg)
ana_mzXML_pos <- newAnalysis(fl_mzXML_pos)
ana_mrm_pos <- newAnalysis(fl_mrm_pos)
ana_mrm_neg <- newAnalysis(fl_mrm_neg)

metadata_entry <- "test metadata"
names(metadata_entry) <- "name test metadata"
ana_mzML_pos <- addMetadata(ana_mzML_pos, metadata = metadata_entry)



### chemical targets ----------------------------------------------------------

#Carbamazepin-d10 is ionized only in positive mode
carbamazepin_d10 <- db[name %in% "Carbamazepin-d10", .(name, mass, rt)]

#Diuron-d6 is ionized in both positive and negative modes
diuron_d6 <- db[name %in% "Diuron-d6", .(name, mass, rt)]

carb_pos <- carbamazepin_d10$mass + 1.0073
carb_rt <- carbamazepin_d10$rt

diu_pos <- diuron_d6$mass + 1.0073
diu_rt <- diuron_d6$rt

sec_dev <- 30
ppm_dev <- 20

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



### tests ---------------------------------------------------------------------

test_that("create empty msAnalysis", {
  expect_s4_class(ana_empty, "msAnalysis")
  expect_equal(unname(analysisName(ana_empty)), NA_character_)
})

test_that("msAnalysis class", {
  expect_s4_class(ana_mzML_pos, "msAnalysis")
  expect_s4_class(ana_mzML_neg, "msAnalysis")
  expect_s4_class(ana_mzXML_pos, "msAnalysis")
  expect_s4_class(ana_mrm_pos, "msAnalysis")
  expect_s4_class(ana_mrm_neg, "msAnalysis")
})

test_that("getter name and filePath", {

  expect_equal(analysisName(ana_mzML_pos),
               tools::file_path_sans_ext(basename(fl_mzML_pos)))

  expect_equal(analysisNames(ana_mzML_pos),
               tools::file_path_sans_ext(basename(fl_mzML_pos)))

  expect_equal(filePath(ana_mzML_pos), fl_mzML_pos)

  expect_equal(filePaths(ana_mzML_pos), fl_mzML_pos)

  expect_s3_class(analysisInfo(ana_mzML_pos), "data.frame")

  expect_s3_class(analysisTable(ana_mzML_pos), "data.table")

})

test_that("metadata", {

  expect_type(getMetadataNames(ana_mzML_pos), "character")

  expect_equal(getMetadata(ana_mzML_pos, which = "polarity"),
               list("polarity" = "positive"))

  expect_equal(getMetadata(ana_mzML_neg, which = "polarity"),
               list("polarity" = "negative"))

  expect_s4_class(addMetadata(ana_mzML_pos, metadata = metadata_entry,
                              overwrite = TRUE),
                  "msAnalysis")

  expect_true("name test metadata" %in% getMetadataNames(ana_mzML_pos))

  expect_equal(getMetadata(ana_mzML_pos, which = "name test metadata"),
               as.list(metadata_entry))

})

test_that("raw data parsing", {

  expect_s3_class(spectra(loadRawData(ana_mzML_pos)), "data.table")
  expect_s3_class(spectra(loadRawData(ana_mzXML_pos)), "data.table")

  expect_length(spectra(loadRawData(ana_mzML_pos)), 9)
  expect_gt(length(spectra(loadRawData(ana_mzXML_pos))), 7)

})

ana_mzML_pos <- loadRawData(ana_mzML_pos)
ana_mzXML_pos <- loadRawData(ana_mzXML_pos)
ana_mrm_pos <- loadRawData(ana_mrm_pos)

test_that("raw data checking", {

  expect_true(hasLoadedSpectra(ana_mzML_pos))
  expect_true(hasLoadedSpectra(ana_mzXML_pos))

  expect_true(hasLoadedChromatograms(ana_mzML_pos))
  expect_true(hasLoadedChromatograms(ana_mzXML_pos))
  expect_true(hasLoadedChromatograms(ana_mrm_pos))

})

test_that("extract data", {

  expect_s3_class(EICs(ana_mzML_pos, mz = targets4), "data.table")

  expect_s3_class(XICs(ana_mzML_pos, mz = targets4), "data.table")

  expect_s3_class(MS2s(ana_mzML_pos, mz = targets4), "data.table")

  expect_s3_class(BPC(ana_mzML_pos), "data.table")

  expect_s3_class(TIC(ana_mzML_pos), "data.table")

})


param <- xcms::CentWaveParam(
  ppm = 12, peakwidth = c(5, 40),
  snthresh = 5, prefilter = c(4, 800),
  mzCenterFun = "mean", integrate = 2,
  mzdiff = -0.0001, fitgauss = TRUE,
  noise = 250, verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = TRUE
)

settings_pp <- createSettings(
  call = "peakPicking",
  algorithm = "xcms3",
  parameters = param
)

test_that("settings", {

  expect_s4_class(
    createSettings(call = "peakPicking", algorithm = "xcms3",
                   parameters = param), "settings")

  expect_equal(
    getSettingsNames(
      addSettings(ana_mzML_pos, settings = settings_pp)), "peakPicking")

})

ana_mzML_pos <- addSettings(ana_mzML_pos, settings = settings_pp)

# plotSpectra(ana_mzML_pos, mz = 212.1796 + 1.0073, ppm = 20)
# plotChromatograms(ana_mzML_pos)
# plotChromatograms(ana_mzML_pos, interactive = TRUE)
# plotTIC(ana_mzML_pos)
# plotTIC(ana_mzML_pos, interactive = TRUE)
# plotBPC(ana_mzML_pos)
# plotBPC(ana_mzML_pos, interactive = TRUE)
# plotXICs(ana_mzML_pos, mz = targets4[2, ])
# plotMS2s(ana_mzML_pos, mz = targets4[2,])
# plotMS2s(ana_mzML_pos, mz = targets4[2,], interactive = TRUE)






