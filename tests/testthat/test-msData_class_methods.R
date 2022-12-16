test_that("suggested dependencies", {

  expect_true(requireNamespace("streamFindData"))

  expect_true(requireNamespace("xcms"))

  expect_true(requireNamespace("patRoon"))

})

### resources -----------------------------------------------------------------

files <- streamFindData::msFilePaths()
db <- streamFindData::msSpikedChemicals()



### preparation ---------------------------------------------------------------

file_set <- files[grepl("influent|blank", files)]

file_set_2 <- files[grepl("o3sw", files)]


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

md_empty <- new("msData")

test_that("create empty msData", {
  expect_s4_class(md_empty, "msData")
  expect_type(analysisNames(md_empty), "list")
})

md_set <- new("msData",
              files = file_set,
              title = "example",
              run_parallel = TRUE)

test_that("getter for names and filePaths", {

  expect_equal(unname(analysisNames(md_set)),
               tools::file_path_sans_ext(basename(file_set)))

  expect_equal(unname(filePaths(md_set)), file_set)

  expect_s3_class(analysisInfo(md_set), "data.frame")

  expect_s3_class(analysisTable(md_set), "data.table")

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

replicateNames(md_set) <- rpl

blankReplicateNames(md_set) <- blk

test_that("test setter and getter for replicates and blanks", {

  expect_equal(unname(replicateNames(md_set)), rpl)

  expect_equal(unname(blankReplicateNames(md_set)), blk)

  expect_equal(names(replicateNames(md_set)), unname(analysisNames(md_set)))

  expect_equal(names(blankReplicateNames(md_set)), unname(analysisNames(md_set)))

})

# add metadata as a named vector

meta_1 <- c("water", "filtering")
names(meta_1) <- c("origin", "usage")

meta_2 <- c("wastewater", "processing")
names(meta_2) <- c("origin", "usage")

meta_vec_list <- c(rep(list(meta_1), 6), rep(list(meta_2), 6))

#optional, but when numbered list the order is taken as is
names(meta_vec_list) <- analysisNames(md_set)

md_set <- addMetadata(md_set, metadata = meta_vec_list)

test_that("metadata", {

  expect_type(getMetadataNames(md_set, simplify = TRUE), "character")

  expect_equal(getMetadata(md_set, which = "polarity"),
     data.table("analysis" = analysisNames(md_set),
                "polarity" = rep(c(rep("negative", 3), rep("positive", 3)), 2)
  ))

  expect_s4_class(addMetadata(md_set, metadata = meta_vec_list,
                              overwrite = TRUE), "msData")

  expect_true("origin" %in% getMetadataNames(md_set, simplify = TRUE))

  expect_equal(unname(polarities(md_set)),
               rep(c(rep("negative", 3), rep("positive", 3)), 2)
  )

})

# as a data.frame (would also work with data.table)

meta_df <- data.frame(
  "analysis" = analysisNames(md_set),
  "origin" = c(rep("water", 6), rep("wastewater", 6)),
  "usage" = c(rep("filtering", 6), rep("processing", 6)),
  "test_df" = rep("test", 12)
)

# Note, the analysis column doesn't need to be added
# but the order of the rows will be used as is.
# when names of the analyses are in analysis column
# that order is taken to match the analysis in the msData object.

#add overwrite to TRUE, as the metadata name is already in the analyses of msData
md_set <- addMetadata(md_set, metadata = meta_df, overwrite = TRUE)

test_that("metadata added as df", {

  expect_true("test_df" %in% getMetadataNames(md_set, simplify = TRUE))

})

md_set_2 <- new("msData",
                files = file_set_2,
                title = "extra samples",
                run_parallel = TRUE)

test_that("gets, adds and subsets msAnalysis in msData", {

  expect_s4_class(getAnalyses(md_set, analyses = 1), "msAnalysis")

  expect_s4_class(
    getAnalyses(md_set, analyses = "01_ww_is_neg_blank-r002"), "msAnalysis"
  )

  expect_length(analysisNames(addAnalyses(
    md_set,
    analysisList = md_set_2@analyses,
    replicates = c(rep("ozonation_neg", 3), rep("ozonation_pos", 3)),
    blanks = c(rep("blank_neg", 3), rep("blank_pos", 3))
  )), 18)

  expect_length(analysisNames(md_set[1:2]), 2)

  expect_length(replicateNames(md_set[1:2]), 2)

})

test_that("raw data parsing", {

  expect_length(spectra(loadRawData(md_set_2)), 10)

})


md_set_2 <- loadRawData(md_set_2, run_parallel = FALSE)


test_that("raw data checking", {

  expect_true(all(hasLoadedSpectra(md_set_2)))
  expect_true(all(hasLoadedChromatograms(md_set_2)))

})

test_that("extract data", {

  expect_s3_class(EICs(md_set_2, mz = targets4), "data.table")

  expect_s3_class(XICs(md_set_2, mz = targets4), "data.table")

  expect_s3_class(MS2s(md_set_2, mz = targets4), "data.table")

  expect_s3_class(BPCs(md_set_2), "data.table")

  expect_s3_class(TICs(md_set_2), "data.table")

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

md_set <- addSettings(md_set, settings = settings_pp, where = "analyses")

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


# TODO make a plot of processing time for parallel and sequential
# When does parallel processing makes a difference?

# start_t <- Sys.time()
#
# test <- getRawData(md_set, run_parallel = TRUE)
#
# Sys.time() - start_t
#
# start_t <- Sys.time()
#
# test <- getRawData(md_set, run_parallel = FALSE)
#
# Sys.time() - start_t






