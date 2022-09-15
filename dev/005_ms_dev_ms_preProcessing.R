
### Example file paths --------------------------------------------------------------------------------------

dir1 <- "C:\\Users\\Ricardo\\Documents\\R_NTS_article\\msfiles"
files1 <- list.files(dir1, full.names = TRUE)

dir2 <- "C:\\Users\\Ricardo\\Documents\\R_DemoProject\\msfiles"
files2 <- list.files(dir2, full.names = TRUE)

fl <- files1[1:3]
files <- fl


### RaMS ---------------------

library(RaMS)

fl <- choose.files()

fl <- c(
  "C:\\Users\\Ricardo\\Documents\\R_DemoProject\\msfiles\\09_Sample_Sciex_MRM_Chromatograms_Nitrosamines_10ngml.mzML"
)

fl <- c(
  "C:\\Users\\Ricardo\\Documents\\R_DemoProject\\msfiles\\10_Sample_Sciex_MRM_Spectra_Nitrosamines_10ngml.mzML"
)



test <- RaMS::grabMSdata(fl, grab_what = c("metadata", "MS1"))
test$metadata$config_data

rm(test)

test <- xml2::read_xml(fl[1])
init_node <- xml2::xml_find_all(test, xpath = "//d1:spectrum")

xml2::xml_ns(test)


test <- xml2::read_xml(files[4])
init_node <- xml2::xml_find_all(test, xpath = '//d1:spectrum/d1:cvParam[@name = "profile spectrum"]')
head(xml2::xml_attr(init_node, "name"))

init_node <- xml2::xml_find_all(test, xpath = '//d1:spectrum[d1:cvParam[@accession="MS:1000128"]]')

test2 <- xml2::as_list(init_node)

xml2::xml_structure(test, indent = 1)


test$indexedmzML$mzML$run$chromatogramList[[3]]$binaryDataArrayList

test <- grabMSdata(file = fl[1], grab_what = "TIC")

msdata <- grabMSdata(files = fl[2], grab_what = "MS1") #rtrange = c(1100/60, 1200/60)

data_nodes <- xml2::xml_find_all(files[1])

#### metadata entries mzR ---------


# $polarity
# [1] "positive"
#
# $centroided
# [1] TRUE
#
# $scanCount
# [1] 1886
#
# $lowMz
# [1] 0
#
# $highMz
# [1] 449.9957
#
# $dStartTime
# [1] 900.093
#
# $dEndTime
# [1] 1349.886
#
# $msLevels
# [1] 1 2
#
# $startTimeStamp
# [1] "2022-06-17T14:45:28Z"
#
# $manufacturer
# [1] "instrument model"
#
# $model
# [1] "Agilent instrument model"
#
# $ionisation
# [1] "microelectrospray"
#
# $analyzer
# [1] "quadrupole"
#
# $detector
# [1] "microchannel plate detector"
#
# $software
# [1] "MassHunter 8.0"
#
# $sample
# [1] ""
#
# $source
# [1] ""

#### cols header mrR ------

# [1] "seqNum"                     "acquisitionNum"             "msLevel"                    "polarity"
# [5] "peaksCount"                 "totIonCurrent"              "retentionTime"              "basePeakMZ"
# [9] "basePeakIntensity"          "collisionEnergy"            "ionisationEnergy"           "lowMZ"
# [13] "highMZ"                     "precursorScanNum"           "precursorMZ"                "precursorCharge"
# [17] "precursorIntensity"         "mergedScan"                 "mergedResultScanNum"        "mergedResultStartScanNum"
# [21] "mergedResultEndScanNum"     "injectionTime"              "filterString"               "spectrumId"
# [25] "centroided"                 "ionMobilityDriftTime"       "isolationWindowTargetMZ"    "isolationWindowLowerOffset"
# [29] "isolationWindowUpperOffset" "scanWindowLowerLimit"       "scanWindowUpperLimit"


metadata(a1)


targets4



files


fl <- files[1]



test <- mzR::openMSfile(fl[1])

test_chrom <- chromatogramHeader(test)
test_chrom <- chromatograms(test)

test <- mzR::close(test)


### Classes Hierarchy ---------------------------------------------------------------------------------------

test <- new("msAnalysis", file = fl[1])




test1 <- new("msData")

test1@analyses <- list(
  a = new("msAnalysis", file = files1[1]),
  b = new("msAnalysis", file = files1[1])
)

analyses(test1)

test2 <- new("msmsData", test1)

is(test1)










sapply(test1@samples, function(x) x@replicate)

rpl <- rep("control", 2)
names(rpl) <- sapply(test1@samples, function(x) x@name)



test1@samples <- lapply(test1@samples, function(x, rpl) {

  rpl_n <- NA

  if (x@name %in% names(rpl)) {
    rpl_n <- rpl[x@name]
  }

  if (!is.na(rpl_n)) x@replicate <- rpl_n

  return(x)

}, rpl = rpl)

sp1 <- test1@samples[[1]]
mt <- sp1@otherInfo






summary(lapply(test1@samples, function(x) x@scans))

test

msd <- newStreamProject(files = fl)

is(msd)

sampleNames(msd)

data.table::rbindlist(msd@samples[[1]])



### Test Shimadzu mzXML errors ------------------------------------------------------------------------------

file <- file.choose()

ana <- newAnalysis(file)
ana <- loadRawData(ana)

ana_rams <- RaMS::grabMSdata(file)

db_2 <- db[,c(1:3, 24:27)]

TICs(ana)
plotTICs(ana, interactive = TRUE)

target <- makeTargets(mz = data.frame(mzmin = 232.5, mzmax = 233.4))
plotEICs(ana, mz = target)

traces <- spectra(ana)

param <- xcms::CentWaveParam(
  ppm = 500, peakwidth = c(2, 40),
  snthresh = 3, prefilter = c(4, 800),
  mzCenterFun = "mean", integrate = 2,
  mzdiff = -0.0001, fitgauss = TRUE,
  noise = 250, verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = TRUE
)

#creating the settings S4 class for peak picking
settings_pp <- createSettings(
  call = "peakPicking",
  algorithm = "xcms3",
  settings = param
)

ana <- peakPicking(ana, settings = settings_pp)

p_ana <- peaks(ana)

plotPeaks(ana, targetsID = "m329.05_d0_r753_t19_p867")





