
### Example file paths --------------------------------------------------------------------------------------

dir1 <- "C:\\Users\\Ricardo\\Documents\\R_NTS_article\\msfiles"
files1 <- list.files(dir1, full.names = TRUE)

dir2 <- "C:\\Users\\Ricardo\\Documents\\R_DemoProject\\msfiles"
files2 <- list.files(dir2, full.names = TRUE)

fl <- files1[1:3]
files <- fl

### Test MRM files -------------------------------------------------------------

files <- streamFindData::msFilePaths()

mrm_neg <- newAnalysis(files[29])
mrm_neg <- loadRawData(mrm_neg)

chromatograms(mrm_neg)[index %in% 3]


plotTICs(mrm_neg)

plotChromatograms(mrm_neg, index = c(35), interactive = TRUE)

RaMS::grabMSdata(files[28], grab_what = "TIC")

object <- mrm_neg















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

### test negative XML ----------------------------------------------------------

files <- list.files(choose.dir(), full.names = TRUE)

set <- newStreamSet(files)


### test saturation ------------------------------------------------------------


files <- list.files(choose.dir(), full.names = TRUE)

set <- newStreamSet(files)

#getMetadata(set, which = "centroided")

#plotEICs(set, analyses = c(16), mz = c(326.232611, 242.1434, 207.1492), ppm = 20, interactive = TRUE)

#plotEICs(set, analyses = c(16), mz = c(267.0698), ppm = 20, interactive = TRUE)

#plotXICs(set, analyses = c(16), mz = c(326.232611, 242.1434, 207.1492), rt = c(956, 1126, 1151), ppm = 20, sec = 30)

plotXICs(set, analyses = c(4, 6, 8), mz = c(242.1434), rt = c(1125), ppm = 200, sec = 30)

#plotEICs(set, analyses = c(16), mz = c(242.1434, 247.1748), ppm = 20, interactive = TRUE)

plotXICs(set, analyses = c(8), mz = c(242.1434, 247.1748), rt = c(1125, 1125), ppm = 150)

### test IM --------------------------------------------------------------------

#fl <- file.choose()
#fl_2 <- file.choose()
fl_2 <- "G:\\004_RawData\\20221017_ion_mobility_data\\centroid\\WorklistData-0001.mzML"

#a1 <- newAnalysis(fl)
acent <- newAnalysis(fl_2)

#plotTIC(a1)
#getMetadata(a1)

#plotTIC(acent)
getMetadata(acent)

#a1 <- loadRawData(a1)
acent <- loadRawData(acent)


#head(spectra(acent))


dt_spec <- data.table::copy(spectra(acent))
dt_spec <- dt_spec[rt >= 550 & rt <= 650, ] #& mz >= 261.0269 & mz <= 261.0373
dt_spec <- dt_spec[mz >= 255 & mz <= 270, ]
dt_spec <- dt_spec[driftTime >= 15 & driftTime <= 20, ]


dt_heat <- dt_spec
dt_heat <- dt_heat[, .(rt, driftTime, intensity)]
#dt_heat$intensity <- log(dt_heat$intensity)
dt_heat <- dt_heat[, .(intensity = sum(intensity)), by = c("rt", "driftTime")]
dt_heat <- tidyr::complete(dt_heat, rt, tidyr::nesting(driftTime), fill = list(intensity = 0))
dt_heat <- tidyr::pivot_wider(dt_heat, names_from = rt, values_from = intensity)
dt_heat_mat <- as.matrix(dt_heat)
row.names(dt_heat_mat) <- dt_heat_mat[, 1]
dt_heat_mat <- dt_heat_mat[, -1]

plot <- plotly::plot_ly(x = colnames(dt_heat_mat), y = rownames(dt_heat_mat),
                z = dt_heat_mat, colors = "RdYlBu", type = "heatmap",
                colorbar = list(title = '<b> Intensity </b>'),
                reversescale = TRUE)

xaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = "Retention Time / seconds",
              titlefont = list(size = 12, color = "black"))

yaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = "Drift Time / miliseconds",
              titlefont = list(size = 12, color = "black"))

plot <- plot %>% layout(
  xaxis = xaxis,
  yaxis = yaxis
)

plot



dt_heat <- dt_spec
dt_heat <- dt_heat[, .(mz, driftTime, intensity)]
#dt_heat$intensity <- log(dt_heat$intensity)
dt_heat$mz <- round(dt_heat$mz, digits = 1)
dt_heat <- dt_heat[, .(intensity = sum(intensity)), by = c("mz", "driftTime")]
dt_heat <- tidyr::complete(dt_heat, mz, tidyr::nesting(driftTime), fill = list(intensity = 0))
dt_heat <- tidyr::pivot_wider(dt_heat, names_from = mz, values_from = intensity)
dt_heat_mat <- as.matrix(dt_heat)
row.names(dt_heat_mat) <- dt_heat_mat[, 1]
dt_heat_mat <- dt_heat_mat[, -1]

plot <- plotly::plot_ly(x = as.numeric(colnames(dt_heat_mat)), y = as.numeric(rownames(dt_heat_mat)),
                        z = dt_heat_mat, colors = "RdYlBu", type = "heatmap",
                        colorbar = list(title = '<b> Intensity </b>'),
                        reversescale = TRUE)

xaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = '<i> m/z </i>',
              titlefont = list(size = 12, color = "black"))

yaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = "Drift Time / miliseconds",
              titlefont = list(size = 12, color = "black"))

plot <- plot %>% layout(
  #legend = list(title = list(text='<b> Intensity </b>')),
  xaxis = xaxis,
  yaxis = yaxis
)

plot



plotEICs(a1, mz = 260.0248201 + 1.0073, rt = 600, ppm = 20, sec = 120, interactive = TRUE)


#fl_3 <- file.choose()
fl_3 <- "G:\\004_RawData\\20221017_ion_mobility_data\\centroid\\mix1.mzML"

amix1 <- newAnalysis(fl_3)

getMetadata(amix1)

amix1 <- loadRawData(amix1)

tic_amix1 <- TIC(amix1)
tic_amix1$rt <- tic_amix1$rt*60
tic_amix1$analysis <- analysisName(amix1)
plotTICs(tic_amix1)

chrom <- chromatograms(amix1)


plotTIC(amix1)
head(spectra(amix1))



dt_spec <- data.table::copy(spectra(amix1))
dt_spec <- dt_spec[rt >= 190 & rt <= 200, ]
dt_spec <- dt_spec[driftTime >= 10 & driftTime <= 25, ]
dt_spec <- dt_spec[ mz >= 0 & mz <= 280, ]
dt_spec[is.na(ce), ce := 0]
dt_spec <- dt_spec[ce > 10, ]

# plotSpectra(amix1, mz = data.frame(
#   mzmin = 0,
#   mzmax = 280,
#   rtmin = 190,
#   rtmax = 200
# ))



dt_heat <- dt_spec
dt_heat <- dt_heat[, .(mz, driftTime, intensity)]
dt_heat$mz <- round(dt_heat$mz, digits = 4)
#dt_heat$intensity <- log(dt_heat$intensity)
dt_heat <- dt_heat[, .(intensity = sum(intensity)), by = c("mz", "driftTime")]
dt_heat <- tidyr::complete(dt_heat, mz, tidyr::nesting(driftTime), fill = list(intensity = 0))
dt_heat <- tidyr::pivot_wider(dt_heat, names_from = mz, values_from = intensity)
dt_heat_mat <- as.matrix(dt_heat)
row.names(dt_heat_mat) <- dt_heat_mat[, 1]
dt_heat_mat <- dt_heat_mat[, -1]

fig <- plotly::plot_ly(x = as.numeric(colnames(dt_heat_mat)), y = as.numeric(rownames(dt_heat_mat)),
                       z = dt_heat_mat) %>% plotly::add_surface(colors = "RdYlBu", reversescale = TRUE, showscale = FALSE,)

xaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = '<i> m/z </i>',
              titlefont = list(size = 12, color = "black"))

yaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = "Drift Time / miliseconds",
              titlefont = list(size = 12, color = "black"))

zaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = "Intensity / counts",
              titlefont = list(size = 12, color = "black"))

fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = '<i> m/z </i>'),
    yaxis = list(title = "Drift Time / miliseconds"),
    zaxis = list(title = "Intensity / counts")
  )
)

fig


plot <- plotly::plot_ly(x = as.numeric(colnames(dt_heat_mat)), y = as.numeric(rownames(dt_heat_mat)),
                        z = dt_heat_mat, colors = "RdYlBu", type = "heatmap",
                        #colorbar = list(title = '<b> Intensity </b>'),
                        showscale = FALSE,
                        reversescale = TRUE)

xaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = '<i> m/z </i>',
              titlefont = list(size = 12, color = "black"))

yaxis <- list(linecolor = toRGB("black"),
              linewidth = 2, title = "Drift Time / miliseconds",
              titlefont = list(size = 12, color = "black"))

plot <- plot %>% layout(
  #legend = list(title = list(text='<b> Intensity </b>')),
  xaxis = xaxis,
  yaxis = yaxis
)

plot











init_time <- Sys.time()

xml <- xml2::read_xml(fl_3)

x_path <- '//d1:spectrum'
x_path <- '//d1:spectrum/d1:scanList/d1:scan/d1:cvParam'
x_path <- '//d1:spectrum/d1:scanList/d1:scan/d1:cvParam[@name="ion mobility drift time"]'
dt <- xml2::xml_find_all(xml, x_path)

x_path_2 <- '//d1:scanList/d1:scan'
dt_2 <- xml2::xml_find_all(dt[[1]], x_path_2)
dt_2[[10000]]


unit <- xml_attr(dt[[1]], "unitName")
dt_vals <- as.numeric(xml_attr(dt, "value"))
rm(xml)
rm(dt)
rm(x_path)
gc()
final_time <- Sys.time()




xml[4]


fl <- fl_3


hd <- loadBasicRawSpectraHeaderMZR(fl)

init_time <- Sys.time()
ms <- mzR::openMSfile(fl_3)
dh <- mzR::header(ms)

dh <- dh[dh$precursorMZ >= 273.12 & dh$precursorMZ <= 273.13, ]

TRUE %in% (dh$precursorMZ >= 273.12 & dh$precursorMZ <= 273.13)

class(dh$precursorMZ)



mzR::close(ms)
rm(ms)
gc()
final_time <- Sys.time()

final_time - init_time




