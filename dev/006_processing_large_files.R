
# load files
files <- list.files(choose.dir(), full.names = TRUE)

fls <- files[1:3]
fl <- files[2]

# a1 <- newStreamSet(file = files[1])
# a1 <- loadRawData(a1)

msd1 <- new("msData", files = fls)
a1 <- newAnalysis(file = fl)

settings_pp <- createSettings(
  call = "peakPicking",
  algorithm = "xcms3",
  parameters = list(xcms::CentWaveParam(
    ppm = 12, peakwidth = c(5, 40),
    snthresh = 8, prefilter = c(4, 1000),
    mzCenterFun = "mean", integrate = 1,
    mzdiff = -0.0001, fitgauss = TRUE,
    noise = 250, verboseColumns = TRUE,
    firstBaselineCheck = TRUE,
    extendLengthMSW = TRUE
  ))
)

a1 <- peakPicking(a1, settings_pp)

msd1 <- peakPicking(msd1, settings_pp)


EICs(a1, mz = peaks(a1)[1:200, ])

EICs(msd1, mz = peaks(msd1)[1:200, ])

object <- a1
object <- msd1

mz <- peaks(msd1)
mz <- mz[, .(id, mz, rt, mzmin, mzmax, rtmin, rtmax)]

plotPeaks(a1, targetsID = peaks(a1)[1:20, ]$id)

plotPeaks(msd1, analyses = 3, targetsID = peaks(msd1)[1:20, ]$id)


test <- spectra(a1)







# targets <- targets[1:100, ]
#
# if (run_parallel) {
#
#   workers <- availableCores() - 1
#
#   if (nrow(targets) > workers) {
#
#     targets$cl <- ceiling(seq_along(targets$id)/ceiling(nrow(targets)/workers))
#     targets <- split(targets, factor(targets$cl))
#
#   } else {
#
#     run_parallel = FALSE
#     targets <- list(targets)
#
#   }
#
# } else {
#
#   targets <- list(targets)
#
# }

streamFind:::mzML_loadMetadata(fl)

streamFind:::mzML_loadMetadata(fl)



library(streamFindData)


files <- msFilePaths()



