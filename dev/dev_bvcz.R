path <- "C:/Users/apoli/Documents/iSoft/240819_BVCZ"
files <- list.files(path = path, pattern = ".d$|.mzML$", full.names = TRUE)

#clear_cache("all")

# Quantification

# ms <- StreamFind::MassSpecEngine$new(analyses = files, centroid = TRUE, levels = 1)
# ms$analyses$blanks <- rep("Blank", length(ms$analyses))
# run(MassSpecMethod_LoadChromatograms_StreamFind(chromatograms = "DAD1 A: Sig=214,4  Ref=off", rtmin = 250, rtmax = 400), engine = ms)
# run(MassSpecMethod_SmoothChromatograms_movingaverage(windowSize = 3), engine = ms)
# run(MassSpecMethod_CorrectChromatogramsBaseline_baseline_als(lambda = 5, p = 0.05, maxit = 10), engine = ms)
# 
# ms$plot_chromatograms_baseline(colorBy = "analyses+targets")
# 
# run(
#   MassSpecMethod_IntegrateChromatograms_StreamFind(
#     merge = TRUE,
#     closeByThreshold = 5,
#     minPeakHeight = 10,
#     minPeakDistance = 3,
#     minPeakWidth = 5,
#     maxPeakWidth = 50,
#     minSN = 1
#   ),
#   engine = ms
# )
# 
# run(MassSpecMethod_QuantifyChromatographicPeaks_StreamFind(), engine = ms)
# 
# ms$plot_chromatograms_peaks(colorBy = "analyses+targets")
# 
# ms$get_chromatograms_peaks()


# Identification

ms2 <- StreamFind::MassSpecEngine$new(analyses = files[grepl("67_BVCZ|QC_BVCZ", files)], centroid = FALSE, levels = 1)

# ms2$plot_spectra_tic(downsize = 3)

# ms2$plot_spectra_ms1(
#   analyses = 6,
#   rt = 345,
#   sec = 5,
#   colorBy = "analyses",
#   presence = 0.1,
#   minIntensity = 1000,
#   interactive = FALSE
# )

# ms2$plot_spectra_xic(
#   analyses = 1,
#   rt = 345,
#   sec = 5,
#   mz = 3110,
#   ppm = 200/3110*1E6
# )

run(
  MassSpecMethod_LoadSpectra_StreamFind(
    mzmin = 2500, #2500,
    mzmax = 3700, #3700,
    rtmin = 346,
    rtmax = 347.5,
    levels = 1
  ),
  engine = ms2
)

run(
  MassSpecMethod_ClusterSpectra_StreamFind(
    val = "mz",
    clustVal = 0.001,
    presence = 0.1
  ),
  engine = ms2
)

# #ms2$plot_spectra()

run(
  MassSpecMethod_CalculateSpectraCharges_StreamFind(
    roundVal = 15,
    relLowCut = 0.2,
    absLowCut = 8000
  ),
  engine = ms2
)

#ms2$plot_spectra_charges()

run(MassSpecMethod_DeconvoluteSpectra_StreamFind(clustVal = 0.001, window = 25),engine = ms2)
 
run(
  MassSpecMethod_SmoothSpectra_movingaverage(
    windowSize = 20
  ),
  engine = ms2
)

run(
  MassSpecMethod_FindSpectraMaxima_StreamFind(
    minWidth = 3,
    maxWidth = 45,
    minHeight = 10000
  ),
  engine = ms2
)

plot_spectra_peaks(ms2$analyses, colorBy = "analyses+targets")

get_spectra_peaks(ms2$analyses)

# 
# ms2$plot_spectra()
# 
run(
  MassSpecMethod_CorrectSpectraBaseline_baseline_als(
    lambda = 9,
    p = 0.02,
    maxit = 10
  ),
  engine = ms2
)

# plot_spectra_baseline(ms2$analyses)
# 
# ms2$plot_spectra()

run(
  MassSpecMethod_IntegrateSpectra_StreamFind(
    merge = TRUE,
    closeByThreshold = 5,
    minPeakHeight = 4000,
    minPeakDistance = 1,
    minPeakWidth = 10,
    maxPeakWidth = 500,
    minSN = 1
  ),
  engine = ms2
)
# 
# plot_spectra_peaks(ms2$analyses, colorBy = "analyses+targets")
# 
# get_spectra_peaks(ms2$analyses)



# 
# run(
#   MassSpecMethod_CalculateSpectraCharges_StreamFind(
#     roundVal = 15,
#     relLowCut = 0.2,
#     absLowCut = 8000
#   ),
#   engine = ms
# )







# library(magrittr)
# 
# dt_pos <- data.table::fread("kjell_pos.csv")
# dt_pos_rp <- dt_pos[, c(1:2)]
# colnames(dt_pos_rp) <- c("rt", "logD")
# dt_pos_rp$phase <- "RP"
# dt_pos_hilic <- dt_pos[, c(3:4)]
# dt_pos_hilic <- dt_pos_hilic[!is.na(dt_pos_hilic[["logD_(pH3)_HILIC"]])]
# colnames(dt_pos_hilic) <- c("rt", "logD")
# dt_pos_hilic$phase <- "HILIC"
# dt_pos <- rbind(dt_pos_rp, dt_pos_hilic)
# dt_pos$polarity <- "Positive"
# 
# dt_neg <- data.table::fread("kjell_neg.csv")
# dt_neg_rp <- dt_neg[, c(1:2)]
# colnames(dt_neg_rp) <- c("rt", "logD")
# dt_neg_rp$phase <- "RP"
# dt_neg_hilic <- dt_neg[, c(3:4)]
# dt_neg_hilic <- dt_neg_hilic[!is.na(dt_neg_hilic[["logD_(pH3)_HILIC"]])]
# colnames(dt_neg_hilic) <- c("rt", "logD")
# dt_neg_hilic$phase <- "HILIC"
# dt_neg <- rbind(dt_neg_rp, dt_neg_hilic)
# dt_neg$polarity <- "Negative"
# 
# dt <- rbind(dt_pos, dt_neg)
# dt$key <- paste0(dt$phase, "_", dt$polarity)
# 
# dt$key <- factor(dt$key, levels = c("RP_Positive", "RP_Negative", "HILIC_Positive", "HILIC_Negative"))
# 
# fig <- plotly::plot_ly(dt, y = ~logD, color = ~key, type = "box", colors = c("darkgreen", "green", "darkblue", "blue")) #boxpoints = "all", jitter = 0.3, pointpos = -1.8
# 
# # save the plot as png with 300 ppi
# plotly::orca(fig, file = "boxplot_kjell.png", width = 1000, height = 800, scale = 2)

