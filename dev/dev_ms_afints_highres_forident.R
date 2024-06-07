
#path <- "F:/example_ms_files"
path <- "C:/Users/apoli/Documents/example_ms_files"




# resources --------------------------------------------------------------------
cols <- c("name", "formula", "mz", "rt")

tof_db <- paste0(path, "/qc_MS2_pos.csv")
tof_db <- data.table::fread(tof_db)
tof_db <- tof_db[, cols, with = FALSE]

afin_db <- paste0(path, "/Composition_Mix-Fusion.csv")
afin_db <- data.table::fread(afin_db)
afin_db <- afin_db[, cols, with = FALSE]

ude_db <- paste0(path, "/mix1_orbitrap_ude.csv")
ude_db <- data.table::fread(ude_db)
ude_db <- ude_db[, cols, with = FALSE]

files <- list.files(path, pattern = ".mzML", full.names = TRUE)

# Settings for annotation of isotopes
afs <- Settings_annotate_features_StreamFind(
  maxIsotopes = 5,
  elements = c("C", "H", "N", "O", "S", "Cl", "Br"),
  mode = "small molecules",
  maxCharge = 1,
  rtWindowAlignment = 0.5,
  maxGaps = 1
)




# tof --------------------------------------------------------------------------

tof_fl <- files[41]

tof_ffs <- Settings_find_features_xcms3_centwave(
  ppm = 12,
  peakwidth = c(5, 40),
  snthresh = 5,
  prefilter = c(5, 1500),
  mzCenterFun = "wMean",
  integrate = 2,
  mzdiff = 0.0005,
  fitgauss = TRUE,
  noise = 500,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = FALSE
)

tof_ms <- MassSpecEngine$new(files = tof_fl, headers = list(name = "tof"))

tof_ms$plot_spectra_eic(mz = tof_db, ppm = 20, sec = 30)




# afin --------------------------------------------------------------------------

afin_fl <- files[29]

afin_ffs <- Settings_find_features_xcms3_centwave(
  ppm = 3,
  peakwidth = c(5, 80),
  snthresh = 10,
  prefilter = c(6, 150000),
  mzCenterFun = "wMean",
  integrate = 1,
  mzdiff = 0.0002,
  fitgauss = TRUE,
  noise = 50000,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = FALSE
)

afin_ms <- MassSpecEngine$new(files = afin_fl, headers = list(name = "afin"))

afin_ms$plot_spectra_eic(mz = afin_db[!is.na(afin_db$rt), ], ppm = 10, sec = 30)




# ude --------------------------------------------------------------------------

ude_fl <- files[32]

ude_ffs <- Settings_find_features_xcms3_centwave(
  ppm = 4,
  peakwidth = c(5, 80),
  snthresh = 10,
  prefilter = c(6, 50000),
  mzCenterFun = "wMean",
  integrate = 2,
  mzdiff = 0.0002,
  fitgauss = TRUE,
  noise = 50000 / 3,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = FALSE
)

ude_ms <- MassSpecEngine$new(files = ude_fl, headers = list(name = "ude"))

ude_ms$plot_spectra_eic(mz = ude_db, ppm = 5, sec = 30)



# xic comparison tof vs orbitrap ----------------------------------------------

toforb <- c(tof_fl, ude_fl)

toforb_ms <- MassSpecEngine$new(files = toforb, headers = list(name = "toforb"))

meto_db <- rbind(tof_db[tof_db$name %in% "Metoprolol", ], ude_db[ude_db$name %in% "Metoprolol", ])

toforb_ms$plot_spectra_xic(mz = meto_db, ppm = 20, sec = 240)



# forident showcase ------------------------------------------------------------

suspects_ms <- tof_ms$subset_features(features = tof_suspects)

slfms2 <- Settings_load_features_ms2_StreamFind(
  isolationWindow = 1.3,
  mzClust = 0.008,
  presence = 0.9,
  minIntensity = 200
)

suspects_ms$load_features_ms2(slfms2)

suspects_ms$plot_features_ms2(loadedMS2 = TRUE, colorBy = "targets")

sssfi <- Settings_suspect_screening_forident(
  addMS2 = TRUE,
  useNeutralMass = FALSE,
  path = getwd(),
  name = "feature_list_forident"
)

suspects_ms$suspect_screening(sssfi)

file.remove("feature_list_forident.txt")



samples <- seq_len(100)
intensities <- c(sample(10000:15000, 99), 6000)
intensities_av <- mean(intensities)
intensities_sd <- sd(intensities)


z_scores <- (intensities - intensities_av) / intensities_sd

z_score_lower <- -3

z_score_upper <- 3

outliers_zscore <- intensities[abs(z_scores) > 3]

fig <- plot_ly(x = samples, showlegend = FALSE)

# Add intensity markers
fig <- fig %>% add_trace(x = samples, y = intensities, type = "scatter", mode = "markers", marker = list(size = 10), name = "Intensity")

# Add Z-score line
fig <- fig %>% add_trace(x = samples, y = z_scores, type = "scatter", mode = "lines", line = list(color = "black", width = 2, dash = "dash"), name = "Z-Score", yaxis = "y2")

# Add Z-score lower limit line
fig <- fig %>% add_trace(x = samples, y = rep(z_score_lower, length(samples)), type = "scatter", mode = "lines", line = list(color = "darkred", width = 2), name = "Lower Limit", yaxis = "y2")

# Add Z-score upper limit line
fig <- fig %>% add_trace(x = samples, y = rep(z_score_upper, length(samples)), type = "scatter", mode = "lines", line = list(color = "orange", width = 2), name = "Upper Limit", yaxis = "y2")

# Add layout with secondary y-axis
fig <- fig %>% layout(
  title = "",
  xaxis = list(title = "Samples"),
  yaxis = list(title = "TNFF"),
  yaxis2 = list(
    title = "Z",
    overlaying = "y",
    side = "right",
    automargin = TRUE
  ),
  margin = list(l = 50, r = 70)
)

fig



Q1 <- quantile(intensities, 0.30)
Q3 <- quantile(intensities, 0.70)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers_iqr <- intensities[intensities < lower_bound | intensities > upper_bound]

fig <- plot_ly(x = samples, showlegend = FALSE)

fig <- fig %>% add_trace(x = samples, y = intensities, type = "scatter", mode = "markers", marker = list(size = 10))

# Add a horizantal line with upper and lower limits based on the standard deviation

fig <- fig %>% add_trace(y = rep(lower_bound, length(samples)), type = "scatter", mode = "lines", line = list(color = "darkred", width = 2), name = "Limit")

fig <- fig %>% add_trace(y = rep(upper_bound, length(samples)), type = "scatter", mode = "lines", line = list(color = "orange", width = 2), name = "Limit")

fig <-  fig %>% layout(title = "", xaxis = list(title = "Samples"), yaxis = list(title = "TNFF"))

fig






