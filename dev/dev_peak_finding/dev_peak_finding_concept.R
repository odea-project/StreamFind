# MARK: Resources
# Resources -------------------------------------------------------------------
source("dev/dev_peak_finding/dev_resources.R")
source("dev/dev_peak_finding/dev_plots.R")

# MARK Setup
# Setup -----------------------------------------------------------------------
devtools::load_all()
ms <- MassSpecEngine$new(analyses = files)

# plot_spectra_tic(ms$Analyses, analyses = c(2:3), levels = 1, downsize = 3)
# plot_spectra_eic(ms$Analyses, analyses = c(2:3), mz = db_merck, ppm = 10, sec = 60, colorBy = "targets", legendNames = TRUE)

# MARK: Get Spectra
# Get Spectra ------------------------------------------------------------------

# TOF targets
tof_target <- data.frame(
  mzmin = 200,
  mzmax = 250,
  rtmin = 0,
  rtmax = 2000
)

# ORBITRAP targets
orb_target <- data.frame(
  mzmin = 200,
  mzmax = 350,
  rtmin = 0,
  rtmax = 2000
)

single_target <- data.frame(
  mzmin = 700,
  mzmax = 750,
  rtmin = 750,
  rtmax = 820
)

spl <- get_raw_spectra(
  ms$Analyses,
  analyses = 3,
  levels = 1
)

# MARK: Peak Finding Concept
# Peak Finding Concept ---------------------------------------------------------

# Parameters for TOF data
parameters <- list(
  rtWindows = data.frame(rtmin = 300, rtmax = 3600),
  noiseBins = 70,
  noiseThreshold = 15,
  noiseQuantile = 0.01,
  minSNR = 3,
  mzrThreshold = 0.005,
  minTraces = 3,
  baselineWindow = 200,
  maxWidth = 100,
  minGaussFit = 0.7
)

# Parameters for Orbitrap data
# parameters <- list(
#   noise = 100000,
#   mzr = 0.002,
#   sn = 3,
#   gaufit = 0.5
# )

# MARK: Restricting m/z range for speed
#spec <- spec[spec$mz > 200 & spec$mz < 250, ]
#spec <- spec[spec$mz > tof_target$mzmin & spec$mz < tof_target$mzmax, ]



# MARK: Splitting by rt
spl <- spl[order(spl$rt), ]
spl <- split(spl, f = spl$rt)
spl <- lapply(spl, function(x) {
  x <- x[x$intensity > 0, ]
  x
})
message("Total rt scans: ", length(spl))

spl <- lapply(spl, function(x, rtWindows) {
  if (nrow(rtWindows) == 0) return(x)
  sel <- FALSE
  for (i in seq_len(nrow(rtWindows))) {
    sel <- sel | (x$rt[1] >= rtWindows$rtmin[i] & x$rt[1] <= rtWindows$rtmax[i])
  }
  if (any(sel)) {
    x
  } else {
    NULL
  }
}, rtWindows = parameters$rtWindows)
spl <- spl[!vapply(spl, is.null, FALSE)]

#plot_3D_spec_list(spl, c(1:100))

source("dev/dev_peak_finding/dev_clean_spectra.R")
spl_clean <- clean_spectra(
  spl,
  noiseBins = parameters$noiseBins,
  noiseThreshold = parameters$noiseThreshold,
  noiseQuantile = parameters$noiseQuantile,
  minSNR = parameters$minSNR,
  mzrThreshold = parameters$mzrThreshold
)
n_orginal <- sum(vapply(spl, nrow, 0))
n_clean <- sum(vapply(spl_clean, nrow, 0))
message("Original points: ", n_orginal, "; After cleaning: ", n_clean, " (reduction of ", round((n_orginal - n_clean) / n_orginal * 100, 2), "% )")

#plot_3D_spec_list(spl_clean, c(1:100))



sp_clean <- data.table::rbindlist(spl_clean, use.names = TRUE, fill = TRUE)
# plot_3D_by_rt(sp_clean[sp_clean$mz > single_target$mzmin & sp_clean$mz < single_target$mzmax & sp_clean$rt > single_target$rtmin & sp_clean$rt < single_target$rtmax, ])



source("dev/dev_peak_finding/dev_cluster_spectra.R")
sp_clustered <- cluster_spectra(
  sp_clean,
  mzrThreshold = parameters$mzrThreshold,
  minTraces = parameters$minTraces,
  minSNR = parameters$minSNR
)
# plot_3D_by_rt(sp_clustered[sp_clustered$mz > single_target$mzmin & sp_clustered$mz < single_target$mzmax & sp_clustered$rt > single_target$rtmin & sp_clustered$rt < single_target$rtmax, ])

#plot_3D_by_rt(sp_clustered, c(50:100))









# MARK: Finding peaks (per cluster)
spl_clusters <- split(sp_clustered, f = sp_clustered$cluster)
message("Total clusters: ", length(spl_clusters))
source("dev/dev_peak_finding/dev_chrom_peak_derivative.R")


temp_spec <- spl_clusters[["7612"]]
temp_spec <- spl_clusters[["11983"]]
temp_spec <- spl_clusters[["7987"]]
temp_spec <- spl_clusters[["7165"]]

peaks_list <- pbapply::pblapply(spl_clusters, function(temp_spec, parameters) {
  if (is.null(temp_spec)) return(NULL)
  if (nrow(temp_spec) < parameters$minTraces) return(NULL)
  peaks <- peak_detect_derivative(temp_spec, parameters$minTraces, parameters$baselineWindow, parameters$maxWidth, parameters$minSNR, parameters$minGaussFit, plotPeaks = TRUE)
  if (is.null(peaks)) return(NULL)
  if (!is.data.frame(peaks)) {
    peaks <- peaks[[1]]
  }
  if (nrow(peaks) == 0) return(NULL)
  peaks$cluster <- temp_spec$cluster[1]
  for (pk in seq_len(nrow(peaks))) {
    peak_sel <- temp_spec$rt >= peaks$rtmin[pk] & temp_spec$rt <= peaks$rtmax[pk]
    peak <- temp_spec[peak_sel, ]
    peak_intensities <- peak$intensity
    peaks$mz[pk] <- mean(peak$mz, na.rm = TRUE)
    peaks$mz_sd[pk] <- sd(peak$mz, na.rm = TRUE)
    peaks$mz_wt[pk] <- sum(peak$mz * peak_intensities, na.rm = TRUE) / sum(peak_intensities, na.rm = TRUE)
    peaks$mz_wt_sd[pk] <- sqrt(sum(peak_intensities * (peak$mz - peaks$mz_wt[pk])^2, na.rm = TRUE) / sum(peak_intensities, na.rm = TRUE))
    appex_idx <- which.max(peak$intensity)
    idx_range <- max(1, appex_idx - 5):min(nrow(peak), appex_idx + 5)
    peaks$mz_appex[pk] <- mean(peak$mz[idx_range], na.rm = TRUE)
    peaks$mz_appex_sd[pk] <- sd(peak$mz[idx_range], na.rm = TRUE)
    peaks$mzmin[pk] <- min(peak$mz, na.rm = TRUE)
    peaks$mzmax[pk] <- max(peak$mz, na.rm = TRUE)
    peaks$ppm[pk] <- (peaks$mzmax[pk] - peaks$mzmin[pk]) / peaks$mz[pk] * 1e6
    peaks$width[pk] <- peaks$rtmax[pk] - peaks$rtmin[pk]
  }
  peaks$id <- paste0(
    "CL", peaks$cluster,
    "_N", seq_len(nrow(peaks)),
    "_MZ", round(peaks$mz, 0),
    "_RT", round(peaks$rt, 0)
  )
  data.table::setcolorder(
    peaks,
    c("id", "rt", "mz", "mz_wt", "mz_appex", "ppm", "width", "intensity", "mzmin", "mzmax", "rtmin", "rtmax")
  )
  peaks
}, parameters = parameters)
peaks_dt <- data.table::rbindlist(peaks_list, use.names = TRUE, fill = TRUE)

# source("dev/dev_peak_finding/dev_plots.R")
# plot_3D_by_rt_with_peaks(sp_clustered[sp_clustered$mz > single_target$mzmin & sp_clustered$mz < single_target$mzmax & sp_clustered$rt > single_target$rtmin & sp_clustered$rt < single_target$rtmax, ], peaks_dt)

message("Total peaks found: ", nrow(peaks_dt))
message("Total peaks found with gaussian fit: ", nrow(peaks_dt[!is.na(peaks_dt$r_squared), ]))

matched_targets <- find_peak_targets(peaks_dt, db_merck, ppm_tol = 10, rt_tol = 30)
matched_targets

plot_peaks_eic(peaks_dt[peaks_dt$id %in% matched_targets$peak_id, ], sp_clustered)

setdiff(db_merck$name, matched_targets$target_name)

generate_peak_formulas(
  peaks_dt[peaks_dt$id %in% matched_targets$peak_id, ],
  mzabs = 0.005,
  ppm = 5,
  elements = list(c("C", 20, 70), c("H", 20, 70), c("N", 0, 3)),
  validation = FALSE,
  charge = 0
)
