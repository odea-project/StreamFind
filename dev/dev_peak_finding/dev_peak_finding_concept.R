# MARK: Resources
# Resources -------------------------------------------------------------------
source("dev/dev_peak_finding/dev_resources.R")
source("dev/dev_peak_finding/dev_plots.R")

# MARK Setup
# Setup -----------------------------------------------------------------------
devtools::load_all()
ms <- MassSpecEngine$new(analyses = files)

# plot_spectra_eic(
#   ms$Analyses,
#   analyses = 6,
#   mass = db[c(3, 6, 7, 10, 11), ],
#   ppm = 20,
#   sec = 60,
#   colorBy = "targets+analyses",
#   legendNames = TRUE
# )

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
  mzmax = 250,
  rtmin = 0,
  rtmax = 2000
)

spec <- get_raw_spectra(
  ms$Analyses,
  analyses = 6, #3
  mz = tof_target, #orb_target #tof_target
  levels = 1
)

# MARK: Peak Finding Concept
# Peak Finding Concept ---------------------------------------------------------

# args for TOF data
args <- list(
  noise = 1000,
  mzr = 0.005,
  minTraces = 8,
  sn = 3,
  gaufit = 0.5
)

# args for Orbitrap data
# args <- list(
#   noise = 100000,
#   mzr = 0.002,
#   sn = 3,
#   gaufit = 0.5
# )

# MARK: Restricting m/z range for speed
#spec <- spec[spec$mz > 200 & spec$mz < 250, ]
#spec <- spec[spec$mz > tof_target$mzmin & spec$mz < tof_target$mzmax, ]




# MARK: Cleaning up noise

# may be expensive for large datasets!!!
# find faster way with similar approach
rt_vals <- unique(spec$rt)
rt_vals <- rt_vals[rt_vals > 1100 & rt_vals < 1200]
spec$noise <- 0
pb <- txtProgressBar(min = 1, max = length(rt_vals), style = 3)
for (i in seq_len(length(rt_vals))) {
  rt_val <- rt_vals[i]
  rt_sel <- spec$rt == rt_val
  ints <- spec$intensity[rt_sel]
  ints <- ints[ints > 0]
  if (length(ints) == 0) next
  noise <- quantile(ints, probs = 0.95)
  spec$noise[rt_sel] <- noise
  setTxtProgressBar(pb, i)
}
close(pb)

noise_dt <- unique(spec[spec$rt > 1000 & spec$rt < 1400, c("rt", "noise")])
# plot(noise_dt$noise ~ noise_dt$rt, type = "l")
# plot_3D_by_rt(spec[spec$mz > 205 & spec$mz < 210 & spec$rt > 1000 & spec$rt < 1400, ])




# MARK: Raise noise to args$noise if below
spec$noise[spec$noise < args$noise] <- args$noise
#plot_3D_by_rt(spec[spec$mz > 205 & spec$mz < 210 & spec$rt > 1000 & spec$rt < 1400, ])




# MARK: Removes noise
spec <- spec[spec$intensity > spec$noise, ]



# MARK: Ordering m/z
spec <- spec[order(spec$mz), ]
#plot_mz_vector(spec[spec$rt > 1100 & spec$rt < 1200, ], interactive = FALSE)



# MARK: First derivative of m/z
diff_mz <- diff(spec$mz)
#plot(diff_mz[diff_mz < 0.1], type = "h")
#abline(h = args$mzr, col = "red")



# MARK: Clustering by m/z
all_clusters <- integer(length(diff_mz))
pb <- txtProgressBar(min = 1, max = length(diff_mz), style = 3)
for (j in seq_along(diff_mz)) {
  if (diff_mz[j] > args$mzr) all_clusters[j] <- 1
  setTxtProgressBar(pb, j)
}
close(pb)
all_clusters <- cumsum(all_clusters)
all_clusters <- c(0, all_clusters)
all_clusters <- all_clusters + 1
spec$cluster <- all_clusters
counter <- table(spec$cluster)
counter <- counter[counter > args$minTraces]
spec$cluster[!spec$cluster %in% names(counter)] <- NA_integer_

# not necessary here, done in summary step
# spec_ordered$mzmin <- NA_real_
# spec_ordered$mzmax <- NA_real_
# for (clust in names(counter)) {
#   idx <- which(spec_ordered$cluster == clust)
#   spec_ordered$mzmin[idx] <- min(spec_ordered$mz[idx])
#   spec_ordered$mzmax[idx] <- max(spec_ordered$mz[idx])
# }

clusters_summary <- spec[, .(
  mz = mean(mz),
  weighted_mz = sum(mz * intensity) / sum(intensity),
  mzmin = min(mz, na.rm = TRUE),
  mzmax = max(mz, na.rm = TRUE),
  dppm = (max(mz) - min(mz)) / mean(mz) * 1e6,
  rtmin = min(rt),
  rtmax = max(rt),
  intensity = sum(intensity),
  points = .N
), by = .(cluster)][!is.na(cluster)]

#plot_mz_vector(spec[spec$rt > 1100 & spec$rt < 1200, ], interactive = TRUE)
#plot_3D_by_rt(spec[spec$rt > 1100 & spec$rt < 1200, ])



# MARK: Merging by rt
# merge mz in each cluster with the same rt
# for profile data, it actually centroids the data
# in principle no effect on centroid data
spec_merged <- spec[, .(
  mz = mean(mz),
  intensity = sum(intensity)
), by = .(rt, cluster)]

#plot_3D_by_rt(spec_merged[spec_merged$rt > 1100 & spec_merged$rt < 1200, ])





# MARK: Finding peaks (per cluster)
peaks_list <- list()
spec_merged <- spec_merged[order(spec_merged$rt), ]
pb <- txtProgressBar(min = 1, max = length(unique(spec_merged$cluster)), style = 3)
clusts <- unique(spec_merged$cluster)
source("dev/dev_peak_finding/dev_chrom_peak_algorithms.R")
pn <- 0
for (i in seq_along(clusts)) {
  clust <- clusts[i]

  clust <- 50

  if (is.na(clust)) next

  spec_clust <- spec_merged[
    spec_merged$cluster == clust,
  ]

  if (nrow(spec_clust) < args$minTraces) {
    setTxtProgressBar(pb, i)
    next
  }

  peaks <- peak_detect_derivative(spec_clust, args$minTraces, args$sn, args$gaufit, plot_peaks = TRUE)
  peaks[[1]]
  peaks[[2]]

  # Other techniques to explore
  #peaks <- peak_detect_cwt(spec_clust)
  #peaks <- peak_detect_wavelets(spec_clust)
  #peaks <- peak_detect_smooth(spec_clust)
  #peaks <- peak_detect_threshold(spec_clust)

  if (is.null(peaks)) {
    setTxtProgressBar(pb, i)
    next
  }

  if (!is.data.frame(peaks)) {
    peaks_dt <- peaks[[1]]
  } else {
    peaks_dt <- peaks
  }

  if (nrow(peaks_dt) == 0) {
    setTxtProgressBar(pb, i)
    next
  }

  peaks_dt$cluster <- clust

  for (pk in seq_len(nrow(peaks_dt))) {
    peak_sel <- spec$cluster == clust & spec$rt >= peaks_dt$rtmin[pk] & peaks_dt$rtmax[pk]
    peak_intensities <- spec$intensity[peak_sel]
    peaks_dt$mz[pk] <- mean(spec$mz[peak_sel], na.rm = TRUE)
    peaks_dt$mz_wt[pk] <- sum(spec$mz[peak_sel] * peak_intensities, na.rm = TRUE) / sum(peak_intensities, na.rm = TRUE)
    peaks_dt$mzmin[pk] <- min(spec$mz[peak_sel], na.rm = TRUE)
    peaks_dt$mzmax[pk] <- max(spec$mz[peak_sel], na.rm = TRUE)
    peaks_dt$ppm [pk] <- (peaks_dt$mzmax[pk] - peaks_dt$mzmin[pk]) / peaks_dt$mz[pk] * 1e6
    peaks_dt$width[pk] <- peaks_dt$rtmax[pk] - peaks_dt$rtmin[pk]
  }

  peaks_number <- paste0("N", seq_len(nrow(peaks_dt)) + pn)
  pn <- pn + nrow(peaks_dt)
  peaks_dt$id <- paste0(peaks_number, "_MZ", round(peaks_dt$mz, 0), "_RT", round(peaks_dt$rt, 0), "_CL", peaks_dt$cluster)
  data.table::setcolorder(peaks_dt, c("id", "rt", "mz", "mz_wt", "ppm", "width", "intensity", "mzmin", "mzmax", "rtmin", "rtmax"))
  peaks_list[[as.character(clust)]] <- peaks_dt
  setTxtProgressBar(pb, i)
}
close(pb)

peaks_dt <- data.table::rbindlist(peaks_list, use.names = TRUE, fill = TRUE)

source("dev/dev_peak_finding/dev_plots.R")
plot_3D_by_rt_with_peaks(spec[spec$rt > 1100 & spec$rt < 1200, ], peaks_dt)

peaks_dt[peaks_dt$rt > 1100 & peaks_dt$rt < 1200, ]
