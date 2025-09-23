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
  mzmin = 600,
  mzmax = 650,
  rtmin = 350,
  rtmax = 450
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
  noiseThreshold = 0,
  noiseQuantile = 0.98,
  minSNR = 3,
  mzrThreshold = 0.005,
  minTraces = 5,
  maxWidth = 50,
  minGaussFit = 0.5
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
#plot_3D_spec_list(spl, c(1:2))


source("dev/dev_peak_finding/dev_clean_spectra.R")
spl_clean <- clean_spectra(
  spl,
  noiseThreshold = parameters$noiseThreshold,
  noiseQuantile = parameters$noiseQuantile,
  minSNR = parameters$minSNR,
  mzrThreshold = parameters$mzrThreshold
)
n_orginal <- sum(vapply(spl, nrow, 0))
n_clean <- sum(vapply(spl_clean, nrow, 0))
message("Original points: ", n_orginal, "; After cleaning: ", n_clean, " (reduction of ", round((n_orginal - n_clean) / n_orginal * 100, 2), "% )")

#plot_3D_spec_list(spl_clean, c(50:100))



sp_clean <- data.table::rbindlist(spl_clean, use.names = TRUE, fill = TRUE)
# plot_3D_by_rt(sp_clean[sp_clean$mz > single_target$mzmin & sp_clean$mz < single_target$mzmax & sp_clean$rt > single_target$rtmin & sp_clean$rt < single_target$rtmax, ])



source("dev/dev_peak_finding/dev_cluster_spectra.R")
sp_clustered <- cluster_spectra(
  sp_clean,
  mzrThreshold = parameters$mzrThreshold,
  minTraces = parameters$minTraces
)
# plot_3D_by_rt(sp_clustered[sp_clustered$mz > single_target$mzmin & sp_clustered$mz < single_target$mzmax & sp_clustered$rt > single_target$rtmin & sp_clustered$rt < single_target$rtmax, ])

#plot_3D_by_rt(sp_clustered, c(50:100))









# MARK: Finding peaks (per cluster)
spec_merged <- spec_merged[order(spec_merged$rt), ]
spec_clusters_list <- split(spec_merged, f = spec_merged$cluster)
source("dev/dev_peak_finding/dev_chrom_peak_derivative.R")

temp_spec <- spec_clusters_list[["1292"]]

peaks_list <- pbapply::pblapply(spec_clusters_list, function(temp_spec, args) {
  if (is.null(temp_spec)) return(NULL)
  if (nrow(temp_spec) < args$minTraces) return(NULL)
  peaks <- peak_detect_derivative(temp_spec, args$minTraces, args$maxWidth, args$sn, args$gaufit, plot_peaks = FALSE)
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
}, args = args)
peaks_dt <- data.table::rbindlist(peaks_list, use.names = TRUE, fill = TRUE)

# source("dev/dev_peak_finding/dev_plots.R")
# plot_3D_by_rt_with_peaks(spec_merged[spec_merged$rt > 1100 & spec_merged$rt < 1200 & spec_merged$mz > 205 & spec_merged$mz < 250, ], peaks_dt)

message("Total peaks found: ", nrow(peaks_dt))


db$mz <- db$mass + 1.007276
matched_targets <- find_peak_targets(peaks_dt, db, ppm_tol = 10, rt_tol = 15)
matched_targets

peaks_dt[peaks_dt$id %in% matched_targets$peak_id, ]

# which names from db are not in matched_targets
setdiff(db$name, matched_targets$target_name)


















pb <- txtProgressBar(min = 1, max = length(unique(spec_merged$cluster)), style = 3)
clusts <- unique(spec_merged$cluster)

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
