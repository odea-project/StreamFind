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
#   mass = db[c(3, 6, 7, 10, 11), ],
#   ppm = 20,
#   sec = 1000,
#   colorBy = "targets+analyses",
#   legendNames = TRUE
# )

# MARK: Get Spectra
# Get Spectra ------------------------------------------------------------------

# TOF targets
tof_target <- data.frame(
  mzmin = 200,
  mzmax = 250,
  rtmin = 1100,
  rtmax = 1200
)

# ORBITRAP targets
orb_target <- data.frame(
  mzmin = 200,
  mzmax = 250,
  rtmin = 700,
  rtmax = 900
)

spec <- get_raw_spectra(
  ms$Analyses,
  analyses = 3,
  mz = orb_target,
  levels = 1
)

#plot_3D_by_rt(spec)

# MARK: Peak Finding Concept
# Peak Finding Concept ---------------------------------------------------------

# args for TOF data
args <- list(
  noise = 500,
  mzThreshold = 0.005
)

# args for Orbitrap data
args <- list(
  noise = 100000,
  mzThreshold = 0.002
)


# MARK: Ordering m/z
spec_ordered <- spec[order(spec$mz), ]
#plot_mz_vector(spec_ordered, interactive = FALSE)




# MARK: Removes noise
spec_ordered <- spec_ordered[spec_ordered$intensity > args$noise, ]
#plot_mz_vector(spec_ordered, interactive = FALSE)




# MARK: First derivative of m/z
diff_mz <- diff(spec_ordered$mz)
#plot_vector_scatter(diff_mz)




# MARK: Clustering by m/z
all_clusters <- integer(length(diff_mz))
for (j in seq_along(diff_mz)) {
  if (diff_mz[j] > args$mzThreshold) all_clusters[j] <- 1
}
all_clusters <- cumsum(all_clusters)
all_clusters <- c(0, all_clusters)
all_clusters <- all_clusters + 1
spec_ordered$cluster <- all_clusters
counter <- table(spec_ordered$cluster)
counter <- counter[counter > 5]
spec_ordered$cluster[!spec_ordered$cluster %in% names(counter)] <- NA_integer_

spec_ordered$mzmin <- NA_real_
spec_ordered$mzmax <- NA_real_
for (clust in names(counter)) {
  idx <- which(spec_ordered$cluster == clust)
  spec_ordered$mzmin[idx] <- min(spec_ordered$mz[idx])
  spec_ordered$mzmax[idx] <- max(spec_ordered$mz[idx])
}

clusters_summary <- spec_ordered[, .(
  mz = mean(mz),
  weighted_mz = sum(mz * intensity) / sum(intensity),
  mzmin = min(mzmin),
  mzmax = max(mzmax),
  dppm = (max(mz) - min(mz)) / mean(mz) * 1e6,
  rtmin = min(rt),
  rtmax = max(rt),
  intensity = sum(intensity),
  points = .N
), by = .(cluster)][!is.na(cluster)]

#plot_mz_vector(spec_ordered, interactive = FALSE)
#plot_3D_by_rt(spec_ordered)




# MARK: Merging by rt
# merge mz in each cluster with the same rt
# for profile data, it actually centroids the data
# in principle no effect on centroid data
spec_ordered_merged <- spec_ordered[, .(
  mz = mean(mz),
  intensity = sum(intensity)
), by = .(rt, cluster)]

#plot_3D_by_rt(spec_ordered_merged)
