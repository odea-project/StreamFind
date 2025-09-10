# MARK: Resources
# Resources -------------------------------------------------------------------
source("dev/dev_peak_finding/dev_resources.R")
source("dev/dev_peak_finding/dev_plots.R")

# MARK Setup
# Setup -----------------------------------------------------------------------
devtools::load_all()
ms <- MassSpecEngine$new(analyses = files)

#plot_spectra_eic(ms$Analyses, mass = db, colorBy = "targets")

# MARK: Get_Spectra
# Get_Spectra ------------------------------------------------------------------
spec <- get_raw_spectra(
  ms$Analyses,
  analyses = 2,
  mz = data.frame(
    mzmin = 200,
    mzmax = 220,
    rtmin = 1100,
    rtmax = 1200
  ),
  ppm = 100,
  sec = 200,
  levels = 1
)

plot_3D_by_rt(spec, c(90))

unique_rt <- unique(spec$rt)
length(unique_rt)

spec_ordered <- spec[order(spec$mz), ]
spec_ordered <- spec_ordered[spec_ordered$intensity > 500, ]
#plot_mz_vector(spec_ordered, interactive = FALSE)

diff_mz <- diff(spec_ordered$mz)
#plot_vector_scatter(diff_mz)


all_clusters <- integer(length(diff_mz))
itMzClust <- 0.005
for (j in seq_along(diff_mz)) {
  if (diff_mz[j] > itMzClust) all_clusters[j] <- 1
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
), by = .(cluster)]

plot_mz_vector(spec_ordered, interactive = FALSE)

plot_3D_by_rt(spec_ordered)

# merge mz in each cluster with the same rt
spec_ordered_merged <- spec_ordered[, .(
  mz = mean(mz),
  intensity = sum(intensity)
), by = .(rt, cluster)]

plot_3D_by_rt(spec_ordered_merged)




plot
