library(data.table)

cluster_spectra <- function(sp, mzrThreshold = 0.005, minTraces = 5) {
  setorder(sp, mz)
  diff_mz <- c(0, diff(sp$mz))
  cluster <- cumsum(diff_mz > mzrThreshold) + 1
  sp[, cluster := cluster]

  # Count cluster sizes and filter
  cluster_sizes <- sp[, .N, by = cluster]
  valid_clusters <- cluster_sizes[N > minTraces, cluster]
  sp <- sp[cluster %in% valid_clusters]

  # Aggregate
  sp <- sp[, .(mz = mz[which.max(intensity)],
               intensity = max(intensity),
               noise = max(noise)),
           by = .(rt, cluster)]
  sp
}