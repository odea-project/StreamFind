library(data.table)

cluster_spectra <- function(sp, mzrThreshold = 0.005, minTraces = 5, minSNR = 3) {
  sp2 <- data.table::copy(sp)
  setorder(sp2, mz)
  diff_mz <- c(0, diff(sp2$mz))
  cluster <- cumsum(diff_mz > mzrThreshold) + 1
  sp2[, cluster := cluster]
  cluster_sizes <- sp2[, .N, by = cluster]
  valid_clusters <- cluster_sizes[N > minTraces, cluster]
  sp2 <- sp2[cluster %in% valid_clusters]
  sp2 <- sp2[,
    .(mz = mz[which.max(intensity)],
      intensity = max(intensity),
      noise = max(noise)), by = .(rt, cluster
    )
  ]
  cluster_stats <- sp2[, .(min_intensity = min(intensity), max_intensity = max(intensity)), by = cluster]
  valid_clusters <- cluster_stats[(max_intensity / min_intensity) > minSNR, cluster]
  sp2 <- sp2[cluster %in% valid_clusters]
  sp2 <- sp2[order(rt)]
  sp2
}