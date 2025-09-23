#-----------------------------------------
# 2. Smoothing + local maxima
peak_detect_smooth <- function(dt, k=5) {
  # Input validation
  checkmate::assert_data_frame(dt, min.rows = 3)
  checkmate::assert_names(names(dt), must.include = c("rt", "mz", "intensity"))
  checkmate::assert_int(k, lower = 3, upper = 50)

  rt <- dt$rt
  mz <- dt$mz
  intensity <- dt$intensity

  # Validate data columns
  checkmate::assert_numeric(rt, finite = TRUE, any.missing = FALSE, min.len = 3)
  checkmate::assert_numeric(mz, finite = TRUE, any.missing = FALSE, min.len = 3)
  checkmate::assert_numeric(intensity, finite = TRUE, any.missing = FALSE, min.len = 3)

  n <- length(intensity)

  # Ensure all columns have same length
  if (length(rt) != n || length(mz) != n) {
    stop("All data columns must have same length: rt=", length(rt), ", mz=", length(mz), ", intensity=", n)
  }

  if (n < k) {
    warning("Data length (", n, ") is less than smoothing window (", k, "). Using simple maxima detection.")
    idx <- which(diff(sign(diff(intensity))) == -2) + 1
  } else {
    # simple moving average smoothing
    smoothed <- filter(intensity, rep(1/k, k), sides=2)
    smoothed[is.na(smoothed)] <- intensity[is.na(smoothed)]

    # local maxima
    idx <- which(diff(sign(diff(smoothed))) == -2) + 1
  }

  if (length(idx) == 0) {
    return(data.frame())
  }

  # Create baseline for get_peak_bounds
  baseline <- rep(min(intensity), n)

  peaks <- lapply(idx, function(i) {
    # Validate index
    if (i < 1 || i > n) {
      return(NULL)
    }

    bounds_rt <- tryCatch(
      get_peak_bounds(rt, i, intensity, baseline),
      error = function(e) c(rt[max(1, i-1)], rt[min(n, i+1)])
    )
    bounds_mz <- tryCatch(
      get_peak_bounds(mz, i, intensity, baseline),
      error = function(e) c(mz[max(1, i-1)], mz[min(n, i+1)])
    )

    list(
      rt = rt[i],
      mz = mz[i],
      intensity = intensity[i],
      rtmin = bounds_rt[1],
      rtmax = bounds_rt[2],
      mzmin = bounds_mz[1],
      mzmax = bounds_mz[2]
    )
  })

  # Filter out NULL results
  peaks <- peaks[!sapply(peaks, is.null)]

  if (length(peaks) == 0) {
    return(data.frame())
  }

  out <- as.data.frame(do.call(rbind, peaks))
  return(out)
}

#-----------------------------------------
# 3. Thresholding + local maxima
peak_detect_threshold <- function(dt, threshold=NULL) {
  # Input validation
  checkmate::assert_data_frame(dt, min.rows = 3)
  checkmate::assert_names(names(dt), must.include = c("rt", "mz", "intensity"))
  if (!is.null(threshold)) {
    checkmate::assert_number(threshold, lower = 0, finite = TRUE)
  }

  rt <- dt$rt
  mz <- dt$mz
  intensity <- dt$intensity

  # Validate data columns
  checkmate::assert_numeric(rt, finite = TRUE, any.missing = FALSE, min.len = 3)
  checkmate::assert_numeric(mz, finite = TRUE, any.missing = FALSE, min.len = 3)
  checkmate::assert_numeric(intensity, finite = TRUE, any.missing = FALSE, min.len = 3)

  n <- length(intensity)

  # Ensure all columns have same length
  if (length(rt) != n || length(mz) != n) {
    stop("All data columns must have same length: rt=", length(rt), ", mz=", length(mz), ", intensity=", n)
  }

  if (is.null(threshold)) {
    if (sd(intensity) == 0) {
      warning("Intensity has zero variance. Using mean as threshold.")
      threshold <- mean(intensity)
    } else {
      threshold <- mean(intensity) + sd(intensity)
    }
  }

  # Only consider above-threshold points
  idx <- which(intensity > threshold)

  if (length(idx) == 0) {
    return(data.frame())
  }

  # Find local maxima within above-threshold region
  if (length(idx) < 3) {
    candidates <- idx  # Use all points if too few
  } else {
    intensity_subset <- intensity[idx]
    local_max_idx <- which(diff(sign(diff(intensity_subset))) == -2) + 1
    candidates <- idx[local_max_idx]
  }

  if (length(candidates) == 0) {
    return(data.frame())
  }

  # Create baseline for get_peak_bounds
  baseline <- rep(min(intensity), n)

  peaks <- lapply(candidates, function(i) {
    # Validate index
    if (i < 1 || i > n) {
      return(NULL)
    }

    bounds_rt <- tryCatch(
      get_peak_bounds(rt, i, intensity, baseline),
      error = function(e) c(rt[max(1, i-1)], rt[min(n, i+1)])
    )
    bounds_mz <- tryCatch(
      get_peak_bounds(mz, i, intensity, baseline),
      error = function(e) c(mz[max(1, i-1)], mz[min(n, i+1)])
    )

    list(
      rt = rt[i],
      mz = mz[i],
      intensity = intensity[i],
      rtmin = bounds_rt[1],
      rtmax = bounds_rt[2],
      mzmin = bounds_mz[1],
      mzmax = bounds_mz[2]
    )
  })

  # Filter out NULL results
  peaks <- peaks[!sapply(peaks, is.null)]

  if (length(peaks) == 0) {
    return(data.frame())
  }

  out <- as.data.frame(do.call(rbind, peaks))
  return(out)
}



# MARK: Clustering


spec_clean <- spec_clean[order(spec_clean$mz), ]
diff_mz <- diff(spec_clean$mz)
diff_mz_eval <- diff_mz > args$mzr
all_clusters <- cumsum(c(1, diff_mz_eval))
spec_clean$cluster <- all_clusters
counter <- table(spec_clean$cluster)
counter <- counter[counter > args$minTraces]
spec_clean$cluster[!spec_clean$cluster %in% names(counter)] <- NA_integer_
clusters_summary <- spec_clean[, .(
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

# plot_mz_vector(spec_clean[spec_clean$rt > single_target$rtmin & spec_clean$rt < single_target$rtmax & spec_clean$mz > single_target$mzmin & spec_clean$mz < single_target$mzmax, ], interactive = TRUE)

# plot_3D_by_rt(spec_clean[spec_clean$rt > single_target$rtmin & spec_clean$rt < single_target$rtmax & spec_clean$mz > single_target$mzmin & spec_clean$mz < single_target$mzmax, ])

message("Total clusters: ", length(unique(spec_clean$cluster)), "; Clusters with > ", args$minTraces, " points: ", nrow(clusters_summary), " (", round(nrow(clusters_summary) / length(unique(spec_clean$cluster)) * 100, 2), "% )")

# MARK: Merging by rt
# merge mz in each cluster with the same rt
# for profile data, it actually centroids the data
# in principle no effect on centroid data
spec_merged <- spec_clean[, .(
  mz = mz[which.max(intensity)],
  intensity = max(intensity)
), by = .(rt, cluster)]

# plot_3D_by_rt(spec_merged[spec_merged$rt > single_target$rtmin & spec_merged$rt < single_target$rtmax & spec_merged$mz > single_target$mzmin & spec_merged$mz < single_target$mzmax, ])
