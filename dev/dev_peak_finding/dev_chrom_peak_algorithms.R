#-----------------------------------------
# Utility: find contiguous regions around a peak
get_bounds <- function(x, apex_idx, intensity, tol=0) {
  n <- length(x)
  # Left bound
  i <- apex_idx
  while (i > 1 && intensity[i-1] > tol) {
    if (intensity[i-1] < intensity[i]) break
    i <- i - 1
  }
  left <- x[i]

  # Right bound
  j <- apex_idx
  while (j < n && intensity[j+1] > tol) {
    if (intensity[j+1] < intensity[j]) break
    j <- j + 1
  }
  right <- x[j]

  return(c(left, right))
}

#-----------------------------------------
# 1. First derivative method
peak_detect_derivative <- function(dt) {
  rt <- dt$rt
  mz <- dt$mz
  intensity <- dt$intensity

  dI <- diff(intensity)
  d2I <- diff(dI)

  # Candidates: where slope changes + to -
  idx <- which(dI[-1] <= 0 & dI[-length(dI)] > 0) + 1

  # Group adjacent peaks if second derivative between them is mostly positive (i.e., valley is shallow)
  if (length(idx) == 0) return(data.frame())
  groups <- list()
  current_group <- idx[1]
  for (i in 2:length(idx)) {
    # Find minimum between peaks
    min_idx <- which.min(intensity[idx[i-1]:idx[i]]) + idx[i-1] - 1
    # If second derivative at minimum is positive (shallow valley), group peaks
    # Prevent grouping if the minimum between peaks is less than half the intensity of the previous peak
    min_intensity <- intensity[min_idx]
    prev_peak_intensity <- intensity[idx[i-1]]
    if (d2I[min_idx-1] > 0 && min_intensity > 0.5 * prev_peak_intensity) {
      current_group <- c(current_group, idx[i])
    } else {
      groups[[length(groups)+1]] <- current_group
      current_group <- idx[i]
    }
    if (d2I[min_idx-1] > 0) {
      current_group <- c(current_group, idx[i])
    } else {
      groups[[length(groups)+1]] <- current_group
      current_group <- idx[i]
    }
  }
  groups[[length(groups)+1]] <- current_group

  # For each group, keep only the peak with the highest intensity
  main_peaks <- sapply(groups, function(g) g[which.max(intensity[g])])

  # Do not exclude peaks after a large negative slope (main peak drop)

  peaks <- lapply(main_peaks, function(i) {
    bounds_rt <- get_bounds(rt, i, intensity)
    bounds_mz <- get_bounds(mz, i, intensity)

    list(
      rt = rt[i],
      mz = mz[i],
      height = intensity[i],
      rtmin = bounds_rt[1],
      rtmax = bounds_rt[2],
      mzmin = bounds_mz[1],
      mzmax = bounds_mz[2]
    )
  })

  out <- as.data.frame(do.call(rbind, peaks))
  return(out)
}

#-----------------------------------------
# 2. Smoothing + local maxima
peak_detect_smooth <- function(dt, k=5) {
  rt <- dt$rt
  mz <- dt$mz
  intensity <- dt$intensity

  # simple moving average smoothing
  smoothed <- filter(intensity, rep(1/k, k), sides=2)
  smoothed[is.na(smoothed)] <- intensity[is.na(smoothed)]

  # local maxima
  idx <- which(diff(sign(diff(smoothed))) == -2) + 1

  peaks <- lapply(idx, function(i) {
    bounds_rt <- get_bounds(rt, i, intensity)
    bounds_mz <- get_bounds(mz, i, intensity)

    list(
      rt = rt[i],
      mz = mz[i],
      height = intensity[i],
      rtmin = bounds_rt[1],
      rtmax = bounds_rt[2],
      mzmin = bounds_mz[1],
      mzmax = bounds_mz[2]
    )
  })

  out <- as.data.frame(do.call(rbind, peaks))
  return(out)
}

#-----------------------------------------
# 3. Thresholding + local maxima
peak_detect_threshold <- function(dt, threshold=NULL) {
  rt <- dt$rt
  mz <- dt$mz
  intensity <- dt$intensity

  if (is.null(threshold)) threshold <- mean(intensity) + sd(intensity)

  # Only consider above-threshold points
  idx <- which(intensity > threshold)

  # Find local maxima within above-threshold region
  candidates <- idx[which(diff(sign(diff(intensity[idx]))) == -2) + 1]

  peaks <- lapply(candidates, function(i) {
    bounds_rt <- get_bounds(rt, i, intensity, tol=threshold)
    bounds_mz <- get_bounds(mz, i, intensity, tol=threshold)

    list(
      rt = rt[i],
      mz = mz[i],
      height = intensity[i],
      rtmin = bounds_rt[1],
      rtmax = bounds_rt[2],
      mzmin = bounds_mz[1],
      mzmax = bounds_mz[2]
    )
  })

  out <- as.data.frame(do.call(rbind, peaks))
  return(out)
}
