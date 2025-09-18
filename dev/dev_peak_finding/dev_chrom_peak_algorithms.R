# MARK: Utility: estimate baseline and find peak boundaries
get_peak_bounds <- function(x, apex_idx, intensity, baseline) {
  n <- length(intensity)
  apex_intensity <- intensity[apex_idx]
  min_intensity_threshold <- 0.05 * apex_intensity

  # Left boundary: start at least 3 points before apex
  left_idx <- max(1, apex_idx - 3)
  min_found <- FALSE
  left_min_int <- intensity[left_idx]
  left_min_idx <- intensity[left_idx]
  while (left_idx > 1) {
    window_start <- max(1, left_idx - 4)
    window_end <- left_idx
    window <- intensity[window_start:window_end]
    min_idx <- which.min(window) + window_start - 1
    min_int <- intensity[min_idx]
    # If minimum is at current left_idx, check if previous is higher (start to increase)
    if (min_int > left_min_int) {
      left_idx <- left_min_idx
      min_found <- TRUE
      break
    }
    # Stop if at or below baseline (with 10% tolerance)
    if (intensity[min_idx] <= baseline[min_idx] * 1.1) {
      left_idx <- min_idx
      min_found <- TRUE
      break
    }
    # Stop if intensity is less than 5% of apex intensity
    if (intensity[min_idx] <= min_intensity_threshold) {
      left_idx <- min_idx
      min_found <- TRUE
      break
    }
    # Move window 1 point to the left
    left_min_int <- min_int
    left_min_idx <- min_idx
    left_idx <- left_idx - 1
  }

  # Right boundary: start at least 3 points after apex
  right_idx <- min(n, apex_idx + 3)
  min_found_r <- FALSE
  right_min_int <- intensity[right_idx]
  right_min_idx <- intensity[right_idx]
  while (right_idx < n) {
    window_start <- right_idx
    window_end <- min(n, right_idx + 3)
    window <- intensity[window_start:window_end]
    min_idx <- which.min(window) + window_start - 1
    min_int <- intensity[min_idx]
    # If minimum is at current right_idx, check if next is higher (start to increase)
    if (min_int > right_min_int) {
      right_idx <- right_min_idx
      min_found_r <- TRUE
      break
    }
    # Stop if at or below baseline (with 10% tolerance)
    if (intensity[min_idx] <= baseline[min_idx] * 1.1) {
      right_idx <- min_idx
      min_found_r <- TRUE
      break
    }
    # Stop if intensity is less than 5% of apex intensity
    if (intensity[min_idx] <= min_intensity_threshold) {
      right_idx <- min_idx
      min_found_r <- TRUE
      break
    }
    # Move window 1 point to the right
    right_min_int <- min_int
    right_min_idx <- min_idx
    right_idx <- right_idx + 1
  }

  return(c(x[left_idx], x[right_idx]))
}

# MARK:  First derivative method
peak_detect_derivative <- function(dt, min_sn, min_gaufit) {
  rt <- dt$rt
  mz <- dt$mz
  intensity <- dt$intensity
  n <- length(intensity)

  dI <- diff(intensity)
  d2I <- diff(dI)

  p <- plotly::plot_ly(x = rt, y = intensity, type = 'scatter', mode = 'lines+markers') %>%
    plotly::add_lines(y = c(0, dI * max(intensity) / max(dI)), name = "1st Derivative", line = list(dash = 'dash')) %>%
    plotly::add_lines(y = c(0, 0, d2I * max(intensity) / max(d2I)), name = "2nd Derivative", line = list(dash = 'dot')) %>%
    plotly::layout(title = "1st and 2nd derivative algorithm", yaxis = list(title = "Intensity"), xaxis = list(title = "Retention Time (rt)"))

  # Candidates: where slope changes + to -
  idx <- which(dI[-1] <= 0 & dI[-length(dI)] > 0) + 1

  # Only keep peaks that fulfill "likely peak" conditions based on 3 points before and after the peak
  if (length(idx) == 0) return(data.frame())
  keep <- logical(length(idx))
  for (k in seq_along(idx)) {
    i <- idx[k]
    # Define ranges for checking
    pre_range <- if (i > 1) seq.int(from = max(i - 5, 1), to = i - 1) else integer(0)
    post_range <- if (i < length(intensity)) seq.int(from = i + 1, to = min(i + 5, length(intensity))) else integer(0)
    # 1st derivative before should be positive, after should be negative
    pre_avg <- if (length(pre_range) > 0) mean(dI[pre_range], na.rm = TRUE) else NA
    post_avg <- if (length(post_range) > 0) mean(dI[post_range - 1], na.rm = TRUE) else NA
    # 2nd derivative at peak should be negative (concave down)
    d2_at_peak <- if (i > 1 && i < length(intensity)) d2I[i - 1] else NA

    # Check for higher apex in preceding or following 3 points
    pre_apex <- if (length(pre_range) > 0) any(intensity[pre_range] >= intensity[i]) else FALSE
    post_apex <- if (length(post_range) > 0) any(intensity[post_range] >= intensity[i]) else FALSE

    # Keep if: 1st derivative changes sign, 2nd derivative is negative, and no higher apex nearby
    keep[k] <- (!is.na(pre_avg) && !is.na(post_avg) && pre_avg > 0 && post_avg < 0) &&
               (!is.na(d2_at_peak) && d2_at_peak < 0) &&
               !pre_apex && !post_apex
  }
  idx <- idx[keep]
  if (length(idx) == 0) return(data.frame())

  cycle_time <- median(diff(rt))
  window_size <- floor(20 / cycle_time)
  baseline <- numeric(n)
  half_window <- floor(window_size / 2)

  for (i in 1:n) {
    start_idx <- max(1, i - half_window)
    end_idx <- min(n, i + half_window)
    baseline[i] <- min(intensity[start_idx:end_idx])
  }

  p <- p %>%
    plotly::add_lines(y = baseline, name = "Estimated Baseline", line = list(dash = 'dashdot', color = 'green'))

  if (n >= 3) {
    # Simple 3-point moving average
    smoothed_baseline <- baseline
    for (i in 2:(n-1)) {
      smoothed_baseline[i] <- mean(baseline[(i-1):(i+1)])
    }
    baseline <- smoothed_baseline
  }

  p <- p %>%
    plotly::add_lines(y = baseline, name = "Smoothed Baseline", line = list(dash = 'solid', color = 'blue'))

  peaks <- lapply(idx, function(i) {
    bounds <- get_peak_bounds(rt, i, intensity, baseline)
    noise <- min(intensity[rt >= bounds[1] & rt <= bounds[2]])
    sn <- if (noise > 0) (intensity[i] - baseline[i]) / noise else NA
    if (is.na(sn)) return(NULL)
    if (sn < min_sn) return(NULL)
    peak_mask <- which(rt >= bounds[1] & rt <= bounds[2])
    if (length(peak_mask) > 0) {
      peak_mask_intensities <- intensity[peak_mask]
      peak_mask_intensities <- peak_mask_intensities - min(peak_mask_intensities) + 1
      peak_mask_rt <- rt[peak_mask]
      A_init <- max(peak_mask_intensities)
      mu_init <- rt[i]
      sigma_init <- (bounds[2] - bounds[1]) / 6  #
      gaussian_model <- function(x, A, mu, sigma, min_int) {
        A * exp(-0.5 * ((x - mu) / sigma)^2)
      }
      fit <- tryCatch(
        nls(
          peak_mask_intensities ~ gaussian_model(peak_mask_rt, A, mu, sigma),
          start = list(A = A_init, mu = mu_init, sigma = sigma_init), control = nls.control(maxiter = 100)
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        fitted_params <- coef(fit)
        A_fit <- fitted_params["A"]
        mu_fit <- fitted_params["mu"]
        sigma_fit <- fitted_params["sigma"]
        y_obs <- peak_mask_intensities
        x_vals <- peak_mask_rt
        y_pred <- A_fit * exp(-((x_vals - mu_fit)^2) / (2 * sigma_fit^2))
        mean_y <- mean(y_obs)
        ss_total <- sum((y_obs - mean_y)^2)
        ss_residual <- sum((y_obs - y_pred)^2)
        r_squared <- 1 - (ss_residual / ss_total)
        if (r_squared < min_gaufit) {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }

    list(
      rt = rt[i],
      mz = mz[i],
      intensity = intensity[i],
      noise = baseline[i],
      sn = sn,
      A = if (exists("A_fit")) A_fit else NA_real_,
      mu = if (exists("mu_fit")) mu_fit else NA_real_,
      sigma = if (exists("sigma_fit")) sigma_fit else NA_real_,
      r_squared = if (exists("r_squared")) r_squared else NA_real_,
      y_pred = if (exists("y_pred")) y_pred else NA,
      x_vals = if (exists("x_vals")) x_vals else NA,
      n_traces = length(peak_mask),
      rtmin = bounds[1],
      rtmax = bounds[2]
    )
  })

  # Add rectangles for each detected peak and a dot at the apex
  for (i in seq_along(idx)) {
    if (is.null(peaks[[i]])) next
    bounds <- get_peak_bounds(rt, idx[i], intensity, baseline)
    text <- paste(
      "Peak ", i, "<br>",
      "RT: ", round(rt[idx[i]], 2), "<br>",
      "m/z: ", round(mz[idx[i]], 4), "<br>",
      "Intensity: ", round(intensity[idx[i]], 2), "<br>",
      "Baseline: ", round(baseline[idx[i]], 2), "<br>",
      "S/N: ", ifelse(is.na(peaks[[i]]$sn), "NA", round(peaks[[i]]$sn, 2)), "<br>",
      "N traces: ", peaks[[i]]$n_traces,
      if (!is.na(peaks[[i]]$A)) paste0("<br>A: ", round(peaks[[i]]$A, 2)) else "",
      if (!is.na(peaks[[i]]$mu)) paste0("<br>mu: ", round(peaks[[i]]$mu, 2)) else "",
      if (!is.na(peaks[[i]]$sigma)) paste0("<br>sigma: ", round(peaks[[i]]$sigma, 2)) else "",
      if (!is.na(peaks[[i]]$r_squared)) paste0("<br>R²: ", round(peaks[[i]]$r_squared, 4)) else ""
    )
    p <- p %>%
      plotly::add_trace(
        x = c(bounds[1], bounds[2], bounds[2], bounds[1], bounds[1]),
        y = c(0, 0, intensity[idx[i]], intensity[idx[i]], 0),
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(255,0,0,0.1)",
        line = list(color = "red", dash = "solid"),
        name = paste0("Peak ", i),
        legendgroup = paste0("Peak ", i),
        showlegend = FALSE
      ) %>%
      plotly::add_markers(
        x = rt[idx[i]],
        y = intensity[idx[i]],
        marker = list(color = 'red', size = 10, symbol = "circle"),
        name = paste0("Peak ", i),
        showlegend = TRUE,
        legendgroup = paste0("Peak ", i),
        text = text,
        hoverinfo = "text"
      ) %>%
      plotly::add_lines(
        x = if (!is.null(peaks[[i]]$x_vals) && !all(is.na(peaks[[i]]$x_vals))) peaks[[i]]$x_vals else c(NA),
        y = if (!is.null(peaks[[i]]$y_pred) && !all(is.na(peaks[[i]]$y_pred))) peaks[[i]]$y_pred else c(NA),
        line = list(color = 'purple', dash = 'dot'),
        name = paste0("Peak ", i),
        showlegend = FALSE,
        legendgroup = paste0("Peak ", i)
      )
  }

  peaks <- lapply(peaks, function(peak) {
    peak$x_vals <- NULL
    peak$y_pred <- NULL
    data.table::as.data.table(peak)
  })

  out <- data.table::rbindlist(peaks, use.names = TRUE, fill = TRUE)

  list(out, p)
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
    bounds_rt <- get_peak_bounds(rt, i, intensity)
    bounds_mz <- get_peak_bounds(mz, i, intensity)

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
    bounds_rt <- get_peak_bounds(rt, i, intensity)
    bounds_mz <- get_peak_bounds(mz, i, intensity)

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

  out <- as.data.frame(do.call(rbind, peaks))
  return(out)
}
