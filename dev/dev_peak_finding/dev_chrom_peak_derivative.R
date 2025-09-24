# MARK: Utility: estimate baseline and find peak boundaries
get_peak_bounds <- function(
    x,
    apex_idx,
    intensity,
    baseline,
    max_half_width = 50) {
  valid <- c(
    checkmate::test_numeric(x, finite = TRUE, any.missing = FALSE, min.len = 5),
    checkmate::test_numeric(intensity, finite = TRUE, any.missing = FALSE, min.len = 5),
    checkmate::test_numeric(baseline, finite = TRUE, any.missing = FALSE, min.len = 5),
    checkmate::test_int(apex_idx, lower = 1, upper = length(intensity))
  )
  n <- length(intensity)
  if (length(x) != n) {
    warning("Vector 'x' must have same length as 'intensity' (", n, " vs ", length(x), ")")
    valid <- c(valid, FALSE)
  }
  if (length(baseline) != n) {
    warning("Vector 'baseline' must have same length as 'intensity' (", n, " vs ", length(baseline), ")")
    valid <- c(valid, FALSE)
  }
  if (!all(valid)) {
    warning("Data too short for robust peak boundary detection (n=", n, "). Using simple boundaries.")
    left_bound <- max(1, apex_idx - 1)
    right_bound <- min(n, apex_idx + 1)
    return(c(x[left_bound], x[right_bound]))
  }
  apex_intensity <- intensity[apex_idx]
  min_intensity_threshold <- 0.01 * apex_intensity

  # Left boundary: start at least 3 points before apex
  left_idx <- max(1, apex_idx - 3)
  min_found <- FALSE
  left_min_int <- intensity[left_idx]
  left_min_idx <- left_idx
  while (left_idx > 1) {
    window_start <- max(1, left_idx - 4)
    window_end <- left_idx

    if (x[apex_idx] - x[window_start] > max_half_width) {
      left_idx <- left_min_idx
      break
    }

    # Validate window bounds
    if (window_start > window_end || window_end > n) {
      break
    }

    window <- intensity[window_start:window_end]
    min_idx <- which.min(window) + window_start - 1

    # Validate min_idx is within bounds
    if (min_idx < 1 || min_idx > n) {
      break
    }

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
  right_min_idx <- right_idx
  while (right_idx < n) {
    window_start <- right_idx
    window_end <- min(n, right_idx + 4)

    if (x[window_end] - x[apex_idx] > max_half_width) {
      right_idx <- right_min_idx
      break
    }

    # Validate window bounds
    if (window_start > window_end || window_start < 1) {
      break
    }

    window <- intensity[window_start:window_end]
    min_idx <- which.min(window) + window_start - 1

    # Validate min_idx is within bounds
    if (min_idx < 1 || min_idx > n) {
      break
    }

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

    if ((x[right_idx] - x[apex_idx]) > max_half_width) {
      right_idx <- min(n, right_idx - 1)
      break
    }
  }

  # span_left <- apex_idx - left_idx
  # span_right <- right_idx - apex_idx
  # if (span_left > span_right) {
  #   right_idx <- min(n, apex_idx + span_left)
  # } else if (span_right > span_left) {
  #   left_idx <- max(1, apex_idx - span_right)
  # }

  c(x[left_idx], x[right_idx])
}

# MARK:  First derivative method
peak_detect_derivative <- function(dt, mintraces, baselineWindow, maxwidth, minSNR, minGaussFit, plotPeaks = FALSE) {
  checkmate::assert_int(mintraces, lower = 0)
  checkmate::assert_int(maxwidth, lower = 0)
  checkmate::assert_int(baselineWindow, lower = 0)
  checkmate::assert_data_frame(dt, min.rows = mintraces)
  checkmate::assert_names(names(dt), must.include = c("rt", "mz", "intensity"))
  checkmate::assert_number(minSNR, lower = 0, finite = TRUE)
  checkmate::assert_number(minGaussFit, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_logical(plotPeaks, len = 1)

  rt <- dt$rt
  mz <- dt$mz
  intensity <- dt$intensity
  checkmate::assert_numeric(rt, finite = TRUE, any.missing = FALSE, min.len = mintraces)
  checkmate::assert_numeric(mz, finite = TRUE, any.missing = FALSE, min.len = mintraces)
  checkmate::assert_numeric(intensity, finite = TRUE, any.missing = FALSE, min.len = mintraces)
  n <- length(intensity)

  if (length(rt) != n || length(mz) != n) {
    stop("All data columns must have same length: rt=", length(rt), ", mz=", length(mz), ", intensity=", n)
  }

  if (any(intensity < 0)) {
    warning("Negative intensities detected. This may cause issues with peak detection. All negative values will be set to zero.")
    intensity[intensity < 0] <- 0
  }

  if (plotPeaks) {
    p <- plotly::plot_ly(
      x = rt, y = intensity, type = "scatter", mode = "lines",
      name = "Raw Intensity", color = "black", colors = "black"
    )
  }

  cycle_time <- median(diff(rt))
  baseline_window_size <- floor(max(mintraces, floor(baselineWindow / cycle_time)) / 2)
  derivative_window_size <- max(floor(mintraces), floor(4 / cycle_time))

  baseline <- numeric(n)
  for (i in seq_len(n)) {
    start_idx <- max(1, i - baseline_window_size)
    end_idx <- min(n, i + baseline_window_size)
    if (start_idx > end_idx || start_idx < 1 || end_idx > n) {
      baseline[i] <- intensity[i]
    } else {
      baseline[i] <- min(intensity[start_idx:end_idx])
    }
  }

  if (n >= 3) {
    smoothed_baseline <- baseline
    for (i in 2:(n - 1)) {
      smoothed_baseline[i] <- mean(baseline[(i - 1):(i + 1)])
    }
    baseline <- smoothed_baseline

    # Smoothing window: 1% of traces, minimum 3
    window_size_smooth <- 2 #max(3, 3) #floor(n * 0.01)
    half_window_smooth <- floor(window_size_smooth / 2)
    smoothed_intensity <- intensity
    for (i in seq_len(n)) {
      start_idx <- max(1, i - half_window_smooth)
      end_idx <- min(n, i + half_window_smooth)
      smoothed_intensity[i] <- mean(intensity[start_idx:end_idx])
    }
  }

  if (plotPeaks) {
    p <- p %>%
      plotly::add_lines(
        y = baseline, name = "Estimated Baseline", line = list(dash = "dashdot", color = "#a3d8a3")
      )
    p <- p %>%
      plotly::add_lines(
        y = baseline, name = "Smoothed Baseline", line = list(dash = "solid", color = "#56b44d")
      )
    p <- p %>%
      plotly::add_lines(
        y = smoothed_intensity, name = "Smoothed Intensity", line = list(dash = "solid", color = "#120eff")
      )
  }

  dI <- diff(smoothed_intensity)
  d2I <- diff(dI)

  if (plotPeaks) {
    p <- p %>%
      plotly::add_lines(
        y = c(0, dI * max(intensity) / max(dI)), name = "1st Derivative", line = list(dash = "dash", color = "#c0bdbd")
      )
    p <- p %>%
      plotly::add_lines(
        y = c(0, 0, d2I * max(intensity) / max(d2I)), name = "2nd Derivative", line = list(dash = "dot", color = "#3f3e3e")
      )
  }

  # Candidates: where slope changes + to -
  idx <- which(dI[-1] <= 0 & dI[-length(dI)] > 0) + 1

  # Only keep peaks that fulfill "likely peak" conditions based on n points before and after the peak
  if (length(idx) == 0) {
    return(data.frame())
  }

  if (plotPeaks) {
    p <- p %>%
      plotly::add_markers(x = rt[idx], y = intensity[idx], name = "Peak Candidates", marker = list(color = "#000000", size = 8, symbol = "circle"))
  }

  keep <- logical(length(idx))
  for (k in seq_along(idx)) {
    i <- idx[k]

    # Skip if too close to edges
    if (i < derivative_window_size || i > n - derivative_window_size) {
      next
    }

    # Define ranges for checking using derivative_window_size
    pre_range <- seq.int(from = max(i - derivative_window_size, 1), to = i - 1)
    post_range <- seq.int(from = i + 1, to = min(i + derivative_window_size, n))

    # 1st derivative before should be positive
    if (length(pre_range) > 0) {
      valid_pre <- pre_range[pre_range >= 2 & pre_range <= i]
      if (length(valid_pre) > 0) {
        pre_avg <- mean(dI[valid_pre - 1], na.rm = TRUE)
      } else {
        pre_avg <- NA
      }
    } else {
      pre_avg <- NA
    }

    # 1st derivative after should be negative
    if (length(post_range) > 0) {
      valid_post <- post_range[post_range - 1 >= 1 & post_range - 1 <= length(dI)]
      if (length(valid_post) > 0) {
        post_avg <- mean(dI[valid_post - 1], na.rm = TRUE)
      } else {
        post_avg <- NA
      }
    } else {
      post_avg <- NA
    }

    # 2nd derivative at peak should be negative (concave down)
    if (i >= 2 && i <= length(d2I)) {
      d2_at_peak <- d2I[i - 1]
    } else {
      d2_at_peak <- NA
    }

    # Check for higher apex in preceding or following traces using derivative_window_size
    if (length(pre_range) > 0) {
      pre_apex <- any(smoothed_intensity[pre_range] >= smoothed_intensity[i])
    } else {
      pre_apex <- FALSE
    }
    if (length(post_range) > 0) {
      post_apex <- any(smoothed_intensity[post_range] >= smoothed_intensity[i])
    } else {
      post_apex <- FALSE
    }

    # Keep if: 1st derivative changes sign, 2nd derivative is negative, and no higher apex nearby
    keep[k] <- (!is.na(pre_avg) && !is.na(post_avg) && pre_avg > 0 && post_avg < 0) &&
      (!is.na(d2_at_peak) && d2_at_peak < 0) &&
      !pre_apex && !post_apex
  }
  idx <- idx[keep]
  if (length(idx) == 0) {
    return(data.frame())
  }

  peaks <- lapply(idx, function(i) {
    if (i < mintraces / 2 || i > n - mintraces / 2) {
      return(NULL)
    }

    bounds <- tryCatch(
      get_peak_bounds(rt, i, smoothed_intensity, baseline, maxwidth / 2),
      error = function(e) {
        warning("Error in get_peak_bounds for peak at index ", i, ": ", e$message)
        c(rt[max(1, i - 1)], rt[min(n, i + 1)])
      }
    )

    if (length(bounds) != 2 || is.na(bounds[1]) || is.na(bounds[2]) || bounds[1] >= bounds[2]) {
      return(NULL)
    }

    peak_mask <- which(rt >= bounds[1] & rt <= bounds[2])
    if (length(peak_mask) == 0) {
      return(NULL)
    }

    peak_intensities <- intensity[peak_mask]
    peak_rt <- rt[peak_mask]
    peak_mz <- mz[peak_mask]
    peak_max_intensity <- max(peak_intensities, na.rm = TRUE)
    peak_n <- length(peak_intensities)
    noise <- min(peak_intensities[c(1:2, (peak_n - 2):peak_n)], na.rm = TRUE)
    signal <- peak_max_intensity - baseline[i]
    if (noise > 0 && signal > 0) {
      sn <- signal / noise
    } else {
      sn <- NA
    }

    if (is.na(sn) || sn < minSNR) {
      return(NULL)
    }

    max_position <- which(peak_intensities == peak_max_intensity)[1]

    rt_at_max <- peak_rt[max_position]
    mz_at_max <- peak_mz[max_position]

    list(
      rt = rt_at_max,
      mz = mz_at_max,
      intensity = peak_max_intensity,
      noise = baseline[i],
      sn = sn,
      A = NA_real_,
      mu = NA_real_,
      sigma = NA_real_,
      r_squared = NA_real_,
      y_pred = NA,
      x_vals = NA,
      n_traces = length(peak_mask),
      rtmin = bounds[1],
      rtmax = bounds[2]
    )
  })

  peaks <- peaks[!sapply(peaks, is.null)]

  # Merge peaks with similar apex intensity (within 10%) and overlapping rt ranges
  if (length(peaks) > 1) {
    merged_peaks <- list()
    used <- rep(FALSE, length(peaks))
    for (i in seq_along(peaks)) {
      if (used[i]) next
      current <- peaks[[i]]
      group <- list(current)
      used[i] <- TRUE
      if (i + 1 > length(peaks)) {
        merged_peaks <- c(merged_peaks, group)
        break
      }
      for (j in (i + 1):length(peaks)) {
        if (used[j]) next
        candidate <- peaks[[j]]
        # Check apex intensity within 10%
        int1 <- current$intensity
        int2 <- candidate$intensity
        int_diff <- abs(int1 - int2) / max(int1, int2)
        # Check overlap
        overlap <- (current$rtmax >= candidate$rtmin) && (candidate$rtmax >= current$rtmin)
        if (int_diff <= 0.10 && overlap) {
          group <- c(group, list(candidate))
          used[j] <- TRUE
        }
      }
      # Merge group if more than one peak
      if (length(group) > 1) {
        merged <- group[[which.max(sapply(group, function(x) x$intensity))]]
        merged$rtmin <- min(sapply(group, function(x) x$rtmin))
        merged$rtmax <- max(sapply(group, function(x) x$rtmax))
        merged$n_traces <- sum(sapply(group, function(x) x$n_traces))
        merged_peaks <- c(merged_peaks, list(merged))
      } else {
        merged_peaks <- c(merged_peaks, group)
      }
    }
    peaks <- merged_peaks
  }

  if (TRUE) {
    peaks <- lapply(peaks, function(i) {
      # Gaussian fitting with validation
      peak_mask <- which(rt >= i$rtmin & rt <= i$rtmax)
      peak_mask_intensities <- intensity[peak_mask]
      peak_mask_intensities <- peak_mask_intensities - min(peak_mask_intensities) + 1
      peak_mask_rt <- rt[peak_mask]

      # Validate fitting parameters
      if (length(peak_mask_intensities) < 3) {
        # Not enough points for fitting
        return(i)
      }

      A_init <- max(peak_mask_intensities)
      mu_init <- i$rt
      sigma_init <- max(1.5, min(5, (i$rtmax - i$rtmin) / 6)) # Ensure positive sigma

      gaussian_model <- function(x, A, mu, sigma) {
        A * exp(-0.5 * ((x - mu) / sigma)^2)
      }

      fit <- tryCatch(
        nls(
          peak_mask_intensities ~ gaussian_model(peak_mask_rt, A, mu, sigma),
          start = list(A = A_init, mu = mu_init, sigma = sigma_init),
          control = nls.control(maxiter = 50)
        ),
        error = function(e) NULL
      )

      if (!is.null(fit)) {
        fitted_params <- coef(fit)
        A_fit <- fitted_params["A"]
        mu_fit <- fitted_params["mu"]
        sigma_fit <- fitted_params["sigma"]
        if (A_fit < i$intensity / 2 || A_fit > i$intensity * 2 || sigma_fit <= 0) {
          return(NULL)
        }
        y_obs <- peak_mask_intensities
        x_vals <- peak_mask_rt
        y_pred <- A_fit * exp(-((x_vals - mu_fit)^2) / (2 * sigma_fit^2))
        mean_y <- mean(y_obs)
        ss_total <- sum((y_obs - mean_y)^2)
        ss_residual <- sum((y_obs - y_pred)^2)
        r_squared <- if (ss_total > 0) 1 - (ss_residual / ss_total) else 0
        if (r_squared < minGaussFit) {
          return(NULL)
        } else {
          i$A <- A_fit
          i$mu <- mu_fit
          i$sigma <- sigma_fit
          i$r_squared <- r_squared
          i$y_pred <- y_pred
          i$x_vals <- x_vals
          return(i)
        }
      }
      i
    })
  }

  if (plotPeaks) {
    for (i in seq_along(peaks)) {
      if (is.null(peaks[[i]])) next
      pk <- peaks[[i]]
      text <- paste(
        "Peak ", i, "<br>",
        "RT: ", round(pk$rt, 2), "<br>",
        "m/z: ", round(pk$mz, 4), "<br>",
        "Intensity: ", round(pk$intensity, 2), "<br>",
        "Noise: ", round(pk$noise, 2), "<br>",
        "S/N: ", ifelse(is.na(pk$sn), "NA", round(pk$sn, 2)), "<br>",
        "N traces: ", pk$n_traces,
        if (!is.na(pk$A)) paste0("<br>A: ", round(pk$A, 2)) else "",
        if (!is.na(pk$mu)) paste0("<br>mu: ", round(pk$mu, 2)) else "",
        if (!is.na(pk$sigma)) paste0("<br>sigma: ", round(pk$sigma, 2)) else "",
        if (!is.na(pk$r_squared)) paste0("<br>R²: ", round(pk$r_squared, 4)) else ""
      )
      p <- p %>%
        plotly::add_trace(
          x = c(pk$rtmin, pk$rtmax, pk$rtmax, pk$rtmin, pk$rtmin),
          y = c(0, 0, pk$intensity, pk$intensity, 0),
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
          x = pk$rt,
          y = pk$intensity,
          marker = list(color = "red", size = 10, symbol = "circle"),
          name = paste0("Peak ", i),
          showlegend = TRUE,
          legendgroup = paste0("Peak ", i),
          text = text,
          hoverinfo = "text"
        ) %>%
        plotly::add_lines(
          x = if (!is.null(pk$x_vals) && !all(is.na(pk$x_vals))) pk$x_vals else c(NA),
          y = if (!is.null(pk$y_pred) && !all(is.na(pk$y_pred))) pk$y_pred else c(NA),
          line = list(color = "purple", dash = "dot"),
          name = paste0("Peak ", i),
          showlegend = FALSE,
          legendgroup = paste0("Peak ", i)
        )
    }

    p <- p %>% plotly::layout(
      yaxis = list(title = "Intensity"),
      xaxis = list(title = "Retention Time (rt)")
    )
  }

  peaks <- lapply(peaks, function(peak) {
    peak$x_vals <- NULL
    peak$y_pred <- NULL
    data.table::as.data.table(peak)
  })

  out <- data.table::rbindlist(peaks, use.names = TRUE, fill = TRUE)

  if (plotPeaks) {
    return(list(out, p))
  }

  return(out)
}
