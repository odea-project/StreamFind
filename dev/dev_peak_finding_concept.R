# MARK Setup
# Setup -----------------------------------------------------------------------
devtools::load_all()
all_files <- StreamFindData::get_ms_file_paths()
files <- all_files[c(1, 7)]
#files <- all_files[grepl("blank|influent|o3sw", all_files)]
db_all <- StreamFindData::get_ms_tof_spiked_chemicals()
db_all <- db_all[grepl("S", db_all$tag), ]
cols <- c("name", "formula", "mass", "rt", "tag")
db_is <- db_all[db_all$tag %in% "IS", ]
db_is <- db_is[, cols, with = FALSE]
db_is <- db_is[!db_is$name %in% c("Ibuprofen-d3", "Naproxen-d3"), ]
db <- db_all[db_all$tag %in% "S", ]
db <- db[, cols, with = FALSE]
db_with_ms2 <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db_with_ms2 <- db_with_ms2[db_with_ms2$tag %in% "S", ]
db_with_ms2 <- db_with_ms2[, c("name", "formula", "mass", "SMILES", "rt", "polarity", "fragments"), with = FALSE]
db_with_ms2$polarity[db_with_ms2$polarity == 1] <- "positive"
db_with_ms2$polarity[is.na(db_with_ms2$polarity)] <- "positive"
db_with_ms2$polarity[db_with_ms2$polarity == -1] <- "negative"
ms <- MassSpecEngine$new(analyses = files)
# ms$Metadata <- list(
#   name = "Wastewater Ozonation Showcase",
#   author = "Ricardo Cunha",
#   description = "Demonstration project"
# )
# rpls <- c(
#   rep("blank_neg", 3),
#   rep("blank_pos", 3),
#   rep("influent_neg", 3),
#   rep("influent_pos", 3),
#   rep("effluent_neg", 3),
#   rep("effluent_pos", 3)
# )
# blks <- c(
#   rep("blank_neg", 3),
#   rep("blank_pos", 3),
#   rep("blank_neg", 3),
#   rep("blank_pos", 3),
#   rep("blank_neg", 3),
#   rep("blank_pos", 3)
# )
# ms$Analyses <- set_replicate_names(ms$Analyses, rpls)
# ms$Analyses <- set_blank_names(ms$Analyses, blks)
# ms$save("ms.rds")

# MARK: App Start
# App Start -------------------------------------------------------------------
#devtools::load_all()
#run_app(file = "ms.rds")

# MARK: Functions
# Functions -------------------------------------------------------------------
plot_vector_scatter <- function(vector_data) {
  plotly::plot_ly(
    x = seq_along(vector_data),
    y = vector_data,
    type = "scatter",
    mode = "markers"
  ) %>%
  plotly::layout(
    title = "Scatter Plot of Vector",
    xaxis = list(title = "Index"),
    yaxis = list(title = "Value")
  ) %>%
  plotly::toWebGL()
}

plot_x_y <- function(data, x_col, y_col) {
  plotly::plot_ly(
    data = data,
    x = as.formula(paste0("~", x_col)),
    y = as.formula(paste0("~", y_col)),
    type = "scatter",
    mode = "markers"
  ) %>%
  plotly::layout(
    title = paste("Scatter Plot of", y_col, "vs", x_col),
    xaxis = list(title = x_col),
    yaxis = list(title = y_col)
  ) %>%
  plotly::toWebGL()
}

plot_3D_by_rt <- function(spec_data, rt_indices) {
  unique_rt_values <- sort(unique(spec_data$rt))
  if (missing(rt_indices)) {
    rt_indices <- seq_along(unique_rt_values)
  }
  p <- plotly::plot_ly()
  for (i in seq_len(length(rt_indices))) {
    rt_idx <- rt_indices[i]
    selected_rt <- unique_rt_values[rt_idx]
    spec_filtered <- spec_data[spec_data$rt == selected_rt, ]

    if (nrow(spec_filtered) > 0) {
      p <- p %>% plotly::add_trace(
        data = spec_filtered,
        x = ~rt,
        y = ~mz,
        z = ~intensity,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 2,
          color = ~intensity,
          colorscale = list(c(0, "blue"), c(1, "red")),
          showscale = FALSE,
          line = list(width = 1, color = "black")
        ),
        name = paste("RT", round(selected_rt, 2), "s (Idx:", rt_idx, ")")
      )
    }
  }
  p %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = "Retention Time (s)"),
      yaxis = list(title = "m/z"),
      zaxis = list(title = "Intensity")
    )
  ) %>%
  plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
}

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

plot_3D_by_rt(spec, c(90, 91, 93, 94, 95))

find_local_maxima <- function(
  data,
  rt_col,
  intensity_col,
  window = 5,
  noise_window = 15,
  min_snr = 3,
  store_roi_data = TRUE) {

  data <- data[order(data[[rt_col]]), ]
  n <- nrow(data)

  rt_windows <- data.frame(
    rt_start = min(data[[rt_col]]),
    idx_start = 1,
    rt_end = min(data[[rt_col]]) + window,
    idx_end = 1
  )

  for (i in seq_len(nrow(data))) {
    rt_windows$idx_end[1] <- rt_windows$idx_end + 1
    if (data$rt[i] > min(data[[rt_col]]) + window) {
      break
    }
  }

  if (tail(rt_windows$rt_end, 1) < max(data[[rt_col]])) {
    while (tail(rt_windows$rt_end, 1) < max(data[[rt_col]])) {
      rt_windows <- rbind(rt_windows, data.frame(
        rt_start = tail(rt_windows$rt_start, 1) + window,
        idx_start = tail(rt_windows$idx_end, 1) + 1,
        rt_end = tail(rt_windows$rt_end, 1) + window,
        idx_end = tail(rt_windows$idx_end, 1) + 1
      ))

      for (i in tail(rt_windows$idx_end, 1):n) {
        rt_windows$idx_end[nrow(rt_windows)] <- rt_windows$idx_end[nrow(rt_windows)] + 1
        if (data$rt[i] > tail(rt_windows$rt_end, 1)) {
          break
        }
      }

      browser()
    }
  }

  browser()

  maxima_indices <- c()
  snr_values <- c()
  roi_data_list <- list()

  for (i in seq_len(nrow(rt_windows))) {
    window_start <- rt_windows$start[i]
    window_end <- rt_windows$end[i]


    if (current_intensity == max(window_intensities)) {
      # Calculate local noise using a larger RT window around the peak
      noise_start <- max(1, i - noise_window)
      noise_end <- min(n, i + noise_window)

      # Get all intensities in the noise window
      noise_region <- data[[intensity_col]][noise_start:noise_end]

      # Remove the peak and its immediate neighbors to avoid peak contamination
      peak_exclude_window <- 3  # Exclude ±3 points around peak
      exclude_start <- max(1, (i - noise_start + 1) - peak_exclude_window)
      exclude_end <- min(length(noise_region), (i - noise_start + 1) + peak_exclude_window)

      if (exclude_start <= exclude_end) {
        noise_data <- noise_region[-c(exclude_start:exclude_end)]
      } else {
        noise_data <- noise_region
      }

      # Calculate noise level using multiple approaches for robustness
      if (length(noise_data) > 5) {
        # Method 1: Standard deviation (good for Gaussian noise)
        noise_sd <- sd(noise_data, na.rm = TRUE)

        # Method 2: Median Absolute Deviation (robust to outliers)
        noise_mad <- mad(noise_data, na.rm = TRUE)

        # Method 3: Mean of lower quartile (baseline estimation)
        noise_lower_quartile <- mean(noise_data[noise_data <= quantile(noise_data, 0.25, na.rm = TRUE)], na.rm = TRUE)

        # Use the most appropriate noise estimate
        # MAD is often more robust for mass spec data
        noise_level <- if (noise_mad > 0) noise_mad else noise_sd

        # Fallback to lower quartile if both above are zero
        if (noise_level == 0 || is.na(noise_level)) {
          noise_level <- noise_lower_quartile
        }
      } else {
        # Fallback for small noise windows
        noise_level <- sd(window_intensities[-((window+1))])
      }

      # Final fallback to avoid division by zero
      if (noise_level == 0 || is.na(noise_level) || is.infinite(noise_level)) {
        noise_level <- min(noise_data[noise_data > 0], na.rm = TRUE)
        if (is.infinite(noise_level) || is.na(noise_level)) {
          noise_level <- 1  # Last resort fallback
        }
      }

      # Calculate signal-to-noise ratio
      snr <- current_intensity / noise_level

      # Only keep peaks with SNR >= min_snr
      if (snr >= min_snr) {
        maxima_indices <- c(maxima_indices, i)
        snr_values <- c(snr_values, snr)

        # Store ROI data (surrounding region) if requested
        if (store_roi_data) {
          roi_window <- 10  # Points around the peak to store
          roi_start <- max(1, i - roi_window)
          roi_end <- min(n, i + roi_window)

          roi_subset <- data[roi_start:roi_end, ]
          roi_data_list[[length(roi_data_list) + 1]] <- list(
            rt = roi_subset[[rt_col]],
            mz = roi_subset[["mz"]],
            intensity = roi_subset[[intensity_col]],
            peak_index = i - roi_start + 1  # Index of peak within ROI data
          )
        }
      }
    }
  }

  # Return a data.table with the specified structure including ROI data
  if (length(maxima_indices) > 0) {
    result_data <- data[maxima_indices, ]
    result_table <- data.table::data.table(
      index = maxima_indices,
      rt = result_data[[rt_col]],
      mz = result_data[["mz"]],
      intensity = result_data[[intensity_col]],
      snr = snr_values
    )

    # Add ROI data as list column if available
    if (store_roi_data && length(roi_data_list) > 0) {
      result_table$roi_data <- roi_data_list
    }

    return(result_table)
  } else {
    empty_table <- data.table::data.table(
      index = integer(),
      rt = numeric(),
      mz = numeric(),
      intensity = numeric(),
      snr = numeric()
    )

    if (store_roi_data) {
      empty_table$roi_data <- list()
    }

    return(empty_table)
  }
}

# Calculate overall noise level for the dataset (alternative approach)
calculate_baseline_noise <- function(spec_data, percentile = 0.1) {
  # Use lower percentile of intensities as noise estimate
  noise_level <- quantile(spec_data$intensity, percentile, na.rm = TRUE)
  return(noise_level)
}

# Alternative SNR calculation using global baseline
baseline_noise <- calculate_baseline_noise(spec, percentile = 0.1)
print(paste("Estimated baseline noise level:", round(baseline_noise, 2)))


spec_ordered <- spec[order(spec$mz), ]
plot_vector_scatter(spec_ordered$mz)

plot_x_y(spec_ordered, "mz", "intensity")

diff_mz <- c(0, diff(spec_ordered$mz))
plot_vector_scatter(diff_mz)


bin_width_hist <- 0.001
min_val_hist <- min(diff_mz, na.rm = TRUE)
max_val_hist <- max(diff_mz, na.rm = TRUE)
breaks_hist <- seq(min_val_hist, max_val_hist + bin_width_hist, by = bin_width_hist)

# Assign each diff_mz value to a bin
bin_indices_hist <- findInterval(diff_mz, breaks_hist, rightmost.closed = TRUE)

# Count occurrences in each bin
counts_hist <- tabulate(bin_indices_hist, nbins = length(breaks_hist) - 1)

# Find the bin with the maximum count (the mode bin)
mode_bin_index <- which.max(counts_hist)
mode_diff_mz <- (breaks_hist[mode_bin_index] + breaks_hist[mode_bin_index + 1]) / 2

# Suggest a bin width as a fraction of the mode (e.g., 1/5th)
suggested_bin_width <- mode_diff_mz / 5

cat(sprintf("Suggested bin width for diff_mz: %.6f (mode: %.6f)\n", suggested_bin_width, mode_diff_mz))

# Optionally plot the histogram for inspection
plot(breaks_hist[-length(breaks_hist)], counts_hist, type = "h", main = "Histogram of diff_mz", xlab = "diff_mz", ylab = "Count")
abline(v = mode_diff_mz, col = "red", lty = 2)





# MARK: LOCAL MAXIMA
mz_tolerance <- 0.01  # Increased from 0.008 to 0.02 for larger windows
mz_overlap <- 0.003    # Overlap between windows to prevent missing peaks
rt_window <- 30
noise_rt_window <- 60  # Larger window for noise calculation
min_snr <- 3  # Minimum signal-to-noise ratio


# calculate the hist for diff_mz manually without hist() function

# Define number of bins or bin width
bin_width <- 0.001
min_val <- min(diff_mz, na.rm = TRUE)
max_val <- max(diff_mz, na.rm = TRUE)
breaks <- seq(min_val, max_val + bin_width, by = bin_width)

# Assign each diff_mz value to a bin
bin_indices <- findInterval(diff_mz, breaks, rightmost.closed = TRUE)

# Count occurrences in each bin
counts <- tabulate(bin_indices, nbins = length(breaks) - 1)

# Create a data.frame for plotting or inspection
hist_manual <- data.frame(
  bin_left = breaks[-length(breaks)],
  bin_right = breaks[-1],
  count = counts
)

# Optionally plot using base R or ggplot2
plot(hist_manual$bin_left, hist_manual$count, type = "h",
     main = "Manual Histogram of diff_mz", xlab = "diff_mz", ylab = "Count")




# Create overlapping m/z windows to prevent missing peaks at boundaries
mz_min_global <- min(spec$mz)
mz_max_global <- max(spec$mz)
mz_step <- mz_tolerance - mz_overlap  # Step size considering overlap

# Generate overlapping breaks
mz_windows <- data.frame(
  start = seq(mz_min_global, mz_max_global - mz_tolerance, by = mz_step),
  end = seq(mz_min_global + mz_tolerance, mz_max_global, by = mz_step)
)

# Ensure the last window covers to the maximum m/z
if (tail(mz_windows$end, 1) < mz_max_global) {
  mz_windows <- rbind(mz_windows, data.frame(
    start = tail(mz_windows$start, 1) + mz_step,
    end = mz_max_global
  ))
}

# print(paste("Created", nrow(mz_windows), "overlapping m/z windows"))
# print(paste("Window size:", mz_tolerance, "Da, Overlap:", mz_overlap, "Da"))

roi_peaks <- data.table::data.table(
  index = integer(),
  rt = numeric(),
  mz = numeric(),
  intensity = numeric(),
  snr = numeric(),
  roi_data = list()
)

# Process each overlapping window
processed_peaks <- data.frame()  # Track processed peaks to avoid duplicates

for (i in 1:nrow(mz_windows)) {
  mz_min <- mz_windows$start[i]
  mz_max <- mz_windows$end[i]

  # Find indices for current m/z window
  start_idx <- which(spec_ordered$mz >= mz_min)[1]
  end_idx <- tail(which(spec_ordered$mz <= mz_max), 1)

  if (!is.na(start_idx) && !is.na(end_idx) && start_idx <= end_idx) {
    window_size <- end_idx - start_idx + 1

    if (window_size > 10) {  # Only process if enough points
      mz_subset <- spec_ordered[start_idx:end_idx, ]

      local_max <- find_local_maxima(
        mz_subset,
        "rt",
        "intensity",
        window = rt_window,
        noise_window = noise_rt_window,
        min_snr = min_snr
      )

      if (nrow(local_max) > 0) {
        # Check for duplicates based on rt and mz proximity

        if (nrow(processed_peaks) > 0) {
          # Remove peaks that are too close to already processed ones
          rt_tolerance <- 2  # seconds
          mz_dup_tolerance <- 0.005  # Da

          keep_indices <- c()
          for (j in 1:nrow(local_max)) {
            rt_diff <- abs(processed_peaks$rt - local_max$rt[j])
            mz_diff <- abs(processed_peaks$mz - local_max$mz[j])

            # Keep if no close match found
            if (!any(rt_diff < rt_tolerance & mz_diff < mz_dup_tolerance)) {
              keep_indices <- c(keep_indices, j)
            }
          }

          if (length(keep_indices) > 0) {
            local_max_filtered <- local_max[keep_indices, ]
            roi_peaks <- data.table::rbindlist(list(roi_peaks, local_max_filtered), use.names = TRUE, fill = TRUE)
            processed_peaks <- rbind(processed_peaks, local_max_filtered[, c("rt", "mz")])
          }
        } else {
          # First window, add all peaks
          roi_peaks <- data.table::rbindlist(list(roi_peaks, local_max), use.names = TRUE, fill = TRUE)
          processed_peaks <- local_max[, c("rt", "mz")]
        }
      }
    }
  }
}

roi_peaks <- roi_peaks[order(roi_peaks$intensity, decreasing = TRUE), ]

# MARK: ROI 3D Plotting Functions
# ROI 3D Plotting Functions ---------------------------------------------------

# Function to plot individual ROI in 3D
plot_roi_3d <- function(roi_table, roi_index) {
  if (roi_index > nrow(roi_table) || roi_index < 1) {
    stop("ROI index out of range")
  }

  if (!"roi_data" %in% names(roi_table)) {
    stop("ROI table does not contain roi_data column")
  }

  roi_info <- roi_table[roi_index, ]
  roi_data <- roi_info$roi_data[[1]]

  if (is.null(roi_data) || length(roi_data) == 0) {
    stop("No ROI data available for this index")
  }

  # Create data frame for plotting
  roi_df <- data.frame(
    rt = roi_data$rt,
    mz = roi_data$mz,
    intensity = roi_data$intensity
  )

  # Identify the peak point
  peak_idx <- roi_data$peak_index

  p <- plotly::plot_ly()

  # Add ROI data points
  p <- p %>% plotly::add_trace(
    data = roi_df,
    x = ~rt,
    y = ~mz,
    z = ~intensity,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 3,
      color = ~intensity,
      colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
      showscale = FALSE,
      opacity = 0.7
    ),
    name = "ROI Data",
    hovertemplate = "RT: %{x:.2f}s<br>m/z: %{y:.4f}<br>Intensity: %{z:.0f}<extra></extra>"
  )

  # Highlight the peak point
  if (peak_idx <= nrow(roi_df)) {
    peak_point <- roi_df[peak_idx, ]
    hover_text <- sprintf("PEAK<br>RT: %%.2fs<br>m/z: %%.4f<br>Intensity: %%.0f<br>SNR: %.2f<extra></extra>", roi_info$snr)

    p <- p %>% plotly::add_trace(
      x = peak_point$rt,
      y = peak_point$mz,
      z = peak_point$intensity,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 10,
        color = "red",
        symbol = "diamond",
        line = list(width = 2, color = "darkred")
      ),
      name = "Peak",
      hovertemplate = paste0("PEAK<br>RT: %{x:.2f}s<br>m/z: %{y:.4f}<br>Intensity: %{z:.0f}<br>SNR: ", round(roi_info$snr, 2), "<extra></extra>")
    )
  }

  p %>%
  plotly::layout(
    title = sprintf("ROI #%d - RT: %.2f s, m/z: %.4f, SNR: %.2f",
                    roi_index, roi_info$rt, roi_info$mz, roi_info$snr),
    scene = list(
      xaxis = list(title = "Retention Time (s)"),
      yaxis = list(title = "m/z"),
      zaxis = list(title = "Intensity")
    )
  ) %>%
  plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
}

# Function to plot multiple ROIs in a grid or sequentially
plot_multiple_rois_3d <- function(roi_table, roi_indices = 1:min(6, nrow(roi_table))) {
  if (max(roi_indices) > nrow(roi_table)) {
    stop("Some ROI indices are out of range")
  }

  plots <- list()

  for (i in roi_indices) {
    plots[[length(plots) + 1]] <- plot_roi_3d(roi_table, i)
  }

  return(plots)
}

# Function to create an interactive ROI explorer
plot_roi_explorer <- function(roi_table, max_rois = 10) {
  if (nrow(roi_table) == 0) {
    stop("No ROIs to plot")
  }

  n_rois <- min(max_rois, nrow(roi_table))

  cat("Available ROI plotting functions:\n")
  cat("1. plot_roi_3d(roi_peaks, index) - Plot individual ROI\n")
  cat("2. plot_multiple_rois_3d(roi_peaks, c(1,2,3)) - Plot multiple ROIs\n")
  cat(sprintf("3. Available ROI indices: 1 to %d\n", nrow(roi_table)))
  cat("\nTop ROIs by intensity:\n")

  for (i in 1:n_rois) {
    roi <- roi_table[i, ]
    cat(sprintf("ROI #%d: RT=%.2fs, m/z=%.4f, Intensity=%.0f, SNR=%.2f\n",
                i, roi$rt, roi$mz, roi$intensity, roi$snr))
  }

  cat("\nExample usage:\n")
  cat("plot_roi_3d(roi_peaks, 1)  # Plot the highest intensity ROI\n")
  cat("plot_multiple_rois_3d(roi_peaks, 1:3)  # Plot top 3 ROIs\n")
}

# Print ROI explorer information
if (nrow(roi_peaks) > 0) {
  plot_roi_explorer(roi_peaks)
}

# Combined 3D plot with raw data and ROI markers
plot_raw_data_with_roi <- function(raw_data, roi_data) {
  p <- plotly::plot_ly()

  # Add raw data as small blue-red gradient points
  p <- p %>% plotly::add_trace(
    data = raw_data,
    x = ~rt,
    y = ~mz,
    z = ~intensity,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 1.5,
      color = ~intensity,
      colorscale = list(c(0, "lightblue"), c(1, "lightcoral")),
      showscale = FALSE,
      opacity = 0.6
    ),
    name = "Raw Data",
    hovertemplate = "RT: %{x:.2f}s<br>m/z: %{y:.4f}<br>Intensity: %{z:.0f}<extra></extra>"
  )

  # Add ROI as green squares
  if (nrow(roi_data) > 0) {
    p <- p %>% plotly::add_trace(
      data = roi_data,
      x = ~rt,
      y = ~mz,
      z = ~intensity,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 8,
        color = "green",
        symbol = "square",
        line = list(width = 2, color = "darkgreen"),
        opacity = 0.9
      ),
      name = "ROI Detected",
      hovertemplate = "ROI Peak<br>RT: %{x:.2f}s<br>m/z: %{y:.4f}<br>Intensity: %{z:.0f}<extra></extra>"
    )
  }

  p %>%
  plotly::layout(
    title = paste("3D Mass Spectrum with", nrow(roi_data), "ROI Detected"),
    scene = list(
      xaxis = list(title = "Retention Time (s)"),
      yaxis = list(title = "m/z"),
      zaxis = list(title = "Intensity")
    )
  ) %>%
  plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
}

# Create the combined plot
combined_plot <- plot_raw_data_with_roi(spec, roi_peaks)

# Return the ROI table
print("=== RETURNING ROI PEAKS TABLE ===")
roi_peaks

combined_plot

plot_roi_3d(roi_peaks, 1)