# Plot the peak detection debug log - 3D/2D visualization of cluster data

library(plotly)

#' Plot cluster data from debug log
#' 
#' @param plot3D Logical. If TRUE (default), creates 3D plot. If FALSE, creates 2D line plot.
#' @param log_file Path to the log file. Default: "log/peak_detection_debug.log"
#' @return A plotly figure object
plot_cluster_data <- function(plot3D = TRUE, log_file = "log/peak_detection_debug.log") {
  
  # Check if log file exists
  if (!file.exists(log_file)) {
    stop("Log file not found: ", log_file)
  }
  
  # Read the log file
  log_lines <- readLines(log_file, warn = FALSE)
  
  # Find the lines with cluster data
  rt_line <- grep("cluster_rt <- c\\(", log_lines, value = TRUE)
  mz_line <- grep("cluster_mz <- c\\(", log_lines, value = TRUE)
  intensity_line <- grep("cluster_intensity <- c\\(", log_lines, value = TRUE)
  noise_line <- grep("cluster_noise <- c\\(", log_lines, value = TRUE)
  smoothed_line <- grep("cluster_smoothed <- c\\(", log_lines, value = TRUE)
  baseline_line <- grep("cluster_baseline <- c\\(", log_lines, value = TRUE)
  
  # Extract the data by evaluating the R code
  if (length(rt_line) > 0 && length(mz_line) > 0 && 
      length(intensity_line) > 0 && length(noise_line) > 0) {
    
    # Remove leading whitespace and extract just the assignment part
    rt_line <- sub("^\\s+", "", rt_line)
    mz_line <- sub("^\\s+", "", mz_line)
    intensity_line <- sub("^\\s+", "", intensity_line)
    noise_line <- sub("^\\s+", "", noise_line)
    
    # Evaluate to get the vectors
    eval(parse(text = rt_line))
    eval(parse(text = mz_line))
    eval(parse(text = intensity_line))
    eval(parse(text = noise_line))
    
    # Get smoothed and baseline if available
    cluster_smoothed <- NULL
    cluster_baseline <- NULL
    if (length(smoothed_line) > 0) {
      smoothed_line <- sub("^\\s+", "", smoothed_line)
      eval(parse(text = smoothed_line))
    }
    if (length(baseline_line) > 0) {
      baseline_line <- sub("^\\s+", "", baseline_line)
      eval(parse(text = baseline_line))
    }
    
    # Get peak positions if available (candidate peaks)
    peak_rt <- NULL
    peak_mz <- NULL
    peak_intensity <- NULL
    peak_lines <- grep("Peak positions \\(RT, m/z, smoothed_intensity\\):", log_lines, value = FALSE)
    if (length(peak_lines) > 0) {
      # Extract peak data from lines following the header
      peak_data_start <- peak_lines[1] + 1
      peak_data <- c()
      for (i in peak_data_start:length(log_lines)) {
        line <- log_lines[i]
        # Stop if we hit another section or empty line
        if (grepl("^\\s*$", line) || grepl("^[A-Z]", line)) break
        # Extract values: format is "  RT=... m/z=... intensity=..."
        if (grepl("RT=", line)) {
          rt_match <- regmatches(line, regexpr("RT=[0-9.]+", line))
          mz_match <- regmatches(line, regexpr("m/z=[0-9.]+", line))
          int_match <- regmatches(line, regexpr("smoothed_intensity=[0-9.]+", line))
          if (length(rt_match) > 0 && length(mz_match) > 0 && length(int_match) > 0) {
            peak_rt <- c(peak_rt, as.numeric(sub("RT=", "", rt_match)))
            peak_mz <- c(peak_mz, as.numeric(sub("m/z=", "", mz_match)))
            peak_intensity <- c(peak_intensity, as.numeric(sub("smoothed_intensity=", "", int_match)))
          }
        }
      }
    }
    
    # Get valid peak positions from "Peak boundaries" section (after boundary calculation & merging)
    valid_peak_rt <- NULL
    valid_peak_mz <- NULL
    valid_peak_intensity <- NULL
    valid_peak_reason <- NULL  # Rejection reason if peak didn't become a feature
    
    boundary_lines <- grep("Peaks after boundary calculation & merging:", log_lines, value = FALSE)
    if (length(boundary_lines) > 0) {
      # Find the "Peak boundaries" header
      boundary_header_idx <- NULL
      for (i in (boundary_lines[1] + 1):min(boundary_lines[1] + 5, length(log_lines))) {
        if (grepl("Peak boundaries \\(apex RT, RT range, width, n_traces\\):", log_lines[i])) {
          boundary_header_idx <- i
          break
        }
      }
      
      if (!is.null(boundary_header_idx)) {
        # Extract peak boundary data from lines following the header
        for (i in (boundary_header_idx + 1):length(log_lines)) {
          line <- log_lines[i]
          # Stop if we hit "Processing peak" section
          if (grepl("Processing peak with original apex", line)) break
          
          # Extract values: format is "  - Apex: RT=1036.10, m/z=1035.4451, intensity=62 | Range: ..."
          if (grepl("- Apex:", line)) {
            rt_match <- regmatches(line, regexpr("RT=[0-9.]+", line))
            mz_match <- regmatches(line, regexpr("m/z=[0-9.]+", line))
            int_match <- regmatches(line, regexpr("intensity=[0-9.]+", line))
            
            if (length(rt_match) > 0 && length(mz_match) > 0 && length(int_match) > 0) {
              rt_val <- as.numeric(sub("RT=", "", rt_match))
              mz_val <- as.numeric(sub("m/z=", "", mz_match))
              int_val <- as.numeric(sub("intensity=", "", int_match))
              
              valid_peak_rt <- c(valid_peak_rt, rt_val)
              valid_peak_mz <- c(valid_peak_mz, mz_val)
              valid_peak_intensity <- c(valid_peak_intensity, int_val)
              
              # Look for rejection reason for this peak (search forward in "Processing peak" section)
              # Match by the FINAL RT (after apex shifts), not the original processing RT
              rejection_reason <- NULL
              for (j in i:min(i + 200, length(log_lines))) {
                check_line <- log_lines[j]
                
                # Check if this is the processing section for our peak (match RT with tolerance)
                if (grepl("Processing peak with original apex", check_line)) {
                  proc_rt_match <- regmatches(check_line, regexpr("RT=[0-9.]+", check_line))
                  if (length(proc_rt_match) > 0) {
                    proc_rt <- as.numeric(sub("RT=", "", proc_rt_match))
                    # Check if RT matches (within 0.1 second tolerance)
                    if (abs(proc_rt - rt_val) < 0.1) {
                      # Found the processing section for this peak, look for rejection/acceptance
                      for (k in (j + 1):min(j + 50, length(log_lines))) {
                        result_line <- log_lines[k]
                        
                        # Check for rejection
                        if (grepl("→ Peak REJECTED", result_line)) {
                          # Extract reason from parentheses
                          reason_match <- regmatches(result_line, regexpr("\\(([^)]+)\\)", result_line))
                          if (length(reason_match) > 0) {
                            rejection_reason <- gsub("^\\(|\\)$", "", reason_match[1])
                          }
                          break
                        }
                        
                        # Check for acceptance - but verify the FINAL RT matches our boundary RT
                        if (grepl("→ FINAL FEATURE:", result_line)) {
                          # Look at the next line for the actual RT
                          if (k + 1 <= length(log_lines)) {
                            rt_line <- log_lines[k + 1]
                            final_rt_match <- regmatches(rt_line, regexpr("RT=[0-9.]+", rt_line))
                            if (length(final_rt_match) > 0) {
                              final_rt <- as.numeric(sub("RT=", "", final_rt_match))
                              # Only mark as accepted if final RT matches our boundary RT
                              if (abs(final_rt - rt_val) < 0.1) {
                                rejection_reason <- "Accepted as feature"
                              } else {
                                rejection_reason <- paste0("Merged into feature at RT=", final_rt)
                              }
                            }
                          }
                          break
                        }
                        
                        # Stop if we hit next peak processing
                        if (grepl("Processing peak with original apex", result_line) && k > j) break
                      }
                      break
                    }
                  }
                }
              }
              
              valid_peak_reason <- c(valid_peak_reason, ifelse(is.null(rejection_reason), "Unknown", rejection_reason))
            }
          }
        }
      }
    }
    
    # Extract final feature peak boundaries and details (not rejected peaks)
    peak_regions <- list()
    feature_annotations <- list()
    fitted_gaussians <- list()  # Store fitted Gaussian data for each feature
    final_feature_lines <- grep("→ FINAL FEATURE:", log_lines, value = FALSE)
    for (line_idx in final_feature_lines) {
      # Extract feature name
      feature_line <- log_lines[line_idx]
      feature_name_match <- regmatches(feature_line, regexpr("CL[0-9]+_[A-Z0-9_]+", feature_line))
      feature_name <- if (length(feature_name_match) > 0) feature_name_match else "Unknown"
      
      # Extract fitted Gaussian data (search backwards from FINAL FEATURE line)
      fitted_rt <- NULL
      fitted_intensity <- NULL
      fitted_mz <- NULL
      for (i in (line_idx - 1):max(1, line_idx - 50)) {
        line <- log_lines[i]
        
        # Look for "Fitted vs Actual intensities:" section
        if (grepl("Fitted vs Actual intensities:", line)) {
          # Next lines should have RT values, Actual, and Fitted
          for (j in (i + 1):min(i + 10, length(log_lines))) {
            data_line <- log_lines[j]
            
            # Extract RT values
            if (grepl("^\\s+RT values:", data_line)) {
              rt_str <- sub("^\\s+RT values:\\s*", "", data_line)
              fitted_rt <- as.numeric(strsplit(rt_str, ",\\s*")[[1]])
            }
            
            # Extract Fitted values
            if (grepl("^\\s+Fitted:", data_line)) {
              fitted_str <- sub("^\\s+Fitted:\\s*", "", data_line)
              fitted_intensity <- as.numeric(strsplit(fitted_str, ",\\s*")[[1]])
            }
            
            # Stop when we hit another section
            if (grepl("Sum of squared", data_line)) break
          }
          
          # If we found fitted data, get corresponding m/z values from cluster
          if (!is.null(fitted_rt) && !is.null(fitted_intensity)) {
            # Match RT values to cluster m/z
            fitted_mz <- sapply(fitted_rt, function(rt) {
              idx <- which.min(abs(cluster_rt - rt))
              if (length(idx) > 0) cluster_mz[idx] else NA
            })
          }
          break
        }
      }
      
      # Look for the RT range line and other details (next few lines after FINAL FEATURE)
      apex_rt <- NULL
      rtmin <- NULL
      rtmax <- NULL
      mz <- NULL
      intensity <- NULL
      sn <- NULL
      
      for (i in (line_idx + 1):min(line_idx + 5, length(log_lines))) {
        line <- log_lines[i]
        
        # Match RT pattern: "RT=967.88 (range: 883.02-987.85, width=104.83s, FWHM=34.95s)"
        if (grepl("RT=.*\\(range:", line)) {
          rt_match <- regmatches(line, regexpr("RT=[0-9.]+", line))
          range_match <- regmatches(line, regexpr("range: [0-9.]+-[0-9.]+", line))
          
          if (length(rt_match) > 0 && length(range_match) > 0) {
            apex_rt <- as.numeric(sub("RT=", "", rt_match))
            range_str <- sub("range: ", "", range_match)
            range_parts <- strsplit(range_str, "-")[[1]]
            rtmin <- as.numeric(range_parts[1])
            rtmax <- as.numeric(range_parts[2])
          }
        }
        
        # Match m/z pattern: "m/z=636.3651 (range: 636.3607-636.3715, ppm=17.0)"
        if (grepl("m/z=[0-9.]+", line) && grepl("range:", line)) {
          mz_match <- regmatches(line, regexpr("m/z=[0-9.]+", line))
          if (length(mz_match) > 0) {
            mz <- as.numeric(sub("m/z=", "", mz_match))
          }
        }
        
        # Match intensity pattern: "Intensity=266, Noise=51, S/N=5.2"
        if (grepl("Intensity=[0-9]+", line)) {
          int_match <- regmatches(line, regexpr("Intensity=[0-9]+", line))
          sn_match <- regmatches(line, regexpr("S/N=[0-9.]+", line))
          if (length(int_match) > 0) {
            intensity <- as.numeric(sub("Intensity=", "", int_match))
          }
          if (length(sn_match) > 0) {
            sn <- as.numeric(sub("S/N=", "", sn_match))
          }
        }
      }
      
      # Extract additional details from RT line
      width <- NULL
      fwhm <- NULL
      if (!is.null(apex_rt)) {
        for (i in (line_idx + 1):min(line_idx + 5, length(log_lines))) {
          line <- log_lines[i]
          if (grepl("width=[0-9.]+s", line)) {
            width_match <- regmatches(line, regexpr("width=[0-9.]+", line))
            fwhm_match <- regmatches(line, regexpr("FWHM=[0-9.]+", line))
            if (length(width_match) > 0) {
              width <- as.numeric(sub("width=", "", width_match))
            }
            if (length(fwhm_match) > 0) {
              fwhm <- as.numeric(sub("FWHM=", "", fwhm_match))
            }
            break
          }
        }
      }
      
      # Extract area and Gaussian R² from Area line
      area <- NULL
      gaussian_r2 <- NULL
      n_traces <- NULL
      for (i in (line_idx + 1):min(line_idx + 5, length(log_lines))) {
        line <- log_lines[i]
        if (grepl("Area=[0-9]+", line)) {
          area_match <- regmatches(line, regexpr("Area=[0-9]+", line))
          r2_match <- regmatches(line, regexpr("Gaussian R²=[-0-9.]+", line))  # Allow negative values
          traces_match <- regmatches(line, regexpr("n_traces=[0-9]+", line))
          if (length(area_match) > 0) {
            area <- as.numeric(sub("Area=", "", area_match))
          }
          if (length(r2_match) > 0) {
            gaussian_r2 <- as.numeric(sub("Gaussian R²=", "", r2_match))
          }
          if (length(traces_match) > 0) {
            n_traces <- as.numeric(sub("n_traces=", "", traces_match))
          }
          break
        }
      }
      
      if (!is.null(apex_rt) && !is.null(rtmin) && !is.null(rtmax)) {
        peak_regions[[length(peak_regions) + 1]] <- list(
          apex_rt = apex_rt,
          rtmin = rtmin,
          rtmax = rtmax
        )
        
        # Store fitted Gaussian data if available
        if (!is.null(fitted_rt) && !is.null(fitted_intensity) && !is.null(fitted_mz)) {
          # Ensure all vectors have the same length and no NAs
          if (length(fitted_rt) == length(fitted_intensity) && 
              length(fitted_rt) == length(fitted_mz) &&
              !any(is.na(fitted_mz))) {
            fitted_gaussians[[length(fitted_gaussians) + 1]] <- list(
              rt = fitted_rt,
              mz = fitted_mz,
              intensity = fitted_intensity
            )
          } else {
            # If lengths don't match or there are NAs, skip this fitted data
            fitted_gaussians[[length(fitted_gaussians) + 1]] <- NULL
          }
        } else {
          # Add empty placeholder to keep indices aligned
          fitted_gaussians[[length(fitted_gaussians) + 1]] <- NULL
        }
        
        # Create detailed annotation text
        annotation_text <- paste0(
          feature_name,
          if (!is.null(mz)) paste0("\nm/z=", round(mz, 4)) else "",
          if (!is.null(apex_rt)) paste0("\nRT=", round(apex_rt, 2), "s") else "",
          if (!is.null(width)) paste0(" (", round(width, 1), "s)") else "",
          if (!is.null(intensity)) paste0("\nInt=", round(intensity, 0)) else "",
          if (!is.null(sn)) paste0(", S/N=", round(sn, 1)) else "",
          if (!is.null(fwhm)) paste0("\nFWHM=", round(fwhm, 2), "s") else "",
          if (!is.null(area)) paste0("\nArea=", round(area, 0)) else "",
          if (!is.null(gaussian_r2)) paste0("\nGauss R²=", round(gaussian_r2, 3)) else "",
          if (!is.null(n_traces)) paste0("\nn=", n_traces) else ""
        )
        
        feature_annotations[[length(feature_annotations) + 1]] <- list(
          rt = apex_rt,
          mz = if (!is.null(mz)) mz else mean(cluster_mz),
          intensity = if (!is.null(intensity)) intensity else max(cluster_intensity),
          text = annotation_text
        )
      }
    }
    
    # Deduplicate features - keep only unique apex RTs (merged peaks create duplicates)
    if (length(feature_annotations) > 0) {
      apex_rts <- sapply(feature_annotations, function(x) x$rt)
      unique_indices <- !duplicated(apex_rts)
      
      peak_regions <- peak_regions[unique_indices]
      feature_annotations <- feature_annotations[unique_indices]
      fitted_gaussians <- fitted_gaussians[unique_indices]
    }
  
    # Create plot based on plot3D flag
    if (plot3D) {
      # Create 3D scatter plot with multiple traces
      fig <- plot_ly() %>%
        add_trace(
          x = cluster_rt,
          y = cluster_mz,
          z = cluster_intensity,
          type = "scatter3d",
          mode = "lines+markers",
          line = list(color = "black", width = 2),
          marker = list(
            size = 3,
            color = "black",
            opacity = 0.6
          ),
          name = "Intensity",
          text = ~paste("RT:", round(cluster_rt, 2),
                        "<br>m/z:", round(cluster_mz, 4),
                        "<br>Intensity:", round(cluster_intensity, 0)),
          hoverinfo = "text"
        ) %>%
        add_trace(
          x = cluster_rt,
          y = cluster_mz,
          z = cluster_noise,
          type = "scatter3d",
          mode = "markers",
          marker = list(
            size = 4,
            color = "red",
            opacity = 0.6
          ),
          name = "Noise",
          text = ~paste("RT:", round(cluster_rt, 2), 
                        "<br>m/z:", round(cluster_mz, 4),
                        "<br>Noise:", round(cluster_noise, 0)),
          hoverinfo = "text"
        )
      
      # Add smoothed trace if available
      if (!is.null(cluster_smoothed)) {
        fig <- fig %>%
          add_trace(
            x = cluster_rt,
            y = cluster_mz,
            z = cluster_smoothed,
            type = "scatter3d",
            mode = "markers",
            marker = list(
              size = 4,
              color = "green",
              opacity = 0.7
            ),
            name = "Smoothed",
            text = ~paste("RT:", round(cluster_rt, 2),
                          "<br>m/z:", round(cluster_mz, 4),
                          "<br>Smoothed:", round(cluster_smoothed, 0)),
            hoverinfo = "text"
          )
      }
      
      # Add baseline trace if available
      if (!is.null(cluster_baseline)) {
        fig <- fig %>%
          add_trace(
            x = cluster_rt,
            y = cluster_mz,
            z = cluster_baseline,
            type = "scatter3d",
            mode = "markers",
            marker = list(
              size = 3,
              color = "orange",
              opacity = 0.5
            ),
            name = "Baseline",
            text = ~paste("RT:", round(cluster_rt, 2),
                          "<br>m/z:", round(cluster_mz, 4),
                          "<br>Baseline:", round(cluster_baseline, 0)),
            hoverinfo = "text"
          )
      }
      
      # Add peak positions if available (candidate peaks) - COMMENTED OUT to show only boundary peaks
      # if (!is.null(peak_rt) && length(peak_rt) > 0) {
      #   fig <- fig %>%
      #     add_trace(
      #       x = peak_rt,
      #       y = peak_mz,
      #       z = peak_intensity,
      #       type = "scatter3d",
      #       mode = "markers",
      #       marker = list(
      #         size = 8,
      #         color = "black",
      #         symbol = "diamond",
      #         opacity = 1.0
      #       ),
      #       name = "Candidate Peaks",
      #       text = ~paste("CANDIDATE PEAK<br>RT:", round(peak_rt, 2),
      #                     "<br>m/z:", round(peak_mz, 4),
      #                     "<br>Intensity:", round(peak_intensity, 0)),
      #       hoverinfo = "text"
      #     )
      # }
      
      # Add valid peak positions if available (after validation)
      if (!is.null(valid_peak_rt) && length(valid_peak_rt) > 0) {
        # Create text with rejection reason
        valid_text_3d <- paste("VALID PEAK<br>RT:", round(valid_peak_rt, 2),
                               "<br>m/z:", round(valid_peak_mz, 4),
                               "<br>Intensity:", round(valid_peak_intensity, 0),
                               "<br>Status:", valid_peak_reason)
        
        fig <- fig %>%
          add_trace(
            x = valid_peak_rt,
            y = valid_peak_mz,
            z = valid_peak_intensity,
            type = "scatter3d",
            mode = "markers",
            marker = list(
              size = 10,
              color = "green",
              symbol = "diamond",
              opacity = 1.0,
              line = list(color = "darkgreen", width = 2)
            ),
            name = "Peaks (after boundary calc)",
            text = valid_text_3d,
            hoverinfo = "text"
          )
      }
      
      # Add peak regions for final features (not rejected) with hover annotations - LAST to appear on top
      if (length(peak_regions) > 0) {
        for (i in seq_along(peak_regions)) {
          region <- peak_regions[[i]]
          annot <- feature_annotations[[i]]
          
          # Filter cluster data to peak region (with small tolerance to ensure edge inclusion)
          rt_tolerance <- 0.1  # seconds
          region_mask <- cluster_rt >= (region$rtmin - rt_tolerance) & cluster_rt <= (region$rtmax + rt_tolerance)
          if (any(region_mask)) {
            region_rt <- cluster_rt[region_mask]
            region_mz <- cluster_mz[region_mask]
            region_intensity <- cluster_intensity[region_mask]
            
            # Create hover text with feature annotation
            hover_text <- paste0(
              "<b>", annot$text, "</b>"
            )
            
            fig <- fig %>%
              add_trace(
                x = region_rt,
                y = region_mz,
                z = region_intensity,
                type = "scatter3d",
                mode = "lines+markers",
                line = list(color = "rgb(0, 100, 200)", width = 3),
                marker = list(
                  size = 5,
                  color = "rgb(0, 100, 200)",
                  opacity = 0.9
                ),
                name = if (i == 1) "Peak Regions" else NULL,
                legendgroup = "peak_regions",
                showlegend = if (i == 1) TRUE else FALSE,
                text = hover_text,
                hoverinfo = "text"
              )
          }
          
          # Add fitted Gaussian trace for this feature (purple line)
          if (i <= length(fitted_gaussians) && !is.null(fitted_gaussians[[i]])) {
            gaussian_data <- fitted_gaussians[[i]]
            # Create text explicitly to avoid size mismatch
            gaussian_text_3d <- paste("Fitted Gaussian<br>RT:", round(gaussian_data$rt, 2),
                                      "<br>m/z:", round(gaussian_data$mz, 4),
                                      "<br>Fitted:", round(gaussian_data$intensity, 1))
            
            fig <- fig %>%
              add_trace(
                x = gaussian_data$rt,
                y = gaussian_data$mz,
                z = gaussian_data$intensity,
                type = "scatter3d",
                mode = "lines+markers",
                line = list(color = "purple", width = 4),
                marker = list(size = 4, color = "purple", opacity = 0.9),
                name = if (i == 1) "Fitted Gaussian" else NULL,
                legendgroup = "fitted_gaussian",
                showlegend = if (i == 1) TRUE else FALSE,
                text = gaussian_text_3d,
                hoverinfo = "text"
              )
          }
        }
      }
      
      fig <- fig %>%
        layout(
          title = "Peak Detection Cluster: Intensity vs Noise",
          scene = list(
            xaxis = list(title = "Retention Time (s)"),
            yaxis = list(title = "m/z"),
            zaxis = list(title = "Signal"),
            camera = list(
              eye = list(x = 1.5, y = 1.5, z = 1.3)
            )
          ),
          showlegend = TRUE
        )
      
    } else {
      # Create 2D line plot with multiple traces
      fig <- plot_ly() %>%
        add_trace(
          x = cluster_rt,
          y = cluster_intensity,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "black", width = 2),
          marker = list(size = 3, color = "black"),
          name = "Intensity",
          text = ~paste("RT:", round(cluster_rt, 2),
                        "<br>Intensity:", round(cluster_intensity, 0)),
          hoverinfo = "text"
        ) %>%
        add_trace(
          x = cluster_rt,
          y = cluster_noise,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "red", width = 2),
          marker = list(size = 4, color = "red"),
          name = "Noise",
          text = ~paste("RT:", round(cluster_rt, 2),
                        "<br>Noise:", round(cluster_noise, 0)),
          hoverinfo = "text"
        )
      
      # Add smoothed trace if available
      if (!is.null(cluster_smoothed)) {
        fig <- fig %>%
          add_trace(
            x = cluster_rt,
            y = cluster_smoothed,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "green", width = 2),
            marker = list(size = 4, color = "green"),
            name = "Smoothed",
            text = ~paste("RT:", round(cluster_rt, 2),
                          "<br>Smoothed:", round(cluster_smoothed, 0)),
            hoverinfo = "text"
          )
      }
      
      # Add baseline trace if available
      if (!is.null(cluster_baseline)) {
        fig <- fig %>%
          add_trace(
            x = cluster_rt,
            y = cluster_baseline,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "orange", width = 2),
            marker = list(size = 4, color = "orange"),
            name = "Baseline",
            text = ~paste("RT:", round(cluster_rt, 2),
                          "<br>Baseline:", round(cluster_baseline, 0)),
            hoverinfo = "text"
          )
      }
      
      # Add peak positions if available (candidate peaks) - COMMENTED OUT to show only boundary peaks
      # if (!is.null(peak_rt) && length(peak_rt) > 0) {
      #   fig <- fig %>%
      #     add_trace(
      #       x = peak_rt,
      #       y = peak_intensity,
      #       type = "scatter",
      #       mode = "markers",
      #       marker = list(
      #         size = 12,
      #         color = "black",
      #         symbol = "triangle-up",
      #         line = list(color = "white", width = 1)
      #       ),
      #       name = "Candidate Peaks",
      #       text = ~paste("CANDIDATE PEAK<br>RT:", round(peak_rt, 2),
      #                     "<br>Intensity:", round(peak_intensity, 0)),
      #       hoverinfo = "text"
      #     )
      # }
      
      # Add valid peak positions if available (after validation)
      if (!is.null(valid_peak_rt) && length(valid_peak_rt) > 0) {
        # Create text with rejection reason
        valid_text_2d <- paste("VALID PEAK<br>RT:", round(valid_peak_rt, 2),
                               "<br>Intensity:", round(valid_peak_intensity, 0),
                               "<br>Status:", valid_peak_reason)
        
        fig <- fig %>%
          add_trace(
            x = valid_peak_rt,
            y = valid_peak_intensity,
            type = "scatter",
            mode = "markers",
            marker = list(
              size = 14,
              color = "darkgreen",
              symbol = "triangle-up",
              line = list(color = "darkgreen", width = 2)
            ),
            name = "Peaks (after boundary calc)",
            text = valid_text_2d,
            hoverinfo = "text"
          )
      }
      
      # Add peak regions for final features (not rejected) as shaded areas with hover annotations - LAST to appear on top
      if (length(peak_regions) > 0 && !is.null(cluster_smoothed)) {
        for (i in seq_along(peak_regions)) {
          region <- peak_regions[[i]]
          annot <- feature_annotations[[i]]
          
          # Filter cluster data to peak region (with small tolerance to ensure edge inclusion)
          rt_tolerance <- 0.1  # seconds
          region_mask <- cluster_rt >= (region$rtmin - rt_tolerance) & cluster_rt <= (region$rtmax + rt_tolerance)
          if (any(region_mask)) {
            region_rt <- cluster_rt[region_mask]
            region_smoothed <- cluster_smoothed[region_mask]
            region_baseline <- if (!is.null(cluster_baseline)) cluster_baseline[region_mask] else rep(0, length(region_rt))
            
            # Create hover text with feature annotation
            hover_text <- paste0(
              "<b>", annot$text, "</b>"
            )
            
            fig <- fig %>%
              add_ribbons(
                x = region_rt,
                ymin = region_baseline,
                ymax = region_smoothed,
                line = list(color = "rgb(0, 100, 200)", width = 3),
                fillcolor = "rgba(0, 100, 200, 0.4)",
                name = if (i == 1) "Peak Regions" else NULL,
                legendgroup = "peak_regions",
                showlegend = FALSE,
                text = hover_text,
                hoverinfo = "text"
              ) %>%
              add_trace(
                x = region_rt,
                y = region_smoothed,
                type = "scatter",
                mode = "markers",
                marker = list(
                  size = 8,
                  color = "rgb(0, 100, 200)",
                  opacity = 0.01,
                  line = list(width = 0)
                ),
                name = if (i == 1) "Peak Regions" else NULL,
                legendgroup = "peak_regions",
                showlegend = if (i == 1) TRUE else FALSE,
                text = hover_text,
                hoverinfo = "text"
              )
          }
          
          # Add fitted Gaussian trace for this feature (purple line)
          if (i <= length(fitted_gaussians) && !is.null(fitted_gaussians[[i]])) {
            gaussian_data <- fitted_gaussians[[i]]
            # Create text explicitly to avoid size mismatch
            gaussian_text_2d <- paste("Fitted Gaussian<br>RT:", round(gaussian_data$rt, 2),
                                      "<br>Fitted:", round(gaussian_data$intensity, 1))
            
            fig <- fig %>%
              add_trace(
                x = gaussian_data$rt,
                y = gaussian_data$intensity,
                type = "scatter",
                mode = "lines+markers",
                line = list(color = "purple", width = 3),
                marker = list(size = 5, color = "purple"),
                name = if (i == 1) "Fitted Gaussian" else NULL,
                legendgroup = "fitted_gaussian",
                showlegend = if (i == 1) TRUE else FALSE,
                text = gaussian_text_2d,
                hoverinfo = "text"
              )
          }
        }
      }
      
      fig <- fig %>%
        layout(
          title = "Peak Detection Cluster: Intensity vs Noise",
          xaxis = list(title = "Retention Time (s)"),
          yaxis = list(title = "Signal"),
          showlegend = TRUE
        )
    }
    
    return(fig)
    
  } else {
    cat("Error: Could not find cluster data in log file.\n")
    cat("Make sure the debug output has been generated.\n")
    return(NULL)
  }
}

# Call the function with default 3D plot
# plot_cluster_data(plot3D = TRUE)
