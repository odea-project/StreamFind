# Speed test for comparing peak finding algorithms
# This script runs the speed test and saves results to CSV files

library(data.table)
library(StreamFind)

# Speed test function
run_speed_test <- function(ps, algorithm_name, iterations = 3, engine) {
  times <- numeric(iterations)
  n_features_list <- list()
  intensity_data <- NULL
  
  for (i in seq_len(iterations)) {
    cat("\n", algorithm_name, "- Iteration", i, "\n")
    clear_cache("all")
    engine$Workflow <- list(ps)
    
    start_time <- Sys.time()
    engine$run_workflow()
    end_time <- Sys.time()
    
    times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Get features and count them per analysis
    tryCatch({
      results_obj <- engine$Analyses$results[[1]]
      features <- get_features(results_obj)
      
      if (!is.null(features) && nrow(features) > 0) {
        # Count features per analysis
        n_features_per_analysis <- features[, .N, by = analysis]
        n_features_list[[i]] <- n_features_per_analysis
        total_features <- sum(n_features_per_analysis$N)
        cat("Total features:", total_features, "\n")
        
        # Store intensity data from last iteration for histogram
        if (i == iterations) {
          intensity_data <- data.table::data.table(
            algorithm = algorithm_name,
            intensity = features$intensity,
            log10_intensity = log10(features$intensity + 1)
          )
        }
      } else {
        n_features_list[[i]] <- data.frame(analysis = character(0), N = numeric(0))
        cat("No features found\n")
      }
    }, error = function(e) {
      cat("Error getting features:", conditionMessage(e), "\n")
      n_features_list[[i]] <- data.frame(analysis = character(0), N = numeric(0))
    })
    
    cat("Time:", round(times[i], 2), "seconds\n")
  }
  
  # Calculate average number of features across iterations
  total_features_per_iter <- sapply(n_features_list, function(x) sum(x$N))
  mean_features <- mean(total_features_per_iter)
  
  return(list(
    stats = data.frame(
      algorithm = algorithm_name,
      iteration = seq_len(iterations),
      time_seconds = times,
      n_features = total_features_per_iter,
      mean_time = mean(times),
      sd_time = sd(times),
      mean_features = mean_features,
      features_per_second = mean_features / mean(times)
    ),
    intensity_data = intensity_data
  ))
}

# Main execution
cat("=== Speed Test Results ===\n")
cat("Starting speed tests...\n")

# Run speed tests
results_native <- run_speed_test(ps_ff_sf, "Native (StreamFind)", iterations = 1, engine = engine)
results_openms <- run_speed_test(ps_ff_openms, "OpenMS", iterations = 1, engine = engine)
results_xcms <- run_speed_test(ps_ff_xcms, "XCMS3 CentWave", iterations = 1, engine = engine)

# Combine results
all_results <- rbind(results_native$stats, results_openms$stats, results_xcms$stats)
all_intensity_data <- rbind(results_native$intensity_data, results_openms$intensity_data, results_xcms$intensity_data)

# Summary statistics
summary_stats <- unique(all_results[, c("algorithm", "mean_time", "sd_time", "mean_features", "features_per_second")])

# Intensity distribution statistics
intensity_summary <- all_intensity_data[, .(
  n_features = .N,
  mean_intensity = mean(intensity),
  median_intensity = median(intensity),
  sd_intensity = sd(intensity),
  q25_intensity = quantile(intensity, 0.25),
  q75_intensity = quantile(intensity, 0.75),
  min_intensity = min(intensity),
  max_intensity = max(intensity)
), by = algorithm]

cat("\n=== Summary Statistics ===\n")
print(summary_stats)

cat("\n=== Intensity Distribution Statistics ===\n")
print(intensity_summary)

# Save results to CSV files
output_dir <- "C:\\Users\\apoli\\Documents\\github\\StreamFind\\dev\\merck_peak_finding"

cat("\nSaving results to CSV files...\n")
fwrite(all_results, file.path(output_dir, "speed_test_all_results.csv"))
fwrite(summary_stats, file.path(output_dir, "speed_test_summary_stats.csv"))
fwrite(all_intensity_data, file.path(output_dir, "speed_test_intensity_data.csv"))
fwrite(intensity_summary, file.path(output_dir, "speed_test_intensity_summary.csv"))

cat("Results saved successfully!\n")
cat("  - speed_test_all_results.csv\n")
cat("  - speed_test_summary_stats.csv\n")
cat("  - speed_test_intensity_data.csv\n")
cat("  - speed_test_intensity_summary.csv\n")
