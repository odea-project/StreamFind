# Peak Finding Algorithm Comparison Script
# Compare speed and results of different MassSpecMethod_FindFeatures algorithms
# Available: xcms (centwave), openms, kpic2, and native

# Load required libraries
library(StreamFind)
library(ggplot2)
library(dplyr)
library(microbenchmark)

# Set up file paths
path <- "C:/Users/apoli/Documents/example_files"
files <- list.files(path, pattern = ".mzML", full.names = TRUE)[1]

if (length(files) == 0) {
  stop("No .mzML files found in the specified path. Please check the path or add test files.")
}

cat("Found", length(files), "mzML files for analysis:\n")
for (i in seq_along(files)) {
  cat("  ", i, ":", basename(files[i]), "\n")
}


dummy <- MassSpecEngine$new(analyses = files) # dummy to cache data files

# Define comparable parameters for each algorithm
# Base parameters similar to the native method you provided
base_params <- list(
  rtWindows = data.frame(rtmin = 300, rtmax = 3000),
  resolution_profile = c(30000, 30000, 35000),
  noiseThreshold = 250,
  minSNR = 3,
  minTraces = 3,
  baselineWindow = 200,
  maxWidth = 100
)

# Create methods with comparable parameters
methods <- list(
  # Native StreamFind method
  native = MassSpecMethod_FindFeatures_native(
    rtWindows = data.frame(rtmin = 300, rtmax = 3000),
    resolution_profile = c(30000, 30000, 35000),
    noiseThreshold = 250,
    minSNR = 3,
    minTraces = 3,
    baselineWindow = 200,
    maxWidth = 100
  ),
  
  # XCMS3 CentWave method with comparable parameters
  xcms = MassSpecMethod_FindFeatures_xcms3_centwave(
    ppm = 15,
    peakwidth = c(5, 100),
    snthresh = 3,
    prefilter = c(3, 250 * 3),
    mzCenterFun = "wMean",
    integrate = 1,
    mzdiff = -2e-04,
    fitgauss = TRUE,
    noise = 250,
    verboseColumns = TRUE,
    firstBaselineCheck = FALSE,
    extendLengthMSW = FALSE
  ),
  
  # OpenMS method with comparable parameters  
  openms = MassSpecMethod_FindFeatures_openms(
    noiseThrInt = 250,
    chromSNR = 3,
    chromFWHM = 8,
    mzPPM = 15,
    reEstimateMTSD = TRUE,
    traceTermCriterion = "sample_rate",
    traceTermOutliers = 5,
    minSampleRate = 1,
    minTraceLength = 3,
    maxTraceLength = -1,
    widthFiltering = "fixed",
    minFWHM = 3,
    maxFWHM = 60,
    traceSNRFiltering = TRUE,
    localRTRange = 0,
    localMZRange = 0,
    isotopeFilteringModel = "none",
    MZScoring13C = FALSE,
    useSmoothedInts = FALSE,
    intSearchRTWindow = 3,
    useFFMIntensities = FALSE,
    verbose = FALSE
  ),
  
  # KPIC2 method with comparable parameters
  kpic2 = MassSpecMethod_FindFeatures_kpic2(
    level = 250,
    mztol = 0.008,
    gap = 2,
    width = 3,
    min_snr = 3,
    kmeans = FALSE,
    alpha = 0.3
  )
)

cache_info_to_remove <- get_cache_info.missing()
clear_cache(cache_info_to_remove$category[grepl("FindFeatures", cache_info_to_remove$category)])

cat("\nConfigured", length(methods), "algorithms for comparison:\n")
for (name in names(methods)) {
  cat("  -", name, "\n")
}

# Function to run a method and measure performance
run_method <- function(method_name, method_obj, files) {
  cat("\nRunning", method_name, "algorithm...\n")
  
  # Create engine for this method
  engine <- MassSpecEngine$new(
    metadata = Metadata(list(name = paste0("comparison_", method_name))),
    analyses = files
  )
  
  # Add the method to the engine
  engine$Workflow[[1]] <- method_obj
  
  # Measure execution time
  start_time <- Sys.time()
  
  tryCatch({
    # Run the method
    engine$run_workflow()
    
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Get results using the proper MassSpecResults_NonTargetAnalysis accessor
    nta_results <- engine$Results[[1]]
    
    # Count features using the get_features_count function
    if (!is.null(nta_results)) {
      # Get feature counts (non-filtered features)
      feature_counts <- get_features_count(nta_results, filtered = FALSE)
      
      # Extract features per file and calculate total
      features_per_file <- feature_counts$features
      total_features <- sum(features_per_file)
    } else {
      # No results found
      features_per_file <- rep(0, length(files))
      total_features <- 0
    }
    
    cat("  Execution time:", round(execution_time, 2), "seconds\n")
    cat("  Total features found:", total_features, "\n")
    cat("  Features per file:", paste(features_per_file, collapse = ", "), "\n")
    
    return(list(
      method = method_name,
      execution_time = execution_time,
      total_features = total_features,
      features_per_file = features_per_file,
      success = TRUE,
      error = NULL
    ))
    
  }, error = function(e) {
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    cat("  ERROR:", e$message, "\n")
    cat("  Execution time before error:", round(execution_time, 2), "seconds\n")
    
    return(list(
      method = method_name,
      execution_time = execution_time,
      total_features = 0,
      features_per_file = rep(0, length(files)),
      success = FALSE,
      error = e$message
    ))
  })
}

# Run comparison for all methods
cat("Starting peak finding algorithm comparison...\n")
results_list <- list()

for (name in names(methods)) {
  results_list[[name]] <- run_method(name, methods[[name]], files)
}

# Compile results into data frame
results_df <- data.frame(
  method = sapply(results_list, function(x) x$method),
  execution_time = sapply(results_list, function(x) x$execution_time),
  total_features = sapply(results_list, function(x) x$total_features),
  success = sapply(results_list, function(x) x$success),
  stringsAsFactors = FALSE
)

# Add features per file information
for (i in seq_along(files)) {
  col_name <- paste0("features_file_", i)
  results_df[[col_name]] <- sapply(results_list, function(x) x$features_per_file[i])
}

# Print summary table
cat("COMPARISON RESULTS SUMMARY\n")
print(results_df)

# Create visualization plots
successful_results <- results_df[results_df$success, ]

if (nrow(successful_results) > 0) {
  # Plot 1: Execution Time Comparison
  p1 <- ggplot(successful_results, aes(x = reorder(method, execution_time), y = execution_time)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = paste0(round(execution_time, 1), "s")), 
              vjust = -0.5, size = 3.5) +
    labs(title = "Peak Finding Algorithm Speed Comparison",
         subtitle = paste("Analysis of", length(files), "mzML files"),
         x = "Algorithm",
         y = "Execution Time (seconds)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 2: Features Found Comparison
  p2 <- ggplot(successful_results, aes(x = reorder(method, total_features), y = total_features)) +
    geom_col(fill = "darkgreen", alpha = 0.7) +
    geom_text(aes(label = total_features), 
              vjust = -0.5, size = 3.5) +
    labs(title = "Number of Features Found by Algorithm",
         subtitle = paste("Total features across", length(files), "files"),
         x = "Algorithm", 
         y = "Total Features Found") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 3: Features vs Speed scatter plot
  if (nrow(successful_results) > 1) {
    p3 <- ggplot(successful_results, aes(x = execution_time, y = total_features)) +
      geom_point(size = 4, alpha = 0.7) +
      geom_text(aes(label = method), vjust = -0.8, hjust = 0.5) +
      labs(title = "Features Found vs Execution Time",
           subtitle = "Trade-off between speed and feature detection",
           x = "Execution Time (seconds)", 
           y = "Total Features Found") +
      theme_minimal()
  }
  
  # Display plots
  print(p1)
  print(p2)
  if (exists("p3")) print(p3)
  
  # Calculate and display performance metrics
  cat("\nPERFORMANCE METRICS:\n")
  if (nrow(successful_results) > 0) {
    fastest <- successful_results[which.min(successful_results$execution_time), ]
    most_features <- successful_results[which.max(successful_results$total_features), ]
    cat("Fastest algorithm:", fastest$method, "(", round(fastest$execution_time, 2), "seconds )\n")
    cat("Most features found:", most_features$method, "(", most_features$total_features, "features )\n")
    # Calculate features per second metric
    successful_results$features_per_second <- successful_results$total_features / successful_results$execution_time
    most_efficient <- successful_results[which.max(successful_results$features_per_second), ]
    cat("Most efficient (features/second):", most_efficient$method, 
        "(", round(most_efficient$features_per_second, 1), "features/sec )\n")
  }
} else {
  cat("\nNo algorithms completed successfully. Check error messages above.\n")
}
cat("\nComparison completed!\n")
