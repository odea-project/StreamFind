
# Close the connection first
rm(engine_db)

# Delete the old database file
file.remove("engine.duckdb")

# Create a new instance with the updated schema
engine_db <- EngineDataBase$new(
  db = "engine.duckdb",
  id = "my_engine",
  data_type = "MassSpec"
)

# Now try setting metadata
engine_db$Metadata <- list(
  name = "Updated Analysis",
  description = "Advanced analysis",
  author = "Jane Smith",
  version = "1.2",
  instrument = "Q-TOF",
  sample_count = 150
)

engine_db$get_engine_info()
engine_db$get_table_info("AuditTrails")
engine_db$get_audit_trail()
engine_db$get_metadata()

fromJSON(engine_db$query_database("SELECT metadata FROM Engines")[1, 1])
engine_db$query_database("SELECT * FROM AuditTrails")


# test_data <- generate_test_data_cpp(100, 1000000)
# result1 <- test_hdf5_performance(test_data, "test_cpp_data.h5", enable_compression = FALSE, enable_chunking = FALSE)
# test_data2 <- test_hdf5_read_performance("test_cpp_data.h5", dataset_name = "matrix_data")

source("duckdb_performance.R")

# Generate mixed data type test data
generate_mixed_test_data <- function(n_cols_numeric = 98, n_cols_string = 2, n_rows = 100000) {
  result <- list()

  # Generate numeric columns
  for (i in 1:n_cols_numeric) {
    result[[paste0("num_col_", i)]] <- rnorm(n_rows, mean = i * 0.1, sd = 1.0)
  }

  # Generate string columns
  for (i in 1:n_cols_string) {
    # Create realistic string data
    if (i == 1) {
      # Category-like strings
      categories <- c("Type_A", "Type_B", "Type_C", "Type_D", "Type_E")
      result[[paste0("str_col_", i)]] <- sample(categories, n_rows, replace = TRUE)
    } else {
      # ID-like strings with varying lengths
      result[[paste0("str_col_", i)]] <- paste0("ID_", sprintf("%08d", sample(1:n_rows, n_rows, replace = TRUE)))
    }
  }

  return(result)
}

# Test function to compare HDF5 vs DuckDB with mixed data types including strings
test_mixed_data_comparison <- function(n_cols_numeric = 48, n_cols_string = 2, n_rows = 50000,
                                      base_name = "mixed_comparison_test", verbose = TRUE) {

  if (verbose) {
    cat("=== Mixed Data Type Performance Comparison ===\n")
    cat("Configuration:\n")
    cat("  Numeric columns:", n_cols_numeric, "\n")
    cat("  String columns:", n_cols_string, "\n")
    cat("  Rows:", n_rows, "\n\n")
  }

  # Generate test data
  if (verbose) cat("1. Generating mixed test data...\n")

  # Use R function for flexibility, but also test C++ generator
  test_data_r <- generate_mixed_test_data(n_cols_numeric, n_cols_string, n_rows)

  # Test with C++ generator as well (if available)
  test_data_cpp <- NULL
  tryCatch({
    test_data_cpp <- generate_mixed_test_data_cpp(n_cols_numeric, n_cols_string, n_rows)
    if (verbose) cat("   ✓ C++ data generator available\n")
  }, error = function(e) {
    if (verbose) cat("   ⚠ C++ data generator not available, using R version\n")
  })

  # Use C++ data if available, otherwise R data
  test_data <- if (!is.null(test_data_cpp)) test_data_cpp else test_data_r

  results <- list()

  # 2. Test HDF5 with native mixed data support
  if (verbose) cat("\n2. Testing HDF5 (native mixed data)...\n")
  hdf5_file <- paste0(base_name, "_hdf5.h5")

  hdf5_write_result <- NULL
  tryCatch({
    hdf5_write_result <- test_hdf5_mixed_performance(test_data, hdf5_file, FALSE, FALSE)
    if (verbose && !"error" %in% names(hdf5_write_result)) {
      cat("   ✓ HDF5 write completed:", hdf5_write_result$write_time_ms, "ms\n")
      cat("   ✓ File size:", round(hdf5_write_result$file_size_mb, 2), "MB\n")
    }
  }, error = function(e) {
    if (verbose) cat("   ✗ HDF5 write failed:", e$message, "\n")
    hdf5_write_result <<- list(error = paste("HDF5 write error:", e$message))
  })

  results$hdf5_write <- hdf5_write_result

  # 3. Test DuckDB with mixed data
  if (verbose) cat("\n3. Testing DuckDB (native mixed data)...\n")
  duckdb_file <- paste0(base_name, "_duckdb.duckdb")

  duckdb_write_result <- NULL
  tryCatch({
    duckdb_write_result <- test_duckdb_write_performance(test_data, duckdb_file, "mixed_data")
    if (verbose && !"error" %in% names(duckdb_write_result)) {
      cat("   ✓ DuckDB write completed:", duckdb_write_result$write_time_ms, "ms\n")
      cat("   ✓ File size:", round(duckdb_write_result$file_size_mb, 2), "MB\n")
    }
  }, error = function(e) {
    if (verbose) cat("   ✗ DuckDB write failed:", e$message, "\n")
    duckdb_write_result <<- list(error = paste("DuckDB write error:", e$message))
  })

  results$duckdb_write <- duckdb_write_result

  # 4. Test read performance
  if (verbose) cat("\n4. Testing read performance...\n")

  # HDF5 read
  hdf5_read_result <- NULL
  if (!"error" %in% names(results$hdf5_write)) {
    tryCatch({
      hdf5_read_result <- test_hdf5_mixed_read_performance(hdf5_file, "mixed_data")
      if (verbose && !"error" %in% names(hdf5_read_result)) {
        cat("   ✓ HDF5 read completed:", hdf5_read_result$performance$read_time_ms, "ms\n")
      }
    }, error = function(e) {
      if (verbose) cat("   ✗ HDF5 read failed:", e$message, "\n")
      hdf5_read_result <<- list(error = paste("HDF5 read error:", e$message))
    })
  }
  results$hdf5_read <- hdf5_read_result

  # DuckDB read
  duckdb_read_result <- NULL
  if (!"error" %in% names(results$duckdb_write)) {
    tryCatch({
      duckdb_read_result <- test_duckdb_read_performance(duckdb_file, "mixed_data")
      if (verbose && !"error" %in% names(duckdb_read_result)) {
        cat("   ✓ DuckDB read completed:", duckdb_read_result$performance$read_time_ms, "ms\n")
      }
    }, error = function(e) {
      if (verbose) cat("   ✗ DuckDB read failed:", e$message, "\n")
      duckdb_read_result <<- list(error = paste("DuckDB read error:", e$message))
    })
  }
  results$duckdb_read <- duckdb_read_result

  # 5. Data integrity verification
  if (verbose) cat("\n5. Verifying data integrity...\n")

  integrity_check <- list(
    hdf5_data_valid = FALSE,
    duckdb_data_valid = FALSE,
    data_matches = FALSE
  )

  # Check HDF5 data integrity
  if (!"error" %in% names(results$hdf5_read)) {
    hdf5_data <- results$hdf5_read$data
    expected_cols <- length(test_data)
    actual_cols <- length(hdf5_data)

    integrity_check$hdf5_data_valid <- (expected_cols == actual_cols)
    if (verbose) {
      if (integrity_check$hdf5_data_valid) {
        cat("   ✓ HDF5 data integrity: OK (", actual_cols, "/", expected_cols, " columns)\n")
      } else {
        cat("   ✗ HDF5 data integrity: FAILED (", actual_cols, "/", expected_cols, " columns)\n")
      }
    }
  }

  # Check DuckDB data integrity
  if (!"error" %in% names(results$duckdb_read)) {
    duckdb_data <- results$duckdb_read$data
    expected_cols <- length(test_data)
    actual_cols <- length(duckdb_data)

    integrity_check$duckdb_data_valid <- (expected_cols == actual_cols)
    if (verbose) {
      if (integrity_check$duckdb_data_valid) {
        cat("   ✓ DuckDB data integrity: OK (", actual_cols, "/", expected_cols, " columns)\n")
      } else {
        cat("   ✗ DuckDB data integrity: FAILED (", actual_cols, "/", expected_cols, " columns)\n")
      }
    }
  }

  results$integrity_check <- integrity_check

  # 6. Performance summary
  if (verbose) {
    cat("\n=== Performance Summary ===\n")
    cat(sprintf("%-15s %10s %10s %10s %12s %15s\n",
                "System", "Write(ms)", "Read(ms)", "Size(MB)", "Throughput", "String Handling"))
    cat(sprintf("%-15s %10s %10s %10s %12s %15s\n",
                "---------------", "--------", "--------", "--------", "----------", "---------------"))

    # HDF5 row
    if (!"error" %in% names(results$hdf5_write)) {
      hdf5_write_time <- results$hdf5_write$write_time_ms
      hdf5_read_time <- if (!"error" %in% names(results$hdf5_read)) {
        results$hdf5_read$performance$read_time_ms
      } else "N/A"
      hdf5_size <- results$hdf5_write$file_size_mb
      hdf5_throughput <- if ("write_throughput_mbps" %in% names(results$hdf5_write)) {
        paste0(round(results$hdf5_write$write_throughput_mbps, 1), " MB/s")
      } else "N/A"

      cat(sprintf("%-15s %10.0f %10s %10.2f %12s %15s\n",
                  "HDF5", hdf5_write_time, hdf5_read_time, hdf5_size, hdf5_throughput, "Native"))
    } else {
      cat(sprintf("%-15s %10s %10s %10s %12s %15s\n",
                  "HDF5", "ERROR", "ERROR", "N/A", "N/A", "Native"))
    }

    # DuckDB row
    if (!"error" %in% names(results$duckdb_write)) {
      duckdb_write_time <- results$duckdb_write$write_time_ms
      duckdb_read_time <- if (!"error" %in% names(results$duckdb_read)) {
        results$duckdb_read$performance$read_time_ms
      } else "N/A"
      duckdb_size <- results$duckdb_write$file_size_mb
      duckdb_throughput <- if ("write_throughput_mbps" %in% names(results$duckdb_write)) {
        paste0(round(results$duckdb_write$write_throughput_mbps, 1), " MB/s")
      } else "N/A"

      cat(sprintf("%-15s %10.0f %10s %10.2f %12s %15s\n",
                  "DuckDB", duckdb_write_time, duckdb_read_time, duckdb_size, duckdb_throughput, "Native"))
    } else {
      cat(sprintf("%-15s %10s %10s %10s %12s %15s\n",
                  "DuckDB", "ERROR", "ERROR", "N/A", "N/A", "Native"))
    }

    # Performance comparison
    if (!"error" %in% names(results$hdf5_write) && !"error" %in% names(results$duckdb_write)) {
      cat("\n=== Speed Comparison ===\n")
      hdf5_faster_write <- results$hdf5_write$write_time_ms < results$duckdb_write$write_time_ms
      write_ratio <- if (hdf5_faster_write) {
        results$duckdb_write$write_time_ms / results$hdf5_write$write_time_ms
      } else {
        results$hdf5_write$write_time_ms / results$duckdb_write$write_time_ms
      }

      cat("Write Performance: ",
          if (hdf5_faster_write) "HDF5" else "DuckDB",
          " is ", round(write_ratio, 2), "x faster\n")

      if (!"error" %in% names(results$hdf5_read) && !"error" %in% names(results$duckdb_read)) {
        hdf5_faster_read <- results$hdf5_read$performance$read_time_ms < results$duckdb_read$performance$read_time_ms
        read_ratio <- if (hdf5_faster_read) {
          results$duckdb_read$performance$read_time_ms / results$hdf5_read$performance$read_time_ms
        } else {
          results$hdf5_read$performance$read_time_ms / results$duckdb_read$performance$read_time_ms
        }

        cat("Read Performance: ",
            if (hdf5_faster_read) "HDF5" else "DuckDB",
            " is ", round(read_ratio, 2), "x faster\n")
      }
    }

    cat("\n")
  }

  # Store test configuration
  results$test_config <- list(
    n_cols_numeric = n_cols_numeric,
    n_cols_string = n_cols_string,
    n_rows = n_rows,
    total_columns = n_cols_numeric + n_cols_string
  )

  return(results)
}

# Quick test function with pre-defined scenarios
quick_string_test <- function(scenario = "small") {

  scenarios <- list(
    small = list(n_cols_numeric = 8, n_cols_string = 2, n_rows = 1000),
    medium = list(n_cols_numeric = 48, n_cols_string = 2, n_rows = 50000),
    large = list(n_cols_numeric = 98, n_cols_string = 2, n_rows = 100000),
    string_heavy = list(n_cols_numeric = 10, n_cols_string = 10, n_rows = 25000)
  )

  if (!scenario %in% names(scenarios)) {
    stop("Available scenarios: ", paste(names(scenarios), collapse = ", "))
  }

  config <- scenarios[[scenario]]
  cat("Running", scenario, "scenario...\n")

  return(test_mixed_data_comparison(
    n_cols_numeric = config$n_cols_numeric,
    n_cols_string = config$n_cols_string,
    n_rows = config$n_rows,
    base_name = paste0("quick_test_", scenario)
  ))
}


test_mixed2 <- test_mixed_data_comparison(
  n_cols_numeric = 76,
  n_cols_string = 2,
  n_rows = 1000000,
  base_name = "mixed_comparison_test",
  verbose = TRUE
)
