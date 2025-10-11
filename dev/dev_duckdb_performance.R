# Load required libraries for DuckDB
library(DBI)
library(duckdb)

#' Test DuckDB write performance with large matrix data
#'
#' @param data_list List containing numeric vectors representing columns
#' @param db_name Name of the DuckDB database file
#' @param table_name Name of the table to create
#' @param compression Compression method: "uncompressed", "gzip", "snappy", or "zstd"
#' @return List containing performance metrics and file information
test_duckdb_write_performance <- function(data_list,
                                          db_name = "performance_test.duckdb",
                                          table_name = "matrix_data",
                                          compression = "uncompressed") {

  result <- list()

  tryCatch({
    # Start timing
    start_time <- Sys.time()

    # Validate input
    if (length(data_list) == 0) {
      stop("Input list is empty")
    }

    n_cols <- length(data_list)
    n_rows <- length(data_list[[1]])

    # Check all columns have same length and detect data types
    column_types <- character(n_cols)
    for (i in seq_along(data_list)) {
      if (length(data_list[[i]]) != n_rows) {
        stop("All columns must have the same length")
      }
      # Detect column type
      if (is.character(data_list[[i]])) {
        column_types[i] <- "character"
      } else if (is.numeric(data_list[[i]])) {
        column_types[i] <- "numeric"
      } else if (is.logical(data_list[[i]])) {
        column_types[i] <- "logical"
      } else {
        column_types[i] <- "other"
      }
    }

    cat("DuckDB Write Performance Test:\n")
    cat("  Dimensions:", n_rows, "x", n_cols, "\n")
    cat("  Column types:", paste(table(column_types), "x", names(table(column_types)), collapse = ", "), "\n")
    cat("  Compression:", compression, "\n")

    # Convert list to data.frame
    conversion_start <- Sys.time()

    # Create column names
    col_names <- paste0("col_", seq_len(n_cols))
    names(data_list) <- col_names

    # Convert to data.frame
    df <- data.frame(data_list)

    conversion_time <- Sys.time()
    conversion_duration_ms <- as.numeric(difftime(conversion_time, conversion_start, units = "secs")) * 1000

    # Remove existing database file if it exists
    if (file.exists(db_name)) {
      file.remove(db_name)
    }

    # Connect to DuckDB
    con <- dbConnect(duckdb::duckdb(), dbdir = db_name)

    write_start_time <- Sys.time()

    # Write table
    dbWriteTable(con, table_name, df, overwrite = TRUE)

    write_end_time <- Sys.time()

    # Force write to disk
    dbExecute(con, "CHECKPOINT")

    # Close connection
    dbDisconnect(con)

    end_time <- Sys.time()

    # Calculate timing
    write_duration_ms <- as.numeric(difftime(write_end_time, write_start_time, units = "secs")) * 1000
    total_duration_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    # Get file size
    file_size <- file.info(db_name)$size

    # Create result list
    result$db_name <- db_name
    result$table_name <- table_name
    result$dimensions <- c(n_rows, n_cols)
    result$conversion_time_ms <- round(conversion_duration_ms, 2)
    result$write_time_ms <- round(write_duration_ms, 2)
    result$total_time_ms <- round(total_duration_ms, 2)
    result$file_size_bytes <- file_size
    result$file_size_mb <- file_size / (1024^2)
    # Calculate data size based on actual column types
    estimated_size_bytes <- 0
    for (i in seq_along(data_list)) {
      if (is.character(data_list[[i]])) {
        # Estimate string size (average character length * 1 byte + overhead)
        avg_string_length <- mean(nchar(data_list[[i]]), na.rm = TRUE)
        estimated_size_bytes <- estimated_size_bytes + (n_rows * (avg_string_length + 8))  # +8 for overhead
      } else if (is.numeric(data_list[[i]])) {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)  # 8 bytes per double
      } else if (is.logical(data_list[[i]])) {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 1)  # 1 byte per logical
      } else {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)  # Default estimate
      }
    }
    result$data_size_mb <- estimated_size_bytes / (1024^2)
    result$compression <- compression

    # Calculate compression ratio
    result$compression_ratio <- result$data_size_mb / result$file_size_mb

    # Calculate write throughput
    result$write_throughput_mbps <- if (write_duration_ms > 0) {
      (result$data_size_mb * 1000) / write_duration_ms
    } else {
      0
    }

    # Print results
    cat("\nDuckDB Write Performance Results:\n")
    cat("  Database:", db_name, "\n")
    cat("  Table:", table_name, "\n")
    cat("  Compression:", compression, "\n")
    cat("  Conversion time:", round(conversion_duration_ms, 2), "ms\n")
    cat("  Write time:", round(write_duration_ms, 2), "ms\n")
    cat("  Total time:", round(total_duration_ms, 2), "ms\n")
    cat("  File size:", round(file_size / (1024^2), 3), "MB\n")
    cat("  Raw data size:", round(result$data_size_mb, 3), "MB\n")
    cat("  Compression ratio:", round(result$compression_ratio, 2), ":1\n")
    cat("  Write throughput:", round(result$write_throughput_mbps, 2), "MB/s\n")

  }, error = function(e) {
    result$error <- paste("Error:", e$message)
    cat("ERROR:", e$message, "\n")
  })

  return(result)
}

#' Test DuckDB read performance for matrix data
#'
#' @param db_name Name of the DuckDB database file to read
#' @param table_name Name of the table to read
#' @return List containing the data as columns and performance metrics
test_duckdb_read_performance <- function(db_name, table_name = "matrix_data") {

  result <- list()
  performance <- list()
  table_info <- list()

  tryCatch({
    # Start timing
    start_time <- Sys.time()

    # Check if database file exists
    if (!file.exists(db_name)) {
      stop("Database file does not exist: ", db_name)
    }

    # Get file size
    file_size <- file.info(db_name)$size

    # Connect to DuckDB
    con <- dbConnect(duckdb::duckdb(), dbdir = db_name, read_only = TRUE)

    # Check if table exists
    tables <- dbListTables(con)
    if (!table_name %in% tables) {
      dbDisconnect(con)
      stop("Table '", table_name, "' not found in database: ", db_name)
    }

    # Get table information
    table_schema <- dbGetQuery(con, paste0("PRAGMA table_info('", table_name, "')"))
    row_count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table_name))$count
    col_count <- nrow(table_schema)

    table_info$name <- table_name
    table_info$dimensions <- c(row_count, col_count)
    table_info$total_elements <- row_count * col_count
    table_info$column_names <- table_schema$name
    table_info$column_types <- table_schema$type

    cat("Reading DuckDB table:\n")
    cat("  Database:", db_name, "\n")
    cat("  Table:", table_name, "\n")
    cat("  Dimensions:", row_count, "x", col_count, "\n")
    cat("  Total elements:", format(row_count * col_count, big.mark = ","), "\n")

    read_start_time <- Sys.time()

    # Read the data
    df <- dbReadTable(con, table_name)

    read_end_time <- Sys.time()

    # Convert to list format (column-wise)
    conversion_start_time <- Sys.time()

    data_list <- as.list(df)

    end_time <- Sys.time()

    # Close connection
    dbDisconnect(con)

    # Calculate timing
    read_duration_ms <- as.numeric(difftime(read_end_time, read_start_time, units = "secs")) * 1000
    conversion_duration_ms <- as.numeric(difftime(end_time, conversion_start_time, units = "secs")) * 1000
    total_duration_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    # Store performance metrics
    performance$db_name <- db_name
    performance$table_name <- table_name
    performance$dimensions <- c(row_count, col_count)
    performance$read_time_ms <- round(read_duration_ms, 2)
    performance$conversion_time_ms <- round(conversion_duration_ms, 2)
    performance$total_time_ms <- round(total_duration_ms, 2)
    performance$file_size_bytes <- file_size
    performance$file_size_mb <- file_size / (1024^2)
    # Calculate data size based on actual column types in the table
    estimated_size_bytes <- 0
    for (i in seq_along(data_list)) {
      col_data <- data_list[[i]]
      if (is.character(col_data)) {
        avg_string_length <- mean(nchar(col_data), na.rm = TRUE)
        estimated_size_bytes <- estimated_size_bytes + (row_count * (avg_string_length + 8))
      } else if (is.numeric(col_data)) {
        estimated_size_bytes <- estimated_size_bytes + (row_count * 8)
      } else if (is.logical(col_data)) {
        estimated_size_bytes <- estimated_size_bytes + (row_count * 1)
      } else {
        estimated_size_bytes <- estimated_size_bytes + (row_count * 8)
      }
    }
    performance$data_size_mb <- estimated_size_bytes / (1024^2)

    # Calculate throughput
    performance$read_throughput_mbps <- performance$data_size_mb / (read_duration_ms / 1000)

    # Print results
    cat("\nDuckDB Read Performance Results:\n")
    cat("  Read time:", round(read_duration_ms, 2), "ms\n")
    cat("  Conversion time:", round(conversion_duration_ms, 2), "ms\n")
    cat("  Total time:", round(total_duration_ms, 2), "ms\n")
    cat("  File size:", round(file_size / (1024^2), 3), "MB\n")
    cat("  Data size:", round(performance$data_size_mb, 3), "MB\n")
    cat("  Read throughput:", round(performance$read_throughput_mbps, 2), "MB/s\n")

    # Prepare final result
    result$data <- data_list
    result$performance <- performance
    result$table_info <- table_info

    # Add some basic statistics about the data
    data_stats <- list()
    if (col_count > 0 && row_count > 0) {
      first_col <- data_list[[1]]
      if (is.numeric(first_col)) {
        sample_size <- min(1000, length(first_col))
        sample_data <- first_col[1:sample_size]

        data_stats$sample_mean_first_col <- mean(sample_data, na.rm = TRUE)
        data_stats$sample_var_first_col <- var(sample_data, na.rm = TRUE)
        data_stats$sample_min_first_col <- min(sample_data, na.rm = TRUE)
        data_stats$sample_max_first_col <- max(sample_data, na.rm = TRUE)
        data_stats$sample_size <- sample_size
      }
    }
    result$data_stats <- data_stats

  }, error = function(e) {
    result$error <- paste("Error:", e$message)
    cat("ERROR:", e$message, "\n")
  })

  return(result)
}

#' Compare HDF5 vs DuckDB performance
#'
#' @param data_list Test data as list of numeric vectors
#' @param base_name Base name for output files
#' @return List containing results from both systems and comparison metrics
compare_hdf5_vs_duckdb <- function(data_list, base_name = "comparison_test") {

  cat("=== HDF5 vs DuckDB Performance Comparison ===\n\n")

  results <- list()

  # Test HDF5 - uncompressed
  cat("1. Testing HDF5 (uncompressed)...\n")
  hdf5_file <- paste0(base_name, "_hdf5.h5")
  results$hdf5_uncompressed <- test_hdf5_performance(data_list, hdf5_file, FALSE, FALSE)

  # Test HDF5 - compressed
  cat("\n2. Testing HDF5 (compressed)...\n")
  hdf5_comp_file <- paste0(base_name, "_hdf5_compressed.h5")
  results$hdf5_compressed <- test_hdf5_performance(data_list, hdf5_comp_file, TRUE, TRUE)

  # Test DuckDB - uncompressed
  cat("\n3. Testing DuckDB (uncompressed)...\n")
  duckdb_file <- paste0(base_name, "_duckdb.duckdb")
  results$duckdb_uncompressed <- test_duckdb_write_performance(data_list, duckdb_file, "matrix_data", "uncompressed")

  # Read tests
  cat("\n4. Testing read performance...\n")
  if (!"error" %in% names(results$hdf5_uncompressed)) {
    results$hdf5_read <- test_hdf5_read_performance(hdf5_file)
  }

  if (!"error" %in% names(results$duckdb_uncompressed)) {
    results$duckdb_read <- test_duckdb_read_performance(duckdb_file)
  }

  # Generate comparison report
  cat("\n=== Performance Comparison Report ===\n")

  # Write performance comparison
  cat("\nWrite Performance:\n")
  cat(sprintf("%-20s %12s %12s %12s %12s\n", "System", "Write(ms)", "Total(ms)", "Size(MB)", "Ratio"))
  cat(sprintf("%-20s %12s %12s %12s %12s\n", "--------------------", "--------", "--------", "--------", "--------"))

  systems <- c("hdf5_uncompressed", "hdf5_compressed", "duckdb_uncompressed")
  for (sys in systems) {
    r <- results[[sys]]
    if (!"error" %in% names(r)) {
      ratio <- if ("compression_ratio" %in% names(r)) sprintf("%.1f:1", r$compression_ratio) else "N/A"
      cat(sprintf("%-20s %12.0f %12.0f %12.2f %12s\n",
                  gsub("_", " ", sys), r$write_time_ms, r$total_time_ms, r$file_size_mb, ratio))
    }
  }

  # Read performance comparison
  if ("hdf5_read" %in% names(results) && "duckdb_read" %in% names(results)) {
    cat("\nRead Performance:\n")
    cat(sprintf("%-20s %12s %12s %12s\n", "System", "Read(ms)", "Total(ms)", "Speed(MB/s)"))
    cat(sprintf("%-20s %12s %12s %12s\n", "--------------------", "--------", "--------", "--------"))

    read_systems <- list("HDF5" = results$hdf5_read, "DuckDB" = results$duckdb_read)
    for (name in names(read_systems)) {
      r <- read_systems[[name]]
      if (!"error" %in% names(r)) {
        perf <- r$performance
        cat(sprintf("%-20s %12.0f %12.0f %12.1f\n",
                    name, perf$read_time_ms, perf$total_time_ms, perf$read_throughput_mbps))
      }
    }
  }

  return(results)
}
