#' @title Filesystem Performance Evaluation for HDF5, DuckDB, and SQLite
#' 
#' @description
#' This script provides comprehensive performance testing for different data storage formats:
#' - HDF5: High-performance binary format (compiled via Makevars and load_all)
#' - DuckDB: Analytical database with compression support
#' - SQLite: Lightweight embedded database
#' 
#' The evaluation tests both read and write performance across different data sizes
#' and includes support for mixed data types (numeric and character columns).
#' 
#' Data sizes tested (as specified in the requirements):
#' - 10x10 (100 elements)
#' - 1000x1000 (1M elements)
#' - 100000x100000 (10B elements)
#' - 1000000x1000000 (1T elements)
#' 
#' Features:
#' - Configurable number of character columns for testing non-numeric data
#' - Detailed performance metrics (write time, read time, file size, throughput)
#' - Comparison reports across all formats
#' - Support for HDF5 compression and chunking
#' - Proper cleanup functions for test files
#' 
#' @author StreamFind Development Team
#' @date 2024

# Load required libraries
library(DBI)
library(duckdb)
library(RSQLite)
library(data.table)

# Note: HDF5 functions are available via load_all() which compiles the C++ code through Makevars

#' Generate test data with specified dimensions and character columns
#'
#' @param n_rows Number of rows
#' @param n_cols Number of numeric columns
#' @param number_character_cols Number of character columns to add
#' @return data.table with numeric and character columns
generate_test_data <- function(n_rows, n_cols, number_character_cols = 0) {
  
  # Generate numeric data
  data_list <- list()
  
  # Add numeric columns
  for (i in seq_len(n_cols)) {
    data_list[[paste0("num_", i)]] <- rnorm(n_rows, mean = i * 10, sd = 5)
  }
  
  # Add character columns if requested
  if (number_character_cols > 0) {
    char_samples <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta")
    
    for (i in seq_len(number_character_cols)) {
      # Generate character data with varying lengths
      char_data <- sample(char_samples, n_rows, replace = TRUE)
      # Add some variation in string length
      char_data <- paste0(char_data, "_", sample(100:999, n_rows, replace = TRUE))
      data_list[[paste0("char_", i)]] <- char_data
    }
  }
  
  return(data.table::as.data.table(data_list))
}

#' Test HDF5 write performance
#'
#' @param data_dt data.table with the data to write
#' @param file_name Name of the HDF5 file
#' @param enable_compression Enable HDF5 compression
#' @param enable_chunking Enable HDF5 chunking
#' @return List containing performance metrics and file information
#' @note HDF5 functions are available via load_all() which compiles the C++ code through Makevars
test_hdf5_write_performance_api <- function(
  data_dt, 
  file_name = "performance_test.h5",
  enable_compression = FALSE, 
  enable_chunking = FALSE) {
  
  result <- list()
  
  tryCatch({
    # Start timing
    start_time <- Sys.time()
    
    # Convert data.table to list format expected by HDF5 mixed function
    # Include both numeric and character columns (mixed data types supported)
    data_list <- as.list(data_dt)
    n_rows <- nrow(data_dt)
    n_cols <- ncol(data_dt)
    # Count column types for reporting
    numeric_cols <- sapply(data_dt, is.numeric)
    character_cols <- sapply(data_dt, is.character)
    n_numeric <- sum(numeric_cols)
    n_character <- sum(character_cols)
    cat("HDF5 Mixed Write Performance Test:\n")
    cat("  Dimensions:", n_rows, "x", n_cols, "(", n_numeric, "numeric,", n_character, "character)\n")
    cat("  Compression:", enable_compression, "\n")
    cat("  Chunking:", enable_chunking, "\n")
    # Remove existing file if it exists
    if (file.exists(file_name)) {
      file.remove(file_name)
    }
    # Call the C++ HDF5 mixed function (available via load_all)
    # Check if we have access to the C++ function
    if (exists("test_hdf5_mixed_performance", envir = .GlobalEnv) || 
        exists("test_hdf5_mixed_performance", envir = asNamespace("StreamFind"))) {
      # Try to call the C++ function directly
      cpp_result <- do.call("test_hdf5_mixed_performance", list(data_list, file_name, enable_compression, enable_chunking))
    } else {
      stop("HDF5 mixed C++ function not available. Please run load_all() first.")
    }
    
    # Check if C++ operation was successful
    if (!"success" %in% names(cpp_result) || !cpp_result$success) {
      if ("error" %in% names(cpp_result)) {
        stop("C++ HDF5 operation failed: ", cpp_result$error)
      } else {
        stop("C++ HDF5 operation failed with unknown error")
      }
    }
    
    # Get file information in R
    if (file.exists(file_name)) {
      file_info <- file.info(file_name)
      file_size_bytes <- file_info$size
      file_size_mb <- file_size_bytes / (1024^2)
    } else {
      stop("HDF5 file was not created successfully")
    }
    
    # Calculate data size based on actual column types
    estimated_size_bytes <- 0
    for (col_name in names(data_dt)) {
      col_data <- data_dt[[col_name]]
      if (is.character(col_data)) {
        # Estimate string size (average character length * 1 byte + overhead)
        avg_string_length <- mean(nchar(col_data), na.rm = TRUE)
        estimated_size_bytes <- estimated_size_bytes + (n_rows * (avg_string_length + 8))  # +8 for overhead
      } else if (is.numeric(col_data)) {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)  # 8 bytes per double
      } else if (is.logical(col_data)) {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 1)  # 1 byte per logical
      } else {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)  # Default estimate
      }
    }
    data_size_mb <- estimated_size_bytes / (1024^2)
    
    # Combine C++ timing results with R-calculated metrics
    result <- cpp_result
    result$file_size_bytes <- file_size_bytes
    result$file_size_mb <- file_size_mb
    result$data_size_mb <- data_size_mb
    result$total_cols_original <- ncol(data_dt)
    result$numeric_cols <- n_numeric
    result$character_cols <- n_character
    
    # Calculate compression ratio
    if (data_size_mb > 0) {
      result$compression_ratio <- data_size_mb / file_size_mb
    } else {
      result$compression_ratio <- 1.0
    }
    
    # Calculate write throughput
    if (result$write_time_ms > 0) {
      result$write_throughput_mbps <- (data_size_mb * 1000) / result$write_time_ms
    } else {
      result$write_throughput_mbps <- 0
    }
    
    # Print additional results calculated in R
    cat("  File size:", round(file_size_mb, 3), "MB\n")
    cat("  Raw data size:", round(data_size_mb, 3), "MB\n")
    cat("  Compression ratio:", round(result$compression_ratio, 2), ":1\n")
    cat("  Write throughput:", round(result$write_throughput_mbps, 2), "MB/s\n")
  }, error = function(e) {
    result$error <- paste("Error:", e$message)
    cat("ERROR:", e$message, "\n")
  })
  return(result)
}

#' Test HDF5 read performance
#'
#' @param file_name Name of the HDF5 file to read
#' @param dataset_name Name of the dataset to read
#' @return List containing the data and performance metrics
#' @note HDF5 functions are available via load_all() which compiles the C++ code through Makevars
test_hdf5_read_performance_api <- function(file_name, group_name = "mixed_data") {
  result <- list()
  
  tryCatch({
    # Check if file exists
    if (!file.exists(file_name)) {
      stop("HDF5 file does not exist: ", file_name)
    }
    
    # Call the C++ HDF5 mixed read function (available via load_all)
    # Check if we have access to the C++ function
    if (exists("test_hdf5_mixed_read_performance", envir = .GlobalEnv) || 
        exists("test_hdf5_mixed_read_performance", envir = asNamespace("StreamFind"))) {
      # Try to call the C++ function directly
      cpp_result <- do.call("test_hdf5_mixed_read_performance", list(file_name, group_name))
    } else {
      stop("HDF5 mixed C++ read function not available. Please run load_all() first.")
    }
    
    # Check if C++ operation was successful
    if (!"success" %in% names(cpp_result$performance) || !cpp_result$performance$success) {
      if ("error" %in% names(cpp_result)) {
        stop("C++ HDF5 read operation failed: ", cpp_result$error)
      } else {
        stop("C++ HDF5 read operation failed with unknown error")
      }
    }
    
    # Get file information in R
    file_info <- file.info(file_name)
    file_size_bytes <- file_info$size
    file_size_mb <- file_size_bytes / (1024^2)
    
    # Extract data and basic info from C++ result
    data_dt <- cpp_result$data
    performance <- cpp_result$performance
    n_rows <- performance$n_rows
    n_numeric_cols <- performance$n_numeric_cols
    n_string_cols <- performance$n_string_cols
    
    # Calculate data size based on actual data returned
    estimated_size_bytes <- 0
    if (length(data_dt) > 0 && n_rows > 0) {
      for (col_name in names(data_dt)) {
        col_data <- data_dt[[col_name]]
        if (is.character(col_data)) {
          avg_string_length <- mean(nchar(col_data), na.rm = TRUE)
          estimated_size_bytes <- estimated_size_bytes + (n_rows * (avg_string_length + 8))
        } else if (is.numeric(col_data)) {
          estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)
        } else if (is.logical(col_data)) {
          estimated_size_bytes <- estimated_size_bytes + (n_rows * 1)
        } else {
          estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)
        }
      }
    }
    data_size_mb <- estimated_size_bytes / (1024^2)
    
    # Add R-calculated metrics to performance
    performance$file_size_bytes <- file_size_bytes
    performance$file_size_mb <- file_size_mb
    performance$data_size_mb <- data_size_mb
    
    # Calculate read throughput
    if (performance$read_time_ms > 0 && data_size_mb > 0) {
      performance$read_throughput_mbps <- (data_size_mb * 1000) / performance$read_time_ms
    } else {
      performance$read_throughput_mbps <- 0
    }
    
    # Prepare final result
    result$data <- data_dt
    result$performance <- performance
    
    # Print additional results calculated in R
    cat("  File size:", round(file_size_mb, 3), "MB\n")
    cat("  Data size:", round(data_size_mb, 3), "MB\n")
    cat("  Read throughput:", round(performance$read_throughput_mbps, 2), "MB/s\n")
    
  }, error = function(e) {
    result$error <- paste("Error:", e$message)
    cat("ERROR:", e$message, "\n")
  })
  
  return(result)
}

#' Test DuckDB write performance
#'
#' @param data_dt data.table with the data to write
#' @param db_name Name of the DuckDB database file
#' @param table_name Name of the table to create
#' @param compression Compression method: "uncompressed", "gzip", "snappy", or "zstd"
#' @return List containing performance metrics and file information
test_duckdb_write_performance <- function(
  data_dt,
  db_name = "performance_test.duckdb",
  table_name = "matrix_data",
  compression = "uncompressed") {

  result <- list()

  tryCatch({
    # Start timing
    start_time <- Sys.time()

    # Validate input
    if (nrow(data_dt) == 0 || ncol(data_dt) == 0) {
      stop("Input data.table is empty")
    }

    n_rows <- nrow(data_dt)
    n_cols <- ncol(data_dt)

    # Detect column types
    column_types <- sapply(data_dt, function(x) {
      if (is.character(x)) "character"
      else if (is.numeric(x)) "numeric"
      else if (is.logical(x)) "logical"
      else "other"
    })

    cat("DuckDB Write Performance Test:\n")
    cat("  Dimensions:", n_rows, "x", n_cols, "\n")
    cat("  Column types:", paste(table(column_types), "x", names(table(column_types)), collapse = ", "), "\n")
    cat("  Compression:", compression, "\n")

    # Convert to data.frame for DuckDB
    conversion_start <- Sys.time()
    df <- as.data.frame(data_dt)
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
    for (col_name in names(data_dt)) {
      col_data <- data_dt[[col_name]]
      if (is.character(col_data)) {
        # Estimate string size (average character length * 1 byte + overhead)
        avg_string_length <- mean(nchar(col_data), na.rm = TRUE)
        estimated_size_bytes <- estimated_size_bytes + (n_rows * (avg_string_length + 8))  # +8 for overhead
      } else if (is.numeric(col_data)) {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)  # 8 bytes per double
      } else if (is.logical(col_data)) {
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

#' Test DuckDB read performance
#'
#' @param db_name Name of the DuckDB database file to read
#' @param table_name Name of the table to read
#' @return List containing the data and performance metrics
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

    # Convert to data.table format
    conversion_start_time <- Sys.time()

    data_dt <- data.table::as.data.table(df)

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
    for (col_name in names(data_dt)) {
      col_data <- data_dt[[col_name]]
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
    result$data <- data_dt
    result$performance <- performance
    result$table_info <- table_info

    # Add some basic statistics about the data
    data_stats <- list()
    if (col_count > 0 && row_count > 0) {
      first_col <- data_dt[[1]]
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

#' Test SQLite write performance
#'
#' @param data_dt data.table with the data to write
#' @param db_name Name of the SQLite database file
#' @param table_name Name of the table to create
#' @return List containing performance metrics and file information
test_sqlite_write_performance <- function(data_dt,
                                         db_name = "performance_test.sqlite",
                                         table_name = "matrix_data") {

  result <- list()

  tryCatch({
    # Start timing
    start_time <- Sys.time()

    # Validate input
    if (nrow(data_dt) == 0 || ncol(data_dt) == 0) {
      stop("Input data.table is empty")
    }

    n_rows <- nrow(data_dt)
    n_cols <- ncol(data_dt)

    # Detect column types
    column_types <- sapply(data_dt, function(x) {
      if (is.character(x)) "character"
      else if (is.numeric(x)) "numeric"
      else if (is.logical(x)) "logical"
      else "other"
    })

    cat("SQLite Write Performance Test:\n")
    cat("  Dimensions:", n_rows, "x", n_cols, "\n")
    cat("  Column types:", paste(table(column_types), "x", names(table(column_types)), collapse = ", "), "\n")

    # Convert to data.frame for SQLite
    conversion_start <- Sys.time()
    df <- as.data.frame(data_dt)
    conversion_time <- Sys.time()
    conversion_duration_ms <- as.numeric(difftime(conversion_time, conversion_start, units = "secs")) * 1000

    # Remove existing database file if it exists
    if (file.exists(db_name)) {
      file.remove(db_name)
    }

    # Connect to SQLite
    con <- dbConnect(RSQLite::SQLite(), dbname = db_name)

    write_start_time <- Sys.time()

    # Write table
    dbWriteTable(con, table_name, df, overwrite = TRUE)

    write_end_time <- Sys.time()

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
    for (col_name in names(data_dt)) {
      col_data <- data_dt[[col_name]]
      if (is.character(col_data)) {
        # Estimate string size (average character length * 1 byte + overhead)
        avg_string_length <- mean(nchar(col_data), na.rm = TRUE)
        estimated_size_bytes <- estimated_size_bytes + (n_rows * (avg_string_length + 8))  # +8 for overhead
      } else if (is.numeric(col_data)) {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)  # 8 bytes per double
      } else if (is.logical(col_data)) {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 1)  # 1 byte per logical
      } else {
        estimated_size_bytes <- estimated_size_bytes + (n_rows * 8)  # Default estimate
      }
    }
    result$data_size_mb <- estimated_size_bytes / (1024^2)

    # Calculate write throughput
    result$write_throughput_mbps <- if (write_duration_ms > 0) {
      (result$data_size_mb * 1000) / write_duration_ms
    } else {
      0
    }

    # Print results
    cat("\nSQLite Write Performance Results:\n")
    cat("  Database:", db_name, "\n")
    cat("  Table:", table_name, "\n")
    cat("  Conversion time:", round(conversion_duration_ms, 2), "ms\n")
    cat("  Write time:", round(write_duration_ms, 2), "ms\n")
    cat("  Total time:", round(total_duration_ms, 2), "ms\n")
    cat("  File size:", round(file_size / (1024^2), 3), "MB\n")
    cat("  Raw data size:", round(result$data_size_mb, 3), "MB\n")
    cat("  Write throughput:", round(result$write_throughput_mbps, 2), "MB/s\n")

  }, error = function(e) {
    result$error <- paste("Error:", e$message)
    cat("ERROR:", e$message, "\n")
  })

  return(result)
}

#' Test SQLite read performance
#'
#' @param db_name Name of the SQLite database file to read
#' @param table_name Name of the table to read
#' @return List containing the data and performance metrics
test_sqlite_read_performance <- function(db_name, table_name = "matrix_data") {

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

    # Connect to SQLite
    con <- dbConnect(RSQLite::SQLite(), dbname = db_name)

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

    cat("Reading SQLite table:\n")
    cat("  Database:", db_name, "\n")
    cat("  Table:", table_name, "\n")
    cat("  Dimensions:", row_count, "x", col_count, "\n")
    cat("  Total elements:", format(row_count * col_count, big.mark = ","), "\n")

    read_start_time <- Sys.time()

    # Read the data
    df <- dbReadTable(con, table_name)

    read_end_time <- Sys.time()

    # Convert to data.table format
    conversion_start_time <- Sys.time()

    data_dt <- data.table::as.data.table(df)

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
    for (col_name in names(data_dt)) {
      col_data <- data_dt[[col_name]]
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
    cat("\nSQLite Read Performance Results:\n")
    cat("  Read time:", round(read_duration_ms, 2), "ms\n")
    cat("  Conversion time:", round(conversion_duration_ms, 2), "ms\n")
    cat("  Total time:", round(total_duration_ms, 2), "ms\n")
    cat("  File size:", round(file_size / (1024^2), 3), "MB\n")
    cat("  Data size:", round(performance$data_size_mb, 3), "MB\n")
    cat("  Read throughput:", round(performance$read_throughput_mbps, 2), "MB/s\n")

    # Prepare final result
    result$data <- data_dt
    result$performance <- performance
    result$table_info <- table_info

    # Add some basic statistics about the data
    data_stats <- list()
    if (col_count > 0 && row_count > 0) {
      first_col <- data_dt[[1]]
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

#' Estimate data size and warn for large datasets
#'
#' @param n_rows Number of rows
#' @param n_cols Number of numeric columns
#' @param char_cols Number of character columns
#' @param avg_char_length Average length of character strings (default: 15)
#' @return List with estimated size in MB and GB, plus warnings
estimate_data_size <- function(n_rows, n_cols, char_cols, avg_char_length = 15) {
  
  # Estimate size in bytes
  numeric_bytes <- n_rows * n_cols * 8  # 8 bytes per double
  char_bytes <- n_rows * char_cols * (avg_char_length + 8)  # avg string length + overhead
  total_bytes <- numeric_bytes + char_bytes
  
  # Convert to MB and GB
  size_mb <- total_bytes / (1024^2)
  size_gb <- total_bytes / (1024^3)
  
  # Generate warnings for large datasets
  warnings <- c()
  if (size_gb > 3) {
    warnings <- c(warnings, paste("WARNING: Dataset estimated at", round(size_gb, 2), 
                                 "GB - may exceed memory limits and take significant time"))
  } else if (size_gb > 1) {
    warnings <- c(warnings, paste("CAUTION: Dataset estimated at", round(size_gb, 2), 
                                 "GB - may take considerable time"))
  }
  
  if (n_rows * (n_cols + char_cols) > 10^8) {
    warnings <- c(warnings, paste("INFO: Dataset has", format(n_rows * (n_cols + char_cols), big.mark=","), 
                                 "total elements - processing may be slow"))
  }
  
  return(list(
    size_bytes = total_bytes,
    size_mb = size_mb,
    size_gb = size_gb,
    warnings = warnings,
    total_elements = n_rows * (n_cols + char_cols)
  ))
}

#' Run comprehensive filesystem performance evaluation
#'
#' @param test_configs List of test configurations. Each element should be a named list with:
#'   - rows: Number of rows
#'   - cols: Number of numeric columns  
#'   - char_cols: Number of character columns
#'   Names of list elements will be used as test identifiers.
#'   Example: list("small" = list(rows=100, cols=50, char_cols=2), 
#'                 "medium" = list(rows=1000, cols=500, char_cols=5))
#' @param base_name Base name for output files
#' @return List containing results from all systems and comparison metrics
run_filesystem_performance_evaluation <- function(test_configs = list(
                                                    "10x10" = list(rows=10, cols=10, char_cols=2),
                                                    "1000x1000" = list(rows=1000, cols=1000, char_cols=2),
                                                    "100000x100000" = list(rows=100000, cols=100000, char_cols=2)
                                                  ),
                                                  base_name = "perf_test") {
  
  cat("=== Comprehensive Filesystem Performance Evaluation ===\n\n")
  
  results <- list()
  
  for (test_name in names(test_configs)) {
    config <- test_configs[[test_name]]
    
    # Validate config structure
    if (!all(c("rows", "cols", "char_cols") %in% names(config))) {
      stop("Each test config must have 'rows', 'cols', and 'char_cols' elements")
    }
    
    n_rows <- config$rows
    n_cols <- config$cols
    char_cols <- config$char_cols
    
    # Estimate data size and show warnings
    size_est <- estimate_data_size(n_rows, n_cols, char_cols)
    
    cat("\n", rep("=", 60), "\n")
    cat("Testing configuration:", test_name, "\n")
    cat("Data dimensions:", n_rows, "rows x", n_cols, "numeric cols +", char_cols, "character cols\n")
    cat("Total columns:", n_cols + char_cols, "\n")
    cat("Estimated data size:", round(size_est$size_mb, 2), "MB (", round(size_est$size_gb, 3), "GB )\n")
    
    # Print warnings if any
    if (length(size_est$warnings) > 0) {
      for (warning in size_est$warnings) {
        cat("*** ", warning, " ***\n")
      }
    }
    
    cat(rep("=", 60), "\n\n")
    
    # Generate test data
    cat("Generating test data...\n")
    data_dt <- generate_test_data(n_rows, n_cols, char_cols)
    cat("Generated data:", nrow(data_dt), "x", ncol(data_dt), "\n")
    
    size_results <- list()
    
    # Test HDF5 (numeric only)
    cat("\n1. Testing HDF5 (uncompressed)...\n")
    hdf5_file <- paste0(base_name, "_", test_name, "_hdf5.h5")
    size_results$hdf5_uncompressed_write <- test_hdf5_write_performance_api(data_dt, hdf5_file, FALSE, FALSE)
    
    cat("\n2. Testing HDF5 (compressed)...\n")
    hdf5_comp_file <- paste0(base_name, "_", test_name, "_hdf5_comp.h5")
    size_results$hdf5_compressed_write <- test_hdf5_write_performance_api(data_dt, hdf5_comp_file, TRUE, TRUE)
    
    # Test DuckDB
    cat("\n3. Testing DuckDB (uncompressed)...\n")
    duckdb_file <- paste0(base_name, "_", test_name, ".duckdb")
    size_results$duckdb_write <- test_duckdb_write_performance(data_dt, duckdb_file, "matrix_data", "uncompressed")
    
    # Test SQLite
    cat("\n4. Testing SQLite...\n")
    sqlite_file <- paste0(base_name, "_", test_name, ".sqlite")
    size_results$sqlite_write <- test_sqlite_write_performance(data_dt, sqlite_file, "matrix_data")
    
    # Read tests
    cat("\n5. Testing read performance...\n")
    
    # HDF5 reads
    if (!"error" %in% names(size_results$hdf5_uncompressed_write)) {
      size_results$hdf5_uncompressed_read <- test_hdf5_read_performance_api(hdf5_file)
    }
    
    if (!"error" %in% names(size_results$hdf5_compressed_write)) {
      size_results$hdf5_compressed_read <- test_hdf5_read_performance_api(hdf5_comp_file)
    }
    
    # DuckDB read
    if (!"error" %in% names(size_results$duckdb_write)) {
      size_results$duckdb_read <- test_duckdb_read_performance(duckdb_file)
    }
    
    # SQLite read
    if (!"error" %in% names(size_results$sqlite_write)) {
      size_results$sqlite_read <- test_sqlite_read_performance(sqlite_file)
    }
    
    # Store results for this test configuration
    results[[test_name]] <- size_results
    
    # Generate comparison report for this configuration
    cat("\n=== Performance Report for", test_name, "===\n")
    
    # Write performance comparison
    cat("\nWrite Performance:\n")
    cat(sprintf("%-25s %12s %12s %12s %12s\n", "System", "Write(ms)", "Total(ms)", "Size(MB)", "Speed(MB/s)"))
    cat(sprintf("%-25s %12s %12s %12s %12s\n", "-------------------------", "--------", "--------", "--------", "--------"))
    
    write_systems <- list(
      "HDF5 (uncompressed)" = size_results$hdf5_uncompressed_write,
      "HDF5 (compressed)" = size_results$hdf5_compressed_write,
      "DuckDB" = size_results$duckdb_write,
      "SQLite" = size_results$sqlite_write
    )
    
    for (name in names(write_systems)) {
      r <- write_systems[[name]]
      if (!"error" %in% names(r)) {
        cat(sprintf("%-25s %12.0f %12.0f %12.2f %12.1f\n",
                    name, r$write_time_ms, r$total_time_ms, r$file_size_mb, r$write_throughput_mbps))
      } else {
        cat(sprintf("%-25s %12s %12s %12s %12s\n", name, "ERROR", "ERROR", "ERROR", "ERROR"))
      }
    }
    
    # Read performance comparison
    cat("\nRead Performance:\n")
    cat(sprintf("%-25s %12s %12s %12s\n", "System", "Read(ms)", "Total(ms)", "Speed(MB/s)"))
    cat(sprintf("%-25s %12s %12s %12s\n", "-------------------------", "--------", "--------", "--------"))
    
    read_systems <- list(
      "HDF5 (uncompressed)" = if("hdf5_uncompressed_read" %in% names(size_results)) size_results$hdf5_uncompressed_read$performance else NULL,
      "HDF5 (compressed)" = if("hdf5_compressed_read" %in% names(size_results)) size_results$hdf5_compressed_read$performance else NULL,
      "DuckDB" = if("duckdb_read" %in% names(size_results)) size_results$duckdb_read$performance else NULL,
      "SQLite" = if("sqlite_read" %in% names(size_results)) size_results$sqlite_read$performance else NULL
    )
    
    for (name in names(read_systems)) {
      r <- read_systems[[name]]
      if (!is.null(r) && !"error" %in% names(r)) {
        cat(sprintf("%-25s %12.0f %12.0f %12.1f\n",
                    name, r$read_time_ms, r$total_time_ms, r$read_throughput_mbps))
      } else {
        cat(sprintf("%-25s %12s %12s %12s\n", name, "N/A", "N/A", "N/A"))
      }
    }
  }
  
  # Generate overall summary
  cat("\n\n", rep("=", 80), "\n")
  cat("OVERALL PERFORMANCE SUMMARY\n")
  cat(rep("=", 80), "\n")
  
  for (test_name in names(results)) {
    cat("\nTest Configuration:", test_name, "\n")
    cat(rep("-", 40), "\n")
    
    test_res <- results[[test_name]]
    
    # Find fastest write and read for each system
    write_times <- list()
    read_times <- list()
    
    if (!"error" %in% names(test_res$hdf5_uncompressed_write)) {
      write_times[["HDF5"]] <- test_res$hdf5_uncompressed_write$write_time_ms
    }
    if (!"error" %in% names(test_res$duckdb_write)) {
      write_times[["DuckDB"]] <- test_res$duckdb_write$write_time_ms  
    }
    if (!"error" %in% names(test_res$sqlite_write)) {
      write_times[["SQLite"]] <- test_res$sqlite_write$write_time_ms
    }
    
    if ("hdf5_uncompressed_read" %in% names(test_res) && !"error" %in% names(test_res$hdf5_uncompressed_read)) {
      read_times[["HDF5"]] <- test_res$hdf5_uncompressed_read$performance$read_time_ms
    }
    if ("hdf5_compressed_read" %in% names(test_res) && !"error" %in% names(test_res$hdf5_compressed_read)) {
      read_times[["HDF5 (compressed)"]] <- test_res$hdf5_compressed_read$performance$read_time_ms
    }
    if ("duckdb_read" %in% names(test_res) && !"error" %in% names(test_res$duckdb_read)) {
      read_times[["DuckDB"]] <- test_res$duckdb_read$performance$read_time_ms
    }
    if ("sqlite_read" %in% names(test_res) && !"error" %in% names(test_res$sqlite_read)) {
      read_times[["SQLite"]] <- test_res$sqlite_read$performance$read_time_ms
    }
    
    if (length(write_times) > 0) {
      fastest_write <- names(write_times)[which.min(unlist(write_times))]
      cat("Fastest Write:", fastest_write, "(", round(write_times[[fastest_write]], 1), "ms )\n")
    }
    
    if (length(read_times) > 0) {
      fastest_read <- names(read_times)[which.min(unlist(read_times))]
      cat("Fastest Read: ", fastest_read, "(", round(read_times[[fastest_read]], 1), "ms )\n")
    }
  }
  
  return(results)
}

# Example usage function
#' Run example performance tests with summary data.table output
#'
#' @param test_configs List of test configurations. Each element should be a named list with:
#'   - rows: Number of rows
#'   - cols: Number of numeric columns  
#'   - char_cols: Number of character columns
#'   Names of list elements will be used as test identifiers.
#'   Example: list("small" = list(rows=100, cols=50, char_cols=2))
#' @return List containing: results (detailed results), summary_dt (data.table for plotting)
#' @export
#' @examples
#' \dontrun{
#' # Custom test configurations
#' configs <- list(
#'   "small_mixed" = list(rows=100, cols=50, char_cols=5),
#'   "medium_numeric" = list(rows=1000, cols=1000, char_cols=2),
#'   "large_strings" = list(rows=10000, cols=100, char_cols=20)
#' )
#' results <- run_example_tests(test_configs = configs)
#' 
#' # Access summary table for plotting
#' summary_dt <- results$summary_dt
#' print(summary_dt)
#' }
run_example_tests <- function(test_configs = list(
                                "small" = list(rows=100, cols=100, char_cols=2),
                                "medium" = list(rows=1000, cols=1000, char_cols=5),
                                "large" = list(rows=10000, cols=1000, char_cols=10),
                                "very_large" = list(rows=100000, cols=1000, char_cols=20),
                                "huge_3gb_plus" = list(rows=500000, cols=1000, char_cols=50)
                              )) {
  
  cat("=== Filesystem Performance Evaluation Example ===\n")
  cat("Test configurations:\n")
  for (name in names(test_configs)) {
    config <- test_configs[[name]]
    cat("  ", name, ":", config$rows, "rows x", config$cols, "cols +", config$char_cols, "char_cols\n")
  }
  cat("\n")
  
  # Test with different configurations
  results <- run_filesystem_performance_evaluation(
    test_configs = test_configs,
    base_name = "example_test"
  )
  
  # Create summary data.table for plotting
  cat("\n=== Creating Summary Data.Table for Plotting ===\n")
  
  # Helper function to safely extract values with defaults
  safe_extract <- function(obj, field, default = NA) {
    if (is.null(obj) || !field %in% names(obj) || is.null(obj[[field]]) || length(obj[[field]]) == 0) {
      return(default)
    }
    return(obj[[field]])
  }
  
  summary_rows <- list()
  
  for (test_name in names(results)) {
    test_res <- results[[test_name]]
    # Extract configuration info from the test name and results
    config <- test_configs[[test_name]]
    if (!is.null(config)) {
      n_rows <- config$rows
      n_cols <- config$cols  
      char_cols <- config$char_cols
      total_elements <- n_rows * (n_cols + char_cols)
    } else {
      # Fallback for legacy format
      n_rows <- NA
      n_cols <- NA
      char_cols <- NA
      total_elements <- NA
    }
    
    # Extract write performance metrics
    systems <- list(
      "HDF5_uncompressed" = test_res$hdf5_uncompressed_write,
      "HDF5_compressed" = test_res$hdf5_compressed_write,
      "DuckDB" = test_res$duckdb_write,
      "SQLite" = test_res$sqlite_write
    )
    
    for (system_name in names(systems)) {
      system_result <- systems[[system_name]]
      
      # Debug: Print what we got for this system
      cat("Processing", system_name, "write result...")
      if (is.null(system_result)) {
        cat(" NULL result\n")
        next
      } else if ("error" %in% names(system_result)) {
        cat(" ERROR:", system_result$error, "\n")
      } else {
        cat(" SUCCESS\n")
      }
      
      if (!"error" %in% names(system_result)) {
        # Write performance row - use safe extraction
        summary_rows[[length(summary_rows) + 1]] <- list(
          data_size = test_name,
          size_value = n_rows,  # Use rows as primary size metric
          total_elements = total_elements,
          system = system_name,
          operation = "write",
          time_ms = safe_extract(system_result, "write_time_ms", NA),
          total_time_ms = safe_extract(system_result, "total_time_ms", NA),
          file_size_mb = safe_extract(system_result, "file_size_mb", NA),
          data_size_mb = safe_extract(system_result, "data_size_mb", NA),
          throughput_mbps = safe_extract(system_result, "write_throughput_mbps", NA),
          compression_ratio = safe_extract(system_result, "compression_ratio", NA),
          character_cols = char_cols,
          numeric_cols = n_cols,
          rows = n_rows,
          status = "success"
        )
      } else {
        # Error row
        summary_rows[[length(summary_rows) + 1]] <- list(
          data_size = test_name,
          size_value = n_rows,
          total_elements = total_elements,
          system = system_name,
          operation = "write",
          time_ms = NA,
          total_time_ms = NA,
          file_size_mb = NA,
          data_size_mb = NA,
          throughput_mbps = NA,
          compression_ratio = NA,
          character_cols = char_cols,
          numeric_cols = n_cols,
          rows = n_rows,
          status = "error"
        )
      }
    }
    
    # Extract read performance metrics
    read_systems <- list(
      "HDF5_uncompressed" = if("hdf5_uncompressed_read" %in% names(test_res)) test_res$hdf5_uncompressed_read else NULL,
      "HDF5_compressed" = if("hdf5_compressed_read" %in% names(test_res)) test_res$hdf5_compressed_read else NULL,
      "DuckDB" = if("duckdb_read" %in% names(test_res)) test_res$duckdb_read else NULL,
      "SQLite" = if("sqlite_read" %in% names(test_res)) test_res$sqlite_read else NULL
    )
    
    for (system_name in names(read_systems)) {
      system_result <- read_systems[[system_name]]
      
      # Debug: Print what we got for this system
      cat("Processing", system_name, "read result...")
      if (is.null(system_result)) {
        cat(" NULL result\n")
        next
      } else if ("error" %in% names(system_result)) {
        cat(" ERROR:", system_result$error, "\n")
      } else {
        cat(" SUCCESS\n")
      }
      
      if (!is.null(system_result) && !"error" %in% names(system_result)) {
        perf <- if("performance" %in% names(system_result)) system_result$performance else system_result
        
        # Read performance row - use safe extraction
        summary_rows[[length(summary_rows) + 1]] <- list(
          data_size = test_name,
          size_value = n_rows,
          total_elements = total_elements,
          system = system_name,
          operation = "read",
          time_ms = safe_extract(perf, "read_time_ms", NA),
          total_time_ms = safe_extract(perf, "total_time_ms", NA),
          file_size_mb = safe_extract(perf, "file_size_mb", NA),
          data_size_mb = safe_extract(perf, "data_size_mb", NA),
          throughput_mbps = safe_extract(perf, "read_throughput_mbps", NA),
          compression_ratio = NA,  # Not applicable for read operations
          character_cols = char_cols,
          numeric_cols = n_cols,
          rows = n_rows,
          status = "success"
        )
      } else if (!is.null(system_result)) {
        # Error row for read
        summary_rows[[length(summary_rows) + 1]] <- list(
          data_size = size_name,
          size_value = size_value,
          total_elements = total_elements,
          system = system_name,
          operation = "read",
          time_ms = NA,
          total_time_ms = NA,
          file_size_mb = NA,
          data_size_mb = NA,
          throughput_mbps = NA,
          compression_ratio = NA,
          character_cols = char_cols,
          status = "error"
        )
      }
    }
  }
  
  # Convert to data.table
  summary_dt <- data.table::rbindlist(summary_rows)
  
  cat("Summary data.table created with", nrow(summary_dt), "rows\n")
  cat("Columns:", paste(names(summary_dt), collapse = ", "), "\n\n")
  
  # Print a preview of the summary
  cat("=== Summary Data.Table Preview ===\n")
  print(summary_dt[, .(data_size, system, operation, time_ms, throughput_mbps, status)])
  
  return(list(
    results = results,
    summary_dt = summary_dt
  ))
}

#' Clean up test files
#'
#' @param base_name Base name used for test files
#' @param test_configs List of test configurations used (to determine file names)
#' @export
clean_test_files <- function(base_name = "perf_test", 
                            test_configs = list(
                              "small" = list(rows=100, cols=100, char_cols=2),
                              "medium" = list(rows=1000, cols=1000, char_cols=5),
                              "large" = list(rows=10000, cols=1000, char_cols=10),
                              "very_large" = list(rows=100000, cols=1000, char_cols=20),
                              "huge_3gb_plus" = list(rows=500000, cols=1000, char_cols=50)
                            )) {
  
  cat("Cleaning up test files...\n")
  
  for (test_name in names(test_configs)) {
    
    # HDF5 files
    hdf5_files <- c(
      paste0(base_name, "_", test_name, "_hdf5.h5"),
      paste0(base_name, "_", test_name, "_hdf5_comp.h5")
    )
    
    # Database files
    db_files <- c(
      paste0(base_name, "_", test_name, ".duckdb"),
      paste0(base_name, "_", test_name, ".sqlite")
    )
    
    all_files <- c(hdf5_files, db_files)
    
    for (file in all_files) {
      if (file.exists(file)) {
        file.remove(file)
        cat("Removed:", file, "\n")
      }
    }
  }
  
  cat("Cleanup completed.\n")
}

plot_summary_dt <- function(summary_dt) {
  # Create comprehensive plots showing filesystem performance comparison
  library(plotly)
  library(data.table)

  # Define system-specific colors that relate to each filesystem
  system_colors <- list(
    "HDF5_uncompressed" = "#1f77b4",    # Blue - scientific/technical format
    "HDF5_compressed" = "#2ca02c",      # Green - compressed variant  
    "DuckDB" = "#ff7f0e",               # Orange - analytical database
    "SQLite" = "#d62728"                # Red - embedded database
  )

  # Prepare data for plotting
  dt <- copy(summary_dt)
  dt$data_size <- factor(dt$data_size, levels = unique(dt$data_size))
  
  # Add color mapping to the data
  dt[, plot_color := system_colors[[system]], by = system]

  # Plot 1: Write Speed Comparison across filesystems
  write_data <- dt[operation == "write" & !is.na(throughput_mbps)]

  p_write <- plot_ly(write_data, x = ~total_elements, y = ~throughput_mbps, 
                    color = ~system, colors = unlist(system_colors),
                    type = 'scatter', mode = 'lines+markers',
                    line = list(width = 3), marker = list(size = 8),
                    legendgroup = ~system,
                    hovertemplate = paste0(
                      "System: %{fullData.name}<br>",
                      "Total elements: %{x}<br>",
                      "Write Throughput: %{y:.2f} MB/s<br>",
                      "Write Time: %{customdata:.3f} s<extra></extra>"
                    ),
                    customdata = ~(time_ms/1000)) %>%
    layout(title = list(text = "Write Speed Comparison", font = list(size = 16)),
          xaxis = list(title = "Total Elements", type = "log", tickangle = -45),
          yaxis = list(title = "Write Throughput (MB/s)"),
          showlegend = FALSE, margin = list(t = 50))

  # Plot 2: Read Speed Comparison across filesystems  
  read_data <- dt[operation == "read" & !is.na(throughput_mbps)]

  p_read <- plot_ly(read_data, x = ~total_elements, y = ~throughput_mbps, 
                    color = ~system, colors = unlist(system_colors),
                    type = 'scatter', mode = 'lines+markers',
                    line = list(width = 3), marker = list(size = 8),
                    legendgroup = ~system,
                    hovertemplate = paste0(
                      "System: %{fullData.name}<br>",
                      "Total elements: %{x}<br>",
                      "Read Throughput: %{y:.2f} MB/s<br>",
                      "Read Time: %{customdata:.3f} s<extra></extra>"
                    ),
                    customdata = ~(time_ms/1000)) %>%
    layout(title = list(text = "Read Speed Comparison", font = list(size = 16)),
          xaxis = list(title = "Total Elements", type = "log", tickangle = -45),
          yaxis = list(title = "Read Throughput (MB/s)"),
          showlegend = FALSE, margin = list(t = 50))

  # Plot 3: File Size Comparison across filesystems
  file_size_data <- dt[operation == "write" & !is.na(file_size_mb)]

  # Create hover text manually to include compression ratio
  file_size_data[, hover_text := paste0(
    "System: ", system, "<br>",
    "Total elements: ", total_elements, "<br>",
    "File size: ", round(file_size_mb, 2), " MB<br>",
    "Data size: ", round(data_size_mb, 2), " MB<br>",
    "Compression ratio: ", ifelse(is.na(compression_ratio), "1.00", round(compression_ratio, 2)), ":1"
  )]

  p_filesize <- plot_ly(file_size_data, x = ~total_elements, y = ~file_size_mb, 
                        color = ~system, colors = unlist(system_colors),
                        type = 'scatter', mode = 'lines+markers',
                        line = list(width = 3), marker = list(size = 8),
                        legendgroup = ~system,
                        hovertemplate = "%{customdata}<extra></extra>",
                        customdata = ~hover_text) %>%
    layout(title = list(text = "File Size on Disk by Filesystem", font = list(size = 16)),
          xaxis = list(title = "Total Elements", type = "log", tickangle = -45),
          yaxis = list(title = "File Size (MB)"),
          showlegend = TRUE, margin = list(t = 50))

  # Create combined subplot layout with shared legend
  final_plot <- subplot(
    list(
      style(p_write, showlegend = FALSE),
      style(p_read, showlegend = FALSE),
      style(p_filesize, showlegend = TRUE)
    ),
    nrows = 3, heights = c(0.33, 0.33, 0.34),
    shareX = TRUE, titleY = TRUE, titleX = TRUE
  )
  # Display the plot
  final_plot
}
