#' @title Cache S3 Class
#' @description S3 class to manage the `Cache` table in a unified project DuckDB file for caching and loading data.
#' @param db Full path to the unified DuckDB file.
#' @return An object of class `Cache`.
#' @export
#' @export
Cache <- function(db) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("duckdb package is required for Cache")
  }
  checkmate::assert_character(db, len = 1)
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_Cache_db_schema(conn)
  .validate_Cache_db_schema(conn)
  structure(list(db = db), class = "Cache")
}

# MARK: save_cache
#' @describeIn Cache Save data to cache table.
#' @param x Cache object
#' @param name Name of cache entry
#' @param hash Hash string for cache table
#' @param description Description of cache entry
#' @param data Any R object to cache (will be serialized)
#' @export
save_cache.Cache <- function(x, name, hash, description, data, ...) {
  stopifnot(inherits(x, "Cache"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (missing(name) || is.null(name)) name <- hash
  if (missing(description) || is.null(description)) description <- "cache entry"
  serialized_data <- serialize(data, connection = NULL)
  DBI::dbExecute(conn, sprintf("DELETE FROM Cache WHERE hash = '%s'", hash))
  sql <- "INSERT INTO Cache (name, description, hash, data, created_at) VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)"
  DBI::dbExecute(conn, sql, list(name, description, hash, list(serialized_data)))
  invisible(TRUE)
}

# MARK: load_cache
#' @describeIn Cache Load data from cache table.
#' @param x Cache object
#' @param name Name of cache entry (not used, for consistency)
#' @param hash Hash string for cache table
#' @return Cached R object (deserialized)
#' @export
load_cache.Cache <- function(x, name, hash, ...) {
  stopifnot(inherits(x, "Cache"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  result <- DBI::dbGetQuery(conn, "SELECT data FROM Cache WHERE hash = ?", list(hash))
  if (nrow(result) == 0) {
    return(NULL)
  }
  unserialize(result$data[[1]])
}

# MARK: get_cache_info
#' @describeIn Cache Get cache information table.
#' @param x Cache object
#' @return Data.frame with cache metadata
#' @export
get_cache_info.Cache <- function(x, ...) {
  stopifnot(inherits(x, "Cache"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "SELECT name, description, hash, created_at FROM Cache")
}

# MARK: clear_cache
#' @describeIn Cache Reset cache (delete all cache entries or specific entries by name)
#' @param x Cache object
#' @param value Character vector of cache entry names to delete. If NULL (default), deletes entire cache database.
#' @export
clear_cache.Cache <- function(x, value = NULL, ...) {
  stopifnot(inherits(x, "Cache"))

  if (is.null(value)) {
    # Delete entire database file and recreate
    if (file.remove(x$db)) {
      Cache(db = x$db)
      invisible(TRUE)
    } else {
      warning("Failed to delete cache file.")
      invisible(FALSE)
    }
  } else {
    # Delete specific entries by name
    conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    # Build SQL with parameterized query for safety
    placeholders <- paste(rep("?", length(value)), collapse = ", ")
    sql <- sprintf("DELETE FROM Cache WHERE name IN (%s)", placeholders)
    rows_deleted <- DBI::dbExecute(conn, sql, as.list(value))

    if (rows_deleted > 0) {
      message(sprintf("Deleted %d cache entry(ies).", rows_deleted))
      invisible(TRUE)
    } else {
      warning("No matching cache entries found.")
      invisible(FALSE)
    }
  }
}

# MARK: size
#' @describeIn Cache Get size of Cache.duckdb file in bytes.
#' @param x Cache object
#' @param unit Optional unit for size, possible values: "bytes", "KB", "MB", "GB" or "auto" (default).
#' @return Integer
#' @export
size.Cache <- function(x, unit = "auto", ...) {
  stopifnot(inherits(x, "Cache"))
  con <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  size_query <- try(DBI::dbGetQuery(con, "PRAGMA table_size('Cache')"), silent = TRUE)
  if (inherits(size_query, "try-error") || is.null(size_query) || nrow(size_query) == 0) {
    # Fallback: return 0 cache table size, or full db file size if you prefer legacy behavior.
    tsize <- 0
  } else {
    tsize <- sum(size_query$total_bytes, na.rm = TRUE)
  }
  val <- tsize
  switch(tolower(unit),
    "bytes" = val,
    "kb" = val / 1024,
    "mb" = val / (1024^2),
    "gb" = val / (1024^3),
    "auto" = {
      if (val < 1024) {
        paste0(val, " bytes")
      } else if (val < 1024^2) {
        paste0(round(val / 1024, 2), " KB")
      } else if (val < 1024^3) {
        paste0(round(val / (1024^2), 2), " MB")
      } else {
        paste0(round(val / (1024^3), 2), " GB")
      }
    },
    stop("Invalid unit specified. Use 'bytes', 'KB', 'MB', 'GB', or 'auto'.")
  )
}

# MARK: DB schema helpers
.create_Cache_db_schema <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Cache (
      name VARCHAR NOT NULL,
      description VARCHAR NOT NULL,
      hash VARCHAR PRIMARY KEY,
      data BLOB NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  invisible(TRUE)
}

.validate_Cache_db_schema <- function(conn) {
  tryCatch(
    {
      table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Cache)")
      required <- list(
        name = "VARCHAR NOT NULL",
        description = "VARCHAR NOT NULL",
        hash = "VARCHAR PRIMARY KEY",
        data = "BLOB NOT NULL",
        created_at = "TIMESTAMP"
      )
      for (col in names(required)) {
        if (!(col %in% table_info$name)) {
          message(sprintf("Adding missing %s column to Cache table...", col))
          DBI::dbExecute(conn, sprintf("ALTER TABLE Cache ADD COLUMN %s %s", col, required[[col]]))
        }
      }
    },
    error = function(e) {
      stop("Schema migration check (CacheManager): ", e$message)
    }
  )
  invisible(TRUE)
}
