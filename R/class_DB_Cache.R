#' @title DB_Cache S3 Class
#' @description S3 class to manage `DB_Cache.duckdb` for caching and loading data.
#' @template arg-projectPath
#' @return An object of class `DB_Cache`.
#' @export
DB_Cache <- function(projectPath = ".") {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("duckdb package is required for DB_Cache")
  }
  db <- file.path(projectPath, "DB_Cache.duckdb")
  checkmate::assert_character(db, len = 1)
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_Cache_db_schema(conn)
  .validate_Cache_db_schema(conn)
  structure(list(db = db), class = "DB_Cache")
}

# MARK: save_cache
#' @describeIn DB_Cache Save data to cache table.
#' @param x DB_Cache object
#' @param name Name of cache entry
#' @param hash Hash string for cache table
#' @param description Description of cache entry
#' @param data Any R object to cache (will be serialized)
#' @export
save_cache.DB_Cache <- function(x, name, hash, description, data, ...) {
  stopifnot(inherits(x, "DB_Cache"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (missing(name) || is.null(name)) name <- hash
  if (missing(description) || is.null(description)) description <- "cache entry"
  serialized_data <- serialize(data, connection = NULL)
  DBI::dbExecute(conn, sprintf("DELETE FROM CacheManager WHERE hash = '%s'", hash))
  sql <- "INSERT INTO CacheManager (name, description, hash, data, created_at) VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)"
  DBI::dbExecute(conn, sql, list(name, description, hash, list(serialized_data)))
  invisible(TRUE)
}

# MARK: load_cache
#' @describeIn DB_Cache Load data from cache table.
#' @param x DB_Cache object
#' @param name Name of cache entry (not used, for consistency)
#' @param hash Hash string for cache table
#' @return Cached R object (deserialized)
#' @export
load_cache.DB_Cache <- function(x, name, hash, ...) {
  stopifnot(inherits(x, "DB_Cache"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  result <- DBI::dbGetQuery(conn, "SELECT data FROM CacheManager WHERE hash = ?", list(hash))
  if (nrow(result) == 0) return(NULL)
  unserialize(result$data[[1]])
}

# MARK: get_cache_info
#' @describeIn DB_Cache Get cache information table.
#' @param x DB_Cache object
#' @return Data.frame with cache metadata
#' @export
get_cache_info.DB_Cache <- function(x, ...) {
  stopifnot(inherits(x, "DB_Cache"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "SELECT name, description, hash, created_at FROM CacheManager")
}

# MARK: clear_cache
#' @describeIn DB_Cache Reset cache (delete all cache entries)
#' @param x DB_Cache object
#' @export
clear_cache.DB_Cache <- function(x, ...) {
  stopifnot(inherits(x, "DB_Cache"))
  if (file.remove(x$db)) {
    DB_Cache(projectPath = dirname(x$db))
    invisible(TRUE)
  } else {
    warning("Failed to delete cache file.")
    invisible(FALSE)
  }
}

# MARK: size
#' @describeIn DB_Cache Get size of Cache.duckdb file in bytes.
#' @param x DB_Cache object
#' @param unit Optional unit for size, possible values: "bytes", "KB", "MB", "GB" or "auto" (default).
#' @return Integer
#' @export
size.DB_Cache <- function(x, unit = "auto", ...) {
  stopifnot(inherits(x, "DB_Cache"))
  db_size <- file.info(x$db)$size
  switch(tolower(unit),
    "bytes" = db_size,
    "kb" = db_size / 1024,
    "mb" = db_size / (1024^2),
    "gb" = db_size / (1024^3),
    "auto" = {
      if (db_size < 1024) {
        paste0(db_size, " bytes")
      } else if (db_size < 1024^2) {
        paste0(round(db_size / 1024, 2), " KB")
      } else if (db_size < 1024^3) {
        paste0(round(db_size / (1024^2), 2), " MB")
      } else {
        paste0(round(db_size / (1024^3), 2), " GB")
      }
    },
    stop("Invalid unit specified. Use 'bytes', 'KB', 'MB', 'GB', or 'auto'.")
  )
}

# MARK: query_db
#' @describeIn DB_Cache Internal: execute a query on the DB.
#' @param x DB_Cache object
#' @param sql SQL query string
#' @param params Query parameters (optional)
#' @export
query_db.DB_Cache <- function(x, sql, params = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .query_db(conn, sql, params)
}

# MARK: list_db_tables
#' @describeIn DB_Cache Internal: list tables in the DB.
#' @param x DB_Cache object
#' @export
list_db_tables.DB_Cache <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .list_db_tables(conn)
}

# MARK: get_db_table_info
#' @describeIn DB_Cache Internal: get table info from the DB.
#' @param x DB_Cache object
#' @param tableName Table name
#' @export
get_db_table_info.DB_Cache <- function(x, tableName) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .get_db_table_info(conn, tableName)
}

# MARK: DB schema helpers
.create_Cache_db_schema <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS CacheManager (
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
      table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(CacheManager)")
      required <- list(
        name = "VARCHAR NOT NULL",
        description = "VARCHAR NOT NULL",
        hash = "VARCHAR PRIMARY KEY",
        data = "BLOB NOT NULL",
        created_at = "TIMESTAMP"
      )
      for (col in names(required)) {
        if (!(col %in% table_info$name)) {
          message(sprintf("Adding missing %s column to CacheManager table...", col))
          DBI::dbExecute(conn, sprintf("ALTER TABLE CacheManager ADD COLUMN %s %s", col, required[[col]]))
        }
      }
    },
    error = function(e) {
      stop("Schema migration check (CacheManager): ", e$message)
    }
  )
  invisible(TRUE)
}
