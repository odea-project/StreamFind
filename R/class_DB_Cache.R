#' @title CacheManager S3 Class
#' @description S3 class to manage Cache.duckdb file for caching and loading data
#' @param db Path to Cache.duckdb file
#' @return An object of class 'CacheManager'
#' @export
CacheManager <- function(db) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("duckdb package is required for CacheManager")
  }
  checkmate::assert_character(db, len = 1)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_Cache_db_schema(conn)
  .validate_Cache_db_schema(conn)
  structure(list(db = db), class = "CacheManager")
}

# MARK: save_cache
#' @describeIn CacheManager Save data to cache table.
#' @param x CacheManager object
#' @param name Name of cache entry
#' @param hash Hash string for cache table
#' @param description Description of cache entry
#' @param data Data.frame to cache
#' @export
#' 
save_cache.CacheManager <- function(x, name, hash, description, data, ...) {
  stopifnot(inherits(x, "CacheManager"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (missing(name) || is.null(name)) name <- hash
  if (missing(description) || is.null(description)) description <- "cache entry"
  DBI::dbWriteTable(conn, hash, data, overwrite = TRUE)
  DBI::dbExecute(conn, sprintf("DELETE FROM CacheManager WHERE hash = '%s'", hash))
  sql <- "INSERT INTO CacheManager (name, description, hash, created_at) VALUES (?, ?, ?, CURRENT_TIMESTAMP)"
  DBI::dbExecute(conn, sql, list(name, description, hash))
  invisible(TRUE)
}

# MARK: load_cache
#' @describeIn CacheManager Load data from cache table.
#' @param x CacheManager object
#' @param name Name of cache entry (not used, for consistency)
#' @param hash Hash string for cache table
#' @return Data.frame
#' @export
#' 
load_cache.CacheManager <- function(x, name, hash, ...) {
  stopifnot(inherits(x, "CacheManager"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  cache_info <- get_cache_info(x)
  if (hash %in% cache_info$hash) {
    data.table::as.data.table(DBI::dbReadTable(conn, hash))
  } else {
    data.table::data.table()
  }
}

# MARK: get_cache_info
#' @describeIn CacheManager Get cache information table.
#' @param x CacheManager object
#' @return Integer
#' @export
#' 
get_cache_info.CacheManager <- function(x, ...) {
  stopifnot(inherits(x, "CacheManager"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "SELECT name, description, hash, created_at FROM CacheManager")
}

# MARK: clear_cache
#' @describeIn CacheManager Reset cache (drop all cache tables and clear CacheManager)
#' @param x CacheManager object
#' @export
#' 
clear_cache.CacheManager <- function(x, ...) {
  stopifnot(inherits(x, "CacheManager"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  hashes <- DBI::dbGetQuery(conn, "SELECT hash FROM CacheManager")$hash
  for (tbl in hashes) {
    DBI::dbExecute(conn, sprintf('DROP TABLE IF EXISTS "%s"', tbl))
  }
  DBI::dbExecute(conn, "DELETE FROM CacheManager")
  invisible(TRUE)
}

# MARK: size
#' @describeIn CacheManager Get size of Cache.duckdb file in bytes.
#' @param x CacheManager object
#' @param unit Optional unit for size, possible values: "bytes", "KB", "MB", "GB" or "auto" (default).
#' @return Integer
#' @export
#' 
size.CacheManager <- function(x, unit = "auto", ...) {
  stopifnot(inherits(x, "CacheManager"))
  db_size <- file.info(x$db)$size
  switch(
    tolower(unit),
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
#' @describeIn CacheManager Internal: execute a query on the DB.
#' @param x CacheManager object
#' @param sql SQL query string
#' @param params Query parameters (optional)
#' @export
query_db.CacheManager <- function(x, sql, params = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .query_db(conn, sql, params)
}

# MARK: list_db_tables
#' @describeIn CacheManager Internal: list tables in the DB.
#' @param x CacheManager object
#' @export
list_db_tables.CacheManager <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .list_db_tables(conn)
}

# MARK: get_db_table_info
#' @describeIn CacheManager Internal: get table info from the DB.
#' @param x CacheManager object
#' @param tableName Table name
#' @export
get_db_table_info.CacheManager <- function(x, tableName) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .get_db_table_info(conn, tableName)
}

# MARK: .create_Cache_db_schema
#' @noRd
.create_Cache_db_schema <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS CacheManager (
      name VARCHAR NOT NULL,
      description VARCHAR NOT NULL,
      hash VARCHAR NOT NULL, -- hash string used as table name
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  invisible(TRUE)
}

# MARK: .validate_Cache_db_schema
#' @noRd
.validate_Cache_db_schema <- function(conn) {
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(CacheManager)")
    required <- list(
      name = "VARCHAR NOT NULL",
      description = "VARCHAR NOT NULL",
      hash = "VARCHAR NOT NULL",
      created_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to CacheManager table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE CacheManager ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (CacheManager): ", e$message)
  })
  invisible(TRUE)
}
