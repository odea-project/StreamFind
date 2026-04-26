# MARK: DB_Analyses
#' @title Base class for database-backed Analyses
#' @description Minimal base class to represent analyses stored in a DuckDB file.
#' Used internally to expose shared S3 helpers for DB-backed analyses classes.
#' @template arg-projectPath
#' @return An object of class `DB_Analyses`.
#' @export
#'
DB_Analyses <- function(projectPath = ".") {
  checkmate::assert_character(projectPath, len = 1, null.ok = FALSE)
  db <- file.path(projectPath, "DB_Analyses.duckdb")
  dataType <- "DB_Generic"
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  structure(
    list(
      db = db,
      dataType = dataType,
      projectPath = projectPath
    ),
    class = "DB_Analyses"
  )
}

#' @describeIn DB_Analyses Validate the DB_Analyses object.
#' @template arg-x-DB_Analyses
#' @export
#'
validate_object.DB_Analyses <- function(x) {
  checkmate::assert_class(x, "DB_Analyses")
  checkmate::assert_names(names(x), must.include = c("db", "dataType"))
  checkmate::assert_character(x$dataType, len = 1, null.ok = FALSE)
  checkmate::assert_character(x$db, len = 1, null.ok = FALSE)
  if (!file.exists(x$db)) {
    stop("DB_Analyses file not found: ", x$db)
  }
  NULL
}

# MARK: query_db
#' @describeIn DB_Analyses Internal: execute a query on the DB.
#' @template arg-x-DB_Analyses
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
#'
query_db.DB_Analyses <- function(x, sql, params = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .query_db(conn, sql, params)
}

# MARK: list_db_tables
#' @describeIn DB_Analyses Internal: list tables in the DB.
#' @template arg-x-DB_Analyses
#' @export
#'
list_db_tables.DB_Analyses <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .list_db_tables(conn)
}

# MARK: get_db_table_info
#' @describeIn DB_Analyses Internal: get table info from the DB.
#' @template arg-x-DB_Analyses
#' @template arg-sql-tableName
#' @export
#'
get_db_table_info.DB_Analyses <- function(x, tableName) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .get_db_table_info(conn, tableName)
}
