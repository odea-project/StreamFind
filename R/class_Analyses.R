# MARK: Analyses
#' @title Base class for database-backed Analyses
#' @description Minimal base class to represent analyses stored in a DuckDB file.
#' Used internally to expose shared S3 helpers for DB-backed analyses classes.
#' @template arg-projectPath
#' @return An object of class `Analyses`.
#' @export
#'
Analyses <- function(projectPath = ".") {
  checkmate::assert_character(projectPath, len = 1, null.ok = FALSE)
  db <- file.path(projectPath, "Analyses.duckdb")
  dataType <- "Generic"
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  structure(
    list(
      db = db,
      dataType = dataType,
      projectPath = projectPath
    ),
    class = "Analyses"
  )
}

#' @describeIn Analyses Validate the Analyses object.
#' @template arg-x-Analyses
#' @export
#'
validate_object.Analyses <- function(x) {
  checkmate::assert_class(x, "Analyses")
  checkmate::assert_names(names(x), must.include = c("db", "dataType"))
  checkmate::assert_character(x$dataType, len = 1, null.ok = FALSE)
  checkmate::assert_character(x$db, len = 1, null.ok = FALSE)
  if (!file.exists(x$db)) {
    stop("Analyses file not found: ", x$db)
  }
  NULL
}

# MARK: query_db
#' @describeIn Analyses Internal: execute a query on the DB.
#' @template arg-x-Analyses
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
#'
query_db.Analyses <- function(x, sql, params = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .query_db(conn, sql, params)
}

# MARK: list_db_tables
#' @describeIn Analyses Internal: list tables in the DB.
#' @template arg-x-Analyses
#' @export
#'
list_db_tables.Analyses <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .list_db_tables(conn)
}

# MARK: get_db_table_info
#' @describeIn Analyses Internal: get table info from the DB.
#' @template arg-x-Analyses
#' @template arg-sql-tableName
#' @export
#'
get_db_table_info.Analyses <- function(x, tableName) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .get_db_table_info(conn, tableName)
}
