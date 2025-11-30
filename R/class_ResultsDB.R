# MARK: ResultsDB
#' @title Base class for database-backed Results
#' @description Minimal base class to represent results stored in a DuckDB file.
#' Requires a `db` path and a `data_type` identifier.
#' @param db Path to DuckDB file.
#' @param data_type Data type string (e.g., "MassSpec").
#' @return An object of class `ResultsDB`.
#' @export
#'
ResultsDB <- function(db, data_type) {
  checkmate::assert_character(db, len = 1, null.ok = FALSE)
  checkmate::assert_character(data_type, len = 1, null.ok = FALSE)
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  structure(
    list(
      db = db,
      data_type = data_type
    ),
    class = "ResultsDB"
  )
}

#' @describeIn ResultsDB Validate the ResultsDB object.
#' @template arg-x-ResultsDB
#' @export
#'
validate_object.ResultsDB <- function(x) {
  checkmate::assert_class(x, "ResultsDB")
  checkmate::assert_names(names(x), must.include = c("db", "data_type"))
  checkmate::assert_character(x$data_type, len = 1, null.ok = FALSE)
  checkmate::assert_character(x$db, len = 1, null.ok = FALSE)
  if (!file.exists(x$db)) {
    stop("ResultsDB file not found: ", x$db)
  }
  NULL
}

# MARK: query_db
#' @describeIn ResultsDB Internal: execute a query on the DB.
#' @template arg-x-ResultsDB
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
#' 
query_db.ResultsDB <- function(x, sql, params = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .query_db(conn, sql, params)
}

# MARK: list_db_tables
#' @describeIn ResultsDB Internal: list tables in the DB.
#' @template arg-x-ResultsDB
#' @export
#' 
list_db_tables.ResultsDB <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .list_db_tables(conn)
}

# MARK: get_db_table_info
#' @describeIn ResultsDB Internal: get table info from the DB.
#' @template arg-x-ResultsDB
#' @template arg-sql-tableName
#' @export
#' 
get_db_table_info.ResultsDB <- function(x, tableName) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .get_db_table_info(conn, tableName)
}
