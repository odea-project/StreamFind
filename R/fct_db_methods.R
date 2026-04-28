#' @title Generic DuckDB Methods for DB-backed StreamFind Objects
#' @description Helper S3 methods for running SQL and retrieving tables/info for any object with a `$db` DuckDB field. Intended for use with objects like `Cache`, `Engine`, or ad-hoc lists with a DuckDB file reference.
#' @details These methods work with *any* S3 object containing a valid `db` field (filename to a DuckDB file). They are exported and can be used directly on such objects.
#' @section Table of Contents:
#' - `query_db.default`: Run SQL queries with (optional) parameter binding.
#' - `list_db_tables.default`: List tables in the DB.
#' - `get_db_table_info.default`: Get column and schema info for a named table.
#' @name db_helpers
#' @examples
#' # NOT RUN: Usage examples
#' # cache <- Cache(db = "path_to_db.duckdb")
#' # list_db_tables(cache)
#' # query_db(cache, "SELECT * FROM Cache WHERE name = ?", params = list("my_name"))
#' # query_db(cache, "SELECT * FROM Cache WHERE name = $entry_name", params = list(entry_name = "my_name"))
#' # get_db_table_info(cache, "Cache")
#' # ad_hoc <- list(db = "path_to_db.duckdb"); class(ad_hoc) <- "AnyClass"; list_db_tables(ad_hoc)
NULL

#' @rdname db_helpers
#' @param x An object with a `$db` field containing the path to a DuckDB file (e.g. a `Cache`, `Engine`, or ad-hoc structure).
#' @param sql A character string: the SQL query to execute.
#' @param params (Optional) A named or unnamed list of parameters to bind to the SQL query's placeholders (use `?` for positional or `$name` for named placeholders). Pass `NULL` if no parameters are needed.
#' @return For `query_db`: a data.frame with the result of the query.
#' @export
query_db.default <- function(x, sql, params = NULL) {
  if (is.null(x$db) || !file.exists(x$db)) stop("Object must have db path and file must exist")
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .query_db(conn, sql, params)
}

#' @rdname db_helpers
#' @param x An object with a `$db` field containing the path to a DuckDB file (e.g. a `Cache`, `Engine`, or ad-hoc structure).
#' @return For `list_db_tables`: a character vector of table names in the database.
#' @export
list_db_tables.default <- function(x) {
  if (is.null(x$db) || !file.exists(x$db)) stop("Object must have db path and file must exist")
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .list_db_tables(conn)
}

#' @rdname db_helpers
#' @param x An object with a `$db` field containing the path to a DuckDB file (e.g. a `Cache`, `Engine`, or ad-hoc structure).
#' @param tableName A character string: the name of the table to get information for.
#' @return For `get_db_table_info`: a data.frame of column names, types, and constraints for the table.
#' @export
get_db_table_info.default <- function(x, tableName) {
  if (is.null(x$db) || !file.exists(x$db)) stop("Object must have db path and file must exist")
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .get_db_table_info(conn, tableName)
}
