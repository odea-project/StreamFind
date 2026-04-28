#' Database-Backed Audit Logging (S3 class)
#'
#' Provides a DB-backed audit trail interface for StreamFind projects: logs create/update/delete/run events automatically,
#' allows querying of all audit logs, and is intended for use by Engine or other StreamFind components.
#'
#' The `AuditTrail` class is an S3 object responsible for ensuring its own table schema and handling all persistence in the unified DuckDB file for this project.
#' All typical list and data-frame operations are returned for queries. This is the standard audit logging interface for the package.
#'
#' @section Table Schema:
#' Table name: `AuditTrail`; columns: operation_type (str), object_type (str), operation_details (json/str), created_at.
#'
#' @section Usage:
#'   audit <- AuditTrail(db = 'file.duckdb')
#'   add_audit_entry(audit, 'run', 'ProcessingStep', list(method='algo'))
#'   get_audit_trail(audit)
#'
#' @aliases AuditTrail AuditTrail-class get_audit_trail.AuditTrail add_audit_entry.AuditTrail
#' @rdname AuditTrail
#'
#' @param db Path to DuckDB database file used for audit logging (the unified DB file)
#' @param x An AuditTrail object (S3)
#' @param operation_type Type of operation (character: e.g. 'create', 'update', 'delete', 'run' etc)
#' @param object_type Type of object being acted on (character, e.g. 'Metadata', 'Workflow', ...)
#' @param details (Optional) Operation details. A list or character; if a list, will be coerced to JSON for DB storage.
#' @param ... Additional arguments (ignored)
#' @return The AuditTrail constructor returns an S3 object. `get_audit_trail` returns a data.frame of audit logs. `add_audit_entry` returns the AuditTrail object (invisible).
#'
#' @export
#'
AuditTrail <- function(db) {
  if (missing(db) || is.null(db)) stop('AuditTrail requires duckdb filepath in db')
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("duckdb package is required for AuditTrail")
  checkmate::assert_character(db, len = 1)
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_AuditTrail_db_schema(conn)
  .validate_AuditTrail_db_schema(conn)
  structure(list(db = db), class = "AuditTrail")
}

#' @rdname AuditTrail
#' @export
#'
get_audit_trail.AuditTrail <- function(x, ...) {
  stopifnot(inherits(x, "AuditTrail"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  audit_df <- DBI::dbGetQuery(conn, "SELECT * FROM AuditTrail ORDER BY created_at DESC")
  audit_df
}

#' @rdname AuditTrail
#' @export
#'
add_audit_entry.AuditTrail <- function(x, operation_type, object_type, details = NULL, ...) {
  stopifnot(inherits(x, "AuditTrail"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
  if (!is.null(details)) {
    details_json <- .convert_to_json(details)
  } else {
    details_json <- "null"
  }
  DBI::dbExecute(conn, "INSERT INTO AuditTrail (operation_type, object_type, operation_details) VALUES (?, ?, ?)",
                 list(operation_type, object_type, as.character(details_json)))
  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
  invisible(x)
}

# Internal helpers: not documented in user .Rd

.create_AuditTrail_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS AuditTrail (operation_type VARCHAR NOT NULL, object_type VARCHAR NOT NULL, operation_details VARCHAR, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")
  invisible(TRUE)
}

.validate_AuditTrail_db_schema <- function(conn) {
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(AuditTrail)")
      required <- list(
        operation_type = "VARCHAR NOT NULL",
        object_type = "VARCHAR NOT NULL",
        operation_details = "VARCHAR",
        created_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
      )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to AuditTrail table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE AuditTrail ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (AuditTrail): ", e$message)
  })
  invisible(TRUE)
}
