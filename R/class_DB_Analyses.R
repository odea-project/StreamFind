# MARK: DB_Analyses
#' @title Base class for database-backed Analyses
#' @description Minimal base class to represent analyses stored in a DuckDB file.
#' Requires a `db` path and a `type` identifier.
#' @param db Path to DuckDB file.
#' @param dataType Data type string (e.g., "MassSpec").
#' @return An object of class `DB_Analyses`.
#' @export
#' 
DB_Analyses <- function(db, dataType) {
  checkmate::assert_character(db, len = 1, null.ok = FALSE)
  checkmate::assert_character(dataType, len = 1, null.ok = FALSE)
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  structure(
    list(
      db = db,
      dataType = dataType
    ),
    class = "DB_Analyses"
  )
}

#' @describeIn DB_Analyses Validate the DB_Analyses object.
#' @param x DB_Analyses object.
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
