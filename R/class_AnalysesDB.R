# MARK: AnalysesDB
#' @title Base class for database-backed Analyses
#' @description Minimal base class to represent analyses stored in a DuckDB file.
#' Requires a `db` path and a `type` identifier.
#' @param db Path to DuckDB file.
#' @param data_type Data type string (e.g., "MassSpec").
#' @return An object of class `AnalysesDB`.
#' @export
#' 
AnalysesDB <- function(db, data_type) {
  checkmate::assert_character(db, len = 1, null.ok = FALSE)
  checkmate::assert_character(data_type, len = 1, null.ok = FALSE)
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  structure(
    list(
      db = db,
      data_type = data_type
    ),
    class = "AnalysesDB"
  )
}

#' @describeIn AnalysesDB Validate the AnalysesDB object.
#' @param x AnalysesDB object.
#' @export
#' 
validate_object.AnalysesDB <- function(x) {
  checkmate::assert_class(x, "AnalysesDB")
  checkmate::assert_names(names(x), must.include = c("db", "data_type"))
  checkmate::assert_character(x$data_type, len = 1, null.ok = FALSE)
  checkmate::assert_character(x$db, len = 1, null.ok = FALSE)
  if (!file.exists(x$db)) {
    stop("AnalysesDB file not found: ", x$db)
  }
  NULL
}
