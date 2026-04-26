# MARK: MassSpecResults_Spectra
#' @title Constructor and methods to handle Mass Spectrometry spectra results using DuckDB
#' @description The `MassSpecResults_Spectra` class stores raw spectra, charge-state
#'   assignments, deconvoluted mass spectra, and spectral peak maxima in a DuckDB backend.
#' @template arg-projectPath
#' @param analyses A data.table containing analysis metadata.
#' @param spectra A data.table with raw spectra data.
#' @param charges A data.table with charge-state assignments (output of CalculateSpectraCharges).
#' @param mass_spectra A data.table with deconvoluted mass spectra.
#' @param spectra_peaks A data.table with spectral peak maxima (output of FindSpectraMaxima).
#' @return An object of class `MassSpecResults_Spectra`.
#' @export
#'
MassSpecResults_Spectra <- function(
  projectPath   = ".",
  analyses      = data.table::data.table(),
  spectra       = data.table::data.table(),
  charges       = data.table::data.table(),
  mass_spectra  = data.table::data.table(),
  spectra_peaks = data.table::data.table()
) {
  if (!requireNamespace("DBI",    quietly = TRUE)) stop("DBI package required.")
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("duckdb package required.")
  db <- file.path(projectPath, "MassSpecResults_Spectra.duckdb")
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  .create_MassSpecAnalyses_Analyses_db_schema(conn)
  .validate_MassSpecAnalyses_Analyses_db_schema(conn)
  .create_MassSpecResults_Spectra_Spectra_db_schema(conn)
  .validate_MassSpecResults_Spectra_Spectra_db_schema(conn)
  .create_MassSpecResults_Spectra_Charges_db_schema(conn)
  .validate_MassSpecResults_Spectra_Charges_db_schema(conn)
  .create_MassSpecResults_Spectra_MassSpectra_db_schema(conn)
  .validate_MassSpecResults_Spectra_MassSpectra_db_schema(conn)
  .create_MassSpecResults_Spectra_SpectraPeaks_db_schema(conn)
  .validate_MassSpecResults_Spectra_SpectraPeaks_db_schema(conn)

  if (nrow(analyses) > 0) {
    .validate_MassSpecAnalyses_analyses_dt(analyses)
    DBI::dbExecute(conn, "DELETE FROM Analyses")
    DBI::dbWriteTable(conn, "Analyses", as.data.frame(analyses), overwrite = TRUE)
  }
  if (nrow(spectra) > 0) {
    .validate_MassSpecResults_Spectra_spectra_dt(spectra)
    DBI::dbExecute(conn, "DELETE FROM Spectra")
    DBI::dbWriteTable(conn, "Spectra", as.data.frame(spectra), append = TRUE)
  }
  if (nrow(charges) > 0) {
    DBI::dbExecute(conn, "DELETE FROM Charges")
    DBI::dbWriteTable(conn, "Charges", as.data.frame(charges), append = TRUE)
  }
  if (nrow(mass_spectra) > 0) {
    DBI::dbExecute(conn, "DELETE FROM MassSpectra")
    DBI::dbWriteTable(conn, "MassSpectra", as.data.frame(mass_spectra), append = TRUE)
  }
  if (nrow(spectra_peaks) > 0) {
    DBI::dbExecute(conn, "DELETE FROM SpectraPeaks")
    DBI::dbWriteTable(conn, "SpectraPeaks", as.data.frame(spectra_peaks), append = TRUE)
  }

  x <- structure(
    list(db = db, dataType = "MassSpec"),
    class = c("MassSpecResults_Spectra", "Results")
  )
  if (is.null(validate_object(x))) x else stop("Invalid MassSpecResults_Spectra object.")
}

#' @describeIn MassSpecResults_Spectra Validate the MassSpecResults_Spectra object.
#' @template arg-x-MassSpecResults_Spectra
#' @export
validate_object.MassSpecResults_Spectra <- function(x) {
  checkmate::assert_class(x, "MassSpecResults_Spectra")
  checkmate::assert_true(identical(x$dataType, "MassSpec"))
  if (!file.exists(x$db)) stop("MassSpecResults_Spectra file not found: ", x$db)
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  required_tables <- c("Analyses", "Spectra", "Charges", "MassSpectra", "SpectraPeaks")
  present <- DBI::dbListTables(conn)
  if (!all(required_tables %in% present))
    stop("Missing required tables in MassSpecResults_Spectra")
  .validate_MassSpecAnalyses_Analyses_db_schema(conn)
  .validate_MassSpecResults_Spectra_Spectra_db_schema(conn)
  .validate_MassSpecResults_Spectra_Charges_db_schema(conn)
  .validate_MassSpecResults_Spectra_MassSpectra_db_schema(conn)
  .validate_MassSpecResults_Spectra_SpectraPeaks_db_schema(conn)
  NextMethod()
}

# MARK: query_db
#' @describeIn MassSpecResults_Spectra Execute a query on the DB.
#' @template arg-x-MassSpecResults_Spectra
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
query_db.MassSpecResults_Spectra <- function(x, sql, params = NULL) NextMethod()

# MARK: list_db_tables
#' @describeIn MassSpecResults_Spectra List tables in the DB.
#' @template arg-x-MassSpecResults_Spectra
#' @export
list_db_tables.MassSpecResults_Spectra <- function(x) NextMethod()

# MARK: get_db_table_info
#' @describeIn MassSpecResults_Spectra Get table info from the DB.
#' @template arg-x-MassSpecResults_Spectra
#' @template arg-sql-tableName
#' @export
get_db_table_info.MassSpecResults_Spectra <- function(x, tableName) NextMethod()

# MARK: show
#' @describeIn MassSpecResults_Spectra Print summary.
#' @template arg-x-MassSpecResults_Spectra
#' @export
show.MassSpecResults_Spectra <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  cat("\n")
  cat(is(x), "\n")
  analyses <- DBI::dbReadTable(conn, "Analyses")
  spec_cnt <- DBI::dbGetQuery(conn, "SELECT analysis, COUNT(*) AS n FROM Spectra GROUP BY analysis")
  chg_cnt  <- DBI::dbGetQuery(conn, "SELECT analysis, COUNT(*) AS n FROM Charges GROUP BY analysis")
  ms_cnt   <- DBI::dbGetQuery(conn, "SELECT analysis, COUNT(*) AS n FROM MassSpectra GROUP BY analysis")
  pk_cnt   <- DBI::dbGetQuery(conn, "SELECT analysis, COUNT(*) AS n FROM SpectraPeaks GROUP BY analysis")
  info <- data.table::data.table(
    analysis      = analyses$analysis,
    replicate     = analyses$replicate,
    spectra_rows  = spec_cnt$n[match(analyses$analysis, spec_cnt$analysis)],
    charge_rows   = chg_cnt$n[match(analyses$analysis, chg_cnt$analysis)],
    mass_spec_rows = ms_cnt$n[match(analyses$analysis, ms_cnt$analysis)],
    peak_rows     = pk_cnt$n[match(analyses$analysis, pk_cnt$analysis)]
  )
  info[is.na(info)] <- 0L
  print(info)
}

# MARK: get_spectra
#' @describeIn MassSpecResults_Spectra Get raw spectra data.
#' @template arg-x-MassSpecResults_Spectra
#' @template arg-analyses
#' @export
get_spectra.MassSpecResults_Spectra <- function(x, analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel) == 0) return(data.table::data.table())
  q  <- sprintf("SELECT * FROM Spectra WHERE analysis IN ('%s')",
                paste(sel, collapse = "','"))
  data.table::as.data.table(DBI::dbGetQuery(conn, q))
}

# MARK: get_spectra_charges
#' @describeIn MassSpecResults_Spectra Get charge-state assignments.
#' @template arg-x-MassSpecResults_Spectra
#' @template arg-analyses
#' @export
get_spectra_charges.MassSpecResults_Spectra <- function(x, analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel) == 0) return(data.table::data.table())
  q <- sprintf("SELECT * FROM Charges WHERE analysis IN ('%s')",
               paste(sel, collapse = "','"))
  data.table::as.data.table(DBI::dbGetQuery(conn, q))
}

# MARK: get_mass_spectra
#' @describeIn MassSpecResults_Spectra Get deconvoluted mass spectra.
#' @template arg-x-MassSpecResults_Spectra
#' @template arg-analyses
#' @export
get_mass_spectra.MassSpecResults_Spectra <- function(x, analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel) == 0) return(data.table::data.table())
  q <- sprintf("SELECT * FROM MassSpectra WHERE analysis IN ('%s')",
               paste(sel, collapse = "','"))
  data.table::as.data.table(DBI::dbGetQuery(conn, q))
}

# MARK: get_spectra_peaks
#' @describeIn MassSpecResults_Spectra Get spectral peak maxima.
#' @template arg-x-MassSpecResults_Spectra
#' @template arg-analyses
#' @export
get_spectra_peaks.MassSpecResults_Spectra <- function(x, analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel) == 0) return(data.table::data.table())
  q <- sprintf("SELECT * FROM SpectraPeaks WHERE analysis IN ('%s')",
               paste(sel, collapse = "','"))
  data.table::as.data.table(DBI::dbGetQuery(conn, q))
}

# ============================================================================
# MARK: internal update helpers
# ============================================================================

#' @noRd
.update_spectra <- function(x, updated_dt) {
  stopifnot(inherits(x, "MassSpecResults_Spectra"))
  .validate_MassSpecResults_Spectra_spectra_dt(updated_dt)
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "DELETE FROM Spectra")
  DBI::dbWriteTable(conn, "Spectra", as.data.frame(updated_dt), append = TRUE)
  invisible(x)
}

#' @noRd
.update_spectra_charges <- function(x, charges_dt) {
  stopifnot(inherits(x, "MassSpecResults_Spectra"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "DELETE FROM Charges")
  if (nrow(charges_dt) > 0)
    DBI::dbWriteTable(conn, "Charges", as.data.frame(charges_dt), append = TRUE)
  invisible(x)
}

#' @noRd
.update_mass_spectra <- function(x, mass_spectra_dt) {
  stopifnot(inherits(x, "MassSpecResults_Spectra"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "DELETE FROM MassSpectra")
  if (nrow(mass_spectra_dt) > 0)
    DBI::dbWriteTable(conn, "MassSpectra", as.data.frame(mass_spectra_dt), append = TRUE)
  invisible(x)
}

#' @noRd
.update_spectra_peaks <- function(x, peaks_dt) {
  stopifnot(inherits(x, "MassSpecResults_Spectra"))
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "DELETE FROM SpectraPeaks")
  if (nrow(peaks_dt) > 0)
    DBI::dbWriteTable(conn, "SpectraPeaks", as.data.frame(peaks_dt), append = TRUE)
  invisible(x)
}

# ============================================================================
# MARK: input validation helpers
# ============================================================================

#' @noRd
.validate_MassSpecResults_Spectra_spectra_dt <- function(x) {
  required <- c("analysis", "replicate", "id", "polarity", "rt", "mz", "intensity")
  missing  <- setdiff(required, colnames(x))
  if (length(missing) > 0)
    stop("Missing required columns in spectra data.table: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

# ============================================================================
# MARK: DB schema helpers
# ============================================================================

#' @noRd
.create_MassSpecResults_Spectra_Spectra_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS Spectra (
    analysis  VARCHAR,
    replicate VARCHAR,
    id        VARCHAR,
    polarity  INTEGER,
    level     INTEGER,
    rt        DOUBLE,
    mz        DOUBLE,
    intensity DOUBLE
  )")
  invisible(TRUE)
}

#' @noRd
.validate_MassSpecResults_Spectra_Spectra_db_schema <- function(conn) {
  tryCatch({
    ti <- DBI::dbGetQuery(conn, "PRAGMA table_info(Spectra)")
    required <- list(analysis = "VARCHAR", replicate = "VARCHAR", id = "VARCHAR",
                     polarity = "INTEGER", level = "INTEGER", rt = "DOUBLE",
                     mz = "DOUBLE", intensity = "DOUBLE")
    for (col in names(required)) {
      if (!(col %in% ti$name))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Spectra ADD COLUMN \"%s\" %s", col, required[[col]]))
    }
  }, error = function(e) stop("Schema migration (Spectra): ", e$message))
  invisible(TRUE)
}

#' @noRd
.create_MassSpecResults_Spectra_Charges_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS Charges (
    analysis   VARCHAR,
    replicate  VARCHAR,
    id         VARCHAR,
    polarity   INTEGER,
    rt         DOUBLE,
    mz         DOUBLE,
    intensity  DOUBLE,
    cluster_mz DOUBLE,
    z          INTEGER,
    mass       DOUBLE
  )")
  invisible(TRUE)
}

#' @noRd
.validate_MassSpecResults_Spectra_Charges_db_schema <- function(conn) {
  tryCatch({
    ti <- DBI::dbGetQuery(conn, "PRAGMA table_info(Charges)")
    required <- list(analysis = "VARCHAR", replicate = "VARCHAR", id = "VARCHAR",
                     polarity = "INTEGER", rt = "DOUBLE", mz = "DOUBLE",
                     intensity = "DOUBLE", cluster_mz = "DOUBLE",
                     z = "INTEGER", mass = "DOUBLE")
    for (col in names(required)) {
      if (!(col %in% ti$name))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Charges ADD COLUMN \"%s\" %s", col, required[[col]]))
    }
  }, error = function(e) stop("Schema migration (Charges): ", e$message))
  invisible(TRUE)
}

#' @noRd
.create_MassSpecResults_Spectra_MassSpectra_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS MassSpectra (
    analysis  VARCHAR,
    replicate VARCHAR,
    id        VARCHAR,
    polarity  INTEGER,
    rt        DOUBLE,
    mass      DOUBLE,
    intensity DOUBLE
  )")
  invisible(TRUE)
}

#' @noRd
.validate_MassSpecResults_Spectra_MassSpectra_db_schema <- function(conn) {
  tryCatch({
    ti <- DBI::dbGetQuery(conn, "PRAGMA table_info(MassSpectra)")
    required <- list(analysis = "VARCHAR", replicate = "VARCHAR", id = "VARCHAR",
                     polarity = "INTEGER", rt = "DOUBLE",
                     mass = "DOUBLE", intensity = "DOUBLE")
    for (col in names(required)) {
      if (!(col %in% ti$name))
        DBI::dbExecute(conn, sprintf("ALTER TABLE MassSpectra ADD COLUMN \"%s\" %s", col, required[[col]]))
    }
  }, error = function(e) stop("Schema migration (MassSpectra): ", e$message))
  invisible(TRUE)
}

#' @noRd
.create_MassSpecResults_Spectra_SpectraPeaks_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS SpectraPeaks (
    analysis  VARCHAR,
    replicate VARCHAR,
    id        VARCHAR,
    polarity  INTEGER,
    rt        DOUBLE,
    mass      DOUBLE,
    mass_min  DOUBLE,
    mass_max  DOUBLE,
    intensity DOUBLE,
    area      DOUBLE,
    sn        DOUBLE,
    width     DOUBLE
  )")
  invisible(TRUE)
}

#' @noRd
.validate_MassSpecResults_Spectra_SpectraPeaks_db_schema <- function(conn) {
  tryCatch({
    ti <- DBI::dbGetQuery(conn, "PRAGMA table_info(SpectraPeaks)")
    required <- list(analysis = "VARCHAR", replicate = "VARCHAR", id = "VARCHAR",
                     polarity = "INTEGER", rt = "DOUBLE",
                     mass = "DOUBLE", mass_min = "DOUBLE", mass_max = "DOUBLE",
                     intensity = "DOUBLE", area = "DOUBLE", sn = "DOUBLE", width = "DOUBLE")
    for (col in names(required)) {
      if (!(col %in% ti$name))
        DBI::dbExecute(conn, sprintf("ALTER TABLE SpectraPeaks ADD COLUMN \"%s\" %s", col, required[[col]]))
    }
  }, error = function(e) stop("Schema migration (SpectraPeaks): ", e$message))
  invisible(TRUE)
}
