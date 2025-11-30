# MARK: MassSpecResults_NonTargetAnalysisDB
#' @title Constructor and methods to handle non-target analysis results for mass spectrometry data
#' @description The `MassSpecResults_NonTargetAnalysis2` class is a child of the [StreamFind::Results] class and is used to store results from non-target analysis (NTA) workflows for mass spectrometry data ("MassSpec"). It is specifically designed to handle the output from `rcpp_nts_find_features2()`.
#' @param info A data frame containing information about the analyses.
#' @param headers A list of data frames containing information about the spectra headers.
#' @param features A list of data frames containing information about the features detected by `rcpp_nts_find_features2()`.
#' @return An object of class `MassSpecResults_NonTargetAnalysis2` with the following structure:
#' \itemize{
#'   \item `type`: The type of the results, which is "MassSpec".
#'   \item `name`: The name of the results, which is "MassSpecResults_NonTargetAnalysis2".
#'   \item `software`: The software used for the analysis, which is "StreamFind".
#'   \item `version`: The version of the software, as a character string.
#'   \item `info`: A data frame containing information about the analyses.
#'   \item `headers`: A list of data frames containing information about the spectra headers.
#'   \item `features`: A list of data frames containing information about the features.
#' }
#' The `info` data.table contains the following columns: analysis, replicate, blank, polarity and file. Each `features` data frame contains the following columns from `rcpp_nts_find_features2()`:
#' \itemize{
#'   \item Core feature properties: feature, group, component, adduct, rt, mz, mass, intensity, noise, sn, area
#'   \item Chromatographic boundaries: rtmin, rtmax, width, mzmin, mzmax, ppm
#'   \item Peak shape characterization: fwhm_rt, fwhm_mz
#'   \item Gaussian fitting parameters: gaussian_A, gaussian_mu, gaussian_sigma, gaussian_r2
#'   \item Processing flags: polarity, filtered, filter, filled, correction
#'   \item Encoded profile data: eic_size, eic_rt, eic_mz, eic_intensity, eic_baseline, eic_smoothed
#'   \item MS spectral data: ms1_size, ms1_mz, ms1_intensity, ms2_size, ms2_mz, ms2_intensity

#' }
#' @export
#'
MassSpecResults_NonTargetAnalysisDB <- function(
  db = file.path("data.sf", "MassSpecResults_NonTargetAnalysis.duckdb"),
  analyses = data.table::data.table(),
  headers = data.table::data.table(),
  features = data.table::data.table()
) {
  if (!requireNamespace("DBI", quietly = TRUE)) stop("DBI package required.")
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("duckdb package required.")
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_MassSpecAnalysesDB_Analyses_db_schema(conn)
  .validate_MassSpecAnalysesDB_Analyses_db_schema(conn)
  .create_MassSpecResults_NonTargetAnalysisDB_Features_db_schema(conn)
  .validate_MassSpecResults_NonTargetAnalysisDB_Features_db_schema(conn)

  insert_analyses <- function(analyses) {
    .validate_MassSpecAnalysesDB_analyses_dt(analyses)
    DBI::dbExecute(conn, "DELETE FROM Analyses")
    DBI::dbWriteTable(conn, "Analyses", analyses, overwrite = TRUE)
  }

  insert_headers <- function(headers) {
    DBI::dbExecute(conn, "DROP TABLE IF EXISTS SpectraHeaders")
    DBI::dbWriteTable(conn, "SpectraHeaders", headers, overwrite = TRUE)
  }

  insert_features <- function(features) {
    .validate_MassSpecResults_NonTargetAnalysisDB_features_dt(features)
    DBI::dbExecute(conn, "DELETE FROM Features")
    DBI::dbWriteTable(conn, "Features", features, overwrite = TRUE)
  }

  if (nrow(analyses) > 0) insert_analyses(analyses)
  if (nrow(headers) > 0) insert_headers(headers)
  if (nrow(features) > 0) insert_features(features)

  x <- structure(
    list(
      db = db,
      data_type = "MassSpec"
    ),
    class = c("MassSpecResults_NonTargetAnalysisDB", "ResultsDB")
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid MassSpecResults_NonTargetAnalysisDB object.")
  }
}

#' @describeIn MassSpecResults_NonTargetAnalysisDB Validates the MassSpecResults_NonTargetAnalysisDB object, returning NULL if valid.
#' @template arg-ntsdb-x
#' @export
#'
validate_object.MassSpecResults_NonTargetAnalysisDB <- function(x) {
  checkmate::assert_class(x, "MassSpecResults_NonTargetAnalysisDB")
  checkmate::assert_true(identical(x$data_type, "MassSpec"))
  if (!file.exists(x$db)) stop("MassSpecResults_NonTargetAnalysisDB file not found: ", x$db)
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  required_tables <- c("Analyses", "SpectraHeaders", "Features")
  present <- DBI::dbListTables(conn)
  if (!all(required_tables %in% present)) stop("Missing required tables in MassSpecResults_NonTargetAnalysisDB")
  .validate_MassSpecAnalysesDB_Analyses_db_schema(conn)
  .validate_MassSpecResults_NonTargetAnalysisDB_Features_db_schema(conn)
  NextMethod()
}

# MARK: show
#' @describeIn MassSpecResults_NonTargetAnalysisDB Prints a summary of the MassSpecResults_NonTargetAnalysisDB object.
#' @template arg-ntsdb-x
#' @export
show.MassSpecResults_NonTargetAnalysisDB <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  cat("\n")
  cat(is(x))
  cat("\n")
  analyses <- DBI::dbReadTable(conn, "Analyses")
  features <- DBI::dbGetQuery(conn, "
    SELECT analysis,
      COUNT(*) AS feature,
      SUM(CASE WHEN filtered THEN 1 ELSE 0 END) AS filtered,
      SUM(CASE WHEN NOT filtered THEN 1 ELSE 0 END) AS not_filtered
    FROM Features
    GROUP BY analysis
  ")
  # groups <- DBI::dbGetQuery(conn, "
  #   SELECT analysis, COUNT(DISTINCT [group]) AS groups
  #   FROM Features
  #   WHERE NOT filtered
  #   GROUP BY analysis
  # ")
  info <- data.table::data.table(
    "analysis" = analyses$analysis,
    "replicate" = analyses$replicate,
    "blank" = analyses$blank,
    "polarity" = analyses$polarity,
    "features" = features$not_filtered[match(analyses$analysis, features$analysis)],
    "filtered" = features$filtered[match(analyses$analysis, features$analysis)]
    # "groups" = groups$groups[match(analyses$analysis, groups$analysis)],
  )
  print(info)
}

# MARK: .validate_MassSpecResults_NonTargetAnalysisDB_features_dt
#' @noRd
.validate_MassSpecResults_NonTargetAnalysisDB_features_dt <- function(x) {
  cols <- c(
    "feature", "component", "adduct", "rt", "mz", "mass", "intensity",
    "noise", "sn", "area", "rtmin", "rtmax", "width", "mzmin", "mzmax", "ppm",
    "fwhm_rt", "fwhm_mz", "gaussian_A", "gaussian_mu", "gaussian_sigma",
    "gaussian_r2", "polarity", "filtered", "filter", "filled", "correction",
    "eic_size", "eic_rt", "eic_mz", "eic_intensity", "eic_baseline",
    "eic_smoothed",  "ms1_size", "ms1_mz", "ms1_intensity", "ms2_size",
    "ms2_mz", "ms2_intensity"
  )
  missing_cols <- setdiff(cols, colnames(x))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in features data.table: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}

# MARK: .create_MassSpecResults_NonTargetAnalysisDB_Features_db_schema
#' @noRd
.create_MassSpecResults_NonTargetAnalysisDB_Features_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS Features (
    analysis VARCHAR PRIMARY KEY,
    feature VARCHAR,
    component VARCHAR,
    adduct VARCHAR,
    rt DOUBLE,
    mz DOUBLE,
    mass DOUBLE,
    intensity DOUBLE,
    noise DOUBLE,
    sn DOUBLE,
    area DOUBLE,
    rtmin DOUBLE,
    rtmax DOUBLE,
    width DOUBLE,
    mzmin DOUBLE,
    mzmax DOUBLE,
    ppm DOUBLE,
    fwhm_rt DOUBLE,
    fwhm_mz DOUBLE,
    gaussian_A DOUBLE,
    gaussian_mu DOUBLE,
    gaussian_sigma DOUBLE,
    gaussian_r2 DOUBLE,
    polarity INTEGER,
    filtered BOOLEAN,
    filter VARCHAR,
    filled BOOLEAN,
    correction DOUBLE,
    eic_size INTEGER,
    eic_rt VARCHAR,
    eic_mz VARCHAR,
    eic_intensity VARCHAR,
    eic_baseline VARCHAR,
    eic_smoothed VARCHAR,
    ms1_size INTEGER,
    ms1_mz VARCHAR,
    ms1_intensity VARCHAR,
    ms2_size INTEGER,
    ms2_mz VARCHAR,
    ms2_intensity VARCHAR
  )")

  invisible(TRUE)
}


# MARK: .validate_MassSpecResults_NonTargetAnalysisDB_Features_db_schema
#' @noRd
.validate_MassSpecResults_NonTargetAnalysisDB_Features_db_schema <- function(conn) {
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Features)")
    required <- list(
      feature = "VARCHAR",
      component = "VARCHAR",
      adduct = "VARCHAR",
      rt = "DOUBLE",
      mz = "DOUBLE",
      mass = "DOUBLE",
      intensity = "DOUBLE",
      noise = "DOUBLE",
      sn = "DOUBLE",
      area = "DOUBLE",
      rtmin = "DOUBLE",
      rtmax = "DOUBLE",
      width = "DOUBLE",
      mzmin = "DOUBLE",
      mzmax = "DOUBLE",
      ppm = "DOUBLE",
      fwhm_rt = "DOUBLE",
      fwhm_mz = "DOUBLE",
      gaussian_A = "DOUBLE",
      gaussian_mu = "DOUBLE",
      gaussian_sigma = "DOUBLE",
      gaussian_r2 = "DOUBLE",
      polarity = "INTEGER",
      filtered = "BOOLEAN",
      filter = "VARCHAR",
      filled = "BOOLEAN",
      correction = "DOUBLE",
      eic_size = "INTEGER",
      eic_rt = "VARCHAR",
      eic_mz = "VARCHAR",
      eic_intensity = "VARCHAR",
      eic_baseline = "VARCHAR",
      eic_smoothed = "VARCHAR",
      ms1_size = "INTEGER",
      ms1_mz = "VARCHAR",
      ms1_intensity = "VARCHAR",
      ms2_size = "INTEGER",
      ms2_mz = "VARCHAR",
      ms2_intensity = "VARCHAR"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to Features table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Features ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (Features): ", e$message)
  })
  invisible(TRUE)
}