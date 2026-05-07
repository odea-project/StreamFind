# MARK: MassSpecAnalyses
#' @title Database-backed Mass Spectrometry Analyses
#' @description The `MassSpecAnalyses` class stores MassSpec analyses metadata and headers in a DuckDB file within a project directory.
#' @template arg-projectPath
#' @template arg-ms-files
#' @template arg-ms-centroid
#' @template arg-ms-levels
#' @return An object of class `MassSpecAnalyses`, extending `Analyses`.
#' @template arg-plot-colorPalette
#' @export
#'
MassSpecAnalyses <- function(
    projectPath = ".",
    files = NULL,
    centroid = FALSE,
    levels = c(1, 2)) {
  projectPath <- .normalize_massspec_project_db_path(projectPath)
  db <- projectPath
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  project_id <- .project_id_from_db_path(db)
  project <- Project$new(db = db, project_id = project_id)
  mass_spec <- ProjectMassSpec$new(db = db, project_id = project_id, .ptr = project$get_ptr())
  if (!is.null(files)) {
    cache_db <- file.path(dirname(db), "Cache.duckdb")
    analyses <- .parse_ms_from_files(files, centroid = centroid, levels = levels, cache_db = cache_db)
    if (is.null(analyses) || length(analyses) == 0) {
      stop("No analyses parsed from files.")
    }
    .write_massspec_analyses_to_project(mass_spec, analyses, truncate = TRUE)
  }
  obj <- structure(
    list(
      db = db,
      dataType = "MassSpec",
      project = project,
      project_mass_spec = mass_spec,
      project_id = project_id
    ),
    class = c("MassSpecAnalyses", "Analyses")
  )
  if (!is.null(validate_object(obj))) stop("Invalid MassSpecAnalyses object!")
  obj
}

# MARK: validate_object
#' @describeIn MassSpecAnalyses Validate the MassSpecAnalyses object (schema + type).
#' @template arg-x-MassSpecAnalyses
#' @export
#'
validate_object.MassSpecAnalyses <- function(x) {
  checkmate::assert_class(x, "MassSpecAnalyses")
  checkmate::assert_true(identical(x$dataType, "MassSpec"))
  if (!file.exists(x$db)) stop("MassSpecAnalyses file not found: ", x$db)
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  required_tables <- c("MS_ANALYSES", "MS_SPECTRA_HEADERS", "MS_CHROMATOGRAMS_HEADERS")
  present <- DBI::dbListTables(conn)
  if (!all(required_tables %in% present)) stop("Missing required tables in MassSpecAnalyses")
  NextMethod()
}

# MARK: query_db (dispatch to base)
#' @describeIn MassSpecAnalyses Internal: execute a query on the DB (delegates to Analyses).
#' @template arg-x-MassSpecAnalyses
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
query_db.MassSpecAnalyses <- function(x, sql, params = NULL) {
  NextMethod()
}

# MARK: list_db_tables (dispatch to base)
#' @describeIn MassSpecAnalyses Internal: list tables in the DB (delegates to Analyses).
#' @template arg-x-MassSpecAnalyses
#' @export
list_db_tables.MassSpecAnalyses <- function(x) {
  NextMethod()
}

# MARK: get_db_table_info (dispatch to base)
#' @describeIn MassSpecAnalyses Internal: get table info from the DB (delegates to Analyses).
#' @template arg-x-MassSpecAnalyses
#' @template arg-sql-tableName
#' @export
get_db_table_info.MassSpecAnalyses <- function(x, tableName) {
  NextMethod()
}

# MARK: add_analyses
#' @describeIn MassSpecAnalyses Add analyses to the DB (append; overwrites only duplicates)
#' @template arg-x-MassSpecAnalyses
#' @template arg-ms-files
#' @template arg-ms-centroid
#' @template arg-ms-levels
#' @export
add_analyses.MassSpecAnalyses <- function(x, files, centroid = FALSE, levels = c(1, 2)) {
  cache_db <- file.path(dirname(x$db), "Cache.duckdb")
  analyses <- .parse_ms_from_files(files, centroid = centroid, levels = levels, cache_db = cache_db)
  if (is.null(analyses) || length(analyses) == 0) {
    stop("No analyses parsed from files.")
  }
  .write_massspec_analyses_to_project(x$project_mass_spec, analyses, truncate = FALSE)
  message("Analyses added to DB.")
  invisible(x)
}

#' MARK: remove_analyses
#' @describeIn MassSpecAnalyses Remove analyses from the DB.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @export
#'
remove_analyses.MassSpecAnalyses <- function(x, analyses) {
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel_names) == 0) {
    message("No analyses to remove.")
    return(invisible(x))
  }
  for (aname in sel_names) {
    x$project_mass_spec$remove_analysis(aname)
  }
  message("Analyses removed from DB.")
  invisible(x)
}

# MARK: get_analysis_names
#' @describeIn MassSpecAnalyses Get analysis names.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
get_analysis_names.MassSpecAnalyses <- function(x) {
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  all_analyses <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY lower(analysis), analysis")$analysis
  res <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY lower(analysis), analysis")$analysis
  names(res) <- all_analyses
  res
}

# MARK: get_replicate_names
#' @describeIn MassSpecAnalyses Get replicate names.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
get_replicate_names.MassSpecAnalyses <- function(x) {
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  all_analyses <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY lower(analysis), analysis")$analysis
  res <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses ORDER BY lower(analysis), analysis")$replicate
  names(res) <- all_analyses
  res
}

# MARK: get_blank_names
#' @describeIn MassSpecAnalyses Get blank names.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
get_blank_names.MassSpecAnalyses <- function(x) {
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  all_analyses <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY lower(analysis), analysis")$analysis
  res <- DBI::dbGetQuery(conn, "SELECT blank FROM Analyses ORDER BY lower(analysis), analysis")$blank
  names(res) <- all_analyses
  res
}

# MARK: set_replicate_names
#' @describeIn MassSpecAnalyses Set replicate names.
#' @template arg-x-MassSpecAnalyses
#' @param value Character vector of replicate names matching the number of analyses.
#' @export
#'
set_replicate_names.MassSpecAnalyses <- function(x, value) {
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  current <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY lower(analysis), analysis")$analysis
  if (length(value) != length(current)) {
    stop("Length of value must equal the number of analyses.")
  }
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
  for (i in seq_along(current)) {
    DBI::dbExecute(conn,
      "UPDATE MS_ANALYSES SET replicate = ? WHERE project_id = ? AND analysis = ?",
      params = list(value[i], x$project_id, current[i])
    )
  }
  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
  message("Replicate names updated.")
  invisible(x)
}

# MARK: set_blank_names
#' @describeIn MassSpecAnalyses Set blank names.
#' @template arg-x-MassSpecAnalyses
#' @param value Character vector of blank names matching the number of analyses.
#' @export
#'
set_blank_names.MassSpecAnalyses <- function(x, value) {
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  current <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY lower(analysis), analysis")$analysis
  if (length(value) != length(current)) {
    stop("Length of value must equal the number of analyses.")
  }
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
  for (i in seq_along(current)) {
    DBI::dbExecute(conn,
      "UPDATE MS_ANALYSES SET blank = ? WHERE project_id = ? AND analysis = ?",
      params = list(value[i], x$project_id, current[i])
    )
  }
  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
  message("Blank names updated.")
  invisible(x)
}

# MARK: get_concentrations
#' @describeIn MassSpecAnalyses Get concentrations.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
get_concentrations.MassSpecAnalyses <- function(x) {
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  all_analyses <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY lower(analysis), analysis")$analysis
  res <- DBI::dbGetQuery(conn, "SELECT concentration FROM Analyses ORDER BY lower(analysis), analysis")$concentration
  names(res) <- all_analyses
  res
}

# MARK: set_concentrations
#' @describeIn MassSpecAnalyses Set concentrations.
#' @template arg-x-MassSpecAnalyses
#' @param value Numeric vector of concentrations matching the number of analyses.
#' @export
#'
set_concentrations.MassSpecAnalyses <- function(x, value) {
  if (!is.numeric(value)) stop("value must be numeric.")
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  current <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY lower(analysis), analysis")$analysis
  if (length(value) != length(current)) {
    stop("Length of value must equal the number of analyses.")
  }
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
  for (i in seq_along(current)) {
    DBI::dbExecute(conn,
      "UPDATE MS_ANALYSES SET concentration = ? WHERE project_id = ? AND analysis = ?",
      params = list(value[i], x$project_id, current[i])
    )
  }
  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
  message("Concentrations updated.")
  invisible(x)
}

# MARK: info
#' @describeIn MassSpecAnalyses Get a summary table.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
info.MassSpecAnalyses <- function(x) {
  conn <- .massspec_project_connection(x)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .ensure_massspec_project_views(conn, x)
  DBI::dbGetQuery(conn, "
    SELECT
      analysis,
      replicate,
      blank,
      type,
      format,
      number_spectra AS spectra,
      number_chromatograms AS chromatograms,
      concentration
    FROM Analyses
    ORDER BY lower(analysis), analysis
  ")
}

# MARK: show
#' @describeIn MassSpecAnalyses Show method for MassSpecAnalyses objects.
#' @template arg-x-MassSpecAnalyses
#' @export
#'
show.MassSpecAnalyses <- function(x, ...) {
  cat("\n")
  cat("MassSpecAnalyses (", nrow(info(x)), ")\n")
  info_table <- info(x)
  if (nrow(info_table) > 0) {
    print(info_table)
  }
}

# MARK: get_spectra_headers
#' @describeIn MassSpecAnalyses Fetch spectra headers for a given analysis.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @export
#'
get_spectra_headers.MassSpecAnalyses <- function(x, analyses = NULL) {
  get_spectra_headers(x$project_mass_spec, analyses = analyses)
}

# MARK: get_raw_spectra_tic
#' @describeIn MassSpecAnalyses Get the total ion current (TIC) spectra for the specified analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @export
#'
get_raw_spectra_tic.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    levels = c(1, 2),
    rt = NULL) {
  get_raw_spectra_tic(x$project_mass_spec, analyses = analyses, levels = levels, rt = rt)
}

# MARK: plot_spectra_tic
#' @describeIn MassSpecAnalyses Plot total ion current (TIC) spectra for the specified analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @template arg-plot-downsize
#' @template arg-plot-xLab
#' @template arg-plot-yLab
#' @template arg-plot-title
#' @template arg-plot-groupBy
#' @template arg-plot-interactive
#' @template arg-plot-colorPalette
#' @export
#'
plot_spectra_tic.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    levels = c(1, 2),
    rt = NULL,
    downsize = NULL,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "analysis",
    interactive = TRUE,
    colorPalette = NULL) {
  plot_spectra_tic(
    x$project_mass_spec,
    analyses = analyses,
    levels = levels,
    rt = rt,
    downsize = downsize,
    xLab = xLab,
    yLab = yLab,
    title = title,
    groupBy = groupBy,
    interactive = interactive,
    colorPalette = colorPalette
  )
}

# MARK: get_raw_spectra_bpc
#' @describeIn MassSpecAnalyses Get the base peak chromatograms (BPC) spectra for the specified analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @export
#'
get_raw_spectra_bpc.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    levels = c(1, 2),
    rt = NULL) {
  get_raw_spectra_bpc(x$project_mass_spec, analyses = analyses, levels = levels, rt = rt)
}

# MARK: plot_spectra_bpc
#' @describeIn MassSpecAnalyses Plot base peak chromatogram (BPC) spectra for the specified analyses.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @template arg-plot-downsize
#' @template arg-plot-xLab
#' @template arg-plot-yLab
#' @template arg-plot-title
#' @template arg-plot-groupBy
#' @template arg-plot-interactive
#' @template arg-plot-colorPalette
#' @export
#'
plot_spectra_bpc.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    levels = c(1, 2),
    rt = NULL,
    downsize = NULL,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "analysis",
    interactive = TRUE,
    colorPalette = NULL) {
  plot_spectra_bpc(
    x$project_mass_spec,
    analyses = analyses,
    levels = levels,
    rt = rt,
    downsize = downsize,
    xLab = xLab,
    yLab = yLab,
    title = title,
    groupBy = groupBy,
    interactive = interactive,
    colorPalette = colorPalette
  )
}

# MARK: get_raw_spectra
#' @describeIn MassSpecAnalyses Get raw spectra data from specified analyses, returning a `data.table` with the spectra data.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-allTraces
#' @template arg-ms-isolationWindow
#' @template arg-ms-minIntensityMS1
#' @template arg-ms-minIntensityMS2
#' @export
#'
get_raw_spectra.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    levels = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    id = NULL,
    allTraces = TRUE,
    isolationWindow = 1.3,
    minIntensityMS1 = 0,
    minIntensityMS2 = 0) {
  get_raw_spectra(
    x$project_mass_spec,
    analyses = analyses,
    levels = levels,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    id = id,
    allTraces = allTraces,
    isolationWindow = isolationWindow,
    minIntensityMS1 = minIntensityMS1,
    minIntensityMS2 = minIntensityMS2
  )
}

# MARK: get_raw_spectra_eic
#' @describeIn MassSpecAnalyses Get extracted ion chromatograms (EIC) for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @export
#'
get_raw_spectra_eic.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    id = NULL) {
  get_raw_spectra_eic(
    x$project_mass_spec,
    analyses = analyses,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    id = id
  )
}

# MARK: plot_spectra_eic
#' @describeIn MassSpecAnalyses Plot extracted ion chromatograms (EIC) for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-plot-downsize
#' @template arg-plot-xLab
#' @template arg-plot-yLab
#' @template arg-plot-title
#' @template arg-plot-groupBy
#' @template arg-plot-interactive
#' @template arg-plot-colorPalette
#' @export
#'
plot_spectra_eic.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    id = NULL,
    downsize = NULL,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = c("analysis", "id"),
    interactive = TRUE,
    colorPalette = NULL) {
  plot_spectra_eic(
    x$project_mass_spec,
    analyses = analyses,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    id = id,
    downsize = downsize,
    xLab = xLab,
    yLab = yLab,
    title = title,
    groupBy = groupBy,
    interactive = interactive,
    colorPalette = colorPalette
  )
}

# MARK: get_raw_spectra_ms1
#' @describeIn MassSpecAnalyses Get MS1 spectra for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @export
#'
get_raw_spectra_ms1.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    id = NULL,
    mzClust = 0.003,
    presence = 0.8,
    minIntensity = 1000) {
  get_raw_spectra_ms1(
    x$project_mass_spec,
    analyses = analyses,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    id = id,
    mzClust = mzClust,
    presence = presence,
    minIntensity = minIntensity
  )
}

# MARK: get_raw_spectra_ms2
#' @describeIn MassSpecAnalyses Get MS2 spectra for the specified analyses and targets.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-id
#' @template arg-ms-isolationWindow
#' @template arg-ms-mzClust
#' @template arg-ms-presence
#' @template arg-ms-minIntensity
#' @export
#'
get_raw_spectra_ms2.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    id = NULL,
    isolationWindow = 1.3,
    mzClust = 0.005,
    presence = 0.8,
    minIntensity = 0) {
  get_raw_spectra_ms2(
    x$project_mass_spec,
    analyses = analyses,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    id = id,
    isolationWindow = isolationWindow,
    mzClust = mzClust,
    presence = presence,
    minIntensity = minIntensity
  )
}

# MARK: get_chromatograms_headers
#' @describeIn MassSpecAnalyses Fetch chromatograms headers for a given analysis.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @export
#'
get_chromatograms_headers.MassSpecAnalyses <- function(x, analyses = NULL) {
  get_chromatograms_headers(x$project_mass_spec, analyses = analyses)
}

# MARK: get_chromatograms
#' @describeIn MassSpecAnalyses Get chromatograms for the specified analyses and chromatogram IDs/indices.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @export
#'
get_chromatograms.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    chromatograms = NULL,
    rtmin = 0,
    rtmax = 0,
    minIntensity = NULL) {
  get_chromatograms(
    x$project_mass_spec,
    analyses = analyses,
    chromatograms = chromatograms,
    rtmin = rtmin,
    rtmax = rtmax,
    minIntensity = minIntensity
  )
}

# MARK: plot_chromatograms
#' @describeIn MassSpecAnalyses Plot chromatograms for the specified analyses and chromatogram IDs/indices.
#' @template arg-x-MassSpecAnalyses
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @template arg-plot-downsize
#' @template arg-plot-xLab
#' @template arg-plot-yLab
#' @template arg-plot-title
#' @template arg-plot-groupBy
#' @template arg-plot-interactive
#' @template arg-plot-colorPalette
#' @export
#'
plot_chromatograms.MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    chromatograms = NULL,
    rtmin = 0,
    rtmax = 0,
    minIntensity = NULL,
    downsize = NULL,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "analysis",
    interactive = TRUE,
    colorPalette = NULL) {
  plot_chromatograms(
    x$project_mass_spec,
    analyses = analyses,
    chromatograms = chromatograms,
    rtmin = rtmin,
    rtmax = rtmax,
    minIntensity = minIntensity,
    downsize = downsize,
    xLab = xLab,
    yLab = yLab,
    title = title,
    groupBy = groupBy,
    interactive = interactive,
    colorPalette = colorPalette
  )
}


# MARK: .validate_MassSpecAnalyses_analyses_dt
#' @noRd
.validate_MassSpecAnalyses_analyses_dt <- function(x) {
  cols <- c(
    "analysis", "replicate", "blank", "file", "format", "type", "polarity",
    "spectra_number", "chromatograms_number", "concentration"
  )
  missing_cols <- setdiff(cols, colnames(x))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in analyses data.table: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}

# MARK: shared-project helpers
#' @noRd
.normalize_massspec_project_db_path <- function(projectPath) {
  checkmate::assert_character(projectPath, len = 1, null.ok = FALSE)
  ext <- tolower(tools::file_ext(projectPath))
  if (ext %in% c("duckdb", "db")) {
    return(projectPath)
  }
  file.path(projectPath, "StreamFind.duckdb")
}

#' @noRd
.project_id_from_db_path <- function(db) {
  tools::file_path_sans_ext(basename(db))
}

#' @noRd
.massspec_project_connection <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  .ensure_massspec_project_views(conn, x)
  conn
}

#' @noRd
.ensure_massspec_project_views <- function(conn, x) {
  project_id_sql <- as.character(DBI::dbQuoteString(conn, x$project_id))
  DBI::dbExecute(conn, paste0(
    "CREATE OR REPLACE TEMP VIEW Analyses AS ",
    "SELECT analysis, replicate, blank, file_path AS file, format, type, NULL::VARCHAR AS polarity, ",
    "number_spectra AS spectra_number, number_chromatograms AS chromatograms_number, concentration ",
    "FROM MS_ANALYSES WHERE project_id = ", project_id_sql
  ))
  DBI::dbExecute(conn, paste0(
    "CREATE OR REPLACE TEMP VIEW SpectraHeaders AS ",
    "SELECT analysis, index, scan, array_length, level, mode, polarity, configuration, lowmz, highmz, bpmz, bpint, tic, rt, mobility, window_mz, ",
    "window_mzlow AS pre_mzlow, window_mzhigh AS pre_mzhigh, precursor_mz AS pre_mz, precursor_charge AS pre_charge, precursor_intensity AS pre_intensity, activation_ce AS pre_ce ",
    "FROM MS_SPECTRA_HEADERS WHERE project_id = ", project_id_sql
  ))
  DBI::dbExecute(conn, paste0(
    "CREATE OR REPLACE TEMP VIEW ChromatogramsHeaders AS ",
    "SELECT analysis, index, id, array_length, polarity, precursor_mz AS pre_mz, product_mz AS pro_mz, activation_ce AS pre_ce ",
    "FROM MS_CHROMATOGRAMS_HEADERS WHERE project_id = ", project_id_sql
  ))
  invisible(TRUE)
}

#' @noRd
.write_massspec_analyses_to_project <- function(project_mass_spec, analyses, truncate = FALSE) {
  incoming_names <- vapply(analyses, function(a) a$name, "")
  dup_incoming <- incoming_names[duplicated(incoming_names)]
  if (length(dup_incoming) > 0) {
    stop("Duplicate analyses within import batch: ", paste(unique(dup_incoming), collapse = ", "))
  }
  if (truncate) {
    existing <- project_mass_spec$list_analyses()
    if (nrow(existing) > 0) {
      for (name in existing$analysis) {
        project_mass_spec$remove_analysis(name)
      }
    }
  }
  analyses <- analyses[order(incoming_names)]
  project_mass_spec$import_files(
    file_paths = vapply(analyses, function(ana) ana$file, ""),
    analyses = vapply(analyses, function(ana) ana$name, ""),
    replicates = vapply(analyses, function(ana) {
      if (is.null(ana$replicate) || is.na(ana$replicate)) "" else as.character(ana$replicate)
    }, ""),
    blanks = vapply(analyses, function(ana) {
      if (is.null(ana$blank) || is.na(ana$blank)) "" else as.character(ana$blank)
    }, "")
  )

  if (length(analyses) > 0) {
    conn <- DBI::dbConnect(duckdb::duckdb(), project_mass_spec$db)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    for (ana in analyses) {
      concentration <- if (is.null(ana$concentration) || is.na(ana$concentration)) NA_real_ else as.numeric(ana$concentration)
      DBI::dbExecute(conn,
        "UPDATE MS_ANALYSES SET concentration = ? WHERE project_id = ? AND analysis = ?",
        params = list(concentration, project_mass_spec$project_id, ana$name)
      )
    }
  }
  invisible(TRUE)
}

# MARK: .create_MassSpecAnalyses_Analyses_db_schema
#' @noRd
.create_MassSpecAnalyses_Analyses_db_schema <- function(conn) {
  DBI::dbExecute(conn, "INSTALL json")
  DBI::dbExecute(conn, "LOAD json")
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Analyses (
      analysis VARCHAR PRIMARY KEY,
      replicate VARCHAR,
      blank VARCHAR,
      file VARCHAR NOT NULL,
      format VARCHAR,
      type VARCHAR,
      polarity VARCHAR,
      spectra_number INTEGER,
      chromatograms_number INTEGER,
      concentration DOUBLE
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS SpectraHeaders (
      analysis VARCHAR,
      index INTEGER,
      scan INTEGER,
      array_length INTEGER,
      level INTEGER,
      mode INTEGER,
      polarity INTEGER,
      configuration INTEGER,
      lowmz DOUBLE,
      highmz DOUBLE,
      bpmz DOUBLE,
      bpint DOUBLE,
      tic DOUBLE,
      rt DOUBLE,
      mobility DOUBLE,
      window_mz DOUBLE,
      pre_mzlow DOUBLE,
      pre_mzhigh DOUBLE,
      pre_mz DOUBLE,
      pre_charge INTEGER,
      pre_intensity DOUBLE,
      pre_ce DOUBLE
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS ChromatogramsHeaders (
      analysis VARCHAR,
      index INTEGER,
      id VARCHAR,
      array_length INTEGER,
      polarity INTEGER,
      pre_mz DOUBLE,
      pro_mz DOUBLE,
      pre_ce DOUBLE
    )
  ")
  invisible(TRUE)
}

# MARK: .validate_MassSpecAnalyses_Analyses_db_schema
#' @noRd
.validate_MassSpecAnalyses_Analyses_db_schema <- function(conn) {
  tryCatch(
    {
      table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Analyses)")
      required <- list(
        analysis = "VARCHAR PRIMARY KEY",
        replicate = "VARCHAR",
        blank = "VARCHAR",
        file = "VARCHAR NOT NULL",
        format = "VARCHAR",
        type = "VARCHAR",
        polarity = "VARCHAR",
        spectra_number = "INTEGER",
        chromatograms_number = "INTEGER",
        concentration = "DOUBLE"
      )
      for (col in names(required)) {
        if (!(col %in% table_info$name)) {
          message(sprintf("Adding missing %s column to Analyses table...", col))
          DBI::dbExecute(conn, sprintf("ALTER TABLE Analyses ADD COLUMN %s %s", col, required[[col]]))
        }
      }
    },
    error = function(e) {
      stop("Schema migration check (Analyses): ", e$message)
    }
  )
  invisible(TRUE)
}

# MARK: .validate_MassSpecAnalyses_Spectra_db_schema
#' @noRd
.validate_MassSpecAnalyses_Spectra_db_schema <- function(conn) {
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(SpectraHeaders)")
    required <- list(
      index = "INTEGER",
      scan = "INTEGER",
      array_length = "INTEGER",
      level = "INTEGER",
      mode = "INTEGER",
      polarity = "INTEGER",
      configuration = "INTEGER",
      lowmz = "DOUBLE",
      highmz = "DOUBLE",
      bpmz = "DOUBLE",
      bpint = "DOUBLE",
      tic = "DOUBLE",
      rt = "DOUBLE",
      mobility = "DOUBLE",
      window_mz = "DOUBLE",
      pre_mzlow = "DOUBLE",
      pre_mzhigh = "DOUBLE",
      pre_mz = "DOUBLE",
      pre_charge = "INTEGER",
      pre_intensity = "DOUBLE",
      pre_ce = "DOUBLE",
      analysis = "VARCHAR"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to SpectraHeaders table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE SpectraHeaders ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (SpectraHeaders): ", e$message)
  })
  invisible(TRUE)
}

# MARK: .validate_MassSpecAnalyses_Chromatograms_db_schema
#' @noRd
.validate_MassSpecAnalyses_Chromatograms_db_schema <- function(conn) {
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(ChromatogramsHeaders)")
    required <- list(
      index = "INTEGER",
      id = "VARCHAR",
      array_length = "INTEGER",
      polarity = "INTEGER",
      pre_mz = "DOUBLE",
      pro_mz = "DOUBLE",
      pre_ce = "DOUBLE",
      analysis = "VARCHAR"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to ChromatogramsHeaders table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE ChromatogramsHeaders ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (ChromatogramsHeaders): ", e$message)
  })
  invisible(TRUE)
}

# MARK: .write_massspec_analyses_to_db
#' @noRd
.write_massspec_analyses_to_db <- function(conn, analyses, truncate = FALSE) {
  incoming_names <- vapply(analyses, function(a) a$name, "")
  dup_incoming <- incoming_names[duplicated(incoming_names)]
  if (length(dup_incoming) > 0) {
    stop("Duplicate analyses within import batch: ", paste(unique(dup_incoming), collapse = ", "))
  }
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)

  if (truncate) {
    DBI::dbExecute(conn, "DELETE FROM Analyses")
    DBI::dbExecute(conn, "DELETE FROM SpectraHeaders")
    DBI::dbExecute(conn, "DELETE FROM ChromatogramsHeaders")
    .create_MassSpecAnalyses_Analyses_db_schema(conn)
  } else {
    # Overwrite duplicates only
    if ("Analyses" %in% DBI::dbListTables(conn)) {
      existing <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
      dup_existing <- intersect(existing, incoming_names)
    if (length(dup_existing) > 0) {
      for (nm in dup_existing) {
        DBI::dbExecute(conn, "DELETE FROM SpectraHeaders WHERE analysis = ?", params = list(nm))
        DBI::dbExecute(conn, "DELETE FROM ChromatogramsHeaders WHERE analysis = ?", params = list(nm))
        DBI::dbExecute(conn, "DELETE FROM Analyses WHERE analysis = ?", params = list(nm))
      }
      }
    }
  }

  # Keep insertion deterministic and alphabetic
  order_idx <- order(incoming_names)
  incoming_names <- incoming_names[order_idx]
  analyses <- analyses[order_idx]

  analyses_df <- data.frame(
    analysis = incoming_names,
    replicate = vapply(analyses, function(a) a$replicate, NA_character_),
    blank = vapply(analyses, function(a) a$blank, NA_character_),
    file = vapply(analyses, function(a) a$file, NA_character_),
    format = vapply(analyses, function(a) a$format, NA_character_),
    type = vapply(analyses, function(a) a$type, NA_character_),
    polarity = vapply(analyses, function(a) a$polarity, NA_character_),
    spectra_number = vapply(analyses, function(a) a$spectra_number, NA_integer_),
    chromatograms_number = vapply(analyses, function(a) a$chromatograms_number, NA_integer_),
    concentration = vapply(analyses, function(a) a$concentration, NA_real_),
    stringsAsFactors = FALSE
  )
  analyses_df <- analyses_df[order(analyses_df$analysis), , drop = FALSE]

  DBI::dbWriteTable(conn, "Analyses", analyses_df, append = TRUE)

  spectra_rows <- lapply(analyses, function(a) {
    if (is.null(a$spectra_headers)) {
      return(NULL)
    }
    df <- as.data.frame(a$spectra_headers)
    if (nrow(df) == 0) {
      return(NULL)
    }
    df$analysis <- a$name
    df
  })

  chrom_rows <- lapply(analyses, function(a) {
    if (is.null(a$chromatograms_headers)) {
      return(NULL)
    }
    df <- as.data.frame(a$chromatograms_headers)
    if (nrow(df) == 0) {
      return(NULL)
    }
    df$analysis <- a$name
    df
  })

  spectra_rows <- Filter(Negate(is.null), spectra_rows)
  chrom_rows <- Filter(Negate(is.null), chrom_rows)

  if (length(spectra_rows) > 0) {
    spectra_df <- data.table::rbindlist(spectra_rows, fill = TRUE)
    data.table::setorder(spectra_df, analysis)
    DBI::dbWriteTable(conn, "SpectraHeaders", spectra_df, append = TRUE)
  }

  if (length(chrom_rows) > 0) {
    chrom_df <- data.table::rbindlist(chrom_rows, fill = TRUE)
    data.table::setorder(chrom_df, analysis)
    DBI::dbWriteTable(conn, "ChromatogramsHeaders", chrom_df, append = TRUE)
  }

  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
}

# MARK: .parse_ms_from_files
#' @noRd
.parse_ms_from_files <- function(
    files = NULL,
    centroid = FALSE,
    levels = c(1, 2),
    cache_db = NULL) {
  if (!is.null(files)) {
    if (is.data.frame(files)) {
      if (all(c("path", "analysis") %in% colnames(files))) {
        files$file <- vapply(
          seq_len(nrow(files)),
          function(x) {
            list.files(
              files$path[x],
              pattern = files$analysis[x],
              full.names = TRUE,
              recursive = FALSE
            )
          },
          ""
        )
      }

      if ("file" %in% colnames(files)) {
        if ("replicate" %in% colnames(files)) {
          replicates <- as.character(files$replicate)
        } else if ("group" %in% colnames(files)) {
          replicates <- as.character(files$group)
        } else {
          replicates <- rep(NA_character_, nrow(files))
        }

        if ("blank" %in% colnames(files)) {
          blanks <- as.character(files$blank)
        } else {
          blanks <- rep(NA_character_, nrow(files))
        }

        files <- files$file
      } else {
        files <- NA_character_
      }
    } else {
      replicates <- rep(NA_character_, length(files))
      blanks <- rep(NA_character_, length(files))
    }

    possible_ms_file_formats <- ".mzML$|.mzXML$|.d$|.raw$|.wiff$"

    valid_files <- vapply(
      files,
      FUN.VALUE = FALSE,
      function(x, possible_ms_file_formats) {
        if (!file.exists(x)) {
          return(FALSE)
        }
        if (FALSE %in% grepl(possible_ms_file_formats, x)) {
          return(FALSE)
        }
        TRUE
      },
      possible_ms_file_formats = possible_ms_file_formats
    )

    if (!all(valid_files)) {
      warning("File/s not valid!")
      return(NULL)
    }

    names(replicates) <- as.character(files)
    names(blanks) <- as.character(files)

    files_to_convert <- vapply(
      files,
      function(x) grepl("d|raw|wiff", tools::file_ext(x)),
      FALSE
    )

    if (any(files_to_convert)) {
      files_to_convert <- files[files_to_convert]
      files_converted <- gsub(".d$", ".mzML", files_to_convert)
      files_converted <- gsub(".raw$", ".mzML", files_converted)
      files_converted <- gsub(".wiff$", ".mzML", files_converted)

      for (i in seq_along(files_converted)) {
        if (file.exists(files_converted[i])) {
          if (files_converted[i] %in% files) {
            files <- files[!files %in% files_to_convert[i]]
          } else {
            files[files == files_to_convert[i]] <- files_converted[i]
          }
        } else {
          dir_search <- dirname(files_converted[i])

          fl_already_converted <- list.files(
            dir_search,
            pattern = paste0(
              "^",
              basename(tools::file_path_sans_ext(files_converted[i])),
              "-.*\\.mzML$"
            ),
            full.names = TRUE
          )

          if (length(fl_already_converted) == 1) {
            files_converted[i] <- fl_already_converted
            files <- files[!files %in% fl_already_converted]
            files[files == files_to_convert[i]] <- fl_already_converted
          } else if (length(fl_already_converted) > 1) {
            warning(paste0(
              "Multiple converted files found for: ",
              basename(tools::file_path_sans_ext(files_to_convert[i])),
              ". Please check the files!"
            ))
            return(NULL)
          }
        }
      }

      files_to_convert_sel <- vapply(
        files,
        function(x) {
          grepl("d|raw|wiff", tools::file_ext(x))
        },
        FALSE
      )

      files_to_convert <- files[files_to_convert_sel]

      if (length(files_to_convert) > 0) {
        filter <- ""

        if (centroid) {
          filter <- "peakPicking vendor"
        }

        if (centroid && is.numeric(levels) && length(levels) > 0) {
          levels <- paste(levels, collapse = "-")
          levels <- paste("msLevel=", levels, collapse = "", sep = "")
          if (filter != "") {
            filter <- paste(filter, levels, sep = " ")
          } else {
            filter <- levels
          }
        }

        optList <- list()
        if (filter != "") {
          optList <- list(filter = filter)
        }

        tryCatch(
          {
            StreamFind::convert_ms_files(
              files = files_to_convert,
              outputFormat = "mzML",
              outputPath = NULL,
              optList = optList
            )

            files <- files[!files %in% files_to_convert]

            for (i in seq_along(files_to_convert)) {
              dir_search <- dirname(files_to_convert[i])

              fl_converted_as_is <- list.files(
                dir_search,
                pattern = paste0(
                  basename(tools::file_path_sans_ext(files_to_convert[i])),
                  ".mzML$"
                ),
                full.names = TRUE
              )

              if (length(fl_converted_as_is) == 1) {
                files <- c(files, fl_converted_as_is)
              } else if (length(fl_converted_as_is) > 1) {
                warning(paste0(
                  "Multiple converted files found for: ",
                  basename(tools::file_path_sans_ext(files_to_convert[i])),
                  ". Please check the files!"
                ))
              } else if (length(fl_converted_as_is) == 0) {
                fl_converted <- list.files(
                  dir_search,
                  pattern = paste0(
                    "^",
                    basename(tools::file_path_sans_ext(files_to_convert[i])),
                    "-.*\\.mzML$"
                  ),
                  full.names = TRUE
                )

                if (length(fl_converted) == 1) {
                  files <- c(files, fl_converted)
                } else if (length(fl_converted) > 1) {
                  warning(paste0(
                    "Multiple converted files found for: ",
                    basename(tools::file_path_sans_ext(files_to_convert[i])),
                    ". Please check the files!"
                  ))
                }
              }
            }

            exist_files <- vapply(files, function(x) file.exists(x), FALSE)
            files <- files[exist_files]
          },
          error = function(e) {
            warning("Error converting files!")
            files <- files[!files %in% files_to_convert]
          },
          warning = function(w) {
            warning("Warning converting files!")
            files <- files[!files %in% files_to_convert]
          }
        )
      }
    }

    cache_manager <- NULL
    if (file.exists(cache_db)) cache_manager <- Cache(db = cache_db)

    analyses <- lapply(files, function(x) {
      if (!is.null(cache_manager)) {
        hash <- .make_hash(x)
        cache_info <- get_cache_info(cache_manager)
        if (hash %in% cache_info$hash) {
          ana <- load_cache(cache_manager, hash = hash)
          if (!is.null(ana)) {
            message("\U1f5ab Loaded cached analysis for ", basename(x), "!")
            return(ana)
          }
        }
      }

      message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
      ana <- rcpp_streamcraft_parse_ms_analysis_from_files(x)

      class_ana <- class(ana)[1]
      if (!class_ana %in% "MassSpecAnalysis") {
        message(" Not Done!")
        return(NULL)
      }

      ana$polarity <- unique(ana$spectra_headers$polarity)
      if (!is.null(ana$polarity)) {
        if (length(ana$polarity) > 1) {
          if (all(ana$polarity %in% c(-1, 1))) {
            ana$polarity <- paste(unique(ana$polarity), collapse = ", ")
            ana$polarity <- gsub("-1", "negative", ana$polarity)
            ana$polarity <- gsub("1", "positive", ana$polarity)
          } else {
            ana$polarity <- "unknown"
          }
        }
        if (ana$polarity == 1) {
          ana$polarity <- "positive"
        } else if (ana$polarity == -1) {
          ana$polarity <- "negative"
        } else {
          ana$polarity <- "unknown"
        }
      } else {
        ana$polarity <- "unknown"
      }

      rpl <- replicates[x]
      if (is.na(rpl)) {
        rpl <- ana$name
        rpl <- sub("-[^-]+$", "", rpl)
      }
      ana$replicate <- rpl

      blk <- blanks[x]
      if (!is.na(blk)) {
        ana$blank <- blk
      }
      ana$blank <- blk

      concentration <- suppressWarnings(as.numeric(ana$name))
      if (is.na(concentration)) {
        ana$concentration <- NA_real_
      } else {
        ana$concentration <- concentration
      }

      message(" Done!")

      if (!is.null(cache_manager)) {
        save_cache(
          cache_manager,
          name = "Parsed MassSpecAnalysis",
          hash = hash,
          description = paste0("Parsed analysis from file ", basename(x)),
          data = ana
        )
        message("\U1f5ab Parsed analysis cached!")
      }

      ana
    })

    names(analyses) <- vapply(analyses, function(x) x[["name"]], "")
    analyses <- analyses[order(names(analyses))]

    if (
      all(vapply(analyses, function(x) "MassSpecAnalysis" %in% is(x), FALSE))
    ) {
      analyses
    } else {
      list()
    }
  } else {
    list()
  }
}
