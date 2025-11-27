# MARK: MassSpecAnalysesDB
#' @title Database-backed Mass Spectrometry Analyses
#' @description The `MassSpecAnalysesDB` class stores MassSpec analyses metadata and headers in a DuckDB file.
#' It mirrors the structure of [StreamFind::MassSpecAnalyses] but keeps data on disk.
#' @param db Path to the MassSpec analyses DuckDB file. Defaults to `data.sf/MassSpecAnalyses.duckdb`.
#' @param files Optional vector or data.frame (see [StreamFind::.get_MassSpecAnalysis_from_files]) to import on creation.
#' @param centroid Logical, passed to `.get_MassSpecAnalysis_from_files` during import.
#' @param levels Integer vector, MS levels to keep during import.
#' @return An object of class `MassSpecAnalysesDB`, extending `AnalysesDB`.
#' @export
#' 
MassSpecAnalysesDB <- function(
  db = file.path("data.sf", "MassSpecAnalyses.duckdb"),
  files = NULL,
  centroid = FALSE,
  levels = c(1, 2)
) {
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  .create_massspec_analyses_db_schema(conn)

  # Inline import of analyses if files are provided (replace existing data)
  if (!is.null(files)) {
    analyses <- .get_MassSpecAnalysis_from_files(files, centroid = centroid, levels = levels)
    if (is.null(analyses) || length(analyses) == 0) {
      stop("No analyses parsed from files.")
    }
    .write_massspec_analyses_to_db(db, analyses, truncate = TRUE)
  }

  obj <- structure(
    list(
      db = db,
      data_type = "MassSpec"
    ),
    class = c("MassSpecAnalysesDB", "AnalysesDB")
  )
  if (!is.null(validate_object(obj))) stop("Invalid MassSpecAnalysesDB object!")
  obj
}

# MARK: validate_object
#' @describeIn MassSpecAnalysesDB Validate the MassSpecAnalysesDB object (schema + type).
#' @param x MassSpecAnalysesDB object.
#' @export
#' 
validate_object.MassSpecAnalysesDB <- function(x) {
  checkmate::assert_class(x, "MassSpecAnalysesDB")
  checkmate::assert_true(identical(x$data_type, "MassSpec"))
  if (!file.exists(x$db)) stop("MassSpecAnalysesDB file not found: ", x$db)
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  required_tables <- c("Analyses", "SpectraHeaders", "ChromatogramsHeaders")
  present <- DBI::dbListTables(conn)
  if (!all(required_tables %in% present)) stop("Missing required tables in MassSpecAnalysesDB")
  NextMethod()
}

#' @describeIn MassSpecAnalysesDB Add analyses to the DB (append; overwrites only duplicates)
#' @param files Files/data.frame accepted by `.get_MassSpecAnalysis_from_files`.
#' @param centroid Logical, passed to parser.
#' @param levels Integer vector, MS levels to keep.
#' @export
add_analyses.MassSpecAnalysesDB <- function(x, files, centroid = FALSE, levels = c(1, 2)) {
  analyses <- .get_MassSpecAnalysis_from_files(files, centroid = centroid, levels = levels)
  if (is.null(analyses) || length(analyses) == 0) {
    stop("No analyses parsed from files.")
  }
  .write_massspec_analyses_to_db(x$db, analyses, truncate = FALSE)
  message("Analyses added to DB.")
  invisible(x)
}

# MARK: get_analysis_names
#' @describeIn MassSpecAnalysesDB Get analysis names.
#' @param x MassSpecAnalysesDB object.
#' @export
#' 
get_analysis_names.MassSpecAnalysesDB <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
}

# MARK: get_replicate_names
#' @describeIn MassSpecAnalysesDB Get replicate names.
#' @param x MassSpecAnalysesDB object.
#' @export
#' 
get_replicate_names.MassSpecAnalysesDB <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses ORDER BY analysis")$replicate
}

# MARK: get_blank_names
#' @describeIn MassSpecAnalysesDB Get blank names.
#' @param x MassSpecAnalysesDB object.
#' @export
#' 
get_blank_names.MassSpecAnalysesDB <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "SELECT blank FROM Analyses ORDER BY analysis")$blank
}

# MARK: set_replicate_names
#' @describeIn MassSpecAnalysesDB Set replicate names.
#' @param x MassSpecAnalysesDB object.
#' @param value Character vector of replicate names matching the number of analyses.
#' @export
#' 
set_replicate_names.MassSpecAnalysesDB <- function(x, value) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  current <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
  if (length(value) != length(current)) {
    stop("Length of value must equal the number of analyses.")
  }
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
  for (i in seq_along(current)) {
    DBI::dbExecute(conn,
      "UPDATE Analyses SET replicate = ? WHERE analysis = ?",
      params = list(value[i], current[i])
    )
  }
  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
  message("Replicate names updated.")
  invisible(x)
}

# MARK: set_blank_names
#' @describeIn MassSpecAnalysesDB Set blank names.
#' @param x MassSpecAnalysesDB object.
#' @param value Character vector of blank names matching the number of analyses.
#' @export
#' 
set_blank_names.MassSpecAnalysesDB <- function(x, value) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  current <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
  if (length(value) != length(current)) {
    stop("Length of value must equal the number of analyses.")
  }
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
  for (i in seq_along(current)) {
    DBI::dbExecute(conn,
      "UPDATE Analyses SET blank = ? WHERE analysis = ?",
      params = list(value[i], current[i])
    )
  }
  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
  message("Blank names updated.")
  invisible(x)
}

# MARK: get_concentrations
#' @describeIn MassSpecAnalysesDB Get concentrations.
#' @param x MassSpecAnalysesDB object.
#' @export
#' 
get_concentrations.MassSpecAnalysesDB <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "SELECT concentration FROM Analyses ORDER BY analysis")$concentration
}

# MARK: set_concentrations
#' @describeIn MassSpecAnalysesDB Set concentrations.
#' @param x MassSpecAnalysesDB object.
#' @param value Numeric vector of concentrations matching the number of analyses.
#' @export
#' 
set_concentrations.MassSpecAnalysesDB <- function(x, value) {
  if (!is.numeric(value)) stop("value must be numeric.")
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  current <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
  if (length(value) != length(current)) {
    stop("Length of value must equal the number of analyses.")
  }
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
  for (i in seq_along(current)) {
    DBI::dbExecute(conn,
      "UPDATE Analyses SET concentration = ? WHERE analysis = ?",
      params = list(value[i], current[i])
    )
  }
  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
  message("Concentrations updated.")
  invisible(x)
}

# MARK: info
#' @describeIn MassSpecAnalysesDB Get a summary table (similar to info.MassSpecAnalyses).
#' @param x MassSpecAnalysesDB object.
#' @export
#' 
info.MassSpecAnalysesDB <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "
    SELECT
      analysis,
      replicate,
      blank,
      type,
      spectra_number AS spectra,
      chromatograms_number AS chromatograms,
      concentration
    FROM Analyses
    ORDER BY analysis
  ")
}

# MARK: get_spectra_headers
#' @describeIn MassSpecAnalysesDB Fetch spectra headers for a given analysis.
#' @param x MassSpecAnalysesDB object.
#' @param analysis Analysis name (file basename).
#' @export
#' 
get_spectra_headers.MassSpecAnalysesDB <- function(x, analysis = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (!"SpectraHeaders" %in% DBI::dbListTables(conn)) return(NULL)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analysis, all_names)
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  names(rpls) <- all_names
  if (length(sel_names) == 0) return(data.table::data.table())
  if (length(sel_names) == length(all_names)) {
    hd <- DBI::dbGetQuery(conn, "SELECT * FROM SpectraHeaders")
    hd$replicate <- rpls[hd$analysis]
    data.table::setcolorder(hd, c("analysis", "replicate"))
    return(hd)
  }
  res_list <- lapply(sel_names, function(aname) {
    hd <- DBI::dbGetQuery(conn, "SELECT * FROM SpectraHeaders WHERE analysis = ?", params = list(aname))
    hd$replicate <- rpls[aname]
    data.table::setcolorder(hd, c("analysis", "replicate"))
    hd
  })
  data.table::rbindlist(res_list, fill = TRUE)
}

# MARK: get_spectra_tic
#' @describeIn MassSpecAnalysesDB Get the total ion current (TIC) spectra for the specified analyses (DuckDB backend).
#' @param x MassSpecAnalysesDB object.
#' @param analyses Character vector of analysis names (default: all).
#' @param levels Integer vector, MS levels to keep (default: c(1,2)).
#' @param rt Numeric vector of length 2, retention time range (optional).
#' @param as_list Logical, return list per analysis (default: FALSE).
#' @export
get_spectra_tic.MassSpecAnalysesDB <- function(
  x,
  analyses = NULL,
  levels = c(1, 2),
  rt = NULL,
  as_list = FALSE
) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  names(rpls) <- all_names
  value <- lapply(sel_names, function(aname) {
    q <- "SELECT analysis, polarity, level, rt, tic FROM SpectraHeaders WHERE analysis = ?"
    res <- DBI::dbGetQuery(conn, q, params = list(aname))
    if (nrow(res) == 0) return(data.table::data.table())
    res$replicate <- rpls[aname]
    data.table::setcolorder(res, c("analysis", "replicate"))
    res <- res[res$level %in% levels, ]
    if (!is.null(rt)) {
      if (length(rt) == 2 && is.numeric(rt)) {
        rt <- sort(rt)
        sel <- res$rt >= rt[1] & res$rt <= rt[2]
        res <- res[sel, ]
      }
    }
    res
  })
  data.table::rbindlist(value, fill = TRUE)
}

# MARK: get_spectra_bpc
#' @describeIn MassSpecAnalysesDB Get the base peak chromatograms (BPC) spectra for the specified analyses (DuckDB backend).
#' @param x MassSpecAnalysesDB object.
#' @param analyses Character vector of analysis names (default: all).
#' @param levels Integer vector, MS levels to keep (default: c(1,2)).
#' @param rt Numeric vector of length 2, retention time range (optional).
#' @param as_list Logical, return list per analysis (default: FALSE).
#' @export
get_spectra_bpc.MassSpecAnalysesDB <- function(
  x,
  analyses = NULL,
  levels = c(1, 2),
  rt = NULL,
  as_list = FALSE
) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  names(rpls) <- all_names
  value <- lapply(sel_names, function(aname) {
    q <- "SELECT analysis, polarity, level, rt, bpmz, bpint FROM SpectraHeaders WHERE analysis = ?"
    res <- DBI::dbGetQuery(conn, q, params = list(aname))
    if (nrow(res) == 0) return(data.table::data.table())
    res$replicate <- rpls[aname]
    data.table::setcolorder(res, c("analysis", "replicate"))
    res <- res[res$level %in% levels, ]
    if (!is.null(rt)) {
      if (length(rt) == 2 && is.numeric(rt)) {
        rt <- sort(rt)
        sel <- res$rt >= rt[1] & res$rt <= rt[2]
        res <- res[sel, ]
      }
    }
    res
  })
  data.table::rbindlist(value, fill = TRUE)
}

# MARK: get_raw_spectra
#' @describeIn MassSpecAnalysesDB Get raw spectra data from specified analyses, returning a `data.table` with the spectra data (DuckDB backend).
#' @param x MassSpecAnalysesDB object.
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
get_raw_spectra.MassSpecAnalysesDB <- function(
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
  minIntensityMS2 = 0
) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) minIntensityMS1 <- 0
  if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) minIntensityMS2 <- 0
  if (is.data.frame(mz)) if ("analysis" %in% colnames(mz)) analyses <- mz$analysis
  if (is.data.frame(mass)) if ("analysis" %in% colnames(mass)) analyses <- mass$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel_names) == 0) return(data.table::data.table())
  hd <- lapply(sel_names, function(aname) {
    q <- "SELECT * FROM SpectraHeaders WHERE analysis = ?"
    DBI::dbGetQuery(conn, q, params = list(aname))
  })
  hd <- data.table::rbindlist(hd)
  polarities <- as.character(unique(hd$polarity))
  highmz <- max(hd$highmz, na.rm = TRUE)
  lowmz <- min(hd$lowmz, na.rm = TRUE)
  highest_rt <- max(hd$rt, na.rm = TRUE)
  lowest_rt <- min(hd$rt, na.rm = TRUE)
  highest_mobility <- max(hd$mobility, na.rm = TRUE)
  lowest_mobility <- min(hd$mobility, na.rm = TRUE)
  has_ion_mobility <- any(hd$mobility > 0, na.rm = TRUE)
  targets <- MassSpecTargets(mass, mz, rt, mobility, ppm, sec, millisec, id, sel_names, polarities)
  if (!.is_targets_empty(targets)) {
    targets$mz[is.na(targets$mz)] <- 0
    targets$mzmax[is.na(targets$mzmax)] <- highmz
    targets$mzmin[is.na(targets$mzmin)] <- lowmz
    targets$mzmax[targets$mzmax == 0] <- highmz
    targets$rt[is.na(targets$rt)] <- 0
    targets$rtmax[is.na(targets$rtmax)] <- highest_rt
    targets$rtmin[is.na(targets$rtmin)] <- lowest_rt
    targets$rtmax[targets$rtmax == 0] <- highest_rt
    targets$mobility[is.na(targets$mobility)] <- 0
    if (has_ion_mobility) {
      targets$mobilitymax[is.na(targets$mobilitymax)] <- highest_mobility
      targets$mobilitymin[is.na(targets$mobilitymin)] <- lowest_mobility
      targets$mobilitymax[targets$mobilitymax == 0] <- highest_mobility
    }
  }
  targets$id[targets$id == 0] <- targets$analysis[targets$id == 0]
  if (is.null(levels)) levels <- unique(hd_summary$level)
  if (!2 %in% levels) allTraces <- TRUE
  if (!is.logical(allTraces)) allTraces <- TRUE
  if (nrow(targets) > 0) {
    if ("polarity" %in% colnames(targets)) {
      targets$polarity <- as.numeric(targets$polarity)
    }
    targets$precursor <- FALSE
    if (!allTraces) {
      if (!any(is.numeric(isolationWindow) | is.integer(isolationWindow))) {
        isolationWindow <- 0
      }
      targets$precursor <- TRUE
      targets$mzmin <- targets$mzmin - (isolationWindow / 2)
      targets$mzmax <- targets$mzmax + (isolationWindow / 2)
      # TODO make case for DIA when pre_mz is not available
    }
  }
  spec_list <- lapply(sel_names, function(a, levels, targets, hd, conn) {
    if ("analysis" %in% colnames(targets)) targets <- targets[targets$analysis %in% a, ]
    if (nrow(targets) == 0) return(data.table::data.table())
    q <- "SELECT * FROM Analyses WHERE analysis = ?"
    a_info <- DBI::dbGetQuery(conn, q, params = list(a))
    hd_a <- hd[hd$analysis == a, ]
    message("\U2699 Parsing spectra from ", basename(a_info$file), "...", appendLF = FALSE)
    if (.is_targets_empty(targets)) targets <- targets[0, ]
    spec <- rcpp_parse_ms_spectra(
      list(
        file = a_info$file,
        spectra_headers = hd_a
      ),
      levels,
      targets,
      minIntensityMS1,
      minIntensityMS2
    )
    message(" Done!")
    if (nrow(spec) == 0) return(data.table::data.table())
    if (!any(spec$mobility > 0)) spec$mobility <- NULL
    if (!"analysis" %in% colnames(spec)) spec$analysis <- a
    if (!"replicate" %in% colnames(spec)) spec$replicate <- a_info$replicate
    if ("id" %in% colnames(spec)) {
      data.table::setorder(spec, id, rt, mz)
    } else {
      data.table::setorder(spec, rt, mz)
    }
  }, levels = levels, targets = targets, hd = hd, conn = conn)
  spec <- data.table::rbindlist(spec_list, fill = TRUE)
  if (nrow(spec) > 0) {
    data.table::setcolorder(spec, c("analysis", "replicate"))
  }
  spec
}

# MARK: get_chromatograms_headers
#' @describeIn MassSpecAnalysesDB Fetch chromatograms headers for a given analysis.
#' @param x MassSpecAnalysesDB object.
#' @param analysis Analysis name (file basename).
#' @export
#' 
get_chromatograms_headers.MassSpecAnalysesDB <- function(x, analysis = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (!"ChromatogramsHeaders" %in% DBI::dbListTables(conn)) return(NULL)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analysis, all_names)
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  names(rpls) <- all_names
  if (length(sel_names) == 0) return(data.table::data.table())
  if (length(sel_names) == length(all_names)) {
    hd <- DBI::dbGetQuery(conn, "SELECT * FROM ChromatogramsHeaders")
    hd$replicate <- rpls[hd$analysis]
    data.table::setcolorder(hd, c("analysis", "replicate"))
    return(hd)
  }
  res_list <- lapply(sel_names, function(aname) {
    hd <- DBI::dbGetQuery(conn, "SELECT * FROM ChromatogramsHeaders WHERE analysis = ?", params = list(aname))
    hd$replicate <- rpls[hd$analysis]
    data.table::setcolorder(hd, c("analysis", "replicate"))
    hd
  })
  data.table::rbindlist(res_list, fill = TRUE)
}

# MARK: query_db
#' @describeIn MassSpecAnalysesDB Internal: execute a query on the DB.
#' @param x MassSpecAnalysesDB object.
#' @param sql SQL query string.
#' @param params Optional parameters for parameterized queries.
#' @export
#' 
query_db.MassSpecAnalysesDB <- function(x, sql, params = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .query_db(conn, sql, params)
}

# MARK: list_db_tables
#' @describeIn MassSpecAnalysesDB Internal: list tables in the DB.
#' @param x MassSpecAnalysesDB object.
#' @export
#' 
list_db_tables.MassSpecAnalysesDB <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .list_db_tables(conn)
}

# MARK: get_db_table_info
#' @describeIn MassSpecAnalysesDB Internal: get table info from the DB.
#' @param x MassSpecAnalysesDB object.
#' @param table_name Name of the table to get info for.
#' @export
#' 
get_db_table_info.MassSpecAnalysesDB <- function(x, table_name) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .get_db_table_info(conn, table_name)
}

# MARK: .create_massspec_analyses_db_schema
#' @describeIn MassSpecAnalysesDB Internal: create schema if missing.
#' @param conn DuckDB connection.
#' @noRd
#' 
.create_massspec_analyses_db_schema <- function(conn) {
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
      spectra_number INTEGER,
      chromatograms_number INTEGER,
      concentration DOUBLE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
}

#' @noRd
.write_massspec_analyses_to_db <- function(db, analyses, truncate = FALSE) {
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_massspec_analyses_db_schema(conn)

  incoming_names <- vapply(analyses, function(a) a$name, "")
  dup_incoming <- incoming_names[duplicated(incoming_names)]
  if (length(dup_incoming) > 0) {
    stop("Duplicate analyses within import batch: ", paste(unique(dup_incoming), collapse = ", "))
  }

  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)

  if (truncate) {
    DBI::dbExecute(conn, "DELETE FROM SpectraHeaders")
    DBI::dbExecute(conn, "DELETE FROM ChromatogramsHeaders")
    DBI::dbExecute(conn, "DELETE FROM Analyses")
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

  analyses_df <- data.frame(
    analysis = incoming_names,
    replicate = vapply(analyses, function(a) a$replicate, NA_character_),
    blank = vapply(analyses, function(a) a$blank, NA_character_),
    file = vapply(analyses, function(a) a$file, NA_character_),
    format = vapply(analyses, function(a) a$format, NA_character_),
    type = vapply(analyses, function(a) a$type, NA_character_),
    spectra_number = vapply(analyses, function(a) a$spectra_number, NA_integer_),
    chromatograms_number = vapply(analyses, function(a) a$chromatograms_number, NA_integer_),
    concentration = vapply(analyses, function(a) a$concentration, NA_real_),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(conn, "Analyses", analyses_df, append = TRUE)

  spectra_rows <- lapply(analyses, function(a) {
    if (is.null(a$spectra_headers)) return(NULL)
    df <- as.data.frame(a$spectra_headers)
    if (nrow(df) == 0) return(NULL)
    df$analysis <- a$name
    df
  })
  chrom_rows <- lapply(analyses, function(a) {
    if (is.null(a$chromatograms_headers)) return(NULL)
    df <- as.data.frame(a$chromatograms_headers)
    if (nrow(df) == 0) return(NULL)
    df$analysis <- a$name
    df
  })

  spectra_rows <- Filter(Negate(is.null), spectra_rows)
  chrom_rows <- Filter(Negate(is.null), chrom_rows)

  if (length(spectra_rows) > 0) {
    spectra_df <- data.table::rbindlist(spectra_rows, fill = TRUE)
    if (!DBI::dbExistsTable(conn, "SpectraHeaders")) {
      DBI::dbWriteTable(conn, "SpectraHeaders", spectra_df[0, ], append = FALSE)
    }
    DBI::dbWriteTable(conn, "SpectraHeaders", spectra_df, append = TRUE)
  } else if (!DBI::dbExistsTable(conn, "SpectraHeaders")) {
    DBI::dbWriteTable(conn, "SpectraHeaders", data.frame(analysis = character()), append = FALSE)
  }

  if (length(chrom_rows) > 0) {
    chrom_df <- data.table::rbindlist(chrom_rows, fill = TRUE)
    if (!DBI::dbExistsTable(conn, "ChromatogramsHeaders")) {
      DBI::dbWriteTable(conn, "ChromatogramsHeaders", chrom_df[0, ], append = FALSE)
    }
    DBI::dbWriteTable(conn, "ChromatogramsHeaders", chrom_df, append = TRUE)
  } else if (!DBI::dbExistsTable(conn, "ChromatogramsHeaders")) {
    DBI::dbWriteTable(conn, "ChromatogramsHeaders", data.frame(analysis = character()), append = FALSE)
  }

  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
}

# MARK: plot_spectra_tic
#' @describeIn MassSpecAnalysesDB Plot total ion current (TIC) spectra for the specified analyses (DuckDB backend).
#' @param x MassSpecAnalysesDB object.
#' @param analyses Analyses to plot (indices or names).
#' @param levels Integer vector, MS levels to keep.
#' @param rt Numeric vector of length 2, retention time range (optional).
#' @param downsize Integer, number of points to downsize to (optional).
#' @param xLab X axis label.
#' @param yLab Y axis label.
#' @param title Plot title.
#' @param groupBy Column to use for color grouping (default: "analysis").
#' @param interactive Logical, use plotly if TRUE, ggplot2 if FALSE.
#' @export
plot_spectra_tic.MassSpecAnalysesDB <- function(
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
  color_palette = NULL
) {
  tic <- get_spectra_tic(x, analyses, levels, rt)
  if (nrow(tic) == 0) {
    message("\U2717 TIC not found for the analyses!")
    return(NULL)
  }
  if (!is.null(downsize) && downsize > 0 && nrow(tic) > downsize) {
    tic <- as.data.table(tic)
    tic$rt <- floor(tic$rt / downsize) * downsize
    tic <- tic[, lapply(.SD, function(col) {
      if (is.numeric(col)) mean(col, na.rm = TRUE)
      else if (is.character(col)) col[1]
      else col[1]
    }), by = .(rt, analysis)]
  }
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  if (is.null(title)) title <- "Total Ion Current (TIC)"
  .plot_lines_tabular_data(
    data = tic,
    xvar = "rt",
    yvar = "tic",
    groupBy = groupBy,
    interactive = interactive,
    title = title,
    xLab = xLab,
    yLab = yLab,
    color_palette = color_palette
  )
}

# MARK: plot_spectra_bpc
#' @describeIn MassSpecAnalysesDB Plot base peak chromatogram (BPC) spectra for the specified analyses (DuckDB backend).
#' @param x MassSpecAnalysesDB object.
#' @param analyses Analyses to plot (indices or names).
#' @param levels Integer vector, MS levels to keep.
#' @param rt Numeric vector of length 2, retention time range (optional).
#' @param downsize Integer, number of points to downsize to (optional).
#' @param xLab X axis label.
#' @param yLab Y axis label.
#' @param title Plot title.
#' @param groupBy Column to use for color grouping (default: "analysis").
#' @param interactive Logical, use plotly if TRUE, ggplot2 if FALSE.
#' @param color_palette Optional vector of colors, otherwise uses an internal function.
#' @export
plot_spectra_bpc.MassSpecAnalysesDB <- function(
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
  color_palette = NULL
) {
  bpc <- get_spectra_bpc(x, analyses, levels, rt)
  if (nrow(bpc) == 0) {
    message("\U2717 BPC not found for the analyses!")
    return(NULL)
  }
  if (!is.null(downsize) && downsize > 0 && nrow(bpc) > downsize) {
    bpc <- as.data.table(bpc)
    bpc[, rt := floor(rt / downsize) * downsize]
    bpc <- bpc[, lapply(.SD, function(col) {
      if (is.numeric(col)) mean(col, na.rm = TRUE)
      else if (is.character(col)) col[1]
      else col[1]
    }), by = .(rt, analysis)]
  }
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  if (is.null(title)) title <- "Base Peak Chromatogram (BPC)"
  .plot_lines_tabular_data(
    data = bpc,
    xvar = "rt",
    yvar = "bpint",
    groupBy = groupBy,
    interactive = interactive,
    title = title,
    xLab = xLab,
    yLab = yLab,
    color_palette = color_palette
  )
}
