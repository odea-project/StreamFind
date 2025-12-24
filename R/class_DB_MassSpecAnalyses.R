# MARK: DB_MassSpecAnalyses
#' @title Database-backed Mass Spectrometry Analyses
#' @description The `DB_MassSpecAnalyses` class stores MassSpec analyses metadata and headers in a DuckDB file.
#' @template arg-ms-db
#' @template arg-ms-files
#' @template arg-ms-centroid
#' @template arg-ms-levels
#' @return An object of class `DB_MassSpecAnalyses`, extending `DB_Analyses`.
#' @template arg-plot-colorPalette
#' @export
#'
DB_MassSpecAnalyses <- function(
    db = file.path("data.sf", "DB_MassSpecAnalyses.duckdb"),
    files = NULL,
    centroid = FALSE,
    levels = c(1, 2)) {
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_DB_MassSpecAnalyses_Analyses_db_schema(conn)
  .validate_DB_MassSpecAnalyses_Analyses_db_schema(conn)
  .validate_DB_MassSpecAnalyses_Spectra_db_schema(conn)
  .validate_DB_MassSpecAnalyses_Chromatograms_db_schema(conn)
  if (!is.null(files)) {
    cache_db <- file.path(dirname(db), "Cache.duckdb")
    analyses <- .parse_ms_from_files(files, centroid = centroid, levels = levels, cache_db = cache_db)
    if (is.null(analyses) || length(analyses) == 0) {
      stop("No analyses parsed from files.")
    }
    .write_massspec_analyses_to_db(conn, analyses, truncate = TRUE)
  }
  obj <- structure(
    list(
      db = db,
      data_type = "DB_MassSpec"
    ),
    class = c("DB_MassSpecAnalyses", "DB_Analyses")
  )
  if (!is.null(validate_object(obj))) stop("Invalid DB_MassSpecAnalyses object!")
  obj
}

# MARK: validate_object
#' @describeIn DB_MassSpecAnalyses Validate the DB_MassSpecAnalyses object (schema + type).
#' @template arg-x-MassSpecAnalyses
#' @export
#'
validate_object.DB_MassSpecAnalyses <- function(x) {
  checkmate::assert_class(x, "DB_MassSpecAnalyses")
  checkmate::assert_true(identical(x$data_type, "DB_MassSpec"))
  if (!file.exists(x$db)) stop("DB_MassSpecAnalyses file not found: ", x$db)
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  required_tables <- c("Analyses", "SpectraHeaders", "ChromatogramsHeaders")
  present <- DBI::dbListTables(conn)
  if (!all(required_tables %in% present)) stop("Missing required tables in DB_MassSpecAnalyses")
  NextMethod()
}

# MARK: add_analyses
#' @describeIn DB_MassSpecAnalyses Add analyses to the DB (append; overwrites only duplicates)
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-ms-files
#' @template arg-ms-centroid
#' @template arg-ms-levels
#' @export
add_analyses.DB_MassSpecAnalyses <- function(x, files, centroid = FALSE, levels = c(1, 2)) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  cache_db <- file.path(dirname(x$db), "Cache.duckdb")
  analyses <- .parse_ms_from_files(files, centroid = centroid, levels = levels, cache_db = cache_db)
  if (is.null(analyses) || length(analyses) == 0) {
    stop("No analyses parsed from files.")
  }
  .write_massspec_analyses_to_db(conn, analyses, truncate = FALSE)
  message("Analyses added to DB.")
  invisible(x)
}

#' MARK: remove_analyses
#' @describeIn DB_MassSpecAnalyses Remove analyses from the DB.
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-analyses
#' @export
#' 
remove_analyses.DB_MassSpecAnalyses <- function(x, analyses) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel_names) == 0) {
    message("No analyses to remove.")
    return(invisible(x))
  }
  DBI::dbExecute(conn, "BEGIN")
  rollback_needed <- TRUE
  on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
  for (aname in sel_names) {
    DBI::dbExecute(conn, "DELETE FROM Analyses WHERE analysis = ?", params = list(aname))
    DBI::dbExecute(conn, "DELETE FROM SpectraHeaders WHERE analysis = ?", params = list(aname))
    DBI::dbExecute(conn, "DELETE FROM ChromatogramsHeaders WHERE analysis = ?", params = list(aname))
  }
  DBI::dbExecute(conn, "COMMIT")
  rollback_needed <- FALSE
  message("Analyses removed from DB.")
  invisible(x)
}

# MARK: get_analysis_names
#' @describeIn DB_MassSpecAnalyses Get analysis names.
#' @template arg-x-DB_MassSpecAnalyses
#' @export
#'
get_analysis_names.DB_MassSpecAnalyses <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_analyses <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
  res <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
  names(res) <- all_analyses
  res
}

# MARK: get_replicate_names
#' @describeIn DB_MassSpecAnalyses Get replicate names.
#' @template arg-x-DB_MassSpecAnalyses
#' @export
#'
get_replicate_names.DB_MassSpecAnalyses <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_analyses <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
  res <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses ORDER BY analysis")$replicate
  names(res) <- all_analyses
  res
}

# MARK: get_blank_names
#' @describeIn DB_MassSpecAnalyses Get blank names.
#' @template arg-x-DB_MassSpecAnalyses
#' @export
#'
get_blank_names.DB_MassSpecAnalyses <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_analyses <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
  res <- DBI::dbGetQuery(conn, "SELECT blank FROM Analyses ORDER BY analysis")$blank
  names(res) <- all_analyses
  res
}

# MARK: set_replicate_names
#' @describeIn DB_MassSpecAnalyses Set replicate names.
#' @template arg-x-DB_MassSpecAnalyses
#' @param value Character vector of replicate names matching the number of analyses.
#' @export
#'
set_replicate_names.DB_MassSpecAnalyses <- function(x, value) {
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
#' @describeIn DB_MassSpecAnalyses Set blank names.
#' @template arg-x-DB_MassSpecAnalyses
#' @param value Character vector of blank names matching the number of analyses.
#' @export
#'
set_blank_names.DB_MassSpecAnalyses <- function(x, value) {
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
#' @describeIn DB_MassSpecAnalyses Get concentrations.
#' @template arg-x-DB_MassSpecAnalyses
#' @export
#'
get_concentrations.DB_MassSpecAnalyses <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_analyses <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses ORDER BY analysis")$analysis
  res <- DBI::dbGetQuery(conn, "SELECT concentration FROM Analyses ORDER BY analysis")$concentration
  names(res) <- all_analyses
  res
}

# MARK: set_concentrations
#' @describeIn DB_MassSpecAnalyses Set concentrations.
#' @template arg-x-DB_MassSpecAnalyses
#' @param value Numeric vector of concentrations matching the number of analyses.
#' @export
#'
set_concentrations.DB_MassSpecAnalyses <- function(x, value) {
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
#' @describeIn DB_MassSpecAnalyses Get a summary table.
#' @template arg-x-DB_MassSpecAnalyses
#' @export
#'
info.DB_MassSpecAnalyses <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "
    SELECT
      analysis,
      replicate,
      blank,
      type,
      polarity,
      spectra_number AS spectra,
      chromatograms_number AS chromatograms,
      concentration
    FROM Analyses
    ORDER BY analysis
  ")
}

# MARK: show
#' @describeIn DB_MassSpecAnalyses Show method for DB_MassSpecAnalyses objects.
#' @template arg-x-DB_MassSpecAnalyses
#' @export
#'
show.DB_MassSpecAnalyses <- function(x, ...) {
  cat("\n")
  cat("DB_MassSpecAnalyses (", nrow(info(x)), ")\n")
  info_table <- info(x)
  if (nrow(info_table) > 0) {
    print(info_table)
  }
}

# MARK: get_spectra_headers
#' @describeIn DB_MassSpecAnalyses Fetch spectra headers for a given analysis.
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-analyses
#' @export
#'
get_spectra_headers.DB_MassSpecAnalyses <- function(x, analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (!"SpectraHeaders" %in% DBI::dbListTables(conn)) {
    return(NULL)
  }
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  names(rpls) <- all_names
  if (length(sel_names) == 0) {
    return(data.table::data.table())
  }
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
#' @describeIn DB_MassSpecAnalyses Get the total ion current (TIC) spectra for the specified analyses.
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @export
#'
get_spectra_tic.DB_MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    levels = c(1, 2),
    rt = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  names(rpls) <- all_names
  value <- lapply(sel_names, function(aname) {
    q <- "SELECT analysis, polarity, level, rt, tic FROM SpectraHeaders WHERE analysis = ?"
    res <- DBI::dbGetQuery(conn, q, params = list(aname))
    if (nrow(res) == 0) {
      return(data.table::data.table())
    }
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

# MARK: plot_spectra_tic
#' @describeIn DB_MassSpecAnalyses Plot total ion current (TIC) spectra for the specified analyses.
#' @template arg-x-DB_MassSpecAnalyses
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
plot_spectra_tic.DB_MassSpecAnalyses <- function(
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
  tic <- get_spectra_tic(x, analyses, levels, rt)
  if (nrow(tic) == 0) {
    message("\U2717 TIC not found for the analyses!")
    return(NULL)
  }
  if (!is.null(downsize) && downsize > 0 && nrow(tic) > downsize) {
    tic <- as.data.table(tic)
    tic$rt <- floor(tic$rt / downsize) * downsize
    tic <- tic[, lapply(.SD, function(col) {
      if (is.numeric(col)) {
        mean(col, na.rm = TRUE)
      } else if (is.character(col)) {
        col[1]
      } else {
        col[1]
      }
    }), by = .(rt, analysis)]
  }
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  .plot_lines_tabular_data(
    data = tic,
    xvar = "rt",
    yvar = "tic",
    groupBy = groupBy,
    interactive = interactive,
    title = title,
    xLab = xLab,
    yLab = yLab,
    colorPalette = colorPalette
  )
}

# MARK: get_spectra_bpc
#' @describeIn DB_MassSpecAnalyses Get the base peak chromatograms (BPC) spectra for the specified analyses.
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-analyses
#' @template arg-ms-levels
#' @template arg-ms-rt
#' @export
#'
get_spectra_bpc.DB_MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    levels = c(1, 2),
    rt = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  names(rpls) <- all_names
  value <- lapply(sel_names, function(aname) {
    q <- "SELECT analysis, polarity, level, rt, bpmz, bpint FROM SpectraHeaders WHERE analysis = ?"
    res <- DBI::dbGetQuery(conn, q, params = list(aname))
    if (nrow(res) == 0) {
      return(data.table::data.table())
    }
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

# MARK: plot_spectra_bpc
#' @describeIn DB_MassSpecAnalyses Plot base peak chromatogram (BPC) spectra for the specified analyses.
#' @template arg-x-DB_MassSpecAnalyses
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
plot_spectra_bpc.DB_MassSpecAnalyses <- function(
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
  bpc <- get_spectra_bpc(x, analyses, levels, rt)
  if (nrow(bpc) == 0) {
    message("\U2717 BPC not found for the analyses!")
    return(NULL)
  }
  if (!is.null(downsize) && downsize > 0 && nrow(bpc) > downsize) {
    bpc <- as.data.table(bpc)
    bpc[, rt := floor(rt / downsize) * downsize]
    bpc <- bpc[, lapply(.SD, function(col) {
      if (is.numeric(col)) {
        mean(col, na.rm = TRUE)
      } else if (is.character(col)) {
        col[1]
      } else {
        col[1]
      }
    }), by = .(rt, analysis)]
  }
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  .plot_lines_tabular_data(
    data = bpc,
    xvar = "rt",
    yvar = "bpint",
    groupBy = groupBy,
    interactive = interactive,
    title = title,
    xLab = xLab,
    yLab = yLab,
    colorPalette = colorPalette
  )
}

# MARK: get_raw_spectra
#' @describeIn DB_MassSpecAnalyses Get raw spectra data from specified analyses, returning a `data.table` with the spectra data.
#' @template arg-x-DB_MassSpecAnalyses
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
get_raw_spectra.DB_MassSpecAnalyses <- function(
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
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  if (!any(is.numeric(minIntensityMS1) | is.integer(minIntensityMS1))) minIntensityMS1 <- 0
  if (!any(is.numeric(minIntensityMS2) | is.integer(minIntensityMS2))) minIntensityMS2 <- 0
  if (is.data.frame(mz)) if ("analysis" %in% colnames(mz)) analyses <- mz$analysis
  if (is.data.frame(mass)) if ("analysis" %in% colnames(mass)) analyses <- mass$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel_names) == 0) {
    return(data.table::data.table())
  }
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
    if (nrow(targets) == 0) {
      return(data.table::data.table())
    }
    q <- "SELECT * FROM Analyses WHERE analysis = ?"
    a_info <- DBI::dbGetQuery(conn, q, params = list(a))
    hd_a <- hd[hd$analysis == a, ]
    message("\U2699 Parsing spectra from ", basename(a_info$file), "...", appendLF = FALSE)
    if (.is_targets_empty(targets)) targets <- targets[0, ]
    spec <- rcpp_streamcraft_parse_ms_spectra(
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
    if (nrow(spec) == 0) {
      return(data.table::data.table())
    }
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

# MARK: get_spectra_eic
#' @describeIn DB_MassSpecAnalyses Get extracted ion chromatograms (EIC) for the specified analyses and targets.
#' @template arg-x-DB_MassSpecAnalyses
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
get_spectra_eic.DB_MassSpecAnalyses <- function(
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
  eic <- get_raw_spectra(
    x,
    analyses,
    levels = 1,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces = TRUE,
    isolationWindow = 1.3,
    minIntensityMS1 = 0,
    minIntensityMS2 = 0
  )
  if (nrow(eic) > 0) {
    intensity <- NULL
    eic <- data.table::as.data.table(eic)
    if (!"id" %in% colnames(eic)) {
      eic$id <- NA_character_
    }
    if (!"polarity" %in% colnames(eic)) {
      eic$polarity <- 0
    }
    cols_summary <- c("analysis", "replicate", "polarity", "id", "rt")
    intensity <- NULL
    mz <- NULL
    eic <- eic[, .(intensity = max(intensity), mz = mean(mz)), by = cols_summary]
    sel_cols <- c("analysis", "replicate", "id", "polarity", "rt", "mz", "intensity")
    eic <- eic[, sel_cols, with = FALSE]
    eic <- unique(eic)
  }
  eic
}

# MARK: get_spectra_ms1
#' @describeIn DB_MassSpecAnalyses Get MS1 spectra for the specified analyses and targets.
#' @template arg-x-DB_MassSpecAnalyses
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
get_spectra_ms1.DB_MassSpecAnalyses <- function(
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
  ms1 <- get_raw_spectra(
    x,
    analyses,
    levels = 1,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    allTraces = TRUE,
    minIntensityMS1 = minIntensity,
    minIntensityMS2 = 0
  )
  if (nrow(ms1) == 0) {
    return(ms1)
  }
  if (!"id" %in% colnames(ms1)) {
    hd <- get_spectra_headers(x, analyses)
    has_ion_mobility <- any(hd$mobility > 0)
    if (has_ion_mobility) {
      ms1$id <- paste(
        round(min(ms1$mz), 4), "-",
        round(max(ms1$mz), 4), "/",
        round(max(ms1$rt), 0), "-",
        round(min(ms1$rt), 0), "/",
        round(max(ms1$mobility), 0), "-",
        round(min(ms1$mobility), 0),
        sep = ""
      )
    } else {
      ms1$id <- paste(
        round(min(ms1$mz), 4), "-",
        round(max(ms1$mz), 4), "/",
        round(max(ms1$rt), 0), "-",
        round(min(ms1$rt), 0),
        sep = ""
      )
    }
  }
  if (!is.numeric(mzClust)) {
    mzClust <- 0.01
  }
  ms1$unique_id <- paste0(ms1$analysis, "_", ms1$id, "_", ms1$polarity)
  ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, FALSE)
  ms1_df <- data.table::rbindlist(ms1_list, fill = TRUE)
  ms1_df <- ms1_df[order(ms1_df$mz), ]
  ms1_df <- ms1_df[order(ms1_df$id), ]
  ms1_df <- ms1_df[order(ms1_df$analysis), ]
  rpls <- get_replicate_names(x)
  ms1_df$replicate <- rpls[ms1_df$analysis]
  data.table::setcolorder(ms1_df, c("analysis", "replicate"))
  ms1_df
}

# MARK: get_spectra_ms2
#' @describeIn DB_MassSpecAnalyses Get MS2 spectra for the specified analyses and targets.
#' @template arg-x-DB_MassSpecAnalyses
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
get_spectra_ms2.DB_MassSpecAnalyses <- function(
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
  ms2 <- get_raw_spectra(
    x,
    analyses,
    levels = 2,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    id,
    isolationWindow = isolationWindow,
    allTraces = FALSE,
    minIntensityMS1 = 0,
    minIntensityMS2 = minIntensity
  )
  if (nrow(ms2) == 0) {
    return(ms2)
  }
  if (!"id" %in% colnames(ms2)) {
    hd <- get_spectra_headers(x, analyses)
    has_ion_mobility <- any(hd$mobility > 0)
    if (has_ion_mobility) {
      ms2$id <- paste(
        round(min(ms2$mz), 4), "-",
        round(max(ms2$mz), 4), "/",
        round(max(ms2$rt), 0), "-",
        round(min(ms2$rt), 0), "/",
        round(max(ms2$mobility), 0), "-",
        round(min(ms2$mobility), 0),
        sep = ""
      )
    } else {
      ms2$id <- paste(
        round(min(ms2$mz), 4), "-",
        round(max(ms2$mz), 4), "/",
        round(max(ms2$rt), 0), "-",
        round(min(ms2$rt), 0),
        sep = ""
      )
    }
  }
  if (!is.numeric(mzClust)) {
    mzClust <- 0.01
  }
  ms2$unique_id <- paste0(ms2$analysis, "_", ms2$id, "_", ms2$polarity)
  ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, FALSE)
  ms2_df <- data.table::rbindlist(ms2_list, fill = TRUE)
  ms2_df <- ms2_df[order(ms2_df$mz), ]
  ms2_df <- ms2_df[order(ms2_df$id), ]
  ms2_df <- ms2_df[order(ms2_df$analysis), ]
  rpls <- get_replicate_names(x)
  ms2_df$replicate <- rpls[ms2_df$analysis]
  data.table::setcolorder(ms2_df, c("analysis", "replicate"))
  ms2_df
}

# MARK: get_chromatograms_headers
#' @describeIn DB_MassSpecAnalyses Fetch chromatograms headers for a given analysis.
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-analyses
#' @export
#'
get_chromatograms_headers.DB_MassSpecAnalyses <- function(x, analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (!"ChromatogramsHeaders" %in% DBI::dbListTables(conn)) {
    return(NULL)
  }
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  names(rpls) <- all_names
  if (length(sel_names) == 0) {
    return(data.table::data.table())
  }
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

# MARK: get_chromatograms
#' @describeIn DB_MassSpecAnalyses Get chromatograms for the specified analyses and chromatogram IDs/indices.
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @export
#'
get_chromatograms.DB_MassSpecAnalyses <- function(
    x,
    analyses = NULL,
    chromatograms = NULL,
    rtmin = 0,
    rtmax = 0,
    minIntensity = NULL) {
  chrom_hd <- get_chromatograms_headers(x, analyses)
  if (nrow(chrom_hd) == 0) {
    message("\U2717 No chromatograms found for the analyses!")
    return(data.table::data.table())
  }
  if (is.numeric(chromatograms)) {
    chrom_hd <- chrom_hd[as.integer(chrom_hd$index) == as.integer(chromatograms), ]
  } else if (is.character(chromatograms)) {
    chrom_hd <- chrom_hd[chrom_hd$id %in% chromatograms, ]
  }
  if (nrow(chrom_hd) == 0) {
    message("\U2717 No chromatograms found for the specified IDs/indices!")
    return(data.table::data.table())
  }
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  analyses_info <- DBI::dbGetQuery(conn, "SELECT analysis, replicate, file FROM Analyses")
  sel_analyses <- unique(chrom_hd$analysis)
  chrom_hd_list <- split(chrom_hd, chrom_hd$analysis)
  chrom_list <- lapply(sel_analyses, function(aname) {
    message("\U2699 Parsing chromatograms from ", aname, "...", appendLF = FALSE)
    chrom <- rcpp_streamcraft_parse_ms_chromatograms(
      list(
        file = analyses_info$file[analyses_info$analysis == aname],
        chromatograms_headers = chrom_hd_list[[aname]]
      ),
      as.integer(chrom_hd_list[[aname]]$index)
    )
    message(" Done!")
    chrom$analysis <- aname
    chrom$replicate <- analyses_info$replicate[analyses_info$analysis == aname]
    chrom
  })
  chrom_dt <- data.table::rbindlist(chrom_list, fill = TRUE)
  if (nrow(chrom_dt) == 0) {
    message("\U2717 No chromatogram data found for the specified analyses!")
    return(data.table::data.table())
  }
  if (is.numeric(minIntensity)) {
    chrom_dt <- chrom_dt[chrom_dt$intensity > minIntensity, ]
  }
  if (is.numeric(rtmin) && is.numeric(rtmax)) {
    if (rtmax > 0) chrom_dt <- chrom_dt[chrom_dt$rt >= rtmin & chrom_dt$rt <= rtmax]
  }
  data.table::setcolorder(chrom_dt, c("analysis", "replicate"))
  chrom_dt
}

# MARK: plot_chromatograms
#' @describeIn DB_MassSpecAnalyses Plot chromatograms for the specified analyses and chromatogram IDs/indices.
#' @template arg-x-DB_MassSpecAnalyses
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
plot_chromatograms.DB_MassSpecAnalyses <- function(
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
  chrom <- get_chromatograms(
    x,
    analyses,
    chromatograms,
    rtmin,
    rtmax,
    minIntensity
  )
  if (nrow(chrom) == 0) {
    message("\U2717 No chromatogram data found for plotting!")
    return(NULL)
  }
  if (!is.null(downsize) && downsize > 0 && nrow(chrom) > downsize) {
    chrom <- as.data.table(chrom)
    chrom$rt <- floor(chrom$rt / downsize) * downsize
    chrom <- chrom[, lapply(.SD, function(col) {
      if (is.numeric(col)) {
        mean(col, na.rm = TRUE)
      } else if (is.character(col)) {
        col[1]
      } else {
        col[1]
      }
    }), by = .(rt, analysis, id)]
  }
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  .plot_lines_tabular_data(
    data = chrom,
    xvar = "rt",
    yvar = "intensity",
    groupBy = groupBy,
    interactive = interactive,
    title = title,
    xLab = xLab,
    yLab = yLab,
    colorPalette = colorPalette
  )
}

# MARK: query_db
#' @describeIn DB_MassSpecAnalyses Internal: execute a query on the DB.
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
#'
query_db.DB_MassSpecAnalyses <- function(x, sql, params = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .query_db(conn, sql, params)
}

# MARK: list_db_tables
#' @describeIn DB_MassSpecAnalyses Internal: list tables in the DB.
#' @template arg-x-DB_MassSpecAnalyses
#' @export
#'
list_db_tables.DB_MassSpecAnalyses <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .list_db_tables(conn)
}

# MARK: get_db_table_info
#' @describeIn DB_MassSpecAnalyses Internal: get table info from the DB.
#' @template arg-x-DB_MassSpecAnalyses
#' @template arg-sql-tableName
#' @export
#'
get_db_table_info.DB_MassSpecAnalyses <- function(x, tableName) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .get_db_table_info(conn, tableName)
}

# MARK: .validate_DB_MassSpecAnalyses_analyses_dt
#' @noRd
.validate_DB_MassSpecAnalyses_analyses_dt <- function(x) {
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

# MARK: .create_DB_MassSpecAnalyses_Analyses_db_schema
#' @noRd
.create_DB_MassSpecAnalyses_Analyses_db_schema <- function(conn) {
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

# MARK: .validate_DB_MassSpecAnalyses_Analyses_db_schema
#' @noRd
.validate_DB_MassSpecAnalyses_Analyses_db_schema <- function(conn) {
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

# MARK: .validate_DB_MassSpecAnalyses_Spectra_db_schema
#' @noRd
.validate_DB_MassSpecAnalyses_Spectra_db_schema <- function(conn) {
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

# MARK: .validate_DB_MassSpecAnalyses_Chromatograms_db_schema
#' @noRd
.validate_DB_MassSpecAnalyses_Chromatograms_db_schema <- function(conn) {
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
    .create_DB_MassSpecAnalyses_Analyses_db_schema(conn)
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
    polarity = vapply(analyses, function(a) a$polarity, NA_character_),
    spectra_number = vapply(analyses, function(a) a$spectra_number, NA_integer_),
    chromatograms_number = vapply(analyses, function(a) a$chromatograms_number, NA_integer_),
    concentration = vapply(analyses, function(a) a$concentration, NA_real_),
    stringsAsFactors = FALSE
  )

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
    DBI::dbWriteTable(conn, "SpectraHeaders", spectra_df, append = TRUE)
  }

  if (length(chrom_rows) > 0) {
    chrom_df <- data.table::rbindlist(chrom_rows, fill = TRUE)
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
    if (file.exists(cache_db)) cache_manager <- CacheManager(cache_db)

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
