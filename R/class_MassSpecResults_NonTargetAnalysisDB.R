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
#' 
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

# MARK: get_features
#' @describeIn MassSpecResults_NonTargetAnalysisDB Retrieves features from the MassSpecResults_NonTargetAnalysisDB object based on specified criteria.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @export
#' 
get_features.MassSpecResults_NonTargetAnalysisDB <- function(
  x,
  analyses = NULL,
  features = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  filtered = FALSE
) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  rpls <- DBI::dbGetQuery(conn, "SELECT replicate FROM Analyses")$replicate
  pols <- DBI::dbGetQuery(conn, "SELECT polarity FROM Analyses")$polarity
  names(rpls) <- all_names
  names(pols) <- all_names
  ids <- NULL
  if (!is.null(features)) {
    if (is.data.frame(features)) {
      if ("analysis" %in% colnames(features)) {
        analyses <- unique(features$analysis)
      }
      if ("name" %in% colnames(features)) {
        ids <- features$name
      }
      features <- features$feature
    } else {
      ids <- names(features)
      features <- features
    }
  }
  if (!is.null(ids)) names(ids) <- features
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  rpls <- rpls[sel_names]
  pols <- pols[sel_names]
  if (length(sel_names) == 0) return(data.table::data.table())
  query <- "SELECT * FROM Features"
  conditions <- c()
  conditions <- c(conditions, sprintf("filtered = %s", ifelse(filtered, "TRUE", "FALSE")))
  conditions <- c(conditions, sprintf("analysis IN ('%s')", paste(sel_names, collapse = "','")))
  if (!is.null(features)) {
    conditions <- c(conditions, sprintf("feature IN ('%s')", paste(features, collapse = "','")))
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
    features <- DBI::dbGetQuery(conn, query)
    if (!is.null(ids)) features$name <- ids[features$feature]
    return(data.table::as.data.table(features))
  } else if (any(!(is.null(mass) && is.null(mz) && is.null(rt) && is.null(mobility)))) {
    targets <- MassSpecTargets(
      mass,
      mz,
      rt,
      mobility,
      ppm,
      sec,
      millisec,
      NULL,
      sel_names,
      pols
    )
    conditions <- apply(targets, 1, function(tgt) {
      sprintf(
        "(mz >= %f AND mz <= %f AND rt >= %f AND rt <= %f AND analysis = '%s' AND polarity = %d)",
        as.numeric(tgt["mzmin"]), as.numeric(tgt["mzmax"]),
        as.numeric(tgt["rtmin"]), as.numeric(tgt["rtmax"]),
        tgt["analysis"], as.integer(tgt["polarity"])
      )
    })
    query <- sprintf("SELECT * FROM Features WHERE %s", paste(conditions, collapse = " OR "))
    features <- DBI::dbGetQuery(conn, query)
    for (i in seq_len(nrow(targets))) {
      tgt <- targets[i, ]
      features$name[
        features$analysis %in% tgt$analysis &
          features$mz >= tgt$mzmin & features$mz <= tgt$mzmax &
            features$rt >= tgt$rtmin & features$rt <= tgt$rtmax
      ] <- tgt$id
    }
    return(data.table::as.data.table(features))
  } else {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
    features <- DBI::dbGetQuery(conn, query)
    if (!is.null(ids)) features$name <- ids[features$feature]
    return(data.table::as.data.table(features))
  }
}

# MARK: plot_features
#' @describeIn MassSpecResults_NonTargetAnalysisDB Plots features from the MassSpecResults_NonTargetAnalysisDB object according to the specified parameters.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @template arg-ms-filtered
#' @template arg-legendNames
#' @template arg-labs
#' @template arg-colorBy
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_features.MassSpecResults_NonTargetAnalysisDB <- function(
  x,
  analyses = NULL,
  features = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 20,
  sec = 60,
  millisec = 5,
  rtExpand = 120,
  mzExpand = 0.001,
  useLoadedData = TRUE,
  filtered = FALSE,
  legendNames = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "targets",
  interactive = TRUE,
  renderEngine = "webgl") {

  fts <- get_features(
    x,
    analyses,
    features,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered
  )

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  # Decode EIC data from base64-encoded strings
  eic_list <- list()
  for (i in seq_len(nrow(fts))) {
    ft <- fts[i, ]

    # Decode EIC data using the new rcpp_decode_eic_data function
    if (!is.na(ft$eic_rt) && !is.na(ft$eic_intensity) &&
      nchar(ft$eic_rt) > 0 && nchar(ft$eic_intensity) > 0) {
      rt_decoded <- rcpp_decode_eic_data(ft$eic_rt)
      intensity_decoded <- rcpp_decode_eic_data(ft$eic_intensity)

      # Decode baseline if available
      baseline_decoded <- NULL
      if (!is.na(ft$eic_baseline) && nchar(ft$eic_baseline) > 0) {
        baseline_decoded <- rcpp_decode_eic_data(ft$eic_baseline)
      }

      if (length(rt_decoded) > 0 && length(intensity_decoded) > 0 &&
        length(rt_decoded) == length(intensity_decoded)) {
        # Create EIC data with baseline if available
        eic_data <- data.table::data.table(
          analysis = ft$analysis,
          feature = ft$feature,
          rt = rt_decoded,
          intensity = intensity_decoded
        )

        # Add baseline column if decoded successfully
        if (!is.null(baseline_decoded) && length(baseline_decoded) == length(rt_decoded)) {
          eic_data$baseline <- baseline_decoded
        } else {
          eic_data$baseline <- 0 # Default to 0 if no baseline available
        }

        eic_list[[i]] <- eic_data
      }
    }
  }

  if (length(eic_list) == 0) {
    message("\U2717 No valid EIC data found for plotting!")
    return(NULL)
  }

  eic <- data.table::rbindlist(eic_list, fill = TRUE)
  fts <- .make_colorBy_varkey(fts, colorBy, legendNames)
  cl <- .get_colors(unique(fts$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)

  if (!interactive) {
    plot <- ggplot2::ggplot(eic, ggplot2::aes(x = rt))
    for (i in seq_len(nrow(fts))) {
      ft <- fts[i, ]
      ft_var <- ft$var
      temp <- eic[eic$analysis == ft$analysis & eic$feature == ft$feature, ]
      if (nrow(temp) > 0) {
        temp$var <- ft_var
        plot <- plot +
          ggplot2::geom_line(
            data = temp,
            ggplot2::aes(y = intensity, color = var)
          )
        # Filter for peak region
        peak_region <- temp[temp$rt >= ft$rtmin & temp$rt <= ft$rtmax, ]
        if (nrow(peak_region) > 0) {
          plot <- plot +
            ggplot2::geom_ribbon(
              data = peak_region,
              ggplot2::aes(
                ymin = rep(0, nrow(peak_region)),
                ymax = intensity,
                fill = var
              ),
              alpha = 0.3
            )
        }
      }
    }
    plot <- plot +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_fill_manual(values = cl50, guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)

    plot
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black")
    )
    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black")
    )
    show_legend <- rep(TRUE, length(cl))
    names(show_legend) <- names(cl)
    plot <- plot_ly()
    for (i in seq_len(nrow(fts))) {
      pk <- fts[i, ]
      ft_var <- pk$var
      hT <- .make_features_hover_string(pk)
      temp <- eic[eic$analysis == pk$analysis & eic$feature == pk$feature, ]
      if (nrow(temp) > 0) {
        peak_region <- temp[temp$rt >= pk$rtmin & temp$rt <= pk$rtmax, ]
        if (nrow(peak_region) > 0) {
          plot <- plot %>%
            add_trace(
              data = peak_region,
              x = ~rt,
              y = ~intensity,
              type = "scatter",
              mode = "markers",
              marker = list(color = cl[ft_var], size = 5),
              text = paste(hT, "<br>RT: ", round(peak_region$rt, 2), "<br>Intensity: ", round(peak_region$intensity, 0)),
              hoverinfo = "text",
              name = ft_var,
              legendgroup = ft_var,
              showlegend = FALSE
            )
          plot <- plot %>%
            plotly::add_ribbons(
              data = peak_region,
              x = ~rt,
              ymin = ~baseline,
              ymax = ~intensity,
              line = list(color = cl[ft_var], width = 1.5),
              fillcolor = cl50[ft_var],
              text = paste(hT, "<br>RT: ", round(peak_region$rt, 2), "<br>Intensity: ", round(peak_region$intensity, 0)),
              hoverinfo = "text",
              name = ft_var,
              legendgroup = ft_var,
              showlegend = show_legend[ft_var]
            )
          show_legend[ft_var] <- FALSE
        }
      }
    }
    # Add full EIC traces as background lines
    for (i in seq_len(nrow(fts))) {
      pk <- fts[i, ]
      ft_var <- pk$var
      temp <- eic[eic$analysis == pk$analysis & eic$feature == pk$feature, ]
      if (nrow(temp) > 0) {
        plot <- plot %>%
          add_trace(
            data = temp,
            x = ~rt,
            y = ~intensity,
            type = "scatter",
            mode = "lines",
            line = list(color = cl[ft_var], width = 0.5),
            name = ft_var,
            legendgroup = ft_var,
            showlegend = FALSE,
            hoverinfo = "skip"
          )
      }
    }

    plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis, title = title)
    if (renderEngine %in% "webgl") {
      # Fix for warnings with hoveron when webgl is used
      plot$x$attrs <- lapply(plot$x$attrs, function(x) {
        if (!is.null(x[["hoveron"]])) {
          x[["hoveron"]] <- NULL
        }
        x
      })
      plot <- plot %>% plotly::toWebGL()
    }
    plot
  }
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