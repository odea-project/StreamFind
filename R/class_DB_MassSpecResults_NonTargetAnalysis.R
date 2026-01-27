# MARK: DB_MassSpecResults_NonTargetAnalysis
#' @title Constructor and methods to handle non-target analysis (NTA) for mass spectrometry data
#' @description Create a `DB_MassSpecResults_NonTargetAnalysis` object (child of [StreamFind::DB_Results]) that reuses an existing `DB_MassSpecAnalyses` DuckDB (for analyses and spectra) and stores NTA features in its own DuckDB.
#' @template arg-projectPath
#' @param features A data.frame/data.table with NTA feature results as produced by [StreamFind::DB_MassSpecMethod_FindFeatures_native], written to the `Features` table.
#' @return An object of class `DB_MassSpecResults_NonTargetAnalysis` (and `DB_Results`) pointing to the feature DuckDB on disk and holding an `analyses` field with the linked `DB_MassSpecAnalyses`.
#' @export
#'
DB_MassSpecResults_NonTargetAnalysis <- function(
    projectPath = ".",
    features = data.table::data.table()) {
  if (!requireNamespace("DBI", quietly = TRUE)) stop("DBI package required.")
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("duckdb package required.")

  analyses_db <- file.path(projectPath, "DB_MassSpecAnalyses.duckdb")
  if (!file.exists(analyses_db)) stop("DB_MassSpecAnalyses.duckdb not found at projectPath: ", analyses_db)
  analyses_obj <- DB_MassSpecAnalyses(projectPath = projectPath)

  db <- file.path(projectPath, "DB_MassSpecResults_NonTargetAnalysis.duckdb")
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema(conn)
  .validate_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema(conn)
  .create_DB_MassSpecResults_NonTargetAnalysis_InternalStandards_db_schema(conn)
  .validate_DB_MassSpecResults_NonTargetAnalysis_InternalStandards_db_schema(conn)
  .create_DB_MassSpecResults_NonTargetAnalysis_Suspects_db_schema(conn)
  .validate_DB_MassSpecResults_NonTargetAnalysis_Suspects_db_schema(conn)

  insert_features <- function(features) {
    .validate_DB_MassSpecResults_NonTargetAnalysis_features_dt(features)
    DBI::dbExecute(conn, "DELETE FROM Features")
    DBI::dbWriteTable(conn, "Features", features, overwrite = TRUE)
  }

  if (!is.null(features)) if (nrow(features) > 0) insert_features(features)

  x <- structure(
    list(
      db = db,
      analyses = analyses_obj,
      dataType = "DB_MassSpec"
    ),
    class = c("DB_MassSpecResults_NonTargetAnalysis", "DB_Results")
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid DB_MassSpecResults_NonTargetAnalysis object.")
  }
}

# MARK: validate_object
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Validates the DB_MassSpecResults_NonTargetAnalysis object, returning NULL if valid.
#' @template arg-ntsdb-x
#' @export
#'
validate_object.DB_MassSpecResults_NonTargetAnalysis <- function(x) {
  checkmate::assert_class(x, "DB_MassSpecResults_NonTargetAnalysis")
  checkmate::assert_true(identical(x$dataType, "DB_MassSpec"))
  if (!file.exists(x$db)) stop("DB_MassSpecResults_NonTargetAnalysis file not found: ", x$db)
  if (is.null(x$analyses) || !inherits(x$analyses, "DB_MassSpecAnalyses")) {
    stop("Field analyses must be a DB_MassSpecAnalyses object.")
  }
  validate_object(x$analyses)
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  required_tables <- c("Features")
  present <- DBI::dbListTables(conn)
  if (!all(required_tables %in% present)) stop("Missing required tables in DB_MassSpecResults_NonTargetAnalysis")
  .validate_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema(conn)
  NextMethod()
}

# MARK: query_db (dispatch to base)
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Internal: execute a query on the DB (delegates to DB_Results).
#' @template arg-ntsdb-x
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
query_db.DB_MassSpecResults_NonTargetAnalysis <- function(x, sql, params = NULL) {
  NextMethod()
}

# MARK: list_db_tables (dispatch to base)
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Internal: list tables in the DB (delegates to DB_Results).
#' @template arg-ntsdb-x
#' @export
list_db_tables.DB_MassSpecResults_NonTargetAnalysis <- function(x) {
  NextMethod()
}

# MARK: get_db_table_info (dispatch to base)
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Internal: get table info from the DB (delegates to DB_Results).
#' @template arg-ntsdb-x
#' @template arg-sql-tableName
#' @export
get_db_table_info.DB_MassSpecResults_NonTargetAnalysis <- function(x, tableName) {
  NextMethod()
}

# MARK: show
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Prints a summary of the DB_MassSpecResults_NonTargetAnalysis object.
#' @template arg-ntsdb-x
#' @export
#'
show.DB_MassSpecResults_NonTargetAnalysis <- function(x) {
  info_analyses <- info(x$analyses)
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  cat("\n")
  cat(is(x))
  cat("\n")
  features <- DBI::dbGetQuery(conn, "
    SELECT analysis,
      COUNT(*) AS feature,
      SUM(CASE WHEN filtered THEN 1 ELSE 0 END) AS filtered,
      SUM(CASE WHEN NOT filtered THEN 1 ELSE 0 END) AS not_filtered
    FROM Features
    GROUP BY analysis
  ")
  groups <- DBI::dbGetQuery(conn, "
    SELECT analysis, COUNT(DISTINCT [feature_group]) AS groups
    FROM Features
    WHERE NOT filtered AND feature_group != ''
    GROUP BY analysis
  ")
  groups$groups[is.na(groups$groups)] <- 0
  info <- data.table::data.table(
    "analysis" = info_analyses$analysis,
    "replicate" = info_analyses$replicate,
    "blank" = info_analyses$blank,
    "polarity" = info_analyses$polarity,
    "features" = features$not_filtered[match(info_analyses$analysis, features$analysis)],
    "filtered" = features$filtered[match(info_analyses$analysis, features$analysis)],
    "feature_groups" = groups$groups[match(info_analyses$analysis, groups$analysis)]
  )
  info <- info[order(tolower(info$analysis), info$analysis), ]
  print(info)
}

# MARK: get_features_count
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Returns a data table with the number of features for each analysis.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-filtered
#' @export
#'
get_features_count.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    filtered = FALSE) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  info_analyses <- info(x$analyses)
  all_names <- info_analyses$analysis
  sel_names <- if (is.null(analyses)) {
    all_names
  } else {
    .resolve_analyses_selection(analyses, all_names)
  }

  if (length(sel_names) == 0) {
    return(data.table::data.table())
  }

  counts <- DBI::dbGetQuery(
    conn,
    sprintf(
      "SELECT analysis,
        COUNT(*) AS total,
        SUM(CASE WHEN filtered THEN 1 ELSE 0 END) AS filtered
      FROM Features
      WHERE analysis IN ('%s')
      GROUP BY analysis",
      paste(sel_names, collapse = "','")
    )
  )
  counts <- data.table::as.data.table(counts)

  info <- data.table::data.table(
    analysis = sel_names,
    replicate = info_analyses$replicate[match(sel_names, info_analyses$analysis)]
  )

  if (nrow(counts) == 0) {
    info$features <- 0
    info$filtered <- 0
    info$components <- 0
    info$groups <- 0
    return(info)
  }

  counts$total[is.na(counts$total)] <- 0
  counts$filtered[is.na(counts$filtered)] <- 0
  counts$not_filtered <- counts$total - counts$filtered

  info$features <- if (filtered) {
    counts$total[match(info$analysis, counts$analysis)]
  } else {
    counts$not_filtered[match(info$analysis, counts$analysis)]
  }
  info$filtered <- counts$filtered[match(info$analysis, counts$analysis)]
  info$features[is.na(info$features)] <- 0
  info$filtered[is.na(info$filtered)] <- 0

  filter_clause <- if (filtered) "" else "AND filtered = FALSE"

  groups <- DBI::dbGetQuery(
    conn,
    sprintf(
      "SELECT analysis, COUNT(DISTINCT feature_group) AS groups
      FROM Features
      WHERE analysis IN ('%s')
        AND feature_group IS NOT NULL
        AND feature_group != ''
        %s
      GROUP BY analysis",
      paste(sel_names, collapse = "','"),
      filter_clause
    )
  )
  groups <- data.table::as.data.table(groups)

  components <- DBI::dbGetQuery(
    conn,
    sprintf(
      "SELECT analysis, COUNT(DISTINCT feature_component) AS components
      FROM Features
      WHERE analysis IN ('%s')
        AND feature_component IS NOT NULL
        AND feature_component != ''
        %s
      GROUP BY analysis",
      paste(sel_names, collapse = "','"),
      filter_clause
    )
  )
  components <- data.table::as.data.table(components)

  info$groups <- groups$groups[match(info$analysis, groups$analysis)]
  info$components <- components$components[match(info$analysis, components$analysis)]
  info$groups[is.na(info$groups)] <- 0
  info$components[is.na(info$components)] <- 0

  info
}

# MARK: plot_features_count
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plots the number of features for each analysis as a bar plot.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-filtered
#' @template arg-yLab
#' @template arg-title
#' @template arg-plot-groupBy
#' @template arg-showLegend
#' @template arg-showHoverText
#' @export
#'
plot_features_count.DB_MassSpecResults_NonTargetAnalysis <- function(
  x,
  analyses = NULL,
  filtered = FALSE,
  yLab = NULL,
  title = NULL,
  groupBy = "analysis",
  showLegend = TRUE,
  showHoverText = TRUE) {
  info <- get_features_count(x, analyses, filtered)

  if (nrow(info) == 0) {
    return(NULL)
  }

  allowed_group_by <- c("analysis", "replicate")
  if (!is.character(groupBy) || length(groupBy) != 1 || !(groupBy %in% allowed_group_by)) {
    stop("groupBy must be one of: ", paste(allowed_group_by, collapse = ", "))
  }

  if (groupBy == "replicate") {
    info$analysis <- info$replicate
  }

  features <- NULL

  info <- info[,
    .(
      features = round(mean(features), digits = 0),
      features_sd = round(sd(features), digits = 0),
      n_analysis = length(features)
    ),
    by = c("analysis")
  ]

  info$features_sd[is.na(info$features_sd)] <- 0

  info <- unique(info)

  if (showHoverText) {
    info$hover_text <- paste(
      info$analysis,
      "<br>",
      "N.: ",
      info$n_analysis,
      "<br>",
      "Features: ",
      info$features,
      " (SD: ",
      info$features_sd,
      ")"
    )
  } else {
    info$hover_text <- ""
  }

  info <- info[order(info$analysis), ]

  colors_tag <- .get_colors(info$analysis)

  if (is.null(yLab)) {
    yLab <- "Number of features"
  }

  plot <- plotly::plot_ly(
    x = info$analysis,
    y = info$features,
    marker = list(color = unname(colors_tag)),
    type = "bar",
    text = info$hover_text,
    hoverinfo = "text",
    error_y = list(
      type = "data",
      array = info$features_sd,
      color = "darkred",
      symmetric = FALSE,
      visible = TRUE
    ),
    name = names(colors_tag),
    showlegend = showLegend
  ) %>%
    plotly::layout(
      xaxis = list(title = NULL, tickfont = list(size = 14)),
      yaxis = list(
        title = yLab,
        tickfont = list(size = 14),
        titlefont = list(size = 18)
      )
    )

  plot
}

# MARK: get_features
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Retrieves features from the DB_MassSpecResults_NonTargetAnalysis object based on specified criteria.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-groups
#' @template arg-ms-components
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
get_features.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    features = NULL,
    groups = NULL,
    components = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    filtered = FALSE) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  analyses_info <- info(x$analyses)
  all_names <- analyses_info$analysis
  rpls <- analyses_info$replicate
  pols <- analyses_info$polarity
  names(rpls) <- all_names
  names(pols) <- all_names
  parse_selection <- function(sel, column, aliases = character(0)) {
    res <- list(values = NULL, analyses = NULL, ids = NULL)
    if (is.null(sel)) return(res)
    col_opts <- c(column, aliases)
    if (is.data.frame(sel)) {
      col_match <- col_opts[col_opts %in% colnames(sel)]
      if (length(col_match) == 0) {
        stop(sprintf("Selection for '%s' must include one of the following columns: %s", column, paste(col_opts, collapse = ", ")))
      }
      res$values <- sel[[col_match[1]]]
      if ("analysis" %in% colnames(sel)) {
        res$analyses <- unique(sel$analysis)
      }
      if ("name" %in% colnames(sel)) {
        res$ids <- sel$name
      }
    } else {
      res$values <- sel
      if (!is.null(names(sel))) res$ids <- names(sel)
    }
    if (!is.null(res$ids)) names(res$ids) <- res$values
    res
  }

  feat_sel <- parse_selection(features, "feature")
  grp_sel <- parse_selection(groups, "feature_group", c("group"))
  comp_sel <- parse_selection(components, "feature_component", c("component"))

  features <- feat_sel$values
  groups <- grp_sel$values
  components <- comp_sel$values
  ids <- feat_sel$ids
  grp_ids <- grp_sel$ids
  comp_ids <- comp_sel$ids
  analyses <- unique(c(analyses, feat_sel$analyses, grp_sel$analyses, comp_sel$analyses))
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  rpls <- rpls[sel_names]
  pols <- pols[sel_names]
  if (length(sel_names) == 0) {
    return(data.table::data.table())
  }
  query <- "SELECT * FROM Features"
  conditions <- c()
  if (!filtered) {
    conditions <- c(conditions, "filtered = FALSE")
  }
  conditions <- c(conditions, sprintf("analysis IN ('%s')", paste(sel_names, collapse = "','")))
  if (!is.null(features) || !is.null(groups) || !is.null(components)) {
    if (!is.null(features)) {
      conditions <- c(conditions, sprintf("feature IN ('%s')", paste(features, collapse = "','")))
    }
    if (!is.null(groups)) {
      conditions <- c(conditions, sprintf("feature_group IN ('%s')", paste(groups, collapse = "','")))
    }
    if (!is.null(components)) {
      conditions <- c(conditions, sprintf("feature_component IN ('%s')", paste(components, collapse = "','")))
    }
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
    features <- DBI::dbGetQuery(conn, query)
    if (!is.null(ids)) features$name <- ids[features$feature]
    if (!is.null(grp_ids)) features$name <- grp_ids[features$feature_group]
    if (!is.null(comp_ids)) features$name <- comp_ids[features$feature_component]
    if ("analysis" %in% colnames(features)) {
      rep_map <- data.table::data.table(analysis = sel_names, replicate = rpls)
      features <- merge(features, rep_map, by = "analysis", all.x = TRUE)
    }
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
      filter_pred <- if (!filtered) "filtered = FALSE" else "1=1"
      sprintf(
        "(mz >= %f AND mz <= %f AND rt >= %f AND rt <= %f AND analysis = '%s' AND polarity = %d AND %s)",
        as.numeric(tgt["mzmin"]), as.numeric(tgt["mzmax"]),
        as.numeric(tgt["rtmin"]), as.numeric(tgt["rtmax"]),
        tgt["analysis"], as.integer(tgt["polarity"]),
        filter_pred
      )
    })
    query <- sprintf("SELECT * FROM Features WHERE %s", paste(conditions, collapse = " OR "))
    features <- DBI::dbGetQuery(conn, query)
    if ("analysis" %in% colnames(features)) {
      rep_map <- data.table::data.table(analysis = sel_names, replicate = rpls)
      features <- merge(features, rep_map, by = "analysis", all.x = TRUE)
    }
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
    if ("analysis" %in% colnames(features)) {
      rep_map <- data.table::data.table(analysis = sel_names, replicate = rpls)
      features <- merge(features, rep_map, by = "analysis", all.x = TRUE)
    }
    return(data.table::as.data.table(features))
  }
}

# MARK: get_features_profile
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Returns a data table with feature-group profiles across analyses.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-groups
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
get_features_profile.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    groups = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    filtered = FALSE) {
  fts <- get_features(
    x = x,
    analyses = analyses,
    groups = groups,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    filtered = filtered
  )

  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  if (!"feature_group" %in% colnames(fts)) {
    warning("Feature groups not found!")
    return(data.table::data.table())
  }

  fts <- fts[!is.na(fts$feature_group) & fts$feature_group != "", ]
  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }

  prof <- fts[, .(intensity = max(intensity, na.rm = TRUE)), by = c("feature_group", "analysis")]
  prof$intensity[is.na(prof$intensity) | is.infinite(prof$intensity)] <- 0

  if ("replicate" %in% colnames(fts)) {
    rep_map <- unique(fts[, .(analysis, replicate)])
    prof <- merge(prof, rep_map, by = "analysis", all.x = TRUE)
  }

  desired_order <- c("analysis", "replicate", "feature_group", "intensity")
  desired_order <- desired_order[desired_order %in% colnames(prof)]
  data.table::setcolorder(prof, c(desired_order, setdiff(colnames(prof), desired_order)))
  prof
}

# MARK: plot_features_profile
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plots feature-group profiles across analyses or replicates.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @template arg-plot-groupBy
#' @template arg-normalized
#' @template arg-yLab
#' @template arg-title
#' @template arg-interactive
#' @template arg-showLegend
#' @export
#'
plot_features_profile.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    groups = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    filtered = FALSE,
    groupBy = "analysis",
    normalized = FALSE,
    yLab = NULL,
    title = NULL,
    interactive = TRUE,
    showLegend = TRUE) {
  prof <- get_features_profile(
    x = x,
    analyses = analyses,
    groups = groups,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    filtered = filtered
  )

  if (nrow(prof) == 0) {
    return(NULL)
  }

  allowed_group_by <- c("analysis", "replicate")
  if (!is.character(groupBy) || length(groupBy) != 1 || !(groupBy %in% allowed_group_by)) {
    stop("groupBy must be one of: ", paste(allowed_group_by, collapse = ", "))
  }

  if (normalized) {
    prof[, intensity := {
      max_int <- max(intensity, na.rm = TRUE)
      if (!is.finite(max_int) || max_int == 0) 0 else intensity / max_int
    }, by = feature_group]
  }

  if (groupBy == "replicate") {
    if (!"replicate" %in% colnames(prof)) {
      warning("Replicate information not available for feature profiles.")
      return(NULL)
    }
    prof <- prof[,
      .(
        intensity = mean(intensity, na.rm = TRUE),
        analysis_sd = stats::sd(intensity, na.rm = TRUE)
      ),
      by = c("feature_group", "replicate")
    ]
    prof$analysis_sd[is.na(prof$analysis_sd)] <- 0
  }

  x_col <- if (groupBy == "replicate") "replicate" else "analysis"
  prof[[x_col]] <- as.character(prof[[x_col]])
  prof$feature_group <- as.character(prof$feature_group)

  if (is.null(yLab)) {
    yLab <- if (normalized) "Relative intensity" else "Intensity"
  }
  xLab <- if (groupBy == "replicate") "Replicate" else "Analysis"

  if (!interactive) {
    plot <- ggplot2::ggplot(
      prof,
      ggplot2::aes_string(x = x_col, y = "intensity", group = "feature_group", color = "feature_group")
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point()
    if (groupBy == "replicate") {
      plot <- plot +
        ggplot2::geom_errorbar(
          ggplot2::aes_string(
            ymin = "intensity - analysis_sd",
            ymax = "intensity + analysis_sd"
          ),
          width = 0.2,
          alpha = 0.6
        )
    }
    plot <- plot +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title, color = "feature_group")
    return(plot)
  }

  colors_tag <- .get_colors(unique(prof$feature_group))
  hover_text <- paste0(
    "group: ", prof$feature_group,
    "<br>", xLab, ": ", prof[[x_col]],
    "<br>intensity: ", round(prof$intensity, 3)
  )

  error_y <- NULL
  if (groupBy == "replicate") {
    error_y <- list(type = "data", array = prof$analysis_sd, visible = TRUE)
  }

  plot <- plotly::plot_ly(
    data = prof,
    x = prof[[x_col]],
    y = ~intensity,
    type = "scattergl",
    mode = "lines+markers",
    color = ~feature_group,
    colors = colors_tag,
    text = hover_text,
    hoverinfo = "text",
    error_y = error_y,
    showlegend = showLegend
  )

    plot <- plot %>%
      plotly::layout(
        title = title,
        xaxis = list(title = NULL, tickfont = list(size = 12)),
        yaxis = list(title = yLab, tickfont = list(size = 12)),
        legend = list(title = list(text = "feature_group"))
      )
  plot
}

# MARK: get_internal_standards
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Retrieves internal standards from the InternalStandards table.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @export
#'
get_internal_standards.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  analyses_info <- info(x$analyses)
  all_names <- analyses_info$analysis

  # Resolve analyses selection
  sel_names <- if (is.null(analyses)) {
    all_names
  } else {
    .resolve_analyses_selection(analyses, all_names)
  }

  if (length(sel_names) == 0) {
    return(data.table::data.table())
  }

  # Build query
  query <- sprintf(
    "SELECT * FROM InternalStandards WHERE analysis IN ('%s')",
    paste(sel_names, collapse = "','")
  )

  result <- DBI::dbGetQuery(conn, query)
  data.table::as.data.table(result)
}

# MARK: plot_features
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plots features from the DB_MassSpecResults_NonTargetAnalysis object according to the specified parameters.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-groups
#' @template arg-ms-components
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @template arg-labs
#' @template arg-plot-groupBy
#' @template arg-interactive
#' @param showDetails Logical, show hover details in interactive plots.
#' @export
#'
plot_features.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    features = NULL,
    groups = NULL,
    components = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    filtered = FALSE,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "feature",
    interactive = TRUE,
    showDetails = FALSE) {
  fts <- get_features(
    x = x,
    analyses = analyses,
    features = features,
    groups = groups,
    components = components,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    filtered = filtered
  )

  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }

  # Decode EIC data from base64-encoded strings
  eic_list <- list()
  for (i in seq_len(nrow(fts))) {
    ft <- fts[i, ]
    sel <- !is.na(ft$eic_rt) && !is.na(ft$eic_intensity)
    sel <- sel && nchar(ft$eic_rt) > 0 && nchar(ft$eic_intensity) > 0
    if (sel) {
      rt_decoded <- rcpp_streamcraft_decode_string(ft$eic_rt)
      intensity_decoded <- rcpp_streamcraft_decode_string(ft$eic_intensity)
      baseline_decoded <- NULL
      if (!is.na(ft$eic_baseline) && nchar(ft$eic_baseline) > 0) {
        baseline_decoded <- rcpp_streamcraft_decode_string(ft$eic_baseline)
      }
      sel2 <- length(rt_decoded) > 0 && length(intensity_decoded) > 0
      sel2 <- sel2 && length(rt_decoded) == length(intensity_decoded)
      if (sel2) {
        ord <- order(rt_decoded)
        rt_decoded <- rt_decoded[ord]
        intensity_decoded <- intensity_decoded[ord]
        if (!is.null(baseline_decoded) && length(baseline_decoded) == length(rt_decoded)) {
          baseline_decoded <- baseline_decoded[ord]
        }
        eic_data <- data.table::data.table(
          analysis = ft$analysis,
          feature = ft$feature,
          rt = rt_decoded,
          intensity = intensity_decoded
        )
        if (!is.null(baseline_decoded) && length(baseline_decoded) == length(rt_decoded)) {
          eic_data$baseline <- baseline_decoded
        } else {
          eic_data$baseline <- 0 # Default to 0 if no baseline available
        }
        eic_list[[i]] <- eic_data
      }
    }
  }

  # Remove NULL entries from eic_list
  eic_list <- eic_list[!sapply(eic_list, is.null)]

  if (length(eic_list) == 0) {
    message("\U2717 No valid EIC data found for plotting!")
    return(NULL)
  }

  eic <- data.table::rbindlist(eic_list, fill = TRUE)
  if (!(is.character(groupBy) && length(groupBy) >= 1 && all(groupBy %in% colnames(fts)))) {
    warning("groupBy columns not found in feature data")
    return(NULL)
  }
  order_idx <- do.call(order, fts[, groupBy, with = FALSE])
  fts <- fts[order_idx]
  vals <- lapply(groupBy, function(col) as.character(fts[[col]]))
  fts$var <- do.call(paste, c(vals, sep = " - "))
  var_levels <- unique(fts$var)
  fts$var <- factor(fts$var, levels = var_levels)
  cl <- .get_colors(var_levels)
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
      ggplot2::labs(color = groupBy)

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
    make_hover_text <- function(pk_row) {
      fmt_num <- function(x, digits = 2) {
        if (is.null(x)) return(NA_real_)
        ifelse(is.na(x), NA, round(as.numeric(x), digits))
      }
      base_lines <- c(
        paste0("analysis: ", pk_row$analysis),
        paste0("feature: ", pk_row$feature),
        paste0("feature_component: ", pk_row$feature_component),
        paste0("feature_group: ", pk_row$feature_group),
        paste0("adduct: ", pk_row$adduct),
        paste0("rt: ", round(pk_row$rt, 2)),
        paste0("m/z: ", round(pk_row$mz, 4)),
        paste0("mass: ", fmt_num(pk_row$mass, 4)),
        paste0("noise: ", fmt_num(pk_row$noise, 0)),
        paste0("intensity: ", round(pk_row$intensity, 0)),
        paste0("sn: ", fmt_num(pk_row$sn, 1)),
        paste0("area: ", fmt_num(pk_row$area, 0)),
        paste0("rtmin: ", fmt_num(pk_row$rtmin, 2)),
        paste0("rtmax: ", fmt_num(pk_row$rtmax, 2)),
        paste0("width: ", fmt_num(pk_row$width, 2)),
        paste0("mzmin: ", fmt_num(pk_row$mzmin, 4)),
        paste0("mzmax: ", fmt_num(pk_row$mzmax, 4)),
        paste0("ppm: ", fmt_num(pk_row$ppm, 1)),
        paste0("fwhm_rt: ", fmt_num(pk_row$fwhm_rt, 2)),
        paste0("fwhm_mz: ", fmt_num(pk_row$fwhm_mz, 4)),
        paste0("gaussian_A: ", fmt_num(pk_row$gaussian_A, 2)),
        paste0("gaussian_mu: ", fmt_num(pk_row$gaussian_mu, 2)),
        paste0("gaussian_sigma: ", fmt_num(pk_row$gaussian_sigma, 2)),
        paste0("gaussian_r2: ", fmt_num(pk_row$gaussian_r2, 4)),
        paste0("jaggedness: ", fmt_num(pk_row$jaggedness, 4)),
        paste0("sharpness: ", fmt_num(pk_row$sharpness, 2)),
        paste0("asymmetry: ", fmt_num(pk_row$asymmetry, 2)),
        paste0("modality: ", pk_row$modality),
        paste0("plates: ", fmt_num(pk_row$plates, 0)),
        paste0("polarity: ", pk_row$polarity),
        paste0("filtered: ", pk_row$filtered),
        paste0("filter: ", pk_row$filter),
        paste0("filled: ", pk_row$filled),
        paste0("correction: ", fmt_num(pk_row$correction, 4)),
        paste0("eic_size: ", pk_row$eic_size),
        paste0("ms1_size: ", pk_row$ms1_size),
        paste0("ms2_size: ", pk_row$ms2_size)
      )
      paste(c(base_lines), collapse = "<br>")
    }
    show_legend <- rep(TRUE, length(cl))
    names(show_legend) <- names(cl)
    plot <- plot_ly()
    for (i in seq_len(nrow(fts))) {
      pk <- fts[i, ]
      ft_var <- pk$var
      hT <- if (showDetails) make_hover_text(pk) else ""
      hoverinfo_val <- if (showDetails) "text" else "skip"
      temp <- eic[eic$analysis == pk$analysis & eic$feature == pk$feature, ]
      if (nrow(temp) > 0) {
        peak_region <- temp[temp$rt >= pk$rtmin & temp$rt <= pk$rtmax, ]
        if (nrow(peak_region) > 0) {
          plot <- plot %>%
            add_trace(
              data = peak_region,
              x = ~rt,
              y = ~intensity,
              type = "scattergl",
              mode = "markers",
              marker = list(color = cl[ft_var], size = 5),
              text = if (showDetails) paste(hT, "<br>RT: ", round(peak_region$rt, 2), "<br>Intensity: ", round(peak_region$intensity, 0)) else NULL,
              hoverinfo = hoverinfo_val,
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
              text = if (showDetails) paste(hT, "<br>RT: ", round(peak_region$rt, 2), "<br>Intensity: ", round(peak_region$intensity, 0)) else NULL,
              hoverinfo = hoverinfo_val,
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
            type = "scattergl",
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
    plot
  }
}

# MARK: map_features
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plot RT vs m/z traces for selected features using EIC data.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-groups
#' @template arg-ms-components
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @template arg-labs
#' @template arg-plot-title
#' @template arg-plot-groupBy
#' @template arg-interactive
#' @param showDetails Logical, show hover details in interactive plots.
#' @export
#'
map_features.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    features = NULL,
    groups = NULL,
    components = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    filtered = FALSE,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "feature",
    interactive = TRUE,
    showDetails = FALSE) {
  fts <- get_features(
    x = x,
    analyses = analyses,
    features = features,
    groups = groups,
    components = components,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    filtered = filtered
  )

  if (nrow(fts) == 0) {
    message("\u2717 Features not found for the targets!")
    return(NULL)
  }

  if (!(is.character(groupBy) && length(groupBy) >= 1 && all(groupBy %in% colnames(fts)))) {
    warning("groupBy columns not found in feature data")
    return(NULL)
  }
  order_idx <- do.call(order, fts[, groupBy, with = FALSE])
  fts <- fts[order_idx]
  vals <- lapply(groupBy, function(col) as.character(fts[[col]]))
  fts$var <- do.call(paste, c(vals, sep = " - "))
  var_levels <- unique(fts$var)
  fts$var <- factor(fts$var, levels = var_levels)
  cl <- .get_colors(var_levels)

  pt_list <- list()
  for (i in seq_len(nrow(fts))) {
    ft <- fts[i, ]
    has_eic <- !is.na(ft$eic_rt) && !is.na(ft$eic_mz) && !is.na(ft$eic_intensity)
    has_eic <- has_eic && nchar(ft$eic_rt) > 0 && nchar(ft$eic_mz) > 0 && nchar(ft$eic_intensity) > 0
    if (!has_eic) next
    rt_dec <- rcpp_streamcraft_decode_string(ft$eic_rt)
    mz_dec <- rcpp_streamcraft_decode_string(ft$eic_mz)
    int_dec <- rcpp_streamcraft_decode_string(ft$eic_intensity)
    if (length(rt_dec) == 0 || length(mz_dec) == 0 || length(int_dec) == 0) next
    if (!(length(rt_dec) == length(mz_dec) && length(rt_dec) == length(int_dec))) next
    ord <- order(rt_dec)
    rt_dec <- rt_dec[ord]
    mz_dec <- mz_dec[ord]
    int_dec <- int_dec[ord]
    max_int <- max(int_dec, na.rm = TRUE)
    if (!is.finite(max_int) || max_int == 0) next
    norm_int <- int_dec / max_int
    pt_list[[length(pt_list) + 1]] <- data.table::data.table(
      analysis = ft$analysis,
      replicate = ft$replicate,
      feature = ft$feature,
      feature_component = ft$feature_component,
      feature_group = ft$feature_group,
      adduct = ft$adduct,
      rt = rt_dec,
      mz = mz_dec,
      intensity = norm_int,
      var = ft$var
    )
  }

  # Remove NULL entries from pt_list
  pt_list <- pt_list[!sapply(pt_list, is.null)]

  if (length(pt_list) == 0) {
    message("\u2717 No valid EIC data found for mapping!")
    return(NULL)
  }

  pts <- data.table::rbindlist(pt_list, fill = TRUE)
  size_scaled <- pts$intensity
  size_scaled[is.na(size_scaled)] <- 0
  size_scaled <- size_scaled * 8 + 2

  if (!interactive) {
    plot <- ggplot2::ggplot(
      pts,
      ggplot2::aes(x = rt, y = mz, color = var, size = intensity)
    ) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_size(range = c(2, 10), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title, color = groupBy)
    return(plot)
  }

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

  hover_vals <- if (showDetails) {
    paste0(
      "analysis: ", pts$analysis,
      "<br>replicate: ", pts$replicate,
      "<br>feature: ", pts$feature,
      "<br>component: ", pts$feature_component,
      "<br>group: ", pts$feature_group,
      "<br>adduct: ", pts$adduct,
      "<br>rt: ", round(pts$rt, 2),
      "<br>m/z: ", round(pts$mz, 4),
      "<br>norm_intensity: ", round(pts$intensity, 3)
    )
  } else {
    ""
  }

  plot <- plot_ly(
    data = pts,
    x = ~rt,
    y = ~mz,
    type = "scattergl",
    mode = "markers",
    color = ~var,
    colors = cl,
    marker = list(size = size_scaled, sizemode = "diameter", opacity = 0.7),
    text = hover_vals,
    hoverinfo = if (showDetails) "text" else "skip"
  )

  plot <- plot %>% plotly::layout(title = title, xaxis = xaxis, yaxis = yaxis, legend = list(title = list(text = groupBy)))
  plot
}

# MARK: plot_features_ms1
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plot MS1 spectra for selected features.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-groups
#' @template arg-ms-components
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-normalized
#' @template arg-ms-filtered
#' @template arg-plot-groupBy
#' @export
#'
plot_features_ms1.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    features = NULL,
    groups = NULL,
    components = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    normalized = FALSE,
    filtered = FALSE,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "feature",
    showText = TRUE,
    interactive = TRUE) {
  fts <- get_features(
    x = x,
    analyses = analyses,
    features = features,
    groups = groups,
    components = components,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    filtered = filtered
  )

  if (nrow(fts) == 0) {
    message("\u2717 MS1 traces not found for the targets!")
    return(NULL)
  }

  ms1_list <- lapply(
    seq_len(nrow(fts)),
    function(i) {
      ft <- fts[i, ]
      sel <- !is.na(ft$ms1_mz) && nchar(ft$ms1_mz) > 0 &&
        !is.na(ft$ms1_intensity) && nchar(ft$ms1_intensity) > 0
      if (!sel) return(data.table::data.table())
      mz_dec <- rcpp_streamcraft_decode_string(ft$ms1_mz)
      int_dec <- rcpp_streamcraft_decode_string(ft$ms1_intensity)
      if (length(mz_dec) == 0 || length(mz_dec) != length(int_dec)) {
        return(data.table::data.table())
      }
      data.table::data.table(
        mz = mz_dec,
        intensity = int_dec,
        analysis = ft$analysis,
        feature = ft$feature
      )
    }
  )

  if (normalized) {
    ms1_list <- lapply(ms1_list, function(z) {
      if (!is.null(z) && nrow(z) > 0) {
        max_int <- max(z$intensity)
        if (max_int > 0) z$intensity <- z$intensity / max_int
      }
      z
    })
  }

  ms1 <- data.table::rbindlist(ms1_list, fill = TRUE)
  if (nrow(ms1) == 0) {
    message("\u2717 MS1 traces not found for the targets!")
    return(NULL)
  }

  analyses_info <- info(x$analyses)
  rpl_map <- analyses_info$replicate
  names(rpl_map) <- analyses_info$analysis
  ms1$replicate <- rpl_map[ms1$analysis]
  data.table::setcolorder(ms1, c("analysis", "replicate", "feature"))

  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_ms1_id <- paste0(ms1$analysis, "-", ms1$feature)
  if ("feature_group" %in% colnames(fts)) {
    fgs <- fts$feature_group
    names(fgs) <- unique_fts_id
    ms1$feature_group <- fgs[unique_ms1_id]
  } else if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms1$feature_group <- fgs[unique_ms1_id]
  }

  if ("feature_component" %in% colnames(fts)) {
    fcs <- fts$feature_component
    names(fcs) <- unique_fts_id
    ms1$feature_component <- fcs[unique_ms1_id]
  } else if ("component" %in% colnames(fts)) {
    fcs <- fts$component
    names(fcs) <- unique_fts_id
    ms1$feature_component <- fcs[unique_ms1_id]
  }

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms1$name <- tar_ids[unique_ms1_id]
    data.table::setcolorder(ms1, c("analysis", "replicate", "name"))
  }

  desired_order <- c("analysis", "replicate", "feature", "feature_group", "feature_component")
  data.table::setcolorder(ms1, c(desired_order, setdiff(colnames(ms1), desired_order)))

  if (!(is.character(groupBy) && length(groupBy) >= 1 && all(groupBy %in% colnames(ms1)))) {
    warning("groupBy columns not found in MS1 data")
    return(NULL)
  }
  vals <- lapply(groupBy, function(col) as.character(ms1[[col]]))
  ms1$var <- do.call(paste, c(vals, sep = " - "))
  ms1$loop <- paste0(ms1$analysis, ms1$replicate, ms1$id, ms1$var)
  cl <- .get_colors(unique(ms1$var))

  if (showText) {
    ms1$text_string <- paste0(round(ms1$mz, 4))
  } else {
    ms1$text_string <- ""
  }

  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"

    plot <- ggplot2::ggplot(
      ms1,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          xend = mz,
          yend = 0,
          color = var
        ),
        linewidth = 1
      )

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = text_string),
          vjust = 0.2,
          hjust = -0.2,
          angle = 90,
          size = 2,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(ms1$intensity) * 1.5)
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = groupBy)

    plot
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"

    ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms1$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms1$intensity) * 1.5)
    )

    plot <- plot_ly()
    seen_vars <- character(0)
    for (lp in unique(ms1$loop)) {
      seg <- ms1[ms1$loop == lp, ]
      if (nrow(seg) == 0) next
      var_val <- seg$var[1]
      show_leg <- !(var_val %in% seen_vars)
      if (show_leg) seen_vars <- c(seen_vars, var_val)
      x_seg <- as.numeric(rbind(seg$mz, seg$mz, rep(NA, nrow(seg))))
      y_seg <- as.numeric(rbind(rep(0, nrow(seg)), seg$intensity, rep(NA, nrow(seg))))
      plot <- plot %>%
        add_trace(
          x = as.vector(x_seg),
          y = as.vector(y_seg),
          type = "scattergl",
          mode = "lines",
          line = list(color = cl[var_val], width = 1),
          name = var_val,
          legendgroup = var_val,
          showlegend = show_leg,
          hoverinfo = "skip"
        )
      if (showText) {
        plot <- plot %>%
          add_trace(
            x = seg$mz,
            y = seg$intensity,
            type = "scattergl",
            mode = "markers+text",
            marker = list(size = 2, color = cl[var_val]),
            text = seg$text_string,
            textposition = "top center",
            textfont = list(size = 9, color = cl[var_val]),
            hoverinfo = "text",
            name = var_val,
            legendgroup = var_val,
            showlegend = FALSE
          )
      }
    }

    plot <- plot %>%
      plotly::layout(
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        uniformtext = list(minsize = 6, mode = "show")
      )

    plot
  }
}

# MARK: plot_features_ms2
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plot MS2 spectra for selected features.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-groups
#' @template arg-ms-components
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-normalized
#' @template arg-ms-filtered
#' @template arg-plot-groupBy
#' @export
#'
plot_features_ms2.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    features = NULL,
    groups = NULL,
    components = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    normalized = TRUE,
    filtered = FALSE,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "feature",
    showText = TRUE,
    interactive = TRUE) {
  fts <- get_features(
    x = x,
    analyses = analyses,
    features = features,
    groups = groups,
    components = components,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    filtered = filtered
  )

  if (nrow(fts) == 0) {
    message("\u2717 MS2 traces not found for the targets!")
    return(NULL)
  }

  ms2_list <- lapply(
    seq_len(nrow(fts)),
    function(i) {
      ft <- fts[i, ]
      sel <- !is.na(ft$ms2_mz) && nchar(ft$ms2_mz) > 0 &&
        !is.na(ft$ms2_intensity) && nchar(ft$ms2_intensity) > 0
      if (!sel) return(data.table::data.table())
      mz_dec <- rcpp_streamcraft_decode_string(ft$ms2_mz)
      int_dec <- rcpp_streamcraft_decode_string(ft$ms2_intensity)
      if (length(mz_dec) == 0 || length(mz_dec) != length(int_dec)) {
        return(data.table::data.table())
      }
      data.table::data.table(
        mz = mz_dec,
        intensity = int_dec,
        analysis = ft$analysis,
        feature = ft$feature,
        is_pre = FALSE
      )
    }
  )

  if (normalized) {
    ms2_list <- lapply(ms2_list, function(z) {
      if (!is.null(z) && nrow(z) > 0) {
        max_int <- max(z$intensity)
        if (max_int > 0) z$intensity <- z$intensity / max_int
      }
      z
    })
  }

  ms2 <- data.table::rbindlist(ms2_list, fill = TRUE)
  if (nrow(ms2) == 0) {
    message("\u2717 MS2 traces not found for the targets!")
    return(NULL)
  }

  analyses_info <- info(x$analyses)
  rpl_map <- analyses_info$replicate
  names(rpl_map) <- analyses_info$analysis
  ms2$replicate <- rpl_map[ms2$analysis]
  data.table::setcolorder(ms2, c("analysis", "replicate", "feature"))

  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_ms2_id <- paste0(ms2$analysis, "-", ms2$feature)
  if ("feature_group" %in% colnames(fts)) {
    fgs <- fts$feature_group
    names(fgs) <- unique_fts_id
    ms2$feature_group <- fgs[unique_ms2_id]
  } else if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms2$feature_group <- fgs[unique_ms2_id]
  }

  if ("feature_component" %in% colnames(fts)) {
    fcs <- fts$feature_component
    names(fcs) <- unique_fts_id
    ms2$feature_component <- fcs[unique_ms2_id]
  } else if ("component" %in% colnames(fts)) {
    fcs <- fts$component
    names(fcs) <- unique_fts_id
    ms2$feature_component <- fcs[unique_ms2_id]
  }

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms2$name <- tar_ids[unique_ms2_id]
    data.table::setcolorder(ms2, c("analysis", "replicate", "name"))
  }

  desired_order <- c("analysis", "replicate", "feature", "feature_group", "feature_component")
  data.table::setcolorder(ms2, c(desired_order, setdiff(colnames(ms2), desired_order)))

  if (!(is.character(groupBy) && length(groupBy) >= 1 && all(groupBy %in% colnames(ms2)))) {
    warning("groupBy columns not found in MS2 data")
    return(NULL)
  }
  vals <- lapply(groupBy, function(col) as.character(ms2[[col]]))
  ms2$var <- do.call(paste, c(vals, sep = " - "))

  if (showText) {
    ms2$text_string <- paste0(round(ms2$mz, 4))
    ms2$text_string[ms2$is_pre] <- paste0("Pre ", ms2$text_string[ms2$is_pre])
  } else {
    ms2$text_string <- ""
  }

  ms2$loop <- paste0(ms2$analysis, ms2$replicate, ms2$id, ms2$var)
  cl <- .get_colors(unique(ms2$var))

  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"

    ms2$linesize <- 1
    ms2$linesize[ms2$is_pre] <- 2

    plot <- ggplot2::ggplot(
      ms2,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(ggplot2::aes(
        xend = mz,
        yend = 0,
        color = var,
        linewidth = linesize
      ))

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = text_string),
          vjust = 0.2,
          hjust = -0.2,
          angle = 90,
          size = 2,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max(ms2$intensity) * 1.5)
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_linewidth_continuous(range = c(1, 2), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = groupBy)

    plot
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"

    ms2$linesize <- 1
    ms2$linesize[ms2$is_pre] <- 2

    ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms2$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms2$intensity) * 1.5)
    )

    plot <- plot_ly()
    seen_vars <- character(0)
    for (lp in unique(ms2$loop)) {
      seg <- ms2[ms2$loop == lp, ]
      if (nrow(seg) == 0) next
      var_val <- seg$var[1]
      show_leg <- !(var_val %in% seen_vars)
      if (show_leg) seen_vars <- c(seen_vars, var_val)
      x_seg <- as.numeric(rbind(seg$mz, seg$mz, rep(NA, nrow(seg))))
      y_seg <- as.numeric(rbind(rep(0, nrow(seg)), seg$intensity, rep(NA, nrow(seg))))
      plot <- plot %>%
        add_trace(
          x = as.vector(x_seg),
          y = as.vector(y_seg),
          type = "scattergl",
          mode = "lines",
          line = list(color = cl[var_val], width = seg$linesize[1]),
          name = var_val,
          legendgroup = var_val,
          showlegend = show_leg,
          hoverinfo = "skip"
        )
      if (showText) {
        plot <- plot %>%
          add_trace(
            x = seg$mz,
            y = seg$intensity,
            type = "scattergl",
            mode = "markers+text",
            marker = list(size = 2, color = cl[var_val]),
            text = paste0(seg$text_string, "  "),
            textposition = "top center",
            textfont = list(size = 9, color = cl[var_val]),
            hoverinfo = "text",
            name = var_val,
            legendgroup = var_val,
            showlegend = FALSE
          )
      }
    }

    plot <- plot %>%
      plotly::layout(
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        uniformtext = list(minsize = 6, mode = "show")
      )

    plot
  }
}

# MARK: suspect_screening
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Performs suspect screening on features stored in the DuckDB
#' against a provided suspect database. Matches features based on mass or *m/z*, retention time, and when available
#' MS2 spectral similarity.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @param suspects A data.frame with suspect information. Must contain columns: `name` (character) and either `mass`
#' (neutral monoisotopic mass) or `mz` (expected m/z). Optional columns: `rt` (retention time in seconds), `formula`
#' (molecular formula), `SMILES`, `fragments` or `fragments_mz` (MS2 fragment m/z values, semicolon-separated),
#' `fragments_int` (MS2 fragment intensities, semicolon-separated), `fragments_formula` (fragment formulas,
#' semicolon-separated).
#' @param ppm Numeric. Mass tolerance in parts-per-million for matching suspect mass or *m/z* to features. Default: 5.
#' @param sec Numeric. Retention time tolerance in seconds for matching suspect RT to features. Default: 10.
#' @param ppmMS2 Numeric. Mass tolerance in ppm for MS2 fragment matching. Default: 10.
#' @param mzrMS2 Numeric. Minimum absolute m/z range for MS2 fragment matching (used when ppm range is smaller).
#' Default: 0.008.
#' @param minCosineSimilarity Numeric. Minimum cosine similarity score (0-1) for MS2 spectral matching to upgrade
#' identification level. Default: 0.7.
#' @param minSharedFragments Integer. Minimum number of shared fragments for MS2 matching to upgrade identification level.
#' Default: 3.
#' @param filtered Logical. If TRUE, includes filtered features in the search. Default: FALSE.
#' @return A data.table with matched suspects.
#' Identification levels: "1" = MS1+RT+MS2 match, "2" = MS1+MS2 match, "3b" = MS1+RT match, "4" = MS1 match only.
#' @export
#'
suspect_screening.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    suspects = NULL,
    ppm = 5,
    sec = 10,
    ppmMS2 = 10,
    mzrMS2 = 0.008,
    minCosineSimilarity = 0.7,
    minSharedFragments = 3,
    filtered = FALSE) {

  if (is.null(suspects)) {
    stop("Argument 'suspects' is required. Provide a data.frame with at least columns 'name' and 'mass' or 'mz'.")
  }

  suspects <- data.table::as.data.table(suspects)
  valid_db <- FALSE

  if (is.data.frame(suspects)) {
    suspects <- data.table::as.data.table(suspects)
    if (
      any(c("mass", "neutralMass") %in% colnames(suspects)) |
        "mz" %in% colnames(suspects)
    ) {
      if ("name" %in% colnames(suspects)) {
        if ("neutralMass" %in% colnames(suspects)) {
          data.table::setnames(suspects, "neutralMass", "mass")
        }
        valid_db <- TRUE
      }
    }
  }

  if (!valid_db) {
    stop(
      "Argument 'suspects' must be a data.frame with at least the columns 'name' and 'mass' or 'mz'!"
    )
  }

  if (!"rt" %in% colnames(suspects)) {
    suspects$rt <- 0
  } else {
    suspects$rt[suspects$rt == ""] <- 0
  }
  suspects$rt <- as.numeric(suspects$rt)
  suspects$rt[is.na(suspects$rt)] <- 0

  # Query features from database based on suspect mass/mz
  if ("mass" %in% colnames(suspects)) {
    features <- get_features(
      x,
      analyses,
      mass = suspects,
      ppm = ppm,
      sec = sec,
      filtered = filtered
    )
  } else if ("mz" %in% colnames(suspects)) {
    features <- get_features(
      x,
      analyses,
      mz = suspects,
      ppm = ppm,
      sec = sec,
      filtered = filtered
    )
  } else {
    stop(
      "Argument 'suspects' must be a data.frame with at least the columns 'name' and 'mass' or 'mz'!"
    )
  }

  if (nrow(features) == 0) {
    message("\U2717 No suspects found!")
    return(data.table::data.table())
  }

  # Process each feature
  get_optional_char <- function(tbl, field) {
    if (field %in% colnames(tbl)) {
      val <- as.character(tbl[[field]][1])
      if (is.na(val) || length(val) == 0) "" else val
    } else {
      ""
    }
  }

  get_optional_numeric <- function(tbl, field, default = 0) {
    if (field %in% colnames(tbl)) {
      val <- suppressWarnings(as.numeric(tbl[[field]][1]))
      if (is.na(val)) default else val
    } else {
      default
    }
  }

  get_database_id <- function(tbl) {
    for (field in c("database_id", "id", "ID")) {
      val <- get_optional_char(tbl, field)
      if (nzchar(val)) return(val)
    }
    ""
  }

  suspects_out <- data.table::data.table()
  for (i in seq_len(nrow(features))) {
    # Match to suspect database
    suspect_db <- suspects[vapply(
      suspects$name,
      function(j) {
        grepl(j, features$name[i])
      },
      FALSE
    )]
    suspect_db <- suspect_db[1, ]

  formula_val <- get_optional_char(suspect_db, "formula")
  smiles_val <- get_optional_char(suspect_db, "SMILES")
  cas_val <- get_optional_char(suspect_db, "CAS")
  score_val <- get_optional_numeric(suspect_db, "score", default = 0)
  database_id_val <- get_database_id(suspect_db)

    # Initialize result row with all columns and default values
    temp <- data.table::data.table(
      analysis = features$analysis[i],
      feature = features$feature[i],
      name = features$name[i],
      polarity = features$polarity[i],
      db_mass = NA_real_,
      exp_mass = features$mass[i],
      error_mass = NA_real_,
      db_rt = suspect_db$rt,
      exp_rt = features$rt[i],
      error_rt = NA_real_,
      intensity = features$intensity[i],
      area = features$area[i],
      id_level = "4",
      score = score_val,
      shared_fragments = 0L,
      cosine_similarity = 0,
      formula = formula_val,
      SMILES = smiles_val,
      CAS = cas_val,
      database_id = database_id_val,
      db_ms2_size = 0L,
      db_ms2_mz = NA_character_,
      db_ms2_intensity = NA_character_,
      db_ms2_formula = NA_character_,
      exp_ms2_size = features$ms2_size[i],
      exp_ms2_mz = features$ms2_mz[i],
      exp_ms2_intensity = features$ms2_intensity[i]
    )

    # Calculate mass information
    if ("mz" %in% colnames(suspect_db) && !is.na(suspect_db$mz)) {
      temp$db_mass <- suspect_db$mz - (features$polarity[i] * 1.007276)
    } else if ("mass" %in% colnames(suspect_db) && !is.na(suspect_db$mass)) {
      temp$db_mass <- suspect_db$mass
    }

    if (!is.na(temp$db_mass) && !is.na(temp$exp_mass)) {
      temp$error_mass <- round(
        ((temp$exp_mass - temp$db_mass) / temp$exp_mass) * 1E6,
        digits = 1
      )
    }

    # Calculate RT error
    if (!is.na(temp$db_rt) && !is.na(temp$exp_rt)) {
      temp$error_rt <- round(temp$exp_rt - temp$db_rt, digits = 1)
    }

    # Upgrade identification level if RT matches
    if (!is.na(temp$db_rt) && temp$db_rt > 0 && !is.na(temp$exp_rt)) {
      temp$id_level <- "3b"
    }

    # MS2 spectral matching
    if (
      "fragments" %in% colnames(suspect_db) ||
        "fragments_mz" %in% colnames(suspect_db)
    ) {
      if ("fragments" %in% colnames(suspect_db)) {
        fragments <- suspect_db$fragments
      } else {
        fragments <- suspect_db$fragments_mz
      }

      if (!is.na(fragments)) {
        # Decode experimental MS2 data from encoded strings
        ms2 <- data.table::data.table()
        if (!is.na(features$ms2_mz[i]) && nchar(features$ms2_mz[i]) > 0 &&
            !is.na(features$ms2_intensity[i]) && nchar(features$ms2_intensity[i]) > 0) {
          mz_dec <- rcpp_streamcraft_decode_string(features$ms2_mz[i])
          int_dec <- rcpp_streamcraft_decode_string(features$ms2_intensity[i])
          if (length(mz_dec) > 0 && length(mz_dec) == length(int_dec)) {
            ms2 <- data.table::data.table(
              mz = mz_dec,
              intensity = int_dec
            )
          }
        }

        if (nrow(ms2) > 0) {
          # Parse suspect fragments
          if ("fragments" %in% colnames(suspect_db)) {
            fragments <- unlist(strsplit(
              fragments,
              split = "; ",
              fixed = TRUE
            ))
            fragments <- strsplit(fragments, " ")
            db_fragments <- data.table::data.table(
              "formula" = vapply(
                fragments,
                function(x) {
                  x[1]
                },
                NA_character_
              ),
              "mz" = vapply(
                fragments,
                function(x) {
                  as.numeric(x[1])
                },
                NA_real_
              ),
              "intensity" = vapply(
                fragments,
                function(x) {
                  as.numeric(x[2])
                },
                NA_real_
              )
            )
          } else {
            fragments <- unlist(strsplit(
              fragments,
              split = ";",
              fixed = TRUE
            ))
            fragments_int <- unlist(strsplit(
              suspect_db$fragments_int,
              split = ";",
              fixed = TRUE
            ))
            if ("fragments_formula" %in% colnames(suspect_db)) {
              if (grepl(";", suspect_db$fragments_formula)) {
                fragments_formula <- unlist(
                  strsplit(
                    suspect_db$fragments_formula,
                    split = ";",
                    fixed = TRUE
                  )
                )
              } else if (!suspect_db$fragments_formula %in% "") {
                fragments_formula <- rep(
                  suspect_db$fragments_formula,
                  length(fragments)
                )
              } else {
                fragments_formula <- rep(
                  NA_character_,
                  length(fragments)
                )
              }
            } else {
              fragments_formula <- rep(NA_character_, length(fragments))
            }

            db_fragments <- data.table::data.table(
              "formula" = fragments_formula,
              "mz" = as.numeric(fragments),
              "intensity" = as.numeric(fragments_int)
            )
          }

        # Store database MS2 as encoded base64 strings
        temp$db_ms2_size <- as.integer(nrow(db_fragments))
        temp$db_ms2_mz <- rcpp_streamcraft_encode_vector(db_fragments$mz)
        temp$db_ms2_intensity <- rcpp_streamcraft_encode_vector(db_fragments$intensity)
        temp$db_ms2_formula <- paste(db_fragments$formula, collapse = ";")

        # Calculate m/z ranges for matching
        mzr <- db_fragments$mz * ppmMS2 / 1E6
        mzr[mzr < mzrMS2] <- mzrMS2
        db_fragments$mzmin <- db_fragments$mz - mzr
        db_fragments$mzmax <- db_fragments$mz + mzr

        # Match suspect fragments to experimental MS2
          db_fragments$exp_idx <- vapply(
            seq_len(nrow(db_fragments)),
            function(z, ms2, db_fragments) {
              idx <- which(
                ms2$mz >= db_fragments$mzmin[z] &
                  ms2$mz <= db_fragments$mzmax[z]
              )
              if (length(idx) == 0) {
                NA_integer_
              } else {
                if (length(idx) > 1) {
                  candidates <- ms2$mz[idx]
                  mz_error <- abs(candidates - db_fragments$mz[z])
                  idx <- idx[which.min(mz_error)]
                  idx <- idx[1]
                }
                as.integer(idx)
              }
            },
            ms2 = ms2,
            db_fragments = db_fragments,
            integer(1)
          )

          number_shared_fragments <- length(db_fragments$exp_idx[
            !is.na(db_fragments$exp_idx)
          ])

          if (number_shared_fragments > 0) {
            # Calculate cosine similarity
            sel <- !is.na(db_fragments$exp_idx)
            intensity_db <- db_fragments$intensity[sel]
            intensity_db <- intensity_db / max(intensity_db)
            intensity_exp <- ms2$intensity[db_fragments$exp_idx[sel]]
            intensity_exp <- intensity_exp / max(intensity_exp)
            dot_pro <- sum(intensity_db * intensity_exp)
            mag_db <- sqrt(sum(intensity_db^2))
            mag_exp <- sqrt(sum(intensity_exp^2))
            cosine_similarity <- round(
              dot_pro / (mag_db * mag_exp),
              digits = 4
            )
          } else {
            cosine_similarity <- 0
          }

          temp$shared_fragments <- as.integer(number_shared_fragments)
          temp$cosine_similarity <- cosine_similarity

          # Upgrade identification level if MS2 matches well
          if (
            number_shared_fragments >= minSharedFragments ||
              cosine_similarity >= minCosineSimilarity
          ) {
            if (temp$id_level == "3b") {
              temp$id_level <- "1"
            } else if (temp$id_level == "4") {
              temp$id_level <- "2"
            }
          }
        }
      }
    }
    suspects_out <- data.table::rbindlist(list(suspects_out, temp), fill = TRUE)
  }
  col_order <- c(
    "analysis", "feature", "name", "polarity",
    "db_mass", "exp_mass", "error_mass",
    "db_rt", "exp_rt", "error_rt",
    "intensity", "area",
    "id_level", "score", "shared_fragments", "cosine_similarity",
    "formula", "SMILES", "CAS", "database_id",
    "db_ms2_size", "db_ms2_mz", "db_ms2_intensity", "db_ms2_formula",
    "exp_ms2_size", "exp_ms2_mz", "exp_ms2_intensity"
  )
  if (nrow(suspects_out) > 0) {
    keep_cols <- intersect(col_order, colnames(suspects_out))
    data.table::setcolorder(suspects_out, keep_cols)
  }
  suspects_out
}

#' @export
#' @noRd
get_suspects.DB_MassSpecResults_NonTargetAnalysis <- function(x, analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Check if table exists
  if (!"Suspects" %in% DBI::dbListTables(conn)) {
    message("\U2717 No suspects table found in database.")
    return(data.table::data.table())
  }

  # Get all suspects or filter by analyses
  if (is.null(analyses)) {
    suspects <- DBI::dbGetQuery(conn, "SELECT * FROM Suspects")
  } else {
    all_analyses <- info(x$analyses)$analysis
    sel_analyses <- .resolve_analyses_selection(analyses, all_analyses)
    if (length(sel_analyses) == 0) {
      return(data.table::data.table())
    }
    placeholders <- paste(rep("?", length(sel_analyses)), collapse = ", ")
    query <- sprintf("SELECT * FROM Suspects WHERE analysis IN (%s)", placeholders)
    suspects <- DBI::dbGetQuery(conn, query, params = as.list(sel_analyses))
  }

  data.table::as.data.table(suspects)
}

# MARK: plot_suspects_ms2
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plot suspect MS2 spectra for selected features.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @template arg-ms-features
#' @template arg-ms-groups
#' @template arg-ms-components
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-normalized
#' @template arg-ms-filtered
#' @template arg-plot-groupBy
#' @export
#'
plot_suspects_ms2.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    analyses = NULL,
    features = NULL,
    groups = NULL,
    components = NULL,
    mass = NULL,
    mz = NULL,
    rt = NULL,
    mobility = NULL,
    ppm = 20,
    sec = 60,
    millisec = 5,
    normalized = TRUE,
    filtered = FALSE,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = c("feature", "candidate_rank"),
    showText = TRUE,
    interactive = TRUE,
    showLegend = TRUE) {
  suspects <- get_suspects(x, analyses = analyses)

  if (nrow(suspects) == 0) {
    message("\u2717 Suspect MS2 traces not found for the targets!")
    return(NULL)
  }

  if (!is.null(features)) {
    if (is.data.frame(features)) {
      if ("analysis" %in% colnames(features)) {
        suspects <- suspects[suspects$analysis %in% unique(features$analysis), ]
      }
      if ("feature" %in% colnames(features)) {
        suspects <- suspects[suspects$feature %in% unique(features$feature), ]
      }
    } else {
      suspects <- suspects[suspects$feature %in% features, ]
    }
  }

  if (nrow(suspects) == 0) {
    message("\u2717 Suspect MS2 traces not found for the targets!")
    return(NULL)
  }

  spec_list <- lapply(
    seq_len(nrow(suspects)),
    function(i) {
      sp <- suspects[i, ]
      out <- list()

      has_exp <- !is.na(sp$exp_ms2_mz) && nchar(sp$exp_ms2_mz) > 0 &&
        !is.na(sp$exp_ms2_intensity) && nchar(sp$exp_ms2_intensity) > 0
      if (has_exp) {
        mz_dec <- rcpp_streamcraft_decode_string(sp$exp_ms2_mz)
        int_dec <- rcpp_streamcraft_decode_string(sp$exp_ms2_intensity)
        if (length(mz_dec) > 0 && length(mz_dec) == length(int_dec)) {
          out[[length(out) + 1]] <- data.table::data.table(
            mz = mz_dec,
            intensity = int_dec,
            analysis = sp$analysis,
            feature = sp$feature,
            name = sp$name,
            candidate_rank = sp$candidate_rank,
            db_ms2_formula = NA_character_,
            source = "exp"
          )
        }
      }

      has_db <- !is.na(sp$db_ms2_mz) && nchar(sp$db_ms2_mz) > 0 &&
        !is.na(sp$db_ms2_intensity) && nchar(sp$db_ms2_intensity) > 0
      if (has_db) {
        mz_dec <- rcpp_streamcraft_decode_string(sp$db_ms2_mz)
        int_dec <- rcpp_streamcraft_decode_string(sp$db_ms2_intensity)
        if (length(mz_dec) > 0 && length(mz_dec) == length(int_dec)) {
          formula_vec <- rep(NA_character_, length(mz_dec))
          if (!is.na(sp$db_ms2_formula) && nzchar(sp$db_ms2_formula)) {
            formula_split <- strsplit(sp$db_ms2_formula, ";", fixed = TRUE)[[1]]
            formula_split <- trimws(formula_split)
            if (length(formula_split) > 0) {
              if (length(formula_split) < length(mz_dec)) {
                formula_split <- c(formula_split, rep(NA_character_, length(mz_dec) - length(formula_split)))
              }
              if (length(formula_split) > length(mz_dec)) {
                formula_split <- formula_split[seq_len(length(mz_dec))]
              }
              formula_vec <- formula_split
            }
          }
          out[[length(out) + 1]] <- data.table::data.table(
            mz = mz_dec,
            intensity = -abs(int_dec),
            analysis = sp$analysis,
            feature = sp$feature,
            name = sp$name,
            candidate_rank = sp$candidate_rank,
            db_ms2_formula = formula_vec,
            source = "db"
          )
        }
      }

      if (length(out) == 0) {
        return(data.table::data.table())
      }
      data.table::rbindlist(out, fill = TRUE)
    }
  )

  spec_list <- spec_list[!sapply(spec_list, function(z) is.null(z) || nrow(z) == 0)]
  suspects_ms2 <- data.table::rbindlist(spec_list, fill = TRUE)
  if (nrow(suspects_ms2) == 0) {
    message("\u2717 Suspect MS2 traces not found for the targets!")
    return(NULL)
  }

  if (!is.numeric(suspects_ms2$intensity)) {
    suspects_ms2$intensity <- suppressWarnings(as.numeric(suspects_ms2$intensity))
  }
  if (!is.numeric(suspects_ms2$mz)) {
    suspects_ms2$mz <- suppressWarnings(as.numeric(suspects_ms2$mz))
  }
  suspects_ms2 <- suspects_ms2[is.finite(suspects_ms2$mz) & is.finite(suspects_ms2$intensity), ]
  if (nrow(suspects_ms2) == 0) {
    message("\u2717 Suspect MS2 traces not found for the targets!")
    return(NULL)
  }

  if (normalized) {
    suspects_ms2[
      ,
      intensity := {
        max_int <- max(abs(intensity), na.rm = TRUE)
        if (is.finite(max_int) && max_int > 0) intensity / max_int else intensity
      },
      by = .(analysis, feature, name, candidate_rank, source)
    ]
  }

  exclude_cols <- c(
    "db_ms2_mz", "db_ms2_intensity", "db_ms2_formula",
    "exp_ms2_mz", "exp_ms2_intensity"
  )
  detail_cols <- setdiff(colnames(suspects), exclude_cols)
  detail_cols <- detail_cols[detail_cols %in% colnames(suspects)]
  detail_cols <- setdiff(detail_cols, colnames(suspects_ms2))
  detail_cols <- unique(c("analysis", "feature", "name", "candidate_rank", detail_cols))
  detail_cols <- detail_cols[detail_cols %in% colnames(suspects)]
  suspects_details <- suspects[, detail_cols, with = FALSE]
  suspects_ms2 <- merge(
    suspects_ms2,
    suspects_details,
    by = c("analysis", "feature", "name", "candidate_rank"),
    all.x = TRUE
  )

  if (!(is.character(groupBy) && length(groupBy) >= 1 && all(groupBy %in% colnames(suspects_ms2)))) {
    warning("groupBy columns not found in suspect MS2 data")
    return(NULL)
  }

  vals <- lapply(groupBy, function(col) as.character(suspects_ms2[[col]]))
  suspects_ms2$var <- do.call(paste, c(vals, sep = " - "))
  suspects_ms2$loop <- paste0(
    suspects_ms2$analysis,
    suspects_ms2$feature,
    suspects_ms2$name,
    suspects_ms2$candidate_rank,
    suspects_ms2$source,
    suspects_ms2$var
  )
  cl <- .get_colors(unique(suspects_ms2$var))
  max_abs_int <- max(abs(suspects_ms2$intensity), na.rm = TRUE)
  if (!is.finite(max_abs_int) || max_abs_int == 0) max_abs_int <- 1

  if (showText) {
    base_txt <- paste0(round(suspects_ms2$mz, 4))
    has_formula <- !is.na(suspects_ms2$db_ms2_formula) &
      nzchar(suspects_ms2$db_ms2_formula) &
      suspects_ms2$source == "db"
    base_txt[has_formula] <- paste0(
      base_txt[has_formula],
      " - ",
      suspects_ms2$db_ms2_formula[has_formula]
    )
    suspects_ms2$text_string_gg <- base_txt
    suspects_ms2$text_string_plotly <- base_txt
    suspects_ms2$text_vjust <- ifelse(suspects_ms2$intensity < 0, 0.5, 0.2)
    suspects_ms2$text_hjust <- ifelse(suspects_ms2$intensity < 0, 1, -0.2)
  } else {
    suspects_ms2$text_string_gg <- ""
    suspects_ms2$text_string_plotly <- ""
    suspects_ms2$text_vjust <- 0.2
    suspects_ms2$text_hjust <- -0.2
  }

  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"

    suspects_ms2$linesize <- 1
    min_mz <- min(suspects_ms2$mz, na.rm = TRUE)
    max_mz <- max(suspects_ms2$mz, na.rm = TRUE)
    x_breaks <- scales::pretty_breaks(n = 6)(c(min_mz, max_mz))
    x_breaks <- x_breaks[x_breaks >= min_mz & x_breaks <= max_mz]

    plot <- ggplot2::ggplot(
      suspects_ms2,
      ggplot2::aes(x = mz, y = intensity, group = loop)
    ) +
      ggplot2::geom_segment(ggplot2::aes(
        xend = mz,
        yend = 0,
        color = var,
        linewidth = linesize
      ))

    if (showText) {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(
            label = text_string_gg,
            vjust = text_vjust,
            hjust = text_hjust
          ),
          angle = 90,
          size = 4,
          show.legend = FALSE
        )
    }

    plot <- plot +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = c(-max_abs_int * 1.5, max_abs_int * 1.5)
      ) +
      ggplot2::annotate(
        "segment",
        x = min_mz, xend = max_mz, y = 0, yend = 0,
        color = "black",
        linewidth = 0.3
      ) +
      ggplot2::geom_segment(
        data = data.table::data.table(x = x_breaks),
        ggplot2::aes(x = x, xend = x, y = 0, yend = -max_abs_int * 0.04),
        inherit.aes = FALSE,
        color = "black",
        linewidth = 0.3
      ) +
      ggplot2::geom_text(
        data = data.table::data.table(x = x_breaks),
        ggplot2::aes(x = x, y = -max_abs_int * 0.09, label = round(x, 2)),
        inherit.aes = FALSE,
        size = 3
      ) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_linewidth_continuous(range = c(1, 2), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        legend.position = if (isTRUE(showLegend)) "right" else "none"
      ) +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = groupBy)

    plot
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"

    ticksMin <- plyr::round_any(min(suspects_ms2$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(suspects_ms2$mz, na.rm = TRUE) * 1.1, 10)

    title <- list(text = title, font = list(size = 12, color = "black"))

    xaxis <- list(
      linecolor = "black",
      title = xLab,
      titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(suspects_ms2$mz) / 10), -1),
      ticks = "outside"
    )

    yaxis <- list(
      linecolor = "black",
      title = yLab,
      titlefont = list(size = 12, color = "black"),
      range = c(-max_abs_int * 1.2, max_abs_int * 1.2)
    )

    plot <- plot_ly()
    seen_vars <- character(0)
    for (lp in unique(suspects_ms2$loop)) {
      seg <- suspects_ms2[suspects_ms2$loop == lp, ]
      if (nrow(seg) == 0) next
      var_val <- seg$var[1]
      show_leg <- !(var_val %in% seen_vars)
      if (show_leg) seen_vars <- c(seen_vars, var_val)
      line_width <- ifelse(seg$source[1] == "db", 1.5, 1)
      hover_fields <- setdiff(
        colnames(seg),
        c(
          "mz", "intensity", "var", "loop", "text_string",
          "source", "db_ms2_formula"
        )
      )
      hover_fields <- hover_fields[hover_fields %in% colnames(seg)]
      hover_text <- apply(seg[, hover_fields, with = FALSE], 1, function(row) {
        row_vals <- as.character(row)
        row_vals[is.na(row_vals)] <- ""
        paste(paste0(hover_fields, ": ", row_vals), collapse = "<br>")
      })
      formula_line <- ifelse(
        seg$source == "db" &
          !is.na(seg$db_ms2_formula) &
          nzchar(seg$db_ms2_formula),
        paste0("<br>formula: ", seg$db_ms2_formula),
        ""
      )
      hover_text <- paste0(
        "source: ", seg$source,
        "<br>m/z: ", round(seg$mz, 4),
        formula_line,
        "<br>intensity: ", round(seg$intensity, 4),
        "<br>", hover_text
      )
      x_seg <- as.numeric(rbind(seg$mz, seg$mz, rep(NA, nrow(seg))))
      y_seg <- as.numeric(rbind(rep(0, nrow(seg)), seg$intensity, rep(NA, nrow(seg))))
      text_seg <- as.vector(rbind(hover_text, hover_text, rep(NA_character_, nrow(seg))))
      plot <- plot %>%
        add_trace(
          x = as.vector(x_seg),
          y = as.vector(y_seg),
          type = "scattergl",
          mode = "lines",
          line = list(color = cl[var_val], width = line_width),
          name = var_val,
          legendgroup = var_val,
          showlegend = show_leg,
          hoverinfo = "text",
          text = text_seg
        )
      if (showText) {
        plot <- plot %>%
          add_trace(
            x = seg$mz,
            y = seg$intensity,
            type = "scattergl",
            mode = "markers+text",
            marker = list(size = 2, color = cl[var_val]),
            text = seg$text_string_plotly,
            textposition = ifelse(seg$source[1] == "db", "bottom center", "top center"),
            textfont = list(size = 9, color = cl[var_val]),
            hoverinfo = "text",
            hovertext = hover_text,
            name = var_val,
            legendgroup = var_val,
            showlegend = FALSE
          )
      }
    }

    plot <- plot %>%
      plotly::layout(
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        uniformtext = list(minsize = 6, mode = "show"),
        showlegend = showLegend
      )

    plot
  }
}

# MARK: get_internal_standards
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Retrieves internal standards from the database.
#' @template arg-ntsdb-x
#' @template arg-analyses
#' @export
#'
get_internal_standards.DB_MassSpecResults_NonTargetAnalysis <- function(x, analyses = NULL) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Check if table exists
  if (!"InternalStandards" %in% DBI::dbListTables(conn)) {
    message("\U2717 No internal standards table found in database.")
    return(data.table::data.table())
  }

  # Get all internal standards or filter by analyses
  if (is.null(analyses)) {
    internal_standards <- DBI::dbGetQuery(conn, "SELECT * FROM InternalStandards")
  } else {
    all_analyses <- info(x$analyses)$analysis
    sel_analyses <- .resolve_analyses_selection(analyses, all_analyses)
    if (length(sel_analyses) == 0) {
      return(data.table::data.table())
    }
    placeholders <- paste(rep("?", length(sel_analyses)), collapse = ", ")
    query <- sprintf("SELECT * FROM InternalStandards WHERE analysis IN (%s)", placeholders)
    internal_standards <- DBI::dbGetQuery(conn, query, params = as.list(sel_analyses))
  }

  internal_standards <- data.table::as.data.table(internal_standards)

  if (nrow(internal_standards) == 0) {
    return(internal_standards)
  }

  # Query Features table to get replicate, feature_group, feature_component, and adduct
  # Build condition to match analysis and feature pairs
  analysis_feature_pairs <- internal_standards[, .(analysis, feature)]
  conditions <- sprintf(
    "(analysis = '%s' AND feature = '%s')",
    analysis_feature_pairs$analysis,
    analysis_feature_pairs$feature
  )
  features_query <- sprintf(
    "SELECT analysis, feature, feature_group, feature_component, adduct FROM Features WHERE %s",
    paste(conditions, collapse = " OR ")
  )
  features_data <- DBI::dbGetQuery(conn, features_query)
  features_data <- data.table::as.data.table(features_data)

  # Get replicate information from analyses
  analyses_info <- info(x$analyses)
  replicate_map <- analyses_info$replicate
  names(replicate_map) <- analyses_info$analysis
  internal_standards$replicate <- replicate_map[internal_standards$analysis]

  # Merge feature data
  if (nrow(features_data) > 0) {
    internal_standards <- merge(
      internal_standards,
      features_data,
      by = c("analysis", "feature"),
      all.x = TRUE
    )
  } else {
    # Add empty columns if no feature data found
    internal_standards$feature_group <- NA_character_
    internal_standards$feature_component <- NA_character_
    internal_standards$adduct <- NA_character_
  }

  # Reorder columns to put replicate, feature_group, feature_component, and adduct after feature
  col_order <- c(
    "analysis", "replicate", "feature", "feature_group", "feature_component", "adduct", "polarity",
    setdiff(colnames(internal_standards), c("analysis", "replicate", "feature", "feature_group", "feature_component", "adduct", "polarity"))
  )
  data.table::setcolorder(internal_standards, col_order)

  internal_standards
}

# MARK: get_fold_change
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Gets a data.table with fold-change analysis between the
#' `replicatesIn` and `replicatesOut`. This method is adapted from the work of
#' \href{https://pubs.acs.org/doi/10.1021/acs.analchem.7b03037}{Bader et al. (2017)}.
#' @param replicatesIn Character vector with the names of the replicates to be considered as the denominator.
#' @param replicatesOut Character vector with the names of the replicates to be considered as the numerator.
#' @param constantThreshold Numeric of length one. The threshold to consider a feature as constant.
#' @param eliminationThreshold Numeric of length one. The threshold to consider a feature as eliminated.
#' @template arg-ms-correctIntensity
#' @param fillZerosWithLowerLimit Logical of length one. When `TRUE` the zero values are filled with the lower limit.
#' @param lowerLimit Numeric of length one. The lower limit to fill the zero values.
#'
#' @export
#'
#' @references
#' \insertRef{bader01}{StreamFind}
#'
get_fold_change.DB_MassSpecResults_NonTargetAnalysis <- function(
  x,
  replicatesIn = NULL,
  replicatesOut = NULL,
  groups = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 4,
  sec = 10,
  millisec = 5,
  filtered = FALSE,
  constantThreshold = 0.5,
  eliminationThreshold = 0.2,
  correctIntensity = FALSE,
  fillZerosWithLowerLimit = FALSE,
  lowerLimit = NA_real_) {
  info_analyses <- info(x$analyses)
  all_names <- info_analyses$analysis
  rpls <- info_analyses$replicate

  if (is.numeric(replicatesIn)) {
    replicatesIn <- unique(rpls[replicatesIn])
  }
  if (is.numeric(replicatesOut)) {
    replicatesOut <- unique(rpls[replicatesOut])
  }

  if (any(is.na(replicatesIn)) || any(is.na(replicatesOut))) {
    message("\u2717 Replicates not found!")
    return(NULL)
  }

  if (length(replicatesIn) == 1 && length(replicatesOut) > 1) {
    replicatesIn <- rep(replicatesIn, length(replicatesOut))
  }

  fts <- get_features(
    x = x,
    analyses = NULL,
    groups = groups,
    mass = mass,
    mz = mz,
    rt = rt,
    mobility = mobility,
    ppm = ppm,
    sec = sec,
    millisec = millisec,
    filtered = filtered
  )

  if (!"feature_group" %in% colnames(fts)) {
    warning("\u2717 Feature groups not found!")
    return(NULL)
  }

  if (correctIntensity && "correction" %in% colnames(fts)) {
    fts$intensity <- fts$intensity * fts$correction
  }

  fts <- fts[!is.na(fts$feature_group) & fts$feature_group != "", ]
  if (nrow(fts) == 0) {
    message("\u2717 Feature groups not found for the targets!")
    return(NULL)
  }

  fts_av <- fts[,
    .(intensity = max(intensity, na.rm = TRUE)),
    by = c("feature_group", "analysis")
  ]
  fts_av <- data.table::dcast(
    fts_av,
    feature_group ~ analysis,
    value.var = "intensity"
  )
  fts_av[is.na(fts_av)] <- 0
  data.table::setnames(fts_av, "feature_group", "group")
  groups_dt <- fts_av

  comb <- data.table::data.table()
  for (rep in seq_len(length(replicatesOut))) {
    out_temp <- all_names[rpls %in% replicatesOut[rep]]
    in_temp <- all_names[rpls %in% replicatesIn[rep]]
    comb_temp <- expand.grid(
      analysisIn = in_temp,
      analysisOut = out_temp,
      replicateIn = replicatesIn[rep],
      replicateOut = replicatesOut[rep]
    )
    comb <- data.table::rbindlist(list(comb, comb_temp), fill = TRUE)
  }

  if (nrow(comb) == 0) {
    warning(
      "\u2717 Combinations could not be made, check replicates IN and OUT!"
    )
    return(NULL)
  }

  fc <- lapply(
    seq_len(nrow(comb)),
    function(z, comb, groups_dt, fillZerosWithLowerLimit) {
      anaIn <- comb$analysisIn[z]
      anaOut <- comb$analysisOut[z]

      selOut <- colnames(groups_dt) %in% as.character(anaOut)
      vecOut <- groups_dt[, selOut, with = FALSE][[1]]

      selIn <- colnames(groups_dt) %in% as.character(anaIn)
      vecIn <- groups_dt[, selIn, with = FALSE][[1]]

      if (fillZerosWithLowerLimit) {
        if (is.na(lowerLimit)) {
          vecOut[vecOut == 0] <- min(vecOut[vecOut > 0])
          vecIn[vecIn == 0] <- min(vecIn[vecIn > 0])
        } else {
          vecOut[vecOut == 0] <- lowerLimit
          vecIn[vecIn == 0] <- lowerLimit
        }
      }

      fc_vec <- as.numeric(vecOut) / as.numeric(vecIn)

      res <- data.table::data.table("group" = groups_dt$group, "fc" = fc_vec)
      res$analysis_in <- anaIn
      res$analysis_out <- anaOut
      res$replicate_in <- comb$replicateIn[z]
      res$replicate_out <- comb$replicateOut[z]
      res$combination <- z
      res
    },
    comb = comb,
    groups_dt = groups_dt,
    fillZerosWithLowerLimit = fillZerosWithLowerLimit
  )

  fc <- data.table::rbindlist(fc)
  sel_nan <- is.nan(fc$fc)
  fc <- fc[!sel_nan, ]
  fc_category <- list(
    "Elimination" = c(0, eliminationThreshold),
    "Decrease" = c(eliminationThreshold, constantThreshold),
    "Constant" = c(constantThreshold, 1 / constantThreshold),
    "Increase" = c(1 / constantThreshold, 1 / eliminationThreshold),
    "Formation" = c(1 / eliminationThreshold, Inf)
  )
  fc_boundaries <- c(
    paste0("(", 0, "-", eliminationThreshold, ")"),
    paste0("(", eliminationThreshold, "-", constantThreshold, ")"),
    paste0("(", constantThreshold, "-", 1 / constantThreshold, ")"),
    paste0("(", 1 / constantThreshold, "-", 1 / eliminationThreshold, ")"),
    paste0("(", 1 / eliminationThreshold, "-Inf)")
  )
  names(fc_boundaries) <- names(fc_category)
  for (i in seq_along(fc_category)) {
    fc$category[
      fc$fc >= fc_category[[i]][1] &
        fc$fc <= fc_category[[i]][2]
    ] <- names(fc_category)[i]
  }
  sel_na_category <- is.na(fc$category)
  fc <- fc[!sel_na_category, ]
  fc$category <- factor(fc$category, levels = names(fc_category))
  fc$bondaries <- paste(fc$category, fc_boundaries[fc$category], sep = "\n")
  fc$bondaries <- factor(
    fc$bondaries,
    levels = paste(names(fc_category), fc_boundaries, sep = "\n")
  )
  fc
}

# MARK: plot_fold_change
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plots the fold-change analysis between the `replicatesIn`
#' and `replicatesOut`. This method is adapted from the work of
#' \href{https://pubs.acs.org/doi/10.1021/acs.analchem.7b03037}{Bader et al. (2017)}.
#' @param replicatesIn Character vector with the names of the replicates to be considered as the denominator.
#' @param replicatesOut Character vector with the names of the replicates to be considered as the numerator.
#' @template arg-ms-groups
#' @template arg-ms-mass
#' @template arg-ms-mz
#' @template arg-ms-rt
#' @template arg-ms-mobility
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-millisec
#' @template arg-ms-filtered
#' @param constantThreshold Numeric of length one. The threshold to consider a feature as constant.
#' @param eliminationThreshold Numeric of length one. The threshold to consider a feature as eliminated.
#' @template arg-ms-correctIntensity
#' @param fillZerosWithLowerLimit Logical of length one. When `TRUE` the zero values are filled with the lower limit.
#' @param lowerLimit Numeric of length one. The lower limit to fill the zero values.
#' @template arg-normalized
#' @template arg-yLab
#' @template arg-title
#' @template arg-interactive
#' @template arg-showLegend
#' @export
#'
plot_fold_change.DB_MassSpecResults_NonTargetAnalysis <- function(
  x,
  replicatesIn = NULL,
  replicatesOut = NULL,
  groups = NULL,
  mass = NULL,
  mz = NULL,
  rt = NULL,
  mobility = NULL,
  ppm = 4,
  sec = 10,
  millisec = 5,
  filtered = FALSE,
  constantThreshold = 0.5,
  eliminationThreshold = 0.2,
  correctIntensity = FALSE,
  fillZerosWithLowerLimit = FALSE,
  lowerLimit = NA_real_,
  normalized = TRUE,
  yLab = NULL,
  title = NULL,
  interactive = TRUE,
  showLegend = TRUE) {
  fc <- get_fold_change(
    x,
    replicatesIn,
    replicatesOut,
    groups,
    mass,
    mz,
    rt,
    mobility,
    ppm,
    sec,
    millisec,
    filtered,
    constantThreshold,
    eliminationThreshold,
    correctIntensity,
    fillZerosWithLowerLimit,
    lowerLimit
  )

  if (is.null(fc)) {
    return(NULL)
  }

  if (nrow(fc) == 0) {
    return(NULL)
  }

  fc_summary_count <- fc[,
    .(count = .N),
    by = c("combination", "bondaries", "replicate_out", "replicate_in")
  ]

  info_analyses <- info(x$analyses)
  fts_all <- get_features(x, analyses = NULL, filtered = filtered)
  groups_counts <- data.table::data.table(
    analysis = info_analyses$analysis,
    replicate = info_analyses$replicate,
    groups = 0
  )
  if ("feature_group" %in% colnames(fts_all)) {
    fts_all <- fts_all[!is.na(fts_all$feature_group) & fts_all$feature_group != "", ]
    if (nrow(fts_all) > 0) {
      group_counts <- fts_all[,
        .(groups = data.table::uniqueN(feature_group)),
        by = analysis
      ]
      groups_counts$groups <- group_counts$groups[match(groups_counts$analysis, group_counts$analysis)]
      groups_counts$groups[is.na(groups_counts$groups)] <- 0
    }
  }
  all_fts <- groups_counts[groups_counts$replicate %in% replicatesIn, ]

  unique_combinations_max <- unique(fc_summary_count[,
    c("combination", "replicate_out", "replicate_in"),
    with = FALSE
  ])

  unique_combinations_min <- unique_combinations_max

  unique_combinations_max$count <- vapply(
    unique_combinations_max$replicate_in,
    function(z, all_fts) {
      max(all_fts$groups[all_fts$replicate == z])
    },
    all_fts = all_fts,
    0
  )

  unique_combinations_min$count <- vapply(
    unique_combinations_min$replicate_in,
    function(z, all_fts) {
      min(all_fts$groups[all_fts$replicate == z])
    },
    all_fts = all_fts,
    0
  )

  unique_combinations_max$bondaries <- "Total\nfeatures in"
  unique_combinations_min$bondaries <- "Total\nfeatures in"

  fc_summary_count <- data.table::rbindlist(
    list(
      unique_combinations_max,
      unique_combinations_min,
      fc_summary_count
    ),
    use.names = TRUE
  )

  if (is.null(yLab)) {
    yLab <- "Number of feature groups"

    if (normalized) {
      yLab <- "Relative number of feature groups"
    }
  }

  if (!interactive) {
    fc_summary_count$bondaries <- paste(
      fc_summary_count$replicate_out,
      fc_summary_count$bondaries,
      sep = "\n"
    )

    fc_summary_count$bondaries <- factor(
      fc_summary_count$bondaries,
      levels = unique(fc_summary_count$bondaries)
    )
    replicate_out <- NULL
    bondaries <- NULL
    fc_levels <- fc_summary_count[, .(replicate_out, bondaries)]
    fc_levels <- unique(fc_levels)

    colours <- .get_colors(unique(fc_levels$replicate_out))
    colours_key <- colours[fc_levels$replicate_out]

    if (normalized) {
      fc_summary_count$uid <- paste0(
        fc_summary_count$replicate_out,
        "_",
        fc_summary_count$combination
      )

      for (i in unique(fc_summary_count$uid)) {
        sel <- fc_summary_count$uid %in% i
        fc_summary_count$count[sel] <- fc_summary_count$count[sel] /
          max(fc_summary_count$count[sel])
      }
    }

    graphics::boxplot(
      fc_summary_count$count ~ fc_summary_count$bondaries,
      data = fc_summary_count,
      col = paste0(colours_key, "50"),
      border = colours_key,
      main = title,
      xlab = NULL,
      ylab = yLab,
      outline = TRUE,
      ylim = c(0, max(fc_summary_count$count) + 1)
    )
    if (showLegend) {
      legend("topright", legend = names(colours), fill = colours)
    }
  } else {
    if (normalized) {
      fc_summary_count$uid <- paste0(
        fc_summary_count$replicate_out,
        "_",
        fc_summary_count$combination
      )
      for (i in unique(fc_summary_count$uid)) {
        sel <- fc_summary_count$uid %in% i
        fc_summary_count$count[sel] <- fc_summary_count$count[sel] /
          max(fc_summary_count$count[sel])
      }
    }

    fig <- plotly::plot_ly(
      data = fc_summary_count,
      x = ~bondaries,
      y = ~count,
      color = ~replicate_out,
      colors = .get_colors(unique(fc_summary_count$replicate_out)),
      type = "box",
      jitter = 0.03,
      showlegend = showLegend
    )
    fig <- fig %>%
      plotly::layout(
        title = title,
        xaxis = list(title = ""),
        yaxis = list(
          title = yLab,
          range = c(
            0,
            max(
              fc_summary_count$count
            ) *
              1.1
          )
        )
      )
    fig
  }
}


# MARK: .validate_DB_MassSpecResults_NonTargetAnalysis_features_dt
#' @noRd
.validate_DB_MassSpecResults_NonTargetAnalysis_features_dt <- function(x) {
  cols <- c(
    "feature", "feature_component", "feature_group", "adduct", "rt", "mz", "mass", "intensity",
    "noise", "sn", "area", "rtmin", "rtmax", "width", "mzmin", "mzmax", "ppm",
    "fwhm_rt", "fwhm_mz", "gaussian_A", "gaussian_mu", "gaussian_sigma",
    "gaussian_r2", "jaggedness", "sharpness", "asymmetry", "modality", "plates",
    "polarity", "filtered", "filter", "filled", "correction",
    "eic_size", "eic_rt", "eic_mz", "eic_intensity", "eic_baseline",
    "eic_smoothed", "ms1_size", "ms1_mz", "ms1_intensity", "ms2_size",
    "ms2_mz", "ms2_intensity"
  )
  missing_cols <- setdiff(cols, colnames(x))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in features data.table: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}

# MARK: .create_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema
#' @noRd
.create_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS Features (
    analysis VARCHAR PRIMARY KEY,
    feature VARCHAR,
    feature_component VARCHAR,
    feature_group VARCHAR,
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
    jaggedness DOUBLE,
    sharpness DOUBLE,
    asymmetry DOUBLE,
    modality INTEGER,
    plates DOUBLE,
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


# MARK: .validate_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema
#' @noRd
.validate_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema <- function(conn) {
  tryCatch(
    {
      table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Features)")
      required <- list(
        feature = "VARCHAR",
        feature_component = "VARCHAR",
        feature_group = "VARCHAR",
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
        jaggedness = "DOUBLE",
        sharpness = "DOUBLE",
        asymmetry = "DOUBLE",
        modality = "INTEGER",
        plates = "DOUBLE",
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
    },
    error = function(e) {
      stop("Schema migration check (Features): ", e$message)
    }
  )
  invisible(TRUE)
}


# MARK: .create_DB_MassSpecResults_NonTargetAnalysis_InternalStandards_db_schema
#' @noRd
.create_DB_MassSpecResults_NonTargetAnalysis_InternalStandards_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS InternalStandards (
    analysis VARCHAR,
    feature VARCHAR,
    candidate_rank INTEGER,
    name VARCHAR,
    polarity INTEGER,
    db_mass DOUBLE,
    exp_mass DOUBLE,
    error_mass DOUBLE,
    db_rt DOUBLE,
    exp_rt DOUBLE,
    error_rt DOUBLE,
    intensity DOUBLE,
    area DOUBLE,
    id_level VARCHAR,
    score DOUBLE,
    shared_fragments INTEGER,
    cosine_similarity DOUBLE,
    formula VARCHAR,
    SMILES VARCHAR,
    CAS VARCHAR,
    database_id VARCHAR,
    db_ms2_size INTEGER,
    db_ms2_mz VARCHAR,
    db_ms2_intensity VARCHAR,
    db_ms2_formula VARCHAR,
    exp_ms2_size INTEGER,
    exp_ms2_mz VARCHAR,
    exp_ms2_intensity VARCHAR,
    PRIMARY KEY (analysis, feature, candidate_rank)
  )")

  invisible(TRUE)
}


# MARK: .validate_DB_MassSpecResults_NonTargetAnalysis_InternalStandards_db_schema
#' @noRd
.validate_DB_MassSpecResults_NonTargetAnalysis_InternalStandards_db_schema <- function(conn) {
  tryCatch(
    {
      table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(InternalStandards)")
      required <- list(
        analysis = "VARCHAR",
        feature = "VARCHAR",
        name = "VARCHAR",
        polarity = "INTEGER",
        db_mass = "DOUBLE",
        exp_mass = "DOUBLE",
        error_mass = "DOUBLE",
        db_rt = "DOUBLE",
        exp_rt = "DOUBLE",
        error_rt = "DOUBLE",
        intensity = "DOUBLE",
        area = "DOUBLE",
        id_level = "VARCHAR",
        score = "DOUBLE",
        shared_fragments = "INTEGER",
        cosine_similarity = "DOUBLE",
        formula = "VARCHAR",
        SMILES = "VARCHAR",
        CAS = "VARCHAR",
        database_id = "VARCHAR",
        db_ms2_size = "INTEGER",
        db_ms2_mz = "VARCHAR",
        db_ms2_intensity = "VARCHAR",
        db_ms2_formula = "VARCHAR",
        candidate_rank = "INTEGER",
        exp_ms2_size = "INTEGER",
        exp_ms2_mz = "VARCHAR",
        exp_ms2_intensity = "VARCHAR"
      )
      for (col in names(required)) {
        if (!(col %in% table_info$name)) {
          message(sprintf("Adding missing %s column to InternalStandards table...", col))
          DBI::dbExecute(conn, sprintf("ALTER TABLE InternalStandards ADD COLUMN %s %s", col, required[[col]]))
        }
      }
    },
    error = function(e) {
      stop("Schema migration check (InternalStandards): ", e$message)
    }
  )
  invisible(TRUE)
}


# MARK: .create_DB_MassSpecResults_NonTargetAnalysis_Suspects_db_schema
#' @noRd
.create_DB_MassSpecResults_NonTargetAnalysis_Suspects_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS Suspects (
    analysis VARCHAR,
    feature VARCHAR,
    candidate_rank INTEGER,
    name VARCHAR,
    polarity INTEGER,
    db_mass DOUBLE,
    exp_mass DOUBLE,
    error_mass DOUBLE,
    db_rt DOUBLE,
    exp_rt DOUBLE,
    error_rt DOUBLE,
    intensity DOUBLE,
    area DOUBLE,
    id_level VARCHAR,
    score DOUBLE,
    shared_fragments INTEGER,
    cosine_similarity DOUBLE,
    formula VARCHAR,
    SMILES VARCHAR,
    CAS VARCHAR,
    database_id VARCHAR,
    db_ms2_size INTEGER,
    db_ms2_mz VARCHAR,
    db_ms2_intensity VARCHAR,
    db_ms2_formula VARCHAR,
    exp_ms2_size INTEGER,
    exp_ms2_mz VARCHAR,
    exp_ms2_intensity VARCHAR,
    PRIMARY KEY (analysis, feature, candidate_rank)
  )")

  invisible(TRUE)
}


# MARK: .validate_DB_MassSpecResults_NonTargetAnalysis_Suspects_db_schema
#' @noRd
.validate_DB_MassSpecResults_NonTargetAnalysis_Suspects_db_schema <- function(conn) {
  tryCatch(
    {
      table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Suspects)")
      required <- list(
        analysis = "VARCHAR",
        feature = "VARCHAR",
        candidate_rank = "INTEGER",
        name = "VARCHAR",
        polarity = "INTEGER",
        db_mass = "DOUBLE",
        exp_mass = "DOUBLE",
        error_mass = "DOUBLE",
        db_rt = "DOUBLE",
        exp_rt = "DOUBLE",
        error_rt = "DOUBLE",
        intensity = "DOUBLE",
        area = "DOUBLE",
        id_level = "VARCHAR",
        score = "DOUBLE",
        shared_fragments = "INTEGER",
        cosine_similarity = "DOUBLE",
        formula = "VARCHAR",
        SMILES = "VARCHAR",
        CAS = "VARCHAR",
        database_id = "VARCHAR",
        db_ms2_size = "INTEGER",
        db_ms2_mz = "VARCHAR",
        db_ms2_intensity = "VARCHAR",
        db_ms2_formula = "VARCHAR",
        exp_ms2_size = "INTEGER",
        exp_ms2_mz = "VARCHAR",
        exp_ms2_intensity = "VARCHAR"
      )
      missing_cols <- setdiff(names(required), table_info$name)
      for (col in missing_cols) {
        message(sprintf("Adding missing %s column to Suspects table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Suspects ADD COLUMN %s %s", col, required[[col]]))
      }

      table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Suspects)")
      pk_cols <- table_info$name[table_info$pk > 0]
      needs_rebuild <- !all(c("analysis", "feature", "candidate_rank") %in% pk_cols)
      if (needs_rebuild) {
        message("Rebuilding Suspects table to update primary key...")
        DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS Suspects_new (
          analysis VARCHAR,
          feature VARCHAR,
          candidate_rank INTEGER,
          name VARCHAR,
          polarity INTEGER,
          db_mass DOUBLE,
          exp_mass DOUBLE,
          error_mass DOUBLE,
          db_rt DOUBLE,
          exp_rt DOUBLE,
          error_rt DOUBLE,
          intensity DOUBLE,
          area DOUBLE,
          id_level VARCHAR,
          score DOUBLE,
          shared_fragments INTEGER,
          cosine_similarity DOUBLE,
          formula VARCHAR,
          SMILES VARCHAR,
          CAS VARCHAR,
          database_id VARCHAR,
          db_ms2_size INTEGER,
          db_ms2_mz VARCHAR,
          db_ms2_intensity VARCHAR,
          db_ms2_formula VARCHAR,
          exp_ms2_size INTEGER,
          exp_ms2_mz VARCHAR,
          exp_ms2_intensity VARCHAR,
          PRIMARY KEY (analysis, feature, candidate_rank)
        )")
        DBI::dbExecute(conn, "
          INSERT INTO Suspects_new
          SELECT
            analysis,
            feature,
            COALESCE(candidate_rank, 1) AS candidate_rank,
            name,
            polarity,
            db_mass,
            exp_mass,
            error_mass,
            db_rt,
            exp_rt,
            error_rt,
            intensity,
            area,
            id_level,
            score,
            shared_fragments,
            cosine_similarity,
            formula,
            SMILES,
            CAS,
            database_id,
            db_ms2_size,
            db_ms2_mz,
            db_ms2_intensity,
            db_ms2_formula,
            exp_ms2_size,
            exp_ms2_mz,
            exp_ms2_intensity
          FROM Suspects
        ")
        DBI::dbExecute(conn, "DROP TABLE Suspects")
        DBI::dbExecute(conn, "ALTER TABLE Suspects_new RENAME TO Suspects")
      }
    },
    error = function(e) {
      stop("Schema migration check (Suspects): ", e$message)
    }
  )
  invisible(TRUE)
}
