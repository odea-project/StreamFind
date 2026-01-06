# MARK: DB_MassSpecResults_NonTargetAnalysis
#' @title Constructor and methods to handle non-target analysis for mass spectrometry data
#' @description The `DB_MassSpecResults_NonTargetAnalysis` class is a child of the [StreamFind::DB_Results] class and is used to store results from non-target analysis (NTA) results from mass spectrometry data ("DB_MassSpec").
#' @param analyses A data frame containing information about the analyses.
#' @param headers A list of data frames containing information about the spectra headers.
#' @param features A list of data frames containing information about the features detected.
#' @return An object of class `DB_MassSpecResults_NonTargetAnalysis` with the following structure:
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
DB_MassSpecResults_NonTargetAnalysis <- function(
    db = file.path("data.sf", "DB_MassSpecResults_NonTargetAnalysis.duckdb"),
    analyses = data.table::data.table(),
    headers = data.table::data.table(),
    features = data.table::data.table()) {
  if (!requireNamespace("DBI", quietly = TRUE)) stop("DBI package required.")
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("duckdb package required.")
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_DB_MassSpecAnalyses_Analyses_db_schema(conn)
  .validate_DB_MassSpecAnalyses_Analyses_db_schema(conn)
  .create_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema(conn)
  .validate_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema(conn)

  insert_analyses <- function(analyses) {
    .validate_DB_MassSpecAnalyses_analyses_dt(analyses)
    DBI::dbExecute(conn, "DELETE FROM Analyses")
    DBI::dbWriteTable(conn, "Analyses", analyses, overwrite = TRUE)
  }

  insert_headers <- function(headers) {
    DBI::dbExecute(conn, "DROP TABLE IF EXISTS SpectraHeaders")
    DBI::dbWriteTable(conn, "SpectraHeaders", headers, overwrite = TRUE)
  }

  insert_features <- function(features) {
    .validate_DB_MassSpecResults_NonTargetAnalysis_features_dt(features)
    DBI::dbExecute(conn, "DELETE FROM Features")
    DBI::dbWriteTable(conn, "Features", features, overwrite = TRUE)
  }

  if (!is.null(analyses)) if (nrow(analyses) > 0) insert_analyses(analyses)
  if (!is.null(headers)) if (nrow(headers) > 0) insert_headers(headers)
  if (!is.null(features)) if (nrow(features) > 0) insert_features(features)

  x <- structure(
    list(
      db = db,
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
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  required_tables <- c("Analyses", "SpectraHeaders", "Features")
  present <- DBI::dbListTables(conn)
  if (!all(required_tables %in% present)) stop("Missing required tables in DB_MassSpecResults_NonTargetAnalysis")
  .validate_DB_MassSpecAnalyses_Analyses_db_schema(conn)
  .validate_DB_MassSpecResults_NonTargetAnalysis_Features_db_schema(conn)
  NextMethod()
}

# MARK: show
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Prints a summary of the DB_MassSpecResults_NonTargetAnalysis object.
#' @template arg-ntsdb-x
#' @export
#'
show.DB_MassSpecResults_NonTargetAnalysis <- function(x) {
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
  groups <- DBI::dbGetQuery(conn, "
    SELECT analysis, COUNT(DISTINCT [feature_group]) AS groups
    FROM Features
    WHERE NOT filtered AND feature_group != ''
    GROUP BY analysis
  ")
  groups$groups[is.na(groups$groups)] <- 0
  info <- data.table::data.table(
    "analysis" = analyses$analysis,
    "replicate" = analyses$replicate,
    "blank" = analyses$blank,
    "polarity" = analyses$polarity,
    "features" = features$not_filtered[match(analyses$analysis, features$analysis)],
    "filtered" = features$filtered[match(analyses$analysis, features$analysis)],
    "feature_groups" = groups$groups[match(analyses$analysis, groups$analysis)]
  )
  info <- info[order(tolower(info$analysis), info$analysis), ]
  print(info)
}

# MARK: get_features
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Retrieves features from the DB_MassSpecResults_NonTargetAnalysis object based on specified criteria.
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
get_features.DB_MassSpecResults_NonTargetAnalysis <- function(
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
    filtered = FALSE) {
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
  if (length(sel_names) == 0) {
    return(data.table::data.table())
  }
  query <- "SELECT * FROM Features"
  conditions <- c()
  conditions <- c(conditions, sprintf("filtered = %s", ifelse(filtered, "TRUE", "FALSE")))
  conditions <- c(conditions, sprintf("analysis IN ('%s')", paste(sel_names, collapse = "','")))
  if (!is.null(features)) {
    conditions <- c(conditions, sprintf("feature IN ('%s')", paste(features, collapse = "','")))
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
    features <- DBI::dbGetQuery(conn, query)
    if (!is.null(ids)) features$name <- ids[features$feature]
    if ("analysis" %in% colnames(features)) {
      rep_map <- data.table::data.table(analysis = all_names, replicate = rpls)
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
      sprintf(
        "(mz >= %f AND mz <= %f AND rt >= %f AND rt <= %f AND analysis = '%s' AND polarity = %d AND filtered = %s)",
        as.numeric(tgt["mzmin"]), as.numeric(tgt["mzmax"]),
        as.numeric(tgt["rtmin"]), as.numeric(tgt["rtmax"]),
        tgt["analysis"], as.integer(tgt["polarity"]),
        ifelse(filtered, "TRUE", "FALSE")
      )
    })
    query <- sprintf("SELECT * FROM Features WHERE %s", paste(conditions, collapse = " OR "))
    features <- DBI::dbGetQuery(conn, query)
    if ("analysis" %in% colnames(features)) {
      rep_map <- data.table::data.table(analysis = all_names, replicate = rpls)
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
      rep_map <- data.table::data.table(analysis = all_names, replicate = rpls)
      features <- merge(features, rep_map, by = "analysis", all.x = TRUE)
    }
    return(data.table::as.data.table(features))
  }
}

# MARK: plot_features
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plots features from the DB_MassSpecResults_NonTargetAnalysis object according to the specified parameters.
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
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "feature",
    interactive = TRUE,
    showDetails = FALSE) {
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

  if (length(eic_list) == 0) {
    message("\U2717 No valid EIC data found for plotting!")
    return(NULL)
  }

  eic <- data.table::rbindlist(eic_list, fill = TRUE)
  if (!(is.character(groupBy) && length(groupBy) >= 1 && all(groupBy %in% colnames(fts)))) {
    warning("groupBy columns not found in feature data")
    return(NULL)
  }
  vals <- lapply(groupBy, function(col) as.character(fts[[col]]))
  fts$var <- do.call(paste, c(vals, sep = " - "))
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

# MARK: plot_features_ms1
#' @describeIn DB_MassSpecResults_NonTargetAnalysis Plot MS1 spectra for selected features.
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
#' @template arg-normalized
#' @template arg-ms-filtered
#' @template arg-plot-groupBy
#' @export
#'
plot_features_ms1.DB_MassSpecResults_NonTargetAnalysis <- function(
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
    normalized = FALSE,
    filtered = FALSE,
    xLab = NULL,
    yLab = NULL,
    title = NULL,
    groupBy = "feature",
    showText = TRUE,
    interactive = TRUE) {
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

  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  rpls <- DBI::dbGetQuery(conn, "SELECT analysis, replicate FROM Analyses")
  rpl_map <- rpls$replicate
  names(rpl_map) <- rpls$analysis
  ms1$replicate <- rpl_map[ms1$analysis]
  data.table::setcolorder(ms1, c("analysis", "replicate", "feature"))

  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_ms1_id <- paste0(ms1$analysis, "-", ms1$feature)
  if ("feature_group" %in% colnames(fts)) {
    fgs <- fts$feature_group
    names(fgs) <- unique_fts_id
    ms1$group <- fgs[unique_ms1_id]
    data.table::setcolorder(ms1, c("analysis", "replicate", "group"))
  } else if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms1$group <- fgs[unique_ms1_id]
    data.table::setcolorder(ms1, c("analysis", "replicate", "group"))
  }

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms1$name <- tar_ids[unique_ms1_id]
    data.table::setcolorder(ms1, c("analysis", "replicate", "name"))
  }

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

  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  rpls <- DBI::dbGetQuery(conn, "SELECT analysis, replicate FROM Analyses")
  rpl_map <- rpls$replicate
  names(rpl_map) <- rpls$analysis
  ms2$replicate <- rpl_map[ms2$analysis]
  data.table::setcolorder(ms2, c("analysis", "replicate", "feature"))

  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_ms2_id <- paste0(ms2$analysis, "-", ms2$feature)
  if ("feature_group" %in% colnames(fts)) {
    fgs <- fts$feature_group
    names(fgs) <- unique_fts_id
    ms2$group <- fgs[unique_ms2_id]
    data.table::setcolorder(ms2, c("analysis", "replicate", "group"))
  } else if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms2$group <- fgs[unique_ms2_id]
    data.table::setcolorder(ms2, c("analysis", "replicate", "group"))
  }

  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms2$name <- tar_ids[unique_ms2_id]
    data.table::setcolorder(ms2, c("analysis", "replicate", "name"))
  }

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

    ms2$linesize <- 0.01
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
            textangle = 90,
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


# MARK: .validate_DB_MassSpecResults_NonTargetAnalysis_features_dt
#' @noRd
.validate_DB_MassSpecResults_NonTargetAnalysis_features_dt <- function(x) {
  cols <- c(
    "feature", "feature_component", "feature_group", "adduct", "rt", "mz", "mass", "intensity",
    "noise", "sn", "area", "rtmin", "rtmax", "width", "mzmin", "mzmax", "ppm",
    "fwhm_rt", "fwhm_mz", "gaussian_A", "gaussian_mu", "gaussian_sigma",
    "gaussian_r2", "polarity", "filtered", "filter", "filled", "correction",
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
