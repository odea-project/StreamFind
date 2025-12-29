# MARK: DB_MassSpecResults_Chromatograms
#' @title Constructor and methods to handle Mass Spectrometry chromatograms results using DuckDB
#' @description The `DB_MassSpecResults_Chromatograms` class represents chromatograms results from mass spectrometry analyses stored in a DuckDB database. This provides efficient storage and retrieval for large-scale chromatographic data.
#' @param db Path to the DuckDB database file. Defaults to "data.sf/MassSpecResults_Chromatograms.duckdb".
#' @param analyses A data.table containing information about the analyses.
#' @param chromatograms A data.table containing chromatogram data with columns: analysis, replicate, index, id, polarity, pre_mz, pre_ce, pro_mz, rt, intensity, and optional baseline/raw columns.
#' @param peaks A data.table containing peak data with columns related to chromatographic peaks.
#' @return An object of class `DB_MassSpecResults_Chromatograms`.
#' @export
#'
DB_MassSpecResults_Chromatograms <- function(
  db = file.path("data.sf", "DB_MassSpecResults_Chromatograms.duckdb"),
  analyses = data.table::data.table(),
  chromatograms = data.table::data.table(),
  peaks = data.table::data.table()
) {
  if (!requireNamespace("DBI", quietly = TRUE)) stop("DBI package required.")
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("duckdb package required.")
  dir.create(dirname(db), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  .create_DB_MassSpecAnalyses_Analyses_db_schema(conn)
  .validate_DB_MassSpecAnalyses_Analyses_db_schema(conn)
  .create_DB_MassSpecResults_Chromatograms_Chromatograms_db_schema(conn)
  .validate_DB_MassSpecResults_Chromatograms_Chromatograms_db_schema(conn)
  .create_DB_MassSpecResults_Chromatograms_Peaks_db_schema(conn)
  .validate_DB_MassSpecResults_Chromatograms_Peaks_db_schema(conn)
  
  insert_analyses <- function(analyses) {
    .validate_DB_MassSpecAnalyses_analyses_dt(analyses)
    DBI::dbExecute(conn, "DELETE FROM Analyses")
    DBI::dbWriteTable(conn, "Analyses", analyses, overwrite = TRUE)
  }
  
  insert_chromatograms <- function(chromatograms) {
    .validate_DB_MassSpecResults_Chromatograms_chromatograms_dt(chromatograms)
    DBI::dbExecute(conn, "DELETE FROM Chromatograms")
    DBI::dbWriteTable(conn, "Chromatograms", chromatograms, overwrite = TRUE)
  }
  
  insert_peaks <- function(peaks) {
    .validate_DB_MassSpecResults_Chromatograms_peaks_dt(peaks)
    DBI::dbExecute(conn, "DELETE FROM Peaks")
    DBI::dbWriteTable(conn, "Peaks", peaks, overwrite = TRUE)
  }
  
  # insert_calibration_model <- function(calibration_model) {
  #   DBI::dbExecute(conn, "DELETE FROM CalibrationModel")
  #   if (length(calibration_model) > 0) {
  #     cal_dt <- data.table::data.table(
  #       id = 1,
  #       model_data = list(serialize(calibration_model, NULL))
  #     )
  #     DBI::dbWriteTable(conn, "CalibrationModel", cal_dt, overwrite = TRUE)
  #   }
  # }
  
  if (nrow(analyses) > 0) insert_analyses(analyses)
  if (nrow(chromatograms) > 0) insert_chromatograms(chromatograms)
  if (nrow(peaks) > 0) insert_peaks(peaks)
  
  x <- structure(
    list(
      db = db,
      dataType = "DB_MassSpec"
    ),
    class = c("DB_MassSpecResults_Chromatograms", "DB_Results")
  )
  if (is.null(validate_object(x))) {
    x
  } else {
    stop("Invalid DB_MassSpecResults_Chromatograms object.")
  }
}

#' @describeIn DB_MassSpecResults_Chromatograms Validates the DB_MassSpecResults_Chromatograms object, returning NULL if valid.
#' @template arg-x-DB_MassSpecResults_Chromatograms
#' @export
#'
validate_object.DB_MassSpecResults_Chromatograms <- function(x) {
  checkmate::assert_class(x, "DB_MassSpecResults_Chromatograms")
  checkmate::assert_true(identical(x$dataType, "MassSpec"))
  if (!file.exists(x$db)) stop("DB_MassSpecResults_Chromatograms file not found: ", x$db)
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  required_tables <- c("Analyses", "Chromatograms", "Peaks")
  present <- DBI::dbListTables(conn)
  if (!all(required_tables %in% present)) stop("Missing required tables in DB_MassSpecResults_Chromatograms")
  .validate_DB_MassSpecAnalyses_Analyses_db_schema(conn)
  .validate_DB_MassSpecResults_Chromatograms_Chromatograms_db_schema(conn)
  .validate_DB_MassSpecResults_Chromatograms_Peaks_db_schema(conn)
  NextMethod()
}

# MARK: show
#' @describeIn DB_MassSpecResults_Chromatograms Prints a summary of the DB_MassSpecResults_Chromatograms object.
#' @template arg-x-DB_MassSpecResults_Chromatograms
#' @export
#' 
show.DB_MassSpecResults_Chromatograms <- function(x) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  cat("\n")
  cat(is(x))
  cat("\n")
  analyses <- DBI::dbReadTable(conn, "Analyses")
  chromatograms <- DBI::dbGetQuery(conn, "
    SELECT analysis, COUNT(*) AS chromatogram_count
    FROM Chromatograms
    GROUP BY analysis
  ")
  peaks <- DBI::dbGetQuery(conn, "
    SELECT analysis, COUNT(*) AS peak_count
    FROM Peaks
    GROUP BY analysis
  ")
  info <- data.table::data.table(
    "analysis" = analyses$analysis,
    "replicate" = analyses$replicate,
    "blank" = analyses$blank,
    "polarity" = analyses$polarity,
    "chromatograms" = chromatograms$chromatogram_count[match(analyses$analysis, chromatograms$analysis)],
    "peaks" = peaks$peak_count[match(analyses$analysis, peaks$analysis)]
  )
  info$chromatograms[is.na(info$chromatograms)] <- 0
  info$peaks[is.na(info$peaks)] <- 0
  print(info)
}

# MARK: get_chromatograms
#' @describeIn DB_MassSpecResults_Chromatograms Get chromatograms from the DB_MassSpecResults_Chromatograms object.
#' @template arg-x-DB_MassSpecResults_Chromatograms
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @export
#'
get_chromatograms.DB_MassSpecResults_Chromatograms <- function(
  x,
  analyses = NULL,
  chromatograms = NULL,
  rtmin = 0,
  rtmax = 0,
  minIntensity = NULL
) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel_names) == 0) return(list())
  query <- "SELECT * FROM Chromatograms"
  conditions <- c()
  conditions <- c(conditions, sprintf("analysis IN ('%s')", paste(sel_names, collapse = "','")))
  if (!is.null(chromatograms)) {
    if (is.numeric(chromatograms)) {
      conditions <- c(conditions, sprintf("\"index\" IN (%s)", paste(chromatograms, collapse = ",")))
    } else if (is.character(chromatograms)) {
      conditions <- c(conditions, sprintf("id IN ('%s')", paste(chromatograms, collapse = "','")))
    }
  }
  if (!is.null(minIntensity) && is.numeric(minIntensity)) {
    conditions <- c(conditions, sprintf("intensity > %f", minIntensity))
  }
  if (is.numeric(rtmin) && is.numeric(rtmax) && rtmax > 0) {
    conditions <- c(conditions, sprintf("rt >= %f AND rt <= %f", rtmin, rtmax))
  }
  query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  chroms <- DBI::dbGetQuery(conn, query)
  
  # Split by analysis and return as list
  chrom_list <- split(data.table::as.data.table(chroms), by = "analysis")
  chrom_list
}

# MARK: plot_chromatograms
#' @describeIn DB_MassSpecResults_Chromatograms Plot chromatograms from the DB_MassSpecResults_Chromatograms object.
#' @template arg-x-DB_MassSpecResults_Chromatograms
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @template arg-normalized
#' @template arg-labs
#' @template arg-title
#' @template arg-colorBy
#' @template arg-legendNames
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_chromatograms.DB_MassSpecResults_Chromatograms <- function(
  x,
  analyses = NULL,
  chromatograms = NULL,
  rtmin = 0,
  rtmax = 0,
  minIntensity = NULL,
  normalized = TRUE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses+targets",
  legendNames = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  chroms <- StreamFind::get_chromatograms(
    x,
    analyses,
    chromatograms,
    rtmin,
    rtmax,
    minIntensity
  )
  chroms <- data.table::rbindlist(chroms)
  if (nrow(chroms) == 0) {
    message("\U2717 DB_MassSpecResults_Chromatograms not found for the analyses!")
    return(NULL)
  }
  pol_key <- c("positive", "negative", "nd")
  names(pol_key) <- c("1", "-1", "0")
  chroms$polarity <- as.character(chroms$polarity)
  chroms$polarity <- pol_key[chroms$polarity]
  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  if (is.null(yLab)) {
    yLab <- "Intensity / counts"
  }
  chroms <- .make_colorBy_varkey(chroms, colorBy, legendNames)
  chroms$loop <- paste0(
    chroms$analysis,
    chroms$replicate,
    chroms$id,
    chroms$var
  )

  if (normalized) {
    chroms <- chroms %>%
      dplyr::group_by(loop) %>%
      dplyr::mutate(intensity = intensity / max(intensity))
  }

  cl <- .get_colors(unique(chroms$var))

  if (!interactive) {
    ggplot2::ggplot(chroms, ggplot2::aes(x = rt, y = intensity, group = loop)) +
      ggplot2::geom_line(ggplot2::aes(color = var)) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
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

    loop <- NULL

    plot <- chroms %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>index: ",
          index,
          "<br>id: ",
          id,
          "<br>polarity: ",
          polarity,
          "<br>pre_mz: ",
          pre_mz,
          "<br>pre_ce: ",
          pre_ce,
          "<br>pro_mz: ",
          pro_mz,
          "<br>rt: ",
          rt,
          "<br>intensity: ",
          intensity
        ),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )

    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }

    plot
  }
}

# MARK: plot_chromatograms_baseline
#' @describeIn DB_MassSpecResults_Chromatograms Plot chromatograms with baseline from the DB_MassSpecResults_Chromatograms object.
#' @template arg-x-DB_MassSpecResults_Chromatograms
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-labs
#' @template arg-title
#' @template arg-colorBy
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_chromatograms_baseline.DB_MassSpecResults_Chromatograms <- function(
  x,
  analyses = NULL,
  chromatograms = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  interactive = TRUE,
  renderEngine = "webgl"
) {
  chroms <- get_chromatograms(x, analyses, chromatograms, minIntensity = 0)
  chroms <- data.table::rbindlist(chroms)

  if (nrow(chroms) == 0) {
    message("\U2717 DB_MassSpecResults_Chromatograms not found for the analyses!")
    return(NULL)
  }

  if (!("baseline" %in% colnames(chroms) && "raw" %in% colnames(chroms))) {
    warning("Baseline not found!")
    return(NULL)
  }

  if (is.null(xLab)) {
    xLab <- "Retention time / seconds"
  }
  if (is.null(yLab)) {
    yLab <- "Intensity / counts"
  }

  chroms <- .make_colorBy_varkey(chroms, colorBy, legendNames = NULL)

  chroms$loop <- paste0(
    chroms$analysis,
    chroms$replicate,
    chroms$id,
    chroms$var
  )

  cl <- .get_colors(unique(chroms$var))

  if (!interactive) {
    ggplot2::ggplot(chroms, ggplot2::aes(x = rt, group = loop)) +
      ggplot2::geom_line(ggplot2::aes(y = raw, color = var)) +
      ggplot2::geom_line(
        ggplot2::aes(y = baseline, color = var),
        linetype = "dashed"
      ) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) +
      ggplot2::labs(color = colorBy)
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

    loop <- NULL

    plot <- chroms %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~raw,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines",
        line = list(width = 1),
        text = ~ paste(
          "<br>analysis: ",
          analysis,
          "<br>replicate: ",
          replicate,
          "<br>index: ",
          index,
          "<br>id: ",
          id,
          "<br>polarity: ",
          polarity,
          "<br>pre_mz: ",
          pre_mz,
          "<br>pre_ce: ",
          pre_ce,
          "<br>pro_mz: ",
          pro_mz,
          "<br>rt: ",
          rt,
          "<br>intensity: ",
          raw
        ),
        hoverinfo = "text",
        name = ~var,
        legendgroup = ~var
      ) %>%
      plotly::add_trace(
        x = ~rt,
        y = ~baseline,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines",
        line = list(dash = 'dash', width = 0.5),
        name = ~var,
        legendgroup = ~var,
        showlegend = FALSE
      ) %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )

    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }

    plot
  }
}

# MARK: get_chromatograms_peaks
#' @describeIn DB_MassSpecResults_Chromatograms Get peaks from the chromatograms object.
#' @template arg-x-DB_MassSpecResults_Chromatograms
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @export
#'
get_chromatograms_peaks.DB_MassSpecResults_Chromatograms <- function(
  x,
  analyses = NULL,
  chromatograms = NULL,
  rtmin = 0,
  rtmax = 0,
  minIntensity = NULL
) {
  conn <- DBI::dbConnect(duckdb::duckdb(), x$db)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  all_names <- DBI::dbGetQuery(conn, "SELECT analysis FROM Analyses")$analysis
  
  sel_names <- .resolve_analyses_selection(analyses, all_names)
  if (length(sel_names) == 0) return(data.table::data.table())
  
  query <- "SELECT * FROM Peaks"
  conditions <- c()
  conditions <- c(conditions, sprintf("analysis IN ('%s')", paste(sel_names, collapse = "','")))
  
  if (!is.null(chromatograms)) {
    if (is.numeric(chromatograms)) {
      conditions <- c(conditions, sprintf("\"index\" IN (%s)", paste(chromatograms, collapse = ",")))
    } else if (is.character(chromatograms)) {
      conditions <- c(conditions, sprintf("id IN ('%s')", paste(chromatograms, collapse = "','")))
    }
  }
  
  if (!is.null(minIntensity) && is.numeric(minIntensity)) {
    conditions <- c(conditions, sprintf("intensity > %f", minIntensity))
  }
  
  if (is.numeric(rtmin) && is.numeric(rtmax) && rtmax > 0) {
    conditions <- c(conditions, sprintf("rt >= %f AND rt <= %f", rtmin, rtmax))
  }
  
  query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  pks <- DBI::dbGetQuery(conn, query)
  
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found for the targets!")
    return(data.table::data.table())
  }
  
  data.table::as.data.table(pks)
}

# MARK: plot_chromatograms_peaks
#' @describeIn DB_MassSpecResults_Chromatograms Plot peaks from the chromatograms object.
#' @template arg-x-DB_MassSpecResults_Chromatograms
#' @template arg-analyses
#' @template arg-chromatograms
#' @template arg-ms-rtmin
#' @template arg-ms-rtmax
#' @template arg-ms-minIntensity
#' @template arg-labs
#' @template arg-title
#' @template arg-colorBy
#' @template arg-legendNames
#' @template arg-interactive
#' @template arg-renderEngine
#' @export
#'
plot_chromatograms_peaks.DB_MassSpecResults_Chromatograms <- function(
  x,
  analyses = NULL,
  chromatograms = NULL,
  rtmin = 0,
  rtmax = 0,
  minIntensity = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses+targets",
  legendNames = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
) {
  pks <- get_chromatograms_peaks(
    x,
    analyses,
    chromatograms,
    rtmin,
    rtmax,
    minIntensity
  )

  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found!")
    return(NULL)
  }

  chroms <- get_chromatograms(x, analyses, chromatograms)
  chroms <- data.table::rbindlist(chroms)

  if (nrow(chroms) == 0) {
    message("\U2717 DB_MassSpecResults_Chromatograms not found!")
    return(NULL)
  }

  if (grepl("targets", colorBy)) {
    pks$id <- pks$peak
  }

  pks <- .make_colorBy_varkey(pks, colorBy, legendNames)

  cl <- .get_colors(unique(pks$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)

  if (!interactive) {
    plot <- ggplot2::ggplot(chroms, ggplot2::aes(x = rt))

    for (i in seq_len(nrow(pks))) {
      pk_analysis <- pks[["analysis"]][i]
      pk_replicate <- pks[["replicate"]][i]
      pk_chrom <- pks[["index"]][i]
      pk_id <- pks[["peak"]][i]
      pk_var <- pks[["var"]][i]
      pk_rtmin <- pks[["rtmin"]][i]
      pk_rtmax <- pks[["rtmax"]][i]

      temp <- dplyr::filter(
        chroms,
        analysis %in%
          pk_analysis &
          replicate %in% pk_replicate &
          index %in% pk_chrom
      )

      temp$var <- pk_var

      plot <- plot +
        ggplot2::geom_line(
          data = temp,
          ggplot2::aes(y = intensity, color = var)
        )

      temp <- temp[temp$rt >= pk_rtmin & temp$rt <= pk_rtmax, ]

      plot <- plot +
        ggplot2::geom_ribbon(
          data = temp,
          ggplot2::aes(
            ymin = rep(min(intensity), length(intensity)),
            ymax = intensity,
            fill = var
          )
        )
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

    plot <- plot_ly(chroms, x = ~rt)

    for (i in seq_len(nrow(pks))) {
      pk_analysis <- pks[["analysis"]][i]
      pk_replicate <- pks[["replicate"]][i]
      pk_chrom <- pks[["index"]][i]
      pk_id <- pks[["peak"]][i]
      pk_var <- pks[["var"]][i]
      pk_rtmin <- pks[["rtmin"]][i]
      pk_rtmax <- pks[["rtmax"]][i]
      pk_sn <- pks[["sn"]][i]

      temp <- dplyr::filter(
        chroms,
        analysis %in%
          pk_analysis &
          replicate %in% pk_replicate &
          index %in% pk_chrom &
          rt >= pk_rtmin &
          rt <= pk_rtmax
      )

      plot <- plot %>%
        add_trace(
          data = temp,
          x = ~rt,
          y = ~intensity,
          type = "scatter",
          mode = "markers",
          marker = list(color = cl[pk_var], size = 5),
          text = ~ paste(
            "<br>analysis: ",
            pk_analysis,
            "<br>replicate: ",
            pk_replicate,
            "<br>chrom: ",
            pk_chrom,
            "<br>peak: ",
            pk_id,
            "<br>S/N: ",
            pk_sn,
            "<br>rt: ",
            round(rt, 2),
            "<br>intensity: ",
            round(intensity, 0)
          ),
          hoverinfo = "text",
          name = pk_var,
          legendgroup = pk_var,
          showlegend = FALSE
        )

      plot <- plot %>%
        plotly::add_ribbons(
          data = temp,
          x = ~rt,
          ymin = ~ min(intensity),
          ymax = ~intensity,
          line = list(color = cl[pk_var], width = 1.5),
          fillcolor = cl50[pk_var],
          text = ~ paste(
            "<br>analysis: ",
            pk_analysis,
            "<br>replicate: ",
            pk_replicate,
            "<br>chrom: ",
            pk_chrom,
            "<br>peak: ",
            pk_id,
            "<br>S/N: ",
            pk_sn,
            "<br>rt: ",
            round(rt, 2),
            "<br>intensity: ",
            round(intensity, 0)
          ),
          hoverinfo = "text",
          name = pk_var,
          legendgroup = pk_var,
          showlegend = show_legend[pk_var]
        )

      show_legend[pk_var] <- FALSE
    }

    for (i in seq_len(nrow(pks))) {
      pk_analysis <- pks[["analysis"]][i]
      pk_replicate <- pks[["replicate"]][i]
      pk_chrom <- pks[["index"]][i]
      pk_var <- pks[["var"]][i]

      plot <- plot %>%
        add_trace(
          data = dplyr::filter(
            chroms,
            analysis %in%
              pk_analysis &
              replicate %in% pk_replicate &
              index %in% pk_chrom
          ),
          x = ~rt,
          y = ~intensity,
          type = "scatter",
          mode = "lines",
          line = list(color = cl[pk_var], width = 0.5),
          name = pk_var,
          legendgroup = pk_var,
          showlegend = FALSE
        )
    }

    plot <- plot %>%
      plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )

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

# MARK: .validate_DB_MassSpecResults_Chromatograms_chromatograms_dt
#' @noRd
.validate_DB_MassSpecResults_Chromatograms_chromatograms_dt <- function(x) {
  required_cols <- c("analysis", "replicate", "index", "id", "polarity", "rt", "intensity")
  missing_cols <- setdiff(required_cols, colnames(x))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in chromatograms data.table: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}

# MARK: .validate_DB_MassSpecResults_Chromatograms_peaks_dt
#' @noRd
.validate_DB_MassSpecResults_Chromatograms_peaks_dt <- function(x) {
  required_cols <- c("analysis", "replicate", "index", "peak", "rt", "rtmin", "rtmax", "intensity")
  missing_cols <- setdiff(required_cols, colnames(x))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in peaks data.table: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}

# MARK: .create_DB_MassSpecResults_Chromatograms_Chromatograms_db_schema
#' @noRd
.create_DB_MassSpecResults_Chromatograms_Chromatograms_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS Chromatograms (
    analysis VARCHAR,
    replicate VARCHAR,
    \"index\" INTEGER,
    id VARCHAR,
    polarity INTEGER,
    pre_mz DOUBLE,
    pre_ce DOUBLE,
    pro_mz DOUBLE,
    rt DOUBLE,
    intensity DOUBLE,
    baseline DOUBLE,
    raw DOUBLE
  )")
  invisible(TRUE)
}

# MARK: .validate_DB_MassSpecResults_Chromatograms_Chromatograms_db_schema
#' @noRd
.validate_DB_MassSpecResults_Chromatograms_Chromatograms_db_schema <- function(conn) {
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Chromatograms)")
    required <- list(
      analysis = "VARCHAR",
      replicate = "VARCHAR",
      "index" = "INTEGER",
      id = "VARCHAR",
      polarity = "INTEGER",
      pre_mz = "DOUBLE",
      pre_ce = "DOUBLE",
      pro_mz = "DOUBLE",
      rt = "DOUBLE",
      intensity = "DOUBLE",
      baseline = "DOUBLE",
      raw = "DOUBLE"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to Chromatograms table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Chromatograms ADD COLUMN \"%s\" %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (Chromatograms): ", e$message)
  })
  invisible(TRUE)
}

# MARK: .create_DB_MassSpecResults_Chromatograms_Peaks_db_schema
#' @noRd
.create_DB_MassSpecResults_Chromatograms_Peaks_db_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS Peaks (
    analysis VARCHAR,
    replicate VARCHAR,
    \"index\" INTEGER,
    id VARCHAR,
    peak VARCHAR,
    polarity INTEGER,
    pre_mz DOUBLE,
    pre_ce DOUBLE,
    pro_mz DOUBLE,
    rt DOUBLE,
    rtmin DOUBLE,
    rtmax DOUBLE,
    intensity DOUBLE,
    area DOUBLE,
    sn DOUBLE,
    width DOUBLE,
    fwhm DOUBLE,
    tailing DOUBLE,
    asymmetry DOUBLE,
    gaussian_similarity DOUBLE
  )")
  invisible(TRUE)
}

# MARK: .validate_DB_MassSpecResults_Chromatograms_Peaks_db_schema
#' @noRd
.validate_DB_MassSpecResults_Chromatograms_Peaks_db_schema <- function(conn) {
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Peaks)")
    required <- list(
      analysis = "VARCHAR",
      replicate = "VARCHAR",
      "index" = "INTEGER",
      id = "VARCHAR",
      peak = "VARCHAR",
      polarity = "INTEGER",
      pre_mz = "DOUBLE",
      pre_ce = "DOUBLE",
      pro_mz = "DOUBLE",
      rt = "DOUBLE",
      rtmin = "DOUBLE",
      rtmax = "DOUBLE",
      intensity = "DOUBLE",
      area = "DOUBLE",
      sn = "DOUBLE",
      width = "DOUBLE",
      fwhm = "DOUBLE",
      tailing = "DOUBLE",
      asymmetry = "DOUBLE",
      gaussian_similarity = "DOUBLE"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to Peaks table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Peaks ADD COLUMN \"%s\" %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (Peaks): ", e$message)
  })
  invisible(TRUE)
}


