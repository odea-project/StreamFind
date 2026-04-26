#' @title plot_lines_tabular_data
#' @description Generic utility to plot tabular data (data.table/data.frame) with user-specified x/y columns.
#' Other columns are used for hover text in interactive mode. Colors are auto-generated for groups.
#' @param data data.table or data.frame to plot (already processed, e.g., downsized).
#' @param xvar Name of column for x axis.
#' @param yvar Name of column for y axis.
#' @param groupBy Name of column for color grouping (optional, default: NULL).
#' @param basicGroupBy Name of column(s) to define individual traces (optional).
#'   Traces will be created for each unique value of `basicGroupBy`, while
#'   `groupBy` controls color mapping. Example: `basicGroupBy = "analysis"`,
#'   `groupBy = "replicate"` will draw one trace per `analysis` and color
#'   traces according to their `replicate` value.
#' @param interactive Logical, use plotly if TRUE, ggplot2 if FALSE.
#' @param title Plot title.
#' @param xLab X axis label.
#' @param yLab Y axis label.
#' @param colorPalette Optional vector of colors, otherwise uses .get_colors.
#' @return Plot object (plotly or ggplot2)
#' @export
#'
.plot_lines_tabular_data <- function(
  data,
  xvar,
  yvar,
  groupBy = NULL,
  basicGroupBy = NULL,
  interactive = TRUE,
  title = NULL,
  xLab = NULL,
  yLab = NULL,
  colorPalette = NULL
) {
  stopifnot(xvar %in% colnames(data), yvar %in% colnames(data))
  if (is.null(xLab)) xLab <- xvar
  if (is.null(yLab)) yLab <- yvar
  if (is.null(title)) title <- paste(yvar, "vs", xvar)

  # Color assignment
  if (!is.null(groupBy)) {
    # Handle multiple groupBy columns
    if (length(groupBy) > 1) {
      # Check all columns exist
      missing_cols <- setdiff(groupBy, colnames(data))
      if (length(missing_cols) > 0) {
        stop("groupBy columns not found in data: ", paste(missing_cols, collapse = ", "))
      } else {
        # Create combined group_uid by pasting columns together
        group_values <- lapply(groupBy, function(col) as.character(data[[col]]))
        data$color_group <- do.call(paste, c(group_values, sep = "-"))
        groups <- unique(data$color_group)
        colors <- if (!is.null(colorPalette)) colorPalette else .get_colors(groups)
      }
    } else if (groupBy %in% colnames(data)) {
      groups <- unique(data[[groupBy]])
      colors <- if (!is.null(colorPalette)) colorPalette else .get_colors(groups)
      data$color_group <- data[[groupBy]]
    } else {
      warning("groupBy column '", groupBy, "' not found in data")
      data$color_group <- "all"
      colors <- .get_colors("all")
    }
  } else {
    data$color_group <- "all"
    colors <- .get_colors("all")
  }

  # Trace grouping (basicGroupBy): determines the individual traces
  if (!is.null(basicGroupBy)) {
    # Handle multiple basicGroupBy columns
    if (length(basicGroupBy) > 1) {
      missing_cols <- setdiff(basicGroupBy, colnames(data))
      if (length(missing_cols) > 0) {
        stop("basicGroupBy columns not found in data: ", paste(missing_cols, collapse = ", "))
      } else {
        basic_values <- lapply(basicGroupBy, function(col) as.character(data[[col]]))
        data$basic_group <- do.call(paste, c(basic_values, sep = "-"))
      }
    } else if (basicGroupBy %in% colnames(data)) {
      data$basic_group <- as.character(data[[basicGroupBy]])
    } else {
      warning("basicGroupBy column '", basicGroupBy, "' not found in data; using color grouping as basic group")
      data$basic_group <- data$color_group
    }
  } else {
    # Fallback: each color_group acts as a trace if basicGroupBy not provided
    data$basic_group <- data$color_group
  }

  # Hover text: all columns except xvar, yvar, and internal grouping cols
  hover_cols <- setdiff(colnames(data), c(xvar, yvar, "color_group", "basic_group"))
  # Use ..hover_cols for data.table compatibility
  if (inherits(data, "data.table")) {
    hover_data <- data[, ..hover_cols]
  } else {
    hover_data <- data[, hover_cols, drop = FALSE]
  }
  hover_text <- apply(hover_data, 1, function(row) {
    paste(paste(hover_cols, row, sep = ": "), collapse = "<br>")
  })

  if (!interactive) {
    library(ggplot2)
    # For static ggplot: color is controlled by `groupBy` (color_group) but
    # each trace should be formed by `basic_group` so use that for grouping.
    p <- ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]], color = color_group, group = basic_group)) +
      geom_line() +
      scale_color_manual(values = colors) +
      theme_classic() +
      labs(x = xLab, y = yLab, title = title, color = groupBy)
    return(p)
  } else {
    library(plotly)
    color_groups <- unique(data$color_group)
    seen_color_groups <- character(0)
    traces <- vector("list", length(unique(data$basic_group)))
    basic_vals <- unique(data$basic_group)
    for (i in seq_along(basic_vals)) {
      trace_data <- data[data$basic_group == basic_vals[i], ]
      trace_hover_text <- paste0(
        hover_text[data$basic_group == basic_vals[i]],
        "<br>x: ", trace_data[[xvar]],
        "<br>y: ", trace_data[[yvar]]
      )
      cg_vals <- as.character(trace_data$color_group)
      cg_mode <- cg_vals[which.max(tabulate(match(cg_vals, unique(cg_vals))))]
      color_idx <- match(cg_mode, color_groups)
      if (is.na(color_idx) || color_idx > length(colors)) color_val <- colors[1] else color_val <- colors[color_idx]
      showlegend_flag <- !(cg_mode %in% seen_color_groups)
      if (showlegend_flag) seen_color_groups <- c(seen_color_groups, cg_mode)
      traces[[i]] <- list(
        x = trace_data[[xvar]],
        y = trace_data[[yvar]],
        type = "scattergl",
        mode = "lines",
        name = as.character(cg_mode),
        legendgroup = as.character(cg_mode),
        showlegend = showlegend_flag,
        line = list(color = color_val, width = 1),
        text = trace_hover_text,
        hoverinfo = "text"
      )
    }
    p <- plotly::plot_ly()
    for (tr in traces) {
      p <- do.call(plotly::add_trace, c(list(p), tr))
    }
    p <- plotly::layout(
      p,
      title = list(text = title, font = list(size = 12, color = "black")),
      xaxis = list(title = xLab, linecolor = "black", titlefont = list(size = 12, color = "black")),
      yaxis = list(title = yLab, linecolor = "black", titlefont = list(size = 12, color = "black"))
    )
    return(p)
  }
}