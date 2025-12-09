#' @title plot_lines_tabular_data
#' @description Generic utility to plot tabular data (data.table/data.frame) with user-specified x/y columns.
#' Other columns are used for hover text in interactive mode. Colors are auto-generated for groups.
#' @param data data.table or data.frame to plot (already processed, e.g., downsized).
#' @param xvar Name of column for x axis.
#' @param yvar Name of column for y axis.
#' @param groupBy Name of column for color grouping (optional, default: NULL).
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

  # Hover text: all columns except xvar, yvar, group
  hover_cols <- setdiff(colnames(data), c(xvar, yvar, "color_group"))
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
    p <- ggplot(data, aes_string(x = xvar, y = yvar, color = "color_group", group = "color_group")) +
      geom_line() +
      scale_color_manual(values = colors) +
      theme_classic() +
      labs(x = xLab, y = yLab, title = title, color = groupBy)
    return(p)
  } else {
    library(plotly)
    p <- plot_ly()
    group_vals <- unique(data$color_group)
    for (i in seq_along(group_vals)) {
      group_data <- data[data$color_group == group_vals[i], ]
      # Build hover text per point, appending x and y values from the actual plot data
      group_hover_text <- paste0(
        hover_text[data$color_group == group_vals[i]],
        "<br>x: ", group_data[[xvar]],
        "<br>y: ", group_data[[yvar]]
      )
      p <- add_trace(
        p,
        x = group_data[[xvar]],
        y = group_data[[yvar]],
        type = "scatter",
        mode = "lines+markers",
        name = as.character(group_vals[i]),
        line = list(color = colors[i], width = 0.5),
        marker = list(size = 2, color = colors[i]),
        text = group_hover_text,
        hoverinfo = "text"
      )
    }
    p <- layout(
      p,
      title = list(text = title, font = list(size = 12, color = "black")),
      xaxis = list(title = xLab, linecolor = "black", titlefont = list(size = 12, color = "black")),
      yaxis = list(title = yLab, linecolor = "black", titlefont = list(size = 12, color = "black"))
    )
    return(p)
  }
}