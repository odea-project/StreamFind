# Plotting Functions for Peak Finding Development
# This file contains all plotting functions used in the peak finding concept development

# Helper function to generate colors safely for any number of clusters
generate_safe_colors <- function(n_colors, palette = "Set1") {
  if (n_colors <= 0) return(character(0))

  # Get maximum colors available for the palette
  max_colors <- switch(palette,
    "Set1" = 9,
    "Set2" = 8,
    "Set3" = 12,
    "Paired" = 12,
    "Dark2" = 8,
    "Accent" = 8,
    9  # default fallback
  )

  if (n_colors <= max_colors) {
    # Use RColorBrewer if within limits
    return(RColorBrewer::brewer.pal(max(3, n_colors), palette))
  } else {
    # Generate base colors from RColorBrewer
    base_colors <- RColorBrewer::brewer.pal(max_colors, palette)

    # Generate additional random colors
    set.seed(123)  # For reproducible colors
    additional_colors <- rainbow(n_colors - max_colors, s = 0.7, v = 0.8)

    # Combine base and additional colors
    return(c(base_colors, additional_colors))
  }
}

# Alternative function using a predefined extended color palette
generate_extended_colors <- function(n_colors) {
  if (n_colors <= 0) return(character(0))

  # Extended color palette combining multiple sources
  extended_palette <- c(
    # RColorBrewer Set1 (9 colors)
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
    "#FFFF33", "#A65628", "#F781BF", "#999999",
    # RColorBrewer Dark2 (8 colors)
    "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
    "#E6AB02", "#A6761D", "#666666",
    # Additional high-contrast colors
    "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
    "#FDB462", "#B3DE69", "#FCCDE5", "#BC80BD", "#CCEBC5",
    "#FFED6F", "#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4",
    "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F", "#BB8FCE"
  )

  if (n_colors <= length(extended_palette)) {
    return(extended_palette[1:n_colors])
  } else {
    # If we need even more colors, cycle through and add random ones
    base_colors <- extended_palette
    set.seed(456)  # Different seed for variety
    additional_colors <- rainbow(n_colors - length(extended_palette),
                                s = runif(n_colors - length(extended_palette), 0.6, 0.9),
                                v = runif(n_colors - length(extended_palette), 0.7, 0.9))
    return(c(base_colors, additional_colors))
  }
}

plot_col_scatter <- function(data, col, interactive = TRUE) {
  if (is.data.frame(data)) {
    y_vals <- data[[col]]
    if ("cluster" %in% colnames(data)) {
      data <- data[!is.na(data$cluster), ]
      y_vals <- data[[col]]
      data$index <- seq_along(y_vals)

      # Generate safe colors for clusters
      clusters <- sort(unique(data$cluster))
      safe_colors <- generate_safe_colors(length(clusters))
      names(safe_colors) <- clusters

      if (interactive) {
        # Plotly version
        plotly::plot_ly(
          data = data,
          x = ~index,
          y = as.formula(paste0("~", col)),
          type = "scatter",
          mode = "markers",
          color = ~factor(cluster),
          colors = safe_colors
        ) %>%
          plotly::layout(
            title = paste("Scatter Plot of", col, "(Colored by Cluster)"),
            xaxis = list(title = "Index"),
            yaxis = list(title = col)
          ) %>%
          plotly::toWebGL()
      } else {
        # ggplot2 version
        data$cluster <- factor(data$cluster)
        p <- ggplot2::ggplot(data, ggplot2::aes(x = index, y = .data[[col]], color = cluster)) +
          ggplot2::geom_point() +
          ggplot2::scale_color_manual(values = safe_colors) +
          ggplot2::labs(
            title = paste("Scatter Plot of", col, "(Colored by Cluster)"),
            x = "Index",
            y = col
          ) +
          ggplot2::theme_minimal()
        return(p)
      }
    } else {
      data$index <- seq_along(y_vals)

      if (interactive) {
        # Plotly version
        plotly::plot_ly(
          data = data,
          x = ~index,
          y = as.formula(paste0("~", col)),
          type = "scatter",
          mode = "markers"
        ) %>%
          plotly::layout(
            title = paste("Scatter Plot of", col),
            xaxis = list(title = "Index"),
            yaxis = list(title = col)
          ) %>%
          plotly::toWebGL()
      } else {
        # ggplot2 version
        p <- ggplot2::ggplot(data, ggplot2::aes(x = index, y = .data[[col]])) +
          ggplot2::geom_point() +
          ggplot2::labs(
            title = paste("Scatter Plot of", col),
            x = "Index",
            y = col
          ) +
          ggplot2::theme_minimal()
        return(p)
      }
    }
  } else {
    # If input is a vector
    vector_data <- data.frame(index = seq_along(data), value = data)
    if (interactive) {
      # Plotly version
      plotly::plot_ly(
        data = vector_data,
        x = ~index,
        y = ~value,
        type = "scatter",
        mode = "markers"
      ) %>%
        plotly::layout(
          title = "Scatter Plot of Vector",
          xaxis = list(title = "Index"),
          yaxis = list(title = "Value")
        ) %>%
        plotly::toWebGL()
    } else {
      # ggplot2 version
      p <- ggplot2::ggplot(vector_data, ggplot2::aes(x = index, y = value)) +
        ggplot2::geom_point() +
        ggplot2::labs(
          title = "Scatter Plot of Vector",
          x = "Index",
          y = "Value"
        ) +
        ggplot2::theme_minimal()
      return(p)
    }
  }
}

plot_mz_vector <- function(data, interactive = TRUE) {
  plot_col_scatter(data, "mz", interactive = interactive)
}

plot_x_y <- function(data, x_col, y_col, interactive = TRUE) {
  has_cluster <- "cluster" %in% colnames(data)
  if (has_cluster) {
    data <- data[!is.na(data$cluster), ]
    # Generate safe colors for clusters
    clusters <- sort(unique(data$cluster))
    safe_colors <- generate_safe_colors(length(clusters))
    names(safe_colors) <- clusters
  }

  if (interactive) {
    # Plotly version
    plotly::plot_ly(
      data = data,
      x = as.formula(paste0("~", x_col)),
      y = as.formula(paste0("~", y_col)),
      type = "scatter",
      mode = "markers",
      color = if (has_cluster) ~factor(cluster) else NULL,
      colors = if (has_cluster) safe_colors else NULL
    ) %>%
    plotly::layout(
      title = paste("Scatter Plot of", y_col, "vs", x_col),
      xaxis = list(title = x_col),
      yaxis = list(title = y_col)
    ) %>%
    plotly::toWebGL()
  } else {
    # ggplot2 version
    if (has_cluster) {
      data$cluster <- factor(data$cluster)
      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = cluster)) +
        ggplot2::geom_point() +
        ggplot2::scale_color_manual(values = safe_colors) +
        ggplot2::labs(
          title = paste("Scatter Plot of", y_col, "vs", x_col),
          x = x_col,
          y = y_col,
          color = "Cluster"
        ) +
        ggplot2::theme_minimal()
    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
        ggplot2::geom_point() +
        ggplot2::labs(
          title = paste("Scatter Plot of", y_col, "vs", x_col),
          x = x_col,
          y = y_col
        ) +
        ggplot2::theme_minimal()
    }
    return(p)
  }
}

plot_3D_by_rt <- function(spec_data, rt_indices) {
  if (!missing(rt_indices)) {
    unique_rt_values <- unique(spec_data$rt)
    selected_rt_values <- unique_rt_values[rt_indices]
    spec_data <- spec_data[spec_data$rt == selected_rt_values, ]
  }
  if (nrow(spec_data) == 0) {
    warning("No data to plot after filtering by rt_indices")
    return(NULL)
  }
  if (nrow(spec_data) > 1000000) {
    set.seed(123)
    spec_data <- spec_data[sample(nrow(spec_data), 1000000), ]
    warning("Data too large, sampling down to 1,000,000 points for plotting")
  }
  has_cluster <- "cluster" %in% colnames(spec_data)
  has_noise <- "noise" %in% colnames(spec_data)
  if (has_noise) {
    clean_data <- data.table::copy(spec_data)
    spec_data <- spec_data[spec_data$intensity > spec_data$noise, ]
  }
  if (has_cluster) {
    spec_data <- spec_data[!is.na(spec_data$cluster), ]
    clusters <- sort(unique(spec_data$cluster))
    cluster_colors <- generate_safe_colors(length(clusters))
    names(cluster_colors) <- clusters
    spec_data$cluster <- factor(spec_data$cluster, levels = clusters)
    p <- plotly::plot_ly(
      data = spec_data,
      x = ~rt,
      y = ~mz,
      z = ~intensity,
      type = "scatter3d",
      mode = "markers",
      color = ~cluster,
      colors = cluster_colors,
      marker = list(
        size = 2,
        line = list(width = 0.5, color = "white")
      ),
      name = paste("Cluster", spec_data$cluster, sep = " ")
    )
  } else {
    p <- plotly::plot_ly(
      data = spec_data,
      x = ~rt,
      y = ~mz,
      z = ~intensity,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 2,
        color = ~intensity,
        colorscale = list(c(0, "darkblue"), c(1, "red")),
        showscale = FALSE,
        line = list(width = 0.5, color = "white")
      ),
      name = "Data Points"
    )
  }
  if (has_noise && !has_cluster) {
    if (nrow(clean_data) > 0) {
      p <- p %>% plotly::add_trace(
        data = clean_data,
        x = ~rt,
        y = ~mz,
        z = ~noise,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 2,
          color = "darkgreen",
          line = list(width = 0.5, color = "white")
        ),
        name = "Noise Points"
      )
    }
  }

  p %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "Retention Time (s)"),
        yaxis = list(title = "m/z"),
        zaxis = list(title = "Intensity")
      ),
      showlegend = TRUE
    )
}

plot_3D_by_rt_with_peaks <- function(spec_data, peaks_dt, rt_indices) {
  if (!missing(rt_indices)) {
    unique_rt_values <- unique(spec_data$rt)
    selected_rt_values <- unique_rt_values[rt_indices]
    spec_data <- spec_data[spec_data$rt %in% selected_rt_values, ]
  }
  if (nrow(spec_data) == 0) {
    warning("No spectral data to plot after filtering by rt_indices")
    return(NULL)
  }
  if (nrow(peaks_dt) == 0) {
    warning("No peaks data to plot")
    return(NULL)
  }
  rt_range <- range(spec_data$rt, na.rm = TRUE)
  mz_range <- range(spec_data$mz, na.rm = TRUE)
  peaks_dt <- peaks_dt[
    rtmax >= rt_range[1] & rtmin <= rt_range[2] &
    mzmax >= mz_range[1] & mzmin <= mz_range[2]
  ]
  peak_colors <- generate_extended_colors(nrow(peaks_dt))
  if (nrow(peaks_dt) == 0) {
    warning("No overlapping peaks to plot after filtering by spectral data ranges")
    return(NULL)
  }
  if (nrow(spec_data) > 1000000) {
    set.seed(123)
    spec_data <- spec_data[sample(nrow(spec_data), 1000000), ]
    warning("Spectral data too large, sampling down to 1,000,000 points for plotting")
  }

  # Find points within any peak
  in_peak <- rep(FALSE, nrow(spec_data))
  peak_indices_list <- vector("list", nrow(peaks_dt))
  for (i in seq_len(nrow(peaks_dt))) {
    peak <- peaks_dt[i, ]
    idx <- which(
      spec_data$rt >= peak$rtmin & spec_data$rt <= peak$rtmax &
      spec_data$mz >= peak$mzmin & spec_data$mz <= peak$mzmax
    )
    in_peak[idx] <- TRUE
    peak_indices_list[[i]] <- idx
  }

  # Plot points not in any peak as blue
  p <- plotly::plot_ly()
  if (any(!in_peak)) {
    p <- p %>% plotly::add_trace(
      data = spec_data[!in_peak, ],
      x = ~rt,
      y = ~mz,
      z = ~intensity,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 2,
        color = "blue",
        line = list(width = 0.5, color = "white")
      ),
      name = "Non-peak Data",
      showlegend = FALSE
    )
  }

  # Add traces for each peak (only points within the peak)
  for (i in seq_len(nrow(peaks_dt))) {
    idx <- peak_indices_list[[i]]
    if (length(idx) > 0) {
      p <- p %>% plotly::add_trace(
        data = spec_data[idx, ],
        x = ~rt,
        y = ~mz,
        z = ~intensity,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 2,
          color = peak_colors[i],
          line = list(width = 0.5, color = "white")
        ),
        name = peaks_dt$id[i],
        showlegend = TRUE
      )
    }
  }

  p %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "Retention Time (s)"),
        yaxis = list(title = "m/z"),
        zaxis = list(title = "Intensity")
      ),
      showlegend = TRUE
    )
}
