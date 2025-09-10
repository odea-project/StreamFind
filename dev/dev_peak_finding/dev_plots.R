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
  # Convert data.table to data.frame to avoid potential issues
  if (data.table::is.data.table(data)) {
    data <- as.data.frame(data)
  }
  
  has_cluster <- "cluster" %in% colnames(data)
  if (has_cluster) {
    data <- data[!is.na(data$cluster), ]
    # Generate safe colors for clusters
    clusters <- sort(unique(data$cluster))
    safe_colors <- generate_safe_colors(length(clusters))
    names(safe_colors) <- clusters
  }
  
  data$index <- seq_along(data$mz)
  
  if (interactive) {
    # Plotly version
    p <- plotly::plot_ly(
      data = data,
      x = ~index,
      y = ~mz,
      type = "scatter",
      mode = "markers",
      color = if (has_cluster) ~factor(cluster) else NULL,
      colors = if (has_cluster) safe_colors else NULL,
      text = paste("<br>MZ:", data$mz, "<br>RT:", data$rt, "<br>Intensity:", data$intensity,
                   if (has_cluster) paste("<br>Cluster:", data$cluster) else ""),
      hoverinfo = "text"
    ) %>%
    plotly::layout(
      xaxis = list(title = "index"),
      yaxis = list(title = "m/z")
    ) %>%
    plotly::toWebGL()
    return(p)
  } else {
    # ggplot2 version
    if (has_cluster && nrow(data) > 0) {
      data$cluster <- factor(data$cluster)
      p <- ggplot2::ggplot(data, ggplot2::aes(x = index, y = mz, color = cluster)) +
        ggplot2::geom_point() +
        ggplot2::scale_color_manual(values = safe_colors) +
        ggplot2::labs(
          x = "index",
          y = "m/z",
          color = "Cluster"
        ) +
        ggplot2::theme_minimal()
    } else if (nrow(data) > 0) {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = index, y = mz)) +
        ggplot2::geom_point() +
        ggplot2::labs(
          x = "index",
          y = "m/z"
        ) +
        ggplot2::theme_minimal()
    } else {
      stop("No data to plot after filtering NA clusters")
    }
    return(p)
  }
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

plot_3D_by_rt <- function(spec_data, rt_indices, interactive = TRUE) {
  unique_rt_values <- sort(unique(spec_data$rt))
  if (!missing(rt_indices)) {
    selected_rt_values <- unique_rt_values[rt_indices]
    spec_data <- spec_data[spec_data$rt %in% selected_rt_values, ]
  }
  has_cluster <- "cluster" %in% colnames(spec_data)
  
  if (interactive) {
    # Plotly 3D version
    if (has_cluster) {
      spec_data <- spec_data[!is.na(spec_data$cluster), ]
      clusters <- sort(unique(spec_data$cluster))
      
      # Use safe color generation instead of RColorBrewer directly
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
          line = list(width = 1, color = "black")
        )
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
          colorscale = list(c(0, "blue"), c(1, "red")),
          showscale = TRUE,
          line = list(width = 1, color = "black")
        )
      )
    }
    p %>%
      plotly::layout(
        scene = list(
          xaxis = list(title = "Retention Time (s)"),
          yaxis = list(title = "m/z"),
          zaxis = list(title = "Intensity")
        ),
        showlegend = TRUE
      ) %>%
      plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
  } else {
    # ggplot2 version - create multiple 2D projections since ggplot2 doesn't support 3D
    warning("3D plotting not supported with ggplot2. Creating 2D projections instead.")
    
    if (has_cluster) {
      spec_data <- spec_data[!is.na(spec_data$cluster), ]
      clusters <- sort(unique(spec_data$cluster))
      cluster_colors <- generate_safe_colors(length(clusters))
      names(cluster_colors) <- clusters
      spec_data$cluster <- factor(spec_data$cluster)
      
      # Create three 2D projections
      p1 <- ggplot2::ggplot(spec_data, ggplot2::aes(x = rt, y = mz, color = cluster)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::scale_color_manual(values = cluster_colors) +
        ggplot2::labs(title = "RT vs m/z (colored by cluster)", x = "Retention Time (s)", y = "m/z") +
        ggplot2::theme_minimal()
      
      p2 <- ggplot2::ggplot(spec_data, ggplot2::aes(x = rt, y = intensity, color = cluster)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::scale_color_manual(values = cluster_colors) +
        ggplot2::labs(title = "RT vs Intensity (colored by cluster)", x = "Retention Time (s)", y = "Intensity") +
        ggplot2::theme_minimal()
      
      p3 <- ggplot2::ggplot(spec_data, ggplot2::aes(x = mz, y = intensity, color = cluster)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::scale_color_manual(values = cluster_colors) +
        ggplot2::labs(title = "m/z vs Intensity (colored by cluster)", x = "m/z", y = "Intensity") +
        ggplot2::theme_minimal()
    } else {
      # Without clusters, color by intensity
      p1 <- ggplot2::ggplot(spec_data, ggplot2::aes(x = rt, y = mz, color = intensity)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::scale_color_gradient(low = "blue", high = "red") +
        ggplot2::labs(title = "RT vs m/z (colored by intensity)", x = "Retention Time (s)", y = "m/z") +
        ggplot2::theme_minimal()
      
      p2 <- ggplot2::ggplot(spec_data, ggplot2::aes(x = rt, y = intensity)) +
        ggplot2::geom_point(alpha = 0.7, color = "darkblue") +
        ggplot2::labs(title = "RT vs Intensity", x = "Retention Time (s)", y = "Intensity") +
        ggplot2::theme_minimal()
      
      p3 <- ggplot2::ggplot(spec_data, ggplot2::aes(x = mz, y = intensity)) +
        ggplot2::geom_point(alpha = 0.7, color = "darkblue") +
        ggplot2::labs(title = "m/z vs Intensity", x = "m/z", y = "Intensity") +
        ggplot2::theme_minimal()
    }
    
    # Return a list of plots
    return(list(
      rt_vs_mz = p1,
      rt_vs_intensity = p2,
      mz_vs_intensity = p3
    ))
  }
}
