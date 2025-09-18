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

plot_3D_by_rt <- function(spec_data, rt_indices) {
  if (!missing(rt_indices)) {
    unique_rt_values <- unique(spec_data$rt)
    selected_rt_values <- unique_rt_values[rt_indices]
    spec_data <- spec_data[spec_data$rt == selected_rt_values, ]
  }
  has_cluster <- "cluster" %in% colnames(spec_data)
  has_noise <- "noise" %in% colnames(spec_data)
  
  # Plotly 3D version (interactive only)
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
        size = 2
      ),
      name = "Data Points"
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
        showscale = FALSE
      ),
      name = "Data Points"
    )
  }

  if (has_noise) {
    # Remove rows with NA noise values
    clean_data <- spec_data[!is.na(spec_data$noise), ]
    
    if (nrow(clean_data) > 0) {
      # Add noise points as scatter3d with green color
      p <- p %>% plotly::add_trace(
        data = clean_data,
        x = ~rt,
        y = ~mz,
        z = ~noise,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 2,
          color = "green"
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

# Function to plot peaks overlaid on background chromatogram
plot_peaks_on_chromatogram <- function(background_data, peaks_data, interactive = TRUE) {
  # Convert data.table to data.frame if needed
  if (data.table::is.data.table(background_data)) {
    background_data <- as.data.frame(background_data)
  }
  if (data.table::is.data.table(peaks_data)) {
    peaks_data <- as.data.frame(peaks_data)
  }
  
  # Standardize column names for background data
  if ("rt" %in% colnames(background_data) && "intensity" %in% colnames(background_data)) {
    # Already has correct names
  } else {
    stop("Background data must contain 'rt' and 'intensity' columns")
  }
  
  # Standardize column names for peaks data
  # Check for different possible column name combinations
  if ("rt" %in% colnames(peaks_data) && "height" %in% colnames(peaks_data)) {
    # Already has correct names
  } else if ("x" %in% colnames(peaks_data) && "y" %in% colnames(peaks_data)) {
    # Rename x -> rt, y -> height
    peaks_data$rt <- peaks_data$x
    peaks_data$height <- peaks_data$y
  } else if ("rt" %in% colnames(peaks_data) && "intensity" %in% colnames(peaks_data)) {
    # Rename intensity -> height
    peaks_data$height <- peaks_data$intensity
  } else {
    stop("Peaks data must contain either ('rt', 'height'), ('x', 'y'), or ('rt', 'intensity') columns")
  }
  
  # Ensure rt columns are numeric
  background_data$rt <- as.numeric(background_data$rt)
  background_data$intensity <- as.numeric(background_data$intensity)
  peaks_data$rt <- as.numeric(peaks_data$rt)
  peaks_data$height <- as.numeric(peaks_data$height)
  
  # Add mz column to peaks if missing (for hover info)
  if (!"mz" %in% colnames(peaks_data)) {
    peaks_data$mz <- NA_real_
  }
  
  if (interactive) {
    # Create plotly interactive plot
    p <- plotly::plot_ly() %>%
      # Add background chromatogram line
      plotly::add_trace(
        data = background_data,
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        mode = "lines",
        name = "Chromatogram",
        line = list(color = "blue", width = 1),
        hovertemplate = "RT: %{x:.2f}<br>Intensity: %{y:.0f}<extra></extra>"
      ) %>%
      # Add peaks as vertical lines with points
      plotly::add_trace(
        data = peaks_data,
        x = ~rt,
        y = ~height,
        type = "scatter",
        mode = "markers+lines",
        name = "Peaks",
        marker = list(
          color = "red",
          size = 8,
          symbol = "triangle-up"
        ),
        line = list(color = "red", width = 2, dash = "dot"),
        hovertemplate = if (all(is.na(peaks_data$mz))) {
          "Peak RT: %{x:.2f}<br>Height: %{y:.0f}<extra></extra>"
        } else {
          "Peak RT: %{x:.2f}<br>Height: %{y:.0f}<br>m/z: %{customdata:.4f}<extra></extra>"
        },
        customdata = if (all(is.na(peaks_data$mz))) NULL else peaks_data$mz
      ) %>%
      # Add vertical lines from x-axis to peak points
      plotly::add_segments(
        data = peaks_data,
        x = ~rt, xend = ~rt,
        y = 0, yend = ~height,
        line = list(color = "red", width = 1, dash = "dash"),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      plotly::layout(
        title = "Detected Peaks on Chromatogram",
        xaxis = list(title = "Retention Time (s)"),
        yaxis = list(title = "Intensity"),
        hovermode = "closest"
      ) %>%
      plotly::toWebGL()
    
    return(p)
    
  } else {
    # Create ggplot2 static plot
    p <- ggplot2::ggplot() +
      # Add background chromatogram line
      ggplot2::geom_line(
        data = background_data,
        ggplot2::aes(x = rt, y = intensity),
        color = "blue",
        linewidth = 0.5,
        alpha = 0.8
      ) +
      # Add vertical lines for peaks
      ggplot2::geom_segment(
        data = peaks_data,
        ggplot2::aes(x = rt, xend = rt, y = 0, yend = height),
        color = "red",
        linetype = "dashed",
        alpha = 0.7
      ) +
      # Add peak points
      ggplot2::geom_point(
        data = peaks_data,
        ggplot2::aes(x = rt, y = height),
        color = "red",
        size = 3,
        shape = 17  # triangle
      ) +
      ggplot2::labs(
        title = "Detected Peaks on Chromatogram",
        x = "Retention Time (s)",
        y = "Intensity"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5)
      )
    
    return(p)
  }
}
