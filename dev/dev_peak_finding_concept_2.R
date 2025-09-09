# MARK Setup
# Setup -----------------------------------------------------------------------
devtools::load_all()
all_files <- StreamFindData::get_ms_file_paths()
files <- all_files[c(1, 7)]
#files <- all_files[grepl("blank|influent|o3sw", all_files)]
db_all <- StreamFindData::get_ms_tof_spiked_chemicals()
db_all <- db_all[grepl("S", db_all$tag), ]
cols <- c("name", "formula", "mass", "rt", "tag")
db_is <- db_all[db_all$tag %in% "IS", ]
db_is <- db_is[, cols, with = FALSE]
db_is <- db_is[!db_is$name %in% c("Ibuprofen-d3", "Naproxen-d3"), ]
db <- db_all[db_all$tag %in% "S", ]
db <- db[, cols, with = FALSE]
db_with_ms2 <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
db_with_ms2 <- db_with_ms2[db_with_ms2$tag %in% "S", ]
db_with_ms2 <- db_with_ms2[, c("name", "formula", "mass", "SMILES", "rt", "polarity", "fragments"), with = FALSE]
db_with_ms2$polarity[db_with_ms2$polarity == 1] <- "positive"
db_with_ms2$polarity[is.na(db_with_ms2$polarity)] <- "positive"
db_with_ms2$polarity[db_with_ms2$polarity == -1] <- "negative"
ms <- MassSpecEngine$new(analyses = files)

# MARK: Functions
# Functions -------------------------------------------------------------------
plot_vector_scatter <- function(vector_data) {
  # If input is a data.frame with 'cluster' column, color by cluster
  if (is.data.frame(vector_data) && "cluster" %in% colnames(vector_data)) {
    # Remove NA values from cluster column
    vector_data <- vector_data[!is.na(vector_data$cluster), ]

    plotly::plot_ly(
      data = vector_data,
      x = seq_along(vector_data[[1]]),
      y = vector_data[[1]],
      type = "scatter",
      mode = "markers",
      color = ~factor(cluster),
      colors = "Set1"
    ) %>%
    plotly::layout(
      title = "Scatter Plot of Vector (Colored by Cluster)",
      xaxis = list(title = "Index"),
      yaxis = list(title = "Value")
    ) %>%
    plotly::toWebGL()
  } else {
    plotly::plot_ly(
      x = seq_along(vector_data),
      y = vector_data,
      type = "scatter",
      mode = "markers"
    ) %>%
    plotly::layout(
      title = "Scatter Plot of Vector",
      xaxis = list(title = "Index"),
      yaxis = list(title = "Value")
    ) %>%
    plotly::toWebGL()
  }
}

plot_mz_vector <- function(data) {
  has_cluster <- "cluster" %in% colnames(data)

  # Remove NA values from cluster column if present
  if (has_cluster) {
    data <- data[!is.na(data$cluster), ]
  }

  plotly::plot_ly(
    data = data,
    x = seq_along(data$mz),
    y = data$mz,
    type = "scatter",
    mode = "markers",
    color = if (has_cluster) ~factor(cluster) else NULL,
    colors = if (has_cluster) "Set1" else NULL,
    text = paste("<br>MZ:", data$mz, "<br>RT:", data$rt, "<br>Intensity:", data$intensity,
                 if (has_cluster) paste("<br>Cluster:", data$cluster) else ""),
    hoverinfo = "text"
  ) %>%
  plotly::layout(
    title = "Scatter Plot of m/z vs Intensity",
    xaxis = list(title = "m/z"),
    yaxis = list(title = "Intensity")
  ) %>%
  plotly::toWebGL()
}

plot_x_y <- function(data, x_col, y_col) {
  # Remove NA values from cluster column if present
  if ("cluster" %in% colnames(data)) {
    data <- data[!is.na(data$cluster), ]
  }

  plotly::plot_ly(
    data = data,
    x = as.formula(paste0("~", x_col)),
    y = as.formula(paste0("~", y_col)),
    type = "scatter",
    mode = "markers",
    color = if ("cluster" %in% colnames(data)) ~factor(cluster) else NULL,
    colors = if ("cluster" %in% colnames(data)) "Set1" else NULL
  ) %>%
  plotly::layout(
    title = paste("Scatter Plot of", y_col, "vs", x_col),
    xaxis = list(title = x_col),
    yaxis = list(title = y_col)
  ) %>%
  plotly::toWebGL()
}

plot_3D_by_rt <- function(spec_data, rt_indices) {
  unique_rt_values <- sort(unique(spec_data$rt))
  if (!missing(rt_indices)) {
    selected_rt_values <- unique_rt_values[rt_indices]
    spec_data <- spec_data[spec_data$rt %in% selected_rt_values, ]
  }

  has_cluster <- "cluster" %in% colnames(spec_data)

  if (has_cluster) {
    # Remove NA values from cluster column
    spec_data <- spec_data[!is.na(spec_data$cluster), ]

    # Generate distinct colors for clusters
    clusters <- sort(unique(spec_data$cluster))
    cluster_colors <- RColorBrewer::brewer.pal(min(max(3, length(clusters)), 11), "Set1")
    if (length(clusters) > length(cluster_colors)) {
      cluster_colors <- rep(cluster_colors, ceiling(length(clusters) / length(cluster_colors)))
    }
    names(cluster_colors) <- clusters

    # Convert cluster to factor with all unique levels from the entire dataset
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
}

# MARK: Get_Spectra
# Get_Spectra ------------------------------------------------------------------
spec <- get_raw_spectra(
  ms$Analyses,
  analyses = 2,
  mz = data.frame(
    mzmin = 200,
    mzmax = 220,
    rtmin = 1100,
    rtmax = 1200
  ),
  ppm = 100,
  sec = 200,
  levels = 1
)

plot_3D_by_rt(spec, c(90))

unique_rt <- unique(spec$rt)
length(unique_rt)

spec_ordered <- spec[order(spec$mz), ]
spec_ordered <- spec_ordered[spec_ordered$intensity > 1000, ]
#plot_mz_vector(spec_ordered)

diff_mz <- diff(spec_ordered$mz)
#plot_vector_scatter(diff_mz)

# Cluster assignment based on mz differences (similar to C++ logic)
all_clusters <- integer(length(diff_mz))  # initialize with zeros

itMzClust <- 0.005  # example threshold, adjust as needed

for (j in seq_along(diff_mz)) {
  if (diff_mz[j] > itMzClust) all_clusters[j] <- 1
}

all_clusters <- cumsum(all_clusters)
all_clusters <- c(0, all_clusters)

all_clusters <- all_clusters + 1  # increment all values by 1

spec_ordered$cluster <- all_clusters

# count how many points are in each cluster
counter <- table(spec_ordered$cluster)
counter <- counter[counter > 5]

spec_ordered$cluster[!spec_ordered$cluster %in% names(counter)] <- NA_integer_

plot_mz_vector(spec_ordered)

plot_3D_by_rt(spec_ordered)

# merge mz in each cluster with the same rt
spec_ordered_merged <- spec_ordered[, .(
  mz = mean(mz),
  intensity = sum(intensity)
), by = .(rt, cluster)]

plot_3D_by_rt(spec_ordered_merged)




plot
