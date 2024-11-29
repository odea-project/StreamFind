
# Resources -------
ms_files <- StreamFindData::get_ms_file_paths()
ms_files <- ms_files[grepl("blank|influent|o3sw", ms_files)]
ms_files_df <- data.frame(
  "file" = ms_files,
  "replicate" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("in_neg", 3),
    rep("in_pos", 3),
    rep("out_neg", 3),
    rep("out_pos", 3)
  ),
  "blank" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3)
  )
)

db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

#path <- "E:/example_ms_files"
path <- "C:/Users/apoli/Documents/example_files"
ms_files_complete <- list.files(path, pattern = ".mzML", full.names = TRUE)

# clear_cache("all")
# ms <- MassSpecEngine$new(analyses = ms_files_complete[c(1, 4)])
# ms$analyses$replicates <- c("centroid", "profile")
# ms$plot_spectra_3d(mass = dbsus[15, ], ppm = 60, sec = 120, colorBy = "replicates")

# NTS workflow -----
ms <- MassSpecEngine$new(analyses = ms_files_df)

show(ms$audit_trail)

as.data.frame(ms$audit_trail)


ms$run(MassSpecSettings_FindFeatures_openms())
ms$run(MassSpecSettings_AnnotateFeatures_StreamFind())
ms$run(MassSpecSettings_FindInternalStandards_StreamFind(database = dbis, ppm = 8, sec = 10))
ms$run(MassSpecSettings_FilterFeatures_StreamFind(excludeIsotopes = TRUE, excludeAdducts = TRUE))
ms$run(MassSpecSettings_GroupFeatures_openms())
ms$run(MassSpecSettings_FilterFeatures_StreamFind(minIntensity = 5000))
ms$run(
  MassSpecSettings_FillFeatures_StreamFind(
    withinReplicate = FALSE,
    rtExpand = 2,
    mzExpand = 0.0005,
    minTracesIntensity = 1000,
    minNumberTraces = 5,
    baseCut = 0.2,
    minSignalToNoiseRatio = 3,
    minGaussianFit = 0.2
  )
)
ms$run(
  MassSpecSettings_CalculateFeaturesQuality_StreamFind(
    filtered = FALSE,
    rtExpand = 2,
    mzExpand = 0.0005,
    minTracesIntensity = 1000,
    minNumberTraces = 6,
    baseCut = 0
  )
)

run(MassSpecSettings_CorrectMatrixSuppression_TiChri(), ms)

get_features(ms$analyses, mass = dbis$mass[1])
plot_groups_profile(ms$analyses, mass = dbis, correctSuppression = FALSE)

ms$get_internal_standards(average = TRUE)



library(reticulate)
reticulate::virtualenv_starter()

py_eval("1+1")

sif_parser <- import("sif_parser")
sif_parser$np_open()
np_open()



ms$nts <- ms$nts[, ms$get_groups(mass = dbsus)$group]


fts <- ms$nts$feature_list

# add an S3 class to the fts list
class(fts) <- c("fts", class(fts))


fts <- .convert_to_json(fts)
write(fts, "fts.json")

fts2 <- jsonlite::fromJSON(
  "fts.json",
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE,
  flatten = FALSE
)

as.data.table(fts2[[1]])

fts[grepl("FL", fts$feature), ]

clear_cache("all")
clear_cache("calculate_quality")
clear_cache("fill_features")

ms$analyses@analyses[[1]]











# PatRoon report -----
ms <- MassSpecEngine$new()
ms$load("ms.rds")
ms$run_app()



ms$get_features()$quality[1]
ms$get_features()$annotation[1]





# ms$run(MassSpecSettings_FindFeatures_openms())
# ms$run(MassSpecSettings_GroupFeatures_openms())
# ms$run(MassSpecSettings_FillFeatures_StreamFind())
# ms$run(MassSpecSettings_AnnotateFeatures_StreamFind())
# ms$run(MassSpecSettings_FindInternalStandards_StreamFind(database = dbis, ppm = 8, sec = 10))
# ms$run(MassSpecSettings_CalculateFeaturesQuality_StreamFind(minNumberTraces = 5))
# ms$run(MassSpecSettings_FilterFeatures_StreamFind(excludeIsotopes = TRUE))
# ms$run(MassSpecSettings_FilterFeatures_patRoon(absMinIntensity = 5000))
# ms$nts <- ms$nts[ , ms$get_groups(mass = dbsus)$group]
# ms$run(MassSpecSettings_LoadFeaturesMS1_StreamFind(filtered = FALSE))
# ms$run(MassSpecSettings_LoadFeaturesMS2_StreamFind(filtered = FALSE))
# ms$run(MassSpecSettings_LoadFeaturesEIC_StreamFind(filtered = FALSE))
# ms$run(MassSpecSettings_LoadMSPeakLists_StreamFind())
# ms$run(MassSpecSettings_GenerateFormulas_genform())
# ms$run(MassSpecSettings_GenerateCompounds_metfrag())
# ms$save("ms.rds")
# report(ms$nts)

# MS Targets -----


StreamFind::MassSpecTargets(
  mass = dbis$mass[3],
)@targets

ms <- MassSpecEngine$new(analyses = ms_files_complete[grepl("pn_", ms_files_complete)][1:3])
ms$get_spectra_headers(analyses = 1)
ms$get_spectra_polarity()

MassSpecTargets(mz = dbsus, analyses = names(ms$analyses), polarities = ms$get_spectra_polarity())
MassSpecTargets(mz = dbsus$mass[1], rt = dbsus$rt[1], polarities = "positive")
MassSpecTargets()

get_features(ms$analyses, mass = dbis$mass[7])
plot_spectra_eic(ms$analyses, analyses = c(1, 4), mass = dbis$mass[7])

# ms <- MassSpecEngine$new(analyses = ms_files_complete[7:24])
ms <- MassSpecEngine$new(analyses = ms_files_df)

ms$run(MassSpecSettings_FindFeatures_openms())
ms$run(MassSpecSettings_AnnotateFeatures_StreamFind(maxCharge = 2))
ms$run(MassSpecSettings_FilterFeatures_StreamFind(excludeIsotopes = TRUE, excludeAdducts = TRUE))
ms$run(MassSpecSettings_GroupFeatures_openms())
ms$run(MassSpecSettings_FilterFeatures_StreamFind(minIntensity = 5000))
ms$run(MassSpecSettings_FillFeatures_StreamFind())
ms$run(MassSpecSettings_CalculateFeaturesQuality_StreamFind(minNumberTraces = 5))
ms$run(MassSpecSettings_FilterFeatures_StreamFind(minSnRatio = 5))
ms$run(MassSpecSettings_FindInternalStandards_StreamFind(database = dbis, ppm = 8, sec = 10))
ms$nts <- ms$nts[, ms$get_groups(mass = dbsus)$group]
ms$run(MassSpecSettings_LoadFeaturesMS1_StreamFind(filtered = FALSE))
ms$run(MassSpecSettings_LoadFeaturesMS2_StreamFind(filtered = FALSE))
ms$run(MassSpecSettings_LoadFeaturesEIC_StreamFind(filtered = FALSE))

clear_cache("calculate_quality")
clear_cache("fill_features")
clear_cache("load_features_eic")
clear_cache("load_features_ms1")
clear_cache("load_features_ms2")

ms$plot_groups_ms2(mass = dbsus)

ms$plot_features(mass = dbsus, legendNames = TRUE)

ms$nts$has_features_eic

get_features_eic(ms$analyses)


fts <- rcpp_ms_load_features_ms1(
  ms$analyses$analyses,
  ms$nts$feature_list,
  filtered = FALSE,
  rtWindow = c(-2, 2),
  mzWindow = c(-1, 6),
  minTracesIntensity = 1000,
  mzClust = 0.005,
  presence = 0.8
)

fts <- rcpp_ms_load_features_ms2(
  ms$analyses$analyses,
  ms$nts$feature_list,
  filtered = FALSE,
  minTracesIntensity = 50,
  isolationWindow = 1.3,
  mzClust = 0.005,
  presence = 0.8
)

fts[[18]]$ms2[[1]]


fts <- ms$get_features()
fts <- fts[fts$group %in% fts$group[fts$filled], ]

# TODO add annotation when filling?

map_components(ms$analyses, mass = dbis[3, ], ppm = 15, sec = 30, filtered = TRUE, colorBy = "groups+analyses")

colnames(res)


get_suspects(ms$analyses, database = dbis, ppm = 15, sec = 30, filtered = TRUE, onGroups = TRUE)
ms$get_features(mass = dbis, filtered = TRUE)

get_internal_standards(ms$analyses)

plot_internal_standards(ms$analyses, presence = F, recovery = F)


# View(ms$get_features(mass = dbis, filtered = TRUE))


# Eval Isotopes and Adducts -----

all_fts <- ms$get_features(filtered = TRUE)[vapply(ms$get_features(filtered = TRUE)[["annotation"]], function(x) length(x) > 0, FALSE), ]
fts <- all_fts[vapply(all_fts[["annotation"]], function(x) x$adduct_cat != "" & x$adduct_time_error > 1, FALSE), ]
time_error <- vapply(fts[["annotation"]], function(x) x$adduct_time_error, 0)
colors <- StreamFind:::.get_colors(unique(fts$analysis))
fts$color <- colors[fts$analysis]
plot(time_error, col = fts$color, main = "Density Plot of Adduct Time Error", xlab = "Time Error", ylab = "Density")
# legend("topright", legend = unique(fts$analysis), col = colors, pch = 1)

comp_fts <- unique(vapply(fts[["annotation"]], function(x) x$component_feature, ""))
fts2 <- all_fts[vapply(all_fts[["annotation"]], function(x) x$component_feature %in% comp_fts & x$iso_step == 0, FALSE), ]
comp_fts2 <- vapply(fts2[["annotation"]], function(x) x$component_feature, "")
fts2 <- split(fts2, comp_fts2)
fts2 <- fts2[vapply(fts2, function(x) nrow(x) > 2, FALSE)]

ms$plot_features(features = fts2[[11]])

all_fts <- ms$get_features(filtered = TRUE)[vapply(ms$get_features(filtered = TRUE)[["annotation"]], function(x) length(x) > 0, FALSE), ]
fts <- all_fts[vapply(all_fts[["annotation"]], function(x) x$iso_step > 0 & x$iso_time_error > 0 , FALSE), ]
time_error <- vapply(fts[["annotation"]], function(x) x$iso_time_error, 0)
colors <- StreamFind:::.get_colors(unique(fts$analysis))
fts$color <- colors[fts$analysis]
plot(time_error, col = fts$color, main = "Density Plot of Isotope Time Error", xlab = "Time Error", ylab = "Density")
# legend("topright", legend = unique(fts$analysis), col = colors, pch = 1)

comp_fts <- unique(vapply(fts[["annotation"]], function(x) x$component_feature, ""))
fts2 <- all_fts[vapply(all_fts[["annotation"]], function(x) x$component_feature %in% comp_fts , FALSE), ]
comp_fts2 <- vapply(fts2[["annotation"]], function(x) x$component_feature, "")
fts2 <- split(fts2, comp_fts2)
fts2 <- fts2[vapply(fts2, function(x) nrow(x) > 2, FALSE)]

ms$plot_features(features = fts2[[2]])




ms$get_suspects(database = dbis, ppm = 15, sec = 30)


clear_cache("find_internal_standards")




ms$plot_features(features = c("f_11694816100682506556", "f_2475152984827434235"))








core <- CoreEngine$new()


core$headers <- ProjectHeaders(name = "Ricardo")


core$save()

core$load()









res <- rcpp_ms_calculate_features_quality(
  ms$analyses$analyses[1:3],
  ms$nts$feature_list[1:3],
  filtered = FALSE,
  rtExpand = 0,
  mzExpand = 0,
  minTracesIntensity = 0,
  minNumberTraces = 6,
  baseCut = 0.3
)


plot_features_count(ms$analyses)



clear_cache("annotate_features")

fts_all <- ms$get_features(analyses = 4)

fts <- ms$get_features(analyses = 4, mass = dbis)

# ms$run(MassSpecSettings_AnnotateFeatures_StreamFind())

# which(ms$nts$feature_list[[4]]$feature %in% "f_3479650755034549175") - 1

which(ms$nts$feature_list[[4]]$feature %in% "f_14501746324183792357") - 1


all.equal(fts_all$feature, res$feature)

length(fts_all$feature)
length(res$feature)

any(duplicated(fts_all$feature))
any(duplicated(res$feature))
any(duplicated(res$index))

res <- rcpp_ms_annotate_features(
  ms$nts$feature_list[1],
  rtWindowAlignment = 0.3,
  maxIsotopes = 8L,
  maxCharge = 2L,
  maxGaps = 1L
)

res <- res[res$iso_size > 1, ]



res$features_mz


map_features_intensity(ms$analyses, interactive = TRUE, colorBy = "replicates")








res <- rcpp_ms_load_features_eic(
  ms$analyses$analyses,
  ms$nts$feature_list,
  filtered = FALSE,
  rtExpand = 0,
  mzExpand = 0,
  minTracesIntensity = 1000
)

res$`01_tof_ww_is_neg_blank-r002`

View(ms$nts$feature_list)



clear_cache("fill_features")


res <- rcpp_ms_calculate_features_quality(
  ms$analyses$analyses[1:3],
  ms$nts$feature_list[1:3],
  filtered = FALSE,
  rtExpand = 0,
  mzExpand = 0,
  minTracesIntensity = 0,
  minNumberTraces = 6,
  baseCut = 0.3
)


fts <- ms$get_features(analyses = 1)
plot_features_distribution(fts)
fts <- fts[fts$intensity > 5000, ]
fts2 <- rcpp_ms_group_features(fts, 30, FALSE)
fts2 <- cbind(fts[, c("analysis", "group"), with = FALSE], fts2)


plot_features_distribution <- function(fts) {
  plotly::plot_ly(
    data = fts,
    x = ~rt,  # X-axis values
    y = ~intensity,  # Y-axis values
    color = ~analysis,  # Coloring by analysis category
    # size = ~mass,  # Dot size based on intensity
    type = 'scatter',  # Use scatter plot
    mode = 'markers',  # Show markers
    # marker = list(
    #   sizemode = 'diameter',
    #   opacity = 0.6
    #   # line = list(width = 1)  # Set line width to a single value
    # ),
    colors = StreamFind:::.get_colors(unique(fts$analysis)),  # Custom color scale
    text = ~paste("Intensity: ", intensity, "<br>Analysis: ", analysis),  # Hover information
    hoverinfo = 'text'  # Use the custom hover text
  ) %>%
    plotly::layout(
      title = "Scatter Plot with Intensity-based Sizes",
      xaxis = list(title = "RT"),
      yaxis = list(title = "Mass"),
      legend = list(title = "Analysis")
    )
}





View(fts2[fts2$group %in% "M304_R646_13253", ])
View(fts2[fts2$rtclust %in% 20, ])
View(fts2[fts2$group2 %in% "1821_22", ])

fts2_l <- fts2[order(fts2$rtclust),]
fts2_l <- split(fts2_l, fts2_l$rtclust)

test <- hist(fts2$intensity, breaks = 100, col = "lightblue", xlab = "Intensity", ylab = "Frequency", main = "Histogram of Intensity Values")
plot(test$density, test$mids, type = "l", col = "red", lwd = 2)















plot_clusters <- function(n, fts2_l) {
  # plotly::plot_ly(fts2_l[[n]], x = ~rt, y = ~mass, z = ~intensity, type = "heatmap", colors = c("blue", "red")) %>%
  #   plotly::layout(title = "Heatmap of RT and Mass", xaxis = list(title = "RT"), yaxis = list(title = "Mass"))
  plotly::plot_ly(
    data = fts2_l[[n]],
    x = ~rt,  # X-axis values
    y = ~mass,  # Y-axis values
    color = ~analysis,  # Coloring by analysis category
    size = ~intensity,  # Dot size based on intensity
    type = 'scatter',  # Use scatter plot
    mode = 'markers',  # Show markers
    marker = list(
      sizemode = 'diameter',
      opacity = 0.6
      # line = list(width = 1)  # Set line width to a single value
    ),
    colors = StreamFind:::.get_colors(unique(fts2_l[[n]]$analysis)),  # Custom color scale
    text = ~paste("Intensity: ", intensity, "<br>Analysis: ", analysis),  # Hover information
    hoverinfo = 'text'  # Use the custom hover text
  ) %>%
    plotly::layout(
      title = "Scatter Plot with Intensity-based Sizes",
      xaxis = list(title = "RT"),
      yaxis = list(title = "Mass"),
      legend = list(title = "Analysis")
    )
  
  # fts2_l[[n]] <- fts2_l[[n]][order(fts2_l[[n]], decreasing = TRUE)]
  # 
  # plotly::plot_ly(
  #   data = fts2_l[[n]],
  #   x = ~rt,  # X-axis values
  #   y = ~mass,  # Y-axis values
  #   color = ~intensity,  # Gradient color by intensity
  #   # size = ~intensity,  # Dot size based on intensity
  #   type = 'scatter',  # Use scatter plot
  #   mode = 'markers',  # Show markers
  #   marker = list(
  #     sizemode = 'diameter', 
  #     opacity = 0.8
  #   ),
  #   colorscale = 'Viridis',  # Color scale for gradient (e.g., 'Viridis', 'Inferno', etc.)
  #   colorbar = list(
  #     title = 'Intensity',  # Title of the colorbar
  #     tickprefix = ''
  #   ),
  #   text = ~paste("Intensity: ", intensity, "<br>Analysis: ", analysis),  # Hover information
  #   hoverinfo = 'text'  # Use the custom hover text
  # ) %>%
  #   plotly::layout(
  #     title = "Scatter Plot with Intensity-based Colors and Sizes",
  #     xaxis = list(title = "RT"),
  #     yaxis = list(title = "Mass")
  #   )
  
}
plot_clusters(24, fts2_l)



#plot(fts2_l[[n]]$rt, fts2_l[[n]]$mass, type = "p", col = StreamFind:::.get_colors(unique(fts2_l[[n]]$analysis))[fts2_l[[n]]$analysis], pch = 19, cex = 1.2)
#legend("topright", legend = unique(fts2_l[[n]]$analysis), col = StreamFind:::.get_colors(unique(fts2_l[[n]]$analysis)), pch = 1)


fts <- fts[order(fts$mass), ]
fts$massdiff <- c(0, diff(fts$mass))
fts$massclust <- rep(0, nrow(fts))
fts$massdev <- fts$mzmax - fts$mzmin
for (i in 2:nrow(fts)) {
  if (fts$massdiff[i] <= fts$massdev[i]) {
    fts$massclust[i] <- fts$massclust[i - 1]
  } else {
    fts$massclust[i] <- fts$massclust[i - 1] + 1
  }
}

fts <- fts[order(fts$rt), ]
fts$rtclust <- rep(0, nrow(fts))
for (i in 2:nrow(fts)) {
  if (fts$rt[i] < fts$rtmax[i - 1]) {
    fts$rtclust[i] <- fts$rtclust[i - 1]
  } else {
    fts$rtclust[i] <- fts$rtclust[i - 1] + 1
  }
}

fts$group2 <- paste0(fts$massclust, "_", fts$rtclust)

unique_groups2 <- unique(fts$group2)




plot(fts$mass, fts$massdiff, type = "l")




plot(fts$rt[1:10000], fts$mass[1:10000], type = "p")


fts_1 <- fts[fts$rt < fts$rtmax[1] & fts$rt > fts$rtmin[1], ]

fts_1 <- fts_1[order(fts_1$mz), ]

plot(fts_1$mz, type = "p")













ms$plot_groups(groups = "M200_R988_7")


View(ms$get_features()[11945])


ms$get_features()


res <- rcpp_ms_fill_features(
  ms$analyses$analyses[1:3],
  ms$get_features(analyses = 1:3),
  withinReplicate = FALSE,
  rtExpand = 1,
  mzExpand = 0.0005,
  minTracesIntensity = 500,
  minNumberTraces = 5,
  baseCut  = 0.3,
  minSignalToNoiseRatio = 5,
  minGaussianFit = 0.2
)

res <- lapply(res, function(x) data.table::rbindlist(x, fill = TRUE))
res <- data.table::rbindlist(res)


res <- lapply(res, function(x) {
  dg <- x$group[duplicated(x$group)]
  x[x$group %in% dg, ]
})

ms$nts$feature_list

data <- res$`03_tof_ww_is_pos_o3sw_effluent-r003`$M404_R1022_2862_03_tof_ww_is_pos_o3sw_effluent
plot(data$rt, data$intensity, main = "Gaussian Fit with Symmetric Data Trimming", xlab = "x", ylab = "y (Intensity)", pch = 19, col = "blue", cex = 1.2)
lines(data$rt, data$fit, col = "red", lwd = 2)

data <- res$`03_tof_ww_is_pos_o3sw_effluent-r003`$M441_R1338_3172_03_tof_ww_is_pos_o3sw_effluent
plot(data$rt, data$intensity, main = "Gaussian Fit with Symmetric Data Trimming", xlab = "x", ylab = "y (Intensity)", pch = 19, col = "blue", cex = 1.2)
lines(data$rt, data$fit, col = "red", lwd = 2)


plot(
  res$`03_tof_ww_is_pos_o3sw_effluent-r003`$M331_R1233_1901_03_tof_ww_is_pos_o3sw_effluent$intensity,
  type = "l",
  xlab = "N traces",
  ylab = "Intensity"
)

data <- res$`03_tof_ww_is_pos_o3sw_effluent-r003`$M331_R1233_1901_03_tof_ww_is_pos_o3sw_effluent
calculate_plot_gau_fit(data)

calculate_plot_gau_fit <- function(data) {
  
  if (nrow(data) < 5) return(cat("Not enough data points for fitting a Gaussian model."))
  
  x <- data$rt
  
  y <- data$intensity
  
  # Step 2: Find the index of the maximum y value
  max_idx <- which.max(y)
  x_max <- x[max_idx]
  
  # Step 3: Symmetrically trim the data around the x of the maximum y value
  n_points <- min(max_idx - 1, length(x) - max_idx) # Number of points to keep on each side
  x_trimmed <- x[(max_idx - n_points):(max_idx + n_points)]
  y_trimmed <- y[(max_idx - n_points):(max_idx + n_points)]
  
  if (length(x_trimmed) < 5) return(cat("Not enough data points for fitting a Gaussian model."))
  
  # Step 4: Initial guesses for Gaussian parameters
  start_params <- list(A = max(y_trimmed), mu = x_max, sigma = sd(x_trimmed))
  
  # Step 5: Fit the Gaussian model to the trimmed data using non-linear least squares
  gaussian_model <- nls(y_trimmed ~ A * exp(-((x_trimmed - mu)^2) / (2 * sigma^2)), 
                        start = start_params)
  
  # Step 6: Extract the fitted parameters
  fitted_params <- coef(gaussian_model)
  A_fitted <- fitted_params["A"]
  mu_fitted <- fitted_params["mu"]
  sigma_fitted <- fitted_params["sigma"]
  
  # Step 7: Generate fitted values based on the model
  x_fit <- seq(min(x_trimmed), max(x_trimmed), length = 100)
  y_fit <- A_fitted * exp(-((x_fit - mu_fitted)^2) / (2 * sigma_fitted^2))
  
  # Step 8: Calculate the R-squared score (goodness of fit)
  y_pred <- A_fitted * exp(-((x_trimmed - mu_fitted)^2) / (2 * sigma_fitted^2))
  ss_total <- sum((y_trimmed - mean(y_trimmed))^2) # Total sum of squares
  ss_residual <- sum((y_trimmed - y_pred)^2)      # Residual sum of squares
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Step 9: Plot the original data and the Gaussian fit
  plot(x_trimmed, y_trimmed, main = "Gaussian Fit with Symmetric Data Trimming", 
       xlab = "x", ylab = "y (Intensity)", pch = 19, col = "blue", cex = 1.2)
  lines(x_fit, y_fit, col = "red", lwd = 2)
  
  # Step 10: Display fitted parameters and R-squared
  cat("Fitted Amplitude (A):", A_fitted, "\n")
  cat("Fitted Mean (mu):", mu_fitted, "\n")
  cat("Fitted Standard Deviation (sigma):", sigma_fitted, "\n")
  cat("R-squared (Goodness of fit):", r_squared, "\n")
  
  
  
  
  # # Step 2: Calculate mean and standard deviation
  # mean_data <- mean(data)
  # sd_data <- sd(data)
  # 
  # # Step 3: Create Gaussian function to predict values
  # gaussian <- function(x) {
  #   (1 / (sd_data * sqrt(2 * pi))) * exp(-((x - mean_data)^2) / (2 * sd_data^2))
  # }
  # 
  # # Step 4: Generate sequence for fitting curve
  # x_fit <- seq(min(data), max(data), length = length(data))
  # 
  # # Step 5: Plot histogram of data and Gaussian fit
  # plot(seq_along(data), data, type = "l", xlab = "N traces", ylab = "Intensity")
  # hist_values <- hist(data, breaks = 10, probability = TRUE, col = "lightblue",
  #                     xlab = "Data", main = "Histogram with Gaussian Fit")
  # 
  # # plot(seq_along(hist_values$density), hist_values$density, col = "red", lwd = 2)
  # 
  # # Overlay the Gaussian fit curve
  # lines(x_fit, gaussian(data), col = "red", lwd = 2)
  # 
  # # Step 6: Compute the fitted values (Gaussian curve at midpoints of histogram bins)
  # bin_centers <- hist_values$mids  # Midpoints of histogram bins
  # predicted <- gaussian(bin_centers)  # Gaussian fit values at bin centers
  # observed <- hist_values$density    # Actual histogram densities
  # 
  # # Step 7: Calculate R-squared (R²)
  # ss_res <- sum((observed - predicted)^2)  # Sum of squared residuals
  # ss_tot <- sum((observed - mean(observed))^2)  # Total sum of squares
  # r_squared <- 1 - (ss_res / ss_tot)
  # 
  # # Step 8: Calculate RMSE (Root Mean Squared Error)
  # rmse <- sqrt(mean((observed - predicted)^2))
  # 
  # # Display R-squared and RMSE
  # cat("R-squared: ", r_squared, "\n")
  # cat("RMSE: ", rmse, "\n")
  # 
  # # Optional: Display mean and standard deviation
  # cat("Mean: ", mean_data, "\n")
  # cat("Standard Deviation: ", sd_data, "\n")
}

























ms$map_features(mass = dbis[7, ], colorBy = "analyses")

# ms$get_features(mass = dbis[7, ])

ms$nts

View(ms$nts$compounds)

patRoon::plotSpectrum(ms$nts$compounds, 1, groupName = "M253_R1015_3567", ms$nts$mspl)

ms$get_features()

ms$get_groups()

raman_files <- StreamFindData::get_raman_file_paths()

ra <- RamanAnalyses(raman_files)


ra <- RamanEngine$new(analyses = raman_files)

ra$get_spectra()

StatisticAnalyses()

stat <- StatisticEngine$new()

is(stat$analyses)

ra$spectra


Spectra()



# S7 Trial -----
ms_files <- StreamFindData::get_ms_file_paths()
db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

## CoreEngine -----
core_file = paste0(getwd(), "/core.rds")

core <- CoreEngine$new()


show(core$audit_trail)

as.data.frame(core$audit_trail)

core$save(core_file)
core$load(core_file)
core <- CoreEngine$new(file = core_file)


a <- MassSpecAnalyses(ms_files[1:3])

a <- add(a, ms_files[4])
a <- remove(a, 4)

a[1] <- ms_files[4]

show(a)

a$spectra_tic


# dbis$mz <- dbis$mass + 1.00726
MassSpecTargets(mz = dbis, polarities = c("positive", "negative"))



ms <- MassSpecEngine$new(analyses = ms_files[1:3])
ms$get_spectra_ms2(analyses = c(1, 2), mass = dbsus[2:3, ])

ms$get_spectra(analyses = c(1, 2), mass = dbsus[c(2:3,9), ])



core <- load(core_file)

core$analyses

is(core)

.get_available_engines()


core@run()



core <- CoreEngine$new()


is(rcpp_parse_ms_analysis(ms_files[1]))

ms <- MassSpecEngine$new()
ms$analyses

ms$NTS


a <- MassSpecAnalyses()
a$replicates["a"]

View(a[[1]])



a <- MassSpecAnalyses(ms_files[1:3])


get_spectra(a, analyses = 1)

a <- remove(a, 2)


a[1] <- ms_files[4]

a$spectra_tic

ms <- MassSpecEngine$new(analyses = ms_files[19:21])
ms$run(MassSpecSettings_FindFeatures_openms())
ms$run(MassSpecSettings_AnnotateFeatures_StreamFind())
ms$run(MassSpecSettings_FindInternalStandards_StreamFind(database = dbis, ppm = 8, sec = 10))
ms$run(MassSpecSettings_GroupFeatures_openms())
ms$run(MassSpecSettings_FillFeatures_StreamFind())
ms$run(MassSpecSettings_FilterFeatures_StreamFind(excludeIsotopes = TRUE))
ms$run(MassSpecSettings_FilterFeatures_patRoon(absMinIntensity = 10000))
ms$NTS <- ms$NTS[ , ms$get_suspects(database = dbsus)$group]
ms$run(MassSpecSettings_LoadFeaturesMS1_StreamFind(filtered = FALSE))
ms$run(MassSpecSettings_LoadFeaturesMS2_StreamFind(filtered = FALSE))
ms$run(MassSpecSettings_LoadFeaturesEIC_StreamFind(filtered = FALSE))
ms$run(MassSpecSettings_CalculateFeaturesQuality_StreamFind())
ms$run(MassSpecSettings_LoadMSPeakLists_StreamFind())
ms$run(MassSpecSettings_GenerateFormulas_genform())
ms$run(MassSpecSettings_GenerateCompounds_metfrag())
ms$run(MassSpecSettings_SuspectScreening_StreamFind(database = dbsus, ppm = 15, sec = 30))

ms$get_suspects(database = dbsus[2, ], ppm = 15, sec = 30)

plot_spectra_bpc(ms$analyses, levels = 1, yLab = "Test")




ms$get_features()

View(ms$get_isotopes())

a <- ms$analyses[1]


a@results$NTS

View(ms$NTS@formulas["M239_R1157_184"])

ms$NTS$group_names

ms$NTS@compounds[1:2]

View(ms$NTS$formulas)

show(ms$NTS[1:2])

ms$get_features()

ms$plot_features(mass = dbis)
ms$get_features_eic(mass = dbis[3, ])
ms$get_features_ms1(mass = dbis[3, ])
ms$get_features_ms2(mass = dbis[3, ])
ms$get_groups(mass = dbis)
ms$get_groups_ms1(mass = dbis[3, ])
ms$get_groups_ms2(mass = dbis[3, ])

View(ms$get_MSPeakLists(useLoadedData = FALSE))



ms$NTS$filtered



ms$analyses$replicates <- c("blank", "rep1", "rep1")
ms$analyses@blanks <- c("blank", "blank", "blank")
ms$analyses$info

a <- add(a, ms_files[4])
a$replicates


ms$get_spectra_tic()










is(a, "StreamFind::Analyses")

a@info

NTS()

a <- MassSpecAnalyses()
a@spectra_headers

a <- MassSpecSettings_AnnotateFeatures_StreamFind()
b <- MassSpecSettings_BinSpectra_StreamFind()
w <- Workflow(settings = list(a, b))

show(w)

a$call
w <- w[[-1]]

show(w)
save(w)
read(Workflow(), "workflow.json")
w$length


show(Analyses(df))

df <- data.frame(a = c(1, 2), b = c(3, 4))
rownames(df) <- c("test1", "test2")

a <- StatisticAnalyses(analyses = df)
show(a)
a@names
a@length



export(a)

a@run <- "test"

run(a)
export(a)

as.ProcessingSettings(MassSpecSettings_AverageSpectra_StreamFind())


new("ProjectHeaders_S4", headers = list(name = "test", author = "test", date = Sys.time()))

a <- ProjectHeaders(name = "test", author = "test", date = Sys.time())

a$date


Workflow()


a <- ProcessingSettings()
b <- ProcessingSettings()
b$engine

b@run

c <- read(b, file = "settings.json")
show(c)

as.ProcessingSettings(MassSpecSettings_BinSpectra_StreamFind())

show(MassSpecSettings_BinSpectra_StreamFind())

w <- Workflow(settings = list(a))
w[[2]] <- b


save(Workflow(settings = list(a)))

read(Workflow(), "workflow.json")


w[1]
w$methods

ca <- ProjectHeaders(headers = list(name = "test", author = "test"))

ca@headers

is(ca)

a <- list(a = "test")

a$a

ca

## Core development

core <- CoreEngine$new()
core$save(paste0(getwd(), "/core.sqlite"))
core$load()

anas <- list(a = c(1, 2), b = c(3, 4))

core$add_analyses(anas)


core$add_settings()



core$workflow







core$workflow <- Workflow()

core$headers$description <- "Example project" 

core$run_app()

# S3 classes: ProjectHeaders, ProcessingSettings, Analysis
# phead <- ProjectHeaders()
# phead

# settings <- ProcessingSettings()
# settings

# MassSpecSettings_BinSpectra_StreamFind()
# 
# ms <- MassSpecEngine$new(files = StreamFindData::get_ms_file_paths()[1])
# ms$add_settings(MassSpecSettings_BinSpectra_StreamFind())
# ms$run_workflow()

# ana <- Analysis()
# ana
# r1 <- RamanEngine$new(files = StreamFindData::get_raman_file_paths())
# r1$get_number_analyses()
# r1$plot_spectra(colorBy = "replicates")
# r1$get_spectra()

ms_files <- StreamFindData::get_ms_file_paths()
ms_files <- ms_files[grepl("blank|influent|o3sw", ms_files)]
ms_files_df <- data.frame(
  "file" = ms_files,
  "replicate" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("in_neg", 3),
    rep("in_pos", 3),
    rep("out_neg", 3),
    rep("out_pos", 3)
  ),
  "blank" = c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3)
  )
)

db <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
cols <- c("name", "formula", "mass", "rt", "polarity", "fragments", "tag")
db <- db[, cols, with = FALSE]
dbis <- db[grepl("IS", db$tag), ]
dbsus <- db[!grepl("IS", db$tag), ]

ps <- list(
  
  MassSpecSettings_FindFeatures_openms(),
  
  MassSpecSettings_AnnotateFeatures_StreamFind(),
  
  MassSpecSettings_GroupFeatures_openms(),

  MassSpecSettings_FindInternalStandards_StreamFind(database = dbis, ppm = 8, sec = 10),

  MassSpecSettings_FilterFeatures_StreamFind(excludeIsotopes = TRUE),

  MassSpecSettings_FilterFeatures_patRoon(absMinIntensity = 5000, maxReplicateIntRSD = 30, blankThreshold = 10, absMinReplicateAbundance = 3),

  MassSpecSettings_LoadFeaturesEIC_StreamFind(rtExpand = 60, mzExpand = 0.0005),

  MassSpecSettings_CalculateQuality_StreamFind(),

  MassSpecSettings_FilterFeatures_StreamFind(minSnRatio = 5),

  MassSpecSettings_LoadFeaturesMS1_StreamFind(),

  MassSpecSettings_LoadFeaturesMS2_StreamFind(),

  MassSpecSettings_LoadMSPeakLists_StreamFind(), # Check patRoon function for issues with MSPeakLists!!!

  #MassSpecSettings_GenerateFormulas_genform(),

  #MassSpecSettings_GenerateCompounds_metfrag()

  MassSpecSettings_SuspectScreening_StreamFind(database = dbsus, ppm = 5, sec = 10)
)

ms <- MassSpecEngine$new(files = ms_files_df[10, ], settings = ps)

ms$run_workflow()





ms$save()

ms <- MassSpecEngine$new()

ms$import("EngineData.json")

ms$get_analyses()

is(ms, "CoreEngine")


ms$plot_spectra_bpc(interactive = F)

names(ms)

ms$run_workflow()

comp <- patRoon::as.data.table(ms$compounds)

View(comp[comp$group %in% ms$get_groups(mass = dbsus)$group, ])

View(ms$get_results("patRoon"))

ms$get_isotopes(
  analyses = 5,
  mass = db[db$name %in% c("Diclofenac", "Candesartan"), ],
  ppm = 5, sec = 10
)


ms$map_isotopes(analyses = 5, mass = db[c(3, 24)], legendNames = TRUE, filtered = TRUE)


# MS2Tox and MS2Quant



# normalize internal standards
fGroupsNorm <- patRoon::normInts(ms$featureGroups, featNorm = "tic", groupNorm = TRUE)
fGroupsNorm <- patRoon::normInts(ms$featureGroups, featNorm = "istd", standards = dbis, adduct = "[M+H]+", 
                                 ISTDRTWindow = 20, ISTDMZWindow = 200, minISTDs = 2)





ms$NTS@features[[1]]









# grs <- ms$get_groups(mass = db, filtered = F, sec = 10)
# 
# ms$remove_groups(groups = grs$group)
# 
# fts <- ms$features
# 
# fts <- patRoon::filter(fts, absMinIntensity = 50000)
# 
# ms$features <- fts





# ms$get_features(mass = dbis, filtered = TRUE)

# ms$features@features

# ms$filtered_features

ms$get_groups(mass = db, filtered = F, sec = 10)

ms$get_suspects()

ms$plot_internal_standards_qc()

ms$plot_spectra_eic(analyses = 13:15, mass = dbis[5, ])






View(ms$get_isotopes(
  analyses = 4,
  mass = db,
  ppm = 8, sec = 10
))

ms$map_isotopes(
  analyses = 4,
  mass = db,
  ppm = 8, sec = 10,
  filtered = TRUE,
  legendNames = TRUE
)



ms$filtered_features 



# ms$analysisInfo
# 
# ms$features
# 
# ms$featureGroups



# new_files <- StreamFindData::get_ms_file_paths()[1:3]
# 
# new_anas <- parse_MassSpecAnalysis(new_files)
# 
# ms$add_analyses(new_anas)
# 
# ms$add_files(new_files)







ms$filtered_features

ms$get_features(mass = dbis, filtered = TRUE)

ms$plot_internal_standards_qc()

# ms$has_features()
# 
# ms$has_groups()
# 
length(ms$features)

View(ms$get_results("patRoon"))


# 
# ms$featureGroups()
# 
# ms$get_feature_list()
# 
# ms$get_features(mass = dbsus)
# 
# ms$get_groups(mass = dbsus, average = TRUE, metadata = TRUE)
# 
# ms$get_features_eic(analyses = 4, mass = dbsus[4, ])
# 
# ms$get_features_ms1(analyses = 1, mass = dbis[1, ])
# 
# ms$get_spectra_ms2(analyses = 4, mass = dbsus)
# 
# ms$get_features_ms2(analyses = 4, mass = dbsus)
# 
# ms$get_groups_ms1(mass = dbis[1, ])
# 
# ms$get_groups_ms2(mass = dbis[1, ])
# 
# ms$get_isotopes(analyses = 1, features = ms$get_features(analyses = 1, mass = dbis[3, ]))
# 
# ms$get_suspects(analyses = 4, database = dbsus)
# 
ms$get_suspects(onGroups = TRUE)

ms$analysisInfo

ms$features

ms$featureGroups







ms$plot_spectra_bpc(colorBy = "replicates", levels = 1)



# patRoon::clearCache("all")
# patRoon::clearCache(c("annotate_features"))
# patRoon::clearCache(c("calculate_quality"))
# patRoon::clearCache(c("load_features_ms2"))
# patRoon::clearCache(c("load_features_ms1"))

ms$add_settings(ps)

ms$run_workflow()

ms$plot_suspects()

ms$plot_groups_overview(analyses = c(4:9, 13:18), groups = ms$get_suspects(), legendNames = TRUE, heights = c(0.25, 0.5, 0.25))














patRoon::clearCache("all")
msana <- parse_MassSpecAnalysis(StreamFindData::get_ms_file_paths()[1])[[1]]
class(msana)
sloop::s3_dispatch(print(msana))
ana


core <- CoreEngine$new()
core$add_settings(Settings_annotate_features_StreamFind())
core$add_analyses(Analysis())
core$get_analyses()










core$get_history()


uv <- UVEngine$new()
uv

r1 <- RamanEngine$new(files = StreamFindData::get_raman_file_paths())

r1

r1$add_replicate_names(c(rep("Sample", 11), rep("Blank", 11)))

r1$add_blank_names(rep("Blank", 22))

r1$get_spectra(analyses = 1)

r1$plot_spectra(colorBy = "replicates", interactive = TRUE)

# plot(r1$get_spectra(analyses = 1)[, 2:3], type = "l")
# 
# plot(r1$get_spectra(2)[1:200, 2:3])
# 
# View(r1$get_analyses())
