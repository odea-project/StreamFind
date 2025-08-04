
# A -----

#' @title Generic `add`
#' 
#' @description S7 generic to add data to `x`.
#' 
#' @param x An object to which data will be added.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
add <- S7::new_generic("add", "x")

# B -----

# C -----

#' @title Generic `clear_cache`
#' 
#' @description S7 generic to clear the cache from `x`.
#' 
#' @param x An object from which the cache will be cleared.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
clear_cache <- S7::new_generic("clear_cache", "x")

# D -----

# E -----

# F -----

# G -----

#' @title Generic `get_cache_info`
#' 
#' @description S3 generic to get information about the cache of `x`.
#' 
#' @param x An object from which the cache information will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_cache_info <- S7::new_generic("get_cache_info", "x")

#' @title Generic `get_chromatograms`
#' 
#' @description S7 generic to get chromatograms from `x`.
#' 
#' @param x An object from which the chromatograms will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_chromatograms <- S7::new_generic("get_chromatograms", "x")

#' @title Generic `get_chromatograms_peaks`
#' 
#' @description S7 generic to get chromatograms peaks from `x`.
#' 
#' @param x An object from which the chromatograms peaks will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_chromatograms_peaks <- S7::new_generic("get_chromatograms_peaks", "x")

#' @title Generic `get_components`
#' 
#' @description S7 generic to get components from `x`.
#' 
#' @param x An object from which the components will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_components <- S7::new_generic("get_components", "x")

#' @title Generic `get_compounds`
#' 
#' @description S7 generic to get compounds from `x`.
#' 
#' @param x An object from which the compounds will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_compounds <- S7::new_generic("get_compounds", "x")

#' @title Generic `get_features`
#' 
#' @description S7 generic to get features from `x`.
#' 
#' @param x An object from which the features will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_features <- S7::new_generic("get_features", "x")

#' @title Generic `get_features_count`
#' 
#' @description S7 generic to get features count from `x`.
#' 
#' @param x An object from which the features count will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_features_count <- S7::new_generic("get_features_count", "x")

#' @title Generic `get_features_eic`
#' 
#' @description S7 generic to get features EIC from `x`.
#' 
#' @param x An object from which the features EIC will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_features_eic <- S7::new_generic("get_features_eic", "x")

#' @title Generic `get_features_ms1`
#' 
#' @description S7 generic to get features MS1 from `x`.
#' 
#' @param x An object from which the features MS1 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_features_ms1 <- S7::new_generic("get_features_ms1", "x")

#' @title Generic `get_features_ms2`
#' 
#' @description S7 generic to get features MS2 from `x`.
#' 
#' @param x An object from which the features MS2 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_features_ms2 <- S7::new_generic("get_features_ms2", "x")

#' @title Generic `get_fold_change`
#' 
#' @description S7 generic to get fold change from `x`.
#' 
#' @param x An object from which the fold change will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_fold_change <- S7::new_generic("get_fold_change", "x")

#' @title Generic `get_formulas`
#' 
#' @description S7 generic to get formulas from `x`.
#' 
#' @param x An object from which the formulas will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_formulas <- S7::new_generic("get_formulas", "x")

#' @title Generic `get_groups`
#' 
#' @description S7 generic to get groups from `x`.
#' 
#' @param x An object from which the groups will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_groups <- S7::new_generic("get_groups", "x")

#' @title Generic `get_groups_ms1`
#' 
#' @description S7 generic to get groups MS1 from `x`.
#' 
#' @param x An object from which the groups MS1 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_groups_ms1 <- S7::new_generic("get_groups_ms1", "x")

#' @title Generic `get_groups_ms2`
#' 
#' @description S7 generic to get groups MS2 from `x`.
#' 
#' @param x An object from which the groups MS2 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_groups_ms2 <- S7::new_generic("get_groups_ms2", "x")

#' @title Generic `get_internal_standards`
#' 
#' @description S7 generic to get internal standards from `x`.
#' 
#' @param x An object from which the internal standards will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_internal_standards <- S7::new_generic("get_internal_standards", "x")

#' @title Generic `get_matrix_suppression`
#' 
#' @description S7 generic to get matrix suppression from `x`.
#' 
#' @param x An object from which the matrix suppression will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_matrix_suppression <- S7::new_generic("get_matrix_suppression", "x")

#' @title Generic `get_model_data`
#' 
#' @description S7 generic to get model data from `x`.
#' 
#' @param x An object from which the model data will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_model_data <- S7::new_generic("get_model_data", "x")

#' @title Generic `get_model_prediction`
#' 
#' @description S7 generic to get model prediction from `x`.
#' 
#' @param x An object from which the model prediction will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_model_prediction <- S7::new_generic("get_model_prediction", "x")

#' @title Generic `get_patRoon_compounds`
#' 
#' @description S7 generic to get [patRoon](https://github.com/rickhelmus/patRoon) compounds class from `x`.
#' 
#' @param x An object from which the patRoon compounds will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
get_patRoon_compounds <- S7::new_generic("get_patRoon_compounds", "x")

#' @title Generic `get_patRoon_features`
#' 
#' @description S7 generic to get [patRoon](https://github.com/rickhelmus/patRoon) features class from `x`.
#' 
#' @param x An object from which the patRoon features will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_patRoon_features <- S7::new_generic("get_patRoon_features", "x")

#' @title Generic `get_patRoon_MSPeakLists`
#' 
#' @description S7 generic to get [patRoon](https://github.com/rickhelmus/patRoon) MSPeakLists class from `x`.
#' 
#' @param x An object from which the patRoon MSPeakLists will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_patRoon_MSPeakLists <- S7::new_generic("get_patRoon_MSPeakLists", "x")

#' @title Generic `get_raw_chromatograms`
#' 
#' @description S7 generic to get raw chromatograms from `x`.
#' 
#' @param x An object from which the raw chromatograms will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_raw_chromatograms <- S7::new_generic("get_raw_chromatograms", "x")

#' @title Generic `get_raw_spectra`
#' 
#' @description S7 generic to get raw spectra from `x`.
#' 
#' @param x An object from which the raw spectra will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_raw_spectra <- S7::new_generic("get_raw_spectra", "x")

#' @title Generic `get_spectra`
#' 
#' @description S7 generic to get spectra from `x`.
#' 
#' @param x An object from which the spectra will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_spectra <- S7::new_generic("get_spectra", "x")

#' @title Generic `get_spectra_bpc`
#' 
#' @description S7 generic to get spectra BPC from `x`.
#' 
#' @param x An object from which the spectra BPC will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
get_spectra_bpc <- S7::new_generic("get_spectra_bpc", "x")

#' @title Generic `get_spectra_eic`
#' 
#' @description S7 generic to get spectra EIC from `x`.
#' 
#' @param x An object from which the spectra EIC will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_spectra_eic <- S7::new_generic("get_spectra_eic", "x")

#' @title Generic `get_spectra_matrix`
#' 
#' @description S7 generic to get spectra matrix from `x`.
#' 
#' @param x An object from which the spectra matrix will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_spectra_matrix <- S7::new_generic("get_spectra_matrix", "x")

#' @title Generic `get_spectra_ms1`
#' 
#' @description S7 generic to get spectra MS1 from `x`.
#' 
#' @param x An object from which the spectra MS1 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_spectra_ms1 <- S7::new_generic("get_spectra_ms1", "x")

#' @title Generic `get_spectra_ms2`
#' 
#' @description S7 generic to get spectra MS2 from `x`.
#' 
#' @param x An object from which the spectra MS2 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_spectra_ms2 <- S7::new_generic("get_spectra_ms2", "x")

#' @title Generic `get_spectra_peaks`
#' 
#' @description S7 generic to get spectra peaks from `x`.
#' 
#' @param x An object from which the spectra peaks will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_spectra_peaks <- S7::new_generic("get_spectra_peaks", "x")

#' @title Generic `get_spectra_tic`
#' 
#' @description S7 generic to get spectra TIC from `x`.
#' 
#' @param x An object from which the spectra TIC will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
get_spectra_tic <- S7::new_generic("get_spectra_tic", "x")

#' @title Generic `get_suspects`
#' 
#' @description S7 generic to get suspects from `x`.
#' 
#' @param x An object from which the suspects will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
get_suspects <- S7::new_generic("get_suspects", "x")

# H -----

# I -----

# J -----

# K -----

# L -----

#' @title Generic `load`
#' 
#' @description S7 generic to load data from `x`.
#' 
#' @param x An object from which the data will be loaded.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
load <- S7::new_generic("load", "x")

#' @title Generic `load_cache`
#' 
#' @description S7 generic to load cache from `x`.
#' 
#' @param x An object from which the cache will be loaded.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
load_cache <- S7::new_generic("load_cache", "x")

#' @title Generic `load_chromatograms`
#' 
#' @description S7 generic to load chromatograms from `x`.
#' 
#' @param x An object from which the chromatograms will be loaded.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
load_chromatograms <- S7::new_generic("load_chromatograms", "x")

#' @title Generic `load_spectra`
#' 
#' @description S7 generic to load spectra from `x`.
#' 
#' @param x An object from which the spectra will be loaded.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
load_spectra <- S7::new_generic("load_spectra", "x")

# M -----

#' @title Generic `map_components`
#' 
#' @description S7 generic to map components from `x`.
#' 
#' @param x An object from which the components will be mapped.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
map_components <- S7::new_generic("map_components", "x")

#' @title Generic `map_features`
#' 
#' @description S7 generic to map features from `x`.
#' 
#' @param x An object from which the features will be mapped.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
map_features <- S7::new_generic("map_features", "x")

#' @title Generic `map_features_intensity`
#' 
#' @description S7 generic to map features intensity from `x`.
#' 
#' @param x An object from which the features intensity will be mapped.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
map_features_intensity <- S7::new_generic("map_features_intensity", "x")

# N -----

# O -----

# P -----

#' @title Generic `plot_chromatograms`
#' 
#' @description S7 generic to plot chromatograms from `x`.
#' 
#' @param x An object from which the chromatograms will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_chromatograms <- S7::new_generic("plot_chromatograms", "x")

#' @title Generic `plot_chromatograms_baseline`
#' 
#' @description S7 generic to plot chromatograms baseline from `x`.
#' 
#' @param x An object from which the chromatograms baseline will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_chromatograms_baseline <- S7::new_generic("plot_chromatograms_baseline", "x")

#' @title Generic `plot_chromatograms_peaks`
#' 
#' @description S7 generic to plot chromatograms peaks from `x`.
#' 
#' @param x An object from which the chromatograms peaks will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_chromatograms_peaks <- S7::new_generic("plot_chromatograms_peaks", "x")

#' @title Generic `plot_components`
#' 
#' @description S7 generic to plot components from `x`.
#' 
#' @param x An object from which the components will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_components <- S7::new_generic("plot_components", "x")

#' @title Generic `plot_contributions`
#' 
#' @description S7 generic to plot contributions from `x`.
#' 
#' @param x An object from which the contributions will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_contributions <- S7::new_generic("plot_contributions", "x")

#' @title Generic `plot_cumulative_explained_variance`
#' 
#' @description S7 generic to plot cumulative explained variance from `x`.
#' 
#' @param x An object from which the cumulative explained variance will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_cumulative_explained_variance <- S7::new_generic("plot_cumulative_explained_variance", "x")

#' @title Generic `plot_data`
#' 
#' @description S7 generic to plot data from `x`.
#' 
#' @param x An object from which the data will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_data <- S7::new_generic("plot_data", "x")

#' @title Generic `plot_explained_variance`
#' 
#' @description S7 generic to plot explained variance from `x`.
#' 
#' @param x An object from which the explained variance will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_explained_variance <- S7::new_generic("plot_explained_variance", "x")

#' @title Generic `plot_features`
#' 
#' @description S7 generic to plot features from `x`.
#' 
#' @param x An object from which the features will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_features <- S7::new_generic("plot_features", "x")

#' @title Generic `plot_features_count`
#' 
#' @description S7 generic to plot features count from `x`.
#' 
#' @param x An object from which the features count will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_features_count <- S7::new_generic("plot_features_count", "x")

#' @title Generic `plot_features_eic`
#' 
#' @description S7 generic to plot features EIC from `x`.
#' 
#' @param x An object from which the features EIC will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_features_eic <- S7::new_generic("plot_features_eic", "x")

#' @title Generic `plot_features_ms1`
#' 
#' @description S7 generic to plot features MS1 from `x`.
#' 
#' @param x An object from which the features MS1 will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_features_ms1 <- S7::new_generic("plot_features_ms1", "x")

#' @title Generic `plot_features_ms2`
#' 
#' @description S7 generic to plot features MS2 from `x`.
#' 
#' @param x An object from which the features MS2 will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_features_ms2 <- S7::new_generic("plot_features_ms2", "x")

#' @title Generic `plot_fold_change`
#' 
#' @description S7 generic to plot fold change from `x`.
#' 
#' @param x An object from which the fold change will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_fold_change <- S7::new_generic("plot_fold_change", "x")

#' @title Generic `plot_groups`
#' 
#' @description S7 generic to plot groups from `x`.
#' 
#' @param x An object from which the groups will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_groups <- S7::new_generic("plot_groups", "x")

#' @title Generic `plot_groups_ms1`
#' 
#' @description S7 generic to plot groups MS1 from `x`.
#' 
#' @param x An object from which the groups MS1 will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_groups_ms1 <- S7::new_generic("plot_groups_ms1", "x")

#' @title Generic `plot_groups_ms2`
#' 
#' @description S7 generic to plot groups MS2 from `x`.
#' 
#' @param x An object from which the groups MS2 will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_groups_ms2 <- S7::new_generic("plot_groups_ms2", "x")

#' @title Generic `plot_groups_overview`
#' 
#' @description S7 generic to plot groups overview from `x`.
#' 
#' @param x An object from which the groups overview will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_groups_overview <- S7::new_generic("plot_groups_overview", "x")

#' @title Generic `plot_groups_profile`
#' 
#' @description S7 generic to plot groups profile from `x`.
#' 
#' @param x An object from which the groups profile will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_groups_profile <- S7::new_generic("plot_groups_profile", "x")

#' @title Generic `plot_internal_standards`
#' 
#' @description S7 generic to plot internal standards from `x`.
#' 
#' @param x An object from which the internal standards will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_internal_standards <- S7::new_generic("plot_internal_standards", "x")

#' @title Generic `plot_loadings`
#' 
#' @description S7 generic to plot loadings from `x`.
#' 
#' @param x An object from which the loadings will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_loadings <- S7::new_generic("plot_loadings", "x")

#' @title Generic `plot_matrix_suppression`
#' 
#' @description S7 generic to plot matrix suppression from `x`.
#' 
#' @param x An object from which the matrix suppression will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_matrix_suppression <- S7::new_generic("plot_matrix_suppression", "x")

#' @title Generic `plot_residuals`
#' 
#' @description S7 generic to plot residuals from `x`.
#' 
#' @param x An object from which the residuals will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_residuals <- S7::new_generic("plot_residuals", "x")

#' @title Generic `plot_residual_distance`
#' 
#' @description S7 generic to plot residual distance from `x`.
#' 
#' @param x An object from which the residual distance will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_residual_distance <- S7::new_generic("plot_residual_distance", "x")

#' @title Generic `plot_resolved_spectra`
#' 
#' @description S7 generic to plot resolved spectra from `x`.
#' 
#' @param x An object from which the resolved spectra will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_resolved_spectra <- S7::new_generic("plot_resolved_spectra", "x")

#' @title Generic `plot_scores`
#' 
#' @description S7 generic to plot scores from `x`.
#' 
#' @param x An object from which the scores will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_scores <- S7::new_generic("plot_scores", "x")

#' @title Generic `plot_spectra`
#' 
#' @description S7 generic to plot spectra from `x`.
#' 
#' @param x An object from which the spectra will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra <- S7::new_generic("plot_spectra", "x")

#' @title Generic `plot_spectra_3d`
#' 
#' @description S7 generic to plot 3D spectra from `x`.
#' 
#' @param x An object from which the 3D spectra will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra_3d <- S7::new_generic("plot_spectra_3d", "x")

#' @title Generic `plot_spectra_baseline`
#' 
#' @description S7 generic to plot spectra baseline from `x`.
#' 
#' @param x An object from which the spectra baseline will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra_baseline <- S7::new_generic("plot_spectra_baseline", "x")

#' @title Generic `plot_spectra_bpc`
#' 
#' @description S7 generic to plot spectra BPC from `x`.
#' 
#' @param x An object from which the spectra BPC will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
plot_spectra_bpc <- S7::new_generic("plot_spectra_bpc", "x")

#' @title Generic `plot_spectra_charges`
#' 
#' @description S7 generic to plot spectra charges from `x`.
#' 
#' @param x An object from which the spectra charges will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra_charges <- S7::new_generic("plot_spectra_charges", "x")

#' @title Generic `plot_spectra_eic`
#' 
#' @description S7 generic to plot spectra EIC from `x`.
#' 
#' @param x An object from which the spectra EIC will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra_eic <- S7::new_generic("plot_spectra_eic", "x")

#' @title Generic `plot_spectra_ms1`
#' 
#' @description S7 generic to plot spectra MS1 from `x`.
#' 
#' @param x An object from which the spectra MS1 will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra_ms1 <- S7::new_generic("plot_spectra_ms1", "x")

#' @title Generic `plot_spectra_ms2`
#' 
#' @description S7 generic to plot spectra MS2 from `x`.
#' 
#' @param x An object from which the spectra MS2 will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra_ms2 <- S7::new_generic("plot_spectra_ms2", "x")

#' @title Generic `plot_spectra_peaks`
#' 
#' @description S7 generic to plot spectra peaks from `x`.
#' 
#' @param x An object from which the spectra peaks will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra_peaks <- S7::new_generic("plot_spectra_peaks", "x")

#' @title Generic `plot_spectra_tic`
#' 
#' @description S7 generic to plot spectra TIC from `x`.
#' 
#' @param x An object from which the spectra TIC will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
plot_spectra_tic <- S7::new_generic("plot_spectra_tic", "x")

#' @title Generic `plot_spectra_xic`
#' 
#' @description S7 generic to plot spectra XIC from `x`.
#' 
#' @param x An object from which the spectra XIC will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_spectra_xic <- S7::new_generic("plot_spectra_xic", "x")

#' @title Generic `plot_suspects`
#' 
#' @description S7 generic to plot suspects from `x`.
#' 
#' @param x An object from which the suspects will be plotted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
plot_suspects <- S7::new_generic("plot_suspects", "x")

#' @title Generic `predict`
#' 
#' @description S7 generic to predict data from `x`.
#' 
#' @param x An object from which the data will be predicted.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
predict <- S7::new_generic("predict", "x")

# Q -----

# R -----

#' @title Generic `read`
#' 
#' @description S7 generic to read data from `x`.
#' 
#' @param x An object from which the data will be read.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
read <- S7::new_generic("read", "x")

#' @title Generic `remove`
#' 
#' @description S7 generic to remove data from `x`.
#' 
#' @param x An object from which the data will be removed.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
remove <- S7::new_generic("remove", "x")

#' @title Generic `report`
#' 
#' @description S7 generic to generate a report from `x`.
#' 
#' @param x An object from which the report will be generated.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
report <- S7::new_generic("report", "x")

#' @title Generic `run`
#' 
#' @description S7 generic to run a process on `x`.
#' 
#' @param x An object on which the process will be run.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
run <- S7::new_generic("run", "x")

# S -----

#' @title Generic `save`
#' 
#' @description S7 generic to save data from `x`.
#' 
#' @param x An object from which the data will be saved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
save <- S7::new_generic("save", "x")

#' @title Generic `save_cache`
#' 
#' @description S7 generic to save cache from `x`.
#' 
#' @param x An object from which the cache will be saved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
save_cache <- S7::new_generic("save_cache", "x")

# T -----

#' @title Generic `test`
#' 
#' @description S7 generic to test data from `x`.
#' 
#' @param x An object from which the data will be tested.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
test <- S7::new_generic("test", "x")

# U -----

# V -----

# W -----

# X -----

# Y -----

# Z -----

# Not Exported -----

#' @noRd
.mod_WorkflowAssembler_Explorer_UI <- S7::new_generic(".mod_WorkflowAssembler_Explorer_UI", "x")

#' @noRd
.mod_WorkflowAssembler_Explorer_Server <- S7::new_generic(".mod_WorkflowAssembler_Explorer_Server", "x")

#' @noRd
.mod_WorkflowAssembler_Result_UI <- S7::new_generic(".mod_WorkflowAssembler_Result_UI", "x")

#' @noRd
.mod_WorkflowAssembler_Result_Server <- S7::new_generic(".mod_WorkflowAssembler_Result_Server", "x")


