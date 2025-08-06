
# A -----

#' @title S3 generic `add`
#' @description S3 generic to add data to `x`.
#' @param x An object to which data will be added.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
add <- function(x, ...) {
  UseMethod("add")
}

# B -----

# C -----

#' @title S3 generic `clear_cache`
#' @description S3 generic to clear the cache from `x`.
#' @param x An object from which the cache will be cleared.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
clear_cache <- function(x, ...) {
  UseMethod("clear_cache")
}

# D -----

# E -----

# F -----

# G -----

#' @title S3 generic `get_names`
#' @description S3 generic to get names from `x`.
#' @param x An object from which the names will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_names <- function(x, ...) {
  UseMethod("get_names")
}

#' @title S3 generic `get_blanks`
#' @description S3 generic to get blanks from `x`.
#' @param x An object from which the blanks will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_blanks <- function(x, ...) {
  UseMethod("get_blanks")
}

#' @title S3 generic `get_cache_info`
#' @description S3 generic to get information about the cache of `x`.
#' @param x An object from which the cache information will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_cache_info <- function(x, ...) {
  UseMethod("get_cache_info")
}

#' @title S3 generic `get_chromatograms`
#' @description S3 generic to get chromatograms from `x`.
#' @param x An object from which the chromatograms will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_chromatograms <- function(x, ...) {
  UseMethod("get_chromatograms")
}

#' @title S3 generic `get_chromatograms_peaks`
#' @description S3 generic to get chromatograms peaks from `x`.
#' @param x An object from which the chromatograms peaks will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_chromatograms_peaks <- function(x, ...) {
  UseMethod("get_chromatograms_peaks")
}

#' @title S3 generic `get_components`
#' @description S3 generic to get components from `x`.
#' @param x An object from which the components will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_components <- function(x, ...) {
  UseMethod("get_components")
}

#' @title S3 generic `get_compounds`
#' @description S3 generic to get compounds from `x`.
#' @param x An object from which the compound will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_compounds <- function(x, ...) {
  UseMethod("get_compounds")
}

#' @title S3 generic `get_concentrations`
#' @description S3 generic to get concentrations from `x`.
#' @param x An object from which the concentrations will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_concentrations <- function(x, ...) {
  UseMethod("get_concentrations")
}

#' @title S3 generic `get_features`
#' @description S3 generic to get features from `x`.
#' @param x An object from which the features will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_features <- function(x, ...) {
  UseMethod("get_features")
}

#' @title S3 generic `get_features_count`
#' @description S3 generic to get features count from `x`.
#' @param x An object from which the features count will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_features_count <- function(x, ...) {
  UseMethod("get_features_count")
}

#' @title S3 generic `get_features_eic`
#' @description S3 generic to get features EIC from `x`.
#' @param x An object from which the features EIC will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_features_eic <- function(x, ...) {
  UseMethod("get_features_eic")
}

#' @title S3 generic `get_features_ms1`
#' @description S3 generic to get features MS1 from `x`.
#' @param x An object from which the features MS1 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_features_ms1 <- function(x, ...) {
  UseMethod("get_features_ms1")
}

#' @title S3 generic `get_features_ms2`
#' @description S3 generic to get features MS2 from `x`.
#' @param x An object from which the features MS2 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_features_ms2 <- function(x, ...) {
  UseMethod("get_features_ms2")
}

#' @title S3 generic `get_fold_change`
#' @description S3 generic to get fold change from `x`.
#' @param x An object from which the fold change will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_fold_change <- function(x, ...) {
  UseMethod("get_fold_change")
}

#' @title S3 generic `get_formulas`
#' @description S3 generic to get formulas from `x`.
#' @param x An object from which the formulas will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_formulas <- function(x, ...) {
  UseMethod("get_formulas")
}

#' @title S3 generic `get_groups`
#' @description S3 generic to get groups from `x`.
#' @param x An object from which the groups will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_groups <- function(x, ...) {
  UseMethod("get_groups")
}

#' @title S3 generic `get_groups_ms1`
#' @description S3 generic to get groups MS1 from `x`.
#' @param x An object from which the groups MS1 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_groups_ms1 <- function(x, ...) {
  UseMethod("get_groups_ms1")
}

#' @title S3 generic `get_groups_ms2`
#' @description S3 generic to get groups MS2 from `x`.
#' @param x An object from which the groups MS2 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_groups_ms2 <- function(x, ...) {
  UseMethod("get_groups_ms2")
}

#' @title S3 generic `get_internal_standards`
#' @description S3 generic to get internal standards from `x`.
#' @param x An object from which the internal standards will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_internal_standards <- function(x, ...) {
  UseMethod("get_internal_standards")
}

#' @title S3 generic `get_matrix_suppression`
#' @description S3 generic to get matrix suppression from `x`.
#' @param x An object from which the matrix suppression will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_matrix_suppression <- function(x, ...) {
  UseMethod("get_matrix_suppression")
}

#' @title S3 generic `get_methods`
#' @description S3 generic to get methods from `x`.
#' @param x An object from which the methods will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_methods <- function(x, ...) {
  UseMethod("get_methods")
}

#' @title S3 generic `get_model_data`
#' @description S3 generic to get model data from `x`.
#' @param x An object from which the model data will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_model_data <- function(x, ...) {
  UseMethod("get_model_data")
}

#' @title S3 generic `get_model_prediction`
#' @description S3 generic to get model prediction from `x`.
#' @param x An object from which the model prediction will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_model_prediction <- function(x, ...) {
  UseMethod("get_model_prediction")
}

#' @title S3 generic `get_patRoon_compounds`
#' @description S3 generic to get [patRoon](https://github.com/rickhelmus/patRoon) compounds class from `x`.
#' @param x An object from which the patRoon compounds will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_patRoon_compounds <- function(x, ...) {
  UseMethod("get_patRoon_compounds")
}

#' @title Generic `get_patRoon_features`
#' @description S3 generic to get [patRoon](https://github.com/rickhelmus/patRoon) features class
#' from `x`.
#' @param x An object from which the patRoon features will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_patRoon_features <- function(x, ...) {
  UseMethod("get_patRoon_features")
}

#' @title Generic `get_patRoon_MSPeakLists`
#' @description S3 generic to get [patRoon](https://github.com/rickhelmus/patRoon) MSPeakLists
#' class from `x`.
#' @param x An object from which the patRoon MSPeakLists will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_patRoon_MSPeakLists <- function(x, ...) {
  UseMethod("get_patRoon_MSPeakLists")
}

#' @title Generic `get_raw_chromatograms`
#' @description S3 generic to get raw chromatograms from `x`.
#' @param x An object from which the raw chromatograms will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_raw_chromatograms <- function(x, ...) {
  UseMethod("get_raw_chromatograms")
}

#' @title Generic `get_raw_spectra`
#' @description S3 generic to get raw spectra from `x`.
#' @param x An object from which the raw spectra will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_raw_spectra <- function(x, ...) {
  UseMethod("get_raw_spectra")
}

#' @title Generic `get_replicates`
#' @description S3 generic to get replicates from `x`.
#' @param x An object from which the replicates will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_replicates <- function(x, ...) {
  UseMethod("get_replicates")
}

#' @title Generic `get_spectra`
#' @description S3 generic to get spectra from `x`.
#' @param x An object from which the spectra will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_spectra <- function(x, ...) {
  UseMethod("get_spectra")
}

#' @title Generic `get_spectra_bpc`
#' @description S3 generic to get spectra BPC from `x`.
#' @param x An object from which the spectra BPC will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#'
get_spectra_bpc <- function(x, ...) {
  UseMethod("get_spectra_bpc")
}

#' @title Generic `get_spectra_eic`
#' @description S3 generic to get spectra EIC from `x`.
#' @param x An object from which the spectra EIC will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_spectra_eic <- function(x, ...) {
  UseMethod("get_spectra_eic")
}

#' @title Generic `get_spectra_headers`
#' @description S3 generic to get spectra headers from `x`.
#' @param x An object from which the spectra headers will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_spectra_headers <- function(x, ...) {
  UseMethod("get_spectra_headers")
}

#' @title Generic `get_spectra_matrix`
#' @description S3 generic to get spectra matrix from `x`.
#' @param x An object from which the spectra matrix will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_spectra_matrix <- function(x, ...) {
  UseMethod("get_spectra_matrix")
}

#' @title Generic `get_spectra_ms1`
#' @description S3 generic to get spectra MS1 from `x`.
#' @param x An object from which the spectra MS1 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_spectra_ms1 <- function(x, ...) {
  UseMethod("get_spectra_ms1")
}

#' @title Generic `get_spectra_ms2`
#' @description S3 generic to get spectra MS2 from `x`.
#' @param x An object from which the spectra MS2 will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_spectra_ms2 <- function(x, ...) {
  UseMethod("get_spectra_ms2")
}

#' @title Generic `get_spectra_peaks`
#' @description S3 generic to get spectra peaks from `x`.
#' @param x An object from which the spectra peaks will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_spectra_peaks <- function(x, ...) {
  UseMethod("get_spectra_peaks")
}

#' @title Generic `get_spectra_tic`
#' @description S3 generic to get spectra TIC from `x`.
#' @param x An object from which the spectra TIC will be retrieved.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#'
get_spectra_tic <- function(x, ...) {
  UseMethod("get_spectra_tic")
}

#' @title Generic `get_suspects`
#' @description S3 generic to get suspects from `x`.
#' @param x An object from which the suspects will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
get_suspects <- function(x, ...) {
  UseMethod("get_suspects")
}

# H -----

# I -----

#' @title Generic `info`
#' @description S3 generic to get information from `x`.
#' @param x An object from which the information will be retrieved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
info <- function(x, ...) {
  UseMethod("info")
}

# J -----

# K -----

# L -----

#' @title Generic `load`
#' @description S3 generic to load data from `x`.
#' @param x An object from which the data will be loaded.
#' @param ... Additional arguments passed to the method.
#' @export
#'
load <- function(x, ...) {
  UseMethod("load")
}

#' @title Generic `load_cache`
#' @description S3 generic to load cache from `x`.
#' @param x An object from which the cache will be loaded.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
load_cache <- function(x, ...) {
  UseMethod("load_cache")
}

#' @title Generic `load_chromatograms`
#' @description S3 generic to load chromatograms from `x`.
#' @param x An object from which the chromatograms will be loaded.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
load_chromatograms <- function(x, ...) {
  UseMethod("load_chromatograms")
}

#' @title Generic `load_spectra`
#' @description S3 generic to load spectra from `x`.
#' @param x An object from which the spectra will be loaded.
#' @param ... Additional arguments passed to the method.
#' 
#' @export
#' 
load_spectra <- function(x, ...) {
  UseMethod("load_spectra")
}

# M -----

#' @title Generic `map_components`
#' @description S3 generic to map components from `x`.
#' @param x An object from which the components will be mapped.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
map_components <- function(x, ...) {
  UseMethod("map_components")
}

#' @title Generic `map_features`
#' @description S3 generic to map features from `x`.
#' @param x An object from which the features will be mapped.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
map_features <- function(x, ...) {
  UseMethod("map_features")
}

#' @title Generic `map_features_intensity`
#' @description S3 generic to map features intensity from `x`.
#' @param x An object from which the features intensity will be mapped.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
map_features_intensity <- function(x, ...) {
  UseMethod("map_features_intensity")
}

# N -----

# O -----

# P -----

#' @title Generic `plot_chromatograms`
#' @description S3 generic to plot chromatograms from `x`.
#' @param x An object from which the chromatograms will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_chromatograms <- function(x, ...) {
  UseMethod("plot_chromatograms")
}

#' @title Generic `plot_chromatograms_baseline`
#' @description S3 generic to plot chromatograms baseline from `x`.
#' @param x An object from which the chromatograms baseline will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_chromatograms_baseline <- function(x, ...) {
  UseMethod("plot_chromatograms_baseline")
}

#' @title Generic `plot_chromatograms_peaks`
#' @description S3 generic to plot chromatograms peaks from `x`.
#' @param x An object from which the chromatograms peaks will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_chromatograms_peaks <- function(x, ...) {
  UseMethod("plot_chromatograms_peaks")
}

#' @title Generic `plot_components`
#' @description S3 generic to plot components from `x`.
#' @param x An object from which the components will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_components <- function(x, ...) {
  UseMethod("plot_components")
}

#' @title Generic `plot_contributions`
#' @description S3 generic to plot contributions from `x`.
#' @param x An object from which the contributions will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_contributions <- function(x, ...) {
  UseMethod("plot_contributions")
}

#' @title Generic `plot_cumulative_explained_variance`
#' @description S3 generic to plot cumulative explained variance from `x`.
#' @param x An object from which the cumulative explained variance will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_cumulative_explained_variance <- function(x, ...) {
  UseMethod("plot_cumulative_explained_variance")
}

#' @title Generic `plot_data`
#' @description S3 generic to plot data from `x`.
#' @param x An object from which the data will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_data <- function(x, ...) {
  UseMethod("plot_data")
}

#' @title Generic `plot_explained_variance`
#' @description S3 generic to plot explained variance from `x`.
#' @param x An object from which the explained variance will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_explained_variance <- function(x, ...) {
  UseMethod("plot_explained_variance")
}

#' @title Generic `plot_features`
#' @description S3 generic to plot features from `x`.
#' @param x An object from which the features will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_features <- function(x, ...) {
  UseMethod("plot_features")
}

#' @title Generic `plot_features_count`
#' @description S3 generic to plot features count from `x`.
#' @param x An object from which the features count will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_features_count <- function(x, ...) {
  UseMethod("plot_features_count")
}

#' @title Generic `plot_features_eic`
#' @description S3 generic to plot features EIC from `x`.
#' @param x An object from which the features EIC will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_features_eic <- function(x, ...) {
  UseMethod("plot_features_eic")
}

#' @title Generic `plot_features_ms1`
#' @description S3 generic to plot features MS1 from `x`.
#' @param x An object from which the features MS1 will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_features_ms1 <- function(x, ...) {
  UseMethod("plot_features_ms1")
}

#' @title Generic `plot_features_ms2`
#' @description S3 generic to plot features MS2 from `x`.
#' @param x An object from which the features MS2 will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_features_ms2 <- function(x, ...) {
  UseMethod("plot_features_ms2")
}

#' @title Generic `plot_fold_change`
#' @description S3 generic to plot fold change from `x`.
#' @param x An object from which the fold change will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_fold_change <- function(x, ...) {
  UseMethod("plot_fold_change")
}

#' @title Generic `plot_groups`
#' @description S3 generic to plot groups from `x`.
#' @param x An object from which the groups will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_groups <- function(x, ...) {
  UseMethod("plot_groups")
}

#' @title Generic `plot_groups_ms1`
#' @description S3 generic to plot groups MS1 from `x`.
#' @param x An object from which the groups MS1 will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_groups_ms1 <- function(x, ...) {
  UseMethod("plot_groups_ms1")
}

#' @title Generic `plot_groups_ms2`
#' @description S3 generic to plot groups MS2 from `x`.
#' @param x An object from which the groups MS2 will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_groups_ms2 <- function(x, ...) {
  UseMethod("plot_groups_ms2")
}

#' @title Generic `plot_groups_overview`
#' @description S3 generic to plot groups overview from `x`.
#' @param x An object from which the groups overview will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_groups_overview <- function(x, ...) {
  UseMethod("plot_groups_overview")
}

#' @title Generic `plot_groups_profile`
#' @description S3 generic to plot groups profile from `x`.
#' @param x An object from which the groups profile will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_groups_profile <- function(x, ...) {
  UseMethod("plot_groups_profile")
}

#' @title Generic `plot_internal_standards`
#' @description S3 generic to plot internal standards from `x`.
#' @param x An object from which the internal standards will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_internal_standards <- function(x, ...) {
  UseMethod("plot_internal_standards")
}

#' @title Generic `plot_loadings`
#' @description S3 generic to plot loadings from `x`.
#' @param x An object from which the loadings will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_loadings <- function(x, ...) {
  UseMethod("plot_loadings")
}

#' @title Generic `plot_matrix_suppression`
#' @description S3 generic to plot matrix suppression from `x`.
#' @param x An object from which the matrix suppression will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_matrix_suppression <- function(x, ...) {
  UseMethod("plot_matrix_suppression")
}

#' @title Generic `plot_residuals`
#' @description S3 generic to plot residuals from `x`.
#' @param x An object from which the residuals will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_residuals <- function(x, ...) {
  UseMethod("plot_residuals")
}

#' @title Generic `plot_residual_distance`
#' @description S3 generic to plot residual distance from `x`.
#' @param x An object from which the residual distance will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_residual_distance <- function(x, ...) {
  UseMethod("plot_residual_distance")
}

#' @title Generic `plot_resolved_spectra`
#' @description S3 generic to plot resolved spectra from `x`.
#' @param x An object from which the resolved spectra will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_resolved_spectra <- function(x, ...) {
  UseMethod("plot_resolved_spectra")
}

#' @title Generic `plot_scores`
#' @description S3 generic to plot scores from `x`.
#' @param x An object from which the scores will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_scores <- function(x, ...) {
  UseMethod("plot_scores")
}

#' @title Generic `plot_spectra`
#' @description S3 generic to plot spectra from `x`.
#' @param x An object from which the spectra will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra <- function(x, ...) {
  UseMethod("plot_spectra")
}

#' @title Generic `plot_spectra_3d`
#' @description S3 generic to plot 3D spectra from `x`.
#' @param x An object from which the 3D spectra will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra_3d <- function(x, ...) {
  UseMethod("plot_spectra_3d")
}

#' @title Generic `plot_spectra_baseline`
#' @description S3 generic to plot spectra baseline from `x`.
#' @param x An object from which the spectra baseline will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra_baseline <- function(x, ...) {
  UseMethod("plot_spectra_baseline")
}

#' @title Generic `plot_spectra_bpc`
#' @description S3 generic to plot spectra BPC from `x`.
#' @param x An object from which the spectra BPC will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#'
plot_spectra_bpc <- function(x, ...) {
  UseMethod("plot_spectra_bpc")
}

#' @title Generic `plot_spectra_charges`
#' @description S3 generic to plot spectra charges from `x`.
#' @param x An object from which the spectra charges will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra_charges <- function(x, ...) {
  UseMethod("plot_spectra_charges")
}

#' @title Generic `plot_spectra_eic`
#' @description S3 generic to plot spectra EIC from `x`.
#' @param x An object from which the spectra EIC will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra_eic <- function(x, ...) {
  UseMethod("plot_spectra_eic")
}

#' @title Generic `plot_spectra_ms1`
#' @description S3 generic to plot spectra MS1 from `x`.
#' @param x An object from which the spectra MS1 will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra_ms1 <- function(x, ...) {
  UseMethod("plot_spectra_ms1")
}

#' @title Generic `plot_spectra_ms2`
#' @description S3 generic to plot spectra MS2 from `x`.
#' @param x An object from which the spectra MS2 will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra_ms2 <- function(x, ...) {
  UseMethod("plot_spectra_ms2")
}

#' @title Generic `plot_spectra_peaks`
#' @description S3 generic to plot spectra peaks from `x`.
#' @param x An object from which the spectra peaks will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra_peaks <- function(x, ...) {
  UseMethod("plot_spectra_peaks")
}

#' @title Generic `plot_spectra_tic`
#' @description S3 generic to plot spectra TIC from `x`.
#' @param x An object from which the spectra TIC will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#'
plot_spectra_tic <- function(x, ...) {
  UseMethod("plot_spectra_tic")
}

#' @title Generic `plot_spectra_xic`
#' @description S3 generic to plot spectra XIC from `x`.
#' @param x An object from which the spectra XIC will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_spectra_xic <- function(x, ...) {
  UseMethod("plot_spectra_xic")
}

#' @title Generic `plot_suspects`
#' @description S3 generic to plot suspects from `x`.
#' @param x An object from which the suspects will be plotted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
plot_suspects <- function(x, ...) {
  UseMethod("plot_suspects")
}

#' @title Generic `predict`
#' @description S3 generic to predict data from `x`.
#' @param x An object from which the data will be predicted.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
predict <- function(x, ...) {
  UseMethod("predict")
}

# Q -----

# R -----

#' @title Generic `read`
#' @description S3 generic to read data from `x`.
#' @param x An object from which the data will be read.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
read <- function(x, ...) {
  UseMethod("read")
}

#' @title Generic `remove`
#' @description S3 generic to remove data from `x`.
#' @param x An object from which the data will be removed.
#' @param ... Additional arguments passed to the method.
#' @export
#'
remove <- function(x, ...) {
  UseMethod("remove")
}

#' @title Generic `report`
#' @description S3 generic to generate a report from `x`.
#' @param x An object from which the report will be generated.
#' @param ... Additional arguments passed to the method.
#' @export
#'
report <- function(x, ...) {
  UseMethod("report")
}

#' @title Generic `run`
#' @description S3 generic to run a process on `x`.
#' @param x An object on which the process will be run.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
run <- function(x, ...) {
  UseMethod("run")
}

# S -----

#' @title S3 generic `save`
#' @description S3 generic to save data from `x`.
#' @param x An object from which the data will be saved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
save <- function(x, ...) {
  UseMethod("save")
}

#' @title Generic `save_cache`
#' @description S3 generic to save cache from `x`.
#' @param x An object from which the cache will be saved.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
save_cache <- function(x, ...) {
  UseMethod("save_cache")
}

#' @title Generic `set_blanks`
#' @description S3 generic to set blanks from `x`.
#' @param x An object in which the blanks will be set.
#' @param ... Additional arguments passed to the method.
#' @export 
#' 
set_blanks <- function(x, ...) {
  UseMethod("set_blanks")
}

#' @title Generic `set_concentrations`
#' @description S3 generic to set concentrations in `x`.
#' @param x An object in which the concentrations will be set.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
set_concentrations <- function(x, ...) {
  UseMethod("set_concentrations")
}

#' @title Generic `set_replicates`
#' @description S3 generic to set replicates in `x`.
#' @param x An object in which the replicates will be set.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
set_replicates <- function(x, ...) {
  UseMethod("set_replicates")
}

#' @title Generic `show`
#' @description S3 generic to show data from `x`.
#' @param x An object from which the data will be shown.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
show <- function(x, ...) {
  UseMethod("show")
}

#' @title Generic `size`
#' @description S3 generic to get the size of `x`.
#' @param x An object whose size will be determined.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
size <- function(x, ...) {
  UseMethod("size")
}

# T -----

#' @title Generic `test`
#' @description S3 generic to test data from `x`.
#' @param x An object from which the data will be tested.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
test <- function(x, ...) {
  UseMethod("test")
}

# U -----

# V -----

#' @title S3 generic `validate_object`
#' @description S3 generic to validate_object the object `x`.
#' @param x An object to be validated.
#' @param ... Additional arguments passed to the method.
#' @export
#' 
validate_object <- function(x, ...) {
  UseMethod("validate_object")
}

# W -----

# X -----

# Y -----

# Z -----

# App Generics -----

#' @noRd
.mod_WorkflowAssembler_Explorer_UI <- function(x, ...) {
  UseMethod(".mod_WorkflowAssembler_Explorer_UI")
}

#' @noRd
.mod_WorkflowAssembler_Explorer_Server <- function(x, ...) {
  UseMethod(".mod_WorkflowAssembler_Explorer_Server")
}

#' @noRd
.mod_WorkflowAssembler_Result_UI <- function(x, ...) {
  UseMethod(".mod_WorkflowAssembler_Result_UI")
}

#' @noRd
.mod_WorkflowAssembler_Result_Server <- function(x, ...) {
  UseMethod(".mod_WorkflowAssembler_Result_Server")
}
