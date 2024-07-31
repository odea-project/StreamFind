#' @title Validate
#' 
#' @description Validates an object.
#' 
#' @param x An object.
#' 
#' @export
#' 
validate <- function(x) {
  UseMethod("validate")
}

## Processing Methods -----

#' @noRd
.s3_AverageSpectra <- function(settings, self, private) {
  UseMethod(".s3_AverageSpectra")
}

#' @noRd
.s3_BinSpectra <- function(settings, self, private) {
  UseMethod(".s3_BinSpectra")
}

#' @noRd
.s3_ClusterSpectra <- function(settings, self, private) {
  UseMethod(".s3_ClusterSpectra")
}

#' @noRd
.s3_CorrectChromatogramsBaseline <- function(settings, self, private) {
  UseMethod(".s3_CorrectChromatogramsBaseline")
}

#' @noRd
.s3_CorrectSpectraBaseline <- function(settings, self, private) {
  UseMethod(".s3_CorrectSpectraBaseline")
}

#' @noRd
.s3_DeleteSpectraSection <- function(settings, self, private) {
  UseMethod(".s3_DeleteSpectraSection")
}

#' @noRd
.s3_IntegrateChromatograms <- function(settings, self, private) {
  UseMethod(".s3_IntegrateChromatograms")
}

#' @noRd
.s3_PrepareClassification <- function(settings, self, private) {
  UseMethod(".s3_PrepareClassification")
}

#' @noRd
.s3_MakeModel <- function(settings, self, private) {
  UseMethod(".s3_MakeModel")
}

#' @noRd
.s3_MergeSpectraTimeSeries <- function(settings, self, private) {
  UseMethod(".s3_MergeSpectraTimeSeries")
}

#' @noRd
.s3_AnnotateFeatures <- function(settings, self, private) {
  UseMethod(".s3_AnnotateFeatures")
}

#' @noRd
.s3_CalculateQuality <- function(settings, self, private) {
  UseMethod(".s3_CalculateQuality")
}

#' @noRd
.s3_CalculateSpectraCharges <- function(settings, self, private) {
  UseMethod(".s3_CalculateSpectraCharges")
}

#' @noRd
.s3_CentroidSpectra <- function(settings, self, private) {
  UseMethod(".s3_CentroidSpectra")
}

#' @noRd
.s3_DeconvoluteSpectra <- function(settings, self, private) {
  UseMethod(".s3_DeconvoluteSpectra")
}

#' @noRd
.s3_FillFeatures <- function(settings, self, private) {
  UseMethod(".s3_FillFeatures")
}

#' @noRd
.s3_FilterFeatures <- function(settings, self, private) {
  UseMethod(".s3_FilterFeatures")
}

#' @noRd
.s3_FindFeatures <- function(settings, self, private) {
  UseMethod(".s3_FindFeatures")
}

#' @noRd
.s3_FindInternalStandards <- function(settings, self, private) {
  UseMethod(".s3_FindInternalStandards")
}

#' @noRd
.s3_GenerateCompounds <- function(settings, self, private) {
  UseMethod(".s3_GenerateCompounds")
}

#' @noRd
.s3_GenerateFormulas <- function(settings, self, private) {
  UseMethod(".s3_GenerateFormulas")
}

#' @noRd
.s3_GroupFeatures <- function(settings, self, private) {
  UseMethod(".s3_GroupFeatures")
}

#' @noRd
.s3_LoadFeaturesEIC <- function(settings, self, private) {
  UseMethod(".s3_LoadFeaturesEIC")
}

#' @noRd
.s3_LoadFeaturesMS1 <- function(settings, self, private) {
  UseMethod(".s3_LoadFeaturesMS1")
}

#' @noRd
.s3_LoadFeaturesMS2 <- function(settings, self, private) {
  UseMethod(".s3_LoadFeaturesMS2")
}

#' @noRd
.s3_LoadMSPeakLists <- function(settings, self, private) {
  UseMethod(".s3_LoadMSPeakLists")
}

#' @noRd
.s3_NormalizeFeatures <- function(settings, self, private) {
  UseMethod(".s3_NormalizeFeatures")
}

#' @noRd
.s3_SuspectScreening <- function(settings, self, private) {
  UseMethod(".s3_SuspectScreening")
}

#' @noRd
.s3_NormalizeSpectra <- function(settings, self, private) {
  UseMethod(".s3_NormalizeSpectra")
}

#' @noRd
.s3_SmoothChromatograms <- function(settings, self, private) {
  UseMethod(".s3_SmoothChromatograms")
}

#' @noRd
.s3_SmoothSpectra <- function(settings, self, private) {
  UseMethod(".s3_SmoothSpectra")
}

#' @noRd
.s3_SubtractBlankSpectra <- function(settings, self, private) {
  UseMethod(".s3_SubtractBlankSpectra")
}

#' @noRd
.s3_SubtractSpectraSection <- function(settings, self, private) {
  UseMethod(".s3_SubtractSpectraSection")
}





















#' @noRd
.s3_average_spectra <- function(settings, self, private) {
  UseMethod(".s3_average_spectra")
}

#' @noRd
.s3_bin_spectra <- function(settings, self, private) {
  UseMethod(".s3_bin_spectra")
}

#' @noRd
.s3_cluster_spectra <- function(settings, self, private) {
  UseMethod(".s3_cluster_spectra")
}

#' @noRd
.s3_correct_chromatograms_baseline <- function(settings, self, private) {
  UseMethod(".s3_correct_chromatograms_baseline")
}

#' @noRd
.s3_correct_spectra_baseline <- function(settings, self, private) {
  UseMethod(".s3_correct_spectra_baseline")
}

#' @noRd
.s3_delete_spectra_section <- function(settings, self, private) {
  UseMethod(".s3_delete_spectra_section")
}

#' @noRd
.s3_integrate_chromatograms <- function(settings, self, private) {
  UseMethod(".s3_integrate_chromatograms")
}

#' @noRd
.s3_prepare_classification <- function(settings, self, private) {
  UseMethod(".s3_prepare_classification")
}

#' @noRd
.s3_make_model <- function(settings, self, private) {
  UseMethod(".s3_make_model")
}

#' @noRd
.s3_merge_spectra_time_series <- function(settings, self, private) {
  UseMethod(".s3_merge_spectra_time_series")
}

#' @noRd
.s3_ms_annotate_features <- function(settings, self, private) {
  UseMethod(".s3_ms_annotate_features")
}

#' @noRd
.s3_ms_calculate_quality <- function(settings, self, private) {
  UseMethod(".s3_ms_calculate_quality")
}

#' @noRd
.s3_ms_calculate_spectra_charges <- function(settings, self, private) {
  UseMethod(".s3_ms_calculate_spectra_charges")
}

#' @noRd
.s3_ms_centroid_spectra <- function(settings, self, private) {
  UseMethod(".s3_ms_centroid_spectra")
}

#' @noRd
.s3_ms_deconvolute_spectra <- function(settings, self, private) {
  UseMethod(".s3_ms_deconvolute_spectra")
}

.s3_ms_fill_features <- function(settings, self, private) {
  UseMethod(".s3_ms_fill_features")
}

#' @noRd
.s3_ms_filter_features <- function(settings, self, private) {
  UseMethod(".s3_ms_filter_features")
}

#' @noRd
.s3_ms_find_features <- function(settings, self, private) {
  UseMethod(".s3_ms_find_features")
}

#' @noRd
.s3_ms_find_internal_standards <- function(settings, self, private) {
  UseMethod(".s3_ms_find_internal_standards")
}

.s3_ms_generate_compounds <- function(settings, self, private) {
  UseMethod(".s3_ms_generate_compounds")
}

.s3_ms_generate_formulas <- function(settings, self, private) {
  UseMethod(".s3_ms_generate_formulas")
}

#' @noRd
.s3_ms_group_features <- function(settings, self, private) {
  UseMethod(".s3_ms_group_features")
}

#' @noRd
.s3_ms_load_features_eic <- function(settings, self, private) {
  UseMethod(".s3_ms_load_features_eic")
}

#' @noRd
.s3_ms_load_features_ms1 <- function(settings, self, private) {
  UseMethod(".s3_ms_load_features_ms1")
}

#' @noRd
.s3_ms_load_features_ms2 <- function(settings, self, private) {
  UseMethod(".s3_ms_load_features_ms2")
}

#' @noRd
.s3_ms_load_MSPeakLists <- function(settings, self, private) {
  UseMethod(".s3_ms_load_MSPeakLists")
}

#' @noRd
.s3_ms_normalize_features <- function(settings, self, private) {
  UseMethod(".s3_ms_normalize_features")
}

#' @noRd
.s3_ms_suspect_screening <- function(settings, self, private) {
  UseMethod(".s3_ms_suspect_screening")
}

#' @noRd
.s3_normalize_spectra <- function(settings, self, private) {
  UseMethod(".s3_normalize_spectra")
}

#' @noRd
.s3_smooth_chromatograms <- function(settings, self, private) {
  UseMethod(".s3_smooth_chromatograms")
}

#' @noRd
.s3_smooth_spectra <- function(settings, self, private) {
  UseMethod(".s3_smooth_spectra")
}

#' @noRd
.s3_subtract_blank_spectra <- function(settings, self, private) {
  UseMethod(".s3_subtract_blank_spectra")
}

#' @noRd
.s3_subtract_spectra_section <- function(settings, self, private) {
  UseMethod(".s3_subtract_spectra_section")
}

## Nor Used Yet -----

#' @noRd
.s3_ms_correct_intensity <- function(settings, self, private) {
  UseMethod(".s3_ms_correct_intensity")
}
