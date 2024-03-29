# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

rcpp_dev_parse_xml <- function(input) {
    .Call(`_StreamFind_rcpp_dev_parse_xml`, input)
}

rcpp_ms_annotation_isotopes <- function(features, maxIsotopes = 5L, elements = as.character( c("C","H", "N", "O", "S", "Cl", "Br")), mode = "small molecules", maxCharge = 1L, rtWindowAlignment = 0.3, maxGaps = 1L, maxCarbons = 80, maxHetero = 15, maxHalogens = 10, verbose = FALSE) {
    .Call(`_StreamFind_rcpp_ms_annotation_isotopes`, features, maxIsotopes, elements, mode, maxCharge, rtWindowAlignment, maxGaps, maxCarbons, maxHetero, maxHalogens, verbose)
}

rcpp_centroid_spectra_qCentroids <- function(spectra, maxScale = 5L, mode = 2L) {
    .Call(`_StreamFind_rcpp_centroid_spectra_qCentroids`, spectra, maxScale, mode)
}

rcpp_ms_cluster_spectra <- function(spectra, mzClust = 0.005, presence = 0.8, verbose = FALSE) {
    .Call(`_StreamFind_rcpp_ms_cluster_spectra`, spectra, mzClust, presence, verbose)
}

rcpp_ms_make_new_groups_id <- function(features, analyses, mzAsMass = TRUE) {
    .Call(`_StreamFind_rcpp_ms_make_new_groups_id`, features, analyses, mzAsMass)
}

rcpp_ms_groups_make_dataframe <- function(features, analyses) {
    .Call(`_StreamFind_rcpp_ms_groups_make_dataframe`, features, analyses)
}

rcpp_ms_groups_correspondence <- function(groups, features, verbose) {
    .Call(`_StreamFind_rcpp_ms_groups_correspondence`, groups, features, verbose)
}

rcpp_parse_spectra_headers <- function(file_path) {
    .Call(`_StreamFind_rcpp_parse_spectra_headers`, file_path)
}

rcpp_parse_spectra <- function(file_path, index = NA_integer_) {
    .Call(`_StreamFind_rcpp_parse_spectra`, file_path, index)
}

rcpp_parse_chromatograms_headers <- function(file_path) {
    .Call(`_StreamFind_rcpp_parse_chromatograms_headers`, file_path)
}

rcpp_parse_chromatograms <- function(file_path, index = NA_integer_) {
    .Call(`_StreamFind_rcpp_parse_chromatograms`, file_path, index)
}

rcpp_parse_ms_analysis <- function(file_path) {
    .Call(`_StreamFind_rcpp_parse_ms_analysis`, file_path)
}

rcpp_parse_ms_analysis_spectra <- function(analysis, index = NA_integer_) {
    .Call(`_StreamFind_rcpp_parse_ms_analysis_spectra`, analysis, index)
}

rcpp_parse_ms_analysis_chromatograms <- function(analysis, index = NA_integer_) {
    .Call(`_StreamFind_rcpp_parse_ms_analysis_chromatograms`, analysis, index)
}

rcpp_parse_asc_file <- function(file_path) {
    .Call(`_StreamFind_rcpp_parse_asc_file`, file_path)
}

rcpp_write_asc_file <- function(file, metadata_list, spectra) {
    invisible(.Call(`_StreamFind_rcpp_write_asc_file`, file, metadata_list, spectra))
}

