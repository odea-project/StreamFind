// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_fill_bin_spectra
std::vector<double> rcpp_fill_bin_spectra(Rcpp::DataFrame spectra, Rcpp::DataFrame bin_mat, Rcpp::List bins, double overlap, std::string summaryFunction);
RcppExport SEXP _StreamFind_rcpp_fill_bin_spectra(SEXP spectraSEXP, SEXP bin_matSEXP, SEXP binsSEXP, SEXP overlapSEXP, SEXP summaryFunctionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type spectra(spectraSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type bin_mat(bin_matSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type bins(binsSEXP);
    Rcpp::traits::input_parameter< double >::type overlap(overlapSEXP);
    Rcpp::traits::input_parameter< std::string >::type summaryFunction(summaryFunctionSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_fill_bin_spectra(spectra, bin_mat, bins, overlap, summaryFunction));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_ms_cluster_spectra
Rcpp::List rcpp_ms_cluster_spectra(Rcpp::DataFrame spectra, double mzClust, double presence, bool verbose);
RcppExport SEXP _StreamFind_rcpp_ms_cluster_spectra(SEXP spectraSEXP, SEXP mzClustSEXP, SEXP presenceSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type spectra(spectraSEXP);
    Rcpp::traits::input_parameter< double >::type mzClust(mzClustSEXP);
    Rcpp::traits::input_parameter< double >::type presence(presenceSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_ms_cluster_spectra(spectra, mzClust, presence, verbose));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parse_ms_analysis
Rcpp::List rcpp_parse_ms_analysis(std::string file_path);
RcppExport SEXP _StreamFind_rcpp_parse_ms_analysis(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type file_path(file_pathSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parse_ms_analysis(file_path));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parse_ms_spectra_headers
Rcpp::List rcpp_parse_ms_spectra_headers(std::string file_path);
RcppExport SEXP _StreamFind_rcpp_parse_ms_spectra_headers(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type file_path(file_pathSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parse_ms_spectra_headers(file_path));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parse_ms_chromatograms_headers
Rcpp::List rcpp_parse_ms_chromatograms_headers(std::string file_path);
RcppExport SEXP _StreamFind_rcpp_parse_ms_chromatograms_headers(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type file_path(file_pathSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parse_ms_chromatograms_headers(file_path));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parse_ms_spectra
Rcpp::List rcpp_parse_ms_spectra(Rcpp::List analysis, std::vector<int> levels, Rcpp::DataFrame targets, float minIntensityMS1, float minIntensityMS2);
RcppExport SEXP _StreamFind_rcpp_parse_ms_spectra(SEXP analysisSEXP, SEXP levelsSEXP, SEXP targetsSEXP, SEXP minIntensityMS1SEXP, SEXP minIntensityMS2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type analysis(analysisSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type levels(levelsSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type targets(targetsSEXP);
    Rcpp::traits::input_parameter< float >::type minIntensityMS1(minIntensityMS1SEXP);
    Rcpp::traits::input_parameter< float >::type minIntensityMS2(minIntensityMS2SEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parse_ms_spectra(analysis, levels, targets, minIntensityMS1, minIntensityMS2));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parse_ms_chromatograms
Rcpp::List rcpp_parse_ms_chromatograms(Rcpp::List analysis, std::vector<int> idx);
RcppExport SEXP _StreamFind_rcpp_parse_ms_chromatograms(SEXP analysisSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type analysis(analysisSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type idx(idxSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parse_ms_chromatograms(analysis, idx));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_nts_load_features_eic
Rcpp::List rcpp_nts_load_features_eic(Rcpp::List info, Rcpp::List spectra_headers, Rcpp::List feature_list, bool filtered, float rtExpand, float mzExpand, float minTracesIntensity);
RcppExport SEXP _StreamFind_rcpp_nts_load_features_eic(SEXP infoSEXP, SEXP spectra_headersSEXP, SEXP feature_listSEXP, SEXP filteredSEXP, SEXP rtExpandSEXP, SEXP mzExpandSEXP, SEXP minTracesIntensitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type info(infoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type spectra_headers(spectra_headersSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type feature_list(feature_listSEXP);
    Rcpp::traits::input_parameter< bool >::type filtered(filteredSEXP);
    Rcpp::traits::input_parameter< float >::type rtExpand(rtExpandSEXP);
    Rcpp::traits::input_parameter< float >::type mzExpand(mzExpandSEXP);
    Rcpp::traits::input_parameter< float >::type minTracesIntensity(minTracesIntensitySEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_nts_load_features_eic(info, spectra_headers, feature_list, filtered, rtExpand, mzExpand, minTracesIntensity));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_ms_load_features_ms1
Rcpp::List rcpp_ms_load_features_ms1(std::vector<std::string> analyses_names, std::vector<std::string> analyses_files, Rcpp::List headers, Rcpp::List features, bool filtered, std::vector<float> rtWindow, std::vector<float> mzWindow, float minTracesIntensity, float mzClust, float presence);
RcppExport SEXP _StreamFind_rcpp_ms_load_features_ms1(SEXP analyses_namesSEXP, SEXP analyses_filesSEXP, SEXP headersSEXP, SEXP featuresSEXP, SEXP filteredSEXP, SEXP rtWindowSEXP, SEXP mzWindowSEXP, SEXP minTracesIntensitySEXP, SEXP mzClustSEXP, SEXP presenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type analyses_names(analyses_namesSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type analyses_files(analyses_filesSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type headers(headersSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type features(featuresSEXP);
    Rcpp::traits::input_parameter< bool >::type filtered(filteredSEXP);
    Rcpp::traits::input_parameter< std::vector<float> >::type rtWindow(rtWindowSEXP);
    Rcpp::traits::input_parameter< std::vector<float> >::type mzWindow(mzWindowSEXP);
    Rcpp::traits::input_parameter< float >::type minTracesIntensity(minTracesIntensitySEXP);
    Rcpp::traits::input_parameter< float >::type mzClust(mzClustSEXP);
    Rcpp::traits::input_parameter< float >::type presence(presenceSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_ms_load_features_ms1(analyses_names, analyses_files, headers, features, filtered, rtWindow, mzWindow, minTracesIntensity, mzClust, presence));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_nts_load_features_ms1
Rcpp::List rcpp_nts_load_features_ms1(Rcpp::List info, Rcpp::List spectra_headers, Rcpp::List feature_list, bool filtered, std::vector<float> rtWindow, std::vector<float> mzWindow, float minTracesIntensity, float mzClust, float presence);
RcppExport SEXP _StreamFind_rcpp_nts_load_features_ms1(SEXP infoSEXP, SEXP spectra_headersSEXP, SEXP feature_listSEXP, SEXP filteredSEXP, SEXP rtWindowSEXP, SEXP mzWindowSEXP, SEXP minTracesIntensitySEXP, SEXP mzClustSEXP, SEXP presenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type info(infoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type spectra_headers(spectra_headersSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type feature_list(feature_listSEXP);
    Rcpp::traits::input_parameter< bool >::type filtered(filteredSEXP);
    Rcpp::traits::input_parameter< std::vector<float> >::type rtWindow(rtWindowSEXP);
    Rcpp::traits::input_parameter< std::vector<float> >::type mzWindow(mzWindowSEXP);
    Rcpp::traits::input_parameter< float >::type minTracesIntensity(minTracesIntensitySEXP);
    Rcpp::traits::input_parameter< float >::type mzClust(mzClustSEXP);
    Rcpp::traits::input_parameter< float >::type presence(presenceSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_nts_load_features_ms1(info, spectra_headers, feature_list, filtered, rtWindow, mzWindow, minTracesIntensity, mzClust, presence));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_ms_load_features_ms2
Rcpp::List rcpp_ms_load_features_ms2(std::vector<std::string> analyses_names, std::vector<std::string> analyses_files, Rcpp::List headers, Rcpp::List features, bool filtered, float minTracesIntensity, float isolationWindow, float mzClust, float presence);
RcppExport SEXP _StreamFind_rcpp_ms_load_features_ms2(SEXP analyses_namesSEXP, SEXP analyses_filesSEXP, SEXP headersSEXP, SEXP featuresSEXP, SEXP filteredSEXP, SEXP minTracesIntensitySEXP, SEXP isolationWindowSEXP, SEXP mzClustSEXP, SEXP presenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type analyses_names(analyses_namesSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type analyses_files(analyses_filesSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type headers(headersSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type features(featuresSEXP);
    Rcpp::traits::input_parameter< bool >::type filtered(filteredSEXP);
    Rcpp::traits::input_parameter< float >::type minTracesIntensity(minTracesIntensitySEXP);
    Rcpp::traits::input_parameter< float >::type isolationWindow(isolationWindowSEXP);
    Rcpp::traits::input_parameter< float >::type mzClust(mzClustSEXP);
    Rcpp::traits::input_parameter< float >::type presence(presenceSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_ms_load_features_ms2(analyses_names, analyses_files, headers, features, filtered, minTracesIntensity, isolationWindow, mzClust, presence));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_nts_load_features_ms2
Rcpp::List rcpp_nts_load_features_ms2(Rcpp::List info, Rcpp::List spectra_headers, Rcpp::List feature_list, bool filtered, float minTracesIntensity, float isolationWindow, float mzClust, float presence);
RcppExport SEXP _StreamFind_rcpp_nts_load_features_ms2(SEXP infoSEXP, SEXP spectra_headersSEXP, SEXP feature_listSEXP, SEXP filteredSEXP, SEXP minTracesIntensitySEXP, SEXP isolationWindowSEXP, SEXP mzClustSEXP, SEXP presenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type info(infoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type spectra_headers(spectra_headersSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type feature_list(feature_listSEXP);
    Rcpp::traits::input_parameter< bool >::type filtered(filteredSEXP);
    Rcpp::traits::input_parameter< float >::type minTracesIntensity(minTracesIntensitySEXP);
    Rcpp::traits::input_parameter< float >::type isolationWindow(isolationWindowSEXP);
    Rcpp::traits::input_parameter< float >::type mzClust(mzClustSEXP);
    Rcpp::traits::input_parameter< float >::type presence(presenceSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_nts_load_features_ms2(info, spectra_headers, feature_list, filtered, minTracesIntensity, isolationWindow, mzClust, presence));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_nts_calculate_features_quality
Rcpp::List rcpp_nts_calculate_features_quality(Rcpp::List info, Rcpp::List spectra_headers, Rcpp::List feature_list, bool filtered, float rtExpand, float mzExpand, float minPeakWidth, float maxPeakWidth, float minTracesIntensity, float minNumberTraces, float baseCut);
RcppExport SEXP _StreamFind_rcpp_nts_calculate_features_quality(SEXP infoSEXP, SEXP spectra_headersSEXP, SEXP feature_listSEXP, SEXP filteredSEXP, SEXP rtExpandSEXP, SEXP mzExpandSEXP, SEXP minPeakWidthSEXP, SEXP maxPeakWidthSEXP, SEXP minTracesIntensitySEXP, SEXP minNumberTracesSEXP, SEXP baseCutSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type info(infoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type spectra_headers(spectra_headersSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type feature_list(feature_listSEXP);
    Rcpp::traits::input_parameter< bool >::type filtered(filteredSEXP);
    Rcpp::traits::input_parameter< float >::type rtExpand(rtExpandSEXP);
    Rcpp::traits::input_parameter< float >::type mzExpand(mzExpandSEXP);
    Rcpp::traits::input_parameter< float >::type minPeakWidth(minPeakWidthSEXP);
    Rcpp::traits::input_parameter< float >::type maxPeakWidth(maxPeakWidthSEXP);
    Rcpp::traits::input_parameter< float >::type minTracesIntensity(minTracesIntensitySEXP);
    Rcpp::traits::input_parameter< float >::type minNumberTraces(minNumberTracesSEXP);
    Rcpp::traits::input_parameter< float >::type baseCut(baseCutSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_nts_calculate_features_quality(info, spectra_headers, feature_list, filtered, rtExpand, mzExpand, minPeakWidth, maxPeakWidth, minTracesIntensity, minNumberTraces, baseCut));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_nts_annotate_features
Rcpp::List rcpp_nts_annotate_features(Rcpp::List feature_list, double rtWindowAlignment, int maxIsotopes, int maxCharge, int maxGaps);
RcppExport SEXP _StreamFind_rcpp_nts_annotate_features(SEXP feature_listSEXP, SEXP rtWindowAlignmentSEXP, SEXP maxIsotopesSEXP, SEXP maxChargeSEXP, SEXP maxGapsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type feature_list(feature_listSEXP);
    Rcpp::traits::input_parameter< double >::type rtWindowAlignment(rtWindowAlignmentSEXP);
    Rcpp::traits::input_parameter< int >::type maxIsotopes(maxIsotopesSEXP);
    Rcpp::traits::input_parameter< int >::type maxCharge(maxChargeSEXP);
    Rcpp::traits::input_parameter< int >::type maxGaps(maxGapsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_nts_annotate_features(feature_list, rtWindowAlignment, maxIsotopes, maxCharge, maxGaps));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_nts_fill_features
Rcpp::List rcpp_nts_fill_features(Rcpp::List info, Rcpp::List spectra_headers, Rcpp::List feature_list, bool withinReplicate, bool filtered, float rtExpand, float mzExpand, float minPeakWidth, float maxPeakWidth, float minTracesIntensity, float minNumberTraces, float minIntensity, float baseCut, float maxSearchWindow, float minSignalToNoiseRatio, float minGaussianFit);
RcppExport SEXP _StreamFind_rcpp_nts_fill_features(SEXP infoSEXP, SEXP spectra_headersSEXP, SEXP feature_listSEXP, SEXP withinReplicateSEXP, SEXP filteredSEXP, SEXP rtExpandSEXP, SEXP mzExpandSEXP, SEXP minPeakWidthSEXP, SEXP maxPeakWidthSEXP, SEXP minTracesIntensitySEXP, SEXP minNumberTracesSEXP, SEXP minIntensitySEXP, SEXP baseCutSEXP, SEXP maxSearchWindowSEXP, SEXP minSignalToNoiseRatioSEXP, SEXP minGaussianFitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type info(infoSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type spectra_headers(spectra_headersSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type feature_list(feature_listSEXP);
    Rcpp::traits::input_parameter< bool >::type withinReplicate(withinReplicateSEXP);
    Rcpp::traits::input_parameter< bool >::type filtered(filteredSEXP);
    Rcpp::traits::input_parameter< float >::type rtExpand(rtExpandSEXP);
    Rcpp::traits::input_parameter< float >::type mzExpand(mzExpandSEXP);
    Rcpp::traits::input_parameter< float >::type minPeakWidth(minPeakWidthSEXP);
    Rcpp::traits::input_parameter< float >::type maxPeakWidth(maxPeakWidthSEXP);
    Rcpp::traits::input_parameter< float >::type minTracesIntensity(minTracesIntensitySEXP);
    Rcpp::traits::input_parameter< float >::type minNumberTraces(minNumberTracesSEXP);
    Rcpp::traits::input_parameter< float >::type minIntensity(minIntensitySEXP);
    Rcpp::traits::input_parameter< float >::type baseCut(baseCutSEXP);
    Rcpp::traits::input_parameter< float >::type maxSearchWindow(maxSearchWindowSEXP);
    Rcpp::traits::input_parameter< float >::type minSignalToNoiseRatio(minSignalToNoiseRatioSEXP);
    Rcpp::traits::input_parameter< float >::type minGaussianFit(minGaussianFitSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_nts_fill_features(info, spectra_headers, feature_list, withinReplicate, filtered, rtExpand, mzExpand, minPeakWidth, maxPeakWidth, minTracesIntensity, minNumberTraces, minIntensity, baseCut, maxSearchWindow, minSignalToNoiseRatio, minGaussianFit));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_ms_group_features
Rcpp::DataFrame rcpp_ms_group_features(Rcpp::DataFrame features, float rt_dev, bool verbose);
RcppExport SEXP _StreamFind_rcpp_ms_group_features(SEXP featuresSEXP, SEXP rt_devSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type features(featuresSEXP);
    Rcpp::traits::input_parameter< float >::type rt_dev(rt_devSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_ms_group_features(features, rt_dev, verbose));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_ms_groups_correspondence
bool rcpp_ms_groups_correspondence(Rcpp::DataFrame groups, Rcpp::DataFrame features, bool verbose);
RcppExport SEXP _StreamFind_rcpp_ms_groups_correspondence(SEXP groupsSEXP, SEXP featuresSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type groups(groupsSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type features(featuresSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_ms_groups_correspondence(groups, features, verbose));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_parse_asc_file
Rcpp::List rcpp_parse_asc_file(std::string file_path);
RcppExport SEXP _StreamFind_rcpp_parse_asc_file(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type file_path(file_pathSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_parse_asc_file(file_path));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_write_asc_file
void rcpp_write_asc_file(const std::string& file, Rcpp::List metadata_list, Rcpp::NumericMatrix spectra);
RcppExport SEXP _StreamFind_rcpp_write_asc_file(SEXP fileSEXP, SEXP metadata_listSEXP, SEXP spectraSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type file(fileSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type metadata_list(metadata_listSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type spectra(spectraSEXP);
    rcpp_write_asc_file(file, metadata_list, spectra);
    return R_NilValue;
END_RCPP
}
// test_read_hdf5
Rcpp::List test_read_hdf5(const std::string& file_name);
RcppExport SEXP _StreamFind_test_read_hdf5(SEXP file_nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type file_name(file_nameSEXP);
    rcpp_result_gen = Rcpp::wrap(test_read_hdf5(file_name));
    return rcpp_result_gen;
END_RCPP
}
// test_create_hdf5
Rcpp::List test_create_hdf5();
RcppExport SEXP _StreamFind_test_create_hdf5() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(test_create_hdf5());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_StreamFind_rcpp_fill_bin_spectra", (DL_FUNC) &_StreamFind_rcpp_fill_bin_spectra, 5},
    {"_StreamFind_rcpp_ms_cluster_spectra", (DL_FUNC) &_StreamFind_rcpp_ms_cluster_spectra, 4},
    {"_StreamFind_rcpp_parse_ms_analysis", (DL_FUNC) &_StreamFind_rcpp_parse_ms_analysis, 1},
    {"_StreamFind_rcpp_parse_ms_spectra_headers", (DL_FUNC) &_StreamFind_rcpp_parse_ms_spectra_headers, 1},
    {"_StreamFind_rcpp_parse_ms_chromatograms_headers", (DL_FUNC) &_StreamFind_rcpp_parse_ms_chromatograms_headers, 1},
    {"_StreamFind_rcpp_parse_ms_spectra", (DL_FUNC) &_StreamFind_rcpp_parse_ms_spectra, 5},
    {"_StreamFind_rcpp_parse_ms_chromatograms", (DL_FUNC) &_StreamFind_rcpp_parse_ms_chromatograms, 2},
    {"_StreamFind_rcpp_nts_load_features_eic", (DL_FUNC) &_StreamFind_rcpp_nts_load_features_eic, 7},
    {"_StreamFind_rcpp_ms_load_features_ms1", (DL_FUNC) &_StreamFind_rcpp_ms_load_features_ms1, 10},
    {"_StreamFind_rcpp_nts_load_features_ms1", (DL_FUNC) &_StreamFind_rcpp_nts_load_features_ms1, 9},
    {"_StreamFind_rcpp_ms_load_features_ms2", (DL_FUNC) &_StreamFind_rcpp_ms_load_features_ms2, 9},
    {"_StreamFind_rcpp_nts_load_features_ms2", (DL_FUNC) &_StreamFind_rcpp_nts_load_features_ms2, 8},
    {"_StreamFind_rcpp_nts_calculate_features_quality", (DL_FUNC) &_StreamFind_rcpp_nts_calculate_features_quality, 11},
    {"_StreamFind_rcpp_nts_annotate_features", (DL_FUNC) &_StreamFind_rcpp_nts_annotate_features, 5},
    {"_StreamFind_rcpp_nts_fill_features", (DL_FUNC) &_StreamFind_rcpp_nts_fill_features, 16},
    {"_StreamFind_rcpp_ms_group_features", (DL_FUNC) &_StreamFind_rcpp_ms_group_features, 3},
    {"_StreamFind_rcpp_ms_groups_correspondence", (DL_FUNC) &_StreamFind_rcpp_ms_groups_correspondence, 3},
    {"_StreamFind_rcpp_parse_asc_file", (DL_FUNC) &_StreamFind_rcpp_parse_asc_file, 1},
    {"_StreamFind_rcpp_write_asc_file", (DL_FUNC) &_StreamFind_rcpp_write_asc_file, 3},
    {"_StreamFind_test_read_hdf5", (DL_FUNC) &_StreamFind_test_read_hdf5, 1},
    {"_StreamFind_test_create_hdf5", (DL_FUNC) &_StreamFind_test_create_hdf5, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_StreamFind(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
