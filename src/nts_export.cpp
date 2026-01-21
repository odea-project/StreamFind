#include <vector>
#include <Rcpp.h>
#include "nts/nts.h"

// MARK: rcpp_nts_find_features2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_find_features2(Rcpp::List info,
                                   Rcpp::List spectra_headers,
                                   std::vector<float> rtWindowsMin,
                                   std::vector<float> rtWindowsMax,
                                   float ppmThreshold = 15.0,
                                   float noiseThreshold = 15.0,
                                   float minSNR = 3.0,
                                   int minTraces = 3,
                                   float baselineWindow = 200.0,
                                   float maxWidth = 100.0,
                                   float baseQuantile = 0.10,
                                   std::string debugAnalysis = "",
                                   float debugMZ = 0.0,
                                   int debugSpecIdx = -1) {
  Rcpp::List features;
  nts::NTS_DATA nts_data(info, spectra_headers, features);
  nts_data.find_features(
    rtWindowsMin,
    rtWindowsMax,
    ppmThreshold,
    noiseThreshold,
    minSNR,
    minTraces,
    baselineWindow,
    maxWidth,
    baseQuantile,
    debugAnalysis,
    debugMZ,
    debugSpecIdx
  );
  return(nts_data.features_as_list_of_dt());
};

// MARK: rcpp_nts_load_features_ms1_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_load_features_ms1_2(Rcpp::List info,
                                        Rcpp::List spectra_headers,
                                        Rcpp::List feature_list,
                                        bool filtered,
                                        std::vector<float> rtWindow,
                                        std::vector<float> mzWindow,
                                        float minTracesIntensity,
                                        float mzClust,
                                        float presence)
{
  nts::NTS_DATA nts_data(info, spectra_headers, feature_list);
  nts_data.load_features_ms1(
      filtered,
      rtWindow,
      mzWindow,
      minTracesIntensity,
      mzClust,
      presence);
  return nts_data.features_as_list_of_dt();
};

// MARK: rcpp_nts_load_features_ms2_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_load_features_ms2_2(Rcpp::List info,
                                        Rcpp::List spectra_headers,
                                        Rcpp::List feature_list,
                                        bool filtered,
                                        float minTracesIntensity,
                                        float isolationWindow,
                                        float mzClust,
                                        float presence)
{
  nts::NTS_DATA nts_data(info, spectra_headers, feature_list);
  nts_data.load_features_ms2(
      filtered,
      minTracesIntensity,
      isolationWindow,
      mzClust,
      presence);
  return nts_data.features_as_list_of_dt();
};

// MARK: rcpp_nts_create_components
// [[Rcpp::export]]
Rcpp::List rcpp_nts_create_components(Rcpp::List info,
                                      Rcpp::List spectra_headers,
                                      Rcpp::List feature_list,
                                      std::vector<float> rtWindow,
                                      float minCorrelation = 0.8,
                                      float debugRT = 0.0,
                                      std::string debugAnalysis = "")
{
  nts::NTS_DATA nts_data(info, spectra_headers, feature_list);
  nts_data.create_components(rtWindow, minCorrelation, debugRT, debugAnalysis);
  return nts_data.features_as_list_of_dt();
};

// MARK: rcpp_nts_annotate_components
// [[Rcpp::export]]
Rcpp::List rcpp_nts_annotate_components(Rcpp::List info,
                                        Rcpp::List spectra_headers,
                                        Rcpp::List feature_list,
                                        int maxIsotopes = 5,
                                        int maxCharge = 1,
                                        int maxGaps = 1,
                                        float ppm = 10.0,
                                        std::string debugComponent = "",
                                        std::string debugAnalysis = "")
{
  nts::NTS_DATA nts_data(info, spectra_headers, feature_list);
  nts_data.annotate_components(maxIsotopes, maxCharge, maxGaps, ppm, debugComponent, debugAnalysis);
  return nts_data.features_as_list_of_dt();
};

// MARK: rcpp_nts_group_features_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_group_features_2(Rcpp::List info,
                                     Rcpp::List spectra_headers,
                                     Rcpp::List feature_list,
                                     std::string method = "obi_warp",
                                     Rcpp::List internal_standards_list = R_NilValue,
                                     float rtDeviation = 5.0,
                                     float ppm = 5.0,
                                     int minSamples = 1,
                                     float binSize = 5.0,
                                     bool debug = false,
                                     float debugRT = 0.0)
{
  nts::NTS_DATA nts_data(info, spectra_headers, feature_list);
  nts_data.group_features(method, internal_standards_list, rtDeviation, ppm, minSamples, binSize, debug, debugRT);
  return nts_data.features_as_list_of_dt();
};

// MARK: rcpp_nts_fill_features_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_fill_features_2(Rcpp::List info,
                                    Rcpp::List spectra_headers,
                                    Rcpp::List feature_list,
                                    bool withinReplicate = false,
                                    bool filtered = false,
                                    float rtExpand = 10.0,
                                    float mzExpand = 0.01,
                                    float maxPeakWidth = 30.0,
                                    float minTracesIntensity = 1000.0,
                                    int minNumberTraces = 5,
                                    float minIntensity = 5000.0,
                                    float rtApexDeviation = 5.0,
                                    float minSignalToNoiseRatio = 3.0,
                                    float minGaussianFit = 0.2,
                                    std::string debugFG = "")
{
  nts::NTS_DATA nts_data(info, spectra_headers, feature_list);
  nts_data.fill_features(
      withinReplicate,
      filtered,
      rtExpand,
      mzExpand,
      maxPeakWidth,
      minTracesIntensity,
      minNumberTraces,
      minIntensity,
      rtApexDeviation,
      minSignalToNoiseRatio,
      minGaussianFit,
      debugFG);
  return nts_data.features_as_list_of_dt();
};
