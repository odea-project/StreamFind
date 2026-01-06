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
                                   float base_quantile = 0.10,
                                   float debug_mz = 0.0) {
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
    base_quantile,
    debug_mz
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
                                      std::vector<float> rtWindow)
{
  nts::NTS_DATA nts_data(info, spectra_headers, feature_list);
  nts_data.create_components(rtWindow);
  return nts_data.features_as_list_of_dt();
};
