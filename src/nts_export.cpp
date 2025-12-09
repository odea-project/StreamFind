#include <vector>
#include <string>
#include <Rcpp.h>
#include "nts/nts.h"

// MARK: rcpp_nts_find_features2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_find_features2(Rcpp::List info,
                                   Rcpp::List spectra_headers,
                                   std::vector<float> rtWindowsMin,
                                   std::vector<float> rtWindowsMax,
                                   std::vector<int> resolution_profile,
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
    resolution_profile,
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
