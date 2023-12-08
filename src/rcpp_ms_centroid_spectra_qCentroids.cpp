#include <string>
#include <vector>
#include <Rcpp.h>

#include "qAlgorithms/src/qalgorithms_utils.cpp"
#include "qAlgorithms/src/qalgorithms_matrix.cpp"
#include "qAlgorithms/src/qalgorithms.cpp"

#include "qAlgorithms/include/qalgorithms_utils.h"
#include "qAlgorithms/include/qalgorithms_matrix.h"
#include "qAlgorithms/include/qalgorithms.h"

#include <chrono>


// [[Rcpp::export]]
Rcpp::List rcpp_centroid_spectra_qCentroids(Rcpp::DataFrame spectra) {
  
  std::vector<std::string> spectra_cols = spectra.names();
  
  Rcpp::List list_out;
  
  if (spectra.nrows() == 0 || spectra_cols.size() == 0) return list_out;
  
  std::vector<std::string> must_have_names = {"scan", "mz", "intensity"};
  
  std::vector<bool> has_must_have_names(3, false);
  
  for (size_t i = 0; i < must_have_names.size(); ++i) {
    for (size_t j = 0; j < spectra_cols.size(); ++j) {
      if (must_have_names[i] == spectra_cols[j]) has_must_have_names[i] = true;
    }
  }
  
  for (bool value : has_must_have_names) {
    if (!value) {
      throw std::runtime_error("Spectra DataFrame does not have all required columns!");
    }
  }

  const std::vector<int>& all_scan = spectra["scan"];
  const std::vector<double>& all_mz = spectra["mz"];
  const std::vector<double>& all_intensity = spectra["intensity"];
  
  int number_spectra = all_scan.size();

  // Rcpp::Rcout << std::endl;
  // Rcpp::Rcout << "Analyses with " << number_spectra << " spectra!" << std::endl;
  // Rcpp::Rcout << std::endl;
  
  q::Matrix xyData(number_spectra, 3);
  
  for (size_t i = 0; i < all_scan.size(); i++) {
    xyData(i,0) = all_mz[i];
    xyData(i,1) = all_intensity[i];
    xyData(i,2) = all_scan[i];
  }
  
  q::tValues tVal;
  
  q::Peakmodel model(2);
  
  model.loadTValues(tVal);
  
  model.addMultipleMeasurements(xyData);
  
  model.runRegression();
  
  list_out["scan"] = model.getPeakProperties(q::Peakproperties::SMPLID);
  list_out["mz"] = model.getPeakProperties(q::Peakproperties::POSITION);
  list_out["intensity"] = model.getPeakProperties(q::Peakproperties::HEIGHT);
  list_out["area"] = model.getPeakProperties(q::Peakproperties::AREA);
  list_out["width"] = model.getPeakProperties(q::Peakproperties::WIDTH);
  list_out["DQS"] = model.getPeakProperties(q::Peakproperties::DQS);
  list_out["SIGMAPOSITION"] = model.getPeakProperties(q::Peakproperties::SIGMAPOSITION);
  list_out["SIGMAHEIGHT"] = model.getPeakProperties(q::Peakproperties::SIGMAHEIGHT);
  list_out["SIGMAAREA"] = model.getPeakProperties(q::Peakproperties::SIGMAAREA);
  list_out["SIGMAWIDTH"] = model.getPeakProperties(q::Peakproperties::SIGMAWIDTH);
  list_out["COEFF_B0"] = model.getPeakProperties(q::Peakproperties::COEFF_B0);
  list_out["COEFF_B1"] = model.getPeakProperties(q::Peakproperties::COEFF_B1);
  list_out["COEFF_B2"] = model.getPeakProperties(q::Peakproperties::COEFF_B2);
  list_out["COEFF_B3"] = model.getPeakProperties(q::Peakproperties::COEFF_B3);
  
  
  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return list_out;
}
