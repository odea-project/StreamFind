#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>

// [[Rcpp::export]]
bool rcpp_ms_groups_correspondence(Rcpp::DataFrame groups,
                                   Rcpp::DataFrame features,
                                   bool verbose) {
  
  bool valid = true;
  
  std::vector<std::string> features_cols = features.names();
  
  if (features.nrows() == 0 || features_cols.size() == 0) {
    throw std::runtime_error("Groups DataFrame is empty!");
  }
  
  std::vector<std::string> must_have_names = {
    "feature", "analysis", "rt", "rtmax", "rtmin",
    "mz", "mzmax", "mzmin", "polarity", "intensity", "group"
  };
  
  std::vector<bool> has_must_have_names(11, false);
  
  for (size_t i = 0; i < must_have_names.size(); ++i) {
    for (size_t j = 0; j < features_cols.size(); ++j) {
      if (must_have_names[i] == features_cols[j]) has_must_have_names[i] = true;
    }
  }
  
  for (bool value : has_must_have_names) {
    if (!value) {
      throw std::runtime_error("Features DataFrame does not have all required columns!");
    }
  }
  
  std::vector<std::string> groups_cols = groups.names();
  
  if (groups.nrows() == 0 || groups_cols.size() == 0) {
    throw std::runtime_error("Groups DataFrame is empty!");
  }
  
  std::vector<std::string> must_have_names2 = {"group", "rt", "mass"};
  
  std::vector<bool> has_must_have_names2(3, false);
  
  for (size_t i = 0; i < must_have_names2.size(); ++i) {
    for (size_t j = 0; j < groups_cols.size(); ++j) {
      if (must_have_names2[i] == groups_cols[j]) has_must_have_names2[i] = true;
    }
  }
  
  for (bool value : has_must_have_names2) {
    if (!value) {
      throw std::runtime_error("Groups DataFrame does not have all required columns!");
    }
  }
  
  const std::vector<std::string>& all_feature_group = features["group"];
  
  const std::vector<std::string>& all_analysis = features["analysis"];
  const std::string* all_analysis_ptr = all_analysis.data();
  
  const std::vector<double>& all_rtmin = features["rtmin"];
  const double* all_rtmin_ptr = all_rtmin.data();
  
  const std::vector<double>& all_rtmax = features["rtmax"];
  const double* all_rtmax_ptr = all_rtmax.data();
  
  const std::vector<double>& all_mass = features["mass"];
  const double* all_mass_ptr = all_mass.data();
  
  const std::vector<double>& all_mz = features["mz"];
  const double* all_mz_ptr = all_mz.data();
  
  const std::vector<double>& all_mzmin = features["mzmin"];
  const double* all_mzmin_ptr = all_mzmin.data();
  
  const std::vector<double>& all_mzmax = features["mzmax"];
  const double* all_mzmax_ptr = all_mzmax.data();
  
  const std::vector<double>& all_intensity = features["intensity"];
  const double* all_intensity_ptr = all_intensity.data();
  
  std::vector<std::string> all_group = groups["group"];
  
  std::vector<double> all_group_rt = groups["rt"];
  
  std::vector<double> all_group_mass = groups["mass"];
  
  const int number_of_features = features.nrows();
  
  const int number_of_groups = groups.nrows();
  
  std::string* g_id;
  
  double* g_rt;
  
  double* g_mass;
  
  std::string* ana;

  for (int i=0; i<number_of_groups; ++i) {
    
    g_id = &all_group[i];
    
    g_rt = &all_group_rt[i];
    
    g_mass = &all_group_mass[i];
    
    std::vector<int> which_idx;
    
    for (int z=0; z<number_of_features; z++) {
      if (all_feature_group[z] == *g_id) which_idx.push_back(z);
    }
    
    const int n_idx = which_idx.size();
    
    std::vector<std::string> analysis(n_idx);
    std::string* analysis_ptr = analysis.data();
    
    
    std::vector<double> rtmin(n_idx);
    double* rtmin_ptr = rtmin.data();
    
    std::vector<double> rtmax(n_idx);
    double* rtmax_ptr = rtmax.data();
    
    std::vector<double> mass(n_idx);
    double* mass_ptr = mass.data();
    
    std::vector<double> massmin(n_idx);
    double* massmin_ptr = massmin.data();
    
    std::vector<double> massmax(n_idx);
    double* massmax_ptr = massmax.data();
    
    std::vector<double> intensity(n_idx);
    double* intensity_ptr = intensity.data();
    
    for (const int& x : which_idx) {
      *(analysis_ptr++) = *(all_analysis_ptr + x);
      *(rtmin_ptr++) = *(all_rtmin_ptr + x);
      *(rtmax_ptr++) = *(all_rtmax_ptr + x);
      *(mass_ptr++) = *(all_mass_ptr + x);
      *(massmin_ptr++) = *(all_mass_ptr + x) - (*(all_mz_ptr + x) - *(all_mzmin_ptr + x));
      *(massmax_ptr++) = *(all_mass_ptr + x) + (*(all_mzmax_ptr + x) - *(all_mz_ptr + x));
      *(intensity_ptr++) = *(all_intensity_ptr + x);
    }
    
   
    auto rtmin_val = std::min_element(rtmin.begin(), rtmin.end());
    auto rtmax_val = std::max_element(rtmax.begin(), rtmax.end());
    
    if (*g_rt > *rtmax_val || *g_rt < *rtmin_val) {
      Rcpp::Rcout << "\n !! The feature group " << *g_id << " (n " << i << ")"
                  << " does not match retention time range in features!! \n";
      
      valid = false;
    }
    
    auto massmin_val = std::min_element(massmin.begin(), massmin.end());
    auto massmax_val = std::max_element(massmax.begin(), massmax.end());
    
    if (*g_mass > *massmax_val || *g_mass < *massmin_val) {
      Rcpp::Rcout << "\n !! The feature group " << *g_id << " (n " << i << ")"
                  << " does not match mass range in features!! \n";
      
      valid = false;
    }

    for (int t = 0; t < n_idx; t++) {

      ana = &analysis[t];
      
      const std::vector<double>& g_intensity = groups[*ana];
      
      double g_int = g_intensity[i];
      
      g_int = std::round(g_int / 1) * 1;
      
      double f_int = intensity[t];
      
      f_int = std::round(f_int / 1) * 1;
      
      if (g_int != f_int) {
        Rcpp::Rcout << "\n !! The feature group " << *g_id << " (n " << i << ")"
                    << " does not match intensity in feature from analysis " << *ana << "! \n";
        
        valid = false;
      }
    }
  }
  
  return valid;
}
