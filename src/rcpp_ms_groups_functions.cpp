#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <Rcpp.h>

double round_to(double value, int decimals) {
  double factor = std::pow(10.0, decimals);
  return std::round(value * factor) / factor;
}

// Utility function to create an ordered index based on a vector
std::vector<int> order_index(const std::vector<float>& x) {
  int n = x.size();
  std::vector<int> idx(n);
  // for (int i = 0; i < n; i++) idx[i] = i;
  std::iota(idx.begin(), idx.end(), 0);
  
  std::sort(idx.begin(), idx.end(), [&](int i, int j) {
    return x[i] < x[j];
  });
  
  // double tol = 1e-9;
  // 
  // std::stable_sort(idx.begin(), idx.end(), [&](int i, int j) {
  //   // Compare with tolerance to handle precision issues
  //   return (std::fabs(x[i] - x[j]) > tol) ? (x[i] < x[j]) : (i < j);
  // });
  
  return idx;
}

// Function to compute mass clusters using sorted indices
std::vector<float> compute_mass_clusters(const std::vector<int>& mass_order,
                                          const std::vector<float>& mass,
                                          const std::vector<float>& massmax) {
  int n = mass_order.size();
  std::vector<float> massclust(n, 0.0);
  
  for (int i = 1; i < n; ++i) {
    int curr_idx = mass_order[i];
    int prev_idx = mass_order[i - 1];
    
    if (mass[curr_idx] < massmax[prev_idx]) {
      massclust[curr_idx] = massclust[prev_idx];
    } else {
      massclust[curr_idx] = massclust[prev_idx] + 1;
    }
  }
  return massclust;
}

// Function to compute RT clusters using sorted indices
std::vector<float> compute_rt_clusters(const std::vector<int>& rt_order,
                                       const std::vector<float>& rt,
                                       const float& rt_dev) {
  int n = rt_order.size();
  std::vector<float> rtclust(n, 0.0);
  
  float rtmax_i = rt[rt_order[0]] + rt_dev;
  
  for (int i = 1; i < n; ++i) {
    int curr_idx = rt_order[i];
    int prev_idx = rt_order[i - 1];
    
    if (rt[curr_idx] < rtmax_i) {
      rtclust[curr_idx] = rtclust[prev_idx];
    } else {
      rtclust[curr_idx] = rtclust[prev_idx] + 1;
      rtmax_i = rt[curr_idx] + rt_dev;
    }
  }
  return rtclust;
}

// [[Rcpp::export]]
Rcpp::DataFrame rcpp_ms_group_features(Rcpp::DataFrame features, float rt_dev = 10, bool verbose = false) {
  
  std::vector<float> mass = features["mass"];
  const std::vector<float>& mz = features["mz"];
  const std::vector<float>& mzmin = features["mzmin"];
  const std::vector<float>& mzmax = features["mzmax"];
  std::vector<float> rt = features["rt"];
  const std::vector<float>& rtmax = features["rtmax"];
  const std::vector<float>& rtmin = features["rtmin"];
  const std::vector<float>& intensity = features["intensity"];
  
  int n = mass.size();

  std::vector<int> mass_order = order_index(mass);  
  
  std::vector<float> massmax(n);
  
  for (int i = 0; i < n; ++i) massmax[i] = mzmax[i] - mzmin[i] + mass[i];
  
  std::vector<float> massclust = compute_mass_clusters(mass_order, mass, massmax);
  
  std::vector<int> rt_order = order_index(rt);
  
  std::vector<float> rtclust = compute_rt_clusters(rt_order, rt, rt_dev);
  
  for (int i = 0; i < n; ++i) {
    massclust[i] += 1;
    rtclust[i] += 1;
  }
  
  // std::vector<float> rtmax2(n);
  // 
  // for (int i = 0; i < n; ++i) {
  //   rtmax2[i] = ((rtmax[i] - rtmin[i]) / 3) + rt[i];
  // }
  // 
  // std::vector<float> rtclust = compute_rt_clusters(rt_order, rt, rtmax2);
  // 
  // std::vector<double> new_mass(n);
  // std::vector<double> new_rt(n);
  // 
  // for (int i = 0; i < n; ++i) {
  //   new_mass[i] = mass[mass_order[i]];
  //   new_rt[i] = rt[rt_order[i]];
  // }
  // 
  std::vector<std::string> group2(n);
  for (int i = 0; i < n; ++i) {
    group2[i] = std::to_string((int)massclust[i]) + "_" + std::to_string((int)rtclust[i]);
  }
  
  // Return as a new dataframe with added columns
  return Rcpp::DataFrame::create(
    // Rcpp::Named("mass_order") = mass_order,
    // Rcpp::Named("mass_1") = new_mass,
    Rcpp::Named("mass") = mass,
    // Rcpp::Named("massmax_2") = massmax,
    Rcpp::Named("massclust") = massclust,
    // Rcpp::Named("rt_order") = rt_order,
    // Rcpp::Named("rt_1") = new_rt,
    Rcpp::Named("rt") = rt,
    // Rcpp::Named("rtmax_2") = rtmax2,
    Rcpp::Named("rtclust") = rtclust,
    Rcpp::Named("intensity") = intensity,
    Rcpp::Named("group2") = group2
  );
  
  // // Step 4: Create index sorted by retention time (rt)
  // std::vector<int> rt_order = order_index(rt);
  
  // // Step 5: Compute RT clusters
  // std::vector<float> rtclust = compute_rt_clusters(rt_order, rt, rtmax);
  
  // // Step 1: Create index sorted by mass
  // std::vector<int> mass_order = order_index(mass);
  
  // // Step 2: Compute mass difference and massdev
  // std::vector<float> massmax(n);
  // 
  // for (int i = 0; i < n; ++i) {
  //   massmax[i] = mzmax[i] - mzmin[i] + mass[i];
  // }
  
  // massdiff[mass_order[0]] = 0;  // First difference is 0
  // for (int i = 1; i < n; ++i) {
  //   massdiff[mass_order[i]] = mass[mass_order[i]] - mass[mass_order[i - 1]];
  // }
  
  // // Step 3: Compute mass clusters using the sorted mass order
  // std::vector<float> massclust = compute_mass_clusters(mass_order, mass, massmax);
  // 
  // // Step 6: Create final group2 (combining massclust and rtclust)
  // std::vector<std::string> group2(n);
  // for (int i = 0; i < n; ++i) {
  //   group2[i] = std::to_string((int)massclust[i]) + "_" + std::to_string((int)rtclust[i]);
  // }
  
  // Return the updated dataframe
  // return features;
  
  // const std::vector<std::string> features_cols = features.names();
  // 
  // const int n_features = features.nrows();
  // 
  // const int n_features_cols = features_cols.size();
  // 
  // if (n_features == 0 || n_features_cols == 0) {
  //   throw std::runtime_error("Features DataFrame is empty!");
  // }
  // 
  // const std::vector<std::string> must_have_names = {
  //   "feature", "analysis", "rt", "rtmax", "rtmin",
  //   "mass", "mz", "mzmax", "mzmin", "intensity"
  // };
  // 
  // const int n_must_have_names = must_have_names.size();
  // 
  // std::vector<bool> has_must_have_names(n_must_have_names, false);
  // 
  // for (size_t i = 0; i < must_have_names.size(); ++i) {
  //   for (size_t j = 0; j < features_cols.size(); ++j) {
  //     if (must_have_names[i] == features_cols[j]) has_must_have_names[i] = true;
  //   }
  // }
  // 
  // for (bool value : has_must_have_names) {
  //   if (!value) {
  //     throw std::runtime_error("Features DataFrame does not have all required columns!");
  //   }
  // }
  // 
  // std::vector<int> idx(n_features);
  // std::iota(all_idx.begin(), idx.end(), 0);
  // 
  // const std::vector<float>& mass = features["mass"];
  // const float* all_mass_ptr = mass.data();
  // 
  // const std::vector<float>& mzmin = features["mzmin"];
  // const float* all_mzmin_ptr = mzmin.data();
  // 
  // const std::vector<float>& mzmax = features["mzmax"];
  // const float* all_mzmax_ptr = mzmax.data();
  // 
  // const std::vector<float>& rt = features["rt"];
  // const float* all_rt_ptr = mzmax.data();
  // 
  // const std::vector<float>& rtmin = features["rtmin"];
  // const float* all_rtmin_ptr = rtmin.data();
  // 
  // const std::vector<float>& rtmax = features["rtmax"];
  // const float* all_rtmax_ptr = rtmax.data();
  // 
  // const std::vector<float>& intensity = features["intensity"];
  // const float* all_intensity_ptr = intensity.data();
  // 
  // std::vector<float> mass_dev(0, n_features);
  // 
  // for (int i = 0; i < n_features; i++) {
  //   mass_dev[i] = mzmax[i] - mzmin[i];
  // }
  // 
  // std::vector<float> mass_clust(0, n_features);
  // std::vector<float> rt_clust(0, n_features);
}

// [[Rcpp::export]]
bool rcpp_ms_groups_correspondence(Rcpp::DataFrame groups,
                                   Rcpp::DataFrame features,
                                   bool verbose) {
  
  bool valid = true;
  
  std::vector<std::string> features_cols = features.names();
  
  if (features.nrows() == 0 || features_cols.size() == 0) {
    throw std::runtime_error("Features DataFrame is empty!");
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
    
    if (*g_rt > *rtmax_val + 2 || *g_rt < *rtmin_val - 2) {
      Rcpp::Rcout << "\n !! The feature group " << *g_id << " (n " << i << ")"
                  << " does not match retention time range in features!! \n";
      
      valid = false;
    }
    
    auto massmin_val = std::min_element(massmin.begin(), massmin.end());
    auto massmax_val = std::max_element(massmax.begin(), massmax.end());
    
    if (*g_mass > *massmax_val + 0.0005 || *g_mass < *massmin_val - 0.0005) {
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
