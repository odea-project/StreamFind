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




// [[Rcpp::export]]
Rcpp::List rcpp_ms_make_new_groups_id(Rcpp::DataFrame features,
                                      Rcpp::CharacterVector analyses,
                                      bool mzAsMass = true) {
  
  Rcpp::List list_out;
  
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
      throw std::runtime_error("The DataFrame does not have all required columns!");
    }
  }
  
  const std::vector<std::string>& all_groups = features["group"];
  
  std::set<std::string> unique_groups_set;
  
  for (const std::string& g : all_groups) {
    if(!(g.empty() || g == "NA")) unique_groups_set.insert(g);
  }
  
  std::vector<std::string> unique_groups(unique_groups_set.begin(), unique_groups_set.end());
  
  std::sort(unique_groups.begin(), unique_groups.end());
  
  const std::vector<std::string>& all_analysis = features["analysis"];
  const std::string* all_analysis_ptr = all_analysis.data();
  
  std::set<std::string> unique_analysis_set;
  
  for (const std::string& a : all_analysis) {
    unique_analysis_set.insert(a);
  }
  
  std::vector<std::string> unique_analysis(unique_analysis_set.begin(), unique_analysis_set.end());
  
  const std::vector<double>& all_rt = features["rt"];
  const double* all_rt_ptr = all_rt.data();
  
  const std::vector<double>& all_rtmin = features["rtmin"];
  const double* all_rtmin_ptr = all_rtmin.data();
  
  const std::vector<double>& all_rtmax = features["rtmax"];
  const double* all_rtmax_ptr = all_rtmax.data();
  
  const std::vector<double>& all_mz = features["mz"];
  const double* all_mz_ptr = all_mz.data();
  
  const std::vector<double>& all_mzmin = features["mzmin"];
  const double* all_mzmin_ptr = all_mzmin.data();
  
  const std::vector<double>& all_mzmax = features["mzmax"];
  const double* all_mzmax_ptr = all_mzmax.data();
  
  const std::vector<int>& all_polarity = features["polarity"];
  const int* all_polarity_ptr = all_polarity.data();
  
  const int number_of_groups = unique_groups.size();
  
  const int number_of_features = features.nrows();
  
  int number_of_analysis = unique_analysis.size();
  
  int number_of_analyses_arg = analyses.size();
  
  if (number_of_analysis < number_of_analyses_arg) {
    number_of_analysis = number_of_analyses_arg;
    unique_analysis = Rcpp::as<std::vector<std::string>>(analyses);
  }
  
  std::sort(unique_analysis.begin(), unique_analysis.end());
  
  Rcpp::NumericMatrix ints(number_of_groups, number_of_analysis);
  Rcpp::CharacterVector colnames_analysis(unique_analysis.begin(), unique_analysis.end());
  Rcpp::colnames(ints) = colnames_analysis;
  
  std::vector<double> g_rt(number_of_groups);
  std::vector<double> g_rtmin(number_of_groups);
  std::vector<double> g_rtmax(number_of_groups);
  std::vector<double> g_rtdev(number_of_groups);
  std::vector<double> g_mz(number_of_groups);
  std::vector<double> g_mass(number_of_groups);
  std::vector<double> g_massmin(number_of_groups);
  std::vector<double> g_massmax(number_of_groups);
  std::vector<double> g_massdev(number_of_groups);
  std::vector<int> g_polarity(number_of_groups);
  std::vector<std::string> g_group(number_of_groups);
  
  std::string* g_id;
  
  for (int i=0; i<number_of_groups; ++i) {
    
    g_id = &unique_groups[i];
    
    std::vector<int> which_idx;
    
    for (int z=0; z<number_of_features; z++) {
      if (all_groups[z] == *g_id) which_idx.push_back(z);
    }
    
    const int n_idx = which_idx.size();
    
    std::vector<std::string> analysis(n_idx);
    std::string* analysis_ptr = analysis.data();
    
    std::vector<double> rt(n_idx);
    double* rt_ptr = rt.data();
    
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
    
    for (const int& x : which_idx) {
      *(analysis_ptr++) = *(all_analysis_ptr + x);
      
      *(rt_ptr++) = *(all_rt_ptr + x);
      *(rtmin_ptr++) = *(all_rtmin_ptr + x);
      *(rtmax_ptr++) = *(all_rtmax_ptr + x);
      
      if (mzAsMass) {
        
        *(mass_ptr++) = *(all_mz_ptr + x);
        *(massmin_ptr++) = *(all_mzmin_ptr + x);
        *(massmax_ptr++) = *(all_mzmax_ptr + x);
        
      } else {
        
        double mass_val;
        
        if (*(all_polarity_ptr + x) == 1) {
          mass_val = *(all_mz_ptr + x) - 1.00726;
          
        } else if (*(all_polarity_ptr + x) == -1) {
          mass_val = *(all_mz_ptr + x) + 1.00726;
          
        } else {
          mass_val = *(all_mz_ptr + x);
        }
        
        *(mass_ptr++) = mass_val;
        
        *(massmin_ptr++) = mass_val - (*(all_mz_ptr + x) - *(all_mzmin_ptr + x));
        
        *(massmax_ptr++) = mass_val + (*(all_mzmax_ptr + x) - *(all_mz_ptr + x));
      }
    }
    
    std::set<std::string> analysis_present_set;
    
    for (const std::string& a : analysis) {
      analysis_present_set.insert(a);
    }
    
    std::vector<std::string> analysis_present(analysis_present_set.begin(), analysis_present_set.end());
    
    int n_analysis_present = analysis_present.size();
    
    if (n_idx > n_analysis_present) {
      Rcpp::Rcout << "\n !! The feature group " << *g_id << " (n " << i << ")"
                  << " has more features than analyses! \n";
    }
    
    double mean_rt = 0.0;
    
    for (double r : rt) mean_rt += r;
    
    g_rt[i] = mean_rt / n_idx;
    
    g_rt[i] = std::round(g_rt[i] / 0.001) * 0.001;
    
    auto rtmin_val = std::min_element(rtmin.begin(), rtmin.end());
    auto rtmax_val = std::max_element(rtmax.begin(), rtmax.end());
    
    if (g_rt[i] > *rtmax_val || g_rt[i] < *rtmin_val) {
      Rcpp::Rcout << "\n !! The feature group " << g_id << " (n " << i << ")"
                  << " does not match retention time range in features!! \n";
    }

    double mean_mass = 0.0;
    
    for (double m : mass) mean_mass += m;
    
    g_mass[i] = mean_mass / n_idx;
    
    g_mass[i] = std::round(g_mass[i] / 0.00000001) * 0.00000001;
    
    auto massmin_val = std::min_element(massmin.begin(), massmin.end());
    auto massmax_val = std::max_element(massmax.begin(), massmax.end());
    
    if (g_mass[i] > *massmax_val || g_mass[i] < *massmin_val) {
      Rcpp::Rcout << "\n !! The feature group " << g_id << " (n " << i << ")"
                  << " does not match mass range in features!! \n";
    }
    
    double rounded_mass = std::round(g_mass[i] / 0.01) * 0.01;
    
    std::ostringstream mass_ostr;
    
    mass_ostr << std::fixed << std::setprecision(3) << rounded_mass;
    
    std::string g_mass_to_id = mass_ostr.str();
    
    double rounded_rt = std::round(g_rt[i] / 1) * 1;
    
    std::ostringstream rt_ostr;
    
    rt_ostr << std::fixed << std::setprecision(0) << rounded_rt;
    
    std::string g_rt_to_id = rt_ostr.str();
    
    std::string i_str = std::to_string(i + 1);
    
    g_group[i] = "m";
    g_group[i] += g_mass_to_id;
    g_group[i] += "_rt";
    g_group[i] += g_rt_to_id;
    g_group[i] += "_g";
    g_group[i] += i_str;
  }
  
  Rcpp::List list_groups;
  
  list_groups["old_group"] = unique_groups;
  
  list_groups["group"] = g_group;
  
  list_groups.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  std::vector<int> rows(g_group.size());
  
  std::iota(rows.begin(), rows.end(), 1);
  
  list_groups.attr("row.names") = rows;
  
  return(list_groups);
}




// [[Rcpp::export]]
Rcpp::List rcpp_ms_groups_make_dataframe(Rcpp::DataFrame features,
                                         Rcpp::CharacterVector analyses) {

  Rcpp::List list_out;

  std::vector<std::string> features_cols = features.names();

  if (features.nrows() == 0 || features_cols.size() == 0) {
    throw std::runtime_error("Features DataFrame is empty!");
  }

  std::vector<std::string> must_have_names = {
    "feature", "analysis", "rt", "rtmax", "rtmin", "mass",
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
      throw std::runtime_error("The DataFrame does not have all required columns!");
    }
  }

  const std::vector<std::string>& all_groups = features["group"];
  
  std::set<std::string> unique_groups_set;
  
  for (const std::string& g : all_groups) {
    if(!(g.empty() || g == "NA")) unique_groups_set.insert(g);
  }
  
  std::vector<std::string> unique_groups(unique_groups_set.begin(), unique_groups_set.end());
  
  std::sort(unique_groups.begin(), unique_groups.end());
  
  // list_out["unique_groups"] = unique_groups;
  
  const std::vector<std::string>& all_analysis = features["analysis"];
  const std::string* all_analysis_ptr = all_analysis.data();
  
  std::set<std::string> unique_analysis_set;
  
  for (const std::string& a : all_analysis) {
    unique_analysis_set.insert(a);
  }
  
  std::vector<std::string> unique_analysis(unique_analysis_set.begin(), unique_analysis_set.end());
  
  // list_out["unique_analysis"] = unique_analysis;
 
  const std::vector<double>& all_rt = features["rt"];
  const double* all_rt_ptr = all_rt.data();
  
  const std::vector<double>& all_rtmin = features["rtmin"];
  const double* all_rtmin_ptr = all_rtmin.data();
  
  const std::vector<double>& all_rtmax = features["rtmax"];
  const double* all_rtmax_ptr = all_rtmax.data();
  
  const std::vector<double>& all_mass = features["mass"];
  const double* all_mass_ptr = all_mass.data();
  
  // const std::vector<double>& all_mz = features["mz"];
  // const double* all_mz_ptr = all_mz.data();
  
  const std::vector<double>& all_mzmin = features["mzmin"];
  const double* all_mzmin_ptr = all_mzmin.data();
  
  const std::vector<double>& all_mzmax = features["mzmax"];
  const double* all_mzmax_ptr = all_mzmax.data();
  
  const std::vector<double>& all_intensity = features["intensity"];
  const double* all_intensity_ptr = all_intensity.data();
  
  const int number_of_groups = unique_groups.size();
  
  const int number_of_features = features.nrows();
  
  int number_of_analysis = unique_analysis.size();
  
  int number_of_analyses_arg = analyses.size();

  if (number_of_analysis < number_of_analyses_arg) {
    number_of_analysis = number_of_analyses_arg;
    unique_analysis = Rcpp::as<std::vector<std::string>>(analyses);
  }
  
  std::sort(unique_analysis.begin(), unique_analysis.end());
  
  Rcpp::NumericMatrix ints(number_of_groups, number_of_analysis);
  Rcpp::CharacterVector colnames_analysis(unique_analysis.begin(), unique_analysis.end());
  Rcpp::colnames(ints) = colnames_analysis;
  
  std::vector<double> g_rt(number_of_groups);
  std::vector<double> g_rtmin(number_of_groups);
  std::vector<double> g_rtmax(number_of_groups);
  std::vector<double> g_rtdev(number_of_groups);
  std::vector<double> g_mz(number_of_groups);
  std::vector<double> g_mass(number_of_groups);
  std::vector<double> g_massmin(number_of_groups);
  std::vector<double> g_massmax(number_of_groups);
  std::vector<double> g_massdev(number_of_groups);
  std::vector<int> g_polarity(number_of_groups);
  std::vector<std::string> g_group(number_of_groups);

  std::string* g_id;

  for (int i=0; i<number_of_groups; ++i) {
    
    g_id = &unique_groups[i];
    
    std::vector<int> which_idx;
    
    for (int z=0; z<number_of_features; z++) {
      if (all_groups[z] == *g_id) which_idx.push_back(z);
    }
    
    const int n_idx = which_idx.size();
    
    std::vector<std::string> analysis(n_idx);
    std::string* analysis_ptr = analysis.data();
    
    std::vector<double> rt(n_idx);
    double* rt_ptr = rt.data();
    
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
      
      *(rt_ptr++) = *(all_rt_ptr + x);
      
      *(rtmin_ptr++) = *(all_rtmin_ptr + x);
      
      *(rtmax_ptr++) = *(all_rtmax_ptr + x);
      
      double mass_val;
      
      mass_val = *(all_mass_ptr + x);
      
      *(mass_ptr++) = mass_val;
      
      *(massmin_ptr++) = *(all_mass_ptr + x) - (*(all_mass_ptr + x) - *(all_mzmin_ptr + x));
      
      *(massmax_ptr++) = *(all_mass_ptr + x) + (*(all_mzmax_ptr + x) - *(all_mass_ptr + x));
      
      *(intensity_ptr++) = *(all_intensity_ptr + x);
    }

    // std::set<std::string> analysis_present_set;
    // 
    // for (const std::string& a : analysis) analysis_present_set.insert(a);
    // 
    // std::vector<std::string> analysis_present(analysis_present_set.begin(), analysis_present_set.end());
    // 
    // int n_analysis_present = analysis_present.size();
    
    double mean_rt = 0.0;
    
    for (double r : rt) mean_rt += r;
    
    g_rt[i] = mean_rt / n_idx;
    
    g_rt[i] = std::round(g_rt[i] / 0.001) * 0.001;
    
    auto rtmin_val = std::min_element(rtmin.begin(), rtmin.end());
    
    auto rtmax_val = std::max_element(rtmax.begin(), rtmax.end());
    
    g_rtdev[i] = *rtmax_val - *rtmin_val;
    
    g_rtdev[i] = std::round(g_rtdev[i] / 1) * 1;
    
    g_rtmin[i] = std::round(*rtmin_val / 0.001) * 0.001;

    g_rtmax[i] = std::round(*rtmax_val / 0.001) * 0.001;
    
    double mean_mass = 0.0;
    
    for (double m : mass) mean_mass += m;
    
    g_mass[i] = mean_mass / n_idx;
    
    g_mass[i] = std::round(g_mass[i] / 0.00000001) * 0.00000001;
    
    auto massmin_val = std::min_element(massmin.begin(), massmin.end());
    
    auto massmax_val = std::max_element(massmax.begin(), massmax.end());
    
    g_massdev[i] = (*massmax_val - *massmin_val) / *massmax_val * 1E6;
    
    g_massdev[i] = std::round(g_massdev[i] / 0.01) * 0.01;
    
    g_group[i] = unique_groups[i];
    
    for (int f = 0; f < n_idx; ++f) {
      for (int a=0; a < number_of_analysis; ++a) {
        if (colnames_analysis[a] == analysis[f]) {
          ints(i, a) = intensity[f];
        }
      }
    }
  }
  
  Rcpp::List list_groups;

  list_groups["group"] = g_group;
  
  list_groups["mass"] = g_mass;
  
  list_groups["rt"] = g_rt;
  
  for (int a = 0; a < number_of_analysis; ++a) {
    Rcpp::NumericVector int_vals = ints( Rcpp::_ , a );
    Rcpp::String ana = colnames_analysis[a];
    list_groups.push_back(int_vals, ana);
  }
  
  list_groups["massdev"] = g_massdev;
  
  list_groups["rtdev"] = g_rtdev;
  
  list_groups["rtmin"] = g_rtmin;
  
  list_groups["rtmax"] = g_rtmax;
  
  // std::vector<bool> filtered(number_of_groups, false);
  // 
  // list_groups["filtered"] = filtered;
  // 
  // SEXP na = R_NaString;
  // 
  // Rcpp::CharacterVector filter(number_of_groups, na);
  // 
  // list_groups["filter"] = filter;

  list_groups.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  std::vector<int> rows(g_group.size());
  
  std::iota(rows.begin(), rows.end(), 1);
  
  list_groups.attr("row.names") = rows;

  return(list_groups);
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
