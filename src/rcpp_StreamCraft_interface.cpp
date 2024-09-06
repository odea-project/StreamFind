#include <vector>
#include <string>
#include <Rcpp.h>
#include <omp.h>
#include <unordered_map>
#include <cmath>
#include <algorithm>
#include <filesystem>

#define STREAMCRAFT_HEADER_ONLY
#include "StreamCraft/src/StreamCraft_lib.hpp"

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis(std::string file_path) {
  
  Rcpp::List list_out;
  
  Rcpp::CharacterVector na_charvec(1, NA_STRING);
  
  Rcpp::DataFrame empty_df;
  
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List empty_list;
  
  sc::MS_ANALYSIS ana(file_path);
  
  list_out["name"] = ana.file_name;
  
  list_out["replicate"] = na_charvec;
  
  list_out["blank"] = na_charvec;
  
  list_out["file"] = ana.file_path;
  
  list_out["format"] = ana.get_format();
  
  list_out["type"] = ana.get_type();
  
  list_out["spectra_number"] = ana.get_number_spectra();
  
  if (ana.get_number_spectra() > 0) {
    sc::MS_SPECTRA_HEADERS hd = ana.get_spectra_headers();
    
    Rcpp::List hdl;
    
    hdl["index"] = hd.index;
    hdl["scan"] = hd.scan;
    hdl["array_length"] = hd.array_length;
    hdl["level"] = hd.level;
    hdl["mode"] = hd.mode;
    hdl["polarity"] = hd.polarity;
    hdl["configuration"] = hd.configuration;
    hdl["lowmz"] = hd.lowmz;
    hdl["highmz"] = hd.highmz;
    hdl["bpmz"] = hd.bpmz;
    hdl["bpint"] = hd.bpint;
    hdl["tic"] = hd.tic;
    hdl["rt"] = hd.rt;
    hdl["mobility"] = hd.mobility;
    hdl["window_mz"] = hd.window_mz;
    hdl["pre_mzlow"] = hd.window_mzlow;
    hdl["pre_mzhigh"] = hd.window_mzhigh;
    hdl["pre_mz"] = hd.precursor_mz;
    hdl["pre_charge"] = hd.precursor_charge;
    hdl["pre_intensity"] = hd.precursor_intensity;
    hdl["pre_ce"] = hd.activation_ce;
    
    hdl.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    
    list_out["spectra_headers"] = hdl;
    
  } else {
    list_out["spectra_headers"] = empty_df;
  }
  
  list_out["spectra"] = empty_df;
  
  list_out["chromatograms_number"] = ana.get_number_chromatograms();
  
  if (ana.get_number_chromatograms() > 0) {
    
    sc::MS_CHROMATOGRAMS_HEADERS hd2 = ana.get_chromatograms_headers();
    
    Rcpp::List hdl2;
    
    hdl2["index"] = hd2.index;
    hdl2["id"] = hd2.id;
    hdl2["array_length"] = hd2.array_length;
    hdl2["polarity"] = hd2.polarity;
    hdl2["pre_mz"] = hd2.precursor_mz;
    hdl2["pro_mz"] = hd2.product_mz;
    hdl2["pre_ce"] = hd2.activation_ce;
    
    hdl2.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    
    list_out["chromatograms_headers"] = hdl2;
    
  } else {
    list_out["chromatograms_headers"] = empty_df;
  }
  
  list_out["chromatograms"] = empty_df;
  
  list_out["metadata"] = empty_list;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("MassSpecAnalysis", "Analysis");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra_headers(std::string file_path) {
  
  Rcpp::List list_out;
  
  sc::MS_ANALYSIS ana(file_path);
  
  if (ana.get_number_spectra() == 0) return list_out;
  
  sc::MS_SPECTRA_HEADERS headers = ana.get_spectra_headers();
  
  list_out["index"] = headers.index;
  list_out["scan"] = headers.scan;
  list_out["array_length"] = headers.array_length;
  list_out["level"] = headers.level;
  list_out["mode"] = headers.mode;
  list_out["polarity"] = headers.polarity;
  list_out["configuration"] = headers.configuration;
  list_out["lowmz"] = headers.lowmz;
  list_out["highmz"] = headers.highmz;
  list_out["bpmz"] = headers.bpmz;
  list_out["bpint"] = headers.bpint;
  list_out["tic"] = headers.tic;
  list_out["rt"] = headers.rt;
  list_out["mobility"] = headers.mobility;
  list_out["window_mz"] = headers.window_mz;
  list_out["pre_mzlow"] = headers.window_mzlow;
  list_out["pre_mzhigh"] = headers.window_mzhigh;
  list_out["pre_mz"] = headers.precursor_mz;
  list_out["pre_charge"] = headers.precursor_charge;
  list_out["pre_intensity"] = headers.precursor_intensity;
  list_out["pre_ce"] = headers.activation_ce;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_chromatograms_headers(std::string file_path) {
  
  Rcpp::List list_out;
  
  sc::MS_ANALYSIS ana(file_path);
  
  if (ana.get_number_chromatograms() == 0) return list_out;
  
  sc::MS_CHROMATOGRAMS_HEADERS headers = ana.get_chromatograms_headers();
  
  list_out["index"] = headers.index;
  list_out["id"] = headers.id;
  list_out["array_length"] = headers.array_length;
  list_out["polarity"] = headers.polarity;
  list_out["pre_mz"] = headers.precursor_mz;
  list_out["pro_mz"] = headers.product_mz;
  list_out["pre_ce"] = headers.activation_ce;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra(Rcpp::List analysis,
                                 std::vector<int> levels,
                                 Rcpp::DataFrame targets,
                                 float minIntensityMS1,
                                 float minIntensityMS2) {
  
  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List out;
  
  const std::string file = analysis["file"];
  
  const Rcpp::List& hd = analysis["spectra_headers"];
  
  const std::vector<float>& rt = hd["rt"];
  
  const float minIntLv1 = minIntensityMS1;
  const float minIntLv2 = minIntensityMS2;

  const int number_spectra = rt.size();
  
  const int number_levels = levels.size();
  
  if (number_spectra == 0) return empty_df;
  
  const int n_tg = targets.nrow();
  
  sc::MS_ANALYSIS ana(file);

  if (n_tg == 0) {
    
    const std::vector<int>& polarity = hd["polarity"];
    const std::vector<int>& configuration = hd["configuration"];
    const std::vector<int>& level = hd["level"];
    const std::vector<float>& pre_mz = hd["pre_mz"];
    const std::vector<float>& pre_ce = hd["pre_ce"];
    const std::vector<float>& mobility = hd["mobility"];
    
    std::vector<bool> spectra_filter(number_spectra, false);
    
    for (int i = 0; i < number_spectra; i++) {
      for (int j = 0; j < number_levels; j++) {
        
        if (configuration[i] >= 3) break;
        
        if (level[i] == levels[j]) {
          spectra_filter[i] = true;
          break;
        }
      }
    }
    
    std::vector<int> indices;
    
    for (int i = 0; i < number_spectra; i++) if (spectra_filter[i]) indices.push_back(i);
    
    const std::vector<std::vector<std::vector<float>>> spectra = ana.get_spectra(indices);
    
    const int number_spectra_extracted = spectra.size();
    
    if (number_spectra != number_spectra_extracted) return empty_df;
    
    const int number_arrays = spectra[0].size();
    
    if (number_arrays == 0) return empty_df;
    
    int total_traces = 0;
    
    for (int i = 0; i < number_spectra; i++) total_traces += spectra[i][0].size();
    
    std::vector<int> polarity_out(total_traces);
    std::vector<int> level_out(total_traces);
    std::vector<float> pre_mz_out(total_traces);
    std::vector<float> pre_ce_out(total_traces);
    std::vector<float> rt_out(total_traces);
    std::vector<float> mobility_out(total_traces);
    std::vector<float> mz_out(total_traces);
    std::vector<float> intensity_out(total_traces);

    int trace = 0;
    
    #pragma omp parallel for
    for (int i = 0; i < number_spectra; i++) {
      const std::vector<float>& mz_ref = spectra[i][0];
      const std::vector<float>& intensity_ref = spectra[i][1];
      const int n = mz_ref.size();
      
      for (int k = 0; k < n; k++) {
        polarity_out[trace] = polarity[i];
        level_out[trace] = level[i];
        pre_mz_out[trace] = pre_mz[i];
        pre_ce_out[trace] = pre_ce[i];
        rt_out[trace] = rt[i];
        mobility_out[trace] = mobility[i];
        mz_out[trace] = mz_ref[k];
        intensity_out[trace] = intensity_ref[k];

        #pragma omp critical
        {
          trace += 1;
        }
      }
    }
    
    out["polarity"] = polarity_out;
    out["level"] = level_out;
    out["pre_mz"] = pre_mz_out;
    out["pre_ce"] = pre_ce_out;
    out["rt"] = rt_out;
    out["mobility"] = mobility_out;
    out["mz"] = mz_out;
    out["intensity"] = intensity_out;
    
    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
    
  } else {
    
    std::vector<int> tg_idx(n_tg);
    std::iota(tg_idx.begin(), tg_idx.end(), 0);
    
    const std::vector<std::string> df_id = targets["id"];
    
    int tg_level = 0;
    if (number_levels == 1) tg_level = levels[0];
    std::vector<int> df_level(n_tg, tg_level);
    Rcpp::CharacterVector tg_col_names = targets.names();
    if (std::find(tg_col_names.begin(), tg_col_names.end(), "level") != tg_col_names.end()) {
      std::vector<int> temp_df_level = targets["level"];
      for (int i = 0; i < n_tg; i++) df_level[i] = temp_df_level[i];
    }
    
    const std::vector<int> df_polarity = targets["polarity"];
    const std::vector<bool> df_precursor = targets["precursor"];
    const std::vector<float> df_mzmin = targets["mzmin"];
    const std::vector<float> df_mzmax = targets["mzmax"];
    const std::vector<float> df_rtmin = targets["rtmin"];
    const std::vector<float> df_rtmax = targets["rtmax"];
    const std::vector<float> df_mobilitymin = targets["mobilitymin"];
    const std::vector<float> df_mobilitymax = targets["mobilitymax"];
    
    sc::MS_TARGETS tg = {
      tg_idx,
      df_id,
      df_level,
      df_polarity,
      df_precursor,
      df_mzmin,
      df_mzmax,
      df_rtmin,
      df_rtmax,
      df_mobilitymin,
      df_mobilitymax
    };

    std::set<int> idx;
    
    const std::vector<int>& hd_index = hd["index"];
    const std::vector<int>& hd_polarity = hd["polarity"];
    const std::vector<int>& hd_configuration = hd["configuration"];
    const std::vector<int>& hd_level = hd["level"];
    const std::vector<float>& hd_pre_mz = hd["pre_mz"];
    const std::vector<float>& hd_pre_mz_low = hd["pre_mzlow"];
    const std::vector<float>& hd_pre_mz_high = hd["pre_mzhigh"];
    const std::vector<float>& hd_pre_ce = hd["pre_ce"];
    const std::vector<float>& hd_mobility = hd["mobility"];
    
    sc::MS_SPECTRA_HEADERS headers;
    headers.resize_all(number_spectra);
    
    headers.index = hd_index;
    headers.rt = rt;
    headers.polarity = hd_polarity;
    headers.configuration = hd_configuration;
    headers.level = hd_level;
    headers.precursor_mz = hd_pre_mz;
    headers.activation_ce = hd_pre_ce;
    headers.mobility = hd_mobility;
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(tg, headers, minIntLv1, minIntLv2);
    
    out["id"] = res.id;
    out["polarity"] = res.polarity;
    out["level"] = res.level;
    out["pre_mz"] = res.pre_mz;
    out["pre_mzlow"] = res.pre_mzlow;
    out["pre_mzhigh"] = res.pre_mzhigh;
    out["pre_ce"] = res.pre_ce;
    out["rt"] = res.rt;
    out["mobility"] = res.mobility;
    out["mz"] = res.mz;
    out["intensity"] = res.intensity;

    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

    return out;
  }
  
  return empty_df;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_chromatograms(Rcpp::List analysis, std::vector<int> idx) {
  
  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List out;
  
  const std::string file = analysis["file"];
  
  const Rcpp::List& hd = analysis["chromatograms_headers"];
  
  const std::vector<int> index = hd["index"];
  const std::vector<std::string> id = hd["id"];
  const std::vector<int> polarity = hd["polarity"];
  const std::vector<float> pre_mz = hd["pre_mz"];
  const std::vector<float> pre_ce = hd["pre_ce"];
  const std::vector<float> pro_mz = hd["pro_mz"];
  
  const int number_chromatograms = index.size();
  
  if (number_chromatograms == 0) return empty_df;
  
  sc::MS_ANALYSIS ana(file);
  
  if (idx.size() == 0) idx = index;
  
  const std::vector<std::vector<std::vector<float>>> chromatograms = ana.get_chromatograms(idx);
  
  const int number_extracted_chromatograms = chromatograms.size();
  
  if (number_extracted_chromatograms == 0) return empty_df;
  
  int total_traces = 0;
  
  for (int i = 0; i < number_extracted_chromatograms; i++) total_traces += chromatograms[i][0].size();
  
  if (total_traces == 0) return empty_df;
    
  std::vector<int> index_out(total_traces);
  std::vector<std::string> id_out(total_traces);
  std::vector<int> polarity_out(total_traces);
  std::vector<float> pre_mz_out(total_traces);
  std::vector<float> pre_ce_out(total_traces);
  std::vector<float> pro_mz_out(total_traces);
  std::vector<float> rt_out(total_traces);
  std::vector<float> intensity_out(total_traces);
    
  int trace = 0;
    
  for (int i = 0; i < number_extracted_chromatograms; i++) {
    
    const std::vector<float>& rtj = chromatograms[i][0];
    const std::vector<float>& intj = chromatograms[i][1];
 
    const int n = rtj.size();
    
    if (n == 0) continue;
      
    for (int j = 0; j < n; j++) {
      
      index_out[trace] = index[i];
      id_out[trace] = id[i];
      polarity_out[trace] = polarity[i];
      pre_mz_out[trace] = pre_mz[i];
      pre_ce_out[trace] = pre_ce[i];
      pro_mz_out[trace] = pro_mz[i];
      rt_out[trace] = rtj[j];
      intensity_out[trace] = intj[j];
      
      trace += 1;
    }
  }
    
  out["index"] = index_out;
  out["id"] = id_out;
  out["polarity"] = polarity_out;
  out["pre_mz"] = pre_mz_out;
  out["pre_ce"] = pre_ce_out;
  out["pro_mz"] = pro_mz_out;
  out["rt"] = rt_out;
  out["intensity"] = intensity_out;
  
  out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return out;
}

float mean(const std::vector<float>& v) {
  return accumulate(v.begin(), v.end(), 0.0) / v.size();
}

float standard_deviation(const std::vector<float>& v, float mean_val) {
  float sum = 0.0;
  for (float num : v) {
    sum += pow(num - mean_val, 2);
  }
  return sqrt(sum / v.size());
}

float estimate_sigma_from_fwhm(const std::vector<float>& x, const std::vector<float>& y, const float& max_y) {
  
  float half_max_y = max_y / 2.0;

  // Find x positions where y crosses half of the max y
  float x1 = 0.0, x2 = 0.0;
  bool found_x1 = false;
  for (size_t i = 0; i < y.size() - 1; ++i) {
    if (y[i] >= half_max_y && y[i+1] <= half_max_y) {
      x1 = x[i];
      found_x1 = true;
    }
    if (found_x1 && y[i] <= half_max_y && y[i+1] >= half_max_y) {
      x2 = x[i+1];
      break;
    }
  }

  double fwhm = x2 - x1;
  double sigma = fwhm / 2.355;
  return sigma;
}

size_t findMaxIndex(const std::vector<float>& v) {
  return max_element(v.begin(), v.end()) - v.begin();
}

size_t findMinIndex(const std::vector<float>& v) {
  return min_element(v.begin(), v.end()) - v.begin();
}

// Function to trim data around the maximum value
void trimDataAroundMax(const std::vector<float> x, std::vector<float>& x_trimmed, const size_t max_position) {
  // Determine the number of points to keep on each side
  size_t n_points = std::min(max_position, x.size() - max_position - 1);
  
  // Resize trimmed vectors
  x_trimmed.resize(2 * n_points + 1);
  
  // Fill trimmed vectors
  for (size_t i = 0; i < x_trimmed.size(); ++i) {
    x_trimmed[i] = x[max_position - n_points + i];
  }
}


float gaussian(float A, float mu, float sigma, float x) {
  return A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
  // return A * exp(-0.5 * pow((x - mu) / sigma, 2));
}

float costFunction(const std::vector<float>& x, const std::vector<float>& y, float A, float mu, float sigma) {
  float cost = 0.0;
  for (size_t i = 0; i < x.size(); ++i) {
    float y_pred = gaussian(A, mu, sigma, x[i]);
    cost += pow(y[i] - y_pred, 2);
  }
  return cost;
}

void fitGaussian(const std::vector<float>& x, const std::vector<float>& y, float& A_fitted, float& mu_fitted, float& sigma_fitted) {
  
  // Optimization parameters
  const float learning_rate = 0.001;
  const int max_iterations = 10000;
  const float tolerance = 1e-08;
  
  for (int iter = 0; iter < max_iterations; ++iter) {
    float current_cost = costFunction(x, y, A_fitted, mu_fitted, sigma_fitted);
    
    // Numerical gradient calculation
    float grad_A = (costFunction(x, y, A_fitted + tolerance, mu_fitted, sigma_fitted) - current_cost) / tolerance;
    float grad_mu = (costFunction(x, y, A_fitted, mu_fitted + tolerance, sigma_fitted) - current_cost) / tolerance;
    float grad_sigma = (costFunction(x, y, A_fitted, mu_fitted, sigma_fitted + tolerance) - current_cost) / tolerance;
    
    // Update parameters using gradient descent
    A_fitted -= learning_rate * grad_A;
    mu_fitted -= learning_rate * grad_mu;
    sigma_fitted -= learning_rate * grad_sigma;
    
    if (sigma_fitted <= 0) {
      sigma_fitted = 2; // Set to a small positive number if non-positive
    }
    if (sigma_fitted > 1e6) { // Arbitrary upper bound for sigma
      sigma_fitted = 10;
    }
    
    // Check convergence
    if (abs(grad_A) < tolerance && abs(grad_mu) < tolerance && abs(grad_sigma) < tolerance) {
      break;
    }
  }
}

// float gaussian_pdf(float x, float mu, float sigma) {
//   return (1.0 / (sigma * sqrt(2 * M_PI))) * exp(-0.5 * pow((x - mu) / sigma, 2));
// }

// A function to compute log of the PDF (y = log(f(x)))
// float log_gaussian_pdf(float x, float mu, float sigma) {
//   float pdf = gaussian_pdf(x, mu, sigma);
//   return log(pdf);
// }

// Perform linear regression to fit y = a2 * x^2 + a1 * x + a0
// void quadratic_regression(const std::vector<float>& x, const std::vector<float>& y,
//                           float &a2, float &a1, float &a0) {
//   int n = x.size();
//   assert(n == y.size() && n > 0);
//   
//   // Calculating sums of powers of x and x*y
//   float sum_x = 0, sum_x2 = 0, sum_x3 = 0, sum_x4 = 0;
//   float sum_y = 0, sum_xy = 0, sum_x2y = 0;
//   
//   for (int i = 0; i < n; ++i) {
//     float xi = x[i];
//     float xi2 = xi * xi;
//     sum_x += xi;
//     sum_x2 += xi2;
//     sum_x3 += xi * xi2;
//     sum_x4 += xi2 * xi2;
//     sum_y += y[i];
//     sum_xy += xi * y[i];
//     sum_x2y += xi2 * y[i];
//   }
//   
//   // Solving the normal equations for quadratic regression
//   float det = n * (sum_x2 * sum_x4 - sum_x3 * sum_x3)
//     - sum_x * (sum_x * sum_x4 - sum_x2 * sum_x3)
//     + sum_x2 * (sum_x * sum_x3 - sum_x2 * sum_x2);
//     
//     // Coefficients
//     a0 = (1.0 / det) * ((sum_x2 * sum_x4 - sum_x3 * sum_x3) * sum_y
//                           + (sum_x3 * sum_x2 - sum_x * sum_x4) * sum_xy
//                           + (sum_x * sum_x3 - sum_x2 * sum_x2) * sum_x2y);
//                           
//                           a1 = (1.0 / det) * ((n * sum_x4 - sum_x2 * sum_x2) * sum_xy
//                                                 + (sum_x2 * sum_x - n * sum_x4) * sum_x2y
//                                                 + (sum_x2 * sum_x2 - sum_x3 * n) * sum_y);
//                                                 
//                                                 a2 = (1.0 / det) * ((n * sum_x2 - sum_x * sum_x) * sum_x2y
//                                                                       + (sum_x2 * sum_x - sum_x * n) * sum_y
//                                                                       + (sum_x * n - sum_x * sum_x2) * sum_xy);
// }

// float calculate_r_squared(const std::vector<float>& x, const std::vector<float>& y,
//                            float a2, float a1, float a0) {
//   int n = x.size();
//   assert(n == y.size());
//   
//   // Mean of observed values (y)
//   float y_mean = mean(y);
//   
//   // Calculate SS_res (sum of squared residuals) and SS_tot (total sum of squares)
//   float ss_res = 0.0;
//   float ss_tot = 0.0;
//   
//   for (int i = 0; i < n; ++i) {
//     float y_pred = a2 * x[i] * x[i] + a1 * x[i] + a0;  // Predicted y value from the model
//     ss_res += pow(y[i] - y_pred, 2);    // (y_i - y_pred)^2
//     ss_tot += pow(y[i] - y_mean, 2);    // (y_i - y_mean)^2
//   }
//   
//   // R-squared formula
//   return 1.0 - (ss_res / ss_tot);
// }

// float calculate_r_squared(const std::vector<float>& actual, const std::vector<float>& predicted) {
//   assert(actual.size() == predicted.size());
//   
//   float actual_mean = mean(actual);
//   float ss_res = 0.0;  // Residual sum of squares
//   float ss_tot = 0.0;  // Total sum of squares
//   
//   for (size_t i = 0; i < actual.size(); ++i) {
//     ss_res += pow(actual[i] - predicted[i], 2);
//     ss_tot += pow(actual[i] - actual_mean, 2);
//   }
//   
//   return 1.0 - (ss_res / ss_tot);  // R² formula
// }

float calculateRSquared(const std::vector<float>& x, const std::vector<float>& y, float A, float mu, float sigma) {
  float ss_total = 0.0;
  float ss_residual = 0.0;
  float mean_y = accumulate(y.begin(), y.end(), 0.0) / y.size();
  
  for (size_t i = 0; i < x.size(); ++i) {
    float y_pred = gaussian(A, mu, sigma, x[i]);
    ss_residual += pow(y[i] - y_pred, 2);
    ss_total += pow(y[i] - mean_y, 2);
  }
  return 1 - (ss_residual / ss_total);
}

std::vector<float> calculateFittingVector(const std::vector<float>& x, float A, float mu, float sigma) {
  std::vector<float> y(x.size());
  for (size_t i = 0; i < x.size(); ++i) {
    y[i] = gaussian(A, mu, sigma, x[i]);
  }
  return y;
}

// [[Rcpp::export]]
Rcpp::List rcpp_ms_fill_features(Rcpp::List analyses,
                                 Rcpp::DataFrame features,
                                 bool withinReplicate = false,
                                 float rtExpand = 0,
                                 float mzExpand = 0,
                                 int minNumberTraces = 5,
                                 float minSignalToNoiseRatio = 3,
                                 float minGaussianFit = 0.5,
                                 float minIntensity = 0) {
  
  Rcpp::List out;
  
  const int number_analyses = analyses.size();
  
  if (number_analyses == 0) return out;
  
  std::vector<std::string> analyses_names(number_analyses);
  std::vector<std::vector<int>> analyses_polarities(number_analyses);
  
  for (int i = 0; i < number_analyses; i++) {
    Rcpp::List analysis = analyses[i];
    std::string name = analysis["name"];
    analyses_names[i] = name;
    Rcpp::List headers = analysis["spectra_headers"];
    std::vector<int> pols = headers["polarity"];
    std::set<int> pols_set(pols.begin(), pols.end());
    std::vector<int> pols_unique(pols_set.begin(), pols_set.end());
    analyses_polarities[i] = pols_unique;
  }
  
  std::cout << "analyses_names size: " << analyses_names.size() << std::endl;
  
  int max_presence = number_analyses;
  
  const int number_features = features.nrow();
  
  if (number_features == 0) return out;
  
  std::vector<std::string> features_cols = features.names();
  
  const int ncol_features = features_cols.size();
  
  if (ncol_features == 0) return out;
  
  std::vector<std::string> must_have_names = {
    "feature", "analysis", "rt", "rtmax", "rtmin",
    "mz", "mzmax", "mzmin", "intensity", "group"
  };
  
  std::vector<bool> has_must_have_names(10, false);
  
  for (size_t i = 0; i < must_have_names.size(); ++i) {
    for (size_t j = 0; j < features_cols.size(); ++j) {
      if (must_have_names[i] == features_cols[j]) has_must_have_names[i] = true;
    }
  }
  
  for (bool value : has_must_have_names) {
    if (!value) {
      return out;
    }
  }
  
  const std::vector<std::string> fts_group = features["group"];
  const std::vector<std::string> fts_replicates = features["replicate"];
  const std::vector<std::string> fts_analysis = features["analysis"];
  const std::vector<float> fts_mass = features["mass"];
  const std::vector<float> fts_mz = features["mz"];
  const std::vector<float> fts_mzmin = features["mzmin"];
  const std::vector<float> fts_mzmax = features["mzmax"];
  const std::vector<float> fts_rt = features["rt"];
  const std::vector<float> fts_rtmin = features["rtmin"];
  const std::vector<float> fts_rtmax = features["rtmax"];
  // const std::vector<float> fts_mobility = features["mobility"];
  // const std::vector<float> fts_mobilitymin = features["mobilitymin"];
  // const std::vector<float> fts_mobilitymax = features["mobilitymax"];
  const std::vector<float> fts_intensity = features["intensity"];
  
  std::vector<std::string> fts_id = fts_group;
  
  if (withinReplicate) {
    
    std::unordered_map<std::string, int> rpl_sizes;
    
    for (int i = 0; i < number_analyses; i++) {
      Rcpp::List analysis = analyses[i];
      std::string rpl = analysis["replicate"];
      rpl_sizes[rpl]++;
    }
    
    for (int i = 0; i < number_features; i++) fts_id[i] = fts_id[i] + "_" + fts_replicates[i];
    
    max_presence = 0;
    for (const auto& rpl : rpl_sizes) if (rpl.second > max_presence) max_presence = rpl.second;
  }
  
  if (max_presence == 0) return out;
  
  std::unordered_map<std::string, int> id_presence;
  for (int i = 0; i < number_features; i++) id_presence[fts_id[i]]++;
  
  std::cout << "max_presence: " << max_presence << std::endl;
  std::cout << "fts_id size: " << fts_id.size() << std::endl;
  std::cout << "id_presence size: " << id_presence.size() << std::endl;
  
  std::vector<std::string> id = fts_id;
  
  id.erase(
    std::remove_if(id.begin(), id.end(), [&](const std::string& name) {
      return id_presence[name] == max_presence;
    }),
    id.end()
  );
  
  if (id.empty()) return out;
  
  std::cout << "id size: " << id.size() << std::endl;
  
  // for (const auto& i : id) {
  //   std::cout << i << " " << id_presence[i] << std::endl;
  // }
  
  std::set<std::string> id_set(id.begin(), id.end());
  
  std::vector<std::string> unique_id(id_set.begin(), id_set.end());
  
  const int number_unique_id = unique_id.size();
  
  std::cout << "unique id size: " << unique_id.size() << std::endl;
  
  std::vector<sc::MS_TARGETS> ana_targets(number_analyses);
  std::vector<std::vector<std::string>> ana_targets_groups(number_analyses);
  std::vector<std::vector<std::string>> ana_targets_replicates(number_analyses);
  
  for (int k = 0; k < number_unique_id; k++) {
    
    std::vector<int> indices;
    for (int i = 0; i < number_features; i++) {
      if (fts_id[i] == unique_id[k]) indices.push_back(i);
    }
    
    if (indices.size() == 0) continue;
    
    std::vector<std::string> fts_group_k(indices.size());
    std::vector<std::string> fts_replicates_k(indices.size());
    std::vector<std::string> fts_analysis_k(indices.size());
    std::vector<float> fts_mass_k(indices.size());
    std::vector<float> fts_mz_k(indices.size());
    std::vector<float> fts_mzmin_k(indices.size());
    std::vector<float> fts_mzmax_k(indices.size());
    std::vector<float> fts_rt_k(indices.size());
    std::vector<float> fts_rtmin_k(indices.size());
    std::vector<float> fts_rtmax_k(indices.size());
    std::vector<float> fts_intensity_k(indices.size());
    
    std::vector<float> fts_mzmin_side_k(indices.size());
    std::vector<float> fts_mzmax_side_k(indices.size());
    
    for (size_t i = 0; i < indices.size(); i++) {
      fts_group_k[i] = fts_group[indices[i]];
      fts_replicates_k[i] = fts_replicates[indices[i]];
      fts_analysis_k[i] = fts_analysis[indices[i]];
      fts_mass_k[i] = fts_mass[indices[i]];
      fts_mz_k[i] = fts_mz[indices[i]];
      fts_mzmin_k[i] = fts_mzmin[indices[i]];
      fts_mzmax_k[i] = fts_mzmax[indices[i]];
      fts_rt_k[i] = fts_rt[indices[i]];
      fts_rtmin_k[i] = fts_rtmin[indices[i]] - rtExpand;
      fts_rtmax_k[i] = fts_rtmax[indices[i]] + rtExpand;
      fts_intensity_k[i] = fts_intensity[indices[i]];
      
      fts_mzmin_side_k[i] = fts_mz[indices[i]] - fts_mzmin[indices[i]];
      fts_mzmax_side_k[i] = fts_mzmax[indices[i]] - fts_mz[indices[i]];
    }
    
    const float mean_mass = std::accumulate(fts_mass_k.begin(), fts_mass_k.end(), 0.0) / fts_mass_k.size();
    const float max_mzmin_side = *std::max_element(fts_mzmin_side_k.begin(), fts_mzmin_side_k.end());
    const float max_mzmax_side = *std::max_element(fts_mzmax_side_k.begin(), fts_mzmax_side_k.end());
    const float min_rtmin = *std::min_element(fts_rtmin_k.begin(), fts_rtmin_k.end());
    const float max_rtmax = *std::max_element(fts_rtmax_k.begin(), fts_rtmax_k.end());
    
    for (int j = 0; j < number_analyses; j++) {
      
      if (withinReplicate) {
        Rcpp::List analysis = analyses[j];
        std::string rpl = analysis["replicate"];
        if (fts_replicates_k[0] != rpl) continue;
      }
      
      if (std::find(fts_analysis_k.begin(), fts_analysis_k.end(), analyses_names[k]) != fts_analysis_k.end()) continue;
      
      std::vector<int> pols = analyses_polarities[j];
      
      for (size_t p = 0; p < pols.size(); p++) {
        
        float mz = mean_mass + (pols[p] * 1.007276);
        float mzmin = mz - max_mzmin_side - mzExpand;
        float mzmax = mz + max_mzmax_side + mzExpand;
        
        const int n_targets = ana_targets[j].index.size();
        ana_targets[j].index.push_back(n_targets);
        ana_targets[j].id.push_back(unique_id[k]);
        ana_targets[j].level.push_back(1);
        ana_targets[j].polarity.push_back(pols[p]);
        ana_targets[j].precursor.push_back(false);
        ana_targets[j].mzmin.push_back(mzmin);
        ana_targets[j].mzmax.push_back(mzmax);
        ana_targets[j].rtmin.push_back(min_rtmin);
        ana_targets[j].rtmax.push_back(max_rtmax);
        ana_targets[j].mobilitymin.push_back(0);
        ana_targets[j].mobilitymax.push_back(0);
        ana_targets_groups[j].push_back(fts_group_k[0]);
        ana_targets_replicates[j].push_back(fts_replicates_k[0]);
      }
      
    }
  }
  
  for (int j = 0; j < number_analyses; j++) {
    
    Rcpp::List out_analyses;
    
    if (ana_targets[j].index.size() == 0) continue;
    
    const Rcpp::List& analysis = analyses[j];
    const Rcpp::List& hd = analysis["spectra_headers"];
    const std::vector<int>& hd_index = hd["index"];
    const int number_spectra = hd_index.size();
    
    if (number_spectra == 0) continue;
    
    const std::vector<int>& hd_polarity = hd["polarity"];
    const std::vector<int>& hd_configuration = hd["configuration"];
    const std::vector<float>& hd_rt = hd["rt"];
    const std::vector<int>& hd_level = hd["level"];
    const std::vector<float>& hd_pre_mz = hd["pre_mz"];
    const std::vector<float>& hd_pre_mz_low = hd["pre_mzlow"];
    const std::vector<float>& hd_pre_mz_high = hd["pre_mzhigh"];
    const std::vector<float>& hd_pre_ce = hd["pre_ce"];
    const std::vector<float>& hd_mobility = hd["mobility"];
    
    sc::MS_SPECTRA_HEADERS headers;
    headers.resize_all(number_spectra);
    
    headers.index = hd_index;
    headers.rt = hd_rt;
    headers.polarity = hd_polarity;
    headers.configuration = hd_configuration;
    headers.level = hd_level;
    headers.precursor_mz = hd_pre_mz;
    headers.activation_ce = hd_pre_ce;
    headers.mobility = hd_mobility;
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_ANALYSIS ana(file);
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(ana_targets[j], headers, minIntensity, 0);
    
    const std::vector<std::string> tg_id = ana_targets[j].id;
    
    const int n_tg = tg_id.size();
    
    for (int i = 0; i < n_tg; i++) {
      const std::string& id_i = tg_id[i];
      
      sc::MS_TARGETS_SPECTRA res_i = res[id_i];
      
      const int n_res_i = res_i.id.size();
      
      if (n_res_i < 5) continue;
      
      size_t max_position = findMaxIndex(res_i.intensity);
      const float max_intensity = res_i.intensity[max_position];
      
      const size_t min_position = findMinIndex(res_i.intensity);
      const float min_intensity = res_i.intensity[min_position];
      const float sn = max_intensity / min_intensity;
      
      if (sn <= minSignalToNoiseRatio) continue;
      
      const float half_cut = max_intensity * 0.5;
      
      std::vector<float> rt_trimmed(n_res_i);
      std::vector<float> mz_trimmed(n_res_i);
      std::vector<float> int_trimmed(n_res_i);
      
      for (int z = 0; z < n_res_i; z++) {
        int_trimmed[z] = res_i.intensity[z] - half_cut;
        rt_trimmed[z] = res_i.rt[z];
        mz_trimmed[z] = res_i.mz[z];
      }
      
      auto it_int_trimmed = int_trimmed.begin();
      auto it_rt_trimmed = rt_trimmed.begin();
      auto it_mz_trimmed = mz_trimmed.begin();
      
      while (it_int_trimmed != int_trimmed.end()) {
        if (*it_int_trimmed <= 0) {
          it_int_trimmed = int_trimmed.erase(it_int_trimmed);
          it_rt_trimmed = rt_trimmed.erase(it_rt_trimmed);
          it_mz_trimmed = mz_trimmed.erase(it_mz_trimmed);
        } else {
          ++it_int_trimmed;
          ++it_rt_trimmed;
          ++it_mz_trimmed;
        }
      }
      
      int n_trimmed = int_trimmed.size();
      
      if (n_trimmed < 3) continue;
      
      max_position = findMaxIndex(int_trimmed);

      trimDataAroundMax(rt_trimmed, rt_trimmed, max_position);
      trimDataAroundMax(mz_trimmed, mz_trimmed, max_position);
      trimDataAroundMax(int_trimmed, int_trimmed, max_position);
      
      n_trimmed = rt_trimmed.size();
      
      if (n_trimmed < 3) continue;
      
      max_position = findMaxIndex(int_trimmed);
      
      float A_fitted = int_trimmed[max_position];
      float mu_fitted = rt_trimmed[max_position];
      float sigma_fitted = (rt_trimmed.back() - rt_trimmed.front()) / 4.0;
      
      if (id_i == "M331_R1233_1901_03_tof_ww_is_pos_o3sw_effluent" && analyses_names[j] == "03_tof_ww_is_pos_o3sw_effluent-r003") {
        std::cout << "Initial: A = " << A_fitted << ", mu = " << mu_fitted << ", sigma = " << sigma_fitted << std::endl;
      }

      fitGaussian(rt_trimmed, int_trimmed, A_fitted, mu_fitted, sigma_fitted);

      if (id_i == "M331_R1233_1901_03_tof_ww_is_pos_o3sw_effluent" && analyses_names[j] == "03_tof_ww_is_pos_o3sw_effluent-r003") {
        std::cout << "Fitted: A = " << A_fitted << ", mu = " << mu_fitted << ", sigma = " << sigma_fitted << std::endl;
      }

      float r_squared = calculateRSquared(rt_trimmed, int_trimmed, A_fitted, mu_fitted, sigma_fitted);
      
      std::vector<std::string> id_trimmed(n_trimmed, id_i);
      std::vector<float> sn_trimmed(n_trimmed, sn);
      std::vector<float> A_trimmed(n_trimmed, A_fitted);
      std::vector<float> mu_trimmed(n_trimmed, mu_fitted);
      std::vector<float> signma_trimmed(n_trimmed, sigma_fitted);
      std::vector<float> r2_trimmed(n_trimmed, r_squared);
      std::vector<float> fit = calculateFittingVector(rt_trimmed, A_fitted, mu_fitted, sigma_fitted);
      Rcpp::List out_targets;
      out_targets["id"] = id_trimmed;
      out_targets["sn"] = sn_trimmed;
      out_targets["A"] = A_trimmed;
      out_targets["mu"] = mu_trimmed;
      out_targets["sigma"] = signma_trimmed;
      out_targets["r_squared"] = r2_trimmed;
      out_targets["rt"] = rt_trimmed;
      out_targets["intensity"] = int_trimmed;
      out_targets["fit"] = fit;
      out_targets.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      out_analyses[id_i] = out_targets;

      // if (r_squared <= minGaussianFit) continue;
      // 
      // const float rt_i = rt_trimmed[max_position_trimmed];
      // 
      // const float mz_i = std::accumulate(mz_trimmed.begin(), mz_trimmed.end(), 0.0) / n_trimmed;
      // 
      // const float mass_i = mz_i - (res_i.polarity[0] * 1.007276);
      // 
      // const float mzmin_i = *std::min_element(mz_trimmed.begin(), mz_trimmed.end());
      // 
      // const float mzmax_i = *std::max_element(mz_trimmed.begin(), mz_trimmed.end());
      // 
      // const float rtmin_i = *std::min_element(rt_trimmed.begin(), rt_trimmed.end());
      // 
      // const float rtmax_i = *std::max_element(rt_trimmed.begin(), rt_trimmed.end());
      // 
      // std::string adduct_i = "[M+H]+";
      // if (res_i.polarity[0] < 0) adduct_i = "[M-H]-";
      // 
      // std::string feature = "filled_" + std::to_string(i +1);
      // 
      // Rcpp::List out_targets;
      // out_targets["analysis"] = analyses_names[j];
      // out_targets["feature"] = feature;
      // out_targets["rt"] = rt_i;
      // out_targets["mz"] = mz_i;
      // out_targets["intensity"] = max_intensity;
      // out_targets["rtmin"] = rtmin_i;
      // out_targets["rtmax"] = rtmax_i;
      // out_targets["mzmin"] = mzmin_i;
      // out_targets["mzmax"] = mzmax_i;
      // out_targets["mass"] = mass_i;
      // out_targets["adduct"] = adduct_i;
      // out_targets["group"] = ana_targets_groups[j][i];
      // // out_targets["replicate"] = ana_targets_replicates[j][i];
      // out_targets["sn"] = sn;
      // out_targets["A"] = A_fitted;
      // out_targets["mu"] = mu_fitted;
      // out_targets["sigma"] = sigma_fitted;
      // out_targets["r_squared"] = r_squared;
      // out_targets.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      // 
      // out_analyses[id_i] = out_targets;
    }
    
    out.push_back(out_analyses);
  }
  
  // for (int j = 0; j < number_analyses; j++) {
  //   Rcpp::List out_targets;
  //   out_targets["index"] = ana_targets[j].index;
  //   out_targets["id"] = ana_targets[j].id;
  //   out_targets["level"] = ana_targets[j].level;
  //   out_targets["polarity"] = ana_targets[j].polarity;
  //   out_targets["precursor"] = ana_targets[j].precursor;
  //   out_targets["mzmin"] = ana_targets[j].mzmin;
  //   out_targets["mzmax"] = ana_targets[j].mzmax;
  //   out_targets["rtmin"] = ana_targets[j].rtmin;
  //   out_targets["rtmax"] = ana_targets[j].rtmax;
  //   out_targets["mobilitymin"] = ana_targets[j].mobilitymin;
  //   out_targets["mobilitymax"] = ana_targets[j].mobilitymax;
  //   out_targets.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  //   out.push_back(out_targets);
  // }
  
  out.names() = analyses_names;
  
  return out;
}
