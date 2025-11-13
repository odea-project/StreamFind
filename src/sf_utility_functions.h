#ifndef SF_UTILITY_FUNCTIONS_H
#define SF_UTILITY_FUNCTIONS_H

#include <Rcpp.h>
#include "StreamCraft_lib.h"

namespace SF_UTILITY
{
  // MARK: INLINE FUNCTIONS

  // MARK: make_empty_dt
  inline Rcpp::List get_empty_dt()
  {
    Rcpp::List out = Rcpp::List::create();
    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
  };

  // MARK: check_list_must_have_names
  inline bool check_list_must_have_names(
      const Rcpp::List &list,
      const std::vector<std::string> &must_have_names)
  {
    std::vector<std::string> names_list = list.names();
    const int must_have_names_size = must_have_names.size();
    if (must_have_names_size == 0)
      return false;
    const int names_list_size = names_list.size();
    if (names_list_size == 0)
      return false;
    std::vector<bool> has_must_have_names(must_have_names_size, false);

    for (int i = 0; i < must_have_names_size; ++i)
    {
      for (int j = 0; j < names_list_size; ++j)
      {
        if (must_have_names[i] == names_list[j])
          has_must_have_names[i] = true;
      }
    }

    bool has_all_must_have_names = true;

    for (int i = 0; i < must_have_names_size; ++i)
    {
      if (!has_must_have_names[i])
      {
        has_all_must_have_names = false;
        break;
      }
    }

    return has_all_must_have_names;
  };

  // MARK: mean
  inline float mean(const std::vector<float> &v)
  {
    return std::accumulate(v.begin(), v.end(), 0.0) / v.size();
  };

  // MARK: standard_deviation
  inline float standard_deviation(const std::vector<float> &v, float mean_val)
  {
    float sum = 0.0;
    for (float num : v)
    {
      sum += pow(num - mean_val, 2);
    }
    return sqrt(sum / v.size());
  };

  // MARK: find_max_index
  inline size_t find_max_index(const std::vector<float> &v)
  {
    return std::max_element(v.begin(), v.end()) - v.begin();
  };

  // MARK: find_min_index
  inline size_t find_min_index(const std::vector<float> &v)
  {
    return std::min_element(v.begin(), v.end()) - v.begin();
  };

  // MARK: gaussian_function
  inline float gaussian_function(const float &A,
                                 const float &mu,
                                 const float &sigma,
                                 const float &x)
  {
    return A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
  };

  // MARK: as_MS_SPECTRA_HEADERS
  inline sc::MS_SPECTRA_HEADERS as_MS_SPECTRA_HEADERS(const Rcpp::List &hd)
  {
    sc::MS_SPECTRA_HEADERS headers;
    const std::vector<int> &hd_index = hd["index"];
    const std::vector<int> &hd_polarity = hd["polarity"];
    const std::vector<int> &hd_configuration = hd["configuration"];
    const std::vector<float> &hd_rt = hd["rt"];
    const std::vector<int> &hd_level = hd["level"];
    const std::vector<float> &hd_pre_mz = hd["pre_mz"];
    const std::vector<float> &hd_pre_mz_low = hd["pre_mzlow"];
    const std::vector<float> &hd_pre_mz_high = hd["pre_mzhigh"];
    const std::vector<float> &hd_pre_ce = hd["pre_ce"];
    const std::vector<float> &hd_mobility = hd["mobility"];
    const int number_spectra = hd_index.size();
    headers.resize_all(number_spectra);
    headers.index = hd_index;
    headers.rt = hd_rt;
    headers.polarity = hd_polarity;
    headers.configuration = hd_configuration;
    headers.level = hd_level;
    headers.precursor_mz = hd_pre_mz;
    headers.activation_ce = hd_pre_ce;
    headers.mobility = hd_mobility;
    return headers;
  };

  // MARK: calculate_mass_resolution_model_param
  // Calculate mass resolution linear model parameters from resolution profile
  // resolution_profile[0] = resolution at 100 Da (or reference point)
  // resolution_profile[1] = resolution at 400 Da
  // resolution_profile[2] = resolution at 1000 Da
  inline std::pair<float, float> calculate_mass_resolution_model_param(const std::vector<int> &resolution_profile)
  {
    // Use three points to fit linear model: (100, res[0]), (400, res[1]), (1000, res[2])
    // For better fit, use least squares with the three points
    const float mz1 = 100.0f, mz2 = 400.0f, mz3 = 1000.0f;
    const float res1 = static_cast<float>(resolution_profile[0]);
    const float res2 = static_cast<float>(resolution_profile[1]);
    const float res3 = static_cast<float>(resolution_profile[2]);
    // Linear least squares fit: y = ax + b where y=resolution, x=mz
    const float n = 3.0f;
    const float sum_x = mz1 + mz2 + mz3;
    const float sum_y = res1 + res2 + res3;
    const float sum_xx = mz1 * mz1 + mz2 * mz2 + mz3 * mz3;
    const float sum_xy = mz1 * res1 + mz2 * res2 + mz3 * res3;
    const float slope = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x);
    const float intercept = (sum_y - slope * sum_x) / n;
    return std::make_pair(slope, intercept);
  };

  // MARK: calculate_mass_resolution_model_threshold
  // Helper function for linear resolution calculation across entire m/z range
  // Creates a linear relationship: mzThreshold = mz / resolution
  // where resolution = slope * mz + intercept
  inline float calculate_mass_resolution_model_threshold(float mz, float slope, float intercept)
  {
    const float resolution = slope * mz + intercept;
    // Add safety check to avoid division by very small resolutions
    return mz / std::max(resolution, 1.0f);
  };

  inline float calculate_mz_threshold_linear(float mz, float slope, float intercept)
  {
    const float resolution = slope * mz + intercept;
    // Add safety check to avoid division by very small resolutions
    return mz / std::max(resolution, 1.0f);
  };

  void denoise_spectra(
    sc::MS_FILE &ana,
    const int &spectrum_idx,
    const float &rt,
    const float &noiseThreshold,
    const int &minTraces,
    const float &slope,
    const float &intercept,
    const bool &debug,
    std::vector<float> &spec_rt,
    std::vector<float> &spec_mz,
    std::vector<float> &spec_intensity,
    std::vector<float> &spec_noise,
    size_t &total_raw_points,
    size_t &total_clean_points);

}; // namespace SF_UTILITY

#endif