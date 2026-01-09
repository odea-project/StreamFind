#ifndef NTS_UTILS_H
#define NTS_UTILS_H

#include <Rcpp.h>
#include "../streamcraft/streamcraft.h"
#include <vector>

// Forward declaration of nts::FEATURE
namespace nts
{
  struct FEATURE;
};

namespace nts::utils
{
  // MARK: DEBUG

  // Debug log file stream (global for debugging)
  extern std::ofstream debug_log;

  // Helper function to initialize debug log with dynamic filename
  void init_debug_log(const std::string& filename, const std::string& header = "");

  // Helper function to close debug log
  void close_debug_log();

  // Macro to write verbose debug output ONLY to log file (not console)
  #define DEBUG_LOG(x) do { \
    if (nts::utils::debug_log.is_open()) { nts::utils::debug_log << x; nts::utils::debug_log.flush(); } \
  } while(0)

  // Macro to write important debug output to both console and log file
  #define DEBUG_OUT(x) do { \
    if (nts::utils::debug_log.is_open()) { nts::utils::debug_log << x; nts::utils::debug_log.flush(); } \
    Rcpp::Rcout << x; \
  } while(0)

  Rcpp::List get_empty_dt();

  bool check_list_must_have_names(const Rcpp::List &list, const std::vector<std::string> &must_have_names);

  float mean(const std::vector<float> &v);

  float standard_deviation(const std::vector<float> &v, float mean_val);

  float quantile(std::vector<float> data, float quantile_fraction);

  std::string encode_floats_base64(const std::vector<float> &input, int precision = 4);

  float gaussian_function(const float &A, const float &mu, const float &sigma, const float &x);

  float gaussian_function_with_baseline(
    const float &A,
    const float &mu,
    const float &sigma,
    const float &baseline,
    const float &x);

  sc::MS_SPECTRA_HEADERS as_MS_SPECTRA_HEADERS(const Rcpp::List &hd);

  std::vector<size_t> get_sort_indices_float(const std::vector<float> &data);

  void reorder_float_data(std::vector<float> &data, const std::vector<size_t> &indices);

  void reorder_int_data(std::vector<int> &data, const std::vector<size_t> &indices);

  void reorder_multiple_vectors(
    const std::vector<size_t> &indices,
    std::vector<float> &vec1);

  void reorder_multiple_vectors(
    const std::vector<size_t> &indices,
    std::vector<float> &vec1,
    std::vector<float> &vec2);

  void reorder_multiple_vectors(
    const std::vector<size_t> &indices,
    std::vector<float> &vec1,
    std::vector<float> &vec2,
    std::vector<float> &vec3);

  void reorder_multiple_vectors(
    const std::vector<size_t> &indices,
    std::vector<float> &vec1,
    std::vector<float> &vec2,
    std::vector<float> &vec3,
    std::vector<float> &vec4);

  void reorder_multiple_vectors(
    const std::vector<size_t> &indices,
    std::vector<float> &vec1,
    std::vector<float> &vec2,
    std::vector<float> &vec3,
    std::vector<float> &vec4,
    std::vector<int> &int_vec);

  std::vector<size_t> filter_above_threshold(
    const std::vector<float> &data,
    const std::vector<float> &thresholds);

  std::vector<int> cluster_by_threshold_float(
    const std::vector<float> &sorted_data,
    const std::vector<float> &thresholds);

  std::vector<float> calculate_baseline(const std::vector<float> &intensity, int windowSize);

  std::vector<float> smooth_intensity_savitzky_golay(
    const std::vector<float> &intensity,
    int windowSize,
    int polyOrder);

  std::vector<float> smooth_intensity(const std::vector<float> &intensity, int windowSize);

  void calculate_derivatives(
    const std::vector<float> &smoothed_intensity,
    std::vector<float> &first_derivative,
    std::vector<float> &second_derivative);

  float calculate_jaggedness(const std::vector<float> &intensity);

  float calculate_sharpness(
    const std::vector<float> &rt,
    const std::vector<float> &intensity,
    float area);

  float calculate_asymmetry(
    const std::vector<float> &rt,
    const std::vector<float> &intensity);

  int calculate_modality(
    const std::vector<float> &smoothed_intensity,
    float min_prominence_ratio);

  float calculate_theoretical_plates(float rt_apex, float width_at_half_height);

  // Calculate peak area using trapezoidal integration
  float calculate_area(const std::vector<float> &rt, const std::vector<float> &intensity);

  // Gaussian fitting functions
  float gaussian_cost_function(
    const std::vector<float> &x,
    const std::vector<float> &y,
    float A, float mu, float sigma);

  void fit_gaussian(
    const std::vector<float> &x,
    const std::vector<float> &y,
    float &A, float &mu, float &sigma,
    float &baseline);

  float calculate_gaussian_rsquared(
    const std::vector<float> &x,
    const std::vector<float> &y,
    float A, float mu, float sigma,
    float baseline);

}; // namespace nts::utils
#endif
