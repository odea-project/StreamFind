#include "utils.h"
#include "nts.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <map>
#include <unordered_map>
#include <set>
#include <functional>
#include <iomanip>
#include <fstream>
#include <numeric>

// MARK: DEBUG

// Debug log file stream (global for debugging)
static std::ofstream debug_log;

// Helper function to initialize debug log with dynamic filename
static void init_debug_log(const std::string& filename, const std::string& header = "")
{
  if (!debug_log.is_open())
  {
    debug_log.open(filename, std::ios::out | std::ios::trunc);
    if (debug_log.is_open() && !header.empty())
    {
      debug_log << header << std::endl;
    }
  }
}

// Helper function to close debug log
static void close_debug_log()
{
  if (debug_log.is_open())
  {
    debug_log.close();
  }
}

// Macro to write verbose debug output ONLY to log file (not console)
#define DEBUG_LOG(x) do { \
  if (debug_log.is_open()) { debug_log << x; debug_log.flush(); } \
} while(0)

// Macro to write important debug output to both console and log file
#define DEBUG_OUT(x) do { \
  if (debug_log.is_open()) { debug_log << x; debug_log.flush(); } \
  Rcpp::Rcout << x; \
} while(0)

// MARK: BASIC UTILITY FUNCTIONS

Rcpp::List nts::utils::get_empty_dt()
{
  Rcpp::List out = Rcpp::List::create();
  out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  return out;
}

bool nts::utils::check_list_must_have_names(
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
}

float nts::utils::mean(const std::vector<float> &v)
{
  return std::accumulate(v.begin(), v.end(), 0.0) / v.size();
}

float nts::utils::standard_deviation(const std::vector<float> &v, float mean_val)
{
  float sum = 0.0;
  for (float num : v)
  {
    sum += pow(num - mean_val, 2);
  }
  return sqrt(sum / v.size());
}

float nts::utils::quantile(std::vector<float> data, float quantile_fraction)
{
  if (data.empty())
    return 0.0f;
  if (quantile_fraction <= 0.0f)
    return *std::min_element(data.begin(), data.end());
  if (quantile_fraction >= 1.0f)
    return *std::max_element(data.begin(), data.end());

  size_t idx = static_cast<size_t>((data.size() - 1) * quantile_fraction);
  std::nth_element(data.begin(), data.begin() + idx, data.end());
  return data[idx];
}

size_t nts::utils::find_max_index(const std::vector<float> &v)
{
  return std::max_element(v.begin(), v.end()) - v.begin();
}

size_t nts::utils::find_min_index(const std::vector<float> &v)
{
  return std::min_element(v.begin(), v.end()) - v.begin();
}

std::string nts::utils::encode_floats_base64(const std::vector<float> &input, int precision)
{
  if (input.empty())
  {
    return "";
  }

  std::string encoded = sc::encode_little_endian_from_float(input, precision);
  return sc::encode_base64(encoded);
}

nts::utils::ClusteredMzIntensity nts::utils::cluster_ms_targets_spectra(const sc::MS_TARGETS_SPECTRA &spectra,
                                                                        const float &mzClust,
                                                                        const float &presence)
{
  ClusteredMzIntensity out;

  const size_t n = spectra.mz.size();
  if (n == 0)
    return out;

  // Sort indices once so we can sweep contiguous clusters by m/z.
  std::vector<size_t> idx(n);
  std::iota(idx.begin(), idx.end(), 0);
  std::sort(idx.begin(), idx.end(), [&](size_t i, size_t j)
            { return spectra.mz[i] < spectra.mz[j]; });

  std::vector<float> sorted_mz(n);
  std::vector<float> sorted_intensity(n);
  std::vector<float> sorted_rt(spectra.rt.size());
  for (size_t k = 0; k < n; ++k)
  {
    const size_t src = idx[k];
    sorted_mz[k] = spectra.mz[src];
    sorted_intensity[k] = spectra.intensity[src];
    if (!sorted_rt.empty() && spectra.rt.size() > src)
      sorted_rt[k] = spectra.rt[src];
  }

  // Pre-compute total unique RT count for presence filtering.
  size_t total_unique_rt = 0;
  if (!sorted_rt.empty())
  {
    std::vector<float> tmp = sorted_rt;
    std::sort(tmp.begin(), tmp.end());
    total_unique_rt = static_cast<size_t>(std::unique(tmp.begin(), tmp.end()) - tmp.begin());
  }

  std::vector<float> new_mz;
  std::vector<float> new_intensity;
  new_mz.reserve(n);
  new_intensity.reserve(n);

  const float mz_tol = std::max(mzClust, 0.0f);
  const float presence_thresh = std::clamp(presence, 0.0f, 1.0f);

  size_t start = 0;
  while (start < n)
  {
    size_t end = start + 1;
    while (end < n && (sorted_mz[end] - sorted_mz[end - 1]) <= mz_tol)
      ++end;

    // Cluster range is [start, end)
    const size_t cluster_size = end - start;

    // Presence filter: require enough unique RTs inside this cluster.
    if (presence_thresh > 0.0f && total_unique_rt > 0)
    {
      std::vector<float> rt_slice;
      rt_slice.reserve(cluster_size);
      for (size_t i = start; i < end; ++i)
        rt_slice.push_back(sorted_rt[i]);
      std::sort(rt_slice.begin(), rt_slice.end());
      size_t unique_rt = static_cast<size_t>(std::unique(rt_slice.begin(), rt_slice.end()) - rt_slice.begin());

      if (static_cast<float>(unique_rt) < presence_thresh * static_cast<float>(total_unique_rt))
      {
        start = end;
        continue;
      }
    }

    float weighted_mz = 0.0f;
    float intensity_sum = 0.0f;
    float max_int = 0.0f;

    for (size_t i = start; i < end; ++i)
    {
      const float inten = sorted_intensity[i];
      weighted_mz += sorted_mz[i] * inten;
      intensity_sum += inten;
      max_int = std::max(max_int, inten);
    }

    if (intensity_sum > 0.0f)
    {
      new_mz.push_back(weighted_mz / intensity_sum);
      new_intensity.push_back(max_int);
    }

    start = end;
  }

  out.mz = std::move(new_mz);
  out.intensity = std::move(new_intensity);
  return out;
}

float nts::utils::gaussian_function(const float &A,
                                    const float &mu,
                                    const float &sigma,
                                    const float &x)
{
  return A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
}

// Gaussian function with baseline
float nts::utils::gaussian_function_with_baseline(const float &A,
                                                   const float &mu,
                                                   const float &sigma,
                                                   const float &baseline,
                                                   const float &x)
{
  return baseline + A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
}

// Helper function: Standard normal PDF φ(z) = (1/√(2π)) * exp(-z²/2)
float nts::utils::standard_normal_pdf(const float &z)
{
  static const float inv_sqrt_2pi = 1.0f / std::sqrt(2.0f * M_PI);
  return inv_sqrt_2pi * std::exp(-0.5f * z * z);
}

// Helper function: Standard normal CDF Φ(z) using error function
// Φ(z) = 0.5 * (1 + erf(z/√2))
float nts::utils::standard_normal_cdf(const float &z)
{
  return 0.5f * (1.0f + std::erf(z / std::sqrt(2.0f)));
}

// Skew-Gaussian (Azzalini skew-normal) function
// f(t) = A * (2/ω) * φ((t-ξ)/ω) * Φ(α*(t-ξ)/ω)
// Where:
//   A = amplitude (height scaling)
//   xi = location (peak center, similar to μ)
//   omega = scale (similar to σ, controls width)
//   alpha = skewness parameter
//     alpha = 0: symmetric Gaussian
//     alpha > 0: right tail (tailing)
//     alpha < 0: left tail (fronting)
float nts::utils::skew_gaussian_function(const float &A,
                                         const float &xi,
                                         const float &omega,
                                         const float &alpha,
                                         const float &t)
{
  if (omega <= 0.0f) return 0.0f; // Invalid omega

  const float z = (t - xi) / omega;
  const float phi_z = standard_normal_pdf(z);
  const float Phi_alpha_z = standard_normal_cdf(alpha * z);

  return A * (2.0f / omega) * phi_z * Phi_alpha_z;
}

// Exponentially Modified Gaussian (EMG) function
// Suitable for chromatographic peaks with tailing
// f(t) = baseline + A * (λ/2) * exp[(λ/2) * (2μ + λσ² - 2t)] * erfc[(μ + λσ² - t) / (√2 * σ)]
// Where:
//   A = amplitude (scales peak height)
//   mu = center of underlying Gaussian (retention time)
//   sigma = std deviation of Gaussian (width)
//   lambda = exponential decay rate (>0 for right tailing, typical 0.01-0.5)
//   baseline = constant offset
float nts::utils::emg_function(const float &A,
                               const float &mu,
                               const float &sigma,
                               const float &lambda,
                               const float &baseline,
                               const float &t)
{
  if (sigma <= 0.0f || lambda <= 0.0f) return baseline;

  const float lambda_half = lambda / 2.0f;
  const float sigma2 = sigma * sigma;
  const float sqrt2_sigma = std::sqrt(2.0f) * sigma;

  const float exp_arg = lambda_half * (2.0f * mu + lambda * sigma2 - 2.0f * t);
  const float erfc_arg = (mu + lambda * sigma2 - t) / sqrt2_sigma;

  return baseline + A * lambda_half * std::exp(exp_arg) * std::erfc(erfc_arg);
}

sc::MS_SPECTRA_HEADERS nts::utils::as_MS_SPECTRA_HEADERS(const Rcpp::List &hd)
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
}

float nts::utils::calculate_mz_threshold_linear(float mz, float slope, float intercept)
{
  const float resolution = slope * mz + intercept;
  // Add safety check to avoid division by very small resolutions
  return mz / std::max(resolution, 1.0f);
}

std::pair<float, float> nts::utils::calculate_linear_model_params(const std::vector<int>& resolution_profile) {
  // Use three points to fit linear model: (100, res[0]), (400, res[1]), (1000, res[2])
  const float mz1 = 100.0f, mz2 = 400.0f, mz3 = 1000.0f;
  const float res1 = static_cast<float>(resolution_profile[0]);
  const float res2 = static_cast<float>(resolution_profile[1]);
  const float res3 = static_cast<float>(resolution_profile[2]);

  // Linear least squares fit: y = ax + b where y=resolution, x=mz
  const float n = 3.0f;
  const float sum_x = mz1 + mz2 + mz3;
  const float sum_y = res1 + res2 + res3;
  const float sum_xx = mz1*mz1 + mz2*mz2 + mz3*mz3;
  const float sum_xy = mz1*res1 + mz2*res2 + mz3*res3;

  const float slope = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x);
  const float intercept = (sum_y - slope * sum_x) / n;

  return std::make_pair(slope, intercept);
}

// MARK: DATA STATISTICS IMPLEMENTATIONS

nts::utils::VectorStats::VectorStats(const std::vector<float> &input_data)
{
  count = input_data.size();
  if (count == 0)
  {
    mean = std_dev = coefficient_variation = 0.0f;
    min_val = max_val = signal_noise_ratio = 0.0f;
    return;
  }
  mean = nts::utils::mean(input_data);
  std_dev = nts::utils::standard_deviation(input_data, mean);
  coefficient_variation = (mean != 0.0f) ? std_dev / mean : 0.0f;
  auto [min_it, max_it] = std::minmax_element(input_data.begin(), input_data.end());
  min_val = *min_it;
  max_val = *max_it;
  float q25_val = nts::utils::quantile(input_data, 0.25f);
  float q90_val = nts::utils::quantile(input_data, 0.90f);
  signal_noise_ratio = (q25_val != 0.0f) ? q90_val / q25_val : 0.0f;
}

// MARK: CLUSTER STATISTICS IMPLEMENTATIONS

nts::utils::ClusterStats::ClusterStats(float intensity)
    : count(1), max(intensity), min(intensity),
      mean(intensity), sum(intensity) {}

void nts::utils::ClusterStats::update(float intensity)
{
  count++;
  max = std::max(max, intensity);
  min = std::min(min, intensity);
  sum += intensity;
  mean = sum / count;
}

// MARK: ADAPTIVE PARAMETERS IMPLEMENTATIONS

nts::utils::AdaptiveNoiseParams::AdaptiveNoiseParams(const VectorStats &stats, int data_size, float base_quantile)
{
  bins = std::max(10, std::min(200, static_cast<int>(std::sqrt(data_size) * 1.5)));
  // Adaptive quantile estimation based on coefficient of variation
  if (stats.coefficient_variation > 2.0f)
  {
    quantile = base_quantile * 0.5f; // High variability
  }
  else if (stats.coefficient_variation > 1.0f)
  {
    quantile = base_quantile; // Medium variability
  }
  else
  {
    quantile = base_quantile * 2.0f; // Low variability
  }
  // Adjust based on signal-to-noise ratio
  if (stats.signal_noise_ratio > 100.0f)
  {
    quantile *= 0.5f; // High dynamic range
    threshold_multiplier = 1.2f;
  }
  else if (stats.signal_noise_ratio < 10.0f)
  {
    quantile *= 1.5f; // Low dynamic range
    threshold_multiplier = 0.8f;
  }
  else
  {
    threshold_multiplier = 1.0f;
  }

  // Dynamic upper limit based on base_quantile to allow aggressive denoising when requested
  float max_quantile = std::max(0.30f, base_quantile * 1.2f); // Allow up to 20% higher than base_quantile
  quantile = std::clamp(quantile, 0.01f, max_quantile);

  // Adjust bins based on data sparsity
  float data_sparsity = static_cast<float>(data_size) / bins;
  if (data_sparsity < 5.0f)
  {
    bins = std::max(5, data_size / 5);
  }
  else if (data_sparsity > 50.0f)
  {
    bins = std::min(200, data_size / 20);
  }
}

// MARK: CORE UTILITIES

std::vector<size_t> nts::utils::get_sort_indices_float(const std::vector<float> &data)
{
  std::vector<size_t> indices(data.size());
  std::iota(indices.begin(), indices.end(), 0);
  std::sort(indices.begin(), indices.end(), [&data](size_t a, size_t b)
            { return data[a] < data[b]; });
  return indices;
}

void nts::utils::reorder_float_data(std::vector<float> &data, const std::vector<size_t> &indices)
{
  std::vector<float> reordered;
  reordered.reserve(data.size());
  for (size_t idx : indices)
  {
    reordered.push_back(data[idx]);
  }
  data = std::move(reordered);
}

void nts::utils::reorder_int_data(std::vector<int> &data, const std::vector<size_t> &indices)
{
  std::vector<int> reordered;
  reordered.reserve(data.size());
  for (size_t idx : indices)
  {
    reordered.push_back(data[idx]);
  }
  data = std::move(reordered);
}

// Overloaded reorder functions for different argument combinations
void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1)
{
  reorder_float_data(vec1, indices);
}

void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
}

void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2,
                                          std::vector<float> &vec3)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
  reorder_float_data(vec3, indices);
}

void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2,
                                          std::vector<float> &vec3,
                                          std::vector<float> &vec4)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
  reorder_float_data(vec3, indices);
  reorder_float_data(vec4, indices);
}

void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2,
                                          std::vector<float> &vec3,
                                          std::vector<float> &vec4,
                                          std::vector<int> &int_vec)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
  reorder_float_data(vec3, indices);
  reorder_float_data(vec4, indices);
  reorder_int_data(int_vec, indices);
}

std::vector<int> nts::utils::calculate_bin_assignments(const std::vector<float> &data, int num_bins)
{
  const size_t n = data.size();
  std::vector<int> assignments(n);

  if (num_bins <= 1 || n == 0)
  {
    std::fill(assignments.begin(), assignments.end(), 0);
    return assignments;
  }

  for (size_t i = 0; i < n; ++i)
  {
    assignments[i] = std::min(static_cast<int>(i * num_bins / n), num_bins - 1);
  }
  return assignments;
}

std::vector<size_t> nts::utils::filter_above_threshold(const std::vector<float> &data,
                                                       const std::vector<float> &thresholds)
{
  std::vector<size_t> indices;
  indices.reserve(data.size() / 2); // Conservative estimate

  const size_t n = std::min(data.size(), thresholds.size());
  for (size_t i = 0; i < n; ++i)
  {
    if (data[i] > thresholds[i])
    {
      indices.push_back(i);
    }
  }
  return indices;
}

std::vector<int> nts::utils::cluster_by_threshold_float(const std::vector<float> &sorted_data,
                                                        const std::vector<float> &thresholds)
{
  const size_t n = sorted_data.size();
  std::vector<int> clusters(n);

  if (n == 0)
    return clusters;

  clusters[0] = 0;
  int current_cluster = 0;

  for (size_t i = 1; i < n; ++i)
  {
    float diff = sorted_data[i] - sorted_data[i - 1];
    float threshold = (i < thresholds.size()) ? thresholds[i] : thresholds.back();

    if (diff > threshold)
    {
      ++current_cluster;
    }
    clusters[i] = current_cluster;
  }

  return clusters;
}

// MARK: SPECTRAL FUNCTIONS

// std::vector<int> nts::utils::cluster_by_mz(const std::vector<float> &mz_values,
//                                            const std::vector<int> &resolution_profile)
// {
//   const size_t n = mz_values.size();
//   if (n == 0)
//     return std::vector<int>();

//   // Calculate linear model parameters from resolution profile
//   const auto [slope, intercept] = calculate_linear_model_params(resolution_profile);

//   // Pre-calculate all thresholds using linear model
//   std::vector<float> thresholds(n);
//   for (size_t i = 0; i < n; ++i)
//   {
//     thresholds[i] = calculate_mz_threshold_linear(mz_values[i], slope, intercept);
//   }
//   return cluster_by_threshold_float(mz_values, thresholds);
// }

std::vector<int> nts::utils::cluster_by_mz(const std::vector<float> &mz_values, const float &ppmThreshold)
{
  const size_t n = mz_values.size();
  if (n == 0)
    return std::vector<int>();

  // Pre-calculate all thresholds using linear model
  std::vector<float> thresholds(n);
  for (size_t i = 0; i < n; ++i)
  {
    thresholds[i] = (mz_values[i] * ppmThreshold) / 1e6f;
  }
  return cluster_by_threshold_float(mz_values, thresholds);
}

std::vector<size_t> nts::utils::filter_valid_clusters(const std::vector<SpectraPoint> &data,
                                                      const std::vector<int> &clusters,
                                                      int minTraces, float minSNR)
{
  // Build cluster statistics map
  std::unordered_map<int, ClusterStats> cluster_stats;

  for (size_t i = 0; i < data.size(); ++i)
  {
    int cluster = clusters[i];
    auto it = cluster_stats.find(cluster);
    if (it == cluster_stats.end())
    {
      cluster_stats.emplace(cluster, ClusterStats(data[i].intensity));
    }
    else
    {
      it->second.update(data[i].intensity);
    }
  }

  // Find valid cluster indices
  std::vector<size_t> valid_indices;
  valid_indices.reserve(data.size() / 2);
  for (size_t i = 0; i < data.size(); ++i)
  {
    int cluster = clusters[i];
    const auto &stats = cluster_stats.at(cluster);
    if (stats.count > minTraces && stats.snr() > minSNR)
    {
      valid_indices.push_back(i);
    }
  }
  return valid_indices;
}

void nts::utils::sort_by_rt_inplace(std::vector<float> &rt, std::vector<float> &mz,
                                    std::vector<float> &intensity, std::vector<float> &noise,
                                    std::vector<int> &cluster)
{
  if (rt.empty())
    return;

  auto indices = get_sort_indices_float(rt);
  reorder_multiple_vectors(indices, rt, mz, intensity, noise, cluster);
}

void nts::utils::cluster_spectra_by_mz(const std::vector<float> &spec_rt,
                                       const std::vector<float> &spec_mz,
                                       const std::vector<float> &spec_intensity,
                                       const std::vector<float> &spec_noise,
                                       const float &ppmThreshold,
                                       int minTraces, float minSNR,
                                       std::vector<float> &final_rt,
                                       std::vector<float> &final_mz,
                                       std::vector<float> &final_intensity,
                                       std::vector<float> &final_noise,
                                       std::vector<int> &final_cluster,
                                       int &number_clusters)
{
  const size_t n = spec_mz.size();
  if (n == 0)
    return;

  auto mz_indices = get_sort_indices_float(spec_mz);
  std::vector<float> sorted_mz(n);
  for (size_t i = 0; i < n; ++i)
  {
    sorted_mz[i] = spec_mz[mz_indices[i]];
  }
  auto sorted_clusters = cluster_by_mz(sorted_mz, ppmThreshold);
  std::vector<int> clusters(n);
  for (size_t i = 0; i < n; ++i)
  {
    clusters[mz_indices[i]] = sorted_clusters[i];
  }

  std::vector<SpectraPoint> data;
  data.reserve(n);
  for (size_t i = 0; i < n; ++i)
  {
    data.emplace_back(spec_rt[i], spec_mz[i], spec_intensity[i], spec_noise[i], clusters[i]);
  }
  auto valid_indices = filter_valid_clusters(data, clusters, minTraces, minSNR);

  // Merge traces within same cluster and RT, keeping highest intensity
  std::map<std::pair<int, float>, size_t> cluster_rt_map; // (cluster, rt) -> index in data

  for (size_t idx : valid_indices)
  {
    const auto &point = data[idx];
    auto key = std::make_pair(point.cluster, point.rt);
    auto it = cluster_rt_map.find(key);

    if (it == cluster_rt_map.end())
    {
      // First trace for this cluster+rt combination
      cluster_rt_map[key] = idx;
    }
    else
    {
      // Already have a trace for this cluster+rt, keep the one with higher intensity
      size_t existing_idx = it->second;
      if (point.intensity > data[existing_idx].intensity)
      {
        cluster_rt_map[key] = idx; // Replace with higher intensity trace
      }
    }
  }

  // Build final output from merged traces
  const size_t final_size = cluster_rt_map.size();
  number_clusters = 0;
  final_rt.clear();
  final_rt.reserve(final_size);
  final_mz.clear();
  final_mz.reserve(final_size);
  final_intensity.clear();
  final_intensity.reserve(final_size);
  final_noise.clear();
  final_noise.reserve(final_size);
  final_cluster.clear();
  final_cluster.reserve(final_size);

  for (const auto &entry : cluster_rt_map)
  {
    size_t idx = entry.second;
    const auto &point = data[idx];
    if (point.cluster + 1 > number_clusters)
      number_clusters = point.cluster + 1;
    final_rt.push_back(point.rt);
    final_mz.push_back(point.mz);
    final_intensity.push_back(point.intensity);
    final_noise.push_back(point.noise);
    final_cluster.push_back(point.cluster);
  }
  sort_by_rt_inplace(final_rt, final_mz, final_intensity, final_noise, final_cluster);
}

// MARK: NOISE PROCESSING IMPLEMENTATIONS

std::vector<float> nts::utils::calculate_noise_levels(const std::vector<float> &intensities,
                                                      const AdaptiveNoiseParams &params,
                                                      float noise_threshold)
{
  const int n = intensities.size();
  std::vector<float> noise_levels(n);
  auto bin_assignments = calculate_bin_assignments(intensities, params.bins);
  std::vector<std::vector<float>> bin_data(params.bins);
  for (int i = 0; i < n; ++i)
  {
    bin_data[bin_assignments[i]].push_back(intensities[i]);
  }
  std::vector<float> bin_quantiles(params.bins, noise_threshold);
  for (int bin_idx = 0; bin_idx < params.bins; ++bin_idx)
  {
    if (!bin_data[bin_idx].empty())
    {
      float quantile_val = nts::utils::quantile(bin_data[bin_idx], params.quantile);
      float adjusted_threshold = quantile_val * params.threshold_multiplier;
      bin_quantiles[bin_idx] = std::max(adjusted_threshold, noise_threshold);
    }
  }
  for (int i = 0; i < n; ++i)
  {
    noise_levels[i] = bin_quantiles[bin_assignments[i]];
  }
  return noise_levels;
}

void nts::utils::filter_and_cluster(const std::vector<float> &raw_mz,
                                    const std::vector<float> &raw_intensity,
                                    const std::vector<float> &raw_noise,
                                    const float &ppmThreshold,
                                    std::vector<float> &final_mz,
                                    std::vector<float> &final_intensity,
                                    std::vector<float> &final_noise)
{
  auto valid_indices = filter_above_threshold(raw_intensity, raw_noise);
  if (valid_indices.empty())
    return;

  std::vector<float> filtered_mz, filtered_intensity, filtered_noise;
  const size_t filtered_size = valid_indices.size();
  filtered_mz.reserve(filtered_size);
  filtered_intensity.reserve(filtered_size);
  filtered_noise.reserve(filtered_size);

  for (size_t idx : valid_indices)
  {
    filtered_mz.push_back(raw_mz[idx]);
    filtered_intensity.push_back(raw_intensity[idx]);
    filtered_noise.push_back(raw_noise[idx]);
  }

  auto sort_indices = get_sort_indices_float(filtered_mz);
  reorder_multiple_vectors(sort_indices, filtered_mz, filtered_intensity, filtered_noise);
  auto clusters = cluster_by_mz(filtered_mz, ppmThreshold);

  // Aggregate by cluster (keep max intensity per cluster)
  std::unordered_map<int, std::tuple<float, float, float>> cluster_data;

  for (size_t i = 0; i < filtered_mz.size(); ++i)
  {
    int cluster = clusters[i];
    auto it = cluster_data.find(cluster);

    if (it == cluster_data.end() ||
        filtered_intensity[i] > std::get<1>(it->second))
    {
      cluster_data[cluster] = std::make_tuple(filtered_mz[i],
                                              filtered_intensity[i],
                                              filtered_noise[i]);
    }
  }

  // Collect final results
  final_mz.clear();
  final_mz.reserve(cluster_data.size());
  final_intensity.clear();
  final_intensity.reserve(cluster_data.size());
  final_noise.clear();
  final_noise.reserve(cluster_data.size());

  for (const auto &[cluster, data] : cluster_data)
  {
    final_mz.push_back(std::get<0>(data));
    final_intensity.push_back(std::get<1>(data));
    final_noise.push_back(std::get<2>(data));
  }
}

void nts::utils::denoise_spectra(
  sc::MS_FILE &ana,
  const int &spectrumIdx,
  const float &rt,
  const float &noiseThreshold,
  const int &minTraces,
  const float &ppmThreshold,
  std::vector<float> &spec_rt,
  std::vector<float> &spec_mz,
  std::vector<float> &spec_intensity,
  std::vector<float> &spec_noise,
  size_t &total_raw_points,
  size_t &total_clean_points,
  const int &debugSpecIdx,
  const float &baseQuantile)
{
  std::vector<std::vector<std::vector<float>>> single_spectrum = ana.get_spectra({spectrumIdx});
  std::vector<float> &raw_mz = single_spectrum[0][0];
  std::vector<float> &raw_intensity = single_spectrum[0][1];

  const int raw_n_traces = raw_mz.size();
  if (raw_n_traces < minTraces)
    return;

  total_raw_points += raw_n_traces;

  // Fast removal of zeros from intensity data for unbiased noise evaluation
  std::vector<float> non_zero_intensities;
  non_zero_intensities.reserve(raw_n_traces); // Reserve space to avoid reallocations

  for (const float intensity : raw_intensity) {
    if (intensity > 0.0f) {
      non_zero_intensities.push_back(intensity);
    }
  }

  // Use non-zero intensities for noise parameter estimation
  VectorStats stats(non_zero_intensities.empty() ? raw_intensity : non_zero_intensities);
  AdaptiveNoiseParams noise_params(stats, raw_n_traces, baseQuantile);

  auto raw_noise = nts::utils::calculate_noise_levels(raw_intensity, noise_params, noiseThreshold);

  bool should_debug = (debugSpecIdx >= 0 && spectrumIdx == debugSpecIdx);

  if (should_debug)
  {
    // Initialize debug log with dynamic filename based on spectrum index
    std::ostringstream log_filename;
    log_filename << "debug_log_denoising_spec" << spectrumIdx << "_rt" << std::fixed << std::setprecision(2) << rt << ".log";
    std::ostringstream header;
    header << "=== Denoising Debug Log for Spectrum " << spectrumIdx
           << " (RT=" << rt << "s) ===" << std::endl;
    init_debug_log(log_filename.str(), header.str());

    // Calculate noise level statistics for debug output
    float noise_mean = nts::utils::mean(raw_noise);
    float noise_stddev = nts::utils::standard_deviation(raw_noise, noise_mean);
    int zeros_removed = raw_n_traces - static_cast<int>(non_zero_intensities.size());
    float max_quantile = std::max(0.30f, baseQuantile * 1.2f);

    DEBUG_LOG("DEBUG Auto noise estimation: baseQuantile=" << baseQuantile
                << " -> adaptive_quantile=" << noise_params.quantile << "/" << max_quantile
                << ", bins=" << noise_params.bins
                << " (CV=" << stats.coefficient_variation << ", SNR=" << stats.signal_noise_ratio
                << ", n=" << raw_n_traces << ", zeros_removed=" << zeros_removed << ")" << std::endl);
    DEBUG_LOG("      Noise levels: mean=" << std::fixed << std::setprecision(2) << noise_mean
                << ", stddev=" << noise_stddev << std::endl);

    // Log raw spectra data (before denoising)
    DEBUG_LOG(std::endl << "Raw Spectra (before denoising): " << raw_n_traces << " traces" << std::endl);
    DEBUG_LOG("   m/z        Intensity    Noise" << std::endl);
    for (int i = 0; i < std::min(raw_n_traces, 100); ++i) // Limit to first 100 for readability
    {
      DEBUG_LOG("   " << std::fixed << std::setprecision(4) << std::setw(10) << raw_mz[i]
                << " " << std::setw(12) << std::setprecision(1) << raw_intensity[i]
                << " " << std::setw(10) << std::setprecision(1) << raw_noise[i] << std::endl);
    }
    if (raw_n_traces > 100)
    {
      DEBUG_LOG("   ... (" << (raw_n_traces - 100) << " more traces omitted)" << std::endl);
    }
  }

  std::vector<float> final_mz, final_intensity, final_noise;
  filter_and_cluster(raw_mz, raw_intensity, raw_noise, ppmThreshold, final_mz, final_intensity, final_noise);

  total_clean_points += final_mz.size();

  if (should_debug)
  {
    // Log cleaned spectra data (after denoising)
    DEBUG_LOG(std::endl << "Cleaned Spectra (after denoising): " << final_mz.size() << " traces" << std::endl);
    DEBUG_LOG("   m/z        Intensity    Noise" << std::endl);
    for (size_t i = 0; i < std::min(final_mz.size(), static_cast<size_t>(100)); ++i)
    {
      DEBUG_LOG("   " << std::fixed << std::setprecision(4) << std::setw(10) << final_mz[i]
                << " " << std::setw(12) << std::setprecision(1) << final_intensity[i]
                << " " << std::setw(10) << std::setprecision(1) << final_noise[i] << std::endl);
    }
    if (final_mz.size() > 100)
    {
      DEBUG_LOG("   ... (" << (final_mz.size() - 100) << " more traces omitted)" << std::endl);
    }

    // Summary statistics
    float reduction_percent = 100.0f * (1.0f - static_cast<float>(final_mz.size()) / static_cast<float>(raw_n_traces));
    DEBUG_LOG(std::endl << "Denoising summary: " << raw_n_traces << " -> " << final_mz.size()
              << " traces (" << std::setprecision(1) << reduction_percent << "% reduction)" << std::endl);

    close_debug_log();
  }

  // Add to output vectors
  for (size_t i = 0; i < final_mz.size(); ++i)
  {
    spec_rt.push_back(rt);
    spec_mz.push_back(final_mz[i]);
    spec_intensity.push_back(final_intensity[i]);
    spec_noise.push_back(final_noise[i]);
  }
}




// MARK: PEAK DETECTION



// MARK: calculate_baseline
std::vector<float> nts::utils::calculate_baseline(const std::vector<float> &intensity, int windowSize)
{
  const size_t n = intensity.size();
  std::vector<float> baseline(n);

  for (size_t i = 0; i < n; ++i)
  {
    size_t start_idx = (i >= static_cast<size_t>(windowSize)) ? i - windowSize : 0;
    size_t end_idx = std::min(n - 1, i + windowSize);

    float min_intensity = intensity[start_idx];
    for (size_t j = start_idx; j <= end_idx; ++j)
    {
      min_intensity = std::min(min_intensity, intensity[j]);
    }
    baseline[i] = min_intensity;
  }

  // Smooth baseline using 3-point moving average
  if (n >= 3)
  {
    std::vector<float> smoothed_baseline = baseline;
    for (size_t i = 1; i < n - 1; ++i)
    {
      smoothed_baseline[i] = (baseline[i-1] + baseline[i] + baseline[i+1]) / 3.0f;
    }
    baseline = std::move(smoothed_baseline);
  }

  return baseline;
}

// MARK: smooth_intensity_savitzky_golay
std::vector<float> nts::utils::smooth_intensity_savitzky_golay(
  const std::vector<float> &intensity,
  int windowSize,
  int polyOrder)
{
  const size_t n = intensity.size();
  std::vector<float> smoothed(n, 0.0f);

  if (windowSize < 3 || windowSize % 2 == 0 || polyOrder < 1 || polyOrder >= windowSize) {
    // Fallback to simple smoothing if parameters are invalid
    return smooth_intensity(intensity, windowSize);
  }

  int half_window = windowSize / 2;

  // Precompute the Savitzky-Golay convolution coefficients (for uniform spacing)
  // Use least-squares fit to a polynomial of given order
  // Reference: Numerical Recipes, or https://en.wikipedia.org/wiki/Savitzky–Golay_filter
  std::vector<float> coeffs(windowSize, 0.0f);
  {
    // Build the design matrix
    std::vector<std::vector<float>> A(windowSize, std::vector<float>(polyOrder + 1, 0.0f));
    for (int i = -half_window; i <= half_window; ++i) {
      for (int j = 0; j <= polyOrder; ++j) {
        A[i + half_window][j] = std::pow(static_cast<float>(i), j);
      }
    }
    // Compute (A^T A)^{-1} A^T for the center point (convolution coefficients)
    // Only need the first row of the pseudoinverse for smoothing
    std::vector<float> AtA(polyOrder + 1, 0.0f);
    std::vector<std::vector<float>> ATA(polyOrder + 1, std::vector<float>(polyOrder + 1, 0.0f));
    for (int i = 0; i <= polyOrder; ++i) {
      for (int j = 0; j <= polyOrder; ++j) {
        float sum = 0.0f;
        for (int k = 0; k < windowSize; ++k) {
          sum += A[k][i] * A[k][j];
        }
        ATA[i][j] = sum;
      }
    }
    // Invert ATA (small matrix, use Gaussian elimination)
    std::vector<std::vector<float>> inv_ATA = ATA;
    int m = polyOrder + 1;
    // Augment with identity
    for (int i = 0; i < m; ++i) {
      inv_ATA[i].resize(2 * m, 0.0f);
      inv_ATA[i][m + i] = 1.0f;
    }
    // Gaussian elimination
    for (int i = 0; i < m; ++i) {
      float diag = inv_ATA[i][i];
      if (std::abs(diag) < 1e-12f) continue;
      for (int j = 0; j < 2 * m; ++j) inv_ATA[i][j] /= diag;
      for (int k = 0; k < m; ++k) {
        if (k == i) continue;
        float factor = inv_ATA[k][i];
        for (int j = 0; j < 2 * m; ++j) {
          inv_ATA[k][j] -= factor * inv_ATA[i][j];
        }
      }
    }
    // Extract inverse
    std::vector<std::vector<float>> ATA_inv(m, std::vector<float>(m, 0.0f));
    for (int i = 0; i < m; ++i)
      for (int j = 0; j < m; ++j)
        ATA_inv[i][j] = inv_ATA[i][m + j];

    // Compute convolution coefficients for smoothing (center point)
    std::vector<float> B(m, 0.0f);
    for (int i = 0; i < windowSize; ++i) {
      B[0] += A[i][0];
    }
    for (int i = 0; i < windowSize; ++i) {
      for (int j = 0; j < m; ++j) {
        B[j] += A[i][j];
      }
    }
    // The smoothing coefficients are the first row of (ATA_inv * A^T) at the center
    for (int k = 0; k < windowSize; ++k) {
      float c = 0.0f;
      for (int j = 0; j < m; ++j) {
        c += ATA_inv[0][j] * A[k][j];
      }
      coeffs[k] = c;
    }
  }

  // Apply convolution (handle edges by reflecting)
  for (size_t i = 0; i < n; ++i) {
    float sum = 0.0f;
    for (int j = -half_window; j <= half_window; ++j) {
      int idx = static_cast<int>(i) + j;
      // Reflect at boundaries
      if (idx < 0) idx = -idx;
      if (idx >= static_cast<int>(n)) idx = 2 * static_cast<int>(n) - idx - 2;
      sum += coeffs[j + half_window] * intensity[idx];
    }
    smoothed[i] = sum;
  }

  return smoothed;
};

// MARK: smooth_intensity
std::vector<float> nts::utils::smooth_intensity(const std::vector<float> &intensity, int windowSize)
{
  const size_t n = intensity.size();
  std::vector<float> smoothed(n);
  int half_window = windowSize / 2;

  for (size_t i = 0; i < n; ++i)
  {
    size_t start_idx = (i >= static_cast<size_t>(half_window)) ? i - half_window : 0;
    size_t end_idx = std::min(n - 1, i + half_window);

    float sum = 0.0f;
    size_t count = 0;
    for (size_t j = start_idx; j <= end_idx; ++j)
    {
      sum += intensity[j];
      count++;
    }
    smoothed[i] = sum / count;
  }

  return smoothed;
}

// MARK: calculate_derivatives
void nts::utils::calculate_derivatives(const std::vector<float> &smoothed_intensity,
                                      std::vector<float> &first_derivative,
                                      std::vector<float> &second_derivative)
{
  const size_t n = smoothed_intensity.size();

  // Calculate first derivative
  first_derivative.clear();
  first_derivative.reserve(n - 1);
  for (size_t i = 0; i < n - 1; ++i)
  {
    first_derivative.push_back(smoothed_intensity[i + 1] - smoothed_intensity[i]);
  }

  // Calculate second derivative
  second_derivative.clear();
  second_derivative.reserve(first_derivative.size() - 1);
  for (size_t i = 0; i < first_derivative.size() - 1; ++i)
  {
    second_derivative.push_back(first_derivative[i + 1] - first_derivative[i]);
  }
}

// MARK: find_peak_candidates
std::vector<int> nts::utils::find_peak_candidates(const std::vector<float> &first_derivative,
                                                  const std::vector<float> &raw_intensity,
                                                  int refineWindow)
{
  std::vector<int> candidates;
  candidates.reserve(first_derivative.size() / 10); // Conservative estimate

  // Find where slope changes from positive to negative (zero-crossing)
  // Note: first_derivative[i] = smoothed[i+1] - smoothed[i]
  // When first_derivative[i-1] > 0 and first_derivative[i] <= 0:
  //   - slope between smoothed[i-1] and smoothed[i] is positive (going up)
  //   - slope between smoothed[i] and smoothed[i+1] is negative (going down)
  //   - therefore the peak is at smoothed[i]
  for (size_t i = 1; i < first_derivative.size(); ++i)
  {
    if (first_derivative[i] <= 0 && first_derivative[i-1] > 0)
    {
      // Peak detected at position i in the smoothed data
      int peak_pos = static_cast<int>(i);

      candidates.push_back(peak_pos);
    }
  }

  return candidates;
}

// MARK: validate_peak_candidates
std::vector<int> nts::utils::validate_peak_candidates(const std::vector<int> &candidates,
                                                      const std::vector<float> &first_derivative,
                                                      const std::vector<float> &second_derivative,
                                                      const std::vector<float> &smoothed_intensity,
                                                      const std::vector<float> &rt,
                                                      const std::vector<float> &intensity,
                                                      bool debug)
{
  std::vector<int> valid_peaks;
  valid_peaks.reserve(candidates.size());
  const int n = static_cast<int>(smoothed_intensity.size());

  if (debug)
  {
    DEBUG_LOG("    Validating " << candidates.size() << " peak candidates..." << std::endl);
  }

  for (int idx : candidates)
  {
    std::string reject_reason = "";

    // Check edge proximity - require minimum 2 points from edge
    const int min_edge_distance = 2;
    if (idx < min_edge_distance || idx >= n - min_edge_distance)
    {
      if (debug && !rt.empty() && idx < static_cast<int>(rt.size()))
      {
        reject_reason = "too close to edge";
      }
      continue;
    }

    // Check first derivative before peak (should be positive)
    // Extend to left until reaching below half peak intensity
    float apex_intensity = smoothed_intensity[idx];
    float half_apex = 0.5f * apex_intensity;
    int pre_start = idx - 1;
    while (pre_start >= 0 && smoothed_intensity[pre_start] >= half_apex)
    {
      pre_start--;
    }

    float pre_avg = 0.0f;
    int pre_count = 0;
    for (int i = pre_start; i < idx; ++i)
    {
      if (i >= 0 && i < static_cast<int>(first_derivative.size()))
      {
        pre_avg += first_derivative[i];
        pre_count++;
      }
    }
    if (pre_count > 0) pre_avg /= pre_count;

    // Check first derivative after peak (should be negative)
    // Extend to right until reaching below half peak intensity
    int post_end = idx + 1;
    while (post_end < n && smoothed_intensity[post_end] >= half_apex)
    {
      post_end++;
    }

    float post_avg = 0.0f;
    int post_count = 0;
    for (int i = idx; i <= post_end; ++i)
    {
      if (i >= 0 && i < static_cast<int>(first_derivative.size()))
      {
        post_avg += first_derivative[i];
        post_count++;
      }
    }
    if (post_count > 0) post_avg /= post_count;

    // Check second derivative at peak (should be negative)
    float d2_at_peak = 0.0f;
    if (idx - 1 >= 0 && idx - 1 < static_cast<int>(second_derivative.size()))
    {
      d2_at_peak = second_derivative[idx - 1];
    }

    // Check for higher apex in preceding or following traces (matching R implementation)
    // A peak candidate is only kept if it's a local maximum among ALL nearby points
    // Only points with strictly HIGHER intensity cause rejection; equal peaks are both kept
    bool pre_apex = false;
    bool post_apex = false;

    // Check pre_range: any point with intensity > current peak (strictly greater)
    // Equal intensity peaks will both be kept and merged in the merging step
    int pre_end = idx - 1;
    for (int i = pre_start; i <= pre_end; ++i)
    {
      if (i >= 0 && i < n && smoothed_intensity[i] > apex_intensity)
      {
        pre_apex = true;
        break;
      }
    }

    // Check post_range: any point with intensity > current peak (strictly greater)
    // Equal intensity peaks will both be kept and merged in the merging step
    int post_start = idx + 1;
    for (int i = post_start; i <= post_end; ++i)
    {
      if (i >= 0 && i < n && smoothed_intensity[i] > apex_intensity)
      {
        post_apex = true;
        break;
      }
    }

    // Build rejection reason if validation fails
    bool pre_valid = pre_avg > 0;
    bool post_valid = post_avg < 0;
    bool is_local_max = !pre_apex && !post_apex;
    if (!is_local_max) reject_reason = "not a local maximum";
    else if (!pre_valid) reject_reason = "d1_before not positive";
    else if (!post_valid) reject_reason = "d1_after not negative";

    if (pre_count > 0 && post_count > 0 && pre_valid && post_valid && is_local_max)
    {
      valid_peaks.push_back(idx);
      if (debug && !rt.empty() && idx < static_cast<int>(rt.size()))
      {
        DEBUG_LOG("        ✓ ACCEPTED: RT=" << std::fixed << std::setprecision(2) << rt[idx]
                   << ", intensity=" << std::setprecision(0) << intensity[idx]
                   << " | d1_before=" << std::setprecision(1) << pre_avg
                   << ", d1_after=" << post_avg
                   << ", d2=" << d2_at_peak << std::endl);
      }
    }
    else if (debug && !rt.empty() && idx < static_cast<int>(rt.size()))
    {
      DEBUG_LOG("        × REJECTED: RT=" << std::fixed << std::setprecision(2) << rt[idx]
                 << ", intensity=" << std::setprecision(0) << intensity[idx]
                 << " | d1_before=" << std::setprecision(1) << pre_avg
                 << ", d1_after=" << post_avg
                 << ", d2=" << d2_at_peak << ", "
                 << "REASON: " << reject_reason << std::endl);
    }
  }

  return valid_peaks;
}

// MARK: calculate_peak_boundaries
std::pair<int, int> nts::utils::calculate_peak_boundaries(int peak_idx,
                                                         const std::vector<float> &rt,
                                                         const std::vector<float> &smoothed_intensity,
                                                         const std::vector<float> &baseline,
                                                         float max_half_width,
                                                         int min_traces,
                                                         float cycle_time,
                                                         bool debug,
                                                         float debugMZ)
{
  const int n = static_cast<int>(rt.size());
  float apex_intensity = smoothed_intensity[peak_idx];
  float min_intensity_threshold = 0.01f * apex_intensity;

  // Left boundary - detect valleys by looking for local minima
  // A valley is a point where intensity is lower than neighbors and then starts increasing
  int left_idx = peak_idx;
  std::string left_stop_reason = "";

  // Move left from apex, looking for valley points
  for (int i = peak_idx - 1; i >= 0; --i)
  {
    // Stop if there is a large RT gap (likely a break in the trace)
    if (i < peak_idx - 1 && rt[i + 1] - rt[i] > max_half_width) {
      left_idx = i + 1; // Stop at the trace closest to the apex
      left_stop_reason = "gap > max_half_width";
      break;
    }
    // Check max_half_width constraint
    if (rt[peak_idx] - rt[i] > max_half_width)
    {
      left_idx = i + 1; // Stop at previous point (inside the limit)
      left_stop_reason = "max_half_width exceeded";
      break;
    }

    // Stop if at or below baseline (with 10% tolerance)
    if (smoothed_intensity[i] <= baseline[i] * 1.1f)
    {
      left_idx = i;
      left_stop_reason = "intensity <= baseline * 1.1";
      break;
    }

    if (smoothed_intensity[i] > apex_intensity * 1.2f) {
      left_idx = i;
      left_stop_reason = "intensity > 120% of apex (rising into another peak)";
      break;
    }

    // Stop if intensity < 1% of apex
    if (smoothed_intensity[i] <= min_intensity_threshold)
    {
      left_idx = i;
      left_stop_reason = "intensity <= 1% of apex";
      break;
    }

    // Check for sustained rising trend (without requiring a valley)
    // This detects when we're entering another peak's right side while moving left
    if (i >= 4 && i < peak_idx - 1)
    {
      // Look at 4 consecutive points starting from current position
      float pt1 = smoothed_intensity[i]; // Current position
      float pt2 = smoothed_intensity[i - 1];
      float pt3 = smoothed_intensity[i - 2];
      float pt4 = smoothed_intensity[i - 3];     // Furthest left

      // Check for consistent rising trend over 4 points after current position
      // If all 4 points are consecutively increasing, we're likely on another peak's ascending edge
      bool is_rising = (pt2 > pt1) && (pt3 > pt2) && (pt4 > pt3);

      if (is_rising)
      {
        // is the valey detected in pt1 at least below half of the apex intensity?
        bool valley_below_half_apex = pt1 < (apex_intensity * 0.5f);
        if (valley_below_half_apex) {
          left_idx = i;
          left_stop_reason = "sustained left rising trend (entering another peak), pt1=" + std::to_string(pt1);
          break;
        }
      }
    }

    left_idx = i;
  }
  if (left_idx <= 0 && left_stop_reason.empty()) left_stop_reason = "reached start of cluster";

  // Right boundary - detect valleys by looking for local minima
  // A valley is a point where intensity is lower than neighbors and then starts increasing
  int right_idx = peak_idx;
  std::string right_stop_reason = "";

  // Move right from apex, looking for valley points
  for (int i = peak_idx + 1; i < n; ++i)
  {
    // Stop if there is a large RT gap (likely a break in the trace)
    if (i > peak_idx + 1 && rt[i] - rt[i - 1] > max_half_width) {
      right_idx = i - 1;
      right_stop_reason = "gap > max_half_width";
      break;
    }
    // Check max_half_width constraint
    if (rt[i] - rt[peak_idx] > max_half_width)
    {
      right_idx = i - 1; // Stop at previous point (inside the limit)
      right_stop_reason = "max_half_width exceeded";
      break;
    }

    // Stop if at or below baseline (with 10% tolerance)
    if (smoothed_intensity[i] <= baseline[i] * 1.1f)
    {
      right_idx = i;
      right_stop_reason = "intensity <= baseline * 1.1";
      break;
    }

    if (smoothed_intensity[i] > apex_intensity * 1.2f) {
      right_idx = i;
      right_stop_reason = "intensity > 120% of apex (rising into another peak)";
      break;
    }

    // Stop if intensity < 1% of apex
    if (smoothed_intensity[i] <= min_intensity_threshold)
    {
      right_idx = i;
      right_stop_reason = "intensity <= 1% of apex";
      break;
    }

    // Check for sustained rising trend (without requiring a valley)
    // This detects when we're entering another peak's left side while moving right
    if (i + 3 < n && i > peak_idx + 1)
    {
      // Look at 4 consecutive points starting from current position
      float pt1 = smoothed_intensity[i];     // Current position
      float pt2 = smoothed_intensity[i + 1];
      float pt3 = smoothed_intensity[i + 2];
      float pt4 = smoothed_intensity[i + 3]; // Furthest ahead

      // Check for consistent rising trend over 4 points
      // If all 4 points are consecutively increasing, we're likely on another peak's ascending edge
      bool is_rising = (pt2 > pt1) && (pt3 > pt2) && (pt4 > pt3);

      if (is_rising)
      {
        // is the valey detected in pt1 at least below half of the apex intensity?
        bool valley_below_half_apex = (pt1 < (apex_intensity * 0.5f));
        if (valley_below_half_apex) {
          right_idx = i;
          right_stop_reason = "sustained right rising trend (entering another peak), pt1=" + std::to_string(pt1);
          break;
        }
      }
    }

    right_idx = i;
  }
  if (right_idx >= n - 1 && right_stop_reason.empty()) right_stop_reason = "reached end of cluster";

  if (debug)
  {
    DEBUG_LOG("        Boundary calculation: apex_idx=" << peak_idx
               << " (RT=" << std::fixed << std::setprecision(2) << rt[peak_idx]
               << ", intensity=" << std::setprecision(0) << apex_intensity << ")" << std::endl);
    DEBUG_LOG("          Left: " << left_idx << " → " << peak_idx
               << " (stopped: " << left_stop_reason << ")" << std::endl);
    DEBUG_LOG("          Right: " << peak_idx << " → " << right_idx
               << " (stopped: " << right_stop_reason << ")" << std::endl);
    DEBUG_LOG("          Thresholds: max_half_width=" << max_half_width
               << "s, min_intensity=" << min_intensity_threshold
               << " (1% of apex)" << std::endl);
  }

  return std::make_pair(left_idx, right_idx);
}

// MARK: calculate_fwhm_boundaries
std::pair<int, int> nts::utils::calculate_fwhm_boundaries(int peak_idx,
                                                         const std::vector<float> &rt,
                                                         const std::vector<float> &intensity,
                                                         int left_boundary, int right_boundary)
{
  // Extract peak region data for FWHM calculation
  std::vector<float> peak_rt, peak_intensity;
  for (int i = left_boundary; i <= right_boundary; ++i)
  {
    peak_rt.push_back(rt[i]);
    peak_intensity.push_back(intensity[i]);
  }

  // Calculate FWHM using existing function (we only need the RT component)
  auto [fwhm_rt_val, fwhm_mz_val, mean_mz_unused] = calculate_fwhm_combined(peak_rt, peak_rt, peak_intensity); // Use peak_rt for both RT and MZ

  // Find FWHM boundaries in the cluster indices
  float rt_at_peak = rt[peak_idx];
  float half_fwhm = fwhm_rt_val / 2.0f;
  float fwhm_left_rt = rt_at_peak - half_fwhm;
  float fwhm_right_rt = rt_at_peak + half_fwhm;

  // Find closest indices for FWHM boundaries
  int fwhm_left_idx = left_boundary;
  int fwhm_right_idx = right_boundary;

  for (int i = left_boundary; i <= right_boundary; ++i)
  {
    if (rt[i] >= fwhm_left_rt)
    {
      fwhm_left_idx = i;
      break;
    }
  }

  for (int i = right_boundary; i >= left_boundary; --i)
  {
    if (rt[i] <= fwhm_right_rt)
    {
      fwhm_right_idx = i;
      break;
    }
  }

  return std::make_pair(fwhm_left_idx, fwhm_right_idx);
}

// MARK: calculate_peak_area
float nts::utils::calculate_peak_area(const std::vector<float> &rt, const std::vector<float> &intensity)
{
  if (rt.size() < 2 || rt.size() != intensity.size())
    return 0.0f;

  float area = 0.0f;
  for (size_t i = 1; i < rt.size(); ++i)
  {
    float dx = rt[i] - rt[i - 1];
    float avg_intensity = (intensity[i] + intensity[i - 1]) / 2.0f;
    area += dx * avg_intensity;
  }

  return std::max(0.0f, area);
}

// MARK: calculate_fwhm_combined
std::tuple<float, float, float> nts::utils::calculate_fwhm_combined(const std::vector<float> &rt,
                                                                      const std::vector<float> &mz,
                                                                      const std::vector<float> &intensity)
{
  if (rt.size() != intensity.size() || mz.size() != intensity.size() || rt.empty())
    return std::make_tuple(0.0f, 0.0f, 0.0f);

  // Find maximum intensity and its position
  auto max_it = std::max_element(intensity.begin(), intensity.end());
  float max_intensity = *max_it;
  size_t max_idx = std::distance(intensity.begin(), max_it);

  // Estimate baseline as the minimum of the first and last points (edge intensities)
  float baseline = std::min(intensity.front(), intensity.back());

  // Calculate half-maximum as half the peak height above baseline
  float peak_height = max_intensity - baseline;
  float half_max = baseline + (peak_height / 2.0f);

  // Find left and right indices where intensity drops to half maximum
  size_t left_idx = max_idx;
  size_t right_idx = max_idx;

  // Search left from apex
  while (left_idx > 0 && intensity[left_idx] > half_max)
  {
    left_idx--;
  }

  // Search right from apex
  while (right_idx < intensity.size() - 1 && intensity[right_idx] > half_max)
  {
    right_idx++;
  }

  float fwhm_rt, fwhm_mz, mean_mz_fwhm;

  // Calculate FWHM in RT dimension (RT difference at half maximum boundaries)
  if (left_idx < right_idx && right_idx < rt.size())
  {
    fwhm_rt = rt[right_idx] - rt[left_idx];
  }
  else
  {
    fwhm_rt = rt.back() - rt.front(); // fallback to total RT width
  }

  // Calculate FWHM in m/z dimension and mean m/z within the RT FWHM region
  if (left_idx < right_idx && right_idx < mz.size())
  {
    // Find min, max, and mean m/z values within the FWHM RT range
    float min_mz = mz[left_idx];
    float max_mz = mz[left_idx];
    float sum_mz = 0.0f;
    size_t count = 0;

    for (size_t i = left_idx; i <= right_idx; ++i)
    {
      min_mz = std::min(min_mz, mz[i]);
      max_mz = std::max(max_mz, mz[i]);
      sum_mz += mz[i];
      count++;
    }

    fwhm_mz = max_mz - min_mz;
    mean_mz_fwhm = (count > 0) ? sum_mz / count : mz[max_idx];
  }
  else
  {
    fwhm_mz = mz.back() - mz.front(); // fallback to total m/z width
    mean_mz_fwhm = mz[max_idx]; // fallback to apex m/z
  }

  return std::make_tuple(fwhm_rt, fwhm_mz, mean_mz_fwhm);
}

// MARK: calculate_fwhm_rt
// Simple FWHM calculation for RT dimension only
float nts::utils::calculate_fwhm_rt(const std::vector<float> &rt, const std::vector<float> &intensity)
{
  if (rt.empty() || intensity.empty() || rt.size() != intensity.size())
    return 0.0f;

  // Find maximum intensity and its position
  auto max_it = std::max_element(intensity.begin(), intensity.end());
  float max_intensity = *max_it;
  size_t max_idx = std::distance(intensity.begin(), max_it);
  float half_max = max_intensity / 2.0f;

  // Find left and right indices where intensity drops to half maximum
  size_t left_idx = max_idx;
  size_t right_idx = max_idx;

  // Search left from apex
  while (left_idx > 0 && intensity[left_idx] > half_max)
    left_idx--;

  // Search right from apex
  while (right_idx < intensity.size() - 1 && intensity[right_idx] > half_max)
    right_idx++;

  // Calculate FWHM in RT dimension
  if (left_idx < right_idx && right_idx < rt.size())
    return rt[right_idx] - rt[left_idx];
  else
    return rt.back() - rt.front(); // fallback to total RT width
}

// MARK: fit_gaussian
void nts::utils::fit_gaussian(const std::vector<float> &x, const std::vector<float> &y,
                             float &A, float &mu, float &sigma, float &baseline)
{
  // Adam optimizer parameters
  const float alpha = 0.01f;   // Learning rate
  const float beta1 = 0.9f;    // First moment decay rate
  const float beta2 = 0.999f;  // Second moment decay rate
  const float epsilon = 1e-8f; // Small value to prevent division by zero
  const int max_iterations = 500;

  float m_A = 0.0f, v_A = 0.0f, m_mu = 0.0f, v_mu = 0.0f, m_sigma = 0.0f, v_sigma = 0.0f;
  float m_baseline = 0.0f, v_baseline = 0.0f;

  for (int iter = 1; iter <= max_iterations; ++iter)
  {
    float grad_A = 0.0f, grad_mu = 0.0f, grad_sigma = 0.0f, grad_baseline = 0.0f;

    // Calculate gradients
    for (size_t i = 0; i < x.size(); ++i)
    {
      float exp_term = std::exp(-std::pow(x[i] - mu, 2) / (2 * std::pow(sigma, 2)));
      float y_pred = baseline + A * exp_term;
      float error = y[i] - y_pred;

      grad_A += -2 * error * exp_term;
      grad_mu += -2 * error * A * exp_term * (x[i] - mu) / std::pow(sigma, 2);
      grad_sigma += -2 * error * A * exp_term * std::pow(x[i] - mu, 2) / std::pow(sigma, 3);
      grad_baseline += -2 * error;
    }

    // Adam update for A
    m_A = beta1 * m_A + (1 - beta1) * grad_A;
    v_A = beta2 * v_A + (1 - beta2) * grad_A * grad_A;
    float A_hat = m_A / (1 - std::pow(beta1, iter));
    float v_A_hat = v_A / (1 - std::pow(beta2, iter));
    A -= alpha * A_hat / (std::sqrt(v_A_hat) + epsilon);
    A = std::max(0.1f, A); // Keep positive

    // Adam update for mu
    m_mu = beta1 * m_mu + (1 - beta1) * grad_mu;
    v_mu = beta2 * v_mu + (1 - beta2) * grad_mu * grad_mu;
    float mu_hat = m_mu / (1 - std::pow(beta1, iter));
    float v_mu_hat = v_mu / (1 - std::pow(beta2, iter));
    mu -= alpha * mu_hat / (std::sqrt(v_mu_hat) + epsilon);

    // Adam update for sigma
    m_sigma = beta1 * m_sigma + (1 - beta1) * grad_sigma;
    v_sigma = beta2 * v_sigma + (1 - beta2) * grad_sigma * grad_sigma;
    float sigma_hat = m_sigma / (1 - std::pow(beta1, iter));
    float v_sigma_hat = v_sigma / (1 - std::pow(beta2, iter));
    sigma -= alpha * sigma_hat / (std::sqrt(v_sigma_hat) + epsilon);
    sigma = std::max(0.1f, std::min(sigma, 100.0f)); // Constrain sigma

    // Adam update for baseline
    m_baseline = beta1 * m_baseline + (1 - beta1) * grad_baseline;
    v_baseline = beta2 * v_baseline + (1 - beta2) * grad_baseline * grad_baseline;
    float baseline_hat = m_baseline / (1 - std::pow(beta1, iter));
    float v_baseline_hat = v_baseline / (1 - std::pow(beta2, iter));
    baseline -= alpha * baseline_hat / (std::sqrt(v_baseline_hat) + epsilon);
    baseline = std::max(0.0f, baseline); // Keep non-negative
  }
}

// MARK: calculate_gaussian_rsquared
float nts::utils::calculate_gaussian_rsquared(const std::vector<float> &x, const std::vector<float> &y,
                                             float A, float mu, float sigma, float baseline)
{
  if (x.empty() || y.empty() || x.size() != y.size())
    return 0.0f;

  float ss_total = 0.0f;
  float ss_residual = 0.0f;
  float mean_y = nts::utils::mean(y);

  for (size_t i = 0; i < x.size(); ++i)
  {
    float y_pred = nts::utils::gaussian_function_with_baseline(A, mu, sigma, baseline, x[i]);
    ss_residual += std::pow(y[i] - y_pred, 2);
    ss_total += std::pow(y[i] - mean_y, 2);
  }

  if (ss_total == 0.0f)
    return 0.0f;

  float r2 = 1.0f - (ss_residual / ss_total);
  // Don't clamp - negative R² indicates fit worse than mean (important diagnostic!)
  return r2;
}

// MARK: fit_emg
// Fit EMG (Exponentially Modified Gaussian) using Adam optimizer
// Optimizes A, mu, sigma, lambda, baseline
void nts::utils::fit_emg(const std::vector<float> &t, const std::vector<float> &y,
                        float &A, float &mu, float &sigma, float &lambda, float &baseline)
{
  if (t.size() != y.size() || t.empty()) return;

  // Adam optimizer parameters
  const float alpha = 0.01f;      // learning rate (smaller for stability with 5 params)
  const float beta1 = 0.9f;       // momentum
  const float beta2 = 0.999f;     // RMSprop
  const float epsilon = 1e-8f;
  const int max_iterations = 1000;

  // Initialize Adam moments for each parameter
  float m_A = 0.0f, v_A = 0.0f;
  float m_mu = 0.0f, v_mu = 0.0f;
  float m_sigma = 0.0f, v_sigma = 0.0f;
  float m_lambda = 0.0f, v_lambda = 0.0f;
  float m_baseline = 0.0f, v_baseline = 0.0f;

  for (int iter = 1; iter <= max_iterations; ++iter)
  {
    // Compute gradients using numerical differentiation
    const float h = 1e-4f;
    float grad_A = 0.0f, grad_mu = 0.0f, grad_sigma = 0.0f, grad_lambda = 0.0f, grad_baseline = 0.0f;

    for (size_t i = 0; i < t.size(); ++i)
    {
      float y_pred = emg_function(A, mu, sigma, lambda, baseline, t[i]);
      float error = y[i] - y_pred;

      // Numerical gradients
      float y_pred_A_plus = emg_function(A + h, mu, sigma, lambda, baseline, t[i]);
      float y_pred_mu_plus = emg_function(A, mu + h, sigma, lambda, baseline, t[i]);
      float y_pred_sigma_plus = emg_function(A, mu, sigma + h, lambda, baseline, t[i]);
      float y_pred_lambda_plus = emg_function(A, mu, sigma, lambda + h, baseline, t[i]);
      float y_pred_baseline_plus = emg_function(A, mu, sigma, lambda, baseline + h, t[i]);

      grad_A += -2.0f * error * (y_pred_A_plus - y_pred) / h;
      grad_mu += -2.0f * error * (y_pred_mu_plus - y_pred) / h;
      grad_sigma += -2.0f * error * (y_pred_sigma_plus - y_pred) / h;
      grad_lambda += -2.0f * error * (y_pred_lambda_plus - y_pred) / h;
      grad_baseline += -2.0f * error * (y_pred_baseline_plus - y_pred) / h;
    }

    // Adam updates for A
    m_A = beta1 * m_A + (1.0f - beta1) * grad_A;
    v_A = beta2 * v_A + (1.0f - beta2) * grad_A * grad_A;
    float A_hat = m_A / (1.0f - std::pow(beta1, iter));
    float v_A_hat = v_A / (1.0f - std::pow(beta2, iter));
    A -= alpha * A_hat / (std::sqrt(v_A_hat) + epsilon);
    A = std::max(0.1f, A); // Keep positive

    // Adam updates for mu
    m_mu = beta1 * m_mu + (1.0f - beta1) * grad_mu;
    v_mu = beta2 * v_mu + (1.0f - beta2) * grad_mu * grad_mu;
    float mu_hat = m_mu / (1.0f - std::pow(beta1, iter));
    float v_mu_hat = v_mu / (1.0f - std::pow(beta2, iter));
    mu -= alpha * mu_hat / (std::sqrt(v_mu_hat) + epsilon);
    mu = std::max(t.front(), std::min(t.back(), mu)); // Keep within RT range

    // Adam updates for sigma
    m_sigma = beta1 * m_sigma + (1.0f - beta1) * grad_sigma;
    v_sigma = beta2 * v_sigma + (1.0f - beta2) * grad_sigma * grad_sigma;
    float sigma_hat = m_sigma / (1.0f - std::pow(beta1, iter));
    float v_sigma_hat = v_sigma / (1.0f - std::pow(beta2, iter));
    sigma -= alpha * sigma_hat / (std::sqrt(v_sigma_hat) + epsilon);
    sigma = std::max(0.01f, std::min(100.0f, sigma)); // Keep reasonable

    // Adam updates for lambda
    m_lambda = beta1 * m_lambda + (1.0f - beta1) * grad_lambda;
    v_lambda = beta2 * v_lambda + (1.0f - beta2) * grad_lambda * grad_lambda;
    float lambda_hat = m_lambda / (1.0f - std::pow(beta1, iter));
    float v_lambda_hat = v_lambda / (1.0f - std::pow(beta2, iter));
    lambda -= alpha * lambda_hat / (std::sqrt(v_lambda_hat) + epsilon);
    lambda = std::max(0.001f, std::min(2.0f, lambda)); // Keep positive and reasonable (typical 0.01-0.5)

    // Adam updates for baseline
    m_baseline = beta1 * m_baseline + (1.0f - beta1) * grad_baseline;
    v_baseline = beta2 * v_baseline + (1.0f - beta2) * grad_baseline * grad_baseline;
    float baseline_hat = m_baseline / (1.0f - std::pow(beta1, iter));
    float v_baseline_hat = v_baseline / (1.0f - std::pow(beta2, iter));
    baseline -= alpha * baseline_hat / (std::sqrt(v_baseline_hat) + epsilon);
    baseline = std::max(0.0f, baseline); // Keep non-negative
  }
}

// MARK: calculate_emg_rsquared
// Calculate R² for EMG fit
float nts::utils::calculate_emg_rsquared(const std::vector<float> &t, const std::vector<float> &y,
                                        float A, float mu, float sigma, float lambda, float baseline)
{
  if (t.empty() || y.empty() || t.size() != y.size())
    return 0.0f;

  float ss_total = 0.0f;
  float ss_residual = 0.0f;
  float mean_y = nts::utils::mean(y);

  for (size_t i = 0; i < t.size(); ++i)
  {
    float y_pred = emg_function(A, mu, sigma, lambda, baseline, t[i]);
    ss_residual += std::pow(y[i] - y_pred, 2);
    ss_total += std::pow(y[i] - mean_y, 2);
  }

  if (ss_total == 0.0f)
    return 0.0f;

  float r2 = 1.0f - (ss_residual / ss_total);
  // Don't clamp - negative R² indicates fit worse than mean
  return r2;
}

// MARK: fit_skew_gaussian
// Fit skew-Gaussian using Levenberg-Marquardt-like optimization
// Starting from initial guesses, optimizes A, xi, omega, alpha
void nts::utils::fit_skew_gaussian(const std::vector<float> &t, const std::vector<float> &y,
                                   float &A, float &xi, float &omega, float &alpha)
{
  // Adam optimizer parameters (works well for skew-Gaussian)
  const float learning_rate = 0.01f;
  const float beta1 = 0.9f;
  const float beta2 = 0.999f;
  const float epsilon = 1e-8f;
  const int max_iterations = 500; // More iterations for 4 parameters

  // Initialize moment vectors for Adam
  float m_A = 0.0f, v_A = 0.0f;
  float m_xi = 0.0f, v_xi = 0.0f;
  float m_omega = 0.0f, v_omega = 0.0f;
  float m_alpha = 0.0f, v_alpha = 0.0f;

  for (int iter = 1; iter <= max_iterations; ++iter)
  {
    float grad_A = 0.0f, grad_xi = 0.0f, grad_omega = 0.0f, grad_alpha = 0.0f;

    // Calculate gradients via numerical differentiation (simple and robust)
    const float h = 1e-4f; // Step size for numerical gradient

    for (size_t i = 0; i < t.size(); ++i)
    {
      float y_pred = skew_gaussian_function(A, xi, omega, alpha, t[i]);
      float error = y[i] - y_pred;

      // Numerical gradients
      float y_pred_A_plus = skew_gaussian_function(A + h, xi, omega, alpha, t[i]);
      grad_A += -2.0f * error * (y_pred_A_plus - y_pred) / h;

      float y_pred_xi_plus = skew_gaussian_function(A, xi + h, omega, alpha, t[i]);
      grad_xi += -2.0f * error * (y_pred_xi_plus - y_pred) / h;

      float y_pred_omega_plus = skew_gaussian_function(A, xi, omega + h, alpha, t[i]);
      grad_omega += -2.0f * error * (y_pred_omega_plus - y_pred) / h;

      float y_pred_alpha_plus = skew_gaussian_function(A, xi, omega, alpha + h, t[i]);
      grad_alpha += -2.0f * error * (y_pred_alpha_plus - y_pred) / h;
    }

    // Adam update for A
    m_A = beta1 * m_A + (1.0f - beta1) * grad_A;
    v_A = beta2 * v_A + (1.0f - beta2) * grad_A * grad_A;
    float m_A_hat = m_A / (1.0f - std::pow(beta1, iter));
    float v_A_hat = v_A / (1.0f - std::pow(beta2, iter));
    A -= learning_rate * m_A_hat / (std::sqrt(v_A_hat) + epsilon);

    // Adam update for xi
    m_xi = beta1 * m_xi + (1.0f - beta1) * grad_xi;
    v_xi = beta2 * v_xi + (1.0f - beta2) * grad_xi * grad_xi;
    float m_xi_hat = m_xi / (1.0f - std::pow(beta1, iter));
    float v_xi_hat = v_xi / (1.0f - std::pow(beta2, iter));
    xi -= learning_rate * m_xi_hat / (std::sqrt(v_xi_hat) + epsilon);

    // Adam update for omega
    m_omega = beta1 * m_omega + (1.0f - beta1) * grad_omega;
    v_omega = beta2 * v_omega + (1.0f - beta2) * grad_omega * grad_omega;
    float m_omega_hat = m_omega / (1.0f - std::pow(beta1, iter));
    float v_omega_hat = v_omega / (1.0f - std::pow(beta2, iter));
    omega -= learning_rate * m_omega_hat / (std::sqrt(v_omega_hat) + epsilon);

    // Adam update for alpha
    m_alpha = beta1 * m_alpha + (1.0f - beta1) * grad_alpha;
    v_alpha = beta2 * v_alpha + (1.0f - beta2) * grad_alpha * grad_alpha;
    float m_alpha_hat = m_alpha / (1.0f - std::pow(beta1, iter));
    float v_alpha_hat = v_alpha / (1.0f - std::pow(beta2, iter));
    alpha -= learning_rate * m_alpha_hat / (std::sqrt(v_alpha_hat) + epsilon);

    // Constrain parameters to reasonable bounds
    A = std::max(0.1f, A); // Amplitude must be positive
    omega = std::max(0.01f, std::min(omega, 1000.0f)); // Width must be positive
    alpha = std::max(-10.0f, std::min(alpha, 10.0f)); // Skewness bounded
  }
}

// MARK: calculate_skew_gaussian_rsquared
// Calculate R² for skew-Gaussian fit
float nts::utils::calculate_skew_gaussian_rsquared(const std::vector<float> &t, const std::vector<float> &y,
                                                   float A, float xi, float omega, float alpha)
{
  if (t.empty() || y.empty() || t.size() != y.size())
    return 0.0f;

  float ss_total = 0.0f;
  float ss_residual = 0.0f;
  float mean_y = nts::utils::mean(y);

  for (size_t i = 0; i < t.size(); ++i)
  {
    float y_pred = nts::utils::skew_gaussian_function(A, xi, omega, alpha, t[i]);
    ss_residual += std::pow(y[i] - y_pred, 2);
    ss_total += std::pow(y[i] - mean_y, 2);
  }

  if (ss_total == 0.0f)
    return 0.0f;

  // R² = 1 - (SS_residual / SS_total)
  // Can be negative if fit is worse than mean, so don't clamp to [0,1]
  return 1.0f - (ss_residual / ss_total);
}




// MARK: POLARITY-SPECIFIC PROCESSING FUNCTIONS



// MARK: process_polarity_clusters
std::vector<nts::FEATURE> nts::utils::process_polarity_clusters(
    const std::vector<float> &clust_rt,
    const std::vector<float> &clust_mz,
    const std::vector<float> &clust_intensity,
    const std::vector<float> &clust_noise,
    const std::vector<int> &clust_cluster,
    int number_clusters,
    int polarity_sign,
    const std::string &adduct_name,
    float mass_correction,
    int minTraces,
    float minSNR,
    float baselineWindow,
    float maxWidth,
    const std::string &analysis_name,
    float debugMZ)
{
  // Initialize debug log file with dynamic filename based on debugMZ
  if (debugMZ > 0.0f)
  {
    std::string filename = "log/debug_log_peak_detection_" + std::to_string(debugMZ) + ".log";
    std::ostringstream header;
    header << "=== Peak Detection Debug Log (m/z = " << std::fixed << std::setprecision(4)
           << debugMZ << ") ===\n";
    init_debug_log(filename, header.str());
  }

  std::map<int, std::vector<int>> cluster_indices;
  for (size_t i = 0; i < clust_cluster.size(); ++i)
  {
    cluster_indices[clust_cluster[i]].push_back(static_cast<int>(i));
  }

  std::vector<nts::FEATURE> polarity_features;

  for (const auto& [cluster_id, indices] : cluster_indices)
  {
    if (indices.size() < static_cast<size_t>(minTraces))
      continue;

    // Enable debug mode only if debugMZ is greater than 0
    bool debug = (debugMZ > 0.0f);

    // Check if this cluster contains the target m/z within its range
    bool cluster_matches_debug_mz = false;
    if (debug && !indices.empty())
    {
      // Find min and max m/z of cluster
      float cluster_min_mz = clust_mz[indices[0]];
      float cluster_max_mz = clust_mz[indices[0]];
      float cluster_mean_mz = 0.0f;

      for (int idx : indices)
      {
        float mz = clust_mz[idx];
        cluster_min_mz = std::min(cluster_min_mz, mz);
        cluster_max_mz = std::max(cluster_max_mz, mz);
        cluster_mean_mz += mz;
      }
      cluster_mean_mz /= indices.size();

      // Check if debugMZ falls within the cluster's m/z range
      if (debugMZ >= cluster_min_mz && debugMZ <= cluster_max_mz)
      {
        cluster_matches_debug_mz = true;
        DEBUG_LOG("DEBUG Processing cluster " << cluster_id << " with " << indices.size()
                    << " traces (polarity: " << polarity_sign << ")" << std::endl);
        DEBUG_LOG("      Cluster m/z range: " << std::fixed << std::setprecision(4)
                    << cluster_min_mz << " to " << cluster_max_mz
                    << " (mean: " << cluster_mean_mz << ")" << std::endl);
        DEBUG_LOG("      Target m/z " << debugMZ << " is within cluster range" << std::endl);
        std::string debug_filename = "log/debug_log_peak_detection_" + std::to_string(debugMZ) + ".log";
        Rcpp::Rcout << "      Processing cluster " << cluster_id << " - detailed output in: " << debug_filename << std::endl;
      }
    }

    std::vector<float> cluster_rt, cluster_mz, cluster_intensity, cluster_noise;
    cluster_rt.reserve(indices.size());
    cluster_mz.reserve(indices.size());
    cluster_intensity.reserve(indices.size());
    cluster_noise.reserve(indices.size());
    for (int idx : indices)
    {
      cluster_rt.push_back(clust_rt[idx]);
      cluster_mz.push_back(clust_mz[idx]);
      cluster_intensity.push_back(clust_intensity[idx]);
      cluster_noise.push_back(clust_noise[idx]);
    }

    // DEBUG: Log raw cluster data for plotting
    if (cluster_matches_debug_mz)
    {
      DEBUG_LOG("      RAW CLUSTER DATA (for plotting):" << std::endl);
      DEBUG_LOG("      cluster_rt <- c(");
      for (size_t i = 0; i < cluster_rt.size(); ++i)
      {
        DEBUG_LOG(std::fixed << std::setprecision(4) << cluster_rt[i]);
        if (i < cluster_rt.size() - 1) DEBUG_LOG(", ");
      }
      DEBUG_LOG(")" << std::endl);

      DEBUG_LOG("      cluster_mz <- c(");
      for (size_t i = 0; i < cluster_mz.size(); ++i)
      {
        DEBUG_LOG(std::fixed << std::setprecision(4) << cluster_mz[i]);
        if (i < cluster_mz.size() - 1) DEBUG_LOG(", ");
      }
      DEBUG_LOG(")" << std::endl);

      DEBUG_LOG("      cluster_intensity <- c(");
      for (size_t i = 0; i < cluster_intensity.size(); ++i)
      {
        DEBUG_LOG(std::fixed << std::setprecision(1) << cluster_intensity[i]);
        if (i < cluster_intensity.size() - 1) DEBUG_LOG(", ");
      }
      DEBUG_LOG(")" << std::endl);

      DEBUG_LOG("      cluster_noise <- c(");
      for (size_t i = 0; i < cluster_noise.size(); ++i)
      {
        DEBUG_LOG(std::fixed << std::setprecision(1) << cluster_noise[i]);
        if (i < cluster_noise.size() - 1) DEBUG_LOG(", ");
      }
      DEBUG_LOG(")" << std::endl);
    }

    const int n = cluster_rt.size();
    if (n < minTraces) continue;

    std::vector<float> rt_diffs;
    rt_diffs.reserve(n - 1);
    for (int i = 1; i < n; ++i)
    {
      rt_diffs.push_back(cluster_rt[i] - cluster_rt[i-1]);
    }
    std::sort(rt_diffs.begin(), rt_diffs.end());
    float cycle_time = rt_diffs[rt_diffs.size() / 2]; // median

    int baseline_window_size = std::max(minTraces, static_cast<int>(std::floor(baselineWindow / cycle_time))) / 2;

    // Check if debug_mz falls within the cluster's m/z range
    if (cluster_matches_debug_mz)
    {
      DEBUG_LOG("      Calculated parameters:" << std::endl);
      DEBUG_LOG("      cycle_time: " << std::fixed << std::setprecision(4) << cycle_time << " sec" << std::endl);
      DEBUG_LOG("      baseline_window_size: " << baseline_window_size << " points" << std::endl);
    }

    auto baseline = calculate_baseline(cluster_intensity, baseline_window_size);
    // Use gentle smoothing for derivatives (window_size=2 means averaging 3 points: [i-1, i, i+1])
    // This reduces noise while preserving sharp peaks

    auto smoothed_intensity = smooth_intensity_savitzky_golay(cluster_intensity, 4, 2);
    // auto smoothed_intensity = smooth_intensity(cluster_intensity, 4);

    // DEBUG: Log smoothed and baseline data for inspection
    if (cluster_matches_debug_mz)
    {
      DEBUG_LOG("      cluster_smoothed <- c(");
      for (size_t i = 0; i < smoothed_intensity.size(); ++i)
      {
        DEBUG_LOG(std::fixed << std::setprecision(1) << smoothed_intensity[i]);
        if (i < smoothed_intensity.size() - 1) DEBUG_LOG(", ");
      }
      DEBUG_LOG(")" << std::endl);

      DEBUG_LOG("      cluster_baseline <- c(");
      for (size_t i = 0; i < baseline.size(); ++i)
      {
        DEBUG_LOG(std::fixed << std::setprecision(1) << baseline[i]);
        if (i < baseline.size() - 1) DEBUG_LOG(", ");
      }
      DEBUG_LOG(")" << std::endl);
    }

    std::vector<float> first_derivative, second_derivative;
    calculate_derivatives(smoothed_intensity, first_derivative, second_derivative);
    auto candidates = find_peak_candidates(first_derivative, smoothed_intensity, 0);

    if (cluster_matches_debug_mz)
    {
      DEBUG_LOG("      Peaks found (candidates): " << candidates.size() << " peaks" << std::endl);
      if (!candidates.empty())
      {
        DEBUG_LOG("      Peak positions (RT, m/z, smoothed_intensity):" << std::endl);
        for (int peak_idx : candidates)
        {
          if (peak_idx < static_cast<int>(cluster_rt.size()))
          {
            DEBUG_LOG("        - RT=" << std::fixed << std::setprecision(2) << cluster_rt[peak_idx]
                       << ", m/z=" << std::setprecision(4) << cluster_mz[peak_idx]
                       << ", smoothed_intensity=" << std::setprecision(0) << smoothed_intensity[peak_idx] << std::endl);
          }
        }
      }
    }

    auto valid_peaks = validate_peak_candidates(
      candidates,
      first_derivative,
      second_derivative,
      smoothed_intensity,
      cluster_rt,
      cluster_intensity,
      cluster_matches_debug_mz
    );

    if (cluster_matches_debug_mz)
    {
      DEBUG_LOG("      Peaks after validation: " << valid_peaks.size() << " peaks" << std::endl);
      if (!valid_peaks.empty())
      {
        DEBUG_LOG("      Valid peak positions (RT, m/z, smoothed_intensity):" << std::endl);
        for (int peak_idx : valid_peaks)
        {
          if (peak_idx < static_cast<int>(cluster_rt.size()))
          {
            DEBUG_LOG("        - RT=" << std::fixed << std::setprecision(2) << cluster_rt[peak_idx]
                       << ", m/z=" << std::setprecision(4) << cluster_mz[peak_idx]
                       << ", smoothed_intensity=" << std::setprecision(0) << smoothed_intensity[peak_idx] << std::endl);
          }
        }
      }
      else
      {
        DEBUG_LOG("      All peaks rejected during validation" << std::endl);
      }
    }

    std::vector<std::pair<int, std::pair<int, int>>> peak_boundaries; // {peak_idx, {left_idx, right_idx}}

    for (int peak_idx : valid_peaks)
    {
      if (peak_idx < minTraces / 2 || peak_idx >= n - minTraces / 2)
        continue;

      auto [left_idx, right_idx] = calculate_peak_boundaries(
        peak_idx,
        cluster_rt,
        smoothed_intensity,
        baseline,
        maxWidth / 2.0f,
        minTraces,
        cycle_time,
        cluster_matches_debug_mz,
        debugMZ
      );
      if (left_idx < right_idx)
      {
        peak_boundaries.emplace_back(peak_idx, std::make_pair(left_idx, right_idx));
      }
    }

    // Check for overlaps using RT boundaries and merge
    bool merged_any = true;
    while (merged_any && peak_boundaries.size() > 1)
    {
      merged_any = false;

      for (size_t i = 0; i < peak_boundaries.size(); ++i)
      {
        for (size_t j = i + 1; j < peak_boundaries.size(); ++j)
        {
          auto& [peak_i, bounds_i] = peak_boundaries[i];
          auto& [peak_j, bounds_j] = peak_boundaries[j];
          auto& [left_i, right_i] = bounds_i;
          auto& [left_j, right_j] = bounds_j;

          // Check if RT ranges overlap using actual peak boundaries
          float rt_min_i = cluster_rt[left_i];
          float rt_max_i = cluster_rt[right_i];
          float rt_min_j = cluster_rt[left_j];
          float rt_max_j = cluster_rt[right_j];

          float apex_rt_i = cluster_rt[peak_i];
          float apex_rt_j = cluster_rt[peak_j];

          bool rt_overlaps = !(rt_max_i < rt_min_j || rt_max_j < rt_min_i);

          // Only merge if boundaries overlap AND apexes are close enough
          // Check if apex distance is small relative to peak widths
          float apex_distance = std::abs(apex_rt_j - apex_rt_i);
          float width_i = rt_max_i - rt_min_i;
          float width_j = rt_max_j - rt_min_j;
          float avg_width = (width_i + width_j) / 2.0f;

          // Merge if apexes are within 30% of average peak width
          // This prevents merging widely separated peaks with overlapping tails
          bool apexes_close = (apex_distance < (avg_width * 0.3f));

          // Additional check: verify there's no deep valley between peaks
          bool has_deep_valley = false;
          if (rt_overlaps && apexes_close)
          {
            // Find the range between the two apexes
            int start_between = std::min(peak_i, peak_j);
            int end_between = std::max(peak_i, peak_j);

            // Find minimum intensity in the region between apexes
            float min_intensity_between = smoothed_intensity[start_between];
            for (int k = start_between; k <= end_between; ++k)
            {
              min_intensity_between = std::min(min_intensity_between, smoothed_intensity[k]);
            }

            // Get the lower apex intensity (minimum of the two peaks being compared)
            float apex_intensity_i = smoothed_intensity[peak_i];
            float apex_intensity_j = smoothed_intensity[peak_j];
            float lower_apex = std::min(apex_intensity_i, apex_intensity_j);

            // Check if valley depth is significant (minimum between peaks is below 70% of lower apex)
            float valley_threshold = lower_apex * 0.70f;
            has_deep_valley = (min_intensity_between < valley_threshold);

            if (has_deep_valley && cluster_matches_debug_mz)
            {
              DEBUG_LOG("      NOT merging peaks due to deep valley: RT1=" << apex_rt_i
                         << " (apex=" << std::setprecision(0) << apex_intensity_i << "), RT2=" << apex_rt_j
                         << " (apex=" << apex_intensity_j << ")"
                         << " | min_between=" << min_intensity_between
                         << " < threshold=" << valley_threshold << " (70% of lower apex=" << lower_apex << ")" << std::endl);
            }
          }

          bool should_merge = rt_overlaps && apexes_close && !has_deep_valley;

          if (should_merge)
          {

            if (cluster_matches_debug_mz)
            {
              DEBUG_LOG("      Merging overlapping peaks: RT1=" << apex_rt_i
                         << " (range: " << rt_min_i << "-" << rt_max_i << "), RT2=" << apex_rt_j
                         << " (range: " << rt_min_j << "-" << rt_max_j << ")"
                         << " | apex_distance=" << std::setprecision(2) << apex_distance
                         << ", threshold=" << (avg_width * 0.3f) << " (30% of avg width)" << std::endl);
            }
          }
          else if (rt_overlaps && cluster_matches_debug_mz)
          {
            DEBUG_LOG("      NOT merging peaks with overlapping tails but distant apexes: RT1=" << apex_rt_i
                       << " (range: " << rt_min_i << "-" << rt_max_i << "), RT2=" << apex_rt_j
                       << " (range: " << rt_min_j << "-" << rt_max_j << ")"
                       << " | apex_distance=" << std::setprecision(2) << apex_distance
                       << " > threshold=" << (avg_width * 0.3f) << std::endl);
          }

          if (should_merge)
          {

            // Choose peak with higher intensity
            bool keep_i = cluster_intensity[peak_i] > cluster_intensity[peak_j];
            size_t keep_idx = keep_i ? i : j;
            size_t remove_idx = keep_i ? j : i;

            // Merge edge boundaries
            auto& [keep_peak, keep_bounds] = peak_boundaries[keep_idx];
            auto& [remove_peak, remove_bounds] = peak_boundaries[remove_idx];
            auto& [keep_left, keep_right] = keep_bounds;
            auto& [remove_left, remove_right] = remove_bounds;

            keep_left = std::min(keep_left, remove_left);
            keep_right = std::max(keep_right, remove_right);

            // Recalculate boundaries with baseline and 1% intensity criteria
            float peak_intensity_1pct = cluster_intensity[keep_peak] * 0.01f;

            // Recalculate left boundary
            for (int k = keep_peak; k >= 0; k--) {
              if (k < keep_left) break;
              if (cluster_intensity[k] <= baseline[k] || cluster_intensity[k] <= peak_intensity_1pct) {
                keep_left = k + 1;
                break;
              }
            }

            // Recalculate right boundary
            for (int k = keep_peak; k < static_cast<int>(cluster_intensity.size()); k++) {
              if (k > keep_right) break;
              if (cluster_intensity[k] <= baseline[k] || cluster_intensity[k] <= peak_intensity_1pct) {
                keep_right = k - 1;
                break;
              }
            }

            // Ensure boundaries are valid
            keep_left = std::max(0, keep_left);
            keep_right = std::min(static_cast<int>(cluster_intensity.size()) - 1, keep_right);

            // Remove the merged peak
            peak_boundaries.erase(peak_boundaries.begin() + remove_idx);
            merged_any = true;
            break;
          }
        }
        if (merged_any) break;
      }
    }

    if (cluster_matches_debug_mz)
    {
      DEBUG_LOG("      Peaks after boundary calculation & merging: " << peak_boundaries.size() << " peaks" << std::endl);
      if (!peak_boundaries.empty())
      {
        DEBUG_LOG("      Peak boundaries (apex RT, RT range, width, n_traces):" << std::endl);
        for (const auto& [peak_idx, bounds] : peak_boundaries)
        {
          const auto& [left_idx, right_idx] = bounds;
          float apex_rt = cluster_rt[peak_idx];
          float apex_mz = cluster_mz[peak_idx];
          float apex_intensity = cluster_intensity[peak_idx];
          float rt_min = cluster_rt[left_idx];
          float rt_max = cluster_rt[right_idx];
          float width = rt_max - rt_min;
          int n_traces = right_idx - left_idx + 1;

          DEBUG_LOG("        - Apex: RT=" << std::fixed << std::setprecision(2) << apex_rt
                     << ", m/z=" << std::setprecision(4) << apex_mz
                     << ", intensity=" << std::setprecision(0) << apex_intensity
                     << " | Range: " << std::setprecision(2) << rt_min << "-" << rt_max
                     << " (width=" << width << "s, n=" << n_traces << ")" << std::endl);
        }
      }
    }

    // Step 2: Calculate final properties for non-overlapping peaks
    for (const auto& [peak_idx, bounds] : peak_boundaries)
    {
      const auto& [left_idx, right_idx] = bounds;

      if (cluster_matches_debug_mz)
      {
        DEBUG_LOG("      Processing peak with original apex at idx=" << peak_idx
                   << " (RT=" << std::fixed << std::setprecision(2) << cluster_rt[peak_idx]
                   << ", intensity=" << std::setprecision(0) << cluster_intensity[peak_idx] << ")" << std::endl);
      }

      // Extract peak region data
      std::vector<float> peak_rt, peak_mz, peak_intensity;
      std::vector<float> peak_baseline, peak_smoothed;

      for (int i = left_idx; i <= right_idx; ++i)
      {
        peak_rt.push_back(cluster_rt[i]);
        peak_mz.push_back(cluster_mz[i]);
        peak_intensity.push_back(cluster_intensity[i]);
        peak_baseline.push_back(baseline[i]);
        peak_smoothed.push_back(smoothed_intensity[i]);
      }

      if (peak_intensity.empty()) continue;

      // Find maximum intensity and its position
      // auto max_it = std::max_element(peak_intensity.begin(), peak_intensity.end());
      // float peak_max_intensity = *max_it;
      // int max_position = std::distance(peak_intensity.begin(), max_it);
      // int actual_max_idx = left_idx + max_position; // Actual index in cluster data
      // float rt_at_max = peak_rt[max_position];

      float avg_width = peak_rt.back() - peak_rt.front(); // or use feature.width if available
      float allowed_shift = avg_width * 0.5f;
      float center_rt = peak_rt[peak_idx - left_idx]; // initial apex RT

      // Find indices within allowed window
      std::vector<int> allowed_indices;
      for (size_t i = 0; i < peak_rt.size(); ++i) {
        if (std::abs(peak_rt[i] - center_rt) <= allowed_shift) {
            allowed_indices.push_back(i);
        }
      }

      // Search for max only within allowed_indices
      int max_position = allowed_indices[0];
      float peak_max_intensity = peak_intensity[max_position];
      for (int idx : allowed_indices) {
        if (peak_intensity[idx] > peak_max_intensity) {
          peak_max_intensity = peak_intensity[idx];
          max_position = idx;
        }
      }
      int actual_max_idx = left_idx + max_position;

      float rt_at_max = peak_rt[max_position];
      float mz_at_max = peak_mz[max_position];

      if (cluster_matches_debug_mz && actual_max_idx != peak_idx)
      {
        DEBUG_LOG("        WARNING: Apex shifted from idx=" << peak_idx
                   << " (RT=" << cluster_rt[peak_idx] << ", intensity=" << cluster_intensity[peak_idx]
                   << ") to idx=" << actual_max_idx
                   << " (RT=" << rt_at_max << ", intensity=" << peak_max_intensity << ")" << std::endl);
      }

      // Calculate noise and S/N using smoothed intensities at the actual peak boundaries
      // This ensures we're measuring baseline noise, not the peak's descending edge
      float noise = 0.0f;
      float sn = 0.0f;

      if (peak_smoothed.size() >= 4)
      {
        // Get smoothed intensities at left edge (first two points)
        float left_edge_1 = peak_smoothed[0];
        float left_edge_2 = peak_smoothed[1];
        // Get smoothed intensities at right edge (last two points)
        float right_edge_1 = peak_smoothed[peak_smoothed.size()-2];
        float right_edge_2 = peak_smoothed[peak_smoothed.size()-1];

        float left_min = std::min(left_edge_1, left_edge_2);
        float right_min = std::min(right_edge_1, right_edge_2);

        // Use the mean of the two edge minimums for more balanced noise estimation
        noise = (left_min + right_min) / 2.0f;
        sn = (noise > 0) ? peak_max_intensity / noise : 0.0f;

        if (cluster_matches_debug_mz)
        {
          DEBUG_LOG("        Noise calculation: left_edge=" << std::setprecision(0) << left_min
                     << " (smoothed at idx " << left_idx << "-" << (left_idx+1) << ")"
                     << ", right_edge=" << right_min
                     << " (smoothed at idx " << (right_idx-1) << "-" << right_idx << ")"
                     << " → noise=" << noise
                     << " (mean of edges), S/N=" << std::setprecision(1) << sn << std::endl);
        }
      }
      else
      {
        // For small peaks, use minimum of smoothed intensities
        noise = *std::min_element(peak_smoothed.begin(), peak_smoothed.end());
        sn = (noise > 0) ? peak_max_intensity / noise : 0.0f;

        if (cluster_matches_debug_mz)
        {
          DEBUG_LOG("        Noise calculation (small peak): min_smoothed=" << std::setprecision(0) << noise
                     << ", S/N=" << std::setprecision(1) << sn << std::endl);
        }
      }

      if (sn < minSNR)
      {
        if (cluster_matches_debug_mz)
        {
          DEBUG_LOG("        → Peak REJECTED (S/N too low): RT=" << std::fixed << std::setprecision(2) << rt_at_max
                     << ", S/N=" << std::setprecision(1) << sn << " < " << minSNR << std::endl);
        }
        continue;
      }

      // Calculate FWHM values and mean m/z within FWHM range (to avoid contamination from low intensity traces)
      auto [fwhm_rt_val, fwhm_mz_val, mean_mz_fwhm] = calculate_fwhm_combined(peak_rt, peak_mz, peak_intensity);

      // Use original smoothed peak data for fitting
      std::vector<float> fit_rt = peak_rt;
      std::vector<float> fit_smoothed = peak_smoothed;

      // Find apex position
      auto max_it_orig = std::max_element(fit_smoothed.begin(), fit_smoothed.end());
      int apex_idx = std::distance(fit_smoothed.begin(), max_it_orig);

      // Check slope correctness using first derivatives and interpolate outliers
      // Left side (from edge to apex): should have POSITIVE slope (rising)
      // Right side (from apex to edge): should have NEGATIVE slope (falling)
      std::vector<bool> needs_interpolation(peak_smoothed.size(), false);
      int interpolated_left = 0, interpolated_right = 0;

      // Check left side: mark consecutive points that break the rising trend for interpolation
      if (apex_idx > 1)
      {
        for (int i = 0; i < apex_idx - 1; ++i)
        {
          if (needs_interpolation[i]) continue; // Skip already marked points

          float current_intensity = peak_smoothed[i];

          // Mark all consecutive points that are not rising for interpolation
          for (int j = i + 1; j < apex_idx; ++j)
          {
            if (peak_smoothed[j] <= current_intensity) // Not rising, needs interpolation
            {
              needs_interpolation[j] = true;
              interpolated_left++;
            }
            else // Found a higher point, continue from here
            {
              break;
            }
          }
        }
      }

      // Check right side: mark consecutive points that break the falling trend for interpolation
      if (apex_idx < static_cast<int>(peak_smoothed.size()) - 2)
      {
        for (int i = apex_idx + 1; i < static_cast<int>(peak_smoothed.size()) - 1; ++i)
        {
          if (needs_interpolation[i]) continue; // Skip already marked points

          float current_intensity = peak_smoothed[i];

          // Mark all consecutive points that are not falling for interpolation
          for (int j = i + 1; j < static_cast<int>(peak_smoothed.size()); ++j)
          {
            if (peak_smoothed[j] >= current_intensity) // Not falling, needs interpolation
            {
              needs_interpolation[j] = true;
              interpolated_right++;
            }
            else // Found a lower point, continue from here
            {
              break;
            }
          }
        }
      }

      // Interpolate marked points using linear interpolation between surrounding valid points
      if (interpolated_left > 0 || interpolated_right > 0)
      {
        std::vector<float> corrected_smoothed = peak_smoothed;

        if (cluster_matches_debug_mz)
        {
          DEBUG_LOG("        Slope checking: found " << interpolated_left << " left outliers, "
                     << interpolated_right << " right outliers to interpolate" << std::endl);
        }

        // Process each consecutive group of outliers
        int i = 0;
        while (i < static_cast<int>(needs_interpolation.size()))
        {
          if (needs_interpolation[i])
          {
            // Find the start of outlier sequence
            int start_outlier = i;

            // Find the end of outlier sequence
            int end_outlier = i;
            while (end_outlier < static_cast<int>(needs_interpolation.size()) && needs_interpolation[end_outlier])
              end_outlier++;
            end_outlier--; // Last outlier index

            // Find valid points before and after the outlier sequence
            int before_idx = start_outlier - 1;
            int after_idx = end_outlier + 1;

            // Ensure we have valid boundary points
            if (before_idx >= 0 && after_idx < static_cast<int>(peak_smoothed.size()))
            {
              float intensity_before = peak_smoothed[before_idx];
              float intensity_after = peak_smoothed[after_idx];
              float rt_before = peak_rt[before_idx];
              float rt_after = peak_rt[after_idx];

              // Linear interpolation for each outlier point
              for (int j = start_outlier; j <= end_outlier; ++j)
              {
                float rt_j = peak_rt[j];
                float t = (rt_j - rt_before) / (rt_after - rt_before); // Interpolation factor
                float interpolated_intensity = intensity_before + t * (intensity_after - intensity_before);

                if (cluster_matches_debug_mz)
                {
                  DEBUG_LOG("        Interpolated idx=" << j
                             << " RT=" << std::fixed << std::setprecision(2) << rt_j
                             << " intensity: " << std::setprecision(0) << peak_smoothed[j]
                             << " → " << interpolated_intensity << std::endl);
                }

                corrected_smoothed[j] = interpolated_intensity;
              }
            }

            i = end_outlier + 1;
          }
          else
          {
            i++;
          }
        }

        fit_smoothed = corrected_smoothed;
      }

      // Calculate FWHM from cleaned peak for parameter estimation
      float fwhm_peak_rt = calculate_fwhm_rt(fit_rt, fit_smoothed);

      // Print fitting data to debug log
      if (cluster_matches_debug_mz)
      {
        DEBUG_LOG("        Fitting data vectors:" << std::endl);
        DEBUG_LOG("          RT values (" << fit_rt.size() << " points): ");
        for (size_t i = 0; i < fit_rt.size(); ++i)
        {
          DEBUG_LOG(std::setprecision(2) << fit_rt[i]);
          if (i < fit_rt.size() - 1) DEBUG_LOG(", ");
        }
        DEBUG_LOG(std::endl);
        DEBUG_LOG("          Smoothed intensity values: ");
        for (size_t i = 0; i < fit_smoothed.size(); ++i)
        {
          DEBUG_LOG(std::setprecision(1) << fit_smoothed[i]);
          if (i < fit_smoothed.size() - 1) DEBUG_LOG(", ");
        }
        DEBUG_LOG(std::endl);
      }

      // Recalculate apex from cleaned data (after slope removal)
      auto fit_max_it = std::max_element(fit_smoothed.begin(), fit_smoothed.end());
      float fit_max_intensity = *fit_max_it;
      int fit_max_position = std::distance(fit_smoothed.begin(), fit_max_it);
      float fit_rt_at_max = fit_rt[fit_max_position];

      if (cluster_matches_debug_mz && std::abs(fit_rt_at_max - rt_at_max) > 1.0f)
      {
        DEBUG_LOG("        Apex recalculated after slope removal: RT=" << std::setprecision(2) << fit_rt_at_max
                   << " (was " << rt_at_max << "), intensity=" << std::setprecision(0) << fit_max_intensity
                   << " (was " << peak_max_intensity << ")" << std::endl);
      }

      // Gaussian fitting parameters
      float gaussian_baseline = std::min(fit_smoothed.front(), fit_smoothed.back());
      float gaussian_A = fit_max_intensity - gaussian_baseline;
      float gaussian_mu = fit_rt_at_max;  // Use recalculated apex RT
      float gaussian_sigma = fwhm_peak_rt / 2.355f;
      if (gaussian_sigma <= 0) gaussian_sigma = (fit_rt.back() - fit_rt.front()) / 4.0f;

      // Optimize Gaussian parameters (with baseline)
      fit_gaussian(fit_rt, fit_smoothed, gaussian_A, gaussian_mu, gaussian_sigma, gaussian_baseline);
      float gaussian_r2 = calculate_gaussian_rsquared(fit_rt, fit_smoothed,
                                                      gaussian_A, gaussian_mu, gaussian_sigma, gaussian_baseline);

      if (cluster_matches_debug_mz)
      {
        DEBUG_LOG("        Gaussian fitting:" << std::endl);
        DEBUG_LOG("          Peak: " << fit_rt.size() << " points"
                     << " (RT range: " << std::setprecision(2) << fit_rt.front()
                     << " to " << fit_rt.back() << ")" << std::endl);
        DEBUG_LOG("          FWHM: " << std::setprecision(3) << fwhm_peak_rt << "s" << std::endl);
        DEBUG_LOG("          Optimized parameters: A=" << std::setprecision(0) << gaussian_A
                     << ", μ=" << std::setprecision(2) << gaussian_mu
                     << ", σ=" << std::setprecision(3) << gaussian_sigma
                     << ", baseline=" << std::setprecision(1) << gaussian_baseline << std::endl);

        // Print fitted values vs actual in horizontal format for easy plotting
        DEBUG_LOG("          Fitted vs Actual intensities:" << std::endl);
        float ss_total = 0.0f, ss_residual = 0.0f;
        float mean_y = nts::utils::mean(fit_smoothed);

        // Calculate all predictions and errors first
        std::vector<float> predictions;
        std::vector<float> errors;
        for (size_t i = 0; i < fit_rt.size(); ++i)
        {
          float pred = gaussian_function_with_baseline(gaussian_A, gaussian_mu, gaussian_sigma, gaussian_baseline, fit_rt[i]);
          float residual = fit_smoothed[i] - pred;
          predictions.push_back(pred);
          errors.push_back(residual);
          ss_residual += residual * residual;
          ss_total += (fit_smoothed[i] - mean_y) * (fit_smoothed[i] - mean_y);
        }

        // Print RT values
        DEBUG_LOG("            RT values: ");
        for (size_t i = 0; i < fit_rt.size(); ++i)
        {
          DEBUG_LOG(std::setprecision(2) << std::fixed << fit_rt[i]);
          if (i < fit_rt.size() - 1) DEBUG_LOG(", ");
        }
        DEBUG_LOG(std::endl);

        // Print actual intensities
        DEBUG_LOG("            Actual:    ");
        for (size_t i = 0; i < fit_smoothed.size(); ++i)
        {
          DEBUG_LOG(std::setprecision(1) << fit_smoothed[i]);
          if (i < fit_smoothed.size() - 1) DEBUG_LOG(", ");
        }
        DEBUG_LOG(std::endl);

        // Print fitted intensities
        DEBUG_LOG("            Fitted:    ");
        for (size_t i = 0; i < predictions.size(); ++i)
        {
          DEBUG_LOG(std::setprecision(1) << predictions[i]);
          if (i < predictions.size() - 1) DEBUG_LOG(", ");
        }
        DEBUG_LOG(std::endl);

        // Print errors
        DEBUG_LOG("            Errors:    ");
        for (size_t i = 0; i < errors.size(); ++i)
        {
          DEBUG_LOG(std::setprecision(1) << std::showpos << errors[i] << std::noshowpos);
          if (i < errors.size() - 1) DEBUG_LOG(", ");
        }
        DEBUG_LOG(std::endl);

        DEBUG_LOG("          Sum of squared residuals: " << std::setprecision(1) << ss_residual << std::endl);
        DEBUG_LOG("          Sum of squared total: " << std::setprecision(1) << ss_total << std::endl);
        DEBUG_LOG("          Gaussian R²=" << std::setprecision(3) << gaussian_r2 << std::endl);
      }

      float final_r2 = gaussian_r2;
      std::string fit_type = "Gaussian";

      // Accept peaks with very poor fit if they have good S/N
      // Many real chromatographic peaks are asymmetric due to tailing, adsorption, etc.
      // R² is informative but shouldn't be a hard rejection criterion with good S/N
      if (final_r2 <= -1.0f)
      {
        if (cluster_matches_debug_mz)
        {
          DEBUG_LOG("        → Peak REJECTED (extremely poor " << fit_type << " fit): RT=" << std::fixed << std::setprecision(2) << rt_at_max
                     << ", R²=" << std::setprecision(3) << final_r2 << " <= -1.0" << std::endl);
        }
        continue;
      }

      // Log acceptance with fit quality
      if (cluster_matches_debug_mz)
      {
        if (final_r2 >= 0.7f)
        {
          DEBUG_LOG("        → Peak ACCEPTED (good " << fit_type << " fit): RT=" << std::fixed << std::setprecision(2) << rt_at_max
                     << ", R²=" << std::setprecision(3) << final_r2 << std::endl);
        }
        else if (final_r2 >= 0.5f)
        {
          DEBUG_LOG("        → Peak ACCEPTED (moderate " << fit_type << " fit): RT=" << std::fixed << std::setprecision(2) << rt_at_max
                     << ", R²=" << std::setprecision(3) << final_r2 << std::endl);
        }
        else
        {
          DEBUG_LOG("        → Peak ACCEPTED (poor " << fit_type << " fit, but good S/N): RT=" << std::fixed << std::setprecision(2) << rt_at_max
                     << ", R²=" << std::setprecision(3) << final_r2 << ", S/N=" << std::setprecision(1) << sn << std::endl);
        }
      }

      // Create FEATURE structure
      nts::FEATURE feature;

      // Basic identification with polarity-specific naming
      std::string polarity_suffix = (polarity_sign > 0) ? "POS" : "NEG";
      feature.analysis = analysis_name;
      feature.feature = "CL" + std::to_string(cluster_id) + "_MZ" + std::to_string(static_cast<int>(std::round(mz_at_max))) +
                       "_RT" + std::to_string(static_cast<int>(std::round(rt_at_max))) + "_" + polarity_suffix;
      feature.feature_group = "";
      feature.feature_component = "";
      feature.adduct = adduct_name; // Set polarity-specific adduct

      // Peak characteristics
      feature.rt = rt_at_max;
      feature.mz = mean_mz_fwhm; // Use mean m/z from FWHM region only

      // Calculate neutral mass using polarity-specific correction
      feature.mass = feature.mz + mass_correction; // mass_correction is negative for positive mode, positive for negative mode

      feature.intensity = peak_max_intensity;
      feature.noise = noise;
      feature.sn = sn;
      feature.polarity = polarity_sign; // Set polarity correctly

      // Calculate boundaries and dimensions
      feature.rtmin = cluster_rt[left_idx];
      feature.rtmax = cluster_rt[right_idx];
      feature.width = feature.rtmax - feature.rtmin;

      // Calculate m/z range and ppm spread
      feature.mzmin = peak_mz[0];
      feature.mzmax = peak_mz[0];
      for (size_t i = 0; i < peak_mz.size(); ++i)
      {
        feature.mzmin = std::min(feature.mzmin, peak_mz[i]);
        feature.mzmax = std::max(feature.mzmax, peak_mz[i]);
      }
      feature.ppm = (feature.mzmax - feature.mzmin) / feature.mz * 1e6f;

      feature.fwhm_rt = fwhm_rt_val;
      feature.fwhm_mz = fwhm_mz_val;

      // Calculate area
      feature.area = calculate_peak_area(peak_rt, peak_intensity);

      // Store Gaussian fitting results
      feature.gaussian_A = gaussian_A;
      feature.gaussian_mu = gaussian_mu;
      feature.gaussian_sigma = gaussian_sigma;
      feature.gaussian_r2 = final_r2;


      feature.filtered = false;
      feature.filter = "";
      feature.filled = false;
      feature.correction = 1.0f;

      feature.eic_size = static_cast<int>(peak_rt.size());
      std::string rt_encoded = sc::encode_little_endian_from_float(peak_rt, 4);
      std::string mz_encoded = sc::encode_little_endian_from_float(peak_mz, 4);
      std::string intensity_encoded = sc::encode_little_endian_from_float(peak_intensity, 4);
      std::string baseline_encoded = sc::encode_little_endian_from_float(peak_baseline, 4);
      std::string smoothed_encoded = sc::encode_little_endian_from_float(peak_smoothed, 4);

      feature.eic_rt = sc::encode_base64(rt_encoded);
      feature.eic_mz = sc::encode_base64(mz_encoded);
      feature.eic_intensity = sc::encode_base64(intensity_encoded);
      feature.eic_baseline = sc::encode_base64(baseline_encoded);
      feature.eic_smoothed = sc::encode_base64(smoothed_encoded);

      // Set MS1 and MS2 as empty for now
      feature.ms1_size = 0;
      feature.ms1_mz = "";
      feature.ms1_intensity = "";
      feature.ms2_size = 0;
      feature.ms2_mz = "";
      feature.ms2_intensity = "";

      if (cluster_matches_debug_mz)
      {
        DEBUG_LOG("        → FINAL FEATURE: " << feature.feature << std::endl);
        DEBUG_LOG("          RT=" << std::fixed << std::setprecision(2) << feature.rt
                   << " (range: " << feature.rtmin << "-" << feature.rtmax
                   << ", width=" << feature.width << "s, FWHM=" << feature.fwhm_rt << "s)" << std::endl);
        DEBUG_LOG("          m/z=" << std::setprecision(4) << feature.mz
                   << " (range: " << feature.mzmin << "-" << feature.mzmax
                   << ", ppm=" << std::setprecision(1) << feature.ppm << ")" << std::endl);
        DEBUG_LOG("          Intensity=" << std::setprecision(0) << feature.intensity
                   << ", Noise=" << feature.noise << ", S/N=" << std::setprecision(1) << feature.sn << std::endl);
        DEBUG_LOG("          Area=" << std::setprecision(0) << feature.area
                   << ", Gaussian R²=" << std::setprecision(3) << feature.gaussian_r2
                   << ", n_traces=" << feature.eic_size << std::endl);
      }

      polarity_features.push_back(feature);
    }

    if (cluster_matches_debug_mz)
    {
      int features_from_cluster = 0;
      for (const auto& feat : polarity_features)
      {
        if (feat.feature.find("CL" + std::to_string(cluster_id) + "_") == 0)
          features_from_cluster++;
      }
      DEBUG_LOG("      ===== Total features extracted from cluster " << cluster_id << ": "
                 << features_from_cluster << " features =====" << std::endl << std::endl);
    }
  }

  // Close debug log at the end of processing to allow new log files to be created
  close_debug_log();

  return polarity_features;
}
