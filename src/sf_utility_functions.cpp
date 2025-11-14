#include "sf_utility_functions.h"
#include "NTS2_utils.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <map>
#include <unordered_map>
#include <set>
#include <functional>
#include <iomanip>

// MARK: BASIC UTILITY FUNCTIONS

Rcpp::List SF_UTILITY::get_empty_dt()
{
  Rcpp::List out = Rcpp::List::create();
  out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  return out;
}

bool SF_UTILITY::check_list_must_have_names(
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

float SF_UTILITY::mean(const std::vector<float> &v)
{
  return std::accumulate(v.begin(), v.end(), 0.0) / v.size();
}

float SF_UTILITY::standard_deviation(const std::vector<float> &v, float mean_val)
{
  float sum = 0.0;
  for (float num : v)
  {
    sum += pow(num - mean_val, 2);
  }
  return sqrt(sum / v.size());
}

float SF_UTILITY::quantile(std::vector<float> data, float quantile_fraction)
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

size_t SF_UTILITY::find_max_index(const std::vector<float> &v)
{
  return std::max_element(v.begin(), v.end()) - v.begin();
}

size_t SF_UTILITY::find_min_index(const std::vector<float> &v)
{
  return std::min_element(v.begin(), v.end()) - v.begin();
}

float SF_UTILITY::gaussian_function(const float &A,
                                    const float &mu,
                                    const float &sigma,
                                    const float &x)
{
  return A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
}

sc::MS_SPECTRA_HEADERS SF_UTILITY::as_MS_SPECTRA_HEADERS(const Rcpp::List &hd)
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

std::pair<float, float> SF_UTILITY::calculate_mass_resolution_model_param(const std::vector<int> &resolution_profile)
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
}

float SF_UTILITY::calculate_mass_resolution_model_threshold(float mz, float slope, float intercept)
{
  const float resolution = slope * mz + intercept;
  // Add safety check to avoid division by very small resolutions
  return mz / std::max(resolution, 1.0f);
}

float SF_UTILITY::calculate_mz_threshold_linear(float mz, float slope, float intercept)
{
  const float resolution = slope * mz + intercept;
  // Add safety check to avoid division by very small resolutions
  return mz / std::max(resolution, 1.0f);
}

// MARK: DATA STATISTICS IMPLEMENTATIONS

SF_UTILITY::VectorStats::VectorStats(const std::vector<float> &input_data)
{
  count = input_data.size();
  if (count == 0)
  {
    mean = std_dev = coefficient_variation = 0.0f;
    min_val = max_val = signal_noise_ratio = 0.0f;
    return;
  }
  mean = SF_UTILITY::mean(input_data);
  std_dev = SF_UTILITY::standard_deviation(input_data, mean);
  coefficient_variation = (mean != 0.0f) ? std_dev / mean : 0.0f;
  auto [min_it, max_it] = std::minmax_element(input_data.begin(), input_data.end());
  min_val = *min_it;
  max_val = *max_it;
  float q25_val = SF_UTILITY::quantile(input_data, 0.25f);
  float q90_val = SF_UTILITY::quantile(input_data, 0.90f);
  signal_noise_ratio = (q25_val != 0.0f) ? q90_val / q25_val : 0.0f;
}

// MARK: CLUSTER STATISTICS IMPLEMENTATIONS

SF_UTILITY::ClusterStats::ClusterStats(float intensity)
    : count(1), max(intensity), min(intensity),
      mean(intensity), sum(intensity) {}

void SF_UTILITY::ClusterStats::update(float intensity)
{
  count++;
  max = std::max(max, intensity);
  min = std::min(min, intensity);
  sum += intensity;
  mean = sum / count;
}

// MARK: ADAPTIVE PARAMETERS IMPLEMENTATIONS

SF_UTILITY::AdaptiveNoiseParams::AdaptiveNoiseParams(const VectorStats &stats, int data_size, float base_quantile)
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

std::vector<size_t> SF_UTILITY::get_sort_indices_float(const std::vector<float> &data)
{
  std::vector<size_t> indices(data.size());
  std::iota(indices.begin(), indices.end(), 0);
  std::sort(indices.begin(), indices.end(), [&data](size_t a, size_t b)
            { return data[a] < data[b]; });
  return indices;
}

void SF_UTILITY::reorder_float_data(std::vector<float> &data, const std::vector<size_t> &indices)
{
  std::vector<float> reordered;
  reordered.reserve(data.size());
  for (size_t idx : indices)
  {
    reordered.push_back(data[idx]);
  }
  data = std::move(reordered);
}

void SF_UTILITY::reorder_int_data(std::vector<int> &data, const std::vector<size_t> &indices)
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
void SF_UTILITY::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1)
{
  reorder_float_data(vec1, indices);
}

void SF_UTILITY::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
}

void SF_UTILITY::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2,
                                          std::vector<float> &vec3)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
  reorder_float_data(vec3, indices);
}

void SF_UTILITY::reorder_multiple_vectors(const std::vector<size_t> &indices,
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

void SF_UTILITY::reorder_multiple_vectors(const std::vector<size_t> &indices,
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

std::vector<int> SF_UTILITY::calculate_bin_assignments(const std::vector<float> &data, int num_bins)
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

std::vector<size_t> SF_UTILITY::filter_above_threshold(const std::vector<float> &data,
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

std::vector<int> SF_UTILITY::cluster_by_threshold_float(const std::vector<float> &sorted_data,
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

std::vector<int> SF_UTILITY::cluster_by_mz(const std::vector<float> &mz_values,
                                           float slope, float intercept)
{
  const size_t n = mz_values.size();
  if (n == 0)
    return std::vector<int>();

  // Pre-calculate all thresholds
  std::vector<float> thresholds(n);
  for (size_t i = 0; i < n; ++i)
  {
    thresholds[i] = calculate_mz_threshold_linear(mz_values[i], slope, intercept);
  }
  return cluster_by_threshold_float(mz_values, thresholds);
}

std::vector<size_t> SF_UTILITY::filter_valid_clusters(const std::vector<SpectraPoint> &data,
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

void SF_UTILITY::sort_by_rt_inplace(std::vector<float> &rt, std::vector<float> &mz,
                                    std::vector<float> &intensity, std::vector<float> &noise,
                                    std::vector<int> &cluster)
{
  if (rt.empty())
    return;

  auto indices = get_sort_indices_float(rt);
  reorder_multiple_vectors(indices, rt, mz, intensity, noise, cluster);
}

void SF_UTILITY::cluster_spectra_by_mz(const std::vector<float> &spec_rt,
                                       const std::vector<float> &spec_mz,
                                       const std::vector<float> &spec_intensity,
                                       const std::vector<float> &spec_noise,
                                       float slope, float intercept,
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
  auto sorted_clusters = cluster_by_mz(sorted_mz, slope, intercept);
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

  const size_t final_size = valid_indices.size();
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
  for (size_t idx : valid_indices)
  {
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

std::vector<float> SF_UTILITY::calculate_noise_levels(const std::vector<float> &intensities,
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
      float quantile_val = SF_UTILITY::quantile(bin_data[bin_idx], params.quantile);
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

void SF_UTILITY::filter_and_cluster(const std::vector<float> &raw_mz,
                                    const std::vector<float> &raw_intensity,
                                    const std::vector<float> &raw_noise,
                                    float slope, float intercept,
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
  auto clusters = cluster_by_mz(filtered_mz, slope, intercept);

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

void SF_UTILITY::denoise_spectra(sc::MS_FILE &ana, const int &spectrum_idx, const float &rt,
                                 const float &noiseThreshold, const int &minTraces,
                                 const float &slope, const float &intercept,
                                 std::vector<float> &spec_rt, std::vector<float> &spec_mz,
                                 std::vector<float> &spec_intensity, std::vector<float> &spec_noise,
                                 size_t &total_raw_points, size_t &total_clean_points, const bool &debug,
                                 const float &base_quantile)
{
  std::vector<std::vector<std::vector<float>>> single_spectrum = ana.get_spectra({spectrum_idx});
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
  AdaptiveNoiseParams noise_params(stats, raw_n_traces, base_quantile);

  auto raw_noise = SF_UTILITY::calculate_noise_levels(raw_intensity, noise_params, noiseThreshold);

  if (debug)
  {
    // Calculate noise level statistics for debug output
    float noise_mean = SF_UTILITY::mean(raw_noise);
    float noise_stddev = SF_UTILITY::standard_deviation(raw_noise, noise_mean);
    int zeros_removed = raw_n_traces - static_cast<int>(non_zero_intensities.size());
    float max_quantile = std::max(0.30f, base_quantile * 1.2f);
    
    Rcpp::Rcout << "DEBUG Auto noise estimation: base_quantile=" << base_quantile
                << " -> adaptive_quantile=" << noise_params.quantile << "/" << max_quantile
                << ", bins=" << noise_params.bins
                << " (CV=" << stats.coefficient_variation << ", SNR=" << stats.signal_noise_ratio
                << ", n=" << raw_n_traces << ", zeros_removed=" << zeros_removed << ")" << std::endl;
    Rcpp::Rcout << "      Noise levels: mean=" << std::fixed << std::setprecision(2) << noise_mean 
                << ", stddev=" << noise_stddev << std::endl;
  }

  std::vector<float> final_mz, final_intensity, final_noise;
  filter_and_cluster(raw_mz, raw_intensity, raw_noise,
                     slope, intercept,
                     final_mz, final_intensity, final_noise);

  total_clean_points += final_mz.size();

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

std::vector<float> SF_UTILITY::calculate_baseline(const std::vector<float> &intensity, int window_size)
{
  const size_t n = intensity.size();
  std::vector<float> baseline(n);

  for (size_t i = 0; i < n; ++i)
  {
    size_t start_idx = (i >= static_cast<size_t>(window_size)) ? i - window_size : 0;
    size_t end_idx = std::min(n - 1, i + window_size);

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

std::vector<float> SF_UTILITY::smooth_intensity(const std::vector<float> &intensity, int window_size)
{
  const size_t n = intensity.size();
  std::vector<float> smoothed(n);
  int half_window = window_size / 2;

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

void SF_UTILITY::calculate_derivatives(const std::vector<float> &smoothed_intensity,
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

std::vector<int> SF_UTILITY::find_peak_candidates(const std::vector<float> &first_derivative)
{
  std::vector<int> candidates;
  candidates.reserve(first_derivative.size() / 10); // Conservative estimate

  // Find where slope changes from positive to negative
  for (size_t i = 1; i < first_derivative.size(); ++i)
  {
    if (first_derivative[i] <= 0 && first_derivative[i-1] > 0)
    {
      candidates.push_back(static_cast<int>(i + 1)); // +1 because derivative is offset by 1
    }
  }

  return candidates;
}

std::vector<int> SF_UTILITY::validate_peak_candidates(const std::vector<int> &candidates,
                                                     const std::vector<float> &first_derivative,
                                                     const std::vector<float> &second_derivative,
                                                     const std::vector<float> &smoothed_intensity,
                                                     int derivative_window_size, int min_traces)
{
  std::vector<int> valid_peaks;
  valid_peaks.reserve(candidates.size());
  const int n = static_cast<int>(smoothed_intensity.size());

  for (int idx : candidates)
  {
    // Check edge proximity
    if (idx < derivative_window_size || idx >= n - derivative_window_size)
      continue;

    // Check first derivative before peak (should be positive)
    float pre_avg = 0.0f;
    int pre_count = 0;
    for (int i = std::max(0, idx - derivative_window_size); i < idx - 1; ++i)
    {
      if (i >= 0 && i < static_cast<int>(first_derivative.size()))
      {
        pre_avg += first_derivative[i];
        pre_count++;
      }
    }
    if (pre_count > 0) pre_avg /= pre_count;

    // Check first derivative after peak (should be negative)
    float post_avg = 0.0f;
    int post_count = 0;
    for (int i = idx; i < std::min(n - 1, idx + derivative_window_size); ++i)
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

    // Validate criteria
    if (pre_count > 0 && post_count > 0 && pre_avg > 0 && post_avg < 0 && d2_at_peak < 0)
    {
      valid_peaks.push_back(idx);
    }
  }

  return valid_peaks;
}

std::pair<int, int> SF_UTILITY::calculate_peak_boundaries(int peak_idx,
                                                         const std::vector<float> &rt,
                                                         const std::vector<float> &smoothed_intensity,
                                                         const std::vector<float> &baseline,
                                                         float max_half_width, int min_traces)
{
  const int n = static_cast<int>(rt.size());
  float apex_intensity = smoothed_intensity[peak_idx];
  float min_intensity_threshold = 0.01f * apex_intensity;

  // Left boundary
  int left_idx = std::max(0, peak_idx - 3);
  while (left_idx > 0)
  {
    if (rt[peak_idx] - rt[left_idx] > max_half_width) break;
    if (smoothed_intensity[left_idx] <= baseline[left_idx] * 1.2f) break;
    if (smoothed_intensity[left_idx] <= min_intensity_threshold) break;
    left_idx--;
  }

  // Right boundary
  int right_idx = std::min(n - 1, peak_idx + 3);
  while (right_idx < n - 1)
  {
    if (rt[right_idx] - rt[peak_idx] > max_half_width) break;
    if (smoothed_intensity[right_idx] <= baseline[right_idx] * 1.2f) break;
    if (smoothed_intensity[right_idx] <= min_intensity_threshold) break;
    right_idx++;
  }

  return std::make_pair(left_idx, right_idx);
}

std::pair<int, int> SF_UTILITY::calculate_fwhm_boundaries(int peak_idx,
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
  auto [fwhm_rt_val, fwhm_mz_val] = calculate_fwhm_combined(peak_rt, peak_rt, peak_intensity); // Use peak_rt for both RT and MZ

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

float SF_UTILITY::calculate_peak_area(const std::vector<float> &rt, const std::vector<float> &intensity)
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

std::pair<float, float> SF_UTILITY::calculate_fwhm_combined(const std::vector<float> &rt,
                                                           const std::vector<float> &mz,
                                                           const std::vector<float> &intensity)
{
  if (rt.size() != intensity.size() || mz.size() != intensity.size() || rt.empty())
    return std::make_pair(0.0f, 0.0f);

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
  {
    left_idx--;
  }

  // Search right from apex
  while (right_idx < intensity.size() - 1 && intensity[right_idx] > half_max)
  {
    right_idx++;
  }

  float fwhm_rt, fwhm_mz;

  // Calculate FWHM in RT dimension (RT difference at half maximum boundaries)
  if (left_idx < right_idx && right_idx < rt.size())
  {
    fwhm_rt = rt[right_idx] - rt[left_idx];
  }
  else
  {
    fwhm_rt = rt.back() - rt.front(); // fallback to total RT width
  }

  // Calculate FWHM in m/z dimension (m/z range within the RT FWHM region)
  if (left_idx < right_idx && right_idx < mz.size())
  {
    // Find min and max m/z values within the FWHM RT range
    float min_mz = mz[left_idx];
    float max_mz = mz[left_idx];
    for (size_t i = left_idx; i <= right_idx; ++i)
    {
      min_mz = std::min(min_mz, mz[i]);
      max_mz = std::max(max_mz, mz[i]);
    }
    fwhm_mz = max_mz - min_mz;
  }
  else
  {
    fwhm_mz = mz.back() - mz.front(); // fallback to total m/z width
  }

  return std::make_pair(fwhm_rt, fwhm_mz);
}

void SF_UTILITY::fit_gaussian(const std::vector<float> &x, const std::vector<float> &y,
                             float &A, float &mu, float &sigma)
{
  // Adam optimizer parameters
  const float alpha = 0.01f;   // Learning rate
  const float beta1 = 0.9f;    // First moment decay rate
  const float beta2 = 0.999f;  // Second moment decay rate
  const float epsilon = 1e-8f; // Small value to prevent division by zero
  const int max_iterations = 300;

  float m_A = 0.0f, v_A = 0.0f, m_mu = 0.0f, v_mu = 0.0f, m_sigma = 0.0f, v_sigma = 0.0f;

  for (int iter = 1; iter <= max_iterations; ++iter)
  {
    float grad_A = 0.0f, grad_mu = 0.0f, grad_sigma = 0.0f;

    // Calculate gradients
    for (size_t i = 0; i < x.size(); ++i)
    {
      float exp_term = std::exp(-std::pow(x[i] - mu, 2) / (2 * std::pow(sigma, 2)));
      float y_pred = A * exp_term;
      float error = y[i] - y_pred;

      grad_A += -2 * error * exp_term;
      grad_mu += -2 * error * A * exp_term * (x[i] - mu) / std::pow(sigma, 2);
      grad_sigma += -2 * error * A * exp_term * std::pow(x[i] - mu, 2) / std::pow(sigma, 3);
    }

    // Adam update for A
    m_A = beta1 * m_A + (1 - beta1) * grad_A;
    v_A = beta2 * v_A + (1 - beta2) * grad_A * grad_A;
    float A_hat = m_A / (1 - std::pow(beta1, iter));
    float v_A_hat = v_A / (1 - std::pow(beta2, iter));
    A -= alpha * A_hat / (std::sqrt(v_A_hat) + epsilon);

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

    // Constrain sigma to reasonable bounds
    sigma = std::max(0.1f, std::min(sigma, 100.0f));
  }
}

float SF_UTILITY::calculate_gaussian_rsquared(const std::vector<float> &x, const std::vector<float> &y,
                                             float A, float mu, float sigma)
{
  if (x.empty() || y.empty() || x.size() != y.size())
    return 0.0f;

  float ss_total = 0.0f;
  float ss_residual = 0.0f;
  float mean_y = SF_UTILITY::mean(y);

  for (size_t i = 0; i < x.size(); ++i)
  {
    float y_pred = SF_UTILITY::gaussian_function(A, mu, sigma, x[i]);
    ss_residual += std::pow(y[i] - y_pred, 2);
    ss_total += std::pow(y[i] - mean_y, 2);
  }

  if (ss_total == 0.0f)
    return 0.0f;

  return std::max(0.0f, std::min(1.0f, 1.0f - (ss_residual / ss_total)));
}

// MARK: POLARITY-SPECIFIC PROCESSING FUNCTIONS

std::vector<NTS2::FEATURE> SF_UTILITY::process_polarity_clusters(
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
    bool debug,
    int debug_cluster)
{
  
  std::map<int, std::vector<int>> cluster_indices;
  for (size_t i = 0; i < clust_cluster.size(); ++i)
  {
    cluster_indices[clust_cluster[i]].push_back(static_cast<int>(i));
  }

  std::vector<NTS2::FEATURE> polarity_features;

  for (const auto& [cluster_id, indices] : cluster_indices)
  {
    if (indices.size() < static_cast<size_t>(minTraces))
      continue;

    if (debug && cluster_id == debug_cluster)
    {
      Rcpp::Rcout << "DEBUG Processing cluster " << cluster_id << " with " << indices.size() << " traces (polarity: " << polarity_sign << ")" << std::endl;
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
    int derivative_window_size = std::max(minTraces, static_cast<int>(std::floor(4.0f / cycle_time)));

    auto baseline = calculate_baseline(cluster_intensity, baseline_window_size);
    auto smoothed_intensity = smooth_intensity(cluster_intensity, 3);
    std::vector<float> first_derivative, second_derivative;
    calculate_derivatives(smoothed_intensity, first_derivative, second_derivative);
    auto candidates = find_peak_candidates(first_derivative);
    auto valid_peaks = validate_peak_candidates(candidates, first_derivative, second_derivative,
                                                            smoothed_intensity, derivative_window_size, minTraces);

    std::vector<std::pair<int, std::pair<int, int>>> peak_boundaries; // {peak_idx, {left_idx, right_idx}}

    for (int peak_idx : valid_peaks)
    {
      if (peak_idx < minTraces / 2 || peak_idx >= n - minTraces / 2)
        continue;

      auto [left_idx, right_idx] = calculate_peak_boundaries(peak_idx, cluster_rt, smoothed_intensity,
                                                                         baseline, maxWidth / 2.0f, minTraces);
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

          bool rt_overlaps = !(rt_max_i < rt_min_j || rt_max_j < rt_min_i);

          if (rt_overlaps)
          {
            if (debug && cluster_id == debug_cluster)
            {
              Rcpp::Rcout << "DEBUG Merging overlapping peaks: RT1=" << cluster_rt[peak_i]
                         << " (range: " << rt_min_i << "-" << rt_max_i << "), RT2=" << cluster_rt[peak_j]
                         << " (range: " << rt_min_j << "-" << rt_max_j << ")" << std::endl;
            }

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

    // Step 2: Calculate final properties for non-overlapping peaks
    for (const auto& [peak_idx, bounds] : peak_boundaries)
    {
      const auto& [left_idx, right_idx] = bounds;

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
      auto max_it = std::max_element(peak_intensity.begin(), peak_intensity.end());
      float peak_max_intensity = *max_it;
      int max_position = std::distance(peak_intensity.begin(), max_it);

      float rt_at_max = peak_rt[max_position];
      float mz_at_max = peak_mz[max_position];

      // Calculate noise and S/N using both edges to ensure Gaussian shape
      std::vector<float> left_edge_intensities, right_edge_intensities;
      float noise = 0.0f;
      float sn = 0.0f;

      if (peak_intensity.size() >= 4)
      {
        // Get left edge intensities (first two points)
        left_edge_intensities = {peak_intensity[0], peak_intensity[1]};
        // Get right edge intensities (last two points)
        right_edge_intensities = {peak_intensity[peak_intensity.size()-2], peak_intensity[peak_intensity.size()-1]};

        float left_min = *std::min_element(left_edge_intensities.begin(), left_edge_intensities.end());
        float right_min = *std::min_element(right_edge_intensities.begin(), right_edge_intensities.end());

        // Use the maximum of the two edge minimums to ensure both sides are low
        noise = std::max(left_min, right_min);
        sn = (noise > 0) ? peak_max_intensity / noise : 0.0f;
      }
      else
      {
        // For small peaks, use minimum of all points
        noise = *std::min_element(peak_intensity.begin(), peak_intensity.end());
        sn = (noise > 0) ? peak_max_intensity / noise : 0.0f;
      }

      if (sn < minSNR) continue;

      // Create FEATURE structure
      NTS2::FEATURE feature;

      // Basic identification with polarity-specific naming
      std::string polarity_suffix = (polarity_sign > 0) ? "POS" : "NEG";
      feature.analysis = analysis_name;
      feature.feature = "CL" + std::to_string(cluster_id) + "_MZ" + std::to_string(static_cast<int>(std::round(mz_at_max))) +
                       "_RT" + std::to_string(static_cast<int>(std::round(rt_at_max))) + "_" + polarity_suffix;
      feature.group = "";
      feature.component = "";
      feature.adduct = adduct_name; // Set polarity-specific adduct

      // Peak characteristics
      feature.rt = rt_at_max;
      feature.mz = mean(peak_mz);

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

      // Calculate FWHM values
      auto [fwhm_rt_val, fwhm_mz_val] = calculate_fwhm_combined(peak_rt, peak_mz, peak_intensity);
      feature.fwhm_rt = fwhm_rt_val;
      feature.fwhm_mz = fwhm_mz_val;

      // Calculate area
      feature.area = calculate_peak_area(peak_rt, peak_intensity);

      // Gaussian fitting
      feature.gaussian_A = peak_max_intensity;
      feature.gaussian_mu = rt_at_max;
      feature.gaussian_sigma = feature.fwhm_rt / 2.355f; // Convert FWHM to sigma
      if (feature.gaussian_sigma <= 0) feature.gaussian_sigma = feature.width / 4.0f;

      fit_gaussian(peak_rt, peak_smoothed, feature.gaussian_A, feature.gaussian_mu, feature.gaussian_sigma);
      feature.gaussian_r2 = calculate_gaussian_rsquared(peak_rt, peak_smoothed,
                                                                   feature.gaussian_A, feature.gaussian_mu, feature.gaussian_sigma);

      
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

      polarity_features.push_back(feature);
    }
  }

  return polarity_features;
}