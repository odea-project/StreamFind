#include "sf_utility_functions.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <map>
#include <unordered_map>
#include <set>
#include <functional>

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
  quantile = std::clamp(quantile, 0.01f, 0.30f);
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

// MARK: CORE UTILITY FUNCTION IMPLEMENTATIONS

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

// MARK: SPECTRAL FUNCTION IMPLEMENTATIONS

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
  const size_t n = raw_mz.size();
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
                                 size_t &total_raw_points, size_t &total_clean_points, const bool &debug)
{
  std::vector<std::vector<std::vector<float>>> single_spectrum = ana.get_spectra({spectrum_idx});
  std::vector<float> &raw_mz = single_spectrum[0][0];
  std::vector<float> &raw_intensity = single_spectrum[0][1];

  const int raw_n_traces = raw_mz.size();
  if (raw_n_traces < minTraces)
    return;

  total_raw_points += raw_n_traces;

  VectorStats stats(raw_intensity);
  AdaptiveNoiseParams noise_params(stats, raw_n_traces);

  if (debug)
  {
    Rcpp::Rcout << "DEBUG Auto noise estimation: bins=" << noise_params.bins
                << ", quantile=" << noise_params.quantile
                << " (CV=" << stats.coefficient_variation << ", SNR=" << stats.signal_noise_ratio
                << ", n=" << raw_n_traces << ")" << std::endl;
  }

  auto raw_noise = SF_UTILITY::calculate_noise_levels(raw_intensity, noise_params, noiseThreshold);

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

// MARK: PEAK DETECTION AND ANALYSIS IMPLEMENTATIONS

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