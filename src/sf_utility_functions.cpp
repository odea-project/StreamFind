#include "sf_utility_functions.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <map>

void SF_UTILITY::denoise_spectra(
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
  size_t &total_clean_points)
{

  std::vector<std::vector<std::vector<float>>> single_spectrum = ana.get_spectra({spectrum_idx});
  std::vector<float> &raw_mz = single_spectrum[0][0];
  std::vector<float> &raw_intensity = single_spectrum[0][1];
  const int raw_n_traces = raw_mz.size();
  if (raw_n_traces < minTraces)
    return;

  // Count raw data points for denoising statistics
  total_raw_points += raw_n_traces;

  std::vector<float> raw_noise(raw_n_traces);

  int auto_noiseBins;
  float auto_noiseQuantile;
  std::vector<float> intensity_copy = raw_intensity;
  std::sort(intensity_copy.begin(), intensity_copy.end());
  auto_noiseBins = std::max(10, std::min(200, static_cast<int>(std::sqrt(raw_n_traces) * 1.5)));

  // Calculate intensity statistics
  float q25 = intensity_copy[static_cast<int>(raw_n_traces * 0.25)];
  // float q50 = intensity_copy[static_cast<int>(raw_n_traces * 0.50)]; // median
  // float q75 = intensity_copy[static_cast<int>(raw_n_traces * 0.75)];
  float q90 = intensity_copy[static_cast<int>(raw_n_traces * 0.90)];
  // float q95 = intensity_copy[static_cast<int>(raw_n_traces * 0.95)];

  // Calculate interquartile range and coefficient of variation
  float mean_intensity = std::accumulate(raw_intensity.begin(), raw_intensity.end(), 0.0f) / raw_n_traces;
  float variance = 0.0f;
  for (float val : raw_intensity)
  {
    variance += (val - mean_intensity) * (val - mean_intensity);
  }
  variance /= raw_n_traces;
  float cv = std::sqrt(variance) / mean_intensity; // coefficient of variation

  // Adaptive quantile estimation based on data characteristics
  if (cv > 2.0f)
  {
    // High variability - use moderate quantile to balance noise removal vs signal preservation
    auto_noiseQuantile = 0.05f; // 5% - captures lower noise floor in variable data
  }
  else if (cv > 1.0f)
  {
    // Medium variability - standard MS denoising approach
    auto_noiseQuantile = 0.10f; // 10% - typical for MS data noise removal
  }
  else
  {
    // Low variability - more aggressive denoising possible
    auto_noiseQuantile = 0.20f; // 20% - can remove more noise when data is consistent
  }

  // Adjust quantile based on signal-to-noise characteristics
  float signal_noise_ratio = q90 / q25;
  if (signal_noise_ratio > 100)
  {
    // High dynamic range - more conservative noise estimation
    auto_noiseQuantile *= 0.5f;
  }
  else if (signal_noise_ratio < 10)
  {
    // Low dynamic range - less conservative
    auto_noiseQuantile *= 1.5f;
  }

  // Ensure quantile is within reasonable bounds
  auto_noiseQuantile = std::max(0.01f, std::min(0.30f, auto_noiseQuantile));

  // Adjust number of bins based on data sparsity
  float data_sparsity = static_cast<float>(raw_n_traces) / auto_noiseBins;
  if (data_sparsity < 5)
  {
    // Too few points per bin - reduce number of bins
    auto_noiseBins = std::max(5, raw_n_traces / 5);
  }
  else if (data_sparsity > 50)
  {
    // Too many points per bin - increase number of bins
    auto_noiseBins = std::min(200, raw_n_traces / 20);
  }

  if (debug)
  {
    Rcpp::Rcout << "DEBUG Auto noise estimation: bins=" << auto_noiseBins
                << ", quantile = " << auto_noiseQuantile
                << " (CV = " << cv << ", SNR = " << signal_noise_ratio
                << ", n = " << raw_n_traces << ")" << std::endl;
  }

  // Create bins for local quantile calculation
  std::vector<int> bins(raw_n_traces);
  for (int j = 0; j < raw_n_traces; ++j)
  {
    bins[j] = std::min(static_cast<int>(j * auto_noiseBins / raw_n_traces), auto_noiseBins - 1);
  }

  // Calculate quantiles for each bin
  std::vector<float> bin_quantiles(auto_noiseBins, noiseThreshold);
  for (int bin = 0; bin < auto_noiseBins; ++bin)
  {
    std::vector<float> bin_intensities;
    for (int j = 0; j < raw_n_traces; ++j)
    {
      if (bins[j] == bin)
      {
        bin_intensities.push_back(raw_intensity[j]);
      }
    }
    if (!bin_intensities.empty())
    {
      std::sort(bin_intensities.begin(), bin_intensities.end());
      int quantile_idx = std::max(0, std::min(static_cast<int>(bin_intensities.size()) - 1,
                                              static_cast<int>(bin_intensities.size() * auto_noiseQuantile)));
      bin_quantiles[bin] = std::max(bin_intensities[quantile_idx], noiseThreshold);
    }
  }

  // Assign noise levels
  for (int j = 0; j < raw_n_traces; ++j)
  {
    raw_noise[j] = bin_quantiles[bins[j]];
  }

  std::vector<float> clean_mz, clean_intensity, clean_noise;
  clean_mz.reserve(raw_n_traces);
  clean_intensity.reserve(raw_n_traces);
  clean_noise.reserve(raw_n_traces);

  for (int j = 0; j < raw_n_traces; ++j)
  {
    if (raw_intensity[j] > raw_noise[j])
    {
      clean_mz.push_back(raw_mz[j]);
      clean_intensity.push_back(raw_intensity[j]);
      clean_noise.push_back(raw_noise[j]);
    }
  }

  if (clean_mz.empty())
    return;

  // Sort by mz - optimized version
  const size_t clean_size = clean_mz.size();

  // Count clean data points for denoising statistics
  total_clean_points += clean_size;

  std::vector<size_t> indices(clean_size);
  std::iota(indices.begin(), indices.end(), 0);
  std::sort(indices.begin(), indices.end(), [&](size_t a, size_t b)
            { return clean_mz[a] < clean_mz[b]; });

  // Apply sorting - optimized version
  std::vector<float> clean_sorted_mz, clean_sorted_intensity, clean_sorted_noise;
  clean_sorted_mz.reserve(clean_size);
  clean_sorted_intensity.reserve(clean_size);
  clean_sorted_noise.reserve(clean_size);

  for (size_t idx : indices)
  {
    clean_sorted_mz.push_back(clean_mz[idx]);
    clean_sorted_intensity.push_back(clean_intensity[idx]);
    clean_sorted_noise.push_back(clean_noise[idx]);
  }

  std::vector<int> cent_clusters;
  cent_clusters.reserve(clean_sorted_mz.size());

  if (clean_sorted_mz.size() > 0)
  {
    std::vector<float> mz_diffs;
    std::vector<float> mz_thresholds;
    mz_diffs.reserve(clean_sorted_mz.size() - 1);
    mz_thresholds.reserve(clean_sorted_mz.size() - 1);

    for (size_t j = 1; j < clean_sorted_mz.size(); ++j)
    {
      mz_diffs.push_back(clean_sorted_mz[j] - clean_sorted_mz[j - 1]);
      mz_thresholds.push_back(calculate_mz_threshold_linear(clean_sorted_mz[j], slope, intercept));
    }

    cent_clusters.push_back(0); // First element always cluster 0
    int current_cluster = 0;

    std::transform(mz_diffs.begin(), mz_diffs.end(), mz_thresholds.begin(),
                   std::back_inserter(cent_clusters),
                   [&current_cluster](float diff, float threshold)
                   {
                     if (diff > threshold)
                     {
                       current_cluster++;
                     }
                     return current_cluster;
                   });
  }

  // Aggregate by cluster (keep max intensity peak per cluster)
  std::map<int, std::tuple<float, float, float>> cent_cluster_data; // cluster -> (mz, intensity, noise)

  for (size_t j = 0; j < clean_sorted_mz.size(); ++j)
  {
    int cluster = cent_clusters[j];
    if (cent_cluster_data.find(cluster) == cent_cluster_data.end())
    {
      cent_cluster_data[cluster] = std::make_tuple(clean_sorted_mz[j], clean_sorted_intensity[j], clean_sorted_noise[j]);
    }
    else
    {
      if (clean_sorted_intensity[j] > std::get<1>(cent_cluster_data[cluster]))
      {
        cent_cluster_data[cluster] = std::make_tuple(clean_sorted_mz[j], clean_sorted_intensity[j], clean_sorted_noise[j]);
      }
    }
  }

  for (const auto &[cluster, data_tuple] : cent_cluster_data)
  {
    spec_rt.push_back(rt);
    spec_mz.push_back(std::get<0>(data_tuple));
    spec_intensity.push_back(std::get<1>(data_tuple));
    spec_noise.push_back(std::get<2>(data_tuple));
  }
};