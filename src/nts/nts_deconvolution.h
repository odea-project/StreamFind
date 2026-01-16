// nts_deconvolution.h
// Feature detection utilities specifically used by NTS_DATA::find_features
// This file contains spectral processing, peak detection, and quality metrics functions

#ifndef NTS_DECONVOLUTION_H
#define NTS_DECONVOLUTION_H

#include <Rcpp.h>
#include <vector>
#include <string>
#include "../streamcraft/streamcraft.h"
#include "nts_utils.h"

namespace nts {
  struct NTS_DATA;  // Forward declaration
  struct FEATURE;   // Forward declaration
}

namespace nts {
namespace deconvolution {

  struct VectorStats
  {
    float mean, std_dev, coefficient_variation;
    float min_val, max_val;
    float signal_noise_ratio;
    size_t count;

    VectorStats() : mean(0.0f), std_dev(0.0f), coefficient_variation(0.0f),
                    min_val(0.0f), max_val(0.0f), signal_noise_ratio(0.0f), count(0) {}

    explicit VectorStats(const std::vector<float> &data);
  };

  struct AdaptiveNoiseParams
  {
    int bins;
    float quantile;
    float threshold_multiplier;

    AdaptiveNoiseParams(const VectorStats &stats, int data_size, float baseQuantile = 0.10f);
  };

  std::vector<int> calculate_bin_assignments(const std::vector<float> &data, int num_bins);

  std::vector<float> calculate_noise_levels(
    const std::vector<float> &intensities,
    const AdaptiveNoiseParams &params,
    float noise_threshold);

  std::vector<int> cluster_by_mz(const std::vector<float> &mz_values, const float &ppmThreshold);

  void filter_and_cluster(
    const std::vector<float> &raw_mz,
    const std::vector<float> &raw_intensity,
    const std::vector<float> &raw_noise,
    const float &ppmThreshold,
    std::vector<float> &final_mz,
    std::vector<float> &final_intensity,
    std::vector<float> &final_noise);

  void denoise_spectra(
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
    const float &baseQuantile = 0.10f);

  struct SpectraPoint
  {
    float rt, mz, intensity, noise;
    int cluster;
    int id;

    SpectraPoint(float r = 0.0f, float m = 0.0f, float i = 0.0f, float n = 0.0f, int c = -1, int point_id = -1)
        : rt(r), mz(m), intensity(i), noise(n), cluster(c), id(point_id) {}
  };

  struct ClusterStats
  {
    int count;
    float max;
    float min;
    float mean;
    float sum;

    explicit ClusterStats(float intensity = 0.0f)
        : count(1), max(intensity), min(intensity),
          mean(intensity), sum(intensity) {}

    void update(float intensity)
    {
      count++;
      max = std::max(max, intensity);
      min = std::min(min, intensity);
      sum += intensity;
      mean = sum / count;
    }

    float range() const { return max - min; }
    float snr() const { return max / std::max(min, 1e-8f); }
  };

  std::vector<size_t> filter_valid_clusters(
    const std::vector<SpectraPoint> &data,
    const std::vector<int> &clusters,
    int minTraces, float minSNR);

  void sort_by_rt_inplace(
    std::vector<float> &rt,
    std::vector<float> &mz,
    std::vector<float> &intensity,
    std::vector<float> &noise,
    std::vector<int> &cluster);

  void cluster_spectra_by_mz(
    const std::vector<float> &spec_rt,
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
    int &number_clusters);

  std::vector<int> find_peak_candidates(
    const std::vector<float> &first_derivative,
    const std::vector<float> &raw_intensity = std::vector<float>(),
    int refineWindow = 0);

  std::vector<int> validate_peak_candidates(
    const std::vector<int> &candidates,
    const std::vector<float> &first_derivative,
    const std::vector<float> &second_derivative,
    const std::vector<float> &smoothed_intensity,
    const std::vector<float> &rt = std::vector<float>(),
    const std::vector<float> &intensity = std::vector<float>(),
    bool debug = false);

  std::pair<int, int> calculate_peak_boundaries(
    int peak_idx,
    const std::vector<float> &rt,
    const std::vector<float> &smoothed_intensity,
    const std::vector<float> &baseline,
    float max_half_width,
    int min_traces,
    float cycle_time,
    bool debug = false,
    float debugMZ = 0.0f);

  float calculate_fwhm_rt(const std::vector<float> &rt, const std::vector<float> &intensity);

  std::tuple<float, float, float> calculate_fwhm_combined(
    const std::vector<float> &rt,
    const std::vector<float> &mz,
    const std::vector<float> &intensity);

  std::vector<nts::FEATURE> process_polarity_clusters(
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
      float debugMZ = 0.0f);

  void find_features_impl(
      nts::NTS_DATA &nts_data,
      const std::vector<float> &rtWindowsMin,
      const std::vector<float> &rtWindowsMax,
      const float &ppmThreshold,
      const float &noiseThreshold,
      const float &minSNR,
      const int &minTraces,
      const float &baselineWindow,
      const float &maxWidth,
      const float &baseQuantile,
      const std::string &debugAnalysis,
      const float &debugMZ,
      const int &debugSpecIdx);

} // namespace deconvolution
} // namespace nts

#endif // NTS_DECONVOLUTION_H
