#ifndef SF_UTILITY_FUNCTIONS_H
#define SF_UTILITY_FUNCTIONS_H

#include <Rcpp.h>
#include "StreamCraft_lib.h"
#include <vector>

// Forward declaration to avoid circular dependency
namespace NTS2 {
  struct FEATURE;
}

namespace SF_UTILITY
{
  // MARK: DATA STRUCTURES

  // Spectral data point structure optimized for cache performance
  struct SpectraPoint
  {
    float rt, mz, intensity, noise;
    int cluster;
    int id;

    SpectraPoint(float r = 0.0f, float m = 0.0f, float i = 0.0f, float n = 0.0f, int c = -1, int point_id = -1)
        : rt(r), mz(m), intensity(i), noise(n), cluster(c), id(point_id) {}
  };

  // Comprehensive statistics structure for float data
  struct VectorStats
  {
    float mean, std_dev, coefficient_variation;
    float min_val, max_val;
    float signal_noise_ratio;
    size_t count;

    // Default constructor
    VectorStats() : mean(0.0f), std_dev(0.0f), coefficient_variation(0.0f),
                    min_val(0.0f), max_val(0.0f), signal_noise_ratio(0.0f), count(0) {}

    // Constructor from data vector (implementation in .cpp)
    explicit VectorStats(const std::vector<float> &data);
  };

  // Cluster statistics for efficient processing
  struct ClusterStats
  {
    int count;
    float max;
    float min;
    float mean;
    float sum;

    explicit ClusterStats(float intensity = 0.0f);
    void update(float intensity);

    float range() const { return max - min; }
    float snr() const { return max / std::max(min, 1e-8f); }
  };

  // Adaptive parameters for noise estimation and processing
  struct AdaptiveNoiseParams
  {
    int bins;
    float quantile;
    float threshold_multiplier;

    AdaptiveNoiseParams(const VectorStats &stats, int data_size, float base_quantile = 0.10f);
  };

  // MARK: CORE UTILITY FUNCTIONS
  Rcpp::List get_empty_dt();

  bool check_list_must_have_names(const Rcpp::List &list,
                                  const std::vector<std::string> &must_have_names);

  float mean(const std::vector<float> &v);
  float standard_deviation(const std::vector<float> &v, float mean_val);
  float quantile(std::vector<float> data, float quantile_fraction);
  size_t find_max_index(const std::vector<float> &v);
  size_t find_min_index(const std::vector<float> &v);

  float gaussian_function(const float &A, const float &mu,
                          const float &sigma, const float &x);

  sc::MS_SPECTRA_HEADERS as_MS_SPECTRA_HEADERS(const Rcpp::List &hd);

  std::pair<float, float> calculate_mass_resolution_model_param(const std::vector<int> &resolution_profile);
  float calculate_mass_resolution_model_threshold(float mz, float slope, float intercept);
  float calculate_mz_threshold_linear(float mz, float slope, float intercept);

  // Sorting indices for float data
  std::vector<size_t> get_sort_indices_float(const std::vector<float> &data);

  // Generic sorting indices with custom comparison
  std::vector<size_t> get_sort_indices_custom(const std::vector<float> &data,
                                              bool ascending = true);

  // Fast data reordering by indices
  void reorder_float_data(std::vector<float> &data, const std::vector<size_t> &indices);
  void reorder_int_data(std::vector<int> &data, const std::vector<size_t> &indices);

  // Reorder multiple vectors simultaneously (overloaded for different combinations)
  void reorder_multiple_vectors(const std::vector<size_t> &indices,
                                std::vector<float> &vec1);
  void reorder_multiple_vectors(const std::vector<size_t> &indices,
                                std::vector<float> &vec1,
                                std::vector<float> &vec2);
  void reorder_multiple_vectors(const std::vector<size_t> &indices,
                                std::vector<float> &vec1,
                                std::vector<float> &vec2,
                                std::vector<float> &vec3);
  void reorder_multiple_vectors(const std::vector<size_t> &indices,
                                std::vector<float> &vec1,
                                std::vector<float> &vec2,
                                std::vector<float> &vec3,
                                std::vector<float> &vec4);
  void reorder_multiple_vectors(const std::vector<size_t> &indices,
                                std::vector<float> &vec1,
                                std::vector<float> &vec2,
                                std::vector<float> &vec3,
                                std::vector<float> &vec4,
                                std::vector<int> &int_vec);

  // Efficient bin assignment calculation
  std::vector<int> calculate_bin_assignments(const std::vector<float> &data, int num_bins);

  // Fast filtering with simple predicates
  std::vector<size_t> filter_above_threshold(const std::vector<float> &data,
                                             const std::vector<float> &thresholds);

  std::vector<size_t> filter_above_value(const std::vector<float> &data, float threshold);

  // Optimized clustering by threshold
  std::vector<int> cluster_by_threshold_float(const std::vector<float> &sorted_data,
                                              const std::vector<float> &thresholds);

  // MARK: SPECIALIZED SPECTRAL FUNCTIONS

  // Fast m/z-based clustering using optimized algorithms
  std::vector<int> cluster_by_mz(const std::vector<float> &mz_values,
                                 float slope, float intercept);

  // Cluster validation and filtering for spectral data
  std::vector<size_t> filter_valid_clusters(const std::vector<SpectraPoint> &data,
                                            const std::vector<int> &clusters,
                                            int minTraces, float minSNR);

  // Sort multiple vectors by reference vector (RT-based sorting)
  void sort_by_rt_inplace(std::vector<float> &rt, std::vector<float> &mz,
                          std::vector<float> &intensity, std::vector<float> &noise,
                          std::vector<int> &cluster);

  // Complete spectral processing pipeline
  void cluster_spectra_by_mz(const std::vector<float> &spec_rt,
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
                             int &number_clusters);

  // MARK: NOISE PROCESSING FUNCTIONS

  // Optimized noise level calculation with adaptive binning
  std::vector<float> calculate_noise_levels(const std::vector<float> &intensities,
                                            const AdaptiveNoiseParams &params,
                                            float noise_threshold);

  // Fast filtering and clustering pipeline for denoising
  void filter_and_cluster(const std::vector<float> &raw_mz,
                          const std::vector<float> &raw_intensity,
                          const std::vector<float> &raw_noise,
                          float slope, float intercept,
                          std::vector<float> &final_mz,
                          std::vector<float> &final_intensity,
                          std::vector<float> &final_noise);

  // Optimized spectral denoising function
  void denoise_spectra(sc::MS_FILE &ana, const int &spectrum_idx, const float &rt,
                       const float &noiseThreshold, const int &minTraces,
                       const float &slope, const float &intercept,
                       std::vector<float> &spec_rt, std::vector<float> &spec_mz,
                       std::vector<float> &spec_intensity, std::vector<float> &spec_noise,
                       size_t &total_raw_points, size_t &total_clean_points, const bool &debug,
                       const float &base_quantile = 0.10f);

  // MARK: PEAK DETECTION AND ANALYSIS FUNCTIONS

  // Baseline calculation using moving minimum
  std::vector<float> calculate_baseline(const std::vector<float> &intensity, int window_size);

  // Smooth intensity data
  std::vector<float> smooth_intensity(const std::vector<float> &intensity, int window_size);

  // Calculate derivatives for peak detection
  void calculate_derivatives(const std::vector<float> &smoothed_intensity,
                            std::vector<float> &first_derivative,
                            std::vector<float> &second_derivative);

  // Find peak candidates based on derivative analysis
  std::vector<int> find_peak_candidates(const std::vector<float> &first_derivative);

  // Validate peak candidates using derivative criteria
  std::vector<int> validate_peak_candidates(const std::vector<int> &candidates,
                                           const std::vector<float> &first_derivative,
                                           const std::vector<float> &second_derivative,
                                           const std::vector<float> &smoothed_intensity,
                                           int derivative_window_size, int min_traces);

  // Calculate peak boundaries
  std::pair<int, int> calculate_peak_boundaries(int peak_idx,
                                               const std::vector<float> &rt,
                                               const std::vector<float> &smoothed_intensity,
                                               const std::vector<float> &baseline,
                                               float max_half_width, int min_traces);

  // Calculate FWHM boundaries
  std::pair<int, int> calculate_fwhm_boundaries(int peak_idx,
                                               const std::vector<float> &rt,
                                               const std::vector<float> &intensity,
                                               int left_boundary, int right_boundary);

  // Calculate peak area using trapezoidal integration
  float calculate_peak_area(const std::vector<float> &rt, const std::vector<float> &intensity);

  // Calculate FWHM (Full Width at Half Maximum)
  float calculate_fwhm_rt(const std::vector<float> &rt, const std::vector<float> &intensity);

  // Calculate FWHM in m/z dimension
  float calculate_fwhm_mz(const std::vector<float> &mz, const std::vector<float> &intensity);

  // Combined FWHM calculation for both RT and MZ dimensions
  std::pair<float, float> calculate_fwhm_combined(const std::vector<float> &rt,
                                                  const std::vector<float> &mz,
                                                  const std::vector<float> &intensity);

  // Gaussian fitting functions
  float gaussian_cost_function(const std::vector<float> &x, const std::vector<float> &y,
                              float A, float mu, float sigma);

  void fit_gaussian(const std::vector<float> &x, const std::vector<float> &y,
                   float &A, float &mu, float &sigma);

  float calculate_gaussian_rsquared(const std::vector<float> &x, const std::vector<float> &y,
                                   float A, float mu, float sigma);

  // MARK: POLARITY-SPECIFIC PROCESSING FUNCTIONS
  
  // Process clusters for a specific polarity and return features
  std::vector<NTS2::FEATURE> process_polarity_clusters(
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
      int debug_cluster);

}; // namespace SF_UTILITY

#endif