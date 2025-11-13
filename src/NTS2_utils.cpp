#include "NTS2_utils.h"

void NTS2::NTS_DATA::find_features(
    const std::vector<float> &rtWindowsMin,
    const std::vector<float> &rtWindowsMax,
    const std::vector<int> &resolution_profile,
    const float &noiseThreshold,
    const float &minSNR,
    const int &minTraces,
    const float &baselineWindow,
    const float &maxWidth)
{

  if (resolution_profile.size() != 3)
  {
    Rcpp::Rcout << "Error: resolution_profile must have exactly 3 elements correspondent to 100, 400, and 1000 Da correspondent resolutions!" << std::endl;
    return;
  }
  if (rtWindowsMin.size() != rtWindowsMax.size())
  {
    Rcpp::Rcout << "Error: rtWindowsMin and rtWindowsMax must have the same length!" << std::endl;
    return;
  }

  const auto [slope, intercept] = SF_UTILITY::calculate_mass_resolution_model_param(resolution_profile);
  Rcpp::Rcout << std::endl;
  Rcpp::Rcout << "Linear resolution model threshold = " << slope << " * m/z + " << intercept << std::endl;
  Rcpp::Rcout << "Reference thresholds: " << std::endl;
  for (float test_mz : {100.0f, 400.0f, 1000.0f})
  {
    float mzThreshold = SF_UTILITY::calculate_mass_resolution_model_threshold(test_mz, slope, intercept);
    Rcpp::Rcout << "  m/z " << test_mz << " -> threshold " << mzThreshold << std::endl;
  }

  // Debug cluster to track in detail
  const int debug_cluster = 160;
  bool debug = true;

  for (size_t a = 0; a < analyses.size(); ++a)
  {
    Rcpp::Rcout << std::endl;
    Rcpp::Rcout << a + 1 << "/" << analyses.size() << " Processing analysis " << analyses[a] << std::endl;
    const sc::MS_SPECTRA_HEADERS &header = headers[a];
    std::vector<int> idx_load;
    std::vector<float> rt_load;
    if (rtWindowsMin.size() > 0)
    {
      const std::vector<float> &rts = header.rt;
      for (size_t w = 0; w < rtWindowsMin.size(); ++w)
      {
        for (size_t i = 0; i < rts.size(); ++i)
        {
          if (rts[i] >= rtWindowsMin[w] && rts[i] <= rtWindowsMax[w])
          {
            if (header.level[i] == 1)
            {
              idx_load.push_back(header.index[i]);
              rt_load.push_back(header.rt[i]);
            }
          }
        }
      }
    }
    else
    {
      idx_load = header.index;
      rt_load = header.rt;
    }

    sc::MS_FILE ana(files[a]);
    std::vector<float> spec_rt, spec_mz, spec_intensity, spec_noise;

    Rcpp::Rcout << "  1/5 Denoising " << idx_load.size() << " spectra" << std::endl;

    size_t total_raw_points = 0;
    size_t total_clean_points = 0;

    for (size_t i = 0; i < idx_load.size(); ++i)
    {
      const float &rt = rt_load[i];
      const int &spectrum_idx = idx_load[i];

      SF_UTILITY::denoise_spectra(
        ana,
        spectrum_idx,
        rt,
        noiseThreshold,
        minTraces,
        slope,
        intercept,
        spec_rt,
        spec_mz,
        spec_intensity,
        spec_noise,
        total_raw_points,
        total_clean_points,
        false
      );
    }

    Rcpp::Rcout << "  2/5 Clustering " << spec_rt.size() << " denoised traces by m/z" << std::endl;

    std::vector<float> clust_rt_sorted_rt, clust_rt_sorted_mz, clust_rt_sorted_intensity, clust_rt_sorted_noise;
    std::vector<int> clust_rt_sorted_cluster;
    int number_clusters = 0;

    SF_UTILITY::cluster_spectra_by_mz(
        spec_rt, spec_mz, spec_intensity, spec_noise,
        slope, intercept, minTraces, minSNR,
        clust_rt_sorted_rt, clust_rt_sorted_mz, clust_rt_sorted_intensity,
        clust_rt_sorted_noise, clust_rt_sorted_cluster, number_clusters
    );

    Rcpp::Rcout << "  3/5 Detecting peaks in " << number_clusters << " valid m/z clusters" << std::endl;

    std::map<int, std::vector<int>> cluster_indices;
    for (size_t i = 0; i < clust_rt_sorted_cluster.size(); ++i)
    {
      cluster_indices[clust_rt_sorted_cluster[i]].push_back(static_cast<int>(i));
    }

    std::vector<FEATURE> all_features;

    for (const auto& [cluster_id, indices] : cluster_indices)
    {
      if (indices.size() < static_cast<size_t>(minTraces))
        continue;

      if (debug && cluster_id == debug_cluster)
      {
        Rcpp::Rcout << "DEBUG Processing cluster " << cluster_id << " with " << indices.size() << " traces" << std::endl;
      }

      std::vector<float> cluster_rt, cluster_mz, cluster_intensity, cluster_noise;
      cluster_rt.reserve(indices.size());
      cluster_mz.reserve(indices.size());
      cluster_intensity.reserve(indices.size());
      cluster_noise.reserve(indices.size());
      for (int idx : indices)
      {
        cluster_rt.push_back(clust_rt_sorted_rt[idx]);
        cluster_mz.push_back(clust_rt_sorted_mz[idx]);
        cluster_intensity.push_back(clust_rt_sorted_intensity[idx]);
        cluster_noise.push_back(clust_rt_sorted_noise[idx]);
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

      auto baseline = SF_UTILITY::calculate_baseline(cluster_intensity, baseline_window_size);
      auto smoothed_intensity = SF_UTILITY::smooth_intensity(cluster_intensity, 3);
      std::vector<float> first_derivative, second_derivative;
      SF_UTILITY::calculate_derivatives(smoothed_intensity, first_derivative, second_derivative);
      auto candidates = SF_UTILITY::find_peak_candidates(first_derivative);
      auto valid_peaks = SF_UTILITY::validate_peak_candidates(candidates, first_derivative, second_derivative,
                                                              smoothed_intensity, derivative_window_size, minTraces);

      std::vector<std::pair<int, std::pair<int, int>>> peak_boundaries; // {peak_idx, {left_idx, right_idx}}

      for (int peak_idx : valid_peaks)
      {
        if (peak_idx < minTraces / 2 || peak_idx >= n - minTraces / 2)
          continue;

        auto [left_idx, right_idx] = SF_UTILITY::calculate_peak_boundaries(peak_idx, cluster_rt, smoothed_intensity,
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
        FEATURE feature;

        // Basic identification
        feature.analysis = analyses[a];
        feature.feature = "CL" + std::to_string(cluster_id) + "_MZ" + std::to_string(static_cast<int>(std::round(mz_at_max))) +
                         "_RT" + std::to_string(static_cast<int>(std::round(rt_at_max)));
        feature.group = "";
        feature.component = "";
        feature.adduct = "";

        // Peak characteristics
        feature.rt = rt_at_max;

        // Calculate intensity-weighted average m/z within peak boundaries for better representation
        float mz_sum = 0.0f, intensity_sum = 0.0f;
        for (size_t i = 0; i < peak_mz.size(); ++i)
        {
          mz_sum += peak_mz[i] * peak_intensity[i];
          intensity_sum += peak_intensity[i];
        }
        feature.mz = intensity_sum > 0 ? mz_sum / intensity_sum : SF_UTILITY::mean(peak_mz);

        // Assume positive polarity and calculate neutral mass (subtract proton)
        feature.mass = feature.mz - 1.007276f; // Subtract proton mass for positive mode

        feature.intensity = peak_max_intensity;
        feature.noise = noise;
        feature.sn = sn;
        feature.polarity = 1; // Assuming positive mode for now

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
        auto [fwhm_rt_val, fwhm_mz_val] = SF_UTILITY::calculate_fwhm_combined(peak_rt, peak_mz, peak_intensity);
        feature.fwhm_rt = fwhm_rt_val;
        feature.fwhm_mz = fwhm_mz_val;

        // Calculate area
        feature.area = SF_UTILITY::calculate_peak_area(peak_rt, peak_intensity);

        // Gaussian fitting
        feature.gaussian_A = peak_max_intensity;
        feature.gaussian_mu = rt_at_max;
        feature.gaussian_sigma = feature.fwhm_rt / 2.355f; // Convert FWHM to sigma
        if (feature.gaussian_sigma <= 0) feature.gaussian_sigma = feature.width / 4.0f;

        SF_UTILITY::fit_gaussian(peak_rt, peak_smoothed, feature.gaussian_A, feature.gaussian_mu, feature.gaussian_sigma);
        feature.gaussian_r2 = SF_UTILITY::calculate_gaussian_rsquared(peak_rt, peak_smoothed,
                                                                     feature.gaussian_A, feature.gaussian_mu, feature.gaussian_sigma);

        // Set default values as requested
        feature.filtered = false;
        feature.filter = "";
        feature.filled = false;
        feature.correction = 1.0f;

        // Serialize and encode EIC data
        feature.eic_size = static_cast<int>(peak_rt.size());

        // Use StreamCraft functions to encode vectors as little-endian then base64
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

        all_features.push_back(feature);
      }
    }

    Rcpp::Rcout << "  4/5 Found " << all_features.size() << " total features after overlap resolution" << std::endl;

    // Store features in the NTS_DATA structure
    features[a] = FEATURES(); // Initialize empty FEATURES structure
    features[a].analysis = analyses[a];

    for (const auto& feature : all_features)
    {
      features[a].append_feature(feature);
    }
    Rcpp::Rcout << "  5/5 Processing complete" << std::endl;
  }
};
