// nts_gap_filling.cpp
// Feature gap filling implementations for NTS_DATA
// This file contains the logic for identifying and filling missing features across analyses

#include "nts_gap_filling.h"
#include "nts_utils.h"
#include "nts_deconvolution.h"
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
#include <sstream>

// MARK: analyze_feature_groups
std::vector<nts::gap_filling::FEATURE_GROUP_INFO> nts::gap_filling::analyze_feature_groups(
    const std::vector<nts::FEATURES> &features,
    const std::vector<std::string> &analyses,
    const std::vector<std::string> &replicates,
    bool withinReplicate,
    int minSamples)
{
  // Build analysis to replicate map
  std::unordered_map<std::string, std::string> analysis_replicate_map;
  for (size_t i = 0; i < analyses.size(); ++i)
  {
    analysis_replicate_map[analyses[i]] = replicates[i];
  }

  // Build a map of feature groups
  std::unordered_map<std::string, std::vector<std::tuple<std::string, float, float, float>>> group_map;

  for (const auto &fts : features)
  {
    for (int i = 0; i < fts.size(); ++i)
    {
      std::string fg = fts.feature_group[i];
      if (fg.empty() || fg == "")
        continue;

      group_map[fg].push_back(std::make_tuple(
          fts.analysis,
          fts.rt[i],
          fts.mz[i],
          fts.mass[i]));
    }
  }

  // Analyze each group
  std::vector<FEATURE_GROUP_INFO> group_infos;

  for (const auto &[fg, group_features] : group_map)
  {
    if (static_cast<int>(group_features.size()) < minSamples)
      continue;

    FEATURE_GROUP_INFO info;
    info.feature_group = fg;
    info.total_features = group_features.size();

    // Calculate median values and collect RT/m/z ranges
    std::vector<float> rts, mzs, masses;
    std::vector<float> rtmins, rtmaxs, mzmins, mzmaxs;
    std::set<std::string> present_set;

    // Need to get full feature data to extract rtmin, rtmax, mzmin, mzmax
    for (const auto &fts : features)
    {
      for (int i = 0; i < fts.size(); ++i)
      {
        if (fts.feature_group[i] == fg)
        {
          rts.push_back(fts.rt[i]);
          mzs.push_back(fts.mz[i]);
          masses.push_back(fts.mass[i]);
          rtmins.push_back(fts.rtmin[i]);
          rtmaxs.push_back(fts.rtmax[i]);
          mzmins.push_back(fts.mzmin[i]);
          mzmaxs.push_back(fts.mzmax[i]);
          present_set.insert(fts.analysis);
        }
      }
    }

    // Calculate medians
    std::sort(rts.begin(), rts.end());
    std::sort(mzs.begin(), mzs.end());
    std::sort(masses.begin(), masses.end());

    size_t mid = rts.size() / 2;
    info.median_rt = (rts.size() % 2 == 0) ? (rts[mid - 1] + rts[mid]) / 2.0f : rts[mid];
    info.median_mz = (mzs.size() % 2 == 0) ? (mzs[mid - 1] + mzs[mid]) / 2.0f : mzs[mid];
    info.median_mass = (masses.size() % 2 == 0) ? (masses[mid - 1] + masses[mid]) / 2.0f : masses[mid];

    // Calculate RT range
    info.rt_range = rts.back() - rts.front();

    // Calculate min/max ranges from feature boundaries
    info.min_rtmin = *std::min_element(rtmins.begin(), rtmins.end());
    info.max_rtmax = *std::max_element(rtmaxs.begin(), rtmaxs.end());
    info.min_mzmin = *std::min_element(mzmins.begin(), mzmins.end());
    info.max_mzmax = *std::max_element(mzmaxs.begin(), mzmaxs.end());

    // If withinReplicate is true, only consider gaps within replicates that have the feature
    if (withinReplicate)
    {
      // Build set of replicates that have this feature group
      std::set<std::string> replicates_with_feature;
      for (const auto &analysis : present_set)
      {
        auto rep_it = analysis_replicate_map.find(analysis);
        if (rep_it != analysis_replicate_map.end())
        {
          replicates_with_feature.insert(rep_it->second);
        }
      }

      // Identify present and missing analyses only within replicates that have the feature
      for (const auto &analysis : analyses)
      {
        auto rep_it = analysis_replicate_map.find(analysis);
        if (rep_it == analysis_replicate_map.end())
          continue;

        std::string replicate = rep_it->second;

        // Only consider this analysis if its replicate has the feature group
        if (replicates_with_feature.find(replicate) == replicates_with_feature.end())
          continue;

        if (present_set.find(analysis) != present_set.end())
        {
          info.present_analyses.push_back(analysis);
        }
        else
        {
          info.missing_analyses.push_back(analysis);
        }
      }
    }
    else
    {
      // Identify present and missing analyses across all analyses
      for (const auto &analysis : analyses)
      {
        if (present_set.find(analysis) != present_set.end())
        {
          info.present_analyses.push_back(analysis);
        }
        else
        {
          info.missing_analyses.push_back(analysis);
        }
      }
    }

    // Only add groups that have missing analyses
    if (!info.missing_analyses.empty())
    {
      group_infos.push_back(info);
    }
  }

  return group_infos;
}

// MARK: extract_eic_for_gap_filling
nts::gap_filling::EIC_DATA nts::gap_filling::extract_eic_for_gap_filling(
    mass_spec::MS_FILE &ana,
    const mass_spec::MS_SPECTRA_HEADERS &headers,
    float target_mz,
    float target_rt,
    float mzExpand,
    float rtExpand,
    float minTracesIntensity,
    float ppmThreshold)
{
  EIC_DATA eic;
  eic.valid = false;
  eic.size = 0;

  // Define RT and m/z windows
  float rt_min = target_rt - rtExpand;
  float rt_max = target_rt + rtExpand;
  float mz_min = target_mz - mzExpand;
  float mz_max = target_mz + mzExpand;

  // Find spectra within RT window
  std::vector<int> spec_indices;
  for (size_t i = 0; i < headers.rt.size(); ++i)
  {
    if (headers.level[i] == 1 &&
        headers.rt[i] >= rt_min &&
        headers.rt[i] <= rt_max)
    {
      spec_indices.push_back(headers.index[i]);
    }
  }

  if (spec_indices.empty())
    return eic;

  // Extract traces within m/z window
  for (int idx : spec_indices)
  {
    float rt = headers.rt[idx];
    std::vector<std::vector<std::vector<float>>> single_spectrum = ana.get_spectra({idx});

    if (single_spectrum.empty() || single_spectrum[0].size() < 2)
      continue;

    std::vector<float> &spec_mz = single_spectrum[0][0];
    std::vector<float> &spec_intensity = single_spectrum[0][1];

    for (size_t i = 0; i < spec_mz.size(); ++i)
    {
      if (spec_mz[i] >= mz_min && spec_mz[i] <= mz_max &&
          spec_intensity[i] >= minTracesIntensity)
      {
        eic.rt.push_back(rt);
        eic.mz.push_back(spec_mz[i]);
        eic.intensity.push_back(spec_intensity[i]);
        eic.noise.push_back(minTracesIntensity); // Placeholder noise
      }
    }
  }

  eic.size = eic.rt.size();
  eic.valid = eic.size > 0;

  return eic;
}

// MARK: pick_peak_from_eic
nts::gap_filling::FILLED_FEATURE_INFO nts::gap_filling::pick_peak_from_eic(
    const EIC_DATA &eic_data,
    const std::string &analysis,
    const std::string &feature_group,
    const std::string &original_feature,
    float target_rt,
    float target_mz,
    float target_mass,
    int polarity,
    float rtApexDeviation,
    float minSNR,
    int minTraces,
    float maxWidth,
    float minGaussianFit,
    bool debug)
{
  FILLED_FEATURE_INFO filled_feature;
  filled_feature.analysis = analysis;
  filled_feature.feature_group = feature_group;
  filled_feature.original_feature = original_feature;
  filled_feature.filled = true;
  filled_feature.polarity = polarity;

  // Default values
  filled_feature.rt = target_rt;
  filled_feature.mz = target_mz;
  filled_feature.mass = target_mass;
  filled_feature.intensity = 0.0f;
  filled_feature.area = 0.0f;
  filled_feature.noise = 0.0f;
  filled_feature.sn = 0.0f;
  filled_feature.rtmin = target_rt;
  filled_feature.rtmax = target_rt;
  filled_feature.width = 0.0f;
  filled_feature.mzmin = target_mz;
  filled_feature.mzmax = target_mz;
  filled_feature.ppm = 0.0f;
  filled_feature.fwhm_rt = 0.0f;
  filled_feature.gaussian_A = 0.0f;
  filled_feature.gaussian_mu = target_rt;
  filled_feature.gaussian_sigma = 0.0f;
  filled_feature.gaussian_r2 = 0.0f;
  filled_feature.correction = 1.0f;
  filled_feature.eic_size = 0;

  if (!eic_data.valid || eic_data.size < minTraces)
    return filled_feature;

  // Sort by RT
  std::vector<float> sorted_rt = eic_data.rt;
  std::vector<float> sorted_mz = eic_data.mz;
  std::vector<float> sorted_intensity = eic_data.intensity;
  std::vector<float> sorted_noise = eic_data.noise;

  auto sort_indices = nts::utils::get_sort_indices_float(sorted_rt);
  nts::utils::reorder_multiple_vectors(sort_indices, sorted_rt, sorted_mz, sorted_intensity, sorted_noise);

  // Aggregate intensity by RT (sum intensities at same RT) and keep max m/z
  std::map<float, float> rt_intensity_map;
  std::map<float, float> rt_mz_map;
  for (size_t i = 0; i < sorted_rt.size(); ++i)
  {
    rt_intensity_map[sorted_rt[i]] += sorted_intensity[i];
    // Keep the maximum m/z value for each RT
    if (rt_mz_map.find(sorted_rt[i]) == rt_mz_map.end() || sorted_mz[i] > rt_mz_map[sorted_rt[i]])
    {
      rt_mz_map[sorted_rt[i]] = sorted_mz[i];
    }
  }

  std::vector<float> unique_rt, unique_intensity, unique_mz;
  for (const auto &[rt, intensity] : rt_intensity_map)
  {
    unique_rt.push_back(rt);
    unique_intensity.push_back(intensity);
    unique_mz.push_back(rt_mz_map[rt]);
  }

  if (debug)
  {
    DEBUG_LOG("\n  After RT aggregation: " << unique_rt.size() << " unique RT points" << std::endl);
    for (size_t i = 0; i < unique_rt.size(); ++i)
    {
      DEBUG_LOG("    RT=" << unique_rt[i] << ", Intensity=" << unique_intensity[i] << std::endl);
    }
  }

  if (unique_rt.size() < static_cast<size_t>(minTraces))
  {
    if (debug) DEBUG_LOG("  -> FAILED: Not enough unique RT points (" << unique_rt.size() << " < " << minTraces << ")" << std::endl);
    return filled_feature;
  }

  // Baseline is the minimum intensity (noise level)
  float baseline_level = *std::min_element(unique_intensity.begin(), unique_intensity.end());

  if (debug) DEBUG_LOG("  Baseline level: " << baseline_level << std::endl);

  // Only smooth if we have enough points (>= 5), using small window to preserve peak shape
  std::vector<float> smoothed;
  if (unique_rt.size() >= 5)
  {
    // Use window=3 and order=1 (linear) for minimal smoothing that preserves peak shape
    smoothed = nts::utils::smooth_intensity_savitzky_golay(unique_intensity, 3, 1);
    if (debug)
    {
      DEBUG_LOG("  Applied light smoothing (window=3, order=1) for " << unique_rt.size() << " points" << std::endl);
      DEBUG_LOG("  After smoothing:" << std::endl);
      for (size_t i = 0; i < smoothed.size(); ++i)
      {
        DEBUG_LOG("    RT=" << unique_rt[i] << ", Raw=" << unique_intensity[i] << ", Smoothed=" << smoothed[i] << std::endl);
      }
    }
  }
  else
  {
    smoothed = unique_intensity; // Use raw intensity for very small EICs
    if (debug) DEBUG_LOG("  Skipped smoothing (only " << unique_rt.size() << " points, need >= 5)" << std::endl);
  }

  // Find apex as maximum intensity
  auto apex_it = std::max_element(smoothed.begin(), smoothed.end());
  int apex_idx = std::distance(smoothed.begin(), apex_it);
  float apex_rt = unique_rt[apex_idx];
  float apex_intensity = smoothed[apex_idx];

  if (debug)
  {
    DEBUG_LOG("  Apex found at index " << apex_idx << ": RT=" << apex_rt << ", Intensity=" << apex_intensity << std::endl);
    DEBUG_LOG("  Target RT: " << target_rt << ", RT apex deviation: " << rtApexDeviation << std::endl);
    DEBUG_LOG("  Distance from target: " << std::abs(apex_rt - target_rt) << std::endl);
  }

  // Check if apex is within search window
  if (std::abs(apex_rt - target_rt) > rtApexDeviation)
  {
    if (debug) DEBUG_LOG("  -> FAILED: Apex outside RT deviation (" << std::abs(apex_rt - target_rt) << " > " << rtApexDeviation << ")" << std::endl);
    return filled_feature;
  }

  // Find FWHM boundaries
  float half_max = (apex_intensity + baseline_level) / 2.0f;
  int left_bound = apex_idx;
  int right_bound = apex_idx;

  if (debug) DEBUG_LOG("  Half-max threshold: " << half_max << std::endl);

  // Find left boundary
  for (int i = apex_idx - 1; i >= 0; --i)
  {
    if (smoothed[i] <= half_max)
    {
      left_bound = i;
      break;
    }
    left_bound = i;
  }

  // Find right boundary
  for (int i = apex_idx + 1; i < static_cast<int>(smoothed.size()); ++i)
  {
    if (smoothed[i] <= half_max)
    {
      right_bound = i;
      break;
    }
    right_bound = i;
  }

  // Ensure minimum width
  if (right_bound - left_bound + 1 < minTraces)
  {
    int expand = (minTraces - (right_bound - left_bound + 1)) / 2;
    left_bound = std::max(0, left_bound - expand);
    right_bound = std::min(static_cast<int>(unique_rt.size()) - 1, right_bound + expand);
  }

  // Ensure maximum width
  float peak_width = unique_rt[right_bound] - unique_rt[left_bound];
  if (peak_width > maxWidth)
  {
    // Shrink from edges
    while (peak_width > maxWidth && left_bound < apex_idx)
    {
      left_bound++;
      peak_width = unique_rt[right_bound] - unique_rt[left_bound];
    }
    while (peak_width > maxWidth && right_bound > apex_idx)
    {
      right_bound--;
      peak_width = unique_rt[right_bound] - unique_rt[left_bound];
    }
  }

  // Extract peak data
  std::vector<float> peak_rt(unique_rt.begin() + left_bound, unique_rt.begin() + right_bound + 1);
  std::vector<float> peak_intensity(unique_intensity.begin() + left_bound, unique_intensity.begin() + right_bound + 1);
  std::vector<float> peak_mz(unique_mz.begin() + left_bound, unique_mz.begin() + right_bound + 1);
  std::vector<float> peak_baseline(peak_rt.size(), baseline_level);

  if (debug)
  {
    DEBUG_LOG("  Peak boundaries: left=" << left_bound << ", right=" << right_bound << std::endl);
    DEBUG_LOG("  Peak data (" << peak_rt.size() << " points):" << std::endl);
    for (size_t i = 0; i < peak_rt.size(); ++i)
    {
      DEBUG_LOG("    RT=" << peak_rt[i] << ", Intensity=" << peak_intensity[i] << std::endl);
    }
  }

  // Calculate peak metrics
  float max_intensity = *std::max_element(peak_intensity.begin(), peak_intensity.end());
  // S/N = (signal - baseline) / baseline
  float signal_above_baseline = max_intensity - baseline_level;
  float peak_sn = baseline_level > 0 ? signal_above_baseline / baseline_level : 0.0f;

  if (debug)
  {
    DEBUG_LOG("  Peak max intensity: " << max_intensity << std::endl);
    DEBUG_LOG("  Baseline: " << baseline_level << std::endl);
    DEBUG_LOG("  Signal above baseline: " << signal_above_baseline << std::endl);
    DEBUG_LOG("  Peak S/N: " << peak_sn << " (min: " << minSNR << ")" << std::endl);
  }

  if (peak_sn < minSNR)
  {
    if (debug) DEBUG_LOG("  -> FAILED: S/N too low (" << peak_sn << " < " << minSNR << ")" << std::endl);
    return filled_feature;
  }

  // Calculate area (trapezoidal rule)
  float area = 0.0f;
  for (size_t i = 1; i < peak_rt.size(); ++i)
  {
    float dt = peak_rt[i] - peak_rt[i - 1];
    float avg_int = (peak_intensity[i] + peak_intensity[i - 1]) / 2.0f;
    area += avg_int * dt;
  }

  // Calculate FWHM (both RT and m/z) - do this before Gaussian fit as it's more reliable
  float fwhm = nts::deconvolution::calculate_fwhm_rt(peak_rt, peak_intensity);
  auto [fwhm_rt_calc, fwhm_mz_calc, mean_mz_fwhm] = nts::deconvolution::calculate_fwhm_combined(
      peak_rt, peak_mz, peak_intensity);

  // Calculate average m/z in peak window
  float avg_mz = std::accumulate(peak_mz.begin(), peak_mz.end(), 0.0f) / peak_mz.size();

  // Calculate quality metrics using raw vectors (before encoding)
  float peak_area_val = nts::utils::calculate_area(peak_rt, peak_intensity);
  float jaggedness_val = nts::utils::calculate_jaggedness(peak_intensity);
  float sharpness_val = nts::utils::calculate_sharpness(peak_rt, peak_intensity, peak_area_val);
  float asymmetry_val = nts::utils::calculate_asymmetry(peak_rt, peak_intensity);
  // For modality, use smoothed data if we smoothed earlier
  std::vector<float> smoothed_for_modality = (peak_rt.size() >= 5)
      ? nts::utils::smooth_intensity_savitzky_golay(peak_intensity, 3, 1)
      : peak_intensity;
  float modality_val = nts::utils::calculate_modality(smoothed_for_modality, 0.1f);
  float plates_val = nts::utils::calculate_theoretical_plates(apex_rt, fwhm);

  // Fit Gaussian - provide initial values for optimization
  float gaussian_baseline = std::min(peak_intensity.front(), peak_intensity.back());
  float gaussian_A = max_intensity - gaussian_baseline;
  float gaussian_mu = apex_rt;
  float gaussian_sigma = fwhm / 2.355f; // FWHM = 2.355 * sigma for Gaussian
  if (gaussian_sigma <= 0) gaussian_sigma = (peak_rt.back() - peak_rt.front()) / 4.0f;

  nts::utils::fit_gaussian(peak_rt, peak_intensity, gaussian_A, gaussian_mu, gaussian_sigma, gaussian_baseline);

  // Calculate R² for the fit
  float gaussian_r2 = nts::utils::calculate_gaussian_rsquared(
      peak_rt, peak_intensity, gaussian_A, gaussian_mu, gaussian_sigma, gaussian_baseline);

  if (debug)
  {
    DEBUG_LOG("\n  Quality Metrics:" << std::endl);
    DEBUG_LOG("    Intensity: " << max_intensity << std::endl);
    DEBUG_LOG("    Area: " << area << std::endl);
    DEBUG_LOG("    S/N: " << peak_sn << std::endl);
    DEBUG_LOG("    FWHM RT: " << fwhm << std::endl);
    DEBUG_LOG("    FWHM m/z: " << fwhm_mz_calc << std::endl);
    DEBUG_LOG("    Jaggedness: " << jaggedness_val << std::endl);
    DEBUG_LOG("    Sharpness: " << sharpness_val << std::endl);
    DEBUG_LOG("    Asymmetry: " << asymmetry_val << std::endl);
    DEBUG_LOG("    Modality: " << modality_val << std::endl);
    DEBUG_LOG("    Plates: " << plates_val << std::endl);
    DEBUG_LOG("\n  Gaussian Fit:" << std::endl);
    DEBUG_LOG("    A: " << gaussian_A << std::endl);
    DEBUG_LOG("    mu: " << gaussian_mu << std::endl);
    DEBUG_LOG("    sigma: " << gaussian_sigma << std::endl);
    DEBUG_LOG("    baseline: " << gaussian_baseline << std::endl);
    DEBUG_LOG("    R²: " << gaussian_r2 << std::endl);
  }

  // Calculate mass based on polarity and adduct
  float proton_mass = 1.007276f;
  float mass;
  std::string adduct;
  if (polarity > 0) {
    // Positive mode: [M+H]+
    mass = avg_mz - proton_mass;
    adduct = "[M+H]+";
  } else if (polarity < 0) {
    // Negative mode: [M-H]-
    mass = avg_mz + proton_mass;
    adduct = "[M-H]-";
  } else {
    // Unknown polarity
    mass = avg_mz;
    adduct = "";
  }

  // Fill in feature info - use apex_rt (actual maximum) as RT
  filled_feature.rt = apex_rt;
  filled_feature.mz = avg_mz;
  filled_feature.mass = mass;
  filled_feature.adduct = adduct;
  filled_feature.intensity = max_intensity;
  filled_feature.area = area;
  filled_feature.noise = baseline_level;
  filled_feature.sn = peak_sn;
  filled_feature.rtmin = peak_rt.front();
  filled_feature.rtmax = peak_rt.back();
  filled_feature.width = peak_rt.back() - peak_rt.front();
  filled_feature.mzmin = *std::min_element(peak_mz.begin(), peak_mz.end());
  filled_feature.mzmax = *std::max_element(peak_mz.begin(), peak_mz.end());
  filled_feature.ppm = std::abs(avg_mz - target_mz) / target_mz * 1e6f;
  filled_feature.fwhm_rt = fwhm;
  filled_feature.fwhm_mz = fwhm_mz_calc;
  filled_feature.gaussian_A = gaussian_A;
  filled_feature.gaussian_mu = gaussian_mu;
  filled_feature.gaussian_sigma = gaussian_sigma;
  filled_feature.gaussian_r2 = gaussian_r2;
  filled_feature.jaggedness = jaggedness_val;
  filled_feature.sharpness = sharpness_val;
  filled_feature.asymmetry = asymmetry_val;
  filled_feature.modality = modality_val;
  filled_feature.plates = plates_val;
  filled_feature.eic_size = peak_rt.size();

  // Encode EIC data
  filled_feature.eic_rt = nts::utils::encode_floats_base64(peak_rt, 4);
  filled_feature.eic_mz = nts::utils::encode_floats_base64(peak_mz, 4);
  filled_feature.eic_intensity = nts::utils::encode_floats_base64(peak_intensity, 4);
  filled_feature.eic_baseline = nts::utils::encode_floats_base64(peak_baseline, 4);
  filled_feature.eic_smoothed = nts::utils::encode_floats_base64(peak_intensity, 4);

  return filled_feature;
}

// MARK: fill_features_impl
void nts::gap_filling::fill_features_impl(
    nts::NTS_DATA &nts_data,
    bool withinReplicate,
    bool filtered,
    float rtExpand,
    float mzExpand,
    float maxPeakWidth,
    float minTracesIntensity,
    int minNumberTraces,
    float minIntensity,
    float rtApexDeviation,
    float minSignalToNoiseRatio,
    float minGaussianFit,
    std::string debugFG)
{
  // Initialize debug log if debugFG is specified
  bool debug = !debugFG.empty();
  if (debug)
  {
    std::string log_file = "log/gap_filling_debug_" + debugFG + ".log";
    nts::utils::init_debug_log(log_file, "=== Gap Filling Debug Log for Feature Group: " + debugFG + " ===");
    DEBUG_LOG("\n=== Parameters ===" << std::endl);
    DEBUG_LOG("  rtExpand: " << rtExpand << std::endl);
    DEBUG_LOG("  mzExpand: " << mzExpand << std::endl);
    DEBUG_LOG("  maxPeakWidth: " << maxPeakWidth << std::endl);
    DEBUG_LOG("  minTracesIntensity: " << minTracesIntensity << std::endl);
    DEBUG_LOG("  minNumberTraces: " << minNumberTraces << std::endl);
    DEBUG_LOG("  minIntensity: " << minIntensity << std::endl);
    DEBUG_LOG("  rtApexDeviation: " << rtApexDeviation << std::endl);
    DEBUG_LOG("  minSignalToNoiseRatio: " << minSignalToNoiseRatio << std::endl);
    DEBUG_LOG("  minGaussianFit: " << minGaussianFit << std::endl);
    DEBUG_LOG("  withinReplicate: " << (withinReplicate ? "true" : "false") << std::endl);
  }

  std::cout << "Starting feature gap filling..." << std::endl;

  // Analyze feature groups to identify gaps
  auto group_infos = analyze_feature_groups(
      nts_data.features,
      nts_data.analyses,
      nts_data.replicates,
      withinReplicate,
      1); // minSamples = 1

  if (group_infos.empty())
  {
    std::cout << "No feature groups with gaps found." << std::endl;
    return;
  }

  std::cout << "Found " << group_infos.size() << " feature groups with gaps." << std::endl;

  // Track filled features
  int total_gaps = 0;
  int filled_gaps = 0;

  // Track filled features count per analysis for feature naming
  std::unordered_map<std::string, int> filled_features_count;

  // Build analysis file map
  std::unordered_map<std::string, std::string> analysis_file_map;
  for (size_t i = 0; i < nts_data.analyses.size(); ++i)
  {
    analysis_file_map[nts_data.analyses[i]] = nts_data.files[i];
  }

  // Build analysis index map
  std::unordered_map<std::string, size_t> analysis_index_map;
  for (size_t i = 0; i < nts_data.analyses.size(); ++i)
  {
    analysis_index_map[nts_data.analyses[i]] = i;
  }

  // Group feature gaps by file for batch extraction
  struct GAP_INFO {
    std::string analysis;
    size_t analysis_idx;
    std::string feature_group;
    std::string target_id;
    float median_rt;
    float median_mz;
    float median_mass;
    int polarity;
  };

  // Store group_info ranges for each feature group
  struct GROUP_RANGES {
    float min_rtmin;
    float max_rtmax;
    float min_mzmin;
    float max_mzmax;
    int polarity;
  };
  std::unordered_map<std::string, GROUP_RANGES> group_ranges_map;

  std::unordered_map<std::string, std::vector<GAP_INFO>> gaps_by_file;

  for (const auto &group_info : group_infos)
  {
    // Determine polarity from existing features
    int polarity = 0;
    for (size_t i = 0; i < nts_data.features.size(); ++i)
    {
      const auto &fts = nts_data.features[i];
      for (int j = 0; j < fts.size(); ++j)
      {
        if (fts.feature_group[j] == group_info.feature_group)
        {
          polarity = fts.polarity[j];
          break;
        }
      }
      if (polarity != 0)
        break;
    }

    // Store ranges for this feature group
    group_ranges_map[group_info.feature_group] = {
      group_info.min_rtmin,
      group_info.max_rtmax,
      group_info.min_mzmin,
      group_info.max_mzmax,
      polarity
    };

    // Group by file
    for (const auto &missing_analysis : group_info.missing_analyses)
    {
      auto file_it = analysis_file_map.find(missing_analysis);
      if (file_it == analysis_file_map.end())
        continue;

      auto analysis_idx_it = analysis_index_map.find(missing_analysis);
      if (analysis_idx_it == analysis_index_map.end())
        continue;

      size_t analysis_idx = analysis_idx_it->second;
      if (analysis_idx >= nts_data.headers.size())
        continue;

      std::string target_id = "GAP_" + group_info.feature_group + "_" + missing_analysis;

      gaps_by_file[file_it->second].push_back({
        missing_analysis,
        analysis_idx,
        group_info.feature_group,
        target_id,
        group_info.median_rt,
        group_info.median_mz,
        group_info.median_mass,
        polarity
      });
      total_gaps++;
    }
  }

  std::cout << "Processing " << gaps_by_file.size() << " files with gaps..." << std::endl;

  // Process each file
  for (const auto &[file_path, gaps] : gaps_by_file)
  {
    std::cout << "  Processing " << gaps.size() << " gaps in file: " << file_path << std::endl;

    // Open MS file once per file
    mass_spec::MS_FILE ana(file_path);

    // Get headers for first gap (all gaps in same file share same headers)
    const auto &headers = nts_data.headers[gaps[0].analysis_idx];


    // Build MS_TARGETS for all gaps in this file, but skip those already present as filtered features
    mass_spec::MS_TARGETS targets;
    std::vector<size_t> valid_gap_indices;
    targets.resize_all(gaps.size()); // Will shrink later if needed

    for (size_t i = 0; i < gaps.size(); ++i)
    {
      const auto &gap = gaps[i];
      const auto &ranges = group_ranges_map[gap.feature_group];
      nts::FEATURES &analysis_features = nts_data.features[gap.analysis_idx];
      bool found_filtered_feature = false;

      for (int j = 0; j < analysis_features.size(); ++j)
      {
        if (analysis_features.filtered[j] &&
            analysis_features.rt[j] >= ranges.min_rtmin &&
            analysis_features.rt[j] <= ranges.max_rtmax &&
            analysis_features.mz[j] >= ranges.min_mzmin &&
            analysis_features.mz[j] <= ranges.max_mzmax &&
            analysis_features.polarity[j] == gap.polarity)
        {
          // Unfilter this feature
          analysis_features.filtered[j] = false;
          analysis_features.filter[j] = "";
          analysis_features.feature_group[j] = gap.feature_group;
          filled_gaps++;
          found_filtered_feature = true;
          // Add debug log here if this is the debug feature group
          bool is_debug_fg = debug && (gap.feature_group == debugFG);
          if (is_debug_fg) {
            DEBUG_LOG("  -> RECOVERED FILTERED FEATURE: " << analysis_features.feature[j] << std::endl);
            DEBUG_LOG("    RT: " << analysis_features.rt[j] << ", m/z: " << analysis_features.mz[j] << std::endl);
            DEBUG_LOG("    Intensity: " << analysis_features.intensity[j] << std::endl);
            DEBUG_LOG("    Changed filtered=TRUE to FALSE, assigned to feature group" << std::endl);
            DEBUG_LOG("  -> SKIPPED GAP TARGET: " << gap.target_id << " (filtered feature recovered before EIC extraction)" << std::endl);
          }
          break;
        }
      }

      if (!found_filtered_feature)
      {
        // Only add to targets if not already recovered
        valid_gap_indices.push_back(i);
        targets.index[i] = i;
        targets.id[i] = gap.target_id;
        targets.level[i] = 1;
        targets.polarity[i] = gap.polarity;
        targets.precursor[i] = false;
        targets.mz[i] = gap.median_mz;
        targets.mzmin[i] = ranges.min_mzmin - mzExpand;
        targets.mzmax[i] = ranges.max_mzmax + mzExpand;
        targets.rt[i] = gap.median_rt;
        targets.rtmin[i] = ranges.min_rtmin - rtExpand;
        targets.rtmax[i] = ranges.max_rtmax + rtExpand;
        targets.mobility[i] = 0.0f;
        targets.mobilitymin[i] = 0.0f;
        targets.mobilitymax[i] = 0.0f;
      }
    }

    // Shrink targets to only valid gaps
    if (valid_gap_indices.size() < gaps.size()) {
      mass_spec::MS_TARGETS shrunk_targets;
      shrunk_targets.resize_all(valid_gap_indices.size());
      for (size_t k = 0; k < valid_gap_indices.size(); ++k) {
        size_t i = valid_gap_indices[k];
        shrunk_targets.index[k] = k;
        shrunk_targets.id[k] = targets.id[i];
        shrunk_targets.level[k] = targets.level[i];
        shrunk_targets.polarity[k] = targets.polarity[i];
        shrunk_targets.precursor[k] = targets.precursor[i];
        shrunk_targets.mz[k] = targets.mz[i];
        shrunk_targets.mzmin[k] = targets.mzmin[i];
        shrunk_targets.mzmax[k] = targets.mzmax[i];
        shrunk_targets.rt[k] = targets.rt[i];
        shrunk_targets.rtmin[k] = targets.rtmin[i];
        shrunk_targets.rtmax[k] = targets.rtmax[i];
        shrunk_targets.mobility[k] = targets.mobility[i];
        shrunk_targets.mobilitymin[k] = targets.mobilitymin[i];
        shrunk_targets.mobilitymax[k] = targets.mobilitymax[i];
      }
      targets = shrunk_targets;
    }

    // Extract all EICs in one batch call (uses OpenMP internally)
    mass_spec::MS_TARGETS_SPECTRA all_eics = ana.get_spectra_targets(targets, headers, minTracesIntensity, 0);

    // Process each gap using extracted EICs
    for (const auto &gap : gaps)
    {
      // Check if this is the feature group we're debugging
      bool is_debug_fg = debug && (gap.feature_group == debugFG);

      if (is_debug_fg)
      {
        const auto &ranges = group_ranges_map[gap.feature_group];
        DEBUG_LOG("\n=== Processing Gap for Debug Feature Group ===" << std::endl);
        DEBUG_LOG("  Feature Group: " << gap.feature_group << std::endl);
        DEBUG_LOG("  Analysis: " << gap.analysis << std::endl);
        DEBUG_LOG("  Target ID: " << gap.target_id << std::endl);
        DEBUG_LOG("  Median RT: " << gap.median_rt << std::endl);
        DEBUG_LOG("  Median m/z: " << gap.median_mz << std::endl);
        DEBUG_LOG("  Median mass: " << gap.median_mass << std::endl);
        DEBUG_LOG("  Polarity: " << gap.polarity << std::endl);
        DEBUG_LOG("\n  Feature Group Observed Ranges:" << std::endl);
        DEBUG_LOG("    RT range (features): " << ranges.min_rtmin << " - " << ranges.max_rtmax << " s" << std::endl);
        DEBUG_LOG("    m/z range (features): " << ranges.min_mzmin << " - " << ranges.max_mzmax << std::endl);
        DEBUG_LOG("\n  EIC Search Windows (expanded by rtExpand=" << rtExpand << ", mzExpand=" << mzExpand << "):" << std::endl);
        DEBUG_LOG("    RT range: " << (ranges.min_rtmin - rtExpand) << " - " << (ranges.max_rtmax + rtExpand) << " s" << std::endl);
        DEBUG_LOG("    m/z range: " << (ranges.min_mzmin - mzExpand) << " - " << (ranges.max_mzmax + mzExpand) << std::endl);
      }

      // ...existing code...

      // Get EIC for this specific target
      mass_spec::MS_TARGETS_SPECTRA eic_spec = all_eics[gap.target_id];

      if (is_debug_fg)
      {
        DEBUG_LOG("  EIC traces found: " << eic_spec.rt.size() << std::endl);
        DEBUG_LOG("  Min traces required: " << minNumberTraces << std::endl);
      }

      if (eic_spec.rt.size() < static_cast<size_t>(minNumberTraces))
      {
        if (is_debug_fg)
        {
          DEBUG_LOG("  -> SKIPPED: Not enough traces (" << eic_spec.rt.size() << " < " << minNumberTraces << ")" << std::endl);
        }
        continue;
      }

      // Build EIC_DATA from MS_TARGETS_SPECTRA
      EIC_DATA eic_data;
      eic_data.rt = eic_spec.rt;
      eic_data.mz = eic_spec.mz;
      eic_data.intensity = eic_spec.intensity;
      eic_data.noise = std::vector<float>(eic_spec.rt.size(), minTracesIntensity);
      eic_data.size = eic_spec.rt.size();
      eic_data.valid = eic_data.size > 0;

      if (!eic_data.valid)
      {
        if (is_debug_fg)
        {
          DEBUG_LOG("  -> SKIPPED: EIC data invalid" << std::endl);
        }
        continue;
      }

      if (is_debug_fg)
      {
        DEBUG_LOG("  EIC data valid with " << eic_data.size << " points" << std::endl);
        DEBUG_LOG("\n  EIC Data Points:" << std::endl);
        for (size_t i = 0; i < eic_data.rt.size(); ++i)
        {
          DEBUG_LOG("    Point " << (i + 1) << ": RT=" << eic_data.rt[i]
                    << ", m/z=" << eic_data.mz[i]
                    << ", Intensity=" << eic_data.intensity[i] << std::endl);
        }
      }

      // Pick peak from EIC
      auto filled_feature = pick_peak_from_eic(
          eic_data,
          gap.analysis,
          gap.feature_group,
          "", // No original feature for filled
          gap.median_rt,
          gap.median_mz,
          gap.median_mass,
          gap.polarity,
          rtApexDeviation,
          minSignalToNoiseRatio,
          minNumberTraces,
          maxPeakWidth,
          minGaussianFit,
          is_debug_fg);

      if (is_debug_fg)
      {
        DEBUG_LOG("  Peak picking results:" << std::endl);
        DEBUG_LOG("    Intensity: " << filled_feature.intensity << " (min: " << minIntensity << ")" << std::endl);
        DEBUG_LOG("    S/N: " << filled_feature.sn << " (min: " << minSignalToNoiseRatio << ")" << std::endl);
        DEBUG_LOG("    Gaussian R²: " << filled_feature.gaussian_r2 << " (min: " << minGaussianFit << ")" << std::endl);
        DEBUG_LOG("    RT: " << filled_feature.rt << " (target: " << gap.median_rt << ")" << std::endl);
        DEBUG_LOG("    m/z: " << filled_feature.mz << " (target: " << gap.median_mz << ")" << std::endl);
        DEBUG_LOG("    Area: " << filled_feature.area << std::endl);
        DEBUG_LOG("    Width: " << filled_feature.width << std::endl);
      }

      // Check if filling was successful
      if (filled_feature.intensity < minIntensity)
      {
        if (is_debug_fg)
        {
          DEBUG_LOG("  -> SKIPPED: Intensity too low (" << filled_feature.intensity << " < " << minIntensity << ")" << std::endl);
        }
        continue;
      }

      // Create FEATURE from FILLED_FEATURE_INFO
      nts::FEATURE new_feature;
      new_feature.analysis = filled_feature.analysis;

      // Increment filled feature count for this analysis
      filled_features_count[gap.analysis]++;
      int feature_number = filled_features_count[gap.analysis];

      std::string polarity_suffix = (filled_feature.polarity > 0) ? "POS" : "NEG";
      new_feature.feature = "FL" + std::to_string(feature_number) + "_MZ" +
                           std::to_string(static_cast<int>(std::round(filled_feature.mz))) +
                           "_RT" + std::to_string(static_cast<int>(std::round(filled_feature.rt))) +
                           "_" + polarity_suffix;

      new_feature.feature_group = filled_feature.feature_group;
      new_feature.feature_component = "";
      new_feature.adduct = filled_feature.adduct;
      new_feature.rt = filled_feature.rt;
      new_feature.mz = filled_feature.mz;
      new_feature.mass = filled_feature.mass;
      new_feature.intensity = filled_feature.intensity;
      new_feature.noise = filled_feature.noise;
      new_feature.sn = filled_feature.sn;
      new_feature.area = filled_feature.area;
      new_feature.rtmin = filled_feature.rtmin;
      new_feature.rtmax = filled_feature.rtmax;
      new_feature.width = filled_feature.width;
      new_feature.mzmin = filled_feature.mzmin;
      new_feature.mzmax = filled_feature.mzmax;
      new_feature.ppm = filled_feature.ppm;
      new_feature.fwhm_rt = filled_feature.fwhm_rt;
      new_feature.fwhm_mz = filled_feature.fwhm_mz;
      new_feature.gaussian_A = filled_feature.gaussian_A;
      new_feature.gaussian_mu = filled_feature.gaussian_mu;
      new_feature.gaussian_sigma = filled_feature.gaussian_sigma;
      new_feature.gaussian_r2 = filled_feature.gaussian_r2;
      new_feature.jaggedness = filled_feature.jaggedness;
      new_feature.sharpness = filled_feature.sharpness;
      new_feature.asymmetry = filled_feature.asymmetry;
      new_feature.modality = filled_feature.modality;
      new_feature.plates = filled_feature.plates;

      new_feature.polarity = filled_feature.polarity;
      new_feature.filtered = false;
      new_feature.filter = "";
      new_feature.filled = true;
      new_feature.correction = filled_feature.correction;
      new_feature.eic_size = filled_feature.eic_size;
      new_feature.eic_rt = filled_feature.eic_rt;
      new_feature.eic_mz = filled_feature.eic_mz;
      new_feature.eic_intensity = filled_feature.eic_intensity;
      new_feature.eic_baseline = filled_feature.eic_baseline;
      new_feature.eic_smoothed = filled_feature.eic_smoothed;
      new_feature.ms1_size = 0;
      new_feature.ms1_mz = "";
      new_feature.ms1_intensity = "";
      new_feature.ms2_size = 0;
      new_feature.ms2_mz = "";
      new_feature.ms2_intensity = "";

      // Add to features
      nts_data.features[gap.analysis_idx].append_feature(new_feature);
      filled_gaps++;

      if (is_debug_fg)
      {
        DEBUG_LOG("  -> SUCCESS: Feature filled and added!" << std::endl);
        DEBUG_LOG("    Feature ID: " << new_feature.feature << std::endl);
        DEBUG_LOG("    Final intensity: " << new_feature.intensity << std::endl);
        DEBUG_LOG("    Final area: " << new_feature.area << std::endl);
      }
    }
  }

  std::cout << "Gap filling complete. Filled " << filled_gaps
              << " out of " << total_gaps << " gaps." << std::endl;

  // Close debug log if it was opened
  if (debug)
  {
    DEBUG_LOG("\n=== Gap Filling Complete ===" << std::endl);
    DEBUG_LOG("Total gaps processed: " << total_gaps << std::endl);
    DEBUG_LOG("Gaps filled: " << filled_gaps << std::endl);
    nts::utils::close_debug_log();
  }
}
