// nts_alignment.cpp
// Implementation of feature alignment and grouping functions

#include "nts_alignment.h"
#include "nts.h"
#include "nts_utils.h"
#include <algorithm>
#include <cmath>
#include <sstream>
#include <iomanip>
#include <set>
#include <map>
#include <unordered_map>
#include <fstream>
#include <numeric>
#include <limits>

namespace nts {
namespace alignment {

// Helper function to interpolate RT shift at a given RT
float interpolate_rt_shift(
  float feature_rt,
  const std::vector<float> &anchor_rts,
  const std::vector<float> &anchor_shifts
) {
  if (anchor_rts.empty()) return 0.0f;

  // Find surrounding anchor points
  int lower_idx = -1;
  int upper_idx = -1;

  for (size_t i = 0; i < anchor_rts.size(); ++i) {
    if (anchor_rts[i] <= feature_rt) {
      lower_idx = i;
    }
    if (anchor_rts[i] >= feature_rt && upper_idx == -1) {
      upper_idx = i;
    }
  }

  if (lower_idx == -1 && upper_idx == -1) {
    return 0.0f;
  } else if (lower_idx == -1) {
    return anchor_shifts[upper_idx];
  } else if (upper_idx == -1) {
    return anchor_shifts[lower_idx];
  } else if (lower_idx == upper_idx) {
    return anchor_shifts[lower_idx];
  } else {
    // Linear interpolation
    float lower_rt = anchor_rts[lower_idx];
    float lower_shift = anchor_shifts[lower_idx];
    float upper_rt = anchor_rts[upper_idx];
    float upper_shift = anchor_shifts[upper_idx];

    float weight = (feature_rt - lower_rt) / (upper_rt - lower_rt);
    return lower_shift + weight * (upper_shift - lower_shift);
  }
}

// Calculate RT shifts using internal standards with linear interpolation
void calculate_rt_shifts_istd(
  std::vector<AlignmentFeature> &features,
  const std::vector<InternalStandard> &internal_standards,
  float bin_size
) {
  // Group internal standards by analysis
  std::map<std::string, std::vector<InternalStandard>> istd_by_analysis;
  for (const auto &istd : internal_standards) {
    istd_by_analysis[istd.analysis].push_back(istd);
  }

  // Sort internal standards by RT for each analysis
  for (auto &pair : istd_by_analysis) {
    std::sort(pair.second.begin(), pair.second.end(),
              [](const InternalStandard &a, const InternalStandard &b) {
                return a.exp_rt < b.exp_rt;
              });
  }

  // Find global RT range across all features
  float min_rt = std::numeric_limits<float>::max();
  float max_rt = std::numeric_limits<float>::min();
  for (const auto &feature : features) {
    if (feature.rt < min_rt) min_rt = feature.rt;
    if (feature.rt > max_rt) max_rt = feature.rt;
  }

  // Create RT bins
  int num_bins = static_cast<int>(std::ceil((max_rt - min_rt) / bin_size));
  if (num_bins < 1) num_bins = 1;

  // Calculate shift for each bin per analysis
  std::map<std::string, std::vector<float>> bin_shifts_by_analysis;

  for (auto &pair : istd_by_analysis) {
    const std::string &analysis_name = pair.first;
    const auto &istd_analysis = pair.second;

    std::vector<float> anchor_rts;
    std::vector<float> anchor_shifts;

    for (const auto &istd : istd_analysis) {
      anchor_rts.push_back(istd.exp_rt);
      anchor_shifts.push_back(istd.rt_shift);
    }

    // Calculate shift for each RT bin center
    std::vector<float> bin_shifts(num_bins, 0.0f);
    for (int i = 0; i < num_bins; ++i) {
      float bin_center = min_rt + (i + 0.5f) * bin_size;
      bin_shifts[i] = interpolate_rt_shift(bin_center, anchor_rts, anchor_shifts);
    }

    bin_shifts_by_analysis[analysis_name] = bin_shifts;
  }

  // Apply RT-dependent correction for each feature using binned shifts
  for (auto &feature : features) {
    auto it = bin_shifts_by_analysis.find(feature.analysis);
    if (it == bin_shifts_by_analysis.end()) {
      // No internal standards for this analysis
      feature.rt_corrected = feature.rt;
      continue;
    }

    // Find the bin for this feature's RT
    int bin_idx = static_cast<int>((feature.rt - min_rt) / bin_size);
    if (bin_idx < 0) bin_idx = 0;
    if (bin_idx >= num_bins) bin_idx = num_bins - 1;

    float shift = it->second[bin_idx];
    feature.rt_corrected = feature.rt - shift;
  }
}

// Calculate RT shifts using OBI-Warp alignment
void calculate_rt_shifts_obiwarp(
  std::vector<AlignmentFeature> &features,
  float ppm_threshold,
  float rt_deviation,
  float bin_size
) {
  // Find reference analysis (most features)
  std::map<std::string, int> analysis_counts;
  for (const auto &feature : features) {
    analysis_counts[feature.analysis]++;
  }

  std::string ref_analysis;
  int max_count = 0;
  for (const auto &pair : analysis_counts) {
    if (pair.second > max_count) {
      max_count = pair.second;
      ref_analysis = pair.first;
    }
  }

  if (max_count < 10) {
    // Insufficient features for alignment
    for (auto &feature : features) {
      feature.rt_corrected = feature.rt;
    }
    return;
  }

  // Get reference features
  std::vector<AlignmentFeature> ref_features;
  for (const auto &feature : features) {
    if (feature.analysis == ref_analysis) {
      ref_features.push_back(feature);
    }
  }

  // Group features by analysis
  std::map<std::string, std::vector<AlignmentFeature*>> features_by_analysis;
  for (auto &feature : features) {
    features_by_analysis[feature.analysis].push_back(&feature);
  }

  // Process each analysis
  for (auto &pair : features_by_analysis) {
    const std::string &analysis_name = pair.first;
    auto &target_features = pair.second;

    if (analysis_name == ref_analysis) {
      // Reference sample has no shift
      for (auto *feature : target_features) {
        feature->rt_corrected = feature->rt;
      }
      continue;
    }

    if (target_features.size() < 10) {
      // Insufficient features for alignment
      for (auto *feature : target_features) {
        feature->rt_corrected = feature->rt;
      }
      continue;
    }

    // Find matching features between reference and target based on mass
    std::vector<AnchorPoint> anchor_points;

    for (const auto &ref_feature : ref_features) {
      float mass_tolerance = ref_feature.mass * ppm_threshold / 1e6f;
      float mass_min = ref_feature.mass - mass_tolerance;
      float mass_max = ref_feature.mass + mass_tolerance;

      // Find target features with matching mass
      float min_rt_diff = rt_deviation * 3.0f;
      AlignmentFeature *best_match = nullptr;

      for (auto *target_feature : target_features) {
        if (target_feature->mass >= mass_min && target_feature->mass <= mass_max) {
          float rt_diff = std::abs(target_feature->rt - ref_feature.rt);
          if (rt_diff < min_rt_diff) {
            min_rt_diff = rt_diff;
            best_match = target_feature;
          }
        }
      }

      if (best_match != nullptr) {
        AnchorPoint anchor;
        anchor.ref_rt = ref_feature.rt;
        anchor.target_rt = best_match->rt;
        anchor.rt_shift = best_match->rt - ref_feature.rt;
        anchor.weight = std::sqrt(ref_feature.intensity * best_match->intensity);
        anchor_points.push_back(anchor);
      }
    }

    if (anchor_points.size() < 5) {
      // Insufficient anchor points
      for (auto *feature : target_features) {
        feature->rt_corrected = feature->rt;
      }
      continue;
    }

    // Sort anchor points by target RT
    std::sort(anchor_points.begin(), anchor_points.end(),
              [](const AnchorPoint &a, const AnchorPoint &b) {
                return a.target_rt < b.target_rt;
              });

    // Extract anchor RTs and shifts
    std::vector<float> anchor_rts;
    std::vector<float> anchor_shifts;
    for (const auto &anchor : anchor_points) {
      anchor_rts.push_back(anchor.target_rt);
      anchor_shifts.push_back(anchor.rt_shift);
    }

    // Find RT range for this analysis
    float min_rt = std::numeric_limits<float>::max();
    float max_rt = std::numeric_limits<float>::min();
    for (const auto *feature : target_features) {
      if (feature->rt < min_rt) min_rt = feature->rt;
      if (feature->rt > max_rt) max_rt = feature->rt;
    }

    // Create RT bins and calculate shift for each bin
    int num_bins = static_cast<int>(std::ceil((max_rt - min_rt) / bin_size));
    if (num_bins < 1) num_bins = 1;

    std::vector<float> bin_shifts(num_bins, 0.0f);
    for (int i = 0; i < num_bins; ++i) {
      float bin_center = min_rt + (i + 0.5f) * bin_size;
      bin_shifts[i] = interpolate_rt_shift(bin_center, anchor_rts, anchor_shifts);
    }

    // Apply RT-dependent correction for each feature using binned shifts
    for (auto *feature : target_features) {
      int bin_idx = static_cast<int>((feature->rt - min_rt) / bin_size);
      if (bin_idx < 0) bin_idx = 0;
      if (bin_idx >= num_bins) bin_idx = num_bins - 1;

      float shift = bin_shifts[bin_idx];
      feature->rt_corrected = feature->rt - shift;
    }
  }
}

// Group features based on mass and corrected RT using tolerance-based clustering
void group_features(
  std::vector<AlignmentFeature> &features,
  float ppm_threshold,
  float rt_deviation,
  int min_samples
) {
  // Sort features by polarity, mass, and RT for efficient grouping
  std::sort(features.begin(), features.end(),
            [](const AlignmentFeature &a, const AlignmentFeature &b) {
              if (a.polarity != b.polarity) return a.polarity < b.polarity;
              if (a.mass != b.mass) return a.mass < b.mass;
              float rt_a = std::isnan(a.rt_corrected) ? a.rt : a.rt_corrected;
              float rt_b = std::isnan(b.rt_corrected) ? b.rt : b.rt_corrected;
              return rt_a < rt_b;
            });

  // Group features iteratively
  int group_counter = 0;
  for (auto &feature : features) {
    feature.group_id = 0;
  }

  for (size_t i = 0; i < features.size(); ++i) {
    if (features[i].group_id > 0) continue;

    // Start a new group
    group_counter++;
    features[i].group_id = group_counter;

    // Get reference mass and RT
    float ref_mass = features[i].mass;
    float ref_rt = std::isnan(features[i].rt_corrected) ? features[i].rt : features[i].rt_corrected;

    float mass_tolerance = ref_mass * ppm_threshold / 1e6f;
    float mass_min = ref_mass - mass_tolerance;
    float mass_max = ref_mass + mass_tolerance;
    float rt_min = ref_rt - rt_deviation;
    float rt_max = ref_rt + rt_deviation;

    // Assign features within tolerance to this group (same polarity)
    for (size_t j = i + 1; j < features.size(); ++j) {
      if (features[j].group_id > 0) continue;
      if (features[j].polarity != features[i].polarity) break; // Different polarity, no more matches
      if (features[j].mass > mass_max) break; // No more matches possible

      float feature_rt = std::isnan(features[j].rt_corrected) ? features[j].rt : features[j].rt_corrected;
      if (features[j].mass >= mass_min &&
          feature_rt >= rt_min &&
          feature_rt <= rt_max) {
        features[j].group_id = group_counter;
      }
    }
  }

  // Calculate representative mass and RT for each group (median values)
  std::map<int, std::vector<float>> group_masses;
  std::map<int, std::vector<float>> group_rts;
  std::map<int, int> group_polarities;

  for (const auto &feature : features) {
    if (feature.group_id > 0) {
      group_masses[feature.group_id].push_back(feature.mass);
      // Use uncorrected RT if rt_corrected is NaN
      float rt_value = std::isnan(feature.rt_corrected) ? feature.rt : feature.rt_corrected;
      group_rts[feature.group_id].push_back(rt_value);
      group_polarities[feature.group_id] = feature.polarity;
    }
  }

  std::map<int, std::string> group_names;
  for (const auto &pair : group_masses) {
    int group_id = pair.first;
    auto masses = pair.second;
    auto rts = group_rts[group_id];
    int polarity = group_polarities[group_id];

    std::sort(masses.begin(), masses.end());
    std::sort(rts.begin(), rts.end());

    float median_mass = masses[masses.size() / 2];
    float median_rt = rts[rts.size() / 2];

    std::string polarity_suffix;
    if (polarity > 0) {
      polarity_suffix = "_POS";
    } else if (polarity < 0) {
      polarity_suffix = "_NEG";
    } else {
      polarity_suffix = "";
    }

    std::string group_name = "FG" + std::to_string(group_id) +
                            "_M" + std::to_string(static_cast<int>(std::round(median_mass))) +
                            "_RT" + std::to_string(static_cast<int>(std::round(median_rt))) +
                            polarity_suffix;
    group_names[group_id] = group_name;
  }

  // Filter groups by minimum samples
  if (min_samples > 1) {
    std::map<int, std::set<std::string>> group_analyses;
    for (const auto &feature : features) {
      if (feature.group_id > 0) {
        group_analyses[feature.group_id].insert(feature.analysis);
      }
    }

    std::set<int> valid_groups;
    for (const auto &pair : group_analyses) {
      if (static_cast<int>(pair.second.size()) >= min_samples) {
        valid_groups.insert(pair.first);
      }
    }

    // Assign feature group names (empty for invalid groups)
    for (auto &feature : features) {
      if (feature.group_id > 0 && valid_groups.count(feature.group_id) > 0) {
        feature.feature_group = group_names[feature.group_id];
      } else {
        feature.feature_group = "";
      }
    }
  } else {
    // Assign all feature group names
    for (auto &feature : features) {
      if (feature.group_id > 0) {
        feature.feature_group = group_names[feature.group_id];
      } else {
        feature.feature_group = "";
      }
    }
  }
}

// MARK: group_features_impl
void group_features_impl(
    nts::NTS_DATA &nts_data,
    const std::string &method,
    const std::vector<InternalStandard> &internal_standards,
    float rt_deviation,
    float ppm_threshold,
    int min_samples,
    float bin_size,
    bool debug,
    float debug_rt)
{
  std::ofstream debug_log;
  if (debug) {
    // Create default log filename
    std::ostringstream log_filename;
    log_filename << "log/debug_log_group_features_method" << method;
    if (debug_rt > 0.0f) {
      log_filename << "_rt" << std::fixed << std::setprecision(2) << debug_rt;
    }
    log_filename << ".log";

    debug_log.open(log_filename.str(), std::ios::out | std::ios::trunc);
    if (debug_log.is_open()) {
      debug_log << "=== Feature Grouping Debug Log ===" << std::endl;
      debug_log << "Method: " << method << std::endl;
      debug_log << "RT Deviation: " << rt_deviation << std::endl;
      debug_log << "PPM Threshold: " << ppm_threshold << std::endl;
      debug_log << "Min Samples: " << min_samples << std::endl;
      debug_log << "RT Bin Size: " << bin_size << " seconds" << std::endl;
      if (debug_rt > 0.0f) {
        debug_log << "Debug RT: " << debug_rt << " +/- " << rt_deviation << std::endl;
      }
      debug_log << std::endl;
    }
  }

  // Collect all features from all analyses
  std::vector<AlignmentFeature> all_features;

  for (size_t a = 0; a < nts_data.analyses.size(); ++a)
  {
    const nts::FEATURES &fts_i = nts_data.features[a];
    for (int i = 0; i < fts_i.size(); ++i)
    {
      AlignmentFeature af;
      af.analysis = nts_data.analyses[a];
      af.feature = fts_i.feature[i];
      af.rt = fts_i.rt[i];
      af.mass = fts_i.mass[i];
      af.intensity = fts_i.intensity[i];
      af.polarity = fts_i.polarity[i];
      af.rt_corrected = fts_i.rt[i]; // Will be updated by alignment
      af.group_id = 0;
      af.feature_group = "";
      all_features.push_back(af);
    }
  }

  if (all_features.empty())
  {
    if (debug_log.is_open()) {
      debug_log << "No features found for grouping." << std::endl;
      debug_log.close();
    }
    return;
  }

  // Perform RT alignment based on method
  if (method == "internal_standards")
  {
    if (!internal_standards.empty())
    {
      if (debug_log.is_open()) {
        debug_log << "--- Internal Standards Alignment ---" << std::endl;
        // Group by analysis
        std::map<std::string, std::vector<InternalStandard>> istd_by_analysis;
        for (const auto &istd : internal_standards) {
          istd_by_analysis[istd.analysis].push_back(istd);
        }

        for (const auto &pair : istd_by_analysis) {
          debug_log << "Analysis: " << pair.first << std::endl;
          for (const auto &istd : pair.second) {
            debug_log << "  " << istd.name << ": exp_rt=" << istd.exp_rt
                     << ", avg_rt=" << istd.avg_rt << ", shift=" << istd.rt_shift << std::endl;
          }
        }
        debug_log << std::endl;
      }

      calculate_rt_shifts_istd(all_features, internal_standards, bin_size);

      // Log RT corrections per analysis
      if (debug_log.is_open()) {
        std::map<std::string, std::vector<float>> shifts_by_analysis;
        for (const auto &feature : all_features) {
          float shift = feature.rt - feature.rt_corrected;
          shifts_by_analysis[feature.analysis].push_back(shift);
        }

        debug_log << "--- RT Corrections Applied ---" << std::endl;
        for (const auto &pair : shifts_by_analysis) {
          if (!pair.second.empty()) {
            float min_shift = *std::min_element(pair.second.begin(), pair.second.end());
            float max_shift = *std::max_element(pair.second.begin(), pair.second.end());
            float avg_shift = std::accumulate(pair.second.begin(), pair.second.end(), 0.0f) / pair.second.size();
            debug_log << "Analysis: " << pair.first << " - shift range: [" << min_shift
                     << ", " << max_shift << "], avg: " << avg_shift << std::endl;
          }
        }
        debug_log << std::endl;

        // Output detailed RT shift vectors for plotting
        debug_log << "--- RT Shift Vectors (for plotting) ---" << std::endl;
        for (const auto &pair : shifts_by_analysis) {
          const std::string &analysis_name = pair.first;

          // Collect RT and shift pairs for this analysis
          std::vector<std::pair<float, float>> rt_shift_pairs;
          for (const auto &feature : all_features) {
            if (feature.analysis == analysis_name) {
              float rt_rounded = std::round(feature.rt);
              float shift = feature.rt - feature.rt_corrected;
              rt_shift_pairs.push_back(std::make_pair(rt_rounded, shift));
            }
          }

          // Sort by RT
          std::sort(rt_shift_pairs.begin(), rt_shift_pairs.end());

          // Remove duplicates (keep first occurrence for each rounded RT)
          std::vector<int> unique_rts;
          std::vector<float> unique_shifts;
          float last_rt = -1.0f;
          for (const auto &pair : rt_shift_pairs) {
            if (pair.first != last_rt) {
              unique_rts.push_back(static_cast<int>(pair.first));
              unique_shifts.push_back(pair.second);
              last_rt = pair.first;
            }
          }

          // Output in R-parseable format
          debug_log << "# Analysis: " << analysis_name << std::endl;
          debug_log << "rt_vector <- c(";
          for (size_t i = 0; i < unique_rts.size(); ++i) {
            if (i > 0) debug_log << ", ";
            debug_log << unique_rts[i];
          }
          debug_log << ")" << std::endl;

          debug_log << "shift_vector <- c(";
          for (size_t i = 0; i < unique_shifts.size(); ++i) {
            if (i > 0) debug_log << ", ";
            if (std::isnan(unique_shifts[i])) {
              debug_log << "NaN";
            } else {
              debug_log << unique_shifts[i];
            }
          }
          debug_log << ")" << std::endl;
          debug_log << std::endl;
        }
      }
    }
    else
    {
      for (auto &feature : all_features)
      {
        feature.rt_corrected = feature.rt;
      }
      if (debug_log.is_open()) {
        debug_log << "No internal standards provided, no RT correction applied." << std::endl << std::endl;
      }
    }
  }
  else if (method == "obi_warp")
  {
    if (debug_log.is_open()) {
      debug_log << "--- OBI-Warp Alignment ---" << std::endl;
    }

    calculate_rt_shifts_obiwarp(all_features, ppm_threshold, rt_deviation, bin_size);

    // Log RT corrections per analysis
    if (debug_log.is_open()) {
      std::map<std::string, std::vector<float>> shifts_by_analysis;
      for (const auto &feature : all_features) {
        float shift = feature.rt - feature.rt_corrected;
        shifts_by_analysis[feature.analysis].push_back(shift);
      }

      debug_log << "--- RT Corrections Applied ---" << std::endl;
      for (const auto &pair : shifts_by_analysis) {
        if (!pair.second.empty()) {
          float min_shift = *std::min_element(pair.second.begin(), pair.second.end());
          float max_shift = *std::max_element(pair.second.begin(), pair.second.end());
          float avg_shift = std::accumulate(pair.second.begin(), pair.second.end(), 0.0f) / pair.second.size();
          debug_log << "Analysis: " << pair.first << " - shift range: [" << min_shift
                   << ", " << max_shift << "], avg: " << avg_shift << std::endl;
        }
      }
      debug_log << std::endl;

      // Output detailed RT shift vectors for plotting
      debug_log << "--- RT Shift Vectors (for plotting) ---" << std::endl;
      for (const auto &pair : shifts_by_analysis) {
        const std::string &analysis_name = pair.first;

        // Collect RT and shift pairs for this analysis
        std::vector<std::pair<float, float>> rt_shift_pairs;
        for (const auto &feature : all_features) {
          if (feature.analysis == analysis_name) {
            float rt_rounded = std::round(feature.rt);
            float shift = feature.rt - feature.rt_corrected;
            rt_shift_pairs.push_back(std::make_pair(rt_rounded, shift));
          }
        }

        // Sort by RT
        std::sort(rt_shift_pairs.begin(), rt_shift_pairs.end());

        // Remove duplicates (keep first occurrence for each rounded RT)
        std::vector<int> unique_rts;
        std::vector<float> unique_shifts;
        float last_rt = -1.0f;
        for (const auto &pair : rt_shift_pairs) {
          if (pair.first != last_rt) {
            unique_rts.push_back(static_cast<int>(pair.first));
            unique_shifts.push_back(pair.second);
            last_rt = pair.first;
          }
        }

        // Output in R-parseable format
        debug_log << "# Analysis: " << analysis_name << std::endl;
        debug_log << "rt_vector <- c(";
        for (size_t i = 0; i < unique_rts.size(); ++i) {
          if (i > 0) debug_log << ", ";
          debug_log << unique_rts[i];
        }
        debug_log << ")" << std::endl;

        debug_log << "shift_vector <- c(";
        for (size_t i = 0; i < unique_shifts.size(); ++i) {
          if (i > 0) debug_log << ", ";
          if (std::isnan(unique_shifts[i])) {
            debug_log << "NaN";
          } else {
            debug_log << unique_shifts[i];
          }
        }
        debug_log << ")" << std::endl;
        debug_log << std::endl;
      }
    }
  }
  else
  {
    // No alignment
    for (auto &feature : all_features)
    {
      feature.rt_corrected = feature.rt;
    }
    if (debug_log.is_open()) {
      debug_log << "No RT alignment method selected." << std::endl << std::endl;
    }
  }

  // Log features in debug RT window before grouping
  if (debug_log.is_open() && debug_rt > 0.0f) {
    debug_log << "--- Features in Debug RT Window (RT=" << debug_rt << " +/- " << rt_deviation << ") ---" << std::endl;
    float rt_min = debug_rt - rt_deviation;
    float rt_max = debug_rt + rt_deviation;

    for (const auto &feature : all_features) {
      if (feature.rt_corrected >= rt_min && feature.rt_corrected <= rt_max) {
        debug_log << "Analysis: " << feature.analysis << ", Feature: " << feature.feature
                 << ", Polarity: " << feature.polarity << ", Mass: " << feature.mass << ", RT: " << feature.rt
                 << ", RT_corrected: " << feature.rt_corrected
                 << ", Shift: " << (feature.rt - feature.rt_corrected) << std::endl;
      }
    }
    debug_log << std::endl;
  }

  // Group features using tolerance-based clustering
  group_features(all_features, ppm_threshold, rt_deviation, min_samples);

  // Log grouping results for debug RT window
  if (debug_log.is_open() && debug_rt > 0.0f) {
    debug_log << "--- Grouping Results for Debug RT Window ---" << std::endl;
    float rt_min = debug_rt - rt_deviation;
    float rt_max = debug_rt + rt_deviation;

    for (const auto &feature : all_features) {
      if (feature.rt_corrected >= rt_min && feature.rt_corrected <= rt_max) {
        debug_log << "Analysis: " << feature.analysis << ", Feature: " << feature.feature
                 << ", Group: " << feature.feature_group << " (ID: " << feature.group_id << ")"
                 << ", Polarity: " << feature.polarity << ", Mass: " << feature.mass << ", RT_corrected: " << feature.rt_corrected << std::endl;
      }
    }
    debug_log << std::endl;
  }

  // Update feature_group in NTS_DATA
  for (const auto &af : all_features)
  {
    // Find the analysis and feature
    for (size_t a = 0; a < nts_data.analyses.size(); ++a)
    {
      if (nts_data.analyses[a] == af.analysis)
      {
        nts::FEATURES &fts_i = nts_data.features[a];
        for (int i = 0; i < fts_i.size(); ++i)
        {
          if (fts_i.feature[i] == af.feature)
          {
            fts_i.feature_group[i] = af.feature_group;
            break;
          }
        }
        break;
      }
    }
  }

  // Count unique groups
  std::set<std::string> unique_groups;
  for (const auto &af : all_features)
  {
    if (!af.feature_group.empty())
    {
      unique_groups.insert(af.feature_group);
    }
  }

  if (debug_log.is_open()) {
    debug_log << "--- Summary ---" << std::endl;
    debug_log << "Total features: " << all_features.size() << std::endl;
    debug_log << "Total groups created: " << unique_groups.size() << std::endl;
    debug_log << "=== End Debug Log ===" << std::endl;
    debug_log.close();
  }
}

} // namespace alignment
} // namespace nts
