#include "nts_componentization.h"
#include "nts_utils.h"
#include "nts.h"
#include <Rcpp.h>
#include <iomanip>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <cmath>

namespace nts
{
  namespace componentization
  {
    // Helper function to decode base64-encoded EIC data
    std::vector<float> decode_eic_base64(const std::string &base64_str) {
      if (base64_str.empty()) return std::vector<float>();
      try {
        std::string decoded = sc::decode_base64(base64_str);
        return sc::decode_little_endian_to_float(decoded, 4);
      } catch (...) {
        return std::vector<float>();
      }
    }

    // Helper function to calculate Pearson correlation between two aligned EIC vectors
    float calculate_pearson_correlation(
      const std::vector<float> &x,
      const std::vector<float> &y
    ) {
      const size_t n = std::min(x.size(), y.size());
      if (n < 3) return 0.0f; // Need at least 3 points

      // Calculate means
      float mean_x = 0.0f, mean_y = 0.0f;
      for (size_t i = 0; i < n; ++i) {
        mean_x += x[i];
        mean_y += y[i];
      }
      mean_x /= static_cast<float>(n);
      mean_y /= static_cast<float>(n);

      // Calculate correlation
      float numerator = 0.0f, denom_x = 0.0f, denom_y = 0.0f;
      for (size_t i = 0; i < n; ++i) {
        const float dx = x[i] - mean_x;
        const float dy = y[i] - mean_y;
        numerator += dx * dy;
        denom_x += dx * dx;
        denom_y += dy * dy;
      }

      if (denom_x < 1e-10f || denom_y < 1e-10f) return 0.0f;
      return numerator / std::sqrt(denom_x * denom_y);
    }

    // Helper function to align two EICs by their RT values (not shifted by apex)
    // This preserves temporal information so time-shifted peaks show poor correlation
    std::pair<std::vector<float>, std::vector<float>> align_eics_by_rt(
      const std::vector<float> &rt1, const std::vector<float> &int1,
      const std::vector<float> &rt2, const std::vector<float> &int2
    ) {
      std::vector<float> aligned1, aligned2;

      if (rt1.empty() || rt2.empty() || int1.empty() || int2.empty()) {
        return {aligned1, aligned2};
      }

      if (rt1.size() != int1.size() || rt2.size() != int2.size()) {
        return {aligned1, aligned2};
      }

      // Find overlapping RT range
      const float rt1_min = *std::min_element(rt1.begin(), rt1.end());
      const float rt1_max = *std::max_element(rt1.begin(), rt1.end());
      const float rt2_min = *std::min_element(rt2.begin(), rt2.end());
      const float rt2_max = *std::max_element(rt2.begin(), rt2.end());

      const float overlap_start = std::max(rt1_min, rt2_min);
      const float overlap_end = std::min(rt1_max, rt2_max);

      if (overlap_start >= overlap_end) {
        return {aligned1, aligned2}; // No overlap
      }

      // For each RT point in EIC1 within overlap, find closest match in EIC2
      aligned1.reserve(rt1.size());
      aligned2.reserve(rt1.size());

      for (size_t i = 0; i < rt1.size(); ++i) {
        const float rt_val = rt1[i];

        // Skip if outside overlap range
        if (rt_val < overlap_start || rt_val > overlap_end) continue;

        // Find closest RT in EIC2
        size_t best_idx = 0;
        float min_diff = std::abs(rt2[0] - rt_val);

        for (size_t j = 1; j < rt2.size(); ++j) {
          const float diff = std::abs(rt2[j] - rt_val);
          if (diff < min_diff) {
            min_diff = diff;
            best_idx = j;
          }
        }

        // Only include if RT values are reasonably close (within 0.5 seconds)
        if (min_diff <= 0.5f) {
          aligned1.push_back(int1[i]);
          aligned2.push_back(int2[best_idx]);
        }
      }

      return {aligned1, aligned2};
    }

    // MARK: create_components_impl
    void create_components_impl(
        nts::NTS_DATA &nts_data,
        const std::vector<float> &rtWindow,
        float minCorrelation,
        float debugRT,
        const std::string &debugAnalysis)
    {
      const float left_offset = rtWindow.size() >= 1 ? rtWindow[0] : 0.0f;
      const float right_offset = rtWindow.size() >= 2 ? rtWindow[1] : 0.0f;

      const bool debug_mode = debugRT > 0.0f;
      if (debug_mode) {
        std::ostringstream log_filename;
        log_filename << "log/debug_log_create_components_"
                     << std::fixed << std::setprecision(2) << debugRT << ".log";
        utils::init_debug_log(log_filename.str(), "=== Component Creation Debug Log ===\n");
      }

      bool debug_triggered = false;

      for (size_t i = 0; i < nts_data.features.size(); ++i)
      {
        FEATURES &fts = nts_data.features[i];
        const int n = fts.size();

        if (n == 0)
        {
          continue;
        }

        const bool debug_this_analysis = debugAnalysis.empty() ||
                                         (i < nts_data.analyses.size() && nts_data.analyses[i] == debugAnalysis);
        const bool should_debug = debug_mode && debug_this_analysis;

        if (should_debug && !debug_triggered) {
          debug_triggered = true;
          const std::string analysis_name = i < nts_data.analyses.size() ? nts_data.analyses[i] : std::to_string(i);
          Rcpp::Rcout << "Debugging components: Analysis '" << analysis_name
                      << "' RT=" << debugRT << " (window " << left_offset << " to " << right_offset
                      << ") -> [" << (debugRT + left_offset) << ", " << (debugRT + right_offset) << "]" << std::endl;
          DEBUG_OUT("\nDebugging analysis: " << analysis_name << "\n");
        }

        std::map<int, std::vector<int>> polarity_groups;
        for (int j = 0; j < n; ++j)
        {
          const FEATURE &ft = fts.get_feature(j);
          polarity_groups[ft.polarity].push_back(j);
        }

        for (const auto &[polarity, feature_indices] : polarity_groups)
        {
          if (feature_indices.empty()) continue;

          int component_counter = 1;

          // Determine polarity suffix
          std::string polarity_suffix;
          if (polarity > 0) {
            polarity_suffix = "_POS";
          } else if (polarity < 0) {
            polarity_suffix = "_NEG";
          } else {
            polarity_suffix = "";
          }

          struct FeatureRT {
            int idx;
            float rt;
          };

          std::vector<FeatureRT> sorted_features;
          sorted_features.reserve(feature_indices.size());

          for (int j : feature_indices) {
            const FEATURE &ft = fts.get_feature(j);
            sorted_features.push_back({j, ft.rt});
          }

          std::sort(sorted_features.begin(), sorted_features.end(),
                    [](const FeatureRT &a, const FeatureRT &b) {
                      return a.rt < b.rt;
                    });

          // Sliding window: each unassigned feature seeds a cluster within RT window
          std::vector<bool> assigned(sorted_features.size(), false);

          for (size_t cluster_idx = 0; cluster_idx < sorted_features.size(); ++cluster_idx) {
            if (assigned[cluster_idx]) continue;

            const float seed_rt = sorted_features[cluster_idx].rt;
            const float window_start = seed_rt + left_offset;
            const float window_end = seed_rt + right_offset;

            std::vector<int> cluster;
            float rt_sum = 0.0f;

            // Collect unassigned features within RT window
            for (size_t j = cluster_idx; j < sorted_features.size(); ++j) {
              if (assigned[j]) continue;

              const float ft_rt = sorted_features[j].rt;

              if (ft_rt > window_end) break;

              // Include if within window
              if (ft_rt >= window_start && ft_rt <= window_end) {
                cluster.push_back(sorted_features[j].idx);
                rt_sum += ft_rt;
              }
            }

            if (cluster.empty()) continue;

            const int cluster_count = static_cast<int>(cluster.size());
            const float cluster_rt_mean = rt_sum / static_cast<float>(cluster_count);

            // Debug if any feature falls within the debug RT window
            bool debug_this_cluster = false;
            if (should_debug) {
              for (const int idx : cluster) {
                const FEATURE &ft = fts.get_feature(idx);
                if (ft.rt >= (debugRT + left_offset) && ft.rt <= (debugRT + right_offset)) {
                  debug_this_cluster = true;
                  break;
                }
              }
              if (debug_this_cluster) {
                DEBUG_LOG("\n--- Analysis " << (i < nts_data.analyses.size() ? nts_data.analyses[i] : std::to_string(i))
                          << " [Polarity=" << polarity << "]: Processing Cluster starting at RT=" << seed_rt
                          << " (" << cluster.size() << " features) ---\n");
                DEBUG_LOG("  Sliding window cluster starting from seed RT=" << seed_rt
                          << " with window [" << window_start << ", "
                          << window_end << "]\n");
                for (const int idx : cluster) {
                  const FEATURE &ft = fts.get_feature(idx);
                  const bool in_debug_window = ft.rt >= (debugRT + left_offset) && ft.rt <= (debugRT + right_offset);
                  DEBUG_LOG("  Feature " << ft.feature << ": RT=" << ft.rt
                            << ", mz=" << ft.mz << ", intensity=" << ft.intensity
                            << (in_debug_window ? " [IN DEBUG WINDOW]" : "") << "\n");
                }
              }
            }

            if (cluster.size() == 1 || minCorrelation <= 0.0f) {
              std::ostringstream oss;
              oss << "FC" << component_counter++ << "_RT" << std::fixed << std::setprecision(0) << cluster_rt_mean << polarity_suffix;
              const std::string component_id = oss.str();

              for (const int idx : cluster) {
                FEATURE ft = fts.get_feature(idx);
                ft.feature_component = component_id;
                fts.set_feature(idx, ft);

                // Mark as assigned
                for (size_t k = 0; k < sorted_features.size(); ++k) {
                  if (sorted_features[k].idx == idx) {
                    assigned[k] = true;
                    break;
                  }
                }
              }
              continue;
            }

            // Decode EICs for correlation analysis
            struct FeatureEIC {
              int idx;
              float rt;
              std::vector<float> eic_rt;
              std::vector<float> eic_int;
            };

            std::vector<FeatureEIC> feature_eics;
            feature_eics.reserve(cluster.size());

            for (const int idx : cluster) {
              const FEATURE &ft = fts.get_feature(idx);
              FeatureEIC feic;
              feic.idx = idx;
              feic.rt = ft.rt;
              feic.eic_rt = decode_eic_base64(ft.eic_rt);
              feic.eic_int = decode_eic_base64(ft.eic_intensity);
              if (!feic.eic_rt.empty() && !feic.eic_int.empty()) {
                feature_eics.push_back(feic);
              }
            }

            if (feature_eics.size() <= 1) {
              std::ostringstream oss;
              oss << "FC" << component_counter++ << "_RT" << std::fixed << std::setprecision(0) << cluster_rt_mean << polarity_suffix;
              const std::string component_id = oss.str();

              for (const int idx : cluster) {
                FEATURE ft = fts.get_feature(idx);
                ft.feature_component = component_id;
                fts.set_feature(idx, ft);

                // Mark as assigned
                for (size_t k = 0; k < sorted_features.size(); ++k) {
                  if (sorted_features[k].idx == idx) {
                    assigned[k] = true;
                    break;
                  }
                }
              }
              continue;
            }

            // Hierarchical clustering based on correlation
            std::vector<std::vector<int>> sub_clusters;
            std::vector<bool> eic_assigned(feature_eics.size(), false);

            for (size_t seed = 0; seed < feature_eics.size(); ++seed) {
              if (eic_assigned[seed]) continue;

              std::vector<int> sub_cluster;
              sub_cluster.push_back(seed);
              eic_assigned[seed] = true;

              if (debug_this_cluster && should_debug) {
                DEBUG_LOG("\n  Sub-cluster with seed feature "
                          << fts.get_feature(feature_eics[seed].idx).feature
                          << " (RT=" << feature_eics[seed].rt << "):\n");
              }

              const float sub_seed_rt = feature_eics[seed].rt;
              const float sub_window_start = sub_seed_rt + left_offset;
              const float sub_window_end = sub_seed_rt + right_offset;

              // Check correlation with features in cluster
              for (size_t candidate = seed + 1; candidate < feature_eics.size(); ++candidate) {
                if (eic_assigned[candidate]) continue;

                // Check correlation with seed
                const auto &eic_seed = feature_eics[seed];
                const auto &eic_cand = feature_eics[candidate];

                auto [aligned1, aligned2] = align_eics_by_rt(
                  eic_seed.eic_rt, eic_seed.eic_int,
                  eic_cand.eic_rt, eic_cand.eic_int
                );

                if (aligned1.size() >= 3) {
                  const float corr = calculate_pearson_correlation(aligned1, aligned2);

                  if (debug_this_cluster && should_debug) {
                    DEBUG_LOG("    Feature " << fts.get_feature(feature_eics[candidate].idx).feature
                              << " (RT=" << feature_eics[candidate].rt << "): correlation="
                              << corr << " (aligned points: " << aligned1.size() << ")");
                  }

                  if (corr >= minCorrelation) {
                    sub_cluster.push_back(candidate);
                    eic_assigned[candidate] = true;
                    if (debug_this_cluster && should_debug) {
                      DEBUG_LOG(" -> GROUPED\n");
                    }
                  } else {
                    if (debug_this_cluster && should_debug) {
                      DEBUG_LOG(" -> SEPARATED\n");
                    }
                  }
                } else {
                  if (debug_this_cluster && should_debug) {
                    DEBUG_LOG("    Feature " << fts.get_feature(feature_eics[candidate].idx).feature
                              << " (RT=" << feature_eics[candidate].rt << "): insufficient overlap ("
                              << aligned1.size() << " points)\n");
                  }
                }
              }

              // Expansion: search for additional features within sub-seed's RT window
              for (size_t k = 0; k < sorted_features.size(); ++k) {
                if (assigned[k]) continue;

                const int feat_idx = sorted_features[k].idx;

                // Check if feature already in current cluster
                bool already_in_cluster = false;
                for (const auto &feic : feature_eics) {
                  if (feic.idx == feat_idx) {
                    already_in_cluster = true;
                    break;
                  }
                }
                if (already_in_cluster) continue;

                const FEATURE &ft = fts.get_feature(feat_idx);

                // Check if within sub-seed's RT window
                if (ft.rt >= sub_window_start && ft.rt <= sub_window_end) {
                  // Decode EIC for this additional feature
                  FeatureEIC additional_feic;
                  additional_feic.idx = feat_idx;
                  additional_feic.rt = ft.rt;
                  additional_feic.eic_rt = decode_eic_base64(ft.eic_rt);
                  additional_feic.eic_int = decode_eic_base64(ft.eic_intensity);

                  if (!additional_feic.eic_rt.empty() && !additional_feic.eic_int.empty()) {
                    const auto &eic_seed = feature_eics[seed];

                    auto [aligned1, aligned2] = align_eics_by_rt(
                      eic_seed.eic_rt, eic_seed.eic_int,
                      additional_feic.eic_rt, additional_feic.eic_int
                    );

                    if (aligned1.size() >= 3) {
                      const float corr = calculate_pearson_correlation(aligned1, aligned2);

                      if (debug_this_cluster && should_debug) {
                        DEBUG_LOG("    [EXPANDED] Feature " << ft.feature
                                  << " (RT=" << additional_feic.rt << "): correlation="
                                  << corr << " (aligned points: " << aligned1.size() << ")");
                      }

                      if (corr >= minCorrelation) {
                        const int new_idx = static_cast<int>(feature_eics.size());
                        feature_eics.push_back(additional_feic);
                        eic_assigned.push_back(true);
                        sub_cluster.push_back(new_idx);
                        assigned[k] = true;

                        if (debug_this_cluster && should_debug) {
                          DEBUG_LOG(" -> GROUPED (EXPANDED)\n");
                        }
                      } else {
                        if (debug_this_cluster && should_debug) {
                          DEBUG_LOG(" -> SEPARATED\n");
                        }
                      }
                    }
                  }
                }
              }

              sub_clusters.push_back(sub_cluster);
            }

            // Assign component IDs to sub-clusters and mark features as assigned
            for (size_t sc = 0; sc < sub_clusters.size(); ++sc) {
              const auto &sub_cluster = sub_clusters[sc];
              float sub_rt_sum = 0.0f;
              for (int sub_idx : sub_cluster) {
                sub_rt_sum += feature_eics[sub_idx].rt;
              }
              const float mean_rt = sub_rt_sum / static_cast<float>(sub_cluster.size());

              std::ostringstream oss;
              oss << "FC" << component_counter++ << "_RT" << std::fixed << std::setprecision(0) << mean_rt << polarity_suffix;
              const std::string component_id = oss.str();

              if (debug_this_cluster && should_debug) {
                DEBUG_LOG("\n  Final Component " << component_id << " ("
                          << sub_cluster.size() << " features):\n");
              }

              for (int sub_idx : sub_cluster) {
                const int feature_idx = feature_eics[sub_idx].idx;
                FEATURE ft = fts.get_feature(feature_idx);
                ft.feature_component = component_id;
                fts.set_feature(feature_idx, ft);

                for (size_t k = 0; k < sorted_features.size(); ++k) {
                  if (sorted_features[k].idx == feature_idx) {
                    assigned[k] = true;
                    break;
                  }
                }

                if (debug_this_cluster && should_debug) {
                  DEBUG_LOG("    " << ft.feature << " (RT=" << ft.rt << ")\n");
                }
              }
            }
          }
        }
      }

      if (debug_mode) {
        utils::close_debug_log();
      }
    }

  } // namespace componentization
} // namespace nts
