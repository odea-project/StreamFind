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

      // Initialize debug logging if debugRT is specified
      const bool debug_mode = debugRT > 0.0f;
      if (debug_mode) {
        std::ostringstream log_filename;
        log_filename << "log/debug_log_create_components_"
                     << std::fixed << std::setprecision(2) << debugRT << ".log";
        utils::init_debug_log(log_filename.str(), "=== Component Creation Debug Log ===\n");
        DEBUG_OUT("Debugging components with RT: " << debugRT
                  << " (window: " << left_offset << " to " << right_offset << ")\n");
        if (!debugAnalysis.empty()) {
          DEBUG_OUT("Filtering for analysis: " << debugAnalysis << "\n");
        }
      }

      for (size_t i = 0; i < nts_data.features.size(); ++i)
      {
        FEATURES &fts = nts_data.features[i];
        const int n = fts.size();

        if (n == 0)
        {
          continue;
        }

        // Initialize component counter for this analysis
        int component_counter = 1;

        // Skip this analysis if debugAnalysis is specified and doesn't match
        const bool debug_this_analysis = debugAnalysis.empty() ||
                                         (i < nts_data.analyses.size() && nts_data.analyses[i] == debugAnalysis);
        const bool should_debug = debug_mode && debug_this_analysis;

        struct Interval
        {
          float start;
          float end;
          int idx;
          float rt_center;
        };

        std::vector<Interval> intervals;
        intervals.reserve(n);

        for (int j = 0; j < n; ++j)
        {
          const FEATURE &ft = fts.get_feature(j);

          const float start = ft.rt + left_offset;
          const float end = ft.rt + right_offset;

          intervals.push_back({start, end, j, ft.rt});

          // Debug features within the debug RT window
          if (should_debug && ft.rt >= (debugRT + left_offset) && ft.rt <= (debugRT + right_offset)) {
            DEBUG_LOG("Analysis " << (i < nts_data.analyses.size() ? nts_data.analyses[i] : std::to_string(i)) << ": Feature " << ft.feature
                      << " at RT=" << ft.rt << " (mz=" << ft.mz
                      << ", intensity=" << ft.intensity << ")\n");
          }
        }

        std::sort(intervals.begin(), intervals.end(), [](const Interval &a, const Interval &b) {
          if (a.start == b.start)
          {
            return a.end < b.end;
          }
          return a.start < b.start;
        });

        std::vector<std::vector<int>> clusters;
        std::vector<float> cluster_rt_sum;
        std::vector<int> cluster_counts;

        // Use an anchor interval per cluster to avoid long chains of overlaps
        float anchor_start = intervals[0].start;
        float anchor_end = intervals[0].end;

        clusters.push_back({intervals[0].idx});
        cluster_rt_sum.push_back(intervals[0].rt_center);
        cluster_counts.push_back(1);

        for (size_t j = 1; j < intervals.size(); ++j)
        {
          const Interval &intv = intervals[j];

          const bool overlaps_anchor = intv.start <= anchor_end && intv.end >= anchor_start;

          if (overlaps_anchor)
          {
            clusters.back().push_back(intv.idx);
            cluster_rt_sum.back() += intv.rt_center;
            cluster_counts.back() += 1;
          }
          else
          {
            clusters.push_back({intv.idx});
            cluster_rt_sum.push_back(intv.rt_center);
            cluster_counts.push_back(1);
            anchor_start = intv.start;
            anchor_end = intv.end;
          }
        }

        // Further subdivide clusters based on EIC correlation
        for (size_t c = 0; c < clusters.size(); ++c)
        {
          const std::vector<int> &cluster = clusters[c];

          // Check if this cluster contains features in the debug RT window
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
                        << ": Processing Cluster " << c << " (" << cluster.size() << " features) ---\n");
              for (const int idx : cluster) {
                const FEATURE &ft = fts.get_feature(idx);
                DEBUG_LOG("  Feature " << ft.feature << ": RT=" << ft.rt
                          << ", mz=" << ft.mz << ", intensity=" << ft.intensity << "\n");
              }
            }
          }

          if (cluster.size() == 1 || minCorrelation <= 0.0f) {
            // No subdivision needed
            const float mean_rt = cluster_rt_sum[c] / static_cast<float>(cluster_counts[c]);
            std::ostringstream oss;
            oss << "FC" << component_counter++ << "_RT" << std::fixed << std::setprecision(0) << mean_rt;
            const std::string component_id = oss.str();

            for (const int idx : cluster) {
              FEATURE ft = fts.get_feature(idx);
              ft.feature_component = component_id;
              fts.set_feature(idx, ft);
            }
            continue;
          }

          // Build correlation matrix and decode EICs once
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
            // All features lack valid EICs, keep them together
            const float mean_rt = cluster_rt_sum[c] / static_cast<float>(cluster_counts[c]);
            std::ostringstream oss;
            oss << "FC" << component_counter++ << "_RT" << std::fixed << std::setprecision(0) << mean_rt;
            const std::string component_id = oss.str();

            for (const int idx : cluster) {
              FEATURE ft = fts.get_feature(idx);
              ft.feature_component = component_id;
              fts.set_feature(idx, ft);
            }
            continue;
          }

          // Perform hierarchical clustering based on correlation
          std::vector<std::vector<int>> sub_clusters;
          std::vector<bool> assigned(feature_eics.size(), false);

          for (size_t seed = 0; seed < feature_eics.size(); ++seed) {
            if (assigned[seed]) continue;

            std::vector<int> sub_cluster;
            sub_cluster.push_back(seed);
            assigned[seed] = true;

            if (debug_this_cluster && should_debug) {
              DEBUG_LOG("\n  Sub-cluster with seed feature "
                        << fts.get_feature(feature_eics[seed].idx).feature
                        << " (RT=" << feature_eics[seed].rt << "):\n");
            }

            // Find all features that correlate well with the seed
            for (size_t candidate = seed + 1; candidate < feature_eics.size(); ++candidate) {
              if (assigned[candidate]) continue;

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
                  assigned[candidate] = true;
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

            sub_clusters.push_back(sub_cluster);
          }

          // Assign component IDs to sub-clusters
          for (size_t sc = 0; sc < sub_clusters.size(); ++sc) {
            const auto &sub_cluster = sub_clusters[sc];
            float sub_rt_sum = 0.0f;
            for (int sub_idx : sub_cluster) {
              sub_rt_sum += feature_eics[sub_idx].rt;
            }
            const float mean_rt = sub_rt_sum / static_cast<float>(sub_cluster.size());

            std::ostringstream oss;
            oss << "FC" << component_counter++ << "_RT" << std::fixed << std::setprecision(0) << mean_rt;
            const std::string component_id = oss.str();

            if (debug_this_cluster && should_debug) {
              DEBUG_LOG("\n  Final Component " << component_id << " ("
                        << sub_cluster.size() << " features):\n");
            }

            for (int sub_idx : sub_cluster) {
              FEATURE ft = fts.get_feature(feature_eics[sub_idx].idx);
              ft.feature_component = component_id;
              fts.set_feature(feature_eics[sub_idx].idx, ft);

              if (debug_this_cluster && should_debug) {
                DEBUG_LOG("    " << ft.feature << " (RT=" << ft.rt << ")\n");
              }
            }
          }
        }
      }

      // Close debug log if it was opened
      if (debug_mode) {
        DEBUG_OUT("\nDebug log closed.\n");
        utils::close_debug_log();
      }
    }

  } // namespace componentization
} // namespace nts
