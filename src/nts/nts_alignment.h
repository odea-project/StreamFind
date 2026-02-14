// nts_alignment.h
// Feature alignment and grouping functions for NTS_DATA

#ifndef NTS_ALIGNMENT_H
#define NTS_ALIGNMENT_H

#include <vector>
#include <string>
#include <map>
#include "nts_utils.h"

namespace nts {
  struct NTS_DATA;  // Forward declaration
  struct FEATURE;   // Forward declaration
}

namespace nts {
namespace alignment {

// Struct to hold internal standard information
struct InternalStandard {
  std::string analysis;
  std::string name;
  float exp_rt;
  float avg_rt;
  float rt_shift;
};

// Struct to hold feature information for alignment
struct AlignmentFeature {
  std::string analysis;
  std::string feature;
  float rt;
  float mass;
  float intensity;
  int polarity;
  float rt_corrected;
  int group_id;
  std::string feature_group;
};

// Struct to hold anchor points for OBI-Warp alignment
struct AnchorPoint {
  float ref_rt;
  float target_rt;
  float rt_shift;
  float weight;
};

// Calculate RT shifts using internal standards with linear interpolation
void calculate_rt_shifts_istd(
  std::vector<AlignmentFeature> &features,
  const std::vector<InternalStandard> &internal_standards,
  float bin_size
);

// Calculate RT shifts using OBI-Warp alignment
void calculate_rt_shifts_obiwarp(
  std::vector<AlignmentFeature> &features,
  float ppm_threshold,
  float rt_deviation,
  float bin_size
);

// Group features based on mass and corrected RT using tolerance-based clustering
void group_features(
  std::vector<AlignmentFeature> &features,
  float ppm_threshold,
  float rt_deviation,
  int min_samples
);

// Helper function to interpolate RT shift at a given RT
float interpolate_rt_shift(
  float feature_rt,
  const std::vector<float> &anchor_rts,
  const std::vector<float> &anchor_shifts
);

// Forward declaration for NTS_DATA
struct NTS_DATA;

// Implementation function for NTS_DATA::group_features
void group_features_impl(
  nts::NTS_DATA &nts_data,
  const std::string &method,
  const std::vector<InternalStandard> &internal_standards,
  float rt_deviation,
  float ppm_threshold,
  int min_samples,
  float bin_size = 5.0f,
  bool debug = false,
  float debug_rt = 0.0f
);

} // namespace alignment
} // namespace nts

#endif // NTS_ALIGNMENT_H
