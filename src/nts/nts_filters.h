// nts_filters.h
// Filtering for NTS_DATA structures

#ifndef NTS_FILTERS_H
#define NTS_FILTERS_H

#include <vector>
#include <string>

namespace nts
{
  struct NTS_DATA;

  namespace filter_features
  {
    void filter_features_impl(
        NTS_DATA &nts_data,
        double minSN,
        double minIntensity,
        double minArea,
        double minWidth,
        double maxWidth,
        double maxPPM,
        double minFwhmRT,
        double maxFwhmRT,
        double minFwhmMZ,
        double maxFwhmMZ,
        double minGaussianA,
        double minGaussianMu,
        double maxGaussianMu,
        double minGaussianSigma,
        double maxGaussianSigma,
        double minGaussianR2,
        double maxJaggedness,
        double minSharpness,
        double minAsymmetry,
        double maxAsymmetry,
        int maxModality,
        bool hasMaxModality,
        double minPlates,
        bool hasOnlyFilled,
        bool onlyFilledValue,
        bool removeFilled,
        int minSizeEIC,
        bool hasMinSizeEIC,
        int minSizeMS1,
        bool hasMinSizeMS1,
        int minSizeMS2,
        bool hasMinSizeMS2,
        double minRelPresenceReplicate,
        bool removeIsotopes,
        bool removeAdducts,
        bool removeLosses);
  } // namespace filter_features

  namespace filter_suspects
  {
    void filter_suspects_impl(
        NTS_DATA &nts_data,
        const std::vector<std::string> &names,
        double minScore,
        double maxErrorRT,
        double maxErrorMass,
        const std::vector<int> &idLevels,
        int minSharedFragments,
        double minCosineSimilarity);
  } // namespace filter_suspects

  namespace filter_internal_standards
  {
    void filter_internal_standards_impl(
        NTS_DATA &nts_data,
        const std::vector<std::string> &names,
        double minScore,
        double maxErrorRT,
        double maxErrorMass,
        const std::vector<int> &idLevels,
        int minSharedFragments,
        double minCosineSimilarity);
  } // namespace filter_internal_standards

  namespace filter_features_ms2
  {
    // Filter MS2 peak lists stored in FEATURES.
    // Steps:
    //   1. Collect MS2 peaks from blank analyses; cluster by mzClust + blankPresenceThreshold.
    //   2. Remove from every feature any MS2 peak within mzClust of a clustered blank peak.
    //   3. Keep only top-N peaks (if top > 0), apply minIntensity and relMinIntensity thresholds.
    // All encoded peak lists are updated in-place (ms2_mz, ms2_intensity, ms2_size).
    void filter_features_ms2_impl(
        NTS_DATA &nts_data,
        int top,                        // 0 = no limit
        float minIntensity,             // NaN = no limit
        float relMinIntensity,          // NaN = no limit
        bool blankClean,
        float mzClust,
        float blankPresenceThreshold,
        float globalPresenceThreshold);
  } // namespace filter_features_ms2
} // namespace nts

#endif // NTS_FILTERS_H
