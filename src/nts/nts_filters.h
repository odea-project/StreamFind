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
} // namespace nts

#endif // NTS_FILTERS_H
