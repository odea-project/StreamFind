// nts_filter_features.h
// Feature filtering for NTS_DATA

#ifndef NTS_FILTER_FEATURES_H
#define NTS_FILTER_FEATURES_H

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
} // namespace nts

#endif // NTS_FILTER_FEATURES_H
