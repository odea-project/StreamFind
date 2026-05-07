// nts_gap_filling.h
// Feature gap filling declarations for NTS_DATA
// This file contains structures and functions for filling missing features across analyses

#ifndef NTS_GAP_FILLING_H
#define NTS_GAP_FILLING_H

#include <vector>
#include <string>
#include <unordered_map>
#include "../mass_spec/reader.h"

namespace nts
{
  // Forward declaration
  struct FEATURE;
  struct FEATURES;
  struct NTS_DATA;

  namespace gap_filling
  {
    // Structure to hold information about a feature group
    struct FEATURE_GROUP_INFO
    {
      std::string feature_group;
      float median_rt;
      float median_mz;
      float median_mass;
      float rt_range;  // Max RT - Min RT
      float min_rtmin; // Minimum rtmin across all features
      float max_rtmax; // Maximum rtmax across all features
      float min_mzmin; // Minimum mzmin across all features
      float max_mzmax; // Maximum mzmax across all features
      std::vector<std::string> present_analyses;
      std::vector<std::string> missing_analyses;
      int total_features;
    };

    // Structure to hold EIC data for gap filling
    struct EIC_DATA
    {
      std::vector<float> rt;
      std::vector<float> mz;
      std::vector<float> intensity;
      std::vector<float> noise;
      int size;
      bool valid;
    };

    // Structure to hold filled feature information
    struct FILLED_FEATURE_INFO
    {
      std::string analysis;
      std::string feature_group;
      std::string original_feature;
      std::string adduct;
      float rt;
      float mz;
      float mass;
      float intensity;
      float area;
      float noise;
      float sn;
      float rtmin;
      float rtmax;
      float width;
      float mzmin;
      float mzmax;
      float ppm;
      float fwhm_rt;
      float fwhm_mz;
      float gaussian_A;
      float gaussian_mu;
      float gaussian_sigma;
      float gaussian_r2;
      float jaggedness;
      float sharpness;
      float asymmetry;
      float modality;
      float plates;
      int polarity;
      bool filled;
      float correction;
      int eic_size;
      std::string eic_rt;
      std::string eic_mz;
      std::string eic_intensity;
      std::string eic_baseline;
      std::string eic_smoothed;
    };

    // Analyze feature groups to identify gaps
    std::vector<FEATURE_GROUP_INFO> analyze_feature_groups(
        const std::vector<FEATURES> &features,
        const std::vector<std::string> &analyses,
        const std::vector<std::string> &replicates,
        bool withinReplicate,
        int minSamples);

    // Extract EIC for a specific m/z and RT window
    EIC_DATA extract_eic_for_gap_filling(
        mass_spec::MS_FILE &ana,
        const mass_spec::MS_SPECTRA_HEADERS &headers,
        float target_mz,
        float target_rt,
        float mzExpand,
        float rtExpand,
        float minTracesIntensity,
        float ppmThreshold);

    // Perform peak picking on EIC data using deconvolution functions
    FILLED_FEATURE_INFO pick_peak_from_eic(
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
        bool debug = false);

    // Main implementation function
    void fill_features_impl(
        NTS_DATA &nts_data,
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
        std::string debugFG = "");

  } // namespace gap_filling
} // namespace nts

#endif // NTS_GAP_FILLING_H
