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
  const int debug_cluster = 439;
  const bool debug = false;

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
      ;
      rt_load = header.rt;
    }

    sc::MS_FILE ana(files[a]);
    std::vector<float> spec_rt, spec_mz, spec_intensity, spec_noise;

    Rcpp::Rcout << "  1/5 Denoising " << idx_load.size() << " spectra" << std::endl;

    // Track denoising statistics
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
        debug,
        spec_rt,
        spec_mz,
        spec_intensity,
        spec_noise,
        total_raw_points,
        total_clean_points
      );
    }
  }
};