#include "NTS2_utils.h"
#include <iomanip>

void NTS2::NTS_DATA::find_features(
    const std::vector<float> &rtWindowsMin,
    const std::vector<float> &rtWindowsMax,
    const std::vector<int> &resolution_profile,
    const float &noiseThreshold,
    const float &minSNR,
    const int &minTraces,
    const float &baselineWindow,
    const float &maxWidth,
    const float &base_quantile)
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
  const int debug_cluster = 160;
  bool debug = true;

  for (size_t a = 0; a < analyses.size(); ++a)
  {
    Rcpp::Rcout << std::endl;
    Rcpp::Rcout << a + 1 << "/" << analyses.size() << " Processing analysis " << analyses[a] << std::endl;
    const sc::MS_SPECTRA_HEADERS &header = headers[a];
    std::vector<int> idx_load;
    std::vector<float> rt_load;
    std::vector<int> polarity_load;
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
              polarity_load.push_back(header.polarity[i]);
            }
          }
        }
      }
    }
    else
    {
      idx_load = header.index;
      rt_load = header.rt;
      polarity_load = header.polarity;
    }

    sc::MS_FILE ana(files[a]);
    
    // Separate data by polarity
    std::vector<float> spec_pos_rt, spec_pos_mz, spec_pos_intensity, spec_pos_noise;
    std::vector<float> spec_neg_rt, spec_neg_mz, spec_neg_intensity, spec_neg_noise;

    Rcpp::Rcout << "  1/5 Denoising " << idx_load.size() << " spectra (separating by polarity)" << std::endl;

    size_t total_raw_points = 0;
    size_t total_clean_points = 0;
    size_t pos_count = 0, neg_count = 0;

    for (size_t i = 0; i < idx_load.size(); ++i)
    {
      const float &rt = rt_load[i];
      const int &spectrum_idx = idx_load[i];
      const int &polarity = polarity_load[i];

      if (polarity > 0) {
        pos_count++;
        SF_UTILITY::denoise_spectra(
          ana,
          spectrum_idx,
          rt,
          noiseThreshold,
          minTraces,
          slope,
          intercept,
          spec_pos_rt,
          spec_pos_mz,
          spec_pos_intensity,
          spec_pos_noise,
          total_raw_points,
          total_clean_points,
          debug && i == 10, // Show debug info for first spectrum only
          base_quantile
        );
      } else if (polarity < 0) {
        neg_count++;
        SF_UTILITY::denoise_spectra(
          ana,
          spectrum_idx,
          rt,
          noiseThreshold,
          minTraces,
          slope,
          intercept,
          spec_neg_rt,
          spec_neg_mz,
          spec_neg_intensity,
          spec_neg_noise,
          total_raw_points,
          total_clean_points,
          debug && i == 10, // Show debug info for first spectrum only
          base_quantile
        );
      }
    }

    Rcpp::Rcout << "      Polarity distribution: " << pos_count << " positive, " << neg_count << " negative spectra" << std::endl;

    // Show denoising statistics
    float denoising_efficiency = total_raw_points > 0 ? 
        (1.0f - static_cast<float>(total_clean_points) / static_cast<float>(total_raw_points)) * 100.0f : 0.0f;
    
    Rcpp::Rcout << "      Denoising stats: " << total_raw_points << " -> " << total_clean_points 
                << " points (" << std::fixed << std::setprecision(1) << denoising_efficiency 
                << "% noise removed, base_quantile=" << base_quantile << ")" << std::endl;

    // Process positive and negative polarities separately
    std::vector<FEATURE> pos_features, neg_features;
    
    // Process positive polarity
    if (spec_pos_rt.size() > 0) {
      Rcpp::Rcout << "  2a/5 Clustering " << spec_pos_rt.size() << " positive polarity traces by m/z" << std::endl;
      
      std::vector<float> pos_clust_rt, pos_clust_mz, pos_clust_intensity, pos_clust_noise;
      std::vector<int> pos_clust_cluster;
      int pos_number_clusters = 0;

      SF_UTILITY::cluster_spectra_by_mz(
          spec_pos_rt, spec_pos_mz, spec_pos_intensity, spec_pos_noise,
          slope, intercept, minTraces, minSNR,
          pos_clust_rt, pos_clust_mz, pos_clust_intensity,
          pos_clust_noise, pos_clust_cluster, pos_number_clusters
      );

      Rcpp::Rcout << "  3a/5 Detecting peaks in " << pos_number_clusters << " positive m/z clusters" << std::endl;
      pos_features = SF_UTILITY::process_polarity_clusters(pos_clust_rt, pos_clust_mz, pos_clust_intensity, 
                                               pos_clust_noise, pos_clust_cluster, pos_number_clusters,
                                               +1, "[M+H]+", -1.007276f, // positive: subtract proton
                                               minTraces, minSNR, baselineWindow, maxWidth, 
                                               analyses[a], debug, debug_cluster);
    }

    // Process negative polarity  
    if (spec_neg_rt.size() > 0) {
      Rcpp::Rcout << "  2b/5 Clustering " << spec_neg_rt.size() << " negative polarity traces by m/z" << std::endl;
      
      std::vector<float> neg_clust_rt, neg_clust_mz, neg_clust_intensity, neg_clust_noise;
      std::vector<int> neg_clust_cluster;
      int neg_number_clusters = 0;

      SF_UTILITY::cluster_spectra_by_mz(
          spec_neg_rt, spec_neg_mz, spec_neg_intensity, spec_neg_noise,
          slope, intercept, minTraces, minSNR,
          neg_clust_rt, neg_clust_mz, neg_clust_intensity,
          neg_clust_noise, neg_clust_cluster, neg_number_clusters
      );

      Rcpp::Rcout << "  3b/5 Detecting peaks in " << neg_number_clusters << " negative m/z clusters" << std::endl;
      neg_features = SF_UTILITY::process_polarity_clusters(neg_clust_rt, neg_clust_mz, neg_clust_intensity, 
                                               neg_clust_noise, neg_clust_cluster, neg_number_clusters,
                                               -1, "[M-H]-", 1.007276f, // negative: add proton  
                                               minTraces, minSNR, baselineWindow, maxWidth, 
                                               analyses[a], debug, debug_cluster);
    }

    // Combine features from both polarities
    std::vector<FEATURE> all_features;
    all_features.reserve(pos_features.size() + neg_features.size());
    all_features.insert(all_features.end(), pos_features.begin(), pos_features.end());
    all_features.insert(all_features.end(), neg_features.begin(), neg_features.end());

    Rcpp::Rcout << "  4/5 Found " << all_features.size() << " total features (" 
                << pos_features.size() << " positive, " << neg_features.size() << " negative)" << std::endl;




    features[a] = FEATURES();
    features[a].analysis = analyses[a];

    for (const auto& feature : all_features)
    {
      features[a].append_feature(feature);
    }

    Rcpp::Rcout << "  5/5 Processing complete" << std::endl;
  }
}


