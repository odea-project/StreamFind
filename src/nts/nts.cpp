#include "nts.h"
#include "utils.h"
#include <iomanip>
#include <filesystem>

// MARK: find_features
void nts::NTS_DATA::find_features(
    const std::vector<float> &rtWindowsMin,
    const std::vector<float> &rtWindowsMax,
    const float &ppmThreshold,
    const float &noiseThreshold,
    const float &minSNR,
    const int &minTraces,
    const float &baselineWindow,
    const float &maxWidth,
    const float &base_quantile,
    const float &debug_mz)
{

  if (rtWindowsMin.size() != rtWindowsMax.size())
  {
    Rcpp::Rcout << "Error: rtWindowsMin and rtWindowsMax must have the same length!" << std::endl;
    return;
  }

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

    Rcpp::Rcout << "  1/5 Denoising " << idx_load.size() << " spectra" << std::endl;

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
        nts::utils::denoise_spectra(
          ana,
          spectrum_idx,
          rt,
          noiseThreshold,
          minTraces,
          ppmThreshold,
          spec_pos_rt,
          spec_pos_mz,
          spec_pos_intensity,
          spec_pos_noise,
          total_raw_points,
          total_clean_points,
          false && i == 10, // Show debug info for first spectrum only
          base_quantile
        );
      } else if (polarity < 0) {
        neg_count++;
        nts::utils::denoise_spectra(
          ana,
          spectrum_idx,
          rt,
          noiseThreshold,
          minTraces,
          ppmThreshold,
          spec_neg_rt,
          spec_neg_mz,
          spec_neg_intensity,
          spec_neg_noise,
          total_raw_points,
          total_clean_points,
          false && i == 10, // Show debug info for first spectrum only
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

      utils::cluster_spectra_by_mz(
        spec_pos_rt, spec_pos_mz, spec_pos_intensity, spec_pos_noise,
        ppmThreshold, minTraces, minSNR,
        pos_clust_rt, pos_clust_mz, pos_clust_intensity,
        pos_clust_noise, pos_clust_cluster, pos_number_clusters
      );

      Rcpp::Rcout << "  3a/5 Detecting peaks in " << pos_number_clusters << " positive m/z clusters" << std::endl;
      pos_features = utils::process_polarity_clusters(
        pos_clust_rt, pos_clust_mz, pos_clust_intensity,
        pos_clust_noise, pos_clust_cluster, pos_number_clusters,
        +1, "[M+H]+", -1.007276f, // positive: subtract proton
        minTraces, minSNR, baselineWindow, maxWidth,
        analyses[a],
        debug_mz
      );
    }

    // Process negative polarity
    if (spec_neg_rt.size() > 0) {
      Rcpp::Rcout << "  2b/5 Clustering " << spec_neg_rt.size() << " negative polarity traces by m/z" << std::endl;
      std::vector<float> neg_clust_rt, neg_clust_mz, neg_clust_intensity, neg_clust_noise;
      std::vector<int> neg_clust_cluster;
      int neg_number_clusters = 0;

      utils::cluster_spectra_by_mz(
        spec_neg_rt, spec_neg_mz, spec_neg_intensity, spec_neg_noise,
        ppmThreshold, minTraces, minSNR,
        neg_clust_rt, neg_clust_mz, neg_clust_intensity,
        neg_clust_noise, neg_clust_cluster, neg_number_clusters
      );

      Rcpp::Rcout << "  3b/5 Detecting peaks in " << neg_number_clusters << " negative m/z clusters" << std::endl;
      neg_features = utils::process_polarity_clusters(
        neg_clust_rt, neg_clust_mz, neg_clust_intensity,
        neg_clust_noise, neg_clust_cluster, neg_number_clusters,
        -1, "[M-H]-", 1.007276f, // negative: add proton
        minTraces, minSNR, baselineWindow, maxWidth,
        analyses[a],
        debug_mz
      );
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

// MARK: load_features_ms1
void nts::NTS_DATA::load_features_ms1(
    bool filtered,
    const std::vector<float> &rtWindow,
    const std::vector<float> &mzWindow,
    float minTracesIntensity,
    float mzClust,
    float presence)
{
  const bool hasRtWindow = rtWindow.size() >= 2;
  const bool hasMzWindow = mzWindow.size() >= 2;

  for (size_t i = 0; i < features.size(); i++)
  {
    FEATURES &fts_i = features[i];

    if (fts_i.size() == 0)
      continue;

    sc::MS_TARGETS targets;
    int counter = 0;

    for (int j = 0; j < fts_i.size(); j++)
    {
      const FEATURE &ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms1_size > 0 && !ft_j.ms1_mz.empty() && !ft_j.ms1_intensity.empty())
        continue;

      float mzmin = ft_j.mzmin;
      float mzmax = ft_j.mzmax;
      float rtmin = ft_j.rtmin;
      float rtmax = ft_j.rtmax;

      if (hasMzWindow)
      {
        mzmin = ft_j.mzmin + mzWindow[0]; // left boundary adjustment
        mzmax = ft_j.mzmax + mzWindow[1]; // right boundary adjustment
      }

      if (hasRtWindow)
      {
        rtmin = ft_j.rtmin + rtWindow[0]; // left boundary adjustment
        rtmax = ft_j.rtmax + rtWindow[1]; // right boundary adjustment
      }

      targets.index.push_back(counter);
      targets.id.push_back(ft_j.feature);
      targets.level.push_back(1);
      targets.polarity.push_back(ft_j.polarity);
      targets.precursor.push_back(false);
      targets.mz.push_back(ft_j.mz);
      targets.mzmin.push_back(mzmin);
      targets.mzmax.push_back(mzmax);
      targets.rt.push_back(ft_j.rt);
      targets.rtmin.push_back(rtmin);
      targets.rtmax.push_back(rtmax);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
      counter++;
    }

    if (targets.id.size() == 0)
      continue;

    const std::string &file_i = files[i];

    if (!std::filesystem::exists(file_i))
      continue;

    const sc::MS_SPECTRA_HEADERS &header_i = headers[i];

    sc::MS_FILE ana(file_i);
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header_i, minTracesIntensity, 0);

    for (int j = 0; j < fts_i.size(); j++)
    {
      FEATURE ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms1_size > 0 && !ft_j.ms1_mz.empty() && !ft_j.ms1_intensity.empty())
        continue;

      const sc::MS_TARGETS_SPECTRA &res_j = res[ft_j.feature];

      const auto clustered = utils::cluster_ms_targets_spectra(res_j, mzClust, presence);
      const int n_res_j = static_cast<int>(clustered.mz.size());

      if (n_res_j == 0)
        continue;

      ft_j.ms1_size = n_res_j;
      ft_j.ms1_mz = utils::encode_floats_base64(clustered.mz);
      ft_j.ms1_intensity = utils::encode_floats_base64(clustered.intensity);

      fts_i.set_feature(j, ft_j);
    }
  }
}

// MARK: load_features_ms2
void nts::NTS_DATA::load_features_ms2(
    bool filtered,
    float minTracesIntensity,
    float isolationWindow,
    float mzClust,
    float presence)
{
  for (size_t i = 0; i < features.size(); i++)
  {
    FEATURES &fts_i = features[i];

    if (fts_i.size() == 0)
      continue;

    sc::MS_TARGETS targets;
    int counter = 0;

    for (int j = 0; j < fts_i.size(); j++)
    {
      const FEATURE &ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms2_size > 0 && !ft_j.ms2_mz.empty() && !ft_j.ms2_intensity.empty())
        continue;

      targets.index.push_back(counter);
      targets.id.push_back(ft_j.feature);
      targets.level.push_back(2);
      targets.polarity.push_back(ft_j.polarity);
      targets.precursor.push_back(true);
      targets.mzmin.push_back(ft_j.mzmin - (isolationWindow / 2));
      targets.mzmax.push_back(ft_j.mzmax + (isolationWindow / 2));
      targets.rtmin.push_back(ft_j.rtmin - 1);
      targets.rtmax.push_back(ft_j.rtmax + 1);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
      counter++;
    }

    if (targets.id.size() == 0)
      continue;

    const std::string &file_i = files[i];

    if (!std::filesystem::exists(file_i))
      continue;

    const sc::MS_SPECTRA_HEADERS &header_i = headers[i];

    sc::MS_FILE ana(file_i);
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header_i, 0, minTracesIntensity);

    for (int j = 0; j < fts_i.size(); j++)
    {
      FEATURE ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms2_size > 0 && !ft_j.ms2_mz.empty() && !ft_j.ms2_intensity.empty())
        continue;

      const sc::MS_TARGETS_SPECTRA &res_j = res[ft_j.feature];

      const auto clustered = utils::cluster_ms_targets_spectra(res_j, mzClust, presence);
      const int n_res_j = static_cast<int>(clustered.mz.size());

      if (n_res_j == 0)
        continue;

      ft_j.ms2_size = n_res_j;
      ft_j.ms2_mz = utils::encode_floats_base64(clustered.mz);
      ft_j.ms2_intensity = utils::encode_floats_base64(clustered.intensity);

      fts_i.set_feature(j, ft_j);
    }
  }
}
