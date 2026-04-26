#include "nts.h"
#include "nts_utils.h"
#include "nts_annotation.h"
#include "nts_componentization.h"
#include "nts_alignment.h"
#include <iomanip>
#include <algorithm>
#include <sstream>
#include <filesystem>
#include <cmath>

// MARK: merge_MS_TARGETS_SPECTRA
nts::MS_SPECTRUM nts::merge_MS_TARGETS_SPECTRA(
  const ms::MS_TARGETS_SPECTRA &spectra,
  const float &mzClust,
  const float &presence)
{
  MS_SPECTRUM out;

  const size_t n = spectra.mz.size();
  if (n == 0)
    return out;

  // Sort indices once so we can sweep contiguous clusters by m/z.
  std::vector<size_t> idx(n);
  std::iota(idx.begin(), idx.end(), 0);
  std::sort(idx.begin(), idx.end(), [&](size_t i, size_t j)
            { return spectra.mz[i] < spectra.mz[j]; });

  std::vector<float> sorted_mz(n);
  std::vector<float> sorted_intensity(n);
  std::vector<float> sorted_rt(spectra.rt.size());
  for (size_t k = 0; k < n; ++k)
  {
    const size_t src = idx[k];
    sorted_mz[k] = spectra.mz[src];
    sorted_intensity[k] = spectra.intensity[src];
    if (!sorted_rt.empty() && spectra.rt.size() > src)
      sorted_rt[k] = spectra.rt[src];
  }

  // Pre-compute total unique RT count for presence filtering.
  size_t total_unique_rt = 0;
  if (!sorted_rt.empty())
  {
    std::vector<float> tmp = sorted_rt;
    std::sort(tmp.begin(), tmp.end());
    total_unique_rt = static_cast<size_t>(std::unique(tmp.begin(), tmp.end()) - tmp.begin());
  }

  std::vector<float> new_mz;
  std::vector<float> new_intensity;
  new_mz.reserve(n);
  new_intensity.reserve(n);

  const float mz_tol = std::max(mzClust, 0.0f);
  const float presence_thresh = std::clamp(presence, 0.0f, 1.0f);

  size_t start = 0;
  while (start < n)
  {
    size_t end = start + 1;
    while (end < n && (sorted_mz[end] - sorted_mz[end - 1]) <= mz_tol)
      ++end;

    // Cluster range is [start, end)
    const size_t cluster_size = end - start;

    // Presence filter: require enough unique RTs inside this cluster.
    if (presence_thresh > 0.0f && total_unique_rt > 0)
    {
      std::vector<float> rt_slice;
      rt_slice.reserve(cluster_size);
      for (size_t i = start; i < end; ++i)
        rt_slice.push_back(sorted_rt[i]);
      std::sort(rt_slice.begin(), rt_slice.end());
      size_t unique_rt = static_cast<size_t>(std::unique(rt_slice.begin(), rt_slice.end()) - rt_slice.begin());

      if (static_cast<float>(unique_rt) < presence_thresh * static_cast<float>(total_unique_rt))
      {
        start = end;
        continue;
      }
    }

    float weighted_mz = 0.0f;
    float intensity_sum = 0.0f;
    float max_int = 0.0f;

    for (size_t i = start; i < end; ++i)
    {
      const float inten = sorted_intensity[i];
      weighted_mz += sorted_mz[i] * inten;
      intensity_sum += inten;
      max_int = std::max(max_int, inten);
    }

    if (intensity_sum > 0.0f)
    {
      new_mz.push_back(weighted_mz / intensity_sum);
      new_intensity.push_back(max_int);
    }

    start = end;
  }

  out.mz = std::move(new_mz);
  out.intensity = std::move(new_intensity);
  return out;
};

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

    ms::MS_TARGETS targets;
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

    const ms::MS_SPECTRA_HEADERS &header_i = headers[i];

    ms::MS_FILE ana(file_i);
    ms::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header_i, minTracesIntensity, 0);

    for (int j = 0; j < fts_i.size(); j++)
    {
      FEATURE ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms1_size > 0 && !ft_j.ms1_mz.empty() && !ft_j.ms1_intensity.empty())
        continue;

      const ms::MS_TARGETS_SPECTRA &res_j = res[ft_j.feature];

      const auto clustered = merge_MS_TARGETS_SPECTRA(res_j, mzClust, presence);
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

    ms::MS_TARGETS targets;
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

    const ms::MS_SPECTRA_HEADERS &header_i = headers[i];

    ms::MS_FILE ana(file_i);
    ms::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header_i, 0, minTracesIntensity);

    for (int j = 0; j < fts_i.size(); j++)
    {
      FEATURE ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms2_size > 0 && !ft_j.ms2_mz.empty() && !ft_j.ms2_intensity.empty())
        continue;

      const ms::MS_TARGETS_SPECTRA &res_j = res[ft_j.feature];

      const auto clustered = merge_MS_TARGETS_SPECTRA(res_j, mzClust, presence);
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
