#include "nts.h"
#include "nts_utils.h"
#include "nts_annotation.h"
#include <iomanip>
#include <algorithm>
#include <sstream>
#include <filesystem>

// MARK: create_components
void nts::NTS_DATA::create_components(const std::vector<float> &rtWindow)
{
  const float left_offset = rtWindow.size() >= 1 ? rtWindow[0] : 0.0f;
  const float right_offset = rtWindow.size() >= 2 ? rtWindow[1] : 0.0f;

  for (size_t i = 0; i < features.size(); ++i)
  {
    FEATURES &fts = features[i];
    const int n = fts.size();

    if (n == 0)
    {
      continue;
    }

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

    for (size_t c = 0; c < clusters.size(); ++c)
    {
      const float mean_rt = cluster_rt_sum[c] / static_cast<float>(cluster_counts[c]);

      std::ostringstream oss;
      oss << "FC_" << std::fixed << std::setprecision(0) << mean_rt;
      const std::string component_id = oss.str();

      for (const int idx : clusters[c])
      {
        FEATURE ft = fts.get_feature(idx);
        ft.feature_component = component_id;
        fts.set_feature(idx, ft);
      }
    }
  }
}

// MARK: merge_MS_TARGETS_SPECTRA
nts::MS_SPECTRUM nts::merge_MS_TARGETS_SPECTRA(
  const sc::MS_TARGETS_SPECTRA &spectra,
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
