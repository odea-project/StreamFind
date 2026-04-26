// nts_blank_subtraction.cpp
// Feature blank subtraction implementations for NTS_DATA

#include "nts_blank_subtraction.h"
#include "nts.h"
#include <unordered_map>
#include <algorithm>

namespace nts::blank_subtraction
{
  void subtract_blank_impl(
      NTS_DATA &nts_data,
      float blankThreshold,
      float rtExpand,
      float mzExpand,
      float minTracesIntensity)
  {
    if (nts_data.analyses.empty())
      return;

    // Map replicate -> analysis indices for blank lookup
    std::unordered_map<std::string, std::vector<size_t>> replicate_to_indices;
    for (size_t i = 0; i < nts_data.analyses.size(); ++i)
    {
      replicate_to_indices[nts_data.replicates[i]].push_back(i);
    }

    for (size_t a = 0; a < nts_data.analyses.size(); ++a)
    {
      FEATURES &fts = nts_data.features[a];
      const int n_features = fts.size();
      if (n_features == 0)
        continue;

      const std::string &blank_rep = nts_data.blanks[a];
      if (blank_rep.empty())
        continue;

      auto blk_it = replicate_to_indices.find(blank_rep);
      if (blk_it == replicate_to_indices.end())
        continue;

      const std::vector<size_t> &blank_indices = blk_it->second;
      if (blank_indices.empty())
        continue;

      // Build targets for this analysis' features
      ms::MS_TARGETS targets;
      targets.resize_all(n_features);

      std::unordered_map<std::string, std::vector<int>> id_to_indices;
      id_to_indices.reserve(static_cast<size_t>(n_features));

      for (int i = 0; i < n_features; ++i)
      {
        targets.index[i] = i;
        targets.id[i] = fts.feature[i];
        targets.level[i] = 1;
        targets.polarity[i] = fts.polarity[i];
        targets.precursor[i] = false;
        targets.mz[i] = fts.mz[i];
        targets.mzmin[i] = fts.mzmin[i] - mzExpand;
        targets.mzmax[i] = fts.mzmax[i] + mzExpand;
        targets.rt[i] = fts.rt[i];
        targets.rtmin[i] = fts.rtmin[i] - rtExpand;
        targets.rtmax[i] = fts.rtmax[i] + rtExpand;
        targets.mobility[i] = 0.0f;
        targets.mobilitymin[i] = 0.0f;
        targets.mobilitymax[i] = 0.0f;

        id_to_indices[fts.feature[i]].push_back(i);
      }

      // Accumulate max intensities per id across blanks
      std::unordered_map<std::string, float> sum_by_id;
      std::unordered_map<std::string, int> count_by_id;
      sum_by_id.reserve(id_to_indices.size());
      count_by_id.reserve(id_to_indices.size());

      for (size_t b = 0; b < blank_indices.size(); ++b)
      {
        size_t blank_idx = blank_indices[b];
        if (blank_idx >= nts_data.files.size() || blank_idx >= nts_data.headers.size())
          continue;

        ms::MS_FILE ana(nts_data.files[blank_idx]);
        const auto &headers = nts_data.headers[blank_idx];
        ms::MS_TARGETS_SPECTRA eics = ana.get_spectra_targets(targets, headers, minTracesIntensity, 0.0f);

        std::unordered_map<std::string, float> max_by_id;
        max_by_id.reserve(id_to_indices.size());

        for (size_t i = 0; i < eics.id.size(); ++i)
        {
          const std::string &id = eics.id[i];
          float intensity = eics.intensity[i];
          auto it = max_by_id.find(id);
          if (it == max_by_id.end() || intensity > it->second)
          {
            max_by_id[id] = intensity;
          }
        }

        for (const auto &kv : max_by_id)
        {
          sum_by_id[kv.first] += kv.second;
          count_by_id[kv.first] += 1;
        }
      }

      // Apply blank subtraction filter
      for (const auto &kv : id_to_indices)
      {
        const std::string &id = kv.first;
        float blank_intensity = 0.0f;
        auto it_count = count_by_id.find(id);
        if (it_count != count_by_id.end() && it_count->second > 0)
        {
          blank_intensity = sum_by_id[id] / static_cast<float>(it_count->second);
        }

        for (int idx : kv.second)
        {
          if (fts.intensity[idx] < (blank_intensity * blankThreshold))
          {
            fts.filtered[idx] = true;
            fts.filter[idx] = "blank_subtraction";
          }
        }
      }
    }
  }
} // namespace nts::blank_subtraction
