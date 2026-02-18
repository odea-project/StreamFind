// nts_filters.cpp
// Filtering implementations for NTS_DATA

#include "nts_filters.h"
#include "nts.h"
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include <iostream>
#include <optional>
#include <functional>

namespace nts::filter_features
{
  namespace
  {
    struct FilterParams
    {
      std::optional<float> minSN;
      std::optional<float> minIntensity;
      std::optional<float> minArea;
      std::optional<float> minWidth;
      std::optional<float> maxWidth;
      std::optional<float> maxPPM;
      std::optional<float> minFwhmRT;
      std::optional<float> maxFwhmRT;
      std::optional<float> minFwhmMZ;
      std::optional<float> maxFwhmMZ;
      std::optional<float> minGaussianA;
      std::optional<float> minGaussianMu;
      std::optional<float> maxGaussianMu;
      std::optional<float> minGaussianSigma;
      std::optional<float> maxGaussianSigma;
      std::optional<float> minGaussianR2;
      std::optional<float> maxJaggedness;
      std::optional<float> minSharpness;
      std::optional<float> minAsymmetry;
      std::optional<float> maxAsymmetry;
      std::optional<int> maxModality;
      std::optional<float> minPlates;

      std::optional<bool> onlyFilled; // TRUE: filter not filled, FALSE: filter filled
      bool removeFilled = false;

      std::optional<int> minSizeEIC;
      std::optional<int> minSizeMS1;
      std::optional<int> minSizeMS2;
      std::optional<float> minRelPresenceReplicate;

      bool removeIsotopes = false;
      bool removeAdducts = false;
      bool removeLosses = false;
    };

    bool is_nan(float v)
    {
      return std::isnan(v);
    }

    void append_filter(std::string &filter, const std::string &name)
    {
      if (filter.empty())
      {
        filter = name;
      }
      else
      {
        filter += " " + name;
      }
    }
  }

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
      bool removeLosses)
  {
    if (nts_data.analyses.empty())
      return;

    FilterParams params;
    if (!std::isnan(minSN)) params.minSN = static_cast<float>(minSN);
    if (!std::isnan(minIntensity)) params.minIntensity = static_cast<float>(minIntensity);
    if (!std::isnan(minArea)) params.minArea = static_cast<float>(minArea);
    if (!std::isnan(minWidth)) params.minWidth = static_cast<float>(minWidth);
    if (!std::isnan(maxWidth)) params.maxWidth = static_cast<float>(maxWidth);
    if (!std::isnan(maxPPM)) params.maxPPM = static_cast<float>(maxPPM);
    if (!std::isnan(minFwhmRT)) params.minFwhmRT = static_cast<float>(minFwhmRT);
    if (!std::isnan(maxFwhmRT)) params.maxFwhmRT = static_cast<float>(maxFwhmRT);
    if (!std::isnan(minFwhmMZ)) params.minFwhmMZ = static_cast<float>(minFwhmMZ);
    if (!std::isnan(maxFwhmMZ)) params.maxFwhmMZ = static_cast<float>(maxFwhmMZ);
    if (!std::isnan(minGaussianA)) params.minGaussianA = static_cast<float>(minGaussianA);
    if (!std::isnan(minGaussianMu)) params.minGaussianMu = static_cast<float>(minGaussianMu);
    if (!std::isnan(maxGaussianMu)) params.maxGaussianMu = static_cast<float>(maxGaussianMu);
    if (!std::isnan(minGaussianSigma)) params.minGaussianSigma = static_cast<float>(minGaussianSigma);
    if (!std::isnan(maxGaussianSigma)) params.maxGaussianSigma = static_cast<float>(maxGaussianSigma);
    if (!std::isnan(minGaussianR2)) params.minGaussianR2 = static_cast<float>(minGaussianR2);
    if (!std::isnan(maxJaggedness)) params.maxJaggedness = static_cast<float>(maxJaggedness);
    if (!std::isnan(minSharpness)) params.minSharpness = static_cast<float>(minSharpness);
    if (!std::isnan(minAsymmetry)) params.minAsymmetry = static_cast<float>(minAsymmetry);
    if (!std::isnan(maxAsymmetry)) params.maxAsymmetry = static_cast<float>(maxAsymmetry);
    if (hasMaxModality) params.maxModality = maxModality;
    if (!std::isnan(minPlates)) params.minPlates = static_cast<float>(minPlates);
    if (hasOnlyFilled) params.onlyFilled = onlyFilledValue;
    params.removeFilled = removeFilled;
    if (hasMinSizeEIC) params.minSizeEIC = minSizeEIC;
    if (hasMinSizeMS1) params.minSizeMS1 = minSizeMS1;
    if (hasMinSizeMS2) params.minSizeMS2 = minSizeMS2;
    if (!std::isnan(minRelPresenceReplicate)) params.minRelPresenceReplicate = static_cast<float>(minRelPresenceReplicate);
    params.removeIsotopes = removeIsotopes;
    params.removeAdducts = removeAdducts;
    params.removeLosses = removeLosses;

    // Precompute replicate mapping
    std::unordered_map<std::string, std::string> analysis_to_replicate;
    analysis_to_replicate.reserve(nts_data.analyses.size());
    for (size_t i = 0; i < nts_data.analyses.size(); ++i)
    {
      analysis_to_replicate[nts_data.analyses[i]] = nts_data.replicates[i];
    }

    // Precompute replicate counts
    std::unordered_map<std::string, int> replicate_counts;
    for (const auto &rep : nts_data.replicates)
    {
      replicate_counts[rep] += 1;
    }

    // Precompute minRelPresenceReplicate map if needed
    std::unordered_set<std::string> low_presence_keys;
    if (params.minRelPresenceReplicate.has_value())
    {
      std::unordered_map<std::string, std::unordered_set<std::string>> grp_analyses;

      for (size_t a = 0; a < nts_data.analyses.size(); ++a)
      {
        const std::string &analysis = nts_data.analyses[a];
        const std::string &replicate = nts_data.replicates[a];
        const FEATURES &fts = nts_data.features[a];

        for (int i = 0; i < fts.size(); ++i)
        {
          if (fts.filtered[i])
            continue;
          const std::string &fg = fts.feature_group[i];
          if (fg.empty())
            continue;
          std::string key = replicate + "|" + fg;
          grp_analyses[key].insert(analysis);
        }
      }

      for (const auto &kv : grp_analyses)
      {
        const std::string &key = kv.first;
        size_t sep = key.find('|');
        std::string replicate = (sep == std::string::npos) ? key : key.substr(0, sep);
        auto rc_it = replicate_counts.find(replicate);
        int n_analyses = (rc_it == replicate_counts.end()) ? 0 : rc_it->second;
        int n_features = static_cast<int>(kv.second.size());
        float rel_presence = 0.0f;
        if (n_analyses > 0 && n_features > 0)
        {
          rel_presence = static_cast<float>(n_features) / static_cast<float>(n_analyses);
        }
        if (rel_presence < params.minRelPresenceReplicate.value())
        {
          low_presence_keys.insert(key);
        }
      }
    }

    auto apply_filter = [&](const std::string &name, const std::function<bool(const FEATURES &, int, const std::string &replicate)> &predicate) {
      int updated = 0;
      for (size_t a = 0; a < nts_data.analyses.size(); ++a)
      {
        FEATURES &fts = nts_data.features[a];
        const std::string &replicate = nts_data.replicates[a];
        for (int i = 0; i < fts.size(); ++i)
        {
          if (fts.filtered[i])
            continue;
          if (predicate(fts, i, replicate))
          {
            fts.filtered[i] = true;
            append_filter(fts.filter[i], name);
            updated++;
          }
        }
      }
      if (updated > 0)
      {
        std::cout << "\u2713 Filtered " << updated << " features by " << name << std::endl;
      }
    };

    if (params.minSN.has_value())
    {
      float v = params.minSN.value();
      apply_filter("minSN", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.sn[i]) && fts.sn[i] < v;
      });
    }

    if (params.minIntensity.has_value())
    {
      float v = params.minIntensity.value();
      apply_filter("minIntensity", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.intensity[i]) && fts.intensity[i] < v;
      });
    }

    if (params.minArea.has_value())
    {
      float v = params.minArea.value();
      apply_filter("minArea", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.area[i]) && fts.area[i] < v;
      });
    }

    if (params.minWidth.has_value())
    {
      float v = params.minWidth.value();
      apply_filter("minWidth", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.width[i]) && fts.width[i] < v;
      });
    }

    if (params.maxWidth.has_value())
    {
      float v = params.maxWidth.value();
      apply_filter("maxWidth", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.width[i]) && fts.width[i] > v;
      });
    }

    if (params.maxPPM.has_value())
    {
      float v = params.maxPPM.value();
      apply_filter("maxPPM", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.ppm[i]) && fts.ppm[i] > v;
      });
    }

    if (params.minFwhmRT.has_value())
    {
      float v = params.minFwhmRT.value();
      apply_filter("minFwhmRT", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.fwhm_rt[i]) && fts.fwhm_rt[i] < v;
      });
    }

    if (params.maxFwhmRT.has_value())
    {
      float v = params.maxFwhmRT.value();
      apply_filter("maxFwhmRT", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.fwhm_rt[i]) && fts.fwhm_rt[i] > v;
      });
    }

    if (params.minFwhmMZ.has_value())
    {
      float v = params.minFwhmMZ.value();
      apply_filter("minFwhmMZ", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.fwhm_mz[i]) && fts.fwhm_mz[i] < v;
      });
    }

    if (params.maxFwhmMZ.has_value())
    {
      float v = params.maxFwhmMZ.value();
      apply_filter("maxFwhmMZ", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.fwhm_mz[i]) && fts.fwhm_mz[i] > v;
      });
    }

    if (params.minGaussianA.has_value())
    {
      float v = params.minGaussianA.value();
      apply_filter("minGaussianA", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.gaussian_A[i]) && fts.gaussian_A[i] < v;
      });
    }

    if (params.minGaussianMu.has_value())
    {
      float v = params.minGaussianMu.value();
      apply_filter("minGaussianMu", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.gaussian_mu[i]) && fts.gaussian_mu[i] < v;
      });
    }

    if (params.maxGaussianMu.has_value())
    {
      float v = params.maxGaussianMu.value();
      apply_filter("maxGaussianMu", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.gaussian_mu[i]) && fts.gaussian_mu[i] > v;
      });
    }

    if (params.minGaussianSigma.has_value())
    {
      float v = params.minGaussianSigma.value();
      apply_filter("minGaussianSigma", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.gaussian_sigma[i]) && fts.gaussian_sigma[i] < v;
      });
    }

    if (params.maxGaussianSigma.has_value())
    {
      float v = params.maxGaussianSigma.value();
      apply_filter("maxGaussianSigma", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.gaussian_sigma[i]) && fts.gaussian_sigma[i] > v;
      });
    }

    if (params.minGaussianR2.has_value())
    {
      float v = params.minGaussianR2.value();
      apply_filter("minGaussianR2", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.gaussian_r2[i]) && fts.gaussian_r2[i] < v;
      });
    }

    if (params.maxJaggedness.has_value())
    {
      float v = params.maxJaggedness.value();
      apply_filter("maxJaggedness", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.jaggedness[i]) && fts.jaggedness[i] > v;
      });
    }

    if (params.minSharpness.has_value())
    {
      float v = params.minSharpness.value();
      apply_filter("minSharpness", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.sharpness[i]) && fts.sharpness[i] < v;
      });
    }

    if (params.minAsymmetry.has_value())
    {
      float v = params.minAsymmetry.value();
      apply_filter("minAsymmetry", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.asymmetry[i]) && fts.asymmetry[i] < v;
      });
    }

    if (params.maxAsymmetry.has_value())
    {
      float v = params.maxAsymmetry.value();
      apply_filter("maxAsymmetry", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.asymmetry[i]) && fts.asymmetry[i] > v;
      });
    }

    if (params.maxModality.has_value())
    {
      int v = params.maxModality.value();
      apply_filter("maxModality", [&](const FEATURES &fts, int i, const std::string &) {
        return fts.modality[i] > v;
      });
    }

    if (params.minPlates.has_value())
    {
      float v = params.minPlates.value();
      apply_filter("minPlates", [&](const FEATURES &fts, int i, const std::string &) {
        return !is_nan(fts.plates[i]) && fts.plates[i] < v;
      });
    }

    if (params.onlyFilled.has_value())
    {
      bool onlyFilled = params.onlyFilled.value();
      if (onlyFilled)
      {
        apply_filter("onlyFilled", [&](const FEATURES &fts, int i, const std::string &) {
          return !fts.filled[i];
        });
      }
      else
      {
        apply_filter("notFilled", [&](const FEATURES &fts, int i, const std::string &) {
          return fts.filled[i];
        });
      }
    }

    if (params.removeFilled)
    {
      apply_filter("removeFilled", [&](const FEATURES &fts, int i, const std::string &) {
        return fts.filled[i];
      });
    }

    if (params.minSizeEIC.has_value())
    {
      int v = params.minSizeEIC.value();
      apply_filter("minSizeEIC", [&](const FEATURES &fts, int i, const std::string &) {
        return fts.eic_size[i] < v;
      });
    }

    if (params.minSizeMS1.has_value())
    {
      int v = params.minSizeMS1.value();
      apply_filter("minSizeMS1", [&](const FEATURES &fts, int i, const std::string &) {
        return fts.ms1_size[i] < v;
      });
    }

    if (params.minSizeMS2.has_value())
    {
      int v = params.minSizeMS2.value();
      apply_filter("minSizeMS2", [&](const FEATURES &fts, int i, const std::string &) {
        return fts.ms2_size[i] < v;
      });
    }

    if (params.minRelPresenceReplicate.has_value())
    {
      float v = params.minRelPresenceReplicate.value();
      if (v > 0.0f)
      {
        apply_filter("minRelPresenceReplicate", [&](const FEATURES &fts, int i, const std::string &replicate) {
          const std::string &fg = fts.feature_group[i];
          if (fg.empty())
            return false;
          std::string key = replicate + "|" + fg;
          return low_presence_keys.find(key) != low_presence_keys.end();
        });
      }
    }

    if (params.removeIsotopes)
    {
      apply_filter("removeIsotopes", [&](const FEATURES &fts, int i, const std::string &) {
        return fts.adduct[i].find("isotope") != std::string::npos;
      });
    }

    if (params.removeAdducts)
    {
      apply_filter("removeAdducts", [&](const FEATURES &fts, int i, const std::string &) {
        return fts.adduct[i].find("adduct") != std::string::npos;
      });
    }

    if (params.removeLosses)
    {
      apply_filter("removeLosses", [&](const FEATURES &fts, int i, const std::string &) {
        return fts.adduct[i].find("loss") != std::string::npos;
      });
    }
  }
} // namespace nts::filter_features

// MARK: filter_suspects
namespace nts::filter_suspects
{
  void filter_suspects_impl(
      NTS_DATA &nts_data,
      const std::vector<std::string> &names,
      double minScore,
      double maxErrorRT,
      double maxErrorMass,
      const std::vector<int> &idLevels,
      int minSharedFragments,
      double minCosineSimilarity)
  {
    for (size_t a = 0; a < nts_data.suspects.size(); ++a)
    {
      SUSPECTS &sus = nts_data.suspects[a];
      if (sus.size() == 0)
        continue;

      std::vector<bool> keep(sus.size(), true);

      for (size_t i = 0; i < sus.size(); ++i)
      {
        // Filter by name
        if (!names.empty())
        {
          bool name_found = false;
          for (const auto &name : names)
          {
            if (sus.name[i].find(name) != std::string::npos)
            {
              name_found = true;
              break;
            }
          }
          if (!name_found)
            keep[i] = false;
        }

        // Filter by score
        if (keep[i] && !std::isnan(minScore) && !std::isnan(sus.score[i]))
        {
          if (sus.score[i] < minScore)
            keep[i] = false;
        }

        // Filter by error_rt
        if (keep[i] && !std::isnan(maxErrorRT) && !std::isnan(sus.error_rt[i]))
        {
          if (std::abs(sus.error_rt[i]) > maxErrorRT)
            keep[i] = false;
        }

        // Filter by error_mass
        if (keep[i] && !std::isnan(maxErrorMass) && !std::isnan(sus.error_mass[i]))
        {
          if (std::abs(sus.error_mass[i]) > maxErrorMass)
            keep[i] = false;
        }

        // Filter by id_level
        if (keep[i] && !idLevels.empty())
        {
          bool level_found = false;
          for (const auto &level : idLevels)
          {
            if (sus.id_level[i] == level)
            {
              level_found = true;
              break;
            }
          }
          if (!level_found)
            keep[i] = false;
        }

        // Filter by shared_fragments
        if (keep[i] && minSharedFragments > 0)
        {
          if (sus.shared_fragments[i] < minSharedFragments)
            keep[i] = false;
        }

        // Filter by cosine_similarity
        if (keep[i] && !std::isnan(minCosineSimilarity) && !std::isnan(sus.cosine_similarity[i]))
        {
          if (sus.cosine_similarity[i] < minCosineSimilarity)
            keep[i] = false;
        }
      }

      // Create filtered suspects
      SUSPECTS filtered;
      for (size_t i = 0; i < sus.size(); ++i)
      {
        if (keep[i])
        {
          SUSPECT s;
          s.analysis = sus.analysis[i];
          s.feature = sus.feature[i];
          s.candidate_rank = sus.candidate_rank[i];
          s.name = sus.name[i];
          s.polarity = sus.polarity[i];
          s.db_mass = sus.db_mass[i];
          s.exp_mass = sus.exp_mass[i];
          s.error_mass = sus.error_mass[i];
          s.db_rt = sus.db_rt[i];
          s.exp_rt = sus.exp_rt[i];
          s.error_rt = sus.error_rt[i];
          s.intensity = sus.intensity[i];
          s.area = sus.area[i];
          s.id_level = sus.id_level[i];
          s.score = sus.score[i];
          s.shared_fragments = sus.shared_fragments[i];
          s.cosine_similarity = sus.cosine_similarity[i];
          s.formula = sus.formula[i];
          s.SMILES = sus.SMILES[i];
          s.InChI = sus.InChI[i];
          s.InChIKey = sus.InChIKey[i];
          s.xLogP = sus.xLogP[i];
          s.database_id = sus.database_id[i];
          s.db_ms2_size = sus.db_ms2_size[i];
          s.db_ms2_mz = sus.db_ms2_mz[i];
          s.db_ms2_intensity = sus.db_ms2_intensity[i];
          s.db_ms2_formula = sus.db_ms2_formula[i];
          s.exp_ms2_size = sus.exp_ms2_size[i];
          s.exp_ms2_mz = sus.exp_ms2_mz[i];
          s.exp_ms2_intensity = sus.exp_ms2_intensity[i];
          filtered.append(s);
        }
      }

      nts_data.suspects[a] = filtered;
    }
  }
} // namespace nts::filter_suspects

// MARK: filter_internal_standards
namespace nts::filter_internal_standards
{
  void filter_internal_standards_impl(
      NTS_DATA &nts_data,
      const std::vector<std::string> &names,
      double minScore,
      double maxErrorRT,
      double maxErrorMass,
      const std::vector<int> &idLevels,
      int minSharedFragments,
      double minCosineSimilarity)
  {
    for (size_t a = 0; a < nts_data.internal_standards.size(); ++a)
    {
      INTERNAL_STANDARDS &istd = nts_data.internal_standards[a];
      if (istd.size() == 0)
        continue;

      std::vector<bool> keep(istd.size(), true);

      for (size_t i = 0; i < istd.size(); ++i)
      {
        // Filter by name
        if (!names.empty())
        {
          bool name_found = false;
          for (const auto &name : names)
          {
            if (istd.name[i].find(name) != std::string::npos)
            {
              name_found = true;
              break;
            }
          }
          if (!name_found)
            keep[i] = false;
        }

        // Filter by score
        if (keep[i] && !std::isnan(minScore) && !std::isnan(istd.score[i]))
        {
          if (istd.score[i] < minScore)
            keep[i] = false;
        }

        // Filter by error_rt
        if (keep[i] && !std::isnan(maxErrorRT) && !std::isnan(istd.error_rt[i]))
        {
          if (std::abs(istd.error_rt[i]) > maxErrorRT)
            keep[i] = false;
        }

        // Filter by error_mass
        if (keep[i] && !std::isnan(maxErrorMass) && !std::isnan(istd.error_mass[i]))
        {
          if (std::abs(istd.error_mass[i]) > maxErrorMass)
            keep[i] = false;
        }

        // Filter by id_level
        if (keep[i] && !idLevels.empty())
        {
          bool level_found = false;
          for (const auto &level : idLevels)
          {
            if (istd.id_level[i] == level)
            {
              level_found = true;
              break;
            }
          }
          if (!level_found)
            keep[i] = false;
        }

        // Filter by shared_fragments
        if (keep[i] && minSharedFragments > 0)
        {
          if (istd.shared_fragments[i] < minSharedFragments)
            keep[i] = false;
        }

        // Filter by cosine_similarity
        if (keep[i] && !std::isnan(minCosineSimilarity) && !std::isnan(istd.cosine_similarity[i]))
        {
          if (istd.cosine_similarity[i] < minCosineSimilarity)
            keep[i] = false;
        }
      }

      // Create filtered internal standards
      INTERNAL_STANDARDS filtered;
      for (size_t i = 0; i < istd.size(); ++i)
      {
        if (keep[i])
        {
          INTERNAL_STANDARD is;
          is.analysis = istd.analysis[i];
          is.feature = istd.feature[i];
          is.candidate_rank = istd.candidate_rank[i];
          is.name = istd.name[i];
          is.polarity = istd.polarity[i];
          is.db_mass = istd.db_mass[i];
          is.exp_mass = istd.exp_mass[i];
          is.error_mass = istd.error_mass[i];
          is.db_rt = istd.db_rt[i];
          is.exp_rt = istd.exp_rt[i];
          is.error_rt = istd.error_rt[i];
          is.intensity = istd.intensity[i];
          is.area = istd.area[i];
          is.id_level = istd.id_level[i];
          is.score = istd.score[i];
          is.shared_fragments = istd.shared_fragments[i];
          is.cosine_similarity = istd.cosine_similarity[i];
          is.formula = istd.formula[i];
          is.SMILES = istd.SMILES[i];
          is.InChI = istd.InChI[i];
          is.InChIKey = istd.InChIKey[i];
          is.xLogP = istd.xLogP[i];
          is.database_id = istd.database_id[i];
          is.db_ms2_size = istd.db_ms2_size[i];
          is.db_ms2_mz = istd.db_ms2_mz[i];
          is.db_ms2_intensity = istd.db_ms2_intensity[i];
          is.db_ms2_formula = istd.db_ms2_formula[i];
          is.exp_ms2_size = istd.exp_ms2_size[i];
          is.exp_ms2_mz = istd.exp_ms2_mz[i];
          is.exp_ms2_intensity = istd.exp_ms2_intensity[i];
          filtered.append(is);
        }
      }

      nts_data.internal_standards[a] = filtered;
    }
  }
} // namespace nts::filter_internal_standards
