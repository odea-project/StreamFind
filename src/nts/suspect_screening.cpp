// suspect_screening.cpp
// Suspect screening implementations for NTS_DATA

#include "suspect_screening.h"
#include "nts.h"
#include "../streamcraft/streamcraft.h"
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <cmath>
#include <limits>

namespace nts::suspect_screening
{
  namespace
  {
    double ppm_tol(double value, double ppm)
    {
      return std::abs(value) * ppm / 1e6;
    }

    bool within_ppm(double value, double target, double ppm)
    {
      double tol = ppm_tol(target, ppm);
      return std::abs(value - target) <= tol;
    }

    bool within_sec(double value, double target, double sec)
    {
      return std::abs(value - target) <= sec;
    }

    std::string encode_floats(const std::vector<double> &input)
    {
      if (input.empty())
        return "";
      std::vector<float> tmp;
      tmp.reserve(input.size());
      for (double v : input)
        tmp.push_back(static_cast<float>(v));
      std::string enc = sc::encode_little_endian_from_float(tmp, 4);
      return sc::encode_base64(enc);
    }

    std::vector<float> decode_floats(const std::string &encoded)
    {
      if (encoded.empty())
        return {};
      std::string decoded = sc::decode_base64(encoded);
      return sc::decode_little_endian_to_float(decoded, 4);
    }

    template <typename T>
    T get_or_default(const std::vector<T> &vec, size_t idx, const T &def)
    {
      if (idx < vec.size())
        return vec[idx];
      return def;
    }
  }

  void suspect_screening_impl(
      NTS_DATA &nts_data,
      const std::vector<std::string> &analyses,
      const std::vector<SuspectQuery> &suspects,
      double ppm,
      double sec,
      double ppmMS2,
      double mzrMS2,
      double minCosineSimilarity,
      int minSharedFragments,
      bool filtered)
  {
    SUSPECTS out;
    if (suspects.empty() || nts_data.analyses.empty())
      return;

    std::unordered_set<std::string> analyses_set;
    if (!analyses.empty())
    {
      analyses_set.insert(analyses.begin(), analyses.end());
    }

    bool use_mass = false;
    for (const auto &sus : suspects)
    {
      if (sus.has_mass)
      {
        use_mass = true;
        break;
      }
    }

    struct FeatureRef
    {
      size_t analysis_idx;
      int feature_idx;
      std::string assigned_name;
    };

    std::vector<FeatureRef> matched;

    // Assign suspect names to matching features (last match wins, mimicking R logic)
    for (size_t a = 0; a < nts_data.analyses.size(); ++a)
    {
      const std::string &analysis = nts_data.analyses[a];
      if (!analyses_set.empty() && analyses_set.find(analysis) == analyses_set.end())
        continue;

      const FEATURES &fts = nts_data.features[a];
      for (int i = 0; i < fts.size(); ++i)
      {
        if (!filtered && fts.filtered[i])
          continue;

        std::string assigned;
        for (const auto &sus : suspects)
        {
          if (use_mass)
          {
            if (!sus.has_mass)
              continue;
            double expected_mz = sus.mass + (static_cast<double>(fts.polarity[i]) * 1.007276);
            if (!within_ppm(fts.mz[i], expected_mz, ppm))
              continue;
          }

          assigned = sus.name;
        }

        if (!assigned.empty())
        {
          matched.push_back({a, i, assigned});
        }
      }
    }

    if (matched.empty())
      return;

    // Build name -> suspect index (first match by grepl-like)
    auto find_suspect = [&](const std::string &feature_name) -> const SuspectQuery * {
      for (const auto &sus : suspects)
      {
        if (feature_name.find(sus.name) != std::string::npos)
          return &sus;
      }
      return nullptr;
    };

    for (const auto &ref : matched)
    {
      const SuspectQuery *sus = find_suspect(ref.assigned_name);
      if (!sus)
        continue;

      const FEATURES &fts = nts_data.features[ref.analysis_idx];
      const int i = ref.feature_idx;
      const size_t idx = static_cast<size_t>(i);

      SUSPECT row;
      row.analysis = nts_data.analyses[ref.analysis_idx];
      row.feature = get_or_default(fts.feature, idx, std::string());
      row.candidate_rank = 1;
      row.name = ref.assigned_name;
      row.polarity = get_or_default(fts.polarity, idx, 0);
      row.exp_mass = static_cast<double>(get_or_default(fts.mass, idx, 0.0f));
      row.exp_rt = static_cast<double>(get_or_default(fts.rt, idx, 0.0f));
      row.intensity = static_cast<double>(get_or_default(fts.intensity, idx, 0.0f));
      row.area = static_cast<double>(get_or_default(fts.area, idx, 0.0f));
      row.id_level = 4;
      row.shared_fragments = 0;
      row.cosine_similarity = 0.0;
      row.score = sus->score;
      row.formula = sus->formula;
      row.SMILES = sus->SMILES;
      row.InChI = sus->InChI;
      row.InChIKey = sus->InChIKey;
      row.xLogP = sus->has_xLogP ? sus->xLogP : std::numeric_limits<double>::quiet_NaN();
      row.database_id = sus->database_id;

      row.db_mass = std::numeric_limits<double>::quiet_NaN();
      if (use_mass && sus->has_mass)
      {
        row.db_mass = sus->mass;
      }

      row.error_mass = std::numeric_limits<double>::quiet_NaN();
      if (std::isfinite(row.db_mass) && std::isfinite(row.exp_mass) && row.exp_mass != 0.0)
      {
        double err = ((row.exp_mass - row.db_mass) / row.exp_mass) * 1e6;
        row.error_mass = std::round(err * 10.0) / 10.0;
      }

      row.db_rt = sus->rt;
      row.error_rt = std::numeric_limits<double>::quiet_NaN();
      bool rt_matched = false;
      if (row.db_rt > 0.0 && std::isfinite(row.exp_rt))
      {
        double err_rt = row.exp_rt - row.db_rt;
        row.error_rt = std::round(err_rt * 10.0) / 10.0;
        rt_matched = within_sec(row.exp_rt, row.db_rt, sec);
      }

      row.db_ms2_size = 0;
      row.db_ms2_mz = "";
      row.db_ms2_intensity = "";
      row.db_ms2_formula = "";
      row.exp_ms2_size = get_or_default(fts.ms2_size, idx, 0);
      row.exp_ms2_mz = get_or_default(fts.ms2_mz, idx, std::string());
      row.exp_ms2_intensity = get_or_default(fts.ms2_intensity, idx, std::string());

      const std::vector<double> *sus_mz = nullptr;
      const std::vector<double> *sus_int = nullptr;
      if (row.polarity > 0)
      {
        sus_mz = &sus->fragments_mz_pos;
        sus_int = &sus->fragments_intensity_pos;
      }
      else if (row.polarity < 0)
      {
        sus_mz = &sus->fragments_mz_neg;
        sus_int = &sus->fragments_intensity_neg;
      }

      const bool can_check_ms2 = (sus_mz && !sus_mz->empty() &&
                                  !row.exp_ms2_mz.empty() &&
                                  !row.exp_ms2_intensity.empty());

      bool ms2_matched = false;
      if (can_check_ms2)
      {
        row.db_ms2_size = static_cast<int>(sus_mz->size());
        row.db_ms2_mz = encode_floats(*sus_mz);
        row.db_ms2_intensity = encode_floats(*sus_int);
        row.db_ms2_formula = "";

        // MS2 matching
        if (!row.exp_ms2_mz.empty() && !row.exp_ms2_intensity.empty())
        {
          std::vector<float> exp_mz = decode_floats(row.exp_ms2_mz);
          std::vector<float> exp_int = decode_floats(row.exp_ms2_intensity);
          if (!exp_mz.empty() && exp_mz.size() == exp_int.size())
          {
            std::vector<int> exp_idx(sus_mz->size(), -1);
            for (size_t z = 0; z < sus_mz->size(); ++z)
            {
              double mz = (*sus_mz)[z];
              double tol = mz * ppmMS2 / 1e6;
              if (tol < mzrMS2)
                tol = mzrMS2;
              double mzmin = mz - tol;
              double mzmax = mz + tol;
              int best_idx = -1;
              double best_err = 0.0;
              for (size_t k = 0; k < exp_mz.size(); ++k)
              {
                if (exp_mz[k] < mzmin || exp_mz[k] > mzmax)
                  continue;
                double err = std::abs(exp_mz[k] - mz);
                if (best_idx == -1 || err < best_err)
                {
                  best_idx = static_cast<int>(k);
                  best_err = err;
                }
              }
              exp_idx[z] = best_idx;
            }

            int shared = 0;
            for (int idx : exp_idx)
            {
              if (idx >= 0)
                shared++;
            }

            row.shared_fragments = shared;
            double cosine = 0.0;
            if (shared > 0)
            {
              std::vector<double> intensity_db;
              std::vector<double> intensity_exp;
              intensity_db.reserve(shared);
              intensity_exp.reserve(shared);
              for (size_t z = 0; z < exp_idx.size(); ++z)
              {
                int idx = exp_idx[z];
                if (idx < 0)
                  continue;
                intensity_db.push_back(get_or_default(*sus_int, z, 0.0));
                intensity_exp.push_back(exp_int[idx]);
              }
              double max_db = *std::max_element(intensity_db.begin(), intensity_db.end());
              double max_exp = *std::max_element(intensity_exp.begin(), intensity_exp.end());
              if (max_db > 0.0 && max_exp > 0.0)
              {
                double dot = 0.0;
                double mag_db = 0.0;
                double mag_exp = 0.0;
                for (size_t k = 0; k < intensity_db.size(); ++k)
                {
                  double dbi = intensity_db[k] / max_db;
                  double exi = intensity_exp[k] / max_exp;
                  dot += dbi * exi;
                  mag_db += dbi * dbi;
                  mag_exp += exi * exi;
                }
                if (mag_db > 0.0 && mag_exp > 0.0)
                {
                  cosine = dot / (std::sqrt(mag_db) * std::sqrt(mag_exp));
                }
              }
            }

            row.cosine_similarity = std::round(cosine * 10000.0) / 10000.0;
            if (row.shared_fragments >= minSharedFragments || row.cosine_similarity >= minCosineSimilarity)
            {
              ms2_matched = true;
            }
          }
        }
      }

      // Assign ID level based on what matched
      // ID 1: mass + RT + MS2
      // ID 2: mass + MS2
      // ID 3: mass + RT
      // ID 4: mass only
      if (rt_matched && ms2_matched)
      {
        row.id_level = 1;
      }
      else if (ms2_matched)
      {
        row.id_level = 2;
      }
      else if (rt_matched)
      {
        row.id_level = 3;
      }
      else
      {
        row.id_level = 4;
      }

      out.append(row);
    }

    // Distribute suspects to the appropriate analysis in nts_data.suspects
    for (size_t i = 0; i < nts_data.analyses.size(); ++i)
    {
      nts_data.suspects[i] = SUSPECTS();
    }

    for (size_t i = 0; i < out.analysis.size(); ++i)
    {
      SUSPECT s;
      s.analysis = out.analysis[i];
      s.feature = out.feature[i];
      s.candidate_rank = out.candidate_rank[i];
      s.name = out.name[i];
      s.polarity = out.polarity[i];
      s.db_mass = out.db_mass[i];
      s.exp_mass = out.exp_mass[i];
      s.error_mass = out.error_mass[i];
      s.db_rt = out.db_rt[i];
      s.exp_rt = out.exp_rt[i];
      s.error_rt = out.error_rt[i];
      s.intensity = out.intensity[i];
      s.area = out.area[i];
      s.id_level = out.id_level[i];
      s.score = out.score[i];
      s.shared_fragments = out.shared_fragments[i];
      s.cosine_similarity = out.cosine_similarity[i];
      s.formula = out.formula[i];
      s.SMILES = out.SMILES[i];
      s.InChI = out.InChI[i];
      s.InChIKey = out.InChIKey[i];
      s.xLogP = out.xLogP[i];
      s.database_id = out.database_id[i];
      s.db_ms2_size = out.db_ms2_size[i];
      s.db_ms2_mz = out.db_ms2_mz[i];
      s.db_ms2_intensity = out.db_ms2_intensity[i];
      s.db_ms2_formula = out.db_ms2_formula[i];
      s.exp_ms2_size = out.exp_ms2_size[i];
      s.exp_ms2_mz = out.exp_ms2_mz[i];
      s.exp_ms2_intensity = out.exp_ms2_intensity[i];

      // Find which analysis index this suspect belongs to
      for (size_t a = 0; a < nts_data.analyses.size(); ++a)
      {
        if (nts_data.analyses[a] == s.analysis)
        {
          nts_data.suspects[a].append(s);
          break;
        }
      }
    }
  }
} // namespace nts::suspect_screening
