#include "nts_annotation.h"
#include "nts.h"
#include <Rcpp.h>

namespace nts
{
  namespace annotation
  {
    // MARK: ISOTOPE_COMBINATIONS Implementation
    ISOTOPE_COMBINATIONS::ISOTOPE_COMBINATIONS(ISOTOPE_SET &isotopes, const int &max_number_elements)
    {
      std::set<std::vector<std::string>> combinations_set;

      for (const ISOTOPE &iso : isotopes.data)
      {
        isotopes_str.push_back(iso.isotope);
        abundances.push_back(iso.abundance);
        abundances_monoisotopic.push_back(iso.abundance_monoisotopic);
        min.push_back(iso.min);
        max.push_back(iso.max);
      }

      for (const std::string &iso : isotopes_str)
      {
        std::vector<std::string> iso_vec(1, iso);
        combinations_set.insert(iso_vec);
      }

      for (int n = 1; n <= max_number_elements; n++)
      {
        std::set<std::vector<std::string>> new_combinations_set;

        for (std::vector<std::string> combination : std::vector<std::vector<std::string>>(combinations_set.begin(), combinations_set.end()))
        {
          if (combination[0] == "2H" || combination[0] == "17O")
            continue;

          if (n > 1 && (combination[0] == "15N" || combination[0] == "33S"))
            continue;

          if (combination.size() >= 2)
            if (combination[1] == "15N" || combination[1] == "33S")
              continue;

          for (const std::string &iso : isotopes_str)
          {
            if (iso == "2H" || iso == "17O")
              continue;

            if (n > 1 && (iso == "15N" || iso == "33S"))
              continue;

            combination.push_back(iso);
            std::stable_sort(combination.begin(), combination.end());
            new_combinations_set.insert(combination);
          }
        }
        combinations_set.insert(new_combinations_set.begin(), new_combinations_set.end());
      }

      std::vector<std::vector<std::string>> tensor_combinations_unordered(combinations_set.begin(), combinations_set.end());
      length = tensor_combinations_unordered.size();

      std::vector<float> isotopes_mass_distances;
      for (const ISOTOPE &iso : isotopes.data)
      {
        isotopes_mass_distances.push_back(iso.mass_distance);
      }

      std::vector<std::vector<float>> tensor_mass_distances_unordered(length);
      std::vector<std::vector<float>> tensor_abundances_unordered(length);
      std::vector<float> mass_distances_unordered(length);

      for (int i = 0; i < length; ++i)
      {
        const std::vector<std::string> &combination = tensor_combinations_unordered[i];
        const int combination_length = combination.size();
        std::vector<float> md(combination_length);
        std::vector<float> ab(combination_length);
        for (int j = 0; j < combination_length; ++j)
        {
          std::string iso = combination[j];
          int idx = std::distance(isotopes_str.begin(), std::find(isotopes_str.begin(), isotopes_str.end(), iso));
          md[j] = isotopes_mass_distances[idx];
          ab[j] = abundances[idx];
          mass_distances_unordered[i] = mass_distances_unordered[i] + isotopes_mass_distances[idx];
        }
        tensor_mass_distances_unordered[i] = md;
        tensor_abundances_unordered[i] = ab;
      }

      std::vector<int> order_idx(length);
      std::iota(order_idx.begin(), order_idx.end(), 0);
      std::stable_sort(order_idx.begin(), order_idx.end(), [&](int i, int j) {
        return mass_distances_unordered[i] < mass_distances_unordered[j];
      });

      tensor_combinations.resize(length);
      tensor_mass_distances.resize(length);
      tensor_abundances.resize(length);
      mass_distances.resize(length);
      step.resize(length);

      for (int i = 0; i < length; i++)
      {
        tensor_combinations[i] = tensor_combinations_unordered[order_idx[i]];
        tensor_mass_distances[i] = tensor_mass_distances_unordered[order_idx[i]];
        tensor_abundances[i] = tensor_abundances_unordered[order_idx[i]];
        mass_distances[i] = mass_distances_unordered[order_idx[i]];
        step[i] = std::round(mass_distances[i]);
      }
    }

    // MARK: ISOTOPE_CHAIN Implementation
    ISOTOPE_CHAIN::ISOTOPE_CHAIN(const int &z, FEATURE mono_ion, float mono_mzr)
    {
      chain.resize(1);
      candidate_indices.resize(1);
      charge.resize(1);
      step.resize(1);
      mz.resize(1);
      rt.resize(1);
      mzr.resize(1);
      isotope.resize(1);
      mass_distance.resize(1);
      theoretical_mass_distance.resize(1);
      mass_distance_error.resize(1);
      time_error.resize(1);
      abundance.resize(1);
      theoretical_abundance_min.resize(1);
      theoretical_abundance_max.resize(1);

      chain[0] = mono_ion;
      candidate_indices[0] = 0;
      charge[0] = z;
      step[0] = 0;
      mz[0] = mono_ion.mz;
      rt[0] = mono_ion.rt;
      mzr[0] = mono_mzr;
      isotope[0] = "";
      mass_distance[0] = 0;
      theoretical_mass_distance[0] = 0;
      mass_distance_error[0] = 0;
      time_error[0] = 0;
      abundance[0] = 1;
      theoretical_abundance_min[0] = 0;
      theoretical_abundance_max[0] = 0;
      number_carbons = 0;
      length = 1;
    }

    // MARK: ADDUCT_SET Implementation
    float ADDUCT_SET::neutralizer(const int &pol)
    {
      if (pol == 1)
      {
        return neutralizers[0].mass_distance;
      }
      return neutralizers[1].mass_distance;
    }

    std::vector<ADDUCT> ADDUCT_SET::adducts(const int &pol)
    {
      std::vector<ADDUCT> out;

      if (pol == 1)
      {
        for (const ADDUCT &a : all_adducts)
        {
          if (a.polarity == 1)
          {
            out.push_back(a);
          }
        }
      }

      if (pol == -1)
      {
        for (const ADDUCT &a : all_adducts)
        {
          if (a.polarity == -1)
          {
            out.push_back(a);
          }
        }
      }

      return out;
    }

    // MARK: CANDIDATE_CHAIN Implementation
    void CANDIDATE_CHAIN::clear()
    {
      chain.clear();
      indices.clear();
    }

    int CANDIDATE_CHAIN::size() const
    {
      return chain.size();
    }

    void CANDIDATE_CHAIN::sort_by_mz()
    {
      if (chain.size() == 0)
        return;

      std::vector<int> new_order(chain.size());
      std::iota(new_order.begin(), new_order.end(), 0);

      std::sort(new_order.begin(), new_order.end(), [this](int i1, int i2) {
        return chain[i1].mz < chain[i2].mz;
      });

      std::vector<FEATURE> chain_sorted;
      std::vector<int> indices_sorted;

      for (size_t i = 0; i < chain.size(); i++)
      {
        chain_sorted.push_back(chain[new_order[i]]);
        indices_sorted.push_back(new_order[i]);
      }

      chain = chain_sorted;
      indices = indices_sorted;
    }

    std::vector<float> CANDIDATE_CHAIN::get_chain_mzr() const
    {
      if (chain.size() == 0)
        return std::vector<float>();

      std::vector<float> mzr(chain.size());

      for (size_t i = 0; i < chain.size(); i++)
      {
        float left = chain[i].mz - chain[i].mzmin;
        float right = chain[i].mzmax - chain[i].mz;
        mzr[i] = left;
        if (left < right)
        {
          mzr[i] = right;
        }
      }
      return mzr;
    }

    float CANDIDATE_CHAIN::get_max_mzr() const
    {
      if (chain.size() == 0)
        return 0.0;

      std::vector<float> mzr = this->get_chain_mzr();
      float max_mzr = *std::max_element(mzr.begin(), mzr.end());
      return max_mzr;
    }

    void CANDIDATE_CHAIN::find_isotopic_candidates(const FEATURE &ft,
                                                    const FEATURES &fts,
                                                    const int &ft_index,
                                                    const int &maxIsotopes,
                                                    const double &rtWindowAlignment)
    {
      const std::string &feature = ft.feature;
      const int &polarity = ft.polarity;
      const float &rt = ft.rt;
      float rtmin = ft.rtmin;
      float rtmax = ft.rtmax;
      const float &mz = ft.mz;
      const float max_mz_chain = (mz + maxIsotopes) * 1.05;
      const float left_rt = rt - rtmin;
      const float right_rt = rtmax - rt;
      float rtW = right_rt;
      if (left_rt < right_rt)
        rtW = left_rt;
      rtW = rtW * rtWindowAlignment;
      rtmin = rt - rtW;
      rtmax = rt + rtW;

      chain.push_back(ft);
      indices.push_back(ft_index);

      const int number_features = fts.size();

      for (int z = 0; z < number_features; ++z)
      {
        const bool within_time_window = fts.rt[z] >= rtmin && fts.rt[z] <= rtmax;
        const bool within_max_mz_chain = fts.mz[z] > mz && fts.mz[z] <= max_mz_chain;
        const bool same_polarity = fts.polarity[z] == polarity;
        const bool not_main_ft = fts.feature[z] != feature;

        if (within_time_window && within_max_mz_chain && same_polarity && not_main_ft)
        {
          chain.push_back(fts.get_feature(z));
          indices.push_back(z);
        }
      }
    }

    // Helper function implementation
    bool is_max_gap_reached(const int &current_step, const int &maxGaps, const std::vector<int> &steps)
    {
      if (steps.size() == 0)
        return false;

      int max_step = *std::max_element(steps.begin(), steps.end());

      if (max_step == 0)
        return false;

      int gaps = current_step - max_step - 1;

      return gaps > maxGaps;
    }

    void CANDIDATE_CHAIN::annotate_isotopes(const ISOTOPE_COMBINATIONS &combinations,
                                             const int &maxIsotopes,
                                             const int &maxCharge,
                                             const int &maxGaps)
    {
      bool is_Mplus = false;
      float mzr = this->get_max_mzr();
      const int number_candidates = chain.size();
      FEATURE mono_ion = chain[0];

      std::vector<ISOTOPE_CHAIN> isotopic_chains;
      isotopic_chains.push_back(ISOTOPE_CHAIN(1, mono_ion, mzr));

      if (maxCharge > 1)
      {
        for (int z = 2; z <= maxCharge; z++)
        {
          isotopic_chains.push_back(ISOTOPE_CHAIN(z, mono_ion, mzr));
        }
      }

      const int number_charges = isotopic_chains.size();

      for (int z = 0; z < number_charges; z++)
      {
        ISOTOPE_CHAIN iso_chain = isotopic_chains[z];
        const int charge = iso_chain.charge[0];
        const int number_steps = maxIsotopes + 1;

        for (int s = 1; s < number_steps; ++s)
        {
          if (is_max_gap_reached(s, maxGaps, iso_chain.step))
            break;

          std::vector<int> which_combinations;

          for (int c = 0; c < combinations.length; ++c)
          {
            if (combinations.step[c] == s)
              which_combinations.push_back(c);
          }

          const int number_combinations = which_combinations.size();

          std::vector<float> mass_distances(number_combinations);

          for (int c = 0; c < number_combinations; ++c)
          {
            mass_distances[c] = combinations.mass_distances[which_combinations[c]] / charge;
          }

          const float mass_distance_max = *std::max_element(mass_distances.begin(), mass_distances.end());
          const float mass_distance_min = *std::min_element(mass_distances.begin(), mass_distances.end());

          for (int candidate_idx = 1; candidate_idx < number_candidates; ++candidate_idx)
          {
            const FEATURE &candidate = chain[candidate_idx];
            const float mz = candidate.mz;
            const float rt = candidate.rt;
            const float intensity = candidate.intensity;

            float candidate_mass_distance = mz - mono_ion.mz;
            float candidate_time_error = std::abs(rt - mono_ion.rt);
            float candidate_mass_distance_min = candidate_mass_distance - mzr;
            float candidate_mass_distance_max = candidate_mass_distance + mzr;

            // M-ION Check
            if (s == 1)
            {
              if (candidate_mass_distance_min < 1.007276 &&
                  candidate_mass_distance_max > 1.007276 &&
                  (intensity / mono_ion.intensity) > 5)
              {
                is_Mplus = true;
                break;
              }
            }

            double combination_mass_error = 10;

            if (mass_distance_min - mzr < candidate_mass_distance && mass_distance_max + mzr > candidate_mass_distance)
            {
              for (int c = 0; c < number_combinations; c++)
              {
                const float candidate_mass_distance_error = std::abs(mass_distances[c] - candidate_mass_distance);
                const std::vector<std::string> &combination = combinations.tensor_combinations[which_combinations[c]];

                float min_rel_int = 1;
                float max_rel_int = 1;

                std::unordered_map<std::string, int> isotope_map;

                for (size_t e = 0; e < combination.size(); ++e)
                {
                  isotope_map[combination[e]]++;
                }

                for (const auto &pair : isotope_map)
                {
                  std::string iso = pair.first;
                  int iso_n = pair.second;

                  const int iso_idx = std::distance(
                      combinations.isotopes_str.begin(),
                      std::find(combinations.isotopes_str.begin(), combinations.isotopes_str.end(), iso));

                  const float iso_ab = combinations.abundances[iso_idx];
                  const float mono_ab = combinations.abundances_monoisotopic[iso_idx];
                  float min_el_num = combinations.min[iso_idx];
                  float max_el_num = combinations.max[iso_idx];

                  if (iso_n == 1 && iso == "13C" && s == 1)
                  {
                    iso_chain.number_carbons = intensity / (iso_ab * mono_ion.intensity);
                    min_el_num = iso_chain.number_carbons * 0.8;
                    max_el_num = iso_chain.number_carbons * 1.2;
                  }

                  if (iso == "13C" && s > 2)
                  {
                    min_el_num = iso_chain.number_carbons * 0.8;
                    max_el_num = iso_chain.number_carbons * 1.2;
                  }

                  if (iso_n == 1)
                  {
                    double min_coef = (min_el_num * std::pow(mono_ab, min_el_num - iso_n) * iso_ab) / std::pow(mono_ab, min_el_num);
                    double max_coef = (max_el_num * std::pow(mono_ab, max_el_num - iso_n) * iso_ab) / std::pow(mono_ab, max_el_num);

                    min_rel_int = min_rel_int * min_coef;
                    max_rel_int = max_rel_int * max_coef;
                  }
                  else
                  {
                    unsigned int fact = 1;
                    for (int a = 1; a <= iso_n; ++a)
                      fact *= a;

                    double min_coef = (std::pow(mono_ab, min_el_num - iso_n) * std::pow(iso_ab, iso_n)) / fact;
                    double max_coef = (std::pow(mono_ab, max_el_num - iso_n) * std::pow(iso_ab, iso_n)) / fact;

                    min_coef = min_coef / std::pow(mono_ab, min_el_num);
                    max_coef = max_coef / std::pow(mono_ab, max_el_num);

                    min_coef = min_coef * min_el_num * (min_el_num - 1);
                    max_coef = max_coef * max_el_num * (max_el_num - 1);

                    for (int t = 2; t <= iso_n - 1; ++t)
                    {
                      min_coef = min_coef * (min_el_num - t);
                      max_coef = max_coef * (max_el_num - t);
                    }

                    min_rel_int = min_rel_int * min_coef;
                    max_rel_int = max_rel_int * max_coef;
                  }
                }

                const float rel_int = intensity / mono_ion.intensity;

                if (candidate_mass_distance_error < combination_mass_error &&
                    candidate_mass_distance_error <= mzr * 1.3 &&
                    rel_int >= min_rel_int * 0.7 &&
                    rel_int <= max_rel_int * 1.3)
                {
                  combination_mass_error = candidate_mass_distance_error;

                  bool is_in_chain = false;
                  size_t is_in_chain_idx = 0;
                  for (size_t t = 1; t < iso_chain.chain.size(); ++t)
                  {
                    if (iso_chain.chain[t].feature == candidate.feature)
                    {
                      is_in_chain = true;
                      is_in_chain_idx = t;
                      break;
                    }
                  }

                  std::string concat_combination = combination[0];
                  for (size_t e = 1; e < combination.size(); ++e)
                  {
                    concat_combination += "/" + combination[e];
                  }

                  if (is_in_chain)
                  {
                    iso_chain.chain[is_in_chain_idx] = candidate;
                    iso_chain.candidate_indices[is_in_chain_idx] = candidate_idx;
                    iso_chain.charge[is_in_chain_idx] = charge;
                    iso_chain.step[is_in_chain_idx] = s;
                    iso_chain.mz[is_in_chain_idx] = mz;
                    iso_chain.rt[is_in_chain_idx] = rt;
                    iso_chain.mzr[is_in_chain_idx] = mzr;
                    iso_chain.isotope[is_in_chain_idx] = concat_combination;
                    iso_chain.mass_distance[is_in_chain_idx] = candidate_mass_distance;
                    iso_chain.theoretical_mass_distance[is_in_chain_idx] = mass_distances[c];
                    iso_chain.mass_distance_error[is_in_chain_idx] = candidate_mass_distance_error;
                    iso_chain.time_error[is_in_chain_idx] = candidate_time_error;
                    iso_chain.abundance[is_in_chain_idx] = rel_int;
                    iso_chain.theoretical_abundance_min[is_in_chain_idx] = min_rel_int;
                    iso_chain.theoretical_abundance_max[is_in_chain_idx] = max_rel_int;
                  }
                  else
                  {
                    iso_chain.chain.push_back(candidate);
                    iso_chain.candidate_indices.push_back(candidate_idx);
                    iso_chain.charge.push_back(charge);
                    iso_chain.step.push_back(s);
                    iso_chain.mz.push_back(mz);
                    iso_chain.rt.push_back(rt);
                    iso_chain.mzr.push_back(mzr);
                    iso_chain.isotope.push_back(concat_combination);
                    iso_chain.mass_distance.push_back(candidate_mass_distance);
                    iso_chain.theoretical_mass_distance.push_back(mass_distances[c]);
                    iso_chain.mass_distance_error.push_back(candidate_mass_distance_error);
                    iso_chain.time_error.push_back(candidate_time_error);
                    iso_chain.abundance.push_back(rel_int);
                    iso_chain.theoretical_abundance_min.push_back(min_rel_int);
                    iso_chain.theoretical_abundance_max.push_back(max_rel_int);
                    iso_chain.length++;
                  }
                }
              }
            }
          }

          if (is_Mplus)
            break;
        }

        if (is_Mplus)
          break;

        isotopic_chains[z] = iso_chain;
      }

      if (!is_Mplus)
      {
        int best_chain = 0;

        for (int z = 0; z < number_charges; z++)
        {
          if (isotopic_chains[z].length > isotopic_chains[best_chain].length)
          {
            best_chain = z;
          }
        }

        ISOTOPE_CHAIN &sel_iso_chain = isotopic_chains[best_chain];

        if (sel_iso_chain.length > 1)
        {
          for (size_t i = 1; i < sel_iso_chain.chain.size(); i++)
          {
            const int candidate_idx = sel_iso_chain.candidate_indices[i];
            FEATURE &temp_candidate = chain[candidate_idx];

            // Format: [MZXXX+i] where XXX is rounded mz and i is isotope step
            int main_mz_rounded = std::round(mono_ion.mz);
            temp_candidate.adduct = "[MZ" + std::to_string(main_mz_rounded) + "+" + std::to_string(sel_iso_chain.step[i]) + "]";
          }
        }
      }
    }

    void CANDIDATE_CHAIN::find_adduct_candidates(const FEATURE &ft,
                                                  const FEATURES &fts,
                                                  const int &ft_index,
                                                  const double &rtWindowAlignment)
    {
      const std::string &feature = ft.feature;
      const int &polarity = ft.polarity;
      const float &rt = ft.rt;
      float rtmin = ft.rtmin;
      float rtmax = ft.rtmax;
      const float &mz = ft.mz;
      const float max_mz_chain = mz + 100;
      const float left_rt = rt - rtmin;
      const float right_rt = rtmax - rt;
      float rtW = right_rt;
      if (left_rt < right_rt)
        rtW = left_rt;
      rtW = rtW * rtWindowAlignment;
      rtmin = rt - rtW;
      rtmax = rt + rtW;

      chain.push_back(ft);
      indices.push_back(ft_index);

      const int number_features = fts.size();

      for (int z = 0; z < number_features; ++z)
      {
        const bool within_time_window = fts.rt[z] >= rtmin && fts.rt[z] <= rtmax;
        const bool within_max_mz_chain = fts.mz[z] > mz && fts.mz[z] <= max_mz_chain;
        const bool same_polarity = fts.polarity[z] == polarity;
        const bool not_main_ft = fts.feature[z] != feature;

        if (within_time_window && within_max_mz_chain && same_polarity && not_main_ft)
        {
          chain.push_back(fts.get_feature(z));
          indices.push_back(z);
        }
      }
    }

    void CANDIDATE_CHAIN::annotate_adducts()
    {
      ADDUCT_SET all_adducts;
      const int &pol = chain[0].polarity;
      const float neutralizer = all_adducts.neutralizer(pol);
      std::vector<ADDUCT> adducts = all_adducts.adducts(pol);
      const int number_candidates = chain.size();
      const std::vector<float> &mzr = this->get_chain_mzr();
      const float &mion_mz = chain[0].mz;
      const float &mion_rt = chain[0].rt;
      const float &mion_mzr = mzr[0];

      for (size_t a = 0; a < adducts.size(); ++a)
      {
        const ADDUCT &adduct = adducts[a];
        const std::string &adduct_element = adduct.element;
        const float &adduct_mass_distance = adduct.mass_distance;

        for (int c = 1; c < number_candidates; ++c)
        {
          // Skip if already annotated
          if (chain[c].adduct != chain[0].adduct)
          {
            continue;
          }

          const float &mz = chain[c].mz;
          const float &rt = chain[c].rt;
          const float exp_mass_distance = mz - (mion_mz + neutralizer);
          const float mass_error = std::abs(exp_mass_distance - adduct_mass_distance);

          if (mass_error < mion_mzr)
          {
            // Format: [MZXXX + Element]
            int main_mz_rounded = std::round(mion_mz);
            chain[c].adduct = "[MZ" + std::to_string(main_mz_rounded) + " + " + adduct_element + "]";
            break;
          }
        }
      }
    }

    // MARK: annotate_components_impl
    void annotate_components_impl(
        nts::NTS_DATA &nts_data,
        int maxIsotopes,
        int maxCharge,
        int maxGaps)
    {
      // Build isotope combinations
      ISOTOPE_SET isotopes;
      std::vector<std::string> elements = {"C", "H", "N", "O", "S", "Cl", "Br", "Si"};
      isotopes.filter(elements);

      const int max_number_elements = 5;

      Rcpp::Rcout << "Building combinatorial isotopic chains with length " << max_number_elements << "...";
      ISOTOPE_COMBINATIONS combinations(isotopes, max_number_elements);
      Rcpp::Rcout << "Done!" << std::endl;

      const int number_analyses = nts_data.features.size();

      if (number_analyses == 0)
      {
        Rcpp::Rcout << "No analyses found for annotation!" << std::endl;
        return;
      }

      for (int a = 0; a < number_analyses; a++)
      {
        nts::FEATURES &fts = nts_data.features[a];
        const int number_features = fts.size();

        if (number_features == 0)
          continue;

        Rcpp::Rcout << "Annotating " << number_features << " features in analysis " << nts_data.analyses[a] << std::endl;

        // Group features by component
        std::unordered_map<std::string, std::vector<int>> component_groups;

        for (int f = 0; f < number_features; f++)
        {
          const nts::FEATURE &ft = fts.get_feature(f);
          if (ft.feature_component.empty())
            continue;

          component_groups[ft.feature_component].push_back(f);
        }

        Rcpp::Rcout << "  Found " << component_groups.size() << " components to annotate" << std::endl;

        // Sort features by mz within each component (required for annotation logic)
        fts.sort_by_mz();

        // Annotate isotopes for each component
        Rcpp::Rcout << "  Annotating isotopes..." << std::endl;

        for (const auto &comp_pair : component_groups)
        {
          const std::vector<int> &component_indices = comp_pair.second;

          if (component_indices.size() < 1)
            continue;

          // Process each feature in the component as a potential monoisotopic ion
          for (size_t i = 0; i < component_indices.size(); i++)
          {
            const int main_ft_idx = component_indices[i];
            nts::FEATURE main_ft = fts.get_feature(main_ft_idx);

            // Skip if already annotated as isotope
            if (main_ft.adduct.find("+") != std::string::npos && main_ft.adduct != main_ft.adduct)
              continue;

            CANDIDATE_CHAIN candidates_chain;
            // Use 1.0 for rtWindowAlignment since component is already time-grouped
            candidates_chain.find_isotopic_candidates(main_ft, fts, main_ft_idx, maxIsotopes, 1.0);

            const int number_candidates = candidates_chain.size();

            if (number_candidates > 1)
            {
              candidates_chain.annotate_isotopes(combinations, maxIsotopes, maxCharge, maxGaps);

              // Update features in the FEATURES structure
              for (size_t c = 0; c < candidates_chain.chain.size(); c++)
              {
                const int idx = candidates_chain.indices[c];
                fts.set_feature(idx, candidates_chain.chain[c]);
              }
            }
          }
        }

        Rcpp::Rcout << "  Done annotating isotopes!" << std::endl;

        // Annotate adducts for each component
        Rcpp::Rcout << "  Annotating adducts..." << std::endl;

        for (const auto &comp_pair : component_groups)
        {
          const std::vector<int> &component_indices = comp_pair.second;

          if (component_indices.size() < 1)
            continue;

          // Process each feature in the component
          for (size_t i = 0; i < component_indices.size(); i++)
          {
            const int main_ft_idx = component_indices[i];
            nts::FEATURE main_ft = fts.get_feature(main_ft_idx);

            // Skip if already annotated with adduct (contains " + ")
            if (main_ft.adduct.find(" + ") != std::string::npos)
              continue;

            CANDIDATE_CHAIN adduct_candidates_chain;
            // Use 1.0 for rtWindowAlignment since component is already time-grouped
            adduct_candidates_chain.find_adduct_candidates(main_ft, fts, main_ft_idx, 1.0);

            const int number_candidates = adduct_candidates_chain.size();

            if (number_candidates > 1)
            {
              adduct_candidates_chain.annotate_adducts();

              // Update features with adduct annotations
              for (size_t c = 1; c < adduct_candidates_chain.chain.size(); c++)
              {
                const int idx = adduct_candidates_chain.indices[c];
                fts.set_feature(idx, adduct_candidates_chain.chain[c]);
              }
            }
          }
        }

        Rcpp::Rcout << "  Done annotating adducts!" << std::endl;
      }

      Rcpp::Rcout << "Component annotation complete!" << std::endl;
    }

  } // namespace annotation
} // namespace nts
