#ifndef NTS_ANNOTATION_H
#define NTS_ANNOTATION_H

#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <algorithm>
#include <cmath>
#include <numeric>

namespace nts {
  struct FEATURE;    // Forward declaration
  struct FEATURES;   // Forward declaration
  struct NTS_DATA;   // Forward declaration
}

namespace nts
{
  namespace annotation
  {
    // MARK: ISOTOPE
    struct ISOTOPE
    {
      std::string element;
      std::string isotope;
      float mass_distance;
      float abundance;
      float abundance_monoisotopic;
      int min;
      int max;

      ISOTOPE(const std::string &e, const std::string &i, float md, float ab, float ab_mono, int mi, int ma)
          : element(e), isotope(i), mass_distance(md), abundance(ab), abundance_monoisotopic(ab_mono), min(mi), max(ma) {}
    };

    // MARK: ISOTOPE_SET
    struct ISOTOPE_SET
    {
      std::vector<ISOTOPE> data = {
          ISOTOPE("C", "13C", 1.0033548378, 0.01078, 0.988922, 1, 100),
          ISOTOPE("H", "2H", 1.0062767, 0.00015574, 0.99984426, 2, 100),
          ISOTOPE("N", "15N", 0.9970349, 0.003663, 0.996337, 1, 15),
          ISOTOPE("O", "17O", 1.004217, 0.00037, 0.99763, 1, 15),
          ISOTOPE("O", "18O", 2.004246, 0.00200, 0.99763, 1, 15),
          ISOTOPE("S", "33S", 0.999388, 0.00750, 0.95018, 1, 10),
          ISOTOPE("S", "34S", 1.995796, 0.04215, 0.95018, 1, 10),
          ISOTOPE("S", "36S", 3.995010, 0.00017, 0.95018, 1, 10),
          ISOTOPE("Cl", "37Cl", 1.997050, 0.24229, 0.75771, 1, 10),
          ISOTOPE("Br", "81Br", 1.997953, 0.49314, 0.50686, 1, 10),
          ISOTOPE("Si", "29Si", 0.999568, 0.04683, 0.92230, 1, 10),
          ISOTOPE("Si", "30Si", 1.996844, 0.03087, 0.92230, 1, 10)};

      void filter(const std::vector<std::string> &el)
      {
        std::unordered_set<std::string> el_set(el.begin(), el.end());
        std::vector<ISOTOPE> data_filtered;
        for (const ISOTOPE &iso : data)
        {
          if (el_set.find(iso.element) != el_set.end())
          {
            data_filtered.push_back(iso);
          }
        }
        data = data_filtered;
      }
    };

    // MARK: ISOTOPE_COMBINATIONS
    struct ISOTOPE_COMBINATIONS
    {
      std::vector<int> step;
      std::vector<std::string> isotopes_str;
      std::vector<float> abundances;
      std::vector<float> abundances_monoisotopic;
      std::vector<int> min;
      std::vector<int> max;
      std::vector<std::vector<std::string>> tensor_combinations;
      std::vector<std::vector<float>> tensor_mass_distances;
      std::vector<std::vector<float>> tensor_abundances;
      std::vector<float> mass_distances;
      int length;

      ISOTOPE_COMBINATIONS(ISOTOPE_SET &isotopes, const int &max_number_elements);
    };

    // MARK: ISOTOPE_CHAIN
    struct ISOTOPE_CHAIN
    {
      std::vector<FEATURE> chain;
      std::vector<int> candidate_indices;
      std::vector<int> charge;
      std::vector<int> step;
      std::vector<float> mz;
      std::vector<float> rt;
      std::vector<float> mzr;
      std::vector<std::string> isotope;
      std::vector<float> mass_distance;
      std::vector<float> theoretical_mass_distance;
      std::vector<float> mass_distance_error;
      std::vector<float> time_error;
      std::vector<float> abundance;
      std::vector<float> theoretical_abundance_min;
      std::vector<float> theoretical_abundance_max;
      float number_carbons;
      int length;

      ISOTOPE_CHAIN(const int &z, FEATURE mono_ion, float mono_mzr);
    };

    // MARK: ADDUCT
    struct ADDUCT
    {
      std::string element;
      int polarity;
      std::string cat;
      int charge;
      float mass_distance;

      ADDUCT(const std::string &e, const int &p, const std::string &c, const float &md, const int &z)
          : element(e), polarity(p), cat(c), charge(z), mass_distance(md) {}
    };

    // MARK: ADDUCT_SET
    struct ADDUCT_SET
    {
      std::vector<ADDUCT> neutralizers{
          ADDUCT("H", 1, "[M+H]+", -1.007276, 1),
          ADDUCT("H", -1, "[M-H]-", 1.007276, 1)};

      std::vector<ADDUCT> all_adducts{
          ADDUCT("Na", 1, "[M+Na]+", 22.989218, 1),
          ADDUCT("K", 1, "[M+K]+", 38.963158, 1),
          ADDUCT("NH4", 1, "[M+NH4]+", 18.033823, 1),
          ADDUCT("Cl", -1, "[M+Cl]-", 34.969402, 1),
          ADDUCT("Br", -1, "[M+Br]-", 78.918885, 1),
          ADDUCT("CHO2", -1, "[M+CHO2]-", 44.998201, 1)};

      float neutralizer(const int &pol);
      std::vector<ADDUCT> adducts(const int &pol);
    };

    // MARK: FRAGMENT_LOSS
    struct FRAGMENT_LOSS
    {
      std::string name;
      std::string formula;
      float mass_loss;
      int polarity;

      FRAGMENT_LOSS(const std::string &n, const std::string &f, float ml, int p)
          : name(n), formula(f), mass_loss(ml), polarity(p) {}
    };

    // MARK: FRAGMENT_LOSS_SET
    struct FRAGMENT_LOSS_SET
    {
      std::vector<FRAGMENT_LOSS> all_losses{
          FRAGMENT_LOSS("water", "H2O", 18.010565, 0),        // neutral, both polarities
          FRAGMENT_LOSS("carbon dioxide", "CO2", 43.989829, 0), // neutral, both polarities
          FRAGMENT_LOSS("ammonia", "NH3", 17.026549, 1),      // positive mode
          FRAGMENT_LOSS("carbon monoxide", "CO", 27.994915, 0), // neutral, both polarities
          FRAGMENT_LOSS("methyl", "CH3", 15.023475, 0),       // neutral, both polarities
          FRAGMENT_LOSS("formic acid", "CH2O2", 46.005479, -1) // negative mode
      };

      std::vector<FRAGMENT_LOSS> losses(const int &pol);
    };

    // MARK: CANDIDATE_CHAIN
    struct CANDIDATE_CHAIN
    {
      std::vector<nts::FEATURE> chain;
      std::vector<int> indices;

      void clear();
      int size() const;
      void sort_by_mz();
      std::vector<float> get_chain_mzr() const;
      float get_max_mzr() const;

      void find_isotopic_candidates(const nts::FEATURE &ft,
                                     const nts::FEATURES &fts,
                                     const int &ft_index,
                                     const int &maxIsotopes,
                                     const std::vector<int> *component_indices = nullptr,
                                     const std::unordered_set<int> *assigned_features = nullptr);

      void annotate_isotopes(const ISOTOPE_COMBINATIONS &combinations,
                              const int &maxIsotopes,
                              const int &maxCharge,
                              const int &maxGaps,
                              bool debug = false);

      void find_adduct_candidates(const nts::FEATURE &ft,
                                   const nts::FEATURES &fts,
                                   const int &ft_index,
                                   const std::vector<int> *component_indices = nullptr);

      void annotate_adducts();

      void find_fragment_candidates(const nts::FEATURE &ft,
                                     const nts::FEATURES &fts,
                                     const int &ft_index,
                                     const std::vector<int> *component_indices = nullptr);

      void annotate_fragments();
    };

    // Helper function
    bool is_max_gap_reached(const int &current_step, const int &maxGaps, const std::vector<int> &steps);

    void annotate_components_impl(
        nts::NTS_DATA &nts_data,
        int maxIsotopes,
        int maxCharge,
        int maxGaps,
        const std::string &debugComponent = "",
        const std::string &debugAnalysis = "");

  } // namespace annotation
} // namespace nts

#endif
