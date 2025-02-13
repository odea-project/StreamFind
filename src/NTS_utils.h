#ifndef NTS_UTILS_H
#define NTS_UTILS_H

#include <iostream>
#include <vector>
#include <string>
#include <tuple>
#include <algorithm>
#include <unordered_set>
#include <numeric>
#include <Rcpp.h>
#include <omp.h>
#include <cmath>
#include <filesystem>
#include "StreamCraft_lib.h"

namespace NTS
{

  // MARK: MS_FEATURES_MZ_SORTED
  struct MS_FEATURES_MZ_SORTED
  {
    int n;
    std::vector<std::string> feature;
    std::vector<int> index;
    std::vector<int> polarity;
    std::vector<float> rt;
    std::vector<float> rtmin;
    std::vector<float> rtmax;
    std::vector<float> mz;
    std::vector<float> mzmin;
    std::vector<float> mzmax;
    std::vector<float> intensity;

    MS_FEATURES_MZ_SORTED(const Rcpp::List &features)
    {

      const std::vector<std::string> &rf_feature = features["feature"];
      const std::vector<int> &rf_polarity = features["polarity"];
      const std::vector<float> &rf_rt = features["rt"];
      const std::vector<float> &rf_rtmin = features["rtmin"];
      const std::vector<float> &rf_rtmax = features["rtmax"];
      const std::vector<float> &rf_mz = features["mz"];
      const std::vector<float> &rf_mzmin = features["mzmin"];
      const std::vector<float> &rf_mzmax = features["mzmax"];
      const std::vector<float> &rf_intensity = features["intensity"];

      n = rf_feature.size();

      std::vector<int> rf_index(n);
      std::iota(rf_index.begin(), rf_index.end(), 0);

      feature.resize(n);
      index.resize(n);
      polarity.resize(n);
      rt.resize(n);
      rtmin.resize(n);
      rtmax.resize(n);
      mz.resize(n);
      mzmin.resize(n);
      mzmax.resize(n);
      intensity.resize(n);

      std::vector<int> idx(n);
      std::iota(idx.begin(), idx.end(), 0);
      std::stable_sort(idx.begin(), idx.end(), [&rf_mz](int i, int j)
                       { return rf_mz[i] < rf_mz[j]; });

      for (int i = 0; i < n; i++)
      {
        feature[i] = rf_feature[idx[i]];
        index[i] = rf_index[idx[i]];
        polarity[i] = rf_polarity[idx[i]];
        rt[i] = rf_rt[idx[i]];
        rtmin[i] = rf_rtmin[idx[i]];
        rtmax[i] = rf_rtmax[idx[i]];
        mz[i] = rf_mz[idx[i]];
        mzmin[i] = rf_mzmin[idx[i]];
        mzmax[i] = rf_mzmax[idx[i]];
        intensity[i] = rf_intensity[idx[i]];
      }
    };
  };

  // MARK: MS_ISOTOPE
  struct MS_ISOTOPE
  {
    std::string element;
    std::string isotope;
    float mass_distance;
    float abundance;
    float abundance_monoisotopic;
    int min;
    int max;

    MS_ISOTOPE(const std::string &element,
               const std::string &isotope,
               float mass_distance,
               float abundance,
               float abundance_monoisotopic,
               int min,
               int max) : element(element),
                          isotope(isotope),
                          mass_distance(mass_distance),
                          abundance(abundance),
                          abundance_monoisotopic(abundance_monoisotopic),
                          min(min),
                          max(max) {};
  };

  // MARK: MS_ISOTOPE_SET
  struct MS_ISOTOPE_SET
  {
    std::vector<MS_ISOTOPE> data = {
        MS_ISOTOPE("C", "13C", 1.0033548378, 0.01078, 0.988922, 1, 100),
        MS_ISOTOPE("H", "2H", 1.0062767, 0.00015574, 0.99984426, 2, 100),
        MS_ISOTOPE("N", "15N", 0.9970349, 0.003663, 0.996337, 1, 15),
        MS_ISOTOPE("O", "17O", 1.004217, 0.00037, 0.99763, 1, 15),
        MS_ISOTOPE("O", "18O", 2.004246, 0.00200, 0.99763, 1, 15),
        MS_ISOTOPE("S", "33S", 0.999388, 0.00750, 0.95018, 1, 10),
        MS_ISOTOPE("S", "34S", 1.995796, 0.04215, 0.95018, 1, 10),
        MS_ISOTOPE("S", "36S", 3.995010, 0.00017, 0.95018, 1, 10),
        MS_ISOTOPE("Cl", "37Cl", 1.997050, 0.24229, 0.75771, 1, 10),
        MS_ISOTOPE("Br", "81Br", 1.997953, 0.49314, 0.50686, 1, 10),
        MS_ISOTOPE("Si", "29Si", 0.999568, 0.04683, 0.92230, 1, 10),
        MS_ISOTOPE("Si", "30Si", 1.996844, 0.03087, 0.92230, 1, 10)
        // MS_ISOTOPE("Ge", "72Ge", 1.997828, 0.27662, 0.21234, 1, 10),
        // MS_ISOTOPE("Ge", "73Ge", 2.999212, 0.07717, 0.21234, 1, 10),
        // MS_ISOTOPE("Ge", "74Ge", 3.996930, 0.35943, 0.21234, 1, 10),
        // MS_ISOTOPE("Ge", "76Ge", 5.997155, 0.07444, 0.21234, 1, 10)
    };

    std::vector<std::string> elements()
    {
      std::vector<std::string> out;
      for (const MS_ISOTOPE &iso : data)
      {
        out.push_back(iso.element);
      }
      return out;
    };

    std::vector<std::string> isotopes()
    {
      std::vector<std::string> out;
      for (const MS_ISOTOPE &iso : data)
      {
        out.push_back(iso.isotope);
      }
      return out;
    };

    std::vector<float> mass_distance()
    {
      std::vector<float> out;
      for (const MS_ISOTOPE &iso : data)
      {
        out.push_back(iso.mass_distance);
      }
      return out;
    };

    std::vector<float> abundance()
    {
      std::vector<float> out;
      for (const MS_ISOTOPE &iso : data)
      {
        out.push_back(iso.abundance);
      }
      return out;
    };

    std::vector<float> abundance_monoisotopic()
    {
      std::vector<float> out;
      for (const MS_ISOTOPE &iso : data)
      {
        out.push_back(iso.abundance_monoisotopic);
      }
      return out;
    };

    std::vector<int> min()
    {
      std::vector<int> out;
      for (const MS_ISOTOPE &iso : data)
      {
        out.push_back(iso.min);
      }
      return out;
    };

    std::vector<int> max()
    {
      std::vector<int> out;
      for (const MS_ISOTOPE &iso : data)
      {
        out.push_back(iso.max);
      }
      return out;
    };

    void filter(const std::vector<std::string> &el)
    {
      std::unordered_set<std::string> el_set(el.begin(), el.end());
      std::vector<MS_ISOTOPE> data_filtered;
      for (const MS_ISOTOPE &iso : data)
      {
        if (el_set.find(iso.element) != el_set.end())
        {
          data_filtered.push_back(iso);
        }
      }
      data = data_filtered;
    };
  };

  // MARK: MS_ISOTOPE_COMBINATIONS
  struct MS_ISOTOPE_COMBINATIONS
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

    MS_ISOTOPE_COMBINATIONS(MS_ISOTOPE_SET isotopes, const int &max_number_elements)
    {

      std::set<std::vector<std::string>> combinations_set;

      isotopes_str = isotopes.isotopes();
      abundances = isotopes.abundance();
      abundances_monoisotopic = isotopes.abundance_monoisotopic();
      min = isotopes.min();
      max = isotopes.max();

      for (const std::string &iso : isotopes_str)
      {
        std::vector<std::string> iso_vec(1);
        iso_vec[0] = iso;
        combinations_set.insert(iso_vec);
      }

      for (int n = 1; n <= max_number_elements; n++)
      {
        std::set<std::vector<std::string>> new_combinations_set;

        for (std::vector<std::string> combination : std::vector<std::vector<std::string>>(combinations_set.begin(), combinations_set.end()))
        {

          // excludes 2H and 17O from the M+2 on due to the low contribution
          if (combination[0] == "2H" || combination[0] == "17O")
            continue;

          // excludes 15N and 33S from the M+3 on due to the low contribution
          if (n > 1 && (combination[0] == "15N" || combination[0] == "33S"))
            continue;

          // excludes 15N and 33S from the M+3 on due to the low contribution
          if (combination.size() >= 2)
            if (combination[1] == "15N" || combination[1] == "33S")
              continue;

          for (const std::string &iso : isotopes_str)
          {

            // excludes 2H and 17O from the M+2 on due to the low contribution
            if (iso == "2H" || iso == "17O")
              continue;

            // excludes 15N and 33S from the M+3 on due to the low contribution
            if (n > 1 && (iso == "15N" || iso == "33S"))
              continue;

            combination.push_back(iso);
            std::stable_sort(combination.begin(), combination.end());
            new_combinations_set.insert(combination);
          }
        }
        combinations_set.insert(new_combinations_set.begin(), new_combinations_set.end());
      }

      std::vector<std::vector<std::string>> tensor_combinations_unordered = std::vector<std::vector<std::string>>(combinations_set.begin(), combinations_set.end());

      length = tensor_combinations_unordered.size();

      const std::vector<float> &isotopes_mass_distances = isotopes.mass_distance();
      const std::vector<float> &isotopes_abundances = isotopes.abundance();

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
          ab[j] = isotopes_abundances[idx];
          mass_distances_unordered[i] = mass_distances_unordered[i] + isotopes_mass_distances[idx];
        }
        tensor_mass_distances_unordered[i] = md;
        tensor_abundances_unordered[i] = ab;
      }

      std::vector<int> order_idx(length);
      std::iota(order_idx.begin(), order_idx.end(), 0);
      std::stable_sort(order_idx.begin(), order_idx.end(), [&](int i, int j)
                       { return mass_distances_unordered[i] < mass_distances_unordered[j]; });

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
        step[i] = std::round(mass_distances[i] * 1) / 1;
      }
    };
  };

  // MARK: MS_ANNOTATION
  struct MS_ANNOTATION
  {
    std::vector<int> index;
    std::vector<std::string> feature;
    std::vector<std::string> component_feature;
    std::vector<int> iso_size;
    std::vector<int> iso_charge;
    std::vector<int> iso_step;
    std::vector<std::string> iso_cat;
    std::vector<std::string> iso_isotope;
    std::vector<float> iso_mzr;
    std::vector<float> iso_relative_intensity;
    std::vector<float> iso_theoretical_min_relative_intensity;
    std::vector<float> iso_theoretical_max_relative_intensity;
    std::vector<float> iso_mass_distance;
    std::vector<float> iso_theoretical_mass_distance;
    std::vector<float> iso_mass_distance_error;
    std::vector<float> iso_time_error;
    std::vector<float> iso_number_carbons;
    std::vector<std::string> adduct_element;
    std::vector<std::string> adduct_cat;
    std::vector<float> adduct_time_error;
    std::vector<float> adduct_mass_error;

    MS_ANNOTATION(const int &n)
    {
      index.resize(n);
      feature.resize(n);
      component_feature.resize(n);
      iso_size.resize(n);
      iso_charge.resize(n);
      iso_step.resize(n);
      iso_cat.resize(n);
      iso_isotope.resize(n);
      iso_mzr.resize(n);
      iso_relative_intensity.resize(n);
      iso_theoretical_min_relative_intensity.resize(n);
      iso_theoretical_max_relative_intensity.resize(n);
      iso_mass_distance.resize(n);
      iso_theoretical_mass_distance.resize(n);
      iso_mass_distance_error.resize(n);
      iso_time_error.resize(n);
      iso_number_carbons.resize(n);
      adduct_element.resize(n);
      adduct_cat.resize(n);
      adduct_time_error.resize(n);
      adduct_mass_error.resize(n);
    };
  };

  // MARK: MS_CANDIDATE_CHAIN
  struct MS_CANDIDATE_CHAIN
  {
    int length;
    std::vector<std::string> feature;
    std::vector<int> index;
    std::vector<float> mz;
    std::vector<float> mzmin;
    std::vector<float> mzmax;
    std::vector<float> rt;
    std::vector<float> intensity;
    std::vector<double> mzr;
    std::vector<double> mzr_left;
    std::vector<double> mzr_right;

    MS_CANDIDATE_CHAIN(const std::vector<int> &candidates,
                       const std::vector<std::string> &all_feature,
                       const std::vector<int> &all_index,
                       const std::vector<float> &all_mz,
                       const std::vector<float> &all_mzmin,
                       const std::vector<float> &all_mzmax,
                       const std::vector<float> &all_rt,
                       const std::vector<float> &all_intensity)
    {

      length = candidates.size();

      feature.resize(length);
      index.resize(length);
      mz.resize(length);
      mzmin.resize(length);
      mzmax.resize(length);
      rt.resize(length);
      intensity.resize(length);
      mzr.resize(length);
      mzr_left.resize(length);
      mzr_right.resize(length);

      for (int i = 0; i < length; i++)
        mz[i] = all_mz[candidates[i]];

      std::vector<int> idx(length);
      std::iota(idx.begin(), idx.end(), 0);
      std::stable_sort(idx.begin(), idx.end(), [&](int i, int j)
                       { return mz[i] < mz[j]; });

      for (int i = 0; i < length; i++)
      {
        feature[i] = all_feature[candidates[idx[i]]];
        index[i] = all_index[candidates[idx[i]]];
        mz[i] = all_mz[candidates[idx[i]]];
        mzmin[i] = all_mzmin[candidates[idx[i]]];
        mzmax[i] = all_mzmax[candidates[idx[i]]];
        rt[i] = all_rt[candidates[idx[i]]];
        intensity[i] = all_intensity[candidates[idx[i]]];
      }

      std::transform(mz.begin(), mz.end(), mzmin.begin(), mzr_left.begin(), std::minus<float>());
      std::transform(mzmax.begin(), mzmax.end(), mz.begin(), mzr_right.begin(), std::minus<float>());

      for (size_t z = 0; z < mz.size(); ++z)
      {
        mzr[z] = mzr_left[z];
        if (mzr[z] < mzr_right[z])
          mzr[z] = mzr_right[z];
      }
    };
  };

  // MARK: MS_ISOTOPE_CHAIN
  struct MS_ISOTOPE_CHAIN
  {
    std::vector<int> index;
    std::vector<std::string> feature;
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

    MS_ISOTOPE_CHAIN(const int &z,
                     const int &mono_index,
                     const std::string &mono_feature,
                     const float &mono_mz,
                     const float &mono_mzr,
                     const float &mono_rt)
    {

      index.resize(1);
      feature.resize(1);
      charge.resize(1);
      step.resize(1);
      mz.resize(1);
      rt.resize(1);
      mzr.resize(1);
      abundance.resize(1);
      mass_distance.resize(1);
      theoretical_mass_distance.resize(1);
      mass_distance_error.resize(1);
      time_error.resize(1);
      isotope.resize(1);
      theoretical_abundance_min.resize(1);
      theoretical_abundance_max.resize(1);

      index[0] = mono_index;
      feature[0] = mono_feature;
      charge[0] = z;
      mz[0] = mono_mz;
      rt[0] = mono_rt;
      mzr[0] = mono_mzr;
      abundance[0] = 1;
      mass_distance[0] = 0;
      theoretical_mass_distance[0] = 0;
      mass_distance_error[0] = 0;
      time_error[0] = 0;
      isotope[0] = "";
      number_carbons = 0;
      length = 1;
    };
  };

  // MARK: MS_ADDUCT
  struct MS_ADDUCT
  {
    std::string element;
    int polarity;
    std::string cat;
    int charge;
    float mass_distance;

    MS_ADDUCT(const std::string &e, const int &p, const std::string &c, const float &md, const int &z)
    {
      element = e;
      polarity = p;
      cat = c;
      charge = z;
      mass_distance = md;
    };
  };

  // MARK: MS_ADDUCT_SET
  struct MS_ADDUCT_SET
  {

    std::vector<MS_ADDUCT> neutralizers{
        MS_ADDUCT("H", 1, "[M+H]+", -1.007276, 1),
        MS_ADDUCT("H", -1, "[M-H]-", 1.007276, 1)};

    std::vector<MS_ADDUCT> all_adducts{

        // Positive Adducts
        MS_ADDUCT("Na", 1, "[M+Na]+", 22.989218, 1),
        MS_ADDUCT("K", 1, "[M+K]+", 38.963158, 1),
        MS_ADDUCT("NH4", 1, "[M+NH4]+", 18.033823, 1),
        MS_ADDUCT("CH3OH", 1, "[M+CH3OH+H]+", 33.033489, 1), // Methanol
        MS_ADDUCT("DMSO", 1, "[M+DMSO+H]+", 79.02122, 1),    // Dimethyl sulfoxide
        MS_ADDUCT("CH3CN", 1, "[M+CH3CN+H]+", 42.033823, 1), // Acetonitrile

        // Negative Adducts
        MS_ADDUCT("Cl", -1, "[M+Cl]-", 34.969402, 1),
        MS_ADDUCT("Br", -1, "[M+Br]-", 78.918885, 1),
        MS_ADDUCT("CHO2", -1, "[M+CHO2]-", 44.998201, 1),     // Formate
        MS_ADDUCT("CH3COO", -1, "[M+CH3COO]-", 59.013851, 1), // Acetate
        MS_ADDUCT("-2H+Na", -1, "[M-2H+Na]-", 20.974666, 1),
        MS_ADDUCT("-2H+K", -1, "[M-2H+K]-", 36.948606, 1)

    };

    float neutralizer(const int &pol)
    {
      if (pol == 1)
        return neutralizers[0].mass_distance;
      return neutralizers[1].mass_distance;
    };

    std::vector<MS_ADDUCT> adducts(const int &pol)
    {
      std::vector<MS_ADDUCT> out;
      if (pol == 1)
        for (const MS_ADDUCT &a : all_adducts)
          if (a.polarity == 1)
            out.push_back(a);
      if (pol == -1)
        for (const MS_ADDUCT &a : all_adducts)
          if (a.polarity == -1)
            out.push_back(a);
      return out;
    };
  };

  // MARK: FUNCTIONS

  sc::MS_SPECTRA_HEADERS get_ms_analysis_list_headers(const Rcpp::List& analysis);

  float mean(const std::vector<float> &v);

  float standard_deviation(const std::vector<float> &v, float mean_val);

  size_t find_max_index(const std::vector<float> &v);

  size_t find_min_index(const std::vector<float> &v);

  void merge_traces_within_rt(std::vector<float> &rt, std::vector<float> &mz, std::vector<float> &intensity);

  void trim_to_equal_length_around_max_position(std::vector<float> &x, const size_t max_position);

  float trapezoidal_area(const std::vector<float> &x, const std::vector<float> &intensity);

  float gaussian(const float &A, const float &mu, const float &sigma, const float &x);

  float fit_gaussian_cost_function(const std::vector<float> &x, const std::vector<float> &y, float A, float mu, float sigma);

  void fit_gaussian(const std::vector<float> &x, const std::vector<float> &y, float &A_fitted, float &mu_fitted, float &sigma_fitted);

  float calculate_gaussian_rsquared(const std::vector<float> &x, const std::vector<float> &y, float A, float mu, float sigma);

  Rcpp::List calculate_gaussian_fit(const std::string &ft, const std::vector<float> &rt, const std::vector<float> &intensity, const float &baseCut);

  std::vector<int> find_isotopic_candidates(
      const int &number_features,
      const std::vector<std::string> &features,
      const std::vector<float> &mzs,
      const std::vector<float> &rts,
      const std::vector<int> &pols,
      const int &pol,
      const std::string &feature,
      const float &mz,
      const float &mzmin,
      const float &mzmax,
      const float &rt,
      float &rtmin,
      float &rtmax,
      const float &rtWindowAlignment,
      const float &max_mz_chain);

  bool is_max_gap_reached(const int &s, const int &maxGaps, const std::vector<int> &steps);

  void annotate_isotopes(MS_ANNOTATION &af,
                         const MS_ISOTOPE_COMBINATIONS &combinations,
                         const MS_CANDIDATE_CHAIN &candidates_chain,
                         const int &maxIsotopes,
                         const int &maxCharge,
                         const int &maxGaps);

  std::vector<int> find_adduct_candidates(
      const int &number_features,
      const std::vector<float> &mzs,
      const std::vector<float> &rts,
      const std::vector<int> &pols,
      const std::vector<int> &iso_step,
      const int &pol,
      const float &mz,
      const float &mzmin,
      const float &mzmax,
      const float &rt,
      float &rtmin,
      float &rtmax,
      const float &rtWindowAlignment,
      const float &max_mz_adducts);

  void annotate_adducts(MS_ANNOTATION &af, const MS_CANDIDATE_CHAIN &candidates_chain, const int &pol);

  Rcpp::List cluster_spectra(const Rcpp::List &spectra, const float &mzClust, const float &presence);

}; // namespace NTS

#endif
