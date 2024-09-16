#include <iostream>
#include <vector>
#include <string>
#include <tuple>
#include <algorithm>
#include <unordered_set>
#include <numeric>
#include <Rcpp.h>

namespace sf {
  
  struct Isotope {
    std::string element;
    std::string isotope;
    float mass_distance;
    float abundance;
    float abundance_monoisotopic;
    int min;
    int max;
    
    Isotope(const std::string& element,
            const std::string& isotope,
            float mass_distance,
            float abundance,
            float abundance_monoisotopic,
            int min,
            int max) :
      element(element),
      isotope(isotope),
      mass_distance(mass_distance),
      abundance(abundance),
      abundance_monoisotopic(abundance_monoisotopic),
      min(min),
      max(max) {};
  };

  struct Isotopes {
    std::vector<Isotope> data = {
      Isotope("C", "13C", 1.0033548378, 0.01078, 0.988922, 1, 100),
      Isotope("H", "2H", 1.0062767, 0.00015574, 0.99984426, 2, 100),
      Isotope("N", "15N", 0.9970349, 0.003663, 0.996337, 1, 15),
      Isotope("O", "17O", 1.004217, 0.00037, 0.99763, 1, 15),
      Isotope("O", "18O", 2.004246, 0.00200, 0.99763, 1, 15),
      Isotope("S", "33S", 0.999388, 0.00750, 0.95018, 1, 10),
      Isotope("S", "34S", 1.995796, 0.04215, 0.95018, 1, 10),
      Isotope("S", "36S", 3.995010, 0.00017, 0.95018, 1, 10),
      Isotope("Cl", "37Cl", 1.997050, 0.24229, 0.75771, 1, 10),
      Isotope("Br", "81Br", 1.997953, 0.49314, 0.50686, 1, 10),
      Isotope("Si", "29Si", 0.999568, 0.04683, 0.92230, 1, 10),
      Isotope("Si", "30Si", 1.996844, 0.03087, 0.92230, 1, 10),
      Isotope("Ge", "72Ge", 1.997828, 0.27662, 0.21234, 1, 10),
      Isotope("Ge", "73Ge", 2.999212, 0.07717, 0.21234, 1, 10),
      Isotope("Ge", "74Ge", 3.996930, 0.35943, 0.21234, 1, 10),
      Isotope("Ge", "76Ge", 5.997155, 0.07444, 0.21234, 1, 10)
    };
    
    std::vector<std::string> elements() {
      std::vector<std::string> out;
      for (const Isotope& iso : data) {
        out.push_back(iso.element);
      }
      return out;
    };
    
    std::vector<std::string> isotopes() {
      std::vector<std::string> out;
      for (const Isotope& iso : data) {
        out.push_back(iso.isotope);
      }
      return out;
    };
    
    std::vector<float> mass_distance() {
      std::vector<float> out;
      for (const Isotope& iso : data) {
        out.push_back(iso.mass_distance);
      }
      return out;
    };
    
    std::vector<float> abundance() {
      std::vector<float> out;
      for (const Isotope& iso : data) {
        out.push_back(iso.abundance);
      }
      return out;
    };
    
    std::vector<float> abundance_monoisotopic() {
      std::vector<float> out;
      for (const Isotope& iso : data) {
        out.push_back(iso.abundance_monoisotopic);
      }
      return out;
    };
    
    std::vector<int> min() {
      std::vector<int> out;
      for (const Isotope& iso : data) {
        out.push_back(iso.min);
      }
      return out;
    };
    
    std::vector<int> max() {
      std::vector<int> out;
      for (const Isotope& iso : data) {
        out.push_back(iso.max);
      }
      return out;
    };
    
    void filter(const std::vector<std::string>& el) {
      std::unordered_set<std::string> el_set(el.begin(), el.end());
      std::vector<Isotope> data_filtered;
      for (const Isotope& iso : data) {
        if (el_set.find(iso.element) != el_set.end()) {
          data_filtered.push_back(iso);
        }
      }
      data = data_filtered;
    };
  };
  
  struct IsotopeCombinations {
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
    
    
    IsotopeCombinations(Isotopes isotopes, const int& max_number_elements) {
      
      std::set<std::vector<std::string>> combinations_set;
      
      isotopes_str = isotopes.isotopes();
      abundances = isotopes.abundance();
      abundances_monoisotopic = isotopes.abundance_monoisotopic();
      min = isotopes.min();
      max = isotopes.max();
      
      for (const std::string& iso : isotopes_str) {
        std::vector<std::string> iso_vec(1);
        iso_vec[0] = iso;
        combinations_set.insert(iso_vec);
      }
      
      for (int n = 1; n <= max_number_elements; n++) {
        std::set<std::vector<std::string>> new_combinations_set;
        
        for (std::vector<std::string> combination : std::vector<std::vector<std::string>>(combinations_set.begin(), combinations_set.end())) {
          
          // excludes 2H and 17O from the M+2 on due to the low contribution
          if (combination[0] == "2H" || combination[0] == "17O") continue;
          
          // excludes 15N and 33S from the M+3 on due to the low contribution
          if (n > 1 &&  (combination[0] == "15N" || combination[0] == "33S")) continue;
          
          // excludes 15N and 33S from the M+3 on due to the low contribution
          if (combination.size() >= 2) if (combination[1] == "15N" || combination[1] == "33S") continue;
          
          for (const std::string& iso : isotopes_str) {
            
            // excludes 2H and 17O from the M+2 on due to the low contribution
            if (iso == "2H" || iso == "17O") continue;
            
            // excludes 15N and 33S from the M+3 on due to the low contribution
            if (n > 1 &&  (iso == "15N" || iso == "33S")) continue;
            
            combination.push_back(iso);
            std::sort(combination.begin(), combination.end());
            new_combinations_set.insert(combination);
          }
        }
        combinations_set.insert(new_combinations_set.begin(), new_combinations_set.end());
      }
      
      std::vector<std::vector<std::string>> tensor_combinations_unordered = std::vector<std::vector<std::string>>(combinations_set.begin(), combinations_set.end());

      length = tensor_combinations_unordered.size();

      const std::vector<float>& isotopes_mass_distances = isotopes.mass_distance();
      const std::vector<float>& isotopes_abundances = isotopes.abundance();

      std::vector<std::vector<float>> tensor_mass_distances_unordered(length);
      std::vector<std::vector<float>> tensor_abundances_unordered(length);
      std::vector<float> mass_distances_unordered(length);

      for (int i = 0; i < length; ++i) {
        const std::vector<std::string>& combination = tensor_combinations_unordered[i];
        const int combination_length = combination.size();
        std::vector<float> md(combination_length);
        std::vector<float> ab(combination_length);
        for(int j = 0; j < combination_length; ++j) {
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
      std::sort(order_idx.begin(), order_idx.end(), [&](int i, int j){return mass_distances_unordered[i] < mass_distances_unordered[j];});

      tensor_combinations.resize(length);
      tensor_mass_distances.resize(length);
      tensor_abundances.resize(length);
      mass_distances.resize(length);
      step.resize(length);

      for (int i = 0; i < length; i++) {
        tensor_combinations[i] = tensor_combinations_unordered[order_idx[i]];
        tensor_mass_distances[i] = tensor_mass_distances_unordered[order_idx[i]];
        tensor_abundances[i] = tensor_abundances_unordered[order_idx[i]];
        mass_distances[i] = mass_distances_unordered[order_idx[i]];
        step[i] = std::round(mass_distances[i] * 1) / 1;
      }
    };
  };

  struct FeaturesDataFrame {
    int n;
    std::vector<std::string> feature;
    std::vector<int> index;
    std::vector<float> rt;
    std::vector<float> rtmin;
    std::vector<float> rtmax;
    std::vector<float> mz;
    std::vector<float> mzmin;
    std::vector<float> mzmax;
    std::vector<float> intensity;
    
    FeaturesDataFrame(const Rcpp::List& analysis) {
      
      const std::vector<std::string>& rf_feature = analysis["feature"];
      const std::vector<float>& rf_rt = analysis["rt"];
      const std::vector<float>& rf_rtmin = analysis["rtmin"];
      const std::vector<float>& rf_rtmax = analysis["rtmax"];
      const std::vector<float>& rf_mz = analysis["mz"];
      const std::vector<float>& rf_mzmin = analysis["mzmin"];
      const std::vector<float>& rf_mzmax = analysis["mzmax"];
      const std::vector<float>& rf_intensity = analysis["intensity"];
      
      n = rf_feature.size();
      
      std::vector<int> rf_index(n);
      std::iota(rf_index.begin(), rf_index.end(), 0);
      
      feature.resize(n);
      index.resize(n);
      rt.resize(n);
      rtmin.resize(n);
      rtmax.resize(n);
      mz.resize(n);
      mzmin.resize(n);
      mzmax.resize(n);
      intensity.resize(n);
      
      std::vector<int> idx(n);
      std::iota(idx.begin(), idx.end(), 0);
      std::sort(idx.begin(), idx.end(), [&rf_mz](int i, int j){return rf_mz[i] < rf_mz[j];});
      
      for (int i = 0; i < n; i++) {
        feature[i] = rf_feature[idx[i]];
        index[i] = rf_index[idx[i]];
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
  
  struct AnnotatedFeatures {
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
    std::vector<float> iso_number_carbons;
    std::vector<std::string> adduct_element;
    std::vector<std::string> adduct_cat;
    std::vector<float> adduct_error;
    
    AnnotatedFeatures(const int& n) {
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
      iso_number_carbons.resize(n);
      adduct_element.resize(n);
      adduct_cat.resize(n);
      adduct_error.resize(n);
    };
  };
  
  struct CandidatesChain {
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
    
    CandidatesChain(const std::vector<int>& candidates,
                    const std::vector<std::string>& all_feature,
                    const std::vector<int>& all_index,
                    const std::vector<float>& all_mz,
                    const std::vector<float>& all_mzmin,
                    const std::vector<float>& all_mzmax,
                    const std::vector<float>& all_rt,
                    const std::vector<float>& all_intensity) {
      
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
      
      const float* all_mz_ptr = all_mz.data();
      float* mz_ptr = mz.data();
      
      for (const int& x : candidates) *(mz_ptr++) = *(all_mz_ptr + x);
      
      std::vector<int> idx(length);
      std::iota(idx.begin(), idx.end(), 0);
      std::sort(idx.begin(), idx.end(), [&](int i, int j){return mz[i] < mz[j];});
      
      for (int i = 0; i < length; i++) {
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
      
      for (size_t z = 0; z < mz.size(); ++z) {
        mzr[z] = mzr_left[z];
        if (mzr[z] < mzr_right[z]) mzr[z] = mzr_right[z];
      }
    };
  };
  
  std::vector<int> find_isotopic_candidates(
      const int& number_features,
      const std::vector<float>& mzs,
      const std::vector<float>& rts,
      const float& mz,
      const float& mzmin,
      const float& mzmax,
      const float& rt,
      float& rtmin,
      float& rtmax,
      const float& rtWindowAlignment,
      const float& max_mz_chain
  ) {
    std::vector<int> candidates;
    
    const float left_rt = rt - rtmin;
    const float right_rt = rtmax - rt;
    float rtW = right_rt;
    if (left_rt < right_rt) rtW = left_rt;
    rtW = rtW * rtWindowAlignment;
    rtmin = rt - rtW;
    rtmax = rt + rtW;
    
    for (int z = 0; z < number_features; ++z) {
      if (rts[z] >= rtmin && rts[z] <= rtmax && mzs[z] >= mz && mzs[z] <= max_mz_chain) {
        candidates.push_back(z);
      }
    }
    
    return candidates;
  };
  
  struct IsotopicChain {
    std::vector<int> index;
    std::vector<std::string> feature;
    std::vector<int> charge;
    std::vector<int> step;
    std::vector<float> mz;
    std::vector<float> mzr;
    std::vector<std::string> isotope;
    std::vector<float> mass_distance;
    std::vector<float> theoretical_mass_distance;
    std::vector<float> mass_distance_error;
    std::vector<float> abundance;
    std::vector<float> theoretical_abundance_min;
    std::vector<float> theoretical_abundance_max;
    float number_carbons;
    int length;
    
    IsotopicChain(const int& z,
                  const int& mono_index,
                  const std::string& mono_feature,
                  const float& mono_mz,
                  const float& mono_mzr) {
      
      index.resize(1);
      feature.resize(1);
      charge.resize(1);
      step.resize(1);
      mz.resize(1);
      mzr.resize(1);
      abundance.resize(1);
      mass_distance.resize(1);
      theoretical_mass_distance.resize(1);
      mass_distance_error.resize(1);
      isotope.resize(1);
      theoretical_abundance_min.resize(1);
      theoretical_abundance_max.resize(1);
      
      index[0] = mono_index;
      feature[0] = mono_feature;
      charge[0] = z;
      mz[0] = mono_mz;
      mzr[0] = mono_mzr;
      abundance[0] = 1;
      mass_distance[0] = 0;
      theoretical_mass_distance[0] = 0;
      mass_distance_error[0] = 0;
      isotope[0] = "";
      number_carbons = 0;
      length = 1;
    };
  };
  
  bool is_max_gap_reached(const int& s, const int& maxGaps, const std::vector<int>& steps) {
    if (steps.size() < 2) return false;
    if (s < maxGaps) return false;
    const int steps_size = steps.size();
    const int last_step = steps[steps_size - 1];
    const int gap = s - last_step;
    if (gap > maxGaps) return true;
    return false;
  };
  
  void annotate_isotopes(AnnotatedFeatures& af,
                         const IsotopeCombinations& combinations,
                         const CandidatesChain& candidates_chain,
                         const int& maxIsotopes,
                         const int& maxCharge,
                         const int& maxGaps) {
    
    bool is_Mplus = false;
    
    double mzr = *std::max_element(candidates_chain.mzr.begin(), candidates_chain.mzr.end());
    
    const int number_candidates = candidates_chain.length;
    
    const std::string& mono_feature = candidates_chain.feature[0];
    const int& mono_index = candidates_chain.index[0];
    const float& mono_mz = candidates_chain.mz[0];
    const float& mono_intensity = candidates_chain.intensity[0];
    const float& mono_mzr = candidates_chain.mzr[0];
    
    std::vector<sf::IsotopicChain> isotopic_chains = { IsotopicChain(1, mono_index, mono_feature, mono_mz, mono_mzr) };
    
    if (maxCharge > 1) {
      for (int z = 2; z <= maxCharge; z++) {
        isotopic_chains.push_back(sf::IsotopicChain(z, mono_index, mono_feature, mono_mz, mono_mzr));
      }
    }
    
    const int number_charges = isotopic_chains.size();
    
    for (int z = 0; z < number_charges; z++) {
      
      sf::IsotopicChain& iso_chain = isotopic_chains[z];
      
      const int charge = iso_chain.charge[0];
      
      const int number_steps = maxIsotopes + 1;
      
      for (int s = 1; s < number_steps; ++s) {

        if (sf::is_max_gap_reached(s, maxGaps, iso_chain.step)) break;

        std::vector<int> which_combinations;

        for (int c = 0; c < combinations.length; ++c) if (combinations.step[c] == s) which_combinations.push_back(c);

        const int number_combinations = which_combinations.size();

        std::vector<float> mass_distances(number_combinations);

        for (int c = 0; c < number_combinations; ++c) {
          mass_distances[c] = combinations.mass_distances[which_combinations[c]];
          mass_distances[c] = mass_distances[c] / charge;
        }

        const float mass_distance_max = *std::max_element(mass_distances.begin(), mass_distances.end());
        const float mass_distance_min = *std::min_element(mass_distances.begin(), mass_distances.end());

        for (int candidate = 1; candidate < number_candidates; ++candidate) {

          const std::string& feature = candidates_chain.feature[candidate];
          const int& index = candidates_chain.index[candidate];
          const float& mz = candidates_chain.mz[candidate];
          const float& intensity = candidates_chain.intensity[candidate];
          const bool& was_annotated = af.iso_step[index] > 0;

          float candidate_mass_distance = mz - mono_mz;
          float candidate_mass_distance_min = candidate_mass_distance - mzr;
          float candidate_mass_distance_max = candidate_mass_distance + mzr;

          // M-ION Check ///////////////////////////////////////////////////////////////////////////////////////////////
          // Check for molecular ion (M) with distance 1.007276 and much higher intensity, for now set to x5
          if (s == 1) {
            
            if (candidate_mass_distance_min < 1.007276 &&
                candidate_mass_distance_max > 1.007276 &&
                (intensity / mono_intensity) > 5) {

              // TODO this will also capture 2H loss and mark it as M+
              // the mass error might give an indication to check

              af.index[mono_index] = mono_index;
              af.feature[mono_index] = feature;
              af.component_feature[mono_index] = mono_feature;
              af.iso_step[mono_index] = -1;
              af.iso_cat[mono_index] = "M+";
              af.iso_isotope[mono_index] = "";
              af.iso_charge[mono_index] = charge;
              af.iso_mzr[mono_index] = mzr;
              af.iso_mass_distance[mono_index] = candidate_mass_distance;
              af.iso_theoretical_mass_distance[mono_index] = 0;
              af.iso_mass_distance_error[mono_index] = std::abs(candidate_mass_distance - 1.007276);
              af.iso_relative_intensity[mono_index] = intensity / mono_intensity;
              af.iso_theoretical_min_relative_intensity[mono_index] = 0;
              af.iso_theoretical_max_relative_intensity[mono_index] = 0;
              af.iso_size[mono_index] = 0;
              af.iso_number_carbons[mono_index] = 0;

              is_Mplus = true;
              break;
            }
          }

          // ISOTOPE Check /////////////////////////////////////////////////////////////////////////////////////////////
          double combination_mass_error = 10; // is updated on the first hit

          // when candidate is inside of the mass distance for isotopic step mass distances
          if (mass_distance_min - mzr < candidate_mass_distance && mass_distance_max + mzr > candidate_mass_distance) {

            // selects the combinations within the mass distance, when duplicated mass distances, the first hit is the one stored
            for (int c = 0; c < number_combinations; c++) {

              const float candidate_mass_distance_error = abs(mass_distances[c] - candidate_mass_distance);
              const std::vector<std::string>& combination = combinations.tensor_combinations[which_combinations[c]];

              float min_rel_int = 1;
              float max_rel_int = 1;

              std::unordered_map<std::string, int> isotope_map;

              for (size_t e = 0; e < combination.size(); ++e) {
                std::string e_el = combination[e];
                isotope_map[e_el]++;
              }

              for (const auto& pair : isotope_map) {
                std::string iso = pair.first;
                int iso_n = pair.second;

                const int iso_idx = std::distance(combinations.isotopes_str.begin(), std::find(combinations.isotopes_str.begin(), combinations.isotopes_str.end(), iso));

                const float iso_ab = combinations.abundances[iso_idx];
                const float mono_ab = combinations.abundances_monoisotopic[iso_idx];
                float min_el_num = combinations.min[iso_idx];
                float max_el_num = combinations.max[iso_idx];

                // narrows the range for n carbons based on estimation
                if (iso_n == 1 && iso == "13C" && s == 1) {
                  iso_chain.number_carbons = intensity/(iso_ab * mono_intensity);
                  min_el_num = iso_chain.number_carbons * 0.8;
                  max_el_num = iso_chain.number_carbons * 1.2;
                }
                
                if (iso == "13C" && s > 2) {
                  min_el_num = iso_chain.number_carbons * 0.8;
                  max_el_num = iso_chain.number_carbons * 1.2;
                }

                // when only one isotope atom, mostly for M+1 or M+2 in case of Cl, Br, Si and S
                if (iso_n == 1) {
                  double min_coef = (min_el_num * std::pow(mono_ab, min_el_num - iso_n) * iso_ab) / std::pow(mono_ab, min_el_num);
                  double max_coef = (max_el_num * std::pow(mono_ab, max_el_num - iso_n) * iso_ab) / std::pow(mono_ab, max_el_num);

                  min_rel_int = min_rel_int * min_coef;
                  max_rel_int = max_rel_int * max_coef;

                  // when second time isotope, mostly for M+n, with n > 1
                } else {

                  unsigned int fact = 1;
                  for (int a = 1; a <= iso_n; ++a) fact *= a;

                  double min_coef = (std::pow(mono_ab, min_el_num - iso_n) * std::pow(iso_ab, iso_n)) / fact;
                  double max_coef = (std::pow(mono_ab, max_el_num - iso_n) * std::pow(iso_ab, iso_n)) / fact;

                  min_coef = min_coef / std::pow(mono_ab, min_el_num);
                  max_coef = max_coef / std::pow(mono_ab, max_el_num);

                  min_coef = min_coef * min_el_num * (min_el_num - 1);
                  max_coef = max_coef * max_el_num * (max_el_num - 1);

                  for (int t = 2; t <= iso_n - 1; ++t) {
                    min_coef = min_coef * (min_el_num - t);
                    max_coef = max_coef * (max_el_num - t);
                  }

                  min_rel_int = min_rel_int * min_coef;
                  max_rel_int = max_rel_int * max_coef;
                }

              } // loop for each unique element in hit md

              const float rel_int = intensity / mono_intensity;

              // selection criteria for best isotope combination for candidate and updates isotopic chain
              // if next combination or candidate is better than previous replaces it
              if (candidate_mass_distance_error < combination_mass_error &&
                  candidate_mass_distance_error <= mzr * 1.3 &&
                  rel_int >= min_rel_int * 0.7 &&
                  rel_int <= max_rel_int * 1.3) {
                
                if (was_annotated) if (af.iso_mass_distance_error[index] <= candidate_mass_distance_error) continue;

                combination_mass_error = candidate_mass_distance_error;

                bool is_in_chain = false;

                for (size_t t = 1; t < iso_chain.feature.size(); ++t) if (iso_chain.feature[t] == feature) is_in_chain = true;

                if (is_in_chain) {
                  const int i = std::distance(iso_chain.feature.begin(), std::find(iso_chain.feature.begin(), iso_chain.feature.end(), feature));
                  std::string concat_combination = combination[0];
                  for (size_t e = 1; e < combination.size(); ++e) concat_combination += "/" + combination[e];
                  iso_chain.feature[i] = feature;
                  iso_chain.index[i] = index;
                  iso_chain.step[i] = i;
                  iso_chain.mz[i] = mz;
                  iso_chain.mzr[i] = mzr;
                  iso_chain.isotope[i] = concat_combination;
                  iso_chain.mass_distance[i] = candidate_mass_distance;
                  iso_chain.theoretical_mass_distance[i] = mass_distances[c];
                  iso_chain.mass_distance_error[i] = candidate_mass_distance_error;
                  iso_chain.abundance[i] = rel_int;
                  iso_chain.theoretical_abundance_min[i] = min_rel_int;
                  iso_chain.theoretical_abundance_max[i] = max_rel_int;

                } else {
                  std::string concat_combination = combination[0];
                  for (size_t e = 1; e < combination.size(); ++e) concat_combination += "/" + combination[e];
                  iso_chain.feature.push_back(feature);
                  iso_chain.index.push_back(index);
                  iso_chain.step.push_back(s);
                  iso_chain.charge.push_back(charge);
                  iso_chain.mz.push_back(mz);
                  iso_chain.mzr.push_back(mzr);
                  iso_chain.isotope.push_back(concat_combination);
                  iso_chain.mass_distance.push_back(candidate_mass_distance);
                  iso_chain.theoretical_mass_distance.push_back(mass_distances[c]);
                  iso_chain.mass_distance_error.push_back(candidate_mass_distance_error);
                  iso_chain.abundance.push_back(rel_int);
                  iso_chain.theoretical_abundance_min.push_back(min_rel_int);
                  iso_chain.theoretical_abundance_max.push_back(max_rel_int);
                  iso_chain.length++;
                }
              }
            } // c loop for each combination within the mass distance
          } // if candidate is within the mass distance
        } // candidate loop

        if (is_Mplus) break;

      } // isotopic step loop
      
      if (is_Mplus) break;
      
    } // charge loop
    
    if (!is_Mplus) {
      
      int best_chain = 0;
      
      for (int z = 0; z < number_charges; z++) if (isotopic_chains[z].length > isotopic_chains[best_chain].length) best_chain = z;
      
      IsotopicChain& iso_chain = isotopic_chains[best_chain];
      
      af.index[mono_index] = mono_index;
      af.feature[mono_index] = mono_feature;
      af.component_feature[mono_index] = mono_feature;
      af.iso_step[mono_index] = 0;
      af.iso_cat[mono_index] = "M+0";
      af.iso_isotope[mono_index] = "";
      af.iso_charge[mono_index] = iso_chain.charge[0];
      af.iso_mzr[mono_index] = iso_chain.mzr[0];
      af.iso_mass_distance[mono_index] = 0;
      af.iso_theoretical_mass_distance[mono_index] = 0;
      af.iso_mass_distance_error[mono_index] = 0;
      af.iso_relative_intensity[mono_index] = 1;
      af.iso_theoretical_min_relative_intensity[mono_index] = 0;
      af.iso_theoretical_max_relative_intensity[mono_index] = 0;
      af.iso_size[mono_index] = iso_chain.length;
      af.iso_number_carbons[mono_index] = iso_chain.number_carbons;
      
      if (iso_chain.length > 1) {
        for (int i = 1; i < iso_chain.length; i++) {
          const int iso_index = iso_chain.index[i];
          af.index[iso_index] = iso_index;
          af.feature[iso_index] = iso_chain.feature[i];
          af.component_feature[iso_index] = mono_feature;
          af.iso_step[iso_index] = iso_chain.step[i];
          af.iso_cat[iso_index] = "M+" + std::to_string(iso_chain.step[i]);
          af.iso_charge[iso_index] = iso_chain.charge[i];
          af.iso_mzr[iso_index] = iso_chain.mzr[i];
          af.iso_mass_distance[iso_index] = iso_chain.mass_distance[i];
          af.iso_theoretical_mass_distance[iso_index] = iso_chain.theoretical_mass_distance[i];
          af.iso_mass_distance_error[iso_index] = iso_chain.mass_distance_error[i];
          af.iso_relative_intensity[iso_index] = iso_chain.abundance[i];
          af.iso_theoretical_min_relative_intensity[iso_index] = iso_chain.theoretical_abundance_min[i];
          af.iso_theoretical_max_relative_intensity[iso_index] = iso_chain.theoretical_abundance_max[i];
          af.iso_size[iso_index] = iso_chain.length;
          af.iso_number_carbons[iso_index] = iso_chain.number_carbons;
          af.iso_isotope[iso_index] = iso_chain.isotope[i];
          
          af.iso_isotope[mono_index] += " " + iso_chain.isotope[i];
        }
      }
    }
  };
  
}; // namespace sf

// [[Rcpp::export]]
Rcpp::List rcpp_ms_annotation_isotopes_V2(
  Rcpp::List features,
  int maxIsotopes = 5,
  std::string mode = "small molecules",
  int maxCharge = 1,
  double rtWindowAlignment = 0.3,
  int maxGaps = 1,
  double maxCarbons = 80,
  double maxHetero = 15,
  double maxHalogens = 10,
  bool verbose = false
) {

  Rcpp::List list_out;
  
  sf::Isotopes isotopes;
  
  std::vector<std::string> elements = {"C","H", "N", "O", "S", "Cl", "Br"};
  
  isotopes.filter(elements);
  
  const int max_number_elements = 5;
  
  Rcpp::Rcout << "Building combinatorial isotopic chains with length "<< max_number_elements << "...";
  sf::IsotopeCombinations combinations(isotopes, max_number_elements);
  Rcpp::Rcout << "Done!" << std::endl;
  
  const int number_analyses = features.size();
  
  if (number_analyses == 0) return features;
  
  for (int a = 0; a < number_analyses; a++) {
    
    const Rcpp::List& analysis = features[a];
    
    const sf::FeaturesDataFrame fdf(analysis);
    
    const std::vector<std::string> fts = analysis["feature"];
    const int n2 = fts.size();
    Rcpp::Rcout << "n2: " << n2 << std::endl;
    
    const int number_features = fdf.n;
    
    sf::AnnotatedFeatures af(number_features);
    
    Rcpp::Rcout << "Annotating isotopes in " << number_features << " features...";
    
    for (int f = 0; f < number_features; f++) {
      
      const int& index = fdf.index[f];
      
      if (af.iso_step[index] > 0) continue; // already isotope
      
      const std::string& feature = fdf.feature[f];
      const float& rt = fdf.rt[f];
      float rtmin = fdf.rtmin[f];
      float rtmax = fdf.rtmax[f];
      const float& mz = fdf.mz[f];
      const float& mzmin = fdf.mzmin[f];
      const float& mzmax = fdf.mzmax[f];
      const float max_mz_chain = (mz + maxIsotopes) * 1.05;
      
      std::vector<int> candidates = sf::find_isotopic_candidates(
        number_features, fdf.mz, fdf.rt, mz, mzmin, mzmax, rt, rtmin, rtmax, rtWindowAlignment, max_mz_chain);
      
      const int number_candidates = candidates.size();
      
      if (number_candidates > 1) {
        sf::CandidatesChain candidates_chain(candidates, fdf.feature, fdf.index, fdf.mz, fdf.mzmin, fdf.mzmax, fdf.rt, fdf.intensity);
        annotate_isotopes(af, combinations, candidates_chain, maxIsotopes, maxCharge, maxGaps);
        
      } else {
        af.index[index] = index;
        af.feature[index] = feature;
        af.component_feature[index] = feature;
        af.iso_step[index] = 0;
        af.iso_cat[index] = "M+0";
        af.iso_isotope[index] = "";
        af.iso_charge[index] = 1;
        af.iso_mzr[index] = 0;
        af.iso_mass_distance[index] = 0;
        af.iso_theoretical_mass_distance[index] = 0;
        af.iso_mass_distance_error[index] = 0;
        af.iso_relative_intensity[index] = 1;
        af.iso_theoretical_min_relative_intensity[index] = 0;
        af.iso_theoretical_max_relative_intensity[index] = 0;
        af.iso_number_carbons[index] = 0;
        af.iso_size[index] = 0;
      }
    }
    
    Rcpp::Rcout << "Done!" << std::endl;
    
    list_out["index"] = af.index;
    list_out["feature"] = af.feature;
    list_out["mz"] = fdf.mz;
    list_out["rt"] = fdf.rt;
    list_out["intensity"] = fdf.intensity;
    list_out["component_feature"] = af.component_feature;
    list_out["iso_size"] = af.iso_size;
    list_out["iso_charge"] = af.iso_charge;
    list_out["iso_step"] = af.iso_step;
    list_out["iso_cat"] = af.iso_cat;
    list_out["iso_isotope"] = af.iso_isotope;
    list_out["iso_mzr"] = af.iso_mzr;
    list_out["iso_relative_intensity"] = af.iso_relative_intensity;
    list_out["iso_theoretical_min_relative_intensity"] = af.iso_theoretical_min_relative_intensity;
    list_out["iso_theoretical_max_relative_intensity"] = af.iso_theoretical_max_relative_intensity;
    list_out["iso_mass_distance"] = af.iso_mass_distance;
    list_out["iso_theoretical_mass_distance"] = af.iso_theoretical_mass_distance;
    list_out["iso_error"] = af.iso_mass_distance_error;
    list_out["iso_number_carbons"] = af.iso_number_carbons;
    list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    
  }
  
  return list_out;
};

