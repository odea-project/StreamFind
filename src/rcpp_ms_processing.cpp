#include <vector>
#include <string>
#include <Rcpp.h>
#include <omp.h>
#include <unordered_map>
#include <cmath>
#include <algorithm>
#include <filesystem>
#include "StreamCraft_lib.h"
#include "cpp_r_obj_exchange.h"
#include "NTS_utils.h"

Rcpp::List cluster_spectra(const Rcpp::List& spectra, const float& mzClust = 0.005, const float& presence = 0.8) {
  
  const std::vector<std::string>& names_spectra = spectra.names();
  
  const std::vector<std::string> must_have_names = { "polarity", "level", "rt", "mz", "intensity" };
  
  const int n_must_have_names = must_have_names.size();
  
  std::vector<bool> has_must_have_names(n_must_have_names, false);
  
  bool has_pre_ce = false;
  bool has_pre_mz = false;
  
  for (size_t i = 0; i < must_have_names.size(); ++i) {
    for (size_t j = 0; j < names_spectra.size(); ++j) {
      if (must_have_names[i] == names_spectra[j]) has_must_have_names[i] = true;
      if (names_spectra[j] == "pre_ce") has_pre_ce = true;
      if (names_spectra[j] == "pre_mz") has_pre_mz = true;
    }
  }
  
  for (bool value : has_must_have_names) {
    if (!value) {
      throw std::runtime_error("The spectra must have the columns polarity, level, rt, pre_mz, mz and intensity!");
    }
  }
  
  const std::vector<int>& org_polarity = spectra["polarity"];
  const std::vector<int>& org_level = spectra["level"];
  const std::vector<float>& org_rt = spectra["rt"];
  const std::vector<float>& org_mz = spectra["mz"];
  const std::vector<float>& org_intensity = spectra["intensity"];
  
  const int n_traces = org_polarity.size();
  
  std::vector<float> org_pre_ce(n_traces);
  
  if (has_pre_ce) {
    const std::vector<float>& org_pre_ce_origin = spectra["pre_ce"];
    for (int i=0; i<n_traces; ++i) {
      org_pre_ce[i] = org_pre_ce_origin[i];
    }
  }
  
  std::vector<float> org_pre_mz(n_traces);
  
  if (has_pre_mz) {
    const std::vector<float>& org_pre_mz_origin = spectra["pre_mz"];
    for (int i=0; i<n_traces; ++i) {
      org_pre_mz[i] = org_pre_mz_origin[i];
    }
  }
  
  std::vector<int> idx(n_traces);
  std::iota(idx.begin(), idx.end(), 0);
  
  std::sort(idx.begin(), idx.end(), [&](int i, int j){return org_mz[i] < org_mz[j];});
  
  std::vector<float> rt(n_traces);
  float* rt_ptr = rt.data();
  
  std::vector<float> mz(n_traces);
  float* mz_ptr = mz.data();
  
  std::vector<float> intensity(n_traces);
  float* intensity_ptr = intensity.data();
  
  std::vector<float> pre_ce(n_traces);
  float* pre_ce_ptr = pre_ce.data();
  
  std::vector<float> pre_mz(n_traces);
  float* pre_mz_ptr = pre_mz.data();
  
  for (const int& x : idx) {
    *(rt_ptr++) = org_rt[x];
    *(mz_ptr++) = org_mz[x];
    *(intensity_ptr++) = org_intensity[x];
    *(pre_ce_ptr++) = org_pre_ce[x];
    *(pre_mz_ptr++) = org_pre_mz[x];
  }
  
  std::set<float> unique_rt_set;
  
  for (const float& r : rt) {
    unique_rt_set.insert(r);
  }
  
  std::vector<float> unique_rt(unique_rt_set.begin(), unique_rt_set.end());
  
  std::set<float> unique_pre_ce_set;
  
  for (const float& c : pre_ce) {
    unique_pre_ce_set.insert(c);
  }
  
  std::vector<float> unique_pre_ce(unique_pre_ce_set.begin(), unique_pre_ce_set.end());
  
  rt_ptr = rt.data();
  pre_ce_ptr = pre_ce.data();
  mz_ptr = mz.data();
  intensity_ptr = intensity.data();
  
  std::vector<float> mz_diff(n_traces - 1);
  
  for (int j = 1; j < n_traces; ++j) {
    mz_diff[j - 1] = mz[j] - mz[j - 1];
  }
  
  float itMzClust = mzClust;
  
  int counter = 0;
  
  bool hasFromSameScan = true;
  
  Rcpp::List out;
  
  while (hasFromSameScan) {
    
    counter = counter + 1;
    
    std::vector<float> new_mz;
    
    std::vector<float> new_intensity;
    
    std::vector<int> all_clusters(mz_diff.size(), 0);
    
    for (size_t j = 0; j < mz_diff.size(); ++j) {
      if (mz_diff[j] > itMzClust) all_clusters[j] = 1;
    }
    
    std::partial_sum(all_clusters.begin(), all_clusters.end(), all_clusters.begin());
    
    all_clusters.insert(all_clusters.begin(), 0);
    
    for (int &val : all_clusters) {
      val += 1;
    }
    
    int n_all_clusters = all_clusters.size();
    
    std::vector<int> idx_clusters(all_clusters.size());
    std::iota(idx_clusters.begin(), idx_clusters.end(), 0);
    
    std::set<int> clusters_set;
    
    for (const int& cl : all_clusters) {
      clusters_set.insert(cl);
    }
    
    std::vector<int> clusters(clusters_set.begin(), clusters_set.end());
    
    int n_clusters = clusters.size();
    
    std::vector<bool> fromSameScan(n_clusters, true);
    
    for (int z=0; z<n_clusters; ++z) {
      
      std::vector<int> temp_idx;
      
      for (int j=0; j<n_all_clusters; ++j) {
        if (all_clusters[j] == clusters[z]) temp_idx.push_back(j);
      }
      
      int n = temp_idx.size();
      
      std::vector<float> temp_rt(n);
      float* temp_rt_ptr = temp_rt.data();
      
      std::vector<float> temp_pre_ce(n);
      float* temp_pre_ce_ptr = temp_pre_ce.data();
      
      std::vector<float> temp_mz(n);
      float* temp_mz_ptr = temp_mz.data();
      
      std::vector<float> temp_intensity(n);
      float* temp_intensity_ptr = temp_intensity.data();
      
      for (const int& x : temp_idx) {
        *(temp_rt_ptr++) = *(rt_ptr + x);
        *(temp_pre_ce_ptr++) = *(pre_ce_ptr + x);
        *(temp_mz_ptr++) = *(mz_ptr + x);
        *(temp_intensity_ptr++) = *(intensity_ptr + x);
      }
      
      std::set<float> unique_temp_rt_set;
      
      for (const float& r : temp_rt) {
        unique_temp_rt_set.insert(r);
      }
      
      std::vector<float> unique_temp_rt(unique_temp_rt_set.begin(), unique_temp_rt_set.end());

      std::set<float> unique_temp_pre_ce_set;
      
      for (const float& r : temp_pre_ce) {
        unique_temp_pre_ce_set.insert(r);
      }
      
      std::vector<float> unique_temp_pre_ce(unique_temp_pre_ce_set.begin(), unique_temp_pre_ce_set.end());
      
      fromSameScan[z] = unique_temp_rt.size() < temp_rt.size();
      
      if (counter > 10) fromSameScan[z] = false;
      
      if (itMzClust < 0.0001) fromSameScan[z] = false;
      
      // when traces are twice in a given scan for cluster breaks the for loop and decreases the itMzClust
      if (fromSameScan[z]) {
        itMzClust = itMzClust - 0.0001;
        break;
      }
      
      bool enough_presence = false;
      
      if (unique_temp_pre_ce.size() < unique_pre_ce.size()) {
        enough_presence = unique_rt.size() * (unique_temp_pre_ce.size() / unique_pre_ce.size()) * presence <= unique_temp_rt.size();
        
      } else {
        enough_presence = unique_rt.size() * presence <= unique_temp_rt.size();
      }
      
      // when is not enough present skips the m/z cluster
      if (!enough_presence) continue;
      
      auto max_intensity_ptr = std::max_element(temp_intensity.begin(), temp_intensity.end());
      new_intensity.push_back(*max_intensity_ptr);
      
      int size_temp_mz = temp_mz.size();
      
      float mz_sum = 0, mz_numWeight = 0;
      
      for (int w = 0; w < size_temp_mz; w++) {
        mz_numWeight = mz_numWeight + temp_mz[w] * temp_intensity[w];
        mz_sum = mz_sum + temp_intensity[w];
      }
      
      float mean_mz = mz_numWeight / mz_sum;
      
      new_mz.push_back(mean_mz);
      
    } // end of clusters for loop
    
    if (new_mz.size() > 0) {
      
      float rt_mean = 0;
      
      for (float val : rt) {
        rt_mean += val;
      }
      
      rt_mean = rt_mean / rt.size();
      
      if (has_pre_mz) {
        
        float pre_mz_mean = 0;
        
        std::vector<bool> is_pre(new_mz.size(), false);
        
        for (const float& val : pre_mz) {
          pre_mz_mean += val;
        }
        
        pre_mz_mean = pre_mz_mean / pre_mz.size();
        
        if (!std::isnan(pre_mz_mean)) {
          for (size_t p = 0; p < new_mz.size(); p++) {
            if ((new_mz[p] >= pre_mz_mean - mzClust) && (new_mz[p] <= pre_mz_mean + mzClust)) {
              is_pre[p] = true;
            }
          }
        }
        
        const  std::vector<int> polarity_out(new_mz.size(), org_polarity[0]);
        const  std::vector<int> level_out(new_mz.size(), org_level[0]);
        const  std::vector<float> pre_mz_out(new_mz.size(), pre_mz_mean);
        const  std::vector<float> rt_out(new_mz.size(), rt_mean);
        
        out["polarity"] = polarity_out;
        out["level"] = level_out;
        out["pre_mz"] = pre_mz_out;
        out["rt"] = rt_out;
        out["mz"] = new_mz;
        out["intensity"] = new_intensity;
        out["is_pre"] = is_pre;
        
      } else {
        
        const  std::vector<int> polarity_out(new_mz.size(), org_polarity[0]);
        const  std::vector<int> level_out(new_mz.size(), org_level[0]);
        const  std::vector<float> rt_out(new_mz.size(), rt_mean);
        
        out["polarity"] = polarity_out;
        out["level"] = level_out;
        out["rt"] = rt_out;
        out["mz"] = new_mz;
        out["intensity"] = new_intensity;
        
      }
    }
    
    hasFromSameScan = false;
    
    for (const bool& l : fromSameScan) {
      if (l) {
        hasFromSameScan = true;
        break;
      }
    }
  } // end of while loop
  
  return out;
}

// MARK: rcpp_ms_annotate_features 

// [[Rcpp::export]]
Rcpp::List rcpp_ms_annotate_features(Rcpp::List feature_list,
                                     double rtWindowAlignment = 0.3,
                                     int maxIsotopes = 5,
                                     int maxCharge = 1,
                                     int maxGaps = 1) {
  
  nts::MS_ISOTOPE_SET isotopes;
  
  std::vector<std::string> elements = {"C","H", "N", "O", "S", "Cl", "Br", "Si"};
  
  isotopes.filter(elements);
  
  const int max_number_elements = 5;
  
  Rcpp::Rcout << "Building combinatorial isotopic chains with length "<< max_number_elements << "...";
  nts::MS_ISOTOPE_COMBINATIONS combinations(isotopes, max_number_elements);
  Rcpp::Rcout << "Done!" << std::endl;
  
  const int number_analyses = feature_list.size();
  
  if (number_analyses == 0) return feature_list;
  
  for (int a = 0; a < number_analyses; a++) {
    
    Rcpp::List features = feature_list[a];
    
    const nts::MS_FEATURES_MZ_SORTED fdf(features);
    
    const std::vector<std::string> fts = features["feature"];
    
    const int number_features = fdf.n;
    
    nts::MS_ANNOTATION af(number_features);
    
    Rcpp::Rcout << "Annotating isotopes in " << number_features << " features...";
    
    for (int f = 0; f < number_features; f++) {
      
      const int& index = fdf.index[f];
      
      if (af.iso_step[index] > 0) continue; // already isotope
      
      const std::string& feature = fdf.feature[f];
      const int& polarity = fdf.polarity[f];
      const float& rt = fdf.rt[f];
      float rtmin = fdf.rtmin[f];
      float rtmax = fdf.rtmax[f];
      const float& mz = fdf.mz[f];
      const float& mzmin = fdf.mzmin[f];
      const float& mzmax = fdf.mzmax[f];
      const float max_mz_chain = (mz + maxIsotopes) * 1.05;
      
      std::vector<int> candidates = nts::find_isotopic_candidates(
        number_features,
        fdf.feature, fdf.mz, fdf.rt, fdf.polarity,
        polarity, feature, mz, mzmin, mzmax, rt, rtmin, rtmax,
        rtWindowAlignment, max_mz_chain
      );
      
      const int number_candidates = candidates.size();
      
      if (number_candidates > 0) {
        
        candidates.insert(candidates.begin(), f);
        
        nts::MS_CANDIDATE_CHAIN candidates_chain(candidates, fdf.feature, fdf.index, fdf.mz, fdf.mzmin, fdf.mzmax, fdf.rt, fdf.intensity);
        
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
        af.iso_time_error[index] = 0;
        af.iso_relative_intensity[index] = 1;
        af.iso_theoretical_min_relative_intensity[index] = 0;
        af.iso_theoretical_max_relative_intensity[index] = 0;
        af.iso_number_carbons[index] = 0;
        af.iso_size[index] = 0;
      }
    }
    
    Rcpp::Rcout << "Done!" << std::endl;
    
    Rcpp::Rcout << "Annotating adducts in " << number_features << " features...";
    
    for (int f = 0; f < number_features; f++) {
      
      const int& index = fdf.index[f];
      
      if (af.iso_step[index] > 0) continue; // already isotope
      
      if (af.adduct_cat[index] != "") continue; // already adduct
      
      const int& polarity = fdf.polarity[f];
      const float& rt = fdf.rt[f];
      float rtmin = fdf.rtmin[f];
      float rtmax = fdf.rtmax[f];
      const float& mz = fdf.mz[f];
      const float& mzmin = fdf.mzmin[f];
      const float& mzmax = fdf.mzmax[f];
      const float max_mz_adducts = (mz + 100);
      
      std::vector<int> candidates = nts::find_adduct_candidates(
        number_features, fdf.mz, fdf.rt, fdf.polarity, af.iso_step, polarity, mz, mzmin, mzmax, rt, rtmin, rtmax, rtWindowAlignment, max_mz_adducts);
      
      const int number_candidates = candidates.size();
      
      if (number_candidates > 0) {
        candidates.insert(candidates.begin(), f);
        nts::MS_CANDIDATE_CHAIN candidates_chain(candidates, fdf.feature, fdf.index, fdf.mz, fdf.mzmin, fdf.mzmax, fdf.rt, fdf.intensity);
        nts::annotate_adducts(af, candidates_chain, polarity);
      }
    }
    
    Rcpp::Rcout << "Done!" << std::endl;
    
    Rcpp::List list_annotation(number_features);
    
    for (int f = 0; f < number_features; f++) {
      
      const int& index = fdf.index[f];
      
      Rcpp::List temp = Rcpp::List::create(
        Rcpp::Named("index") = index,
        Rcpp::Named("feature") = af.feature[f],
                                           Rcpp::Named("component_feature") = af.component_feature[f],
                                                                                                  Rcpp::Named("iso_size") = af.iso_size[f],
                                                                                                                                       Rcpp::Named("iso_charge") = af.iso_charge[f],
                                                                                                                                                                                Rcpp::Named("iso_step") = af.iso_step[f],
                                                                                                                                                                                                                     Rcpp::Named("iso_cat") = af.iso_cat[f],
                                                                                                                                                                                                                                                        Rcpp::Named("iso_isotope") = af.iso_isotope[f],
                                                                                                                                                                                                                                                                                                   Rcpp::Named("iso_mzr") = af.iso_mzr[f],
                                                                                                                                                                                                                                                                                                                                      Rcpp::Named("iso_relative_intensity") = af.iso_relative_intensity[f],
                                                                                                                                                                                                                                                                                                                                                                                                       Rcpp::Named("iso_theoretical_min_relative_intensity") = af.iso_theoretical_min_relative_intensity[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Rcpp::Named("iso_theoretical_max_relative_intensity") = af.iso_theoretical_max_relative_intensity[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Rcpp::Named("iso_mass_distance") = af.iso_mass_distance[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Rcpp::Named("iso_theoretical_mass_distance") = af.iso_theoretical_mass_distance[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Rcpp::Named("iso_mass_error") = af.iso_mass_distance_error[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Rcpp::Named("iso_time_error") = af.iso_time_error[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Rcpp::Named("iso_number_carbons") = af.iso_number_carbons[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Rcpp::Named("adduct_element") = af.adduct_element[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Rcpp::Named("adduct_cat") = af.adduct_cat[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Rcpp::Named("adduct_time_error") = af.adduct_time_error[f],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Rcpp::Named("adduct_mass_error") = af.adduct_mass_error[f]
      );
      
      temp.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      list_annotation[index] = temp;
    }
    
    features["annotation"] = list_annotation;
    
    feature_list[a] = features;
    
    // list_out["index"] = af.index;
    // list_out["feature"] = af.feature;
    // list_out["mz"] = fdf.mz;
    // list_out["rt"] = fdf.rt;
    // list_out["intensity"] = fdf.intensity;
    // list_out["component_feature"] = af.component_feature;
    // list_out["iso_size"] = af.iso_size;
    // list_out["iso_charge"] = af.iso_charge;
    // list_out["iso_step"] = af.iso_step;
    // list_out["iso_cat"] = af.iso_cat;
    // list_out["iso_isotope"] = af.iso_isotope;
    // list_out["iso_mzr"] = af.iso_mzr;
    // list_out["iso_relative_intensity"] = af.iso_relative_intensity;
    // list_out["iso_theoretical_min_relative_intensity"] = af.iso_theoretical_min_relative_intensity;
    // list_out["iso_theoretical_max_relative_intensity"] = af.iso_theoretical_max_relative_intensity;
    // list_out["iso_mass_distance"] = af.iso_mass_distance;
    // list_out["iso_theoretical_mass_distance"] = af.iso_theoretical_mass_distance;
    // list_out["iso_error"] = af.iso_mass_distance_error;
    // list_out["iso_number_carbons"] = af.iso_number_carbons;
    // list_out["adduct_element"] = af.adduct_element;
    // list_out["adduct_cat"] = af.adduct_cat;
    // list_out["adduct_error"] = af.adduct_error;
    // list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    
  }
  
  return feature_list;
};

// [[Rcpp::export]]
Rcpp::List rcpp_ms_load_features_eic(Rcpp::List analyses,
                                     Rcpp::List features,
                                     bool filtered = false,
                                     float rtExpand = 0,
                                     float mzExpand = 0,
                                     float minTracesIntensity = 0) {
  
  const int number_analyses = analyses.size();
  
  if (number_analyses == 0) return features;
  
  std::vector<std::string> analyses_names(number_analyses);
  
  for (int i = 0; i < number_analyses; i++) {
    Rcpp::List analysis = analyses[i];
    std::string name = analysis["name"];
    analyses_names[i] = name;
  }
  
  const int number_features_analyses = features.size();
  
  if (number_features_analyses != number_analyses) return features;
  
  std::vector<std::string> features_analyses_names = features.names();
  
  for (int i = 0; i < number_analyses; i++) {
    
    if (features_analyses_names[i] != analyses_names[i]) return features;
    
    Rcpp::List features_i = features[i];
    
    const std::vector<std::string>& features_i_names = features_i.names();
    
    const int n_features_i_names = features_i_names.size();
    
    if (n_features_i_names == 0) return features;
    
    std::vector<std::string> must_have_names = {
      "feature", "rt", "rtmax", "rtmin", "mz", "mzmax", "mzmin", "polarity", "filtered", "eic"
    };
    
    const int n_must_have_names = must_have_names.size();
    
    std::vector<bool> has_must_have_names(n_must_have_names, false);
    
    for (int j = 0; j < n_must_have_names; ++j) {
      for (int k = 0; k < n_features_i_names; ++k) {
        if (must_have_names[j] == features_i_names[k]) has_must_have_names[j] = true;
      }
    }
    
    for (bool value : has_must_have_names) {
      if (!value) {
        return features;
      }
    }
    
    const std::vector<std::string>& fts_id = features_i["feature"];
    const std::vector<bool>& fts_filtered = features_i["filtered"];
    const std::vector<int>& fts_polarity = features_i["polarity"];
    const std::vector<float>& fts_rt = features_i["rt"];
    const std::vector<float>& fts_rtmin = features_i["rtmin"];
    const std::vector<float>& fts_rtmax = features_i["rtmax"];
    const std::vector<float>& fts_mz = features_i["mz"];
    const std::vector<float>& fts_mzmin = features_i["mzmin"];
    const std::vector<float>& fts_mzmax = features_i["mzmax"];
    const std::vector<float>& fts_intensity = features_i["intensity"];
    std::vector<Rcpp::List> fts_eic = features_i["eic"];
    
    const int n_features = fts_id.size();
    
    if (n_features == 0) return features;
    
    sc::MS_TARGETS targets;
    
    int counter = 0;
    for (int j = 0; j < n_features; j++) {
      
      if (!filtered) if (fts_filtered[j]) continue;
      
      const int eic_size = fts_eic[j].size();
      if (eic_size > 0) continue;
      
      targets.index.push_back(counter);
      counter++;
      targets.id.push_back(fts_id[j]);
      targets.level.push_back(1);
      targets.polarity.push_back(fts_polarity[j]);
      targets.precursor.push_back(false);
      targets.mzmin.push_back(fts_mzmin[j] - mzExpand);
      targets.mzmax.push_back(fts_mzmax[j] + mzExpand);
      targets.rtmin.push_back(fts_rtmin[j] - rtExpand);
      targets.rtmax.push_back(fts_rtmax[j] + rtExpand);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
    }
    
    if (targets.id.size() == 0) continue;
    
    const Rcpp::List& analysis = analyses[i];
    
    sc::MS_SPECTRA_HEADERS headers = sc_r::get_ms_analysis_list_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_FILE ana(file);
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, headers, minTracesIntensity, 0);
    
    for (int j = 0; j < n_features; j++) {
      
      if (!filtered) if (fts_filtered[j]) continue;
      
      const std::string& id_j = fts_id[j];
      
      sc::MS_TARGETS_SPECTRA res_j = res[id_j];
      
      nts::merge_traces_within_rt(res_j.rt, res_j.mz, res_j.intensity);
      
      const int n = res_j.rt.size();
      const std::vector<std::string> id_vec = std::vector<std::string>(n, id_j);
      const std::vector<int> level_vec = std::vector<int>(n, res_j.level[0]);
      const std::vector<int> polarity_vec = std::vector<int>(n, res_j.polarity[0]);
      
      Rcpp::List eic = Rcpp::List::create(
        Rcpp::Named("feature") = id_vec,
        Rcpp::Named("polarity") = polarity_vec,
        Rcpp::Named("level") = level_vec,
        Rcpp::Named("rt") = res_j.rt,
        Rcpp::Named("mz") = res_j.mz,
        Rcpp::Named("intensity") = res_j.intensity
      );
      
      eic.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      fts_eic[j] = eic;
    }
    
    features_i["eic"] = fts_eic;
    features[i] = features_i;
  }
  
  return features;
}

// [[Rcpp::export]]
Rcpp::List rcpp_ms_load_features_ms1(Rcpp::List analyses,
                                     Rcpp::List features,
                                     bool filtered,
                                     std::vector<float> rtWindow,
                                     std::vector<float> mzWindow,
                                     float minTracesIntensity,
                                     float mzClust,
                                     float presence) {
  
  const int number_analyses = analyses.size();
  
  if (number_analyses == 0) return features;
  
  std::vector<std::string> analyses_names(number_analyses);
  
  for (int i = 0; i < number_analyses; i++) {
    Rcpp::List analysis = analyses[i];
    std::string name = analysis["name"];
    analyses_names[i] = name;
  }
  
  const int number_features_analyses = features.size();
  
  if (number_features_analyses != number_analyses) return features;
  
  std::vector<std::string> features_analyses_names = features.names();
  
  int rtWindowMax_idx = nts::find_max_index(rtWindow);
  int rtWindowMin_idx = nts::find_min_index(rtWindow);
  int mzWindowMax_idx = nts::find_max_index(mzWindow);
  int mzWindowMin_idx = nts::find_min_index(mzWindow);
  
  for (int i = 0; i < number_analyses; i++) {
    
    if (features_analyses_names[i] != analyses_names[i]) return features;
    
    Rcpp::List features_i = features[i];
    
    const std::vector<std::string>& features_i_names = features_i.names();
    
    const int n_features_i_names = features_i_names.size();
    
    if (n_features_i_names == 0) return features;
    
    std::vector<std::string> must_have_names = {
      "feature", "rt", "rtmax", "rtmin", "mz", "mzmax", "mzmin", "polarity", "filtered", "eic"
    };
    
    const int n_must_have_names = must_have_names.size();
    
    std::vector<bool> has_must_have_names(n_must_have_names, false);
    
    for (int j = 0; j < n_must_have_names; ++j) {
      for (int k = 0; k < n_features_i_names; ++k) {
        if (must_have_names[j] == features_i_names[k]) has_must_have_names[j] = true;
      }
    }
    
    for (bool value : has_must_have_names) {
      if (!value) {
        return features;
      }
    }
    
    const std::vector<std::string>& fts_id = features_i["feature"];
    const std::vector<bool>& fts_filtered = features_i["filtered"];
    const std::vector<int>& fts_polarity = features_i["polarity"];
    const std::vector<float>& fts_rt = features_i["rt"];
    const std::vector<float>& fts_mz = features_i["mz"];
    const std::vector<float>& fts_intensity = features_i["intensity"];
    std::vector<Rcpp::List> fts_ms1 = features_i["ms1"];
    
    const int n_features = fts_id.size();
    
    if (n_features == 0) return features;
    
    sc::MS_TARGETS targets;
    
    int counter = 0;
    for (int j = 0; j < n_features; j++) {
      if (!filtered) if (fts_filtered[j]) continue;
      
      const int ms1_size = fts_ms1[j].size();
      if (ms1_size > 0) continue;
      
      targets.index.push_back(counter);
      counter++;
      targets.id.push_back(fts_id[j]);
      targets.level.push_back(1);
      targets.polarity.push_back(fts_polarity[j]);
      targets.precursor.push_back(false);
      targets.mzmin.push_back(fts_mz[j] + mzWindow[mzWindowMin_idx]);
      targets.mzmax.push_back(fts_mz[j] + mzWindow[mzWindowMax_idx]);
      targets.rtmin.push_back(fts_rt[j] + rtWindow[rtWindowMin_idx]);
      targets.rtmax.push_back(fts_rt[j] + rtWindow[rtWindowMax_idx]);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
    }
    
    if (targets.id.size() == 0) continue;
    
    const Rcpp::List& analysis = analyses[i];
    
    sc::MS_SPECTRA_HEADERS headers = sc_r::get_ms_analysis_list_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_FILE ana(file);
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, headers, minTracesIntensity, 0);
    
    for (int j = 0; j < n_features; j++) {
      
      if (!filtered) if (fts_filtered[j]) continue;
      
      const std::string& id_j = fts_id[j];
      
      sc::MS_TARGETS_SPECTRA res_j = res[id_j];
      
      const int n_res_j = res_j.rt.size();
      
      if (n_res_j == 0) continue;
      
      const std::vector<std::string> id_vec = std::vector<std::string>(n_res_j, id_j);
      
      const Rcpp::List ms1 = Rcpp::List::create(
        Rcpp::Named("feature") = id_vec,
        Rcpp::Named("polarity") = res_j.polarity,
        Rcpp::Named("level") = res_j.level,
        Rcpp::Named("pre_mz") = res_j.pre_mz,
        Rcpp::Named("pre_ce") = res_j.pre_ce,
        Rcpp::Named("rt") = res_j.rt,
        Rcpp::Named("mz") = res_j.mz,
        Rcpp::Named("intensity") = res_j.intensity
      );
      
      Rcpp::List ms1_clustered = cluster_spectra(ms1, mzClust, presence);
      
      ms1_clustered.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      fts_ms1[j] = ms1_clustered;
    }
    
    features_i["ms1"] = fts_ms1;
    features[i] = features_i;
  }
  
  return features;
}

// [[Rcpp::export]]
Rcpp::List rcpp_ms_load_features_ms2(Rcpp::List analyses,
                                     Rcpp::List features,
                                     bool filtered,
                                     float minTracesIntensity,
                                     float isolationWindow,
                                     float mzClust,
                                     float presence) {
  
  const int number_analyses = analyses.size();
  
  if (number_analyses == 0) return features;
  
  std::vector<std::string> analyses_names(number_analyses);
  
  for (int i = 0; i < number_analyses; i++) {
    Rcpp::List analysis = analyses[i];
    std::string name = analysis["name"];
    analyses_names[i] = name;
  }
  
  const int number_features_analyses = features.size();
  
  if (number_features_analyses != number_analyses) return features;
  
  std::vector<std::string> features_analyses_names = features.names();
  
  for (int i = 0; i < number_analyses; i++) {
    
    if (features_analyses_names[i] != analyses_names[i]) return features;
    
    Rcpp::List features_i = features[i];
    
    const std::vector<std::string>& features_i_names = features_i.names();
    
    const int n_features_i_names = features_i_names.size();
    
    if (n_features_i_names == 0) return features;
    
    std::vector<std::string> must_have_names = {
      "feature", "rt", "rtmax", "rtmin", "mz", "mzmax", "mzmin", "polarity", "filtered", "eic"
    };
    
    const int n_must_have_names = must_have_names.size();
    
    std::vector<bool> has_must_have_names(n_must_have_names, false);
    
    for (int j = 0; j < n_must_have_names; ++j) {
      for (int k = 0; k < n_features_i_names; ++k) {
        if (must_have_names[j] == features_i_names[k]) has_must_have_names[j] = true;
      }
    }
    
    for (bool value : has_must_have_names) {
      if (!value) {
        return features;
      }
    }
    
    const std::vector<std::string>& fts_id = features_i["feature"];
    const std::vector<bool>& fts_filtered = features_i["filtered"];
    const std::vector<int>& fts_polarity = features_i["polarity"];
    const std::vector<float>& fts_rt = features_i["rt"];
    const std::vector<float>& fts_rtmin = features_i["rtmin"];
    const std::vector<float>& fts_rtmax = features_i["rtmax"];
    const std::vector<float>& fts_mz = features_i["mz"];
    const std::vector<float>& fts_intensity = features_i["intensity"];
    std::vector<Rcpp::List> fts_ms2 = features_i["ms2"];
    
    const int n_features = fts_id.size();
    
    if (n_features == 0) return features;
    
    sc::MS_TARGETS targets;
    
    int counter = 0;
    for (int j = 0; j < n_features; j++) {
      
      if (!filtered) if (fts_filtered[j]) continue;
      
      const int ms2_size = fts_ms2[j].size();
      if (ms2_size > 0) continue;
      
      targets.index.push_back(counter);
      counter++;
      targets.id.push_back(fts_id[j]);
      targets.level.push_back(2);
      targets.polarity.push_back(fts_polarity[j]);
      targets.precursor.push_back(true);
      targets.mzmin.push_back(fts_mz[j] - (isolationWindow / 2));
      targets.mzmax.push_back(fts_mz[j] + (isolationWindow / 2));
      targets.rtmin.push_back(fts_rtmin[j] - 1);
      targets.rtmax.push_back(fts_rtmax[j] + 1);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
    }
    
    if (targets.id.size() == 0) continue;
    
    const Rcpp::List& analysis = analyses[i];
    
    sc::MS_SPECTRA_HEADERS headers = sc_r::get_ms_analysis_list_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_FILE ana(file);
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, headers, 0, minTracesIntensity);
    
    for (int j = 0; j < n_features; j++) {
      
      if (!filtered) if (fts_filtered[j]) continue;
      
      const std::string& id_j = fts_id[j];
      
      sc::MS_TARGETS_SPECTRA res_j = res[id_j];
      
      const int n_res_j = res_j.rt.size();
      
      if (n_res_j == 0) continue;
      
      const std::vector<std::string> id_vec = std::vector<std::string>(n_res_j, id_j);
      
      const  Rcpp::List ms2 = Rcpp::List::create(
        Rcpp::Named("feature") = id_vec,
        Rcpp::Named("polarity") = res_j.polarity,
        Rcpp::Named("level") = res_j.level,
        Rcpp::Named("pre_mz") = res_j.pre_mz,
        Rcpp::Named("pre_ce") = res_j.pre_ce,
        Rcpp::Named("rt") = res_j.rt,
        Rcpp::Named("mz") = res_j.mz,
        Rcpp::Named("intensity") = res_j.intensity
      );
      
      Rcpp::List ms2_clustered = cluster_spectra(ms2, mzClust, presence);
      
      ms2_clustered.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      fts_ms2[j] = ms2_clustered;
    }
    
    features_i["ms2"] = fts_ms2;
    features[i] = features_i;
  }
  
  return features;
}

// [[Rcpp::export]]
Rcpp::List rcpp_ms_fill_features(Rcpp::List analyses,
                                 Rcpp::DataFrame features,
                                 bool withinReplicate = false,
                                 float rtExpand = 0,
                                 float mzExpand = 0,
                                 float minTracesIntensity = 0,
                                 float minNumberTraces = 5,
                                 float baseCut = 0,
                                 float minSignalToNoiseRatio = 3,
                                 float minGaussianFit = 0.5) {
  
  Rcpp::List out;
  
  const int number_analyses = analyses.size();
  
  if (number_analyses == 0) return out;
  
  std::vector<std::string> analyses_names(number_analyses);
  std::vector<std::string> analyses_rpls(number_analyses);
  std::vector<std::vector<int>> analyses_polarities(number_analyses);
  
  for (int i = 0; i < number_analyses; i++) {
    Rcpp::List analysis = analyses[i];
    std::string name = analysis["name"];
    analyses_names[i] = name;
    std::string rpl = analysis["replicate"];
    analyses_rpls[i] = rpl;
    Rcpp::List headers = analysis["spectra_headers"];
    std::vector<int> pols = headers["polarity"];
    std::set<int> pols_set(pols.begin(), pols.end());
    std::vector<int> pols_unique(pols_set.begin(), pols_set.end());
    analyses_polarities[i] = pols_unique;
  }
  
  int max_presence = number_analyses;
  
  const int number_features = features.nrow();
  
  if (number_features == 0) return out;
  
  std::vector<std::string> features_cols = features.names();
  
  const int ncol_features = features_cols.size();
  
  if (ncol_features == 0) return out;
  
  std::vector<std::string> must_have_names = {
    "feature", "analysis", "rt", "rtmax", "rtmin",
    "mz", "mzmax", "mzmin", "intensity", "group"
  };
  
  std::vector<bool> has_must_have_names(10, false);
  
  for (size_t i = 0; i < must_have_names.size(); ++i) {
    for (size_t j = 0; j < features_cols.size(); ++j) {
      if (must_have_names[i] == features_cols[j]) has_must_have_names[i] = true;
    }
  }
  
  for (bool value : has_must_have_names) {
    if (!value) {
      return out;
    }
  }
  
  const std::vector<std::string>& fts_group = features["group"];
  const std::vector<std::string>& fts_replicates = features["replicate"];
  const std::vector<std::string>& fts_analysis = features["analysis"];
  const std::vector<float>& fts_mass = features["mass"];
  const std::vector<float>& fts_mz = features["mz"];
  const std::vector<float>& fts_mzmin = features["mzmin"];
  const std::vector<float>& fts_mzmax = features["mzmax"];
  const std::vector<float>& fts_rt = features["rt"];
  const std::vector<float>& fts_rtmin = features["rtmin"];
  const std::vector<float>& fts_rtmax = features["rtmax"];
  // const std::vector<float> fts_mobility = features["mobility"];
  // const std::vector<float> fts_mobilitymin = features["mobilitymin"];
  // const std::vector<float> fts_mobilitymax = features["mobilitymax"];
  const std::vector<float>& fts_intensity = features["intensity"];
  
  std::vector<std::string> fts_id = fts_group;
  
  if (withinReplicate) {
    
    std::unordered_map<std::string, int> rpl_sizes;
    
    for (int i = 0; i < number_analyses; i++) {
      Rcpp::List analysis = analyses[i];
      std::string rpl = analysis["replicate"];
      rpl_sizes[rpl]++;
    }
    
    max_presence = 0;
    for (const auto& rpl : rpl_sizes) if (rpl.second > max_presence) max_presence = rpl.second;
    
    for (int i = 0; i < number_features; i++) fts_id[i] = fts_id[i] + "_" + fts_replicates[i];
  }
  
  if (max_presence == 0) return out;
  
  std::unordered_map<std::string, int> id_presence;
  for (int i = 0; i < number_features; i++) id_presence[fts_id[i]]++;
  
  std::vector<std::string> id = fts_id;
  
  id.erase(
    std::remove_if(id.begin(), id.end(), [&](const std::string& name) {
      return id_presence[name] == max_presence;
    }),
    id.end()
  );
  
  if (id.empty()) return out;
  
  std::set<std::string> id_set(id.begin(), id.end());
  
  std::vector<std::string> unique_id(id_set.begin(), id_set.end());
  
  const int number_unique_id = unique_id.size();
  
  std::vector<sc::MS_TARGETS> ana_targets(number_analyses);
  std::vector<std::vector<std::string>> ana_targets_groups(number_analyses);
  std::vector<std::vector<std::string>> ana_targets_replicates(number_analyses);
  
  Rcpp::Rcout << "Building targets for filling " << number_unique_id << " feature groups...";
  
  for (int k = 0; k < number_unique_id; k++) {
    
    std::vector<int> indices;
    for (int i = 0; i < number_features; i++) {
      if (fts_id[i] == unique_id[k]) indices.push_back(i);
    }
    
    if (indices.size() == 0) continue;
    
    std::vector<std::string> fts_group_k(indices.size());
    std::vector<std::string> fts_replicates_k(indices.size());
    std::vector<std::string> fts_analysis_k(indices.size());
    std::vector<float> fts_mass_k(indices.size());
    std::vector<float> fts_mz_k(indices.size());
    std::vector<float> fts_mzmin_k(indices.size());
    std::vector<float> fts_mzmax_k(indices.size());
    std::vector<float> fts_rt_k(indices.size());
    std::vector<float> fts_rtmin_k(indices.size());
    std::vector<float> fts_rtmax_k(indices.size());
    std::vector<float> fts_intensity_k(indices.size());
    std::vector<float> fts_mzmin_side_k(indices.size());
    std::vector<float> fts_mzmax_side_k(indices.size());
    
    for (size_t i = 0; i < indices.size(); i++) {
      fts_group_k[i] = fts_group[indices[i]];
      fts_replicates_k[i] = fts_replicates[indices[i]];
      fts_analysis_k[i] = fts_analysis[indices[i]];
      fts_mass_k[i] = fts_mass[indices[i]];
      fts_mz_k[i] = fts_mz[indices[i]];
      fts_mzmin_k[i] = fts_mzmin[indices[i]];
      fts_mzmax_k[i] = fts_mzmax[indices[i]];
      fts_rt_k[i] = fts_rt[indices[i]];
      fts_rtmin_k[i] = fts_rtmin[indices[i]] - rtExpand;
      fts_rtmax_k[i] = fts_rtmax[indices[i]] + rtExpand;
      fts_intensity_k[i] = fts_intensity[indices[i]];
      
      fts_mzmin_side_k[i] = fts_mz[indices[i]] - fts_mzmin[indices[i]];
      fts_mzmax_side_k[i] = fts_mzmax[indices[i]] - fts_mz[indices[i]];
    }
    
    const float mean_mass = std::accumulate(fts_mass_k.begin(), fts_mass_k.end(), 0.0) / fts_mass_k.size();
    const float max_mzmin_side = *std::max_element(fts_mzmin_side_k.begin(), fts_mzmin_side_k.end());
    const float max_mzmax_side = *std::max_element(fts_mzmax_side_k.begin(), fts_mzmax_side_k.end());
    const float min_rtmin = *std::min_element(fts_rtmin_k.begin(), fts_rtmin_k.end());
    const float max_rtmax = *std::max_element(fts_rtmax_k.begin(), fts_rtmax_k.end());
    
    for (int j = 0; j < number_analyses; j++) {
      
      if (withinReplicate) {
        const Rcpp::List& analysis = analyses[j];
        const std::string& rpl = analysis["replicate"];
        
        // continues if analysis replicate j is not within the same replicate as the feature group
        if (fts_replicates_k[0] != rpl) continue;
      }
      
      bool has_analysis = false;
      
      for (size_t i = 0; i < indices.size(); i++) {
        if (fts_analysis_k[i] == analyses_names[j]) has_analysis = true;
      }
      
      if (has_analysis) continue;
      
      std::vector<int> pols = analyses_polarities[j];
      
      for (size_t p = 0; p < pols.size(); p++) {
        
        float mz = mean_mass + (pols[p] * 1.007276);
        float mzmin = mz - max_mzmin_side - mzExpand;
        float mzmax = mz + max_mzmax_side + mzExpand;
        
        const int n_targets = ana_targets[j].index.size();
        ana_targets[j].index.push_back(n_targets);
        ana_targets[j].id.push_back(unique_id[k]);
        ana_targets[j].level.push_back(1);
        ana_targets[j].polarity.push_back(pols[p]);
        ana_targets[j].precursor.push_back(false);
        ana_targets[j].mzmin.push_back(mzmin);
        ana_targets[j].mzmax.push_back(mzmax);
        ana_targets[j].rtmin.push_back(min_rtmin);
        ana_targets[j].rtmax.push_back(max_rtmax);
        ana_targets[j].mobilitymin.push_back(0);
        ana_targets[j].mobilitymax.push_back(0);
        ana_targets_groups[j].push_back(fts_group_k[0]);
        ana_targets_replicates[j].push_back(fts_replicates_k[0]);
      }
    }
  }
  
  Rcpp::Rcout << "Done!" << std::endl;
  
  for (int j = 0; j < number_analyses; j++) {
    
    Rcpp::List out_analyses;
    
    int n_j_targets = ana_targets[j].id.size();
    
    if (n_j_targets == 0) continue;
    
    Rcpp::Rcout << "Extracting " << ana_targets[j].id.size() << " EICs from analysis " << analyses_names[j] << "...";
    
    const Rcpp::List& analysis = analyses[j];
    
    const sc::MS_SPECTRA_HEADERS headers = sc_r::get_ms_analysis_list_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_FILE ana(file);
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(ana_targets[j], headers, minTracesIntensity, 0);
    
    const int n_traces = res.id.size();
    
    Rcpp::Rcout << " Done! " << std::endl;
    
    for (int i = n_j_targets - 1; i >= 0; --i) {
      
      const std::string& id_i = ana_targets[j].id[i];
      
      int count = 0;
      
      for (int z = 0; z < n_traces; z++) if (res.id[z] == id_i) count++;
      
      if (count < minNumberTraces) {
        ana_targets[j].index.erase(ana_targets[j].index.begin() + i);
        ana_targets[j].id.erase(ana_targets[j].id.begin() + i);
        ana_targets[j].level.erase(ana_targets[j].level.begin() + i);
        ana_targets[j].polarity.erase(ana_targets[j].polarity.begin() + i);
        ana_targets[j].precursor.erase(ana_targets[j].precursor.begin() + i);
        ana_targets[j].mzmin.erase(ana_targets[j].mzmin.begin() + i);
        ana_targets[j].mzmax.erase(ana_targets[j].mzmax.begin() + i);
        ana_targets[j].rtmin.erase(ana_targets[j].rtmin.begin() + i);
        ana_targets[j].rtmax.erase(ana_targets[j].rtmax.begin() + i);
        ana_targets[j].mobilitymin.erase(ana_targets[j].mobilitymin.begin() + i);
        ana_targets[j].mobilitymax.erase(ana_targets[j].mobilitymax.begin() + i);
        ana_targets_groups[j].erase(ana_targets_groups[j].begin() + i);
        ana_targets_replicates[j].erase(ana_targets_replicates[j].begin() + i);
      }
    }
    
    n_j_targets = ana_targets[j].id.size();
    
    Rcpp::Rcout << "Filling " << n_j_targets << " features from analysis " << analyses_names[j] << "...";
    
    const std::vector<std::string> tg_id = ana_targets[j].id;
    
    for (int i = 0; i < n_j_targets; i++) {
      
      const std::string& id_i = tg_id[i];
      
      sc::MS_TARGETS_SPECTRA res_i = res[id_i];
      
      const int n_res_i = res_i.id.size();
      
      if (n_res_i < minNumberTraces) continue;
      
      nts::merge_traces_within_rt(res_i.rt, res_i.mz, res_i.intensity);

      Rcpp::List quality = nts::calculate_gaussian_fit(id_i, res_i.rt, res_i.intensity, baseCut);

      const float& sn = quality["sn"];

      const float& gauss_f = quality["gauss_f"];

      if (sn < minSignalToNoiseRatio) continue;

      if (gauss_f < minGaussianFit) continue;
      
      const float area_i = nts::trapezoidal_area(res_i.rt, res_i.intensity);
      
      const size_t max_position = nts::find_max_index(res_i.intensity);
 
      const float rt_i = res_i.rt[max_position];

      const float mz_i = nts::mean(res_i.mz);

      const float mass_i = mz_i - (res_i.polarity[0] * 1.007276);

      const float mzmin_i = *std::min_element(res_i.mz.begin(), res_i.mz.end());

      const float mzmax_i = *std::max_element(res_i.mz.begin(), res_i.mz.end());

      const float rtmin_i = *std::min_element(res_i.rt.begin(), res_i.rt.end());

      const float rtmax_i = *std::max_element(res_i.rt.begin(), res_i.rt.end());

      std::string adduct_i = "[M+H]+";
      if (res_i.polarity[0] < 0) adduct_i = "[M-H]-";

      std::string feature = "filled_" + std::to_string(i + 1);
      
      // std::string enc_rt = sc::encode_little_endian_from_float(res_i.rt, 4);
      // std::string enc_mz = sc::encode_little_endian_from_float(res_i.mz, 4);
      // std::string enc_intensity = sc::encode_little_endian_from_float(res_i.intensity, 4);
      // enc_rt = sc::encode_base64(enc_rt);
      // enc_mz = sc::encode_base64(enc_mz);
      // enc_intensity = sc::encode_base64(enc_intensity);
      
      const int n = res_i.rt.size();
      const std::vector<std::string> id_vec = std::vector<std::string>(n, id_i);
      
      Rcpp::List eic = Rcpp::List::create(
        Rcpp::Named("feature") = id_vec,
        Rcpp::Named("polarity") = res_i.polarity,
        Rcpp::Named("level") = res_i.level,
        Rcpp::Named("rt") = res_i.rt,
        Rcpp::Named("mz") = res_i.mz,
        Rcpp::Named("intensity") = res_i.intensity
      );
      
      eic.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      Rcpp::List list_eic;
      list_eic.push_back(eic);
      
      Rcpp::List list_quality;
      list_quality.push_back(quality);
      
      Rcpp::List empty_list = Rcpp::List::create(R_NilValue);

      Rcpp::List out_targets;
      out_targets["analysis"] = analyses_names[j];
      out_targets["feature"] = feature;
      out_targets["rt"] = rt_i;
      out_targets["mz"] = mz_i;
      out_targets["intensity"] = res_i.intensity[max_position];
      out_targets["area"] = area_i;
      out_targets["rtmin"] = rtmin_i;
      out_targets["rtmax"] = rtmax_i;
      out_targets["mzmin"] = mzmin_i;
      out_targets["mzmax"] = mzmax_i;
      out_targets["polarity"] = res_i.polarity[0];
      out_targets["mass"] = mass_i;
      out_targets["adduct"] = adduct_i;
      out_targets["filtered"] = false;
      out_targets["filled"] = true;
      out_targets["quality"] = list_quality;
      out_targets["isotope"] = empty_list;
      out_targets["eic"] = list_eic;
      out_targets["ms1"] = empty_list;
      out_targets["ms2"] = empty_list;
      out_targets["istd"] = empty_list;
      out_targets["suspects"] = empty_list;
      out_targets["group"] = ana_targets_groups[j][i];
      out_targets.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      out_analyses[feature] = out_targets;
    }
    
    out.push_back(out_analyses);
    
    Rcpp::Rcout << " Done! " << std::endl;
  }
  
  out.names() = analyses_names;
  
  return out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_ms_calculate_features_quality(Rcpp::List analyses,
                                              Rcpp::List features,
                                              bool filtered = false,
                                              float rtExpand = 0,
                                              float mzExpand = 0,
                                              float minTracesIntensity = 0,
                                              float minNumberTraces = 5,
                                              float baseCut = 0) {

  const int number_analyses = analyses.size();
  
  if (number_analyses == 0) return features;
  
  std::vector<std::string> analyses_names(number_analyses);
  
  for (int i = 0; i < number_analyses; i++) {
    Rcpp::List analysis = analyses[i];
    std::string name = analysis["name"];
    analyses_names[i] = name;
  }
  
  const int number_features_analyses = features.size();
  
  if (number_features_analyses != number_analyses) return features;
  
  std::vector<std::string> features_analyses_names = features.names();
  
  for (int i = 0; i < number_analyses; i++) {
    if (features_analyses_names[i] != analyses_names[i]) return features;
    
    Rcpp::List features_i = features[i];
    
    const std::vector<std::string>& features_i_names = features_i.names();
    
    const int n_features_i_names = features_i_names.size();
    
    if (n_features_i_names == 0) return features;
    
    std::vector<std::string> must_have_names = {
      "feature", "rt", "rtmax", "rtmin", "mz", "mzmax", "mzmin", "polarity", "filtered", "quality", "eic"
    };
    
    const int n_must_have_names = must_have_names.size();
    
    std::vector<bool> has_must_have_names(n_must_have_names, false);
    
    for (int j = 0; j < n_must_have_names; ++j) {
      for (int k = 0; k < n_features_i_names; ++k) {
        if (must_have_names[j] == features_i_names[k]) has_must_have_names[j] = true;
      }
    }
    
    for (bool value : has_must_have_names) {
      if (!value) {
        return features;
      }
    }
    
    const std::vector<std::string>& fts_id = features_i["feature"];
    const std::vector<bool>& fts_filtered = features_i["filtered"];
    const std::vector<int>& fts_polarity = features_i["polarity"];
    const std::vector<float>& fts_rt = features_i["rt"];
    const std::vector<float>& fts_rtmin = features_i["rtmin"];
    const std::vector<float>& fts_rtmax = features_i["rtmax"];
    const std::vector<float>& fts_mz = features_i["mz"];
    const std::vector<float>& fts_mzmin = features_i["mzmin"];
    const std::vector<float>& fts_mzmax = features_i["mzmax"];
    const std::vector<float>& fts_intensity = features_i["intensity"];
    
    std::vector<Rcpp::List> fts_quality = features_i["quality"];
    std::vector<Rcpp::List> fts_eic = features_i["eic"];
    
    const int n_features = fts_id.size();
    
    if (n_features == 0) return features;
    
    sc::MS_TARGETS targets;
    
    std::vector<bool> has_quality(n_features, false);
    
    std::vector<bool> has_eic(n_features, false);
    
    int counter = 0;
    for (int j = 0; j < n_features; j++) {
      
      const Rcpp::List& quality = fts_quality[j];
      
      const int n_quality = quality.size();
      
      if (n_quality > 0) {
        has_quality[j] = true;
        continue;
      }
      
      if (!filtered) if (fts_filtered[j]) continue;
      
      const Rcpp::List& eic = fts_eic[j];
      
      const int n_eic = eic.size();
      
      if (n_eic > 0) {
        has_eic[j] = true;
        continue;
      }
      
      targets.index.push_back(counter);
      counter++;
      targets.id.push_back(fts_id[j]);
      targets.level.push_back(1);
      targets.polarity.push_back(fts_polarity[j]);
      targets.precursor.push_back(false);
      targets.mzmin.push_back(fts_mzmin[j] - mzExpand);
      targets.mzmax.push_back(fts_mzmax[j] + mzExpand);
      targets.rtmin.push_back(fts_rtmin[j] - rtExpand);
      targets.rtmax.push_back(fts_rtmax[j] + rtExpand);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
    }
    
    if (targets.id.size() == 0) continue;
    
    const Rcpp::List& analysis = analyses[i];
    
    const sc::MS_SPECTRA_HEADERS headers = sc_r::get_ms_analysis_list_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_FILE ana(file);
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, headers, minTracesIntensity, 0);
    
    for (int j = 0; j < n_features; j++) {
      
      if (has_quality[j]) continue;
      
      if (!filtered) if (fts_filtered[j]) continue;
      
      const std::string& id_j = fts_id[j];
      
      std::vector<float> rt;
      std::vector<float> mz;
      std::vector<float> intensity;
      
      if (has_eic[j]) {
        const Rcpp::List& eic = fts_eic[j];
        const std::vector<float>& rt_ref = eic["rt"];
        const std::vector<float>& mz_ref = eic["mz"];
        const std::vector<float>& intensity_ref = eic["intensity"];
        rt = rt_ref;
        mz = mz_ref;
        intensity = intensity_ref;
      } else {
        sc::MS_TARGETS_SPECTRA res_j = res[id_j];
        rt = res_j.rt;
        mz = res_j.mz;
        intensity = res_j.intensity;
        nts::merge_traces_within_rt(rt, mz, intensity);
        Rcpp::List eic = Rcpp::List::create(
          Rcpp::Named("feature") = id_j,
          Rcpp::Named("polarity") = res_j.polarity,
          Rcpp::Named("level") = res_j.level,
          Rcpp::Named("rt") = rt,
          Rcpp::Named("mz") = mz,
          Rcpp::Named("intensity") = intensity
        );
        fts_eic[j] = eic;
      }
      
      Rcpp::List quality = Rcpp::List::create(
        Rcpp::Named("feature") = id_j,
        Rcpp::Named("noise") = 0,
        Rcpp::Named("sn") = 0,
        Rcpp::Named("gauss_a") = 0,
        Rcpp::Named("gauss_u") = 0,
        Rcpp::Named("gauss_s") = 0,
        Rcpp::Named("gauss_f") = 0
      );

      const int n = rt.size();

      if (n > minNumberTraces) quality = nts::calculate_gaussian_fit(id_j, rt, intensity, baseCut);
      
      fts_quality[j] = quality;
    }
    
    features_i["eic"] = fts_eic;
    features_i["quality"] = fts_quality;
    features[i] = features_i;
  }
  
  return features;
}
