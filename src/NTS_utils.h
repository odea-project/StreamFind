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
#include <limits>
#include "StreamCraft_lib.h"

namespace NTS
{
  // MARK: INLINE FUNCTIONS
  
  // MARK: make_empty_dt
  inline Rcpp::List get_empty_dt()
  {
    Rcpp::List out = Rcpp::List::create();
    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
  };
  
  // MARK: check_must_have_names_list
  inline bool check_must_have_names_list(const Rcpp::List &list,
                                         const std::vector<std::string> &must_have_names)
  {
    std::vector<std::string> names_list = list.names();
    const int must_have_names_size = must_have_names.size();
    if (must_have_names_size == 0) return false;
    const int names_list_size = names_list.size();
    if (names_list_size == 0) return false;
    std::vector<bool> has_must_have_names(must_have_names_size, false);
    
    for (int i = 0; i < must_have_names_size; ++i)
    {
      for (int j = 0; j < names_list_size; ++j)
      {
        if (must_have_names[i] == names_list[j])
          has_must_have_names[i] = true;
      }
    }
    
    bool has_all_must_have_names = true;
    
    for (int i = 0; i < must_have_names_size; ++i)
    {
      if (!has_must_have_names[i])
      {
        has_all_must_have_names = false;
        break;
      }
    }
    
    return has_all_must_have_names;
  };
  
  // MARK: mean
  inline float mean(const std::vector<float> &v)
  {
    return std::accumulate(v.begin(), v.end(), 0.0) / v.size();
  };
  
  // MARK: standard_deviation
  inline float standard_deviation(const std::vector<float> &v,
                                  float mean_val)
  {
    float sum = 0.0;
    for (float num : v)
    {
      sum += pow(num - mean_val, 2);
    }
    return sqrt(sum / v.size());
  };
  
  // MARK: find_max_index
  inline size_t find_max_index(const std::vector<float> &v)
  {
    return std::max_element(v.begin(), v.end()) - v.begin();
  };
  
  // MARK: find_min_index
  inline size_t find_min_index(const std::vector<float> &v)
  {
    return std::min_element(v.begin(), v.end()) - v.begin();
  };
  
  // MARK: gaussian_function
  inline float gaussian_function(const float &A,
                                 const float &mu,
                                 const float &sigma,
                                 const float &x)
  {
    return A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
  };
  
  // MARK: ONLY DEFINED FUNCTIONS

  // MARK: as_MS_SPECTRA_HEADERS
  sc::MS_SPECTRA_HEADERS as_MS_SPECTRA_HEADERS(const Rcpp::List &hd);
  
  // MARK: merge_traces_within_rt
  void merge_traces_within_rt(std::vector<float> &rt,
                              std::vector<float> &mz,
                              std::vector<float> &intensity);
  
  // MARK: find_central_max_index
  size_t find_central_max_index(const std::vector<float> &rt,
                                const std::vector<float> &intensity,
                                const float &rt_mean,
                                const float &rtWindow);
  
  // MARK: trim_eic_by_low_cut
  void trim_eic_by_low_cut(std::vector<float> &rt,
                           std::vector<float> &mz,
                           std::vector<float> &intensity,
                           const float &lowCut);
  
  // MARK: trim_to_equal_length_around_max_position
  void trim_to_equal_length_around_max_position(std::vector<float> &rt,
                                                std::vector<float> &mz,
                                                std::vector<float> &intensity,
                                                const size_t max_position,
                                                const int minDiffSize,
                                                const int minTraces,
                                                const float maxTimeHalfWidth);
  
  // MARK: trim_peak_base
  void trim_peak_base(std::vector<float> &rt,
                      std::vector<float> &mz,
                      std::vector<float> &intensity,
                      size_t &max_position,
                      const float curRatio);
  
  // MARK: apply_moving_average
  void apply_moving_average(std::vector<float>& x,
                            const size_t &start,
                            const size_t &end,
                            const int &windowSize);
  
  // MARK: smooth_eic_sides
  void smooth_eic_sides(std::vector<float>& x,
                        const size_t &max_position,
                        const int &windowSize);
  
  // MARK: fit_gaussian_cost_function
  float fit_gaussian_cost_function(const std::vector<float> &x,
                                   const std::vector<float> &y,
                                   const float &A,
                                   const float &mu,
                                   const float &sigma);
  
  // MARK: fit_gaussian
  void fit_gaussian(const std::vector<float> &x,
                    const std::vector<float> &y,
                    float &A, float &mu,
                    float &sigma);
  
  // MARK: calculate_gaussian_rsquared
  float calculate_gaussian_rsquared(const std::vector<float> &x,
                                    const std::vector<float> &y,
                                    const float &A,
                                    const float &mu,
                                    const float &sigma);
  
  // MARK: trapezoidal_area
  float trapezoidal_area(const std::vector<float> &x,
                         const std::vector<float> &intensity);
  
  // MARK: is_max_gap_reached
  bool is_max_gap_reached(const int &s, const int &maxGaps,
                          const std::vector<int> &steps);
  
  // MARK: cluster_spectra
  Rcpp::List cluster_spectra(const Rcpp::List &spectra,
                             const float &mzClust, const float &presence);
  
  // MARK: cluster_spectra_internal
  void cluster_spectra_internal(std::vector<float> &pre_ce,
                                std::vector<float> &pre_mz,
                                std::vector<float> &rt,
                                std::vector<float> &mz,
                                std::vector<float> &intensity,
                                std::vector<bool> &is_pre,
                                const float &mzClust,
                                const float &presence);
  
  // MARK: STRUCTS
  
  // MARK: FEATURE_EIC
  struct FEATURE_EIC
  {
    std::string feature = "";
    int polarity;
    int level;
    std::vector<float> rt;
    std::vector<float> mz;
    std::vector<float> intensity;
    
    int size() const
    {
      return rt.size();
    };
    
    Rcpp::List to_list_dt() const
    {
      if (feature == "" || rt.size() == 0)
        return get_empty_dt();
      
      std::vector<std::string> feature_v(rt.size(), feature);
      std::vector<int> polarity_v(rt.size(), polarity);
      std::vector<int> level_v(rt.size(), level);
      
      Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("feature") = feature_v,
        Rcpp::Named("polarity") = polarity_v,
        Rcpp::Named("level") = level_v,
        Rcpp::Named("rt") = rt,
        Rcpp::Named("mz") = mz,
        Rcpp::Named("intensity") = intensity
      );
      
      out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      return out;
    };
    
    void import_from_list(const Rcpp::List &eic_list)
    {
      if (eic_list.size() == 0)
        return;
      
      std::vector<std::string> must_have_names = {
        "feature", "polarity", "level", "rt", "mz", "intensity"
      };
      
      if (!check_must_have_names_list(eic_list, must_have_names))
      {
        Rcpp::Rcout << "Error: EIC list does not have all required names." << std::endl;
        return;
      }
      
      const std::vector<std::string> &feature_ref = Rcpp::as<std::vector<std::string>>(eic_list["feature"]);
      const std::vector<int> &polarity_ref = Rcpp::as<std::vector<int>>(eic_list["polarity"]);
      const std::vector<int> &level_ref = Rcpp::as<std::vector<int>>(eic_list["level"]);
      const std::vector<float> &rt_ref = Rcpp::as<std::vector<float>>(eic_list["rt"]);
      const std::vector<float> &mz_ref = Rcpp::as<std::vector<float>>(eic_list["mz"]);
      const std::vector<float> &intensity_ref = Rcpp::as<std::vector<float>>(eic_list["intensity"]);
      
      const int n = rt_ref.size();
      if (n == 0) return;
      
      feature = feature_ref[0];
      polarity = polarity_ref[0];
      level = level_ref[0];
      rt = rt_ref;
      mz = mz_ref;
      intensity = intensity_ref;
    };
    
    void import_from_ms_targets_spectra(const sc::MS_TARGETS_SPECTRA &spectra)
    {
      if (spectra.rt.size() > 0)
      {
        feature = spectra.id[0];
        polarity = spectra.polarity[0];
        level = spectra.level[0];
        rt = spectra.rt;
        mz = spectra.mz;
        intensity = spectra.intensity;
        NTS::merge_traces_within_rt(rt, mz, intensity);
      }
    };
  };

  // MARK: FEATURE_MS1
  struct FEATURE_MS1
  {
    std::string feature = "";
    int polarity;
    int level;
    std::vector<float> pre_ce;
    std::vector<float> pre_mz;
    std::vector<float> rt;
    std::vector<float> mz;
    std::vector<float> intensity;
    std::vector<bool> is_pre;
    
    int size() const
    {
      return rt.size();
    };
    
    Rcpp::List to_list_dt() const
    {
      if (feature == "" || rt.size() == 0)
        return get_empty_dt();
      
      std::vector<std::string> feature_v(rt.size(), feature);
      std::vector<int> polarity_v(rt.size(), polarity);
      std::vector<int> level_v(rt.size(), level);
      
      Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("feature") = feature_v,
        Rcpp::Named("polarity") = polarity_v,
        Rcpp::Named("level") = level_v,
        Rcpp::Named("pre_mz") = pre_mz,
        Rcpp::Named("rt") = rt,
        Rcpp::Named("mz") = mz,
        Rcpp::Named("intensity") = intensity,
        Rcpp::Named("is_pre") = is_pre
      );
      
      out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      return out;
    };
    
    void import_from_list(const Rcpp::List &ms1_list)
    {
      if (ms1_list.size() == 0)
        return;
      
      std::vector<std::string> must_have_names = {
        "feature", "polarity", "level", "pre_mz", "rt", "mz", "intensity", "is_pre"
      };
      
      if (!check_must_have_names_list(ms1_list, must_have_names))
      {
        Rcpp::Rcout << "Error: MS1 list does not have all required names." << std::endl;
        return;
      }
      
      const std::vector<std::string> &feature_ref = Rcpp::as<std::vector<std::string>>(ms1_list["feature"]);
      const std::vector<int> &polarity_ref = Rcpp::as<std::vector<int>>(ms1_list["polarity"]);
      const std::vector<int> &level_ref = Rcpp::as<std::vector<int>>(ms1_list["level"]);
      const std::vector<float> &pre_mz_ref = Rcpp::as<std::vector<float>>(ms1_list["pre_mz"]);
      const std::vector<float> &rt_ref = Rcpp::as<std::vector<float>>(ms1_list["rt"]);
      const std::vector<float> &mz_ref = Rcpp::as<std::vector<float>>(ms1_list["mz"]);
      const std::vector<float> &intensity_ref = Rcpp::as<std::vector<float>>(ms1_list["intensity"]);
      const std::vector<bool> &is_pre_ref = Rcpp::as<std::vector<bool>>(ms1_list["is_pre"]);
      
      const int n = rt_ref.size();
      if (n == 0)
        return;
      
      feature = feature_ref[0];
      polarity = polarity_ref[0];
      level = level_ref[0];
      std::vector<float> pre_ce_temp(n, 0.0f);
      pre_ce = pre_ce_temp;
      pre_mz = pre_mz_ref;
      rt = rt_ref;
      mz = mz_ref;
      intensity = intensity_ref;
      is_pre = is_pre_ref;
    };
    
    void import_from_ms_targets_spectra(const sc::MS_TARGETS_SPECTRA &spectra)
    {
      if (spectra.rt.size() > 0)
      {
        feature = spectra.id[0];
        polarity = spectra.polarity[0];
        level = spectra.level[0];
        pre_ce = spectra.pre_ce;
        pre_mz = spectra.pre_mz;
        rt = spectra.rt;
        mz = spectra.mz;
        intensity = spectra.intensity;
        std::vector<bool> is_pre_temp(spectra.rt.size(), false);
        is_pre = is_pre_temp;
      }
    };
    
    void sort_by_mz()
    {
      if (rt.size() == 0)
        return;
      
      const int n = rt.size();
      
      std::vector<size_t> indices(mz.size());
      std::iota(indices.begin(), indices.end(), 0);
      std::sort(indices.begin(), indices.end(), [&](size_t i1, size_t i2) { return mz[i1] < mz[i2]; });
      
      std::vector<float> pre_ce_temp(n);
      std::vector<float> pre_mz_temp(n);
      std::vector<float> rt_temp(n);
      std::vector<float> mz_temp(n);
      std::vector<float> intensity_temp(n);
      std::vector<bool> is_pre_temp(n);
      
      for (size_t i = 0; i < indices.size(); ++i)
      {
        pre_ce_temp[i] = pre_ce[indices[i]];
        pre_mz_temp[i] = pre_mz[indices[i]];
        rt_temp[i] = rt[indices[i]];
        mz_temp[i] = mz[indices[i]];
        intensity_temp[i] = intensity[indices[i]];
        is_pre_temp[i] = is_pre[indices[i]];
      }
      
      pre_ce = pre_ce_temp;
      pre_mz = pre_mz_temp;
      rt = rt_temp;
      mz = mz_temp;
      intensity = intensity_temp;
      is_pre = is_pre_temp;
    };
    
    void set_precursor_mz(const float &val)
    {
      if (pre_mz.size() == 0)
        return;
      
      for (size_t i = 0; i < pre_mz.size(); ++i)
      {
        if (is_pre[i])
          pre_mz[i] = val;
      }
    };
    
    void cluster(const float &mzClust, const float &presence)
    {
      if (rt.size() > 0)
      {
        this->sort_by_mz();
        cluster_spectra_internal(pre_ce, pre_mz, rt, mz, intensity, is_pre, mzClust, presence);
      }
    };
  };
  
  // MARK: FEATURE_MS2
  struct FEATURE_MS2
  {
    std::string feature = "";
    int polarity;
    int level;
    std::vector<float> pre_ce;
    std::vector<float> pre_mz;
    std::vector<float> rt;
    std::vector<float> mz;
    std::vector<float> intensity;
    std::vector<bool> is_pre;
    
    int size() const
    {
      return rt.size();
    };
    
    Rcpp::List to_list_dt() const
    {
      if (feature == "" || rt.size() == 0)
        return get_empty_dt();
      
      std::vector<std::string> feature_v(rt.size(), feature);
      std::vector<int> polarity_v(rt.size(), polarity);
      std::vector<int> level_v(rt.size(), level);
      
      Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("feature") = feature_v,
        Rcpp::Named("polarity") = polarity_v,
        Rcpp::Named("level") = level_v,
        Rcpp::Named("pre_mz") = pre_mz,
        Rcpp::Named("rt") = rt,
        Rcpp::Named("mz") = mz,
        Rcpp::Named("intensity") = intensity,
        Rcpp::Named("is_pre") = is_pre
      );
      
      out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      return out;
    };
    
    void import_from_list(const Rcpp::List &ms2_list)
    {
      if (ms2_list.size() == 0)
        return;
      
      std::vector<std::string> must_have_names = {
        "feature", "polarity", "level", "pre_mz", "rt", "mz", "intensity", "is_pre"
      };
      
      if (!check_must_have_names_list(ms2_list, must_have_names))
      {
        Rcpp::Rcout << "Error: MS2 list does not have all required names." << std::endl;
        return;
      }
      
      const std::vector<std::string> &feature_ref = Rcpp::as<std::vector<std::string>>(ms2_list["feature"]);
      const std::vector<int> &polarity_ref = Rcpp::as<std::vector<int>>(ms2_list["polarity"]);
      const std::vector<int> &level_ref = Rcpp::as<std::vector<int>>(ms2_list["level"]);
      const std::vector<float> &pre_mz_ref = Rcpp::as<std::vector<float>>(ms2_list["pre_mz"]);
      const std::vector<float> &rt_ref = Rcpp::as<std::vector<float>>(ms2_list["rt"]);
      const std::vector<float> &mz_ref = Rcpp::as<std::vector<float>>(ms2_list["mz"]);
      const std::vector<float> &intensity_ref = Rcpp::as<std::vector<float>>(ms2_list["intensity"]);
      const std::vector<bool> &is_pre_ref = Rcpp::as<std::vector<bool>>(ms2_list["is_pre"]);
      
      const int n = rt_ref.size();
      if (n == 0)
        return;
      
      feature = feature_ref[0];
      polarity = polarity_ref[0];
      level = level_ref[0];
      std::vector<float> pre_ce_temp(n, 0.0f);
      pre_ce = pre_ce_temp;
      pre_mz = pre_mz_ref;
      rt = rt_ref;
      mz = mz_ref;
      intensity = intensity_ref;
      is_pre = is_pre_ref;
    };
    
    void import_from_ms_targets_spectra(const sc::MS_TARGETS_SPECTRA &spectra)
    {
      if (spectra.rt.size() > 0)
      {
        feature = spectra.id[0];
        polarity = spectra.polarity[0];
        level = spectra.level[0];
        pre_ce = spectra.pre_ce;
        pre_mz = spectra.pre_mz;
        rt = spectra.rt;
        mz = spectra.mz;
        intensity = spectra.intensity;
        std::vector<bool> is_pre_temp(spectra.rt.size(), false);
        is_pre = is_pre_temp;
      }
    };
    
    void sort_by_mz()
    {
      if (rt.size() == 0)
        return;
      
      const int n = rt.size();
      
      std::vector<size_t> indices(mz.size());
      std::iota(indices.begin(), indices.end(), 0);
      std::sort(indices.begin(), indices.end(), [&](size_t i1, size_t i2) { return mz[i1] < mz[i2]; });
      
      std::vector<float> pre_ce_temp(n);
      std::vector<float> pre_mz_temp(n);
      std::vector<float> rt_temp(n);
      std::vector<float> mz_temp(n);
      std::vector<float> intensity_temp(n);
      std::vector<bool> is_pre_temp(n);
      
      for (size_t i = 0; i < indices.size(); ++i)
      {
        pre_ce_temp[i] = pre_ce[indices[i]];
        pre_mz_temp[i] = pre_mz[indices[i]];
        rt_temp[i] = rt[indices[i]];
        mz_temp[i] = mz[indices[i]];
        intensity_temp[i] = intensity[indices[i]];
        is_pre_temp[i] = is_pre[indices[i]];
      }
      
      pre_ce = pre_ce_temp;
      pre_mz = pre_mz_temp;
      rt = rt_temp;
      mz = mz_temp;
      intensity = intensity_temp;
      is_pre = is_pre_temp;
    };
    
    void set_precursor_mz(const float &val)
    {
      if (pre_mz.size() == 0)
        return;
      
      for (size_t i = 0; i < pre_mz.size(); ++i)
      {
        if (is_pre[i])
          pre_mz[i] = val;
      }
    };
    
    void cluster(const float &mzClust, const float &presence)
    {
      if (rt.size() > 0)
      {
        this->sort_by_mz();
        cluster_spectra_internal(pre_ce, pre_mz, rt, mz, intensity, is_pre, mzClust, presence);
      }
    };
  };
  
  // MARK: FEATURE_QUALITY
  struct FEATURE_QUALITY
  {
    std::string feature = "";
    float noise = 0.0f;
    float sn = 0.0f;
    float gauss_a = 0.0f;
    float gauss_u = 0.0f;
    float gauss_s = 0.0f;
    float gauss_f = 0.0f;
    
    Rcpp::List to_list_dt() const
    {
      if (feature == "")
        return get_empty_dt();
      
      Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("feature") = feature,
        Rcpp::Named("noise") = noise,
        Rcpp::Named("sn") = sn,
        Rcpp::Named("gauss_a") = gauss_a,
        Rcpp::Named("gauss_u") = gauss_u,
        Rcpp::Named("gauss_s") = gauss_s,
        Rcpp::Named("gauss_f") = gauss_f
      );
      
      out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      return out;
    };
    
    void import_from_list(const Rcpp::List &quality_list)
    {
      if (quality_list.size() == 0)
        return;
    
      std::vector<std::string> must_have_names = {
        "feature", "noise", "sn", "gauss_a", "gauss_u", "gauss_s", "gauss_f"
      };
      
      if (!check_must_have_names_list(quality_list, must_have_names))
      {
        Rcpp::Rcout << "Error: Quality list does not have all required names." << std::endl;
        return;
      }
      
      const std::vector<std::string> &feature_ref = Rcpp::as<std::vector<std::string>>(quality_list["feature"]);
      const std::vector<float> &noise_ref = Rcpp::as<std::vector<float>>(quality_list["noise"]);
      const std::vector<float> &sn_ref = Rcpp::as<std::vector<float>>(quality_list["sn"]);
      const std::vector<float> &gauss_a_ref = Rcpp::as<std::vector<float>>(quality_list["gauss_a"]);
      const std::vector<float> &gauss_u_ref = Rcpp::as<std::vector<float>>(quality_list["gauss_u"]);
      const std::vector<float> &gauss_s_ref = Rcpp::as<std::vector<float>>(quality_list["gauss_s"]);
      const std::vector<float> &gauss_f_ref = Rcpp::as<std::vector<float>>(quality_list["gauss_f"]);
      
      const int n = feature_ref.size();
      if (n == 0)
        return;
      
      feature = feature_ref[0];
      noise = noise_ref[0];
      sn = sn_ref[0];
      gauss_a = gauss_a_ref[0];
      gauss_u = gauss_u_ref[0];
      gauss_s = gauss_s_ref[0];
      gauss_f = gauss_f_ref[0];
    };
  };
  
  // MARK: FEATURE_ANNOTATION 
  struct FEATURE_ANNOTATION
  {
    std::string feature = "";
    std::string component_feature = "";
    int iso_size = 0;
    int iso_charge = 0;
    int iso_step = 0;
    std::string iso_cat = "";
    std::string iso_isotope = "";
    float iso_mzr = 0.0f;
    float iso_relative_intensity = 0.0f;
    float iso_theoretical_min_relative_intensity = 0.0f;
    float iso_theoretical_max_relative_intensity = 0.0f;
    float iso_mass_distance = 0.0f;
    float iso_theoretical_mass_distance = 0.0f;
    float iso_mass_distance_error = 0.0f;
    float iso_time_error = 0.0f;
    float iso_number_carbons = 0.0f;
    std::string adduct_element = "";
    std::string adduct_cat = "";
    float adduct_time_error = 0.0f;
    float adduct_mass_error = 0.0f;
    
    void as_only_monoisotopic_ion(const std::string &ft)
    {
      feature = ft;
      component_feature = ft;
      iso_step = 0;
      iso_cat = "M+0";
      iso_isotope = "";
      iso_charge = 1;
      iso_mzr = 0;
      iso_mass_distance = 0;
      iso_theoretical_mass_distance = 0;
      iso_mass_distance_error = 0;
      iso_time_error = 0;
      iso_relative_intensity = 1;
      iso_theoretical_min_relative_intensity = 0;
      iso_theoretical_max_relative_intensity = 0;
      iso_number_carbons = 0;
      iso_size = 0;
    };
    
    Rcpp::List to_list_dt() const
    {
      if (feature == "")
        return get_empty_dt();
      
      Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("feature") = feature,
        Rcpp::Named("component_feature") = component_feature,
        Rcpp::Named("iso_size") = iso_size,
        Rcpp::Named("iso_charge") = iso_charge,
        Rcpp::Named("iso_step") = iso_step,
        Rcpp::Named("iso_cat") = iso_cat,
        Rcpp::Named("iso_isotope") = iso_isotope,
        Rcpp::Named("iso_mzr") = iso_mzr,
        Rcpp::Named("iso_relative_intensity") = iso_relative_intensity,
        Rcpp::Named("iso_theoretical_min_relative_intensity") = iso_theoretical_min_relative_intensity,
        Rcpp::Named("iso_theoretical_max_relative_intensity") = iso_theoretical_max_relative_intensity,
        Rcpp::Named("iso_mass_distance") = iso_mass_distance,
        Rcpp::Named("iso_theoretical_mass_distance") = iso_theoretical_mass_distance,
        Rcpp::Named("iso_mass_distance_error") = iso_mass_distance_error,
        Rcpp::Named("iso_time_error") = iso_time_error,
        Rcpp::Named("iso_number_carbons") = iso_number_carbons,
        Rcpp::Named("adduct_element") = adduct_element,
        Rcpp::Named("adduct_cat") = adduct_cat,
        Rcpp::Named("adduct_time_error") = adduct_time_error,
        Rcpp::Named("adduct_mass_error") = adduct_mass_error
      );
      
      out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      return out;
    };
    
    void import_from_list(const Rcpp::List &annotation_list)
    {
      if (annotation_list.size() == 0)
        return;
      
      std::vector<std::string> must_have_names = {
        "feature", "component_feature", "iso_size", "iso_charge", "iso_step", "iso_cat",
        "iso_isotope", "iso_mzr", "iso_relative_intensity", "iso_theoretical_min_relative_intensity",
        "iso_theoretical_max_relative_intensity", "iso_mass_distance", "iso_theoretical_mass_distance",
        "iso_mass_distance_error", "iso_time_error", "iso_number_carbons", "adduct_element",
        "adduct_cat", "adduct_time_error", "adduct_mass_error"
      };
      
      if (!check_must_have_names_list(annotation_list, must_have_names))
      {
        Rcpp::Rcout << "Error: Annotation list does not have all required names." << std::endl;
        return;
      }
      
      const std::vector<std::string> &feature_ref = Rcpp::as<std::vector<std::string>>(annotation_list["feature"]);
      const std::vector<std::string> &component_feature_ref = Rcpp::as<std::vector<std::string>>(annotation_list["component_feature"]);
      const std::vector<int> &iso_size_ref = Rcpp::as<std::vector<int>>(annotation_list["iso_size"]);
      const std::vector<int> &iso_charge_ref = Rcpp::as<std::vector<int>>(annotation_list["iso_charge"]);
      const std::vector<int> &iso_step_ref = Rcpp::as<std::vector<int>>(annotation_list["iso_step"]);
      const std::vector<std::string> &iso_cat_ref = Rcpp::as<std::vector<std::string>>(annotation_list["iso_cat"]);
      const std::vector<std::string> &iso_isotope_ref = Rcpp::as<std::vector<std::string>>(annotation_list["iso_isotope"]);
      const std::vector<float> &iso_mzr_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_mzr"]);
      const std::vector<float> &iso_relative_intensity_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_relative_intensity"]);
      const std::vector<float> &iso_theoretical_min_relative_intensity_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_theoretical_min_relative_intensity"]);
      const std::vector<float> &iso_theoretical_max_relative_intensity_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_theoretical_max_relative_intensity"]);
      const std::vector<float> &iso_mass_distance_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_mass_distance"]);
      const std::vector<float> &iso_theoretical_mass_distance_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_theoretical_mass_distance"]);
      const std::vector<float> &iso_mass_distance_error_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_mass_distance_error"]);
      const std::vector<float> &iso_time_error_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_time_error"]);
      const std::vector<float> &iso_number_carbons_ref = Rcpp::as<std::vector<float>>(annotation_list["iso_number_carbons"]);
      const std::vector<std::string> &adduct_element_ref = Rcpp::as<std::vector<std::string>>(annotation_list["adduct_element"]);
      const std::vector<std::string> &adduct_cat_ref = Rcpp::as<std::vector<std::string>>(annotation_list["adduct_cat"]);
      const std::vector<float> &adduct_time_error_ref = Rcpp::as<std::vector<float>>(annotation_list["adduct_time_error"]);
      const std::vector<float> &adduct_mass_error_ref = Rcpp::as<std::vector<float>>(annotation_list["adduct_mass_error"]);
      
      const int n = feature_ref.size();
      if (n == 0)
        return;
      
      feature = feature_ref[0];
      component_feature = component_feature_ref[0];
      iso_size = iso_size_ref[0];
      iso_charge = iso_charge_ref[0];
      iso_step = iso_step_ref[0];
      iso_cat = iso_cat_ref[0];
      iso_isotope = iso_isotope_ref[0];
      iso_mzr = iso_mzr_ref[0];
      iso_relative_intensity = iso_relative_intensity_ref[0];
      iso_theoretical_min_relative_intensity = iso_theoretical_min_relative_intensity_ref[0];
      iso_theoretical_max_relative_intensity = iso_theoretical_max_relative_intensity_ref[0];
      iso_mass_distance = iso_mass_distance_ref[0];
      iso_theoretical_mass_distance = iso_theoretical_mass_distance_ref[0];
      iso_mass_distance_error = iso_mass_distance_error_ref[0];
      iso_time_error = iso_time_error_ref[0];
      iso_number_carbons = iso_number_carbons_ref[0];
      adduct_element = adduct_element_ref[0];
      adduct_cat = adduct_cat_ref[0];
      adduct_time_error = adduct_time_error_ref[0];
      adduct_mass_error = adduct_mass_error_ref[0];
    };
  };
  
  // MARK: FEATURE
  struct 
    FEATURE
  {
    std::string analysis;
    std::string feature;
    std::string group;
    float rt;
    float mz;
    float intensity;
    float area;
    float rtmin;
    float rtmax;
    float mzmin;
    float mzmax;
    float mass;
    int polarity;
    std::string adduct;
    bool filtered;
    std::string filter;
    bool filled;
    float correction;
    FEATURE_EIC eic;
    FEATURE_MS1 ms1;
    FEATURE_MS2 ms2;
    FEATURE_QUALITY quality;
    FEATURE_ANNOTATION annotation;
    Rcpp::List istd;
    Rcpp::List suspects;
    Rcpp::List formulas;
    Rcpp::List compounds;
    
    void calculate_quality(const float &baseCut,
                           const float &rtWindow,
                           const float &maxTimeHalfWidth);
    
    void update_properties()
    {
      if (eic.rt.size() > 0)
      {
        mzmin = *std::min_element(eic.mz.begin(), eic.mz.end());
        mzmax = *std::max_element(eic.mz.begin(), eic.mz.end());
        rtmin = *std::min_element(eic.rt.begin(), eic.rt.end());
        rtmax = *std::max_element(eic.rt.begin(), eic.rt.end());
        
        size_t max_position = find_central_max_index(eic.rt, eic.intensity, rt, 0);
        
        if (!(max_position < 1 || max_position >= eic.rt.size() - 1))
        {
          rt = eic.rt[max_position];
          intensity = eic.intensity[max_position];
        }
        
        area = trapezoidal_area(eic.rt, eic.intensity);
      }
    }
  };
  
  // MARK: FEATURES
  struct FEATURES
  {
    std::string analysis;
    std::vector<std::string> feature;
    std::vector<std::string> group;
    std::vector<float> rt;
    std::vector<float> mz;
    std::vector<float> intensity;
    std::vector<float> area;
    std::vector<float> rtmin;
    std::vector<float> rtmax;
    std::vector<float> mzmin;
    std::vector<float> mzmax;
    std::vector<float> mass;
    std::vector<int> polarity;
    std::vector<std::string> adduct;
    std::vector<bool> filtered;
    std::vector<std::string> filter;
    std::vector<bool> filled;
    std::vector<float> correction;
    std::vector<FEATURE_EIC> eic;
    std::vector<FEATURE_MS1> ms1;
    std::vector<FEATURE_MS2> ms2;
    std::vector<FEATURE_QUALITY> quality;
    std::vector<FEATURE_ANNOTATION> annotation;
    std::vector<Rcpp::List> istd;
    std::vector<Rcpp::List> suspects;
    std::vector<Rcpp::List> formulas;
    std::vector<Rcpp::List> compounds;
    bool valid = false;
    
    void import_from_list(const std::string &a, const Rcpp::List &fts)
    {
      analysis = a;
      
      if (fts.size() == 0)
      {
        return;
      }
      
      std::vector<std::string> must_have_names = {
        "feature", "group",
        "rt", "mz",
        "intensity", "area",
        "rtmin",  "rtmax", "mzmin", "mzmax",
        "mass", "polarity", "adduct",
        "filtered", "filter", "filled", "correction",
        "eic", "ms1", "ms2",
        "quality", "annotation", "istd",
        "suspects", "formulas", "compounds"
      };
      
      if (!check_must_have_names_list(fts, must_have_names))
      {
        Rcpp::Rcout << "Error: FEATURES::import_from_list() - missing required names in the list." << std::endl;
        return;
      }
      
      const std::vector<std::string> &rf_feature = Rcpp::as<std::vector<std::string>>(fts["feature"]);
      const std::vector<std::string> &rf_group = Rcpp::as<std::vector<std::string>>(fts["group"]);
      const std::vector<float> &rf_rt = Rcpp::as<std::vector<float>>(fts["rt"]);
      const std::vector<float> &rf_mz = Rcpp::as<std::vector<float>>(fts["mz"]);
      const std::vector<float> &rf_intensity = Rcpp::as<std::vector<float>>(fts["intensity"]);
      const std::vector<float> &rf_area = Rcpp::as<std::vector<float>>(fts["area"]);
      const std::vector<float> &rf_rtmin = Rcpp::as<std::vector<float>>(fts["rtmin"]);
      const std::vector<float> &rf_rtmax = Rcpp::as<std::vector<float>>(fts["rtmax"]);
      const std::vector<float> &rf_mzmin = Rcpp::as<std::vector<float>>(fts["mzmin"]);
      const std::vector<float> &rf_mzmax = Rcpp::as<std::vector<float>>(fts["mzmax"]);
      const std::vector<float> &rf_mass = Rcpp::as<std::vector<float>>(fts["mass"]);
      const std::vector<int> &rf_polarity = Rcpp::as<std::vector<int>>(fts["polarity"]);
      const std::vector<std::string> &rf_adduct = Rcpp::as<std::vector<std::string>>(fts["adduct"]);
      const std::vector<bool> &rf_filtered = Rcpp::as<std::vector<bool>>(fts["filtered"]);
      const std::vector<std::string> &rf_filter = Rcpp::as<std::vector<std::string>>(fts["filter"]);
      const std::vector<bool> &rf_filled = Rcpp::as<std::vector<bool>>(fts["filled"]);
      const std::vector<float> &rf_correction = Rcpp::as<std::vector<float>>(fts["correction"]);
      const std::vector<Rcpp::List> &rf_eic = Rcpp::as<std::vector<Rcpp::List>>(fts["eic"]);
      const std::vector<Rcpp::List> &rf_ms1 = Rcpp::as<std::vector<Rcpp::List>>(fts["ms1"]);
      const std::vector<Rcpp::List> &rf_ms2 = Rcpp::as<std::vector<Rcpp::List>>(fts["ms2"]);
      const std::vector<Rcpp::List> &rf_quality = Rcpp::as<std::vector<Rcpp::List>>(fts["quality"]);
      const std::vector<Rcpp::List> &rf_annotation = Rcpp::as<std::vector<Rcpp::List>>(fts["annotation"]);
      const std::vector<Rcpp::List> &rf_istd = Rcpp::as<std::vector<Rcpp::List>>(fts["istd"]);
      const std::vector<Rcpp::List> &rf_suspects = Rcpp::as<std::vector<Rcpp::List>>(fts["suspects"]);
      const std::vector<Rcpp::List> &rf_formulas = Rcpp::as<std::vector<Rcpp::List>>(fts["formulas"]);
      const std::vector<Rcpp::List> &rf_compounds = Rcpp::as<std::vector<Rcpp::List>>(fts["compounds"]);
      
      const int n = rf_feature.size();
      
      if (n == 0)
      {
        return;
      }
      
      eic.resize(n);
      ms1.resize(n);
      ms2.resize(n);
      quality.resize(n);
      annotation.resize(n);
      istd.resize(n);
      suspects.resize(n);
      formulas.resize(n);
      compounds.resize(n);
      
      feature = rf_feature;
      group = rf_group;
      rt = rf_rt;
      mz = rf_mz;
      intensity = rf_intensity;
      area = rf_area;
      rtmin = rf_rtmin;
      rtmax = rf_rtmax;
      mzmin = rf_mzmin;
      mzmax = rf_mzmax;
      mass = rf_mass;
      polarity = rf_polarity;
      adduct = rf_adduct;
      filtered = rf_filtered;
      filter = rf_filter;
      filled = rf_filled;
      correction = rf_correction;
      
      for (int i = 0; i < n; i++)
      {
        const Rcpp::List &eic_list = rf_eic[i];
        const Rcpp::List &ms1_list = rf_ms1[i];
        const Rcpp::List &ms2_list = rf_ms2[i];
        const Rcpp::List &quality_list = rf_quality[i];
        const Rcpp::List &annotation_list = rf_annotation[i];
        const Rcpp::List &istd_list = rf_istd[i];
        const Rcpp::List &suspects_list = rf_suspects[i];
        const Rcpp::List &formulas_list = rf_formulas[i];
        const Rcpp::List &compounds_list = rf_compounds[i];
        
        FEATURE_EIC eic_i;
        eic_i.import_from_list(eic_list);
        eic[i] = eic_i;
        
        FEATURE_MS1 ms1_i;
        ms1_i.import_from_list(ms1_list);
        ms1[i] = ms1_i;
        
        FEATURE_MS2 ms2_i;
        ms2_i.import_from_list(ms2_list);
        ms2[i] = ms2_i;
        
        FEATURE_QUALITY quality_i;
        quality_i.import_from_list(quality_list);
        quality[i] = quality_i;
        
        FEATURE_ANNOTATION annotation_i;
        annotation_i.import_from_list(annotation_list);
        annotation[i] = annotation_i;
        
        istd[i] = istd_list;
        suspects[i] = suspects_list;
        formulas[i] = formulas_list;
        compounds[i] = compounds_list;
      }
      
      valid = true;
    };
    
    int size() const
    {
      return feature.size();
    };
    
    FEATURE get_feature(const int &i) const {
      FEATURE feature_i;
      feature_i.analysis = analysis;
      feature_i.feature = feature[i];
      feature_i.group = group[i];
      feature_i.rt = rt[i];
      feature_i.mz = mz[i];
      feature_i.intensity = intensity[i];
      feature_i.area = area[i];
      feature_i.rtmin = rtmin[i];
      feature_i.rtmax = rtmax[i];
      feature_i.mzmin = mzmin[i];
      feature_i.mzmax = mzmax[i];
      feature_i.mass = mass[i];
      feature_i.polarity = polarity[i];
      feature_i.adduct = adduct[i];
      feature_i.filtered = filtered[i];
      feature_i.filter = filter[i];
      feature_i.filled = filled[i];
      feature_i.correction = correction[i];
      feature_i.eic = eic[i];
      feature_i.ms1 = ms1[i];
      feature_i.ms2 = ms2[i];
      feature_i.quality = quality[i];
      feature_i.annotation = annotation[i];
      feature_i.istd = istd[i];
      feature_i.suspects = suspects[i];
      feature_i.formulas = formulas[i];
      feature_i.compounds = compounds[i];
      return feature_i;
    };
    
    void set_feature(const int &i, const FEATURE &feature_i) {
      feature[i] = feature_i.feature;
      group[i] = feature_i.group;
      rt[i] = feature_i.rt;
      mz[i] = feature_i.mz;
      intensity[i] = feature_i.intensity;
      area[i] = feature_i.area;
      rtmin[i] = feature_i.rtmin;
      rtmax[i] = feature_i.rtmax;
      mzmin[i] = feature_i.mzmin;
      mzmax[i] = feature_i.mzmax;
      mass[i] = feature_i.mass;
      polarity[i] = feature_i.polarity;
      adduct[i] = feature_i.adduct;
      filtered[i] = feature_i.filtered;
      filter[i] = feature_i.filter;
      filled[i] = feature_i.filled;
      correction[i] = feature_i.correction;
      eic[i] = feature_i.eic;
      ms1[i] = feature_i.ms1;
      ms2[i] = feature_i.ms2;
      quality[i] = feature_i.quality;
      annotation[i] = feature_i.annotation;
      istd[i] = feature_i.istd;
      suspects[i] = feature_i.suspects;
      formulas[i] = feature_i.formulas;
      compounds[i] = feature_i.compounds;
    };
    
    void append_feature(const FEATURE &feature_i) {
      feature.push_back(feature_i.feature);
      group.push_back(feature_i.group);
      rt.push_back(feature_i.rt);
      mz.push_back(feature_i.mz);
      intensity.push_back(feature_i.intensity);
      area.push_back(feature_i.area);
      rtmin.push_back(feature_i.rtmin);
      rtmax.push_back(feature_i.rtmax);
      mzmin.push_back(feature_i.mzmin);
      mzmax.push_back(feature_i.mzmax);
      mass.push_back(feature_i.mass);
      polarity.push_back(feature_i.polarity);
      adduct.push_back(feature_i.adduct);
      filtered.push_back(feature_i.filtered);
      filter.push_back(feature_i.filter);
      filled.push_back(feature_i.filled);
      correction.push_back(feature_i.correction);
      eic.push_back(feature_i.eic);
      ms1.push_back(feature_i.ms1);
      ms2.push_back(feature_i.ms2);
      quality.push_back(feature_i.quality);
      annotation.push_back(feature_i.annotation);
      istd.push_back(feature_i.istd);
      suspects.push_back(feature_i.suspects);
      formulas.push_back(feature_i.formulas);
      compounds.push_back(feature_i.compounds);
    };
    
    Rcpp::List to_list_dt() const
    {
      
      int n = feature.size();
      
      if (n == 0 || !valid)
      {
        return get_empty_dt();
      }
      
      std::vector<Rcpp::List> eic_list(n);
      std::vector<Rcpp::List> ms1_list(n);
      std::vector<Rcpp::List> ms2_list(n);
      std::vector<Rcpp::List> quality_list(n);
      std::vector<Rcpp::List> annotation_list(n);
      std::vector<Rcpp::List> istd_list(n);
      std::vector<Rcpp::List> suspects_list(n);
      std::vector<Rcpp::List> formulas_list(n);
      std::vector<Rcpp::List> compounds_list(n);
      
      for (int i = 0; i < n; i++)
      {
        eic_list[i] = eic[i].to_list_dt();
        ms1_list[i] = ms1[i].to_list_dt();
        ms2_list[i] = ms2[i].to_list_dt();
        quality_list[i] = quality[i].to_list_dt();
        annotation_list[i] = annotation[i].to_list_dt();
        istd_list[i] = istd[i];
        istd_list[i].attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        suspects_list[i] = suspects[i];
        suspects_list[i].attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        formulas_list[i] = formulas[i];
        formulas_list[i].attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        compounds_list[i] = compounds[i];
        compounds_list[i].attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      }
      
      Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("feature") = feature,
        Rcpp::Named("group") = group,
        Rcpp::Named("rt") = rt,
        Rcpp::Named("mz") = mz,
        Rcpp::Named("intensity") = intensity,
        Rcpp::Named("area") = area,
        Rcpp::Named("rtmin") = rtmin,
        Rcpp::Named("rtmax") = rtmax,
        Rcpp::Named("mzmin") = mzmin,
        Rcpp::Named("mzmax") = mzmax,
        Rcpp::Named("mass") = mass,
        Rcpp::Named("polarity") = polarity,
        Rcpp::Named("adduct") = adduct,
        Rcpp::Named("filtered") = filtered,
        Rcpp::Named("filter") = filter,
        Rcpp::Named("filled") = filled,
        Rcpp::Named("correction") = correction,
        Rcpp::Named("eic") = eic_list,
        Rcpp::Named("ms1") = ms1_list,
        Rcpp::Named("ms2") = ms2_list,
        Rcpp::Named("quality") = quality_list,
        Rcpp::Named("annotation") = annotation_list,
        Rcpp::Named("istd") = istd_list,
        Rcpp::Named("suspects") = suspects_list,
        Rcpp::Named("formulas") = formulas_list,
        Rcpp::Named("compounds") = compounds_list
      );
      
      out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      return out;
    };
    
    void sort_by_mz() {
      
      if (!valid)
      {
        Rcpp::Rcout << "Error: FEATURES::sort_by_mz() - features are not valid." << std::endl;
        return;
      }
      
      if (feature.size() == 0)
      {
        return;
      }
      
      std::vector<int> indices(feature.size());
      std::iota(indices.begin(), indices.end(), 0);
      
      std::sort(indices.begin(), indices.end(), [this](int i1, int i2) { return mz[i1] < mz[i2]; });
      
      std::vector<std::string> feature_sorted;
      std::vector<std::string> group_sorted;
      std::vector<float> rt_sorted;
      std::vector<float> mz_sorted;
      std::vector<float> intensity_sorted;
      std::vector<float> area_sorted;
      std::vector<float> rtmin_sorted;
      std::vector<float> rtmax_sorted;
      std::vector<float> mzmin_sorted;
      std::vector<float> mzmax_sorted;
      std::vector<float> mass_sorted;
      std::vector<int> polarity_sorted;
      std::vector<std::string> adduct_sorted;
      std::vector<bool> filtered_sorted;
      std::vector<std::string> filter_sorted;
      std::vector<bool> filled_sorted;
      std::vector<float> correction_sorted;
      std::vector<FEATURE_EIC> eic_sorted;
      std::vector<FEATURE_MS1> ms1_sorted;
      std::vector<FEATURE_MS2> ms2_sorted;
      std::vector<FEATURE_QUALITY> quality_sorted;
      std::vector<FEATURE_ANNOTATION> annotation_sorted;
      std::vector<Rcpp::List> istd_sorted;
      std::vector<Rcpp::List> suspects_sorted;
      std::vector<Rcpp::List> formulas_sorted;
      std::vector<Rcpp::List> compounds_sorted;
      
      for (size_t i = 0; i < feature.size(); i++)
      {
        feature_sorted.push_back(feature[indices[i]]);
        group_sorted.push_back(group[indices[i]]);
        rt_sorted.push_back(rt[indices[i]]);
        mz_sorted.push_back(mz[indices[i]]);
        intensity_sorted.push_back(intensity[indices[i]]);
        area_sorted.push_back(area[indices[i]]);
        rtmin_sorted.push_back(rtmin[indices[i]]);
        rtmax_sorted.push_back(rtmax[indices[i]]);
        mzmin_sorted.push_back(mzmin[indices[i]]);
        mzmax_sorted.push_back(mzmax[indices[i]]);
        mass_sorted.push_back(mass[indices[i]]);
        polarity_sorted.push_back(polarity[indices[i]]);
        adduct_sorted.push_back(adduct[indices[i]]);
        filtered_sorted.push_back(filtered[indices[i]]);
        filter_sorted.push_back(filter[indices[i]]);
        filled_sorted.push_back(filled[indices[i]]);
        correction_sorted.push_back(correction[indices[i]]);
        eic_sorted.push_back(eic[indices[i]]);
        ms1_sorted.push_back(ms1[indices[i]]);
        ms2_sorted.push_back(ms2[indices[i]]);
        quality_sorted.push_back(quality[indices[i]]);
        annotation_sorted.push_back(annotation[indices[i]]);
        istd_sorted.push_back(istd[indices[i]]);
        suspects_sorted.push_back(suspects[indices[i]]);
        formulas_sorted.push_back(formulas[indices[i]]);
        compounds_sorted.push_back(compounds[indices[i]]);
      }
      
      feature = feature_sorted;
      group = group_sorted;
      rt = rt_sorted;
      mz = mz_sorted;
      intensity = intensity_sorted;
      area = area_sorted;
      rtmin = rtmin_sorted;
      rtmax = rtmax_sorted;
      mzmin = mzmin_sorted;
      mzmax = mzmax_sorted;
      mass = mass_sorted;
      polarity = polarity_sorted;
      adduct = adduct_sorted;
      filtered = filtered_sorted;
      filter = filter_sorted;
      filled = filled_sorted;
      correction = correction_sorted;
      eic = eic_sorted;
      ms1 = ms1_sorted;
      ms2 = ms2_sorted;
      quality = quality_sorted;
      annotation = annotation_sorted;
      istd = istd_sorted;
      suspects = suspects_sorted;
      formulas = formulas_sorted;
      compounds = compounds_sorted;
    };
  };
  
  struct NTS_DATA
  {
    std::vector<std::string> analyses;
    std::vector<std::string> replicates;
    std::vector<std::string> blanks;
    std::vector<std::string> files;
    std::vector<sc::MS_SPECTRA_HEADERS> headers;
    std::vector<FEATURES> features;
    bool valid = false;
    
    NTS_DATA(const Rcpp::List &info,
             const Rcpp::List &spectra_headers,
             const Rcpp::List &feature_list)
    {
      
      std::vector<std::string> info_must_have_names = {
        "analysis", "replicate", "blank", "file"
      };
      
      if (!check_must_have_names_list(info, info_must_have_names))
      {
        Rcpp::Rcout << "Error: NTS_DATA::NTS_DATA() - missing required names in the list." << std::endl;
        return;
      }
      
      const std::vector<std::string> &analyses_names = Rcpp::as<std::vector<std::string>>(info["analysis"]);
      const std::vector<std::string> &replicates_names = Rcpp::as<std::vector<std::string>>(info["replicate"]);
      const std::vector<std::string> &blanks_names = Rcpp::as<std::vector<std::string>>(info["blank"]);
      const std::vector<std::string> &analyses_files = Rcpp::as<std::vector<std::string>>(info["file"]);
      
      analyses = analyses_names;
      replicates = replicates_names;
      blanks = blanks_names;
      files = analyses_files;
      
      const int number_analyses = analyses.size();
      
      if (number_analyses == 0)
      {
        Rcpp::Rcout << "Error: No analyses given!" << std::endl;
        return;
      }
      
      const int number_files = files.size();
      
      if (number_analyses != number_files)
      {
        Rcpp::Rcout << "Error: Files not of the same same as analyses!" << std::endl;
        return;
      }
      
      const int number_features_analyses = feature_list.size();
      
      if (number_features_analyses != number_analyses)
      {
        Rcpp::Rcout << "Error: Feature list not of the same size as analyses!" << std::endl;
        return;
      }
      
      const int number_headers_analyses = spectra_headers.size();
      
      if (number_headers_analyses != number_analyses)
      {
        Rcpp::Rcout << "Error: Spectra headers not of the same size as analyses!" << std::endl;
        return;
      }
      
      std::vector<std::string> names_features = feature_list.names();
      
      for (int i = 0; i < number_analyses; i++)
      {
        if (analyses[i] != names_features[i])
        {
          Rcpp::Rcout << "Error: Feature list names not matching analyses!" << std::endl;
          return;
        }
      }
      
      std::vector<std::string> names_headers = spectra_headers.names();
      
      for (int i = 0; i < number_analyses; i++)
      {
        if (analyses[i] != names_headers[i])
        {
          Rcpp::Rcout << "Error: Spectra headers names not matching analyses!" << std::endl;
          return;
        }
      }
      
      headers.resize(number_analyses);
      features.resize(number_analyses);
      
      bool valid_features = true;
      
      for (int i = 0; i < number_analyses; i++)
      {
        const Rcpp::List &header_ref = Rcpp::as<Rcpp::List>(spectra_headers[i]);
        const Rcpp::List &feature_ref = Rcpp::as<Rcpp::List>(feature_list[i]);
        
        headers[i] = as_MS_SPECTRA_HEADERS(header_ref);
        features[i].import_from_list(analyses[i], feature_ref);
        
        if (!features[i].valid)
        {
          valid_features = false;
          Rcpp::Rcout << "Error: Features not valid!" << std::endl;
          break;
        }
      }
      
      if (!valid_features)
        return;
      
      valid = true;
    };
    
    int size() const
    {
      return analyses.size();
    };
    
    Rcpp::List features_as_list_of_dt() const
    {
      const int n = features.size();
      Rcpp::List out(n);
      
      if (n == 0 || !valid)
      {
        return out;
      }
      
      for (int i = 0; i < n; i++)
      {
        out[i] = features[i].to_list_dt();
      }
      
      Rcpp::CharacterVector names(n);
      for (int i = 0; i < n; i++)
      {
        names[i] = analyses[i];
      }
      out.attr("names") = names;
      
      return out;
    };
    
    std::vector<std::vector<int>> get_polarities() const
    {
      std::vector<std::vector<int>> out;
      
      if (features.size() == 0 || !valid)
      {
        return out;
      }
      
      for (const FEATURES &fts : features)
      {
        std::vector<int> polarities(fts.polarity);
        std::set<int> pols_set(polarities.begin(), polarities.end());
        std::vector<int> pols_unique(pols_set.begin(), pols_set.end());
        out.push_back(pols_unique);
      }
      return out;
    };
  };
  
  // MARK: ANNOTATION_ISOTOPE
  struct ANNOTATION_ISOTOPE
  {
    std::string element;
    std::string isotope;
    float mass_distance;
    float abundance;
    float abundance_monoisotopic;
    int min;
    int max;

    ANNOTATION_ISOTOPE(const std::string &element,
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

  // MARK: ANNOTATION_ISOTOPE_SET
  struct ANNOTATION_ISOTOPE_SET
  {
    std::vector<ANNOTATION_ISOTOPE> data = {
        ANNOTATION_ISOTOPE("C", "13C", 1.0033548378, 0.01078, 0.988922, 1, 100),
        ANNOTATION_ISOTOPE("H", "2H", 1.0062767, 0.00015574, 0.99984426, 2, 100),
        ANNOTATION_ISOTOPE("N", "15N", 0.9970349, 0.003663, 0.996337, 1, 15),
        ANNOTATION_ISOTOPE("O", "17O", 1.004217, 0.00037, 0.99763, 1, 15),
        ANNOTATION_ISOTOPE("O", "18O", 2.004246, 0.00200, 0.99763, 1, 15),
        ANNOTATION_ISOTOPE("S", "33S", 0.999388, 0.00750, 0.95018, 1, 10),
        ANNOTATION_ISOTOPE("S", "34S", 1.995796, 0.04215, 0.95018, 1, 10),
        ANNOTATION_ISOTOPE("S", "36S", 3.995010, 0.00017, 0.95018, 1, 10),
        ANNOTATION_ISOTOPE("Cl", "37Cl", 1.997050, 0.24229, 0.75771, 1, 10),
        ANNOTATION_ISOTOPE("Br", "81Br", 1.997953, 0.49314, 0.50686, 1, 10),
        ANNOTATION_ISOTOPE("Si", "29Si", 0.999568, 0.04683, 0.92230, 1, 10),
        ANNOTATION_ISOTOPE("Si", "30Si", 1.996844, 0.03087, 0.92230, 1, 10)
        // ANNOTATION_ISOTOPE("Ge", "72Ge", 1.997828, 0.27662, 0.21234, 1, 10),
        // ANNOTATION_ISOTOPE("Ge", "73Ge", 2.999212, 0.07717, 0.21234, 1, 10),
        // ANNOTATION_ISOTOPE("Ge", "74Ge", 3.996930, 0.35943, 0.21234, 1, 10),
        // ANNOTATION_ISOTOPE("Ge", "76Ge", 5.997155, 0.07444, 0.21234, 1, 10)
    };

    std::vector<std::string> elements()
    {
      std::vector<std::string> out;
      for (const ANNOTATION_ISOTOPE &iso : data)
      {
        out.push_back(iso.element);
      }
      return out;
    };

    std::vector<std::string> isotopes()
    {
      std::vector<std::string> out;
      for (const ANNOTATION_ISOTOPE &iso : data)
      {
        out.push_back(iso.isotope);
      }
      return out;
    };

    std::vector<float> mass_distance()
    {
      std::vector<float> out;
      for (const ANNOTATION_ISOTOPE &iso : data)
      {
        out.push_back(iso.mass_distance);
      }
      return out;
    };

    std::vector<float> abundance()
    {
      std::vector<float> out;
      for (const ANNOTATION_ISOTOPE &iso : data)
      {
        out.push_back(iso.abundance);
      }
      return out;
    };

    std::vector<float> abundance_monoisotopic()
    {
      std::vector<float> out;
      for (const ANNOTATION_ISOTOPE &iso : data)
      {
        out.push_back(iso.abundance_monoisotopic);
      }
      return out;
    };

    std::vector<int> min()
    {
      std::vector<int> out;
      for (const ANNOTATION_ISOTOPE &iso : data)
      {
        out.push_back(iso.min);
      }
      return out;
    };

    std::vector<int> max()
    {
      std::vector<int> out;
      for (const ANNOTATION_ISOTOPE &iso : data)
      {
        out.push_back(iso.max);
      }
      return out;
    };

    void filter(const std::vector<std::string> &el)
    {
      std::unordered_set<std::string> el_set(el.begin(), el.end());
      std::vector<ANNOTATION_ISOTOPE> data_filtered;
      for (const ANNOTATION_ISOTOPE &iso : data)
      {
        if (el_set.find(iso.element) != el_set.end())
        {
          data_filtered.push_back(iso);
        }
      }
      data = data_filtered;
    };
  };
  
  // MARK: ANNOTATION_ISOTOPE_COMBINATIONS
  struct ANNOTATION_ISOTOPE_COMBINATIONS
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

    ANNOTATION_ISOTOPE_COMBINATIONS(ANNOTATION_ISOTOPE_SET isotopes, const int &max_number_elements)
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
      std::stable_sort(order_idx.begin(), order_idx.end(), [&](int i, int j) { return mass_distances_unordered[i] < mass_distances_unordered[j]; });

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
  
  // MARK: ANNOTATION_ISOTOPE_CHAIN
  struct ANNOTATION_ISOTOPE_CHAIN
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
    
    ANNOTATION_ISOTOPE_CHAIN(const int &z, FEATURE mono_ion, float mono_mzr)
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
    };
  };
  
  // MARK: ANNOTATION_ADDUCT
  struct ANNOTATION_ADDUCT
  {
    std::string element;
    int polarity;
    std::string cat;
    int charge;
    float mass_distance;
    
    ANNOTATION_ADDUCT(const std::string &e, const int &p, const std::string &c, const float &md, const int &z)
    {
      element = e;
      polarity = p;
      cat = c;
      charge = z;
      mass_distance = md;
    };
  };
  
  // MARK: ANNOTATION_ADDUCT_SET
  struct ANNOTATION_ADDUCT_SET
  {
    
    std::vector<ANNOTATION_ADDUCT> neutralizers{
      ANNOTATION_ADDUCT("H", 1, "[M+H]+", -1.007276, 1),
      ANNOTATION_ADDUCT("H", -1, "[M-H]-", 1.007276, 1)};
    
    std::vector<ANNOTATION_ADDUCT> all_adducts{
      // Positive Adducts
      ANNOTATION_ADDUCT("Na", 1, "[M+Na]+", 22.989218, 1),
      ANNOTATION_ADDUCT("K", 1, "[M+K]+", 38.963158, 1),
      ANNOTATION_ADDUCT("NH4", 1, "[M+NH4]+", 18.033823, 1),
      // ANNOTATION_ADDUCT("CH3OH", 1, "[M+CH3OH+H]+", 33.033489, 1), // Methanol
      ANNOTATION_ADDUCT("DMSO", 1, "[M+DMSO+H]+", 79.02122, 1),    // Dimethyl sulfoxide
      ANNOTATION_ADDUCT("CH3CN", 1, "[M+CH3CN+H]+", 42.033823, 1), // Acetonitrile
      // Negative Adducts
      ANNOTATION_ADDUCT("Cl", -1, "[M+Cl]-", 34.969402, 1),
      ANNOTATION_ADDUCT("Br", -1, "[M+Br]-", 78.918885, 1),
      ANNOTATION_ADDUCT("CHO2", -1, "[M+CHO2]-", 44.998201, 1),     // Formate
      ANNOTATION_ADDUCT("CH3COO", -1, "[M+CH3COO]-", 59.013851, 1), // Acetate
      ANNOTATION_ADDUCT("-2H+Na", -1, "[M-2H+Na]-", 20.974666, 1),
      ANNOTATION_ADDUCT("-2H+K", -1, "[M-2H+K]-", 36.948606, 1)
    };
    
    float neutralizer(const int &pol)
    {
      if (pol == 1)
      {
        return neutralizers[0].mass_distance;
      }
      return neutralizers[1].mass_distance;
    };
    
    std::vector<ANNOTATION_ADDUCT> adducts(const int &pol)
    {
      std::vector<ANNOTATION_ADDUCT> out;
      
      if (pol == 1)
      {
        for (const ANNOTATION_ADDUCT &a : all_adducts)
        {
          if (a.polarity == 1)
          {
            out.push_back(a);
          }
        }
      }

      if (pol == -1)
      {
        for (const ANNOTATION_ADDUCT &a : all_adducts)
        {
          if (a.polarity == -1)
          {
            out.push_back(a);
          }
        }
      }
      
      return out;
    };
  };
  
  // MARK: ANNOTATION_CANDIDATE_CHAIN
  struct ANNOTATION_CANDIDATE_CHAIN
  {
    std::vector<FEATURE> chain;
    std::vector<int> indices;
    
    void clear()
    {
      chain.clear();
      indices.clear();
    };
    
    int size() const
    {
      return chain.size();
    };
    
    void sort_by_mz()
    {
      if (chain.size() == 0)
        return;
      
      std::vector<int> new_order(chain.size());
      std::iota(new_order.begin(), new_order.end(), 0);
      
      std::sort(new_order.begin(), new_order.end(), [this](int i1, int i2) { return chain[i1].mz < chain[i2].mz; });
      
      std::vector<FEATURE> chain_sorted;
      std::vector<int> indices_sorted;
      
      for (size_t i = 0; i < chain.size(); i++)
      {
        chain_sorted.push_back(chain[new_order[i]]);
        indices_sorted.push_back(new_order[i]);
      }
      
      chain = chain_sorted;
      indices = indices_sorted;
    };
    
    std::vector<float> get_chain_mzr() const
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
    };
    
    float get_max_mzr() const
    {
      if (chain.size() == 0)
        return 0.0;
      
      std::vector<float> mzr = this->get_chain_mzr();
      float max_mzr = *std::max_element(mzr.begin(), mzr.end());
      return max_mzr;
    };
    
    void find_isotopic_candidates(const FEATURE &ft,
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
    };
    
    void annotate_isotopes(const NTS::ANNOTATION_ISOTOPE_COMBINATIONS &combinations,
                           const int &maxIsotopes,
                           const int &maxCharge,
                           const int &maxGaps);
    
    void find_adduct_candidates(const FEATURE &ft,
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
    };
    
    void annotate_adducts();
  };

}; // namespace NTS

#endif
