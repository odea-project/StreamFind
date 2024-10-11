#include <vector>
#include <string>
#include <Rcpp.h>
#include <omp.h>
#include <unordered_map>
#include <cmath>
#include <algorithm>
#include <filesystem>

#define STREAMCRAFT_HEADER_ONLY
#include "StreamCraft/src/StreamCraft_lib.hpp"

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis(std::string file_path) {
  
  Rcpp::List list_out;
  
  Rcpp::CharacterVector na_charvec(1, NA_STRING);
  
  Rcpp::DataFrame empty_df;
  
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List empty_list;
  
  sc::MS_ANALYSIS ana(file_path);
  
  list_out["name"] = ana.file_name;
  
  list_out["replicate"] = na_charvec;
  
  list_out["blank"] = na_charvec;
  
  list_out["file"] = ana.file_path;
  
  list_out["format"] = ana.get_format();
  
  list_out["type"] = ana.get_type();
  
  list_out["spectra_number"] = ana.get_number_spectra();
  
  if (ana.get_number_spectra() > 0) {
    sc::MS_SPECTRA_HEADERS hd = ana.get_spectra_headers();
    
    Rcpp::List hdl;
    
    hdl["index"] = hd.index;
    hdl["scan"] = hd.scan;
    hdl["array_length"] = hd.array_length;
    hdl["level"] = hd.level;
    hdl["mode"] = hd.mode;
    hdl["polarity"] = hd.polarity;
    hdl["configuration"] = hd.configuration;
    hdl["lowmz"] = hd.lowmz;
    hdl["highmz"] = hd.highmz;
    hdl["bpmz"] = hd.bpmz;
    hdl["bpint"] = hd.bpint;
    hdl["tic"] = hd.tic;
    hdl["rt"] = hd.rt;
    hdl["mobility"] = hd.mobility;
    hdl["window_mz"] = hd.window_mz;
    hdl["pre_mzlow"] = hd.window_mzlow;
    hdl["pre_mzhigh"] = hd.window_mzhigh;
    hdl["pre_mz"] = hd.precursor_mz;
    hdl["pre_charge"] = hd.precursor_charge;
    hdl["pre_intensity"] = hd.precursor_intensity;
    hdl["pre_ce"] = hd.activation_ce;
    
    hdl.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    
    list_out["spectra_headers"] = hdl;
    
  } else {
    list_out["spectra_headers"] = empty_df;
  }
  
  list_out["spectra"] = empty_df;
  
  list_out["chromatograms_number"] = ana.get_number_chromatograms();
  
  if (ana.get_number_chromatograms() > 0) {
    
    sc::MS_CHROMATOGRAMS_HEADERS hd2 = ana.get_chromatograms_headers();
    
    Rcpp::List hdl2;
    
    hdl2["index"] = hd2.index;
    hdl2["id"] = hd2.id;
    hdl2["array_length"] = hd2.array_length;
    hdl2["polarity"] = hd2.polarity;
    hdl2["pre_mz"] = hd2.precursor_mz;
    hdl2["pro_mz"] = hd2.product_mz;
    hdl2["pre_ce"] = hd2.activation_ce;
    
    hdl2.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    
    list_out["chromatograms_headers"] = hdl2;
    
  } else {
    list_out["chromatograms_headers"] = empty_df;
  }
  
  list_out["chromatograms"] = empty_df;
  
  list_out["metadata"] = empty_list;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("MassSpecAnalysis", "Analysis");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra_headers(std::string file_path) {
  
  Rcpp::List list_out;
  
  sc::MS_ANALYSIS ana(file_path);
  
  if (ana.get_number_spectra() == 0) return list_out;
  
  sc::MS_SPECTRA_HEADERS headers = ana.get_spectra_headers();
  
  list_out["index"] = headers.index;
  list_out["scan"] = headers.scan;
  list_out["array_length"] = headers.array_length;
  list_out["level"] = headers.level;
  list_out["mode"] = headers.mode;
  list_out["polarity"] = headers.polarity;
  list_out["configuration"] = headers.configuration;
  list_out["lowmz"] = headers.lowmz;
  list_out["highmz"] = headers.highmz;
  list_out["bpmz"] = headers.bpmz;
  list_out["bpint"] = headers.bpint;
  list_out["tic"] = headers.tic;
  list_out["rt"] = headers.rt;
  list_out["mobility"] = headers.mobility;
  list_out["window_mz"] = headers.window_mz;
  list_out["pre_mzlow"] = headers.window_mzlow;
  list_out["pre_mzhigh"] = headers.window_mzhigh;
  list_out["pre_mz"] = headers.precursor_mz;
  list_out["pre_charge"] = headers.precursor_charge;
  list_out["pre_intensity"] = headers.precursor_intensity;
  list_out["pre_ce"] = headers.activation_ce;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_chromatograms_headers(std::string file_path) {
  
  Rcpp::List list_out;
  
  sc::MS_ANALYSIS ana(file_path);
  
  if (ana.get_number_chromatograms() == 0) return list_out;
  
  sc::MS_CHROMATOGRAMS_HEADERS headers = ana.get_chromatograms_headers();
  
  list_out["index"] = headers.index;
  list_out["id"] = headers.id;
  list_out["array_length"] = headers.array_length;
  list_out["polarity"] = headers.polarity;
  list_out["pre_mz"] = headers.precursor_mz;
  list_out["pro_mz"] = headers.product_mz;
  list_out["pre_ce"] = headers.activation_ce;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra(Rcpp::List analysis,
                                 std::vector<int> levels,
                                 Rcpp::DataFrame targets,
                                 float minIntensityMS1,
                                 float minIntensityMS2) {
  
  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List out;
  
  const std::string file = analysis["file"];
  
  const Rcpp::List& hd = analysis["spectra_headers"];
  
  const std::vector<float>& rt = hd["rt"];
  
  const float minIntLv1 = minIntensityMS1;
  const float minIntLv2 = minIntensityMS2;

  const int number_spectra = rt.size();
  
  const int number_levels = levels.size();
  
  if (number_spectra == 0) return empty_df;
  
  const int n_tg = targets.nrow();
  
  sc::MS_ANALYSIS ana(file);

  if (n_tg == 0) {
    
    const std::vector<int>& polarity = hd["polarity"];
    const std::vector<int>& configuration = hd["configuration"];
    const std::vector<int>& level = hd["level"];
    const std::vector<float>& pre_mz = hd["pre_mz"];
    const std::vector<float>& pre_ce = hd["pre_ce"];
    const std::vector<float>& mobility = hd["mobility"];
    
    std::vector<bool> spectra_filter(number_spectra, false);
    
    for (int i = 0; i < number_spectra; i++) {
      for (int j = 0; j < number_levels; j++) {
        
        if (configuration[i] >= 3) break;
        
        if (level[i] == levels[j]) {
          spectra_filter[i] = true;
          break;
        }
      }
    }
    
    std::vector<int> indices;
    
    for (int i = 0; i < number_spectra; i++) if (spectra_filter[i]) indices.push_back(i);
    
    const std::vector<std::vector<std::vector<float>>> spectra = ana.get_spectra(indices);
    
    const int number_spectra_extracted = spectra.size();
    
    const int number_arrays = spectra[0].size();
    
    if (number_arrays == 0) return empty_df;
    
    int total_traces = 0;
    
    for (int i = 0; i < number_spectra_extracted; i++) {
      const int n = spectra[i][0].size(); 
      total_traces += n;
    }
    
    std::vector<int> polarity_out(total_traces);
    std::vector<int> level_out(total_traces);
    std::vector<float> pre_mz_out(total_traces);
    std::vector<float> pre_ce_out(total_traces);
    std::vector<float> rt_out(total_traces);
    std::vector<float> mobility_out(total_traces);
    std::vector<float> mz_out(total_traces);
    std::vector<float> intensity_out(total_traces);

    int trace = 0;
    for (int i = 0; i < number_spectra_extracted; i++) {
      const std::vector<float>& mz_ref = spectra[i][0];
      const std::vector<float>& intensity_ref = spectra[i][1];
      const int n = mz_ref.size();
      
      for (int k = 0; k < n; k++) {
        polarity_out[trace] = polarity[indices[i]];
        level_out[trace] = level[indices[i]];
        pre_mz_out[trace] = pre_mz[indices[i]];
        pre_ce_out[trace] = pre_ce[indices[i]];
        rt_out[trace] = rt[indices[i]];
        mobility_out[trace] = mobility[indices[i]];
        mz_out[trace] = mz_ref[k];
        intensity_out[trace] = intensity_ref[k];
        trace += 1;
      }
    }
    
    out["polarity"] = polarity_out;
    out["level"] = level_out;
    out["pre_mz"] = pre_mz_out;
    out["pre_ce"] = pre_ce_out;
    out["rt"] = rt_out;
    out["mobility"] = mobility_out;
    out["mz"] = mz_out;
    out["intensity"] = intensity_out;
    
    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
    
  } else {
    
    std::vector<int> tg_idx(n_tg);
    std::iota(tg_idx.begin(), tg_idx.end(), 0);
    
    const std::vector<std::string> df_id = targets["id"];
    
    int tg_level = 0;
    if (number_levels == 1) tg_level = levels[0];
    std::vector<int> df_level(n_tg, tg_level);
    Rcpp::CharacterVector tg_col_names = targets.names();
    if (std::find(tg_col_names.begin(), tg_col_names.end(), "level") != tg_col_names.end()) {
      std::vector<int> temp_df_level = targets["level"];
      for (int i = 0; i < n_tg; i++) df_level[i] = temp_df_level[i];
    }
    
    const std::vector<int> df_polarity = targets["polarity"];
    const std::vector<bool> df_precursor = targets["precursor"];
    const std::vector<float> df_mzmin = targets["mzmin"];
    const std::vector<float> df_mzmax = targets["mzmax"];
    const std::vector<float> df_rtmin = targets["rtmin"];
    const std::vector<float> df_rtmax = targets["rtmax"];
    const std::vector<float> df_mobilitymin = targets["mobilitymin"];
    const std::vector<float> df_mobilitymax = targets["mobilitymax"];
    
    sc::MS_TARGETS tg = {
      tg_idx,
      df_id,
      df_level,
      df_polarity,
      df_precursor,
      df_mzmin,
      df_mzmax,
      df_rtmin,
      df_rtmax,
      df_mobilitymin,
      df_mobilitymax
    };

    std::set<int> idx;
    
    const std::vector<int>& hd_index = hd["index"];
    const std::vector<int>& hd_polarity = hd["polarity"];
    const std::vector<int>& hd_configuration = hd["configuration"];
    const std::vector<int>& hd_level = hd["level"];
    const std::vector<float>& hd_pre_mz = hd["pre_mz"];
    const std::vector<float>& hd_pre_mz_low = hd["pre_mzlow"];
    const std::vector<float>& hd_pre_mz_high = hd["pre_mzhigh"];
    const std::vector<float>& hd_pre_ce = hd["pre_ce"];
    const std::vector<float>& hd_mobility = hd["mobility"];
    
    sc::MS_SPECTRA_HEADERS headers;
    headers.resize_all(number_spectra);
    
    headers.index = hd_index;
    headers.rt = rt;
    headers.polarity = hd_polarity;
    headers.configuration = hd_configuration;
    headers.level = hd_level;
    headers.precursor_mz = hd_pre_mz;
    headers.activation_ce = hd_pre_ce;
    headers.mobility = hd_mobility;
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(tg, headers, minIntLv1, minIntLv2);
    
    out["id"] = res.id;
    out["polarity"] = res.polarity;
    out["level"] = res.level;
    out["pre_mz"] = res.pre_mz;
    out["pre_mzlow"] = res.pre_mzlow;
    out["pre_mzhigh"] = res.pre_mzhigh;
    out["pre_ce"] = res.pre_ce;
    out["rt"] = res.rt;
    out["mobility"] = res.mobility;
    out["mz"] = res.mz;
    out["intensity"] = res.intensity;

    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

    return out;
  }
  
  return empty_df;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_chromatograms(Rcpp::List analysis, std::vector<int> idx) {
  
  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List out;
  
  const std::string file = analysis["file"];
  
  const Rcpp::List& hd = analysis["chromatograms_headers"];
  
  const std::vector<int> index = hd["index"];
  const std::vector<std::string> id = hd["id"];
  const std::vector<int> polarity = hd["polarity"];
  const std::vector<float> pre_mz = hd["pre_mz"];
  const std::vector<float> pre_ce = hd["pre_ce"];
  const std::vector<float> pro_mz = hd["pro_mz"];
  
  const int number_chromatograms = index.size();
  
  if (number_chromatograms == 0) return empty_df;
  
  sc::MS_ANALYSIS ana(file);
  
  if (idx.size() == 0) idx = index;
  
  const std::vector<std::vector<std::vector<float>>> chromatograms = ana.get_chromatograms(idx);
  
  const int number_extracted_chromatograms = chromatograms.size();
  
  if (number_extracted_chromatograms == 0) return empty_df;
  
  int total_traces = 0;
  
  for (int i = 0; i < number_extracted_chromatograms; i++) total_traces += chromatograms[i][0].size();
  
  if (total_traces == 0) return empty_df;
    
  std::vector<int> index_out(total_traces);
  std::vector<std::string> id_out(total_traces);
  std::vector<int> polarity_out(total_traces);
  std::vector<float> pre_mz_out(total_traces);
  std::vector<float> pre_ce_out(total_traces);
  std::vector<float> pro_mz_out(total_traces);
  std::vector<float> rt_out(total_traces);
  std::vector<float> intensity_out(total_traces);
    
  int trace = 0;
    
  for (int i = 0; i < number_extracted_chromatograms; i++) {
    
    const std::vector<float>& rtj = chromatograms[i][0];
    const std::vector<float>& intj = chromatograms[i][1];
 
    const int n = rtj.size();
    
    if (n == 0) continue;
      
    for (int j = 0; j < n; j++) {
      
      index_out[trace] = index[i];
      id_out[trace] = id[i];
      polarity_out[trace] = polarity[i];
      pre_mz_out[trace] = pre_mz[i];
      pre_ce_out[trace] = pre_ce[i];
      pro_mz_out[trace] = pro_mz[i];
      rt_out[trace] = rtj[j];
      intensity_out[trace] = intj[j];
      
      trace += 1;
    }
  }
    
  out["index"] = index_out;
  out["id"] = id_out;
  out["polarity"] = polarity_out;
  out["pre_mz"] = pre_mz_out;
  out["pre_ce"] = pre_ce_out;
  out["pro_mz"] = pro_mz_out;
  out["rt"] = rt_out;
  out["intensity"] = intensity_out;
  
  out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return out;
}

sc::MS_SPECTRA_HEADERS get_analysis_headers(const Rcpp::List& analysis) {
  sc::MS_SPECTRA_HEADERS headers;
  const Rcpp::List& hd = analysis["spectra_headers"];
  const std::vector<int>& hd_index = hd["index"];
  const std::vector<int>& hd_polarity = hd["polarity"];
  const std::vector<int>& hd_configuration = hd["configuration"];
  const std::vector<float>& hd_rt = hd["rt"];
  const std::vector<int>& hd_level = hd["level"];
  const std::vector<float>& hd_pre_mz = hd["pre_mz"];
  const std::vector<float>& hd_pre_mz_low = hd["pre_mzlow"];
  const std::vector<float>& hd_pre_mz_high = hd["pre_mzhigh"];
  const std::vector<float>& hd_pre_ce = hd["pre_ce"];
  const std::vector<float>& hd_mobility = hd["mobility"];
  
  const int number_spectra = hd_index.size();
  headers.resize_all(number_spectra);
  
  headers.index = hd_index;
  headers.rt = hd_rt;
  headers.polarity = hd_polarity;
  headers.configuration = hd_configuration;
  headers.level = hd_level;
  headers.precursor_mz = hd_pre_mz;
  headers.activation_ce = hd_pre_ce;
  headers.mobility = hd_mobility;
  
  return headers;
}

void merge_traces_within_rt(std::vector<float>& rt, std::vector<float>& mz, std::vector<float>& intensity) {
  std::vector<float> rt_out;
  std::vector<float> mz_out;
  std::vector<float> intensity_out;
  for (size_t z = 0; z < rt.size(); z++) {
    if (std::find(rt_out.begin(), rt_out.end(), rt[z]) == rt_out.end()) {
      rt_out.push_back(rt[z]);
      mz_out.push_back(mz[z]);
      intensity_out.push_back(intensity[z]);
    } else {
      auto it = std::find(rt_out.begin(), rt_out.end(), rt[z]);
      int index = std::distance(rt_out.begin(), it);
      if (intensity[z] > intensity_out[index]) {
        mz_out[index] = mz[z];
        intensity_out[index] = intensity[z];
      }
    }
  }
  rt = rt_out;
  mz = mz_out;
  intensity = intensity_out;
}

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

float mean(const std::vector<float>& v) {
  return accumulate(v.begin(), v.end(), 0.0) / v.size();
}

float standard_deviation(const std::vector<float>& v, float mean_val) {
  float sum = 0.0;
  for (float num : v) {
    sum += pow(num - mean_val, 2);
  }
  return sqrt(sum / v.size());
}

size_t find_max_index(const std::vector<float>& v) {
  return max_element(v.begin(), v.end()) - v.begin();
}

size_t find_min_index(const std::vector<float>& v) {
  return min_element(v.begin(), v.end()) - v.begin();
}

void trim_to_equal_length_around_max_position(std::vector<float>& x, const size_t max_position) {
  const int n = x.size();
  std::vector<float> x_out(n);
  size_t n_points = std::min(max_position, x.size() - max_position - 1);
  x_out.resize(2 * n_points + 1);
  for (size_t i = 0; i < x_out.size(); ++i) {
    x_out[i] = x[max_position - n_points + i];
  }
  x = x_out;
}

float gaussian(float A, float mu, float sigma, float x) {
  return A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
  // return A * exp(-0.5 * pow((x - mu) / sigma, 2));
}

float fit_gaussian_cost_function(const std::vector<float>& x, const std::vector<float>& y, float A, float mu, float sigma) {
  float cost = 0.0;
  for (size_t i = 0; i < x.size(); ++i) {
    float y_pred = gaussian(A, mu, sigma, x[i]);
    cost += pow(y[i] - y_pred, 2);
  }
  return cost;
}

void fit_gaussian(const std::vector<float>& x, const std::vector<float>& y, float& A_fitted, float& mu_fitted, float& sigma_fitted) {
  
  // Optimization parameters
  const float learning_rate = 0.001;
  const int max_iterations = 10000;
  const float tolerance = 1e-08;
  
  for (int iter = 0; iter < max_iterations; ++iter) {
    float current_cost = fit_gaussian_cost_function(x, y, A_fitted, mu_fitted, sigma_fitted);
    
    // Numerical gradient calculation
    float grad_A = (fit_gaussian_cost_function(x, y, A_fitted + tolerance, mu_fitted, sigma_fitted) - current_cost) / tolerance;
    float grad_mu = (fit_gaussian_cost_function(x, y, A_fitted, mu_fitted + tolerance, sigma_fitted) - current_cost) / tolerance;
    float grad_sigma = (fit_gaussian_cost_function(x, y, A_fitted, mu_fitted, sigma_fitted + tolerance) - current_cost) / tolerance;
    
    // Update parameters using gradient descent
    A_fitted -= learning_rate * grad_A;
    mu_fitted -= learning_rate * grad_mu;
    sigma_fitted -= learning_rate * grad_sigma;
    
    if (sigma_fitted <= 0) {
      sigma_fitted = 2; // Set to a small positive number if non-positive
    }
    if (sigma_fitted > 1e6) { // Arbitrary upper bound for sigma
      sigma_fitted = 10;
    }
    
    // Check convergence
    if (abs(grad_A) < tolerance && abs(grad_mu) < tolerance && abs(grad_sigma) < tolerance) {
      break;
    }
  }
}

float calculate_gaussian_rsquared(const std::vector<float>& x, const std::vector<float>& y, float A, float mu, float sigma) {
  float ss_total = 0.0;
  float ss_residual = 0.0;
  float mean_y = accumulate(y.begin(), y.end(), 0.0) / y.size();
  
  for (size_t i = 0; i < x.size(); ++i) {
    float y_pred = gaussian(A, mu, sigma, x[i]);
    ss_residual += pow(y[i] - y_pred, 2);
    ss_total += pow(y[i] - mean_y, 2);
  }
  return 1 - (ss_residual / ss_total);
}

Rcpp::List calculate_gaussian_fit(const std::string& ft,
                                  const std::vector<float>& rt,
                                  const std::vector<float>& intensity,
                                  const float& baseCut) {
  
  float noise = 0;
  float sn = 0;
  float A_fitted = 0;
  float mu_fitted = 0;
  float sigma_fitted = 0;
  float r_squared = 0;
  
  Rcpp::List quality = Rcpp::List::create(
    Rcpp::Named("feature") = ft,
    Rcpp::Named("noise") = noise,
    Rcpp::Named("sn") = sn,
    Rcpp::Named("gauss_a") = A_fitted,
    Rcpp::Named("gauss_u") = mu_fitted,
    Rcpp::Named("gauss_s") = sigma_fitted,
    Rcpp::Named("gauss_f") = r_squared
  );
  
  size_t max_position = find_max_index(intensity);
  const float max_intensity = intensity[max_position];
  
  const size_t min_position = find_min_index(intensity);
  noise = intensity[min_position];
  sn = max_intensity / noise;
  noise = round(noise);
  sn = round(sn * 10) / 10;
  quality["noise"] = noise;
  quality["sn"] = sn;
  
  const float low_cut = max_intensity * baseCut;
  
  const int n = rt.size();
  
  std::vector<float> rt_trimmed(n);
  std::vector<float> int_trimmed(n);
  
  for (int z = 0; z < n; z++) {
    int_trimmed[z] = intensity[z] - low_cut;
    rt_trimmed[z] = rt[z];
  }
  
  auto it_int_trimmed = int_trimmed.begin();
  auto it_rt_trimmed = rt_trimmed.begin();
  
  while (it_int_trimmed != int_trimmed.end()) {
    if (*it_int_trimmed <= 0) {
      it_int_trimmed = int_trimmed.erase(it_int_trimmed);
      it_rt_trimmed = rt_trimmed.erase(it_rt_trimmed);
    } else {
      ++it_int_trimmed;
      ++it_rt_trimmed;
    }
  }
  
  int n_trimmed = int_trimmed.size();
  
  if (n_trimmed < 3) return quality;
  
  max_position = find_max_index(int_trimmed);
  
  trim_to_equal_length_around_max_position(rt_trimmed, max_position);
  trim_to_equal_length_around_max_position(int_trimmed, max_position);
  
  n_trimmed = rt_trimmed.size();
  
  if (n_trimmed < 3) return quality;
  
  max_position = find_max_index(int_trimmed);
  
  A_fitted = int_trimmed[max_position];
  mu_fitted = rt_trimmed[max_position];
  sigma_fitted = (rt_trimmed.back() - rt_trimmed.front()) / 4.0;
  
  fit_gaussian(rt_trimmed, int_trimmed, A_fitted, mu_fitted, sigma_fitted);
  
  r_squared = calculate_gaussian_rsquared(rt_trimmed, int_trimmed, A_fitted, mu_fitted, sigma_fitted);
  
  A_fitted = round(A_fitted);
  mu_fitted = round(mu_fitted * 10) / 10;
  sigma_fitted = round(sigma_fitted * 10) / 10;
  r_squared = round(r_squared * 10000) / 10000;
  
  quality["gauss_a"] = A_fitted;
  quality["gauss_u"] = mu_fitted;
  quality["gauss_s"] = sigma_fitted;
  quality["gauss_f"] = r_squared;
  
  return quality;
}

// std::vector<float> get_fit_gaussian_vector(const std::vector<float>& x, float A, float mu, float sigma) {
//   std::vector<float> y(x.size());
//   for (size_t i = 0; i < x.size(); ++i) {
//     y[i] = gaussian(A, mu, sigma, x[i]);
//   }
//   return y;
// }

float trapezoidal_area(const std::vector<float>& x, const std::vector<float>& intensity) {
  if (x.size() != intensity.size() || x.size() < 2) {
    throw std::invalid_argument("Vectors must have the same size and contain at least two elements.");
  }
  
  float area = 0.0;
  
  // Iterate over the x and intensity vectors and apply the trapezoidal rule
  for (std::size_t i = 1; i < x.size(); ++i) {
    float dx = x[i] - x[i - 1];  // Difference between consecutive x values
    float avg_intensity = (intensity[i] + intensity[i - 1]) / 2.0;  // Average of consecutive intensities
    area += dx * avg_intensity;  // Trapezoid area for this segment
  }
  
  return area;
}

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
    
    sc::MS_SPECTRA_HEADERS headers = get_analysis_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_ANALYSIS ana(file);
    
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, headers, minTracesIntensity, 0);
    
    for (int j = 0; j < n_features; j++) {
      
      if (!filtered) if (fts_filtered[j]) continue;
      
      const std::string& id_j = fts_id[j];
      
      sc::MS_TARGETS_SPECTRA res_j = res[id_j];
      
      merge_traces_within_rt(res_j.rt, res_j.mz, res_j.intensity);
      
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
  
  int rtWindowMax_idx = find_max_index(rtWindow);
  int rtWindowMin_idx = find_min_index(rtWindow);
  int mzWindowMax_idx = find_max_index(mzWindow);
  int mzWindowMin_idx = find_min_index(mzWindow);
  
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
    
    sc::MS_SPECTRA_HEADERS headers = get_analysis_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_ANALYSIS ana(file);
    
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
    
    sc::MS_SPECTRA_HEADERS headers = get_analysis_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_ANALYSIS ana(file);
    
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
    
    const sc::MS_SPECTRA_HEADERS headers = get_analysis_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_ANALYSIS ana(file);
    
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
      
      merge_traces_within_rt(res_i.rt, res_i.mz, res_i.intensity);

      Rcpp::List quality = calculate_gaussian_fit(id_i, res_i.rt, res_i.intensity, baseCut);

      const float& sn = quality["sn"];

      const float& gauss_f = quality["gauss_f"];

      if (sn < minSignalToNoiseRatio) continue;

      if (gauss_f < minGaussianFit) continue;
      
      const float area_i = trapezoidal_area(res_i.rt, res_i.intensity);
      
      const size_t max_position = find_max_index(res_i.intensity);
 
      const float rt_i = res_i.rt[max_position];

      const float mz_i = mean(res_i.mz);

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
    
    const sc::MS_SPECTRA_HEADERS headers = get_analysis_headers(analysis);
    
    const std::string file = analysis["file"];
    
    if (!std::filesystem::exists(file)) continue;
    
    sc::MS_ANALYSIS ana(file);
    
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
        merge_traces_within_rt(rt, mz, intensity);
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

      if (n > minNumberTraces) quality = calculate_gaussian_fit(id_j, rt, intensity, baseCut);
      
      fts_quality[j] = quality;
    }
    
    features_i["eic"] = fts_eic;
    features_i["quality"] = fts_quality;
    features[i] = features_i;
  }
  
  return features;
}
