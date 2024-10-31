#include <vector>
#include <string>
#include <Rcpp.h>
#include <omp.h>
#include <unordered_map>
#include <cmath>
#include <algorithm>
#include <filesystem>
#include "StreamCraft_lib.h"

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis(std::string file_path) {
  
  Rcpp::List list_out;
  
  Rcpp::CharacterVector na_charvec(1, NA_STRING);
  
  Rcpp::DataFrame empty_df;
  
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List empty_list;
  
  sc::MS_FILE ana(file_path);
  
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
};

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra_headers(std::string file_path) {
  
  Rcpp::List list_out;
  
  sc::MS_FILE ana(file_path);
  
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
};

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_chromatograms_headers(std::string file_path) {
  
  Rcpp::List list_out;
  
  sc::MS_FILE ana(file_path);
  
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
};

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
  
  sc::MS_FILE ana(file);
  
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
};

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
  
  sc::MS_FILE ana(file);
  
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
};
