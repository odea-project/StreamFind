#include <vector>
#include <string>
#include <Rcpp.h>

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
    // hdl["id"] = hd.id;
    hdl["scan"] = hd.scan;
    hdl["array_length"] = hd.array_length;
    hdl["level"] = hd.level;
    hdl["mode"] = hd.mode;
    hdl["polarity"] = hd.polarity;
    hdl["lowmz"] = hd.lowmz;
    hdl["highmz"] = hd.highmz;
    hdl["bpmz"] = hd.bpmz;
    hdl["bpint"] = hd.bpint;
    hdl["tic"] = hd.tic;
    // hdl["title"] = hd.title;
    hdl["rt"] = hd.rt;
    hdl["mobility"] = hd.mobility;
    // hdl["filter_string"] = hd.filter_string;
    // hdl["config"] = hd.config;
    // hdl["injection_ion_time"] = hd.injection_ion_time;
    // hdl["pre_scan"] = hd.precursor_mz;
    hdl["window_mz"] = hd.window_mz;
    hdl["pre_mzlow"] = hd.window_mzlow;
    hdl["pre_mzhigh"] = hd.window_mzhigh;
    hdl["pre_mz"] = hd.precursor_mz;
    hdl["pre_charge"] = hd.precursor_charge;
    hdl["pre_intensity"] = hd.precursor_intensity;
    // hdl["pre_type"] = hd.activation_type;
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
    // hdl2["activation_type"] = hd2.activation_type;
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
  // list_out["id"] = headers.id;
  list_out["scan"] = headers.scan;
  list_out["array_length"] = headers.array_length;
  list_out["level"] = headers.level;
  list_out["mode"] = headers.mode;
  list_out["polarity"] = headers.polarity;
  list_out["lowmz"] = headers.lowmz;
  list_out["highmz"] = headers.highmz;
  list_out["bpmz"] = headers.bpmz;
  list_out["bpint"] = headers.bpint;
  list_out["tic"] = headers.tic;
  // list_out["title"] = headers.title;
  list_out["rt"] = headers.rt;
  list_out["mobility"] = headers.mobility;
  // list_out["filter_string"] = headers.filter_string;
  // list_out["config"] = headers.config;
  // list_out["injection_ion_time"] = headers.injection_ion_time;
  // list_out["pre_scan"] = headers.precursor_scan;
  list_out["window_mz"] = headers.window_mz;
  list_out["pre_mzlow"] = headers.window_mzlow;
  list_out["pre_mzhigh"] = headers.window_mzhigh;
  // list_out["pre_scan"] = headers.precursor_scan;
  list_out["pre_mz"] = headers.precursor_mz;
  list_out["pre_charge"] = headers.precursor_charge;
  list_out["pre_intensity"] = headers.precursor_intensity;
  // list_out["activation_type"] = headers.activation_type;
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
  // list_out["activation_type"] = headers.activation_type;
  list_out["pre_ce"] = headers.activation_ce;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra(Rcpp::List analysis,
                                 std::vector<int> levels,
                                 Rcpp::DataFrame targets,
                                 double minIntensityMS1,
                                 double minIntensityMS2) {
  
  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List out;
  
  const std::string file = analysis["file"];
  
  const Rcpp::List& hd = analysis["spectra_headers"];
  
  const std::vector<int> polarity = hd["polarity"];
  const std::vector<int> level = hd["level"];
  const std::vector<double> pre_mz = hd["pre_mz"];
  // const std::vector<double> pre_mzlow = hd["pre_mzlow"];
  // const std::vector<double> pre_mzhigh = hd["pre_mzhigh"];
  const std::vector<double> pre_ce = hd["pre_ce"];
  const std::vector<double> rt = hd["rt"];
  const std::vector<double> mobility = hd["mobility"];
  
  const double minIntLv1 = minIntensityMS1;
  const double minIntLv2 = minIntensityMS2;

  const int number_spectra = rt.size();
  
  const int number_levels = levels.size();
  
  if (number_spectra == 0) return empty_df;
  
  std::vector<bool> level_filter(number_spectra, false);
  
  for (int i = 0; i < number_spectra; i++) {
    for (int j = 0; j < number_levels; j++) {
      if (level[i] == levels[j]) {
        level_filter[i] = true;
        break;
      }
    }
  }
  
  const int n_tg = targets.nrow();
  
  sc::MS_ANALYSIS ana(file);

  if (n_tg == 0) {
    
    std::vector<int> indices;
    
    for (int i = 0; i < number_spectra; i++) if (level_filter[i]) indices.push_back(i);
    
    const std::vector<std::vector<std::vector<double>>> spectra = ana.get_spectra(indices);
    
    const int number_spectra_extracted = spectra.size();
    
    if (number_spectra != number_spectra_extracted) return empty_df;
    
    const int number_arrays = spectra[0].size();
    
    if (number_arrays == 0) return empty_df;
    
    int total_traces = 0;
    
    for (int i = 0; i < number_spectra; i++) total_traces += spectra[i][0].size();
    
    std::vector<int> polarity_out(total_traces);
    std::vector<int> level_out(total_traces);
    std::vector<double> pre_mz_out(total_traces);
    // std::vector<double> pre_mzlow_out(total_traces);
    // std::vector<double> pre_mzhigh_out(total_traces);
    std::vector<double> pre_ce_out(total_traces);
    std::vector<double> rt_out(total_traces);
    std::vector<double> mobility_out(total_traces);
    std::vector<double> mz_out(total_traces);
    std::vector<double> intensity_out(total_traces);

    int trace = 0;
    
    for (int i = 0; i < number_spectra; i++) {
      const std::vector<double>& mz_ref = spectra[i][0];
      const std::vector<double>& intensity_ref = spectra[i][1];
      const int n = mz_ref.size();
      
      for (int k = 0; k < n; k++) {
        polarity_out[trace] = polarity[i];
        level_out[trace] = level[i];
        pre_mz_out[trace] = pre_mz[i];
        // pre_mzlow_out[trace] = pre_mzlow[i];
        // pre_mzhigh_out[trace] = pre_mzhigh[i];
        pre_ce_out[trace] = pre_ce[i];
        rt_out[trace] = rt[i];
        mobility_out[trace] = mobility[i];
        mz_out[trace] = mz_ref[k];
        intensity_out[trace] = intensity_ref[k];

        trace += 1;
      }
    }
    
    out["polarity"] = polarity_out;
    out["level"] = level_out;
    out["pre_mz"] = pre_mz_out;
    // out["pre_mzlow"] = pre_mzlow_out;
    // out["pre_mzhigh"] = pre_mzhigh_out;
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
    std::vector<int> dummy_level(n_tg);
    const std::vector<int> df_polarity = targets["polarity"];
    const std::vector<bool> df_precursor = targets["precursor"];
    const std::vector<double> df_mzmin = targets["mzmin"];
    const std::vector<double> df_mzmax = targets["mzmax"];
    const std::vector<double> df_rtmin = targets["rtmin"];
    const std::vector<double> df_rtmax = targets["rtmax"];
    const std::vector<double> df_mobilitymin = targets["mobilitymin"];
    const std::vector<double> df_mobilitymax = targets["mobilitymax"];
    
    sc::MS_TARGETS tg = {
      tg_idx,
      df_id,
      dummy_level,
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

    for (int i = 0; i < n_tg; i++) {
      for (int j = 0; j < number_spectra; j++) {

        if (level_filter[j]) {
          if ((rt[j] >= tg.rtmin[i] && rt[j] <= tg.rtmax[i]) || tg.rtmax[i] == 0) {
            if (polarity[j] == tg.polarity[i]) {
              if ((mobility[j] >= tg.mobilitymin[i] && mobility[j] <= tg.mobilitymax[i]) || tg.mobilitymax[i] == 0) {
                if (tg.precursor[i]) {
                  if ((pre_mz[j] >= tg.mzmin[i] && pre_mz[j] <= tg.mzmax[i]) || tg.mzmax[i] == 0) {
                    idx.insert(j);
                  }
                } else {
                  idx.insert(j);
                }
              }
            }
          }
        }
      }
    }

    std::vector<int> idx_vector(idx.begin(), idx.end());

    std::sort(idx_vector.begin(), idx_vector.end());
    
    // Rcpp::Rcout << "idx_vector: ";
    // for (auto it = idx.begin(); it != idx.end(); ++it) {
    //   Rcpp::Rcout << *it << " ";
    // }
    // Rcpp::Rcout << std::endl;
    
    const int number_spectra_targets = idx_vector.size();

    if (number_spectra_targets == 0) return empty_df;
    
    std::vector<std::string> id_out;
    std::vector<int> polarity_out;
    std::vector<int> level_out;
    std::vector<double> pre_mz_out;
    // std::vector<double> pre_mzlow_out;
    // std::vector<double> pre_mzhigh_out;
    std::vector<double> pre_ce_out;
    std::vector<double> rt_out;
    std::vector<double> mobility_out;
    std::vector<double> mz_out;
    std::vector<double> intensity_out;
    
    #pragma omp parallel
    {
      std::vector<std::string> id_priv;
      std::vector<int> polarity_priv;
      std::vector<int> level_priv;
      std::vector<double> pre_mz_priv;
      // std::vector<double> pre_mzlow_priv;
      // std::vector<double> pre_mzhigh_priv;
      std::vector<double> pre_ce_priv;
      std::vector<double> rt_priv;
      std::vector<double> mobility_priv;
      std::vector<double> mz_priv;
      std::vector<double> intensity_priv;
    
      #pragma omp for
      for (int i = 0; i < number_spectra_targets; i++) {
        
        const std::vector<int> i_idx = { idx_vector[i] };
        
        std::vector<std::vector<std::vector<double>>> spectra = ana.get_spectra(i_idx);
        
        const int n_traces = spectra[0][1].size();
        
        if (n_traces == 0) continue;
        
        const int& i_polarity = polarity[i_idx[0]];
        const int& i_level = level[i_idx[0]];
        const double& i_pre_mz = pre_mz[i_idx[0]];
        // const double& i_pre_mzlow = pre_mzlow[i_idx[0]];
        // const double& i_pre_mzhigh = pre_mzhigh[i_idx[0]];
        const double& i_rt = rt[i_idx[0]];
        const double& i_mobility = mobility[i_idx[0]];
        
        for (int j = 0; j < n_tg; j++) {
          
          if (tg.polarity[j] == i_polarity) {
            
            if (tg.rtmax[j] == 0 || (i_rt >= tg.rtmin[j] && i_rt <= tg.rtmax[j])) {
              
              if (tg.mobilitymax[j] == 0 || (i_mobility >= tg.mobilitymin[j] && i_mobility <= tg.mobilitymax[j])) {
                
                if (tg.precursor[j]) {
                  
                  if ((i_pre_mz >= tg.mzmin[j] && i_pre_mz <= tg.mzmax[j]) || tg.mzmax[j] == 0) {
                    
                    for (int k = 0; k < n_traces; k++) {
                      
                      if (spectra[0][1][k] >= minIntLv2 && i_level == 2) {
                        id_priv.push_back(tg.id[j]);
                        polarity_priv.push_back(i_polarity);
                        level_priv.push_back(i_level);
                        pre_mz_priv.push_back(i_pre_mz);
                        // pre_mzlow_priv.push_back(i_pre_mzlow);
                        // pre_mzhigh_priv.push_back(i_pre_mzhigh);
                        pre_ce_priv.push_back(pre_ce[i_idx[0]]);
                        rt_priv.push_back(i_rt);
                        mobility_priv.push_back(i_mobility);
                        mz_priv.push_back(spectra[0][0][k]);
                        intensity_priv.push_back(spectra[0][1][k]);
                      }
                    }
                  }
                } else {
                  
                  for (int k = 0; k < n_traces; k++) {
                    
                    if ((spectra[0][0][k] >= tg.mzmin[j] && spectra[0][0][k] <= tg.mzmax[j]) || tg.mzmax[j] == 0) {
                      
                      if ((spectra[0][1][k] >= minIntLv2 && i_level == 2) || (spectra[0][1][k] >= minIntLv1 && i_level == 1)) {
                        id_priv.push_back(tg.id[j]);
                        polarity_priv.push_back(i_polarity);
                        level_priv.push_back(i_level);
                        pre_mz_priv.push_back(i_pre_mz);
                        // pre_mzlow_priv.push_back(i_pre_mzlow);
                        // pre_mzhigh_priv.push_back(i_pre_mzhigh);
                        pre_ce_priv.push_back(pre_ce[i_idx[0]]);
                        rt_priv.push_back(i_rt);
                        mobility_priv.push_back(i_mobility);
                        mz_priv.push_back(spectra[0][0][k]);
                        intensity_priv.push_back(spectra[0][1][k]);
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      #pragma omp critical
      {
        id_out.insert(id_out.end(), id_priv.begin(), id_priv.end());
        polarity_out.insert(polarity_out.end(), polarity_priv.begin(), polarity_priv.end());
        level_out.insert(level_out.end(), level_priv.begin(), level_priv.end());
        pre_mz_out.insert(pre_mz_out.end(), pre_mz_priv.begin(), pre_mz_priv.end());
        // pre_mzlow_out.insert(pre_mzlow_out.end(), pre_mzlow_priv.begin(), pre_mzlow_priv.end());
        // pre_mzhigh_out.insert(pre_mzhigh_out.end(), pre_mzhigh_priv.begin(), pre_mzhigh_priv.end());
        pre_ce_out.insert(pre_ce_out.end(), pre_ce_priv.begin(), pre_ce_priv.end());
        rt_out.insert(rt_out.end(), rt_priv.begin(), rt_priv.end());
        mobility_out.insert(mobility_out.end(), mobility_priv.begin(), mobility_priv.end());
        mz_out.insert(mz_out.end(), mz_priv.begin(), mz_priv.end());
        intensity_out.insert(intensity_out.end(), intensity_priv.begin(), intensity_priv.end());
      }
    }

    out["id"] = id_out;
    out["polarity"] = polarity_out;
    out["level"] = level_out;
    out["pre_mz"] = pre_mz_out;
    // out["pre_mzlow"] = pre_mzlow_out;
    // out["pre_mzhigh"] = pre_mzhigh_out;
    out["pre_ce"] = pre_ce_out;
    out["rt"] = rt_out;
    out["mobility"] = mobility_out;
    out["mz"] = mz_out;
    out["intensity"] = intensity_out;

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
  const std::vector<double> pre_mz = hd["pre_mz"];
  const std::vector<double> pre_ce = hd["pre_ce"];
  const std::vector<double> pro_mz = hd["pro_mz"];
  
  const int number_chromatograms = index.size();
  
  if (number_chromatograms == 0) return empty_df;
  
  sc::MS_ANALYSIS ana(file);
  
  if (idx.size() == 0) idx = index;
  
  const std::vector<std::vector<std::vector<double>>> chromatograms = ana.get_chromatograms(idx);
  
  const int number_extracted_chromatograms = chromatograms.size();
  
  if (number_extracted_chromatograms == 0) return empty_df;
  
  int total_traces = 0;
  
  for (int i = 0; i < number_extracted_chromatograms; i++) total_traces += chromatograms[i][0].size();
  
  if (total_traces == 0) return empty_df;
    
  std::vector<int> index_out(total_traces);
  std::vector<std::string> id_out(total_traces);
  std::vector<int> polarity_out(total_traces);
  std::vector<double> pre_mz_out(total_traces);
  std::vector<double> pre_ce_out(total_traces);
  std::vector<double> pro_mz_out(total_traces);
  std::vector<double> rt_out(total_traces);
  std::vector<double> intensity_out(total_traces);
    
  int trace = 0;
    
  for (int i = 0; i < number_extracted_chromatograms; i++) {
    
    const std::vector<double>& rtj = chromatograms[i][0];
    const std::vector<double>& intj = chromatograms[i][1];
 
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
