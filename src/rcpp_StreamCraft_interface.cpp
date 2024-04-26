#include <vector>
#include <string>
#include <Rcpp.h>

#define STREAMCRAFT_HEADER_ONLY
#include "StreamCraft/src/StreamCraft_lib.hpp"

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis_v2(std::string file_path) {
  
  Rcpp::List list_out;
  
  Rcpp::CharacterVector na_charvec(1, NA_STRING);
  
  Rcpp::DataFrame empty_df;
  
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List empty_list;
  
  sc::MS_ANALYSIS ana = file_path;
  
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
    hdl["drift"] = hd.drift;
    // hdl["filter_string"] = hd.filter_string;
    // hdl["config"] = hd.config;
    // hdl["injection_ion_time"] = hd.injection_ion_time;
    hdl["pre_scan"] = hd.precursor_scan;
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
    hdl2["precursor_mz"] = hd2.precursor_mz;
    hdl2["product_mz"] = hd2.product_mz;
    hdl2["activation_type"] = hd2.activation_type;
    hdl2["activation_ce"] = hd2.activation_ce;
    
    hdl2.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    
    list_out["chromatograms_headers"] = hdl2;
    
  } else {
    list_out["chromatograms_headers"] = empty_df;
  }
  
  list_out["chromatograms"] = empty_df;
  
  list_out["features_eic"] = empty_list;
  
  list_out["features"] = empty_df;
  
  list_out["metadata"] = empty_list;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("MassSpecAnalysis", "Analysis");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra_headers_v2(std::string file_path) {
  
  Rcpp::List list_out;
  
  sc::MS_ANALYSIS ana = file_path;
  
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
  list_out["drift"] = headers.drift;
  // list_out["filter_string"] = headers.filter_string;
  // list_out["config"] = headers.config;
  // list_out["injection_ion_time"] = headers.injection_ion_time;
  list_out["pre_scan"] = headers.precursor_scan;
  list_out["window_mz"] = headers.window_mz;
  list_out["pre_mzlow"] = headers.window_mzlow;
  list_out["pre_mzhigh"] = headers.window_mzhigh;
  list_out["pre_scan"] = headers.precursor_scan;
  list_out["pre_mz"] = headers.precursor_mz;
  list_out["precursor_charge"] = headers.precursor_charge;
  list_out["precursor_intensity"] = headers.precursor_intensity;
  // list_out["activation_type"] = headers.activation_type;
  list_out["pre_ce"] = headers.activation_ce;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_chromatograms_headers_v2(std::string file_path) {
  
  Rcpp::List list_out;
  
  sc::MS_ANALYSIS ana = file_path;
  
  if (ana.get_number_chromatograms() == 0) return list_out;
  
  sc::MS_CHROMATOGRAMS_HEADERS headers = ana.get_chromatograms_headers();
  
  list_out["index"] = headers.index;
  list_out["id"] = headers.id;
  list_out["array_length"] = headers.array_length;
  list_out["polarity"] = headers.polarity;
  list_out["precursor_mz"] = headers.precursor_mz;
  list_out["product_mz"] = headers.product_mz;
  list_out["activation_type"] = headers.activation_type;
  list_out["activation_ce"] = headers.activation_ce;
  
  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra_v2(Rcpp::List analysis,
                                    std::vector<int> levels,
                                    Rcpp::DataFrame targets,
                                    double minIntensityMS1,
                                    double minIntensityMS2) {
  
  // levels, targets, minIntensityMS1, minIntensityMS2
  
  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::List out;
  
  const std::string& file = analysis["file"];
  
  const Rcpp::List& hd = analysis["spectra_headers"];
  
  const std::vector<int>& polarity = hd["polarity"];
  const std::vector<int>& level = hd["level"];
  const std::vector<double>& pre_mz = hd["pre_mz"];
  const std::vector<double>& pre_mzlow = hd["pre_mzlow"];
  const std::vector<double>& pre_mzhigh = hd["pre_mzhigh"];
  const std::vector<double>& pre_ce = hd["pre_ce"];
  const std::vector<double>& rt = hd["rt"];
  const std::vector<double>& drift = hd["drift"];

  const int number_spectra = rt.size();
  
  std::vector<bool> filter(level.size(), false);
  
  for (int i = 0; i < number_spectra; i++) {
    if (std::find(levels.begin(), levels.end(), level[i]) != levels.end()) {
      filter[i] = true;
    }
  }
  
  sc::MS_ANALYSIS ana = file;

  if (targets.nrow() == 0) {
    
    std::vector<int> indices;
    
    for (int i = 0; i < number_spectra; i++) if (filter[i]) indices.push_back(i);
    
    const std::vector<std::vector<std::vector<double>>> spectra = ana.get_spectra(indices);
    
    const int number_spectra_extracted = spectra.size();
    
    if (number_spectra != number_spectra_extracted) return empty_df;
    
    const int number_arrays = spectra[0].size();
    
    if (number_arrays == 0) return empty_df;
    
    int total_traces = 0;
    
    for (int i = 0; i < number_spectra; i++) total_traces += spectra[i][0].size();
    
    // add elements to the output list to hold the total_traces from each [i][j] spectra array
    
    std::vector<int> polarity_out(total_traces);
    std::vector<int> level_out(total_traces);
    std::vector<double> pre_mz_out(total_traces);
    std::vector<double> pre_mzlow_out(total_traces);
    std::vector<double> pre_mzhigh_out(total_traces);
    std::vector<double> pre_ce_out(total_traces);
    std::vector<double> rt_out(total_traces);
    std::vector<double> drift_out(total_traces);
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
        pre_mzlow_out[trace] = pre_mzlow[i];
        pre_mzhigh_out[trace] = pre_mzhigh[i];
        pre_ce_out[trace] = pre_ce[i];
        rt_out[trace] = rt[i];
        drift_out[trace] = drift[i];
        mz_out[trace] = mz_ref[k];
        intensity_out[trace] = intensity_ref[k];

        trace += 1;
      }
    }
    
    out["polarity"] = polarity_out;
    out["level"] = level_out;
    out["pre_mz"] = pre_mz_out;
    out["pre_mzlow"] = pre_mzlow_out;
    out["pre_mzhigh"] = pre_mzhigh_out;
    out["pre_ce"] = pre_ce_out;
    out["rt"] = rt_out;
    out["drift"] = drift_out;
    out["mz"] = mz_out;
    out["intensity"] = intensity_out;
    
    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
    
  } else {
    
    
    
    
    
    
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  // collapse the nested spectra into a data frame adding the rt for each level 1 spectra
  
  
  
  
  
  // if (result) {
  //   pugi::xml_node root = doc.document_element();
  //   
  //   Rcpp::List run = analysis["spectra_headers"];
  //   Rcpp::IntegerVector scan = run["scan"];
  //   
  //   if (scan.size() == 0) {
  //     xml_utils::spectraHeaders headers = xml_utils::parse_spectra_headers(root);
  //     Rcpp::List run = xml_utils::spectraHeaders_to_list(headers);
  //     Rcpp::IntegerVector scan = run["scan"];
  //   }
  //   
  //   if (scan.size() > 0) {
  //     const Rcpp::IntegerVector& polarity = run["polarity"];
  //     const Rcpp::IntegerVector& level = run["level"];
  //     const Rcpp::IntegerVector& pre_scan = run["pre_scan"];
  //     const Rcpp::NumericVector& pre_mz = run["pre_mz"];
  //     const Rcpp::NumericVector& pre_mzlow = run["pre_mzlow"];
  //     const Rcpp::NumericVector& pre_mzhigh = run["pre_mzhigh"];
  //     const Rcpp::NumericVector& pre_ce = run["pre_ce"];
  //     const Rcpp::NumericVector& rt = run["rt"];
  //     const Rcpp::NumericVector& drift = run["drift"];
  //     
  //     Rcpp::ListOf<Rcpp::NumericMatrix> bins;
  //     
  //     // returns all spectra when index is NA
  //     if (Rcpp::IntegerVector::is_na(index[0])) {
  //       
  //       index = run["index"];
  //       bins = xml_utils::parse_spectra(root);
  //       
  //       // return only selected scans, by adapting the index vector
  //     } else {
  //       bins = xml_utils::parse_partial_spectra(root, index);
  //     }
  //     
  //     int number_spectra = bins.size();
  //     
  //     if (number_spectra > 0) {
  //       
  //       int n_cols_bin = 0;
  //       
  //       for (int c = 0; c < number_spectra; c++) {
  //         n_cols_bin = bins[c].ncol();
  //         if (n_cols_bin > 0) {
  //           break;
  //         }
  //       }
  //       
  //       if (n_cols_bin == 0) return empty_df;
  //       
  //       int n_cols = n_cols_bin + 11;
  //       
  //       int n_rows = 0;
  //       for (int i = 0; i < number_spectra; i++) {
  //         n_rows += bins[i].nrow();
  //       }
  //       
  //       Rcpp::NumericMatrix out_mat(n_rows, n_cols_bin);
  //       
  //       Rcpp::IntegerVector index_out(n_rows);
  //       Rcpp::IntegerVector scan_out(n_rows);
  //       Rcpp::IntegerVector polarity_out(n_rows);
  //       Rcpp::IntegerVector level_out(n_rows);
  //       Rcpp::IntegerVector pre_scan_out(n_rows);
  //       Rcpp::NumericVector pre_mz_out(n_rows);
  //       Rcpp::NumericVector pre_mzlow_out(n_rows);
  //       Rcpp::NumericVector pre_mzhigh_out(n_rows);
  //       Rcpp::NumericVector pre_ce_out(n_rows);
  //       Rcpp::NumericVector rt_out(n_rows);
  //       Rcpp::NumericVector drift_out(n_rows);
  //       
  //       // bool has_ion_mobility = run["has_ion_mobility"];
  //       // Rcpp::NumericVector drift_out;
  //       // 
  //       // if (has_ion_mobility) {
  //       //   
  //       // }
  //       
  //       int b = 0;
  //       int rowOffset = 0;
  //       
  //       for (int i : index) {
  //         
  //         Rcpp::NumericMatrix bin = bins[b];
  //         
  //         int bin_rows = bin.nrow();
  //         
  //         if (bin_rows > 0) {
  //           for (int r = 0; r < bin_rows; r++) {
  //             
  //             int offset = rowOffset + r;
  //             
  //             index_out[offset] = i;
  //             scan_out[offset] = scan[i];
  //             polarity_out[offset] = polarity[i];
  //             level_out[offset] = level[i];
  //             pre_scan_out[offset] = pre_scan[i];
  //             pre_ce_out[offset] = pre_ce[i];
  //             pre_mz_out[offset] = pre_mz[i];
  //             pre_mzlow_out[offset] = pre_mzlow[i];
  //             pre_mzhigh_out[offset] = pre_mzhigh[i];
  //             rt_out[offset] = rt[i];
  //             drift_out[offset] = drift[i];
  //             
  //             for (int j = 0; j < n_cols_bin; j++) {
  //               out_mat(offset, j) = bin(r, j);
  //             }
  //           }
  //           
  //           rowOffset += bin_rows;
  //         }
  //         b += 1;
  //       }
  //       
  //       Rcpp::List output(n_cols);
  //       
  //       output[0] = index_out;
  //       output[1] = scan_out;
  //       output[2] = polarity_out;
  //       output[3] = level_out;
  //       output[4] = pre_scan_out;
  //       output[5] = pre_ce_out;
  //       output[6] = pre_mz_out;
  //       output[7] = pre_mzlow_out;
  //       output[8] = pre_mzhigh_out;
  //       output[9] = rt_out;
  //       output[10] = drift_out;
  //       
  //       for (int i = 0; i < n_cols_bin; ++i) {
  //         output[i + 11] = out_mat(Rcpp::_, i);
  //       }
  //       
  //       const Rcpp::CharacterVector cols = {
  //         "index",
  //         "scan",
  //         "polarity",
  //         "level",
  //         "pre_scan",
  //         "pre_ce",
  //         "pre_mz",
  //         "pre_mzlow",
  //         "pre_mzhigh",
  //         "rt",
  //         "drift",
  //         "mz",
  //         "intensity"
  //       };
  //       
  //       output.attr("names") = cols;
  //       
  //       output.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  //       
  //       return output;
  //       
  //     } else {
  //       Rcpp::Rcout << "\u2717 Requested spectra not found! " << std::endl;
  //     }
  //   } else {
  //     Rcpp::Rcout << "\u2717 File does not have spectra! " << std::endl;
  //   }
  // } else {
  //   Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  // }
  
  return empty_df;
}
