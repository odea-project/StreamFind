#include <iostream>
#include "xml_utils.h"
#include <string>
#include <vector>
#include <Rcpp.h>
#include <list>
#include <cstdio>
#include <cstring>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <zlib.h>
#include <algorithm>
#include <iterator>





// [[Rcpp::export]]
Rcpp::List rcpp_parse_spectra_headers(std::string file_path) {
  
  Rcpp::List list_out;
  
  const char * path = file_path.c_str();
  pugi::xml_document doc;
  pugi::xml_parse_result result = doc.load_file(path);
  
  if (result) {
    
    pugi::xml_node root = doc.document_element();
    
    xml_utils::spectraHeaders headers_cpp = xml_utils::parse_spectra_headers(root);
    
    return xml_utils::spectraHeaders_to_list(headers_cpp);
    
  } else {
    Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }
  
  return list_out;
}





// [[Rcpp::export]]
Rcpp::List rcpp_parse_spectra(std::string file_path, Rcpp::IntegerVector index = NA_INTEGER) {
  
  Rcpp::List list_output;
  
  const char * path = file_path.c_str();
  
  pugi::xml_document doc;
  
  pugi::xml_parse_result result = doc.load_file(path);
  
  if (result) {
    pugi::xml_node root = doc.document_element();
    
    if (Rcpp::IntegerVector::is_na(index[0])) {
      return xml_utils::parse_spectra(root);
      
    } else {
      return xml_utils::parse_partial_spectra(root, index);
    }
    
  } else {
    Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }
  
  return list_output;
}





// [[Rcpp::export]]
Rcpp::List rcpp_parse_chromatograms_headers(std::string file_path) {
  
  Rcpp::List list_out;
  
  const char * path = file_path.c_str();
  pugi::xml_document doc;
  pugi::xml_parse_result result = doc.load_file(path);
  
  if (result) {
    
    pugi::xml_node root = doc.document_element();
    
    xml_utils::chromatogramsHeaders headers_cpp = xml_utils::parse_chromatograms_headers(root);
    
    return xml_utils::chromatogramsHeaders_to_list(headers_cpp);
    
  } else {
    Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }
  
  return list_out;
}





// [[Rcpp::export]]
Rcpp::List rcpp_parse_chromatograms(std::string file_path, Rcpp::IntegerVector index = NA_INTEGER) {
  
  Rcpp::List list_output;
  
  const char * path = file_path.c_str();
  
  pugi::xml_document doc;
  
  pugi::xml_parse_result result = doc.load_file(path);
  
  if (result) {
    pugi::xml_node root = doc.document_element();
    
    if (Rcpp::IntegerVector::is_na(index[0])) {
      return xml_utils::parse_chromatograms(root);
      
    } else {
      return xml_utils::parse_partial_chromatograms(root, index);
    }
    
  } else {
    Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }
  
  return list_output;
}





// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis(std::string file_path) {
  
  Rcpp::List list_out;
  
  const char * path = file_path.c_str();
  pugi::xml_document doc;
  pugi::xml_parse_result result = doc.load_file(path);
  
  if (result) {
    
    pugi::xml_node root = doc.document_element();
    
    std::string root_name = root.name();
    
    pugi::xml_node node_in;
    
    if (strcmp("indexedmzML", root_name.c_str()) == 0) {
      node_in = root.child("mzML");
    } else if (strcmp("mzXML", root_name.c_str()) == 0) {
      node_in = root;
    } else {
      Rcpp::Rcout << "\u2717 The file must be in valid mzML or mzXML format!" << std::endl;
    }
    
    if (node_in != NULL) {
      
      std::size_t lastSeparator = file_path.find_last_of("/\\");
      std::size_t lastDot = file_path.find_last_of(".");
      std::string file_name = file_path.substr(lastSeparator + 1, lastDot - lastSeparator - 1);
      
      Rcpp::CharacterVector na_charvec(1, NA_STRING);
      
      list_out["name"] = file_name;
      
      list_out["replicate"] = na_charvec;
      
      list_out["blank"] = na_charvec;
      
      list_out["file"] = file_path;
      
      std::string file_format = node_in.name();
      
      list_out["format"] = file_format;
      
      xml_utils::spectraHeaders spec_headers_cpp = xml_utils::parse_spectra_headers(root);
      
      xml_utils::chromatogramsHeaders chrom_headers_cpp = xml_utils::parse_chromatograms_headers(root);
      
      xml_utils::runSummary summary = xml_utils::run_summary(spec_headers_cpp, chrom_headers_cpp);

      list_out["type"] = summary.type;
      
      std::string search_run = "//run";
      pugi::xpath_node xps_run = root.select_node(search_run.c_str());
      Rcpp::String time_stamp = xps_run.node().attribute("startTimeStamp").as_string();
      
      if (time_stamp == "") {
        list_out["time_stamp"] = na_charvec;
      } else {
        list_out["time_stamp"] = time_stamp;
      }
      
      list_out["spectra_number"] = summary.spectra_number;
      
      list_out["spectra_mode"] = Rcpp::wrap(summary.mode);
      
      list_out["spectra_levels"] = summary.levels;
      
      list_out["mz_low"] = summary.mz_low;
      
      list_out["mz_high"] = summary.mz_high;
      
      list_out["rt_start"] = summary.rt_start;
      
      list_out["rt_end"] = summary.rt_end;
      
      list_out["polarity"] = Rcpp::wrap(summary.polarity);
      
      list_out["has_ion_mobility"] = summary.has_ion_mobility;
      
      list_out["chromatograms_number"] = summary.chromatograms_number;
      
      list_out["software"] = xml_utils::parse_software(root);
      
      list_out["instrument"] = xml_utils::parse_instrument(root);
      
      list_out["run"] = xml_utils::spectraHeaders_to_list(spec_headers_cpp);
      
      Rcpp::DataFrame empty_df;
      empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      
      Rcpp::List empty_list;
      
      list_out["spectra"] = empty_df;
      
      list_out["chromatograms"] = empty_df;
      
      list_out["features_eic"] = empty_list;
      
      list_out["features"] = empty_df;
      
      list_out["metadata"] = empty_list;
      
      list_out.attr("class") = Rcpp::CharacterVector::create("MassSpecAnalysis");
      
    } else {
      Rcpp::Rcout << "\u2717 MS file not conform!" << std::endl;
    }
    
  } else {
    Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }
  
  return list_out;
}





// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis_spectra(Rcpp::List analysis, Rcpp::IntegerVector index = NA_INTEGER) {
  
  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::String path_str = analysis["file"];
  
  const char * path = path_str.get_cstring();
  
  pugi::xml_document doc;
  
  pugi::xml_parse_result result = doc.load_file(path);
  
  if (result) {
    pugi::xml_node root = doc.document_element();
    
    Rcpp::List run = analysis["run"];
    Rcpp::IntegerVector scan = run["scan"];
    
    if (scan.size() == 0) {
      xml_utils::spectraHeaders headers = xml_utils::parse_spectra_headers(root);
      Rcpp::List run = xml_utils::spectraHeaders_to_list(headers);
      Rcpp::IntegerVector scan = run["scan"];
    }
    
    if (scan.size() > 0) {
      const Rcpp::IntegerVector& polarity = run["polarity"];
      const Rcpp::IntegerVector& level = run["level"];
      const Rcpp::IntegerVector& pre_scan = run["pre_scan"];
      const Rcpp::NumericVector& pre_mz = run["pre_mz"];
      const Rcpp::NumericVector& pre_mzlow = run["pre_mzlow"];
      const Rcpp::NumericVector& pre_mzhigh = run["pre_mzhigh"];
      const Rcpp::NumericVector& pre_ce = run["pre_ce"];
      const Rcpp::NumericVector& rt = run["rt"];
      const Rcpp::NumericVector& drift = run["drift"];
      
      Rcpp::ListOf<Rcpp::NumericMatrix> bins;
      
      // returns all spectra when index is NA
      if (Rcpp::IntegerVector::is_na(index[0])) {
        
        index = run["index"];
        bins = xml_utils::parse_spectra(root);
        
        // return only selected scans, by adapting the index vector
      } else {
        bins = xml_utils::parse_partial_spectra(root, index);
      }
      
      int number_spectra = bins.size();
      
      if (number_spectra > 0) {
        
        int n_cols_bin = 0;
        
        for (int c = 0; c < number_spectra; c++) {
          n_cols_bin = bins[c].ncol();
          if (n_cols_bin > 0) {
            break;
          }
        }
        
        if (n_cols_bin == 0) return empty_df;
        
        int n_cols = n_cols_bin + 11;
        
        int n_rows = 0;
        for (int i = 0; i < number_spectra; i++) {
          n_rows += bins[i].nrow();
        }
        
        Rcpp::NumericMatrix out_mat(n_rows, n_cols_bin);
        
        Rcpp::IntegerVector index_out(n_rows);
        Rcpp::IntegerVector scan_out(n_rows);
        Rcpp::IntegerVector polarity_out(n_rows);
        Rcpp::IntegerVector level_out(n_rows);
        Rcpp::IntegerVector pre_scan_out(n_rows);
        Rcpp::NumericVector pre_mz_out(n_rows);
        Rcpp::NumericVector pre_mzlow_out(n_rows);
        Rcpp::NumericVector pre_mzhigh_out(n_rows);
        Rcpp::NumericVector pre_ce_out(n_rows);
        Rcpp::NumericVector rt_out(n_rows);
        Rcpp::NumericVector drift_out(n_rows);
        
        // bool has_ion_mobility = run["has_ion_mobility"];
        // Rcpp::NumericVector drift_out;
        // 
        // if (has_ion_mobility) {
        //   
        // }
        
        int b = 0;
        int rowOffset = 0;
        
        for (int i : index) {
          
          Rcpp::NumericMatrix bin = bins[b];
          
          int bin_rows = bin.nrow();
          
          if (bin_rows > 0) {
            for (int r = 0; r < bin_rows; r++) {
              
              int offset = rowOffset + r;
              
              index_out[offset] = i;
              scan_out[offset] = scan[i];
              polarity_out[offset] = polarity[i];
              level_out[offset] = level[i];
              pre_scan_out[offset] = pre_scan[i];
              pre_ce_out[offset] = pre_ce[i];
              pre_mz_out[offset] = pre_mz[i];
              pre_mzlow_out[offset] = pre_mzlow[i];
              pre_mzhigh_out[offset] = pre_mzhigh[i];
              rt_out[offset] = rt[i];
              drift_out[offset] = drift[i];
              
              for (int j = 0; j < n_cols_bin; j++) {
                out_mat(offset, j) = bin(r, j);
              }
            }
            
            rowOffset += bin_rows;
          }
          b += 1;
        }
        
        Rcpp::List output(n_cols);
        
        output[0] = index_out;
        output[1] = scan_out;
        output[2] = polarity_out;
        output[3] = level_out;
        output[4] = pre_scan_out;
        output[5] = pre_ce_out;
        output[6] = pre_mz_out;
        output[7] = pre_mzlow_out;
        output[8] = pre_mzhigh_out;
        output[9] = rt_out;
        output[10] = drift_out;
        
        for (int i = 0; i < n_cols_bin; ++i) {
          output[i + 11] = out_mat(Rcpp::_, i);
        }
        
        const Rcpp::CharacterVector cols = {
          "index",
          "scan",
          "polarity",
          "level",
          "pre_scan",
          "pre_ce",
          "pre_mz",
          "pre_mzlow",
          "pre_mzhigh",
          "rt",
          "drift",
          "mz",
          "intensity"
        };
        
        output.attr("names") = cols;
        
        output.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        
        return output;
        
      } else {
        Rcpp::Rcout << "\u2717 Requested spectra not found! " << std::endl;
      }
    } else {
      Rcpp::Rcout << "\u2717 File does not have spectra! " << std::endl;
    }
  } else {
    Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }
  
  return empty_df;
}





// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis_chromatograms(Rcpp::List analysis, Rcpp::IntegerVector index = NA_INTEGER) {
  
  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  Rcpp::String path_str = analysis["file"];
  
  const char * path = path_str.get_cstring();
  
  pugi::xml_document doc;
  
  pugi::xml_parse_result result = doc.load_file(path);
  
  if (result) {
    pugi::xml_node root = doc.document_element();
    
    xml_utils::chromatogramsHeaders headers = xml_utils::parse_chromatograms_headers(root);
    Rcpp::List run = xml_utils::chromatogramsHeaders_to_list(headers);
    
    if (headers.index.size() > 0) {
      
      const Rcpp::CharacterVector& id = run["id"];
      const Rcpp::CharacterVector& polarity = run["polarity"];
      const Rcpp::NumericVector& pre_mz = run["pre_mz"];
      const Rcpp::NumericVector& pre_ce = run["pre_ce"];
      const Rcpp::NumericVector& pro_mz = run["pro_mz"];
      
      Rcpp::ListOf<Rcpp::NumericMatrix> bins;
      
      // returns all spectra when index is NA
      if (Rcpp::IntegerVector::is_na(index[0])) {
        index = run["index"];
        bins = xml_utils::parse_chromatograms(root);
        
        // return only selected scans, by adapting the index vector
      } else {
        bins = xml_utils::parse_partial_chromatograms(root, index);
      }
      
      int number_chroms = bins.size();
      
      if (number_chroms > 0) {
        
        int n_cols_bin = bins[0].ncol();
        
        int n_cols = n_cols_bin + 6;
        
        int n_rows = 0;
        for (int i = 0; i < number_chroms; i++) {
          n_rows += bins[i].nrow();
        }
        
        Rcpp::NumericMatrix out_mat(n_rows, n_cols_bin);
        
        Rcpp::IntegerVector index_out(n_rows);
        Rcpp::CharacterVector id_out(n_rows);
        Rcpp::CharacterVector polarity_out(n_rows);
        Rcpp::NumericVector pre_mz_out(n_rows);
        Rcpp::NumericVector pre_ce_out(n_rows);
        Rcpp::NumericVector pro_mz_out(n_rows);
        
        int b = 0;
        int rowOffset = 0;
        
        for (int i : index) {
          
          Rcpp::NumericMatrix bin = bins[b];
          
          int bin_rows = bin.nrow();
          int bin_cols = bin.ncol();
          
          if (bin_rows > 0) {
            for (int r = 0; r < bin_rows; r++) {
              
              int offset = rowOffset + r;
              
              index_out[offset] = i;
              id_out(offset) = id(i);
              polarity_out(offset) = polarity(i);
              pre_ce_out[offset] = pre_ce[i];
              pre_mz_out[offset] = pre_mz[i];
              pro_mz_out[offset] = pro_mz[i];
              
              
              for (int j = 0; j < bin_cols; j++) {
                out_mat(offset, j) = bin(r, j);
              }
            }
            
            rowOffset += bin_rows;
          }
          b += 1;
        }
        
        Rcpp::List output(n_cols);
        
        output[0] = index_out;
        output[1] = id_out;
        output[2] = polarity_out;
        output[3] = pre_ce_out;
        output[4] = pre_mz_out;
        output[5] = pro_mz_out;
        
        for (int i = 0; i < n_cols_bin; ++i) {
          output[i + 6] = out_mat(Rcpp::_, i);
        }
        
        Rcpp::CharacterVector cols = {"index", "id", "polarity", "pre_ce", "pre_mz", "pro_mz"};
        Rcpp::CharacterVector cols_bins = Rcpp::colnames(bins[0]);
        
        for (const auto& el : cols_bins) {
          cols.push_back(el);
        }
        
        output.attr("names") = cols;
        
        
        output.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        
        return output;
        
      } else {
        Rcpp::Rcout << "\u2717 Requested chromatograms not found! " << std::endl;
      }
    } else {
      Rcpp::Rcout << "\u2717 File does not have chromatograms! " << std::endl;
    }
  } else {
    Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }
  
  return empty_df;
}
