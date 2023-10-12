#include <iostream>
#include "xml_utils.h"
#include <string>
#include <vector>
#include <list>
#include <Rcpp.h>
#include <cstdio>
#include <cstring>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <zlib.h>
#include <algorithm>
#include <iterator>



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

