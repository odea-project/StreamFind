#include <iostream>
#include "external_libraries.hpp"
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
Rcpp::List rcpp_parse_msAnalysis_spectra(Rcpp::List analysis, Rcpp::IntegerVector which = NA_INTEGER) {

  Rcpp::List list_output;

  Rcpp::String path_str = analysis["file"];

  const char * path = path_str.get_cstring();

  pugi::xml_document doc;

  pugi::xml_parse_result result = doc.load_file(path);

  if (result) {
    pugi::xml_node root = doc.document_element();

    Rcpp::List run = analysis["run"];
    xml_utils::runHeaders headers = xml_utils::list_to_runHeaders(run);
    std::vector<int> index =  headers.index;
    int number_spectra = index.size();

    if (number_spectra == 0) {
      xml_utils::runHeaders headers = xml_utils::parse_run(root);
      index =  headers.index;
      number_spectra = index.size();
    }

    if (number_spectra > 0) {

      // returns all spectra when index is NA
      if (Rcpp::IntegerVector::is_na(which[0])) {
        
        Rcpp::ListOf<Rcpp::NumericMatrix> bins = xml_utils::parse_spectra(root);

        int n_cols_bin = bins[0].ncol();

        int n_cols = n_cols_bin + 7;

        int n_rows = 0;

        for (int i = 0; i < number_spectra; i++) {
          n_rows += bins[i].nrow();
        }

        Rcpp::Rcout << n_rows << std::endl;

        Rcpp::NumericMatrix out_mat(n_rows, n_cols);

        int rowOffset = 0;

        for (int i = 0; i < number_spectra; i++) {

          Rcpp::NumericMatrix bin = bins[i];

          int bin_rows = bin.nrow();
          
          for (int r = 0; r < bin_rows; r++) {
            
            out_mat(0, rowOffset + r) = headers.index[i];
            out_mat(1, rowOffset + r) = headers.scan[i];
            out_mat(2, rowOffset + r) = headers.level[i];
            out_mat(3, rowOffset + r) = headers.pre_scan[i];
            out_mat(4, rowOffset + r) = headers.pre_ce[i];
            out_mat(5, rowOffset + r) = headers.pre_mz[i];
            out_mat(6, rowOffset + r) = headers.rt[i];

            for (int j = 0; j < n_cols_bin; j++) {
              out_mat(7 + j, rowOffset + r) = bin(r, j);
            }

          Rcpp::Rcout << out_mat(rowOffset, 1) << std::endl;

          }
          



          // add the run columns to the matrix
          // Rcpp::NumericMatrix bin_with_run(bin_rows, n_cols);

          // Rcpp::IntegerVector idx = Rcpp::rep(headers.index[i], bin_rows);
          // bin_with_run(Rcpp::_, 0) = idx;

          // Rcpp::IntegerVector scan = Rcpp::rep(headers.scan[i], bin_rows);
          // bin_with_run(Rcpp::_, 1) = scan;

          // Rcpp::IntegerVector level = Rcpp::rep(headers.level[i], bin_rows);
          // bin_with_run(Rcpp::_, 2) = level;

          // Rcpp::IntegerVector pre_scan = Rcpp::rep(headers.pre_scan[i], bin_rows);
          // bin_with_run(Rcpp::_, 3) = pre_scan;

          // Rcpp::NumericVector pre_ce = Rcpp::rep(headers.pre_ce[i], bin_rows);
          // bin_with_run(Rcpp::_, 4) = pre_ce;

          // Rcpp::NumericVector pre_mz = Rcpp::rep(headers.pre_mz[i], bin_rows);
          // bin_with_run(Rcpp::_, 5) = pre_mz;

          // Rcpp::NumericVector rt = Rcpp::rep(headers.rt[i], bin_rows);
          // bin_with_run(Rcpp::_, 6) = rt;

          






          // Rcpp::NumericMatrix new_mat = Rcpp::clone(bin_with_run);
          // new_mat.attr("dim") = Rcpp::Dimension(bin_rows, n_cols);

          

          // // copy the matrix data into the new matrix
          // std::memcpy(out_mat.begin() + i * bin_rows, bin_with_run.begin(), bin_rows * n_cols * sizeof(double));

          rowOffset += bin_rows + 1;
        }

        Rcpp::List output(n_cols);
        
        // for (int i = 0; i < n_cols; i++) {
        //   output[i] = out_mat(Rcpp::_, i);
        // }

        // const Rcpp::CharacterVector cols = {"index", "scan", "level", "pre_scan", "pre_ce", "pre_mz", "rt", "mz", "intenisty"};
        // output.attr("names") = cols;

        
        // output.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

        return output;

      } else {

        // Rcpp::List bins = xml_utils::parse_partial_spectra(root, index);


        return xml_utils::parse_partial_spectra(root, which);
      }
    } else {
      std::cout << "\u2717 File does not have spectra! " << std::endl;
    }

  } else {
    std::cout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }

  return list_output;
}

