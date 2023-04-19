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
Rcpp::List rcpp_parse_msAnalysis_chromatograms(Rcpp::List analysis, Rcpp::IntegerVector index = NA_INTEGER) {

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
        std::cout << "\u2717 Requested chromatograms not found! " << std::endl;
      }
    } else {
      std::cout << "\u2717 File does not have chromatograms! " << std::endl;
    }
  } else {
    std::cout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }

  return empty_df;
}

