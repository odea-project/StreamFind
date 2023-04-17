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
Rcpp::List rcpp_parse_spectra(std::string file_path, Rcpp::IntegerVector which = NA_INTEGER) {

  Rcpp::List list_output;

  const char * path = file_path.c_str();

  pugi::xml_document doc;

  pugi::xml_parse_result result = doc.load_file(path);

  if (result) {
    pugi::xml_node root = doc.document_element();

    if (Rcpp::IntegerVector::is_na(which[0])) {

      return xml_utils::parse_spectra(root);

    } else {

      // Rcpp::List run = rcpp_parse_run(path);
      // Rcpp::List bins = xml_utils::parse_partial_spectra(root, index);

      return xml_utils::parse_partial_spectra(root, which);
    }

  } else {
    std::cout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }

  return list_output;
}
