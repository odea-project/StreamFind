#include <iostream>
#include "pugixml.h"
#include "xml_utils.h"
#include <string>
#include <vector>
#include <Rcpp.h>

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
