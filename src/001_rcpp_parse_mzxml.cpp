#include <iostream>
#include "001_rcpp_parse_mzxml.h"
#include <string>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_parse_mzxml_files(std::string file_path) {

  const char * path = file_path.c_str();

  Rcpp::List list_out;

  Rcpp::Rcout << "\nParsing data.....\n\n";

  pugi::xml_document doc;

  pugi::xml_parse_result result = doc.load_file(path);

  if (result) {
    Rcpp::Rcout << "\nParsed! \n";
  }

  // load the XML file
  // if (!doc.load_file(file_path)) return -1;
  //
  // xml_node tools = doc.child("msRun");
  //
  //
  //
  // for (xml_node_iterator it = tools.begin(); it != tools.end(); ++it)
  // {
  //   cout << "Employees:";
  //
  //   for (xml_attribute_iterator ait = it->attributes_begin();
  //        ait != it->attributes_end(); ++ait)
  //   {
  //     cout << " " << ait->name() <<
  //       "=" << ait->value();
  //   }
  //
  //   cout << endl;
  // }
  //
  // cout << endl;
  return list_out;
}
