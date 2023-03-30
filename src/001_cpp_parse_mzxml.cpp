#include <iostream>
#include "001_rcpp_parse_mzxml.h"
#include <string>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List parse_msAnalysis_from_mzxml(std::string file_path) {

  const char * path = file_path.c_str();

  Rcpp::List list_out;

  Rcpp::Rcout << "\n \u2699 Parsing data...\n\n";

  pugi::xml_document doc;

  pugi::xml_parse_result result = doc.load_file(path);

  if (result) {

    Rcpp::Rcout << "Load result: " << result.description() << std::endl;

    pugi::xml_node mzml = doc.first_child().first_child();

    Rcpp::Rcout << "Name First: " << mzml.name() << std::endl;

    std::string search_softwareList = "//softwareList";
    pugi::xpath_node xpathNode = doc.select_node(search_softwareList.c_str());
    Rcpp::Rcout << "Soft name: " << xpathNode.node().name() << std::endl;

    for (pugi::xml_node soft: xpathNode.node().children("software"))
    {
      std::cout << "Software:";

      for (pugi::xml_attribute attr: soft.attributes())
      {
        std::cout << " " << attr.name() << "=" << attr.value();
      }

      for (pugi::xml_node child: soft.children())
      {
        std::cout << ", child " << child.name();
      }

      std::cout << std::endl;
    }

    // Rcpp::Rcout << "First node value: " << instrument.attribute("name").value() << "\n";


    // for (pugi::xml_node_iterator it = instrument.begin(); it != instrument.end(); ++it) {
    //   Rcpp::Rcout << "Instrument entries:";
    //
    //   for (pugi::xml_attribute_iterator ait = it->attributes_begin();
    //        ait != it->attributes_end(); ++ait)
    //   {
    //     Rcpp::Rcout << " " << ait->name() <<
    //       "=" << ait->value();
    //   }
    //
    //   Rcpp::Rcout << std::endl;
    // }


    // inst_x <- "//d1:msInstrument/child::node()[starts-with(name(), 'ms')]"
    // inst_n <- xml_find_all(xml_data, xpath = inst_x)
    // if (length(inst_n) > 0) {
    //   inst_names <- xml_attr(inst_n, "category")
    //   inst_vals <- xml_attr(inst_n, "value")
    // } else {
    //   inst_names <- "instrument_data"
    //   inst_vals <- NA_character_
    // }
    // instrument <- data.frame(name = inst_names, value = inst_vals)
    //   instrument <- split(instrument, instrument$name)
    //   instrument <- lapply(instrument, function(x) x$value)
















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
