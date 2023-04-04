#ifndef XML_UTILS_H
#define XML_UTILS_H

#include <iostream>
#include <string>
#include <vector>
#include <list>
#include "external_libraries.hpp"
#include <Rcpp.h>

namespace xml_utils {

  struct mzmlObj {
    std::vector<int> index;
    std::vector<int> scan;
    std::vector<double> rt;
    std::vector<double> drift;
    std::vector<int> level;
    std::vector<std::string> polarity;
    std::vector<std::string> mode;
    std::vector<double> mzlow;
    std::vector<double> mzhigh;
    std::vector<double> bpcmz;
    std::vector<double> bpcint;
    std::vector<double> ticint;
    std::vector<int> pre_scan;
    std::vector<double> pre_mz;
    std::vector<double> pre_loweroffset;
    std::vector<double> pre_upperoffset;
    std::vector<double> pre_ce;
  };

  std::list<std::vector<std::string>> mzml_instrument_parser(pugi::xml_node node_mzml);

  std::list<std::vector<std::string>> mzml_software_parser(pugi::xml_node node_mzml);

  std::string mzml_time_stamp_parser(pugi::xml_node node_mzml);

  mzmlObj mzml_headers_parser(pugi::xml_node node_mzml);

} // xml_utils

#endif
