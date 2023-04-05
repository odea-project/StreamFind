#ifndef XML_UTILS_H
#define XML_UTILS_H

#include <iostream>
#include <string>
#include <vector>
#include <list>
#include "external_libraries.hpp"
#include <Rcpp.h>

namespace xml_utils {

  struct runHeaders {
    std::string file_format;
    std::string time_stamp;
    std::vector<int> index;
    std::vector<int> id;
    std::vector<int> scan;
    std::vector<int> traces;
    std::vector<std::string> polarity;
    std::vector<double> bpcmz;
    std::vector<double> bpcint;
    std::vector<double> ticint;
    std::vector<int> level;
    std::vector<std::string> mode;
    std::vector<double> mzlow;
    std::vector<double> mzhigh;
    std::vector<double> rt;
    std::vector<double> drift;
    std::vector<int> pre_scan;
    std::vector<double> pre_mz;
    std::vector<double> pre_loweroffset;
    std::vector<double> pre_upperoffset;
    std::vector<double> pre_ce;
  }; // runHeaders

struct runHeadersOriginal {
  std::string fileFormat;
  std::string run_id;
  std::string run_defaultInstrumentConfigurationRef;
  std::string run_startTimeStamp;
  std::string run_defaultSourceFileRef;
  int specList_count;
  std::string specList_defaultDataProcessingRef;
  std::vector<int> spec_index;
  std::vector<int> spec_id;
  std::vector<int> scan;
  std::vector<int> spec_defaultArrayLength;
  std::vector<std::string> spec_polarity;
  std::vector<double> spec_bpcmz;
  std::vector<double> spec_bpcint;
  std::vector<double> spec_ticint;
  std::vector<int> spec_level;
  std::vector<std::string> spec_mode;
  std::vector<double> spec_mzlow;
  std::vector<double> spec_mzhigh;
  std::string spec_title;
  std::vector<double> scan_rt;
  std::vector<double> scan_drift;
  std::string scan_filter_string;
  std::vector<double> scan_injectionIonTime;
  std::vector<int> pre_scan;
  std::vector<double> pre_mz;
  std::vector<double> pre_loweroffset;
  std::vector<double> pre_upperoffset;
  std::vector<double> ion_mz;
  std::vector<double> ion_charge;
  std::vector<double> ion_intensity;
  std::vector<double> activation_type;
  std::vector<double> activation_ce;
}; // runHeadersOriginal

  struct runSummary {
    std::string file_format;
    std::string time_stamp;
    double spectra_number;
    std::vector<std::string> spectra_mode;
    std::vector<int> spectra_levels;
    double mz_low;
    double mz_high;
    double rt_start;
    double rt_end;
    std::vector<std::string> polarity;
    bool has_ion_mobility;
    runHeaders headers;
  }; // runSummary

  std::list<std::vector<std::string>> mzml_instrument_parser(pugi::xml_node node_mzml);

  std::list<std::vector<std::string>> mzxml_instrument_parser(pugi::xml_node node_mzxml);

  std::list<std::vector<std::string>> mzml_software_parser(pugi::xml_node node_mzml);

  std::list<std::vector<std::string>> mzxml_software_parser(pugi::xml_node node_mzxml);

  runHeaders mzml_run_headers_parser(pugi::xml_node node_mzml);

  runHeaders mzxml_run_headers_parser(pugi::xml_node node_mzxml);

  runSummary run_summary(pugi::xml_node node);

} // xml_utils

#endif
