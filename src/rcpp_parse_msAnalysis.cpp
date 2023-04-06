#include <iostream>
#include "external_libraries.hpp"
#include "xml_utils.h"
#include <string>
#include <vector>
#include <list>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_parse_msAnalysis(std::string file_path) {

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
      std::cout << "\u2717 The file must be in valid mzML or mzXML format!" << std::endl;
    }

    if (node_in != NULL) {

      // std::cout << "\u2713 " << node_in.name() << " opened!" << std::endl;
      //
      // std::cout << "\u2699 Parsing data...";

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

      list_out["type"] = na_charvec;


      std::list<std::vector<std::string>> instrument_list_cpp;

      if (strcmp("mzML", file_format.c_str()) == 0) {
        instrument_list_cpp = xml_utils::mzml_instrument_parser(node_in);
      } else if (strcmp("mzXML", root_name.c_str()) == 0) {
        instrument_list_cpp = xml_utils::mzxml_instrument_parser(node_in);
      }

      Rcpp::List instrument_list;
      auto it = instrument_list_cpp.begin();
      instrument_list["category"] = Rcpp::wrap(*it);
      std::advance(it, 1);
      instrument_list["value"] = Rcpp::wrap(*it);
      instrument_list.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");


      std::list<std::vector<std::string>> software_list_cpp;

      if (strcmp("mzML", file_format.c_str()) == 0) {
        software_list_cpp = xml_utils::mzml_software_parser(node_in);
      } else if (strcmp("mzXML", root_name.c_str()) == 0) {
        software_list_cpp = xml_utils::mzxml_software_parser(node_in);
      } else {
        std::list<std::vector<std::string>> software_list_cpp;
        std::vector<std::string> names;
        std::vector<std::string> ids;
        std::vector<std::string> version;
        software_list_cpp.push_back(names);
        software_list_cpp.push_back(ids);
        software_list_cpp.push_back(version);
      }

      Rcpp::List software_list;
      auto it2 = software_list_cpp.begin();
      software_list["name"] = Rcpp::wrap(*it2);
      std::advance(it2, 1);
      software_list["id"] = Rcpp::wrap(*it2);
      std::advance(it2, 1);
      software_list["version"] = Rcpp::wrap(*it2);
      software_list.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      //TODO make case polarity switching

      xml_utils::runSummary summary;
      summary = xml_utils::run_summary(node_in);

      Rcpp::String time_stamp = Rcpp::wrap(summary.time_stamp);
      if (time_stamp == "") {
        list_out["time_stamp"] = na_charvec;
      } else {
        list_out["time_stamp"] = time_stamp;
      }

      list_out["spectra_number"] = summary.spectra_number;

      Rcpp::StringVector mode = Rcpp::wrap(summary.spectra_mode);
      if (mode.size() == 1 && mode(0) == "") {
        list_out["spectra_mode"] = na_charvec;
      } else {

        if (mode(0) == "centroid spectrum") {
          mode(0) = "centroid";
        } else if (mode(0) == "profile spectrum") {
          mode(0) = "profile";
        }

        list_out["spectra_mode"] = mode;
      }

      list_out["spectra_levels"] = summary.spectra_levels;

      if (std::isnan(summary.mz_low)) {
        list_out["mz_low"] = NA_REAL;
      } else {
        list_out["mz_low"] = summary.mz_low;
      }

      if (std::isnan(summary.mz_high)) {
        list_out["mz_high"] = NA_REAL;
      } else {
        list_out["mz_high"] = summary.mz_high;
      }

      if (std::isnan(summary.rt_start)) {
        list_out["rt_start"] = NA_REAL;
      } else {
        list_out["rt_start"] = summary.rt_start;
      }

      if (std::isnan(summary.rt_end)) {
        list_out["rt_end"] = NA_REAL;
      } else {
        list_out["rt_end"] = summary.rt_end;
      }

      Rcpp::StringVector polarity = Rcpp::wrap(summary.polarity);
      if (polarity.size() == 1 && polarity(0) == "") {
        list_out["polarity"] = na_charvec;
      } else {

        if (polarity(0) == "positive scan") {
          polarity(0) = "positive";
        } else if (polarity(0) == "negative scan") {
          polarity(0) = "negative";
        }

        list_out["polarity"] = polarity;
      }

      list_out["has_ion_mobility"] = summary.has_ion_mobility;

      list_out["chromatograms_number"] = 0;

      Rcpp::List headers;
      headers["index"] = summary.headers.index;
      headers["scan"] = summary.headers.scan;
      headers["traces"] = summary.headers.traces;
      headers["level"] = summary.headers.level;
      headers["rt"] = summary.headers.rt;

      if (summary.has_ion_mobility) {
        headers["drift"] = summary.headers.drift;
      }

      headers["bpc_mz"] = summary.headers.bpcmz;
      headers["bpc_intensity"] = summary.headers.bpcint;
      headers["tic_intensity"] = summary.headers.ticint;

      Rcpp::IntegerVector pre_scan = Rcpp::wrap(summary.headers.pre_scan);
      Rcpp::NumericVector pre_mz = Rcpp::wrap(summary.headers.pre_mz);
      Rcpp::NumericVector pre_ce = Rcpp::wrap(summary.headers.pre_ce);

      for (int i = 0; i < pre_scan.size(); i++) {

        if (pre_scan[i] == -1) {
          pre_scan[i] = NA_INTEGER;
        }

        if (std::isnan(pre_mz[i])) {
          pre_mz[i] = NA_REAL;
        }

        if (std::isnan(pre_ce[i])) {
          pre_ce[i] = NA_REAL;
        }

      }

      headers["pre_scan"] = pre_scan;
      headers["pre_mz"] = pre_mz;
      headers["pre_ce"] = pre_ce;

      headers.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      list_out["software"] = software_list;

      list_out["instrument"] = instrument_list;

      list_out["run"] = headers;

      if (summary.spectra_number > 0) {
        Rcpp::IntegerVector levels = Rcpp::wrap(headers["level"]);
        levels = Rcpp::unique(levels);
        if (levels.size() > 1) {
          if (summary.has_ion_mobility) {
            list_out["type"] = "IM-MS/MS";
          } else {
            list_out["type"] = "MS/MS";
          }
        } else {
          list_out["type"] = "MS";
        }
      }

      Rcpp::DataFrame empty_df;
      empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      Rcpp::List empty_list;

      list_out["spectra"] = empty_df;
      list_out["chromatograms"] = empty_df;
      list_out["features"] = empty_df;
      list_out["metadata"] = empty_list;

      // std::cout << " Done!" << std::endl;

      list_out.attr("class") = Rcpp::CharacterVector::create("msAnalysis");

    } else {
      std::cout << "\u2717 MS file not conform!" << std::endl;
    }

  } else {
    std::cout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }

  return list_out;
}
