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

      xml_utils::runHeaders headers_cpp = xml_utils::parse_run(root);

      xml_utils::runSummary summary = xml_utils::run_summary(headers_cpp);

      if (summary.spectra_number > 0) {
        Rcpp::IntegerVector levels = Rcpp::wrap(headers_cpp.level);
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
      } else {
        list_out["type"] = na_charvec;
      }

      //TODO make case polarity switching

      std::string search_run = "//run";
      pugi::xpath_node xps_run = root.select_node(search_run.c_str());
      Rcpp::String time_stamp = xps_run.node().attribute("startTimeStamp").as_string();

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

      list_out["software"] = xml_utils::parse_software(root);

      list_out["instrument"] = xml_utils::parse_instrument(root);

      Rcpp::List headers;
      headers["index"] = headers_cpp.index;
      headers["scan"] = headers_cpp.scan;
      headers["traces"] = headers_cpp.traces;
      headers["level"] = headers_cpp.level;
      headers["rt"] = headers_cpp.rt;

      if (summary.has_ion_mobility) {
        headers["drift"] = headers_cpp.drift;
      }

      headers["bpc_mz"] = headers_cpp.bpcmz;
      headers["bpc_intensity"] = headers_cpp.bpcint;
      headers["tic_intensity"] = headers_cpp.ticint;

      Rcpp::IntegerVector pre_scan = Rcpp::wrap(headers_cpp.pre_scan);
      Rcpp::NumericVector pre_mz = Rcpp::wrap(headers_cpp.pre_mz);
      Rcpp::NumericVector pre_ce = Rcpp::wrap(headers_cpp.pre_ce);

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

      list_out["run"] = headers;

      Rcpp::DataFrame empty_df;
      empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      Rcpp::List empty_list;

      list_out["spectra"] = empty_df;
      list_out["chromatograms"] = empty_df;
      list_out["features"] = empty_df;
      list_out["metadata"] = empty_list;

      list_out.attr("class") = Rcpp::CharacterVector::create("msAnalysis");

    } else {
      std::cout << "\u2717 MS file not conform!" << std::endl;
    }

  } else {
    std::cout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }

  return list_out;
}
