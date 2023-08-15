#include <iostream>
#include "pugixml.h"
#include "xml_utils.h"
#include <string>
#include <vector>
#include <list>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis(std::string file_path) {

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
      Rcpp::Rcout << "\u2717 The file must be in valid mzML or mzXML format!" << std::endl;
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

      xml_utils::spectraHeaders spec_headers_cpp = xml_utils::parse_spectra_headers(root);

      xml_utils::chromatogramsHeaders chrom_headers_cpp = xml_utils::parse_chromatograms_headers(root);

      xml_utils::runSummary summary = xml_utils::run_summary(spec_headers_cpp, chrom_headers_cpp);

      if (summary.spectra_number > 0) {
        Rcpp::IntegerVector levels = Rcpp::wrap(spec_headers_cpp.level);
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
      } else if (summary.chromatograms_number > 0) {
        list_out["type"] = "SRM";

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

      list_out["spectra_mode"] = Rcpp::wrap(summary.mode);

      list_out["spectra_levels"] = summary.levels;

      list_out["mz_low"] = summary.mz_low;

      list_out["mz_high"] = summary.mz_high;

      list_out["rt_start"] = summary.rt_start;

      list_out["rt_end"] = summary.rt_end;

      list_out["polarity"] = Rcpp::wrap(summary.polarity);

      list_out["has_ion_mobility"] = summary.has_ion_mobility;

      list_out["chromatograms_number"] = summary.chromatograms_number;

      list_out["software"] = xml_utils::parse_software(root);

      list_out["instrument"] = xml_utils::parse_instrument(root);

      list_out["run"] = xml_utils::spectraHeaders_to_list(spec_headers_cpp);

      Rcpp::DataFrame empty_df;
      empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      Rcpp::List empty_list;

      list_out["spectra"] = empty_df;
      list_out["chromatograms"] = empty_df;
      list_out["features_eic"] = empty_list;
      list_out["features"] = empty_df;
      list_out["metadata"] = empty_list;

      list_out.attr("class") = Rcpp::CharacterVector::create("MassSpecAnalysis");

    } else {
      Rcpp::Rcout << "\u2717 MS file not conform!" << std::endl;
    }

  } else {
    Rcpp::Rcout << "\u2717 XML file could not be opened! " << result.description() << std::endl;
  }

  return list_out;
}
