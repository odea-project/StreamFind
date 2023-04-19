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
Rcpp::List rcpp_parse_xml(std::string file_path)
{

  Rcpp::List list_out;

  const char * path = file_path.c_str();

  pugi::xml_document doc;

  pugi::xml_parse_result result = doc.load_file(path);

  if (result) {

    pugi::xml_node root = doc.document_element();

    std::string root_name = root.name();

    pugi::xml_node node_in;

    const Rcpp::CharacterVector cols = {"mz","intenisty"};

    if (strcmp("indexedmzML", root_name.c_str()) == 0) {
      node_in = root.child("mzML");

      std::string search_run = "//chromatogramList";
      pugi::xpath_node xp_spec_list = node_in.select_node(search_run.c_str());
      pugi::xml_node spec_list = xp_spec_list.node();

      if (spec_list != NULL) {

        pugi::xml_node first_spec = spec_list.child("chromatogram");

        Rcpp::Rcout << first_spec.name() << std::endl;

        const std::vector<int> precision = xml_utils::mzml_get_precision(first_spec);

        Rcpp::Rcout << precision.size() << std::endl;

        Rcpp::Rcout << precision[0] << " " << precision[1] << std::endl;

        const std::vector<std::string> compression = xml_utils::mzml_get_compression(first_spec);

        Rcpp::Rcout << compression.size() << std::endl;

        // std::vector<pugi::xml_node> spectra;
        //
        // for (pugi::xml_node child = spec_list.first_child(); child; child = child.next_sibling()) {
        //   spectra.push_back(child);
        // }
        //
        // int number_spectra = spectra.size();
        //
        // Rcpp::List list_out2(number_spectra);
        //
        // for (int i = 0; i < number_spectra; i++) {
        //   list_out2[i] = xml_utils::mzml_parse_binary_data_from_spectrum_node(spectra[i], precision, compression, cols);
        // }
        //
        // return list_out2;

      }

    } else if (strcmp("mzXML", root_name.c_str()) == 0) {

    }
  }

  return list_out;
}

// old code --------------------------------------------------------------------
// -----------------------------------------------------------------------------
//------------------------------------------------------------------------------

// namespace mzml_find {
//
// bool rt(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "scan start time") == 0;
// }
//
// bool drift(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "ion mobility drift time") == 0;
// }
//
// bool level(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "ms level") == 0;
// }
//
// bool mzlow(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "lowest observed m/z") == 0;
// }
//
// bool mzhigh(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "highest observed m/z") == 0;
// }
//
// bool bpcmz(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "base peak m/z") == 0;
// }
//
// bool bpcint(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "base peak intensity") == 0;
// }
//
// bool ticint(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "total ion current") == 0;
// }
//
// bool polpos(pugi::xml_node node)
// {
//   return strcmp(node.attribute("accession").as_string(), "MS:1000130") == 0;
// }
//
// bool polneg(pugi::xml_node node)
// {
//   return strcmp(node.attribute("accession").as_string(), "MS:1000129") == 0;
// }
//
// bool centroid(pugi::xml_node node)
// {
//   return strcmp(node.attribute("accession").as_string(), "MS:1000127") == 0;
// }
//
// bool profile(pugi::xml_node node)
// {
//   return strcmp(node.attribute("accession").as_string(), "MS:1000128") == 0;
// }
//
// bool pre_mz(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "isolation window target m/z") == 0;
// }
//
// bool pre_loweroffset(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "isolation window lower offset") == 0;
// }
//
// bool pre_upperoffset(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "isolation window upper offset") == 0;
// }
//
// bool pre_ce(pugi::xml_node node)
// {
//   return strcmp(node.attribute("name").as_string(), "collision energy") == 0;
// }
//
// bool precision_float(pugi::xml_node node)
// {
//   return strcmp(node.attribute("accession").as_string(), "MS:1000523") == 0;
// }
//
// bool precision_integer(pugi::xml_node node)
// {
//   return strcmp(node.attribute("accession").as_string(), "MS:1000522") == 0;
// }
//
// bool compression(pugi::xml_node node)
// {
//   return strcmp(node.attribute("accession").as_string(), "MS:1000574") == 0;
// }
// } // mzml_find

// namespace other_utils {
//
// const char          fillchar = '=';
//
// // 00000000001111111111222222
// // 01234567890123456789012345
// static std::string  cvt = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
//
// // 22223333333333444444444455
// // 67890123456789012345678901
// "abcdefghijklmnopqrstuvwxyz"
//
// // 555555556666
// // 234567890123
// "0123456789+/";
//
// }



// if (result) {
//
//   node_in = doc.first_child();
//
//   if (node_in != NULL) {
//     std::string first_name;
//     first_name = node_in.name();
//
//     if (strcmp("indexedmzML", first_name.c_str()) == 0) {
//       node_in = node_in.first_child();
//       std::cout << "\u2713 " << node_in.name() << " opened!" << std::endl;
//     }
//
//   } else {
//     std::cout << "\u2717 First node of the xml file found!" << std::endl;
//   }
//
// } else {
//   std::cout << "\u2717 Not opened with result: " << result.description() << std::endl;
// }

// std::cout << "Load result: " << result.description() << std::endl;

// std::cout << "Node name: " << node_in.name() << std::endl;
//
// std::cout << "Second name: " << doc.first_child().name() << std::endl;
//
// pugi::xml_node mzml = doc.first_child().first_child();
//
// std::cout << "File structure: " << mzml.name() << std::endl;

// std::cout << std::endl;
// std::cout << std::endl;


// std::list<std::vector<std::string>> instrument_list;
// instrument_list = xml_utils::mzml_instrument_parser(node_in);
// auto it = instrument_list.begin();
// Rcpp::StringVector instrument_class_names = Rcpp::wrap(*it);
// std::advance(it, 1);
// Rcpp::StringVector instrument = Rcpp::wrap(*it);
// instrument.names() = instrument_class_names;
// list_out["instrument"] = instrument_list;
//
// std::list<std::vector<std::string>> software_list_cpp;
// software_list_cpp = xml_utils::mzml_software_parser(node_in);
// Rcpp::List software_list;
// auto it2 = software_list_cpp.begin();
// software_list["name"] = Rcpp::wrap(*it2);
// std::advance(it2, 1);
// software_list["id"] = Rcpp::wrap(*it2);
// std::advance(it2, 1);
// software_list["version"] = Rcpp::wrap(*it2);
//
// software_list.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
//
// list_out["software"] = software_list;
//
// xml_utils::runSummary summary;
// summary = xml_utils::run_summary(node_in);
// Rcpp::List summary_list;
//
// summary_list["file_format"] = summary.file_format;
// summary_list["time_stamp"] = summary.time_stamp;
// summary_list["spectra_number"] = summary.spectra_number;
// summary_list["spectra_mode"] = summary.spectra_mode;
// summary_list["spectra_levels"] = summary.spectra_levels;
// summary_list["mzlow"] = summary.mz_low;
// summary_list["mzhigh"] = summary.mz_high;
// summary_list["rt_start"] = summary.rt_start;
// summary_list["rt_end"] = summary.rt_end;
// summary_list["polarity"] = summary.polarity;
// summary_list["has_im"] = summary.has_ion_mobility;
//
// Rcpp::List headers;
// headers["index"] = summary.headers.index;
// headers["scan"] = summary.headers.scan;
// headers["traces"] = summary.headers.traces;
// headers["level"] = summary.headers.level;
// headers["bpc_mz"] = summary.headers.bpcmz;
// headers["bpc_intensity"] = summary.headers.bpcint;
// headers["tic_intensity"] = summary.headers.ticint;
// headers["pre_scan"] = summary.headers.pre_scan;
// headers["pre_mz"] = summary.headers.pre_mz;
// headers["pre_ce"] = summary.headers.pre_ce;
// // make case polarity switching
//
// headers.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
//
// summary_list["run"] = headers;
//
// list_out["summary"] = summary_list;
//
// std::cout << " Done!" << std::endl;

// xml_utils::runHeaders test;
//
// test = xml_utils::mzml_run_headers_parser(mzml);

// Rcpp::List test_list;
// // test_list["time_stamp"] = test.time_stamp;
// test_list["index"] = test.index;
// test_list["scan"] = test.scan;
// test_list["traces"] = test.traces;
// test_list["rt"] = test.rt;
// test_list["drift"] = test.drift;
// test_list["level"] = test.level;
// test_list["polarity"] = test.polarity;
// test_list["mode"] = test.mode;
// test_list["mzlow"] = test.mzlow;
// test_list["mzhigh"] = test.mzhigh;
// test_list["bpc_mz"] = test.bpcmz;
// test_list["bpc_intensity"] = test.bpcint;
// test_list["tic_intensity"] = test.ticint;
// test_list["pre_scan"] = test.pre_scan;
// test_list["pre_mz"] = test.pre_mz;
// test_list["pre_loweroffset"] = test.pre_loweroffset;
// test_list["pre_upperoffset"] = test.pre_upperoffset;
// test_list["pre_ce"] = test.pre_ce;
//
// test_list.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
//
//
// list_out["test"] = test_list;

// get instrument reference
// - gets information from "referenceableParamGroup" when available and
// from direct childs of "instrumentConfiguration" it return a cpp list of
// two elements with "names" and "values"

// std::list<std::vector<std::string>> instrument;
// std::vector<std::string> inst_names;
// std::vector<std::string> inst_values;
//
// std::string search_ref = "//referenceableParamGroup";
// pugi::xpath_node xp_ref = doc.select_node(search_ref.c_str());
// if (xp_ref.node() != NULL) {
//   for (pugi::xml_node temp: xp_ref.node().children())
//   {
//     if (temp.attribute("name") != NULL) {
//       inst_names.push_back(temp.attribute("name").as_string());
//       std::string val_value = temp.attribute("value").as_string();
//       if (val_value != "") {
//         inst_values.push_back(temp.attribute("value").as_string());
//       } else {
//         inst_values.push_back(temp.attribute("name").as_string());
//       }
//     }
//   }
// }
//
// std::string search_inst = "//instrumentConfiguration";
// pugi::xpath_node xp_inst = doc.select_node(search_inst.c_str());
// if (xp_inst.node() != NULL) {
//   for (pugi::xml_node temp: xp_inst.node().children())
//   {
//     if (temp.attribute("name") != NULL) {
//       inst_names.push_back(temp.attribute("name").as_string());
//       std::string val_value = temp.attribute("value").as_string();
//       if (val_value != "") {
//         inst_values.push_back(temp.attribute("value").as_string());
//       } else {
//         inst_values.push_back(temp.attribute("name").as_string());
//       }
//     }
//   }
// }
//
// std::string search_config = "//componentList/child::node()";
// pugi::xpath_node_set xps_config = doc.select_nodes(search_config.c_str());
// if (xps_config.size() > 0) {
//   for (pugi::xpath_node_set::const_iterator it = xps_config.begin(); it != xps_config.end(); ++it)
//   {
//     pugi::xpath_node node = *it;
//     for (pugi::xml_node temp: node.node().children())
//     {
//       inst_names.push_back(node.node().name());
//       inst_values.push_back(temp.attribute("name").as_string());
//     }
//   }
// }
//
// instrument.push_back(inst_names);
// instrument.push_back(inst_values);
//
// list_out["instrument"] = instrument;

// for (auto vect : instrument) {
//   // Each element of the list is
//   // a vector itself
//   std::vector<std::string> currentVector = vect;
//
//   std::cout << "[ ";
//
//   // Printing vector contents
//   for (auto element : currentVector)
//     std::cout << element << ' ';
//
//   std::cout << ']';
//   std::cout << '\n';
// }

// std::cout << "\u2023 Instrument reference node with name " << xp_node_inst.node().name() << std::endl;
//
// for (pugi::xml_node temp: xp_node_inst.node().children())
// {
//   std::cout << "\u25e6 Child name: " << temp.name() << std::endl;
//
//   for (pugi::xml_attribute attr: temp.attributes())
//   {
//     std::cout << " " << attr.name() << "=" << attr.value();
//   }
//
//   std::cout << std::endl;
// }

// std::cout << std::endl;
// std::cout << std::endl;

// get instrument configuration

// std::list<std::vector<std::string>> configuration;
// std::vector<std::string> config_names;
// std::vector<std::string> config_values;

// std::string search_config = "//componentList/child::node()";
// pugi::xpath_node_set xps_config = doc.select_nodes(search_config.c_str());
// if (xps_config.size() > 0) {
//   for (pugi::xpath_node_set::const_iterator it = xps_config.begin(); it != xps_config.end(); ++it)
//   {
//     pugi::xpath_node node = *it;
//     for (pugi::xml_node temp: node.node().children())
//     {
//       config_names.push_back(node.node().name());
//       config_values.push_back(temp.attribute("name").as_string());
//     }
//   }
// }
//
// configuration.push_back(config_names);
// configuration.push_back(config_values);
//
// list_out["configuration"] = configuration;




// for (pugi::xpath_node_set::const_iterator it = xp_node_config.begin(); it != xp_node_config.end(); ++it)
// {
//   pugi::xpath_node node = *it;
//   std::cout << "\u2023 Node name: " << node.node().name() << "\n";
//
//   for (pugi::xml_node temp: node.node().children())
//   {
//
//     std::cout << "\u25e6 Child name: " << temp.name() << std::endl;
//
//     for (pugi::xml_attribute attr: temp.attributes())
//     {
//       std::cout << " " << attr.name() << "=" << attr.value();
//     }
//
//     std::cout << std::endl;
//   }
// }

// std::cout << std::endl;
// std::cout << std::endl;

// get software list

// std::string search_software = "//softwareList/child::node()";
// pugi::xpath_node_set xp_node_software = doc.select_nodes(search_software.c_str());
// std::cout << "\u2217 Software node set with " << xp_node_software.size() << " nodes" << std::endl;
//
// for (pugi::xpath_node_set::const_iterator it = xp_node_software.begin(); it != xp_node_software.end(); ++it)
// {
//   pugi::xpath_node node = *it;
//   std::cout << "\u2023 Node name: " << node.node().name() << "\n";
//
//   for (pugi::xml_attribute attr_node: node.node().attributes())
//   {
//     std::cout << " " << attr_node.name() << "=" << attr_node.value();
//   }
//
//   std::cout << std::endl;
//
//   for (pugi::xml_node temp: node.node().children())
//   {
//
//     std::cout << "\u25e6 Child name: " << temp.name() << std::endl;
//
//     for (pugi::xml_attribute attr: temp.attributes())
//     {
//       std::cout << " " << attr.name() << "=" << attr.value();
//     }
//
//     std::cout << std::endl;
//   }
// }

// std::cout << std::endl;
// std::cout << std::endl;



// get run information ----------------------------------------------------------------------------

// std::string search_run = "//run";
// pugi::xpath_node xp_node_run = doc.select_node(search_run.c_str());
//
// pugi::xml_node spec_list = xp_node_run.node().child("spectrumList");
//
// std::cout << "\u2217 The node name is: " << spec_list.name() << std::endl;
//
// pugi::xml_node first_spec = spec_list.first_child();
//
// pugi::xml_node msl_node = first_spec.find_child_by_attribute("cvParam", "name", "ms level");
//
// std::cout << "\u2217 The node name is: " << msl_node.name() << std::endl;
//
// std::cout << "\u2217 The attr val in: " << msl_node.attribute("value").as_int() << std::endl;


// std::cout << "\u2217 The " << time_stamp.name() << " is " << time_stamp.value() << std::endl;

// get spectra index and scan numbers
// std::vector<int> index;
// std::vector<int> scan;
// std::vector<double> rt;
// std::vector<double> drift;
// std::vector<int> level;
// std::vector<std::string> polarity;
// std::vector<std::string> mode;
// std::vector<double> mzlow;
// std::vector<double> mzhigh;
// std::vector<double> bpcmz;
// std::vector<double> bpcint;
// std::vector<double> ticint;
// std::vector<int> pre_scan;
// std::vector<double> pre_mz;
// std::vector<double> pre_loweroffset;
// std::vector<double> pre_upperoffset;
// std::vector<double> pre_ce;

// std::string search_spectra = "//spectrum";
// pugi::xpath_node_set spectra = xp_node_run.node().select_nodes(search_spectra.c_str());

// std::cout << "\u2217 The file has " << spectra.size() << " spectra" << std::endl;

// std::cout << std::endl;
// std::cout << std::endl;

// if (spectra.size() > 0) {
//
//   for (pugi::xpath_node_set::const_iterator it = spectra.begin(); it != spectra.end(); ++it) {
//     pugi::xpath_node node = *it;
//     index.push_back(node.node().attribute("index").as_int());
//
//     std::string scan_t = node.node().attribute("id").as_string();
//     int scan_n;
//     std::sscanf(scan_t.c_str(), "%*[^=]=%d", &scan_n);
//     scan.push_back(scan_n);
//
//     pugi::xml_node node_scan = node.node().child("scanList").child("scan");
//
//     pugi::xml_node node_rt = node_scan.find_child(mzml_find::rt);
//     std::string rt_unit = node_rt.attribute("unitName").as_string();
//     double rt_t = node_rt.attribute("value").as_double();
//     if (rt_unit == "minute") rt_t = rt_t * 60;
//     rt.push_back(rt_t);
//
//     pugi::xml_node node_drift = node_scan.find_child(mzml_find::drift);
//     std::string node_drift_name = node_drift.name();
//
//     if (strcmp(node_drift_name.c_str(), "cvParam") == 0) {
//       // std::string drift_unit = node_drift.attribute("unitName").as_string();
//       double drift_t = node_drift.attribute("value").as_double();
//       // if (rt_unit == "millisecond") drift_t = drift_t / 0.001;
//       drift.push_back(drift_t);
//     } else {
//       drift.push_back(nan(""));
//     }
//
//     pugi::xml_node node_level = node.node().find_child(mzml_find::level);
//     level.push_back(node_level.attribute("value").as_int());
//
//     pugi::xml_node node_pol_pos = node.node().find_child(mzml_find::polpos);
//     std::string pol_pos = node_pol_pos.attribute("name").as_string();
//
//     pugi::xml_node node_pol_neg = node.node().find_child(mzml_find::polneg);
//     std::string pol_neg = node_pol_neg.attribute("name").as_string();
//
//     if (strcmp(pol_pos.c_str(), "positive scan") == 0) {
//       polarity.push_back(node_pol_pos.attribute("name").as_string());
//     } else if (strcmp(pol_neg.c_str(), "negative scan") == 0) {
//       polarity.push_back(pol_neg);
//     } else {
//       polarity.push_back("NA");
//     }
//
//     pugi::xml_node node_centroid = node.node().find_child(mzml_find::centroid);
//     std::string centroid = node_centroid.attribute("name").as_string();
//
//     pugi::xml_node node_profile = node.node().find_child(mzml_find::profile);
//     std::string profile = node_profile.attribute("name").as_string();
//
//     if (strcmp(centroid.c_str(), "centroid spectrum") == 0) {
//       mode.push_back(centroid);
//     } else if (strcmp(profile.c_str(), "profile spectrum") == 0) {
//       mode.push_back(profile);
//     } else {
//       mode.push_back("NA");
//     }
//
//     pugi::xml_node node_mzlow = node.node().find_child(mzml_find::mzlow);
//     mzlow.push_back(node_mzlow.attribute("value").as_double());
//
//     pugi::xml_node node_mzhigh = node.node().find_child(mzml_find::mzhigh);
//     mzhigh.push_back(node_mzhigh.attribute("value").as_double());
//
//     pugi::xml_node node_bpcmz = node.node().find_child(mzml_find::bpcmz);
//     bpcmz.push_back(node_bpcmz.attribute("value").as_double());
//
//     pugi::xml_node node_bpcint = node.node().find_child(mzml_find::bpcint);
//     bpcint.push_back(node_bpcint.attribute("value").as_double());
//
//     pugi::xml_node node_ticint = node.node().find_child(mzml_find::ticint);
//     ticint.push_back(node_ticint.attribute("value").as_double());
//
//     pugi::xml_node node_precursor = node.node().child("precursorList").child("precursor");
//     std::string has_precursor = node_precursor.name();
//
//     if (strcmp(has_precursor.c_str(), "precursor") == 0) {
//
//       std::string pre_scan_t = node_precursor.attribute("spectrumRef").as_string();
//       int pre_scan_n;
//       std::sscanf(pre_scan_t.c_str(), "%*[^=]=%d", &pre_scan_n);
//       pre_scan.push_back(pre_scan_n);
//
//       pugi::xml_node node_isolation = node_precursor.child("isolationWindow");
//       pugi::xml_node node_pre_mz = node_isolation.find_child(mzml_find::pre_mz);
//       pre_mz.push_back(node_pre_mz.attribute("value").as_double());
//
//       pugi::xml_node node_pre_loweroffset = node_isolation.find_child(mzml_find::pre_loweroffset);
//       pre_loweroffset.push_back(node_pre_loweroffset.attribute("value").as_double());
//
//       pugi::xml_node node_pre_upperoffset = node_isolation.find_child(mzml_find::pre_upperoffset);
//       pre_upperoffset.push_back(node_pre_upperoffset.attribute("value").as_double());
//
//       pugi::xml_node node_activation = node_precursor.child("activation");
//       pugi::xml_node node_pre_ce = node_activation.find_child(mzml_find::pre_ce);
//       pre_ce.push_back(node_pre_ce.attribute("value").as_double());
//
//     } else {
//       pre_scan.push_back(-1);
//       pre_mz.push_back(nan(""));
//       pre_loweroffset.push_back(nan(""));
//       pre_upperoffset.push_back(nan(""));
//       pre_ce.push_back(nan(""));
//     }
//
//
//   }


// pugi::xml_node node_binary = spectra.first().node().child("binaryDataArrayList").child("binaryDataArray");
//
// std::string node_binary_name = node_binary.name();
//
// std::string precision_str;
// int precision_int;
// std::string compression;
//
// pugi::xml_node node_precision_float = node_binary.find_child(mzml_find::precision_float);
// std::string node_precision_float_name = node_precision_float.name();
//
// if (strcmp(node_precision_float_name.c_str(), "cvParam") == 0) {
//   precision_str = node_precision_float.attribute("name").as_string();
// } else {
//   pugi::xml_node node_precision_integer = node_binary.find_child(mzml_find::precision_integer);
//   precision_str = node_precision_integer.attribute("name").as_string();
// }
// std::sscanf(precision_str.c_str(), "%d%*c", &precision_int);
//
// pugi::xml_node node_compression = node_binary.find_child(mzml_find::compression);
// std::string node_compression_name = node_compression.name();
//
// if (strcmp(node_compression_name.c_str(), "cvParam") == 0) {
//   compression = node_compression.attribute("name").as_string();
// } else {
//   compression = "none";
// }
//
// if (compression == "zlib" || compression == "zlib compression") {
//   compression = "gzip";
// } else {
//   compression = "none";
// }
//
// pugi::xml_node node_binary_data = node_binary.child("binary");
// std::string encoded_data = node_binary_data.text().as_string();
//
// // via base64 library
// std::string decoded_data = base64_decode(encoded_data, false);

// const std::string& data = encoded_data;
//
// std::string::size_type  i;
// char c;
// char c1;
// std::string::size_type  len = data.length();
// std::vector<char>  ret;
//
// for (i = 0; i < len; ++i)
// {
//   c = (char) other_utils::cvt.find(data[i]);
//   ++i;
//   c1 = (char) other_utils::cvt.find(data[i]);
//   c = (c << 2) | ((c1 >> 4) & 0x3);
//   ret.push_back(c);
//   if (++i < len)
//   {
//     c = data[i];
//     if (other_utils::fillchar == c)
//       break;
//     c = (char) other_utils::cvt.find(c);
//     c1 = ((c1 << 4) & 0xf0) | ((c >> 2) & 0xf);
//     ret.push_back(c1);
//   }
//   if (++i < len)
//   {
//     c1 = data[i];
//     if (other_utils::fillchar == c1)
//       break;
//     c1 = (char) other_utils::cvt.find(c1);
//     c = ((c << 6) & 0xc0) | c1;
//     ret.push_back(c);
//   }
// }


// // std::cout << "The encoded text: " << encoded_data << std::endl;
//
// const char * pointer = decoded_data.data();
// std::size_t size = decoded_data.size();
//
// // Check if compressed. Can check both gzip and zlib.
// bool test_compress = gzip::is_compressed(pointer, size); // false
//
// std::cout << "Is compressed: " << test_compress << std::endl;
//
//
// // https://stackoverflow.com/questions/50056779/decompress-text-string-with-zlib
//
// z_stream zs;                        // z_stream is zlib's control structure
// memset(&zs, 0, sizeof(zs));
// inflateInit(&zs);
// zs.next_in = (Bytef*)decoded_data.data();
// zs.avail_in = decoded_data.size();
//
// int ret2;
// char outbuffer[32768];
// std::string outstring;
// do
// {
//   zs.next_out = reinterpret_cast<Bytef*>(outbuffer);
//   zs.avail_out = sizeof(outbuffer);
//   ret2 = inflate(&zs, 0);
//   if (outstring.size() < zs.total_out)
//   {
//     outstring.append(outbuffer, zs.total_out - outstring.size());
//   }
// }
// while (ret2 == Z_OK);
// inflateEnd(&zs);
//
//
//
// std::string decompressed_data = gzip::decompress(pointer, size);
//
//
// // std::cout << "The decompressed data: " << outstring << std::endl;
//
// // std::cout << "The decompressed data: " << decompressed_data << std::endl;
//
// std::string byteStr = decompressed_data;
// std::vector<unsigned char> byteVec(byteStr.begin(), byteStr.end()); // convert to vector
// std::vector<double> doubleVec(byteVec.size() / sizeof(double)); // create vector of doubles
//
// // sizeof(double) gives the precision which is 64/8
//
// std::cout << sizeof(double) << std::endl;
//
// int length_doubles = doubleVec.size();
//
// for (int i = 0; i < length_doubles; ++i) {
//   doubleVec[i] = reinterpret_cast<double&>(byteVec[i * sizeof(double)]); // reinterpret cast
// }

// print the values in the double vector
// for (const auto& val : doubleVec) {
//   std::cout << val << " ";
// }


// list_out["base64"] = encoded_data;
// list_out["direct"] = outstring;
// list_out["gzip"] = decompressed_data;
// list_out["result"] = doubleVec;


// std::cout << "The compression is " << compression << std::endl;


// std::cout << std::endl;
// std::cout << std::endl;

// get rt vals
// std::vector<double> rt;
// std::string search_rt = "//scanList/scan/cvParam[@name='scan start time']";
// pugi::xpath_node_set scans_rt = xp_node_run.node().select_nodes(search_rt.c_str());
// for (pugi::xpath_node_set::const_iterator it = scans_rt.begin(); it != scans_rt.end(); ++it)
// {
//   pugi::xpath_node node = *it;
//   std::string rt_unit = node.node().attribute("unitName").as_string();
//   double rt_t = node.node().attribute("value").as_double();
//   if (rt_unit == "minute") rt_t = rt_t * 60;
//   rt.push_back(rt_t);
// }

// get mzlow vals
// std::vector<double> mzlow;
// std::string search_mzlow = "//spectrum/cvParam[@name='lowest observed m/z']";
// pugi::xpath_node_set nodes_mzlow = xp_node_run.node().select_nodes(search_mzlow.c_str());
// for (pugi::xpath_node_set::const_iterator it = nodes_mzlow.begin(); it != nodes_mzlow.end(); ++it)
// {
//   pugi::xpath_node node = *it;
//   mzlow.push_back(node.node().attribute("value").as_double());
// }

// get mzhigh vals
// std::vector<double> mzhigh;
// std::string search_mzhigh = "//spectrum/cvParam[@name='highest observed m/z']";
// pugi::xpath_node_set nodes_mzhigh = xp_node_run.node().select_nodes(search_mzhigh.c_str());
// for (pugi::xpath_node_set::const_iterator it = nodes_mzhigh.begin(); it != nodes_mzhigh.end(); ++it)
// {
//   pugi::xpath_node node = *it;
//   mzhigh.push_back(node.node().attribute("value").as_double());
// }

// // get level vals
// std::vector<int> level;
// std::string search_level = "//spectrum/cvParam[@name='ms level']";
// pugi::xpath_node_set nodes_level = xp_node_run.node().select_nodes(search_level.c_str());
// for (pugi::xpath_node_set::const_iterator it = nodes_level.begin(); it != nodes_level.end(); ++it)
// {
//   pugi::xpath_node node = *it;
//   level.push_back(node.node().attribute("value").as_int());
// }

// get polarity vals
// std::string polarity;
// std::string search_polarity_pos = "//spectrum/cvParam[@accession='MS:1000130']";
// std::string search_polarity_neg = "//spectrum/cvParam[@accession='MS:1000129']";
// pugi::xpath_node_set nodes_polarity_pos = xp_node_run.node().select_nodes(search_polarity_pos.c_str());
// pugi::xpath_node_set nodes_polarity_neg = xp_node_run.node().select_nodes(search_polarity_neg.c_str());

// if (nodes_polarity_pos.size() > 0 && nodes_polarity_neg.size() > 0) {
//   polarity = "both";
// } else if (nodes_polarity_neg.size() > 0) {
//   polarity = "negative";
// } else if (nodes_polarity_pos.size() > 0) {
//   polarity = "positive";
// } else {
//   polarity = "NA";
// }

// get positive polarity vals
// std::vector<std::string> polarity_pos;
// std::string search_polarity_pos = "//spectrum/cvParam[@accession='MS:1000130']";
// pugi::xpath_node_set nodes_polarity_pos = xp_node_run.node().select_nodes(search_polarity_pos.c_str());
// std::cout << "Number nodes pol " << nodes_polarity_pos.size() << std::endl;
// for (pugi::xpath_node_set::const_iterator it = nodes_polarity_pos.begin(); it != nodes_polarity_pos.end(); ++it)
// {
//   pugi::xpath_node node = *it;
//   polarity_pos.push_back(node.node().attribute("name").as_string());
// }

// get negative polarity vals
// std::vector<std::string> polarity_neg;
// std::string search_polarity_neg = "//spectrum/cvParam[@accession='MS:1000129']";
// pugi::xpath_node_set nodes_polarity_neg = xp_node_run.node().select_nodes(search_polarity_neg.c_str());
// std::cout << "Number nodes pol " << nodes_polarity_neg.size() << std::endl;
// for (pugi::xpath_node_set::const_iterator it = nodes_polarity_neg.begin(); it != nodes_polarity_neg.end(); ++it)
// {
//   pugi::xpath_node node = *it;
//   if (node.node().name() == "") {
//     // polarity_neg.push_back("");
//     std::cout << "correct!" << std::endl;
//   } else {
//     // polarity_neg.push_back(node.node().attribute("value").as_string());
//   }
// }

// get spectra_mode
// std::string spectra_mode;
// std::string search_mode_cent = "//spectrum/cvParam[@accession='MS:1000127']";
// std::string search_mode_prof = "//spectrum/cvParam[@accession='MS:1000128']";
// pugi::xpath_node_set nodes_mode_cent = xp_node_run.node().select_nodes(search_mode_cent.c_str());
// pugi::xpath_node_set nodes_mode_prof = xp_node_run.node().select_nodes(search_mode_prof.c_str());
//
// if (nodes_mode_cent.size() > 0 && nodes_mode_prof.size() > 0) {
//   spectra_mode = "both";
// } else if (nodes_mode_prof.size() > 0) {
//   spectra_mode = "profile";
// } else if (nodes_mode_cent.size() > 0) {
//   spectra_mode = "centroid";
// } else {
//   spectra_mode = "NA";
// }




// centroided_x <- '//d1:spectrum/d1:cvParam[@accession="MS:1000127"]'
// centroided_n <- xml_find_all(xml_data, xpath = centroided_x)
//   if (length(centroided_n) > 0) {
//     spectra_mode <- "centroid"
//   } else {
//     profile_x <- '//d1:spectrum/d1:cvParam[@accession="MS:1000128"]'
//     profile_n <- xml_find_all(xml_data, xpath = profile_x)
//     if (length(profile_n) > 0) {
//       spectra_mode <- "profile"
//     } else {
//       spectra_mode <- NA_character_
//     }
//   }




// for (int i=0; i<2; ++i) {
//   std::cout << "\u2217 The scan number " << i << " has the following values: " << std::endl;
//   std::cout << "index: " << index[i] << std::endl;
//   std::cout << "scan: " << scan[i] << std::endl;
//   std::cout << "rt: " << rt[i] << std::endl;
//   std::cout << "drift: " << drift[i] << std::endl;
//   std::cout << "level: "<< level[i] << std::endl;
//   std::cout << "polarity: "<< polarity[i] << std::endl;
//   std::cout << "mode: "<< mode[i] << std::endl;
//   std::cout << "mzlow: "<< mzlow[i] << std::endl;
//   std::cout << "mzhigh: "<< mzhigh[i] << std::endl;
//   std::cout << "bpc mz: "<< bpcmz[i] << std::endl;
//   std::cout << "bpc intensity: "<< bpcint[i] << std::endl;
//   std::cout << "tic intensity: "<< ticint[i] << std::endl;
//   std::cout << "pre_scan: "<< pre_scan[i] << std::endl;
//   std::cout << "pre_mz: "<< pre_mz[i] << std::endl;
//   std::cout << "pre_loweroffset: "<< pre_loweroffset[i] << std::endl;
//   std::cout << "pre_upperoffset: "<< pre_upperoffset[i] << std::endl;
//   std::cout << "pre_ce: "<< pre_ce[i] << std::endl;
//
//
//   std::cout << std::endl;
// }
//
// std::cout << std::endl;
// std::cout << std::endl;
