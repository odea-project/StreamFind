#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <set>
#include <algorithm>
#include "external_libraries.hpp"
#include "xml_utils.h"

std::list<std::vector<std::string>> xml_utils::mzml_instrument_parser(pugi::xml_node node_mzml) {

  std::list<std::vector<std::string>> output;
  std::vector<std::string> names;
  std::vector<std::string> vals;

  std::string search_ref = "//referenceableParamGroup";
  pugi::xpath_node xp_ref = node_mzml.select_node(search_ref.c_str());

  if (xp_ref.node() != NULL) {
    for (pugi::xml_node temp: xp_ref.node().children())
    {
      if (temp.attribute("name") != NULL) {
        names.push_back(temp.attribute("name").as_string());
        std::string val = temp.attribute("value").as_string();
        if (val != "") {
          vals.push_back(temp.attribute("value").as_string());
        } else {
          vals.push_back(temp.attribute("name").as_string());
        }
      }
    }
  }

  std::string search_inst = "//instrumentConfiguration";
  pugi::xpath_node xp_inst = node_mzml.select_node(search_inst.c_str());

  if (xp_inst.node() != NULL) {
    for (pugi::xml_node temp: xp_inst.node().children())
    {
      if (temp.attribute("name") != NULL) {
        names.push_back(temp.attribute("name").as_string());
        std::string val = temp.attribute("value").as_string();
        if (val != "") {
          vals.push_back(temp.attribute("value").as_string());
        } else {
          vals.push_back(temp.attribute("name").as_string());
        }
      }
    }
  }

  std::string search_config = "//componentList/child::node()";
  pugi::xpath_node_set xps_config = node_mzml.select_nodes(search_config.c_str());

  if (xps_config.size() > 0) {
    for (pugi::xpath_node_set::const_iterator it = xps_config.begin(); it != xps_config.end(); ++it)
    {
      pugi::xpath_node node = *it;
      for (pugi::xml_node temp: node.node().children())
      {
        names.push_back(node.node().name());
        vals.push_back(temp.attribute("name").as_string());
      }
    }
  }

  if (names.size() > 0) {
    output.push_back(names);
    output.push_back(vals);
  }

  return output;
}

std::list<std::vector<std::string>> xml_utils::mzxml_instrument_parser(pugi::xml_node node_mzxml) {

  std::list<std::vector<std::string>> output;
  std::vector<std::string> names;
  std::vector<std::string> vals;

  std::string search_inst = "//msInstrument/child::node()[starts-with(name(), 'ms')]";
  pugi::xpath_node_set xps_inst = node_mzxml.select_nodes(search_inst.c_str());

  if (xps_inst.size() > 0) {
    for (pugi::xpath_node_set::const_iterator it = xps_inst.begin(); it != xps_inst.end(); ++it)
    {
      pugi::xpath_node node = *it;
      names.push_back(node.node().attribute("category").as_string());
      vals.push_back(node.node().attribute("value").as_string());
    }
  }

  if (names.size() > 0) {
    output.push_back(names);
    output.push_back(vals);
  }

  return output;
}

std::list<std::vector<std::string>> xml_utils::mzml_software_parser(pugi::xml_node node_mzml) {

  std::list<std::vector<std::string>> output;
  std::vector<std::string> names;
  std::vector<std::string> ids;
  std::vector<std::string> version;

  std::string search_software = "//softwareList/child::node()";
  pugi::xpath_node_set xps_software = node_mzml.select_nodes(search_software.c_str());

  if (xps_software.size() > 0) {
    for (pugi::xpath_node_set::const_iterator it = xps_software.begin(); it != xps_software.end(); ++it)
    {
      pugi::xpath_node node = *it;
      for (pugi::xml_node temp: node.node().children())
      {
        std::string name = temp.attribute("name").as_string();
        if (name != "") {
          names.push_back(name);
          ids.push_back(node.node().attribute("id").as_string());
          version.push_back(node.node().attribute("version").as_string());
        }
      }
    }
  }

  output.push_back(names);
  output.push_back(ids);
  output.push_back(version);


  return output;
}

std::list<std::vector<std::string>> xml_utils::mzxml_software_parser(pugi::xml_node node_mzxml) {

  std::list<std::vector<std::string>> output;
  std::vector<std::string> names;
  std::vector<std::string> ids;
  std::vector<std::string> version;

  std::string search_software = "//msInstrument/child::node()[starts-with(name(), 'soft')]";
  pugi::xpath_node_set xps_software = node_mzxml.select_nodes(search_software.c_str());

  if (xps_software.size() > 0) {
    for (pugi::xpath_node_set::const_iterator it = xps_software.begin(); it != xps_software.end(); ++it)
    {
      pugi::xpath_node node = *it;
      names.push_back(node.node().attribute("name").as_string());
      ids.push_back(node.node().attribute("type").as_string());
      version.push_back(node.node().attribute("version").as_string());
    }
  }

  output.push_back(names);
  output.push_back(ids);
  output.push_back(version);

  return output;
}

xml_utils::runHeaders xml_utils::mzml_run_headers_parser(pugi::xml_node node_mzml) {

  xml_utils::runHeaders output;

  output.file_format = node_mzml.name();

  std::string search_run = "//run";
  pugi::xpath_node xps_run = node_mzml.select_node(search_run.c_str());

  output.time_stamp = xps_run.node().attribute("startTimeStamp").as_string();

  pugi::xml_node spec_list = xps_run.node().child("spectrumList");

  if (spec_list != NULL) {
    for (pugi::xml_node spec: spec_list.children("spectrum"))
    {
      output.index.push_back(spec.attribute("index").as_int());

      std::string scan = spec.attribute("id").as_string();

      std::size_t poslastEqual = scan.rfind('=');
      int scan_n = std::stoi(scan.substr(poslastEqual + 1));
      // std::sscanf(scan.c_str(), "%*[^=]=%d", &scan_n);
      output.scan.push_back(scan_n);

      output.traces.push_back(spec.attribute("defaultArrayLength").as_int());

      pugi::xml_node node_scan = spec.child("scanList").child("scan");

      pugi::xml_node rt = node_scan.find_child_by_attribute("cvParam", "name", "scan start time");
      std::string rt_unit = rt.attribute("unitName").as_string();
      double rt_val = rt.attribute("value").as_double();
      if (rt_unit == "minute") rt_val = rt_val * 60;
      output.rt.push_back(rt_val);

      pugi::xml_node drift = node_scan.find_child_by_attribute("cvParam", "name", "ion mobility drift time");
      if (drift != NULL) {
        // std::string drift_unit = node_drift.attribute("unitName").as_string();
        double drift_val = drift.attribute("value").as_double();
        // if (rt_unit == "millisecond") drift_t = drift_t / 0.001;
        output.drift.push_back(drift_val);
      } else {
        output.drift.push_back(nan(""));
      }

      pugi::xml_node level = spec.find_child_by_attribute("cvParam", "name", "ms level");
      output.level.push_back(level.attribute("value").as_int());

      pugi::xml_node pol_pos = spec.find_child_by_attribute("cvParam", "accession", "MS:1000130");
      pugi::xml_node pol_neg = spec.find_child_by_attribute("cvParam", "accession", "MS:1000129");

      if (pol_pos != NULL) {
        output.polarity.push_back(pol_pos.attribute("name").as_string());
      } else if (pol_neg != NULL) {
        output.polarity.push_back(pol_neg.attribute("name").as_string());
      } else {
        output.polarity.push_back("NA");
      }

      pugi::xml_node centroid = spec.find_child_by_attribute("cvParam", "accession", "MS:1000127");
      pugi::xml_node profile = spec.find_child_by_attribute("cvParam", "accession", "MS:1000128");

      if (centroid != NULL) {
        output.mode.push_back(centroid.attribute("name").as_string());
      } else if (profile != NULL) {
        output.mode.push_back(profile.attribute("name").as_string());
      } else {
        output.mode.push_back("NA");
      }

      pugi::xml_node mzlow = spec.find_child_by_attribute("cvParam", "name", "lowest observed m/z");
      output.mzlow.push_back(mzlow.attribute("value").as_double());

      pugi::xml_node mzhigh = spec.find_child_by_attribute("cvParam", "name", "highest observed m/z");
      output.mzhigh.push_back(mzhigh.attribute("value").as_double());

      pugi::xml_node bpcmz = spec.find_child_by_attribute("cvParam", "name", "base peak m/z");
      output.bpcmz.push_back(bpcmz.attribute("value").as_double());

      pugi::xml_node bpcint = spec.find_child_by_attribute("cvParam", "name", "base peak intensity");
      output.bpcint.push_back(bpcint.attribute("value").as_double());

      pugi::xml_node ticint = spec.find_child_by_attribute("cvParam", "name", "total ion current");
      output.ticint.push_back(ticint.attribute("value").as_double());

      pugi::xml_node precursor = spec.child("precursorList").child("precursor");

      if (precursor != NULL) {

        std::string pre_scan = precursor.attribute("spectrumRef").as_string();
        int pre_scan_n;
        std::sscanf(pre_scan.c_str(), "%*[^=]=%d", &pre_scan_n);
        output.pre_scan.push_back(pre_scan_n);

        pugi::xml_node isolation = precursor.child("isolationWindow");

        pugi::xml_node pre_mz = isolation.find_child_by_attribute("cvParam", "name", "isolation window target m/z");
        output.pre_mz.push_back(pre_mz.attribute("value").as_double());

        pugi::xml_node pre_loweroffset = isolation.find_child_by_attribute("cvParam", "name", "isolation window lower offset");
        output.pre_loweroffset.push_back(pre_loweroffset.attribute("value").as_double());

        pugi::xml_node pre_upperoffset = isolation.find_child_by_attribute("cvParam", "name", "isolation window upper offset");
        output.pre_upperoffset.push_back(pre_upperoffset.attribute("value").as_double());

        pugi::xml_node activation = precursor.child("activation");

        pugi::xml_node pre_ce = activation.find_child_by_attribute("cvParam", "name", "collision energy");
        output.pre_ce.push_back(pre_ce.attribute("value").as_double());

      } else {
        output.pre_scan.push_back(-1);
        output.pre_mz.push_back(nan(""));
        output.pre_loweroffset.push_back(nan(""));
        output.pre_upperoffset.push_back(nan(""));
        output.pre_ce.push_back(nan(""));
      }
    }
  }

  return output;
}

xml_utils::runHeaders xml_utils::mzxml_run_headers_parser(pugi::xml_node node_mzxml) {

  xml_utils::runHeaders output;

  output.file_format = node_mzxml.name();

  std::string search_run = "//msRun";
  pugi::xpath_node xps_run = node_mzxml.select_node(search_run.c_str());

  output.time_stamp = xps_run.node().attribute("startTimeStamp").as_string();

  int counter = 1;

  for (pugi::xml_node spec: xps_run.node().children("scan"))
  {
    output.index.push_back(counter);

    counter++;

    output.scan.push_back(spec.attribute("num").as_int());

    output.traces.push_back(spec.attribute("peaksCount").as_int());

    std::string rt = spec.attribute("retentionTime").as_string();
    double rt_n;
    std::sscanf(rt.c_str(), "%*[^0123456789]%lf", &rt_n);
    char last_char = '\0';
    std::sscanf(rt.c_str() + rt.size() - 1, "%c", &last_char);
    if (last_char != 'S') rt_n = rt_n * 60;
    output.rt.push_back(rt_n);

    output.drift.push_back(nan(""));

    output.level.push_back(spec.attribute("msLevel").as_int());

    std::string pol_sign = spec.attribute("polarity").as_string();

    if (pol_sign == "+") {
      output.polarity.push_back("positive");
    } else if (pol_sign == "-") {
      output.polarity.push_back("negative");
    } else {
      output.polarity.push_back("NA");
    }

    int centroided = spec.attribute("centroided").as_int();

    if (centroided == 1) {
      output.mode.push_back("centroid");
    } else if (centroided == 0) {
      output.mode.push_back("profile");
    } else {
      output.mode.push_back("NA");
    }

    output.mzlow.push_back(spec.attribute("lowMz").as_double());

    output.mzhigh.push_back(spec.attribute("highMz").as_double());

    output.bpcmz.push_back(spec.attribute("basePeakMz").as_double());

    output.bpcint.push_back(spec.attribute("basePeakIntensity").as_double());

    output.ticint.push_back(spec.attribute("totIonCurrent").as_double());

    pugi::xml_node precursor = spec.child("precursorMz");

    if (precursor != NULL) {

      output.pre_scan.push_back(-1);

      output.pre_mz.push_back(precursor.text().as_double());

      output.pre_ce.push_back(spec.attribute("collisionEnergy").as_double());

      output.pre_loweroffset.push_back(nan(""));

      output.pre_upperoffset.push_back(nan(""));

    } else {
      output.pre_scan.push_back(-1);
      output.pre_mz.push_back(nan(""));
      output.pre_loweroffset.push_back(nan(""));
      output.pre_upperoffset.push_back(nan(""));
      output.pre_ce.push_back(nan(""));
    }
  }

  return output;
}

xml_utils::runSummary xml_utils::run_summary(pugi::xml_node node) {

  xml_utils::runHeaders headers;

  xml_utils::runSummary output;

  std::string file_format = node.name();

  if (strcmp("mzML", file_format.c_str()) == 0) {
    headers = mzml_run_headers_parser(node);

  } else if (strcmp("mzXML", file_format.c_str()) == 0) {
    headers = mzxml_run_headers_parser(node);

  } else {

  }

  output.file_format = headers.file_format;

  output.time_stamp = headers.time_stamp;

  output.spectra_number = headers.scan.size();

  if (output.spectra_number < -1) output.spectra_number = 0;

  if (output.spectra_number > 0) {

    std::set<std::string> mode(headers.mode.begin(), headers.mode.end());
    std::vector<std::string> mode_v(mode.begin(), mode.end());
    output.spectra_mode = mode_v;

    std::set<int> level(headers.level.begin(), headers.level.end());
    std::vector<int> level_v(level.begin(), level.end());
    output.spectra_levels = level_v;

    auto mzlow = std::min_element(headers.mzlow.begin(), headers.mzlow.end());
    output.mz_low = *mzlow;

    auto mzhigh = std::max_element(headers.mzhigh.begin(), headers.mzhigh.end());
    output.mz_high = *mzhigh;

    auto rtmin = std::min_element(headers.rt.begin(), headers.rt.end());
    output.rt_start = *rtmin;

    auto rtmax = std::max_element(headers.rt.begin(), headers.rt.end());
    output.rt_end = *rtmax;

    std::set<std::string> polarity(headers.polarity.begin(), headers.polarity.end());
    std::vector<std::string> polarity_v(polarity.begin(), polarity.end());
    output.polarity = polarity_v;


    bool has_im = std::all_of(headers.drift.begin(), headers.drift.end(), [](double d) { return std::isnan(d); });
    output.has_ion_mobility = !has_im;

  } else {
    std::vector<std::string> empty_string_v(1, "");
    std::vector<int> empty_int_vec(1, 0);

    output.spectra_mode = empty_string_v;
    output.spectra_levels = empty_int_vec;
    output.mz_low = nan("");
    output.mz_high = nan("");
    output.rt_start = nan("");
    output.rt_end = nan("");
    output.polarity = empty_string_v;
    output.has_ion_mobility = false;
  }

  output.headers = headers;

  return output;

}
