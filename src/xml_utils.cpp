#include <iostream>
#include <string>
#include <vector>
#include <list>
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

  output.push_back(names);
  output.push_back(vals);

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

std::string xml_utils::mzml_time_stamp_parser(pugi::xml_node node_mzml) {

  std::string search_run = "//run";

  pugi::xpath_node xps_run = node_mzml.select_node(search_run.c_str());

  return xps_run.node().attribute("startTimeStamp").as_string();

}

xml_utils::mzmlObj xml_utils::mzml_headers_parser(pugi::xml_node node_mzml) {

  xml_utils::mzmlObj output;

  std::string search_run = "//run";
  pugi::xpath_node xps_run = node_mzml.select_node(search_run.c_str());

  pugi::xml_node spec_list = xps_run.node().child("spectrumList");

  if (spec_list != NULL) {
    for (pugi::xml_node spec: spec_list.children("spectrum"))
    {
      output.index.push_back(spec.attribute("index").as_int());

      std::string scan = spec.attribute("id").as_string();
      int scan_n;
      std::sscanf(scan.c_str(), "%*[^=]=%d", &scan_n);
      output.scan.push_back(scan_n);

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
