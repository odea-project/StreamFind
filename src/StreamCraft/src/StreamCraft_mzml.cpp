#include "StreamCraft_mzml.hpp"
#include <omp.h>
#include <filesystem>
#include <cstring>
#include <algorithm>
#include <set>

int sc::mzml::MZML_SPECTRUM::extract_spec_index() const {
  return spec.attribute("index").as_int();
};

std::string sc::mzml::MZML_SPECTRUM::extract_spec_id() const {
  return spec.attribute("id").as_string();
};

int sc::mzml::MZML_SPECTRUM::extract_spec_scan() const {
  std::string spec_id = spec.attribute("id").as_string();
  std::size_t poslastEqual = spec_id.rfind('=');
  return std::stoi(spec_id.substr(poslastEqual + 1));
};

int sc::mzml::MZML_SPECTRUM::extract_spec_array_length() const {
  return spec.attribute("defaultArrayLength").as_int();
};

int sc::mzml::MZML_SPECTRUM::extract_spec_level() const {
  pugi::xml_node level_node = spec.find_child_by_attribute("cvParam", "name", "ms level");
  return level_node.attribute("value").as_int();
};

std::string sc::mzml::MZML_SPECTRUM::extract_spec_mode() const {
  pugi::xml_node centroid_node = spec.find_child_by_attribute("cvParam", "accession", "MS:1000127");
  pugi::xml_node profile_node = spec.find_child_by_attribute("cvParam", "accession", "MS:1000128");
  if (centroid_node) {
    return "centroid";
  } else if (profile_node) {
    return "profile";
  } else {
    return "";
  }
};

std::string sc::mzml::MZML_SPECTRUM::extract_spec_polarity() const {
  pugi::xml_node pol_pos_node = spec.find_child_by_attribute("cvParam", "accession", "MS:1000130");
  pugi::xml_node pol_neg_node = spec.find_child_by_attribute("cvParam", "accession", "MS:1000129");
  if (pol_pos_node) {
    return "positive";
  } else if (pol_neg_node) {
    return "negative";
  } else {
    return "";
  }
};

double sc::mzml::MZML_SPECTRUM::extract_spec_lowmz() const {
  pugi::xml_node lowmz_node = spec.find_child_by_attribute("cvParam", "name", "lowest observed m/z");
  return lowmz_node.attribute("value").as_double();
};

double sc::mzml::MZML_SPECTRUM::extract_spec_highmz() const {
  pugi::xml_node highmz_node = spec.find_child_by_attribute("cvParam", "name", "highest observed m/z");
  return highmz_node.attribute("value").as_double();
};

double sc::mzml::MZML_SPECTRUM::extract_spec_bpmz() const {
  pugi::xml_node bpmz_node = spec.find_child_by_attribute("cvParam", "name", "base peak m/z");
  return bpmz_node.attribute("value").as_double();
};

double sc::mzml::MZML_SPECTRUM::extract_spec_bpint() const {
  pugi::xml_node bpint_node = spec.find_child_by_attribute("cvParam", "name", "base peak intensity");
  return bpint_node.attribute("value").as_double();
};

double sc::mzml::MZML_SPECTRUM::extract_spec_tic() const {
  pugi::xml_node tic_node = spec.find_child_by_attribute("cvParam", "name", "total ion current");
  return tic_node.attribute("value").as_double();
};

std::string sc::mzml::MZML_SPECTRUM::extract_spec_title() const {
  pugi::xml_node title_node = spec.find_child_by_attribute("cvParam", "name", "spectrum title");
  return title_node.attribute("value").as_string();
};

double sc::mzml::MZML_SPECTRUM::extract_scan_rt() const {
  pugi::xml_node node_scan = spec.child("scanList").child("scan");
  pugi::xml_node rt_node = node_scan.find_child_by_attribute("cvParam", "name", "scan start time");
  std::string rt_unit = rt_node.attribute("unitName").as_string();
  double rt_val = rt_node.attribute("value").as_double();
  if (rt_unit == "minute") rt_val = rt_val * 60;
  return rt_val;
};

double sc::mzml::MZML_SPECTRUM::extract_scan_drift() const {
  pugi::xml_node node_scan = spec.child("scanList").child("scan");
  pugi::xml_node drift_node = node_scan.find_child_by_attribute("cvParam", "name", "ion mobility drift time");
  return drift_node.attribute("value").as_double();
};

std::string sc::mzml::MZML_SPECTRUM::extract_scan_filter_string() const {
  pugi::xml_node node_scan = spec.child("scanList").child("scan");
  pugi::xml_node filter_string_node = node_scan.find_child_by_attribute("cvParam", "name", "filter string");
  return filter_string_node.attribute("value").as_string();
};

int sc::mzml::MZML_SPECTRUM::extract_scan_config() const {
  pugi::xml_node node_scan = spec.child("scanList").child("scan");
  pugi::xml_node config_node = node_scan.find_child_by_attribute("cvParam", "name", "preset scan configuration");
  return config_node.attribute("value").as_int();
};

double sc::mzml::MZML_SPECTRUM::extract_scan_injection_ion_time() const {
  pugi::xml_node node_scan = spec.child("scanList").child("scan");
  pugi::xml_node iit_node = node_scan.find_child_by_attribute("cvParam", "name", "ion injection time");
  return iit_node.attribute("value").as_double();
};

int sc::mzml::MZML_SPECTRUM::extract_precursor_scan() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  std::string pre_scan_str = precursor.attribute("spectrumRef").as_string();
  if (pre_scan_str != "") {
    std::size_t poslastEqual = pre_scan_str.rfind('=');
    return std::stoi(pre_scan_str.substr(poslastEqual + 1));
  } else {
    return 0;
  }
};

double sc::mzml::MZML_SPECTRUM::extract_window_mz() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  pugi::xml_node isolation = precursor.child("isolationWindow");
  pugi::xml_node window_mz_node = isolation.find_child_by_attribute("cvParam", "name", "isolation window target m/z");
  return window_mz_node.attribute("value").as_double();
};

double sc::mzml::MZML_SPECTRUM::extract_window_mzlow() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  pugi::xml_node isolation = precursor.child("isolationWindow");
  pugi::xml_node window_mzlow_node = isolation.find_child_by_attribute("cvParam", "name", "isolation window lower offset");
  return window_mzlow_node.attribute("value").as_double();
};

double sc::mzml::MZML_SPECTRUM::extract_window_mzhigh() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  pugi::xml_node isolation = precursor.child("isolationWindow");
  pugi::xml_node window_mzhigh_node = isolation.find_child_by_attribute("cvParam", "name", "isolation window upper offset");
  return window_mzhigh_node.attribute("value").as_double();
};

double sc::mzml::MZML_SPECTRUM::extract_ion_mz() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  pugi::xml_node slected_ion = precursor.child("selectedIonList").first_child();
  pugi::xml_node ion_mz_node = slected_ion.find_child_by_attribute("cvParam", "name", "selected ion m/z");
  return ion_mz_node.attribute("value").as_double();
};

double sc::mzml::MZML_SPECTRUM::extract_ion_intensity() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  pugi::xml_node slected_ion = precursor.child("selectedIonList").first_child();
  pugi::xml_node ion_intensity_node = slected_ion.find_child_by_attribute("cvParam", "name", "peak intensity");
  return ion_intensity_node.attribute("value").as_double();
};

int sc::mzml::MZML_SPECTRUM::extract_ion_charge() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  pugi::xml_node slected_ion = precursor.child("selectedIonList").first_child();
  pugi::xml_node ion_charge_node = slected_ion.find_child_by_attribute("cvParam", "name", "charge state");
  return ion_charge_node.attribute("value").as_int();
};

std::string sc::mzml::MZML_SPECTRUM::extract_activation_type() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  pugi::xml_node activation = precursor.child("activation");
  if (activation) {
    pugi::xml_node activation_type_node = activation.first_child();
    return activation_type_node.name();
  } else {
    return "";
  }
};

double sc::mzml::MZML_SPECTRUM::extract_activation_ce() const {
  pugi::xml_node precursor = spec.child("precursorList").child("precursor");
  pugi::xml_node activation = precursor.child("activation");
  if (activation) {
    pugi::xml_node activation_ce_node = activation.find_child_by_attribute("cvParam", "name", "collision energy");
    return activation_ce_node.attribute("value").as_double();
  } else {
    return 0;
  }
};

std::vector<sc::MZML_BINARY_METADATA> sc::mzml::MZML_SPECTRUM::extract_binary_metadata() const {

  std::vector<MZML_BINARY_METADATA> mtd_vec;

  const pugi::xml_node binary_list = spec.child("binaryDataArrayList");

  int counter = 0;

  for (const pugi::xml_node& bin: binary_list.children("binaryDataArray")) {

    MZML_BINARY_METADATA mtd;

    const pugi::xml_node node_integer_32 = bin.find_child_by_attribute("cvParam", "accession", "MS:1000519");

    const pugi::xml_node node_float_32 = bin.find_child_by_attribute("cvParam", "accession", "MS:1000521");

    const pugi::xml_node node_integer_64 = bin.find_child_by_attribute("cvParam", "accession", "MS:1000522");

    const pugi::xml_node node_float_64 = bin.find_child_by_attribute("cvParam", "accession", "MS:1000523");

    if (node_float_64) {
      mtd.precision_name = node_float_64.attribute("name").as_string();
      mtd.precision_accession = node_float_64.attribute("accession").as_string();
      mtd.precision_type = "float";
      mtd.precision_int = 64;

    } else if (node_float_32) {
      mtd.precision_name = node_float_32.attribute("name").as_string();
      mtd.precision_accession = node_float_32.attribute("accession").as_string();
      mtd.precision_type = "float";
      mtd.precision_int = 32;

    } else if (node_integer_64){
      mtd.precision_name = node_integer_64.attribute("name").as_string();
      mtd.precision_accession = node_integer_64.attribute("accession").as_string();
      mtd.precision_type = "integer";
      mtd.precision_int = 64;

    } else if (node_integer_32){
      mtd.precision_name = node_integer_32.attribute("name").as_string();
      mtd.precision_accession = node_integer_32.attribute("accession").as_string();
      mtd.precision_type = "integer";
      mtd.precision_int = 32;

    } else {
      throw("Encoding precision with accession MS:1000521, MS:1000522 or MS:1000523 not found!");
    }

    const pugi::xml_node node_comp = bin.find_child_by_attribute("cvParam", "accession", "MS:1000574");

    if (node_comp) {
      mtd.compression = node_comp.attribute("name").as_string();

      if (mtd.compression == "zlib" || mtd.compression == "zlib compression") {
        mtd.compressed = true;

      } else {
        mtd.compressed = false;
      }
    }

    bool has_bin_data_type = false;

    for (size_t i = 0; 1 < mzml_possible_accessions_binary_data.size(); ++i) {
      pugi::xml_node node_data_type = bin.find_child_by_attribute("cvParam", "accession", mzml_possible_accessions_binary_data[i].c_str());

      if (node_data_type) {

        has_bin_data_type = true;

        mtd.data_name = node_data_type.attribute("name").as_string();

        mtd.data_accession = node_data_type.attribute("accession").as_string();

        mtd.data_value = node_data_type.attribute("value").as_string();

        mtd.data_unit = node_data_type.attribute("unitName").as_string();

        mtd.data_name_short = mzml_possible_short_name_binary_data[i];

        break;
      }
    }

    if (!has_bin_data_type) {
      throw("Encoded data type could not be found matching the mzML official vocabolary!");
    }

    if (mtd.data_name_short == "other") {
      mtd.data_name_short = mtd.data_value;
    }

    if (mtd.data_name_short == "") {
      mtd.data_name_short = "val_" + std::to_string(counter);
    }

    mtd.index = counter;

    mtd_vec.push_back(mtd);

    counter++;
  }

  return mtd_vec;
};

std::vector<std::vector<double>> sc::mzml::MZML_SPECTRUM::extract_binary_data(const std::vector<MZML_BINARY_METADATA>& mtd) const {

  std::vector<std::vector<double>> spectrum;

  const int number_traces = spec.attribute("defaultArrayLength").as_int();

  const pugi::xml_node node_binary_list = spec.child("binaryDataArrayList");

  const int number_bins = node_binary_list.attribute("count").as_int();

  const int number_spectra_binary_arrays = mtd.size();

  if (number_spectra_binary_arrays != number_bins) {
    throw("Binary array length does not match binary array length binary metadata!");
  }

  spectrum.resize(number_bins);

  int counter = 0;

  for (auto i = node_binary_list.children("binaryDataArray").begin(); i != node_binary_list.children("binaryDataArray").end(); ++i) {

    const pugi::xml_node& bin = *i;

    const pugi::xml_node node_binary = bin.child("binary");

    const std::string encoded_string = node_binary.child_value();

    std::string decoded_string = sc::decode_base64(encoded_string);

    if (mtd[counter].compressed) {
      decoded_string = sc::decompress_zlib(decoded_string);
    }

    spectrum[counter] = sc::decode_little_endian(decoded_string, mtd[counter].precision_int / 8);

    int bin_array_size = spectrum[counter].size();

    if (bin_array_size != number_traces) {
      throw("Number of traces in binary array does not match the value of the spectrum header!");
    }

    if (mtd[counter].data_name_short == "time") {
      pugi::xml_node node_unit = bin.find_child_by_attribute("cvParam", "unitCvRef", "UO");
      std::string unit = node_unit.attribute("unitName").as_string();

      if (unit == "minute") {
        for (double &j : spectrum[counter]) {
          j *= 60;
        }
      }
    }

    counter++;
  }

  return spectrum;
};

int sc::mzml::MZML_CHROMATOGRAM::extract_index() const {
  return chrom.attribute("index").as_int();
};

std::string sc::mzml::MZML_CHROMATOGRAM::extract_id() const {
  return chrom.attribute("id").as_string();
};

int sc::mzml::MZML_CHROMATOGRAM::extract_array_length() const {
  return chrom.attribute("defaultArrayLength").as_int();
};

std::string sc::mzml::MZML_CHROMATOGRAM::extract_polarity() const {
  const pugi::xml_node pol_pos_node = chrom.find_child_by_attribute("cvParam", "accession", "MS:1000130");
  const pugi::xml_node pol_neg_node = chrom.find_child_by_attribute("cvParam", "accession", "MS:1000129");
  if (pol_pos_node) {
    return "positive";
  } else if (pol_neg_node) {
    return "negative";
  } else {
    return "";
  }
};

double sc::mzml::MZML_CHROMATOGRAM::extract_precursor_mz() const {
  const pugi::xml_node precursor = chrom.child("precursor");
  const pugi::xml_node isolation = precursor.child("isolationWindow");
  const pugi::xml_node pre_mz_node = isolation.find_child_by_attribute("cvParam", "name", "isolation window target m/z");
  return pre_mz_node.attribute("value").as_double();
};

std::string sc::mzml::MZML_CHROMATOGRAM::extract_activation_type() const {
  const pugi::xml_node precursor = chrom.child("precursor");
  const pugi::xml_node activation = precursor.child("activation");
  if (activation) {
    const pugi::xml_node activation_type_node = activation.first_child();
    return activation_type_node.name();
  } else {
    return "";
  }
};

double sc::mzml::MZML_CHROMATOGRAM::extract_activation_ce() const {
  const pugi::xml_node precursor = chrom.child("precursor");
  const pugi::xml_node activation = precursor.child("activation");
  if (activation) {
    const pugi::xml_node activation_ce_node = activation.find_child_by_attribute("cvParam", "name", "collision energy");
    return activation_ce_node.attribute("value").as_double();
  } else {
    return 0;
  }
};

double sc::mzml::MZML_CHROMATOGRAM::extract_product_mz() const {
  const pugi::xml_node product = chrom.child("product");
  const pugi::xml_node isolation = product.child("isolationWindow");
  const pugi::xml_node pro_mz_node = isolation.find_child_by_attribute("cvParam", "name", "isolation window target m/z");
  return pro_mz_node.attribute("value").as_double();
};

std::vector<std::vector<double>> sc::mzml::MZML_CHROMATOGRAM::extract_binary_data() const {

  std::vector<std::vector<double>> chromatogram;

  const int number_traces = chrom.attribute("defaultArrayLength").as_int();

  const pugi::xml_node node_binary_list = chrom.child("binaryDataArrayList");

  const int number_bins = node_binary_list.attribute("count").as_int();

  chromatogram.resize(number_bins);

  int counter = 0;

  for (auto i = node_binary_list.children("binaryDataArray").begin(); i != node_binary_list.children("binaryDataArray").end(); ++i) {

    const pugi::xml_node& bin = *i;

    MZML_BINARY_METADATA mtd;

    const pugi::xml_node node_integer_32 = bin.find_child_by_attribute("cvParam", "accession", "MS:1000519");

    const pugi::xml_node node_float_32 = bin.find_child_by_attribute("cvParam", "accession", "MS:1000521");

    const pugi::xml_node node_integer_64 = bin.find_child_by_attribute("cvParam", "accession", "MS:1000522");

    const pugi::xml_node node_float_64 = bin.find_child_by_attribute("cvParam", "accession", "MS:1000523");

    if (node_float_64) {
      mtd.precision_name = node_float_64.attribute("name").as_string();
      mtd.precision_accession = node_float_64.attribute("accession").as_string();
      mtd.precision_type = "float";
      mtd.precision_int = 64;

    } else if (node_float_32) {
      mtd.precision_name = node_float_32.attribute("name").as_string();
      mtd.precision_accession = node_float_32.attribute("accession").as_string();
      mtd.precision_type = "float";
      mtd.precision_int = 32;

    } else if (node_integer_64){
      mtd.precision_name = node_integer_64.attribute("name").as_string();
      mtd.precision_accession = node_integer_64.attribute("accession").as_string();
      mtd.precision_type = "integer";
      mtd.precision_int = 64;

    } else if (node_integer_32){
      mtd.precision_name = node_integer_32.attribute("name").as_string();
      mtd.precision_accession = node_integer_32.attribute("accession").as_string();
      mtd.precision_type = "integer";
      mtd.precision_int = 32;

    } else {
      throw("Encoding precision with accession MS:1000521, MS:1000522 or MS:1000523 not found!");
    }

    const pugi::xml_node node_comp = bin.find_child_by_attribute("cvParam", "accession", "MS:1000574");

    if (node_comp) {
      mtd.compression = node_comp.attribute("name").as_string();

      if (mtd.compression == "zlib" || mtd.compression == "zlib compression") {
        mtd.compressed = true;

      } else {
        mtd.compressed = false;
      }
    }

    bool has_bin_data_type = false;

    for (size_t i = 0; 1 < mzml_possible_accessions_binary_data.size(); ++i) {
      pugi::xml_node node_data_type = bin.find_child_by_attribute("cvParam", "accession", mzml_possible_accessions_binary_data[i].c_str());

      if (node_data_type) {

        has_bin_data_type = true;

        mtd.data_name = node_data_type.attribute("name").as_string();

        mtd.data_accession = node_data_type.attribute("accession").as_string();

        mtd.data_value = node_data_type.attribute("value").as_string();

        mtd.data_unit = node_data_type.attribute("unitName").as_string();

        mtd.data_name_short = mzml_possible_short_name_binary_data[i];

        break;
      }
    }

    if (!has_bin_data_type) {
      throw("Encoded data type could not be found matching the mzML official vocabolary!");
    }

    if (mtd.data_name_short == "other") {
      mtd.data_name_short = mtd.data_value;
    }

    if (mtd.data_name_short == "") {
      mtd.data_name_short = "val_" + std::to_string(counter);
    }

    mtd.index = counter;

    const pugi::xml_node node_binary = bin.child("binary");

    const std::string encoded_string = node_binary.child_value();

    std::string decoded_string = sc::decode_base64(encoded_string);

    if (mtd.compressed) {
      decoded_string = sc::decompress_zlib(decoded_string);
    }

    chromatogram[counter] = sc::decode_little_endian(decoded_string, mtd.precision_int / 8);

    const int bin_array_size = chromatogram[counter].size();

    if (bin_array_size != number_traces) {
      throw("Number of traces in binary array does not match the value of the chromatogram header!");
    }

    if (mtd.data_name_short == "time") {
      pugi::xml_node node_unit = bin.find_child_by_attribute("cvParam", "unitCvRef", "UO");
      std::string unit = node_unit.attribute("unitName").as_string();

      if (unit == "minute") {
        for (double &j : chromatogram[counter]) {
          j *= 60;
        }
      }
    }

    counter++;
  }

  if (counter > 0) {
    for (int i = 1; i < counter; i++) {
      if (chromatogram[0].size() != chromatogram[i].size()) {
        throw("Number of traces in binary arrays of the chromatogram does not match!");
      }
    }
  }

  return chromatogram;
}

sc::mzml::MZML::MZML(const std::string& file) {

  file_path = file;

  file_dir = file.substr(0, file.find_last_of("/\\") + 1);

  if (file_dir.back() == '/') file_dir.pop_back();

  file_name = file.substr(file.find_last_of("/\\") + 1);
  
  file_extension = file_name.substr(file_name.find_last_of(".") + 1);
  
  file_name = file_name.substr(0, file_name.find_last_of("."));

  const char* path = file.c_str();

  loading_result = doc.load_file(path, pugi::parse_default | pugi::parse_declaration | pugi::parse_pi);

  if (loading_result) {
    root = doc.document_element();

    if (!root) {
      std::cerr << "Root element is empty!" << std::endl;

    } else {
      format = root.name();

      if ("indexedmzML" == format) {
        format = "mzML";

      } else {
        std::cerr << "Root element must be indexedmzML!" << std::endl;
      }
    }

  } else {
    std::cerr << "mzML file could not be opened!" << std::endl << loading_result.description() << std::endl;
  }

  if (format == "mzML") {
    root = root.first_child();
  }

  name = root.name();

  if (get_number_spectra() > 0) spectra_nodes = link_vector_spectra_nodes();
  if (get_number_chromatograms() > 0) chrom_nodes = link_vector_chrom_nodes();
};

void sc::mzml::MZML::print() const {
  std::cout << name << std::endl;
  std::cout << std::endl;
  std::cout << " File:                      " << file_path << std::endl;
  std::cout << std::endl;
  std::cout << " Number of spectra:         " << get_number_spectra() << std::endl;
  // std::cout << " Spectra mode (first):      " << get_spectra_mode() << std::endl;
  std::cout << " Number of binnary arrays:  " << get_number_spectra_binary_arrays() << std::endl;
  std::cout << " Name of binary arrays:     ";
  // if (number_spectra_binary_arrays > 0) {
  //   for (int i = 0; i < number_spectra_binary_arrays; ++i) {
  //     std::cout << spectra_binary_metadata[i].data_name_short;
  //     if (i < (number_spectra_binary_arrays -1)) {
  //       std::cout << ", ";
  //     }
  //   }
  // }
  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << " Number of chromatograms:   " << get_number_chromatograms() << std::endl;
  std::cout << std::endl;
};

void sc::mzml::MZML::print_spectra_binary_metadata() const {
  const std::vector<MZML_BINARY_METADATA> mtd = get_spectra_binary_metadata();
  if (mtd.size() > 0) {
    for (size_t i = 0; i < mtd.size(); ++i) {
      mtd[i].print();
    }
  }
};

std::vector<pugi::xml_node> sc::mzml::MZML::link_vector_spectra_nodes() const {

  std::vector<pugi::xml_node> spectra;

  std::string search_run = "//run";

  pugi::xpath_node xps_run = root.select_node(search_run.c_str());

  pugi::xml_node spec_list = xps_run.node().child("spectrumList");

  if (spec_list) {
    for (pugi::xml_node child = spec_list.first_child(); child; child = child.next_sibling()) {
      spectra.push_back(child);
    }
  
  } else {
    std::cerr << "Spectra list not found in the mzML file!" << std::endl;
  }
  
  return spectra;
};

std::vector<pugi::xml_node> sc::mzml::MZML::link_vector_chrom_nodes() const {

  std::vector<pugi::xml_node> chrom_nodes;

  std::string search_run = "//run";

  pugi::xpath_node xps_run = root.select_node(search_run.c_str());

  pugi::xml_node chrom_list = xps_run.node().child("chromatogramList");

  if (chrom_list) {
    for (pugi::xml_node child = chrom_list.first_child(); child; child = child.next_sibling()) {
      chrom_nodes.push_back(child);
    }
  
  } else {
    std::cerr << "Chromatogram list not found in the mzML file!" << std::endl;
  }
  
  return chrom_nodes;
};

int sc::mzml::MZML::get_number_spectra() const {
  const std::string search_run = "//run";
  const pugi::xpath_node xps_run = root.select_node(search_run.c_str());
  const pugi::xml_node spec_list = xps_run.node().child("spectrumList");
  return spec_list.attribute("count").as_int();
};

int sc::mzml::MZML::get_number_chromatograms() const {
  const std::string search_run = "//run";
  const pugi::xpath_node xps_run = root.select_node(search_run.c_str());
  const pugi::xml_node chrom_list = xps_run.node().child("chromatogramList");
  return chrom_list.attribute("count").as_int();
};

int sc::mzml::MZML::get_number_spectra_binary_arrays() const {
  const std::string search_run = "//run";
  const pugi::xpath_node xps_run = root.select_node(search_run.c_str());
  const pugi::xml_node spec_list = xps_run.node().child("spectrumList");
  return spec_list.first_child().child("binaryDataArrayList").attribute("count").as_int();
};

std::vector<std::string> sc::mzml::MZML::get_spectra_binary_short_names() const {
  const int number_spectra_binary_arrays = get_number_spectra_binary_arrays();
  std::vector<std::string> names(number_spectra_binary_arrays);
  if (number_spectra_binary_arrays > 0) {
    const std::string search_run = "//run";
    const pugi::xpath_node xps_run = root.select_node(search_run.c_str());
    const pugi::xml_node spec_list = xps_run.node().child("spectrumList");
    const pugi::xml_node spec = spec_list.first_child();
    const sc::MZML_SPECTRUM spectrum(spec);
    const std::vector<sc::MZML_BINARY_METADATA> binary_metadata = spectrum.extract_binary_metadata();

    for (size_t i = 0; i < binary_metadata.size(); ++i) {
      names[i] = binary_metadata[i].data_name_short;
    }
  }

  return names;
};

std::vector<sc::MZML_BINARY_METADATA> sc::mzml::MZML::get_spectra_binary_metadata() const {
  const int number_spectra_binary_arrays = get_number_spectra_binary_arrays();
  std::vector<sc::MZML_BINARY_METADATA> metadata(number_spectra_binary_arrays);
  if (number_spectra_binary_arrays > 0) {
    const std::string search_run = "//run";
    const pugi::xpath_node xps_run = root.select_node(search_run.c_str());
    const pugi::xml_node spec_list = xps_run.node().child("spectrumList");
    const pugi::xml_node spec = spec_list.first_child();
    const sc::MZML_SPECTRUM spectrum(spec);
    metadata = spectrum.extract_binary_metadata();
  }

  return metadata;
};

std::string sc::mzml::MZML::get_type() const {
  const int number_spectra = get_number_spectra();
  std::string type = "Unknown";
  if (number_spectra > 0) {
    const std::vector<int>& level = get_level();
    const std::vector<double>& pre_mz = get_spectra_precursor_mz();
    const std::vector<double>& pre_mzhigh = get_spectra_precursor_window_mzhigh();
    bool no_pre_mz = std::all_of(pre_mz.begin(), pre_mz.end(), [](double d) { return std::isnan(d); });
    bool no_pre_mzhigh = std::all_of(pre_mzhigh.begin(), pre_mzhigh.end(), [](double d) { return std::isnan(d); });
    if (level.size() > 1) {
      if (no_pre_mz) {
        if (no_pre_mzhigh) {
          type = "MS/MS-AllIons";
        } else {
          type = "MS/MS-DIA";
        }
      } else {
        type = "MS/MS-DDA";
      }
    } else if (level[0] == 1) {
      type = "MS";
    } else {
      type = "MSn";
    }
  }
  return type;
};

std::string sc::mzml::MZML::get_time_stamp() const {
  const std::string search_run = "//run";
  const pugi::xpath_node xps_run = root.select_node(search_run.c_str());
  return xps_run.node().attribute("startTimeStamp").as_string();
};

std::vector<int> sc::mzml::MZML::get_spectra_index(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<int> idxs;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return idxs;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  idxs.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    idxs[i] = spec.extract_spec_index();
  }

  return idxs;
};

std::vector<int> sc::mzml::MZML::get_spectra_scan_number(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<int> scans;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return scans;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  scans.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    scans[i] = spec.extract_spec_scan();
  }

  return scans;
};

std::vector<int> sc::mzml::MZML::get_spectra_array_length(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<int> lengths;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return lengths;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  lengths.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    lengths[i] = spec.extract_spec_array_length();
  }

  return lengths;
};

std::vector<int> sc::mzml::MZML::get_spectra_level(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<int> levels;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return levels;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  levels.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    levels[i] = spec.extract_spec_level();
  }

  return levels;
};

std::vector<std::string> sc::mzml::MZML::get_spectra_mode(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<std::string> modes;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return modes;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  modes.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    modes[i] = spec.extract_spec_mode();
  }

  return modes;
};

std::vector<std::string> sc::mzml::MZML::get_spectra_polarity(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<std::string> polarities;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return polarities;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  polarities.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    polarities[i] = spec.extract_spec_polarity();
  }

  return polarities;
};

std::vector<double> sc::mzml::MZML::get_spectra_lowmz(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> lowmzs;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return lowmzs;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  lowmzs.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    lowmzs[i] = spec.extract_spec_lowmz();
  }

  return lowmzs;
};

std::vector<double> sc::mzml::MZML::get_spectra_highmz(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> highmzs;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return highmzs;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  highmzs.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    highmzs[i] = spec.extract_spec_highmz();
  }

  return highmzs;
};

std::vector<double> sc::mzml::MZML::get_spectra_bpmz(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> bpmzs;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return bpmzs;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  bpmzs.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    bpmzs[i] = spec.extract_spec_bpmz();
  }

  return bpmzs;
};

std::vector<double> sc::mzml::MZML::get_spectra_bpint(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> bpints;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return bpints;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  bpints.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    bpints[i] = spec.extract_spec_bpint();
  }

  return bpints;
};

std::vector<double> sc::mzml::MZML::get_spectra_tic(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> tics;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return tics;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  tics.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    tics[i] = spec.extract_spec_tic();
  }

  return tics;
};

std::vector<double> sc::mzml::MZML::get_spectra_rt(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> rts;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return rts;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  rts.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    rts[i] = spec.extract_scan_rt();
  }

  return rts;
};

std::vector<double> sc::mzml::MZML::get_spectra_drift(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> dts;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return dts;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  dts.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    dts[i] = spec.extract_scan_drift();
  }

  return dts;
};

std::vector<int> sc::mzml::MZML::get_spectra_precursor_scan(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<int> scans;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return scans;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  scans.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    scans[i] = spec.extract_precursor_scan();
  }

  return scans;
};

std::vector<double> sc::mzml::MZML::get_spectra_precursor_mz(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> mzs;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return mzs;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  mzs.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    mzs[i] = spec.extract_ion_mz();
  }

  return mzs;
};

std::vector<double> sc::mzml::MZML::get_spectra_precursor_window_mz(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> mzs;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return mzs;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  mzs.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    mzs[i] = spec.extract_window_mz();
  }

  return mzs;
};

std::vector<double> sc::mzml::MZML::get_spectra_precursor_window_mzlow(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> offsets;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return offsets;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  offsets.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    offsets[i] = spec.extract_window_mzlow();
  }

  return offsets;
};

std::vector<double> sc::mzml::MZML::get_spectra_precursor_window_mzhigh(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> offsets;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return offsets;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  offsets.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    offsets[i] = spec.extract_window_mzhigh();
  }

  return offsets;
};

std::vector<double> sc::mzml::MZML::get_spectra_collision_energy(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();
  
  std::vector<double> energies;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return energies;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const int n = indices.size();
  const std::vector<int> f_indices = indices;

  energies.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    const int& idx = f_indices[i];
    const sc::MZML_SPECTRUM& spec(spectra_nodes[idx]);
    energies[i] = spec.extract_activation_ce();
  }

  return energies;
};

std::vector<std::string> sc::mzml::MZML::get_polarity() const {
  const std::vector<std::string>& polarity = get_spectra_polarity();
  std::set<std::string> unique_polarity(polarity.begin(), polarity.end());
  return std::vector<std::string>(unique_polarity.begin(), unique_polarity.end());
};

std::vector<std::string> sc::mzml::MZML::get_mode() const {
  const std::vector<std::string>& mode = get_spectra_mode();
  std::set<std::string> unique_mode(mode.begin(), mode.end());
  return std::vector<std::string>(unique_mode.begin(), unique_mode.end());
};

std::vector<int> sc::mzml::MZML::get_level() const {
  const std::vector<int>& levels = get_spectra_level();
  std::set<int> unique_level(levels.begin(), levels.end());
  return std::vector<int>(unique_level.begin(), unique_level.end());
};

double sc::mzml::MZML::get_min_mz() const {
  const std::vector<double>& mz_low = get_spectra_lowmz();
  return *std::min_element(mz_low.begin(), mz_low.end());
};

double sc::mzml::MZML::get_max_mz() const {
  const std::vector<double>& mz_high = get_spectra_highmz();
  return *std::max_element(mz_high.begin(), mz_high.end());
};

double sc::mzml::MZML::get_start_rt() const {
  const std::vector<double>& rt = get_spectra_rt();
  return *std::min_element(rt.begin(), rt.end());
};

double sc::mzml::MZML::get_end_rt() const {
  const std::vector<double>& rt = get_spectra_rt();
  return *std::max_element(rt.begin(), rt.end());
};

bool sc::mzml::MZML::has_ion_mobility() const {
  const std::vector<double>& drift = get_spectra_drift();
  std::set<double> unique_drift(drift.begin(), drift.end());
  return unique_drift.size() > 1;
};

sc::MS_SUMMARY sc::mzml::MZML::get_summary() const {
  sc::MS_SUMMARY summary;
  summary.file_name = file_name;
  summary.file_path = file_path;
  summary.file_dir = file_dir;
  summary.file_extension = file_extension;
  summary.number_spectra = get_number_spectra();
  summary.number_chromatograms = get_number_chromatograms();
  summary.number_spectra_binary_arrays = get_number_spectra_binary_arrays();
  summary.format = format;
  summary.type = get_type();
  summary.polarity = get_polarity();
  summary.mode = get_mode();
  summary.level = get_level();
  summary.min_mz = get_min_mz();
  summary.max_mz = get_max_mz();
  summary.start_rt = get_start_rt();
  summary.end_rt = get_end_rt();
  summary.has_ion_mobility = has_ion_mobility();
  summary.time_stamp = get_time_stamp();
  return summary;
};

sc::MS_SPECTRA_HEADERS sc::mzml::MZML::get_spectra_headers(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();

  sc::MS_SPECTRA_HEADERS headers;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return headers;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const std::vector<int> idxs = indices;
  
  const int n = idxs.size();

  if (n == 0) {
    std::cerr << "No indices given!" << std::endl;
    return headers;
  }

  if (spectra_nodes.size() == 0) {
    std::cerr << "No spectra found!" << std::endl;
    return headers;
  }

  headers.resize_all(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; i++) {

    const int& index = idxs[i];

    const sc::MZML_SPECTRUM& sp = spectra_nodes[index];

    headers.index[i] = sp.extract_spec_index();
    headers.scan[i] = sp.extract_spec_scan();
    headers.array_length[i] = sp.extract_spec_array_length();
    headers.level[i] = sp.extract_spec_level();
    headers.mode[i] = sp.extract_spec_mode();
    headers.polarity[i] = sp.extract_spec_polarity();
    headers.lowmz[i] = sp.extract_spec_lowmz();
    headers.highmz[i] = sp.extract_spec_highmz();
    headers.bpmz[i] = sp.extract_spec_bpmz();
    headers.bpint[i] = sp.extract_spec_bpint();
    headers.tic[i] = sp.extract_spec_tic();
    headers.title[i] = sp.extract_spec_title();
    headers.rt[i] = sp.extract_scan_rt();
    headers.drift[i] = sp.extract_scan_drift();
    headers.filter_string[i] = sp.extract_scan_filter_string();
    headers.config[i] = sp.extract_scan_config();
    headers.injection_ion_time[i] = sp.extract_scan_injection_ion_time();

    if (sp.has_precursor()) {
      headers.precursor_scan[i] = sp.extract_precursor_scan();
      headers.window_mz[i] = sp.extract_window_mz();
      headers.window_mzlow[i] = sp.extract_window_mzlow();
      headers.window_mzhigh[i] = sp.extract_window_mzhigh();

      if (sp.has_selected_ion()) {
        headers.precursor_mz[i] = sp.extract_ion_mz();
        headers.precursor_intensity[i] = sp.extract_ion_intensity();
        headers.precursor_charge[i] = sp.extract_ion_charge();
      }

      if (sp.has_activation()) {
        headers.activation_type[i] = sp.extract_activation_type();
        headers.activation_ce[i] = sp.extract_activation_ce();
      }
    }
  } // end of i loop

  return headers;
};

sc::MS_CHROMATOGRAMS_HEADERS sc::mzml::MZML::get_chromatograms_headers(std::vector<int> indices) const {

  const int number_chromatograms = get_number_chromatograms();

  sc::MS_CHROMATOGRAMS_HEADERS headers;

  if (number_chromatograms == 0) {
    std::cerr << "There are no chromatograms in the mzML file!" << std::endl;
    return headers;
  }

  if (indices.size() == 0) {
    indices.resize(number_chromatograms);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const std::vector<int> idxs = indices;

  const int n = idxs.size();

  if (n == 0) {
    std::cerr << "No indices given!" << std::endl;
    return headers;
  }

  if (chrom_nodes.size() == 0) {
    std::cerr << "No chromatograms found!" << std::endl;
    return headers;
  }

  headers.resize_all(n);

  for (int i = 0; i < n; i++) {

    const int& index = idxs[i];

    const MZML_CHROMATOGRAM& ch(chrom_nodes[index]);

    headers.index[i] = ch.extract_index();
    headers.id[i] = ch.extract_id();
    headers.array_length[i] = ch.extract_array_length();
    headers.polarity[i] = ch.extract_polarity();
    
    if (ch.has_precursor()) {
      headers.precursor_mz[i] = ch.extract_precursor_mz();
      
      if (ch.has_activation()) {
        headers.activation_type[i] = ch.extract_activation_type();
        headers.activation_ce[i] = ch.extract_activation_ce();
      }
      
      if (ch.has_product()) {
        headers.product_mz[i] = ch.extract_product_mz();
      }
    }
  } // end of i loop

  return headers;
};

std::vector<std::vector<std::vector<double>>> sc::mzml::MZML::get_spectra(std::vector<int> indices) const {

  const int number_spectra = get_number_spectra();

  std::vector<std::vector<std::vector<double>>> sp;

  if (number_spectra == 0) {
    std::cerr << "There are no spectra in the mzML file!" << std::endl;
    return sp;
  }

  if (indices.size() == 0) {
    indices.resize(number_spectra);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const std::vector<int> idxs = indices;

  const int n = idxs.size();

  if (n == 0) {
    std::cerr << "No indices given!" << std::endl;
    return sp;
  }

  if (spectra_nodes.size() == 0) {
    std::cerr << "No spectra found!" << std::endl;
    return sp;
  }

  sp.resize(n);

  const std::vector<sc::MZML_BINARY_METADATA> binary_metadata = get_spectra_binary_metadata();

  #pragma omp parallel for
  for (int i = 0; i < n; i++) {
    const int& index = idxs[i];
    const sc::MZML_SPECTRUM& spec = spectra_nodes[index];
    sp[i] = spec.extract_binary_data(binary_metadata);
  }

  return sp;
};

std::vector<std::vector<std::vector<double>>> sc::mzml::MZML::get_chromatograms(std::vector<int> indices) const {

  const int number_chromatograms = get_number_chromatograms();

  std::vector<std::vector<std::vector<double>>> chr;

  if (number_chromatograms == 0) {
    std::cerr << "There are no chromatograms in the mzML file!" << std::endl;
    return chr;
  }
  
  if (indices.size() == 0) {
    indices.resize(number_chromatograms);
    std::iota(indices.begin(), indices.end(), 0);
  }

  const std::vector<int> idxs = indices;

  int n = idxs.size();

  if (n == 0) {
    std::cerr << "Warning: No indices given!" << std::endl;
    return chr;
  }

  if (chrom_nodes.size() == 0) {
    std::cerr << "No chromatograms found!" << std::endl;
    return chr;
  }

  chr.resize(n);

  // #pragma omp parallel for
  for (int i = 0; i < n; i++) {
    const int& index = idxs[i];
    const MZML_CHROMATOGRAM& ch = chrom_nodes[index];
    chr[i] = ch.extract_binary_data();
  }
  
  return chr;
};

std::vector<std::vector<std::string>> sc::mzml::MZML::get_software() const {

  std::vector<std::vector<std::string>> output(3);

  std::string search_software = "//softwareList/child::node()";

  pugi::xpath_node_set xps_software = root.select_nodes(search_software.c_str());

  if (xps_software.size() > 0) {

    for (pugi::xpath_node_set::const_iterator it = xps_software.begin(); it != xps_software.end(); ++it) {
      
      pugi::xpath_node node = *it;

      for (pugi::xml_node temp: node.node().children()) {
        std::string name = temp.attribute("name").as_string();

        if (name != "") {
          output[0].push_back(name);
          output[1].push_back(node.node().attribute("id").as_string());
          output[2].push_back(node.node().attribute("version").as_string());
        }
      }
    }
  }

  return output;
};

std::vector<std::vector<std::string>> sc::mzml::MZML::get_hardware() const {

  std::vector<std::vector<std::string>> output(2);

  std::string search_ref = "//referenceableParamGroup";
  pugi::xpath_node xp_ref = root.select_node(search_ref.c_str());

  if (xp_ref.node() != NULL) {
    for (pugi::xml_node temp: xp_ref.node().children()) {
      if (temp.attribute("name") != NULL) {
        output[0].push_back(temp.attribute("name").as_string());
        std::string val = temp.attribute("value").as_string();
        if (val != "") {
          output[1].push_back(temp.attribute("value").as_string());
        } else {
          output[1].push_back(temp.attribute("name").as_string());
        }
      }
    }
  }

  std::string search_inst = "//instrumentConfiguration";
  pugi::xpath_node xp_inst = root.select_node(search_inst.c_str());

  if (xp_inst.node() != NULL) {
    for (pugi::xml_node temp: xp_inst.node().children()) {
      if (temp.attribute("name") != NULL) {
        output[0].push_back(temp.attribute("name").as_string());
        std::string val = temp.attribute("value").as_string();
        if (val != "") {
          output[1].push_back(temp.attribute("value").as_string());
        } else {
          output[1].push_back(temp.attribute("name").as_string());
        }
      }
    }
  }

  std::string search_config = "//componentList/child::node()";
  pugi::xpath_node_set xps_config = root.select_nodes(search_config.c_str());

  if (xps_config.size() > 0) {
    for (pugi::xpath_node_set::const_iterator it = xps_config.begin(); it != xps_config.end(); ++it) {
      pugi::xpath_node node = *it;
      for (pugi::xml_node temp: node.node().children())
      {
        output[0].push_back(node.node().name());
        output[1].push_back(temp.attribute("name").as_string());
      }
    }
  }

  return output;
};

void sc::mzml::MZML::write_spectra(
  const std::vector<std::vector<std::vector<double>>>& spectra,
  const std::vector<std::string>& names, MS_SPECTRA_MODE mode, bool compress, bool save, std::string save_suffix) {

  if (spectra.size() == 0) {
    std::cerr << "No spectra to write!" << std::endl;
    return;
  }

  if (spectra[0].size() != names.size()) {
    std::cerr << "Number of spectra and names do not match!" << std::endl;
    return;
  }

  std::string search_run = "//run";
  
  pugi::xml_node run_node = root.select_node(search_run.c_str()).node();
  
  pugi::xml_node spec_list_node = run_node.child("spectrumList");

  std::vector<pugi::xml_node> spectra_nodes;

  if (spec_list_node) {

    for (pugi::xml_node child = spec_list_node.first_child(); child; child = child.next_sibling()) {
      spectra_nodes.push_back(child);
    }

    if (spectra_nodes.size() != spectra.size()) {
      std::cerr << "Number of spectra in the file and the number of spectra to write do not match!" << std::endl;
      return;
    }

  } else {
    std::cerr << "Spectrum list not found in the mzML file!" << std::endl;
    return;
  }

  int number_spectra = spectra.size();

  std::string number_spectra_str = std::to_string(number_spectra);

  spec_list_node.attribute("count").set_value(number_spectra_str.c_str());

  for (size_t i = 0; i < spectra.size(); i++) {

    pugi::xml_node spec = spectra_nodes[i];

    const std::vector<double>& mz = spectra[i][0];

    const std::vector<double>& intensity = spectra[i][1];

    spec.attribute("defaultArrayLength").set_value(spectra[i][0].size());

    if (mode == MS_SPECTRA_MODE::CENTROID) {
      pugi::xml_node node_mode = spec.find_child_by_attribute("cvParam", "accession", "MS:1000128");

      if (node_mode) {
        node_mode.attribute("accession").set_value("MS:1000127");
        node_mode.attribute("name").set_value("centroid spectrum");
      }
    }

    pugi::xml_node low_mz_node = spec.find_child_by_attribute("cvParam", "name", "lowest observed m/z");

    double low_mz = *std::min_element(mz.begin(), mz.end());  

    low_mz_node.attribute("value").set_value(low_mz);

    pugi::xml_node high_mz_node = spec.find_child_by_attribute("cvParam", "name", "highest observed m/z");

    double high_mz = *std::max_element(mz.begin(), mz.end());

    high_mz_node.attribute("value").set_value(high_mz);
    
    pugi::xml_node bp_mz_node = spec.find_child_by_attribute("cvParam", "name", "base peak m/z");

    double bp_mz = mz[std::distance(intensity.begin(), std::max_element(intensity.begin(), intensity.end()))];

    bp_mz_node.attribute("value").set_value(bp_mz);

    pugi::xml_node bp_int_node = spec.find_child_by_attribute("cvParam", "name", "base peak intensity");

    double bp_int = *std::max_element(intensity.begin(), intensity.end());

    bp_int_node.attribute("value").set_value(bp_int);

    pugi::xml_node tic_node = spec.find_child_by_attribute("cvParam", "name", "total ion current");

    double tic = std::accumulate(intensity.begin(), intensity.end(), 0.0);

    tic_node.attribute("value").set_value(tic);

    pugi::xml_node bin_array_list = spec.child("binaryDataArrayList");

    bin_array_list.remove_children();

    bin_array_list.attribute("count").set_value(spectra[i].size());

    for (size_t j = 0; j < spectra[i].size(); j++) {

      const std::vector<double>& x = spectra[i][j];

      std::string x_enc = sc::encode_little_endian(x, 8);
      
      if (compress) x_enc = sc::compress_zlib(x_enc);
      
      x_enc = sc::encode_base64(x_enc);

      pugi::xml_node bin_array = bin_array_list.append_child("binaryDataArray");

      bin_array.append_attribute("encodedLength") = x_enc.size();

      pugi::xml_node bin = bin_array.append_child("cvParam");

      bin.append_attribute("cvRef") = "MS";

      bin.append_attribute("accession") = "MS:1000523";

      bin.append_attribute("name") = "64-bit float";

      bin.append_attribute("value") = "";

      bin = bin_array.append_child("cvParam");

      if (compress) {
        bin.append_attribute("cvRef") = "MS";
        bin.append_attribute("accession") = "MS:1000574";
        bin.append_attribute("name") = "zlib compression";
        bin.append_attribute("value") = "";

      } else {
        bin.append_attribute("cvRef") = "MS";
        bin.append_attribute("accession") = "MS:1000576";
        bin.append_attribute("name") = "no compression";
        bin.append_attribute("value") = "";
      }

      bin = bin_array.append_child("cvParam");

      bin.append_attribute("cvRef") = "MS";

      if (j == 0) {
        bin.append_attribute("accession") = "MS:1000514";
        bin.append_attribute("name") = "m/z array";
        bin.append_attribute("value") = "";
        bin.append_attribute("unitCvRef") = "MS";
        bin.append_attribute("unitAccession") = "MS:1000040";
        bin.append_attribute("unitName") = "m/z";

      } else if (j == 1) {
        bin.append_attribute("accession") = "MS:1000515";
        bin.append_attribute("name") = "intensity array";
        bin.append_attribute("value") = "";
        bin.append_attribute("unitCvRef") = "MS";
        bin.append_attribute("unitAccession") = "MS:1000131";
        bin.append_attribute("unitName") = "number of detector counts";

      } else {
        bin.append_attribute("accession") = "MS:1000786";
        bin.append_attribute("name") = "non-standard data array";
        bin.append_attribute("value") = names[j].c_str();
      }

      pugi::xml_node bin_data = bin_array.append_child("binary");

      bin_data.append_child(pugi::node_pcdata).set_value(x_enc.c_str());
    }
  }

  if (save) {

    if (save_suffix == "") save_suffix = "_modified";

    std::string new_file_path = file_dir + "/" + file_name + save_suffix + "." + file_extension;

    if (new_file_path == file_path) {
      std::cerr << "The new file path is the same as the original file path!" << std::endl;
      return;
    }
    
    if (std::filesystem::exists(new_file_path)) std::filesystem::remove(new_file_path);

    if (!doc.save_file(new_file_path.c_str())) std::cerr << "Error saving the file!" << std::endl;
  }
};

void sc::mzml::test_extract_spectra_mzml(const std::string& file) {
  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << "Test Extract Spectra mzML file:" << std::endl;
  std::cout << std::endl;

  MZML mzml(file);

  std::cout << "Root name: " << mzml.name << std::endl;

  std::cout << "Number of spectra: " << mzml.get_number_spectra() << std::endl;

  MS_SPECTRA_HEADERS hd;

  hd = mzml.get_spectra_headers();

  int number = hd.index.size();

  std::cout << "Size of vector in headers struct: " << number << std::endl;

  std::cout << "Retention time of 10th spectrum: " << hd.rt[10] << std::endl;

  std::cout << "Number of binary arrays: " << mzml.get_number_spectra_binary_arrays() << std::endl;

  std::vector<std::vector<std::vector<double>>> spectra;

  std::vector<int> indices = {10, 15};

  spectra = mzml.get_spectra(indices);

  std::cout << "Number of extracted spectra: " << spectra.size() << std::endl;

  std::cout << "Number of traces in the first extracted spectrum: " << spectra[0][0].size() << std::endl;

  std::cout << std::endl;
};

void sc::mzml::test_extract_chromatograms_mzml(const std::string& file) {
  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << "Test Chromatograms mzML file:" << std::endl;
  std::cout << std::endl;

  MZML mzml(file);

  std::cout << "Root name: " << mzml.name << std::endl;

  std::cout << "Number of chromatograms: " << mzml.get_number_chromatograms() << std::endl;

  MS_CHROMATOGRAMS_HEADERS ch;

  ch = mzml.get_chromatograms_headers();

  int number_chroms = ch.index.size();

  std::cout << "Size of vector in headers chroms struct: " << number_chroms << std::endl;

  std::cout << "Polarity of 5th chrom: " << ch.polarity[5] << std::endl;

  std::vector<std::vector<std::vector<double>>> chroms;

  std::vector<int> indices = {1, 5, 6};

  chroms = mzml.get_chromatograms(indices);

  std::cout << "Number of extracted chroms: " << chroms.size() << std::endl;

  std::cout << "Number of variables in 1st chromatogram: " << chroms[0].size() << std::endl;

  std::cout << "Number of variables in 6th chromatogram: " << chroms[2].size() << std::endl;

  std::cout << "Number of traces in the first extracted chrom: " << chroms[0][0].size() << std::endl;

  std::cout << std::endl;
};
