#define PUGIXML_HEADER_ONLY
#include "../external/pugixml-1.14/src/pugixml.hpp"

#include "mzml.h"
#include "utils.h"

#include <limits>
#include <filesystem>

namespace mass_spec {
namespace mzml {

namespace {

struct ParsedArray {
  std::string name;
  std::vector<float> values;
};

bool cv_has_name(const pugi::xml_node& node, const char* needle) {
  for (auto cv : node.children("cvParam")) {
    const std::string name = cv.attribute("name").as_string();
    const std::string accession = cv.attribute("accession").as_string();
    if (name.find(needle) != std::string::npos || accession.find(needle) != std::string::npos) return true;
  }
  return false;
}

std::string cv_first_name(const pugi::xml_node& node, const char* needle) {
  for (auto cv : node.children("cvParam")) {
    const std::string name = cv.attribute("name").as_string();
    const std::string accession = cv.attribute("accession").as_string();
    if (name.find(needle) != std::string::npos || accession.find(needle) != std::string::npos) return name;
  }
  return {};
}

float parse_scan_time(const pugi::xml_node& scan_node) {
  for (auto cv : scan_node.children("cvParam")) {
    const std::string name = cv.attribute("name").as_string();
    if (name.find("scan start time") != std::string::npos) {
      const std::string unit = cv.attribute("unitName").as_string();
      const float value = static_cast<float>(cv.attribute("value").as_double());
      if (unit.find("minute") != std::string::npos) {
        return value * 60.0f;
      }
      return value;
    }
  }
  return 0.0f;
}

std::vector<float> decode_binary(const pugi::xml_node& array_node) {
  std::string encoded = array_node.child_value("binary");
  if (encoded.empty()) return {};

  bool compressed = false;
  bool precision64 = false;
  for (auto cv : array_node.children("cvParam")) {
    const std::string name = cv.attribute("name").as_string();
    if (name.find("zlib compression") != std::string::npos) compressed = true;
    if (name.find("64-bit float") != std::string::npos) precision64 = true;
  }

  std::string decoded = utils::decode_base64(encoded);
  if (compressed) decoded = mass_spec::utils::decompress_zlib(decoded);
  if (decoded.empty()) return {};
  return utils::decode_little_endian_to_float(decoded, precision64 ? 8 : 4);
}

ParsedArray parse_array(const pugi::xml_node& array_node) {
  ParsedArray out;
  for (auto cv : array_node.children("cvParam")) {
    const std::string name = cv.attribute("name").as_string();
    if (name.find("m/z array") != std::string::npos || name.find("intensity array") != std::string::npos || name.find("time array") != std::string::npos) {
      out.name = name;
      break;
    }
  }
  out.values = decode_binary(array_node);
  return out;
}

MS_SPECTRUM make_spectrum(const pugi::xml_node& spectrum_node) {
  MS_SPECTRUM s{};
  s.index = spectrum_node.attribute("index").as_int();
  s.scan = s.index;
  s.array_length = spectrum_node.attribute("defaultArrayLength").as_int();

  int ms_level = 1;
  int polarity = 0;
  float rt = 0.0f;
  float prec_mz = 0.0f;
  float prec_int = 0.0f;
  int prec_charge = 0;
  float ce = 0.0f;

  for (auto cv : spectrum_node.children("cvParam")) {
    const std::string name = cv.attribute("name").as_string();
    if (name == "ms level") ms_level = cv.attribute("value").as_int(1);
    else if (name.find("positive scan") != std::string::npos) polarity = 1;
    else if (name.find("negative scan") != std::string::npos) polarity = -1;
    else if (name.find("lowest observed m/z") != std::string::npos) s.lowmz = cv.attribute("value").as_float(0.0f);
    else if (name.find("highest observed m/z") != std::string::npos) s.highmz = cv.attribute("value").as_float(0.0f);
    else if (name.find("base peak m/z") != std::string::npos) s.bpmz = cv.attribute("value").as_float(0.0f);
    else if (name.find("base peak intensity") != std::string::npos) s.bpint = cv.attribute("value").as_float(0.0f);
    else if (name.find("total ion current") != std::string::npos) s.tic = cv.attribute("value").as_float(0.0f);
  }
  s.level = ms_level;
  s.polarity = polarity;

  auto scan_list = spectrum_node.child("scanList");
  if (scan_list) {
    auto scan = scan_list.child("scan");
    if (scan) rt = parse_scan_time(scan);
  }
  s.rt = rt;

  auto prec_list = spectrum_node.child("precursorList");
  if (prec_list) {
    auto precursor = prec_list.child("precursor");
    if (precursor) {
      for (auto cv : precursor.children("cvParam")) {
        const std::string name = cv.attribute("name").as_string();
        if (name.find("activation energy") != std::string::npos) ce = cv.attribute("value").as_float(0.0f);
      }
      auto selected = precursor.child("selectedIonList").child("selectedIon");
      if (selected) {
        for (auto cv : selected.children("cvParam")) {
          const std::string name = cv.attribute("name").as_string();
          if (name.find("selected ion m/z") != std::string::npos) prec_mz = cv.attribute("value").as_float(0.0f);
          else if (name.find("peak intensity") != std::string::npos) prec_int = cv.attribute("value").as_float(0.0f);
          else if (name.find("charge state") != std::string::npos) prec_charge = cv.attribute("value").as_int(0);
        }
      }
    }
  }
  s.precursor_mz = prec_mz;
  s.precursor_intensity = prec_int;
  s.precursor_charge = prec_charge;
  s.activation_ce = ce;

  auto bdal = spectrum_node.child("binaryDataArrayList");
  if (bdal) {
    std::vector<float> mz;
    std::vector<float> intensity;
    for (auto bda : bdal.children("binaryDataArray")) {
      ParsedArray arr = parse_array(bda);
      if (arr.name.find("m/z array") != std::string::npos) mz = std::move(arr.values);
      else if (arr.name.find("intensity array") != std::string::npos) intensity = std::move(arr.values);
    }
    s.binary_names = {"mz", "intensity"};
    s.binary_data = {std::move(mz), std::move(intensity)};
    s.binary_arrays_count = static_cast<int>(s.binary_data.size());
    if (!s.binary_data.empty() && !s.binary_data[0].empty()) {
      if (s.lowmz == 0.0f) {
        s.lowmz = *std::min_element(s.binary_data[0].begin(), s.binary_data[0].end());
      }
      if (s.highmz == 0.0f) {
        s.highmz = *std::max_element(s.binary_data[0].begin(), s.binary_data[0].end());
      }
    }
    if (s.tic == 0.0f && s.binary_data.size() > 1) {
      for (float intensity_value : s.binary_data[1]) {
        s.tic += intensity_value;
        if (intensity_value > s.bpint) {
          s.bpint = intensity_value;
        }
      }
    }
    if (s.bpmz == 0.0f && s.bpint > 0.0f && s.binary_data.size() > 1) {
      for (size_t i = 0; i < s.binary_data[1].size() && i < s.binary_data[0].size(); ++i) {
        if (s.binary_data[1][i] == s.bpint) {
          s.bpmz = s.binary_data[0][i];
          break;
        }
      }
    }
  }
  return s;
}

MS_CHROMATOGRAMS_HEADERS make_chrom_headers(const std::vector<MS_SPECTRUM>& specs, const std::vector<pugi::xml_node>& chrom_nodes) {
  MS_CHROMATOGRAMS_HEADERS out;
  out.resize_all(static_cast<int>(chrom_nodes.size()));
  for (size_t i = 0; i < chrom_nodes.size(); ++i) {
    const auto& ch = chrom_nodes[i];
    out.index[i] = ch.attribute("index").as_int(static_cast<int>(i));
    out.id[i] = ch.attribute("id").as_string();
    out.array_length[i] = ch.attribute("defaultArrayLength").as_int();
    out.polarity[i] = 0;
    out.precursor_mz[i] = 0.0f;
    out.activation_ce[i] = 0.0f;
    out.product_mz[i] = 0.0f;
    for (auto cv : ch.children("cvParam")) {
      const std::string name = cv.attribute("name").as_string();
      if (name.find("positive scan") != std::string::npos) out.polarity[i] = 1;
      if (name.find("negative scan") != std::string::npos) out.polarity[i] = -1;
    }
    auto prec = ch.child("precursor");
    if (prec) {
      auto sel = prec.child("isolationWindow");
      if (sel) {
        for (auto cv : sel.children("cvParam")) {
          const std::string name = cv.attribute("name").as_string();
          if (name.find("isolation window target m/z") != std::string::npos) out.precursor_mz[i] = cv.attribute("value").as_float(0.0f);
        }
      }
      for (auto cv : prec.children("cvParam")) {
        const std::string name = cv.attribute("name").as_string();
        if (name.find("collision energy") != std::string::npos) out.activation_ce[i] = cv.attribute("value").as_float(0.0f);
      }
    }
    auto bdal = ch.child("binaryDataArrayList");
    if (bdal) {
      for (auto bda : bdal.children("binaryDataArray")) {
        ParsedArray arr = parse_array(bda);
        if (arr.name.find("time array") != std::string::npos) {
          // no-op for headers
        }
      }
    }
  }
  return out;
}

} // namespace

struct Impl {
  pugi::xml_document doc;
  std::string file_path;
  std::string file_name;
  std::vector<MS_SPECTRUM> spectra;
  std::vector<pugi::xml_node> chrom_nodes;
  bool loaded = false;
};

Reader::Reader(const std::string& file) : MS_READER(file), pimpl(std::make_unique<Impl>()) {
  pimpl->file_path = file;
  pimpl->file_name = std::filesystem::path(file).filename().string();
  pugi::xml_parse_result result = pimpl->doc.load_file(file.c_str());
  if (!result) throw std::runtime_error(std::string("Failed to parse mzML file: ") + result.description());
  auto root = pimpl->doc.document_element();
  for (auto node : root.select_nodes("//spectrumList/spectrum")) pimpl->spectra.push_back(make_spectrum(node.node()));
  for (auto node : root.select_nodes("//chromatogramList/chromatogram")) pimpl->chrom_nodes.push_back(node.node());
  pimpl->loaded = true;
}

Reader::~Reader() = default;
std::string Reader::get_format() { return "mzML"; }
std::string Reader::get_type() { return "MS"; }
int Reader::get_number_spectra() { return static_cast<int>(pimpl->spectra.size()); }
int Reader::get_number_chromatograms() { return static_cast<int>(pimpl->chrom_nodes.size()); }
std::vector<std::vector<std::vector<float>>> Reader::get_spectra(std::vector<int> indices) {
  std::vector<int> idx = indices;
  if (idx.empty()) { idx.resize(pimpl->spectra.size()); std::iota(idx.begin(), idx.end(), 0); }
  std::vector<std::vector<std::vector<float>>> out;
  out.reserve(idx.size());
  for (int i : idx) {
    if (i < 0 || static_cast<size_t>(i) >= pimpl->spectra.size()) continue;
    out.push_back(pimpl->spectra[i].binary_data);
  }
  return out;
}
std::vector<std::vector<std::vector<float>>> Reader::get_chromatograms(std::vector<int> indices) {
  std::vector<int> idx = indices;
  if (idx.empty()) { idx.resize(pimpl->chrom_nodes.size()); std::iota(idx.begin(), idx.end(), 0); }
  std::vector<std::vector<std::vector<float>>> out;
  out.reserve(idx.size());
  for (int i : idx) {
    if (i < 0 || static_cast<size_t>(i) >= pimpl->chrom_nodes.size()) continue;
    auto node = pimpl->chrom_nodes[i];
    std::vector<float> rt;
    std::vector<float> inten;
    auto bdal = node.child("binaryDataArrayList");
    if (bdal) {
      for (auto bda : bdal.children("binaryDataArray")) {
        ParsedArray arr = parse_array(bda);
        if (arr.name.find("time array") != std::string::npos) rt = std::move(arr.values);
        else if (arr.name.find("intensity array") != std::string::npos) inten = std::move(arr.values);
      }
    }
    out.push_back({std::move(rt), std::move(inten)});
  }
  return out;
}
int Reader::get_number_spectra_binary_arrays() { return static_cast<int>(pimpl->spectra.size() * 2); }
std::string Reader::get_time_stamp() { return {}; }
std::vector<int> Reader::get_polarity() { std::vector<int> out; for (const auto& s : pimpl->spectra) out.push_back(s.polarity); return out; }
std::vector<int> Reader::get_mode() { return std::vector<int>(pimpl->spectra.size(), 0); }
std::vector<int> Reader::get_level() { std::vector<int> out; for (const auto& s : pimpl->spectra) out.push_back(s.level); return out; }
std::vector<int> Reader::get_configuration() { return std::vector<int>(pimpl->spectra.size(), 0); }
float Reader::get_min_mz() {
  float out = std::numeric_limits<float>::max();
  for (const auto& s : pimpl->spectra) {
    if (s.lowmz > 0.0f && s.lowmz < out) out = s.lowmz;
  }
  return out == std::numeric_limits<float>::max() ? 0.0f : out;
}
float Reader::get_max_mz() {
  float out = 0.0f;
  for (const auto& s : pimpl->spectra) {
    if (s.highmz > out) out = s.highmz;
  }
  return out;
}
float Reader::get_start_rt() {
  float out = std::numeric_limits<float>::max();
  for (const auto& s : pimpl->spectra) {
    if (s.rt > 0.0f && s.rt < out) out = s.rt;
  }
  return out == std::numeric_limits<float>::max() ? 0.0f : out;
}
float Reader::get_end_rt() {
  float out = 0.0f;
  for (const auto& s : pimpl->spectra) {
    if (s.rt > out) out = s.rt;
  }
  return out;
}
bool Reader::has_ion_mobility() { return false; }
MS_SUMMARY Reader::get_summary() {
  MS_SUMMARY s{};
  s.file_name = pimpl->file_name;
  s.file_path = pimpl->file_path;
  s.file_dir = std::filesystem::path(pimpl->file_path).parent_path().string();
  s.file_extension = std::filesystem::path(pimpl->file_path).extension().string();
  s.number_spectra = static_cast<int>(pimpl->spectra.size());
  s.number_chromatograms = static_cast<int>(pimpl->chrom_nodes.size());
  s.number_spectra_binary_arrays = get_number_spectra_binary_arrays();
  s.format = "mzML";
  s.type = "MS";
  s.min_mz = get_min_mz();
  s.max_mz = get_max_mz();
  s.start_rt = get_start_rt();
  s.end_rt = get_end_rt();
  s.has_ion_mobility = false;
  return s;
}

std::vector<int> Reader::get_spectra_index(std::vector<int> indices) { if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); } std::vector<int> out; for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].index); return out; }
std::vector<int> Reader::get_spectra_scan_number(std::vector<int> indices) { return get_spectra_index(std::move(indices)); }
std::vector<int> Reader::get_spectra_array_length(std::vector<int> indices) { if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); } std::vector<int> out; for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].array_length); return out; }
std::vector<int> Reader::get_spectra_level(std::vector<int> indices) { if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); } std::vector<int> out; for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].level); return out; }
std::vector<int> Reader::get_spectra_configuration(std::vector<int> indices) { return std::vector<int>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0); }
std::vector<int> Reader::get_spectra_mode(std::vector<int> indices) { return std::vector<int>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0); }
std::vector<int> Reader::get_spectra_polarity(std::vector<int> indices) { if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); } std::vector<int> out; for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].polarity); return out; }
std::vector<float> Reader::get_spectra_lowmz(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_highmz(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_bpmz(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_bpint(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_tic(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_rt(std::vector<int> indices) { if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); } std::vector<float> out; for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].rt); return out; }
std::vector<float> Reader::get_spectra_mobility(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<int> Reader::get_spectra_precursor_scan(std::vector<int> indices) { return std::vector<int>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0); }
std::vector<float> Reader::get_spectra_precursor_mz(std::vector<int> indices) { if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); } std::vector<float> out; for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].precursor_mz); return out; }
std::vector<float> Reader::get_spectra_precursor_window_mz(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_precursor_window_mzlow(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_precursor_window_mzhigh(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_collision_energy(std::vector<int> indices) { if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); } std::vector<float> out; for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].activation_ce); return out; }

MS_SPECTRA_HEADERS Reader::get_spectra_headers(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  MS_SPECTRA_HEADERS h; h.resize_all(static_cast<int>(indices.size()));
  int j = 0;
  for (int i : indices) {
    if (i < 0 || static_cast<size_t>(i) >= pimpl->spectra.size()) continue;
    const auto& s = pimpl->spectra[i];
    h.index[j] = s.index; h.scan[j] = s.scan; h.array_length[j] = s.array_length; h.level[j] = s.level; h.mode[j] = s.mode; h.polarity[j] = s.polarity;
    h.lowmz[j] = s.lowmz; h.highmz[j] = s.highmz; h.bpmz[j] = s.bpmz; h.bpint[j] = s.bpint; h.tic[j] = s.tic; h.configuration[j] = s.configuration; h.rt[j] = s.rt; h.mobility[j] = s.mobility;
    h.window_mz[j] = s.window_mz; h.window_mzlow[j] = s.window_mzlow; h.window_mzhigh[j] = s.window_mzhigh; h.precursor_mz[j] = s.precursor_mz; h.precursor_intensity[j] = s.precursor_intensity; h.precursor_charge[j] = s.precursor_charge; h.activation_ce[j] = s.activation_ce; ++j;
  }
  return h;
}

MS_CHROMATOGRAMS_HEADERS Reader::get_chromatograms_headers(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->chrom_nodes.size()); std::iota(indices.begin(), indices.end(), 0); }
  MS_CHROMATOGRAMS_HEADERS h; h.resize_all(static_cast<int>(indices.size()));
  int j = 0;
  for (int i : indices) {
    if (i < 0 || static_cast<size_t>(i) >= pimpl->chrom_nodes.size()) continue;
    const auto& ch = pimpl->chrom_nodes[i];
    h.index[j] = ch.attribute("index").as_int(); h.id[j] = ch.attribute("id").as_string(); h.array_length[j] = ch.attribute("defaultArrayLength").as_int(); h.polarity[j] = 0; h.precursor_mz[j] = 0.0f; h.activation_ce[j] = 0.0f; h.product_mz[j] = 0.0f; ++j;
  }
  return h;
}

std::vector<std::vector<std::string>> Reader::get_software() { return {}; }
std::vector<std::vector<std::string>> Reader::get_hardware() { return {}; }
MS_SPECTRUM Reader::get_spectrum(const int& idx) { return (idx >= 0 && static_cast<size_t>(idx) < pimpl->spectra.size()) ? pimpl->spectra[idx] : MS_SPECTRUM{}; }

} // namespace mzml
} // namespace ms
