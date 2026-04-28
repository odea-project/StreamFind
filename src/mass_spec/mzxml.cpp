#define PUGIXML_HEADER_ONLY
#include "../external/pugixml-1.14/src/pugixml.hpp"

#include "mzxml.h"
#include "utils.h"

#include <filesystem>

namespace ms {
namespace mzxml {

namespace {

float parse_rt(const std::string& rt) {
  if (rt.empty()) return 0.0f;
  if (rt.rfind("PT", 0) == 0 && rt.back() == 'S') {
    return std::stof(rt.substr(2, rt.size() - 3));
  }
  return 0.0f;
}

std::vector<float> decode_peaks(const pugi::xml_node& peaks_node, int precision, bool compressed) {
  std::string encoded = peaks_node.child_value();
  if (encoded.empty()) return {};
  std::string decoded = utils::decode_base64(encoded);
  if (compressed) decoded = ms::utils::decompress_zlib(decoded);
  if (decoded.empty()) return {};
  return utils::decode_little_endian_to_float(decoded, precision);
}

MS_SPECTRUM make_spectrum(const pugi::xml_node& scan) {
  MS_SPECTRUM s{};
  s.index = scan.attribute("num").as_int();
  s.scan = s.index;
  s.array_length = scan.attribute("peaksCount").as_int();
  s.level = scan.attribute("msLevel").as_int(1);
  s.mode = 0;
  s.polarity = 0;
  s.lowmz = scan.attribute("lowMz").as_float(0.0f);
  s.highmz = scan.attribute("highMz").as_float(0.0f);
  s.bpmz = scan.attribute("basePeakMz").as_float(0.0f);
  s.bpint = scan.attribute("basePeakIntensity").as_float(0.0f);
  s.tic = scan.attribute("totIonCurrent").as_float(0.0f);
  s.configuration = 0;
  s.rt = parse_rt(scan.attribute("retentionTime").as_string());
  s.mobility = 0.0f;

  auto prec = scan.child("precursorMz");
  if (prec) {
    s.precursor_mz = prec.text().as_float(0.0f);
    s.precursor_charge = prec.attribute("precursorCharge").as_int(0);
    s.activation_ce = prec.attribute("collisionEnergy").as_float(0.0f);
  }

  auto peaks = scan.child("peaks");
  if (peaks) {
    int precision = peaks.attribute("precision").as_int(32);
    bool compressed = std::string(peaks.attribute("compressionType").as_string()).find("zlib") != std::string::npos;
    std::vector<float> vals = decode_peaks(peaks, precision, compressed);
    std::vector<float> mz;
    std::vector<float> intensity;
    for (size_t i = 0; i + 1 < vals.size(); i += 2) {
      mz.push_back(vals[i]);
      intensity.push_back(vals[i + 1]);
    }
    s.binary_names = {"mz", "intensity"};
    s.binary_data = {std::move(mz), std::move(intensity)};
    s.binary_arrays_count = static_cast<int>(s.binary_data.size());
  }

  return s;
}

} // namespace

struct Impl {
  pugi::xml_document doc;
  std::string file_path;
  std::string file_name;
  std::vector<MS_SPECTRUM> spectra;
  bool loaded = false;
};

Reader::Reader(const std::string& file) : MS_READER(file), pimpl(std::make_unique<Impl>()) {
  pimpl->file_path = file;
  pimpl->file_name = std::filesystem::path(file).filename().string();
  pugi::xml_parse_result result = pimpl->doc.load_file(file.c_str());
  if (!result) throw std::runtime_error(std::string("Failed to parse mzXML file: ") + result.description());

  auto root = pimpl->doc.document_element();
  for (auto msrun : root.children("msRun")) {
    for (auto scan : msrun.children("scan")) pimpl->spectra.push_back(make_spectrum(scan));
  }
  pimpl->loaded = true;
}

Reader::~Reader() = default;

std::string Reader::get_format() { return "mzXML"; }
std::string Reader::get_type() { return "MS"; }
int Reader::get_number_spectra() { return static_cast<int>(pimpl->spectra.size()); }
int Reader::get_number_chromatograms() { return 0; }
int Reader::get_number_spectra_binary_arrays() { return static_cast<int>(pimpl->spectra.size() * 2); }
std::string Reader::get_time_stamp() { return {}; }
std::vector<int> Reader::get_polarity() {
  std::vector<int> out;
  for (const auto& s : pimpl->spectra) out.push_back(s.polarity);
  return out;
}
std::vector<int> Reader::get_mode() { return std::vector<int>(pimpl->spectra.size(), 0); }
std::vector<int> Reader::get_level() {
  std::vector<int> out;
  for (const auto& s : pimpl->spectra) out.push_back(s.level);
  return out;
}
std::vector<int> Reader::get_configuration() { return std::vector<int>(pimpl->spectra.size(), 0); }
float Reader::get_min_mz() { return 0.0f; }
float Reader::get_max_mz() { return 0.0f; }
float Reader::get_start_rt() { return 0.0f; }
float Reader::get_end_rt() { return 0.0f; }
bool Reader::has_ion_mobility() { return false; }

MS_SUMMARY Reader::get_summary() {
  MS_SUMMARY s{};
  s.file_name = pimpl->file_name;
  s.file_path = pimpl->file_path;
  s.file_dir = std::filesystem::path(pimpl->file_path).parent_path().string();
  s.file_extension = std::filesystem::path(pimpl->file_path).extension().string();
  s.number_spectra = static_cast<int>(pimpl->spectra.size());
  s.number_chromatograms = 0;
  s.number_spectra_binary_arrays = get_number_spectra_binary_arrays();
  s.format = "mzXML";
  s.type = "MS";
  s.has_ion_mobility = false;
  return s;
}

std::vector<int> Reader::get_spectra_index(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  std::vector<int> out;
  for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].index);
  return out;
}

std::vector<int> Reader::get_spectra_scan_number(std::vector<int> indices) { return get_spectra_index(std::move(indices)); }
std::vector<int> Reader::get_spectra_array_length(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  std::vector<int> out;
  for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].array_length);
  return out;
}
std::vector<int> Reader::get_spectra_level(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  std::vector<int> out;
  for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].level);
  return out;
}
std::vector<int> Reader::get_spectra_configuration(std::vector<int> indices) { return std::vector<int>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0); }
std::vector<int> Reader::get_spectra_mode(std::vector<int> indices) { return std::vector<int>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0); }
std::vector<int> Reader::get_spectra_polarity(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  std::vector<int> out;
  for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].polarity);
  return out;
}
std::vector<float> Reader::get_spectra_lowmz(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_highmz(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_bpmz(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_bpint(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_tic(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_rt(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  std::vector<float> out;
  for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].rt);
  return out;
}
std::vector<float> Reader::get_spectra_mobility(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<int> Reader::get_spectra_precursor_scan(std::vector<int> indices) { return std::vector<int>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0); }
std::vector<float> Reader::get_spectra_precursor_mz(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  std::vector<float> out;
  for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].precursor_mz);
  return out;
}
std::vector<float> Reader::get_spectra_precursor_window_mz(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_precursor_window_mzlow(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_precursor_window_mzhigh(std::vector<int> indices) { return std::vector<float>(indices.empty() ? pimpl->spectra.size() : indices.size(), 0.0f); }
std::vector<float> Reader::get_spectra_collision_energy(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  std::vector<float> out;
  for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].activation_ce);
  return out;
}

MS_SPECTRA_HEADERS Reader::get_spectra_headers(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  MS_SPECTRA_HEADERS h; h.resize_all(static_cast<int>(indices.size()));
  int j = 0;
  for (int i : indices) {
    if (i < 0 || static_cast<size_t>(i) >= pimpl->spectra.size()) continue;
    const auto& s = pimpl->spectra[i];
    h.index[j] = s.index; h.scan[j] = s.scan; h.array_length[j] = s.array_length; h.level[j] = s.level; h.mode[j] = s.mode; h.polarity[j] = s.polarity;
    h.lowmz[j] = s.lowmz; h.highmz[j] = s.highmz; h.bpmz[j] = s.bpmz; h.bpint[j] = s.bpint; h.tic[j] = s.tic; h.configuration[j] = s.configuration; h.rt[j] = s.rt; h.mobility[j] = s.mobility;
    h.window_mz[j] = s.window_mz; h.window_mzlow[j] = s.window_mzlow; h.window_mzhigh[j] = s.window_mzhigh; h.precursor_mz[j] = s.precursor_mz; h.precursor_intensity[j] = s.precursor_intensity; h.precursor_charge[j] = s.precursor_charge; h.activation_ce[j] = s.activation_ce;
    ++j;
  }
  return h;
}

MS_CHROMATOGRAMS_HEADERS Reader::get_chromatograms_headers(std::vector<int>) { return {}; }

std::vector<std::vector<std::vector<float>>> Reader::get_spectra(std::vector<int> indices) {
  if (indices.empty()) { indices.resize(pimpl->spectra.size()); std::iota(indices.begin(), indices.end(), 0); }
  std::vector<std::vector<std::vector<float>>> out;
  out.reserve(indices.size());
  for (int i : indices) if (i >= 0 && static_cast<size_t>(i) < pimpl->spectra.size()) out.push_back(pimpl->spectra[i].binary_data);
  return out;
}

std::vector<std::vector<std::vector<float>>> Reader::get_chromatograms(std::vector<int>) { return {}; }
std::vector<std::vector<std::string>> Reader::get_software() { return {}; }
std::vector<std::vector<std::string>> Reader::get_hardware() { return {}; }
MS_SPECTRUM Reader::get_spectrum(const int& idx) { return (idx >= 0 && static_cast<size_t>(idx) < pimpl->spectra.size()) ? pimpl->spectra[idx] : MS_SPECTRUM{}; }

} // namespace mzxml
} // namespace ms
