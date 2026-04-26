#include "reader.h"
#include "mzml.h"
#include "mzxml.h"

#include <algorithm>
#include <cctype>
#include <fstream>
#include <filesystem>

namespace ms {

std::string detect_format(const std::string& file_path) {
  std::string lower = file_path;
  std::transform(lower.begin(), lower.end(), lower.begin(), [](unsigned char c) {
    return static_cast<char>(std::tolower(c));
  });

  if (lower.find(".mzml") != std::string::npos) return "mzML";
  if (lower.find(".mzxml") != std::string::npos) return "mzXML";

  std::ifstream file(file_path);
  std::string line;
  for (int i = 0; i < 10 && std::getline(file, line); ++i) {
    if (line.find("mzML") != std::string::npos) return "mzML";
    if (line.find("mzXML") != std::string::npos) return "mzXML";
  }

  return "unknown";
}

std::unique_ptr<MS_READER> create_reader(const std::string& file_path) {
  const std::string format = detect_format(file_path);
  if (format == "mzML") return std::make_unique<mzml::Reader>(file_path);
  if (format == "mzXML") return std::make_unique<mzxml::Reader>(file_path);
  throw std::runtime_error("Unsupported file format: " + format);
}

MS_FILE::MS_FILE(const std::string& file) {
  file_path = file;
  file_dir = std::filesystem::path(file).parent_path().string();
  file_name = std::filesystem::path(file).stem().string();
  file_extension = std::filesystem::path(file).extension().string();
  if (!file_extension.empty() && file_extension.front() == '.') file_extension.erase(file_extension.begin());
  format = detect_format(file);
  format_case = 0;
  if (format == "mzXML") format_case = 1;
  ms = create_reader(file);
}

MS_TARGETS_SPECTRA MS_FILE::get_spectra_targets(const MS_TARGETS& targets, const MS_SPECTRA_HEADERS& hd, const float& minIntLv1, const float& minIntLv2) {
  MS_TARGETS_SPECTRA out;
  const std::vector<int> all_indices = [] (size_t n) {
    std::vector<int> idx(n);
    std::iota(idx.begin(), idx.end(), 0);
    return idx;
  }(hd.size());

  auto raw = ms->get_spectra(all_indices);
  out.id.reserve(targets.id.size());
  for (size_t t = 0; t < targets.id.size(); ++t) {
    const bool precursor = t < targets.precursor.size() ? targets.precursor[t] : false;
    const int level = t < targets.level.size() ? targets.level[t] : 0;
    const int polarity = t < targets.polarity.size() ? targets.polarity[t] : 0;
    const float mzmin = t < targets.mzmin.size() ? targets.mzmin[t] : 0.0f;
    const float mzmax = t < targets.mzmax.size() ? targets.mzmax[t] : 0.0f;
    const float rtmin = t < targets.rtmin.size() ? targets.rtmin[t] : 0.0f;
    const float rtmax = t < targets.rtmax.size() ? targets.rtmax[t] : 0.0f;
    const float mzcenter = t < targets.mz.size() ? targets.mz[t] : 0.0f;
    const float mmin = mzmin == 0.0f && mzmax == 0.0f ? mzcenter - 0.01f : mzmin;
    const float mmax = mzmin == 0.0f && mzmax == 0.0f ? mzcenter + 0.01f : mzmax;

    out.id.push_back(targets.id[t]);
    out.polarity.push_back(polarity);
    out.level.push_back(level);
    out.pre_mz.push_back(mzcenter);
    out.pre_mzlow.push_back(mmin);
    out.pre_mzhigh.push_back(mmax);
    out.pre_ce.push_back(0.0f);
    out.rt.push_back(t < targets.rt.size() ? targets.rt[t] : 0.0f);
    out.mobility.push_back(t < targets.mobility.size() ? targets.mobility[t] : 0.0f);

    std::vector<float> mz;
    std::vector<float> intensity;
    for (size_t i = 0; i < hd.rt.size(); ++i) {
      if (i >= hd.level.size() || i >= hd.polarity.size()) continue;
      if (level != 0 && hd.level[i] != level) continue;
      // Select all matching scans and concatenate peak lists.
      if (i >= hd.level.size() || i >= hd.polarity.size()) continue;
      if (level != 0 && hd.level[i] != level) continue;
      if (polarity != 0 && hd.polarity[i] != polarity) continue;
      if (rtmin != 0.0f && hd.rt[i] < rtmin) continue;
      if (rtmax != 0.0f && hd.rt[i] > rtmax) continue;
      if (precursor && (i < hd.precursor_mz.size())) {
        const float pmz = hd.precursor_mz[i];
        if (mmin != 0.0f || mmax != 0.0f) {
          if (pmz < mmin || pmz > mmax) continue;
        }
      }
      if (i >= raw.size()) continue;
      const auto& scan = raw[i];
      if (scan.size() < 2) continue;
      for (size_t k = 0; k < scan[0].size(); ++k) {
        const float mzv = scan[0][k];
        const float inv = scan[1][k];
        if (level == 1 && inv < minIntLv1) continue;
        if (level >= 2 && inv < minIntLv2) continue;
        if (mzv < mmin || mzv > mmax) continue;
        mz.push_back(mzv);
        intensity.push_back(inv);
      }
    }

    out.mz.insert(out.mz.end(), mz.begin(), mz.end());
    out.intensity.insert(out.intensity.end(), intensity.begin(), intensity.end());
  }
  return out;
}

} // namespace ms
