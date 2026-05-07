#pragma once

#include <algorithm>
#include <string>
#include <unordered_set>
#include <vector>

namespace mass_spec {

struct MS_TARGETS {
  std::vector<int> index;
  std::vector<std::string> id;
  std::vector<int> level;
  std::vector<int> polarity;
  std::vector<bool> precursor;
  std::vector<float> mz;
  std::vector<float> mzmin;
  std::vector<float> mzmax;
  std::vector<float> rt;
  std::vector<float> rtmin;
  std::vector<float> rtmax;
  std::vector<float> mobility;
  std::vector<float> mobilitymin;
  std::vector<float> mobilitymax;

  void resize_all(int n) {
    index.resize(n);
    id.resize(n);
    level.resize(n);
    polarity.resize(n);
    precursor.resize(n);
    mz.resize(n);
    mzmin.resize(n);
    mzmax.resize(n);
    rt.resize(n);
    rtmin.resize(n);
    rtmax.resize(n);
    mobility.resize(n);
    mobilitymin.resize(n);
    mobilitymax.resize(n);
  }
};

struct MS_TARGETS_SPECTRA {
  std::vector<std::string> id;
  std::vector<int> polarity;
  std::vector<int> level;
  std::vector<float> pre_mz;
  std::vector<float> pre_mzlow;
  std::vector<float> pre_mzhigh;
  std::vector<float> pre_ce;
  std::vector<float> rt;
  std::vector<float> mobility;
  std::vector<float> mz;
  std::vector<float> intensity;

  void resize_all(int n) {
    id.resize(n);
    polarity.resize(n);
    level.resize(n);
    pre_mz.resize(n);
    pre_mzlow.resize(n);
    pre_mzhigh.resize(n);
    pre_ce.resize(n);
    rt.resize(n);
    mobility.resize(n);
    mz.resize(n);
    intensity.resize(n);
  }

  size_t size() const { return id.size(); }

  int number_ids() const {
    std::unordered_set<std::string> unique_ids(id.begin(), id.end());
    return static_cast<int>(unique_ids.size());
  }

  MS_TARGETS_SPECTRA operator[](const std::string& unique_id) const {
    MS_TARGETS_SPECTRA target;
    for (size_t i = 0; i < id.size(); ++i) {
      if (id[i] == unique_id) {
        target.id.push_back(id[i]);
        target.polarity.push_back(polarity[i]);
        target.level.push_back(level[i]);
        target.pre_mz.push_back(pre_mz[i]);
        target.pre_mzlow.push_back(pre_mzlow[i]);
        target.pre_mzhigh.push_back(pre_mzhigh[i]);
        target.pre_ce.push_back(pre_ce[i]);
        target.rt.push_back(rt[i]);
        target.mobility.push_back(mobility[i]);
        target.mz.push_back(mz[i]);
        target.intensity.push_back(intensity[i]);
      }
    }
    return target;
  }
};

struct MS_TARGETS_INPUT {
  std::size_t size = 0;
  std::vector<std::string> id;
  std::vector<std::string> analysis;
  std::vector<std::string> polarity;
  std::vector<double> mass;
  std::vector<double> mass_min;
  std::vector<double> mass_max;
  std::vector<double> mz;
  std::vector<double> mzmin;
  std::vector<double> mzmax;
  std::vector<double> rt;
  std::vector<double> rtmin;
  std::vector<double> rtmax;
  std::vector<double> mobility;
  std::vector<double> mobilitymin;
  std::vector<double> mobilitymax;

  bool empty() const { return size == 0; }
};

struct MS_TARGETS_REQUEST {
  std::vector<std::string> analyses;
  std::vector<int> levels;
  MS_TARGETS_INPUT mass;
  MS_TARGETS_INPUT mz;
  MS_TARGETS_INPUT rt;
  MS_TARGETS_INPUT mobility;
  std::vector<std::string> id;
  double ppm = 20.0;
  double sec = 60.0;
  double millisec = 5.0;
  bool all_traces = true;
  double isolation_window = 1.3;
  float min_intensity_ms1 = 0.0f;
  float min_intensity_ms2 = 0.0f;
};

std::vector<std::string> sanitize_analyses(const std::vector<std::string>& analyses);
std::vector<MS_TARGETS> build_targets_by_analysis(const MS_TARGETS_REQUEST& request,
                                                  const std::vector<std::string>& analyses,
                                                  const std::vector<std::string>& polarities);
bool has_effective_targets(const MS_TARGETS& targets);
MS_TARGETS subset_targets(const MS_TARGETS& targets,
                          const std::vector<int>& levels,
                          bool all_traces,
                          double isolation_window);

}  // namespace ms
