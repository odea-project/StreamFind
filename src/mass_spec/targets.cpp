#include "targets.h"

#include <cmath>
#include <set>
#include <sstream>

namespace mass_spec {

namespace {

constexpr double kProtonMass = 1.007276;

std::string trim_copy(const std::string& value) {
  std::size_t start = 0;
  while (start < value.size() && std::isspace(static_cast<unsigned char>(value[start])) != 0) {
    ++start;
  }
  std::size_t end = value.size();
  while (end > start && std::isspace(static_cast<unsigned char>(value[end - 1])) != 0) {
    --end;
  }
  return value.substr(start, end - start);
}

double non_negative_or_zero(double value) {
  if (!std::isfinite(value) || value < 0.0) {
    return 0.0;
  }
  return value;
}

std::string normalize_polarity(const std::string& value) {
  const auto trimmed = trim_copy(value);
  if (trimmed == "positive") return "1";
  if (trimmed == "negative") return "-1";
  return trimmed;
}

bool is_empty_target_row(const MS_TARGETS& targets, std::size_t index) {
  const auto sum = targets.mz[index] + targets.rt[index] + targets.mobility[index] +
                   targets.mzmin[index] + targets.mzmax[index] +
                   targets.rtmin[index] + targets.rtmax[index] +
                   targets.mobilitymin[index] + targets.mobilitymax[index];
  return std::abs(sum) < 1e-12;
}

double value_or_zero(const std::vector<double>& values, std::size_t index) {
  if (index >= values.size()) return 0.0;
  return non_negative_or_zero(values[index]);
}

std::string string_or_empty(const std::vector<std::string>& values, std::size_t index) {
  if (index >= values.size()) return std::string();
  return trim_copy(values[index]);
}

std::string make_default_target_id(double mzmin,
                                   double mzmax,
                                   double rtmin,
                                   double rtmax,
                                   double mobilitymin,
                                   double mobilitymax) {
  std::ostringstream oss;
  oss.setf(std::ios::fixed);
  oss.precision(3);
  oss << mzmin << '-' << mzmax << '/';
  oss.precision(0);
  oss << rtmin << '-' << rtmax << '/' << mobilitymin << '-' << mobilitymax;
  return oss.str();
}

bool is_valid_polarity(const std::string& polarity) {
  return polarity.empty() || polarity == "1" || polarity == "-1" || polarity == "1|-1";
}

struct TargetSeed {
  std::string id;
  std::string analysis;
  std::string polarity;
  double mass = 0.0;
  double mass_min = 0.0;
  double mass_max = 0.0;
  double mz = 0.0;
  double mzmin = 0.0;
  double mzmax = 0.0;
  double rt = 0.0;
  double rtmin = 0.0;
  double rtmax = 0.0;
  double mobility = 0.0;
  double mobilitymin = 0.0;
  double mobilitymax = 0.0;
};

TargetSeed seed_from_table_row(const MS_TARGETS_INPUT& table, std::size_t index) {
  TargetSeed seed;
  seed.id = string_or_empty(table.id, index);
  seed.analysis = string_or_empty(table.analysis, index);
  seed.polarity = normalize_polarity(string_or_empty(table.polarity, index));
  seed.mass = value_or_zero(table.mass, index);
  seed.mass_min = value_or_zero(table.mass_min, index);
  seed.mass_max = value_or_zero(table.mass_max, index);
  seed.mz = value_or_zero(table.mz, index);
  seed.mzmin = value_or_zero(table.mzmin, index);
  seed.mzmax = value_or_zero(table.mzmax, index);
  seed.rt = value_or_zero(table.rt, index);
  seed.rtmin = value_or_zero(table.rtmin, index);
  seed.rtmax = value_or_zero(table.rtmax, index);
  seed.mobility = value_or_zero(table.mobility, index);
  seed.mobilitymin = value_or_zero(table.mobilitymin, index);
  seed.mobilitymax = value_or_zero(table.mobilitymax, index);
  return seed;
}

std::vector<TargetSeed> expand_seed_polarity(const TargetSeed& seed,
                                             const std::vector<std::string>& polarities) {
  std::vector<TargetSeed> out;
  const auto polarity = normalize_polarity(seed.polarity);
  if (!is_valid_polarity(polarity)) {
    return out;
  }

  if (polarity == "1|-1") {
    if (seed.mz > 0.0 || seed.mzmin > 0.0 || seed.mzmax > 0.0) {
      return out;
    }
    TargetSeed positive = seed;
    positive.polarity = "1";
    TargetSeed negative = seed;
    negative.polarity = "-1";
    out.push_back(positive);
    out.push_back(negative);
    return out;
  }

  if (!polarity.empty()) {
    out.push_back(seed);
    return out;
  }

  if (polarities.empty()) {
    TargetSeed positive = seed;
    positive.polarity = "1";
    TargetSeed negative = seed;
    negative.polarity = "-1";
    out.push_back(positive);
    out.push_back(negative);
    return out;
  }

  for (const auto& candidate : polarities) {
    TargetSeed expanded = seed;
    expanded.polarity = normalize_polarity(candidate);
    out.push_back(expanded);
  }
  return out;
}

std::vector<TargetSeed> expand_seed_analyses(const std::vector<TargetSeed>& seeds,
                                             const std::vector<std::string>& analyses) {
  std::vector<TargetSeed> out;
  for (const auto& seed : seeds) {
    if (!seed.analysis.empty()) {
      out.push_back(seed);
      continue;
    }
    if (analyses.empty()) {
      out.push_back(seed);
      continue;
    }
    for (const auto& analysis : analyses) {
      TargetSeed expanded = seed;
      expanded.analysis = analysis;
      out.push_back(expanded);
    }
  }
  return out;
}

void finalize_seed_ranges(TargetSeed& seed, double ppm, double sec, double millisec) {
  if (seed.mass > 0.0 && seed.mz == 0.0 && seed.mzmin == 0.0 && seed.mzmax == 0.0) {
    const double sign = seed.polarity == "-1" ? -1.0 : 1.0;
    seed.mz = non_negative_or_zero(seed.mass + (kProtonMass * sign));
  }

  if (seed.mass_min > 0.0 && seed.mass_max > 0.0 && seed.mz == 0.0 && seed.mzmin == 0.0 && seed.mzmax == 0.0) {
    const double sign = seed.polarity == "-1" ? -1.0 : 1.0;
    seed.mzmin = non_negative_or_zero(seed.mass_min + (kProtonMass * sign));
    seed.mzmax = non_negative_or_zero(seed.mass_max + (kProtonMass * sign));
  }

  if (seed.mz > 0.0 && seed.mzmin == 0.0 && seed.mzmax == 0.0) {
    const double ppm_delta = (ppm / 1e6) * seed.mz;
    seed.mzmin = non_negative_or_zero(seed.mz - ppm_delta);
    seed.mzmax = non_negative_or_zero(seed.mz + ppm_delta);
  }
  if (seed.mz == 0.0 && (seed.mzmin > 0.0 || seed.mzmax > 0.0)) {
    seed.mz = (seed.mzmin + seed.mzmax) / 2.0;
  }

  if (seed.rt > 0.0 && seed.rtmin == 0.0 && seed.rtmax == 0.0) {
    seed.rtmin = non_negative_or_zero(seed.rt - sec);
    seed.rtmax = non_negative_or_zero(seed.rt + sec);
  }
  if (seed.rt == 0.0 && (seed.rtmin > 0.0 || seed.rtmax > 0.0)) {
    seed.rt = (seed.rtmin + seed.rtmax) / 2.0;
  }

  if (seed.mobility > 0.0 && seed.mobilitymin == 0.0 && seed.mobilitymax == 0.0) {
    seed.mobilitymin = non_negative_or_zero(seed.mobility - millisec);
    seed.mobilitymax = non_negative_or_zero(seed.mobility + millisec);
  }
  if (seed.mobility == 0.0 && (seed.mobilitymin > 0.0 || seed.mobilitymax > 0.0)) {
    seed.mobility = (seed.mobilitymin + seed.mobilitymax) / 2.0;
  }

  seed.mz = non_negative_or_zero(seed.mz);
  seed.mzmin = non_negative_or_zero(seed.mzmin);
  seed.mzmax = non_negative_or_zero(seed.mzmax);
  seed.rt = non_negative_or_zero(seed.rt);
  seed.rtmin = non_negative_or_zero(seed.rtmin);
  seed.rtmax = non_negative_or_zero(seed.rtmax);
  seed.mobility = non_negative_or_zero(seed.mobility);
  seed.mobilitymin = non_negative_or_zero(seed.mobilitymin);
  seed.mobilitymax = non_negative_or_zero(seed.mobilitymax);

  if (seed.id.empty()) {
    seed.id = make_default_target_id(seed.mzmin, seed.mzmax, seed.rtmin, seed.rtmax, seed.mobilitymin, seed.mobilitymax);
  }
}

std::vector<TargetSeed> build_target_seeds(const MS_TARGETS_INPUT& table,
                                           const std::vector<std::string>& analyses,
                                           const std::vector<std::string>& polarities,
                                           const std::vector<std::string>& ids,
                                           double ppm,
                                           double sec,
                                           double millisec) {
  std::vector<TargetSeed> seeds;
  seeds.reserve(table.size);
  for (std::size_t i = 0; i < table.size; ++i) {
    seeds.push_back(seed_from_table_row(table, i));
  }

  std::vector<TargetSeed> with_polarity;
  for (const auto& seed : seeds) {
    auto expanded = expand_seed_polarity(seed, polarities);
    with_polarity.insert(with_polarity.end(), expanded.begin(), expanded.end());
  }

  auto expanded = expand_seed_analyses(with_polarity, analyses);
  for (std::size_t i = 0; i < expanded.size(); ++i) {
    if (i < ids.size()) {
      const auto override_id = trim_copy(ids[i]);
      if (!override_id.empty()) {
        expanded[i].id = override_id;
      }
    }
    finalize_seed_ranges(expanded[i], ppm, sec, millisec);
  }
  return expanded;
}

}  // namespace

std::vector<std::string> sanitize_analyses(const std::vector<std::string>& analyses) {
  std::vector<std::string> out;
  out.reserve(analyses.size());
  for (const auto& analysis : analyses) {
    const auto trimmed = trim_copy(analysis);
    if (!trimmed.empty()) {
      out.push_back(trimmed);
    }
  }
  return out;
}

std::vector<MS_TARGETS> build_targets_by_analysis(const MS_TARGETS_REQUEST& request,
                                                  const std::vector<std::string>& analyses,
                                                  const std::vector<std::string>& polarities) {
  const auto selected_analyses = sanitize_analyses(analyses.empty() ? request.analyses : analyses);
  if (selected_analyses.empty()) {
    return {};
  }

  const MS_TARGETS_INPUT* selected = nullptr;
  if (!request.mz.empty()) {
    selected = &request.mz;
  } else if (!request.mass.empty()) {
    selected = &request.mass;
  } else if (!request.rt.empty()) {
    selected = &request.rt;
  } else if (!request.mobility.empty()) {
    selected = &request.mobility;
  }

  if (selected == nullptr) {
    return {};
  }

  const auto seeds = build_target_seeds(*selected,
                                        selected_analyses,
                                        polarities,
                                        request.id,
                                        request.ppm,
                                        request.sec,
                                        request.millisec);

  std::vector<MS_TARGETS> out;
  out.reserve(selected_analyses.size());
  for (const auto& analysis : selected_analyses) {
    std::vector<const TargetSeed*> matching;
    for (const auto& seed : seeds) {
      if (seed.analysis == analysis) {
        matching.push_back(&seed);
      }
    }
    MS_TARGETS targets;
    targets.resize_all(static_cast<int>(matching.size()));
    for (std::size_t i = 0; i < matching.size(); ++i) {
      const auto& seed = *matching[i];
      targets.index[i] = static_cast<int>(i);
      targets.id[i] = seed.id;
      targets.level[i] = 0;
      targets.polarity[i] = seed.polarity == "-1" ? -1 : 1;
      targets.precursor[i] = false;
      targets.mz[i] = static_cast<float>(seed.mz);
      targets.mzmin[i] = static_cast<float>(seed.mzmin);
      targets.mzmax[i] = static_cast<float>(seed.mzmax);
      targets.rt[i] = static_cast<float>(seed.rt);
      targets.rtmin[i] = static_cast<float>(seed.rtmin);
      targets.rtmax[i] = static_cast<float>(seed.rtmax);
      targets.mobility[i] = static_cast<float>(seed.mobility);
      targets.mobilitymin[i] = static_cast<float>(seed.mobilitymin);
      targets.mobilitymax[i] = static_cast<float>(seed.mobilitymax);
    }
    out.push_back(std::move(targets));
  }
  return out;
}

bool has_effective_targets(const MS_TARGETS& targets) {
  if (targets.id.empty()) {
    return false;
  }
  for (std::size_t i = 0; i < targets.id.size(); ++i) {
    if (!is_empty_target_row(targets, i)) {
      return true;
    }
  }
  return false;
}

MS_TARGETS subset_targets(const MS_TARGETS& targets,
                          const std::vector<int>& levels,
                          bool all_traces,
                          double isolation_window) {
  MS_TARGETS out;
  out.resize_all(static_cast<int>(targets.id.size()));
  for (std::size_t i = 0; i < targets.id.size(); ++i) {
    out.index[i] = static_cast<int>(i);
    out.id[i] = trim_copy(targets.id[i]);
    out.level[i] = levels.empty() ? 0 : levels.front();
    out.polarity[i] = targets.polarity[i];
    out.precursor[i] = !all_traces;
    out.mz[i] = targets.mz[i];
    out.mzmin[i] = targets.mzmin[i];
    out.mzmax[i] = targets.mzmax[i];
    out.rt[i] = targets.rt[i];
    out.rtmin[i] = targets.rtmin[i];
    out.rtmax[i] = targets.rtmax[i];
    out.mobility[i] = targets.mobility[i];
    out.mobilitymin[i] = targets.mobilitymin[i];
    out.mobilitymax[i] = targets.mobilitymax[i];
    if (!all_traces) {
      out.mzmin[i] -= static_cast<float>(isolation_window / 2.0);
      out.mzmax[i] += static_cast<float>(isolation_window / 2.0);
    }
  }
  return out;
}

}  // namespace ms
