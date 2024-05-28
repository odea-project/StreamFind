#include "StreamCraft_lib.hpp"
#include <iostream>
#include <set>

void sc::welcome() {
  std::cout << std::endl;
  std::cout << "  \033[1;34mStream\033[0m\033[1;31mCraft\033[0m" << std::endl;
  std::cout << std::endl;
};

sc::MS_ANALYSIS::MS_ANALYSIS(const std::string& file) {
  file_path = file;
  
  file_dir = file.substr(0, file.find_last_of("/\\") + 1);
  if (file_dir.back() == '/') file_dir.pop_back();

  file_name = file.substr(file.find_last_of("/\\") + 1);
  
  file_extension = file_name.substr(file_name.find_last_of(".") + 1);
  
  file_name = file_name.substr(0, file_name.find_last_of("."));

  if (std::find(possible_formats.begin(), possible_formats.end(), file_extension) == possible_formats.end()) {
    std::cerr << "Invalid file extension for MS_ANALYSIS!" << std::endl;
  }

  format_case = std::distance(possible_formats.begin(), std::find(possible_formats.begin(), possible_formats.end(), file_extension));

  switch (format_case) {

    case 0: {
      ms = std::make_unique<MS_FILE<MZML>>(file);
      break;
    }

    case 1: {
      ms = std::make_unique<MS_FILE<MZXML>>(file);
      break;
    }
    
    default:
      break;
  }
};

void sc::MS_ANALYSIS::print() {
  std::cout << "MS_ANALYSIS (" << possible_formats[format_case] << ")" << std::endl;
  std::cout << "  File:                      " << file_path << std::endl;
  std::cout << "  Format:                    " << get_format() << std::endl;
  std::cout << "  Type:                      " << get_type() << std::endl;
  std::cout << "  Number of spectra:         " << get_number_spectra() << std::endl;
  std::cout << "  Number of binnary arrays:  " << get_number_spectra_binary_arrays() << std::endl;
  std::cout << "  Number of chromatograms:   " << get_number_chromatograms() << std::endl;
  std::cout << std::endl;
};

std::vector<std::vector<std::vector<double>>> sc::MS_ANALYSIS::extract_spectra_targets_simple(const sc::MS_TARGETS& targets) {

  int number_targets = targets.index.size();

  std::vector<std::vector<std::vector<double>>> res(number_targets);

  const std::vector<double>& rts = get_spectra_rt();

  const std::vector<int>& levels = get_spectra_level();

  int number_rts = rts.size();

  std::set<int> idx;

  for (int i = 0; i < number_targets; i++) {
    for (int j = 0; j < number_rts; j++) {

      if (levels[j] == targets.level[i] || targets.level[i] == 0) {
        if ((rts[j] >= targets.rtmin[i] && rts[j] <= targets.rtmax[i]) || targets.rtmax[i] == 0) {
          idx.insert(j);
        }
      }
    }
  }

  std::vector<int> idx_vector(idx.begin(), idx.end());

  std::sort(idx_vector.begin(), idx_vector.end());

  const std::vector<std::vector<std::vector<double>>>& spectra = get_spectra(idx_vector);

  int number_spectra_targets = spectra.size();

  #pragma omp parallel for
  for (int i = 0; i < number_targets; i++) {

    res[i].resize(4);

    for (int j = 0; j < number_spectra_targets; j++) {

      const double& level = levels[idx_vector[j]];

      const double& rt = rts[idx_vector[j]];

      if (targets.rtmax[i] != 0) {

        if ((rt >= targets.rtmin[i] && rt <= targets.rtmax[i])) {

          const std::vector<double>& mzj = spectra[j][0];
          const std::vector<double>& intj = spectra[j][1];

          int number_points = mzj.size();

          for (int k = 0; k < number_points; k++) {

            if (mzj[k] >= targets.mzmin[i] && mzj[k] <= targets.mzmax[i]) {
              res[i][0].push_back(level);
              res[i][1].push_back(rt);
              res[i][2].push_back(mzj[k]);
              res[i][3].push_back(intj[k]);
            }
          }
        } 
      } else {
        const std::vector<double>& mzj = spectra[j][0];
        const std::vector<double>& intj = spectra[j][1];

        int number_points = mzj.size();

        for (int k = 0; k < number_points; k++) {

          if (mzj[k] >= targets.mzmin[i] && mzj[k] <= targets.mzmax[i]) {
            res[i][0].push_back(level);
            res[i][1].push_back(rt);
            res[i][2].push_back(mzj[k]);
            res[i][3].push_back(intj[k]);
          }
        }
      }
    }
  }

  return res;
};

std::vector<std::vector<std::vector<double>>> sc::MS_ANALYSIS::extract_spectra_targets_with_drift(const sc::MS_TARGETS& targets) {

  int number_targets = targets.index.size();

  std::vector<std::vector<std::vector<double>>> res(number_targets);

  const std::vector<double>& drifts = get_spectra_drift();

  const std::vector<double>& rts = get_spectra_rt();

  const std::vector<int>& levels = get_spectra_level();

  for (int i = 0; i < number_targets; i++) {
    if (drifts.size() == 0 && targets.driftmax[i] != 0) {
      std::cerr << "Drift values not present in file!" << std::endl;
      return res;
    }
  }
      
  int number_rts = rts.size();

  std::set<int> idx;

  for (int i = 0; i < number_targets; i++) {
    for (int j = 0; j < number_rts; j++) {

      if (levels[j] == targets.level[i] || targets.level[i] == 0) {
        if ((rts[j] >= targets.rtmin[i] && rts[j] <= targets.rtmax[i]) || targets.rtmax[i] == 0) {
          if (drifts.size() == 0 || targets.driftmax[i] == 0) {
            idx.insert(j);
          } else {
            if ((drifts[j] >= targets.driftmin[i] && drifts[j] <= targets.driftmax[i]) || targets.driftmax[i] == 0) {
              idx.insert(j);
            }
          }
        }
      }
    }
  }

  std::vector<int> idx_vector(idx.begin(), idx.end());

  std::sort(idx_vector.begin(), idx_vector.end());

  const std::vector<std::vector<std::vector<double>>>& spectra = get_spectra(idx_vector);

  int number_spectra_targets = spectra.size();

  #pragma omp parallel for
  for (int i = 0; i < number_targets; i++) {

    res[i].resize(5);

    for (int j = 0; j < number_spectra_targets; j++) {

      const double& level = levels[idx_vector[j]];

      const double& rt = rts[idx_vector[j]];

      const double& drift = drifts[idx_vector[j]];

      if (targets.rtmax[i] != 0 && targets.driftmax[i] != 0) {

        if ((rt >= targets.rtmin[i] && rt <= targets.rtmax[i])) {

          if ((drift >= targets.driftmin[i] && drift <= targets.driftmax[i])) {

            const std::vector<double>& mzj = spectra[j][0];
            const std::vector<double>& intj = spectra[j][1];

            int number_points = mzj.size();

            for (int k = 0; k < number_points; k++) {

              if (mzj[k] >= targets.mzmin[i] && mzj[k] <= targets.mzmax[i]) {
                res[i][0].push_back(level);
                res[i][1].push_back(rt);
                res[i][2].push_back(drift);
                res[i][3].push_back(mzj[k]);
                res[i][4].push_back(intj[k]);
              }
            }
          }
        }

      } else if (targets.driftmax[i] != 0) {

        if ((drift >= targets.driftmin[i] && drift <= targets.driftmax[i])) {

          const std::vector<double>& mzj = spectra[j][0];
          const std::vector<double>& intj = spectra[j][1];

          int number_points = mzj.size();

          for (int k = 0; k < number_points; k++) {

            if (mzj[k] >= targets.mzmin[i] && mzj[k] <= targets.mzmax[i]) {
              res[i][0].push_back(level);
              res[i][1].push_back(rt);
              res[i][2].push_back(drift);
              res[i][3].push_back(mzj[k]);
              res[i][4].push_back(intj[k]);
            }
          }
        }
              

      } else {
        const std::vector<double>& mzj = spectra[j][0];
        const std::vector<double>& intj = spectra[j][1];

        int number_points = mzj.size();

        for (int k = 0; k < number_points; k++) {

          if (mzj[k] >= targets.mzmin[i] && mzj[k] <= targets.mzmax[i]) {
            res[i][0].push_back(level);
            res[i][1].push_back(rt);
            res[i][2].push_back(drift);
            res[i][3].push_back(mzj[k]);
            res[i][4].push_back(intj[k]);
          }
        }
      }
    }
  }

  return res;
};

std::vector<std::vector<std::vector<double>>> sc::MS_ANALYSIS::get_spectra_targets(const sc::MS_TARGETS& targets) {

  const int number_spectra = get_number_spectra();

  const int number_targets = targets.index.size();

  std::vector<std::vector<std::vector<double>>> res;

  if (number_targets == 0) {
    std::cerr << "No targets defined!" << std::endl;
    return res;
  }

  if (number_spectra == 0) {
    std::cerr << "No spectra found in file!" << std::endl;
    return res;
  }

  const int number_spectra_binary_arrays = get_number_spectra_binary_arrays();

  if (number_spectra_binary_arrays == 0) {
    std::cerr << "No binary arrays found in file!" << std::endl;
    return res;
  }

  







  bool with_drift = false;

  for (int i = 0; i < number_targets; i++) if (targets.driftmax[i] != 0) with_drift = true;

  if (with_drift) {
    return extract_spectra_targets_with_drift(targets);

  } else {
    return extract_spectra_targets_simple(targets);
  }
};

std::vector<std::vector<std::vector<double>>> sc::MS_ANALYSIS::get_spectra_dda_targets(const sc::MS_TARGETS& targets) {

  const int number_spectra = get_number_spectra();

  int number_targets = targets.index.size();

  std::vector<std::vector<std::vector<double>>> res(number_targets);

  if (number_targets == 0) {
    std::cerr << "No targets defined!" << std::endl;
    return res;
  }

  if (number_spectra == 0) {
    std::cerr << "No spectra found in file!" << std::endl;
    return res;
  }

  const int number_spectra_binary_arrays = get_number_spectra_binary_arrays();

  if (number_spectra_binary_arrays == 0) {
    std::cerr << "No binary arrays found in file!" << std::endl;
    return res;
  }

  const std::vector<double>& rts = get_spectra_rt();

  const std::vector<int>& levels = get_spectra_level();

  const std::vector<double>& pre_mzs = get_spectra_precursor_mz();

  const std::vector<double>& ces = get_spectra_collision_energy();

  if (std::all_of(pre_mzs.begin(), pre_mzs.end(), [](double i) { return i == 0; })) {
    std::cerr << "No precursor m/z values found in file!" << std::endl;
    return res;
  }

  int number_rts = rts.size();

  std::set<int> idx;

  for (int i = 0; i < number_targets; i++) {
    
    for (int j = 0; j < number_rts; j++) {

      if (levels[j] == (targets.level[i])) {
        
        if ((rts[j] >= targets.rtmin[i] && rts[j] <= targets.rtmax[i]) || targets.rtmax[i] == 0) {  
          
          if ((pre_mzs[j] >= targets.mzmin[i] && pre_mzs[j] <= targets.mzmax[i])) {
            idx.insert(j);
          }
        }
      }
    }
  }

  std::vector<int> idx_vector(idx.begin(), idx.end());

  std::sort(idx_vector.begin(), idx_vector.end());

  const std::vector<std::vector<std::vector<double>>>& spectra = get_spectra(idx_vector);

  int number_spectra_targets = spectra.size();

  #pragma omp parallel for
  for (int i = 0; i < number_targets; i++) {

    res[i].resize(6);

    for (int j = 0; j < number_spectra_targets; j++) {

      const double& level = levels[idx_vector[j]];

      const double& rt = rts[idx_vector[j]];

      const double& pre_mz = pre_mzs[idx_vector[j]];

      const double& ce = ces[idx_vector[j]];

      if (targets.rtmax[i] != 0) {

        if ((rt >= targets.rtmin[i] && rt <= targets.rtmax[i])) {

          if ((pre_mz >= targets.mzmin[i] && pre_mz <= targets.mzmax[i])) {

            const std::vector<double>& mzj = spectra[j][0];
            const std::vector<double>& intj = spectra[j][1];

            int number_points = mzj.size();

            for (int k = 0; k < number_points; k++) {
              res[i][0].push_back(level);
              res[i][1].push_back(rt);
              res[i][2].push_back(pre_mz);
              res[i][3].push_back(ce);
              res[i][4].push_back(mzj[k]);
              res[i][5].push_back(intj[k]);
            }
          }
        }

      } else {

        if ((pre_mz >= targets.mzmin[i] && pre_mz <= targets.mzmax[i])) {

          const std::vector<double>& mzj = spectra[j][0];
          const std::vector<double>& intj = spectra[j][1];

          int number_points = mzj.size();

          for (int k = 0; k < number_points; k++) {
            res[i][0].push_back(level);
            res[i][1].push_back(rt);
            res[i][2].push_back(pre_mz);
            res[i][3].push_back(ce);
            res[i][4].push_back(mzj[k]);
            res[i][5].push_back(intj[k]);
          }
        }
      }
    }
  }

  return res;
};
