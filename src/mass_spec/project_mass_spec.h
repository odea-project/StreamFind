#pragma once

#include "reader.h"
#include "targets.h"
#include "../project/project.h"

namespace mass_spec {

struct MS_ANALYSIS_ROW : public project::ROW {
  std::string analysis;
  std::string replicate;
  std::string blank;
  std::string file_name;
  std::string file_path;
  std::string file_dir;
  std::string file_extension;
  std::string format;
  std::string type;
  std::string time_stamp;
  int number_spectra = 0;
  int number_chromatograms = 0;
  int number_spectra_binary_arrays = 0;
  double min_mz = 0.0;
  double max_mz = 0.0;
  double start_rt = 0.0;
  double end_rt = 0.0;
  bool has_ion_mobility = false;
  double concentration = 0.0;
};

struct MS_SPECTRA_HEADER_ROW : public project::ROW {
  std::string analysis;
  int index = 0;
  int scan = 0;
  int array_length = 0;
  int level = 0;
  int mode = 0;
  int polarity = 0;
  int configuration = 0;
  double lowmz = 0.0;
  double highmz = 0.0;
  double bpmz = 0.0;
  double bpint = 0.0;
  double tic = 0.0;
  double rt = 0.0;
  double mobility = 0.0;
  double window_mz = 0.0;
  double window_mzlow = 0.0;
  double window_mzhigh = 0.0;
  double precursor_mz = 0.0;
  double precursor_intensity = 0.0;
  int precursor_charge = 0;
  double activation_ce = 0.0;
};

struct MS_CHROMATOGRAM_HEADER_ROW : public project::ROW {
  std::string analysis;
  int index = 0;
  std::string id;
  int array_length = 0;
  int polarity = 0;
  double precursor_mz = 0.0;
  double activation_ce = 0.0;
  double product_mz = 0.0;
};

struct MS_SPECTRA_TIC_ROW {
  std::string analysis;
  std::string replicate;
  int polarity = 0;
  int level = 0;
  double rt = 0.0;
  double tic = 0.0;
  double bpmz = 0.0;
  double bpint = 0.0;
};

struct MS_RAW_SPECTRUM_ROW {
  std::string analysis;
  std::string replicate;
  std::string id;
  int polarity = 0;
  int level = 0;
  double pre_mz = 0.0;
  double pre_mzlow = 0.0;
  double pre_mzhigh = 0.0;
  double pre_ce = 0.0;
  double rt = 0.0;
  double mobility = 0.0;
  double mz = 0.0;
  double intensity = 0.0;
};

class PROJECT_MASS_SPEC {
 public:
  explicit PROJECT_MASS_SPEC(std::shared_ptr<project::CONTEXT> ctx);

  static void create_schema(const std::shared_ptr<project::CONTEXT>& ctx);
  static void validate_schema(const std::shared_ptr<project::CONTEXT>& ctx);

  void import_files(const std::vector<std::string>& file_paths,
                    const std::vector<std::string>& analyses = {},
                    const std::vector<std::string>& replicates = {},
                    const std::vector<std::string>& blanks = {});
  void remove_analysis(const std::string& analysis);

  std::vector<MS_ANALYSIS_ROW> get_analyses() const;
  std::vector<std::string> get_analysis_names() const;
  std::vector<std::string> get_replicate_names() const;
  std::vector<std::string> get_blank_names() const;
  std::vector<double> get_concentrations() const;
  void set_replicate_names(const std::vector<std::string>& values);
  void set_blank_names(const std::vector<std::string>& values);
  void set_concentrations(const std::vector<double>& values);
  std::vector<MS_SPECTRA_HEADER_ROW> get_spectra_headers(const std::vector<std::string>& analyses = {}) const;
  std::vector<MS_CHROMATOGRAM_HEADER_ROW> get_chromatograms_headers(const std::vector<std::string>& analyses = {}) const;
  std::vector<MS_SPECTRA_TIC_ROW> get_spectra_tic(const std::vector<std::string>& analyses = {},
                                                  const std::vector<int>& levels = {},
                                                  double rt_min = 0.0,
                                                  double rt_max = 0.0) const;
  std::vector<MS_RAW_SPECTRUM_ROW> get_raw_spectra(const MS_TARGETS_REQUEST& request) const;
  std::vector<std::vector<std::vector<float>>> get_chromatograms_data(const std::string& analysis,
                                                                      const std::vector<int>& indices) const;

 private:
  std::shared_ptr<project::CONTEXT> ctx_;

  void import_file_with_connection(duckdb_connection con,
                                   const std::string& file_path,
                                   const std::string& analysis,
                                   const std::string& replicate,
                                   const std::string& blank);

  static constexpr const char* analyses_table_name() { return "MS_ANALYSES"; }
  static constexpr const char* spectra_headers_table_name() { return "MS_SPECTRA_HEADERS"; }
  static constexpr const char* chromatograms_headers_table_name() { return "MS_CHROMATOGRAMS_HEADERS"; }
};

}  // namespace mass_spec
