#ifndef MASS_SPEC_READER_H
#define MASS_SPEC_READER_H

#include "targets.h"

#include <algorithm>
#include <cmath>
#include <memory>
#include <numeric>
#include <stdexcept>
#include <string>
#include <vector>

namespace mass_spec {

struct MS_SPECTRUM {
  int index;
  int scan;
  int array_length;
  int level;
  int mode;
  int polarity;
  float lowmz;
  float highmz;
  float bpmz;
  float bpint;
  float tic;
  int configuration;
  float rt;
  float mobility;
  float window_mz;
  float window_mzlow;
  float window_mzhigh;
  float precursor_mz;
  float precursor_intensity;
  int precursor_charge;
  float activation_ce;
  int binary_arrays_count;
  std::vector<std::string> binary_names;
  std::vector<std::vector<float>> binary_data;
};

struct MS_SPECTRA_HEADERS {
  std::vector<int> index;
  std::vector<int> scan;
  std::vector<int> array_length;
  std::vector<int> level;
  std::vector<int> mode;
  std::vector<int> polarity;
  std::vector<float> lowmz;
  std::vector<float> highmz;
  std::vector<float> bpmz;
  std::vector<float> bpint;
  std::vector<float> tic;
  std::vector<int> configuration;
  std::vector<float> rt;
  std::vector<float> mobility;
  std::vector<float> window_mz;
  std::vector<float> window_mzlow;
  std::vector<float> window_mzhigh;
  std::vector<float> precursor_mz;
  std::vector<float> precursor_intensity;
  std::vector<int> precursor_charge;
  std::vector<float> activation_ce;

  void resize_all(int n) {
    index.resize(n);
    scan.resize(n);
    array_length.resize(n);
    level.resize(n);
    mode.resize(n);
    polarity.resize(n);
    lowmz.resize(n);
    highmz.resize(n);
    bpmz.resize(n);
    bpint.resize(n);
    tic.resize(n);
    configuration.resize(n);
    rt.resize(n);
    mobility.resize(n);
    window_mz.resize(n);
    window_mzlow.resize(n);
    window_mzhigh.resize(n);
    precursor_mz.resize(n);
    precursor_intensity.resize(n);
    precursor_charge.resize(n);
    activation_ce.resize(n);
  }

  size_t size() const { return index.size(); }
};

struct MS_SUMMARY {
  std::string file_name;
  std::string file_path;
  std::string file_dir;
  std::string file_extension;
  int number_spectra;
  int number_chromatograms;
  int number_spectra_binary_arrays;
  std::string format;
  std::string time_stamp;
  std::vector<int> polarity;
  std::vector<int> mode;
  std::vector<int> level;
  std::vector<int> configuration;
  std::string type;
  float min_mz;
  float max_mz;
  float start_rt;
  float end_rt;
  bool has_ion_mobility;
};

struct MS_CHROMATOGRAMS_HEADERS {
  std::vector<int> index;
  std::vector<std::string> id;
  std::vector<int> array_length;
  std::vector<int> polarity;
  std::vector<float> precursor_mz;
  std::vector<float> activation_ce;
  std::vector<float> product_mz;

  void resize_all(int n) {
    index.resize(n);
    id.resize(n);
    array_length.resize(n);
    polarity.resize(n);
    precursor_mz.resize(n);
    activation_ce.resize(n);
    product_mz.resize(n);
  }

  size_t size() const { return index.size(); }
};

class MS_READER {
 public:
  explicit MS_READER(const std::string& file) : file_(file) {}
  virtual ~MS_READER() = default;

  virtual int get_number_spectra() = 0;
  virtual int get_number_chromatograms() = 0;
  virtual int get_number_spectra_binary_arrays() = 0;
  virtual std::string get_format() = 0;
  virtual std::string get_type() = 0;
  virtual std::string get_time_stamp() = 0;
  virtual std::vector<int> get_polarity() = 0;
  virtual std::vector<int> get_mode() = 0;
  virtual std::vector<int> get_level() = 0;
  virtual std::vector<int> get_configuration() = 0;
  virtual float get_min_mz() = 0;
  virtual float get_max_mz() = 0;
  virtual float get_start_rt() = 0;
  virtual float get_end_rt() = 0;
  virtual bool has_ion_mobility() = 0;
  virtual MS_SUMMARY get_summary() = 0;
  virtual std::vector<int> get_spectra_index(std::vector<int> indices = {}) = 0;
  virtual std::vector<int> get_spectra_scan_number(std::vector<int> indices = {}) = 0;
  virtual std::vector<int> get_spectra_array_length(std::vector<int> indices = {}) = 0;
  virtual std::vector<int> get_spectra_level(std::vector<int> indices = {}) = 0;
  virtual std::vector<int> get_spectra_configuration(std::vector<int> indices = {}) = 0;
  virtual std::vector<int> get_spectra_mode(std::vector<int> indices = {}) = 0;
  virtual std::vector<int> get_spectra_polarity(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_lowmz(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_highmz(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_bpmz(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_bpint(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_tic(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_rt(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_mobility(std::vector<int> indices = {}) = 0;
  virtual std::vector<int> get_spectra_precursor_scan(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_precursor_mz(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_precursor_window_mz(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_precursor_window_mzlow(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_precursor_window_mzhigh(std::vector<int> indices = {}) = 0;
  virtual std::vector<float> get_spectra_collision_energy(std::vector<int> indices = {}) = 0;
  virtual MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {}) = 0;
  virtual MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) = 0;
  virtual std::vector<std::vector<std::vector<float>>> get_spectra(std::vector<int> indices = {}) = 0;
  virtual std::vector<std::vector<std::vector<float>>> get_chromatograms(std::vector<int> indices = {}) = 0;
  virtual std::vector<std::vector<std::string>> get_software() = 0;
  virtual std::vector<std::vector<std::string>> get_hardware() = 0;
  virtual MS_SPECTRUM get_spectrum(const int& idx) = 0;

  const std::string& file() const { return file_; }

 protected:
  std::string file_;
};

class MS_FILE {
 public:
  explicit MS_FILE(const std::string& file);

  int get_number_spectra() { return ms->get_number_spectra(); }
  int get_number_chromatograms() { return ms->get_number_chromatograms(); }
  int get_number_spectra_binary_arrays() { return ms->get_number_spectra_binary_arrays(); }
  std::string get_format() { return ms->get_format(); }
  std::string get_type() { return ms->get_type(); }
  std::string get_time_stamp() { return ms->get_time_stamp(); }
  std::vector<int> get_polarity() { return ms->get_polarity(); }
  std::vector<int> get_mode() { return ms->get_mode(); }
  std::vector<int> get_level() { return ms->get_level(); }
  std::vector<int> get_configuration() { return ms->get_configuration(); }
  float get_min_mz() { return ms->get_min_mz(); }
  float get_max_mz() { return ms->get_max_mz(); }
  float get_start_rt() { return ms->get_start_rt(); }
  float get_end_rt() { return ms->get_end_rt(); }
  bool has_ion_mobility() { return ms->has_ion_mobility(); }
  MS_SUMMARY get_summary() { return ms->get_summary(); }
  std::vector<int> get_spectra_index(std::vector<int> indices = {}) { return ms->get_spectra_index(indices); }
  std::vector<int> get_spectra_scan_number(std::vector<int> indices = {}) { return ms->get_spectra_scan_number(indices); }
  std::vector<int> get_spectra_array_length(std::vector<int> indices = {}) { return ms->get_spectra_array_length(indices); }
  std::vector<int> get_spectra_level(std::vector<int> indices = {}) { return ms->get_spectra_level(indices); }
  std::vector<int> get_spectra_mode(std::vector<int> indices = {}) { return ms->get_spectra_mode(indices); }
  std::vector<int> get_spectra_polarity(std::vector<int> indices = {}) { return ms->get_spectra_polarity(indices); }
  std::vector<float> get_spectra_lowmz(std::vector<int> indices = {}) { return ms->get_spectra_lowmz(indices); }
  std::vector<float> get_spectra_highmz(std::vector<int> indices = {}) { return ms->get_spectra_highmz(indices); }
  std::vector<float> get_spectra_bpmz(std::vector<int> indices = {}) { return ms->get_spectra_bpmz(indices); }
  std::vector<float> get_spectra_bpint(std::vector<int> indices = {}) { return ms->get_spectra_bpint(indices); }
  std::vector<float> get_spectra_tic(std::vector<int> indices = {}) { return ms->get_spectra_tic(indices); }
  std::vector<float> get_spectra_rt(std::vector<int> indices = {}) { return ms->get_spectra_rt(indices); }
  std::vector<float> get_spectra_mobility(std::vector<int> indices = {}) { return ms->get_spectra_mobility(indices); }
  std::vector<int> get_spectra_precursor_scan(std::vector<int> indices = {}) { return ms->get_spectra_precursor_scan(indices); }
  std::vector<float> get_spectra_precursor_mz(std::vector<int> indices = {}) { return ms->get_spectra_precursor_mz(indices); }
  std::vector<float> get_spectra_precursor_window_mz(std::vector<int> indices = {}) { return ms->get_spectra_precursor_window_mz(indices); }
  std::vector<float> get_spectra_precursor_window_mzlow(std::vector<int> indices = {}) { return ms->get_spectra_precursor_window_mzlow(indices); }
  std::vector<float> get_spectra_precursor_window_mzhigh(std::vector<int> indices = {}) { return ms->get_spectra_precursor_window_mzhigh(indices); }
  std::vector<float> get_spectra_collision_energy(std::vector<int> indices = {}) { return ms->get_spectra_collision_energy(indices); }
  MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {}) { return ms->get_spectra_headers(indices); }
  MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) { return ms->get_chromatograms_headers(indices); }
  std::vector<std::vector<std::vector<float>>> get_spectra(std::vector<int> indices = {}) { return ms->get_spectra(indices); }
  std::vector<std::vector<std::vector<float>>> get_chromatograms(std::vector<int> indices = {}) { return ms->get_chromatograms(indices); }
  std::vector<std::vector<std::string>> get_software() { return ms->get_software(); }
  std::vector<std::vector<std::string>> get_hardware() { return ms->get_hardware(); }
  MS_SPECTRUM get_spectrum(const int& index) { return ms->get_spectrum(index); }
  MS_TARGETS_SPECTRA get_spectra_targets(const MS_TARGETS& targets, const MS_SPECTRA_HEADERS& hd, const float& minIntLv1, const float& minIntLv2);

  std::unique_ptr<MS_READER> ms;
  std::string file_path;
  std::string file_dir;
  std::string file_name;
  std::string file_extension;
  std::string format;
  int format_case = -1;

 private:
  const std::vector<std::string> possible_formats = {"mzML", "mzXML"};
};

std::string detect_format(const std::string& file_path);
std::unique_ptr<MS_READER> create_reader(const std::string& file_path);

} // namespace ms

#endif // MASS_SPEC_READER_H
