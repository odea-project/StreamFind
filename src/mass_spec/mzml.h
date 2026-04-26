#ifndef MASS_SPEC_MZML_H
#define MASS_SPEC_MZML_H

#include "reader.h"
#include <memory>

namespace ms {
namespace mzml {

struct Impl;

class Reader : public MS_READER {
 public:
  explicit Reader(const std::string& file);
  ~Reader() override;

  int get_number_spectra() override;
  int get_number_chromatograms() override;
  int get_number_spectra_binary_arrays() override;
  std::string get_format() override;
  std::string get_type() override;
  std::string get_time_stamp() override;
  std::vector<int> get_polarity() override;
  std::vector<int> get_mode() override;
  std::vector<int> get_level() override;
  std::vector<int> get_configuration() override;
  float get_min_mz() override;
  float get_max_mz() override;
  float get_start_rt() override;
  float get_end_rt() override;
  bool has_ion_mobility() override;
  MS_SUMMARY get_summary() override;
  std::vector<int> get_spectra_index(std::vector<int> indices = {}) override;
  std::vector<int> get_spectra_scan_number(std::vector<int> indices = {}) override;
  std::vector<int> get_spectra_array_length(std::vector<int> indices = {}) override;
  std::vector<int> get_spectra_level(std::vector<int> indices = {}) override;
  std::vector<int> get_spectra_configuration(std::vector<int> indices = {}) override;
  std::vector<int> get_spectra_mode(std::vector<int> indices = {}) override;
  std::vector<int> get_spectra_polarity(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_lowmz(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_highmz(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_bpmz(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_bpint(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_tic(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_rt(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_mobility(std::vector<int> indices = {}) override;
  std::vector<int> get_spectra_precursor_scan(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_precursor_mz(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_precursor_window_mz(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_precursor_window_mzlow(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_precursor_window_mzhigh(std::vector<int> indices = {}) override;
  std::vector<float> get_spectra_collision_energy(std::vector<int> indices = {}) override;
  MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {}) override;
  MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) override;
  std::vector<std::vector<std::vector<float>>> get_spectra(std::vector<int> indices = {}) override;
  std::vector<std::vector<std::vector<float>>> get_chromatograms(std::vector<int> indices = {}) override;
  std::vector<std::vector<std::string>> get_software() override;
  std::vector<std::vector<std::string>> get_hardware() override;
  MS_SPECTRUM get_spectrum(const int& idx) override;

 private:
  std::unique_ptr<Impl> pimpl;
};

} // namespace mzml
} // namespace ms

#endif // MASS_SPEC_MZML_H
