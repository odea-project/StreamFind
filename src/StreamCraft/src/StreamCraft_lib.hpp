#ifndef STREAMCRAFT_LIB_HPP
#define STREAMCRAFT_LIB_HPP

#include <vector>
#include <string>

#define PUGIXML_PATH "../../pugixml-1.14/src/pugixml.hpp"

#include "StreamCraft_utils.hpp"
#include "StreamCraft_mzml.hpp"
#include "StreamCraft_mzxml.hpp"

namespace sc {

  void welcome();

  class VirtualMassSpecFile {
    public:
      virtual ~VirtualMassSpecFile() = default;
      virtual int get_number_spectra() = 0;
      virtual int get_number_chromatograms() = 0;
      virtual int get_number_spectra_binary_arrays() = 0;
      virtual MS_SPECTRA_HEADERS get_first_spectra_headers() = 0;
      virtual MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {}) = 0;
      virtual MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) = 0;
      virtual std::vector<std::vector<std::vector<double>>> get_spectra(std::vector<int> indices = {}) = 0;
      virtual std::vector<std::vector<std::vector<double>>> get_chromatograms(std::vector<int> indices = {}) = 0;
      virtual std::vector<int> get_spectra_index(std::vector<int> indices = {}) = 0;
      virtual std::vector<int> get_spectra_scan_number(std::vector<int> indices = {}) = 0;
      virtual std::vector<int> get_spectra_array_length(std::vector<int> indices = {}) = 0;
      virtual std::vector<int> get_spectra_level(std::vector<int> indices = {}) = 0;
      virtual std::vector<std::string> get_spectra_mode(std::vector<int> indices = {}) = 0;
      virtual std::vector<std::string> get_spectra_polarity(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_lowmz(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_highmz(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_bpmz(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_bpint(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_tic(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_rt(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_drift(std::vector<int> indices = {}) = 0;
      virtual std::vector<int> get_spectra_precursor_scan(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_precursor_mz(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_precursor_window_mz(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_precursor_window_mzlow(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_precursor_window_mzhigh(std::vector<int> indices = {}) = 0;
      virtual std::vector<double> get_spectra_collision_energy(std::vector<int> indices = {}) = 0;
      virtual std::vector<std::vector<std::string>> get_software() = 0;
      virtual std::vector<std::vector<std::string>> get_hardware() = 0;
  };

  template <typename T>
  class MassSpecFile  : public VirtualMassSpecFile {
    public:
      MassSpecFile(const std::string& file) : ms(file) {}
      int get_number_spectra() override { return ms.number_spectra; }
      int get_number_chromatograms() override { return ms.number_chromatograms; }
      int get_number_spectra_binary_arrays() { return ms.number_spectra_binary_arrays; }
      MS_SPECTRA_HEADERS get_first_spectra_headers() override { return ms.first_spectra_headers; }
      MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {}) override { return ms.get_spectra_headers(indices); }
      MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) override { return ms.get_chromatograms_headers(indices); }
      std::vector<std::vector<std::vector<double>>> get_spectra(std::vector<int> indices = {}) override { return ms.get_spectra(indices); }
      std::vector<std::vector<std::vector<double>>> get_chromatograms(std::vector<int> indices = {}) override { return ms.get_chromatograms(indices); }
      std::vector<int> get_spectra_index(std::vector<int> indices = {}) { return ms.get_spectra_index(indices); }
      std::vector<int> get_spectra_scan_number(std::vector<int> indices = {}) { return ms.get_spectra_scan_number(indices); }
      std::vector<int> get_spectra_array_length(std::vector<int> indices = {}) { return ms.get_spectra_array_length(indices); }
      std::vector<int> get_spectra_level(std::vector<int> indices = {}) { return ms.get_spectra_level(indices); }
      std::vector<std::string> get_spectra_mode(std::vector<int> indices = {}) { return ms.get_spectra_mode(indices); }
      std::vector<std::string> get_spectra_polarity(std::vector<int> indices = {}) { return ms.get_spectra_polarity(indices); }
      std::vector<double> get_spectra_lowmz(std::vector<int> indices = {}) { return ms.get_spectra_lowmz(indices); }
      std::vector<double> get_spectra_highmz(std::vector<int> indices = {}) { return ms.get_spectra_highmz(indices); }
      std::vector<double> get_spectra_bpmz(std::vector<int> indices = {}) { return ms.get_spectra_bpmz(indices); }
      std::vector<double> get_spectra_bpint(std::vector<int> indices = {}) { return ms.get_spectra_bpint(indices); }
      std::vector<double> get_spectra_tic(std::vector<int> indices = {}) { return ms.get_spectra_tic(indices); }
      std::vector<double> get_spectra_rt(std::vector<int> indices = {}) { return ms.get_spectra_rt(indices); }
      std::vector<double> get_spectra_drift(std::vector<int> indices = {}) { return ms.get_spectra_drift(indices); }
      std::vector<int> get_spectra_precursor_scan(std::vector<int> indices = {}) { return ms.get_spectra_precursor_scan(indices); }
      std::vector<double> get_spectra_precursor_mz(std::vector<int> indices = {}) { return ms.get_spectra_precursor_mz(indices); }
      std::vector<double> get_spectra_precursor_window_mz(std::vector<int> indices = {}) { return ms.get_spectra_precursor_window_mz(indices); }
      std::vector<double> get_spectra_precursor_window_mzlow(std::vector<int> indices = {}) { return ms.get_spectra_precursor_window_mzlow(indices); }
      std::vector<double> get_spectra_precursor_window_mzhigh(std::vector<int> indices = {}) { return ms.get_spectra_precursor_window_mzhigh(indices); }
      std::vector<double> get_spectra_collision_energy(std::vector<int> indices = {}) { return ms.get_spectra_collision_energy(indices); }
      std::vector<std::vector<std::string>> get_software() { return ms.get_software(); }
      std::vector<std::vector<std::string>> get_hardware() { return ms.get_hardware(); }
      T& open() { return ms; };

    private:
      T ms;
  };

  class MassSpecAnalysis {

    private:
    
      std::vector<std::string> possible_formats = { "mzML", "mzXML", "animl" };

      std::vector<std::vector<std::vector<double>>> extract_spectra_targets_simple(const MS_TARGETS& targets);

      std::vector<std::vector<std::vector<double>>> extract_spectra_targets_with_drift(const MS_TARGETS& targets);

    public:

      std::string file_path;

      std::string file_dir;

      std::string file_name;

      std::string file_extension;

      int format_case;

      std::unique_ptr<VirtualMassSpecFile> ms;

      int number_spectra;

      int number_chromatograms;

      int number_spectra_binary_arrays;

      MS_SPECTRA_HEADERS first_spectra_headers;

      MassSpecAnalysis(const std::string& file);

      MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {}) { return ms->get_spectra_headers(indices); }
      MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) { return ms->get_chromatograms_headers(indices); }
      std::vector<std::vector<std::vector<double>>> get_spectra(std::vector<int> indices = {}) { return ms->get_spectra(indices); }
      std::vector<std::vector<std::vector<double>>> get_chromatograms(std::vector<int> indices = {}) { return ms->get_chromatograms(indices); }
      std::vector<int> get_spectra_index(std::vector<int> indices = {}) { return ms->get_spectra_index(indices); }
      std::vector<int> get_spectra_scan_number(std::vector<int> indices = {}) { return ms->get_spectra_scan_number(indices); }
      std::vector<int> get_spectra_array_length(std::vector<int> indices = {}) { return ms->get_spectra_array_length(indices); }
      std::vector<int> get_spectra_level(std::vector<int> indices = {}) { return ms->get_spectra_level(indices); }
      std::vector<std::string> get_spectra_mode(std::vector<int> indices = {}) { return ms->get_spectra_mode(indices); }
      std::vector<std::string> get_spectra_polarity(std::vector<int> indices = {}) { return ms->get_spectra_polarity(indices); }
      std::vector<double> get_spectra_lowmz(std::vector<int> indices = {}) { return ms->get_spectra_lowmz(indices); }
      std::vector<double> get_spectra_highmz(std::vector<int> indices = {}) { return ms->get_spectra_highmz(indices); }
      std::vector<double> get_spectra_bpmz(std::vector<int> indices = {}) { return ms->get_spectra_bpmz(indices); }
      std::vector<double> get_spectra_bpint(std::vector<int> indices = {}) { return ms->get_spectra_bpint(indices); }
      std::vector<double> get_spectra_tic(std::vector<int> indices = {}) { return ms->get_spectra_tic(indices); }
      std::vector<double> get_spectra_rt(std::vector<int> indices = {}) { return ms->get_spectra_rt(indices); }
      std::vector<double> get_spectra_drift(std::vector<int> indices = {}) { return ms->get_spectra_drift(indices); }
      std::vector<int> get_spectra_precursor_scan(std::vector<int> indices = {}) { return ms->get_spectra_precursor_scan(indices); }
      std::vector<double> get_spectra_precursor_mz(std::vector<int> indices = {}) { return ms->get_spectra_precursor_mz(indices); }
      std::vector<double> get_spectra_precursor_window_mz(std::vector<int> indices = {}) { return ms->get_spectra_precursor_window_mz(indices); }
      std::vector<double> get_spectra_precursor_window_mzlow(std::vector<int> indices = {}) { return ms->get_spectra_precursor_window_mzlow(indices); }
      std::vector<double> get_spectra_precursor_window_mzhigh(std::vector<int> indices = {}) { return ms->get_spectra_precursor_window_mzhigh(indices); }
      std::vector<double> get_spectra_collision_energy(std::vector<int> indices = {}) { return ms->get_spectra_collision_energy(indices); }
      std::vector<std::vector<std::string>> get_software() { return ms->get_software(); }
      std::vector<std::vector<std::string>> get_hardware() { return ms->get_hardware(); }
      std::vector<std::vector<std::vector<double>>> get_spectra_targets(const MS_TARGETS& targets);
      std::vector<std::vector<std::vector<double>>> get_spectra_dda_targets(const MS_TARGETS& targets);
      void print();
  };

}; // namespace sc

#endif // SCLIB_HPP

#if defined(STREAMCRAFT_HEADER_ONLY) && !defined(STREAMCRAFT_LIB_SOURCE)
#	define STREAMCRAFT_LIB_SOURCE "StreamCraft_lib.cpp"
#	include STREAMCRAFT_LIB_SOURCE
#endif
