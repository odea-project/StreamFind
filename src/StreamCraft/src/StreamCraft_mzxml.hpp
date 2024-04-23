#ifndef STREAMCRAFT_MZXML_HPP
#define STREAMCRAFT_MZXML_HPP

#include <iostream>
#include <vector>
#include <string>
#include <numeric>

#define PUGIXML_HEADER_ONLY

#ifndef PUGIXML_PATH
#define PUGIXML_PATH "../../pugixml-1.14/src/pugixml.hpp"
#endif

#include PUGIXML_PATH

#define STREAMCRAFT_HEADER_ONLY
#include "StreamCraft_utils.hpp"

namespace sc {

  inline namespace mzxml {

    class MZXML_BINARY_METADATA {

      public:
        int precision;
        std::string compression;
        bool compressed;
        std::string byte_order;

        void print() {
          std::cout << std::endl;
          std::cout << "Precision:                 " << precision << std::endl;
          std::cout << "Compression:               " << compression << std::endl;
          std::cout << "Byte order:           " << byte_order << std::endl;
          std::cout << std::endl;
        };
    };

    class MZXML {

      private:

        MZXML_BINARY_METADATA binary_metadata;

        void extract_binary_metadata(const pugi::xml_node& first_node);

        std::vector<pugi::xml_node> link_vector_spectra_nodes();

        int extract_spec_index(const pugi::xml_node& spec);
        std::string extract_spec_id(const pugi::xml_node& spec);
        int extract_spec_scan(const pugi::xml_node& spec);
        int extract_spec_array_length(const pugi::xml_node& spec);
        int extract_spec_level(const pugi::xml_node& spec);
        std::string extract_spec_mode(const pugi::xml_node& spec);
        std::string extract_spec_polarity(const pugi::xml_node& spec);
        double extract_spec_lowmz(const pugi::xml_node& spec);
        double extract_spec_highmz(const pugi::xml_node& spec);
        double extract_spec_bpmz(const pugi::xml_node& spec);
        double extract_spec_bpint(const pugi::xml_node& spec);
        double extract_spec_tic(const pugi::xml_node& spec);
        double extract_scan_rt(const pugi::xml_node& spec);
        double extract_ion_mz(const pugi::xml_node& spec);
        double extract_activation_ce(const pugi::xml_node& spec);

        MS_SPECTRA_HEADERS extract_spectra_headers(const std::vector<int>& idxs);

        std::vector<std::vector<double>> extract_spectrum(const pugi::xml_node& spectrum_node);

        std::vector<std::vector<std::vector<double>>> extract_spectra(const std::vector<int>& idxs);

      public:

        std::string file_path;

        std::string file_dir;

        std::string file_name;

        std::string file_extension;

        pugi::xml_document doc;

        pugi::xml_parse_result loading_result;

        pugi::xml_node root;

        std::string format;

        std::string name;

        int number_spectra;

        int number_chromatograms = 0;

        int number_spectra_binary_arrays = 2;

        MS_SPECTRA_HEADERS first_spectra_headers;

        MZXML(const std::string& file);

        MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {});

        MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) {
          MS_CHROMATOGRAMS_HEADERS chromatograms_headers;
          std::cout << "mzXML format does not have chromatograms!" << std::endl;
          return chromatograms_headers;
        };

        std::vector<std::vector<std::vector<double>>> get_spectra(std::vector<int> indices = {});

        std::vector<std::vector<std::vector<double>>> get_chromatograms(std::vector<int> indices = {}) {
          std::vector<std::vector<std::vector<double>>> chromatograms;
          std::cout << "mzXML format does not have chromatograms!" << std::endl;
          return chromatograms;
        };

        std::vector<int> get_spectra_index(std::vector<int> indices = {});
        std::vector<int> get_spectra_scan_number(std::vector<int> indices = {});
        std::vector<int> get_spectra_array_length(std::vector<int> indices = {});
        std::vector<int> get_spectra_level(std::vector<int> indices = {});
        std::vector<std::string> get_spectra_mode(std::vector<int> indices = {});
        std::vector<std::string> get_spectra_polarity(std::vector<int> indices = {});
        std::vector<double> get_spectra_lowmz(std::vector<int> indices = {});
        std::vector<double> get_spectra_highmz(std::vector<int> indices = {});
        std::vector<double> get_spectra_bpmz(std::vector<int> indices = {});
        std::vector<double> get_spectra_bpint(std::vector<int> indices = {});
        std::vector<double> get_spectra_tic(std::vector<int> indices = {});
        std::vector<double> get_spectra_rt(std::vector<int> indices = {});
        std::vector<double> get_spectra_precursor_mz(std::vector<int> indices = {});
        std::vector<double> get_spectra_collision_energy(std::vector<int> indices = {});

        std::vector<double> get_spectra_drift(std::vector<int> indices = {}) {
          std::vector<double> drift;
          std::cout << "mzXML format does not hold drift values!" << std::endl;
          return drift;
        };
        std::vector<int> get_spectra_precursor_scan(std::vector<int> indices = {}) {
          std::vector<int> precursor_scan;
          std::cout << "mzXML format does not hold precursor scan values!" << std::endl;
          return precursor_scan;
        };
        std::vector<double> get_spectra_precursor_window_mz(std::vector<int> indices = {}) {
          std::vector<double> precursor_window_mz;
          std::cout << "mzXML format does not hold precursor window mz values!" << std::endl;
          return precursor_window_mz;
        };
        std::vector<double> get_spectra_precursor_window_mzlow(std::vector<int> indices = {}) {
          std::vector<double> precursor_window_mzlow;
          std::cout << "mzXML format does not hold precursor window mz low values!" << std::endl;
          return precursor_window_mzlow;
        };
        std::vector<double> get_spectra_precursor_window_mzhigh(std::vector<int> indices = {}) {
          std::vector<double> precursor_window_mzhigh;
          std::cout << "mzXML format does not hold precursor window mz high values!" << std::endl;
          return precursor_window_mzhigh;
        };

        std::vector<std::vector<std::string>> get_software();
        std::vector<std::vector<std::string>> get_hardware();
        
        void print();

        void print_binary_metadata() { binary_metadata.print(); };

    }; // class MZXML

    void test_extract_spectra_mzxml(const std::string& file);

  }; // namespace mzxml

}; // namespace sc

#endif // STREAMCRAFT_MZXML_HPP

#if defined(STREAMCRAFT_HEADER_ONLY) && !defined(STREAMCRAFT_MZXML_SOURCE)
#	define STREAMCRAFT_MZXML_SOURCE "StreamCraft_mzxml.cpp"
#	include STREAMCRAFT_MZXML_SOURCE
#endif
