#ifndef STREAMCRAFT_UTILS_HPP
#define STREAMCRAFT_UTILS_HPP

#include <iostream>
#include <string>
#include <vector>

namespace sc {

  inline namespace utils {

    struct MS_SPECTRUM {
      int index;
      int scan;
      int array_length;
      int level;
      int mode;
      int polarity;
      double lowmz;
      double highmz;
      double bpmz;
      double bpint;
      double tic;
      double rt;
      double drift;
      double window_mz;
      double window_mzlow;
      double window_mzhigh;
      double precursor_mz;
      double precursor_intensity;
      int precursor_charge;
      double activation_ce;
      std::vector<std::vector<double>> binary_data;
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
      std::string type;
      double min_mz;
      double max_mz;
      double start_rt;
      double end_rt;
      bool has_ion_mobility;
    };

    enum MS_SPECTRA_MODE {
      CENTROID,
      PROFILE
    };

    struct MS_SPECTRA_HEADERS {
      std::vector<int> index;
      std::vector<std::string> id;
      std::vector<int> scan;
      std::vector<int> array_length;
      std::vector<int> level;
      std::vector<int> mode;
      std::vector<int> polarity;
      std::vector<double> lowmz;
      std::vector<double> highmz;
      std::vector<double> bpmz;
      std::vector<double> bpint;
      std::vector<double> tic;
      std::vector<std::string> title;
      std::vector<double> rt;
      std::vector<double> drift;
      std::vector<std::string> filter_string;
      std::vector<int> config;
      std::vector<double> injection_ion_time;
      std::vector<int> precursor_scan;
      std::vector<double> window_mz;
      std::vector<double> window_mzlow;
      std::vector<double> window_mzhigh;
      std::vector<double> precursor_mz;
      std::vector<double> precursor_intensity;
      std::vector<int> precursor_charge;
      std::vector<std::string> activation_type;
      std::vector<double> activation_ce;

      void resize_all(int n) {
        index.resize(n);
        id.resize(n);
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
        title.resize(n);
        rt.resize(n);
        drift.resize(n);
        filter_string.resize(n);
        config.resize(n);
        injection_ion_time.resize(n);
        precursor_scan.resize(n);
        window_mz.resize(n);
        window_mzlow.resize(n);
        window_mzhigh.resize(n);
        precursor_mz.resize(n);
        precursor_intensity.resize(n);
        precursor_charge.resize(n);
        activation_type.resize(n);  
        activation_ce.resize(n);
      }
    };

    struct MS_CHROMATOGRAMS_HEADERS {
      std::vector<int> index;
      std::vector<std::string> id;
      std::vector<int> array_length;
      std::vector<int> polarity;
      std::vector<double> precursor_mz;
      std::vector<std::string> activation_type;
      std::vector<double> activation_ce;
      std::vector<double> product_mz;

      void resize_all(int n) {
        index.resize(n);
        id.resize(n);
        array_length.resize(n);
        polarity.resize(n);
        precursor_mz.resize(n);
        activation_type.resize(n);
        activation_ce.resize(n);
        product_mz.resize(n);
      };
    };

    struct MS_TARGETS {
      std::vector<int> index;
      std::vector<std::string> id;
      std::vector<int> level;
      std::vector<int> polarity;
      std::vector<bool> precursor;
      std::vector<double> mzmin;
      std::vector<double> mzmax;
      std::vector<double> rtmin;
      std::vector<double> rtmax;
      std::vector<double> driftmin;
      std::vector<double> driftmax;

      void resize_all(int n) {
        index.resize(n);
        id.resize(n);
        level.resize(n);
        polarity.resize(n);
        precursor.resize(n);
        mzmin.resize(n);
        mzmax.resize(n);
        rtmin.resize(n);
        rtmax.resize(n);
        driftmin.resize(n);
        driftmax.resize(n);
      };

      MS_TARGETS operator[](int i) {
        MS_TARGETS target;
        target.index.push_back(index[i]);
        target.id.push_back(id[i]);
        target.level.push_back(level[i]);
        target.polarity.push_back(polarity[i]);
        target.precursor.push_back(precursor[i]);
        target.mzmin.push_back(mzmin[i]);
        target.mzmax.push_back(mzmax[i]);
        target.rtmin.push_back(rtmin[i]);
        target.rtmax.push_back(rtmax[i]);
        target.driftmin.push_back(driftmin[i]);
        target.driftmax.push_back(driftmax[i]);
        return target;
      };
    };

    class VIRTUAL_MS_FILE {
      public:
        virtual ~VIRTUAL_MS_FILE() = default;
        virtual int get_number_spectra() = 0;
        virtual int get_number_chromatograms() = 0;
        virtual int get_number_spectra_binary_arrays() = 0;
        virtual std::string get_format() = 0;
        virtual std::string get_type() = 0;
        virtual std::string get_time_stamp() = 0;
        virtual std::vector<int> get_polarity() = 0;
        virtual std::vector<int> get_mode() = 0;
        virtual std::vector<int> get_level() = 0;
        virtual double get_min_mz() = 0;
        virtual double get_max_mz() = 0;
        virtual double get_start_rt() = 0;
        virtual double get_end_rt() = 0;
        virtual bool has_ion_mobility() = 0;
        virtual MS_SUMMARY get_summary() = 0;
        virtual std::vector<int> get_spectra_index(std::vector<int> indices = {}) = 0;
        virtual std::vector<int> get_spectra_scan_number(std::vector<int> indices = {}) = 0;
        virtual std::vector<int> get_spectra_array_length(std::vector<int> indices = {}) = 0;
        virtual std::vector<int> get_spectra_level(std::vector<int> indices = {}) = 0;
        virtual std::vector<int> get_spectra_mode(std::vector<int> indices = {}) = 0;
        virtual std::vector<int> get_spectra_polarity(std::vector<int> indices = {}) = 0;
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
        virtual MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {}) = 0;
        virtual MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) = 0;
        virtual std::vector<std::vector<std::vector<double>>> get_spectra(std::vector<int> indices = {}) = 0;
        virtual std::vector<std::vector<std::vector<double>>> get_chromatograms(std::vector<int> indices = {}) = 0;
        virtual std::vector<std::vector<std::string>> get_software() = 0;
        virtual std::vector<std::vector<std::string>> get_hardware() = 0;
        virtual MS_SPECTRUM get_spectrum(int index) = 0;
    };

    template <typename T>
    class MS_FILE : public VIRTUAL_MS_FILE {
      public:
        MS_FILE(const std::string& file) : ms(file) {}
        int get_number_spectra() override { return ms.get_number_spectra(); }
        int get_number_chromatograms() override { return ms.get_number_chromatograms(); }
        int get_number_spectra_binary_arrays() { return ms.get_number_spectra_binary_arrays(); }
        std::string get_format() { return ms.get_format(); }
        std::string get_type() { return ms.get_type(); }
        std::string get_time_stamp() { return ms.get_time_stamp(); }
        std::vector<int> get_polarity() { return ms.get_polarity(); }
        std::vector<int> get_mode() { return ms.get_mode(); }
        std::vector<int> get_level() { return ms.get_level(); }
        double get_min_mz() { return ms.get_min_mz(); }
        double get_max_mz() { return ms.get_max_mz(); }
        double get_start_rt() { return ms.get_start_rt(); }
        double get_end_rt() { return ms.get_end_rt(); }
        bool has_ion_mobility() { return ms.has_ion_mobility(); }
        MS_SUMMARY get_summary() { return ms.get_summary(); }
        std::vector<int> get_spectra_index(std::vector<int> indices = {}) { return ms.get_spectra_index(indices); }
        std::vector<int> get_spectra_scan_number(std::vector<int> indices = {}) { return ms.get_spectra_scan_number(indices); }
        std::vector<int> get_spectra_array_length(std::vector<int> indices = {}) { return ms.get_spectra_array_length(indices); }
        std::vector<int> get_spectra_level(std::vector<int> indices = {}) { return ms.get_spectra_level(indices); }
        std::vector<int> get_spectra_mode(std::vector<int> indices = {}) { return ms.get_spectra_mode(indices); }
        std::vector<int> get_spectra_polarity(std::vector<int> indices = {}) { return ms.get_spectra_polarity(indices); }
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
        MS_SPECTRA_HEADERS get_spectra_headers(std::vector<int> indices = {}) override { return ms.get_spectra_headers(indices); }
        MS_CHROMATOGRAMS_HEADERS get_chromatograms_headers(std::vector<int> indices = {}) override { return ms.get_chromatograms_headers(indices); }
        std::vector<std::vector<std::vector<double>>> get_spectra(std::vector<int> indices = {}) override { return ms.get_spectra(indices); }
        std::vector<std::vector<std::vector<double>>> get_chromatograms(std::vector<int> indices = {}) override { return ms.get_chromatograms(indices); }
        std::vector<std::vector<std::string>> get_software() { return ms.get_software(); }
        std::vector<std::vector<std::string>> get_hardware() { return ms.get_hardware(); }
        MS_SPECTRUM get_spectrum(int index) override { return ms.get_spectrum(index); }

      private:
        T ms;
    };

    std::string encode_little_endian(const std::vector<double>& input, const int& precision);

    std::vector<double> decode_little_endian(const std::string& str, const int& precision);

    std::string encode_big_endian(const std::vector<double>& input, const int& precision);

    std::vector<double> decode_big_endian(const std::string& str, const int& precision);

    std::string compress_zlib(const std::string& str);

    std::string decompress_zlib(const std::string& compressed_string);

    std::string encode_base64(const std::string& str);

    std::string decode_base64(const std::string& encoded_string);
  }; // namespace utils
};

#endif // STREAMCRAFT_UTILS_HPP

#if defined(STREAMCRAFT_HEADER_ONLY) && !defined(STREAMCRAFT_UTILS_SOURCE)
#	define STREAMCRAFT_UTILS_SOURCE "StreamCraft_utils.cpp"
#	include STREAMCRAFT_UTILS_SOURCE
#endif