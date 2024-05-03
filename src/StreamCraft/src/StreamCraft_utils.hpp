#ifndef STREAMCRAFT_UTILS_HPP
#define STREAMCRAFT_UTILS_HPP

#include <iostream>
#include <string>
#include <vector>

namespace sc {

  inline namespace utils {

    class VIRTUAL_MS_SPECTRUM {
      public:
        virtual ~VIRTUAL_MS_SPECTRUM() = default;
        virtual int extract_spec_index() = 0;
        virtual std::string extract_spec_id() const = 0;
        virtual int extract_spec_scan() const = 0;
        virtual int extract_spec_array_length() const = 0;
        virtual int extract_spec_level() const = 0;
        virtual int extract_spec_mode() const = 0;
        virtual int extract_spec_polarity() const = 0;
        virtual double extract_spec_lowmz() const = 0;
        virtual double extract_spec_highmz() const = 0;
        virtual double extract_spec_bpmz() const = 0;
        virtual double extract_spec_bpint() const = 0;
        virtual double extract_spec_tic() const = 0;
        virtual double extract_scan_rt() const = 0;
        virtual double extract_ion_mz() const = 0;
        virtual double extract_activation_ce() const = 0;
        virtual bool has_precursor() const = 0;
        // virtual std::vector<std::vector<double>> extract_binary_data(const MZXML_BINARY_METADATA&  mtd) const = 0;
    };

    template <typename T>
    class MS_SPECTRUM : public VIRTUAL_MS_SPECTRUM {
      public:
        MS_SPECTRUM(T spec_in) : spec(spec_in) {}
        int extract_spec_index() override { return spec.extract_spec_index(); }
        std::string extract_spec_id() const override { return spec.extract_spec_id(); }
        int extract_spec_scan() const override { return spec.extract_spec_scan(); }
        int extract_spec_array_length() const override { return spec.extract_spec_array_length(); }
        int extract_spec_level() const override { return spec.extract_spec_level(); }
        int extract_spec_mode() const override { return spec.extract_spec_mode(); }
        int extract_spec_polarity() const override { return spec.extract_spec_polarity(); }
        double extract_spec_lowmz() const override { return spec.extract_spec_lowmz(); }
        double extract_spec_highmz() const override { return spec.extract_spec_highmz(); }
        double extract_spec_bpmz() const override { return spec.extract_spec_bpmz(); }
        double extract_spec_bpint() const override { return spec.extract_spec_bpint(); }
        double extract_spec_tic() const override { return spec.extract_spec_tic(); }
        double extract_scan_rt() const override { return spec.extract_scan_rt(); }
        double extract_ion_mz() const override { return spec.extract_ion_mz(); }
        double extract_activation_ce() const override { return spec.extract_activation_ce(); }
        bool has_precursor() const override { return spec.has_precursor(); }
        // std::vector<std::vector<double>> extract_binary_data(const MZXML_BINARY_METADATA&  mtd) const override { return spec.extract_binary_data(mtd); }

      private:
        T spec;
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

    std::string encode_little_endian(const std::vector<double>& input, const int& precision);

    std::vector<double> decode_little_endian(const std::string& str, const int& precision);

    std::string encode_big_endian(const std::vector<double>& input, const int& precision);

    std::vector<double> decode_big_endian(const std::string& str, const int& precision);

    std::string compress_zlib(const std::string& str);

    std::string decompress_zlib(const std::string& compressed_string);

    std::string encode_base64(const std::string& str);

    std::string decode_base64(const std::string& encoded_string);

    void test_encoding_decoding_little_endian(const std::vector<double>& input, const int& precision);

    void test_encoding_decoding_big_endian(const std::vector<double>& input, const int& precision);

  }; // namespace utils
};

#endif // STREAMCRAFT_UTILS_HPP

#if defined(STREAMCRAFT_HEADER_ONLY) && !defined(STREAMCRAFT_UTILS_SOURCE)
#	define STREAMCRAFT_UTILS_SOURCE "StreamCraft_utils.cpp"
#	include STREAMCRAFT_UTILS_SOURCE
#endif