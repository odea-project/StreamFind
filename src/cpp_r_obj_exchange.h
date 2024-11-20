#ifndef STREAMCRAFT_R_INTERFACE_H
#define STREAMCRAFT_R_INTERFACE_H

#include <vector>
#include <string>
#include <Rcpp.h>
#include "StreamCraft_lib.h"

namespace sc_r {

  sc::MS_SPECTRA_HEADERS get_ms_analysis_list_headers(const Rcpp::List& analysis) {
    sc::MS_SPECTRA_HEADERS headers;
    const Rcpp::List& hd = analysis["spectra_headers"];
    const std::vector<int>& hd_index = hd["index"];
    const std::vector<int>& hd_polarity = hd["polarity"];
    const std::vector<int>& hd_configuration = hd["configuration"];
    const std::vector<float>& hd_rt = hd["rt"];
    const std::vector<int>& hd_level = hd["level"];
    const std::vector<float>& hd_pre_mz = hd["pre_mz"];
    const std::vector<float>& hd_pre_mz_low = hd["pre_mzlow"];
    const std::vector<float>& hd_pre_mz_high = hd["pre_mzhigh"];
    const std::vector<float>& hd_pre_ce = hd["pre_ce"];
    const std::vector<float>& hd_mobility = hd["mobility"];
    
    const int number_spectra = hd_index.size();
    headers.resize_all(number_spectra);
    
    headers.index = hd_index;
    headers.rt = hd_rt;
    headers.polarity = hd_polarity;
    headers.configuration = hd_configuration;
    headers.level = hd_level;
    headers.precursor_mz = hd_pre_mz;
    headers.activation_ce = hd_pre_ce;
    headers.mobility = hd_mobility;
    
    return headers;
  };
};

#endif // STREAMCRAFT_R_INTERFACE_H
