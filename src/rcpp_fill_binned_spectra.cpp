#include <iostream>
#include <vector>
#include <string>
#include <tuple>
#include <algorithm>
#include <unordered_set>
#include <numeric>
#include <Rcpp.h>
#include <omp.h>

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
std::vector<double> rcpp_fill_bin_spectra(Rcpp::DataFrame spectra,
                                          Rcpp::DataFrame bin_mat,
                                          Rcpp::List bins,
                                          double overlap = 0,
                                          std::string summaryFunction = "max") {
  
  std::vector<std::string> bin_names = bins.names();
  
  std::vector<std::string> spectra_names = spectra.names();

  int number_bin_names = bin_names.size();

  int number_bins = bin_mat.nrow();
  
  int number_ints = spectra.nrow();
  
  std::vector<int> bin_name_index;
  
  std::vector<double> bin_sizes(number_bins);
  
  std::vector<std::vector<double>> bin_vals(number_bin_names);
  
  std::vector<std::vector<double>> spectra_mat(number_bin_names);
  
  for (int i = 0; i < number_bin_names; i++) {
    bin_sizes[i] = bins[i];
    
    std::vector<double> bin_i = bin_mat[i];
    
    bin_vals[i] = bin_i;
    
    std::string bin_name = bin_names[i];
    
    std::vector<double> spec_i = spectra[bin_name];
    
    spectra_mat[i] = spec_i;
  }
  
  std::vector<double> output(number_bins, 0.0);
  
  std::vector<double> ints = spectra["intensity"];
  
  #pragma omp parallel for
  for(int i = 0; i < number_bins; i++) {
    
    int num_threads = 0;

    num_threads = omp_get_num_threads();

    int thread_num = omp_get_thread_num();

    // #pragma omp critical
    // {
    //   Rprintf("T %d out of %d and ", thread_num, num_threads);
    //   
    //   Rprintf("Bin %d ", i);
    // }
    
    std::vector<int> which_ints;

    for (int j = 0; j < number_ints; j++) {

      std::vector<bool> is_in_bin(number_bin_names, false);

      for (int k = 0; k < number_bin_names; k++) {
        
        const double& k_size = bin_sizes[k];

        const std::vector<double>& k_vals = bin_vals[k];

        const double& k_val = k_vals[i];
        
        double k_low = k_val - (k_size / 2);

        double k_high = k_val + (k_size / 2);

        k_low = k_low - (k_size * overlap);

        k_high = k_high + (k_size * overlap);

        const std::vector<double>& s_vals = spectra_mat[k];

        const double& s_val = s_vals[j];

        if (s_val >= k_low && s_val <= k_high) {
          is_in_bin[k] = true;
        }
      }

      if (std::all_of(is_in_bin.begin(), is_in_bin.end(), [](bool x) { return x; })) {
        which_ints.push_back(j);
      }
    }

    int n_sel_ints = which_ints.size();
    
    // #pragma omp critical
    // {
    //   Rprintf("Ints %d; ", n_sel_ints);
    // }

    if (n_sel_ints > 0) {
      std::vector<double> ints_temp(n_sel_ints);
      double* ints_temp_ptr = ints_temp.data();

      for (const int& x : which_ints) {
        *(ints_temp_ptr++) = ints[x];
      }

      double summary_int = 0.0;

      if (summaryFunction == "max") {
        auto maxElementIterator = std::max_element(ints_temp.begin(), ints_temp.end());
        summary_int = *maxElementIterator;

      } else {
        double sum = std::accumulate(ints_temp.begin(), ints_temp.end(), 0.0);
        summary_int = sum / static_cast<double>(ints_temp.size());
      }

      #pragma omp critical
      {
        output[i] = summary_int;
      }
    }
  }
  
  return output;
}
