#include <string>
#include <vector>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_centroid_spectra(Rcpp::DataFrame spectra) {

  Rcpp::List list_out;

  std::vector<int> all_scan = spectra["scan"];
  std::vector<double> all_rt = spectra["rt"];
  std::vector<double> all_mz = spectra["mz"];
  std::vector<double> all_intensity = spectra["intensity"];

  std::size_t number_traces = all_scan.size();

  // Finds unique scans and rt vals
  std::set<int> unique_scans_set(all_scan.begin(), all_scan.end());
  std::vector<int> unique_scans(unique_scans_set.begin(), unique_scans_set.end());

  std::set<double> unique_rt_set(all_rt.begin(), all_rt.end());
  std::vector<double> unique_rt_vals(unique_rt_set.begin(), unique_rt_set.end());

  // Number of scans should match the number of unique rt values
  // - When ion mobility is applied this is TRUE, meaning that the rt values should
  // be amended with drift time values
  if (unique_scans.size() != unique_rt_vals.size()) {
    Rcpp::Rcout << "\u2717 The number of scans differs from the number of retention time values!" << std::endl;
    return list_out;
  }

  int unique_scans_size = unique_scans.size();

  // Loops over each scan
  // - Possibly the loop could run parallel using OpenMP or RcppParallel
  // - For now a simple for loop can be implemented
  for (int i = 0; i < unique_scans_size; i++) {

    // get the actual scan number as it can be different from iterator i
    int scan = unique_scans.at(i);

    // Diagnostic output
    Rcpp::Rcout << "\u2699 Processing scan: " << scan << " " << std::endl;

    // finds the indexes of the scan values and gets them
    std::vector<std::size_t> which_idx;

    for (std::size_t z = 0; z < number_traces; z++) {
      if (all_scan[z] == scan) which_idx.push_back(z);
    }

    int size_spec = which_idx.size();

    std::vector<double> mz(size_spec);
    std::vector<double> intensity(size_spec);

    for (std::size_t z = 0; z < which_idx.size(); ++z) {
      mz[z] = all_mz[which_idx[z]];
      intensity[z] = all_intensity[which_idx[z]];
    }

    // sorts both mz and intensity by mz, according to Max this is needed
    // most likely, when parsing spectra the m/z are ordered by default
    // but to be sure it is done here
    std::vector<std::size_t> sort_idx(mz.size());
    std::iota(sort_idx.begin(), sort_idx.end(), 0);
    std::sort(sort_idx.begin(), sort_idx.end(), [&](std::size_t i, std::size_t j){return mz[i] < mz[j];});

    std::vector<double> sorted_mz(mz.size());
    std::vector<double> sorted_intensity(mz.size());

    for (std::size_t z = 0; z < sort_idx.size(); ++z) {
      sorted_mz[z] = mz[sort_idx[z]];
      sorted_intensity[z] = intensity[sort_idx[z]];
    }



    // further implementation...



    // Diagnostic print out
    // int n = sorted_mz.size();
    // for (int z = 0; z < n; z++) {
    //   std::cout << sorted_mz.at(z) << " ";
    // }
    // std::cout << std::endl;
    // std::cout << std::endl;



    // Closing as a DataFrame per scan
    // - perhaps this could remain as vectors by inserting one vector after the other
    double rt = unique_rt_vals.at(i);

    Rcpp::DataFrame df = Rcpp::DataFrame::create(
      Rcpp::Named("scan") = scan,
      Rcpp::Named("rt") = rt,
      Rcpp::Named("mz") = sorted_mz,
      Rcpp::Named("intensity") = sorted_intensity
    );

    df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

    list_out["output"]= df;
  }

  return list_out;
}
