#include <iostream>
#include <vector>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_ms_annotation_isotopes(Rcpp::DataFrame features) {

  Rcpp::List list_out;

  int number_of_features = features.nrows();

  std::vector<std::string> ids = features["feature"];

  std::vector<double> rts = features["rt"];
  std::vector<double> rtmins = features["rtmin"];
  std::vector<double> rtmaxs = features["rtmax"];
  std::vector<double> mzs = features["mz"];
  std::vector<double> mzmins = features["mzmin"];
  std::vector<double> mzmaxs = features["mzmax"];
  // NumericVector intensities = features["intensity"];


  for (int i = 218; i < 219; ++i) {

    std::string id = ids[i];

    std::cout << id << std::endl;

    double rtmin = rtmins[i];
    double rtmax = rtmaxs[i];
    double mzmin = mzmins[i];
    double mzmax = mzmaxs[i];

    double mzr = (mzmax - mzmin) / 2; // better to replace for the sd of the eic mzs
    std::cout << mzr << std::endl;

    std::vector<int> which_pks;
    for (int z = 0; z < number_of_features; ++z) {
      if (rts[z] >= rtmin && rts[z] <= rtmax) {
        which_pks.push_back(z);
      }
    }

    int number_pks = which_pks.size();
    std::cout << number_pks << std::endl;

    const double* mzs_ptr = mzs.data();

    std::vector<double> omz(number_pks);
    double* omz_ptr = omz.data();

    for (const int& x : which_pks) {
      *(omz_ptr++) = *(mzs_ptr + x);
    }

    std::vector<int> idx(omz.size());
    std::iota(idx.begin(), idx.end(), 0);
    std::sort(idx.begin(), idx.end(), [&](int i, int j){return omz[i] < omz[j];});

    const double* omz_ptr2 = omz.data();

    std::vector<double> mz(number_pks);
    double* mz_ptr = mz.data();

    const int* which_pks_ptr = which_pks.data();

    std::vector<int> pks_idx(number_pks);
    int* pks_idx_ptr = pks_idx.data();

    for (const int& x : idx) {
      *(mz_ptr++) = *(omz_ptr2 + x);
      *(pks_idx_ptr++) = *(which_pks_ptr + x);
    }








    // std::cout << mz.size() << std::endl;

    // mz = mzs[which_idx];

    for (size_t z = 0; z < mz.size(); ++z) {
      std::cout << which_pks[z] << " " << mz[z] << " " << pks_idx[z] << std::endl;
    }








  }

  return list_out;
}
