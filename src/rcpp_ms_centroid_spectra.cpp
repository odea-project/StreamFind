#include <string.h>
#include <vector>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::DataFrame rcpp_centroid_spectra(Rcpp::List spectra) {

  Rcpp::List list_out;

  std::vector<double> rt = spectra["mz"];
  std::vector<double> mz = spectra["mz"];

  std::string str_ex = "Test string";

  list_out["rt"] = rt;
  list_out["mz"] = mz;

  return list_out;
}
