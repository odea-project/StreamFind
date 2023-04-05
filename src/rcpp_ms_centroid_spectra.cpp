#include <string.h>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::DataFrame rcpp_centroid_spectra(Rcpp::List spectra) {

  Rcpp::List list_out;

  std::string str_ex = "Test string";

  list_out["test"] = str_ex;


  return list_out;
}
