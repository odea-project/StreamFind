#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List rcpp_extract_eics(Rcpp::List spec, Rcpp::DataFrame targets) {

  const Rcpp::NumericVector rtmins = targets["rtmin"];
  const Rcpp::NumericVector rtmaxs = targets["rtmax"];
  const Rcpp::NumericVector mzmins = targets["mzmin"];
  const Rcpp::NumericVector mzmaxs = targets["mzmax"];
  const Rcpp::StringVector ids = targets["id"];
  const Rcpp::StringVector analyses = targets["analysis"];

  bool replicate_names = false;
  Rcpp::StringVector replicates;

  bool has_rpl = targets.containsElementNamed("replicate");

  if (has_rpl) {
    replicate_names = true;
    replicates = targets["replicate"];
  }

  const int totalNumber = targets.nrows();

  Rcpp::List eics(totalNumber);

  for (int i=0; i<totalNumber; ++i) {

    Rcpp::String ana = analyses(i);
    Rcpp::String id = ids(i);

    double rtmin = rtmins[i];
    double rtmax = rtmaxs[i];
    double mzmin = mzmins[i];
    double mzmax = mzmaxs[i];

    Rcpp::DataFrame temp_df = Rcpp::as<Rcpp::DataFrame>(spec[ana]);

    Rcpp::NumericVector rt = temp_df["rt"];
    Rcpp::NumericVector mz = temp_df["mz"];
    Rcpp::NumericVector intensity = temp_df["intensity"];

    if (rtmax == 0) rtmax = *std::max_element(rt.begin(), rt.end());
    if (mzmax == 0) mzmax = *std::max_element(rt.begin(), rt.end());

    Rcpp::LogicalVector eval_rtmin = rt >= rtmin;
    Rcpp::LogicalVector eval_rtmax = rt <= rtmax;
    Rcpp::LogicalVector eval_mzmin = mz >= mzmin;
    Rcpp::LogicalVector eval_mzmax = mz <= mzmax;

    Rcpp::LogicalVector eval = eval_rtmin & eval_rtmax;
    eval = eval & eval_mzmin;
    eval = eval & eval_mzmax;

    rt = rt[eval];
    intensity = intensity[eval];

    Rcpp::LogicalVector eval_dupli = Rcpp::duplicated(rt);
    Rcpp::NumericVector rt_dupli = rt[eval_dupli];

    if (replicate_names && rt.size() > 0) {

      Rcpp::String rpl = replicates(i);

      eics[i] = Rcpp::DataFrame::create(Rcpp::Named("analysis") = ana,
                                        Rcpp::Named("replicate") = rpl,
                                        Rcpp::Named("id") = id,
                                        Rcpp::Named("rt") = rt,
                                        Rcpp::Named("intensity") = intensity);

    } else if (rt.size() > 0){

      eics[i] = Rcpp::DataFrame::create(Rcpp::Named("analysis") = ana,
                                        Rcpp::Named("id") = id,
                                        Rcpp::Named("rt") = rt,
                                        Rcpp::Named("intensity") = intensity);
    } else {

      eics[i] = Rcpp::DataFrame::create();

    }
  }

  return eics;
}
