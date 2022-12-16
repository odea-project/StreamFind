#include <string>
#include <vector>
#include <numeric>
#include <math.h>

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List rcpp_extract_xics(Rcpp::List spec, Rcpp::DataFrame targets) {

  const Rcpp::NumericVector rtmins = targets["rtmin"];
  const Rcpp::NumericVector rtmaxs = targets["rtmax"];
  const Rcpp::NumericVector mzmins = targets["mzmin"];
  const Rcpp::NumericVector mzmaxs = targets["mzmax"];
  const Rcpp::NumericVector rt_ids = targets["rt"];
  const Rcpp::NumericVector mz_ids = targets["mz"];
  const Rcpp::StringVector ids = targets["id"];
  const Rcpp::StringVector analyses = targets["analysis"];

  bool replicate_names = false;
  Rcpp::StringVector replicates;

  if (targets.containsElementNamed("replicate")) {
    replicate_names = true;
    replicates = targets["replicate"];
  }

  const int totalNumber = targets.nrows();

  Rcpp::List xics(totalNumber);

  for (int i=0; i<totalNumber; ++i) {

    Rcpp::String ana = analyses(i);
    Rcpp::String id = ids(i);

    double rtmin = rtmins[i];
    double rtmax = rtmaxs[i];
    double mzmin = mzmins[i];
    double mzmax = mzmaxs[i];
    double mz_id = mz_ids[i];
    double rt_id = rt_ids[i];

    Rcpp::DataFrame temp_df = Rcpp::as<Rcpp::DataFrame>(spec[ana]);

    Rcpp::NumericVector rt = temp_df["rt"];
    Rcpp::NumericVector mz = temp_df["mz"];
    Rcpp::NumericVector intensity = temp_df["intensity"];

    Rcpp::LogicalVector eval_rtmin = rt >= rtmin;
    Rcpp::LogicalVector eval_rtmax = rt <= rtmax;
    Rcpp::LogicalVector eval_mzmin = mz >= mzmin;
    Rcpp::LogicalVector eval_mzmax = mz <= mzmax;
    Rcpp::LogicalVector eval = eval_rtmin & eval_rtmax;
    eval = eval & eval_mzmin;
    eval = eval & eval_mzmax;

    rt = rt[eval];
    mz = mz[eval];
    intensity = intensity[eval];

    if (replicate_names && rt.size() > 0) {

      Rcpp::String rpl = replicates(i);

      xics[i] = Rcpp::DataFrame::create(Rcpp::Named("analysis") = ana,
                                        Rcpp::Named("replicate") = rpl,
                                        Rcpp::Named("id") = id,
                                        Rcpp::Named("mz_id") = mz_id,
                                        Rcpp::Named("rt_id") = rt_id,
                                        Rcpp::Named("rt") = rt,
                                        Rcpp::Named("mz") = mz,
                                        Rcpp::Named("intensity") = intensity);

    } else if (rt.size() > 0) {

      xics[i] = Rcpp::DataFrame::create(Rcpp::Named("analysis") = ana,
                                        Rcpp::Named("id") = id,
                                        Rcpp::Named("mz_id") = mz_id,
                                        Rcpp::Named("rt_id") = rt_id,
                                        Rcpp::Named("rt") = rt,
                                        Rcpp::Named("mz") = mz,
                                        Rcpp::Named("intensity") = intensity);
    } else {

      xics[i] = Rcpp::DataFrame::create();

    }
  }

  return xics;
}
