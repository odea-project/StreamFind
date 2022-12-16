#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_ms_extract_eics_from_dataframe(DataFrame spec, DataFrame targets) {

  const NumericVector rtmins = targets["rtmin"];
  const NumericVector rtmaxs = targets["rtmax"];
  const NumericVector mzmins = targets["mzmin"];
  const NumericVector mzmaxs = targets["mzmax"];
  const StringVector ids = targets["id"];

  const int totalTargetsNumber = targets.nrows();

  StringVector target_id;

  double rtmin;
  double rtmax;
  double mzmin;
  double mzmax;

  NumericVector rt;
  NumericVector mz;
  NumericVector intensity;

  LogicalVector eval_rtmin;
  LogicalVector eval_rtmax;
  LogicalVector eval_mzmin;
  LogicalVector eval_mzmax;

  LogicalVector eval;

  List eics(totalTargetsNumber);

  for (int i=0; i<totalTargetsNumber; ++i) {

    target_id = ids(i);

    rtmin = rtmins[i];
    rtmax = rtmaxs[i];
    mzmin = mzmins[i];
    mzmax = mzmaxs[i];

    rt = spec["rt"];
    mz = spec["mz"];
    intensity = spec["intensity"];

    if (rtmax == 0) rtmax = *std::max_element(rt.begin(), rt.end());
    if (mzmax == 0) mzmax = *std::max_element(rt.begin(), rt.end());

    eval_rtmin = rt >= rtmin;
    eval_rtmax = rt <= rtmax;
    eval_mzmin = mz >= mzmin;
    eval_mzmax = mz <= mzmax;

    eval = eval_rtmin & eval_rtmax;
    eval = eval & eval_mzmin;
    eval = eval & eval_mzmax;

    rt = rt[eval];
    mz = mz[eval];
    intensity = intensity[eval];

    target_id = Rcpp::rep(target_id, rt.size());

    if (rt.size() > 0) {

      eics[i] = DataFrame::create(
        Named("id") = target_id,
        Named("rt") = rt,
        Named("mz") = mz,
        Named("intensity") = intensity);

    } else {

      eics[i] = DataFrame::create();

    }
  }

  return(eics);
}
