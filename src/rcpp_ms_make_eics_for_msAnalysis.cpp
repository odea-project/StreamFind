#include <iostream>
#include <string>
#include <vector>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame rcpp_ms_make_eics_for_msAnalysis(DataFrame spec, DataFrame targets) {

  const vector<double> rtmins = targets["rtmin"];
  const vector<double> rtmaxs = targets["rtmax"];
  const vector<double> mzmins = targets["mzmin"];
  const vector<double> mzmaxs = targets["mzmax"];

  const vector<string> ids = targets["id"];

  const int totalTargetsNumber = targets.nrows();

  NumericVector spec_rt = spec["rt"];
  NumericVector spec_mz = spec["mz"];
  NumericVector spec_intensity = spec["intensity"];

  string target_id;
  vector<string> target_id_vec;

  double rtmin;
  double rtmax;
  double mzmin;
  double mzmax;

  vector<string> final_id;
  vector<double> final_rt;
  vector<double> final_mz;
  vector<double> final_intensity;

  for (int i=0; i<totalTargetsNumber; ++i) {

    target_id = ids[i];

    const int totalTracesNumber = spec_rt.size();

    vector<double> rt;
    vector<double> mz;
    vector<double> intensity;

    rtmin = rtmins[i];
    rtmax = rtmaxs[i];
    mzmin = mzmins[i];
    mzmax = mzmaxs[i];

    if (rtmax == 0) rtmax = *std::max_element(spec_rt.begin(), spec_rt.end());
    if (mzmax == 0) mzmax = *std::max_element(spec_mz.begin(), spec_mz.end());

    for (int j=0; j<totalTracesNumber; ++j) {

      if ((spec_rt[j] >= rtmin) &
          (spec_rt[j] <= rtmax) &
          (spec_mz[j] >= mzmin) &
          (spec_mz[j] <= mzmax)) {

        rt.push_back(spec_rt[j]);
        mz.push_back(spec_mz[j]);
        intensity.push_back(spec_intensity[j]);

      }
    }

    if (rt.size() > 0) {

      target_id_vec.assign(rt.size(), target_id);

      final_id.insert(final_id.end(), target_id_vec.begin(), target_id_vec.end());
      final_rt.insert(final_rt.end(), rt.begin(), rt.end());
      final_mz.insert(final_mz.end(), mz.begin(), mz.end());
      final_intensity.insert(final_intensity.end(), intensity.begin(), intensity.end());

    }
  }

  DataFrame eics = DataFrame::create(
    Named("id") = final_id,
    Named("rt") = final_rt,
    Named("mz") = final_mz,
    Named("intensity") = final_intensity);

  return(eics);
}
