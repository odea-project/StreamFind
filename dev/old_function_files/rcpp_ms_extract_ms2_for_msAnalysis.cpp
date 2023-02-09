#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_ms_extract_ms2_for_msAnalysis(DataFrame spec,
                                        DataFrame targets,
                                        double mzClust,
                                        bool verbose) {

  const StringVector ids = targets["id"];

  const NumericVector rtmins = targets["rtmin"];
  const NumericVector rtmaxs = targets["rtmax"];
  const NumericVector mzmins = targets["mzmin"];
  const NumericVector mzmaxs = targets["mzmax"];

  const int totalTargetsNumber = targets.nrows();

  StringVector target_id;

  double rtmin;
  double rtmax;
  double mzmin;
  double mzmax;

  NumericVector rt;
  NumericVector ce;
  NumericVector preMZ;
  NumericVector mz;
  NumericVector intensity;

  LogicalVector eval_rtmin;
  LogicalVector eval_rtmax;
  LogicalVector eval_mzmin;
  LogicalVector eval_mzmax;

  LogicalVector eval;

  IntegerVector idx;
  NumericVector mz_diff;

  LogicalVector hasFromSameScan = true;

  LogicalVector log_clusters;
  IntegerVector mz_clusters;

  NumericVector new_mz;
  NumericVector temp_mz;
  double temp_mz_mean;

  NumericVector new_intensity;
  NumericVector temp_intensity;
  double temp_intensity_mean;

  IntegerVector temp_idx;
  NumericVector temp_rt;
  NumericVector temp_rt_unique;

  LogicalVector isPre;

  double preMZ_mean = 0;
  double rt_mean = 0;

  List eics(totalTargetsNumber);

  for (int i=0; i<totalTargetsNumber; ++i) {

    target_id = ids(i);

    rtmin = rtmins[i];
    rtmax = rtmaxs[i];
    mzmin = mzmins[i];
    mzmax = mzmaxs[i];

    rt = spec["rt"];
    ce = spec["ce"];
    preMZ= spec["preMZ"];
    mz = spec["mz"];
    intensity = spec["intensity"];

    if (rtmax == 0) rtmax = *std::max_element(rt.begin(), rt.end());
    if (mzmax == 0) mzmax = *std::max_element(mz.begin(), mz.end());

    eval_rtmin = rt >= rtmin;
    eval_rtmax = rt <= rtmax;
    eval_mzmin = preMZ >= mzmin;
    eval_mzmax = preMZ <= mzmax;

    eval = eval_rtmin & eval_rtmax;
    eval = eval & eval_mzmin;
    eval = eval & eval_mzmax;

    rt = rt[eval];
    ce = ce[eval];
    preMZ = preMZ[eval];
    mz = mz[eval];
    intensity = intensity[eval];

    if (mz.size() > 0) {

      idx = seq_along(mz) - 1;
      std::sort(idx.begin(), idx.end(), [&](int i, int j){return mz[i] < mz[j];});

      rt = rt[idx];
      ce = ce[idx];
      preMZ = preMZ[idx];
      mz = mz[idx];
      intensity = intensity[idx];

      mz_diff = diff(mz);

      // TODO replace by automated estimation of the width for clustering to
      // avoid arbitrary "mzClust" value. For now, the while loop checks if
      // more than one "equal" mass is in the same scan (i.e., rt).

      mzClust = mzClust + (mzClust * 0.1);

      while (is_true(any(hasFromSameScan))) {

        mzClust = mzClust - (mzClust * 0.1);

        log_clusters = mz_diff > mzClust;

        mz_clusters = Rcpp::rep(0, mz_diff.size());

        mz_clusters[log_clusters] = 1;

        std::partial_sum(mz_clusters.begin(), mz_clusters.end(), mz_clusters.begin());

        mz_clusters.push_front(0);

        mz_clusters = mz_clusters + 1;

        IntegerVector idx_clusters = seq_along(mz_clusters) - 1;
        IntegerVector unique_clusters = unique(mz_clusters);
        unique_clusters.sort();
        int unique_clusters_size = unique_clusters.size();

        hasFromSameScan = rep(true, unique_clusters_size);

        new_mz.erase(new_mz.begin(), new_mz.end());
        new_intensity.erase(new_intensity.begin(), new_intensity.end());

        for (int z=0; z<unique_clusters_size; ++z) {

          temp_idx = idx_clusters[mz_clusters == unique_clusters[z]];

          temp_mz = mz[temp_idx];
          temp_mz_mean = sum(temp_mz) / temp_mz.size();
          new_mz.push_back(temp_mz_mean);

          temp_intensity = intensity[temp_idx];
          temp_intensity_mean = sum(temp_intensity) / temp_intensity.size();
          new_intensity.push_back(temp_intensity_mean);

          temp_rt = rt[temp_idx];
          temp_rt_unique = unique(temp_rt);

          hasFromSameScan[z] = temp_rt_unique.size() < temp_rt.size();

          if (hasFromSameScan[z] & verbose) {
            Rcpp::Rcout << "The m/z " << temp_mz_mean << " of " << target_id[0] <<
              " is present more than once in the same scan!" << "\n";
          }
        }
      }

      if (verbose) {
        Rcpp::Rcout << "Clustering for " << target_id[0] <<
          " preformed with " << mzClust << " Da" << "\n";
      }

      preMZ_mean = sum(preMZ) / preMZ.size();
      rt_mean = sum(rt) / rt.size();

      isPre = rep(false, new_mz.size());
      isPre[(new_mz >= (preMZ_mean - mzClust)) & (new_mz <= (preMZ_mean + mzClust))] = true;

    }

    if (mz.size() > 0) {

      eics[i] = DataFrame::create(
        Named("id") = target_id,
        Named("preMZ") = preMZ_mean,
        Named("rt") = rt_mean,
        Named("mz") = new_mz,
        Named("intensity") = new_intensity,
        Named("isPre") = isPre);

    } else {

      eics[i] = DataFrame::create();

    }

    new_mz.erase(new_mz.begin(), new_mz.end());
    new_intensity.erase(new_intensity.begin(), new_intensity.end());
    hasFromSameScan = true;

  }

  return(eics);
}
