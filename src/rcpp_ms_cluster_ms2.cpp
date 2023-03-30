#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_ms_cluster_ms2(DataFrame ms2, double mzClust, bool verbose) {

  StringVector all_ids = ms2["unique_id"];
  StringVector unique_ids = unique(all_ids);
  int totalNumberIds = unique_ids.size();
  int n = all_ids.size();

  StringVector target_id;

  StringVector analysis;
  StringVector id;
  NumericVector rt;
  NumericVector preMZ;
  NumericVector mz;
  NumericVector intensity;

  IntegerVector idx;
  NumericVector mz_diff;

  double itMzClust;

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

  List ms2_out(totalNumberIds);

  for (int i=0; i<totalNumberIds; ++i) {

    target_id = unique_ids(i);

    analysis = ms2["analysis"];
    id = ms2["id"];
    rt = ms2["rt"];
    preMZ= ms2["preMZ"];
    mz = ms2["mz"];
    intensity = ms2["intensity"];

    IntegerVector which_idx;
    for (int z=0; z<n; z++) {
      if (all_ids(z) == target_id(0)) which_idx.push_back(z);
    }

    analysis = analysis[which_idx];
    analysis = unique(analysis);
    id = id[which_idx];
    id = unique(id);
    rt = rt[which_idx];
    preMZ = preMZ[which_idx];
    mz = mz[which_idx];
    intensity = intensity[which_idx];

    if (mz.size() > 0) {

      idx = seq_along(mz) - 1;
      std::sort(idx.begin(), idx.end(), [&](int i, int j){return mz[i] < mz[j];});

      rt = rt[idx];
      preMZ = preMZ[idx];
      mz = mz[idx];
      intensity = intensity[idx];

      mz_diff = diff(mz);

      // TODO replace by automated estimation of the width for clustering to
      // avoid arbitrary "mzClust" value. For now, the while loop checks if
      // more than one "equal" mass is in the same scan (i.e., rt).

      itMzClust = mzClust;

      int counter = 0;

      while (is_true(any(hasFromSameScan))) {

        counter = counter + 1;

        if (verbose) Rcpp::Rcout << "Clustering " << target_id[0] << " with " << itMzClust << " Da...";

        log_clusters = mz_diff > itMzClust;

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

          temp_intensity = intensity[temp_idx];
          temp_intensity_mean = max(temp_intensity);
          new_intensity.push_back(temp_intensity_mean);

          temp_mz = mz[temp_idx];

          // temp_mz = temp_mz[temp_intensity == temp_intensity_mean];
          // temp_mz_mean = sum(temp_mz) / temp_mz.size();
          // new_mz.push_back(temp_mz_mean);

          // weighted mean with intensities
          int size_temp_mz = temp_mz.size();
          float mz_sum = 0, mz_numWeight = 0;
          for (int w = 0; w < size_temp_mz; w++) {
            mz_numWeight = mz_numWeight + temp_mz[w] * temp_intensity[w];
            mz_sum = mz_sum + temp_intensity[w];
          }
          temp_mz_mean = mz_numWeight / mz_sum;
          new_mz.push_back(temp_mz_mean);

          temp_rt = rt[temp_idx];
          temp_rt_unique = unique(temp_rt);

          hasFromSameScan[z] = temp_rt_unique.size() < temp_rt.size();

          if (counter > 10) hasFromSameScan[z] = false;
          if (itMzClust < 0.0001) hasFromSameScan[z] = false;

          if (hasFromSameScan[z] & verbose) {
            double min_mz = min(temp_mz);
            double max_mz = max(temp_mz);
            Rcpp::Rcout << "\n The m/z cluster " << min_mz << " to " << max_mz <<
              " of " << target_id[0] << " has traces from the same scan at:\n";
            Rcpp::Rcout << temp_rt << "\n";
            Rcpp::Rcout << temp_mz << "\n\n";

            itMzClust = itMzClust - 0.0001;
          }
        }
      }

      preMZ_mean = sum(preMZ) / preMZ.size();
      rt_mean = sum(rt) / rt.size();

      isPre = rep(false, new_mz.size());
      isPre[(new_mz >= (preMZ_mean - mzClust)) & (new_mz <= (preMZ_mean + mzClust))] = true;

      if (verbose) Rcpp::Rcout << "Done! \n\n";
    }

    if (mz.size() > 0) {

      ms2_out[i] = DataFrame::create(
        Named("analysis") = analysis,
        Named("id") = id,
        Named("preMZ") = preMZ_mean,
        Named("rt") = rt_mean,
        Named("mz") = new_mz,
        Named("intensity") = new_intensity,
        Named("isPre") = isPre
      );

      new_mz.erase(new_mz.begin(), new_mz.end());
      new_intensity.erase(new_intensity.begin(), new_intensity.end());
      hasFromSameScan = true;

    } else {
      ms2_out[i] = DataFrame::create();
    }
  }
  return(ms2_out);
}
