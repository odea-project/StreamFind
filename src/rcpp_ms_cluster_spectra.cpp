#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_ms_cluster_spectra(DataFrame spectra, double mzClust, bool verbose) {

  StringVector all_ids = spectra["unique_id"];
  StringVector unique_ids = unique(all_ids);
  int totalNumberIds = unique_ids.size();

  StringVector target_id;

  StringVector analysis;
  StringVector id;
  NumericVector rt;
  NumericVector mz;
  NumericVector intensity;

  LogicalVector eval;

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

  double rt_mean = 0;

  List list_out(totalNumberIds);

  for (int i=0; i<totalNumberIds; ++i) {

    target_id = unique_ids(i);

    analysis = spectra["analysis"];
    id = spectra["id"];
    rt = spectra["rt"];
    mz = spectra["mz"];
    intensity = spectra["intensity"];

    int n = all_ids.size();
    LogicalVector eval = rep(false, n);

    for (int z=0; z<n; z++) {
      eval(z) = (all_ids(z) == target_id(0));
    }

    analysis = analysis[eval];
    analysis = unique(analysis);
    id = id[eval];
    id = unique(id);
    rt = rt[eval];
    mz = mz[eval];
    intensity = intensity[eval];

    if (mz.size() > 0) {

      idx = seq_along(mz) - 1;
      std::sort(idx.begin(), idx.end(), [&](int i, int j){return mz[i] < mz[j];});

      rt = rt[idx];
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
          temp_intensity_mean = mean(temp_intensity);
          new_intensity.push_back(temp_intensity_mean);

          temp_mz = mz[temp_idx];
          // NumericVector temp_mz_2 = temp_mz[temp_intensity == temp_intensity_mean];
          // temp_mz_mean = sum(temp_mz_2) / temp_mz_2.size();
          // temp_mz_mean = sum(temp_mz) / temp_mz.size();

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

      if (verbose) Rcpp::Rcout << "Done! \n\n";

      rt_mean = sum(rt) / rt.size();
    }

    if (mz.size() > 0) {

      list_out[i] = DataFrame::create(
        Named("analysis") = analysis,
        Named("id") = id,
        Named("rt") = rt_mean,
        Named("mz") = new_mz,
        Named("intensity") = new_intensity
      );

      new_mz.erase(new_mz.begin(), new_mz.end());
      new_intensity.erase(new_intensity.begin(), new_intensity.end());
      hasFromSameScan = true;

    } else {
      list_out[i] = DataFrame::create();
    }
  }
  return(list_out);
}
