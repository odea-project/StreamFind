#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame rcpp_ms_get_feature_groups_ranges(StringVector groups,
                                            Rcpp::DataFrame features) {

  StringVector groups_ids = groups;
  int number_of_groups = groups_ids.size();

  StringVector g_id;

  int number_of_features = features.nrows();

  StringVector features_groups_ids = features["group"];
  StringVector features_analyses = features["analysis"];
  NumericVector features_rt = features["rt"];
  NumericVector features_rtmax = features["rtmax"];
  NumericVector features_rtmin = features["rtmin"];
  NumericVector features_mass = features["mass"];
  NumericVector features_mz = features["mz"];
  NumericVector features_mzmax = features["mzmax"];
  NumericVector features_mzmin = features["mzmin"];
  LogicalVector features_filled = features["filled"];
  StringVector features_adduct = features["adduct"];


  NumericVector g_rt(number_of_groups);
  NumericVector g_rtmin(number_of_groups);
  NumericVector g_rtmax(number_of_groups);
  NumericVector g_mz(number_of_groups);
  NumericVector g_mass(number_of_groups);
  NumericVector g_mass_left(number_of_groups);
  NumericVector g_mass_right(number_of_groups);
  LogicalVector g_filled(number_of_groups);
  StringVector g_adduct(number_of_groups);



  for (int i=0; i<groups_ids.size(); ++i) {
    g_id = groups_ids(i);

    IntegerVector which_idx;
    for (int z=0; z<number_of_features; z++) {
      if (features_groups_ids(z) == g_id(0)) which_idx.push_back(z);
    }

    int which_idx_size = which_idx.size();

    NumericVector f_rt(which_idx_size);
    NumericVector f_rtmin(which_idx_size);
    NumericVector f_rtmax(which_idx_size);
    NumericVector f_mz(which_idx_size);
    NumericVector f_mass(which_idx_size);
    NumericVector f_mzmin(which_idx_size);
    NumericVector f_mzmax(which_idx_size);
    LogicalVector f_filled(which_idx_size);
    StringVector f_adduct(which_idx_size);

    f_rt = features_rt[which_idx];
    f_rtmin = features_rtmin[which_idx];
    f_rtmax = features_rtmax[which_idx];
    f_mz = features_mz[which_idx];
    f_mass = features_mass[which_idx];
    f_mzmin = features_mzmin[which_idx];
    f_mzmax = features_mzmax[which_idx];
    f_filled = features_filled[which_idx];
    f_adduct = features_adduct[which_idx];


    g_rt[i] = mean(f_rt);
    g_rt[i] = std::round(g_rt[i] / 0.001) * 0.001;

    g_rtmin[i] = min(f_rtmin);
    g_rtmax[i] = max(f_rtmax);

    g_mz[i] = mean(f_mz);
    g_mz[i] = std::round(g_mz[i] / 0.00000001) * 0.00000001;

    g_mass[i] = mean(f_mass);
    g_mass[i] = std::round(g_mass[i] / 0.00000001) * 0.00000001;

    g_mass_left[i] = max((f_mz - f_mzmin) / f_mz * 1E6);
    g_mass_right[i] = max((f_mzmax - f_mz) / f_mz * 1E6);

    g_filled[i] = is_true(any(f_filled));

    StringVector a_ion = unique(f_adduct);
    if (a_ion.size() > 1) {
      g_adduct(i) = "[M]";
    } else {
      g_adduct(i) = a_ion(0);
    }
  }

  DataFrame df_out = DataFrame::create(
    Named("group") = groups_ids,
    Named("rt") = g_rt,
    Named("rtmin") = g_rtmin,
    Named("rtmax") = g_rtmax,
    Named("mz") = g_mz,
    Named("mass") = g_mass,
    Named("mass_left") = g_mass_left,
    Named("mass_right") = g_mass_right,
    Named("filled") = g_filled,
    Named("adduct") = g_adduct
  );

  return(df_out);
}
