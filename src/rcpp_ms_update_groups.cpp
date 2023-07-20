#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame rcpp_ms_update_groups(Rcpp::DataFrame features, Rcpp::StringVector analyses) {

  DataFrame out_temp;

  StringVector features_cols = features.names();

  if (features.nrows() == 0 && features_cols.size() == 0) {
    Rcout << "!! Features data.frame is empty!";
    return(out_temp);
  }

  StringVector must_have_cols = {
    "feature", "analysis",
    "rt", "rtmax", "rtmin",
    "mz", "mzmax", "mzmin", "mass",
    "filled", "adduct", "intensity", "group"
  };

  for (int i=0; i<must_have_cols.size(); ++i) {
    auto str_position = std::find(features_cols.begin(), features_cols.end(), must_have_cols(i));

    if (str_position == features_cols.end()) {
      Rcout << "!! The column " << must_have_cols(i) << " not found in features!";
      return(out_temp);
    }
  }

  StringVector features_groups_ids = features["group"];
  StringVector groups_ids = sort_unique(features_groups_ids);
  LogicalVector is_na_val = is_na(groups_ids);
  groups_ids = groups_ids[!is_na_val];

  int number_of_groups = groups_ids.size();

  StringVector g_id;

  int number_of_features = features.nrows();

  StringVector features_ids = features["feature"];
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
  NumericVector features_ints = features["intensity"];

  StringVector polarities = unique(features_adduct);

  bool multiple_polarities;

  if (polarities.size() > 1) {
    multiple_polarities = true;
  } else {
    multiple_polarities = false;
  }

  analyses = sort_unique(analyses);
  int n_analyses = analyses.size();
  NumericMatrix ints(number_of_groups, n_analyses);
  Rcpp::colnames(ints) = analyses;

  NumericVector g_rt(number_of_groups);
  NumericVector g_rtmin(number_of_groups);
  NumericVector g_rtmax(number_of_groups);
  NumericVector g_rtdev(number_of_groups);
  NumericVector g_mz(number_of_groups);
  NumericVector g_mass(number_of_groups);
  NumericVector g_mass_left(number_of_groups);
  NumericVector g_mass_right(number_of_groups);
  NumericVector g_massdev(number_of_groups);
  LogicalVector g_filled(number_of_groups);
  StringVector g_adduct(number_of_groups);

  for (int i=0; i<groups_ids.size(); ++i) {
    g_id = groups_ids(i);

    IntegerVector which_idx;
    for (int z=0; z<number_of_features; ++z) {
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
    StringVector f_ids(which_idx_size);
    StringVector f_analyses(which_idx_size);
    NumericVector f_ints(which_idx_size);

    f_rt = features_rt[which_idx];
    f_rtmin = features_rtmin[which_idx];
    f_rtmax = features_rtmax[which_idx];
    f_mz = features_mz[which_idx];
    f_mass = features_mass[which_idx];
    f_mzmin = features_mzmin[which_idx];
    f_mzmax = features_mzmax[which_idx];
    f_filled = features_filled[which_idx];
    f_adduct = features_adduct[which_idx];
    f_analyses = features_analyses[which_idx];
    f_ints = features_ints[which_idx];

    g_rt[i] = mean(f_rt);
    g_rt[i] = std::round(g_rt[i] / 0.001) * 0.001;

    g_rtdev[i] = max(f_rtmax) - min(f_rtmin);
    g_rtdev[i] = std::round(g_rtdev[i] / 1) * 1;

    g_mz[i] = mean(f_mz);
    g_mz[i] = std::round(g_mz[i] / 0.00000001) * 0.00000001;

    g_mass[i] = mean(f_mass);
    g_mass[i] = std::round(g_mass[i] / 0.00000001) * 0.00000001;

    g_mass_left[i] = max((f_mz - f_mzmin) / f_mz * 1E6);
    g_mass_right[i] = max((f_mzmax - f_mz) / f_mz * 1E6);

    g_massdev[i] = g_mass_left[i] + g_mass_right[i];
    g_massdev[i] = std::round(g_massdev[i] / 0.01) * 0.01;

    g_filled[i] = is_true(any(f_filled));

    StringVector a_ion = unique(f_adduct);

    if (multiple_polarities) {
      g_adduct(i) = "[M]";
    } else {
      g_adduct(i) = a_ion(0);
    }

    for (int f=0; f<which_idx_size; ++f) {
      StringVector f_ana(1);
      f_ana(0) = f_analyses(f);

      for (int a=0; a<n_analyses; ++a) {
        if (analyses(a) == f_ana(0)) {
          ints(i, a) = f_ints[f];
        }
      }
    }
  }

  StringVector na_filter(number_of_groups);

  IntegerVector idx_mass;
  idx_mass = seq_along(g_mass) - 1;
  std::sort(idx_mass.begin(), idx_mass.end(), [&](int i, int j){return g_mass[i] < g_mass[j];});

  groups_ids = groups_ids[idx_mass];
  g_rt = g_rt[idx_mass];
  g_mz = g_mz[idx_mass];
  g_mass = g_mass[idx_mass];
  g_rtdev = g_rtdev[idx_mass];
  g_massdev = g_massdev[idx_mass];
  g_adduct = g_adduct[idx_mass];
  g_filled = g_filled[idx_mass];

  IntegerVector idx_rt;
  idx_rt = seq_along(g_rt) - 1;
  std::sort(idx_rt.begin(), idx_rt.end(), [&](int i, int j){return g_rt[i] < g_rt[j];});

  groups_ids = groups_ids[idx_rt];
  g_rt = g_rt[idx_rt];
  g_mz = g_mz[idx_rt];
  g_mass = g_mass[idx_rt];
  g_rtdev = g_rtdev[idx_rt];
  g_massdev = g_massdev[idx_rt];
  g_adduct = g_adduct[idx_rt];
  g_filled = g_filled[idx_rt];

  List list_groups = List::create(
    Named("group") = groups_ids,
    Named("rt") = g_rt
  );

  if (multiple_polarities) {
    list_groups["mass"] = g_mass;
  } else {
    list_groups["mz"] = g_mz;
  }

  for (int a=0; a<n_analyses; ++a) {
    NumericVector int_vals = ints( _ , a );
    int_vals = int_vals[idx_mass];
    int_vals = int_vals[idx_rt];
    std::vector< std::string > ana(1);
    ana[0] = analyses(a);
    list_groups.push_back(int_vals, ana[0]);
  }

  list_groups["rtdev"] = g_rtdev;
  list_groups["massdev"] = g_massdev;
  list_groups["adduct"] = g_adduct;

  if (!multiple_polarities) list_groups["mass"] = g_mass;

  list_groups["filled"] = g_filled;
  list_groups["filtered"] = rep(false, number_of_groups);

  list_groups.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  return(list_groups);
}
