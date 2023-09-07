#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_ms_make_groups_update_features(Rcpp::DataFrame features) {

  List list_out_temp = List::create();

  StringVector features_cols = features.names();

  if (features.nrows() == 0 && features_cols.size() == 0) {
    Rcout << "!! Features data.frame is empty!";
    return(list_out_temp);
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
      return(list_out_temp);
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

  StringVector analyses = sort_unique(features_analyses);
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

  char dot = '.';
  char r_char = 'r';

  for (int i=0; i<groups_ids.size(); ++i) {
    g_id = groups_ids(i);

    IntegerVector which_idx;
    for (int z=0; z<number_of_features; ++z) {
      if (features_groups_ids(z) == g_id(0)) which_idx.push_back(z);
    }

    int which_idx_size = which_idx.size();
    
    if (which_idx_size > n_analyses) {
      Rcpp::Rcout << "\n !! The feature group " << g_id << " (n " << i << ")" 
      << " has more features than analyses! \n";
    }

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
    
    double g_rtmin_val = min(f_rtmin);
    double g_rtmax_val = max(f_rtmax);
    
    if (g_rt[i] > g_rtmax_val || g_rt[i] < g_rtmin_val) {
      Rcpp::Rcout << "\n !! The feature group " << g_id << " (n " << i << ")" 
      << " does not match retention time range in features!! \n";
    }

    g_rtdev[i] = g_rtmax_val - g_rtmin_val;
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

    NumericVector round_vals(which_idx_size);
    double round_val;

    for (int f=0; f<which_idx_size; ++f) {
      StringVector f_ana(1);
      f_ana(0) = f_analyses(f);

      for (int a = 0; a < n_analyses; ++a) {
        if (analyses(a) == f_ana(0)) {
          ints(i, a) = f_ints[f];
        }
      }

      int pos_dot = 0;
      int pos_rt = 0;

      for (int p=0; p<features_ids[f].size(); ++p) {
        char temp_f_c = features_ids[f][p];
        char temp_f_c_2 = features_ids[f][p];
        if (temp_f_c == dot) pos_dot = p;
        if (temp_f_c_2 == r_char) pos_rt = p - 2;
      }
      int d_dig = pos_rt - pos_dot;
      round_vals[f] = 1;

      for(int t=0; t<d_dig; t++) {
        round_vals[f] = round_vals[f]/10;
      }
    }
    round_val = min(round_vals);

    String g_rt_to_id = std::round(g_rt[i] / 1) * 1;

    if (multiple_polarities) {

      String g_mass_to_id = std::round(g_mass[i] / round_val) * round_val;

      groups_ids(i) = "m";
      groups_ids(i) += g_mass_to_id;
      groups_ids(i) += "_rt";
      groups_ids(i) += g_rt_to_id;
      groups_ids(i) += "_g";
      groups_ids(i) += i;

    } else {

      String g_mz_to_id = std::round(g_mz[i] / round_val) * round_val;

      groups_ids(i) = "mz";
      groups_ids(i) += g_mz_to_id;
      groups_ids(i) += "_rt";
      groups_ids(i) += g_rt_to_id;
      groups_ids(i) += "_g";
      groups_ids(i) += i;

    }

    for (int f = 0; f < which_idx_size; ++f) {
      int f_to_change = which_idx[f];
      features_groups_ids(f_to_change) = groups_ids(i);
    }
  }

  features["group"] = features_groups_ids;

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

  List list_out = List::create(
    Named("features") = features
  );

  List list_groups = List::create(
    Named("group") = groups_ids,
    Named("rt") = g_rt
  );

  if (multiple_polarities) {
    list_groups["mass"] = g_mass;
  } else {
    list_groups["mz"] = g_mz;
  }

  for (int a = 0; a < n_analyses; ++a) {
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

  Rcpp::CharacterVector filter_column(number_of_groups);

  for (int i = 0; i < number_of_groups; ++i) {
    filter_column[i] = Rcpp::CharacterVector::get_na();
  }

  list_groups["filter"] = filter_column;

  list_groups.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  list_out["groups"] = list_groups;

  return(list_out);
}
