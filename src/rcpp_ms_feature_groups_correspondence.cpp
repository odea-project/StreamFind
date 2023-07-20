#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
bool rcpp_ms_feature_groups_correspondence(Rcpp::DataFrame groups,
                                           Rcpp::DataFrame features,
                                           bool verbose) {

  StringVector groups_ids = groups["group"];
  NumericVector groups_rt_vals = groups["rt"];
  NumericVector groups_mass_vals = groups["mass"];

  int number_of_features = features.nrows();

  StringVector features_groups_ids = features["group"];
  StringVector features_analyses = features["analysis"];
  NumericVector features_rtmax_vals = features["rtmax"];
  NumericVector features_rtmin_vals = features["rtmin"];
  NumericVector features_mass_vals = features["mass"];
  NumericVector features_mz_vals = features["mz"];
  NumericVector features_mzmax_vals = features["mzmax"];
  NumericVector features_mzmin_vals = features["mzmin"];
  NumericVector features_intensity_vals = features["intensity"];

  StringVector g_id;
  double g_rt;
  double g_mass;

  NumericVector fg_rtmax_vals;
  NumericVector fg_rtmin_vals;
  NumericVector fg_mass_vals;
  NumericVector fg_mz_vals;
  NumericVector fg_mzmax_vals;
  NumericVector fg_mzmin_vals;

  double max_ppm;
  double min_ppm;
  double mean_mass;

  bool check_rt;
  bool check_mass;

  for (int i=0; i<groups_ids.size(); ++i) {
    g_id = groups_ids(i);

    IntegerVector which_idx;
    for (int z=0; z<number_of_features; z++) {
      if (features_groups_ids(z) == g_id(0)) which_idx.push_back(z);
    }

    g_rt = groups_rt_vals[i];
    fg_rtmax_vals = features_rtmax_vals[which_idx];
    fg_rtmin_vals = features_rtmin_vals[which_idx];
    double max_rt = max(fg_rtmax_vals);
    double min_rt = min(fg_rtmin_vals);
    check_rt = (max_rt >= g_rt) && (min_rt <= g_rt);

    if (!check_rt) {
      if (verbose) {
        Rcpp::Rcout << "\n !! The feature group " << g_id <<
        " does not match retention time range in features! \n";
      }
      return(false);
    }

    g_mass = groups_mass_vals[i];
    g_mass = std::round(g_mass / 0.0001) * 0.0001;
    fg_mass_vals = features_mass_vals[which_idx];
    fg_mz_vals = features_mz_vals[which_idx];
    fg_mzmax_vals = features_mzmax_vals[which_idx];
    fg_mzmin_vals = features_mzmin_vals[which_idx];
    fg_mzmax_vals = (fg_mzmax_vals - fg_mz_vals) / fg_mzmax_vals * 1E6;
    fg_mzmin_vals = (fg_mzmin_vals - fg_mz_vals) / fg_mzmin_vals * 1E6;
    max_ppm = max(fg_mzmax_vals);
    min_ppm = min(fg_mzmin_vals);
    mean_mass = mean(fg_mass_vals);
    double mass_min = (min_ppm / 1E6 *  mean_mass) +  mean_mass;
    double mass_max = (max_ppm / 1E6 *  mean_mass) +  mean_mass;
    mass_min = std::round(mass_min / 0.0001) * 0.0001;
    mass_max = std::round(mass_max / 0.0001) * 0.0001;

    check_mass = (mass_max >= g_mass) && (mass_min <= g_mass);

    if (!check_mass) {
      if (verbose) {
        Rcpp::Rcout << "\n !! The feature group " << g_id <<
        " does not match mass range in features! \n";
      }
      return(false);
    }

    StringVector fg_analyses;
    fg_analyses = features_analyses[which_idx];
    NumericVector fg_intensity_vals = features_intensity_vals[which_idx];

    for (int t=0; t<fg_analyses.size(); t++) {
      std::vector<std::string> ana(1);
      ana[0] = fg_analyses(t);
      NumericVector group_intensity = groups[ana[0]];
      double g_int = group_intensity[i];
      g_int = std::round(g_int / 1) * 1;
      double f_int = fg_intensity_vals[t];
      f_int = std::round(f_int / 1) * 1;
      if (g_int != f_int) {
        if (verbose) {
          Rcpp::Rcout << "\n !! The feature group " << g_id <<
          " does not match intensity in feature from analysis " << ana[0] << "! \n";
        }
        return(false);
      }
    }
  }

  return(true);
}
