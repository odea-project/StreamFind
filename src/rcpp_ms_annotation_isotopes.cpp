#include <iostream>
#include <vector>
#include <string>
#include <tuple>
#include <algorithm>
#include <unordered_set>
#include <numeric>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_ms_annotation_isotopes(Rcpp::DataFrame features,
                                       int maxIsotopes = 5,
                                       int maxCharge = 1,
                                       double rtWindowAlignment = 0.2,
                                       int maxGaps = 1) {

  Rcpp::List list_out;

  int number_of_features = features.nrows();

  // Order Input data by mz //////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<std::string> all_ids_unsorted = features["feature"];
  std::vector<int> all_idx_unsorted = features["index"];
  std::vector<double> all_rt_unsorted = features["rt"];
  std::vector<double> all_rtmin_unsorted = features["rtmin"];
  std::vector<double> all_rtmax_unsorted = features["rtmax"];
  std::vector<double> all_mz_unsorted = features["mz"];
  std::vector<double> all_mzmin_unsorted = features["mzmin"];
  std::vector<double> all_mzmax_unsorted = features["mzmax"];
  std::vector<double> all_intensity_unsorted = features["intensity"];
  std::vector<double> all_sn_unsorted = features["sn"];

  const std::string* all_ids_unsorted_ptr = all_ids_unsorted.data();
  const int* all_idx_unsorted_ptr = all_idx_unsorted.data();
  const double* all_rt_unsorted_ptr = all_rt_unsorted.data();
  const double* all_mz_unsorted_ptr = all_mz_unsorted.data();
  const double* all_mzmin_unsorted_ptr = all_mzmin_unsorted.data();
  const double* all_rtmin_unsorted_ptr = all_rtmin_unsorted.data();
  const double* all_rtmax_unsorted_ptr = all_rtmax_unsorted.data();
  const double* all_mzmax_unsorted_ptr = all_mzmax_unsorted.data();
  const double* all_intensity_unsorted_ptr = all_intensity_unsorted.data();
  const double* all_sn_unsorted_ptr = all_sn_unsorted.data();

  std::vector<int> sort_features_idx(number_of_features);
  std::iota(sort_features_idx.begin(), sort_features_idx.end(), 0);
  std::sort(sort_features_idx.begin(), sort_features_idx.end(),
    [&](int i, int j){return all_mz_unsorted[i] < all_mz_unsorted[j];});

  std::vector<std::string> all_ids(number_of_features);
  std::vector<int> all_idx(number_of_features);
  std::vector<double> all_rt(number_of_features);
  std::vector<double> all_rtmin(number_of_features);
  std::vector<double> all_rtmax(number_of_features);
  std::vector<double> all_mz(number_of_features);
  std::vector<double> all_mzmin(number_of_features);
  std::vector<double> all_mzmax(number_of_features);
  std::vector<double> all_intensity(number_of_features);
  std::vector<double> all_sn(number_of_features);

  std::string* all_ids_ptr = all_ids.data();
  int* all_idx_ptr = all_idx.data();
  double* all_rt_ptr = all_rt.data();
  double* all_mz_ptr = all_mz.data();
  double* all_mzmin_ptr = all_mzmin.data();
  double* all_rtmin_ptr = all_rtmin.data();
  double* all_rtmax_ptr = all_rtmax.data();
  double* all_mzmax_ptr = all_mzmax.data();
  double* all_intensity_ptr = all_intensity.data();
  double* all_sn_ptr = all_sn.data();

  for (const int& x : sort_features_idx) {
    *(all_ids_ptr++) = *(all_ids_unsorted_ptr + x);
    *(all_idx_ptr++) = *(all_idx_unsorted_ptr + x);
    *(all_rt_ptr++) = *(all_rt_unsorted_ptr + x);
    *(all_mz_ptr++) = *(all_mz_unsorted_ptr + x);
    *(all_mzmin_ptr++) = *(all_mzmin_unsorted_ptr + x);
    *(all_rtmin_ptr++) = *(all_rtmin_unsorted_ptr + x);
    *(all_rtmax_ptr++) = *(all_rtmax_unsorted_ptr + x);
    *(all_mzmax_ptr++) = *(all_mzmax_unsorted_ptr + x);
    *(all_intensity_ptr++) = *(all_intensity_unsorted_ptr + x);
    *(all_sn_ptr++) = *(all_sn_unsorted_ptr + x);
  }

  // Rcpp::DataFrame sorted_features_df = Rcpp::DataFrame::create(
  //   Rcpp::Named("index") = all_idx,
  //   Rcpp::Named("feature") = all_ids,
  //   Rcpp::Named("mz") = all_mz,
  //   Rcpp::Named("rt") = all_rt
  // );
  //
  // list_out["sorted_features"] = sorted_features_df;





  // Mass accuracy of input data /////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<double> all_mzr(all_mzmax.size());

  std::transform(
    all_mzmax.begin(), all_mzmax.end(),
    all_mzmin.begin(), all_mzr.begin(),
    std::minus<double>()
  );

  double mzr_max = *std::max_element(all_mzr.begin(), all_mzr.end());
  double mzr_min = *std::min_element(all_mzr.begin(), all_mzr.end());
  double mzr_mean = std::accumulate(all_mzr.begin(), all_mzr.end(), 0.0) / all_mzr.size();

  Rcpp::Rcout << "Mass deviation: " << std::endl;
  Rcpp::Rcout << "Max: " << mzr_max << std::endl;
  Rcpp::Rcout << "Min: " << mzr_min << std::endl;
  Rcpp::Rcout << "Mean: " << mzr_mean << std::endl;

  // calculates the number of decimal numbers
  // according to the mean mass deviation,
  // which is dependent on the mass resolution
  std::string mzr_str = std::to_string(mzr_mean);
  size_t decimal_pos = mzr_str.find(".");
  int decimal_numbers = 1;
  if (decimal_pos != std::string::npos) {
    for (size_t d = decimal_pos + 1; d < mzr_str.length(); d++) {
      if (mzr_str[d] == '0') {
        decimal_numbers++;
      } else {
        break;
      }
    }
  }

  Rcpp::Rcout << std::endl;
  Rcpp::Rcout << "Decimal numbers: " << decimal_numbers << std::endl;
  Rcpp::Rcout << std::endl;





  // Make mass diff combinations //////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<std::string> iso_elements = {
    "C13",
    "H2",
    "N15",
    "O17",
    "O18", //
    "S33",
    "S34",
    // "S36",
    "Cl37",
    "Br81"
    // "Si29",
    // "Si30"
  };

  // iso mass difference from monoisotopic ion
  std::vector<double> iso_md = {
    1.0033548378, // C13
    1.0062767, // H2
    0.9970349, // N15
    1.0042169, // O17
    2.004246, //O18
    0.9993878, // S33
    1.995796, // S34
    // 3.99501, // S36
    1.9970499, // Cl37
    1.9979534 // Br81
    // 0.99956819, // Si29
    // 1.99684369 // Si30
  };

  // iso relative abundance from monoisotopic ion
  std::vector<double> iso_ab = {
    0.0107800, // C13
    0.00015574, // H2
    0.00366300, // N15
    0.00037200, // O17
    0.00200040, // O18
    0.00750000, // S33
    0.04215000, // S34
    // 0.00017000, // S36
    0.24229000, // Cl37
    0.49314000 // Br81
    // 0.0468316, // Si29
    // 0.0308716 // Si30
  };

  // iso relative abundance from monoisotopic ion
  std::vector<double> iso_mono = {
    0.988922, // C
    0.99984426, // H
    0.996337, // N
    0.997628, // O
    0.997628, // O
    0.95018, // S
    0.95018, // S
    // 0.95018, // S
    0.75771, // Cl
    0.50686 // Br
    // 0.9222968, // Si
    // 0.9222968// Si
  };

  // iso minimum number of elements
  std::vector<double> iso_min_el = {
    2, // C
    2, // H
    1, // N
    1, // O
    1, // O
    1, // S
    1, // S
    // 0.00017000, // S
    1, // Cl
    1 // Br
    // 1, // Si
    // 1// Si
  };

  // iso maximum number of elements
  std::vector<double> iso_max_el = {
    80, // C
    80, // H
    15, // N
    15, // O
    15, // O
    10, // S
    10, // S
    // 0.00017000, // S
    10, // Cl
    10 // Br
    // 5, // Si
    // 5// Si
  };

  int max_number_elements = 3;

  std::set<std::vector<std::string>> combinations;
  for (const std::string& el : iso_elements) {
    std::vector<std::string> el_v(1);
    el_v[0] = el;
    combinations.insert(el_v);
  }

  for (int n = 1; n <= max_number_elements; n++) {
    std::vector<std::vector<std::string>> combinations_vec(
        combinations.begin(),
        combinations.end()
    );

    std::set<std::vector<std::string>> new_combinations;

    for (const std::vector<std::string>& prev_combination : combinations_vec) {

      // excludes H2 and O17 from the M+2 on due to the low contribution
      if (prev_combination[0] == "H2" || prev_combination[0] == "O17") {
        continue;
      }

      // excludes N15 and S33 from the M+3 on due to the low contribution
      if (n > 1 &&  (prev_combination[0] == "N15" || prev_combination[0] == "S33")) {
        continue;
      }

      // excludes N15 and S33 from the M+3 on due to the low contribution
      if (prev_combination.size() >= 2) {
        if (prev_combination[1] == "N15" || prev_combination[1] == "S33") {
          continue;
        }
      }

      for (const std::string& el : iso_elements) {

        // excludes H2 and O17 from the M+2 on due to the low contribution
        if (el == "H2" || el == "O17") continue;

        // excludes N15 and S33 from the M+3 on due to the low contribution
        if (n > 1 &&  (el == "N15" || el == "S33")) continue;

        std::vector<std::string> combination = prev_combination;
        combination.push_back(el);
        std::sort(combination.begin(), combination.end());
        new_combinations.insert(combination);
      }
    }
    combinations.insert(new_combinations.begin(), new_combinations.end());
  }

  std::vector<std::vector<std::string>> el_comb_unordered(
    combinations.begin(),
    combinations.end()
  );

  Rcpp::NumericVector el_md_key = Rcpp::wrap(iso_md);
  el_md_key.names() = iso_elements;

  Rcpp::NumericVector el_ab_key = Rcpp::wrap(iso_ab);
  el_ab_key.names() = iso_elements;

  std::vector<std::vector<double>> md_comb_unordered(el_comb_unordered.size());
  std::vector<std::vector<double>> ab_comb_unordered(el_comb_unordered.size());

  std::vector<double> iso_sum_md_unordered(el_comb_unordered.size());

  for (size_t i = 0; i < el_comb_unordered.size(); ++i) {
    const std::vector<std::string>& el_v = el_comb_unordered[i];
    std::vector<double> md_v(el_v.size());
    std::vector<double> ab_v(el_v.size());
    for(size_t j = 0; j < el_v.size(); ++j) {
      std::string el = el_v[j];
      md_v[j] = el_md_key[el];
      ab_v[j] = el_ab_key[el];
      iso_sum_md_unordered[i] = iso_sum_md_unordered[i] + el_md_key[el];
    }
    md_comb_unordered[i] = md_v;
    ab_comb_unordered[i] = ab_v;
  }

  std::vector<int> iso_el_idx(iso_sum_md_unordered.size());
  std::iota(iso_el_idx.begin(), iso_el_idx.end(), 0);
  std::sort(iso_el_idx.begin(), iso_el_idx.end(),
    [&](int i, int j){return iso_sum_md_unordered[i] < iso_sum_md_unordered[j];});

  const double* iso_sum_md_unordered_ptr = iso_sum_md_unordered.data();

  std::vector<std::vector<std::string>> el_combinations(iso_sum_md_unordered.size());
  std::vector<std::vector<double>> el_md_combinations(iso_sum_md_unordered.size());
  std::vector<std::vector<double>> el_ab_combinations(iso_sum_md_unordered.size());
  std::vector<double> iso_sum_md(iso_sum_md_unordered.size());
  double* iso_sum_md_ptr = iso_sum_md.data();

  int counter = 0;
  for (const int& x : iso_el_idx) {
    *(iso_sum_md_ptr++) = *(iso_sum_md_unordered_ptr + x);
    el_combinations[counter] =  el_comb_unordered[x];
    el_md_combinations[counter] =  md_comb_unordered[x];
    el_ab_combinations[counter] =  ab_comb_unordered[x];
    counter++;
  }

  Rcpp::List el_combinations_list;
  Rcpp::List el_md_combinations_list;
  Rcpp::List el_ab_combinations_list;
  for (const std::vector<std::string>& x : el_combinations) el_combinations_list.push_back(x);
  for (const std::vector<double>& x : el_md_combinations) el_md_combinations_list.push_back(x);
  for (const std::vector<double>& x : el_ab_combinations) el_ab_combinations_list.push_back(x);

  list_out["el_md_key"] = el_md_key;
  list_out["el_ab_key"] = el_ab_key;
  list_out["el_combinations"] = el_combinations_list;
  list_out["el_md_combinations"] = el_md_combinations_list;
  list_out["el_ab_combinations"] = el_ab_combinations_list;
  list_out["iso_sum_md"] = iso_sum_md;





  // merging based on resolution or mass deviation ///////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////


  // double multiplier = std::pow(10.0, decimal_numbers);

  std::vector<double> iso_sum_md_rounded(iso_sum_md.size());

  for (size_t i = 0; i < iso_sum_md.size(); i++) {
    iso_sum_md_rounded[i] = iso_sum_md[i];
    // iso_sum_md_rounded[i] = std::round(iso_sum_md[i] * multiplier) / multiplier;
  }

  // does not remove duplicates, consequently the first hit is the one stored
  // as following duplicates will give the same error
  // std::sort(iso_sum_md_rounded.begin(), iso_sum_md_rounded.end());
  // auto last = std::unique(iso_sum_md_rounded.begin(), iso_sum_md_rounded.end());
  // iso_sum_md_rounded.erase(last, iso_sum_md_rounded.end());

  Rcpp::IntegerVector iso_md_step(iso_sum_md_rounded.size());
  for (size_t i = 0; i < iso_sum_md_rounded.size(); i++) {
    iso_md_step[i] = std::round(iso_sum_md_rounded[i] * 1) / 1;
  }

  list_out["iso_sum_md_rounded"] = iso_sum_md_rounded;
  list_out["iso_md_step"] = iso_md_step;






  // Charges vector //////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<int> zvals(maxCharge);
  std::iota(zvals.begin(), zvals.end(), 1);

  // Output data.frame preparation ///////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  int iso_gr = 0;
  std::vector<int> feat_iso_gr(number_of_features);
  std::vector<int> feat_iso_z(number_of_features);
  std::vector<std::string> feat_iso_cat(number_of_features);
  std::vector<int> feat_iso_step(number_of_features);
  std::vector<double> feat_iso_int(number_of_features);
  std::vector<double> feat_iso_diff(number_of_features);
  std::vector<double> feat_iso_md(number_of_features);
  std::vector<double> feat_iso_error(number_of_features);
  std::vector<std::string> feat_iso_feat(number_of_features);
  Rcpp::CharacterVector feat_iso_el(number_of_features);

  std::vector<double> feat_estimated_carbons(number_of_features);
  int final_estimated_number_carbons;




  // Main loop ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  //mz259.085_rt1081_f143
  // for (int i = 122; i < 123; ++i) {

  // mz223.064_rt1174_f248, charge two not right 1.00066 in the iso 1
  // possibly highly affected by Nitegen or Oxygen
  //  for (int i = 34; i < 35; ++i) {

  // mz273.167_rt945_f35, not assigned with c13
  // for (int i = 159; i < 160; ++i) {

  // mz276.079_rt1164_f230, chain without C13 but iso 2
  // for (int i = 167; i < 168; ++i) {

  // mz213.147_rt1112_f177, single feature with chain length 0
  // for (int i = 19; i < 20; ++i) {

  // mz233.025_rt1161_f224, Diuron
  // for (int i = 55; i < 56; ++i) {

  // mz245.161_rt1121_f182
  // for (int i = 91; i < 92; ++i) {

  // mz246.169_rt1122_f184, which is M+ of mz247.177_rt1121_f183 with mass diff of 0.008 Da
  // for (int i = 93; i < 94; ++i) {

  // mz247.177_rt1121_f183
  // for (int i = 96; i < 97; ++i) {

  // Carbamazepin-d10
  // for (int i = 95; i < 96; ++i) {

  // Diuron-d6
  // for (int i = 75; i < 76; ++i) {

  // #### Orbitrap data from AFIN-TS

  // mz254.059_rt712_f2111, Sulfamethoxazole
  // for (int i = 4686; i < 4687; ++i) {

  // mz192.138_rt1071_f9041, DEET
  // for (int i = 2652; i < 2653; ++i) {

  // mz249.019_rt1133_f12642, Linuron
  // for (int i = 4490; i < 4491; ++i) {

  // mz388.105_rt1244_f15264, Pyraclostrobin
  // for (int i = 9778; i < 9779; ++i) {

  // mz399.25_rt1276_f15896, highest feature
  // for (int i = 10074; i < 10075; ++i) {

  // #### Orbitrap data from AFIN-TS High Resolution

  // highest feature
  // for (int i = 104; i < 105; ++i) {


  for (int i = 0; i < number_of_features; ++i) {

    std::string id = all_ids[i];
    double mz = all_mz[i];
    double rt = all_rt[i];
    double rtmin = all_rtmin[i];
    double rtmax = all_rtmax[i];
    double intensity = all_intensity[i];

    bool is_Mplus = false;

    // Rcpp::Rcout << "Feature: " << id << std::endl;
    // Rcpp::Rcout << "Intensity: " << intensity << std::endl;





    // not yet in an iso group, create chain of features
    if (feat_iso_gr[i] == 0) {

      // Build Chain /////////////////////////////////////////////////////////////////////////////////
      ////////////////////////////////////////////////////////////////////////////////////////////////

      double left_rt = rt - rtmin;
      double right_rt = rtmax - rt;

      double rtW;
      if (left_rt < right_rt) {
        rtW = left_rt;
      } else {
        rtW = right_rt;
      }

      rtW = rtW * rtWindowAlignment;

      rtmin = rt - rtW;
      rtmax = rt + rtW;

      double maxisomz = (mz + maxIsotopes) * 1.05;

      std::vector<int> which_fts;
      for (int z = 0; z < number_of_features; ++z) {
        if (all_rt[z] >= rtmin && all_rt[z] <= rtmax && all_mz[z] >= mz && all_mz[z] <= maxisomz) {
          which_fts.push_back(z);
        }
      }

      int number_chain_features = which_fts.size();
      // Rcpp::Rcout << "Number of features in chain: " << number_chain_features << std::endl;

      if (number_chain_features > 1) {
        double mzmin = all_mzmin[i];
        double mzmax = all_mzmax[i];
        double mzr_i = mzmax - mzmin;

        const double* all_mz_ptr = all_mz.data();

        std::vector<double> org_mz(number_chain_features);
        double* org_mz_ptr = org_mz.data();

        for (const int& x : which_fts) {
          *(org_mz_ptr++) = *(all_mz_ptr + x);
        }

        std::vector<int> idx(org_mz.size());
        std::iota(idx.begin(), idx.end(), 0);
        std::sort(idx.begin(), idx.end(), [&](int i, int j){return org_mz[i] < org_mz[j];});

        const double* org_mz_ptr2 = org_mz.data();

        std::vector<double> chain_mz(number_chain_features);
        double* chain_mz_ptr = chain_mz.data();

        const int* which_fts_ptr = which_fts.data();

        std::vector<int> chain_idx(number_chain_features);
        int* chain_idx_ptr = chain_idx.data();

        std::vector<std::string> chain_id(number_chain_features);
        std::string* chain_id_ptr = chain_id.data();

        std::vector<double> chain_rt(number_chain_features);
        double* chain_rt_ptr = chain_rt.data();

        std::vector<double> chain_intensity(number_chain_features);
        double* chain_intensity_ptr = chain_intensity.data();

        for (const int& x : idx) {
          *(chain_mz_ptr++) = *(org_mz_ptr2 + x);
          *(chain_idx_ptr++) = *(which_fts_ptr + x);
          *(chain_id_ptr++) =  all_ids[*(which_fts_ptr + x)];
          *(chain_intensity_ptr++) =  all_intensity[*(which_fts_ptr + x)];
          *(chain_rt_ptr++) =  all_rt[*(which_fts_ptr + x)];
        }

        // TODO perhaps here the chain could be filter by EIC correlation

        Rcpp::Rcout << std::endl;
        Rcpp::Rcout << "Candidates in chain: " << std::endl;
        for (size_t z = 0; z < chain_mz.size(); ++z) {
          Rcpp::Rcout << chain_idx[z] << "   " <<
            chain_mz[z] << "   " <<
            chain_mz[z] - mz << "   " <<
            chain_intensity[z] << "   " <<
            chain_intensity[z]/intensity << "   " <<
            chain_rt[z] << "   " <<
            chain_rt[z] - rt <<  std::endl;
        }
        Rcpp::Rcout <<  std::endl;
        Rcpp::Rcout <<  std::endl;

        // Chain evaluation ////////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////////////////////////////

        Rcpp::List hits_mz_list(maxCharge);
        Rcpp::List hits_step_list(maxCharge);
        Rcpp::List hits_idx_list(maxCharge);
        Rcpp::List hits_ints_list(maxCharge);
        Rcpp::List hits_diff_list(maxCharge);
        Rcpp::List hits_md_list(maxCharge);
        Rcpp::List hits_error_list(maxCharge);
        Rcpp::List hits_el_list(maxCharge);
        Rcpp::List hits_ab_list(maxCharge);

        std::vector<double> estimated_number_carbons(zvals.size());

        for (size_t z = 0; z < zvals.size(); z++) {
          // Rcpp::Rcout << std::endl;
          // Rcpp::Rcout << "Charge: " << zvals[z] << std::endl;
          // Rcpp::Rcout << std::endl;

          Rcpp::List hits_iso_mz(maxIsotopes + 1);
          Rcpp::NumericVector mz_monoiso;
          mz_monoiso.push_back(mz);
          hits_iso_mz[0] = mz_monoiso;

          Rcpp::List hits_iso_step(maxIsotopes + 1);
          Rcpp::NumericVector step_monoiso;
          step_monoiso.push_back(0);
          hits_iso_step[0] = step_monoiso;

          Rcpp::List hits_iso_idx(maxIsotopes + 1);
          Rcpp::IntegerVector idx_monoiso;
          idx_monoiso.push_back(chain_idx[0]);
          hits_iso_idx[0] = idx_monoiso;

          Rcpp::List hits_iso_ints(maxIsotopes + 1);
          Rcpp::NumericVector int_monoiso;
          int_monoiso.push_back(1);
          hits_iso_ints[0] = int_monoiso;

          Rcpp::List hits_iso_diff(maxIsotopes + 1);
          Rcpp::NumericVector diff_monoiso;
          diff_monoiso.push_back(0);
          hits_iso_diff[0] = diff_monoiso;

          Rcpp::List hits_iso_md(maxIsotopes + 1);
          Rcpp::NumericVector md_monoiso;
          md_monoiso.push_back(0);
          hits_iso_md[0] = md_monoiso;

          Rcpp::List hits_iso_error(maxIsotopes + 1);
          Rcpp::NumericVector error_monoiso;
          error_monoiso.push_back(0);
          hits_iso_error[0] = error_monoiso;

          Rcpp::List hits_iso_el(maxIsotopes + 1);
          Rcpp::CharacterVector el_monoiso;
          el_monoiso.push_back("");
          hits_iso_el[0] = el_monoiso;

          Rcpp::List hits_iso_ab(maxIsotopes + 1);
          Rcpp::NumericVector ab_monoiso;
          ab_monoiso.push_back(1);
          hits_iso_ab[0] = ab_monoiso;

          // fill with 0s the iso output for the max isotopes
          Rcpp::NumericVector empty_vector;
          empty_vector.push_back(0);

          Rcpp::IntegerVector int_empty_vector;
          int_empty_vector.push_back(nan(""));

          Rcpp::CharacterVector char_empty_vector;
          char_empty_vector.push_back("");

          for (int iso = 1; iso < maxIsotopes + 1; ++iso) {
            hits_iso_mz[iso] = empty_vector;
            hits_iso_step[iso] = empty_vector;
            hits_iso_idx[iso] = empty_vector;
            hits_iso_ints[iso] = empty_vector;
            hits_iso_diff[iso] = empty_vector;
            hits_iso_md[iso] = empty_vector;
            hits_iso_error[iso] = empty_vector;
            hits_iso_el[iso] = char_empty_vector;
            hits_iso_ab[iso] = empty_vector;
          }

          // list_out["hits_iso_md_empty"] = hits_iso_md;
          // list_out["hits_iso_el_empty"] = hits_iso_el;
          // list_out["hits_iso_ab_empty"] = hits_iso_ab;

          estimated_number_carbons[z] = 0;

          for (int iso = 1; iso < maxIsotopes + 1; ++iso) {
            Rcpp::Rcout << "step " << iso << std::endl;
            Rcpp::Rcout << std::endl;

            // If iso above maximum gaps, only continues chain if at
            // least a hit was found to step n-1 and n-2
            if (iso > maxGaps) {
              Rcpp::IntegerVector gaps;
              for (int g = 1; g <= maxGaps + 1; ++g) {
                const Rcpp::NumericVector& temp_hits_iso_mz = hits_iso_mz[iso - g];
                bool is_gap = temp_hits_iso_mz[0] == 0;
                if (is_gap) gaps.push_back(1);
              }

              if (gaps.size() > maxGaps) break;
            }

            // filter possible isotopes for the step and
            // calculate md according to charge
            Rcpp::LogicalVector iso_step_filter = iso_md_step == iso;
            Rcpp::NumericVector temp_iso_md = Rcpp::wrap(iso_sum_md_rounded);
            Rcpp::IntegerVector temp_iso_idx(temp_iso_md.size());
            std::iota(temp_iso_idx.begin(), temp_iso_idx.end(), 0);
            temp_iso_md = temp_iso_md[iso_step_filter];
            temp_iso_idx = temp_iso_idx[iso_step_filter];
            temp_iso_md = temp_iso_md / zvals[z];

            double temp_iso_md_max = *std::max_element(temp_iso_md.begin(), temp_iso_md.end());
            double temp_iso_md_min = *std::min_element(temp_iso_md.begin(), temp_iso_md.end());

            Rcpp::NumericVector hits;
            Rcpp::NumericVector step;
            Rcpp::IntegerVector idx;
            Rcpp::NumericVector ints;
            Rcpp::NumericVector diff;
            Rcpp::NumericVector md;
            Rcpp::NumericVector error;
            Rcpp::CharacterVector el;

            for (int f = 1; f < number_chain_features; ++f) {
              // Rcpp::Rcout << "Feature: " << all_ids[chain_idx[f]] << std::endl;
              // Rcpp::Rcout << std::endl;

              double mzmin_f = all_mzmin[chain_idx[f]];
              double mzmax_f = all_mzmax[chain_idx[f]];
              double mzr_f = mzmax_f - mzmin_f;

              // updates the mass accuracy for the highest overall:
              // mean then feature and then candidate
              double mzr = mzr_mean;
              if (mzr_mean < mzr_i) {
                mzr = mzr_i;
              } else if (mzr_i < mzr_f) {
                mzr = mzr_f;
              };

              double candidate_diff = chain_mz[f] - mz;
              double candidate_diff_min = candidate_diff - mzr;
              double candidate_diff_max = candidate_diff + mzr;

              // Check for molecular ion (M+) with distance 1.007276
              // and much higher intensity, for now set to x5
              if (iso == 1) {
                if (candidate_diff_min < 1.007276 &&
                    candidate_diff_max > 1.007276 &&
                    all_intensity[chain_idx[f]]/intensity > 5) {
                  Rcpp::Rcout <<  std::endl;
                  Rcpp::Rcout << "Potential M+: " << id << std::endl;
                  Rcpp::Rcout << "  - diff: " << candidate_diff << std::endl;
                  Rcpp::Rcout << "  - max diff: " << candidate_diff_max << std::endl;
                  Rcpp::Rcout << "  - min diff: " << candidate_diff_min << std::endl;
                  Rcpp::Rcout << "  - [M+H]+: " << chain_id[f] << std::endl;
                  Rcpp::Rcout << "  - int ratio: " << all_intensity[chain_idx[f]]/intensity << std::endl;
                  Rcpp::Rcout <<  std::endl;

                  // TODO this will also capture H2 loss and mark it as M+
                  // the mass error might give an indication to check

                  feat_iso_cat[i] = "M+";
                  feat_iso_diff[i] = candidate_diff;
                  feat_iso_int[i] = all_intensity[chain_idx[f]]/intensity;
                  feat_iso_feat[i] = chain_id[f];

                  is_Mplus = true;
                  break;
                }
              }

              bool is_iso_candidate = false;
              double mass_error = 10; // is updated on the first hit
              double candidate_iso_md;
              Rcpp::CharacterVector el_temp;
              Rcpp::NumericVector ab_temp;

              // when candidate is inside of the mass range for iso step
              if (temp_iso_md_min - mzr < candidate_diff && temp_iso_md_max + mzr > candidate_diff) {
                Rcpp::Rcout << "Feature: " << all_ids[chain_idx[f]] << " with mass_diff " << candidate_diff << std::endl;
                Rcpp::Rcout << std::endl;

                // selects the md within the mass dev
                // when duplicated md, the first hit is the one stored
                for (int j = 0; j < temp_iso_md.size(); j++) {

                  double candidate_md_error = abs(temp_iso_md[j] - candidate_diff);

                  int candidate_idx = temp_iso_idx[j];
                  Rcpp::CharacterVector candidate_el = el_combinations_list[candidate_idx];
                  Rcpp::NumericVector candidate_ab = el_ab_combinations_list[candidate_idx];

                  double min_rel_int = 1;
                  double max_rel_int = 1;

                  std::unordered_map<std::string, int> count_map;

                  for (int e = 0; e < candidate_el.size(); ++e) {
                    std::string e_el = Rcpp::as<std::string>(candidate_el[e]);
                    count_map[e_el]++;
                  }

                  for (const auto& pair : count_map) {
                    std::string e_el = pair.first;
                    int number_el = pair.second;

                    Rcpp::Rcout << "El: " << e_el <<  "(" << temp_iso_md[j] << ")" << " with error " << candidate_md_error <<  std::endl;

                    double e_ab = 0;
                    double mono_ab = 0;
                    int min_el_num;
                    int max_el_num;
                    for (size_t a = 0; a <= iso_elements.size(); ++a) {
                      if (iso_elements[a] == e_el) {
                        e_ab = iso_ab[a];
                        mono_ab = iso_mono[a];

                        if (number_el == 1 && e_el == "C13" && iso == 1) {
                          estimated_number_carbons[z] = all_intensity[chain_idx[f]]/(e_ab * intensity);

                          Rcpp::Rcout << std::endl;
                          Rcpp::Rcout << "Estimated N. carbons:" << estimated_number_carbons[z] << std::endl;
                          Rcpp::Rcout << std::endl;
                        }

                        if (estimated_number_carbons[z] > 0 && e_el == "C13") {
                          min_el_num = estimated_number_carbons[z] * 0.9;
                          max_el_num = estimated_number_carbons[z] * 1.1;
                        } else {
                          min_el_num = iso_min_el[a];
                          max_el_num = iso_max_el[a];
                        }
                      }
                    }

                    if (iso == 4) {
                      Rcpp::Rcout << pair.first << ": " << number_el << " with ab:" << e_ab << std::endl;
                      Rcpp::Rcout << "mono_iso ab" << ": " << mono_ab << std::endl;
                      Rcpp::Rcout << "min number el" << ": " << min_el_num << std::endl;
                      Rcpp::Rcout << "max number el" << ": " << max_el_num << std::endl;
                    }



                    // when only one isotope atom, mostly for M+1
                    // or M+2 for Cl, Br, Si and S
                    if (number_el == 1) {
                      double min_coef = (min_el_num * std::pow(mono_ab, min_el_num - number_el) * e_ab) / std::pow(mono_ab, min_el_num);
                      double max_coef = (max_el_num * std::pow(mono_ab, max_el_num - number_el) * e_ab) / std::pow(mono_ab, max_el_num);

                      min_rel_int = min_rel_int * min_coef;
                      max_rel_int = max_rel_int * max_coef;

                    // when second time isotope, mostly for M+2 ...
                    } else {

                      unsigned int fact = 1;
                      for (int a = 1; a <= number_el; ++a) {
                        fact *= a;
                      }

                      if (iso == 4) Rcpp::Rcout << "factorial" << ": " << fact << std::endl;

                      double min_coef = (std::pow(mono_ab, min_el_num - number_el) * std::pow(e_ab, number_el)) / fact;
                      double max_coef = (std::pow(mono_ab, max_el_num - number_el) * std::pow(e_ab, number_el)) / fact;

                      min_coef = min_coef / std::pow(mono_ab, min_el_num);
                      max_coef = max_coef / std::pow(mono_ab, max_el_num);

                      min_coef = min_coef * min_el_num * (min_el_num - 1);
                      max_coef = max_coef * max_el_num * (max_el_num - 1);

                      for (int t = 2; t <= number_el - 1; ++t) {
                        if (iso == 4) Rcpp::Rcout << "run extra multiplication" << std::endl;
                        min_coef = min_coef * (min_el_num - t);
                        max_coef = max_coef * (max_el_num - t);
                      }



                      min_rel_int = min_rel_int * min_coef;
                      max_rel_int = max_rel_int * max_coef;
                    }
                  }






                  // if (candidate_ab.size() == 1) {
                  //
                  //   std::string e_el = Rcpp::as<std::string>(candidate_el[0]);
                  //
                  //   if ((e_el == "C13") || (e_el == "H2")) {
                  //     max_el_num = 80;
                  //   } else if (e_el == "Cl37" || (e_el == "Br81")) {
                  //     max_el_num = 5;
                  //   }
                  //
                  //   double mono_ab = 0;
                  //   for (int a = 0; a <  iso_elements.size(); ++a) {
                  //     if (iso_elements[a] == e_el) mono_ab = iso_mono[a];
                  //   }
                  //
                  //
                  //
                  // } else {
                  //
                  //   // to calculate the M+2 with more than one atom the elements
                  //   // first we extract unique el, then count their presence and continue
                  //   // with the estimation based on the expansion (book page 499) for
                  //   // repeating elements or direct multiplication for single el
                  //   Rcpp::CharacterVector candidate_el_unique = Rcpp::unique(candidate_el);
                  //
                  //   // std::unordered_map<std::string, int> count_map;
                  //   //
                  //   // for (int e = 0; e < candidate_el.size(); ++e) {
                  //   //   std::string e_el = Rcpp::as<std::string>(candidate_el[e]);
                  //   //   count_map[e_el]++;
                  //   // }
                  //
                  //   // Rcpp::Rcout << "Map Contents:" << std::endl;
                  //   // for (const auto& pair : count_map) {
                  //   //   Rcpp::Rcout << pair.first << ": " << pair.second << std::endl;
                  //   // }
                  //
                  //
                  //
                  //
                  //
                  //   for (int e = 0; e < candidate_ab.size(); ++e) {
                  //
                  //     if ((candidate_el[e] == "C13") || (candidate_el[e] == "H2")) {
                  //       max_el_num = 80;
                  //     } else if (candidate_el[e] == "Cl37" || (candidate_el[e] == "Br81")) {
                  //       max_el_num = 5;
                  //     }
                  //
                  //     if (e == 0) {
                  //       rel_int = candidate_ab[0];
                  //     } else {
                  //       rel_int = rel_int * candidate_ab[e];
                  //     }
                  //
                  //   }
                  //
                  //   min_rel_int = (rel_int/candidate_ab.size()) * min_el_num * (min_el_num - 1);
                  //
                  //   max_rel_int = (rel_int/candidate_ab.size()) * max_el_num * (max_el_num - 1);
                  //
                  // }

                  double f_rel_int = all_intensity[chain_idx[f]]/intensity;

                  Rcpp::Rcout << "rel min: " << min_rel_int * 0.95 << std::endl;
                  Rcpp::Rcout << "rel max: " << max_rel_int * 1.05 << std::endl;
                  Rcpp::Rcout << "rel f: " << f_rel_int << std::endl;


                  if (candidate_md_error < mass_error &&
                      f_rel_int >= min_rel_int * 0.95 &&
                      f_rel_int <= max_rel_int * 1.05) {

                    mass_error = candidate_md_error;
                    candidate_iso_md = temp_iso_md[j];
                    is_iso_candidate = true;

                    el_temp = candidate_el;
                    ab_temp = candidate_ab;

                    Rcpp::Rcout << "Hit: " << temp_iso_md[j] << std::endl;
                  }
                  Rcpp::Rcout << std::endl;
                  // TODO evaluate the mass error for wrong assignment
                }
              }

              if (is_iso_candidate) {

                // relative intensity validation ///////////////////////////////////////////////////////////////
                ////////////////////////////////////////////////////////////////////////////////////////////////


                // for (int e = 0; e < ab_temp.size(); ++e) {
                //   Rcpp::Rcout << "step " << iso << " ab: " << ab_temp[e] << std::endl;
                // }
                //
                // double rel_int = 1;
                //
                // if (ab_temp.size() == 1) {
                //   rel_int = ab_temp[0];
                // } else {
                //   for (int e = 0; e < ab_temp.size(); ++e) {
                //     rel_int = rel_int * ab_temp[e];
                //   }
                // }
                //
                // int min_el_num = 1;
                // int max_el_num = 10;
                //
                // if ((el_temp[0] == "C13") || (el_temp[0] == "H2")) {
                //   max_el_num = 60;
                // } else if (el_temp[0] == "Cl37") {
                //   max_el_num = 5;
                // }
                //
                // double min_rel_int = min_el_num * rel_int;
                // double max_rel_int = max_el_num * rel_int;
                //
                // double f_rel_int = all_intensity[chain_idx[f]]/intensity;
                //
                // Rcpp::Rcout << "rel min: " << min_rel_int << std::endl;
                // Rcpp::Rcout << "rel max: " << max_rel_int << std::endl;
                // Rcpp::Rcout << "rel f: " << f_rel_int << std::endl;


                // TODO validate intensity based on md matched
                // if out of the boundaries continue to next feature without adding candidate to chain

                // compares the error and if lower update
                if (feat_iso_gr[chain_idx[f]] != 0) {
                  if (feat_iso_error[chain_idx[f]] < mass_error) {
                    continue;
                  }
                }

                std::string concat_el_temp = Rcpp::as<std::string>(el_temp[0]);
                for (int e = 1; e < el_temp.size(); ++e) {
                  concat_el_temp += " " + Rcpp::as<std::string>(el_temp[e]);
                }

                Rcpp::Rcout << "step " << iso << " elements: " << concat_el_temp << std::endl;
                Rcpp::Rcout <<  std::endl;

                hits.push_back(chain_mz[f]);
                step.push_back(iso);
                idx.push_back(chain_idx[f]);
                ints.push_back(chain_intensity[f] / intensity);
                diff.push_back(candidate_diff);
                md.push_back(candidate_iso_md);
                el.push_back(concat_el_temp);
                error.push_back(mass_error);
              }
            } // loop for each feature in chain

            if (is_Mplus) break;

            // when there are hits, updates the iso step list
            if (hits.size() > 0) {
              hits_iso_mz[iso] = hits;
              hits_iso_step[iso] = step;
              hits_iso_idx[iso] = idx;
              hits_iso_ints[iso] = ints;
              hits_iso_diff[iso] = diff;
              hits_iso_md[iso] = md;
              hits_iso_el[iso] = el;
              hits_iso_error[iso] = error;
            }
          } // lop for each iso step

          if (is_Mplus) break;
          // if (has_gap) continue;

          hits_mz_list[z] = hits_iso_mz;
          hits_step_list[z] = hits_iso_step;
          hits_idx_list[z] = hits_iso_idx;
          hits_ints_list[z] = hits_iso_ints;
          hits_diff_list[z] = hits_iso_diff;
          hits_md_list[z] = hits_iso_md;
          hits_el_list[z] = hits_iso_el;
          hits_error_list[z] = hits_iso_error;

        } // loop for chain evaluation for each charge

        if (is_Mplus) continue;

        // list_out["hits_mz_list"] = hits_mz_list;
        // list_out["hits_diff_list"] = hits_diff_list;
        // list_out["hits_md_list"] = hits_md_list;
        // list_out["hits_el_list"] = hits_el_list;
        // list_out["hits_error_list"] = hits_error_list;



        // Chain selection /////////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////////////////////////////

        int chain_length = 0;
        int pos_first_in_chain = 0;
        int charge_idx = 0;
        Rcpp::NumericVector final_chain;

        for (size_t z = 0; z < zvals.size(); z++) {
          const Rcpp::List& chain = hits_mz_list[z];
          Rcpp::NumericVector flat_chain;
          for (int iso = 0; iso < maxIsotopes + 1; ++iso) {
            const Rcpp::List& iso_step = chain[iso];
              for (int m = 0; m < iso_step.size(); m++) {
                flat_chain.push_back(iso_step[m]);
              }
          }

          int chain_counter = 0;
          int pos_of_first_hit_after_0 = 0;
          for (int m = 0; m < flat_chain.size(); m++) {
            if (flat_chain[m] > 0) chain_counter++;
            if (m != 0 && flat_chain[m] > 0 && pos_of_first_hit_after_0 == 0) {
              pos_of_first_hit_after_0 = m;
            }
          }

          if (chain_counter > chain_length) {
            pos_first_in_chain = pos_of_first_hit_after_0;
            chain_length = chain_counter;
            final_chain = flat_chain;
            charge_idx = z;

          } else if (chain_counter == chain_length) {

            if (pos_of_first_hit_after_0 < pos_first_in_chain) {
              pos_first_in_chain = pos_of_first_hit_after_0;
              chain_length = chain_counter;
              final_chain = flat_chain;
              charge_idx = z;
            }

            // Rcpp::Rcout << std::endl;
            // Rcpp::Rcout << "Chain from different charges with the same length!!!" << std::endl;
            // Rcpp::Rcout << std::endl;

            // TODO or select based on lowest mass error
          }

          // list_out.push_back(final_chain);


        } // loop for chain selection for each charge



        // Chain description ///////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////////////////////////////

        if (final_chain.size() > 0) {
          const Rcpp::List& step = hits_step_list[charge_idx];
          const Rcpp::List& idx = hits_idx_list[charge_idx];
          const Rcpp::List& ints = hits_ints_list[charge_idx];
          const Rcpp::List& diff = hits_diff_list[charge_idx];
          const Rcpp::List& md = hits_md_list[charge_idx];
          const Rcpp::List& el = hits_el_list[charge_idx];
          const Rcpp::List& error = hits_error_list[charge_idx];
          Rcpp::NumericVector flat_step;
          Rcpp::NumericVector flat_idx;
          Rcpp::NumericVector flat_ints;
          Rcpp::NumericVector flat_diff;
          Rcpp::NumericVector flat_md;
          Rcpp::CharacterVector flat_el;
          Rcpp::NumericVector flat_error;
          for (int iso = 0; iso < maxIsotopes + 1; ++iso) {
            const Rcpp::List& step_step = step[iso];
            const Rcpp::List& idx_step = idx[iso];
            const Rcpp::List& ints_step = ints[iso];
            const Rcpp::List& diff_step = diff[iso];
            const Rcpp::List& md_step = md[iso];
            const Rcpp::List& el_step = el[iso];
            const Rcpp::List& error_step = error[iso];

            for (int m = 0; m < step_step.size(); m++) {
              flat_step.push_back(step_step[m]);
              flat_idx.push_back(idx_step[m]);
              flat_ints.push_back(ints_step[m]);
              flat_diff.push_back(diff_step[m]);
              flat_md.push_back(md_step[m]);
              flat_el.push_back(el_step[m]);
              flat_error.push_back(error_step[m]);
            }
          }

          final_estimated_number_carbons = int (estimated_number_carbons[charge_idx]);

          // rounds when decimal is above or equal to 0.5
          double rounding = estimated_number_carbons[charge_idx] - final_estimated_number_carbons;
          if (rounding >= 0.5) final_estimated_number_carbons = final_estimated_number_carbons + 1;

          // list_out["step"] = flat_step;
          // list_out["chain"] = final_chain;
          // list_out["idx"] = flat_idx;
          // list_out["ints"] = flat_ints;
          // list_out["diff"] = flat_diff;
          // list_out["md"] = flat_md;
          // list_out["el"] = flat_el;
          // list_out["error"] = flat_error;


          // Validate chain by position //////////////////////////////////////////////////////////////////
          ////////////////////////////////////////////////////////////////////////////////////////////////


          // Write chain in output when has isotopes /////////////////////////////////////////////////////
          ////////////////////////////////////////////////////////////////////////////////////////////////

          bool has_isotopes = false;

          for (int m = 0; m < final_chain.size(); m++) {
            if (m != 0 && final_chain[m] != 0) {
              has_isotopes = true;
            }
          }

          if (has_isotopes) {
            iso_gr++;

            for (int m = 0; m < final_chain.size(); m++) {

              if (final_chain[m] != 0) {
                feat_iso_gr[flat_idx[m]] = iso_gr;
                feat_iso_step[flat_idx[m]] = flat_step[m];
                // feat_iso_cat[flat_idx[m]] = "[M+" + step_step[m];
                // feat_iso_cat[flat_idx[m]] = feat_iso_cat[i] + "]";
                feat_iso_diff[flat_idx[m]] = flat_diff[m];
                feat_iso_md[flat_idx[m]] = flat_md[m];
                feat_iso_el[flat_idx[m]] = flat_el[m];
                feat_iso_error[flat_idx[m]] = flat_error[m];
                feat_iso_int[flat_idx[m]] = flat_ints[m];
                feat_iso_feat[flat_idx[m]] = id;
                feat_iso_z[flat_idx[m]] = zvals[charge_idx];
                feat_estimated_carbons[flat_idx[m]] = final_estimated_number_carbons;
              }
            }
          }
        } else { // if chain size is 0

        }
      } else { // if only 1 feature

      }
    } else { // if iso group is already

    }
  } // end main loop

  Rcpp::DataFrame output_df = Rcpp::DataFrame::create(
    Rcpp::Named("index") = all_idx,
    Rcpp::Named("feature") = all_ids,
    Rcpp::Named("mz") = all_mz,
    Rcpp::Named("rt") = all_rt,
    Rcpp::Named("intensity") = all_intensity,
    Rcpp::Named("iso_gr") = feat_iso_gr,
    Rcpp::Named("iso_z") = feat_iso_z,
    // Rcpp::Named("iso_cat") = feat_iso_cat,
    Rcpp::Named("hits_iso_step") = feat_iso_step,
    Rcpp::Named("iso_rel") = feat_iso_int,
    Rcpp::Named("hits_iso_diff") = feat_iso_diff,
    Rcpp::Named("hits_iso_md") = feat_iso_md,
    Rcpp::Named("hits_iso_el") = feat_iso_el,
    Rcpp::Named("hits_n_carbons") = feat_estimated_carbons,
    Rcpp::Named("hits_iso_error") = feat_iso_error,
    Rcpp::Named("iso_feat") = feat_iso_feat
  );

  list_out["output"] = output_df;

  return list_out;
}


// Old Make mass diff combinations /////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////////////////////////////

// std::vector<std::tuple<std::string, double>> iso_combined;
// for (size_t i = 0; i < iso_md.size(); ++i) {
//   iso_combined.push_back(std::make_tuple(iso_elements[i], iso_md[i]));
// }

// int offset = 0;

// for (int r = 1; r <= max_number_elements; r++) {
//   int iterations =  iso_combined.size();

//   for (int i = offset; i < iterations; ++i) {

//     for (int e = 0; e < 10; ++e) {
//       std::string el = std::get<0>(iso_combined[i]) + std::get<0>(iso_combined[e]);
//       std::sort(el.begin(), el.end());
//       double m = std::get<1>(iso_combined[i]) + std::get<1>(iso_combined[e]);
//       iso_combined.push_back(std::make_tuple(el, m));
//     }
//   }

//   std::sort(iso_combined.begin(), iso_combined.end(), [](const auto& a, const auto& b) {
//       return std::get<1>(a) < std::get<1>(b);
//   });

//   auto last = std::unique(iso_combined.begin(), iso_combined.end(),
//     [](const std::tuple<std::string, double>& a, const std::tuple<std::string, double>& b) {
//         return  std::get<0>(a) == std::get<0>(b);
//     });

//   iso_combined.erase(last, iso_combined.end());

//   offset = offset + iterations;
//   // Rcpp::Rcout << "The offset is " << offset << std::endl;
// }

// Rcpp::CharacterVector column0;
// std::vector<double> column1;

// // Iterate over the tuples and fill the DataFrame columns
// for (auto& tpl : iso_combined) {
//     column0.push_back(std::get<0>(tpl));
//     column1.push_back(std::get<1>(tpl));
// }

// // Create the DataFrame with the columns
// Rcpp::DataFrame df = Rcpp::DataFrame::create(
//   Rcpp::Named("elements") = column0,
//   Rcpp::Named("massdiff") = column1
// );

// list_out["iso_combined"] = df;

// double multiplier = std::pow(10.0, decimal_numbers);

// std::vector<double> mass_diff_res(column1.size());

// for (size_t v = 0; v < column1.size(); v++) {
//   mass_diff_res[v] = std::round(column1[v] * multiplier) / multiplier;
// }

// std::sort(mass_diff_res.begin(), mass_diff_res.end());

// auto last = std::unique(mass_diff_res.begin(), mass_diff_res.end());

// mass_diff_res.erase(last, mass_diff_res.end());

// list_out["unique_vec"] = mass_diff_res;

// std::vector<double> hits_iso_step(mass_diff_res.size());
// for (size_t v = 0; v < mass_diff_res.size(); v++) {
//   hits_iso_step[v] = std::round(mass_diff_res[v] * 1) / 1;
// }


// Rcpp::Rcout << std::endl;
// Rcpp::Rcout << "Charge distance: " << std::endl;

// std::vector<double> exp_C_dist(zvals.size());
// std::vector<double> exp_N_dist(zvals.size());

// for (size_t z = 0; z < zvals.size(); z++) {
//   double C_step = C_diff / zvals[z];
//   // Rcpp::Rcout << "For z:" << z << "C dist: " << C_step << std::endl;
//   double step_step = N_diff / zvals[z];
//   // Rcpp::Rcout << "For z:" << z << "N dist: " << step_step << std::endl;

//   double C_candidate = mz;
//   double N_candidate = mz;

//   isomat(z, 0) = mz;

//   for (int col = 1; col < maxIsotopes + 1; ++col) {

//     bool found_hit = false;

//     for (int f = 1; f < number_chain_features; ++f) {

//       if (col == 1) {
//         double C_candidate_diff = abs(chain_mz[f] - C_candidate);

//         // matching c13 but need to validate intensity
//         if (C_candidate_diff <= 0.005) {
//           Rcpp::Rcout << "C13 with " << C_candidate_diff << std::endl;
//           isomat(z, col) = chain_mz[f];
//           found_hit = true;
//         }

//       } else {
//         double C_candidate_diff = abs(chain_mz[f] - C_candidate);
//         double N_candidate_diff = abs(chain_mz[f] - N_candidate);

//         if (C_candidate_diff <= 0.005) {
//           Rcpp::Rcout << "Iso n" << col << " with C error " << C_candidate_diff << std::endl;
//           isomat(z, col) = chain_mz[f];
//           found_hit = true;

//         } else if (N_candidate_diff <= 0.005) {
//           Rcpp::Rcout << "Iso n" << col << " with N error " << N_candidate_diff << std::endl;
//           isomat(z, col) = chain_mz[f];
//           found_hit = true;
//         }
//       }
//     }

//     if (found_hit) {
//       C_candidate = isomat(z, col);
//       N_candidate = isomat(z, col);
//     } else {
//       C_candidate = C_candidate + C_step;
//       N_candidate = C_candidate + N_step;
//     }
//   }
// }
