#include <iostream>
#include <vector>
#include <string>
#include <tuple>
#include <algorithm>
#include <unordered_set>
#include <numeric>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_ms_annotation_isotopes(
  Rcpp::DataFrame features,
  int maxIsotopes = 5,
  Rcpp::CharacterVector elements = Rcpp::CharacterVector::create("C","H", "N", "O", "S", "Cl", "Br"),
  std::string mode = "small molecules",
  int maxCharge = 1,
  double rtWindowAlignment = 0.3,
  int maxGaps = 1,
  double maxCarbons = 80,
  double maxHetero = 15,
  double maxHalogens = 10,
  bool verbose = false
) {

  
  
  Rcpp::List list_out;
  
  
  
  
  
  // Check arguments /////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////
  
  std::vector<std::string> features_cols = features.names();
  
  if (features.nrows() == 0 || features_cols.size() == 0) {
    throw std::runtime_error("Features DataFrame is empty!");
  }
  
  std::vector<std::string> must_have_names = {
    "feature", "index", "rt", "rtmin", "rtmax",
    "mz", "mzmin", "mzmax", "polarity", "intensity"
  };
  
  std::vector<bool> has_must_have_names(10, false);
  
  for (size_t i = 0; i < must_have_names.size(); ++i) {
    for (size_t j = 0; j < features_cols.size(); ++j) {
      if (must_have_names[i] == features_cols[j]) has_must_have_names[i] = true;
    }
  }
  
  for (bool value : has_must_have_names) {
    if (!value) {
      throw std::runtime_error("The DataFrame features does not have all required columns!");
    }
  }
  
  
  
  
  
  
  // Order Input data by mz //////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////
  
  int number_of_features = features.nrows();
  
  if (verbose) {
    Rcpp::Rcout << "Processing " << number_of_features << "..." << std::endl;
  }

  std::vector<std::string> all_ids_unsorted = features["feature"];
  std::vector<int> all_idx_unsorted = features["index"];
  std::vector<double> all_rt_unsorted = features["rt"];
  std::vector<double> all_rtmin_unsorted = features["rtmin"];
  std::vector<double> all_rtmax_unsorted = features["rtmax"];
  std::vector<double> all_mz_unsorted = features["mz"];
  std::vector<double> all_mzmin_unsorted = features["mzmin"];
  std::vector<double> all_mzmax_unsorted = features["mzmax"];
  std::vector<double> all_intensity_unsorted = features["intensity"];

  const std::string* all_ids_unsorted_ptr = all_ids_unsorted.data();
  const int* all_idx_unsorted_ptr = all_idx_unsorted.data();
  const double* all_rt_unsorted_ptr = all_rt_unsorted.data();
  const double* all_mz_unsorted_ptr = all_mz_unsorted.data();
  const double* all_mzmin_unsorted_ptr = all_mzmin_unsorted.data();
  const double* all_rtmin_unsorted_ptr = all_rtmin_unsorted.data();
  const double* all_rtmax_unsorted_ptr = all_rtmax_unsorted.data();
  const double* all_mzmax_unsorted_ptr = all_mzmax_unsorted.data();
  const double* all_intensity_unsorted_ptr = all_intensity_unsorted.data();

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

  std::string* all_ids_ptr = all_ids.data();
  int* all_idx_ptr = all_idx.data();
  double* all_rt_ptr = all_rt.data();
  double* all_mz_ptr = all_mz.data();
  double* all_mzmin_ptr = all_mzmin.data();
  double* all_rtmin_ptr = all_rtmin.data();
  double* all_rtmax_ptr = all_rtmax.data();
  double* all_mzmax_ptr = all_mzmax.data();
  double* all_intensity_ptr = all_intensity.data();

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
  }

  // Rcpp::DataFrame sorted_features_df = Rcpp::DataFrame::create(
  //   Rcpp::Named("index") = all_idx,
  //   Rcpp::Named("feature") = all_ids,
  //   Rcpp::Named("mz") = all_mz,
  //   Rcpp::Named("rt") = all_rt
  // );

  // list_out["sorted_features"] = sorted_features_df;





  // possible elements ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////


  if (mode == "small molecules") {
    maxCarbons = 80;
    maxHetero = 15;
    maxHalogens = 10;
  }

  std::vector<std::string> select_elements = {
    "C",
    "H",
    "N",
    "O",
    "O",
    "S",
    "S",
    "S",
    "Cl",
    "Br",
    "Si",
    "Si",
    "Ge",
    "Ge",
    "Ge",
    "Ge"
  };

  std::vector<std::string> all_iso_elements = {
    "13C",
    "2H",
    "15N",
    "17O",
    "18O",
    "33S",
    "34S",
    "36S",
    "37Cl",
    "81Br",
    "29Si",
    "30Si",
    "72Ge",
    "73Ge",
    "74Ge",
    "76Ge"
  };

  // iso mass difference from first isotope
  std::vector<double> all_iso_md = {
    1.0033548378, // 13C
    1.0062767, // 2H
    0.9970349, // 15N
    1.0042169, // 17O
    2.004246, //18O
    0.9993878, // 33S
    1.995796, // 34S
    3.99501, // 36S
    1.9970499, // 37Cl
    1.9979534, // 81Br
    0.99956819, // 29Si
    1.99684369, // 30Si
    1.9978284, // 72Ge
    2.9992115, // 73Ge
    3.9969304, // 74Ge
    5.9971552 // 76Ge
  };

  // iso relative abundance from first isotope
  std::vector<double> all_iso_ab = {
    0.0107800, // 13C
    0.00015574, // 2H
    0.00366300, // 15N
    0.00037200, // 17O
    0.00200040, // 18O
    0.00750000, // 33S
    0.04215000, // 34S
    0.00017000, // 36S
    0.24229000, // 37Cl
    0.49314000, // 81Br
    0.0468316, // 29Si
    0.0308716, // 30Si
    0.27662, // 72Ge
    0.07717, // 73Ge
    0.35943, // 74Ge
    0.07444 // 76Ge
  };

  // iso relative abundance from first isotope (for the majority the monoisotope)
  std::vector<double> all_iso_mono = {
    0.988922, // C
    0.99984426, // H
    0.996337, // N
    0.997628, // O
    0.997628, // O
    0.95018, // S
    0.95018, // S
    0.95018, // S
    0.75771, // Cl
    0.50686, // Br
    0.9222968, // Si
    0.9222968, // Si
    0.21234, // Ge
    0.21234, // Ge
    0.21234, // Ge
    0.21234 // Ge
  };

  // iso minimum number of elements
  std::vector<double> all_iso_min_el = {
    2, // C
    2, // H
    1, // N
    1, // O
    1, // O
    1, // S
    1, // S
    1, // S
    1, // Cl
    1,  // Br
    1, // Si
    1, // Si
    1, // Ge
    1, // Ge
    1, // Ge
    1 // Ge
  };

  // iso maximum number of elements
  std::vector<double> all_iso_max_el = {
    maxCarbons, // C
    maxCarbons, // H
    maxHetero, // N
    maxHetero, // O
    maxHetero, // O
    maxHalogens, // S
    maxHalogens, // S
    maxHalogens, // S
    maxHalogens, // Cl
    maxHalogens, // Br
    maxHalogens, // Si
    maxHalogens, // Si
    maxHalogens, // Ge
    maxHalogens, // Ge
    maxHalogens, // Ge
    maxHalogens // Ge
  };







  // Select elements based on input ///////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////

  int n = elements.size();
  std::vector<std::string> elements_sv(n);

  for (int e = 0; e < n; ++e) {
    elements_sv[e] = Rcpp::as<std::string>(elements[e]);
  }

  std::vector<int> which_elements;
  for (int e = 0; e < n; ++e) {
    for (size_t s = 0; s < select_elements.size(); ++s) {
      if (select_elements[s] ==  elements_sv[e]) {
        which_elements.push_back(s);
      }
    }
  }

  std::vector<std::string> iso_elements;
  std::vector<double> iso_md;
  std::vector<double> iso_ab;
  std::vector<double> iso_mono;
  std::vector<double> iso_min_el;
  std::vector<double> iso_max_el;

  for (int idx : which_elements) {
    iso_elements.push_back(all_iso_elements[idx]);
    iso_md.push_back(all_iso_md[idx]);
    iso_ab.push_back(all_iso_ab[idx]);
    iso_mono.push_back(all_iso_mono[idx]);
    iso_min_el.push_back(all_iso_min_el[idx]);
    iso_max_el.push_back(all_iso_max_el[idx]);
  }






  // Make mass diff combinations //////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////

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

      // excludes 2H and 17O from the M+2 on due to the low contribution
      if (prev_combination[0] == "2H" || prev_combination[0] == "17O") {
        continue;
      }

      // excludes 15N and 33S from the M+3 on due to the low contribution
      if (n > 1 &&  (prev_combination[0] == "15N" || prev_combination[0] == "33S")) {
        continue;
      }

      // excludes 15N and 33S from the M+3 on due to the low contribution
      if (prev_combination.size() >= 2) {
        if (prev_combination[1] == "15N" || prev_combination[1] == "33S") {
          continue;
        }
      }

      for (const std::string& el : iso_elements) {

        // excludes 2H and 17O from the M+2 on due to the low contribution
        if (el == "2H" || el == "17O") continue;

        // excludes 15N and 33S from the M+3 on due to the low contribution
        if (n > 1 &&  (el == "15N" || el == "33S")) continue;

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

  Rcpp::IntegerVector iso_md_step(iso_sum_md.size());
  for (size_t i = 0; i < iso_sum_md.size(); i++) {
    iso_md_step[i] = std::round(iso_sum_md[i] * 1) / 1;
  }

  // list_out["el_md_key"] = el_md_key;
  // list_out["el_ab_key"] = el_ab_key;
  // list_out["el_combinations"] = el_combinations_list;
  // list_out["el_md_combinations"] = el_md_combinations_list;
  // list_out["el_ab_combinations"] = el_ab_combinations_list;
  // list_out["iso_sum_md"] = iso_sum_md;
  // list_out["iso_md_step"] = iso_md_step;





  // Charges vector //////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<int> zvals(maxCharge);
  std::iota(zvals.begin(), zvals.end(), 1);

  // Output data.frame preparation ///////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  int iso_gr = 1;
  
  std::vector<int> feat_iso_gr(number_of_features);
  std::vector<int> feat_iso_z(number_of_features);
  std::vector<std::string> feat_iso_cat(number_of_features);
  std::vector<int> feat_iso_step(number_of_features);
  std::vector<double> feat_iso_int(number_of_features);
  std::vector<double> feat_iso_diff(number_of_features);
  std::vector<double> feat_iso_md(number_of_features);
  std::vector<double> feat_iso_error(number_of_features);
  std::vector<double> feat_iso_mz_sd(number_of_features);
  std::vector<std::string> feat_iso_feat(number_of_features);
  std::vector<std::string> feat_iso_el(number_of_features);
  std::vector<int> feat_iso_gr_size(number_of_features);


  // estimation carbons //////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<double> feat_estimated_carbons(number_of_features);
  int final_estimated_number_carbons;


  // Main loop ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  //mz259.085_rt1081_f143
  // for (int i = 122; i < 123; ++i) {

  // mz223.064_rt1174_f248, charge two not right 1.00066 in the iso 1
  // possibly highly affected by Nitegen or Oxygen
  //  for (int i = 34; i < 35; ++i) {

  // mz273.167_rt945_f35, not assigned with 13C
  // for (int i = 159; i < 160; ++i) {

  // mz276.079_rt1164_f230, chain without 13C but iso 2
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
  // for (int i = 236; i < 237; ++i) {


  for (int i = 0; i < number_of_features; ++i) {

    std::string id = all_ids[i];
    double mz = all_mz[i];
    double rt = all_rt[i];
    double rtmin = all_rtmin[i];
    double rtmax = all_rtmax[i];
    double intensity = all_intensity[i];

    bool is_Mplus = false;

    // not yet in an iso group, create chain of features
    if (feat_iso_gr[i] == 0) {

      // Build Chain /////////////////////////////////////////////////////////////////////////////////
      ////////////////////////////////////////////////////////////////////////////////////////////////

      if (verbose) {
        Rcpp::Rcout << "Annotating feature " << id << std::endl;
        Rcpp::Rcout << std::endl;
      }
      
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
      
      if (verbose) {
        Rcpp::Rcout << number_chain_features <<  " features between " << rtmin << " and " << rtmax << std::endl;
        Rcpp::Rcout << std::endl;
      }

      if (number_chain_features > 1) {
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

        std::vector<double> chain_mzmin(number_chain_features);
        double* chain_mzmin_ptr = chain_mzmin.data();

        std::vector<double> chain_mzmax(number_chain_features);
        double* chain_mzmax_ptr = chain_mzmax.data();

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
          *(chain_mzmin_ptr++) =  all_mzmin[*(which_fts_ptr + x)];
          *(chain_mzmax_ptr++) =  all_mzmax[*(which_fts_ptr + x)];
          *(chain_idx_ptr++) = *(which_fts_ptr + x);
          *(chain_id_ptr++) =  all_ids[*(which_fts_ptr + x)];
          *(chain_intensity_ptr++) =  all_intensity[*(which_fts_ptr + x)];
          *(chain_rt_ptr++) =  all_rt[*(which_fts_ptr + x)];
        }

        // Chain mass resolution ///////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////////////////////////////

        std::vector<double> chain_mzr(chain_mz.size());
        std::vector<double> chain_mzr_left(chain_mz.size());
        std::vector<double> chain_mzr_right(chain_mz.size());

        std::transform(
          chain_mz.begin(), chain_mz.end(),
          chain_mzmin.begin(), chain_mzr_left.begin(),
          std::minus<double>()
        );

        std::transform(
          chain_mzmax.begin(), chain_mzmax.end(),
          chain_mz.begin(), chain_mzr_right.begin(),
          std::minus<double>()
        );

        for (size_t z = 0; z < chain_mz.size(); ++z) {
          chain_mzr[z] = chain_mzr_left[z];
          if (chain_mzr[z] < chain_mzr_right[z]) {
            chain_mzr[z] = chain_mzr_right[z];
          }
        }

        // TODO perhaps here the chain could be filter by EIC correlation

        if (verbose) {
          Rcpp::Rcout << std::endl;
          Rcpp::Rcout << "Candidates in chain for: "<< id << std::endl;
          
          Rcpp::Rcout << std::left << std::setw(5) << "i"
                      << std::setw(10) << "mz"
                      << std::setw(10) << "mz_diff"
                      << std::setw(10) << "mz_sd"
                      << std::setw(10) << "int"
                      << std::setw(10) << "rel_int"
                      << std::setw(10) << "rt"
                      << std::setw(10) << "rt_diff" << std::endl;
          
          for (size_t z = 0; z < chain_mz.size(); ++z) {
            Rcpp::Rcout << std::left << std::setw(5) << chain_idx[z]
                        << std::fixed << std::setprecision(4) << std::setw(10) << chain_mz[z]
                        << std::fixed << std::setprecision(4) << std::setw(10) << chain_mz[z] - mz
                        << std::fixed << std::setprecision(4) << std::setw(10) << chain_mzr[z]
                        << std::fixed << std::setprecision(1) << std::setw(10) << chain_intensity[z]
                        << std::fixed << std::setprecision(3) << std::setw(10) << chain_intensity[z]/intensity
                        << std::setw(10) << chain_rt[z]
                        << std::fixed << std::setprecision(3) << std::setw(10) << chain_rt[z] - rt <<  std::endl;
          }  
        }

        // Chain evaluation ////////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////////////////////////////

        Rcpp::List hits_mz_list(maxCharge);
        Rcpp::List hits_step_list(maxCharge);
        Rcpp::List hits_idx_list(maxCharge);
        Rcpp::List hits_ints_list(maxCharge);
        Rcpp::List hits_diff_list(maxCharge);
        Rcpp::List hits_md_list(maxCharge);
        Rcpp::List hits_error_list(maxCharge);
        Rcpp::List hits_mz_sd_list(maxCharge);
        Rcpp::List hits_el_list(maxCharge);
        Rcpp::List hits_ab_list(maxCharge);

        std::vector<double> estimated_number_carbons(zvals.size());

        for (size_t z = 0; z < zvals.size(); z++) {
          // Rcpp::Rcout << std::endl;
          // Rcpp::Rcout << "Charge: " << zvals[z] << std::endl;

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

          Rcpp::List hits_iso_mz_sd(maxIsotopes + 1);
          Rcpp::NumericVector mz_sd_monoiso;
          mz_sd_monoiso.push_back(chain_mzr[0]);
          hits_iso_mz_sd[0] = mz_sd_monoiso;

          Rcpp::List hits_iso_error(maxIsotopes + 1);
          Rcpp::NumericVector error_monoiso;
          error_monoiso.push_back(0);
          hits_iso_error[0] = error_monoiso;

          Rcpp::List hits_iso_el(maxIsotopes + 1);
          std::vector<std::string> el_monoiso;
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

          std::vector<std::string> string_empty_vector;
          string_empty_vector.push_back("");

          for (int iso = 1; iso < maxIsotopes + 1; ++iso) {
            hits_iso_mz[iso] = empty_vector;
            hits_iso_step[iso] = empty_vector;
            hits_iso_idx[iso] = empty_vector;
            hits_iso_ints[iso] = empty_vector;
            hits_iso_diff[iso] = empty_vector;
            hits_iso_md[iso] = empty_vector;
            hits_iso_error[iso] = empty_vector;
            hits_iso_mz_sd[iso] = empty_vector;
            hits_iso_el[iso] = string_empty_vector;
            hits_iso_ab[iso] = empty_vector;
          }

          // list_out["hits_iso_md_empty"] = hits_iso_md;
          // list_out["hits_iso_el_empty"] = hits_iso_el;
          // list_out["hits_iso_ab_empty"] = hits_iso_ab;

          estimated_number_carbons[z] = 0;
          double max_chain_mzr = *std::max_element(chain_mzr.begin(), chain_mzr.end());

          for (int iso = 1; iso < maxIsotopes + 1; ++iso) {
            
            if (verbose) {
              Rcpp::Rcout << std::endl;
              Rcpp::Rcout << "Step: " << iso << std::endl;  
            }

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
            Rcpp::NumericVector temp_iso_md = Rcpp::wrap(iso_sum_md);
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
            Rcpp::NumericVector mz_sd;
            std::vector<std::string> el;

            for (int f = 1; f < number_chain_features; ++f) {
              double mzr = max_chain_mzr;
              double candidate_diff = chain_mz[f] - mz;
              double candidate_diff_min = candidate_diff - mzr;
              double candidate_diff_max = candidate_diff + mzr;

              // Check for molecular ion (M+) with distance 1.007276
              // and much higher intensity, for now set to x5
              if (iso == 1) {
                if (candidate_diff_min < 1.007276 &&
                    candidate_diff_max > 1.007276 &&
                    all_intensity[chain_idx[f]]/intensity > 5) {
                  
                  if (verbose) {
                    Rcpp::Rcout <<  std::endl;
                    Rcpp::Rcout << "Potential M+: " << id << std::endl;
                    Rcpp::Rcout << "  - diff: " << candidate_diff << std::endl;
                    Rcpp::Rcout << "  - max diff: " << candidate_diff_max << std::endl;
                    Rcpp::Rcout << "  - min diff: " << candidate_diff_min << std::endl;
                    Rcpp::Rcout << "  - [M+H]+: " << chain_id[f] << std::endl;
                    Rcpp::Rcout << "  - int ratio: " << all_intensity[chain_idx[f]]/intensity << std::endl;
                  }
                  
                  // TODO this will also capture 2H loss and mark it as M+
                  // the mass error might give an indication to check

                  feat_iso_cat[i] = "M";
                  feat_iso_step[i] = -1;
                  feat_iso_diff[i] = candidate_diff;
                  feat_iso_int[i] = intensity/all_intensity[chain_idx[f]];
                  feat_iso_mz_sd[i] = mzr;
                  feat_iso_feat[i] = chain_id[f];

                  is_Mplus = true;
                  break;
                }
              }

              bool is_iso_candidate = false;
              double mass_error = 10; // is updated on the first hit
              double candidate_iso_md = 0;
              std::vector<std::string> el_temp;
              Rcpp::NumericVector ab_temp;

              // when candidate is inside of the mass +/- sd for iso step
              if (temp_iso_md_min - mzr < candidate_diff && temp_iso_md_max + mzr > candidate_diff) {
                // Rcpp::Rcout << std::endl;
                // Rcpp::Rcout << "Eval " << all_ids[chain_idx[f]] << " with mass_diff " << candidate_diff << ": " << std::endl;

                // selects the md within the mass dev
                // when duplicated md, the first hit is the one stored
                for (int j = 0; j < temp_iso_md.size(); j++) {

                  double candidate_md_error = abs(temp_iso_md[j] - candidate_diff);

                  int candidate_idx = temp_iso_idx[j];
                  std::vector<std::string> candidate_el = el_combinations_list[candidate_idx];
                  Rcpp::NumericVector candidate_ab = el_ab_combinations_list[candidate_idx];

                  double min_rel_int = 1;
                  double max_rel_int = 1;

                  std::unordered_map<std::string, int> count_map;

                  for (size_t e = 0; e < candidate_el.size(); ++e) {
                    std::string e_el = candidate_el[e];
                    count_map[e_el]++;
                  }

                  for (const auto& pair : count_map) {
                    std::string e_el = pair.first;
                    int number_el = pair.second;

                    double e_ab = 0;
                    double mono_ab = 0;
                    int min_el_num = 1;
                    int max_el_num = 10;

                    for (size_t a = 0; a <= iso_elements.size(); ++a) {

                      if (iso_elements[a] == e_el) {
                        e_ab = iso_ab[a];
                        mono_ab = iso_mono[a];

                        if (number_el == 1 && e_el == "13C" && iso == 1) {
                          estimated_number_carbons[z] = all_intensity[chain_idx[f]]/(e_ab * intensity);
                          if (estimated_number_carbons[z] > iso_max_el[a]) {
                            estimated_number_carbons[z] = 0;
                          }
                          
                          if (verbose) {
                            Rcpp::Rcout << std::endl;
                            Rcpp::Rcout << "Estimated N. carbons:" << estimated_number_carbons[z] << std::endl;
                          }
                        }

                        if (estimated_number_carbons[z] > 0 && estimated_number_carbons[z] < iso_max_el[a] && e_el == "13C") {
                          min_el_num = estimated_number_carbons[z] * 0.9;
                          max_el_num = estimated_number_carbons[z] * 1.1;

                        } else {
                          min_el_num = iso_min_el[a];
                          max_el_num = iso_max_el[a];
                        }
                      }
                    }

                    // when only one isotope atom, mostly for M+1
                    // or M+2 for Cl, Br, Si and S
                    if (number_el == 1) {
                      double min_coef = (min_el_num * std::pow(mono_ab, min_el_num - number_el) * e_ab) / std::pow(mono_ab, min_el_num);
                      double max_coef = (max_el_num * std::pow(mono_ab, max_el_num - number_el) * e_ab) / std::pow(mono_ab, max_el_num);

                      min_rel_int = min_rel_int * min_coef;
                      max_rel_int = max_rel_int * max_coef;

                      // if (number_el == 1 && e_el == "13C" && iso == 1) {
                      //   list_out["max_rel_int_13C"] = max_rel_int;
                      // }

                    // when second time isotope, mostly for M+n, with n > 1
                    } else {

                      unsigned int fact = 1;
                      for (int a = 1; a <= number_el; ++a) {
                        fact *= a;
                      }

                      double min_coef = (std::pow(mono_ab, min_el_num - number_el) * std::pow(e_ab, number_el)) / fact;
                      double max_coef = (std::pow(mono_ab, max_el_num - number_el) * std::pow(e_ab, number_el)) / fact;

                      min_coef = min_coef / std::pow(mono_ab, min_el_num);
                      max_coef = max_coef / std::pow(mono_ab, max_el_num);

                      min_coef = min_coef * min_el_num * (min_el_num - 1);
                      max_coef = max_coef * max_el_num * (max_el_num - 1);

                      for (int t = 2; t <= number_el - 1; ++t) {
                        min_coef = min_coef * (min_el_num - t);
                        max_coef = max_coef * (max_el_num - t);
                      }

                      min_rel_int = min_rel_int * min_coef;
                      max_rel_int = max_rel_int * max_coef;

                      // if (candidate_el.size() == 2) {
                      //   if (candidate_el[0] == "13C" && candidate_el[1] == "13C" && iso == 2) {
                      //     list_out["max_rel_int_13C_13C"] = max_rel_int;
                      //   }
                      // }
                      //
                      // if (candidate_el.size() == 3) {
                      //   if (candidate_el[0] == "13C" && candidate_el[1] == "13C" && candidate_el[2] == "13C" && iso == 3) {
                      //     list_out["max_rel_int_13C_13C_13C"] = max_rel_int;
                      //   }
                      // }
                      //
                      // if (candidate_el.size() == 4) {
                      //   if (candidate_el[0] == "13C" && candidate_el[1] == "13C" && candidate_el[2] == "13C" && candidate_el[3] == "13C" && iso == 4) {
                      //     list_out["max_rel_int_13C_13C_13C_13C"] = max_rel_int;
                      //   }
                      // }

                    }
                  } // loop for each unique element in hit md

                  double f_rel_int = all_intensity[chain_idx[f]]/intensity;

                  // Rcpp::Rcout << std::endl;
                  // Rcpp::Rcout << "Elements: ";
                  // for (int e = 0; e < candidate_el.size(); ++e) {
                  //   Rcpp::Rcout << candidate_el[e] << " ";
                  // }
                  // Rcpp::Rcout << "(" << temp_iso_md[j] << ")"<< std::endl;
                  // Rcpp::Rcout << "rel min: " << min_rel_int * 0.7 << std::endl;
                  // Rcpp::Rcout << "rel max: " << max_rel_int * 1.3 << std::endl;
                  // Rcpp::Rcout << "rel f: " << f_rel_int << std::endl;
                  // bool int_eval_temp = f_rel_int >= min_rel_int * 0.7 && f_rel_int <= max_rel_int * 1.3;
                  // Rcpp::Rcout << "int eval: " << int_eval_temp << std::endl;
                  // Rcpp::Rcout << "mass error: " << candidate_md_error << std::endl;
                  // Rcpp::Rcout << "mass max error: " << mzr * 1.3 << std::endl;
                  // bool mass_eval_temp = candidate_md_error < mass_error && candidate_md_error <= mzr * 1.3;
                  // Rcpp::Rcout << "mass eval: " << mass_eval_temp << std::endl;

                  if (candidate_md_error < mass_error &&
                      candidate_md_error <= mzr * 1.3 &&
                      f_rel_int >= min_rel_int * 0.7 &&
                      f_rel_int <= max_rel_int * 1.3) {

                    // Rcpp::Rcout << "!!!!!! Hit !!!!!!" << std::endl;

                    mass_error = candidate_md_error;
                    candidate_iso_md = temp_iso_md[j];
                    is_iso_candidate = true;

                    el_temp = candidate_el;
                    ab_temp = candidate_ab;
                  }
                } // loop for each hit md
              }

              if (is_iso_candidate) {

                // compares the error and if lower update
                if (feat_iso_gr[chain_idx[f]] != 0) {
                  if (feat_iso_error[chain_idx[f]] < mass_error) {
                    continue;
                  }
                }

                std::string concat_el_temp = el_temp[0];
                for (size_t e = 1; e < el_temp.size(); ++e) {
                  concat_el_temp += " " + el_temp[e];
                }

                // Rcpp::Rcout << std::endl;
                // Rcpp::Rcout << "Step " << iso << " elements: " << concat_el_temp << std::endl;
                // Rcpp::Rcout <<  std::endl;

                hits.push_back(chain_mz[f]);
                step.push_back(iso);
                idx.push_back(chain_idx[f]);
                ints.push_back(chain_intensity[f] / intensity);
                diff.push_back(candidate_diff);
                md.push_back(candidate_iso_md);
                el.push_back(concat_el_temp);
                error.push_back(mass_error);
                mz_sd.push_back(mzr);
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
              hits_iso_mz_sd[iso] = mz_sd;
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
          hits_mz_sd_list[z] = hits_iso_mz_sd;

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
          const Rcpp::List& mz_sd = hits_mz_sd_list[charge_idx];
          Rcpp::NumericVector flat_step;
          Rcpp::NumericVector flat_idx;
          Rcpp::NumericVector flat_ints;
          Rcpp::NumericVector flat_diff;
          Rcpp::NumericVector flat_md;
          std::vector<std::string> flat_el;
          Rcpp::NumericVector flat_error;
          Rcpp::NumericVector flat_mz_sd;
          for (int iso = 0; iso < maxIsotopes + 1; ++iso) {
            const Rcpp::List& step_step = step[iso];
            const Rcpp::List& idx_step = idx[iso];
            const Rcpp::List& ints_step = ints[iso];
            const Rcpp::List& diff_step = diff[iso];
            const Rcpp::List& md_step = md[iso];
            const Rcpp::List& el_step = el[iso];
            const Rcpp::List& error_step = error[iso];
            const Rcpp::List& mz_sd_step = mz_sd[iso];

            for (int m = 0; m < step_step.size(); m++) {
              flat_step.push_back(step_step[m]);
              flat_idx.push_back(idx_step[m]);
              flat_ints.push_back(ints_step[m]);
              flat_diff.push_back(diff_step[m]);
              flat_md.push_back(md_step[m]);
              flat_el.push_back(el_step[m]);
              flat_error.push_back(error_step[m]);
              flat_mz_sd.push_back(mz_sd_step[m]);
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
            // iso_gr++;

            for (int m = 0; m < final_chain.size(); m++) {

              if (final_chain[m] != 0) {
                feat_iso_gr[flat_idx[m]] = iso_gr;
                feat_iso_step[flat_idx[m]] = flat_step[m];

                std::string cat = "M+";
                int rounded_step = static_cast<int>(std::round(flat_step[m]));
                cat +=  std::to_string(rounded_step);
                feat_iso_cat[flat_idx[m]] = cat;

                feat_iso_diff[flat_idx[m]] = flat_diff[m];
                feat_iso_md[flat_idx[m]] = flat_md[m];
                feat_iso_el[flat_idx[m]] = flat_el[m];
                feat_iso_error[flat_idx[m]] = flat_error[m];
                feat_iso_mz_sd[flat_idx[m]] = flat_mz_sd[m];
                feat_iso_int[flat_idx[m]] = flat_ints[m];
                feat_iso_feat[flat_idx[m]] = id;
                feat_iso_z[flat_idx[m]] = zvals[charge_idx];
                feat_estimated_carbons[flat_idx[m]] = final_estimated_number_carbons;
              }
            }
          } else {
            double mzmin = all_mzmin[i];
            double mzmax = all_mzmax[i];
            double mzr = mzmax - mzmin;

            // make single iso group
            feat_iso_gr[i] = iso_gr;
            feat_iso_step[i] = 0;
            feat_iso_cat[i] = "M+0";
            feat_iso_diff[i] = 0;
            feat_iso_md[i] = 0;
            feat_iso_el[i] = "";
            feat_iso_error[i] = 0;
            feat_iso_mz_sd[i] = mzr;
            feat_iso_int[i] = 1;
            feat_iso_feat[i] = id;
            feat_iso_z[i] = 1;
            feat_estimated_carbons[i] = 0;
          }

        } else { // if chain size is 0
          double mzmin = all_mzmin[i];
          double mzmax = all_mzmax[i];
          double mzr = mzmax - mzmin;

          // make single iso group
          feat_iso_gr[i] = iso_gr;
          feat_iso_step[i] = 0;
          feat_iso_cat[i] = "M+0";
          feat_iso_diff[i] = 0;
          feat_iso_md[i] = 0;
          feat_iso_el[i] = "";
          feat_iso_error[i] = 0;
          feat_iso_mz_sd[i] = mzr;
          feat_iso_int[i] = 1;
          feat_iso_feat[i] = id;
          feat_iso_z[i] = 1;
          feat_estimated_carbons[i] = 0;
        }

      } else { // if only 1 feature
        double mzmin = all_mzmin[i];
        double mzmax = all_mzmax[i];
        double mzr = mzmax - mzmin;

        // make single iso group
        feat_iso_gr[i] = iso_gr;
        feat_iso_step[i] = 0;
        feat_iso_cat[i] = "M+0";
        feat_iso_diff[i] = 0;
        feat_iso_md[i] = 0;
        feat_iso_el[i] = "";
        feat_iso_error[i] = 0;
        feat_iso_mz_sd[i] = mzr;
        feat_iso_int[i] = 1;
        feat_iso_feat[i] = id;
        feat_iso_z[i] = 1;
        feat_estimated_carbons[i] = 0;
      }

    } else { // if iso group is already

    }

    iso_gr++;
  } // end main loop

  // adds iso group to M+ ions with iso step -1
  // cleans iso groups with size one
  for (int i = 0; i < number_of_features; ++i) {

    bool is_m_ion = false;
    if (feat_iso_step[i] == -1) is_m_ion = true;

    if (feat_iso_gr[i] > 0 || is_m_ion) {
      std::string gr_feat = feat_iso_feat[i];
      int gr = feat_iso_gr[i];
      int count = 0;

      for (int f = 0; f < number_of_features; ++f) {

        if (is_m_ion) {
          if (all_ids[f] == gr_feat) {
            feat_iso_gr[i] = feat_iso_gr[f];
          }
        }

        if (feat_iso_gr[f] == gr) count++;
      }

      feat_iso_gr_size[i] = count;

      if (count <= 1) {
        double mzmin = all_mzmin[i];
        double mzmax = all_mzmax[i];
        double mzr = mzmax - mzmin;

        // make single iso group
        feat_iso_step[i] = 0;
        feat_iso_cat[i] = "M+0";
        feat_iso_diff[i] = 0;
        feat_iso_md[i] = 0;
        feat_iso_el[i] = "";
        feat_iso_error[i] = 0;
        feat_iso_mz_sd[i] = mzr;
        feat_iso_int[i] = 1;
        feat_iso_feat[i] = all_ids[i];
        feat_iso_z[i] = 1;
        feat_estimated_carbons[i] = 0;
      }
    }
  }


  Rcpp::DataFrame output_df = Rcpp::DataFrame::create(
    Rcpp::Named("index") = all_idx,
    Rcpp::Named("feature") = all_ids,
    Rcpp::Named("mz") = all_mz,
    Rcpp::Named("rt") = all_rt,
    Rcpp::Named("intensity") = all_intensity,
    Rcpp::Named("iso_gr") = feat_iso_gr,
    Rcpp::Named("iso_z") = feat_iso_z,
    Rcpp::Named("iso_cat") = feat_iso_cat,
    Rcpp::Named("iso_step") = feat_iso_step,
    Rcpp::Named("iso_md_diff") = feat_iso_diff,
    Rcpp::Named("iso_md_hit") = feat_iso_md,
    Rcpp::Named("iso_md_error") = feat_iso_error,
    Rcpp::Named("iso_mz_sd") = feat_iso_mz_sd,
    Rcpp::Named("iso_rel_int") = feat_iso_int,
    Rcpp::Named("iso_el") = feat_iso_el,
    Rcpp::Named("iso_n_carbons") = feat_estimated_carbons,
    Rcpp::Named("iso_feat") = feat_iso_feat,
    Rcpp::Named("iso_size") = feat_iso_gr_size
  );
  
  list_out["output"] = output_df;
  
  Rcpp::List list_out2(number_of_features);
  
  // Rcpp::CharacterVector feat_iso_el_new(number_of_features);
  
  // for (int i = 0; i < number_of_features; ++i) {
  //   if (TYPEOF(feat_iso_el[i]) == CHARSXP) {
  //     SEXP str = Rf_coerceVector(feat_iso_el[i], STRSXP);
  //     feat_iso_el_new[i] = Rcpp::String(CHAR(STRING_ELT(str, 0)));
  //   } else {
  //     feat_iso_el_new[i] = feat_iso_el[i];
  //   }
  // }
  
  for (int i = 0; i < number_of_features; ++i) {
    
    // if (feat_iso_el[i] == "") {
    //   feat_iso_el[i] = NA_STRING;
    // }
    // 
    // if (TYPEOF(feat_iso_el[i]) == CHARSXP) {
    //   feat_iso_el[i] = Rcpp::String(feat_iso_el[i]);
    // }
    
    list_out2[i] = Rcpp::List::create(
      Rcpp::Named("feature") = all_ids[i],
      Rcpp::Named("cluster") = feat_iso_gr[i],
      Rcpp::Named("cluster_size") = feat_iso_gr_size[i],
      Rcpp::Named("cluster_feature") = feat_iso_feat[i],
      Rcpp::Named("elements") = feat_iso_el[i],
      Rcpp::Named("carbons") = feat_estimated_carbons[i],
      Rcpp::Named("charge") = feat_iso_z[i],
      Rcpp::Named("tag") = feat_iso_cat[i],
      Rcpp::Named("step") = feat_iso_step[i],
      Rcpp::Named("md_diff") = feat_iso_diff[i],
      Rcpp::Named("md_hit") = feat_iso_md[i],
      Rcpp::Named("md_error") = feat_iso_error[i],
      Rcpp::Named("mass_deviation") = feat_iso_mz_sd[i],
      Rcpp::Named("relative_intensity") = feat_iso_int[i]
    );
  }

  list_out["output2"] = list_out2;
  
  return list_out;
}

// merging based on resolution or mass deviation ///////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////

// double multiplier = std::pow(10.0, decimal_numbers);

// std::vector<double> iso_sum_md_rounded(iso_sum_md.size());
//
// for (size_t i = 0; i < iso_sum_md.size(); i++) {
//   iso_sum_md_rounded[i] = iso_sum_md[i];
//   // iso_sum_md_rounded[i] = std::round(iso_sum_md[i] * multiplier) / multiplier;
// }
//
// // does not remove duplicates, consequently the first hit is the one stored
// // as following duplicates will give the same error
// // std::sort(iso_sum_md_rounded.begin(), iso_sum_md_rounded.end());
// // auto last = std::unique(iso_sum_md_rounded.begin(), iso_sum_md_rounded.end());
// // iso_sum_md_rounded.erase(last, iso_sum_md_rounded.end());
//
// Rcpp::IntegerVector iso_md_step(iso_sum_md_rounded.size());
// for (size_t i = 0; i < iso_sum_md_rounded.size(); i++) {
//   iso_md_step[i] = std::round(iso_sum_md_rounded[i] * 1) / 1;
// }
//
// list_out["iso_sum_md_rounded"] = iso_sum_md_rounded;
// list_out["iso_md_step"] = iso_md_step;




// Mass accuracy of input data /////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////

// std::vector<double> all_mzr(all_mzmax.size());
//
// std::transform(
//   all_mzmax.begin(), all_mzmax.end(),
//   all_mzmin.begin(), all_mzr.begin(),
//   std::minus<double>()
// );
//
// double mzr_max = *std::max_element(all_mzr.begin(), all_mzr.end());
// double mzr_min = *std::min_element(all_mzr.begin(), all_mzr.end());
// double mzr_mean = std::accumulate(all_mzr.begin(), all_mzr.end(), 0.0) / all_mzr.size();
//
// // calculates the number of decimal numbers
// // according to the mean mass deviation,
// // which is dependent on the mass resolution
// std::string mzr_str = std::to_string(mzr_mean);
// size_t decimal_pos = mzr_str.find(".");
// int decimal_numbers = 1;
// if (decimal_pos != std::string::npos) {
//   for (size_t d = decimal_pos + 1; d < mzr_str.length(); d++) {
//     if (mzr_str[d] == '0') {
//       decimal_numbers++;
//     } else {
//       break;
//     }
//   }
// }

// list_out["resolution_min"] = mzr_min;
// list_out["resolution_max"] = mzr_max;
// list_out["resolution_av"] = mzr_mean;
// list_out["resolution_decimal"] = decimal_numbers;


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

//         // matching 13C but need to validate intensity
//         if (C_candidate_diff <= 0.005) {
//           Rcpp::Rcout << "13C with " << C_candidate_diff << std::endl;
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



// if (candidate_ab.size() == 1) {
//
//   std::string e_el = Rcpp::as<std::string>(candidate_el[0]);
//
//   if ((e_el == "13C") || (e_el == "2H")) {
//     max_el_num = 80;
//   } else if (e_el == "37Cl" || (e_el == "81Br")) {
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
//     if ((candidate_el[e] == "13C") || (candidate_el[e] == "2H")) {
//       max_el_num = 80;
//     } else if (candidate_el[e] == "37Cl" || (candidate_el[e] == "81Br")) {
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
