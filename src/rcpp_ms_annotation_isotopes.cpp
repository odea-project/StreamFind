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
                                       int maxIsotopes = 6,
                                       int maxCharge = 3,
                                       double rtWindowAlignment = 0.75,
                                       int maxGaps = 2) {

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
  std::sort(sort_features_idx.begin(), sort_features_idx.end(), [&](int i, int j){return all_mz_unsorted[i] < all_mz_unsorted[j];});

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

  Rcpp::DataFrame sorted_features_df = Rcpp::DataFrame::create(
    Rcpp::Named("index") = all_idx,
    Rcpp::Named("feature") = all_ids,
    Rcpp::Named("mz") = all_mz,
    Rcpp::Named("rt") = all_rt
  );

  // list_out["sorted_features"] = sorted_features_df;





  // Mass accuracy of input data /////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<double> all_mzr(all_mzmax.size());
  std::transform(all_mzmax.begin(), all_mzmax.end(), all_mzmin.begin(), all_mzr.begin(), std::minus<double>());

  double mzr_max = *std::max_element(all_mzr.begin(), all_mzr.end());
  double mzr_min = *std::min_element(all_mzr.begin(), all_mzr.end());
  double mzr_mean = std::accumulate(all_mzr.begin(), all_mzr.end(), 0.0) / all_mzr.size();

  std::cout << "Mass deviation: " << std::endl;
  std::cout << "Max: " << mzr_max << std::endl;
  std::cout << "Min: " << mzr_min << std::endl;
  std::cout << "Mean: " << mzr_mean << std::endl;

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

  std::cout << std::endl;
  std::cout << "Decimal numbers: " << decimal_numbers << std::endl;
  std::cout << std::endl;





  // Make mass diff combinations //////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<std::string> iso_elements = {
    "C",
    // "H",
    "N",
    "O",
    "O2", //
    "S",
    "S2",
    // "S3",
    "Cl",
    "Br"
  };

  std::vector<double> iso_md = {
    1.0033548378, // C - 13
    // 1.0062767, // H - 2
    0.9970349, // N - 15
    1.0042169, // O - 17
    2.004246, //O2 - 18
    0.9993878, // S - 33
    1.995796, // S2 - 34
    // 3.99501, // S3 - 36
    1.9970499, // Cl - 37
    1.9979534 // Br - 81
  };

  std::vector<double> iso_ab = {
    0.01107800, // C
    // 0.00015574, // H
    0.00366300, // N
    0.00037200, // O
    0.00200040, // O2
    0.00750000, // S
    0.04215000, // S2
    // 0.00017000, // S3
    0.24229000, // Cl
    0.49314000 // Br
  };

  int max_number_elements = 4;

  std::set<std::vector<std::string>> combinations;
  for (const std::string& el : iso_elements) {
    std::vector<std::string> el_v(1);
    el_v[0] = el;
    combinations.insert(el_v);
  }

  for (int n = 1; n <= max_number_elements; n++) {
    std::vector<std::vector<std::string>> combinations_vec(combinations.begin(), combinations.end());
    std::set<std::vector<std::string>> new_combinations;

    for (const std::vector<std::string>& prev_combination : combinations_vec) {
        for (const std::string& el : iso_elements) {
          std::vector<std::string> combination = prev_combination;
          combination.push_back(el);
          std::sort(combination.begin(), combination.end());
          new_combinations.insert(combination);
        }
    }
    combinations.insert(new_combinations.begin(), new_combinations.end());
  }

  std::vector<std::vector<std::string>> CbEl0(combinations.begin(), combinations.end());

  Rcpp::NumericVector el_md_key = Rcpp::wrap(iso_md);
  el_md_key.names() = iso_elements;

  Rcpp::NumericVector el_ab_key = Rcpp::wrap(iso_ab);
  el_ab_key.names() = iso_elements;

  std::vector<std::vector<double>> CbMd0(CbEl0.size());
  std::vector<std::vector<double>> CbAb0(CbEl0.size());

  std::vector<double> IsoMd_unordered(CbEl0.size());

  for (size_t i = 0; i < CbEl0.size(); ++i) {
    const std::vector<std::string>& el_v = CbEl0[i];
    std::vector<double> md_v(el_v.size());
    std::vector<double> ab_v(el_v.size());
    for(size_t j = 0; j < el_v.size(); ++j) {
      std::string el = el_v[j];
      md_v[j] = el_md_key[el];
      ab_v[j] = el_ab_key[el];
      IsoMd_unordered[i] = IsoMd_unordered[i] + el_md_key[el];
    }
    CbMd0[i] = md_v;
    CbAb0[i] = ab_v;
  }

  std::vector<int> IsoMd_idx(IsoMd_unordered.size());
  std::iota(IsoMd_idx.begin(), IsoMd_idx.end(), 0);
  std::sort(IsoMd_idx.begin(), IsoMd_idx.end(), [&](int i, int j){return IsoMd_unordered[i] < IsoMd_unordered[j];});

  const double* IsoMd_unordered_ptr = IsoMd_unordered.data();

  std::vector<std::vector<std::string>> CbEl(IsoMd_unordered.size());
  std::vector<std::vector<double>> CbMd(IsoMd_unordered.size());
  std::vector<std::vector<double>> CbAb(IsoMd_unordered.size());
  std::vector<double> IsoMd(IsoMd_unordered.size());
  double* IsoMd_ptr = IsoMd.data();

  int counter = 0;
  for (const int& x : IsoMd_idx) {
    *(IsoMd_ptr++) = *(IsoMd_unordered_ptr + x);
    CbEl[counter] =  CbEl0[x];
    CbMd[counter] =  CbMd0[x];
    CbAb[counter] =  CbAb0[x];
    counter++;
  }

  Rcpp::List CbEl_list;
  Rcpp::List CbMd_list;
  Rcpp::List CbAb_list;
  for (const std::vector<std::string>& x : CbEl) CbEl_list.push_back(x);
  for (const std::vector<double>& x : CbMd) CbMd_list.push_back(x);
  for (const std::vector<double>& x : CbAb) CbAb_list.push_back(x);

  // list_out["el_md_key"] = el_md_key;
  // list_out["el_ab_key"] = el_ab_key;
  // list_out["CbEl"] = CbEl_list;
  // list_out["CbMd"] = CbMd_list;
  // list_out["CbAb"] = CbAb_list;
  // list_out["IsoMd"] = IsoMd;





  // merging based on resolution or mass deviation ///////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////


  double multiplier = std::pow(10.0, decimal_numbers);

  std::vector<double> IsoMd_merged(IsoMd.size());

  for (size_t i = 0; i < IsoMd.size(); i++) {
    IsoMd_merged[i] = std::round(IsoMd[i] * multiplier) / multiplier;
  }

  // std::sort(IsoMd_merged.begin(), IsoMd_merged.end());
  // auto last = std::unique(IsoMd_merged.begin(), IsoMd_merged.end());
  // IsoMd_merged.erase(last, IsoMd_merged.end());

  Rcpp::IntegerVector IsoStep(IsoMd_merged.size());
  for (size_t i = 0; i < IsoMd_merged.size(); i++) {
    IsoStep[i] = std::round(IsoMd_merged[i] * 1) / 1;
  }

  // list_out["IsoMd_merged"] = IsoMd_merged;






  // Other preparations //////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<int> zvals(maxCharge);
  std::iota(zvals.begin(), zvals.end(), 1);

  int iso_gr = 0;
  std::vector<int> feat_iso_gr(number_of_features);
  std::vector<int> feat_iso_z(number_of_features);
  std::vector<std::string> feat_iso_cat(number_of_features);
  std::vector<int> feat_iso_n(number_of_features);
  std::vector<double> feat_iso_int(number_of_features);
  std::vector<double> feat_iso_diff(number_of_features);
  std::vector<double> feat_iso_elmass(number_of_features);
  std::vector<double> feat_iso_error(number_of_features);
  std::vector<std::string> feat_iso_feat(number_of_features);





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

  // Orbitrap data from AFIN-TS

  // mz254.059_rt712_f2111, Sulfamethoxazole
  // for (int i = 4686; i < 4687; ++i) {

  // mz192.138_rt1071_f9041, DEET
  // for (int i = 2652; i < 2653; ++i) {

  // mz249.019_rt1133_f12642, Linuron
  // for (int i = 4490; i < 4491; ++i) {

  // mz388.105_rt1244_f15264, Pyraclostrobin
  for (int i = 9778; i < 9779; ++i) {


  // for (int i = 0; i < number_of_features; ++i) {

    std::string id = all_ids[i];
    double mz = all_mz[i];
    double rt = all_rt[i];
    double rtmin = all_rtmin[i];
    double rtmax = all_rtmax[i];
    double intensity = all_intensity[i];

    bool is_Mplus = false;

    // std::cout << "Feature: " << id << std::endl;





    // not yet in an iso group, create chain of features
    if (feat_iso_gr[i] == 0) {

      // Build Chain /////////////////////////////////////////////////////////////////////////////////
      ////////////////////////////////////////////////////////////////////////////////////////////////

      double left_rt = rt -rtmin;
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

      double maxisomz = (mz + maxIsotopes) * 1.1;

      std::vector<int> which_fts;
      for (int z = 0; z < number_of_features; ++z) {
        if (all_rt[z] >= rtmin && all_rt[z] <= rtmax && all_mz[z] >= mz && all_mz[z] <= maxisomz) {
          which_fts.push_back(z);
        }
      }

      int number_chain_features = which_fts.size();
      // std::cout << "Number of features in chain: " << number_chain_features << std::endl;

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

        std::vector<int> chain_intensity(number_chain_features);
        int* chain_intensity_ptr = chain_intensity.data();

        for (const int& x : idx) {
          *(chain_mz_ptr++) = *(org_mz_ptr2 + x);
          *(chain_idx_ptr++) = *(which_fts_ptr + x);
          *(chain_id_ptr++) =  all_ids[*(which_fts_ptr + x)];
          *(chain_intensity_ptr++) =  all_intensity[*(which_fts_ptr + x)];
        }

        // TODO perhaps here the chain could be filter by EIC correlation

        std::cout << std::endl;
        std::cout << "Chain: " << std::endl;
        for (size_t z = 0; z < chain_mz.size(); ++z) {
          std::cout << chain_idx[z] << " " << chain_mz[z] << " " << chain_intensity[z] << std::endl;
        }

        std::cout << std::endl;
        std::cout << "Chain mass diff: " << std::endl;
        for (size_t z = 0; z < chain_mz.size(); ++z) {
          std::cout << chain_mz[z] - mz << std::endl;
        }

        // Chain evaluation ////////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////////////////////////////

        Rcpp::List iso_hits_list(maxCharge);
        Rcpp::List iso_n_list(maxCharge);
        Rcpp::List iso_idx_list(maxCharge);
        Rcpp::List iso_ints_list(maxCharge);
        Rcpp::List iso_diff_list(maxCharge);
        Rcpp::List iso_elmass_list(maxCharge);
        Rcpp::List iso_error_list(maxCharge);

        for (size_t z = 0; z < zvals.size(); z++) {
          // std::cout << "Charge: " << zvals[z] << std::endl;

          Rcpp::List iso_hits(maxIsotopes + 1);
          Rcpp::NumericVector mz_v;
          mz_v.push_back(mz);
          iso_hits[0] = mz_v;

          Rcpp::List iso_n(maxIsotopes + 1);
          Rcpp::NumericVector n_v;
          n_v.push_back(0);
          iso_n[0] = n_v;

          Rcpp::List iso_idx(maxIsotopes + 1);
          Rcpp::IntegerVector idx_v;
          idx_v.push_back(chain_idx[0]);
          iso_idx[0] = idx_v;

          Rcpp::List iso_ints(maxIsotopes + 1);
          Rcpp::NumericVector int_v;
          int_v.push_back(1);
          iso_ints[0] = int_v;

          Rcpp::List iso_diff(maxIsotopes + 1);
          Rcpp::NumericVector diff_v;
          diff_v.push_back(0);
          iso_diff[0] = diff_v;

          Rcpp::List iso_elmass(maxIsotopes + 1);
          Rcpp::NumericVector elmass_v;
          elmass_v.push_back(0);
          iso_elmass[0] = elmass_v;

          Rcpp::List iso_error(maxIsotopes + 1);
          Rcpp::NumericVector error_v;
          error_v.push_back(0);
          iso_error[0] = error_v;

          // fill with 0s the iso output for the max isotopes
          Rcpp::NumericVector empty_vec;
          Rcpp::IntegerVector int_empty_vec;
          empty_vec.push_back(0);
          int_empty_vec.push_back(nan(""));
          for (int iso = 1; iso < maxIsotopes + 1; ++iso) {
            iso_hits[iso] = empty_vec;
            iso_n[iso] = empty_vec;
            iso_idx[iso] = empty_vec;
            iso_ints[iso] = empty_vec;
            iso_diff[iso] = empty_vec;
            iso_elmass[iso] = empty_vec;
            iso_error[iso] = empty_vec;
          }

          for (int iso = 1; iso < maxIsotopes + 1; ++iso) {

            // if iso above 3, only continues chain if at least a hit was found to step n-1 and n-2
            if (iso >= maxGaps + 1) {
              Rcpp::LogicalVector gaps;
              for (int g = 1; g <= maxGaps; g++) {
                const Rcpp::NumericVector& temp_vec_iso_hits = iso_hits[iso - g];
                bool is_gap = temp_vec_iso_hits[0] == 0;
                gaps.push_back(is_gap);
              }

              if (Rcpp::is_true(Rcpp::all(gaps))) break;
            }

            Rcpp::LogicalVector iso_filter = IsoStep == iso;
            Rcpp::NumericVector IsoMd_temp = Rcpp::wrap(IsoMd_merged);
            IsoMd_temp = IsoMd_temp[iso_filter];
            IsoMd_temp = IsoMd_temp / zvals[z];

            double IsoMd_max = *std::max_element(IsoMd_temp.begin(), IsoMd_temp.end());
            double IsoMd_min = *std::min_element(IsoMd_temp.begin(), IsoMd_temp.end());

            Rcpp::NumericVector hits;
            Rcpp::NumericVector n;
            Rcpp::IntegerVector idx;
            Rcpp::NumericVector ints;
            Rcpp::NumericVector diff;
            Rcpp::NumericVector elmass;
            Rcpp::NumericVector error;

            for (int f = 1; f < number_chain_features; ++f) {

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

              // Check for molecular ion (M+) with distance 1.007276 and much higher intensity
              if (iso == 1) {
                if (candidate_diff_min < 1.007276 && candidate_diff_max > 1.007276 && all_intensity[chain_idx[f]]/intensity > 2) {
                  // std::cout <<  std::endl;
                  // std::cout << "Potential M+: " << id << std::endl;
                  // std::cout << "  - diff: " << candidate_diff << std::endl;
                  // std::cout << "  - max diff: " << candidate_diff_max << std::endl;
                  // std::cout << "  - min diff: " << candidate_diff_min << std::endl;
                  // std::cout << "  - [M+H]+: " << chain_id[f] << std::endl;
                  // std::cout << "  - int ratio: " << all_intensity[chain_idx[f]]/intensity << std::endl;
                  // std::cout <<  std::endl;

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
              double mass_error = 10;
              double candidate_IsoMd;

              // when candidate is inside of the mass range for iso step
              if (IsoMd_min - mzr < candidate_diff && IsoMd_max + mzr > candidate_diff) {

                // selects the best IsoMd for the candidate
                for (int j = 0; j < IsoMd_temp.size(); j++) {

                  if (abs(IsoMd_temp[j] - candidate_diff) < mass_error) {
                    mass_error = abs(IsoMd_temp[j] - candidate_diff);
                    candidate_IsoMd = IsoMd_temp[j];
                    is_iso_candidate = true;
                  }
                  // TODO evaluate the mass error for wrong assignement

                }
              }

              if (is_iso_candidate) {

                // relative intensity validation ///////////////////////////////////////////////////////////////
                ////////////////////////////////////////////////////////////////////////////////////////////////

                // TODO validate intensity based on elmass matched
                // if out of the boundaries continue to next feature without adding candidate to chain

                // compares the error and if lower update
                if (feat_iso_gr[chain_idx[f]] != 0) {
                  if (feat_iso_error[chain_idx[f]] < mass_error) {
                    continue;
                  }
                }

                hits.push_back(chain_mz[f]);
                n.push_back(iso);
                idx.push_back(chain_idx[f]);
                ints.push_back(chain_intensity[f] / intensity);
                diff.push_back(candidate_diff);
                elmass.push_back(candidate_IsoMd);
                error.push_back(mass_error);
              }
            }

            if (is_Mplus) break;

            // when there are hits, updates the iso step list
            if (hits.size() > 0) {
              iso_hits[iso] = hits;
              iso_n[iso] = n;
              iso_idx[iso] = idx;
              iso_ints[iso] = ints;
              iso_diff[iso] = diff;
              iso_elmass[iso] = elmass;
              iso_error[iso] = error;
            }
          } // loop for each feature in chain

          if (is_Mplus) break;
          // if (has_gap) continue;

          iso_hits_list[z] = iso_hits;
          iso_n_list[z] = iso_n;
          iso_idx_list[z] = iso_idx;
          iso_ints_list[z] = iso_ints;
          iso_diff_list[z] = iso_diff;
          iso_elmass_list[z] = iso_elmass;
          iso_error_list[z] = iso_error;

        } // loop for chain evaluation for each charge

        if (is_Mplus) continue;

        // list_out["iso_hits_list"] = iso_hits_list;
        // list_out["iso_diff_list"] = iso_diff_list;
        // list_out["iso_elmass_list"] = iso_elmass_list;
        // list_out["iso_error_list"] = iso_error_list;



        // Chain selection /////////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////////////////////////////

        int chain_length = 0;
        int pos_first_in_chain = 0;
        int charge_idx = 0;
        Rcpp::NumericVector final_chain;

        for (size_t z = 0; z < zvals.size(); z++) {
          const Rcpp::List& chain = iso_hits_list[z];
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

            // std::cout << std::endl;
            // std::cout << "Chain from different charges with the same length!!!" << std::endl;
            // std::cout << std::endl;

            // TODO or select based on lowest mass error
          }

          // list_out.push_back(final_chain);


        } // loop for chain selection for each charge



        // Chain description ///////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////////////////////////////

        if (final_chain.size() > 0) {
          const Rcpp::List& n = iso_n_list[charge_idx];
          const Rcpp::List& idx = iso_idx_list[charge_idx];
          const Rcpp::List& ints = iso_ints_list[charge_idx];
          const Rcpp::List& diff = iso_diff_list[charge_idx];
          const Rcpp::List& elmass = iso_elmass_list[charge_idx];
          const Rcpp::List& error = iso_error_list[charge_idx];
          Rcpp::NumericVector flat_n;
          Rcpp::NumericVector flat_idx;
          Rcpp::NumericVector flat_ints;
          Rcpp::NumericVector flat_diff;
          Rcpp::NumericVector flat_elmass;
          Rcpp::NumericVector flat_error;
          for (int iso = 0; iso < maxIsotopes + 1; ++iso) {
            const Rcpp::List& n_step = n[iso];
            const Rcpp::List& idx_step = idx[iso];
            const Rcpp::List& ints_step = ints[iso];
            const Rcpp::List& diff_step = diff[iso];
            const Rcpp::List& elmass_step = elmass[iso];
            const Rcpp::List& error_step = error[iso];

            for (int m = 0; m < n_step.size(); m++) {
              flat_n.push_back(n_step[m]);
              flat_idx.push_back(idx_step[m]);
              flat_ints.push_back(ints_step[m]);
              flat_diff.push_back(diff_step[m]);
              flat_elmass.push_back(elmass_step[m]);
              flat_error.push_back(error_step[m]);
            }
          }

          // list_out["n"] = flat_n;
          // list_out["chain"] = final_chain;
          // list_out["idx"] = flat_idx;
          // list_out["ints"] = flat_ints;
          // list_out["diff"] = flat_diff;
          // list_out["elmass"] = flat_elmass;
          // list_out["error"] = flat_error;


          // Validate chain by position //////////////////////////////////////////////////////////////////
          ////////////////////////////////////////////////////////////////////////////////////////////////




          // Write chain in output df when has isotopes //////////////////////////////////////////////////
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
                feat_iso_n[flat_idx[m]] = flat_n[m];
                // feat_iso_cat[flat_idx[m]] = "[M+" + n_step[m];
                // feat_iso_cat[flat_idx[m]] = feat_iso_cat[i] + "]";
                feat_iso_diff[flat_idx[m]] = flat_diff[m];
                feat_iso_elmass[flat_idx[m]] = flat_elmass[m];
                feat_iso_error[flat_idx[m]] = flat_error[m];
                feat_iso_int[flat_idx[m]] = flat_ints[m];
                feat_iso_feat[flat_idx[m]] = id;
                feat_iso_z[flat_idx[m]] = zvals[charge_idx];
              }
            }
          }
        } else { // if chain size is 0

          // list_out["chain"] = final_chain;

        } // if chain size is 0








      } else { // if only 1 feature


      } // if only 1 feature










    } else { // if iso group is already

    }


















  } // end main loop


  // std::vector<int> feat_iso_gr(number_of_features);
  // std::vector<std::string> feat_iso_cat(number_of_features);
  // std::vector<int> feat_iso_n(number_of_features);
  // std::vector<double> feat_iso_int(number_of_features);
  // std::vector<double> feat_iso_diff(number_of_features);
  // std::vector<double> feat_iso_elmass(number_of_features);
  // std::vector<double> feat_iso_error(number_of_features);
  // std::vector<double> feat_iso_feat(number_of_features);

  Rcpp::DataFrame output_df = Rcpp::DataFrame::create(
    Rcpp::Named("index") = all_idx,
    Rcpp::Named("feature") = all_ids,
    Rcpp::Named("mz") = all_mz,
    Rcpp::Named("rt") = all_rt,
    Rcpp::Named("intensity") = all_intensity,
    Rcpp::Named("iso_gr") = feat_iso_gr,
    Rcpp::Named("iso_z") = feat_iso_z,
    // Rcpp::Named("iso_cat") = feat_iso_cat,
    Rcpp::Named("iso_n") = feat_iso_n,
    Rcpp::Named("iso_rel") = feat_iso_int,
    Rcpp::Named("iso_diff") = feat_iso_diff,
    Rcpp::Named("iso_elmass") = feat_iso_elmass,
    Rcpp::Named("iso_error") = feat_iso_error,
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
//   // std::cout << "The offset is " << offset << std::endl;
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

// std::vector<double> iso_n(mass_diff_res.size());
// for (size_t v = 0; v < mass_diff_res.size(); v++) {
//   iso_n[v] = std::round(mass_diff_res[v] * 1) / 1;
// }


// std::cout << std::endl;
// std::cout << "Charge distance: " << std::endl;

// std::vector<double> exp_C_dist(zvals.size());
// std::vector<double> exp_N_dist(zvals.size());

// for (size_t z = 0; z < zvals.size(); z++) {
//   double C_step = C_diff / zvals[z];
//   // std::cout << "For z:" << z << "C dist: " << C_step << std::endl;
//   double N_step = N_diff / zvals[z];
//   // std::cout << "For z:" << z << "N dist: " << N_step << std::endl;

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
//           std::cout << "C13 with " << C_candidate_diff << std::endl;
//           isomat(z, col) = chain_mz[f];
//           found_hit = true;
//         }

//       } else {
//         double C_candidate_diff = abs(chain_mz[f] - C_candidate);
//         double N_candidate_diff = abs(chain_mz[f] - N_candidate);

//         if (C_candidate_diff <= 0.005) {
//           std::cout << "Iso n" << col << " with C error " << C_candidate_diff << std::endl;
//           isomat(z, col) = chain_mz[f];
//           found_hit = true;

//         } else if (N_candidate_diff <= 0.005) {
//           std::cout << "Iso n" << col << " with N error " << N_candidate_diff << std::endl;
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
