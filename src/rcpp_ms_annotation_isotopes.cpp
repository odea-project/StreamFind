#include <iostream>
#include <vector>
#include <string>
#include <tuple>
#include <algorithm>
#include <unordered_set>
#include <numeric>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_ms_annotation_isotopes(Rcpp::DataFrame features, int maxIsotopes = 6, int maxCharge = 3) {

  Rcpp::List list_out;

  int number_of_features = features.nrows();
  std::vector<std::string> ids = features["feature"];
  std::vector<double> all_rt = features["rt"];
  std::vector<double> all_rtmin = features["rtmin"];
  std::vector<double> all_rtmax = features["rtmax"];
  std::vector<double> all_mz = features["mz"];
  std::vector<double> all_mzmin = features["mzmin"];
  std::vector<double> all_mzmax = features["mzmax"];
  std::vector<double> all_intensity = features["intensity"];

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
  ////////////////////////////////////////////////////////////////////////////////////////////////

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
    1.0033548378, // C
    // 1.0062767, // H
    0.9970349, // N
    1.0042169, // O
    2.004246, //O2
    0.9993878, // S
    1.995796, // S2
    // 3.99501, // S3
    1.9970499, // Cl
    1.9979534 // Br  
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
  list_out["el_md_key"] = el_md_key;

  Rcpp::NumericVector el_ab_key = Rcpp::wrap(iso_ab);
  el_ab_key.names() = iso_elements;
  list_out["el_ab_key"] = el_ab_key;

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
  
  list_out["CbEl"] = CbEl_list;
  list_out["CbMd"] = CbMd_list;
  list_out["CbAb"] = CbAb_list;
  list_out["IsoMd"] = IsoMd;





  // merging based on resolution or mass deviation ///////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////


  double multiplier = std::pow(10.0, decimal_numbers);

  std::vector<double> IsoMd_merged(IsoMd.size());

  for (size_t i = 0; i < IsoMd.size(); i++) {
    IsoMd_merged[i] = std::round(IsoMd[i] * multiplier) / multiplier;
  }

  std::sort(IsoMd_merged.begin(), IsoMd_merged.end());
  auto last = std::unique(IsoMd_merged.begin(), IsoMd_merged.end());
  IsoMd_merged.erase(last, IsoMd_merged.end());

  Rcpp::IntegerVector IsoStep(IsoMd_merged.size());
  for (size_t i = 0; i < IsoMd_merged.size(); i++) {
    IsoStep[i] = std::round(IsoMd_merged[i] * 1) / 1;
  }

  list_out["IsoMd_merged"] = IsoMd_merged;






  // Other preparations //////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  std::vector<int> zvals(maxCharge);
  std::iota(zvals.begin(), zvals.end(), 1);

  int iso_gr = 1;
  std::vector<int> feat_iso_gr(number_of_features);


  





  // Main loop ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////

  for (int i = 218; i < 219; ++i) {

    std::string id = ids[i];
    double mz = all_mz[i];

    std::cout << "Feature: " << id << std::endl;

    // not yet in an iso group, create chain of features
    if (feat_iso_gr[i] == 0) {
      double rtmin = all_rtmin[i];
      double rtmax = all_rtmax[i];
      double mzmin = all_mzmin[i];
      double mzmax = all_mzmax[i];

      // Perhaps the rtmin and rtmax can be shrinked based on central rt

      double maxisomz = (mz + maxIsotopes) * 1.05;

      std::vector<int> which_fts;
      for (int z = 0; z < number_of_features; ++z) {
        if (all_rt[z] >= rtmin && all_rt[z] <= rtmax && all_mz[z] >= mz && all_mz[z] <= maxisomz) {
          which_fts.push_back(z);
        }
      }

      int number_chain_features = which_fts.size();
      std::cout << "Number of features in chain: " << number_chain_features << std::endl;

      // if more than 1 feature is present for chain
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

        const int* which_fts_ptr = which_fts.data();

        std::vector<int> chain_idx(number_chain_features);
        int* chain_idx_ptr = chain_idx.data();

        std::vector<int> chain_intensity(number_chain_features);
        int* chain_intensity_ptr = chain_intensity.data();

        for (const int& x : idx) {
          *(chain_mz_ptr++) = *(org_mz_ptr2 + x);
          *(chain_idx_ptr++) = *(which_fts_ptr + x);
          *(chain_intensity_ptr++) =  all_intensity[*(which_fts_ptr + x)];
        }

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

        Rcpp::List iso_list(maxCharge);

        for (size_t z = 0; z < zvals.size(); z++) {
          std::cout << "Charge: " << zvals[z] << std::endl;

          // Make chains based on iso position wihch is the rounded unit of the mass_diff
          // acccording to the defined max isos
          // A position can have more than 1 hit for higher resolutions
          // charge is validated by position
          // make a container for filling hits and write the in the output
          // gaps are avoided by unfilled position and the actual mass diff is stored to the output

          // isomat(z, 0) = mz;

          Rcpp::List iso_hits(maxIsotopes + 1);
          iso_hits[0] = mz;

          for (int iso = 1; iso < maxIsotopes + 1; ++iso) {

            Rcpp::LogicalVector iso_filter = IsoStep == iso;
            Rcpp::NumericVector IsoMd_temp = Rcpp::wrap(IsoMd_merged);
            IsoMd_temp = IsoMd_temp[iso_filter];
            IsoMd_temp = IsoMd_temp / zvals[z];

            Rcpp::NumericVector hits;

            for (int f = 1; f < number_chain_features; ++f) {
              double canditate_diff = chain_mz[f] - mz;
              double candidate_diff_min = canditate_diff - mzr_mean;
              double candidate_diff_max = canditate_diff + mzr_mean;

              // std::cout << std::endl;
              // std::cout << "Candidate: " << candidate_diff_min << " to " << candidate_diff_max << std::endl;

              bool is_iso_candidate = false;
              double mass_error = 10;
              for (size_t j = 0; j < IsoMd_temp.size(); j++) {
                if (IsoMd_temp[j] > candidate_diff_min && IsoMd_temp[j] < candidate_diff_max) {
                  if (abs(IsoMd_temp[j] - canditate_diff) < mass_error) {
                    mass_error = abs(IsoMd_temp[j] - canditate_diff);
                    is_iso_candidate = true;
                  }
                }
              }

              if (is_iso_candidate) {
                
                hits.push_back(chain_mz[f]);
              }
            }

            iso_hits[iso] = hits;
          }
          
          iso_list[z] = iso_hits;
        }

        list_out["iso_list"] = iso_list;



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
        //         double C_canditate_diff = abs(chain_mz[f] - C_candidate);

        //         // matching c13 but need to validate intensity
        //         if (C_canditate_diff <= 0.005) {
        //           std::cout << "C13 with " << C_canditate_diff << std::endl;
        //           isomat(z, col) = chain_mz[f];
        //           found_hit = true;
        //         }

        //       } else {
        //         double C_canditate_diff = abs(chain_mz[f] - C_candidate);
        //         double N_canditate_diff = abs(chain_mz[f] - N_candidate);

        //         if (C_canditate_diff <= 0.005) {
        //           std::cout << "Iso n" << col << " with C error " << C_canditate_diff << std::endl;
        //           isomat(z, col) = chain_mz[f];
        //           found_hit = true;

        //         } else if (N_canditate_diff <= 0.005) {
        //           std::cout << "Iso n" << col << " with N error " << N_canditate_diff << std::endl;
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

        

        
        



        


        


        // std::vector<double> expected_dist = isotope_diff / zvals;


        // std::cout << mz.size() << std::endl;

        // mz = all_mz[which_idx];

      // only 1 feature
      } else {



      }

      

    }

    








    








  }

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
