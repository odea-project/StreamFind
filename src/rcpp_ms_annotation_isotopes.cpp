#include <iostream>
#include <vector>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List rcpp_ms_annotation_isotopes(Rcpp::DataFrame features, int maxIsotopes = 6, int maxCharge = 3) {

  Rcpp::List list_out;

  double N_diff = 0.9970349;
  double C_diff = 1.0033548378;

  std::vector<int> zvals(maxCharge);
  std::iota(zvals.begin(), zvals.end(), 1);

  int number_of_features = features.nrows();

  std::vector<std::string> ids = features["feature"];

  std::vector<double> all_rt = features["rt"];
  std::vector<double> all_rtmin = features["rtmin"];
  std::vector<double> all_rtmax = features["rtmax"];
  std::vector<double> all_mz = features["mz"];
  std::vector<double> all_mzmin = features["mzmin"];
  std::vector<double> all_mzmax = features["mzmax"];
  // NumericVector intensities = features["intensity"];

  std::vector<int> isogr(number_of_features);

  for (int i = 218; i < 219; ++i) {

    std::string id = ids[i];
    double mz = all_mz[i];

    std::cout << "Feature: " << id << std::endl;

    // not yet in an iso group, create chain of features
    if (isogr[i] == 0) {
      double rtmin = all_rtmin[i];
      double rtmax = all_rtmax[i];
      double mzmin = all_mzmin[i];
      double mzmax = all_mzmax[i];

      double mzr = mzmax - mzmin; // better to replace for the sd of the eic all_mz
      std::cout << "Mass deviation: " << mzr << std::endl;

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

        std::vector<int> fts_idx(number_chain_features);
        int* fts_idx_ptr = fts_idx.data();

        for (const int& x : idx) {
          *(chain_mz_ptr++) = *(org_mz_ptr2 + x);
          *(fts_idx_ptr++) = *(which_fts_ptr + x);
        }

        std::cout << "Charge distance: " << std::endl;

        std::vector<double> exp_C_dist(zvals.size());
        std::vector<double> exp_N_dist(zvals.size());

        Rcpp::NumericMatrix isomat(zvals.size(), maxIsotopes + 1);

        for (size_t z = 0; z < zvals.size(); z++) {
          double C_step = C_diff / zvals[z];
          std::cout << C_step << std::endl;

          double N_step = N_diff / zvals[z];
          std::cout << N_step << std::endl;

          double C_candidate = mz + C_step;
          double N_candidate = mz + N_step;

          isomat(z, 0) = mz;

          for (int col = 1; col < maxIsotopes + 1; col++) {

            for (int f = 1; f < number_chain_features; f++) {
              
              if (col == 1) {
                double C_canditate_error = chain_mz[f] - C_candidate;

                // matching c13 but need to valid intensity
                if (C_canditate_error <= mzr) {
                  isomat(z, col) = chain_mz[f];
                  C_candidate = chain_mz[f] + C_step;
                  N_candidate = chain_mz[f] + N_step;

                } else {
                  C_candidate = C_candidate + C_step;
                  N_candidate = C_candidate + N_step;
                }

              } else {
                double C_canditate_error = chain_mz[f] - C_candidate;
                double N_canditate_error = chain_mz[f] - N_candidate;

                if (C_canditate_error <= mzr || N_canditate_error <= mzr) {
                  isomat(z, col) = chain_mz[f];
                  C_candidate = chain_mz[f] + C_step;  
                  N_candidate = chain_mz[f] + N_step;

                } else {
                  C_candidate = C_candidate + C_step;
                  N_candidate = C_candidate + N_step;
                }

              }
              
              
              
              
              
              


            }

          }
        }

        

        list_out["isomat"] = isomat;
        



        


        


        // std::vector<double> expected_dist = isotope_diff / zvals;


        // std::cout << mz.size() << std::endl;

        // mz = all_mz[which_idx];

        std::cout << "Chain: " << std::endl;


        for (size_t z = 0; z < chain_mz.size(); ++z) {
          std::cout << which_fts[z] << " " << chain_mz[z] << " " << fts_idx[z] << std::endl;
        }


      // only 1 feature
      } else {



      }

      

    }

    








    








  }

  return list_out;
}
