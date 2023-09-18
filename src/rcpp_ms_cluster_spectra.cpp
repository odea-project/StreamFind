#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <Rcpp.h>




// [[Rcpp::export]]
Rcpp::List rcpp_ms_cluster_spectra(Rcpp::DataFrame spectra,
                                   double mzClust = 0.005,
                                   double presence = 0.8,
                                   bool verbose = false) {
  
  const std::vector<std::string>& names_spectra = spectra.names();
  
  const std::vector<std::string> must_have_names = {
    "unique_id", "analysis", "polarity", "id", "rt", "mz", "intensity"
  };
  
  std::vector<bool> has_must_have_names(7, false);
  
  bool has_pre_ce = false;
  
  bool has_pre_mz = false;
  
  for (size_t i = 0; i < must_have_names.size(); ++i) {
    for (size_t j = 0; j < names_spectra.size(); ++j) {
      if (must_have_names[i] == names_spectra[j]) has_must_have_names[i] = true;
      if (names_spectra[j] == "pre_ce") has_pre_ce = true;
      if (names_spectra[j] == "pre_mz") has_pre_mz = true;
    }
  }
  
  for (bool value : has_must_have_names) {
    if (!value) {
      throw std::runtime_error("The DataFrame must have the columns unique_id, analysis, polarity, id, rt, pre_mz, mz and intensity!");
    }
  }
  
  const std::vector<std::string>& all_unique_id = spectra["unique_id"];
  
  const std::vector<std::string>& all_analysis = spectra["analysis"];
  const std::string* all_analysis_ptr = all_analysis.data();
  
  const std::vector<int>& all_polarity = spectra["polarity"];
  const int* all_polarity_ptr = all_polarity.data();
  
  const std::vector<std::string>& all_id = spectra["id"];
  const std::string* all_id_ptr = all_id.data();
  
  const std::vector<double>& all_rt = spectra["rt"];
  const double* all_rt_ptr = all_rt.data();
  
  const std::vector<double>& all_mz = spectra["mz"];
  const double* all_mz_ptr = all_mz.data();
  
  const std::vector<double>& all_intensity = spectra["intensity"];
  const double* all_intensity_ptr = all_intensity.data();
  
  std::set<std::string> unique_ids_set;
  
  for (const std::string& id : all_unique_id) {
    unique_ids_set.insert(id);
  }
  
  std::vector<std::string> unique_ids(unique_ids_set.begin(), unique_ids_set.end());
  
  const int n_all_ids = unique_ids.size();
  
  const int n_unique_ids = all_unique_id.size();
  
  std::string* target_id;
  
  double* all_pre_ce_ptr;
  
  if (has_pre_ce) {
    std::vector<double> all_pre_ce = spectra["pre_ce"];
    all_pre_ce_ptr = all_pre_ce.data();
    
  } else {
    double nan_val = std::nan(""); 
    all_pre_ce_ptr = &nan_val;
  }
  
  double* all_pre_mz_ptr;
  
  if (has_pre_mz) {
    std::vector<double> all_pre_mz = spectra["pre_mz"];
    all_pre_mz_ptr = all_pre_mz.data();
    
  } else {
    double nan_val = std::nan(""); 
    all_pre_mz_ptr = &nan_val;
  }

  Rcpp::List spectra_out(n_all_ids);
  
  
  for (int i=0; i<n_all_ids; ++i) {
    
    target_id = &unique_ids[i];
    
    std::vector<int> which_idx;
    
    for (int z=0; z<n_unique_ids; z++) {
      if (all_unique_id[z] == *target_id) which_idx.push_back(z);
    }
    
    const int n_idx = which_idx.size();
    
    std::vector<std::string> analysis(n_idx);
    std::string* analysis_ptr = analysis.data();
    
    std::vector<int> polarity(n_idx);
    int* polarity_ptr = polarity.data();
    
    std::vector<std::string> id(n_idx);
    std::string* id_ptr = id.data();
    
    std::vector<double> u_rt(n_idx);
    double* u_rt_ptr = u_rt.data();
    
    std::vector<double> u_pre_mz(n_idx);
    double* u_pre_mz_ptr = u_pre_mz.data();
    
    std::vector<double> u_pre_ce(n_idx);
    double* u_pre_ce_ptr = u_pre_ce.data();
    
    std::vector<double> u_mz(n_idx);
    double* u_mz_ptr = u_mz.data();
    
    std::vector<double> u_intensity(n_idx);
    double* u_intensity_ptr = u_intensity.data();

    for (const int& x : which_idx) {
      *(analysis_ptr++) = *(all_analysis_ptr + x);
      *(polarity_ptr++) = *(all_polarity_ptr + x);
      *(id_ptr++) = *(all_id_ptr + x);
      *(u_rt_ptr++) = *(all_rt_ptr + x);
      *(u_mz_ptr++) = *(all_mz_ptr + x);
      *(u_intensity_ptr++) = *(all_intensity_ptr + x);
      
      if (has_pre_ce) {
        *(u_pre_ce_ptr++) = *(all_pre_ce_ptr + x);
      } else {
        *(u_pre_ce_ptr++) = *(all_pre_ce_ptr);
      }
      
      if (has_pre_mz) {
        *(u_pre_mz_ptr++) = *(all_pre_mz_ptr + x);
      } else {
        *(u_pre_mz_ptr++) = *(all_pre_mz_ptr);
      }
    }
    
    // list_out["u_mz"] = u_mz;
    
    if (u_mz.size() > 0) {
      
      std::vector<int> idx(u_mz.size());
      std::iota(idx.begin(), idx.end(), 0);
      
      // list_out["idx"] = idx;
      
      std::sort(idx.begin(), idx.end(), [&](int i, int j){return u_mz[i] < u_mz[j];});
      
      // list_out["o_idx"] = idx;
      
      u_rt_ptr = u_rt.data();
      u_pre_mz_ptr = u_pre_mz.data();
      u_pre_ce_ptr = u_pre_ce.data();
      u_mz_ptr = u_mz.data();
      u_intensity_ptr = u_intensity.data();
      
      std::vector<double> rt(n_idx);
      double* rt_ptr = rt.data();
      
      std::vector<double> pre_ce(n_idx);
      double* pre_ce_ptr = pre_ce.data();
      
      std::vector<double> mz(n_idx);
      double* mz_ptr = mz.data();
      
      std::vector<double> intensity(n_idx);
      double* intensity_ptr = intensity.data();

      for (const int& x : idx) {
        *(rt_ptr++) = *(u_rt_ptr + x);
        *(mz_ptr++) = *(u_mz_ptr + x);
        *(intensity_ptr++) = *(u_intensity_ptr + x);
        *(pre_ce_ptr++) = *(u_pre_ce_ptr + x);
      }
      
      // list_out["mz"] = mz;
      
      // list_out["ce"] = pre_ce;
      
      std::set<double> unique_rt_set;
      
      for (const double& r : rt) {
        unique_rt_set.insert(r);
      }
      
      std::vector<double> unique_rt(unique_rt_set.begin(), unique_rt_set.end());
      
      std::set<double> unique_pre_ce_set;
      
      for (const double& c : pre_ce) {
        unique_pre_ce_set.insert(c);
      }
      
      std::vector<double> unique_pre_ce(unique_pre_ce_set.begin(), unique_pre_ce_set.end());
      
      // list_out["unique_rt"] = unique_rt;

      // list_out["unique_pre_ce"] = unique_pre_ce;
      
      rt_ptr = rt.data();
      pre_ce_ptr = pre_ce.data();
      mz_ptr = mz.data();
      intensity_ptr = intensity.data();
      
      std::vector<double> mz_diff(n_idx - 1);
      
      for (int j = 1; j < n_idx; ++j) {
        mz_diff[j - 1] = mz[j] - mz[j - 1];
      }
      
      // list_out["mz_diff"] = mz_diff;
      
      // list_out["mz_diff_size"] = mz_diff.size();
      
      double itMzClust = mzClust;

      int counter = 0;
      
      bool hasFromSameScan = true;
      
      
      
      
      while (hasFromSameScan) {

        counter = counter + 1;
        
        std::vector<double> new_mz;
        
        std::vector<double> new_intensity;

        if (verbose) Rcpp::Rcout << "Clustering " << *target_id << " with " << itMzClust << " Da...";

        std::vector<int> all_clusters(mz_diff.size(), 0);
        
        for (size_t j = 0; j < mz_diff.size(); ++j) {
          if (mz_diff[j] > itMzClust) all_clusters[j] = 1;
        }
        
        std::partial_sum(all_clusters.begin(), all_clusters.end(), all_clusters.begin());
        
        all_clusters.insert(all_clusters.begin(), 0);
        
        // list_out["all_clusters_raw"] = all_clusters;
        
        for (int &val : all_clusters) {
          val += 1;
        }
        
        int n_all_clusters = all_clusters.size();
        
        // list_out["all_clusters"] = all_clusters;

        std::vector<int> idx_clusters(all_clusters.size());
        std::iota(idx_clusters.begin(), idx_clusters.end(), 0);
        
        std::set<int> clusters_set;
        
        for (const int& cl : all_clusters) {
          clusters_set.insert(cl);
        }
        
        std::vector<int> clusters(clusters_set.begin(), clusters_set.end());
        
        int n_clusters = clusters.size();
        
        // list_out["clusters"] = clusters;
        
        std::vector<bool> fromSameScan(n_clusters, true);
        
        for (int z=0; z<n_clusters; ++z) {
          
          // list_out["cluster"] = clusters[z];
          
          std::vector<int> temp_idx;
          
          for (int j=0; j<n_all_clusters; ++j) {
            if (all_clusters[j] == clusters[z]) temp_idx.push_back(j);
          }
          
          int n = temp_idx.size();
          
          // list_out["n"] = n;
          
          std::vector<double> temp_rt(n);
          double* temp_rt_ptr = temp_rt.data();
          
          std::vector<double> temp_pre_ce(n);
          double* temp_pre_ce_ptr = temp_pre_ce.data();
          
          std::vector<double> temp_mz(n);
          double* temp_mz_ptr = temp_mz.data();
          
          std::vector<double> temp_intensity(n);
          double* temp_intensity_ptr = temp_intensity.data();
          
          for (const int& x : temp_idx) {
            *(temp_rt_ptr++) = *(rt_ptr + x);
            *(temp_pre_ce_ptr++) = *(pre_ce_ptr + x);
            *(temp_mz_ptr++) = *(mz_ptr + x);
            *(temp_intensity_ptr++) = *(intensity_ptr + x);
          }
          
          // list_out["temp_rt"] = temp_rt;
          
          std::set<double> unique_temp_rt_set;
          
          for (const double& r : temp_rt) {
            unique_temp_rt_set.insert(r);
          }
          
          std::vector<double> unique_temp_rt(unique_temp_rt_set.begin(), unique_temp_rt_set.end());
          
          // list_out["unique_temp_rt"] = unique_temp_rt;
          
          std::set<double> unique_temp_pre_ce_set;
          
          for (const double& r : temp_pre_ce) {
            unique_temp_pre_ce_set.insert(r);
          }
          
          std::vector<double> unique_temp_pre_ce(unique_temp_pre_ce_set.begin(), unique_temp_pre_ce_set.end());
          
          // list_out["unique_temp_pre_ce"] = unique_temp_pre_ce;

          fromSameScan[z] = unique_temp_rt.size() < temp_rt.size();
          
          if (counter > 10) fromSameScan[z] = false;
          
          if (itMzClust < 0.0001) fromSameScan[z] = false;
          
          // when traces are twice in a given scan breaks the loop when decreases the itMzClust
          if (fromSameScan[z]) {
            
            if (verbose) {
              auto min_mz = std::min_element(temp_mz.begin(), temp_mz.end());
              auto max_mz = std::max_element(temp_mz.begin(), temp_mz.end());
              Rcpp::Rcout << "\n The m/z cluster " << *min_mz << " to " << *max_mz << " of " << *target_id << " has traces from the same scan at:\n";
              for (const double& r : temp_rt) {
                Rcpp::Rcout << r << " ";
              }
              Rcpp::Rcout << "\n";
              for (const double& m : temp_mz) {
                Rcpp::Rcout << m << " ";
              }
              Rcpp::Rcout << "\n\n";
            }
            
            itMzClust = itMzClust - 0.0001;
            
            break;
          }
          
          bool pre_ceHasNaN = false;
          
          for (const double& val : unique_pre_ce) {
            if (std::isnan(val)) {
              pre_ceHasNaN = true;
              break;
            }
          }
          
          bool enough_presence = false;
          
          if (pre_ceHasNaN) {
            enough_presence = unique_rt.size() * presence <= unique_temp_rt.size();
            
          } else {
            // case when a trace is only present in a CE and not in others applied
            if (unique_temp_pre_ce.size() < unique_pre_ce.size()) {
              enough_presence = unique_rt.size() * (unique_temp_pre_ce.size() / unique_pre_ce.size()) * presence <= unique_temp_rt.size();
              
            } else {
              enough_presence = unique_rt.size() * presence <= unique_temp_rt.size();
            }
          }
          
          // when is not enough present skips the m/z cluster
          if (!enough_presence) continue;
          
          auto max_intensity_ptr = std::max_element(temp_intensity.begin(), temp_intensity.end());
          new_intensity.push_back(*max_intensity_ptr);
          
          int size_temp_mz = temp_mz.size();
          
          float mz_sum = 0, mz_numWeight = 0;
          
          for (int w = 0; w < size_temp_mz; w++) {
            mz_numWeight = mz_numWeight + temp_mz[w] * temp_intensity[w];
            mz_sum = mz_sum + temp_intensity[w];
          }
          
          double mean_mz = mz_numWeight / mz_sum;
          
          new_mz.push_back(mean_mz);
          
        } // end of clusters for loop
        
        if (new_mz.size() > 0) {

          double rt_mean = 0;
          
          for (double val : rt) {
            rt_mean += val;
          }
          
          rt_mean = rt_mean / rt.size();
          
          bool pre_mzHasNaN = false;
          
          for (const double& val : u_pre_mz) {
            if (std::isnan(val)) {
              pre_mzHasNaN = true;
              break;
            }
          }
          
          if (has_pre_mz & !pre_mzHasNaN) {
            
            double pre_mz_mean = 0;
            
            std::vector<bool> is_pre(new_mz.size(), false);
            
            for (double val : u_pre_mz) {
              pre_mz_mean += val;
            }
            
            pre_mz_mean = pre_mz_mean / u_pre_mz.size();
            
            // list_out["pre_mz"] = pre_mz_mean;
            
            if (!std::isnan(pre_mz_mean)) {
              for (size_t p = 0; p < new_mz.size(); p++) {
                if ((new_mz[p] >= pre_mz_mean - mzClust) && (new_mz[p] <= pre_mz_mean + mzClust)) {
                  is_pre[p] = true;
                }
              }
            }
            
            spectra_out[i] = Rcpp::DataFrame::create(
              Rcpp::Named("analysis") = analysis[0],
                Rcpp::Named("id") = id[0],
                Rcpp::Named("polarity") = polarity[0],
                Rcpp::Named("pre_mz") = pre_mz_mean,
                Rcpp::Named("rt") = rt_mean,
                Rcpp::Named("mz") = new_mz,
                Rcpp::Named("intensity") = new_intensity,
                Rcpp::Named("is_pre") = is_pre
            );
            
          } else {
            spectra_out[i] = Rcpp::DataFrame::create(
              Rcpp::Named("analysis") = analysis[0],
              Rcpp::Named("id") = id[0],
              Rcpp::Named("polarity") = polarity[0],
              Rcpp::Named("rt") = rt_mean,
              Rcpp::Named("mz") = new_mz,
              Rcpp::Named("intensity") = new_intensity
            );
            
            
            
          }
        } else {
          spectra_out[i] = Rcpp::DataFrame::create();
        }
        
        hasFromSameScan = false;
        
        for (const bool& l : fromSameScan) {
          if (l) {
            hasFromSameScan = true;
            break;
          }
        }
        
        if (!hasFromSameScan) if (verbose) Rcpp::Rcout << "Done! \n\n";

      } // end of while loop
      
    } // if u_mz is not empty

  } // end main for loop
  
  return(spectra_out);
}
