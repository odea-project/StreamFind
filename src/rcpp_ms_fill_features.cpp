// #include <vector>
// #include <string>
// #include <Rcpp.h>
// #include <unordered_map>
// #include <algorithm>
// 
// #ifndef STREAMCRAFT_HEADER_ONLY
// #define STREAMCRAFT_HEADER_ONLY
// #endif
// 
// #include "StreamCraft/src/StreamCraft_lib.hpp"


// Rcpp::List rcpp_ms_fill_features(Rcpp::List analyses,
//                                  Rcpp::DataFrame features,
//                                  bool withinReplicate = false,
//                                  double rt_expand = 0,
//                                  double mz_expand = 0,
//                                  double minIntensity = 0) {
//   
//   Rcpp::List out;
//   
//   const int number_analyses = analyses.size();
//   
//   if (number_analyses == 0) return out;
//   
//   std::vector<std::string> analyses_names(number_analyses);
//   std::vector<std::vector<int>> analyses_polarities(number_analyses);
//   
//   for (int i = 0; i < number_analyses; i++) {
//     analyses_names[i] = analyses[i]["name"];
//     Rcpp::List headers = analyses[i]["spectra_headers"];
//     std::vector<int> pols = headers["polarity"];
//     std::set<int> pols_set(pols.begin(), pols.end());
//     std::vector<int> pols_unique(pols_set.begin(), pols_set.end());
//     analyses_polarities[i] = pols_unique;
//   }
//   
//   int max_presence = number_analyses;
//   
//   const int number_features = features.nrow();
//   
//   if (number_features == 0) return out;
//   
//   std::vector<std::string> features_cols = features.names();
//   
//   if (features_cols.size() == 0) return out;
//   
//   std::vector<std::string> must_have_names = {
//     "feature", "analysis", "rt", "rtmax", "rtmin",
//     "mz", "mzmax", "mzmin", "polarity", "intensity", "group"
//   };
//   
//   std::vector<bool> has_must_have_names(11, false);
//   
//   for (size_t i = 0; i < must_have_names.size(); ++i) {
//     for (size_t j = 0; j < features_cols.size(); ++j) {
//       if (must_have_names[i] == features_cols[j]) has_must_have_names[i] = true;
//     }
//   }
//   
//   for (bool value : has_must_have_names) {
//     if (!value) {
//       return out;
//     }
//   }
//   
//   const std::vector<std::string> fts_group = features["group"];
//   const std::vector<std::string> fts_replicates = features["replicate"];
//   const std::vector<std::string> fts_analysis = features["analysis"];
//   const std::vector<std::string> fts_polarity = features["polarity"];
//   const std::vector<float> fts_mass = features["mass"];
//   const std::vector<float> fts_mz = features["mz"];
//   const std::vector<float> fts_mzmin = features["mzmin"];
//   const std::vector<float> fts_mzmax = features["mzmax"];
//   const std::vector<float> fts_rt = features["rt"];
//   const std::vector<float> fts_rtmin = features["rtmin"];
//   const std::vector<float> fts_rtmax = features["rtmax"];
//   // const std::vector<float> fts_mobility = features["mobility"];
//   // const std::vector<float> fts_mobilitymin = features["mobilitymin"];
//   // const std::vector<float> fts_mobilitymax = features["mobilitymax"];
//   const std::vector<float> fts_intensity = features["intensity"];
//   
//   std::vector<std::string> fts_id = fts_group;
//   
//   if (withinReplicate) {
//     
//     std::unordered_map<std::string, int> rpl_sizes;
//     
//     for (int i = 0; i < number_analyses; i++) {
//       Rcpp::List analysis = analyses[i];
//       std::string rpl = analysis["replicate"];
//       rpl_sizes[rpl]++;
//     }
//     
//     for (int i = 0; i < number_features; i++) fts_id[i] = fts_id[i] + "_" + fts_replicates[i];
//     
//     max_presence = 0;
//     for (const auto& rpl : rpl_sizes) if (rpl.second > max_presence) max_presence = rpl.second;
//   }
//   
//   if (max_presence == 0) return out;
//   
//   std::unordered_map<std::string, int> id_presence;
//   for (int i = 0; i < number_features; i++) id_presence[fts_id[i]]++;
//   
//   std::cout << "max_presence: " << max_presence << std::endl;
//   std::cout << "fts_id size: " << fts_id.size() << std::endl;
//   std::cout << "id_presence size: " << id_presence.size() << std::endl;
//   
//   std::vector<std::string> id = fts_id;
//   
//   id.erase(
//     std::remove_if(id.begin(), id.end(), [&](const std::string& name) {
//       return id_presence[name] == max_presence;
//     }),
//     id.end()
//   );
//   
//   if (id.empty()) return out;
//   
//   std::cout << "id size: " << id.size() << std::endl;
//   
//   // for (const auto& i : id) {
//   //   std::cout << i << " " << id_presence[i] << std::endl;
//   // }
//   
//   std::set<std::string> id_set(id.begin(), id.end());
//   
//   std::vector<std::string> unique_id(id_set.begin(), id_set.end());
//   
//   std::cout << "unique id size: " << unique_id.size() << std::endl;
//   
//   std::vector<sc::MS_TARGETS> ana_targets(number_analyses);
//   
//   for (int k = 0; k < number_features; k++) {
//     
//     if (std::find(unique_id.begin(), unique_id.end(), fts_id[k]) == unique_id.end()) continue;
//     
//     for (int j = 0; j < number_analyses; j++) {
//       
//       if (analyses_names[j] != fts_analysis[k]) {
//         
//         std::vector<int> pols = analyses_polarities[j];
//         
//         for (size_t p = 0; p < pols.size(); p++) {
//           float mz = fts_mass[k] + (pols[p] * 1.007276);
//           float mzmin = mz - (fts_mz[k] - fts_mzmin[k]) - mz_expand;
//           float mzmax = mz + (fts_mzmax[k] - fts_mz[k]) + mz_expand;
//           
//           int n_targets = ana_targets[j].size() - 1;
//           ana_targets[j].index.push_back(n_targets + 1);
//           ana_targets[j].id.push_back(fts_id[k]);
//           ana_targets[j].level.push_back(1);
//           ana_targets[j].polarity.push_back(pols[p]);
//           ana_targets[j].precursor.push_back(false);
//           ana_targets[j].mzmin.push_back(mzmin);
//           ana_targets[j].mzmax.push_back(mzmax);
//           ana_targets[j].rtmin.push_back(fts_rtmin[k] - rt_expand);
//           ana_targets[j].rtmax.push_back(fts_rtmax[k] + rt_expand);
//           ana_targets[j].mobilitymin.push_back(0);
//           ana_targets[j].mobilitymax.push_back(0);
//         }
//       }
//     }
//   }
//   
//   for (int j = 0; j < number_analyses; j++) {
//     Rcpp::List out_targets;
//     out_targets["id"] = ana_targets[j].id;
//     out_targets["level"] = ana_targets[j].level;
//     out_targets["polarity"] = ana_targets[j].polarity;
//     out_targets["precursor"] = ana_targets[j].precursor;
//     out_targets["mzmin"] = ana_targets[j].mzmin;
//     out_targets["mzmax"] = ana_targets[j].mzmax;
//     out_targets["rtmin"] = ana_targets[j].rtmin;
//     out_targets["rtmax"] = ana_targets[j].rtmax;
//     out_targets["mobilitymin"] = ana_targets[j].mobilitymin;
//     out_targets["mobilitymax"] = ana_targets[j].mobilitymax;
//     out_targets.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
//     out.push_back(out_targets);
//   }
// 
//   return out;
// }