// clustering.cpp - Spectral clustering algorithms
// Implements m/z threshold clustering with presence filter

#include "clustering.h"
#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include <set>
#include <cmath>
#include <stdexcept>
#include <ostream>

namespace ms {
namespace clustering {

SpectraClusterOutput cluster_spectra(
    const SpectraClusterInput& input,
    double mzClust,
    double presence,
    bool verbose)
{
    const auto& all_unique_id = input.unique_id;
    const auto& all_analysis = input.analysis;
    const auto& all_polarity = input.polarity;
    const auto& all_id = input.id;
    const auto& all_rt = input.rt;
    const auto& all_mz = input.mz;
    const auto& all_intensity = input.intensity;
    
    const size_t n = all_unique_id.size();
    
    if (n == 0) {
        return SpectraClusterOutput{};
    }
    
    // Get unique IDs
    std::set<std::string> unique_ids_set(all_unique_id.begin(), all_unique_id.end());
    std::vector<std::string> unique_ids(unique_ids_set.begin(), unique_ids_set.end());
    
    SpectraClusterOutput output;
    output.spectra.resize(unique_ids.size());
    
    for (size_t i = 0; i < unique_ids.size(); ++i) {
        const std::string& target_id = unique_ids[i];
        
        if (verbose) {
            Rcpp::Rcout << "Clustering " << target_id << std::endl;
        }
        
        // Find indices for this unique_id
        std::vector<int> which_idx;
        for (size_t z = 0; z < n; ++z) {
            if (all_unique_id[z] == target_id) {
                which_idx.push_back(z);
            }
        }
        
        if (which_idx.empty()) continue;
        
        // Extract data for this ID
        std::vector<std::string> analysis(which_idx.size());
        std::vector<int> polarity(which_idx.size());
        std::vector<std::string> id(which_idx.size());
        std::vector<double> rt(which_idx.size());
        std::vector<double> mz(which_idx.size());
        std::vector<double> intensity(which_idx.size());
        
        for (size_t j = 0; j < which_idx.size(); ++j) {
            int idx = which_idx[j];
            analysis[j] = all_analysis[idx];
            polarity[j] = all_polarity[idx];
            id[j] = all_id[idx];
            rt[j] = all_rt[idx];
            mz[j] = all_mz[idx];
            intensity[j] = all_intensity[idx];
        }
        
        // Sort by m/z
        std::vector<int> idx(mz.size());
        std::iota(idx.begin(), idx.end(), 0);
        std::sort(idx.begin(), idx.end(), [&](int i, int j) {
            return mz[i] < mz[j];
        });
        
        std::vector<double> sorted_mz(mz.size());
        std::vector<double> sorted_intensity(mz.size());
        std::vector<double> sorted_rt(mz.size());
        
        for (size_t j = 0; j < mz.size(); ++j) {
            sorted_mz[j] = mz[idx[j]];
            sorted_intensity[j] = intensity[idx[j]];
            sorted_rt[j] = rt[idx[j]];
        }
        
        // Calculate m/z differences
        std::vector<double> mz_diff(mz.size() - 1);
        for (size_t j = 1; j < mz.size(); ++j) {
            mz_diff[j - 1] = sorted_mz[j] - sorted_mz[j - 1];
        }
        
        // Iterative clustering
        double itMzClust = mzClust;
        int counter = 0;
        bool hasFromSameScan = true;
        
        std::vector<double> new_mz;
        std::vector<double> new_intensity;
        
        while (hasFromSameScan && counter < 10) {
            counter++;
            new_mz.clear();
            new_intensity.clear();
            
            if (verbose) {
                Rcpp::Rcout << "Using an mzClust of " << itMzClust << " Da" << std::endl;
            }
            
            // Cluster by m/z threshold
            std::vector<int> all_clusters(mz_diff.size(), 0);
            for (size_t j = 0; j < mz_diff.size(); ++j) {
                if (mz_diff[j] > itMzClust) {
                    all_clusters[j] = 1;
                }
            }
            
            std::partial_sum(all_clusters.begin(), all_clusters.end(), all_clusters.begin());
            all_clusters.insert(all_clusters.begin(), 0);
            for (int& val : all_clusters) {
                val += 1;
            }
            
            // Get unique clusters
            std::set<int> clusters_set(all_clusters.begin(), all_clusters.end());
            std::vector<int> clusters(clusters_set.begin(), clusters_set.end());
            
            for (int cluster_id : clusters) {
                std::vector<int> temp_idx;
                for (size_t j = 0; j < all_clusters.size(); ++j) {
                    if (all_clusters[j] == cluster_id) {
                        temp_idx.push_back(j);
                    }
                }
                
                // Check presence filter
                std::set<double> unique_rt_set;
                for (int tidx : temp_idx) {
                    unique_rt_set.insert(sorted_rt[tidx]);
                }
                
                double presence_threshold = unique_rt_set.size() * presence;
                if (temp_idx.size() >= presence_threshold) {
                    // Find max intensity
                    double max_int = 0.0;
                    double weighted_mz = 0.0;
                    double total_int = 0.0;
                    
                    for (int tidx : temp_idx) {
                        if (sorted_intensity[tidx] > max_int) {
                            max_int = sorted_intensity[tidx];
                        }
                        weighted_mz += sorted_mz[tidx] * sorted_intensity[tidx];
                        total_int += sorted_intensity[tidx];
                    }
                    
                    new_intensity.push_back(max_int);
                    new_mz.push_back(total_int / (total_int + 1e-14));
                }
            }
            
            // Check if we need another iteration
            hasFromSameScan = (new_mz.size() == 0);
            if (hasFromSameScan) {
                itMzClust -= 0.0001;
            }
        }
        
        // Store result
        if (!new_mz.empty()) {
            output.spectra[i]["rt"] = {std::accumulate(sorted_rt.begin(), sorted_rt.end(), 0.0) / sorted_rt.size()};
            output.spectra[i]["mz"] = new_mz;
            output.spectra[i]["intensity"] = new_intensity;
            output.spectra[i]["polarity"] = std::vector<double>(1, static_cast<double>(polarity[0]));
            output.spectra[i]["analysis"] = std::vector<double>(1, 0.0); // Placeholder - analysis is string
            // Note: Full implementation would need proper string handling for analysis field
        }
    }
    
    return output;
}

} // namespace clustering
} // namespace ms
