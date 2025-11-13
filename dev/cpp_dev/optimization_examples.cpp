// Optimized version of key sections in rcpp_nts_find_features
// These are specific code improvements you can implement

#include <vector>
#include <unordered_map>
#include <execution>  // C++17 parallel algorithms
#include <algorithm>
#include <numeric>

// 1. MEMORY OPTIMIZATION: Pre-allocate vectors
void optimize_memory_allocation() {
    // Instead of letting vectors grow dynamically, estimate and pre-allocate
    
    // Estimate based on input size
    const size_t estimated_traces = idx_load.size() * 50; // Rough estimate
    
    // Pre-allocate all main vectors
    spec_rt.reserve(estimated_traces);
    spec_mz.reserve(estimated_traces);
    spec_intensity.reserve(estimated_traces);
    spec_noise.reserve(estimated_traces);
    
    // For temporary vectors in loops, also pre-allocate
    std::vector<float> clean_mz, clean_intensity, clean_noise;
    clean_mz.reserve(raw_n_traces);
    clean_intensity.reserve(raw_n_traces);
    clean_noise.reserve(raw_n_traces);
}

// 2. ALGORITHMIC OPTIMIZATION: Vectorized clustering
std::vector<int> optimized_clustering(const std::vector<float>& mz_values, 
                                     float slope, float intercept) {
    const size_t n = mz_values.size();
    std::vector<int> clusters(n);
    
    if (n == 0) return clusters;
    
    // Pre-calculate all thresholds at once (vectorized)
    std::vector<float> thresholds(n - 1);
    std::transform(mz_values.begin() + 1, mz_values.end(),
                   thresholds.begin(),
                   [slope, intercept](float mz) {
                       return calculate_mz_threshold_linear(mz, slope, intercept);
                   });
    
    // Calculate differences (vectorized)
    std::vector<float> diffs(n - 1);
    std::transform(mz_values.begin() + 1, mz_values.end(),
                   mz_values.begin(), diffs.begin(),
                   std::minus<float>());
    
    // Assign clusters in single pass
    clusters[0] = 0;
    int current_cluster = 0;
    
    for (size_t i = 0; i < diffs.size(); ++i) {
        if (diffs[i] > thresholds[i]) {
            ++current_cluster;
        }
        clusters[i + 1] = current_cluster;
    }
    
    return clusters;
}

// 3. DATA STRUCTURE OPTIMIZATION: Use structs for better cache locality
struct SpectraPoint {
    float rt, mz, intensity, noise;
    int cluster;
};

// Instead of separate vectors, use vector of structs for better cache performance
std::vector<SpectraPoint> spectra_data;

// 4. PARALLEL PROCESSING: Parallelize independent operations
void parallel_denoising(std::vector<SpectraPoint>& data, 
                       const std::vector<int>& idx_load,
                       sc::MS_FILE& ana) {
    
    // Process spectra in parallel
    #pragma omp parallel for schedule(dynamic)
    for (size_t i = 0; i < idx_load.size(); ++i) {
        // Each thread processes one spectrum
        process_single_spectrum(i, idx_load[i], ana, data);
    }
}

// 5. OPTIMIZED BASELINE CALCULATION: Sliding window minimum
std::vector<float> optimized_baseline_calculation(const std::vector<float>& intensities,
                                                 int window_size) {
    const int n = intensities.size();
    std::vector<float> baseline(n);
    
    // Use deque for efficient sliding window minimum
    std::deque<std::pair<float, int>> window;
    
    for (int i = 0; i < n; ++i) {
        // Remove elements outside current window
        while (!window.empty() && window.front().second < i - window_size) {
            window.pop_front();
        }
        
        // Maintain monotonic deque (minimum at front)
        while (!window.empty() && window.back().first >= intensities[i]) {
            window.pop_back();
        }
        
        window.push_back({intensities[i], i});
        baseline[i] = window.front().first;
    }
    
    return baseline;
}

// 6. OPTIMIZED SORTING: Use indices instead of copying data
template<typename T>
std::vector<size_t> get_sort_indices(const std::vector<T>& data) {
    std::vector<size_t> indices(data.size());
    std::iota(indices.begin(), indices.end(), 0);
    
    // Sort indices instead of copying data
    std::sort(std::execution::par_unseq,  // Parallel sorting
              indices.begin(), indices.end(),
              [&data](size_t a, size_t b) {
                  return data[a] < data[b];
              });
    
    return indices;
}

// 7. CACHE-FRIENDLY CLUSTER PROCESSING
void process_clusters_optimized(const std::unordered_map<int, std::vector<int>>& cluster_indices,
                               const std::vector<SpectraPoint>& data) {
    
    // Sort clusters by size (process larger clusters first for better load balancing)
    std::vector<std::pair<int, size_t>> cluster_sizes;
    cluster_sizes.reserve(cluster_indices.size());
    
    for (const auto& [cluster_id, indices] : cluster_indices) {
        cluster_sizes.emplace_back(cluster_id, indices.size());
    }
    
    std::sort(cluster_sizes.begin(), cluster_sizes.end(),
              [](const auto& a, const auto& b) { return a.second > b.second; });
    
    // Process in parallel with dynamic scheduling
    #pragma omp parallel for schedule(dynamic)
    for (size_t i = 0; i < cluster_sizes.size(); ++i) {
        int cluster_id = cluster_sizes[i].first;
        const auto& indices = cluster_indices.at(cluster_id);
        
        process_single_cluster(cluster_id, indices, data);
    }
}

// 8. MEMORY POOL FOR FREQUENT ALLOCATIONS
class MemoryPool {
private:
    std::vector<std::unique_ptr<std::vector<float>>> pool_;
    size_t next_available_ = 0;
    
public:
    std::vector<float>* get_vector(size_t size) {
        if (next_available_ < pool_.size()) {
            auto* vec = pool_[next_available_++].get();
            vec->clear();
            vec->reserve(size);
            return vec;
        } else {
            pool_.push_back(std::make_unique<std::vector<float>>());
            pool_.back()->reserve(size);
            return pool_.back().get();
        }
    }
    
    void reset() { next_available_ = 0; }
};

// 9. OPTIMIZED PEAK DETECTION: Use early termination
bool is_valid_peak(const std::vector<float>& intensities, 
                   const std::vector<float>& baseline,
                   int peak_idx, float min_snr) {
    
    // Quick checks first (most likely to fail)
    float apex_intensity = intensities[peak_idx];
    float apex_baseline = baseline[peak_idx];
    
    if (apex_intensity / apex_baseline < min_snr) {
        return false;  // Early termination
    }
    
    // More expensive checks only if needed
    return perform_detailed_peak_validation(intensities, baseline, peak_idx);
}

// 10. BATCH PROCESSING: Process multiple spectra at once
std::vector<std::vector<std::vector<float>>> batch_read_spectra(sc::MS_FILE& ana,
                                                               const std::vector<int>& indices) {
    // Read multiple spectra in one I/O operation
    return ana.get_spectra(indices);  // Assumes this is more efficient than individual reads
}

// Usage example showing how to integrate optimizations:
void optimized_rcpp_nts_find_features_section() {
    // 1. Pre-allocate memory
    optimize_memory_allocation();
    
    // 2. Use batch I/O
    auto all_spectra = batch_read_spectra(ana, idx_load);
    
    // 3. Use optimized data structures
    std::vector<SpectraPoint> spectra_data;
    spectra_data.reserve(estimated_total_points);
    
    // 4. Use memory pool for temporary allocations
    MemoryPool temp_memory_pool;
    
    // 5. Parallel processing where possible
    parallel_denoising(spectra_data, idx_load, ana);
    
    // 6. Optimized clustering
    auto sort_indices = get_sort_indices(extract_mz_values(spectra_data));
    auto clusters = optimized_clustering(mz_values, slope, intercept);
    
    // 7. Cache-friendly cluster processing
    process_clusters_optimized(cluster_indices, spectra_data);
}