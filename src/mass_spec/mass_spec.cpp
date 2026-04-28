// mass_spec.cpp — implementations of ms:: algorithms
// C++17, no Rcpp dependency.

#include "mass_spec.h"
#include <cstring>
#include <stdexcept>
#include <map>
#include <unordered_map>
#include <cassert>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <limits>

// ============================================================================
// MARK: internal helpers
// ============================================================================

namespace {

/// Reflect index at boundaries (mirror-padding).
static inline int reflect_idx(int i, int n) {
    if (n <= 1) return 0;
    if (i < 0)  return -i;
    if (i >= n) return 2 * n - i - 2;
    return i;
}

/// Trapezoidal area between two consecutive points (no baseline subtraction).
static inline double trap(double x0, double y0, double x1, double y1) {
    return 0.5 * (y0 + y1) * (x1 - x0);
}

} // anonymous namespace

// ============================================================================
// MARK: calculate_spectra_charges
// ============================================================================

std::vector<ms::ChargeRow> ms::calculate_spectra_charges(
    const std::vector<SpectrumPoint>& pts,
    int   polarity,
    double round_val,
    double rel_low_cut,
    double abs_low_cut,
    int    top_charges)
{
    if (pts.empty()) return {};

    // Base-peak intensity
    double base_int = 0.0;
    for (auto& p : pts) base_int = std::max(base_int, p.intensity);
    double int_cut = std::max(abs_low_cut, rel_low_cut * base_int);

    // Cluster by round(mz / round_val) * round_val
    // Use a map to accumulate sum intensity per cluster
    std::map<double, std::pair<double,double>> cluster_map; // cluster_mz → (sum_int, sum_mzmz)
    std::map<double, int> cluster_cnt;
    for (auto& p : pts) {
        if (round_val <= 0) round_val = 1.0;
        double ckey = std::round(p.mz / round_val) * round_val;
        cluster_map[ckey].first  += p.intensity;
        cluster_map[ckey].second += p.mz * p.intensity;
        cluster_cnt[ckey]++;
    }

    // Build cluster list: (cluster_mz, mean_mz_weighted, sum_intensity)
    struct Cluster { double c_mz; double w_mz; double sum_int; };
    std::vector<Cluster> clusters;
    clusters.reserve(cluster_map.size());
    for (auto& kv : cluster_map) {
        double sum_int = kv.second.first;
        if (sum_int < int_cut) continue;
        double w_mz = kv.second.second / sum_int;
        clusters.push_back({kv.first, w_mz, sum_int});
    }
    if (clusters.size() < 2) return {};

    // Sort by m/z ascending
    std::sort(clusters.begin(), clusters.end(), [](const Cluster& a, const Cluster& b){
        return a.w_mz < b.w_mz;
    });

    const int nc = (int)clusters.size();

    // For each cluster i, compute z_left (from left neighbour) and z_right
    // z = mz_i / (mz_i - mz_{i-1}) for left  →  same denominator as charge spacing
    // z = -mz_i / (mz_{i+1} - mz_i) for right  (using proton mass approximately)
    // More precisely: expected spacing for charge z is ~1/z Da,
    //   so z ≈ 1 / |mz_i - mz_{i-1}|  (rounded to nearest integer)
    std::vector<int> z_arr(nc, 0);
    for (int i = 0; i < nc; ++i) {
        double z_left = 0.0, z_right = 0.0;
        if (i > 0) {
            double delta = clusters[i].w_mz - clusters[i - 1].w_mz;
            if (delta > 0.0) z_left  = 1.0 / delta;
        }
        if (i < nc - 1) {
            double delta = clusters[i + 1].w_mz - clusters[i].w_mz;
            if (delta > 0.0) z_right = 1.0 / delta;
        }
        int zl = (z_left  > 0.5) ? (int)std::round(z_left)  : 0;
        int zr = (z_right > 0.5) ? (int)std::round(z_right) : 0;
        // Take the more reliable (larger coverage = higher-intensity neighbour)
        if (i == 0)       z_arr[i] = zr;
        else if (i==nc-1) z_arr[i] = zl;
        else z_arr[i] = (clusters[i-1].sum_int >= clusters[i+1].sum_int) ? zl : zr;
    }

    // Pick top_charges most-intense clusters as "anchor" candidates
    std::vector<int> by_int(nc);
    std::iota(by_int.begin(), by_int.end(), 0);
    std::sort(by_int.begin(), by_int.end(), [&](int a, int b){
        return clusters[a].sum_int > clusters[b].sum_int;
    });

    int n_cand = std::min(top_charges, nc);

    // For each anchor, try to build a consistent charge series and score it.
    // We use the anchor's z as the reference charge.
    // "score" = number of consistent charges matching anchor_z ±1 within mass tolerance.
    std::vector<ChargeRow> best;
    int best_score = -1;

    for (int ci = 0; ci < n_cand; ++ci) {
        int anchor = by_int[ci];
        int z_ref  = (z_arr[anchor] > 0) ? z_arr[anchor] : 1;

        std::vector<ChargeRow> rows;
        int score = 0;
        for (int k = 0; k < nc; ++k) {
            int zk = (z_arr[k] > 0) ? z_arr[k] : z_ref;
            if (zk < 1) zk = 1;
            double mass = (double)zk * (clusters[k].w_mz - 1.007276);
            if (polarity < 0) mass = (double)zk * (clusters[k].w_mz + 1.007276);
            if (std::abs(zk - z_ref) <= 1) ++score;
            rows.push_back({clusters[k].w_mz, clusters[k].sum_int,
                            clusters[k].c_mz, zk, mass, polarity});
        }
        if (score > best_score) {
            best_score = score;
            best = rows;
        }
    }

    return best;
}


// ============================================================================
// MARK: cluster_masses
// ============================================================================

std::vector<ms::MassPoint> ms::cluster_masses(
    const std::vector<MassPoint>& pts,
    double clust_val)
{
    if (pts.empty()) return {};

    // Sort by mass
    std::vector<MassPoint> sorted = pts;
    std::sort(sorted.begin(), sorted.end(), [](const MassPoint& a, const MassPoint& b){
        return a.mass < b.mass;
    });

    std::vector<MassPoint> result;
    int i = 0;
    const int m = (int)sorted.size();

    while (i < m) {
        // Collect cluster: all points within clust_val of sorted[i].mass
        double start_mass = sorted[i].mass;
        double sum_int    = 0.0;
        double sum_mass_w = 0.0;

        int j = i;
        while (j < m && sorted[j].mass - start_mass <= clust_val) {
            sum_int    += sorted[j].intensity;
            sum_mass_w += sorted[j].mass * sorted[j].intensity;
            ++j;
        }
        double avg_mass = (sum_int > 0) ? sum_mass_w / sum_int : start_mass;
        result.push_back({avg_mass, sum_int});
        i = j;
    }

    return result;
}


// ============================================================================
// MARK: deconvolute_spectrum
// ============================================================================

std::vector<ms::MassPoint> ms::deconvolute_spectrum(
    const std::vector<SpectrumPoint>& spectrum_pts,
    const std::vector<ChargeRow>&     charges,
    double clust_val,
    double window)
{
    if (spectrum_pts.empty() || charges.empty()) return {};

    std::vector<MassPoint> all_mass_pts;
    all_mass_pts.reserve(charges.size() * 10);

    for (auto& ch : charges) {
        if (ch.z < 1) continue;
        double mz_lo = ch.mz - window;
        double mz_hi = ch.mz + window;

        for (auto& sp : spectrum_pts) {
            if (sp.mz < mz_lo) continue;
            if (sp.mz > mz_hi) break;
            double mass = (double)ch.z * (sp.mz - 1.007276);
            if (ch.polarity < 0) mass = (double)ch.z * (sp.mz + 1.007276);
            all_mass_pts.push_back({mass, sp.intensity});
        }
    }

    if (all_mass_pts.empty()) return {};
    return cluster_masses(all_mass_pts, clust_val);
}
