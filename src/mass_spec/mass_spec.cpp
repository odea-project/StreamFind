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


#if 0
// ============================================================================
// MARK: whittaker_smooth
// ============================================================================

namespace ms {
namespace processing {

std::vector<double> whittaker_smooth(
    const std::vector<double>& y,
    const std::vector<double>& w,
    double lambda,
    int differences)
{
    const int n = (int)y.size();
    if (n < 2) return y;
    if (differences < 1) differences = 1;
    if (differences >= n) differences = n - 1;

    const int bw = differences;
    const int nd = n - differences;   // rows of difference matrix D

    // Build D'D band (upper triangular stored as A_band[j][k] = A[j][j+k]).
    // Also accumulate W on diagonal.
    // For row r of D (0..nd-1):
    //   D[r, c] = binom[c-r] * (-1)^{c-r}  for c = r..r+differences
    // (D'D)[i][j] = sum_r D[r,i]*D[r,j]  →  only ii<=jj branch (upper)
    std::vector<int> binom = binomial_row(differences);

    // A_band[j][k] = A[j][j+k], k = 0..bw
    std::vector<std::vector<double>> A_band(n, std::vector<double>(bw + 1, 0.0));

    // Weights on diagonal
    for (int i = 0; i < n; ++i)
        A_band[i][0] += w[i];

    // Accumulate lambda * D'D  (only upper triangle)
    for (int r = 0; r < nd; ++r) {
        for (int ii = r; ii <= r + differences; ++ii) {
            const int si = ii - r;
            const double vi = binom[si] * (si % 2 == 0 ? 1.0 : -1.0);
            for (int jj = ii; jj <= r + differences; ++jj) {
                const int sj = jj - r;
                const double vj = binom[sj] * (sj % 2 == 0 ? 1.0 : -1.0);
                A_band[ii][jj - ii] += lambda * vi * vj;
            }
        }
    }

    // Banded Cholesky factorisation (in-place, overwrites A_band).
    // After factorisation: A_band[j][0] = L[j][j],
    //                      A_band[j][k] = L[j+k][j]  (for k >= 1).
    for (int j = 0; j < n; ++j) {
        double s = A_band[j][0];
        const int lo = std::max(0, j - bw);
        for (int k = lo; k < j; ++k) {
            const double val = A_band[k][j - k];   // L[j][k]
            s -= val * val;
        }
        if (s < 1e-14) s = 1e-14;
        A_band[j][0] = std::sqrt(s);

        for (int dk = 1; dk <= bw && j + dk < n; ++dk) {
            double s2 = A_band[j][dk];   // A[j+dk][j] = A[j][j+dk] (symmetric)
            for (int k = lo; k < j; ++k) {
                const int ok1 = j - k;       // L[j][k]       stored at A_band[k][j-k]
                const int ok2 = j + dk - k;  // L[j+dk][k]    stored at A_band[k][j+dk-k]
                if (ok1 <= bw && ok2 <= bw)
                    s2 -= A_band[k][ok1] * A_band[k][ok2];
            }
            A_band[j][dk] = s2 / A_band[j][0];
        }
    }

    // RHS: b[i] = w[i] * y[i]
    std::vector<double> b(n);
    for (int i = 0; i < n; ++i) b[i] = w[i] * y[i];

    // Forward substitution: L * p = b
    std::vector<double> p(n);
    for (int i = 0; i < n; ++i) {
        double s = b[i];
        for (int k = std::max(0, i - bw); k < i; ++k)
            s -= A_band[k][i - k] * p[k];  // L[i][k]
        p[i] = s / A_band[i][0];
    }

    // Back substitution: L^T * z = p
    std::vector<double> z(n);
    for (int i = n - 1; i >= 0; --i) {
        double s = p[i];
        for (int k = i + 1; k < std::min(n, i + bw + 1); ++k)
            s -= A_band[i][k - i] * z[k];  // L^T[i][k] = L[k][i] = A_band[i][k-i]
        z[i] = s / A_band[i][0];
    }

    return z;
}


// ============================================================================
// MARK: airpls_baseline
// ============================================================================

std::vector<double> airpls_baseline(
    const std::vector<double>& y,
    double lambda,
    int differences,
    int itermax)
{
    const int n = (int)y.size();
    if (n < 2) return y;

    std::vector<double> w(n, 1.0);
    std::vector<double> z = y;

    // Total absolute signal (used for convergence)
    double sum_abs_y = 0.0;
    for (double v : y) sum_abs_y += std::abs(v);
    if (sum_abs_y < 1e-14) return std::vector<double>(n, 0.0);

    for (int iter = 1; iter <= itermax; ++iter) {
        z = whittaker_smooth(y, w, lambda, differences);

        // Compute negative residuals d = y - z
        double sum_neg = 0.0;
        double max_neg = 0.0;
        for (int i = 0; i < n; ++i) {
            double d = y[i] - z[i];
            if (d < 0.0) {
                sum_neg += std::abs(d);
                if (std::abs(d) > max_neg) max_neg = std::abs(d);
            }
        }

        // Convergence test
        if (sum_neg < 0.001 * sum_abs_y) break;

        // Update weights
        for (int i = 0; i < n; ++i) {
            double d = y[i] - z[i];
            if (d >= 0.0) {
                w[i] = 0.0;
            } else {
                w[i] = std::exp((double)iter * std::abs(d) / sum_neg);
                if (w[i] > 1e6) w[i] = 1e6;
            }
        }
        // Pin boundary weights
        const double bnd_val = std::exp((double)iter * max_neg / sum_neg);
        w[0]     = (bnd_val < 1e6) ? bnd_val : 1e6;
        w[n - 1] = w[0];
    }

    return z;
}


// ============================================================================
// MARK: als_baseline
// ============================================================================

std::vector<double> als_baseline(
    const std::vector<double>& y,
    double lambda,
    double p,
    int maxit)
{
    const int n = (int)y.size();
    if (n < 2) return y;

    std::vector<double> w(n, 1.0);
    std::vector<double> z = y;

    for (int iter = 0; iter < maxit; ++iter) {
        z = whittaker_smooth(y, w, lambda, 2);

        bool converged = true;
        for (int i = 0; i < n; ++i) {
            double w_new = (y[i] > z[i]) ? p : (1.0 - p);
            if (std::abs(w_new - w[i]) > 1e-6) converged = false;
            w[i] = w_new;
        }
        if (converged) break;
    }

    return z;
}


// ============================================================================
// MARK: moving_average
// ============================================================================

std::vector<double> moving_average(
    const std::vector<double>& y,
    int window_size)
{
    const int n = (int)y.size();
    if (n == 0 || window_size <= 1) return y;

    if (window_size % 2 == 0) window_size++;  // force odd
    int hw = window_size / 2;

    std::vector<double> out(n, 0.0);
    for (int i = 0; i < n; ++i) {
        double sum = 0.0;
        int cnt    = 0;
        for (int j = i - hw; j <= i + hw; ++j) {
            int idx = reflect_idx(j, n);
            sum += y[idx];
            ++cnt;
        }
        out[i] = sum / cnt;
    }
    return out;
}


// ============================================================================
// MARK: savitzky_golay
// ============================================================================

std::vector<double> savitzky_golay(
    const std::vector<double>& y,
    int fl,
    int forder,
    int dorder)
{
    const int n = (int)y.size();
    if (n == 0) return y;

    // Sanitise parameters
    if (fl % 2 == 0) fl++;
    if (fl < 3)  fl = 3;
    if (fl > n)  fl = (n % 2 == 0) ? n - 1 : n;
    if (forder >= fl) forder = fl - 1;
    if (forder < 1)   forder = 1;
    if (dorder > forder) dorder = forder;
    if (dorder < 0)      dorder = 0;

    const int hw = fl / 2;
    const int m  = forder + 1;  // number of polynomial coefficients

    // Vandermonde matrix A: A[k][j] = (k - hw)^j,  k=0..fl-1, j=0..forder
    std::vector<std::vector<double>> A(fl, std::vector<double>(m));
    for (int k = 0; k < fl; ++k) {
        double t = (double)(k - hw);
        A[k][0] = 1.0;
        for (int j = 1; j < m; ++j) A[k][j] = A[k][j - 1] * t;
    }

    // A^T A  (m × m symmetric)
    std::vector<std::vector<double>> ATA(m, std::vector<double>(m, 0.0));
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < m; ++j)
            for (int k = 0; k < fl; ++k)
                ATA[i][j] += A[k][i] * A[k][j];

    // Invert ATA via Gauss-Jordan on augmented [ATA | I]
    std::vector<std::vector<double>> aug  = ATA;
    std::vector<std::vector<double>> inv(m, std::vector<double>(m, 0.0));
    for (int i = 0; i < m; ++i) inv[i][i] = 1.0;

    for (int col = 0; col < m; ++col) {
        // Pivot
        int pivot = col;
        for (int k = col + 1; k < m; ++k)
            if (std::abs(aug[k][col]) > std::abs(aug[pivot][col])) pivot = k;
        std::swap(aug[col], aug[pivot]);
        std::swap(inv[col], inv[pivot]);
        double diag = aug[col][col];
        if (std::abs(diag) < 1e-14) continue;
        for (int j = 0; j < m; ++j) { aug[col][j] /= diag; inv[col][j] /= diag; }
        for (int row = 0; row < m; ++row) {
            if (row == col) continue;
            double f = aug[row][col];
            for (int j = 0; j < m; ++j) {
                aug[row][j] -= f * aug[col][j];
                inv[row][j] -= f * inv[col][j];
            }
        }
    }

    // Filter coefficients: h[k] = dorder! * A[k,:] . inv[:,dorder]
    double dorder_fact = 1.0;
    for (int d = 1; d <= dorder; ++d) dorder_fact *= d;

    std::vector<double> h(fl, 0.0);
    for (int k = 0; k < fl; ++k)
        for (int j = 0; j < m; ++j)
            h[k] += A[k][j] * inv[j][dorder];
    for (int k = 0; k < fl; ++k) h[k] *= dorder_fact;

    // Convolve with mirror-padding at boundaries
    std::vector<double> out(n, 0.0);
    for (int i = 0; i < n; ++i) {
        double s = 0.0;
        for (int k = 0; k < fl; ++k)
            s += h[k] * y[reflect_idx(i - hw + k, n)];
        out[i] = s;
    }
    return out;
}


// ============================================================================
// MARK: integrate_peak
// ============================================================================

double integrate_peak(
    const std::vector<double>& x,
    const std::vector<double>& y,
    double x_start,
    double x_end)
{
    const int n = (int)x.size();
    if (n < 2 || x_start >= x_end) return 0.0;

    // Find boundary indices
    int i0 = 0, i1 = n - 1;
    for (int i = 0; i < n; ++i) { if (x[i] >= x_start) { i0 = i; break; } }
    for (int i = n - 1; i >= 0; --i) { if (x[i] <= x_end) { i1 = i; break; } }
    if (i0 > i1) return 0.0;

    // Linear baseline connecting (x[i0], y[i0]) to (x[i1], y[i1])
    double slope = (i1 > i0) ? (y[i1] - y[i0]) / (x[i1] - x[i0]) : 0.0;

    double area = 0.0;
    for (int i = i0; i < i1; ++i) {
        double base0 = y[i0] + slope * (x[i]     - x[i0]);
        double base1 = y[i0] + slope * (x[i + 1] - x[i0]);
        double sig0  = y[i]     - base0;
        double sig1  = y[i + 1] - base1;
        area += 0.5 * (sig0 + sig1) * (x[i + 1] - x[i]);
    }
    return std::max(0.0, area);
}


// ============================================================================
// MARK: find_peaks_local_maxima
// ============================================================================

std::vector<Peak> find_peaks_local_maxima(
    const std::vector<double>& x,
    const std::vector<double>& y,
    double min_height,
    double min_width,
    double max_width)
{
    const int n = (int)x.size();
    if (n < 3) return {};

    std::vector<Peak> peaks;

    for (int i = 1; i < n - 1; ++i) {
        if (y[i] <= y[i - 1] || y[i] <= y[i + 1]) continue;
        if (y[i] < min_height) continue;

        // Walk left to find valley (first ascending from left)
        int left = i;
        while (left > 0 && y[left - 1] <= y[left]) --left;

        // Walk right to find valley
        int right = i;
        while (right < n - 1 && y[right + 1] <= y[right]) ++right;

        double w = x[right] - x[left];
        if (min_width > 0 && w < min_width) continue;
        if (max_width > 0 && w > max_width) continue;

        Peak pk;
        pk.idx       = i;
        pk.x_val     = x[i];
        pk.x_min     = x[left];
        pk.x_max     = x[right];
        pk.intensity = y[i];
        pk.width     = w;
        pk.area      = integrate_peak(x, y, x[left], x[right]);

        // S/N: peak / max of 4-window just outside boundaries
        double base_sig = 0.0;
        {
            int bl0 = std::max(0, left - 2);
            int bl1 = std::min(n - 1, left + 1);
            for (int k = bl0; k <= bl1; ++k) base_sig = std::max(base_sig, y[k]);
            int br0 = std::max(0, right - 1);
            int br1 = std::min(n - 1, right + 2);
            for (int k = br0; k <= br1; ++k) base_sig = std::max(base_sig, y[k]);
        }
        pk.sn = (base_sig > 1e-10) ? y[i] / base_sig : y[i];

        peaks.push_back(pk);
    }

    return peaks;
}


// ============================================================================
// MARK: find_peaks_pracma
// ============================================================================

std::vector<Peak> find_peaks_pracma(
    const std::vector<double>& x,
    const std::vector<double>& y,
    bool   merge,
    double close_by_threshold,
    double min_peak_height,
    double min_peak_distance,
    double min_peak_width,
    double max_peak_width,
    double min_sn)
{
    const int n = (int)x.size();
    if (n < 3) return {};

    // 1. Find all local maxima
    std::vector<int> apex_idx;
    for (int i = 1; i < n - 1; ++i)
        if (y[i] > y[i - 1] && y[i] > y[i + 1] && y[i] >= min_peak_height)
            apex_idx.push_back(i);

    if (apex_idx.empty()) return {};

    // 2. For each apex, find left/right boundaries (local minima)
    struct RawPeak { int apex; int left; int right; };
    std::vector<RawPeak> raw;
    raw.reserve(apex_idx.size());

    for (int ai : apex_idx) {
        int left = ai;
        while (left > 0 && y[left - 1] <= y[left]) --left;
        int right = ai;
        while (right < n - 1 && y[right + 1] <= y[right]) ++right;
        raw.push_back({ai, left, right});
    }

    // 3. Optional merge: peaks closer than close_by_threshold (in x units)
    if (merge && close_by_threshold > 0.0) {
        bool changed = true;
        while (changed) {
            changed = false;
            for (int i = 0; i + 1 < (int)raw.size(); ++i) {
                if (raw[i].apex < 0) continue;
                for (int j = i + 1; j < (int)raw.size(); ++j) {
                    if (raw[j].apex < 0) continue;
                    double dist = x[raw[j].apex] - x[raw[i].apex];
                    if (dist <= 0.0) dist = -dist;
                    if (dist <= close_by_threshold) {
                        // Keep higher-intensity apex
                        if (y[raw[j].apex] > y[raw[i].apex]) {
                            raw[i].apex = -1;  // mark for removal
                        } else {
                            raw[j].apex = -1;
                        }
                        changed = true;
                        break;
                    }
                }
                if (changed) break;
            }
        }
        // Remove merged
        std::vector<RawPeak> kept;
        for (auto& rp : raw)
            if (rp.apex >= 0) kept.push_back(rp);
        raw = kept;
    }

    // 4. Filter by minpeakdistance
    if (min_peak_distance > 0.0 && raw.size() > 1) {
        // Sort by intensity descending, greedily keep
        std::sort(raw.begin(), raw.end(), [&](const RawPeak& a, const RawPeak& b) {
            return y[a.apex] > y[b.apex];
        });
        std::vector<bool> keep(raw.size(), true);
        for (int i = 0; i < (int)raw.size(); ++i) {
            if (!keep[i]) continue;
            for (int j = i + 1; j < (int)raw.size(); ++j) {
                if (!keep[j]) continue;
                if (std::abs(x[raw[j].apex] - x[raw[i].apex]) < min_peak_distance)
                    keep[j] = false;
            }
        }
        std::vector<RawPeak> kept;
        for (int i = 0; i < (int)raw.size(); ++i)
            if (keep[i]) kept.push_back(raw[i]);
        // Re-sort by peak position
        std::sort(kept.begin(), kept.end(),
                  [](const RawPeak& a, const RawPeak& b){ return a.apex < b.apex; });
        raw = kept;
    }

    // 5. Build Peak structs, apply width / S/N filters
    std::vector<Peak> peaks;
    peaks.reserve(raw.size());

    for (auto& rp : raw) {
        double w = x[rp.right] - x[rp.left];
        if (min_peak_width > 0.0 && w < min_peak_width) continue;
        if (max_peak_width > 0.0 && w > max_peak_width) continue;

        // S/N
        double base_sig = 0.0;
        {
            int bl0 = std::max(0, rp.left - 2);
            int bl1 = std::min(n - 1, rp.left + 1);
            for (int k = bl0; k <= bl1; ++k) base_sig = std::max(base_sig, y[k]);
            int br0 = std::max(0, rp.right - 1);
            int br1 = std::min(n - 1, rp.right + 2);
            for (int k = br0; k <= br1; ++k) base_sig = std::max(base_sig, y[k]);
        }
        double sn = (base_sig > 1e-10) ? y[rp.apex] / base_sig : y[rp.apex];
        if (sn < min_sn) continue;

        Peak pk;
        pk.idx       = rp.apex;
        pk.x_val     = x[rp.apex];
        pk.x_min     = x[rp.left];
        pk.x_max     = x[rp.right];
        pk.intensity = y[rp.apex];
        pk.width     = w;
        pk.area      = integrate_peak(x, y, x[rp.left], x[rp.right]);
        pk.sn        = sn;

        peaks.push_back(pk);
    }

    return peaks;
}


// ============================================================================
// MARK: OLS
// ============================================================================

OLSResult ols_fit(
    const std::vector<double>& x_vals,
    const std::vector<double>& y_vals,
    int degree)
{
    OLSResult result;
    result.success   = false;
    result.r_squared = 0.0;

    const int n = (int)x_vals.size();
    if (n < 2 || (int)y_vals.size() != n || degree < 1 || degree >= n) {
        result.coefficients.assign(degree + 1, std::numeric_limits<double>::quiet_NaN());
        return result;
    }

    const int m = degree + 1;

    // Build A (n × m) Vandermonde
    std::vector<std::vector<double>> A(n, std::vector<double>(m));
    for (int i = 0; i < n; ++i) {
        A[i][0] = 1.0;
        for (int j = 1; j < m; ++j) A[i][j] = A[i][j - 1] * x_vals[i];
    }

    // A^T A  (m × m)
    std::vector<std::vector<double>> ATA(m, std::vector<double>(m, 0.0));
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < m; ++j)
            for (int k = 0; k < n; ++k)
                ATA[i][j] += A[k][i] * A[k][j];

    // A^T y  (m-vector)
    std::vector<double> ATy(m, 0.0);
    for (int i = 0; i < m; ++i)
        for (int k = 0; k < n; ++k)
            ATy[i] += A[k][i] * y_vals[k];

    // Solve ATA * c = ATy via Gaussian elimination with partial pivoting
    // Augment [ATA | ATy]
    std::vector<std::vector<double>> aug(m, std::vector<double>(m + 1));
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < m; ++j) aug[i][j] = ATA[i][j];
        aug[i][m] = ATy[i];
    }
    for (int col = 0; col < m; ++col) {
        int pivot = col;
        for (int k = col + 1; k < m; ++k)
            if (std::abs(aug[k][col]) > std::abs(aug[pivot][col])) pivot = k;
        std::swap(aug[col], aug[pivot]);
        double diag = aug[col][col];
        if (std::abs(diag) < 1e-14) return result;  // singular
        for (int j = col; j <= m; ++j) aug[col][j] /= diag;
        for (int row = 0; row < m; ++row) {
            if (row == col) continue;
            double f = aug[row][col];
            for (int j = col; j <= m; ++j) aug[row][j] -= f * aug[col][j];
        }
    }

    std::vector<double> c(m);
    for (int i = 0; i < m; ++i) c[i] = aug[i][m];

    // Compute R²
    double y_mean = 0.0;
    for (double v : y_vals) y_mean += v;
    y_mean /= n;

    double ss_tot = 0.0, ss_res = 0.0;
    for (int i = 0; i < n; ++i) {
        double y_pred = 0.0;
        for (int j = 0; j < m; ++j) y_pred += c[j] * std::pow(x_vals[i], j);
        ss_res += (y_vals[i] - y_pred) * (y_vals[i] - y_pred);
        ss_tot += (y_vals[i] - y_mean) * (y_vals[i] - y_mean);
    }
    result.r_squared = (ss_tot > 1e-14) ? 1.0 - ss_res / ss_tot : 1.0;

    result.coefficients = c;
    result.success = true;
    return result;
}

double ols_predict(const OLSResult& model, double x) {
    double val = 0.0;
    const int m = (int)model.coefficients.size();
    double xpow = 1.0;
    for (int j = 0; j < m; ++j) {
        val  += model.coefficients[j] * xpow;
        xpow *= x;
    }
    return val;
}


} // namespace processing
} // namespace ms
#endif


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
