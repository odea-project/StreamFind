// processing.cpp - Signal processing algorithms for mass spectrometry
// Implements smoothing, baseline correction, and peak finding

#include "processing.h"
#include <cstring>
#include <stdexcept>
#include <map>
#include <unordered_map>
#include <cassert>
#include <cmath>
#include <algorithm>

namespace mass_spec {
namespace processing {

// ============================================================================
// MARK: internal helpers
// ============================================================================

namespace {

/// Reflect index at boundaries (mirror-padding).
static inline int reflect_idx(int i, int n) {
    if (n <= 1) return 0;
    if (i < 0) return -i;
    if (i >= n) return 2 * n - i - 2;
    return i;
}

/// Trapezoidal area between two consecutive points (no baseline subtraction).
static inline double trap(double x0, double y0, double x1, double y1) {
    return 0.5 * (y0 + y1) * (x1 - x0);
}

/// Compute binomial coefficients C(n, k) for small n via Pascal.
std::vector<int> binomial_row(int n) {
    std::vector<int> b(n + 1, 0);
    b[0] = 1;
    for (int k = 1; k <= n; ++k)
        b[k] = b[k - 1] * (n - k + 1) / k;
    return b;
}

} // anonymous namespace

// ============================================================================
// MARK: whittaker_smooth
// ============================================================================

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
    const int nd = n - differences; // rows of difference matrix D
    
    // Build D'D band (upper triangular stored as A_band[j][k] = A[j][j+k]).
    // Also accumulate W on diagonal.
    std::vector<int> binom = binomial_row(differences);
    
    // A_band[j][k] = A[j][j+k], k = 0..bw
    std::vector<std::vector<double>> A_band(n, std::vector<double>(bw + 1, 0.0));
    
    // Weights on diagonal
    for (int i = 0; i < n; ++i)
        A_band[i][0] += w[i];
    
    // Accumulate lambda * D'D (only upper triangle)
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
    
    // Banded Cholesky factorisation
    for (int j = 0; j < n; ++j) {
        double s = A_band[j][0];
        const int lo = std::max(0, j - bw);
        for (int k = lo; k < j; ++k) {
            const double val = A_band[k][j - k];
            s -= val * val;
        }
        if (s < 1e-14) s = 1e-14;
        A_band[j][0] = std::sqrt(s);
        
        for (int dk = 1; dk <= bw && j + dk < n; ++dk) {
            double s2 = A_band[j][dk];
            for (int k = lo; k < j; ++k) {
                const int ok1 = j - k;
                const int ok2 = j + dk - k;
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
            s -= A_band[k][i - k] * p[k];
        p[i] = s / A_band[i][0];
    }
    
    // Back substitution: L^T * z = p
    std::vector<double> z(n);
    for (int i = n - 1; i >= 0; --i) {
        double s = p[i];
        for (int k = i + 1; k < std::min(n, i + bw + 1); ++k)
            s -= A_band[i][k - i] * z[k];
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
    
    double sum_abs_y = 0.0;
    for (double v : y) sum_abs_y += std::abs(v);
    if (sum_abs_y < 1e-14) return std::vector<double>(n, 0.0);
    
    for (int iter = 1; iter <= itermax; ++iter) {
        z = whittaker_smooth(y, w, lambda, differences);
        
        double sum_neg = 0.0;
        double max_neg = 0.0;
        for (int i = 0; i < n; ++i) {
            double d = y[i] - z[i];
            if (d < 0.0) {
                sum_neg += std::abs(d);
                if (std::abs(d) > max_neg) max_neg = std::abs(d);
            }
        }
        
        if (sum_neg < 0.001 * sum_abs_y) break;
        
        for (int i = 0; i < n; ++i) {
            double d = y[i] - z[i];
            if (d >= 0.0) {
                w[i] = 0.0;
            } else {
                w[i] = std::exp((double)iter * std::abs(d) / sum_neg);
                if (w[i] > 1e6) w[i] = 1e6;
            }
        }
        const double bnd_val = std::exp((double)iter * max_neg / sum_neg);
        w[0] = (bnd_val < 1e6) ? bnd_val : 1e6;
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
    std::vector<double> z(n);
    
    for (int iter = 0; iter < maxit; ++iter) {
        z = whittaker_smooth(y, w, lambda, 2);
        
        bool converged = true;
        for (int i = 0; i < n; ++i) {
            bool cond = (y[i] > z[i]);
            double new_w = cond ? p : (1.0 - p);
            if (w[i] != new_w) converged = false;
            w[i] = new_w;
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
    if (n == 0) return {};
    if (window_size < 1) window_size = 1;
    
    std::vector<double> result(n);
    int hw = window_size / 2;
    
    for (int i = 0; i < n; ++i) {
        double sum = 0.0;
        int count = 0;
        for (int j = -hw; j <= hw; ++j) {
            int idx = reflect_idx(i + j, n);
            sum += y[idx];
            count++;
        }
        result[i] = sum / count;
    }
    
    return result;
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
    if (n == 0) return {};
    if (fl % 2 == 0) fl += 1;
    if (fl > n) fl = n;
    if (fl < 3) fl = 3;
    
    int hw = fl / 2;
    std::vector<double> result(n);
    
    // Build convolution coefficients
    std::vector<std::vector<double>> A(fl, std::vector<double>(forder + 1));
    for (int i = 0; i < fl; ++i) {
        double x = i - hw;
        for (int j = 0; j <= forder; ++j) {
            A[i][j] = std::pow(x, j);
        }
    }
    
    // Solve (A'A)^-1 A' for convolution coefficients
    std::vector<std::vector<double>> AtA(forder + 1, std::vector<double>(forder + 1, 0.0));
    for (int j = 0; j <= forder; ++j) {
        for (int k = 0; k <= forder; ++k) {
            for (int i = 0; i < fl; ++i) {
                AtA[j][k] += A[i][j] * A[i][k];
            }
        }
    }
    
    // Invert AtA using Gauss-Jordan
    std::vector<std::vector<double>> invAtA(forder + 1, std::vector<double>(forder + 1, 0.0));
    for (int i = 0; i <= forder; ++i) invAtA[i][i] = 1.0;
    
    for (int k = 0; k <= forder; ++k) {
        double pivot = AtA[k][k];
        if (std::abs(pivot) < 1e-14) continue;
        for (int j = 0; j <= forder; ++j) {
            AtA[k][j] /= pivot;
            invAtA[k][j] /= pivot;
        }
        for (int i = 0; i <= forder; ++i) {
            if (i != k) {
                double factor = AtA[i][k];
                for (int j = 0; j <= forder; ++j) {
                    AtA[i][j] -= factor * AtA[k][j];
                    invAtA[i][j] -= factor * invAtA[k][j];
                }
            }
        }
    }
    
    // Convolution coefficients for derivative
    std::vector<double> conv(fl);
    for (int i = 0; i < fl; ++i) {
        conv[i] = 0.0;
        for (int j = 0; j <= forder; ++j) {
            conv[i] += invAtA[dorder][j] * A[i][j];
        }
        conv[i] *= std::tgamma(dorder + 1);
    }
    
    // Apply convolution
    for (int i = 0; i < n; ++i) {
        result[i] = 0.0;
        for (int j = -hw; j <= hw; ++j) {
            int idx = reflect_idx(i + j, n);
            result[i] += conv[j + hw] * y[idx];
        }
    }
    
    return result;
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
    if (x.size() != y.size() || x.size() < 2) return 0.0;
    
    double area = 0.0;
    for (size_t i = 1; i < x.size(); ++i) {
        double x0 = x[i - 1], x1 = x[i];
        double y0 = y[i - 1], y1 = y[i];
        
        if (x1 <= x_start || x0 >= x_end) continue;
        
        // Clip to integration bounds
        if (x0 < x_start) {
            double t = (x_start - x[0]) / (x[1] - x[0]);
            y0 = y[0] + t * (y[1] - y[0]);
            x0 = x_start;
        }
        if (x1 > x_end) {
            double t = (x_end - x[0]) / (x[1] - x[0]);
            y1 = y[0] + t * (y[1] - y[0]);
            x1 = x_end;
        }
        
        area += trap(x0, y0, x1, y1);
    }
    
    return area;
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
        
        // Walk left to find boundary
        int left = i;
        while (left > 0 && y[left - 1] <= y[left]) left--;
        while (left > 0 && y[left - 1] < y[left]) left--;
        
        // Walk right to find boundary
        int right = i;
        while (right < n - 1 && y[right + 1] <= y[right]) right++;
        while (right < n - 1 && y[right + 1] < y[right]) right++;
        
        double width = x[right] - x[left];
        if (width < min_width || (max_width > 0 && width > max_width)) continue;
        
        // Calculate area
        double area = 0.0;
        for (int j = left; j < right; ++j) {
            area += trap(x[j], y[j], x[j + 1], y[j + 1]);
        }
        
        Peak peak;
        peak.idx = i;
        peak.x_val = x[i];
        peak.x_min = x[left];
        peak.x_max = x[right];
        peak.intensity = y[i];
        peak.area = area;
        peak.width = width;
        peak.sn = 0.0; // TODO: calculate S/N
        
        peaks.push_back(peak);
    }
    
    return peaks;
}

// ============================================================================
// MARK: find_peaks_pracma
// ============================================================================

std::vector<Peak> find_peaks_pracma(
    const std::vector<double>& x,
    const std::vector<double>& y,
    bool merge,
    double close_by_threshold,
    double min_peak_height,
    double min_peak_distance,
    double min_peak_width,
    double max_peak_width,
    double min_sn)
{
    // Simplified implementation - full implementation would match pracma R package
    return find_peaks_local_maxima(x, y, min_peak_height, min_peak_width, max_peak_width);
}

// ============================================================================
// MARK: ols_fit
// ============================================================================

OLSResult ols_fit(
    const std::vector<double>& x_vals,
    const std::vector<double>& y_vals,
    int degree)
{
    const int n = (int)x_vals.size();
    if (n < degree + 1 || y_vals.size() != (size_t)n) {
        return {{}, 0.0, false};
    }
    
    // Build design matrix
    std::vector<std::vector<double>> X(n, std::vector<double>(degree + 1));
    for (int i = 0; i < n; ++i) {
        X[i][0] = 1.0;
        for (int j = 1; j <= degree; ++j) {
            X[i][j] = std::pow(x_vals[i], j);
        }
    }
    
    // X'X
    std::vector<std::vector<double>> XtX(degree + 1, std::vector<double>(degree + 1, 0.0));
    for (int j = 0; j <= degree; ++j) {
        for (int k = 0; k <= degree; ++k) {
            for (int i = 0; i < n; ++i) {
                XtX[j][k] += X[i][j] * X[i][k];
            }
        }
    }
    
    // X'y
    std::vector<double> Xty(degree + 1, 0.0);
    for (int j = 0; j <= degree; ++j) {
        for (int i = 0; i < n; ++i) {
            Xty[j] += X[i][j] * y_vals[i];
        }
    }
    
    // Solve XtX * beta = Xty using Gaussian elimination
    std::vector<std::vector<double>> A = XtX;
    std::vector<double> b = Xty;
    
    for (int k = 0; k <= degree; ++k) {
        double pivot = A[k][k];
        if (std::abs(pivot) < 1e-14) {
            return {{}, 0.0, false};
        }
        for (int j = k; j <= degree; ++j) A[k][j] /= pivot;
        b[k] /= pivot;
        
        for (int i = k + 1; i <= degree; ++i) {
            double factor = A[i][k];
            for (int j = k; j <= degree; ++j) A[i][j] -= factor * A[k][j];
            b[i] -= factor * b[k];
        }
    }
    
    std::vector<double> beta(degree + 1);
    for (int i = degree; i >= 0; --i) {
        beta[i] = b[i];
        for (int j = i + 1; j <= degree; ++j) {
            beta[i] -= A[i][j] * beta[j];
        }
    }
    
    // Calculate R-squared
    double y_mean = 0.0;
    for (double y : y_vals) y_mean += y;
    y_mean /= n;
    
    double ss_tot = 0.0, ss_res = 0.0;
    for (int i = 0; i < n; ++i) {
        double y_pred = beta[0];
        for (int j = 1; j <= degree; ++j) {
            y_pred += beta[j] * std::pow(x_vals[i], j);
        }
        ss_res += (y_vals[i] - y_pred) * (y_vals[i] - y_pred);
        ss_tot += (y_vals[i] - y_mean) * (y_vals[i] - y_mean);
    }
    
    double r_squared = 1.0 - ss_res / (ss_tot + 1e-14);
    
    return {beta, r_squared, true};
}

// ============================================================================
// MARK: ols_predict
// ============================================================================

double ols_predict(const OLSResult& model, double x) {
    if (!model.success || model.coefficients.empty()) return 0.0;
    
    double y = model.coefficients[0];
    double x_pow = x;
    for (size_t j = 1; j < model.coefficients.size(); ++j) {
        y += model.coefficients[j] * x_pow;
        x_pow *= x;
    }
    return y;
}

} // namespace processing
} // namespace ms
