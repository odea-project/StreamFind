#ifndef MASS_SPEC_PROCESSING_H
#define MASS_SPEC_PROCESSING_H

#include <vector>
#include <string>

namespace mass_spec {
namespace processing {

// ============================================================================
// MARK: Baseline / Smoothing
// ============================================================================

/// Whittaker smoother: solve (diag(w) + lambda * D'D) * z = diag(w) * y
/// via banded Cholesky decomposition.
/// @param y signal vector
/// @param w weight vector (same length as y; all ones for equal weights)
/// @param lambda smoothing penalty (larger = smoother)
/// @param differences order of finite-difference penalty (1 = 1st diff, 2 = 2nd diff)
std::vector<double> whittaker_smooth(
    const std::vector<double>& y,
    const std::vector<double>& w,
    double lambda,
    int differences);

/// airPLS baseline (Zhang et al. 2010). Returns the estimated baseline.
/// Subtract from y to get corrected signal.
std::vector<double> airpls_baseline(
    const std::vector<double>& y,
    double lambda = 10.0,
    int differences = 1,
    int itermax = 20);

/// Asymmetric Least Squares baseline (Eilers & Boelens 2005).
/// Returns the estimated baseline.
std::vector<double> als_baseline(
    const std::vector<double>& y,
    double lambda = 1e5,
    double p = 0.001,
    int maxit = 10);

/// Simple symmetric moving average.
std::vector<double> moving_average(
    const std::vector<double>& y,
    int window_size);

/// Savitzky-Golay filter.
/// @param fl filter length (must be odd, >= 3)
/// @param forder polynomial order
/// @param dorder derivative order (0 = smoothing, 1 = 1st derivative, ...)
std::vector<double> savitzky_golay(
    const std::vector<double>& y,
    int fl,
    int forder,
    int dorder = 0);

// ============================================================================
// MARK: Peak finding
// ============================================================================

struct Peak {
    int idx;           ///< index into input x/y of the apex
    double x_val;      ///< x value at apex
    double x_min;      ///< x value at left boundary
    double x_max;      ///< x value at right boundary
    double intensity;  ///< y value at apex
    double area;       ///< trapezoidal area (baseline-subtracted)
    double sn;         ///< signal-to-noise ratio
    double width;      ///< x_max - x_min
};

/// Trapezoidal integration between x_start and x_end with linear baseline
/// connecting the boundary values.
double integrate_peak(
    const std::vector<double>& x,
    const std::vector<double>& y,
    double x_start,
    double x_end);

/// Local-maxima peak finder.
/// Walks outward from each local maximum to the nearest ascending valley.
std::vector<Peak> find_peaks_local_maxima(
    const std::vector<double>& x,
    const std::vector<double>& y,
    double min_height = 0.0,
    double min_width = 0.0,
    double max_width = 0.0);

/// pracma-style peak finder (local maxima + boundary walk + optional merge).
std::vector<Peak> find_peaks_pracma(
    const std::vector<double>& x,
    const std::vector<double>& y,
    bool merge = true,
    double close_by_threshold = 45.0,
    double min_peak_height = 0.0,
    double min_peak_distance = 10.0,
    double min_peak_width = 5.0,
    double max_peak_width = 120.0,
    double min_sn = 10.0);

// ============================================================================
// MARK: OLS polynomial regression
// ============================================================================

struct OLSResult {
    std::vector<double> coefficients; ///< [intercept, b1, b2, ...]
    double r_squared;
    bool success;
};

OLSResult ols_fit(
    const std::vector<double>& x_vals,
    const std::vector<double>& y_vals,
    int degree);

double ols_predict(const OLSResult& model, double x);

} // namespace processing
} // namespace ms

#endif // MASS_SPEC_PROCESSING_H
