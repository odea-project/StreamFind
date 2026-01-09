#include "nts_utils.h"
#include "nts.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <map>
#include <unordered_map>
#include <set>
#include <functional>
#include <iomanip>
#include <fstream>
#include <numeric>

// MARK: DEBUG (in nts::utils namespace)
namespace nts::utils
{
  // Debug log file stream (global for debugging)
  std::ofstream debug_log;

  // Helper function to initialize debug log with dynamic filename
  void init_debug_log(const std::string &filename, const std::string &header)
  {
    if (!debug_log.is_open())
    {
      debug_log.open(filename, std::ios::out | std::ios::trunc);
      if (debug_log.is_open() && !header.empty())
      {
        debug_log << header << std::endl;
      }
    }
  };

  // Helper function to close debug log
  void close_debug_log()
  {
    if (debug_log.is_open())
    {
      debug_log.close();
    }
  };
}; // namespace nts::utils

// MARK: get_empty_dt
Rcpp::List nts::utils::get_empty_dt()
{
  Rcpp::List out = Rcpp::List::create();
  out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  return out;
}

// MARK: check_list_must_have_names
bool nts::utils::check_list_must_have_names(
    const Rcpp::List &list,
    const std::vector<std::string> &must_have_names)
{
  std::vector<std::string> names_list = list.names();
  const int must_have_names_size = must_have_names.size();
  if (must_have_names_size == 0)
    return false;
  const int names_list_size = names_list.size();
  if (names_list_size == 0)
    return false;
  std::vector<bool> has_must_have_names(must_have_names_size, false);

  for (int i = 0; i < must_have_names_size; ++i)
  {
    for (int j = 0; j < names_list_size; ++j)
    {
      if (must_have_names[i] == names_list[j])
        has_must_have_names[i] = true;
    }
  }

  bool has_all_must_have_names = true;

  for (int i = 0; i < must_have_names_size; ++i)
  {
    if (!has_must_have_names[i])
    {
      has_all_must_have_names = false;
      break;
    }
  }

  return has_all_must_have_names;
}

// MARK:: mean, standard_deviation
float nts::utils::mean(const std::vector<float> &v)
{
  return std::accumulate(v.begin(), v.end(), 0.0) / v.size();
}

float nts::utils::standard_deviation(const std::vector<float> &v, float mean_val)
{
  float sum = 0.0;
  for (float num : v)
  {
    sum += pow(num - mean_val, 2);
  }
  return sqrt(sum / v.size());
}

// MARK:: quantile
float nts::utils::quantile(std::vector<float> data, float quantile_fraction)
{
  if (data.empty())
    return 0.0f;
  if (quantile_fraction <= 0.0f)
    return *std::min_element(data.begin(), data.end());
  if (quantile_fraction >= 1.0f)
    return *std::max_element(data.begin(), data.end());

  size_t idx = static_cast<size_t>((data.size() - 1) * quantile_fraction);
  std::nth_element(data.begin(), data.begin() + idx, data.end());
  return data[idx];
}

// MARK: encode_floats_base64
std::string nts::utils::encode_floats_base64(const std::vector<float> &input, int precision)
{
  if (input.empty())
  {
    return "";
  }

  std::string encoded = sc::encode_little_endian_from_float(input, precision);
  return sc::encode_base64(encoded);
}

// MARK: gaussian_function
float nts::utils::gaussian_function(const float &A,
                                    const float &mu,
                                    const float &sigma,
                                    const float &x)
{
  return A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
}

// MARK: gaussian_function_with_baseline
float nts::utils::gaussian_function_with_baseline(const float &A,
                                                  const float &mu,
                                                  const float &sigma,
                                                  const float &baseline,
                                                  const float &x)
{
  return baseline + A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
}

// MARK: as_MS_SPECTRA_HEADERS
sc::MS_SPECTRA_HEADERS nts::utils::as_MS_SPECTRA_HEADERS(const Rcpp::List &hd)
{
  sc::MS_SPECTRA_HEADERS headers;
  const std::vector<int> &hd_index = hd["index"];
  const std::vector<int> &hd_polarity = hd["polarity"];
  const std::vector<int> &hd_configuration = hd["configuration"];
  const std::vector<float> &hd_rt = hd["rt"];
  const std::vector<int> &hd_level = hd["level"];
  const std::vector<float> &hd_pre_mz = hd["pre_mz"];
  const std::vector<float> &hd_pre_mz_low = hd["pre_mzlow"];
  const std::vector<float> &hd_pre_mz_high = hd["pre_mzhigh"];
  const std::vector<float> &hd_pre_ce = hd["pre_ce"];
  const std::vector<float> &hd_mobility = hd["mobility"];
  const int number_spectra = hd_index.size();
  headers.resize_all(number_spectra);
  headers.index = hd_index;
  headers.rt = hd_rt;
  headers.polarity = hd_polarity;
  headers.configuration = hd_configuration;
  headers.level = hd_level;
  headers.precursor_mz = hd_pre_mz;
  headers.activation_ce = hd_pre_ce;
  headers.mobility = hd_mobility;
  return headers;
}

// MARK: get_sort_indices_float
std::vector<size_t> nts::utils::get_sort_indices_float(const std::vector<float> &data)
{
  std::vector<size_t> indices(data.size());
  std::iota(indices.begin(), indices.end(), 0);
  std::sort(indices.begin(), indices.end(), [&data](size_t a, size_t b)
            { return data[a] < data[b]; });
  return indices;
}

// MARK: reorder_float_data
void nts::utils::reorder_float_data(std::vector<float> &data, const std::vector<size_t> &indices)
{
  std::vector<float> reordered;
  reordered.reserve(data.size());
  for (size_t idx : indices)
  {
    reordered.push_back(data[idx]);
  }
  data = std::move(reordered);
}

// MARK: reorder_int_data
void nts::utils::reorder_int_data(std::vector<int> &data, const std::vector<size_t> &indices)
{
  std::vector<int> reordered;
  reordered.reserve(data.size());
  for (size_t idx : indices)
  {
    reordered.push_back(data[idx]);
  }
  data = std::move(reordered);
}

// MARK: reorder_multiple_vectors
void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1)
{
  reorder_float_data(vec1, indices);
}

void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
}

void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2,
                                          std::vector<float> &vec3)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
  reorder_float_data(vec3, indices);
}

void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2,
                                          std::vector<float> &vec3,
                                          std::vector<float> &vec4)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
  reorder_float_data(vec3, indices);
  reorder_float_data(vec4, indices);
}

void nts::utils::reorder_multiple_vectors(const std::vector<size_t> &indices,
                                          std::vector<float> &vec1,
                                          std::vector<float> &vec2,
                                          std::vector<float> &vec3,
                                          std::vector<float> &vec4,
                                          std::vector<int> &int_vec)
{
  reorder_float_data(vec1, indices);
  reorder_float_data(vec2, indices);
  reorder_float_data(vec3, indices);
  reorder_float_data(vec4, indices);
  reorder_int_data(int_vec, indices);
}

// MARK: filter_above_threshold
std::vector<size_t> nts::utils::filter_above_threshold(
    const std::vector<float> &data,
    const std::vector<float> &thresholds)
{
  std::vector<size_t> indices;
  indices.reserve(data.size() / 2); // Conservative estimate

  const size_t n = std::min(data.size(), thresholds.size());
  for (size_t i = 0; i < n; ++i)
  {
    if (data[i] > thresholds[i])
    {
      indices.push_back(i);
    }
  }
  return indices;
}

// MARK: cluster_by_threshold_float
std::vector<int> nts::utils::cluster_by_threshold_float(
    const std::vector<float> &sorted_data,
    const std::vector<float> &thresholds)
{
  const size_t n = sorted_data.size();
  std::vector<int> clusters(n);

  if (n == 0)
    return clusters;

  clusters[0] = 0;
  int current_cluster = 0;

  for (size_t i = 1; i < n; ++i)
  {
    float diff = sorted_data[i] - sorted_data[i - 1];
    float threshold = (i < thresholds.size()) ? thresholds[i] : thresholds.back();

    if (diff > threshold)
    {
      ++current_cluster;
    }
    clusters[i] = current_cluster;
  }

  return clusters;
}

// MARK: calculate_baseline
std::vector<float> nts::utils::calculate_baseline(const std::vector<float> &intensity, int windowSize)
{
  const size_t n = intensity.size();
  std::vector<float> baseline(n);

  for (size_t i = 0; i < n; ++i)
  {
    size_t start_idx = (i >= static_cast<size_t>(windowSize)) ? i - windowSize : 0;
    size_t end_idx = std::min(n - 1, i + windowSize);

    float min_intensity = intensity[start_idx];
    for (size_t j = start_idx; j <= end_idx; ++j)
    {
      min_intensity = std::min(min_intensity, intensity[j]);
    }
    baseline[i] = min_intensity;
  }

  // Smooth baseline using 3-point moving average
  if (n >= 3)
  {
    std::vector<float> smoothed_baseline = baseline;
    for (size_t i = 1; i < n - 1; ++i)
    {
      smoothed_baseline[i] = (baseline[i - 1] + baseline[i] + baseline[i + 1]) / 3.0f;
    }
    baseline = std::move(smoothed_baseline);
  }

  return baseline;
}

// MARK: smooth_intensity_savitzky_golay
std::vector<float> nts::utils::smooth_intensity_savitzky_golay(
    const std::vector<float> &intensity,
    int windowSize,
    int polyOrder)
{
  const size_t n = intensity.size();
  std::vector<float> smoothed(n, 0.0f);

  if (windowSize < 3 || windowSize % 2 == 0 || polyOrder < 1 || polyOrder >= windowSize)
  {
    // Fallback to simple smoothing if parameters are invalid
    return smooth_intensity(intensity, windowSize);
  }

  int half_window = windowSize / 2;

  // Precompute the Savitzky-Golay convolution coefficients (for uniform spacing)
  // Use least-squares fit to a polynomial of given order
  // Reference: Numerical Recipes, or https://en.wikipedia.org/wiki/Savitzky–Golay_filter
  std::vector<float> coeffs(windowSize, 0.0f);
  {
    // Build the design matrix
    std::vector<std::vector<float>> A(windowSize, std::vector<float>(polyOrder + 1, 0.0f));
    for (int i = -half_window; i <= half_window; ++i)
    {
      for (int j = 0; j <= polyOrder; ++j)
      {
        A[i + half_window][j] = std::pow(static_cast<float>(i), j);
      }
    }
    // Compute (A^T A)^{-1} A^T for the center point (convolution coefficients)
    // Only need the first row of the pseudoinverse for smoothing
    std::vector<float> AtA(polyOrder + 1, 0.0f);
    std::vector<std::vector<float>> ATA(polyOrder + 1, std::vector<float>(polyOrder + 1, 0.0f));
    for (int i = 0; i <= polyOrder; ++i)
    {
      for (int j = 0; j <= polyOrder; ++j)
      {
        float sum = 0.0f;
        for (int k = 0; k < windowSize; ++k)
        {
          sum += A[k][i] * A[k][j];
        }
        ATA[i][j] = sum;
      }
    }
    // Invert ATA (small matrix, use Gaussian elimination)
    std::vector<std::vector<float>> inv_ATA = ATA;
    int m = polyOrder + 1;
    // Augment with identity
    for (int i = 0; i < m; ++i)
    {
      inv_ATA[i].resize(2 * m, 0.0f);
      inv_ATA[i][m + i] = 1.0f;
    }
    // Gaussian elimination
    for (int i = 0; i < m; ++i)
    {
      float diag = inv_ATA[i][i];
      if (std::abs(diag) < 1e-12f)
        continue;
      for (int j = 0; j < 2 * m; ++j)
        inv_ATA[i][j] /= diag;
      for (int k = 0; k < m; ++k)
      {
        if (k == i)
          continue;
        float factor = inv_ATA[k][i];
        for (int j = 0; j < 2 * m; ++j)
        {
          inv_ATA[k][j] -= factor * inv_ATA[i][j];
        }
      }
    }
    // Extract inverse
    std::vector<std::vector<float>> ATA_inv(m, std::vector<float>(m, 0.0f));
    for (int i = 0; i < m; ++i)
      for (int j = 0; j < m; ++j)
        ATA_inv[i][j] = inv_ATA[i][m + j];

    // Compute convolution coefficients for smoothing (center point)
    std::vector<float> B(m, 0.0f);
    for (int i = 0; i < windowSize; ++i)
    {
      B[0] += A[i][0];
    }
    for (int i = 0; i < windowSize; ++i)
    {
      for (int j = 0; j < m; ++j)
      {
        B[j] += A[i][j];
      }
    }
    // The smoothing coefficients are the first row of (ATA_inv * A^T) at the center
    for (int k = 0; k < windowSize; ++k)
    {
      float c = 0.0f;
      for (int j = 0; j < m; ++j)
      {
        c += ATA_inv[0][j] * A[k][j];
      }
      coeffs[k] = c;
    }
  }

  // Apply convolution (handle edges by reflecting)
  for (size_t i = 0; i < n; ++i)
  {
    float sum = 0.0f;
    for (int j = -half_window; j <= half_window; ++j)
    {
      int idx = static_cast<int>(i) + j;
      // Reflect at boundaries
      if (idx < 0)
        idx = -idx;
      if (idx >= static_cast<int>(n))
        idx = 2 * static_cast<int>(n) - idx - 2;
      sum += coeffs[j + half_window] * intensity[idx];
    }
    smoothed[i] = sum;
  }

  return smoothed;
};

// MARK: smooth_intensity
std::vector<float> nts::utils::smooth_intensity(const std::vector<float> &intensity, int windowSize)
{
  const size_t n = intensity.size();
  std::vector<float> smoothed(n);
  int half_window = windowSize / 2;

  for (size_t i = 0; i < n; ++i)
  {
    size_t start_idx = (i >= static_cast<size_t>(half_window)) ? i - half_window : 0;
    size_t end_idx = std::min(n - 1, i + half_window);

    float sum = 0.0f;
    size_t count = 0;
    for (size_t j = start_idx; j <= end_idx; ++j)
    {
      sum += intensity[j];
      count++;
    }
    smoothed[i] = sum / count;
  }

  return smoothed;
}

// MARK: calculate_derivatives
void nts::utils::calculate_derivatives(const std::vector<float> &smoothed_intensity,
                                       std::vector<float> &first_derivative,
                                       std::vector<float> &second_derivative)
{
  const size_t n = smoothed_intensity.size();

  // Calculate first derivative
  first_derivative.clear();
  first_derivative.reserve(n - 1);
  for (size_t i = 0; i < n - 1; ++i)
  {
    first_derivative.push_back(smoothed_intensity[i + 1] - smoothed_intensity[i]);
  }

  // Calculate second derivative
  second_derivative.clear();
  second_derivative.reserve(first_derivative.size() - 1);
  for (size_t i = 0; i < first_derivative.size() - 1; ++i)
  {
    second_derivative.push_back(first_derivative[i + 1] - first_derivative[i]);
  }
};

// MARK: calculate_peak_area
float nts::utils::calculate_area(const std::vector<float> &rt, const std::vector<float> &intensity)
{
  if (rt.size() < 2 || rt.size() != intensity.size())
    return 0.0f;

  float area = 0.0f;
  for (size_t i = 1; i < rt.size(); ++i)
  {
    float dx = rt[i] - rt[i - 1];
    float avg_intensity = (intensity[i] + intensity[i - 1]) / 2.0f;
    area += dx * avg_intensity;
  }

  return std::max(0.0f, area);
}

// MARK: fit_gaussian
void nts::utils::fit_gaussian(const std::vector<float> &x, const std::vector<float> &y,
                              float &A, float &mu, float &sigma, float &baseline)
{
  // Adam optimizer parameters
  const float alpha = 0.01f;   // Learning rate
  const float beta1 = 0.9f;    // First moment decay rate
  const float beta2 = 0.999f;  // Second moment decay rate
  const float epsilon = 1e-8f; // Small value to prevent division by zero
  const int max_iterations = 500;

  float m_A = 0.0f, v_A = 0.0f, m_mu = 0.0f, v_mu = 0.0f, m_sigma = 0.0f, v_sigma = 0.0f;
  float m_baseline = 0.0f, v_baseline = 0.0f;

  for (int iter = 1; iter <= max_iterations; ++iter)
  {
    float grad_A = 0.0f, grad_mu = 0.0f, grad_sigma = 0.0f, grad_baseline = 0.0f;

    // Calculate gradients
    for (size_t i = 0; i < x.size(); ++i)
    {
      float exp_term = std::exp(-std::pow(x[i] - mu, 2) / (2 * std::pow(sigma, 2)));
      float y_pred = baseline + A * exp_term;
      float error = y[i] - y_pred;

      grad_A += -2 * error * exp_term;
      grad_mu += -2 * error * A * exp_term * (x[i] - mu) / std::pow(sigma, 2);
      grad_sigma += -2 * error * A * exp_term * std::pow(x[i] - mu, 2) / std::pow(sigma, 3);
      grad_baseline += -2 * error;
    }

    // Adam update for A
    m_A = beta1 * m_A + (1 - beta1) * grad_A;
    v_A = beta2 * v_A + (1 - beta2) * grad_A * grad_A;
    float A_hat = m_A / (1 - std::pow(beta1, iter));
    float v_A_hat = v_A / (1 - std::pow(beta2, iter));
    A -= alpha * A_hat / (std::sqrt(v_A_hat) + epsilon);
    A = std::max(0.1f, A); // Keep positive

    // Adam update for mu
    m_mu = beta1 * m_mu + (1 - beta1) * grad_mu;
    v_mu = beta2 * v_mu + (1 - beta2) * grad_mu * grad_mu;
    float mu_hat = m_mu / (1 - std::pow(beta1, iter));
    float v_mu_hat = v_mu / (1 - std::pow(beta2, iter));
    mu -= alpha * mu_hat / (std::sqrt(v_mu_hat) + epsilon);

    // Adam update for sigma
    m_sigma = beta1 * m_sigma + (1 - beta1) * grad_sigma;
    v_sigma = beta2 * v_sigma + (1 - beta2) * grad_sigma * grad_sigma;
    float sigma_hat = m_sigma / (1 - std::pow(beta1, iter));
    float v_sigma_hat = v_sigma / (1 - std::pow(beta2, iter));
    sigma -= alpha * sigma_hat / (std::sqrt(v_sigma_hat) + epsilon);
    sigma = std::max(0.1f, std::min(sigma, 100.0f)); // Constrain sigma

    // Adam update for baseline
    m_baseline = beta1 * m_baseline + (1 - beta1) * grad_baseline;
    v_baseline = beta2 * v_baseline + (1 - beta2) * grad_baseline * grad_baseline;
    float baseline_hat = m_baseline / (1 - std::pow(beta1, iter));
    float v_baseline_hat = v_baseline / (1 - std::pow(beta2, iter));
    baseline -= alpha * baseline_hat / (std::sqrt(v_baseline_hat) + epsilon);
    baseline = std::max(0.0f, baseline); // Keep non-negative
  }
}

// MARK: calculate_gaussian_rsquared
float nts::utils::calculate_gaussian_rsquared(const std::vector<float> &x, const std::vector<float> &y,
                                              float A, float mu, float sigma, float baseline)
{
  if (x.empty() || y.empty() || x.size() != y.size())
    return 0.0f;

  float ss_total = 0.0f;
  float ss_residual = 0.0f;
  float mean_y = nts::utils::mean(y);

  for (size_t i = 0; i < x.size(); ++i)
  {
    float y_pred = nts::utils::gaussian_function_with_baseline(A, mu, sigma, baseline, x[i]);
    ss_residual += std::pow(y[i] - y_pred, 2);
    ss_total += std::pow(y[i] - mean_y, 2);
  }

  if (ss_total == 0.0f)
    return 0.0f;

  float r2 = 1.0f - (ss_residual / ss_total);
  // Don't clamp - negative R² indicates fit worse than mean (important diagnostic!)
  return r2;
}

// MARK: calculate_jaggedness
// Measures peak smoothness (lower = smoother = better quality)
// Returns normalized jaggedness score (0 = perfectly smooth, higher = more jagged)
float nts::utils::calculate_jaggedness(const std::vector<float> &intensity)
{
  if (intensity.size() < 3)
    return 0.0f;

  float max_intensity = *std::max_element(intensity.begin(), intensity.end());
  if (max_intensity == 0.0f)
    return 0.0f;

  float jaggedness = 0.0f;
  for (size_t i = 1; i < intensity.size() - 1; ++i)
  {
    float expected = (intensity[i - 1] + intensity[i + 1]) / 2.0f;
    jaggedness += std::abs(intensity[i] - expected);
  }

  // Normalize by number of points and max intensity
  return jaggedness / ((intensity.size() - 2) * max_intensity);
}

// MARK: calculate_sharpness
// Measures how "sharp" vs "flat" the peak is (higher = sharper)
// Returns sharpness score
float nts::utils::calculate_sharpness(const std::vector<float> &rt,
                                      const std::vector<float> &intensity,
                                      float area)
{
  if (rt.empty() || intensity.empty() || rt.size() != intensity.size())
    return 0.0f;

  float max_intensity = *std::max_element(intensity.begin(), intensity.end());
  float width = rt.back() - rt.front();

  if (width == 0.0f || area == 0.0f)
    return 0.0f;

  // Sharpness = peak_height / (width * sqrt(area))
  // Higher values indicate sharper, more defined peaks
  return max_intensity / (width * std::sqrt(std::abs(area)));
}

// MARK: calculate_asymmetry
// Calculates asymmetry factor (tailing factor) at 10% of peak height
// Returns: 1.0 = symmetric, >1.0 = tailing, <1.0 = fronting
float nts::utils::calculate_asymmetry(const std::vector<float> &rt,
                                      const std::vector<float> &intensity)
{
  if (rt.size() < 3 || intensity.size() < 3 || rt.size() != intensity.size())
    return 1.0f;

  // Find apex
  auto max_it = std::max_element(intensity.begin(), intensity.end());
  float max_intensity = *max_it;
  size_t max_idx = std::distance(intensity.begin(), max_it);

  // Calculate 10% height (USP standard)
  float baseline = std::min(intensity.front(), intensity.back());
  float peak_height = max_intensity - baseline;
  float target_height = baseline + (peak_height * 0.1f);

  // Find left and right points at 10% height
  size_t left_idx = 0;
  size_t right_idx = intensity.size() - 1;

  // Search left from apex
  for (size_t i = max_idx; i > 0; --i)
  {
    if (intensity[i] <= target_height)
    {
      left_idx = i;
      break;
    }
  }

  // Search right from apex
  for (size_t i = max_idx; i < intensity.size(); ++i)
  {
    if (intensity[i] <= target_height)
    {
      right_idx = i;
      break;
    }
  }

  if (left_idx >= max_idx || right_idx <= max_idx)
    return 1.0f;

  float rt_apex = rt[max_idx];
  float A = rt_apex - rt[left_idx];  // Left width
  float B = rt[right_idx] - rt_apex; // Right width

  if (A == 0.0f)
    return 10.0f; // Maximum asymmetry if left side is zero

  // USP tailing factor: T = (A + B) / (2*A)
  // Simplified: asymmetry_factor = B / A
  return B / A;
};

// MARK: calculate_modality
// Counts number of local maxima in smoothed intensity (co-elution detection)
// Returns: number of local maxima (1 = single peak, >1 = multiple peaks/co-elution)
int nts::utils::calculate_modality(const std::vector<float> &smoothed_intensity,
                                   float min_prominence_ratio)
{
  if (smoothed_intensity.size() < 3)
    return 1;

  float global_max = *std::max_element(smoothed_intensity.begin(), smoothed_intensity.end());
  float min_prominence = global_max * min_prominence_ratio;

  int local_maxima_count = 0;

  for (size_t i = 1; i < smoothed_intensity.size() - 1; ++i)
  {
    // Check if it's a local maximum
    if (smoothed_intensity[i] > smoothed_intensity[i - 1] &&
        smoothed_intensity[i] > smoothed_intensity[i + 1])
    {
      // Check prominence (must be significant relative to global max)
      if (smoothed_intensity[i] >= min_prominence)
      {
        local_maxima_count++;
      }
    }
  }

  return std::max(1, local_maxima_count);
};

// MARK: calculate_theoretical_plates
// Calculates chromatographic efficiency (theoretical plates)
// Returns: number of theoretical plates (N)
float nts::utils::calculate_theoretical_plates(float rt_apex,
                                               float width_at_half_height)
{
  if (width_at_half_height == 0.0f || rt_apex == 0.0f)
    return 0.0f;

  // N = 5.54 * (tR / W₀.₅)²
  // where tR is retention time at apex and W₀.₅ is width at half height
  return 5.54f * std::pow(rt_apex / width_at_half_height, 2.0f);
};
