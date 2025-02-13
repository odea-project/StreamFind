#include "NTS_utils.h"

// MARK: GET_MS_ANALYSIS_LIST_HEADERS
sc::MS_SPECTRA_HEADERS NTS::get_ms_analysis_list_headers(const Rcpp::List &analysis)
{
  sc::MS_SPECTRA_HEADERS headers;
  const Rcpp::List &hd = analysis["spectra_headers"];
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
};

// MARK: MEAN
float NTS::mean(const std::vector<float> &v)
{
  return std::accumulate(v.begin(), v.end(), 0.0) / v.size();
};

// MARK: STANDARD_DEVIATION
float NTS::standard_deviation(const std::vector<float> &v, float mean_val)
{
  float sum = 0.0;
  for (float num : v)
  {
    sum += pow(num - mean_val, 2);
  }
  return sqrt(sum / v.size());
};

// MARK: FIND_MAX_INDEX
size_t NTS::find_max_index(const std::vector<float> &v)
{
  return std::max_element(v.begin(), v.end()) - v.begin();
};

// MARK: FIND_MIN_INDEX
size_t NTS::find_min_index(const std::vector<float> &v)
{
  return std::min_element(v.begin(), v.end()) - v.begin();
};

// MARK: MERGE_TRACES_WITHIN_RT
void NTS::merge_traces_within_rt(std::vector<float> &rt, std::vector<float> &mz, std::vector<float> &intensity)
{
  std::vector<float> rt_out;
  std::vector<float> mz_out;
  std::vector<float> intensity_out;
  for (size_t z = 0; z < rt.size(); z++)
  {
    if (std::find(rt_out.begin(), rt_out.end(), rt[z]) == rt_out.end())
    {
      rt_out.push_back(rt[z]);
      mz_out.push_back(mz[z]);
      intensity_out.push_back(intensity[z]);
    }
    else
    {
      auto it = std::find(rt_out.begin(), rt_out.end(), rt[z]);
      int index = std::distance(rt_out.begin(), it);
      if (intensity[z] > intensity_out[index])
      {
        mz_out[index] = mz[z];
        intensity_out[index] = intensity[z];
      }
    }
  }
  rt = rt_out;
  mz = mz_out;
  intensity = intensity_out;
};

// MARK: TRIM_TO_EQUAL_LENGTH_AROUND_MAX_POSITION
void NTS::trim_to_equal_length_around_max_position(std::vector<float> &x, const size_t max_position)
{
  const int n = x.size();
  std::vector<float> x_out(n);
  size_t n_points = std::min(max_position, x.size() - max_position - 1);
  x_out.resize(2 * n_points + 1);
  for (size_t i = 0; i < x_out.size(); ++i)
  {
    x_out[i] = x[max_position - n_points + i];
  }
  x = x_out;
};

// MARK: TRAPEZOIDAL_AREA
float NTS::trapezoidal_area(const std::vector<float> &x, const std::vector<float> &intensity)
{
  if (x.size() != intensity.size() || x.size() < 2)
  {
    throw std::invalid_argument("Vectors must have the same size and contain at least two elements.");
  }

  float area = 0.0;

  // Iterate over the x and intensity vectors and apply the trapezoidal rule
  for (std::size_t i = 1; i < x.size(); ++i)
  {
    float dx = x[i] - x[i - 1];                                    // Difference between consecutive x values
    float avg_intensity = (intensity[i] + intensity[i - 1]) / 2.0; // Average of consecutive intensities
    area += dx * avg_intensity;                                    // Trapezoid area for this segment
  }

  return area;
};

// MARK: GAUSSIAN
float NTS::gaussian(const float &A, const float &mu, const float &sigma, const float &x)
{
  return A * exp(-pow(x - mu, 2) / (2 * pow(sigma, 2)));
};

// MARK: FIT_GAUSSIAN_COST_FUNCTION
float NTS::fit_gaussian_cost_function(const std::vector<float> &x, const std::vector<float> &y, float A, float mu, float sigma)
{
  float cost = 0.0;
  for (size_t i = 0; i < x.size(); ++i)
  {
    float y_pred = NTS::gaussian(A, mu, sigma, x[i]);
    cost += pow(y[i] - y_pred, 2);
  }
  return cost;
};

// MARK: FIT_GAUSSIAN
void NTS::fit_gaussian(const std::vector<float> &x, const std::vector<float> &y, float &A_fitted, float &mu_fitted, float &sigma_fitted)
{

  // Optimization parameters
  const float learning_rate = 0.001;
  const int max_iterations = 10000;
  const float tolerance = 1e-08;

  for (int iter = 0; iter < max_iterations; ++iter)
  {
    float current_cost = fit_gaussian_cost_function(x, y, A_fitted, mu_fitted, sigma_fitted);

    // Numerical gradient calculation
    float grad_A = (fit_gaussian_cost_function(x, y, A_fitted + tolerance, mu_fitted, sigma_fitted) - current_cost) / tolerance;
    float grad_mu = (fit_gaussian_cost_function(x, y, A_fitted, mu_fitted + tolerance, sigma_fitted) - current_cost) / tolerance;
    float grad_sigma = (fit_gaussian_cost_function(x, y, A_fitted, mu_fitted, sigma_fitted + tolerance) - current_cost) / tolerance;

    // Update parameters using gradient descent
    A_fitted -= learning_rate * grad_A;
    mu_fitted -= learning_rate * grad_mu;
    sigma_fitted -= learning_rate * grad_sigma;

    if (sigma_fitted <= 0)
    {
      sigma_fitted = 2; // Set to a small positive number if non-positive
    }
    if (sigma_fitted > 1e6)
    { // Arbitrary upper bound for sigma
      sigma_fitted = 10;
    }

    // Check convergence
    if (abs(grad_A) < tolerance && abs(grad_mu) < tolerance && abs(grad_sigma) < tolerance)
    {
      break;
    }
  }
};

// MARK: CALCULATE_GAUSSIAN_RSQUARED
float NTS::calculate_gaussian_rsquared(const std::vector<float> &x, const std::vector<float> &y, float A, float mu, float sigma)
{
  float ss_total = 0.0;
  float ss_residual = 0.0;
  float mean_y = accumulate(y.begin(), y.end(), 0.0) / y.size();

  for (size_t i = 0; i < x.size(); ++i)
  {
    float y_pred = NTS::gaussian(A, mu, sigma, x[i]);
    ss_residual += pow(y[i] - y_pred, 2);
    ss_total += pow(y[i] - mean_y, 2);
  }
  return 1 - (ss_residual / ss_total);
};

// MARK: CALCULATE_GAUSSIAN_FIT
Rcpp::List NTS::calculate_gaussian_fit(const std::string &ft,
                                       const std::vector<float> &rt,
                                       const std::vector<float> &intensity,
                                       const float &baseCut)
{

  float noise = 0;
  float sn = 0;
  float A_fitted = 0;
  float mu_fitted = 0;
  float sigma_fitted = 0;
  float r_squared = 0;

  Rcpp::List quality = Rcpp::List::create(
      Rcpp::Named("feature") = ft,
      Rcpp::Named("noise") = noise,
      Rcpp::Named("sn") = sn,
      Rcpp::Named("gauss_a") = A_fitted,
      Rcpp::Named("gauss_u") = mu_fitted,
      Rcpp::Named("gauss_s") = sigma_fitted,
      Rcpp::Named("gauss_f") = r_squared);
  
  if (intensity.empty())
  {
    return quality;
  }

  size_t max_position = NTS::find_max_index(intensity);
  
  if (max_position == 0 || max_position >= intensity.size())
  {
    return quality;
  }
  
  const float max_intensity = intensity[max_position];
  
  const std::vector<float> left_intensity = std::vector<float>(intensity.begin(), intensity.begin() + max_position);
  const size_t min_left_position = NTS::find_min_index(left_intensity);
  const float noise_left = left_intensity[min_left_position];
  const float sn_left = max_intensity / noise_left;
  
  const std::vector<float> right_intensity = std::vector<float>(intensity.begin() + max_position, intensity.end());
  const size_t min_right_position = NTS::find_min_index(right_intensity);
  const float noise_right = right_intensity[min_right_position];
  const float sn_right = max_intensity / noise_right;
  
  if (sn_left > sn_right)
  {
    noise = noise_right;
    sn = sn_right;
  }
  else
  {
    noise = noise_left;
    sn = sn_left;
  }
  
  noise = round(noise);
  sn = round(sn * 10) / 10;
  quality["noise"] = noise;
  quality["sn"] = sn;

  const float low_cut = max_intensity * baseCut;

  const int n = rt.size();

  std::vector<float> rt_trimmed(n);
  std::vector<float> int_trimmed(n);

  for (int z = 0; z < n; z++)
  {
    int_trimmed[z] = intensity[z] - low_cut;
    rt_trimmed[z] = rt[z];
  }

  auto it_int_trimmed = int_trimmed.begin();
  auto it_rt_trimmed = rt_trimmed.begin();

  while (it_int_trimmed != int_trimmed.end())
  {
    if (*it_int_trimmed <= 0)
    {
      it_int_trimmed = int_trimmed.erase(it_int_trimmed);
      it_rt_trimmed = rt_trimmed.erase(it_rt_trimmed);
    }
    else
    {
      ++it_int_trimmed;
      ++it_rt_trimmed;
    }
  }

  int n_trimmed = int_trimmed.size();

  if (n_trimmed < 3)
    return quality;

  max_position = NTS::find_max_index(int_trimmed);

  trim_to_equal_length_around_max_position(rt_trimmed, max_position);
  trim_to_equal_length_around_max_position(int_trimmed, max_position);

  n_trimmed = rt_trimmed.size();

  if (n_trimmed < 3)
    return quality;

  max_position = NTS::find_max_index(int_trimmed);

  A_fitted = int_trimmed[max_position];
  mu_fitted = rt_trimmed[max_position];
  sigma_fitted = (rt_trimmed.back() - rt_trimmed.front()) / 4.0;

  fit_gaussian(rt_trimmed, int_trimmed, A_fitted, mu_fitted, sigma_fitted);

  r_squared = calculate_gaussian_rsquared(rt_trimmed, int_trimmed, A_fitted, mu_fitted, sigma_fitted);

  A_fitted = round(A_fitted);
  mu_fitted = round(mu_fitted * 10) / 10;
  sigma_fitted = round(sigma_fitted * 10) / 10;
  r_squared = round(r_squared * 10000) / 10000;

  quality["gauss_a"] = A_fitted;
  quality["gauss_u"] = mu_fitted;
  quality["gauss_s"] = sigma_fitted;
  quality["gauss_f"] = r_squared;

  return quality;
};

// MARK: FIND_ISOTOPIC_CANDIDATES
std::vector<int> NTS::find_isotopic_candidates(
    const int &number_features,
    const std::vector<std::string> &features,
    const std::vector<float> &mzs,
    const std::vector<float> &rts,
    const std::vector<int> &pols,
    const int &pol,
    const std::string &feature,
    const float &mz,
    const float &mzmin,
    const float &mzmax,
    const float &rt,
    float &rtmin,
    float &rtmax,
    const float &rtWindowAlignment,
    const float &max_mz_chain)
{
  std::vector<int> candidates;

  const float left_rt = rt - rtmin;
  const float right_rt = rtmax - rt;
  float rtW = right_rt;
  if (left_rt < right_rt)
    rtW = left_rt;
  rtW = rtW * rtWindowAlignment;
  rtmin = rt - rtW;
  rtmax = rt + rtW;

  for (int z = 0; z < number_features; ++z)
  {
    if (rts[z] >= rtmin && rts[z] <= rtmax && mzs[z] > mz && mzs[z] <= max_mz_chain && pols[z] == pol && features[z] != feature)
    {
      candidates.push_back(z);
    }
  }

  return candidates;
};

// MARK: IS_MAX_GAP_REACHED
bool NTS::is_max_gap_reached(const int &s, const int &maxGaps, const std::vector<int> &steps)
{
  if (steps.size() < 2)
    return false;
  if (s < maxGaps)
    return false;
  const int steps_size = steps.size();
  const int last_step = steps[steps_size - 1];
  const int gap = s - last_step;
  if (gap > maxGaps)
    return true;
  return false;
};

// MARK: ANNOTATE_ISOTOPES
void NTS::annotate_isotopes(MS_ANNOTATION &af,
                            const MS_ISOTOPE_COMBINATIONS &combinations,
                            const MS_CANDIDATE_CHAIN &candidates_chain,
                            const int &maxIsotopes,
                            const int &maxCharge,
                            const int &maxGaps)
{

  bool is_Mplus = false;

  double mzr = *std::max_element(candidates_chain.mzr.begin(), candidates_chain.mzr.end());

  const int number_candidates = candidates_chain.length;

  const std::string &mono_feature = candidates_chain.feature[0];
  const int &mono_index = candidates_chain.index[0];
  const float &mono_mz = candidates_chain.mz[0];
  const float &mono_rt = candidates_chain.rt[0];
  const float &mono_intensity = candidates_chain.intensity[0];
  const float &mono_mzr = candidates_chain.mzr[0];

  std::vector<NTS::MS_ISOTOPE_CHAIN> isotopic_chains = {MS_ISOTOPE_CHAIN(1, mono_index, mono_feature, mono_mz, mono_mzr, mono_rt)};

  if (maxCharge > 1)
  {
    for (int z = 2; z <= maxCharge; z++)
    {
      isotopic_chains.push_back(NTS::MS_ISOTOPE_CHAIN(z, mono_index, mono_feature, mono_mz, mono_mzr, mono_rt));
    }
  }

  const int number_charges = isotopic_chains.size();

  for (int z = 0; z < number_charges; z++)
  {

    NTS::MS_ISOTOPE_CHAIN &iso_chain = isotopic_chains[z];

    const int charge = iso_chain.charge[0];

    const int number_steps = maxIsotopes + 1;

    for (int s = 1; s < number_steps; ++s)
    {

      if (NTS::is_max_gap_reached(s, maxGaps, iso_chain.step))
        break;

      std::vector<int> which_combinations;

      for (int c = 0; c < combinations.length; ++c)
        if (combinations.step[c] == s)
          which_combinations.push_back(c);

      const int number_combinations = which_combinations.size();

      std::vector<float> mass_distances(number_combinations);

      for (int c = 0; c < number_combinations; ++c)
      {
        mass_distances[c] = combinations.mass_distances[which_combinations[c]];
        mass_distances[c] = mass_distances[c] / charge;
      }

      const float mass_distance_max = *std::max_element(mass_distances.begin(), mass_distances.end());
      const float mass_distance_min = *std::min_element(mass_distances.begin(), mass_distances.end());

      for (int candidate = 1; candidate < number_candidates; ++candidate)
      {

        const std::string &feature = candidates_chain.feature[candidate];
        const int &index = candidates_chain.index[candidate];
        const float &mz = candidates_chain.mz[candidate];
        const float &rt = candidates_chain.rt[candidate];
        const float &intensity = candidates_chain.intensity[candidate];
        const bool &was_annotated = af.iso_step[index] > 0;

        float candidate_mass_distance = mz - mono_mz;
        float candidate_time_error = std::abs(rt - mono_rt);
        float candidate_mass_distance_min = candidate_mass_distance - mzr;
        float candidate_mass_distance_max = candidate_mass_distance + mzr;

        // M-ION Check ///////////////////////////////////////////////////////////////////////////////////////////////
        // Check for molecular ion (M) with distance 1.007276 and much higher intensity, for now set to x5
        if (s == 1)
        {

          if (candidate_mass_distance_min < 1.007276 &&
              candidate_mass_distance_max > 1.007276 &&
              (intensity / mono_intensity) > 5)
          {

            // TODO this will also capture 2H loss and mark it as M+
            // the mass error might give an indication to check

            af.index[mono_index] = mono_index;
            af.feature[mono_index] = mono_feature;
            af.component_feature[mono_index] = feature;
            af.iso_step[mono_index] = -1;
            af.iso_cat[mono_index] = "M+";
            af.iso_isotope[mono_index] = "";
            af.iso_charge[mono_index] = charge;
            af.iso_mzr[mono_index] = mzr;
            af.iso_mass_distance[mono_index] = candidate_mass_distance;
            af.iso_theoretical_mass_distance[mono_index] = 0;
            af.iso_mass_distance_error[mono_index] = std::abs(candidate_mass_distance - 1.007276);
            af.iso_time_error[mono_index] = candidate_time_error;
            af.iso_relative_intensity[mono_index] = mono_intensity / intensity;
            af.iso_theoretical_min_relative_intensity[mono_index] = 0;
            af.iso_theoretical_max_relative_intensity[mono_index] = 0;
            af.iso_size[mono_index] = 0;
            af.iso_number_carbons[mono_index] = 0;

            is_Mplus = true;
            break;
          }
        }

        // ISOTOPE Check /////////////////////////////////////////////////////////////////////////////////////////////
        double combination_mass_error = 10; // is updated on the first hit

        // when candidate is inside of the mass distance for isotopic step mass distances
        if (mass_distance_min - mzr < candidate_mass_distance && mass_distance_max + mzr > candidate_mass_distance)
        {

          // selects the combinations within the mass distance, when duplicated mass distances, the first hit is the one stored
          for (int c = 0; c < number_combinations; c++)
          {

            const float candidate_mass_distance_error = abs(mass_distances[c] - candidate_mass_distance);
            const std::vector<std::string> &combination = combinations.tensor_combinations[which_combinations[c]];

            float min_rel_int = 1;
            float max_rel_int = 1;

            std::unordered_map<std::string, int> isotope_map;

            for (size_t e = 0; e < combination.size(); ++e)
            {
              std::string e_el = combination[e];
              isotope_map[e_el]++;
            }

            for (const auto &pair : isotope_map)
            {
              std::string iso = pair.first;
              int iso_n = pair.second;

              const int iso_idx = std::distance(combinations.isotopes_str.begin(), std::find(combinations.isotopes_str.begin(), combinations.isotopes_str.end(), iso));

              const float iso_ab = combinations.abundances[iso_idx];
              const float mono_ab = combinations.abundances_monoisotopic[iso_idx];
              float min_el_num = combinations.min[iso_idx];
              float max_el_num = combinations.max[iso_idx];

              // narrows the range for n carbons based on estimation
              if (iso_n == 1 && iso == "13C" && s == 1)
              {
                iso_chain.number_carbons = intensity / (iso_ab * mono_intensity);
                min_el_num = iso_chain.number_carbons * 0.8;
                max_el_num = iso_chain.number_carbons * 1.2;
              }

              if (iso == "13C" && s > 2)
              {
                min_el_num = iso_chain.number_carbons * 0.8;
                max_el_num = iso_chain.number_carbons * 1.2;
              }

              // when only one isotope atom, mostly for M+1 or M+2 in case of Cl, Br, Si and S
              if (iso_n == 1)
              {
                double min_coef = (min_el_num * std::pow(mono_ab, min_el_num - iso_n) * iso_ab) / std::pow(mono_ab, min_el_num);
                double max_coef = (max_el_num * std::pow(mono_ab, max_el_num - iso_n) * iso_ab) / std::pow(mono_ab, max_el_num);

                min_rel_int = min_rel_int * min_coef;
                max_rel_int = max_rel_int * max_coef;

                // when second time isotope, mostly for M+n, with n > 1
              }
              else
              {

                unsigned int fact = 1;
                for (int a = 1; a <= iso_n; ++a)
                  fact *= a;

                double min_coef = (std::pow(mono_ab, min_el_num - iso_n) * std::pow(iso_ab, iso_n)) / fact;
                double max_coef = (std::pow(mono_ab, max_el_num - iso_n) * std::pow(iso_ab, iso_n)) / fact;

                min_coef = min_coef / std::pow(mono_ab, min_el_num);
                max_coef = max_coef / std::pow(mono_ab, max_el_num);

                min_coef = min_coef * min_el_num * (min_el_num - 1);
                max_coef = max_coef * max_el_num * (max_el_num - 1);

                for (int t = 2; t <= iso_n - 1; ++t)
                {
                  min_coef = min_coef * (min_el_num - t);
                  max_coef = max_coef * (max_el_num - t);
                }

                min_rel_int = min_rel_int * min_coef;
                max_rel_int = max_rel_int * max_coef;
              }

            } // loop for each unique element in hit md

            const float rel_int = intensity / mono_intensity;

            // selection criteria for best isotope combination for candidate and updates isotopic chain
            // if next combination or candidate is better than previous replaces it
            if (candidate_mass_distance_error < combination_mass_error &&
                candidate_mass_distance_error <= mzr * 1.3 &&
                rel_int >= min_rel_int * 0.7 &&
                rel_int <= max_rel_int * 1.3)
            {

              if (was_annotated)
                if (af.iso_mass_distance_error[index] <= candidate_mass_distance_error)
                  continue;

              combination_mass_error = candidate_mass_distance_error;

              bool is_in_chain = false;

              for (size_t t = 1; t < iso_chain.feature.size(); ++t)
                if (iso_chain.feature[t] == feature)
                  is_in_chain = true;

              if (is_in_chain)
              {
                const int i = std::distance(iso_chain.feature.begin(), std::find(iso_chain.feature.begin(), iso_chain.feature.end(), feature));
                std::string concat_combination = combination[0];
                for (size_t e = 1; e < combination.size(); ++e)
                  concat_combination += "/" + combination[e];
                iso_chain.feature[i] = feature;
                iso_chain.index[i] = index;
                iso_chain.step[i] = i;
                iso_chain.mz[i] = mz;
                iso_chain.mzr[i] = mzr;
                iso_chain.rt[i] = rt;
                iso_chain.isotope[i] = concat_combination;
                iso_chain.mass_distance[i] = candidate_mass_distance;
                iso_chain.theoretical_mass_distance[i] = mass_distances[c];
                iso_chain.mass_distance_error[i] = candidate_mass_distance_error;
                iso_chain.time_error[i] = candidate_time_error;
                iso_chain.abundance[i] = rel_int;
                iso_chain.theoretical_abundance_min[i] = min_rel_int;
                iso_chain.theoretical_abundance_max[i] = max_rel_int;
              }
              else
              {
                std::string concat_combination = combination[0];
                for (size_t e = 1; e < combination.size(); ++e)
                  concat_combination += "/" + combination[e];
                iso_chain.feature.push_back(feature);
                iso_chain.index.push_back(index);
                iso_chain.step.push_back(s);
                iso_chain.charge.push_back(charge);
                iso_chain.mz.push_back(mz);
                iso_chain.mzr.push_back(mzr);
                iso_chain.rt.push_back(rt);
                iso_chain.isotope.push_back(concat_combination);
                iso_chain.mass_distance.push_back(candidate_mass_distance);
                iso_chain.theoretical_mass_distance.push_back(mass_distances[c]);
                iso_chain.mass_distance_error.push_back(candidate_mass_distance_error);
                iso_chain.time_error.push_back(candidate_time_error);
                iso_chain.abundance.push_back(rel_int);
                iso_chain.theoretical_abundance_min.push_back(min_rel_int);
                iso_chain.theoretical_abundance_max.push_back(max_rel_int);
                iso_chain.length++;
              }
            }
          } // c loop for each combination within the mass distance
        } // if candidate is within the mass distance
      } // candidate loop

      if (is_Mplus)
        break;

    } // isotopic step loop

    if (is_Mplus)
      break;

  } // charge loop

  if (!is_Mplus)
  {

    int best_chain = 0;

    for (int z = 0; z < number_charges; z++)
      if (isotopic_chains[z].length > isotopic_chains[best_chain].length)
        best_chain = z;

    MS_ISOTOPE_CHAIN &iso_chain = isotopic_chains[best_chain];

    af.index[mono_index] = mono_index;
    af.feature[mono_index] = mono_feature;
    af.component_feature[mono_index] = mono_feature;
    af.iso_step[mono_index] = 0;
    af.iso_cat[mono_index] = "M+0";
    af.iso_isotope[mono_index] = "";
    af.iso_charge[mono_index] = iso_chain.charge[0];
    af.iso_mzr[mono_index] = std::round(iso_chain.mzr[0] * 100000.0) / 100000.0;
    af.iso_mass_distance[mono_index] = 0;
    af.iso_theoretical_mass_distance[mono_index] = 0;
    af.iso_mass_distance_error[mono_index] = 0;
    af.iso_time_error[mono_index] = 0;
    af.iso_relative_intensity[mono_index] = 1;
    af.iso_theoretical_min_relative_intensity[mono_index] = 0;
    af.iso_theoretical_max_relative_intensity[mono_index] = 0;
    af.iso_size[mono_index] = iso_chain.length;
    iso_chain.number_carbons = std::round(iso_chain.number_carbons);
    af.iso_number_carbons[mono_index] = iso_chain.number_carbons;

    if (iso_chain.length > 1)
    {
      for (int i = 1; i < iso_chain.length; i++)
      {
        const int iso_index = iso_chain.index[i];
        af.index[iso_index] = iso_index;
        af.feature[iso_index] = iso_chain.feature[i];
        af.component_feature[iso_index] = mono_feature;
        af.iso_step[iso_index] = iso_chain.step[i];
        af.iso_cat[iso_index] = "M+" + std::to_string(iso_chain.step[i]);
        af.iso_charge[iso_index] = iso_chain.charge[i];
        af.iso_mzr[iso_index] = std::round(iso_chain.mzr[i] * 100000.0) / 100000.0;
        af.iso_mass_distance[iso_index] = std::round(iso_chain.mass_distance[i] * 100000.0) / 100000.0;
        af.iso_theoretical_mass_distance[iso_index] = std::round(iso_chain.theoretical_mass_distance[i] * 100000.0) / 100000.0;
        af.iso_mass_distance_error[iso_index] = std::round(iso_chain.mass_distance_error[i] * 100000.0) / 100000.0;
        af.iso_time_error[iso_index] = std::round(iso_chain.time_error[i] * 10.0) / 10.0;
        af.iso_relative_intensity[iso_index] = std::round(iso_chain.abundance[i] * 100000.0) / 100000.0;
        af.iso_theoretical_min_relative_intensity[iso_index] = std::round(iso_chain.theoretical_abundance_min[i] * 100000.0) / 100000.0;
        af.iso_theoretical_max_relative_intensity[iso_index] = std::round(iso_chain.theoretical_abundance_max[i] * 100000.0) / 100000.0;
        af.iso_size[iso_index] = iso_chain.length;
        af.iso_number_carbons[iso_index] = iso_chain.number_carbons;
        af.iso_isotope[iso_index] = iso_chain.isotope[i];

        af.iso_isotope[mono_index] += " " + iso_chain.isotope[i];
      }
    }
  }
};

// MARK: FIND_ADDUCT_CANDIDATES
std::vector<int> NTS::find_adduct_candidates(
    const int &number_features,
    const std::vector<float> &mzs,
    const std::vector<float> &rts,
    const std::vector<int> &pols,
    const std::vector<int> &iso_step,
    const int &pol,
    const float &mz,
    const float &mzmin,
    const float &mzmax,
    const float &rt,
    float &rtmin,
    float &rtmax,
    const float &rtWindowAlignment,
    const float &max_mz_adducts)
{
  std::vector<int> candidates;

  const float left_rt = rt - rtmin;
  const float right_rt = rtmax - rt;
  float rtW = right_rt;
  if (left_rt < right_rt)
    rtW = left_rt;
  rtW = rtW * rtWindowAlignment;
  rtmin = rt - rtW;
  rtmax = rt + rtW;

  for (int z = 0; z < number_features; ++z)
  {
    if (rts[z] >= rtmin && rts[z] <= rtmax && mzs[z] > mz && mzs[z] <= max_mz_adducts && pols[z] == pol && iso_step[z] == 0)
    {
      candidates.push_back(z);
    }
  }

  return candidates;
};

// MARK: ANNOTATE_ADDUCTS
void NTS::annotate_adducts(MS_ANNOTATION &af, const MS_CANDIDATE_CHAIN &candidates_chain, const int &pol)
{

  NTS::MS_ADDUCT_SET all_adducts;

  const float neutralizer = all_adducts.neutralizer(pol);

  std::vector<MS_ADDUCT> adducts = all_adducts.adducts(pol);

  const int number_candidates = candidates_chain.length;

  const std::string &mion_feature = candidates_chain.feature[0];
  const float &mion_mz = candidates_chain.mz[0];
  const float &mion_rt = candidates_chain.rt[0];
  const float &mion_mzr = candidates_chain.mzr[0];

  for (size_t a = 0; a < adducts.size(); ++a)
  {

    const MS_ADDUCT &adduct = adducts[a];

    const std::string &adduct_element = adduct.element;
    const std::string &adduct_cat = adduct.cat;
    const float &adduct_mass_distance = adduct.mass_distance;

    for (int c = 1; c < number_candidates; ++c)
    {

      const int &index = candidates_chain.index[c];

      if (af.adduct_cat[index] != "")
        continue;

      const std::string &feature = candidates_chain.feature[c];
      const float &mz = candidates_chain.mz[c];
      const float &rt = candidates_chain.rt[c];
      const float exp_mass_distance = mz - (mion_mz + neutralizer);
      const float time_error = std::abs(rt - mion_rt);
      const float mass_error = abs(exp_mass_distance - adduct_mass_distance);

      if (mass_error < mion_mzr)
      {
        af.index[index] = index;
        af.feature[index] = feature;
        af.component_feature[index] = mion_feature;
        af.adduct_cat[index] = adduct_cat;
        af.adduct_element[index] = adduct_element;
        af.adduct_time_error[index] = std::round(time_error * 10.0) / 10.0;
        af.adduct_mass_error[index] = std::round(mass_error * 100000.0) / 100000.0;
        break;
      }
    }
  }
};

// MARK: CLUSTER_SPECTRA
Rcpp::List NTS::cluster_spectra(const Rcpp::List &spectra, const float &mzClust = 0.005, const float &presence = 0.8)
{

  const std::vector<std::string> &names_spectra = spectra.names();

  const std::vector<std::string> must_have_names = {"polarity", "level", "rt", "mz", "intensity"};

  const int n_must_have_names = must_have_names.size();

  std::vector<bool> has_must_have_names(n_must_have_names, false);

  bool has_pre_ce = false;
  bool has_pre_mz = false;

  for (size_t i = 0; i < must_have_names.size(); ++i)
  {
    for (size_t j = 0; j < names_spectra.size(); ++j)
    {
      if (must_have_names[i] == names_spectra[j])
        has_must_have_names[i] = true;
      if (names_spectra[j] == "pre_ce")
        has_pre_ce = true;
      if (names_spectra[j] == "pre_mz")
        has_pre_mz = true;
    }
  }

  for (bool value : has_must_have_names)
  {
    if (!value)
    {
      throw std::runtime_error("The spectra must have the columns polarity, level, rt, pre_mz, mz and intensity!");
    }
  }

  const std::vector<int> &org_polarity = spectra["polarity"];
  const std::vector<int> &org_level = spectra["level"];
  const std::vector<float> &org_rt = spectra["rt"];
  const std::vector<float> &org_mz = spectra["mz"];
  const std::vector<float> &org_intensity = spectra["intensity"];

  const int n_traces = org_polarity.size();

  std::vector<float> org_pre_ce(n_traces);

  if (has_pre_ce)
  {
    const std::vector<float> &org_pre_ce_origin = spectra["pre_ce"];
    for (int i = 0; i < n_traces; ++i)
    {
      org_pre_ce[i] = org_pre_ce_origin[i];
    }
  }

  std::vector<float> org_pre_mz(n_traces);

  if (has_pre_mz)
  {
    const std::vector<float> &org_pre_mz_origin = spectra["pre_mz"];
    for (int i = 0; i < n_traces; ++i)
    {
      org_pre_mz[i] = org_pre_mz_origin[i];
    }
  }

  std::vector<int> idx(n_traces);
  std::iota(idx.begin(), idx.end(), 0);

  std::sort(idx.begin(), idx.end(), [&](int i, int j)
            { return org_mz[i] < org_mz[j]; });

  std::vector<float> rt(n_traces);
  float *rt_ptr = rt.data();

  std::vector<float> mz(n_traces);
  float *mz_ptr = mz.data();

  std::vector<float> intensity(n_traces);
  float *intensity_ptr = intensity.data();

  std::vector<float> pre_ce(n_traces);
  float *pre_ce_ptr = pre_ce.data();

  std::vector<float> pre_mz(n_traces);
  float *pre_mz_ptr = pre_mz.data();

  for (const int &x : idx)
  {
    *(rt_ptr++) = org_rt[x];
    *(mz_ptr++) = org_mz[x];
    *(intensity_ptr++) = org_intensity[x];
    *(pre_ce_ptr++) = org_pre_ce[x];
    *(pre_mz_ptr++) = org_pre_mz[x];
  }

  std::set<float> unique_rt_set;

  for (const float &r : rt)
  {
    unique_rt_set.insert(r);
  }

  std::vector<float> unique_rt(unique_rt_set.begin(), unique_rt_set.end());

  std::set<float> unique_pre_ce_set;

  for (const float &c : pre_ce)
  {
    unique_pre_ce_set.insert(c);
  }

  std::vector<float> unique_pre_ce(unique_pre_ce_set.begin(), unique_pre_ce_set.end());

  rt_ptr = rt.data();
  pre_ce_ptr = pre_ce.data();
  mz_ptr = mz.data();
  intensity_ptr = intensity.data();

  std::vector<float> mz_diff(n_traces - 1);

  for (int j = 1; j < n_traces; ++j)
  {
    mz_diff[j - 1] = mz[j] - mz[j - 1];
  }

  float itMzClust = mzClust;

  int counter = 0;

  bool hasFromSameScan = true;

  Rcpp::List out;

  while (hasFromSameScan)
  {

    counter = counter + 1;

    std::vector<float> new_mz;

    std::vector<float> new_intensity;

    std::vector<int> all_clusters(mz_diff.size(), 0);

    for (size_t j = 0; j < mz_diff.size(); ++j)
    {
      if (mz_diff[j] > itMzClust)
        all_clusters[j] = 1;
    }

    std::partial_sum(all_clusters.begin(), all_clusters.end(), all_clusters.begin());

    all_clusters.insert(all_clusters.begin(), 0);

    for (int &val : all_clusters)
    {
      val += 1;
    }

    int n_all_clusters = all_clusters.size();

    std::vector<int> idx_clusters(all_clusters.size());
    std::iota(idx_clusters.begin(), idx_clusters.end(), 0);

    std::set<int> clusters_set;

    for (const int &cl : all_clusters)
    {
      clusters_set.insert(cl);
    }

    std::vector<int> clusters(clusters_set.begin(), clusters_set.end());

    int n_clusters = clusters.size();

    std::vector<bool> fromSameScan(n_clusters, true);

    for (int z = 0; z < n_clusters; ++z)
    {

      std::vector<int> temp_idx;

      for (int j = 0; j < n_all_clusters; ++j)
      {
        if (all_clusters[j] == clusters[z])
          temp_idx.push_back(j);
      }

      int n = temp_idx.size();

      std::vector<float> temp_rt(n);
      float *temp_rt_ptr = temp_rt.data();

      std::vector<float> temp_pre_ce(n);
      float *temp_pre_ce_ptr = temp_pre_ce.data();

      std::vector<float> temp_mz(n);
      float *temp_mz_ptr = temp_mz.data();

      std::vector<float> temp_intensity(n);
      float *temp_intensity_ptr = temp_intensity.data();

      for (const int &x : temp_idx)
      {
        *(temp_rt_ptr++) = *(rt_ptr + x);
        *(temp_pre_ce_ptr++) = *(pre_ce_ptr + x);
        *(temp_mz_ptr++) = *(mz_ptr + x);
        *(temp_intensity_ptr++) = *(intensity_ptr + x);
      }

      std::set<float> unique_temp_rt_set;

      for (const float &r : temp_rt)
      {
        unique_temp_rt_set.insert(r);
      }

      std::vector<float> unique_temp_rt(unique_temp_rt_set.begin(), unique_temp_rt_set.end());

      std::set<float> unique_temp_pre_ce_set;

      for (const float &r : temp_pre_ce)
      {
        unique_temp_pre_ce_set.insert(r);
      }

      std::vector<float> unique_temp_pre_ce(unique_temp_pre_ce_set.begin(), unique_temp_pre_ce_set.end());

      fromSameScan[z] = unique_temp_rt.size() < temp_rt.size();

      if (counter > 10)
        fromSameScan[z] = false;

      if (itMzClust < 0.0001)
        fromSameScan[z] = false;

      // when traces are twice in a given scan for cluster breaks the for loop and decreases the itMzClust
      if (fromSameScan[z])
      {
        itMzClust = itMzClust - 0.0001;
        break;
      }

      bool enough_presence = false;

      if (unique_temp_pre_ce.size() < unique_pre_ce.size())
      {
        enough_presence = unique_rt.size() * (unique_temp_pre_ce.size() / unique_pre_ce.size()) * presence <= unique_temp_rt.size();
      }
      else
      {
        enough_presence = unique_rt.size() * presence <= unique_temp_rt.size();
      }

      // when is not enough present skips the m/z cluster
      if (!enough_presence)
        continue;

      auto max_intensity_ptr = std::max_element(temp_intensity.begin(), temp_intensity.end());
      new_intensity.push_back(*max_intensity_ptr);

      int size_temp_mz = temp_mz.size();

      float mz_sum = 0, mz_numWeight = 0;

      for (int w = 0; w < size_temp_mz; w++)
      {
        mz_numWeight = mz_numWeight + temp_mz[w] * temp_intensity[w];
        mz_sum = mz_sum + temp_intensity[w];
      }

      float mean_mz = mz_numWeight / mz_sum;

      new_mz.push_back(mean_mz);

    } // end of clusters for loop

    if (new_mz.size() > 0)
    {

      float rt_mean = 0;

      for (float val : rt)
      {
        rt_mean += val;
      }

      rt_mean = rt_mean / rt.size();

      if (has_pre_mz)
      {

        float pre_mz_mean = 0;

        std::vector<bool> is_pre(new_mz.size(), false);

        for (const float &val : pre_mz)
        {
          pre_mz_mean += val;
        }

        pre_mz_mean = pre_mz_mean / pre_mz.size();

        if (!std::isnan(pre_mz_mean))
        {
          for (size_t p = 0; p < new_mz.size(); p++)
          {
            if ((new_mz[p] >= pre_mz_mean - mzClust) && (new_mz[p] <= pre_mz_mean + mzClust))
            {
              is_pre[p] = true;
            }
          }
        }

        const std::vector<int> polarity_out(new_mz.size(), org_polarity[0]);
        const std::vector<int> level_out(new_mz.size(), org_level[0]);
        const std::vector<float> pre_mz_out(new_mz.size(), pre_mz_mean);
        const std::vector<float> rt_out(new_mz.size(), rt_mean);

        out["polarity"] = polarity_out;
        out["level"] = level_out;
        out["pre_mz"] = pre_mz_out;
        out["rt"] = rt_out;
        out["mz"] = new_mz;
        out["intensity"] = new_intensity;
        out["is_pre"] = is_pre;
      }
      else
      {

        const std::vector<int> polarity_out(new_mz.size(), org_polarity[0]);
        const std::vector<int> level_out(new_mz.size(), org_level[0]);
        const std::vector<float> rt_out(new_mz.size(), rt_mean);

        out["polarity"] = polarity_out;
        out["level"] = level_out;
        out["rt"] = rt_out;
        out["mz"] = new_mz;
        out["intensity"] = new_intensity;
      }
    }

    hasFromSameScan = false;

    for (const bool &l : fromSameScan)
    {
      if (l)
      {
        hasFromSameScan = true;
        break;
      }
    }
  } // end of while loop

  return out;
};
