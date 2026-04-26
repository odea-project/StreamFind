// mass_spec_export.cpp — Rcpp bridge for ms:: algorithms
// Provides 9 exported functions callable from R via .Call.

// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
#include "mass_spec/mass_spec.h"
#include "mass_spec/utils.h"
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <set>

using namespace Rcpp;

// ============================================================================
// Helpers for grouping chromatogram rows by (analysis, id)
// ============================================================================

namespace {

struct GroupKey {
    std::string analysis;
    std::string id;
    bool operator<(const GroupKey& o) const {
        if (analysis != o.analysis) return analysis < o.analysis;
        return id < o.id;
    }
};

/// Build a sorted map from group key → vector of row indices.
std::map<GroupKey, std::vector<int>> build_groups(
    const CharacterVector& analysis_col,
    const CharacterVector& id_col)
{
    std::map<GroupKey, std::vector<int>> groups;
    const int n = analysis_col.size();
    for (int i = 0; i < n; ++i)
        groups[{as<std::string>(analysis_col[i]), as<std::string>(id_col[i])}].push_back(i);
    return groups;
}

/// Sort a group's indices by rt (or x-axis).
void sort_by_rt(std::vector<int>& idx, const NumericVector& rt_col) {
    std::sort(idx.begin(), idx.end(), [&](int a, int b){
        return rt_col[a] < rt_col[b];
    });
}

} // anonymous namespace


// ============================================================================
// MARK: rcpp_ms_smooth_chromatograms
// ============================================================================

//' Smooth chromatograms using moving-average or Savitzky-Golay filter
//'
//' @param chromatograms  data.frame with columns: analysis, replicate, index, id,
//'                       polarity, pre_mz, pre_ce, pro_mz, rt, intensity
//' @param method         "movingaverage" or "savgol"
//' @param window_size    window size (odd integer) for moving average
//' @param fl             filter length (odd) for SG
//' @param forder         polynomial order for SG
//' @param dorder         derivative order for SG (0 = smoothing)
//' @return data.frame with updated intensity column
// [[Rcpp::export]]
DataFrame rcpp_ms_smooth_chromatograms(
    DataFrame chromatograms,
    std::string method     = "movingaverage",
    int window_size        = 5,
    int fl                 = 11,
    int forder             = 4,
    int dorder             = 0)
{
    CharacterVector analysis_col = chromatograms["analysis"];
    CharacterVector id_col       = chromatograms["id"];
    NumericVector   rt_col       = chromatograms["rt"];
    NumericVector   int_col      = chromatograms["intensity"];

    NumericVector out_int = clone(int_col);

    auto groups = build_groups(analysis_col, id_col);
    for (auto& kv : groups) {
        auto& idx = kv.second;
        sort_by_rt(idx, rt_col);

        std::vector<double> y(idx.size());
        for (int k = 0; k < (int)idx.size(); ++k) y[k] = int_col[idx[k]];

        std::vector<double> smoothed;
        if (method == "savgol") {
            smoothed = ms::savitzky_golay(y, fl, forder, dorder);
        } else {
            smoothed = ms::moving_average(y, window_size);
        }
        for (int k = 0; k < (int)idx.size(); ++k)
            out_int[idx[k]] = smoothed[k];
    }

    DataFrame result = clone(chromatograms);
    result["intensity"] = out_int;
    return result;
}


// ============================================================================
// MARK: rcpp_ms_correct_baseline_airpls
// ============================================================================

//' Correct chromatogram baselines using airPLS
//'
//' @param chromatograms  data.frame with at least: analysis, id, rt, intensity
//' @param lambda         smoothing penalty
//' @param differences    order of difference operator (1 or 2)
//' @param itermax        maximum iterations
//' @return data.frame with updated intensity, and added/updated baseline + raw columns
// [[Rcpp::export]]
DataFrame rcpp_ms_correct_baseline_airpls(
    DataFrame chromatograms,
    double lambda      = 10.0,
    int differences    = 1,
    int itermax        = 20)
{
    CharacterVector analysis_col = chromatograms["analysis"];
    CharacterVector id_col       = chromatograms["id"];
    NumericVector   rt_col       = chromatograms["rt"];
    NumericVector   int_col      = chromatograms["intensity"];

    const int n = int_col.size();
    NumericVector out_int      = clone(int_col);
    NumericVector out_baseline = NumericVector(n, NA_REAL);
    NumericVector out_raw      = clone(int_col);

    // If raw column already exists, use it as the true raw signal
    bool has_raw = chromatograms.containsElementNamed("raw");
    if (has_raw) {
        NumericVector raw_col = chromatograms["raw"];
        for (int i = 0; i < n; ++i) {
            if (!NumericVector::is_na(raw_col[i])) out_raw[i] = raw_col[i];
        }
    }

    auto groups = build_groups(analysis_col, id_col);
    for (auto& kv : groups) {
        auto& idx = kv.second;
        sort_by_rt(idx, rt_col);

        std::vector<double> y(idx.size());
        for (int k = 0; k < (int)idx.size(); ++k) y[k] = out_raw[idx[k]];

        std::vector<double> baseline = ms::airpls_baseline(y, lambda, differences, itermax);

        for (int k = 0; k < (int)idx.size(); ++k) {
            double corrected = y[k] - baseline[k];
            out_int[idx[k]]      = (corrected > 0.0) ? corrected : 0.0;
            out_baseline[idx[k]] = baseline[k];
            out_raw[idx[k]]      = y[k];
        }
    }

    DataFrame result = clone(chromatograms);
    result["intensity"] = out_int;
    result["baseline"]  = out_baseline;
    result["raw"]       = out_raw;
    return result;
}


// ============================================================================
// MARK: rcpp_ms_correct_baseline_als
// ============================================================================

//' Correct chromatogram baselines using Asymmetric Least Squares (ALS)
//'
//' @param chromatograms  data.frame with at least: analysis, id, rt, intensity
//' @param lambda         smoothing penalty
//' @param p              asymmetry parameter (0 < p < 0.5; typical 0.001–0.01)
//' @param maxit          maximum iterations
//' @return data.frame with updated intensity, and added/updated baseline + raw columns
// [[Rcpp::export]]
DataFrame rcpp_ms_correct_baseline_als(
    DataFrame chromatograms,
    double lambda = 1e5,
    double p      = 0.001,
    int    maxit  = 10)
{
    CharacterVector analysis_col = chromatograms["analysis"];
    CharacterVector id_col       = chromatograms["id"];
    NumericVector   rt_col       = chromatograms["rt"];
    NumericVector   int_col      = chromatograms["intensity"];

    const int n = int_col.size();
    NumericVector out_int      = clone(int_col);
    NumericVector out_baseline = NumericVector(n, NA_REAL);
    NumericVector out_raw      = clone(int_col);

    bool has_raw = chromatograms.containsElementNamed("raw");
    if (has_raw) {
        NumericVector raw_col = chromatograms["raw"];
        for (int i = 0; i < n; ++i)
            if (!NumericVector::is_na(raw_col[i])) out_raw[i] = raw_col[i];
    }

    auto groups = build_groups(analysis_col, id_col);
    for (auto& kv : groups) {
        auto& idx = kv.second;
        sort_by_rt(idx, rt_col);

        std::vector<double> y(idx.size());
        for (int k = 0; k < (int)idx.size(); ++k) y[k] = out_raw[idx[k]];

        std::vector<double> baseline = ms::als_baseline(y, lambda, p, maxit);

        for (int k = 0; k < (int)idx.size(); ++k) {
            double corrected = y[k] - baseline[k];
            out_int[idx[k]]      = (corrected > 0.0) ? corrected : 0.0;
            out_baseline[idx[k]] = baseline[k];
            out_raw[idx[k]]      = y[k];
        }
    }

    DataFrame result = clone(chromatograms);
    result["intensity"] = out_int;
    result["baseline"]  = out_baseline;
    result["raw"]       = out_raw;
    return result;
}


// ============================================================================
// MARK: rcpp_ms_find_chrom_peaks_local_maxima
// ============================================================================

//' Find chromatographic peaks using local-maxima method
//'
//' @param chromatograms  data.frame with analysis, replicate, index, id, polarity,
//'                       pre_mz, pre_ce, pro_mz, rt, intensity
//' @param min_height     minimum peak height
//' @param min_width      minimum peak width (rt units; 0 = no filter)
//' @param max_width      maximum peak width (rt units; 0 = no filter)
//' @return data.frame of peaks with schema matching MassSpecResults_Chromatograms Peaks table
// [[Rcpp::export]]
DataFrame rcpp_ms_find_chrom_peaks_local_maxima(
    DataFrame chromatograms,
    double min_height = 0.0,
    double min_width  = 0.0,
    double max_width  = 0.0)
{
    CharacterVector analysis_col  = chromatograms["analysis"];
    CharacterVector replicate_col = chromatograms["replicate"];
    IntegerVector   index_col     = chromatograms["index"];
    CharacterVector id_col        = chromatograms["id"];
    IntegerVector   polarity_col  = chromatograms["polarity"];
    NumericVector   pre_mz_col    = chromatograms["pre_mz"];
    NumericVector   pre_ce_col    = chromatograms["pre_ce"];
    NumericVector   pro_mz_col    = chromatograms["pro_mz"];
    NumericVector   rt_col        = chromatograms["rt"];
    NumericVector   int_col       = chromatograms["intensity"];

    // Output columns
    CharacterVector out_analysis;
    CharacterVector out_replicate;
    IntegerVector   out_index;
    CharacterVector out_id;
    CharacterVector out_peak;
    IntegerVector   out_polarity;
    NumericVector   out_pre_mz;
    NumericVector   out_pre_ce;
    NumericVector   out_pro_mz;
    NumericVector   out_rt;
    NumericVector   out_rtmin;
    NumericVector   out_rtmax;
    NumericVector   out_intensity;
    NumericVector   out_area;
    NumericVector   out_sn;
    NumericVector   out_width;
    NumericVector   out_fwhm;
    NumericVector   out_tailing;
    NumericVector   out_asymmetry;
    NumericVector   out_gaussian_similarity;

    auto groups = build_groups(analysis_col, id_col);
    for (auto& kv : groups) {
        const auto& gkey = kv.first;
        auto& idx = kv.second;
        sort_by_rt(idx, rt_col);

        // Use first row for metadata
        int ref = idx[0];
        std::string aname = gkey.analysis;
        std::string gid   = gkey.id;

        std::vector<double> rt(idx.size()), y(idx.size());
        for (int k = 0; k < (int)idx.size(); ++k) {
            rt[k] = rt_col[idx[k]];
            y[k]  = int_col[idx[k]];
        }

        auto peaks = ms::find_peaks_local_maxima(rt, y, min_height, min_width, max_width);

        int pk_num = 1;
        for (auto& pk : peaks) {
            std::string peak_id = gid + "_" + std::to_string(pk_num++);
            out_analysis.push_back(aname);
            out_replicate.push_back(as<std::string>(replicate_col[ref]));
            out_index.push_back(index_col[ref]);
            out_id.push_back(gid);
            out_peak.push_back(peak_id);
            out_polarity.push_back(polarity_col[ref]);
            out_pre_mz.push_back(pre_mz_col[ref]);
            out_pre_ce.push_back(pre_ce_col[ref]);
            out_pro_mz.push_back(pro_mz_col[ref]);
            out_rt.push_back(pk.x_val);
            out_rtmin.push_back(pk.x_min);
            out_rtmax.push_back(pk.x_max);
            out_intensity.push_back(pk.intensity);
            out_area.push_back(pk.area);
            out_sn.push_back(pk.sn);
            out_width.push_back(pk.width);
            out_fwhm.push_back(NA_REAL);
            out_tailing.push_back(NA_REAL);
            out_asymmetry.push_back(NA_REAL);
            out_gaussian_similarity.push_back(NA_REAL);
        }
    }

    return DataFrame::create(
        Named("analysis")             = out_analysis,
        Named("replicate")            = out_replicate,
        Named("index")                = out_index,
        Named("id")                   = out_id,
        Named("peak")                 = out_peak,
        Named("polarity")             = out_polarity,
        Named("pre_mz")               = out_pre_mz,
        Named("pre_ce")               = out_pre_ce,
        Named("pro_mz")               = out_pro_mz,
        Named("rt")                   = out_rt,
        Named("rtmin")                = out_rtmin,
        Named("rtmax")                = out_rtmax,
        Named("intensity")            = out_intensity,
        Named("area")                 = out_area,
        Named("sn")                   = out_sn,
        Named("width")                = out_width,
        Named("fwhm")                 = out_fwhm,
        Named("tailing")              = out_tailing,
        Named("asymmetry")            = out_asymmetry,
        Named("gaussian_similarity")  = out_gaussian_similarity,
        Named("stringsAsFactors")     = false
    );
}


// ============================================================================
// MARK: rcpp_ms_integrate_chromatograms
// ============================================================================

//' Find chromatographic peaks and integrate using pracma-style algorithm
//'
//' @param chromatograms       data.frame (same columns as above)
//' @param merge               merge close-by peaks?
//' @param close_by_threshold  merge distance in rt units
//' @param min_peak_height     minimum peak height
//' @param min_peak_distance   minimum distance between peaks (rt units)
//' @param min_peak_width      minimum peak width (rt units)
//' @param max_peak_width      maximum peak width (rt units)
//' @param min_sn              minimum signal-to-noise
//' @return data.frame of peaks
// [[Rcpp::export]]
DataFrame rcpp_ms_integrate_chromatograms(
    DataFrame chromatograms,
    bool   merge              = true,
    double close_by_threshold = 45.0,
    double min_peak_height    = 0.0,
    double min_peak_distance  = 10.0,
    double min_peak_width     = 5.0,
    double max_peak_width     = 120.0,
    double min_sn             = 10.0)
{
    CharacterVector analysis_col  = chromatograms["analysis"];
    CharacterVector replicate_col = chromatograms["replicate"];
    IntegerVector   index_col     = chromatograms["index"];
    CharacterVector id_col        = chromatograms["id"];
    IntegerVector   polarity_col  = chromatograms["polarity"];
    NumericVector   pre_mz_col    = chromatograms["pre_mz"];
    NumericVector   pre_ce_col    = chromatograms["pre_ce"];
    NumericVector   pro_mz_col    = chromatograms["pro_mz"];
    NumericVector   rt_col        = chromatograms["rt"];
    NumericVector   int_col       = chromatograms["intensity"];

    CharacterVector out_analysis;
    CharacterVector out_replicate;
    IntegerVector   out_index;
    CharacterVector out_id;
    CharacterVector out_peak;
    IntegerVector   out_polarity;
    NumericVector   out_pre_mz;
    NumericVector   out_pre_ce;
    NumericVector   out_pro_mz;
    NumericVector   out_rt;
    NumericVector   out_rtmin;
    NumericVector   out_rtmax;
    NumericVector   out_intensity;
    NumericVector   out_area;
    NumericVector   out_sn;
    NumericVector   out_width;
    NumericVector   out_fwhm;
    NumericVector   out_tailing;
    NumericVector   out_asymmetry;
    NumericVector   out_gaussian_similarity;

    auto groups = build_groups(analysis_col, id_col);
    for (auto& kv : groups) {
        const auto& gkey = kv.first;
        auto& idx = kv.second;
        sort_by_rt(idx, rt_col);

        int ref = idx[0];
        std::string aname = gkey.analysis;
        std::string gid   = gkey.id;

        std::vector<double> rt(idx.size()), y(idx.size());
        for (int k = 0; k < (int)idx.size(); ++k) {
            rt[k] = rt_col[idx[k]];
            y[k]  = int_col[idx[k]];
        }

        auto peaks = ms::find_peaks_pracma(rt, y, merge, close_by_threshold,
                                            min_peak_height, min_peak_distance,
                                            min_peak_width, max_peak_width, min_sn);

        int pk_num = 1;
        for (auto& pk : peaks) {
            std::string peak_id = gid + "_" + std::to_string(pk_num++);
            out_analysis.push_back(aname);
            out_replicate.push_back(as<std::string>(replicate_col[ref]));
            out_index.push_back(index_col[ref]);
            out_id.push_back(gid);
            out_peak.push_back(peak_id);
            out_polarity.push_back(polarity_col[ref]);
            out_pre_mz.push_back(pre_mz_col[ref]);
            out_pre_ce.push_back(pre_ce_col[ref]);
            out_pro_mz.push_back(pro_mz_col[ref]);
            out_rt.push_back(pk.x_val);
            out_rtmin.push_back(pk.x_min);
            out_rtmax.push_back(pk.x_max);
            out_intensity.push_back(pk.intensity);
            out_area.push_back(pk.area);
            out_sn.push_back(pk.sn);
            out_width.push_back(pk.width);
            out_fwhm.push_back(NA_REAL);
            out_tailing.push_back(NA_REAL);
            out_asymmetry.push_back(NA_REAL);
            out_gaussian_similarity.push_back(NA_REAL);
        }
    }

    return DataFrame::create(
        Named("analysis")             = out_analysis,
        Named("replicate")            = out_replicate,
        Named("index")                = out_index,
        Named("id")                   = out_id,
        Named("peak")                 = out_peak,
        Named("polarity")             = out_polarity,
        Named("pre_mz")               = out_pre_mz,
        Named("pre_ce")               = out_pre_ce,
        Named("pro_mz")               = out_pro_mz,
        Named("rt")                   = out_rt,
        Named("rtmin")                = out_rtmin,
        Named("rtmax")                = out_rtmax,
        Named("intensity")            = out_intensity,
        Named("area")                 = out_area,
        Named("sn")                   = out_sn,
        Named("width")                = out_width,
        Named("fwhm")                 = out_fwhm,
        Named("tailing")              = out_tailing,
        Named("asymmetry")            = out_asymmetry,
        Named("gaussian_similarity")  = out_gaussian_similarity,
        Named("stringsAsFactors")     = false
    );
}


// ============================================================================
// MARK: rcpp_ms_quantify_peaks_ols
// ============================================================================

//' Quantify peaks using OLS calibration
//'
//' Fits a polynomial (degree 1, 2 or 3) through the calibration points and
//' predicts concentration for all peaks using the fitted model.
//'
//' @param peaks          data.frame with at minimum: analysis, id, peak, area or intensity.
//'                       Calibration analyses must have a non-NA concentration column.
//' @param concentrations Named numeric vector: names = analysis names, values = concentration
//'                       (NA for unknowns).
//' @param value          "area" or "intensity" – which signal measure to fit against
//' @param model          "linear", "quadratic", or "cubic"
//' @return peaks data.frame with added column "concentration"
// [[Rcpp::export]]
DataFrame rcpp_ms_quantify_peaks_ols(
    DataFrame       peaks,
    NumericVector   concentrations,
    std::string     value = "area",
    std::string     model = "linear")
{
    // concentrations is a named numeric vector (names = analysis names)
    CharacterVector conc_names = concentrations.names();

    CharacterVector analysis_col = peaks["analysis"];
    CharacterVector id_col       = peaks["id"];
    CharacterVector peak_col     = peaks["peak"];
    const int n = analysis_col.size();

    NumericVector signal_col = peaks.containsElementNamed(value.c_str())
        ? peaks[value] : peaks["area"];

    // Build calibration set per chrom id
    // Key: id;  Value: (concentration, signal) pairs
    std::map<std::string, std::vector<std::pair<double,double>>> calib;
    for (int i = 0; i < n; ++i) {
        std::string an = as<std::string>(analysis_col[i]);
        // Lookup concentration for this analysis
        double conc = NA_REAL;
        for (int k = 0; k < (int)conc_names.size(); ++k) {
            if (as<std::string>(conc_names[k]) == an) {
                conc = concentrations[k];
                break;
            }
        }
        if (NumericVector::is_na(conc)) continue;
        std::string id = as<std::string>(id_col[i]);
        double sig = signal_col[i];
        if (!NumericVector::is_na(sig)) calib[id].push_back({conc, sig});
    }

    // Fit model per id
    int degree = 1;
    if (model == "quadratic") degree = 2;
    if (model == "cubic")     degree = 3;

    std::map<std::string, ms::OLSResult> models;
    for (auto& kv : calib) {
        if ((int)kv.second.size() < degree + 1) continue;
        std::vector<double> xs, ys;
        for (auto& p : kv.second) { xs.push_back(p.first); ys.push_back(p.second); }
        models[kv.first] = ms::ols_fit(xs, ys, degree);
    }

    // Predict
    NumericVector conc_out(n, NA_REAL);
    for (int i = 0; i < n; ++i) {
        std::string id = as<std::string>(id_col[i]);
        auto it = models.find(id);
        if (it == models.end() || !it->second.success) continue;
        double sig = signal_col[i];
        if (NumericVector::is_na(sig)) continue;
        // Invert model: find concentration c such that f(c) ≈ sig
        // For linear: c = (sig - b0) / b1
        // For higher degrees: simple Newton on f(c) - sig = 0
        const auto& coefs = it->second.coefficients;
        const int m = (int)coefs.size();
        if (m < 2 || std::abs(coefs[1]) < 1e-14) continue;
        double c = (sig - coefs[0]) / coefs[1];  // linear initial guess
        if (m > 2) {
            // Newton iterations
            for (int iter = 0; iter < 20; ++iter) {
                double fc = ms::ols_predict(it->second, c) - sig;
                // derivative: f'(c) = b1 + 2*b2*c + ...
                double dfc = 0.0;
                double cpow = 1.0;
                for (int j = 1; j < m; ++j) { dfc += (double)j * coefs[j] * cpow; cpow *= c; }
                if (std::abs(dfc) < 1e-14) break;
                double step = fc / dfc;
                c -= step;
                if (std::abs(step) < 1e-9) break;
            }
        }
        conc_out[i] = c;
    }

    DataFrame result = clone(peaks);
    result["concentration"] = conc_out;
    return result;
}


// ============================================================================
// MARK: rcpp_ms_calculate_spectra_charges
// ============================================================================

//' Calculate charge states for mass spectra
//'
//' Groups input rows by (analysis, id), averages spectra across scans per group,
//' then calls the C++ charge-state algorithm.
//'
//' @param spectra     data.frame with: analysis, replicate, id, polarity, rt, mz, intensity
//' @param round_val   m/z rounding denominator for clustering (e.g. 35)
//' @param rel_low_cut relative intensity cut (fraction of base peak)
//' @param abs_low_cut absolute intensity cut
//' @param top_charges number of top anchor candidates
//' @return data.frame with columns: analysis, replicate, id, polarity, mz, intensity,
//'         cluster_mz, z, mass, rt
// [[Rcpp::export]]
DataFrame rcpp_ms_calculate_spectra_charges(
    DataFrame spectra,
    double round_val   = 35.0,
    double rel_low_cut = 0.2,
    double abs_low_cut = 300.0,
    int    top_charges = 5)
{
    CharacterVector analysis_col  = spectra["analysis"];
    CharacterVector replicate_col = spectra["replicate"];
    CharacterVector id_col        = spectra["id"];
    IntegerVector   polarity_col  = spectra["polarity"];
    NumericVector   rt_col        = spectra["rt"];
    NumericVector   mz_col        = spectra["mz"];
    NumericVector   int_col       = spectra["intensity"];

    CharacterVector out_analysis;
    CharacterVector out_replicate;
    CharacterVector out_id;
    IntegerVector   out_polarity;
    NumericVector   out_rt;
    NumericVector   out_mz;
    NumericVector   out_intensity;
    NumericVector   out_cluster_mz;
    IntegerVector   out_z;
    NumericVector   out_mass;

    auto groups = build_groups(analysis_col, id_col);
    for (auto& kv : groups) {
        const auto& gkey = kv.first;
        const auto& idx  = kv.second;

        int ref = idx[0];
        int pol = polarity_col[ref];

        // Average mz across scans: bin by rounded mz, average rt, sum intensity
        std::map<double, std::pair<double,double>> merged; // rounded_mz → (sum_int, cnt_rt)
        std::map<double, int> cnt;
        for (int k : idx) {
            double rmz = std::round(mz_col[k] * 1000.0) / 1000.0; // 3 decimal places
            merged[rmz].first  += int_col[k];
            merged[rmz].second += rt_col[k];
            cnt[rmz]++;
        }

        // Build spectrum points
        std::vector<ms::SpectrumPoint> pts;
        pts.reserve(merged.size());
        for (auto& mv : merged)
            pts.push_back({mv.first, mv.second.first / (double)cnt[mv.first]});

        // Sort by mz
        std::sort(pts.begin(), pts.end(), [](const ms::SpectrumPoint& a, const ms::SpectrumPoint& b){
            return a.mz < b.mz;
        });

        double avg_rt = 0.0;
        for (int k : idx) avg_rt += rt_col[k];
        avg_rt /= idx.size();

        auto charges = ms::calculate_spectra_charges(pts, pol, round_val, rel_low_cut, abs_low_cut, top_charges);

        for (auto& ch : charges) {
            out_analysis.push_back(gkey.analysis);
            out_replicate.push_back(as<std::string>(replicate_col[ref]));
            out_id.push_back(gkey.id);
            out_polarity.push_back(pol);
            out_rt.push_back(avg_rt);
            out_mz.push_back(ch.mz);
            out_intensity.push_back(ch.intensity);
            out_cluster_mz.push_back(ch.cluster_mz);
            out_z.push_back(ch.z);
            out_mass.push_back(ch.mass);
        }
    }

    return DataFrame::create(
        Named("analysis")    = out_analysis,
        Named("replicate")   = out_replicate,
        Named("id")          = out_id,
        Named("polarity")    = out_polarity,
        Named("rt")          = out_rt,
        Named("mz")          = out_mz,
        Named("intensity")   = out_intensity,
        Named("cluster_mz")  = out_cluster_mz,
        Named("z")           = out_z,
        Named("mass")        = out_mass,
        Named("stringsAsFactors") = false
    );
}


// ============================================================================
// MARK: rcpp_ms_deconvolute_spectra
// ============================================================================

//' Deconvolute charge-state spectra to neutral-mass spectra
//'
//' @param spectra   data.frame with: analysis, replicate, id, polarity, rt, mz, intensity
//' @param charges   data.frame from rcpp_ms_calculate_spectra_charges:
//'                  analysis, id, polarity, mz, z, cluster_mz, mass
//' @param clust_val mass clustering tolerance (Da)
//' @param window    m/z window around each charge to extract (half-width in m/z units)
//' @return data.frame with same schema as spectra but mz column replaced by mass
// [[Rcpp::export]]
DataFrame rcpp_ms_deconvolute_spectra(
    DataFrame spectra,
    DataFrame charges,
    double clust_val = 0.1,
    double window    = 20.0)
{
    CharacterVector s_analysis  = spectra["analysis"];
    CharacterVector s_replicate = spectra["replicate"];
    CharacterVector s_id        = spectra["id"];
    IntegerVector   s_polarity  = spectra["polarity"];
    NumericVector   s_rt        = spectra["rt"];
    NumericVector   s_mz        = spectra["mz"];
    NumericVector   s_int       = spectra["intensity"];

    CharacterVector c_analysis  = charges["analysis"];
    CharacterVector c_id        = charges["id"];
    IntegerVector   c_polarity  = charges["polarity"];
    IntegerVector   c_z         = charges["z"];
    NumericVector   c_mz        = charges["mz"];
    NumericVector   c_cmz       = charges["cluster_mz"];
    NumericVector   c_mass      = charges["mass"];

    // Index charge rows by (analysis, id)
    std::map<GroupKey, std::vector<ms::ChargeRow>> charge_map;
    for (int i = 0; i < (int)c_analysis.size(); ++i) {
        GroupKey gk = {as<std::string>(c_analysis[i]), as<std::string>(c_id[i])};
        ms::ChargeRow cr;
        cr.mz         = c_mz[i];
        cr.intensity  = 0.0;
        cr.cluster_mz = c_cmz[i];
        cr.z          = c_z[i];
        cr.mass       = c_mass[i];
        cr.polarity   = c_polarity[i];
        charge_map[gk].push_back(cr);
    }

    CharacterVector out_analysis;
    CharacterVector out_replicate;
    CharacterVector out_id;
    IntegerVector   out_polarity;
    NumericVector   out_rt;
    NumericVector   out_mass;
    NumericVector   out_intensity;

    auto groups = build_groups(s_analysis, s_id);
    for (auto& kv : groups) {
        const auto& gkey = kv.first;
        const auto& idx  = kv.second;

        auto charge_it = charge_map.find(gkey);
        if (charge_it == charge_map.end()) continue;
        const auto& crows = charge_it->second;

        int ref = idx[0];
        double avg_rt = 0.0;
        for (int k : idx) avg_rt += s_rt[k];
        avg_rt /= idx.size();

        // Build sorted spectrum
        std::vector<ms::SpectrumPoint> pts;
        pts.reserve(idx.size());
        for (int k : idx) pts.push_back({s_mz[k], s_int[k]});
        std::sort(pts.begin(), pts.end(), [](const ms::SpectrumPoint& a, const ms::SpectrumPoint& b){
            return a.mz < b.mz;
        });

        auto mass_pts = ms::deconvolute_spectrum(pts, crows, clust_val, window);

        for (auto& mp : mass_pts) {
            out_analysis.push_back(gkey.analysis);
            out_replicate.push_back(as<std::string>(s_replicate[ref]));
            out_id.push_back(gkey.id);
            out_polarity.push_back(s_polarity[ref]);
            out_rt.push_back(avg_rt);
            out_mass.push_back(mp.mass);
            out_intensity.push_back(mp.intensity);
        }
    }

    return DataFrame::create(
        Named("analysis")   = out_analysis,
        Named("replicate")  = out_replicate,
        Named("id")         = out_id,
        Named("polarity")   = out_polarity,
        Named("rt")         = out_rt,
        Named("mass")       = out_mass,
        Named("intensity")  = out_intensity,
        Named("stringsAsFactors") = false
    );
}


// ============================================================================
// MARK: rcpp_ms_find_spectra_maxima
// ============================================================================

//' Find peaks/maxima in (deconvoluted) spectra
//'
//' Operates identically to rcpp_ms_find_chrom_peaks_local_maxima but on the
//' mass/intensity axis of spectra.
//'
//' @param spectra     data.frame with: analysis, replicate, id, polarity, rt, mass, intensity
//' @param min_height  minimum peak height
//' @param min_width   minimum width in mass units (0 = no filter)
//' @param max_width   maximum width in mass units (0 = no filter)
//' @return data.frame of spectral peaks
// [[Rcpp::export]]
DataFrame rcpp_ms_find_spectra_maxima(
    DataFrame spectra,
    double min_height = 0.0,
    double min_width  = 0.0,
    double max_width  = 0.0)
{
    CharacterVector analysis_col  = spectra["analysis"];
    CharacterVector replicate_col = spectra["replicate"];
    CharacterVector id_col        = spectra["id"];
    IntegerVector   polarity_col  = spectra["polarity"];
    NumericVector   rt_col        = spectra["rt"];

    // Accept either "mass" or "mz" column as the x axis
    NumericVector mass_col = spectra.containsElementNamed("mass")
        ? spectra["mass"] : spectra["mz"];
    NumericVector int_col = spectra["intensity"];

    CharacterVector out_analysis;
    CharacterVector out_replicate;
    CharacterVector out_id;
    IntegerVector   out_polarity;
    NumericVector   out_rt;
    NumericVector   out_mass;
    NumericVector   out_mass_min;
    NumericVector   out_mass_max;
    NumericVector   out_intensity;
    NumericVector   out_area;
    NumericVector   out_sn;
    NumericVector   out_width;

    auto groups = build_groups(analysis_col, id_col);
    for (auto& kv : groups) {
        const auto& gkey = kv.first;
        auto& idx = kv.second;

        // Sort by mass
        std::sort(idx.begin(), idx.end(), [&](int a, int b){
            return mass_col[a] < mass_col[b];
        });

        int ref = idx[0];
        std::vector<double> mass(idx.size()), y(idx.size());
        for (int k = 0; k < (int)idx.size(); ++k) {
            mass[k] = mass_col[idx[k]];
            y[k]    = int_col[idx[k]];
        }

        double avg_rt = 0.0;
        for (int k : idx) avg_rt += rt_col[k];
        avg_rt /= idx.size();

        auto peaks = ms::find_peaks_local_maxima(mass, y, min_height, min_width, max_width);

        for (auto& pk : peaks) {
            out_analysis.push_back(gkey.analysis);
            out_replicate.push_back(as<std::string>(replicate_col[ref]));
            out_id.push_back(gkey.id);
            out_polarity.push_back(polarity_col[ref]);
            out_rt.push_back(avg_rt);
            out_mass.push_back(pk.x_val);
            out_mass_min.push_back(pk.x_min);
            out_mass_max.push_back(pk.x_max);
            out_intensity.push_back(pk.intensity);
            out_area.push_back(pk.area);
            out_sn.push_back(pk.sn);
            out_width.push_back(pk.width);
        }
    }

    return DataFrame::create(
        Named("analysis")   = out_analysis,
        Named("replicate")  = out_replicate,
        Named("id")         = out_id,
        Named("polarity")   = out_polarity,
        Named("rt")         = out_rt,
        Named("mass")       = out_mass,
        Named("mass_min")   = out_mass_min,
        Named("mass_max")   = out_mass_max,
        Named("intensity")  = out_intensity,
        Named("area")       = out_area,
        Named("sn")         = out_sn,
        Named("width")      = out_width,
        Named("stringsAsFactors") = false
    );
}

// ==========================================================================
// MARK: streamcraft compatibility exports
// ==========================================================================

// [[Rcpp::export]]
std::vector<float> rcpp_streamcraft_decode_string(std::string base64_encoded) {
    if (base64_encoded.empty()) return std::vector<float>();
    try {
        std::string decoded_binary = ms::utils::decode_base64(base64_encoded);
        return ms::utils::decode_little_endian_to_float(decoded_binary, 4);
    } catch (const std::exception& e) {
        Rcpp::warning(std::string("Failed to decode string: ") + e.what());
        return std::vector<float>();
    }
}

// [[Rcpp::export]]
std::string rcpp_streamcraft_encode_vector(Rcpp::NumericVector numeric_vector) {
    if (numeric_vector.size() == 0) return "";
    try {
        std::vector<float> float_vector = Rcpp::as<std::vector<float>>(numeric_vector);
        std::string encoded_binary = ms::utils::encode_little_endian_from_float(float_vector, 4);
        return ms::utils::encode_base64(encoded_binary);
    } catch (const std::exception& e) {
        Rcpp::warning(std::string("Failed to encode vector: ") + e.what());
        return "";
    }
}

// [[Rcpp::export]]
Rcpp::List rcpp_streamcraft_parse_ms_analysis_from_files(std::string file_path) {
    Rcpp::List list_out;
    Rcpp::CharacterVector na_charvec(1, NA_STRING);
    Rcpp::DataFrame empty_df;
    empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    Rcpp::List empty_list;

    ms::MS_FILE ana(file_path);

    list_out["name"] = ana.file_name;
    list_out["replicate"] = na_charvec;
    list_out["blank"] = na_charvec;
    list_out["file"] = ana.file_path;
    list_out["format"] = ana.get_format();
    list_out["type"] = ana.get_type();
    list_out["spectra_number"] = ana.get_number_spectra();

    if (ana.get_number_spectra() > 0) {
        ms::MS_SPECTRA_HEADERS hd = ana.get_spectra_headers();
        Rcpp::List hdl;
        hdl["index"] = hd.index;
        hdl["scan"] = hd.scan;
        hdl["array_length"] = hd.array_length;
        hdl["level"] = hd.level;
        hdl["mode"] = hd.mode;
        hdl["polarity"] = hd.polarity;
        hdl["configuration"] = hd.configuration;
        hdl["lowmz"] = hd.lowmz;
        hdl["highmz"] = hd.highmz;
        hdl["bpmz"] = hd.bpmz;
        hdl["bpint"] = hd.bpint;
        hdl["tic"] = hd.tic;
        hdl["rt"] = hd.rt;
        hdl["mobility"] = hd.mobility;
        hdl["window_mz"] = hd.window_mz;
        hdl["pre_mzlow"] = hd.window_mzlow;
        hdl["pre_mzhigh"] = hd.window_mzhigh;
        hdl["pre_mz"] = hd.precursor_mz;
        hdl["pre_charge"] = hd.precursor_charge;
        hdl["pre_intensity"] = hd.precursor_intensity;
        hdl["pre_ce"] = hd.activation_ce;
        hdl.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        list_out["spectra_headers"] = hdl;
    } else {
        list_out["spectra_headers"] = empty_df;
    }

    list_out["spectra"] = empty_df;
    list_out["chromatograms_number"] = ana.get_number_chromatograms();

    if (ana.get_number_chromatograms() > 0) {
        ms::MS_CHROMATOGRAMS_HEADERS hd2 = ana.get_chromatograms_headers();
        Rcpp::List hdl2;
        hdl2["index"] = hd2.index;
        hdl2["id"] = hd2.id;
        hdl2["array_length"] = hd2.array_length;
        hdl2["polarity"] = hd2.polarity;
        hdl2["pre_mz"] = hd2.precursor_mz;
        hdl2["pro_mz"] = hd2.product_mz;
        hdl2["pre_ce"] = hd2.activation_ce;
        hdl2.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        list_out["chromatograms_headers"] = hdl2;
    } else {
        list_out["chromatograms_headers"] = empty_df;
    }

    list_out["chromatograms"] = empty_df;
    list_out["metadata"] = empty_list;
    list_out.attr("class") = Rcpp::CharacterVector::create("MassSpecAnalysis", "Analysis");
    return list_out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_streamcraft_parse_ms_spectra(Rcpp::List analysis,
                                             std::vector<int> levels,
                                             Rcpp::DataFrame targets,
                                             float minIntensityMS1,
                                             float minIntensityMS2) {
    Rcpp::DataFrame empty_df;
    empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    Rcpp::List out;

    const std::string file = analysis["file"];
    const Rcpp::List& hd = analysis["spectra_headers"];
    const std::vector<float>& rt = hd["rt"];
    const int number_spectra = rt.size();
    const int number_levels = levels.size();
    if (number_spectra == 0) return empty_df;

    ms::MS_FILE ana(file);

    if (targets.nrow() == 0) {
        const std::vector<int>& polarity = hd["polarity"];
        const std::vector<int>& configuration = hd["configuration"];
        const std::vector<int>& level = hd["level"];
        const std::vector<float>& pre_mz = hd["pre_mz"];
        const std::vector<float>& pre_ce = hd["pre_ce"];
        const std::vector<float>& mobility = hd["mobility"];
        std::vector<bool> spectra_filter(number_spectra, false);
        for (int i = 0; i < number_spectra; i++) {
            for (int j = 0; j < number_levels; j++) {
                if (configuration[i] >= 3) break;
                if (level[i] == levels[j]) { spectra_filter[i] = true; break; }
            }
        }
        std::vector<int> indices;
        for (int i = 0; i < number_spectra; i++) if (spectra_filter[i]) indices.push_back(i);
        const std::vector<std::vector<std::vector<float>>> spectra = ana.get_spectra(indices);
        if (spectra.empty()) return empty_df;
        int total_traces = 0;
        for (const auto& spec : spectra) total_traces += spec[0].size();
        std::vector<int> polarity_out(total_traces);
        std::vector<int> level_out(total_traces);
        std::vector<float> pre_mz_out(total_traces);
        std::vector<float> pre_ce_out(total_traces);
        std::vector<float> rt_out(total_traces);
        std::vector<float> mobility_out(total_traces);
        std::vector<float> mz_out(total_traces);
        std::vector<float> intensity_out(total_traces);
        int trace = 0;
        for (size_t i = 0; i < spectra.size(); i++) {
            const auto& mz_ref = spectra[i][0];
            const auto& intensity_ref = spectra[i][1];
            for (size_t k = 0; k < mz_ref.size(); k++) {
                polarity_out[trace] = polarity[indices[i]];
                level_out[trace] = level[indices[i]];
                pre_mz_out[trace] = pre_mz[indices[i]];
                pre_ce_out[trace] = pre_ce[indices[i]];
                rt_out[trace] = rt[indices[i]];
                mobility_out[trace] = mobility[indices[i]];
                mz_out[trace] = mz_ref[k];
                intensity_out[trace] = intensity_ref[k];
                trace += 1;
            }
        }
        out["polarity"] = polarity_out;
        out["level"] = level_out;
        out["pre_mz"] = pre_mz_out;
        out["pre_ce"] = pre_ce_out;
        out["rt"] = rt_out;
        out["mobility"] = mobility_out;
        out["mz"] = mz_out;
        out["intensity"] = intensity_out;
        out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        return out;
    }

    std::vector<int> tg_idx(targets.nrow());
    std::iota(tg_idx.begin(), tg_idx.end(), 0);
    const std::vector<std::string> df_id = targets["id"];
    int tg_level = 0;
    if (number_levels == 1) tg_level = levels[0];
    std::vector<int> df_level(targets.nrow(), tg_level);
    Rcpp::CharacterVector tg_col_names = targets.names();
    if (std::find(tg_col_names.begin(), tg_col_names.end(), "level") != tg_col_names.end()) {
        std::vector<int> temp_df_level = targets["level"];
        for (int i = 0; i < targets.nrow(); i++) df_level[i] = temp_df_level[i];
    }
    const std::vector<int> df_polarity = targets["polarity"];
    const std::vector<bool> df_precursor = targets["precursor"];
    const std::vector<float> df_mz = targets["mz"];
    const std::vector<float> df_mzmin = targets["mzmin"];
    const std::vector<float> df_mzmax = targets["mzmax"];
    const std::vector<float> df_rt = targets["rt"];
    const std::vector<float> df_rtmin = targets["rtmin"];
    const std::vector<float> df_rtmax = targets["rtmax"];
    const std::vector<float> df_mobility = targets["mobility"];
    const std::vector<float> df_mobilitymin = targets["mobilitymin"];
    const std::vector<float> df_mobilitymax = targets["mobilitymax"];

    ms::MS_TARGETS tg{tg_idx, df_id, df_level, df_polarity, df_precursor, df_mz, df_mzmin, df_mzmax, df_rt, df_rtmin, df_rtmax, df_mobility, df_mobilitymin, df_mobilitymax};
    const std::vector<int>& hd_index = hd["index"];
    const std::vector<int>& hd_polarity = hd["polarity"];
    const std::vector<int>& hd_configuration = hd["configuration"];
    const std::vector<int>& hd_level = hd["level"];
    const std::vector<float>& hd_pre_mz = hd["pre_mz"];
    const std::vector<float>& hd_pre_mz_low = hd["pre_mzlow"];
    const std::vector<float>& hd_pre_mz_high = hd["pre_mzhigh"];
    const std::vector<float>& hd_pre_ce = hd["pre_ce"];
    const std::vector<float>& hd_mobility = hd["mobility"];
    ms::MS_SPECTRA_HEADERS headers;
    headers.resize_all(number_spectra);
    headers.index = hd_index;
    headers.rt = rt;
    headers.polarity = hd_polarity;
    headers.configuration = hd_configuration;
    headers.level = hd_level;
    headers.precursor_mz = hd_pre_mz;
    headers.activation_ce = hd_pre_ce;
    headers.mobility = hd_mobility;
    ms::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(tg, headers, minIntensityMS1, minIntensityMS2);
    out["id"] = res.id;
    out["polarity"] = res.polarity;
    out["level"] = res.level;
    out["pre_mz"] = res.pre_mz;
    out["pre_mzlow"] = res.pre_mzlow;
    out["pre_mzhigh"] = res.pre_mzhigh;
    out["pre_ce"] = res.pre_ce;
    out["rt"] = res.rt;
    out["mobility"] = res.mobility;
    out["mz"] = res.mz;
    out["intensity"] = res.intensity;
    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
}

// [[Rcpp::export]]
Rcpp::List rcpp_streamcraft_parse_ms_chromatograms(Rcpp::List analysis, std::vector<int> idx) {
    Rcpp::DataFrame empty_df;
    empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    Rcpp::List out;
    const std::string file = analysis["file"];
    const Rcpp::List& hd = analysis["chromatograms_headers"];
    const std::vector<int> index = hd["index"];
    const std::vector<std::string> id = hd["id"];
    const std::vector<int> polarity = hd["polarity"];
    const std::vector<float> pre_mz = hd["pre_mz"];
    const std::vector<float> pre_ce = hd["pre_ce"];
    const std::vector<float> pro_mz = hd["pro_mz"];
    if (index.empty()) return empty_df;
    ms::MS_FILE ana(file);
    if (idx.empty()) idx = index;
    const std::vector<std::vector<std::vector<float>>> chromatograms = ana.get_chromatograms(idx);
    if (chromatograms.empty()) return empty_df;
    int total_traces = 0;
    for (const auto& c : chromatograms) total_traces += c[0].size();
    if (total_traces == 0) return empty_df;
    std::vector<int> index_out(total_traces);
    std::vector<std::string> id_out(total_traces);
    std::vector<int> polarity_out(total_traces);
    std::vector<float> pre_mz_out(total_traces);
    std::vector<float> pre_ce_out(total_traces);
    std::vector<float> pro_mz_out(total_traces);
    std::vector<float> rt_out(total_traces);
    std::vector<float> intensity_out(total_traces);
    int trace = 0;
    for (size_t i = 0; i < chromatograms.size(); i++) {
        const auto& rtj = chromatograms[i][0];
        const auto& intj = chromatograms[i][1];
        for (size_t j = 0; j < rtj.size(); j++) {
            index_out[trace] = index[idx[i]];
            id_out[trace] = id[idx[i]];
            polarity_out[trace] = polarity[idx[i]];
            pre_mz_out[trace] = pre_mz[idx[i]];
            pre_ce_out[trace] = pre_ce[idx[i]];
            pro_mz_out[trace] = pro_mz[idx[i]];
            rt_out[trace] = rtj[j];
            intensity_out[trace] = intj[j];
            trace += 1;
        }
    }
    out["index"] = index_out;
    out["id"] = id_out;
    out["polarity"] = polarity_out;
    out["pre_mz"] = pre_mz_out;
    out["pre_ce"] = pre_ce_out;
    out["pro_mz"] = pro_mz_out;
    out["rt"] = rt_out;
    out["intensity"] = intensity_out;
    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
}

// ==========================================================================
// MARK: spectral clustering export
// ==========================================================================

// [[Rcpp::export]]
Rcpp::List rcpp_ms_cluster_spectra(Rcpp::DataFrame spectra,
                                   double mzClust = 0.005,
                                   double presence = 0.8,
                                   bool verbose = false) {
    const std::vector<std::string>& names_spectra = spectra.names();
    const std::vector<std::string> must_have_names = {"unique_id", "analysis", "polarity", "id", "rt", "mz", "intensity"};
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
        if (!value) throw std::runtime_error("The DataFrame must have the columns unique_id, analysis, polarity, id, rt, pre_mz, mz and intensity!");
    }

    const std::vector<std::string>& all_unique_id = spectra["unique_id"];
    const std::vector<std::string>& all_analysis = spectra["analysis"];
    const std::vector<int>& all_polarity = spectra["polarity"];
    const std::vector<std::string>& all_id = spectra["id"];
    const std::vector<double>& all_rt = spectra["rt"];
    const std::vector<double>& all_mz = spectra["mz"];
    const std::vector<double>& all_intensity = spectra["intensity"];

    std::set<std::string> unique_ids_set(all_unique_id.begin(), all_unique_id.end());
    std::vector<std::string> unique_ids(unique_ids_set.begin(), unique_ids_set.end());
    const int n_unique_ids = unique_ids.size();
    const int n_all_unique_ids = all_unique_id.size();
    if (verbose) {
        Rcpp::Rcout << "Clustering " << n_unique_ids << " ids from " << n_all_unique_ids << " spectra" << std::endl;
        Rcpp::Rcout << std::endl;
    }
    std::vector<double> all_pre_ce(n_all_unique_ids);
    if (has_pre_ce) {
        const std::vector<double>& all_pre_ce_origin = spectra["pre_ce"];
        for (int i = 0; i < n_all_unique_ids; ++i) all_pre_ce[i] = all_pre_ce_origin[i];
    } else {
        all_pre_ce.push_back(std::nan(""));
    }
    std::vector<double> all_pre_mz(n_all_unique_ids);
    if (has_pre_mz) {
        const std::vector<double>& all_pre_mz_origin = spectra["pre_mz"];
        for (int i = 0; i < n_all_unique_ids; ++i) all_pre_mz[i] = all_pre_mz_origin[i];
    } else {
        all_pre_mz.push_back(std::nan(""));
    }

    Rcpp::List spectra_out(n_unique_ids);
    for (int i = 0; i < n_unique_ids; ++i) {
        std::string target_id = unique_ids[i];
        if (verbose) Rcpp::Rcout << "Clustering " << target_id << std::endl;
        std::vector<int> which_idx;
        for (int z = 0; z < n_all_unique_ids; z++) if (all_unique_id[z] == target_id) which_idx.push_back(z);
        const int n_idx = which_idx.size();
        if (n_idx == 0) continue;
        std::vector<std::string> analysis(n_idx);
        std::vector<int> polarity(n_idx);
        std::vector<std::string> id(n_idx);
        std::vector<double> u_rt(n_idx), u_pre_mz(n_idx), u_pre_ce(n_idx), u_mz(n_idx), u_intensity(n_idx);
        for (int j = 0; j < n_idx; ++j) {
            int x = which_idx[j];
            analysis[j] = all_analysis[x];
            polarity[j] = all_polarity[x];
            id[j] = all_id[x];
            u_rt[j] = all_rt[x];
            u_mz[j] = all_mz[x];
            u_intensity[j] = all_intensity[x];
            u_pre_ce[j] = has_pre_ce ? all_pre_ce[x] : all_pre_ce[0];
            u_pre_mz[j] = has_pre_mz ? all_pre_mz[x] : all_pre_mz[0];
        }
        std::vector<int> idx(u_mz.size());
        std::iota(idx.begin(), idx.end(), 0);
        std::sort(idx.begin(), idx.end(), [&](int a, int b){ return u_mz[a] < u_mz[b]; });
        std::vector<double> rt(n_idx), pre_ce(n_idx), mz(n_idx), intensity(n_idx);
        for (int j = 0; j < n_idx; ++j) {
            rt[j] = u_rt[idx[j]];
            mz[j] = u_mz[idx[j]];
            intensity[j] = u_intensity[idx[j]];
            pre_ce[j] = u_pre_ce[idx[j]];
        }
        std::vector<double> mz_diff(n_idx - 1);
        for (int j = 1; j < n_idx; ++j) mz_diff[j - 1] = mz[j] - mz[j - 1];
        double itMzClust = mzClust;
        int counter = 0;
        bool hasFromSameScan = true;
        while (hasFromSameScan) {
            counter = counter + 1;
            std::vector<double> new_mz;
            std::vector<double> new_intensity;
            std::vector<int> all_clusters(mz_diff.size(), 0);
            for (size_t j = 0; j < mz_diff.size(); ++j) if (mz_diff[j] > itMzClust) all_clusters[j] = 1;
            std::partial_sum(all_clusters.begin(), all_clusters.end(), all_clusters.begin());
            all_clusters.insert(all_clusters.begin(), 0);
            for (int& val : all_clusters) val += 1;
            std::set<int> clusters_set(all_clusters.begin(), all_clusters.end());
            std::vector<int> clusters(clusters_set.begin(), clusters_set.end());
            std::vector<bool> fromSameScan(clusters.size(), true);
            for (int z = 0; z < (int)clusters.size(); ++z) {
                std::vector<int> temp_idx;
                for (int j = 0; j < (int)all_clusters.size(); ++j) if (all_clusters[j] == clusters[z]) temp_idx.push_back(j);
                int n = temp_idx.size();
                std::vector<double> temp_rt(n), temp_pre_ce(n), temp_mz(n), temp_intensity(n);
                for (int k = 0; k < n; ++k) {
                    int x = temp_idx[k];
                    temp_rt[k] = rt[x]; temp_pre_ce[k] = pre_ce[x]; temp_mz[k] = mz[x]; temp_intensity[k] = intensity[x];
                }
                std::set<double> unique_temp_rt_set(temp_rt.begin(), temp_rt.end());
                std::set<double> unique_pre_ce_set(pre_ce.begin(), pre_ce.end());
                fromSameScan[z] = unique_temp_rt_set.size() < temp_rt.size();
                if (counter > 10 || itMzClust < 0.0001) fromSameScan[z] = false;
                if (fromSameScan[z]) { itMzClust = itMzClust - 0.0001; break; }
                bool enough_presence = false;
                std::set<double> unique_rt_set(rt.begin(), rt.end());
                std::set<double> unique_temp_pre_ce_set(temp_pre_ce.begin(), temp_pre_ce.end());
                bool pre_ceHasNaN = false;
                for (const double& val : pre_ce) if (std::isnan(val)) { pre_ceHasNaN = true; break; }
                if (pre_ceHasNaN) enough_presence = unique_rt_set.size() * presence <= unique_temp_rt_set.size();
                else if (unique_temp_pre_ce_set.size() < unique_pre_ce_set.size()) enough_presence = unique_rt_set.size() * (unique_temp_pre_ce_set.size() / (double)unique_pre_ce_set.size()) * presence <= unique_temp_rt_set.size();
                else enough_presence = unique_rt_set.size() * presence <= unique_temp_rt_set.size();
                if (!enough_presence) continue;
                auto max_intensity_ptr = std::max_element(temp_intensity.begin(), temp_intensity.end());
                new_intensity.push_back(*max_intensity_ptr);
                float mz_sum = 0, mz_numWeight = 0;
                for (int w = 0; w < n; w++) { mz_numWeight = mz_numWeight + temp_mz[w] * temp_intensity[w]; mz_sum = mz_sum + temp_intensity[w]; }
                new_mz.push_back(mz_numWeight / mz_sum);
            }
            if (!new_mz.empty()) {
                double rt_mean = std::accumulate(rt.begin(), rt.end(), 0.0) / rt.size();
                bool pre_mzHasNaN = false;
                for (const double& val : u_pre_mz) if (std::isnan(val)) { pre_mzHasNaN = true; break; }
                if (has_pre_mz && !pre_mzHasNaN) {
                    double pre_mz_mean = std::accumulate(u_pre_mz.begin(), u_pre_mz.end(), 0.0) / u_pre_mz.size();
                    std::vector<bool> is_pre(new_mz.size(), false);
                    for (size_t p = 0; p < new_mz.size(); p++) if ((new_mz[p] >= pre_mz_mean - mzClust) && (new_mz[p] <= pre_mz_mean + mzClust)) is_pre[p] = true;
                    spectra_out[i] = Rcpp::DataFrame::create(Named("analysis") = analysis[0], Named("id") = id[0], Named("polarity") = polarity[0], Named("pre_mz") = pre_mz_mean, Named("rt") = rt_mean, Named("mz") = new_mz, Named("intensity") = new_intensity, Named("is_pre") = is_pre);
                } else {
                    spectra_out[i] = Rcpp::DataFrame::create(Named("analysis") = analysis[0], Named("id") = id[0], Named("polarity") = polarity[0], Named("rt") = rt_mean, Named("mz") = new_mz, Named("intensity") = new_intensity);
                }
            } else {
                spectra_out[i] = Rcpp::DataFrame::create();
            }
            hasFromSameScan = false;
            for (const bool& l : fromSameScan) if (l) { hasFromSameScan = true; break; }
            if (!hasFromSameScan && verbose) Rcpp::Rcout << "Done! \n\n";
        }
    }
    return spectra_out;
}
