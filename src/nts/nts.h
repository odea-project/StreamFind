#ifndef NTS_H
#define NTS_H

#include <Rcpp.h>
#include <numeric>
#include <unordered_map>
#include "nts_utils.h"
#include "nts_deconvolution.h"
#include "nts_annotation.h"
#include "../streamcraft/streamcraft.h"

namespace nts
{
  struct MS_SPECTRUM
  {
    std::vector<float> mz;
    std::vector<float> intensity;
  };

  MS_SPECTRUM merge_MS_TARGETS_SPECTRA(
    const sc::MS_TARGETS_SPECTRA &spectra,
    const float &mzClust,
    const float &presence);

  // MARK: FEATURE
  struct FEATURE
  {
    std::string analysis;
    std::string feature;
    std::string feature_component;
    std::string feature_group;
    std::string adduct;
    float rt;
    float mz;
    float mass;
    float intensity;
    float noise;
    float sn;
    float area;
    float rtmin;
    float rtmax;
    float width;
    float mzmin;
    float mzmax;
    float ppm;
    float fwhm_rt;
    float fwhm_mz;
    float gaussian_A;
    float gaussian_mu;
    float gaussian_sigma;
    float gaussian_r2;
    float jaggedness;
    float sharpness;
    float asymmetry;
    int modality;
    float plates;
    int polarity;
    bool filtered;
    std::string filter;
    bool filled;
    float correction;
    int eic_size;
    std::string eic_rt;
    std::string eic_mz;
    std::string eic_intensity;
    std::string eic_baseline;
    std::string eic_smoothed;
    int ms1_size;
    std::string ms1_mz;
    std::string ms1_intensity;
    int ms2_size;
    std::string ms2_mz;
    std::string ms2_intensity;
  };

  // MARK: FEATURES
  struct FEATURES
  {
    std::string analysis;
    std::vector<std::string> feature;
    std::vector<std::string> feature_group;
    std::vector<std::string> feature_component;
    std::vector<std::string> adduct;
    std::vector<float> rt;
    std::vector<float> mz;
    std::vector<float> mass;
    std::vector<float> intensity;
    std::vector<float> noise;
    std::vector<float> sn;
    std::vector<float> area;
    std::vector<float> rtmin;
    std::vector<float> rtmax;
    std::vector<float> width;
    std::vector<float> mzmin;
    std::vector<float> mzmax;
    std::vector<float> ppm;
    std::vector<float> fwhm_rt;
    std::vector<float> fwhm_mz;
    std::vector<float> gaussian_A;
    std::vector<float> gaussian_mu;
    std::vector<float> gaussian_sigma;
    std::vector<float> gaussian_r2;
    std::vector<float> jaggedness;
    std::vector<float> sharpness;
    std::vector<float> asymmetry;
    std::vector<int> modality;
    std::vector<float> plates;
    std::vector<int> polarity;
    std::vector<bool> filtered;
    std::vector<std::string> filter;
    std::vector<bool> filled;
    std::vector<float> correction;
    std::vector<int> eic_size;
    std::vector<std::string> eic_rt;
    std::vector<std::string> eic_mz;
    std::vector<std::string> eic_intensity;
    std::vector<std::string> eic_baseline;
    std::vector<std::string> eic_smoothed;
    std::vector<int> ms1_size;
    std::vector<std::string> ms1_mz;
    std::vector<std::string> ms1_intensity;
    std::vector<int> ms2_size;
    std::vector<std::string> ms2_mz;
    std::vector<std::string> ms2_intensity;

    int size() const
    {
      return feature.size();
    };

    FEATURE get_feature(const int &i) const
    {
      FEATURE feature_i;
      feature_i.analysis = analysis;
      feature_i.feature = feature[i];
      feature_i.feature_group = feature_group[i];
      feature_i.feature_component = feature_component[i];
      feature_i.adduct = adduct[i];
      feature_i.rt = rt[i];
      feature_i.mz = mz[i];
      feature_i.mass = mass[i];
      feature_i.intensity = intensity[i];
      feature_i.noise = noise[i];
      feature_i.sn = sn[i];
      feature_i.area = area[i];
      feature_i.rtmin = rtmin[i];
      feature_i.rtmax = rtmax[i];
      feature_i.width = width[i];
      feature_i.mzmin = mzmin[i];
      feature_i.mzmax = mzmax[i];
      feature_i.ppm = ppm[i];
      feature_i.fwhm_rt = fwhm_rt[i];
      feature_i.fwhm_mz = fwhm_mz[i];
      feature_i.gaussian_A = gaussian_A[i];
      feature_i.gaussian_mu = gaussian_mu[i];
      feature_i.gaussian_sigma = gaussian_sigma[i];
      feature_i.gaussian_r2 = gaussian_r2[i];
      feature_i.jaggedness = jaggedness[i];
      feature_i.sharpness = sharpness[i];
      feature_i.asymmetry = asymmetry[i];
      feature_i.modality = modality[i];
      feature_i.plates = plates[i];
      feature_i.polarity = polarity[i];
      feature_i.filtered = filtered[i];
      feature_i.filter = filter[i];
      feature_i.filled = filled[i];
      feature_i.correction = correction[i];
      feature_i.eic_size = eic_size[i];
      feature_i.eic_rt = eic_rt[i];
      feature_i.eic_mz = eic_mz[i];
      feature_i.eic_intensity = eic_intensity[i];
      feature_i.eic_baseline = eic_baseline[i];
      feature_i.eic_smoothed = eic_smoothed[i];
      feature_i.ms1_size = ms1_size[i];
      feature_i.ms1_mz = ms1_mz[i];
      feature_i.ms1_intensity = ms1_intensity[i];
      feature_i.ms2_size = ms2_size[i];
      feature_i.ms2_mz = ms2_mz[i];
      feature_i.ms2_intensity = ms2_intensity[i];
      return feature_i;
    };

    void set_feature(const int &i, const FEATURE &feature_i)
    {
      feature[i] = feature_i.feature;
      feature_group[i] = feature_i.feature_group;
      feature_component[i] = feature_i.feature_component;
      adduct[i] = feature_i.adduct;
      rt[i] = feature_i.rt;
      mz[i] = feature_i.mz;
      mass[i] = feature_i.mass;
      intensity[i] = feature_i.intensity;
      noise[i] = feature_i.noise;
      sn[i] = feature_i.sn;
      area[i] = feature_i.area;
      rtmin[i] = feature_i.rtmin;
      rtmax[i] = feature_i.rtmax;
      width[i] = feature_i.width;
      mzmin[i] = feature_i.mzmin;
      mzmax[i] = feature_i.mzmax;
      ppm[i] = feature_i.ppm;
      fwhm_rt[i] = feature_i.fwhm_rt;
      fwhm_mz[i] = feature_i.fwhm_mz;
      gaussian_A[i] = feature_i.gaussian_A;
      gaussian_mu[i] = feature_i.gaussian_mu;
      gaussian_sigma[i] = feature_i.gaussian_sigma;
      gaussian_r2[i] = feature_i.gaussian_r2;
      jaggedness[i] = feature_i.jaggedness;
      sharpness[i] = feature_i.sharpness;
      asymmetry[i] = feature_i.asymmetry;
      modality[i] = feature_i.modality;
      plates[i] = feature_i.plates;
      polarity[i] = feature_i.polarity;
      filtered[i] = feature_i.filtered;
      filter[i] = feature_i.filter;
      filled[i] = feature_i.filled;
      correction[i] = feature_i.correction;
      eic_size[i] = feature_i.eic_size;
      eic_rt[i] = feature_i.eic_rt;
      eic_mz[i] = feature_i.eic_mz;
      eic_intensity[i] = feature_i.eic_intensity;
      eic_baseline[i] = feature_i.eic_baseline;
      eic_smoothed[i] = feature_i.eic_smoothed;
      ms1_size[i] = feature_i.ms1_size;
      ms1_mz[i] = feature_i.ms1_mz;
      ms1_intensity[i] = feature_i.ms1_intensity;
      ms2_size[i] = feature_i.ms2_size;
      ms2_mz[i] = feature_i.ms2_mz;
      ms2_intensity[i] = feature_i.ms2_intensity;
    };

    void append_feature(const FEATURE &feature_i)
    {
      feature.push_back(feature_i.feature);
      feature_group.push_back(feature_i.feature_group);
      feature_component.push_back(feature_i.feature_component);
      adduct.push_back(feature_i.adduct);
      rt.push_back(feature_i.rt);
      mz.push_back(feature_i.mz);
      mass.push_back(feature_i.mass);
      intensity.push_back(feature_i.intensity);
      noise.push_back(feature_i.noise);
      sn.push_back(feature_i.sn);
      area.push_back(feature_i.area);
      rtmin.push_back(feature_i.rtmin);
      rtmax.push_back(feature_i.rtmax);
      width.push_back(feature_i.width);
      mzmin.push_back(feature_i.mzmin);
      mzmax.push_back(feature_i.mzmax);
      ppm.push_back(feature_i.ppm);
      fwhm_rt.push_back(feature_i.fwhm_rt);
      fwhm_mz.push_back(feature_i.fwhm_mz);
      gaussian_A.push_back(feature_i.gaussian_A);
      gaussian_mu.push_back(feature_i.gaussian_mu);
      gaussian_sigma.push_back(feature_i.gaussian_sigma);
      gaussian_r2.push_back(feature_i.gaussian_r2);
      jaggedness.push_back(feature_i.jaggedness);
      sharpness.push_back(feature_i.sharpness);
      asymmetry.push_back(feature_i.asymmetry);
      modality.push_back(feature_i.modality);
      plates.push_back(feature_i.plates);
      polarity.push_back(feature_i.polarity);
      filtered.push_back(feature_i.filtered);
      filter.push_back(feature_i.filter);
      filled.push_back(feature_i.filled);
      correction.push_back(feature_i.correction);
      eic_size.push_back(feature_i.eic_size);
      eic_rt.push_back(feature_i.eic_rt);
      eic_mz.push_back(feature_i.eic_mz);
      eic_intensity.push_back(feature_i.eic_intensity);
      eic_baseline.push_back(feature_i.eic_baseline);
      eic_smoothed.push_back(feature_i.eic_smoothed);
      ms1_size.push_back(feature_i.ms1_size);
      ms1_mz.push_back(feature_i.ms1_mz);
      ms1_intensity.push_back(feature_i.ms1_intensity);
      ms2_size.push_back(feature_i.ms2_size);
      ms2_mz.push_back(feature_i.ms2_mz);
      ms2_intensity.push_back(feature_i.ms2_intensity);
    };

    void import_from_list(const std::string &a, const Rcpp::List &fts)
    {
      analysis = a;

      if (fts.size() == 0)
      {
        return;
      }

      std::vector<std::string> must_have_names = {
          "feature", "feature_group", "feature_component", "adduct", "rt", "mz", "mass",
          "intensity", "noise", "sn", "area",
          "rtmin", "rtmax", "width",
          "mzmin", "mzmax", "ppm",
          "fwhm_rt", "fwhm_mz",
          "gaussian_A", "gaussian_mu", "gaussian_sigma", "gaussian_r2",
          "jaggedness", "sharpness", "asymmetry", "modality", "plates",
          "polarity", "filtered", "filter", "filled", "correction",
          "eic_size", "eic_rt", "eic_mz", "eic_intensity", "eic_baseline", "eic_smoothed",
          "ms1_size", "ms1_mz", "ms1_intensity",
          "ms2_size", "ms2_mz", "ms2_intensity"};

      if (!utils::check_list_must_have_names(fts, must_have_names))
      {
        Rcpp::Rcout << "Error: FEATURES::import_from_list() - missing required names in the list." << std::endl;
        return;
      }

      feature = Rcpp::as<std::vector<std::string>>(fts["feature"]);
      feature_group = Rcpp::as<std::vector<std::string>>(fts["feature_group"]);
      feature_component = Rcpp::as<std::vector<std::string>>(fts["feature_component"]);
      adduct = Rcpp::as<std::vector<std::string>>(fts["adduct"]);
      rt = Rcpp::as<std::vector<float>>(fts["rt"]);
      mz = Rcpp::as<std::vector<float>>(fts["mz"]);
      mass = Rcpp::as<std::vector<float>>(fts["mass"]);
      intensity = Rcpp::as<std::vector<float>>(fts["intensity"]);
      noise = Rcpp::as<std::vector<float>>(fts["noise"]);
      sn = Rcpp::as<std::vector<float>>(fts["sn"]);
      area = Rcpp::as<std::vector<float>>(fts["area"]);
      rtmin = Rcpp::as<std::vector<float>>(fts["rtmin"]);
      rtmax = Rcpp::as<std::vector<float>>(fts["rtmax"]);
      width = Rcpp::as<std::vector<float>>(fts["width"]);
      mzmin = Rcpp::as<std::vector<float>>(fts["mzmin"]);
      mzmax = Rcpp::as<std::vector<float>>(fts["mzmax"]);
      ppm = Rcpp::as<std::vector<float>>(fts["ppm"]);
      fwhm_rt = Rcpp::as<std::vector<float>>(fts["fwhm_rt"]);
      fwhm_mz = Rcpp::as<std::vector<float>>(fts["fwhm_mz"]);
      gaussian_A = Rcpp::as<std::vector<float>>(fts["gaussian_A"]);
      gaussian_mu = Rcpp::as<std::vector<float>>(fts["gaussian_mu"]);
      gaussian_sigma = Rcpp::as<std::vector<float>>(fts["gaussian_sigma"]);
      gaussian_r2 = Rcpp::as<std::vector<float>>(fts["gaussian_r2"]);
      jaggedness = Rcpp::as<std::vector<float>>(fts["jaggedness"]);
      sharpness = Rcpp::as<std::vector<float>>(fts["sharpness"]);
      asymmetry = Rcpp::as<std::vector<float>>(fts["asymmetry"]);
      modality = Rcpp::as<std::vector<int>>(fts["modality"]);
      plates = Rcpp::as<std::vector<float>>(fts["plates"]);
      polarity = Rcpp::as<std::vector<int>>(fts["polarity"]);
      filtered = Rcpp::as<std::vector<bool>>(fts["filtered"]);
      filter = Rcpp::as<std::vector<std::string>>(fts["filter"]);
      filled = Rcpp::as<std::vector<bool>>(fts["filled"]);
      correction = Rcpp::as<std::vector<float>>(fts["correction"]);
      eic_size = Rcpp::as<std::vector<int>>(fts["eic_size"]);
      eic_rt = Rcpp::as<std::vector<std::string>>(fts["eic_rt"]);
      eic_mz = Rcpp::as<std::vector<std::string>>(fts["eic_mz"]);
      eic_intensity = Rcpp::as<std::vector<std::string>>(fts["eic_intensity"]);
      eic_baseline = Rcpp::as<std::vector<std::string>>(fts["eic_baseline"]);
      eic_smoothed = Rcpp::as<std::vector<std::string>>(fts["eic_smoothed"]);
      ms1_size = Rcpp::as<std::vector<int>>(fts["ms1_size"]);
      ms1_mz = Rcpp::as<std::vector<std::string>>(fts["ms1_mz"]);
      ms1_intensity = Rcpp::as<std::vector<std::string>>(fts["ms1_intensity"]);
      ms2_size = Rcpp::as<std::vector<int>>(fts["ms2_size"]);
      ms2_mz = Rcpp::as<std::vector<std::string>>(fts["ms2_mz"]);
      ms2_intensity = Rcpp::as<std::vector<std::string>>(fts["ms2_intensity"]);
    };

    Rcpp::List to_list_dt() const
    {
      int n = feature.size();
      if (n == 0)
      {
        return utils::get_empty_dt();
      }

      Rcpp::List out = Rcpp::List::create(
          Rcpp::Named("feature") = feature,
          Rcpp::Named("feature_group") = feature_group,
          Rcpp::Named("feature_component") = feature_component,
          Rcpp::Named("adduct") = adduct,
          Rcpp::Named("rt") = rt,
          Rcpp::Named("mz") = mz,
          Rcpp::Named("mass") = mass,
          Rcpp::Named("intensity") = intensity,
          Rcpp::Named("noise") = noise,
          Rcpp::Named("sn") = sn,
          Rcpp::Named("area") = area,
          Rcpp::Named("rtmin") = rtmin,
          Rcpp::Named("rtmax") = rtmax,
          Rcpp::Named("width") = width,
          Rcpp::Named("mzmin") = mzmin,
          Rcpp::Named("mzmax") = mzmax,
          Rcpp::Named("ppm") = ppm,
          Rcpp::Named("fwhm_rt") = fwhm_rt,
          Rcpp::Named("fwhm_mz") = fwhm_mz,
          Rcpp::Named("gaussian_A") = gaussian_A,
          Rcpp::Named("gaussian_mu") = gaussian_mu,
          Rcpp::Named("gaussian_sigma") = gaussian_sigma,
          Rcpp::Named("gaussian_r2") = gaussian_r2,
          Rcpp::Named("jaggedness") = jaggedness,
          Rcpp::Named("sharpness") = sharpness,
          Rcpp::Named("asymmetry") = asymmetry,
          Rcpp::Named("modality") = modality,
          Rcpp::Named("plates") = plates,
          Rcpp::Named("polarity") = polarity,
          Rcpp::Named("filtered") = filtered,
          Rcpp::Named("filter") = filter,
          Rcpp::Named("filled") = filled,
          Rcpp::Named("correction") = correction,
          Rcpp::Named("eic_size") = eic_size,
          Rcpp::Named("eic_rt") = eic_rt,
          Rcpp::Named("eic_mz") = eic_mz,
          Rcpp::Named("eic_intensity") = eic_intensity,
          Rcpp::Named("eic_baseline") = eic_baseline,
          Rcpp::Named("eic_smoothed") = eic_smoothed,
          Rcpp::Named("ms1_size") = ms1_size,
          Rcpp::Named("ms1_mz") = ms1_mz,
          Rcpp::Named("ms1_intensity") = ms1_intensity,
          Rcpp::Named("ms2_size") = ms2_size,
          Rcpp::Named("ms2_mz") = ms2_mz,
          Rcpp::Named("ms2_intensity") = ms2_intensity);

      out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      return out;
    };

    void sort_by_mz()
    {
      if (feature.size() == 0)
        return;

      std::vector<int> new_order(feature.size());
      std::iota(new_order.begin(), new_order.end(), 0);

      std::sort(new_order.begin(), new_order.end(), [this](int i1, int i2) {
        return mz[i1] < mz[i2];
      });

      // Create sorted copies of all vectors
      std::vector<std::string> feature_sorted(feature.size());
      std::vector<std::string> feature_group_sorted(feature.size());
      std::vector<std::string> feature_component_sorted(feature.size());
      std::vector<std::string> adduct_sorted(feature.size());
      std::vector<float> rt_sorted(feature.size());
      std::vector<float> mz_sorted(feature.size());
      std::vector<float> mass_sorted(feature.size());
      std::vector<float> intensity_sorted(feature.size());
      std::vector<float> noise_sorted(feature.size());
      std::vector<float> sn_sorted(feature.size());
      std::vector<float> area_sorted(feature.size());
      std::vector<float> rtmin_sorted(feature.size());
      std::vector<float> rtmax_sorted(feature.size());
      std::vector<float> width_sorted(feature.size());
      std::vector<float> mzmin_sorted(feature.size());
      std::vector<float> mzmax_sorted(feature.size());
      std::vector<float> ppm_sorted(feature.size());
      std::vector<float> fwhm_rt_sorted(feature.size());
      std::vector<float> fwhm_mz_sorted(feature.size());
      std::vector<float> gaussian_A_sorted(feature.size());
      std::vector<float> gaussian_mu_sorted(feature.size());
      std::vector<float> gaussian_sigma_sorted(feature.size());
      std::vector<float> gaussian_r2_sorted(feature.size());
      std::vector<float> jaggedness_sorted(feature.size());
      std::vector<float> sharpness_sorted(feature.size());
      std::vector<float> asymmetry_sorted(feature.size());
      std::vector<int> modality_sorted(feature.size());
      std::vector<float> plates_sorted(feature.size());
      std::vector<int> polarity_sorted(feature.size());
      std::vector<bool> filtered_sorted(feature.size());
      std::vector<std::string> filter_sorted(feature.size());
      std::vector<bool> filled_sorted(feature.size());
      std::vector<float> correction_sorted(feature.size());
      std::vector<int> eic_size_sorted(feature.size());
      std::vector<std::string> eic_rt_sorted(feature.size());
      std::vector<std::string> eic_mz_sorted(feature.size());
      std::vector<std::string> eic_intensity_sorted(feature.size());
      std::vector<std::string> eic_baseline_sorted(feature.size());
      std::vector<std::string> eic_smoothed_sorted(feature.size());
      std::vector<int> ms1_size_sorted(feature.size());
      std::vector<std::string> ms1_mz_sorted(feature.size());
      std::vector<std::string> ms1_intensity_sorted(feature.size());
      std::vector<int> ms2_size_sorted(feature.size());
      std::vector<std::string> ms2_mz_sorted(feature.size());
      std::vector<std::string> ms2_intensity_sorted(feature.size());

      for (size_t i = 0; i < feature.size(); i++)
      {
        int idx = new_order[i];
        feature_sorted[i] = feature[idx];
        feature_group_sorted[i] = feature_group[idx];
        feature_component_sorted[i] = feature_component[idx];
        adduct_sorted[i] = adduct[idx];
        rt_sorted[i] = rt[idx];
        mz_sorted[i] = mz[idx];
        mass_sorted[i] = mass[idx];
        intensity_sorted[i] = intensity[idx];
        noise_sorted[i] = noise[idx];
        sn_sorted[i] = sn[idx];
        area_sorted[i] = area[idx];
        rtmin_sorted[i] = rtmin[idx];
        rtmax_sorted[i] = rtmax[idx];
        width_sorted[i] = width[idx];
        mzmin_sorted[i] = mzmin[idx];
        mzmax_sorted[i] = mzmax[idx];
        ppm_sorted[i] = ppm[idx];
        fwhm_rt_sorted[i] = fwhm_rt[idx];
        fwhm_mz_sorted[i] = fwhm_mz[idx];
        gaussian_A_sorted[i] = gaussian_A[idx];
        gaussian_mu_sorted[i] = gaussian_mu[idx];
        gaussian_sigma_sorted[i] = gaussian_sigma[idx];
        gaussian_r2_sorted[i] = gaussian_r2[idx];
        jaggedness_sorted[i] = jaggedness[idx];
        sharpness_sorted[i] = sharpness[idx];
        asymmetry_sorted[i] = asymmetry[idx];
        modality_sorted[i] = modality[idx];
        plates_sorted[i] = plates[idx];
        polarity_sorted[i] = polarity[idx];
        filtered_sorted[i] = filtered[idx];
        filter_sorted[i] = filter[idx];
        filled_sorted[i] = filled[idx];
        correction_sorted[i] = correction[idx];
        eic_size_sorted[i] = eic_size[idx];
        eic_rt_sorted[i] = eic_rt[idx];
        eic_mz_sorted[i] = eic_mz[idx];
        eic_intensity_sorted[i] = eic_intensity[idx];
        eic_baseline_sorted[i] = eic_baseline[idx];
        eic_smoothed_sorted[i] = eic_smoothed[idx];
        ms1_size_sorted[i] = ms1_size[idx];
        ms1_mz_sorted[i] = ms1_mz[idx];
        ms1_intensity_sorted[i] = ms1_intensity[idx];
        ms2_size_sorted[i] = ms2_size[idx];
        ms2_mz_sorted[i] = ms2_mz[idx];
        ms2_intensity_sorted[i] = ms2_intensity[idx];
      }

      // Replace with sorted vectors
      feature = feature_sorted;
      feature_group = feature_group_sorted;
      feature_component = feature_component_sorted;
      adduct = adduct_sorted;
      rt = rt_sorted;
      mz = mz_sorted;
      mass = mass_sorted;
      intensity = intensity_sorted;
      noise = noise_sorted;
      sn = sn_sorted;
      area = area_sorted;
      rtmin = rtmin_sorted;
      rtmax = rtmax_sorted;
      width = width_sorted;
      mzmin = mzmin_sorted;
      mzmax = mzmax_sorted;
      ppm = ppm_sorted;
      fwhm_rt = fwhm_rt_sorted;
      fwhm_mz = fwhm_mz_sorted;
      gaussian_A = gaussian_A_sorted;
      gaussian_mu = gaussian_mu_sorted;
      gaussian_sigma = gaussian_sigma_sorted;
      gaussian_r2 = gaussian_r2_sorted;
      jaggedness = jaggedness_sorted;
      sharpness = sharpness_sorted;
      asymmetry = asymmetry_sorted;
      modality = modality_sorted;
      plates = plates_sorted;
      polarity = polarity_sorted;
      filtered = filtered_sorted;
      filter = filter_sorted;
      filled = filled_sorted;
      correction = correction_sorted;
      eic_size = eic_size_sorted;
      eic_rt = eic_rt_sorted;
      eic_mz = eic_mz_sorted;
      eic_intensity = eic_intensity_sorted;
      eic_baseline = eic_baseline_sorted;
      eic_smoothed = eic_smoothed_sorted;
      ms1_size = ms1_size_sorted;
      ms1_mz = ms1_mz_sorted;
      ms1_intensity = ms1_intensity_sorted;
      ms2_size = ms2_size_sorted;
      ms2_mz = ms2_mz_sorted;
      ms2_intensity = ms2_intensity_sorted;
    };
  };

  // MARK: NTS_DATA
  struct NTS_DATA
  {
    std::vector<std::string> analyses;
    std::vector<std::string> replicates;
    std::vector<std::string> blanks;
    std::vector<std::string> files;
    std::vector<sc::MS_SPECTRA_HEADERS> headers;
    std::vector<FEATURES> features;

    NTS_DATA(Rcpp::List info,
             Rcpp::List spectra_headers,
             Rcpp::List feature_list)
    {

      std::vector<std::string> info_must_have_names = {
          "analysis", "replicate", "blank", "file"};

      if (!utils::check_list_must_have_names(info, info_must_have_names))
      {
        Rcpp::Rcout << "Error: NTS_DATA() - missing required names in the info list." << std::endl;
        return;
      }

      analyses = Rcpp::as<std::vector<std::string>>(info["analysis"]);
      const int number_analyses = analyses.size();

      if (number_analyses == 0)
      {
        Rcpp::Rcout << "Error: No analyses given!" << std::endl;
        return;
      }

      replicates = Rcpp::as<std::vector<std::string>>(info["replicate"]);
      blanks = Rcpp::as<std::vector<std::string>>(info["blank"]);
      files = Rcpp::as<std::vector<std::string>>(info["file"]);
      headers.resize(number_analyses);
      features.resize(number_analyses);

      if (spectra_headers.size() == 0 || spectra_headers.size() != number_analyses)
      {
        for (int i = 0; i < number_analyses; i++)
        {
          sc::MS_FILE ana(files[i]);
          headers[i] = ana.get_spectra_headers();
        }
      }
      else
      {
        for (int i = 0; i < number_analyses; i++)
        {
          const Rcpp::List &header_ref = Rcpp::as<Rcpp::List>(spectra_headers[i]);
          headers[i] = utils::as_MS_SPECTRA_HEADERS(header_ref);
        }
      }

      if (feature_list.size() != 0 && feature_list.size() != number_analyses)
      {
        Rcpp::Rcout << "Error: No feature list given or size mismatch with analyses!" << std::endl;
        return;
      }

      if (feature_list.size() > 0)
      {
        for (int i = 0; i < number_analyses; i++)
        {
          const Rcpp::List &feature_ref = Rcpp::as<Rcpp::List>(feature_list[i]);
          features[i].import_from_list(analyses[i], feature_ref);
        }
      }
    };

    int size() const
    {
      return analyses.size();
    };

    Rcpp::List features_as_list_of_dt() const
    {
      const int n = features.size();
      Rcpp::List out(n);
      if (n == 0)
      {
        return out;
      }
      for (int i = 0; i < n; i++)
      {
        out[i] = features[i].to_list_dt();
      }
      Rcpp::CharacterVector names(n);
      for (int i = 0; i < n; i++)
      {
        names[i] = analyses[i];
      }
      out.attr("names") = names;
      return out;
    };

    void find_features(
        const std::vector<float> &rtWindowsMin,
        const std::vector<float> &rtWindowsMax,
        const float &ppmThreshold,
        const float &noiseThreshold,
        const float &minSNR,
        const int &minTraces,
        const float &baselineWindow,
        const float &maxWidth,
        const float &baseQuantile,
        const float &debugMZ = 0.0f,
        const int &debugSpecIdx = -1)
    {
      deconvolution::find_features_impl(
        *this,
        rtWindowsMin,
        rtWindowsMax,
        ppmThreshold,
        noiseThreshold,
        minSNR,
        minTraces,
        baselineWindow,
        maxWidth,
        baseQuantile,
        debugMZ,
        debugSpecIdx
      );
    }

    void create_components(const std::vector<float> &rtWindow);

    void annotate_components(
        int maxIsotopes = 5,
        int maxCharge = 1,
        int maxGaps = 1)
    {
      annotation::annotate_components_impl(*this, maxIsotopes, maxCharge, maxGaps);
    }

    void load_features_ms1(
        bool filtered,
        const std::vector<float> &rtWindow,
        const std::vector<float> &mzWindow,
        float minTracesIntensity,
        float mzClust,
        float presence);

    void load_features_ms2(
        bool filtered,
        float minTracesIntensity,
        float isolationWindow,
        float mzClust,
        float presence);

  };
}; // namespace nts

#endif
