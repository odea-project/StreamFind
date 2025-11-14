#ifndef NTS2_UTILS_H
#define NTS2_UTILS_H

#include <Rcpp.h>
#include "sf_utility_functions.h"
#include "StreamCraft_lib.h"

namespace NTS2
{
  // MARK: STRUCTS

  // MARK: FEATURE
  struct FEATURE
  {
    std::string analysis;
    std::string feature;
    std::string group;
    std::string component;
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
    std::vector<std::string> group;
    std::vector<std::string> component;
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
      feature_i.group = group[i];
      feature_i.component = component[i];
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
      group[i] = feature_i.group;
      component[i] = feature_i.component;
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
      group.push_back(feature_i.group);
      component.push_back(feature_i.component);
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
          "feature", "group", "component", "adduct", "rt", "mz", "mass",
          "intensity", "noise", "sn", "area",
          "rtmin", "rtmax", "width",
          "mzmin", "mzmax", "ppm",
          "fwhm_rt", "fwhm_mz",
          "gaussian_A", "gaussian_mu", "gaussian_sigma", "gaussian_r2",
          "polarity", "filtered", "filter", "filled", "correction",
          "eic_size", "eic_rt", "eic_mz", "eic_intensity", "eic_baseline", "eic_smoothed",
          "ms1_size", "ms1_mz", "ms1_intensity",
          "ms2_size", "ms2_mz", "ms2_intensity"};

      if (!SF_UTILITY::check_list_must_have_names(fts, must_have_names))
      {
        Rcpp::Rcout << "Error: FEATURES::import_from_list() - missing required names in the list." << std::endl;
        return;
      }

      feature = Rcpp::as<std::vector<std::string>>(fts["feature"]);
      group = Rcpp::as<std::vector<std::string>>(fts["group"]);
      component = Rcpp::as<std::vector<std::string>>(fts["component"]);
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
        return SF_UTILITY::get_empty_dt();
      }

      Rcpp::List out = Rcpp::List::create(
          Rcpp::Named("feature") = feature,
          Rcpp::Named("group") = group,
          Rcpp::Named("component") = component,
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

      if (!SF_UTILITY::check_list_must_have_names(info, info_must_have_names))
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
          headers[i] = SF_UTILITY::as_MS_SPECTRA_HEADERS(header_ref);
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
        const std::vector<int> &resolution_profile,
        const float &noiseThreshold,
        const float &minSNR,
        const int &minTraces,
        const float &baselineWindow,
        const float &maxWidth,
        const float &base_quantile);

  };
}; // namespace NTS2

#endif
