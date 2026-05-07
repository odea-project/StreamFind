#ifndef NTS_H
#define NTS_H

#include <numeric>
#include <unordered_map>
#include <iostream>
#include "nts_utils.h"
#include "nts_deconvolution.h"
#include "nts_annotation.h"
#include "nts_componentization.h"
#include "nts_alignment.h"
#include "nts_gap_filling.h"
#include "nts_blank_subtraction.h"
#include "nts_filters.h"
#include "suspect_screening.h"
#include "metfrag_runner.h"
#include "assign_transformation_products.h"
#include "../mass_spec/reader.h"

namespace nts
{
  // MARK: MS_SPECTRUM
  struct MS_SPECTRUM
  {
    std::vector<float> mz;
    std::vector<float> intensity;
  };

  // MARK: merge_MS_TARGETS_SPECTRA
  MS_SPECTRUM merge_MS_TARGETS_SPECTRA(
      const mass_spec::MS_TARGETS_SPECTRA &spectra,
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

    void set_analysis(const std::string &a)
    {
      analysis = a;
    }

    void sort_by_mz()
    {
      if (feature.size() == 0)
        return;

      std::vector<int> new_order(feature.size());
      std::iota(new_order.begin(), new_order.end(), 0);

      std::sort(new_order.begin(), new_order.end(), [this](int i1, int i2)
                { return mz[i1] < mz[i2]; });

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

  // MARK: SUSPECTS
  struct SUSPECT
  {
    std::string analysis;
    std::string feature;
    int candidate_rank;
    std::string name;
    int polarity;
    double db_mass;
    double exp_mass;
    double error_mass;
    double db_rt;
    double exp_rt;
    double error_rt;
    double intensity;
    double area;
    int id_level;
    double score;
    int shared_fragments;
    double cosine_similarity;
    std::string formula;
    std::string SMILES;
    std::string InChI;
    std::string InChIKey;
    double xLogP;
    std::string database_id;
    int db_ms2_size;
    std::string db_ms2_mz;
    std::string db_ms2_intensity;
    std::string db_ms2_formula;
    int exp_ms2_size;
    std::string exp_ms2_mz;
    std::string exp_ms2_intensity;
  };

  // MARK: INTERNAL_STANDARD
  struct INTERNAL_STANDARD
  {
    std::string analysis;
    std::string feature;
    int candidate_rank;
    std::string name;
    int polarity;
    double db_mass;
    double exp_mass;
    double error_mass;
    double db_rt;
    double exp_rt;
    double error_rt;
    double intensity;
    double area;
    int id_level;
    double score;
    int shared_fragments;
    double cosine_similarity;
    std::string formula;
    std::string SMILES;
    std::string InChI;
    std::string InChIKey;
    double xLogP;
    std::string database_id;
    int db_ms2_size;
    std::string db_ms2_mz;
    std::string db_ms2_intensity;
    std::string db_ms2_formula;
    int exp_ms2_size;
    std::string exp_ms2_mz;
    std::string exp_ms2_intensity;
  };

  struct SUSPECTS
  {
    std::vector<std::string> analysis;
    std::vector<std::string> feature;
    std::vector<int> candidate_rank;
    std::vector<std::string> name;
    std::vector<int> polarity;
    std::vector<double> db_mass;
    std::vector<double> exp_mass;
    std::vector<double> error_mass;
    std::vector<double> db_rt;
    std::vector<double> exp_rt;
    std::vector<double> error_rt;
    std::vector<double> intensity;
    std::vector<double> area;
    std::vector<int> id_level;
    std::vector<double> score;
    std::vector<int> shared_fragments;
    std::vector<double> cosine_similarity;
    std::vector<std::string> formula;
    std::vector<std::string> SMILES;
    std::vector<std::string> InChI;
    std::vector<std::string> InChIKey;
    std::vector<double> xLogP;
    std::vector<std::string> database_id;
    std::vector<int> db_ms2_size;
    std::vector<std::string> db_ms2_mz;
    std::vector<std::string> db_ms2_intensity;
    std::vector<std::string> db_ms2_formula;
    std::vector<int> exp_ms2_size;
    std::vector<std::string> exp_ms2_mz;
    std::vector<std::string> exp_ms2_intensity;

    int size() const
    {
      return analysis.size();
    }

    void append(const SUSPECT &s)
    {
      analysis.push_back(s.analysis);
      feature.push_back(s.feature);
      candidate_rank.push_back(s.candidate_rank);
      name.push_back(s.name);
      polarity.push_back(s.polarity);
      db_mass.push_back(s.db_mass);
      exp_mass.push_back(s.exp_mass);
      error_mass.push_back(s.error_mass);
      db_rt.push_back(s.db_rt);
      exp_rt.push_back(s.exp_rt);
      error_rt.push_back(s.error_rt);
      intensity.push_back(s.intensity);
      area.push_back(s.area);
      id_level.push_back(s.id_level);
      score.push_back(s.score);
      shared_fragments.push_back(s.shared_fragments);
      cosine_similarity.push_back(s.cosine_similarity);
      formula.push_back(s.formula);
      SMILES.push_back(s.SMILES);
      InChI.push_back(s.InChI);
      InChIKey.push_back(s.InChIKey);
      xLogP.push_back(s.xLogP);
      database_id.push_back(s.database_id);
      db_ms2_size.push_back(s.db_ms2_size);
      db_ms2_mz.push_back(s.db_ms2_mz);
      db_ms2_intensity.push_back(s.db_ms2_intensity);
      db_ms2_formula.push_back(s.db_ms2_formula);
      exp_ms2_size.push_back(s.exp_ms2_size);
      exp_ms2_mz.push_back(s.exp_ms2_mz);
      exp_ms2_intensity.push_back(s.exp_ms2_intensity);
    }
  };

  // MARK: INTERNAL_STANDARDS
  struct INTERNAL_STANDARDS
  {
    std::vector<std::string> analysis;
    std::vector<std::string> feature;
    std::vector<int> candidate_rank;
    std::vector<std::string> name;
    std::vector<int> polarity;
    std::vector<double> db_mass;
    std::vector<double> exp_mass;
    std::vector<double> error_mass;
    std::vector<double> db_rt;
    std::vector<double> exp_rt;
    std::vector<double> error_rt;
    std::vector<double> intensity;
    std::vector<double> area;
    std::vector<int> id_level;
    std::vector<double> score;
    std::vector<int> shared_fragments;
    std::vector<double> cosine_similarity;
    std::vector<std::string> formula;
    std::vector<std::string> SMILES;
    std::vector<std::string> InChI;
    std::vector<std::string> InChIKey;
    std::vector<double> xLogP;
    std::vector<std::string> database_id;
    std::vector<int> db_ms2_size;
    std::vector<std::string> db_ms2_mz;
    std::vector<std::string> db_ms2_intensity;
    std::vector<std::string> db_ms2_formula;
    std::vector<int> exp_ms2_size;
    std::vector<std::string> exp_ms2_mz;
    std::vector<std::string> exp_ms2_intensity;

    int size() const
    {
      return analysis.size();
    }

    void append(const INTERNAL_STANDARD &is)
    {
      analysis.push_back(is.analysis);
      feature.push_back(is.feature);
      candidate_rank.push_back(is.candidate_rank);
      name.push_back(is.name);
      polarity.push_back(is.polarity);
      db_mass.push_back(is.db_mass);
      exp_mass.push_back(is.exp_mass);
      error_mass.push_back(is.error_mass);
      db_rt.push_back(is.db_rt);
      exp_rt.push_back(is.exp_rt);
      error_rt.push_back(is.error_rt);
      intensity.push_back(is.intensity);
      area.push_back(is.area);
      id_level.push_back(is.id_level);
      score.push_back(is.score);
      shared_fragments.push_back(is.shared_fragments);
      cosine_similarity.push_back(is.cosine_similarity);
      formula.push_back(is.formula);
      SMILES.push_back(is.SMILES);
      InChI.push_back(is.InChI);
      InChIKey.push_back(is.InChIKey);
      xLogP.push_back(is.xLogP);
      database_id.push_back(is.database_id);
      db_ms2_size.push_back(is.db_ms2_size);
      db_ms2_mz.push_back(is.db_ms2_mz);
      db_ms2_intensity.push_back(is.db_ms2_intensity);
      db_ms2_formula.push_back(is.db_ms2_formula);
      exp_ms2_size.push_back(is.exp_ms2_size);
      exp_ms2_mz.push_back(is.exp_ms2_mz);
      exp_ms2_intensity.push_back(is.exp_ms2_intensity);
    }
  };

  struct NTS_INFO
  {
    std::vector<std::string> analyses;
    std::vector<std::string> replicates;
    std::vector<std::string> blanks;
    std::vector<std::string> files;

    int size() const
    {
      return analyses.size();
    }
  };

  // MARK: NTS_DATA
  struct NTS_DATA
  {
    std::vector<std::string> analyses;
    std::vector<std::string> replicates;
    std::vector<std::string> blanks;
    std::vector<std::string> files;
    std::vector<mass_spec::MS_SPECTRA_HEADERS> headers;
    std::vector<FEATURES> features;
    std::vector<SUSPECTS> suspects;
    std::vector<INTERNAL_STANDARDS> internal_standards;

    NTS_DATA(const NTS_INFO &info,
              const std::vector<mass_spec::MS_SPECTRA_HEADERS> &spectra_headers,
             const std::vector<FEATURES> &feature_list,
             const std::vector<SUSPECTS> &suspects_cpp = std::vector<SUSPECTS>(),
             const std::vector<INTERNAL_STANDARDS> &internal_standards_cpp = std::vector<INTERNAL_STANDARDS>())
    {

      analyses = info.analyses;
      const size_t number_analyses = analyses.size();

      if (number_analyses == 0)
      {
        std::cerr << "Error: No analyses given!" << std::endl;
        return;
      }

      replicates = info.replicates;
      blanks = info.blanks;
      files = info.files;
      headers.resize(number_analyses);
      features.resize(number_analyses);
      suspects.resize(number_analyses);
      internal_standards.resize(number_analyses);

      if (spectra_headers.size() == 0 || spectra_headers.size() != number_analyses)
      {
        for (size_t i = 0; i < number_analyses; i++)
        {
          mass_spec::MS_FILE ana(files[i]);
          headers[i] = ana.get_spectra_headers();
        }
      }
      else
      {
        for (size_t i = 0; i < number_analyses; i++)
        {
          headers[i] = spectra_headers[i];
        }
      }

      if (feature_list.size() != 0 && feature_list.size() != number_analyses)
      {
        std::cerr << "Error: No feature list given or size mismatch with analyses!" << std::endl;
        return;
      }

      if (feature_list.size() > 0)
      {
        for (size_t i = 0; i < number_analyses; i++)
        {
          features[i] = feature_list[i];
          features[i].set_analysis(analyses[i]);
        }
      }

      if (suspects_cpp.size() > 0 && suspects_cpp.size() == number_analyses)
      {
        for (size_t i = 0; i < number_analyses; i++)
        {
          suspects[i] = suspects_cpp[i];
        }
      }

      if (internal_standards_cpp.size() > 0 && internal_standards_cpp.size() == number_analyses)
      {
        for (size_t i = 0; i < number_analyses; i++)
        {
          internal_standards[i] = internal_standards_cpp[i];
        }
      }
    };

    int size() const
    {
      return analyses.size();
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
        const std::string &debugAnalysis = "",
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
          debugAnalysis,
          debugMZ,
          debugSpecIdx);
    }

    void create_components(
        const std::vector<float> &rtWindow,
        float minCorrelation = 0.8f,
        float debugRT = 0.0f,
        const std::string &debugAnalysis = "")
    {
      componentization::create_components_impl(*this, rtWindow, minCorrelation, debugRT, debugAnalysis);
    }

    void annotate_components(
        int maxIsotopes = 5,
        int maxCharge = 1,
        int maxGaps = 1,
        float ppm = 10.0,
        const std::string &debugComponent = "",
        const std::string &debugAnalysis = "")
    {
      annotation::annotate_components_impl(*this, maxIsotopes, maxCharge, maxGaps, ppm, debugComponent, debugAnalysis);
    }

    void group_features(
        const std::string &method,
        float rtDeviation,
        float ppm,
        int minSamples,
        float binSize = 5.0f,
        bool debug = false,
        float debugRT = 0.0f)
    {
      alignment::group_features_impl(*this, method, rtDeviation, ppm, minSamples, binSize, debug, debugRT);
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

    void fill_features(
        bool withinReplicate,
        bool filtered,
        float rtExpand,
        float mzExpand,
        float maxPeakWidth,
        float minTracesIntensity,
        int minNumberTraces,
        float minIntensity,
        float rtApexDeviation,
        float minSignalToNoiseRatio,
        float minGaussianFit,
        std::string debugFG = "")
    {
      gap_filling::fill_features_impl(
          *this,
          withinReplicate,
          filtered,
          rtExpand,
          mzExpand,
          maxPeakWidth,
          minTracesIntensity,
          minNumberTraces,
          minIntensity,
          rtApexDeviation,
          minSignalToNoiseRatio,
          minGaussianFit,
          debugFG);
    }

    void subtract_blank(
        float blankThreshold,
        float rtExpand,
        float mzExpand,
        float minTracesIntensity = 0.0f)
    {
      blank_subtraction::subtract_blank_impl(
          *this,
          blankThreshold,
          rtExpand,
          mzExpand,
          minTracesIntensity);
    }

    void filter_features(
        double minSN,
        double minIntensity,
        double minArea,
        double minWidth,
        double maxWidth,
        double maxPPM,
        double minFwhmRT,
        double maxFwhmRT,
        double minFwhmMZ,
        double maxFwhmMZ,
        double minGaussianA,
        double minGaussianMu,
        double maxGaussianMu,
        double minGaussianSigma,
        double maxGaussianSigma,
        double minGaussianR2,
        double maxJaggedness,
        double minSharpness,
        double minAsymmetry,
        double maxAsymmetry,
        int maxModality,
        bool hasMaxModality,
        double minPlates,
        bool hasOnlyFilled,
        bool onlyFilledValue,
        bool removeFilled,
        int minSizeEIC,
        bool hasMinSizeEIC,
        int minSizeMS1,
        bool hasMinSizeMS1,
        int minSizeMS2,
        bool hasMinSizeMS2,
        double minRelPresenceReplicate,
        bool removeIsotopes,
        bool removeAdducts,
        bool removeLosses)
    {
      filter_features::filter_features_impl(
          *this,
          minSN,
          minIntensity,
          minArea,
          minWidth,
          maxWidth,
          maxPPM,
          minFwhmRT,
          maxFwhmRT,
          minFwhmMZ,
          maxFwhmMZ,
          minGaussianA,
          minGaussianMu,
          maxGaussianMu,
          minGaussianSigma,
          maxGaussianSigma,
          minGaussianR2,
          maxJaggedness,
          minSharpness,
          minAsymmetry,
          maxAsymmetry,
          maxModality,
          hasMaxModality,
          minPlates,
          hasOnlyFilled,
          onlyFilledValue,
          removeFilled,
          minSizeEIC,
          hasMinSizeEIC,
          minSizeMS1,
          hasMinSizeMS1,
          minSizeMS2,
          hasMinSizeMS2,
          minRelPresenceReplicate,
          removeIsotopes,
          removeAdducts,
          removeLosses);
    }

    void suspect_screening(
        const std::vector<std::string> &analyses,
        const std::vector<suspect_screening::SuspectQuery> &suspects,
        double ppm,
        double sec,
        double ppmMS2,
        double mzrMS2,
        double minCosineSimilarity,
        int minSharedFragments,
        bool filtered)
    {
      suspect_screening::suspect_screening_impl(
          *this,
          analyses,
          suspects,
          ppm,
          sec,
          ppmMS2,
          mzrMS2,
          minCosineSimilarity,
          minSharedFragments,
          filtered);
    }

    void filter_suspects(
        const std::vector<std::string> &names,
        double minScore,
        double maxErrorRT,
        double maxErrorMass,
        const std::vector<int> &idLevels,
        int minSharedFragments,
        double minCosineSimilarity)
    {
      filter_suspects::filter_suspects_impl(
          *this,
          names,
          minScore,
          maxErrorRT,
          maxErrorMass,
          idLevels,
          minSharedFragments,
          minCosineSimilarity);
    }

    void filter_internal_standards(
        const std::vector<std::string> &names,
        double minScore,
        double maxErrorRT,
        double maxErrorMass,
        const std::vector<int> &idLevels,
        int minSharedFragments,
        double minCosineSimilarity)
    {
      filter_internal_standards::filter_internal_standards_impl(
          *this,
          names,
          minScore,
          maxErrorRT,
          maxErrorMass,
          idLevels,
          minSharedFragments,
          minCosineSimilarity);
    }

    void metfrag_screening(
        const std::vector<std::string> &analyses,
        const metfrag_runner::MetFragParams &params)
    {
      metfrag_runner::metfrag_screening_impl(*this, analyses, params);
    }
  };
}; // namespace nts

#endif
