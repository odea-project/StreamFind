#include <vector>
#include <string>
#include <Rcpp.h>
#include <omp.h>
#include <unordered_map>
#include <cmath>
#include <algorithm>
#include <filesystem>
#include "StreamCraft_lib.h"
#include "NTS_utils.h"

// MARK: rcpp_parse_ms_analysis
// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_analysis(std::string file_path)
{

  Rcpp::List list_out;

  Rcpp::CharacterVector na_charvec(1, NA_STRING);

  Rcpp::DataFrame empty_df;

  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  Rcpp::List empty_list;

  sc::MS_FILE ana(file_path);

  list_out["name"] = ana.file_name;

  list_out["replicate"] = na_charvec;

  list_out["blank"] = na_charvec;

  list_out["file"] = ana.file_path;

  list_out["format"] = ana.get_format();

  list_out["type"] = ana.get_type();

  list_out["spectra_number"] = ana.get_number_spectra();

  if (ana.get_number_spectra() > 0)
  {
    sc::MS_SPECTRA_HEADERS hd = ana.get_spectra_headers();

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
  }
  else
  {
    list_out["spectra_headers"] = empty_df;
  }

  list_out["spectra"] = empty_df;

  list_out["chromatograms_number"] = ana.get_number_chromatograms();

  if (ana.get_number_chromatograms() > 0)
  {

    sc::MS_CHROMATOGRAMS_HEADERS hd2 = ana.get_chromatograms_headers();

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
  }
  else
  {
    list_out["chromatograms_headers"] = empty_df;
  }

  list_out["chromatograms"] = empty_df;

  list_out["metadata"] = empty_list;

  list_out.attr("class") = Rcpp::CharacterVector::create("MassSpecAnalysis", "Analysis");

  return list_out;
};

// MARK: rcpp_parse_ms_spectra_headers
// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra_headers(std::string file_path)
{

  Rcpp::List list_out;

  sc::MS_FILE ana(file_path);

  if (ana.get_number_spectra() == 0)
    return list_out;

  sc::MS_SPECTRA_HEADERS headers = ana.get_spectra_headers();

  list_out["index"] = headers.index;
  list_out["scan"] = headers.scan;
  list_out["array_length"] = headers.array_length;
  list_out["level"] = headers.level;
  list_out["mode"] = headers.mode;
  list_out["polarity"] = headers.polarity;
  list_out["configuration"] = headers.configuration;
  list_out["lowmz"] = headers.lowmz;
  list_out["highmz"] = headers.highmz;
  list_out["bpmz"] = headers.bpmz;
  list_out["bpint"] = headers.bpint;
  list_out["tic"] = headers.tic;
  list_out["rt"] = headers.rt;
  list_out["mobility"] = headers.mobility;
  list_out["window_mz"] = headers.window_mz;
  list_out["pre_mzlow"] = headers.window_mzlow;
  list_out["pre_mzhigh"] = headers.window_mzhigh;
  list_out["pre_mz"] = headers.precursor_mz;
  list_out["pre_charge"] = headers.precursor_charge;
  list_out["pre_intensity"] = headers.precursor_intensity;
  list_out["pre_ce"] = headers.activation_ce;

  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  return list_out;
};

// MARK: rcpp_parse_ms_chromatograms_headers
// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_chromatograms_headers(std::string file_path)
{

  Rcpp::List list_out;

  sc::MS_FILE ana(file_path);

  if (ana.get_number_chromatograms() == 0)
    return list_out;

  sc::MS_CHROMATOGRAMS_HEADERS headers = ana.get_chromatograms_headers();

  list_out["index"] = headers.index;
  list_out["id"] = headers.id;
  list_out["array_length"] = headers.array_length;
  list_out["polarity"] = headers.polarity;
  list_out["pre_mz"] = headers.precursor_mz;
  list_out["pro_mz"] = headers.product_mz;
  list_out["pre_ce"] = headers.activation_ce;

  list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  return list_out;
};

// MARK: rcpp_parse_ms_spectra
// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_spectra(Rcpp::List analysis,
                                 std::vector<int> levels,
                                 Rcpp::DataFrame targets,
                                 float minIntensityMS1,
                                 float minIntensityMS2)
{

  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  Rcpp::List out;

  const std::string file = analysis["file"];

  const Rcpp::List &hd = analysis["spectra_headers"];

  const std::vector<float> &rt = hd["rt"];

  const float minIntLv1 = minIntensityMS1;
  const float minIntLv2 = minIntensityMS2;

  const int number_spectra = rt.size();

  const int number_levels = levels.size();

  if (number_spectra == 0)
    return empty_df;

  const int n_tg = targets.nrow();

  sc::MS_FILE ana(file);

  if (n_tg == 0)
  {

    const std::vector<int> &polarity = hd["polarity"];
    const std::vector<int> &configuration = hd["configuration"];
    const std::vector<int> &level = hd["level"];
    const std::vector<float> &pre_mz = hd["pre_mz"];
    const std::vector<float> &pre_ce = hd["pre_ce"];
    const std::vector<float> &mobility = hd["mobility"];

    std::vector<bool> spectra_filter(number_spectra, false);

    for (int i = 0; i < number_spectra; i++)
    {
      for (int j = 0; j < number_levels; j++)
      {

        if (configuration[i] >= 3)
          break;

        if (level[i] == levels[j])
        {
          spectra_filter[i] = true;
          break;
        }
      }
    }

    std::vector<int> indices;

    for (int i = 0; i < number_spectra; i++)
      if (spectra_filter[i])
        indices.push_back(i);

    const std::vector<std::vector<std::vector<float>>> spectra = ana.get_spectra(indices);

    const int number_spectra_extracted = spectra.size();

    const int number_arrays = spectra[0].size();

    if (number_arrays == 0)
      return empty_df;

    int total_traces = 0;

    for (int i = 0; i < number_spectra_extracted; i++)
    {
      const int n = spectra[i][0].size();
      total_traces += n;
    }

    std::vector<int> polarity_out(total_traces);
    std::vector<int> level_out(total_traces);
    std::vector<float> pre_mz_out(total_traces);
    std::vector<float> pre_ce_out(total_traces);
    std::vector<float> rt_out(total_traces);
    std::vector<float> mobility_out(total_traces);
    std::vector<float> mz_out(total_traces);
    std::vector<float> intensity_out(total_traces);

    int trace = 0;
    for (int i = 0; i < number_spectra_extracted; i++)
    {
      const std::vector<float> &mz_ref = spectra[i][0];
      const std::vector<float> &intensity_ref = spectra[i][1];
      const int n = mz_ref.size();

      for (int k = 0; k < n; k++)
      {
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
  else
  {

    std::vector<int> tg_idx(n_tg);
    std::iota(tg_idx.begin(), tg_idx.end(), 0);

    const std::vector<std::string> df_id = targets["id"];

    int tg_level = 0;
    if (number_levels == 1)
      tg_level = levels[0];
    std::vector<int> df_level(n_tg, tg_level);
    Rcpp::CharacterVector tg_col_names = targets.names();
    if (std::find(tg_col_names.begin(), tg_col_names.end(), "level") != tg_col_names.end())
    {
      std::vector<int> temp_df_level = targets["level"];
      for (int i = 0; i < n_tg; i++)
        df_level[i] = temp_df_level[i];
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

    sc::MS_TARGETS tg = {
        tg_idx,
        df_id,
        df_level,
        df_polarity,
        df_precursor,
        df_mz,
        df_mzmin,
        df_mzmax,
        df_rt,
        df_rtmin,
        df_rtmax,
        df_mobility,
        df_mobilitymin,
        df_mobilitymax};

    std::set<int> idx;

    const std::vector<int> &hd_index = hd["index"];
    const std::vector<int> &hd_polarity = hd["polarity"];
    const std::vector<int> &hd_configuration = hd["configuration"];
    const std::vector<int> &hd_level = hd["level"];
    const std::vector<float> &hd_pre_mz = hd["pre_mz"];
    const std::vector<float> &hd_pre_mz_low = hd["pre_mzlow"];
    const std::vector<float> &hd_pre_mz_high = hd["pre_mzhigh"];
    const std::vector<float> &hd_pre_ce = hd["pre_ce"];
    const std::vector<float> &hd_mobility = hd["mobility"];

    sc::MS_SPECTRA_HEADERS headers;
    headers.resize_all(number_spectra);

    headers.index = hd_index;
    headers.rt = rt;
    headers.polarity = hd_polarity;
    headers.configuration = hd_configuration;
    headers.level = hd_level;
    headers.precursor_mz = hd_pre_mz;
    headers.activation_ce = hd_pre_ce;
    headers.mobility = hd_mobility;

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(tg, headers, minIntLv1, minIntLv2);

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

  return empty_df;
};

// MARK: rcpp_parse_ms_chromatograms
// [[Rcpp::export]]
Rcpp::List rcpp_parse_ms_chromatograms(Rcpp::List analysis, std::vector<int> idx)
{

  Rcpp::DataFrame empty_df;
  empty_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  Rcpp::List out;

  const std::string file = analysis["file"];

  const Rcpp::List &hd = analysis["chromatograms_headers"];

  const std::vector<int> index = hd["index"];
  const std::vector<std::string> id = hd["id"];
  const std::vector<int> polarity = hd["polarity"];
  const std::vector<float> pre_mz = hd["pre_mz"];
  const std::vector<float> pre_ce = hd["pre_ce"];
  const std::vector<float> pro_mz = hd["pro_mz"];

  const int number_chromatograms = index.size();

  if (number_chromatograms == 0)
    return empty_df;

  sc::MS_FILE ana(file);

  if (idx.size() == 0)
    idx = index;

  const std::vector<std::vector<std::vector<float>>> chromatograms = ana.get_chromatograms(idx);

  const int number_extracted_chromatograms = chromatograms.size();

  if (number_extracted_chromatograms == 0)
    return empty_df;

  int total_traces = 0;

  for (int i = 0; i < number_extracted_chromatograms; i++)
    total_traces += chromatograms[i][0].size();
  
  if (total_traces == 0)
    return empty_df;

  std::vector<int> index_out(total_traces);
  std::vector<std::string> id_out(total_traces);
  std::vector<int> polarity_out(total_traces);
  std::vector<float> pre_mz_out(total_traces);
  std::vector<float> pre_ce_out(total_traces);
  std::vector<float> pro_mz_out(total_traces);
  std::vector<float> rt_out(total_traces);
  std::vector<float> intensity_out(total_traces);

  int trace = 0;

  for (int i = 0; i < number_extracted_chromatograms; i++)
  {

    const std::vector<float> &rtj = chromatograms[i][0];
    const std::vector<float> &intj = chromatograms[i][1];

    const int n = rtj.size();

    if (n == 0)
      continue;

    for (int j = 0; j < n; j++)
    {

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
};

// MARK: rcpp_ms_annotate_features
// [[Rcpp::export]]
Rcpp::List rcpp_ms_annotate_features(Rcpp::List feature_list,
                                     double rtWindowAlignment = 0.3,
                                     int maxIsotopes = 5,
                                     int maxCharge = 1,
                                     int maxGaps = 1)
{

  NTS::MS_ISOTOPE_SET isotopes;

  std::vector<std::string> elements = {"C", "H", "N", "O", "S", "Cl", "Br", "Si"};

  isotopes.filter(elements);

  const int max_number_elements = 5;

  Rcpp::Rcout << "Building combinatorial isotopic chains with length " << max_number_elements << "...";
  NTS::MS_ISOTOPE_COMBINATIONS combinations(isotopes, max_number_elements);
  Rcpp::Rcout << "Done!" << std::endl;

  const int number_analyses = feature_list.size();

  if (number_analyses == 0)
    return feature_list;

  for (int a = 0; a < number_analyses; a++)
  {

    Rcpp::List features = feature_list[a];

    const NTS::MS_FEATURES_MZ_SORTED fdf(features);

    const std::vector<std::string> fts = features["feature"];

    const int number_features = fdf.n;

    NTS::MS_ANNOTATION af(number_features);

    Rcpp::Rcout << "Annotating isotopes in " << number_features << " features...";

    for (int f = 0; f < number_features; f++)
    {

      const int &index = fdf.index[f];

      if (af.iso_step[index] > 0)
        continue; // already isotope

      const std::string &feature = fdf.feature[f];
      const int &polarity = fdf.polarity[f];
      const float &rt = fdf.rt[f];
      float rtmin = fdf.rtmin[f];
      float rtmax = fdf.rtmax[f];
      const float &mz = fdf.mz[f];
      const float &mzmin = fdf.mzmin[f];
      const float &mzmax = fdf.mzmax[f];
      const float max_mz_chain = (mz + maxIsotopes) * 1.05;

      std::vector<int> candidates = NTS::find_isotopic_candidates(
          number_features,
          fdf.feature, fdf.mz, fdf.rt, fdf.polarity,
          polarity, feature, mz, mzmin, mzmax, rt, rtmin, rtmax,
          rtWindowAlignment, max_mz_chain);

      const int number_candidates = candidates.size();

      if (number_candidates > 0)
      {

        candidates.insert(candidates.begin(), f);

        NTS::MS_CANDIDATE_CHAIN candidates_chain(candidates, fdf.feature, fdf.index, fdf.mz, fdf.mzmin, fdf.mzmax, fdf.rt, fdf.intensity);

        annotate_isotopes(af, combinations, candidates_chain, maxIsotopes, maxCharge, maxGaps);
      }
      else
      {
        af.index[index] = index;
        af.feature[index] = feature;
        af.component_feature[index] = feature;
        af.iso_step[index] = 0;
        af.iso_cat[index] = "M+0";
        af.iso_isotope[index] = "";
        af.iso_charge[index] = 1;
        af.iso_mzr[index] = 0;
        af.iso_mass_distance[index] = 0;
        af.iso_theoretical_mass_distance[index] = 0;
        af.iso_mass_distance_error[index] = 0;
        af.iso_time_error[index] = 0;
        af.iso_relative_intensity[index] = 1;
        af.iso_theoretical_min_relative_intensity[index] = 0;
        af.iso_theoretical_max_relative_intensity[index] = 0;
        af.iso_number_carbons[index] = 0;
        af.iso_size[index] = 0;
      }
    }

    Rcpp::Rcout << "Done!" << std::endl;

    Rcpp::Rcout << "Annotating adducts in " << number_features << " features...";

    for (int f = 0; f < number_features; f++)
    {

      const int &index = fdf.index[f];

      if (af.iso_step[index] > 0)
        continue; // already isotope

      if (af.adduct_cat[index] != "")
        continue; // already adduct

      const int &polarity = fdf.polarity[f];
      const float &rt = fdf.rt[f];
      float rtmin = fdf.rtmin[f];
      float rtmax = fdf.rtmax[f];
      const float &mz = fdf.mz[f];
      const float &mzmin = fdf.mzmin[f];
      const float &mzmax = fdf.mzmax[f];
      const float max_mz_adducts = (mz + 100);

      std::vector<int> candidates = NTS::find_adduct_candidates(
          number_features, fdf.mz, fdf.rt, fdf.polarity, af.iso_step, polarity, mz, mzmin, mzmax, rt, rtmin, rtmax, rtWindowAlignment, max_mz_adducts);

      const int number_candidates = candidates.size();

      if (number_candidates > 0)
      {
        candidates.insert(candidates.begin(), f);
        NTS::MS_CANDIDATE_CHAIN candidates_chain(candidates, fdf.feature, fdf.index, fdf.mz, fdf.mzmin, fdf.mzmax, fdf.rt, fdf.intensity);
        NTS::annotate_adducts(af, candidates_chain, polarity);
      }
    }

    Rcpp::Rcout << "Done!" << std::endl;

    Rcpp::List list_annotation(number_features);

    for (int f = 0; f < number_features; f++)
    {

      const int &index = fdf.index[f];

      Rcpp::List temp = Rcpp::List::create(
          Rcpp::Named("index") = index,
          Rcpp::Named("feature") = af.feature[f],
          Rcpp::Named("component_feature") = af.component_feature[f],
          Rcpp::Named("iso_size") = af.iso_size[f],
          Rcpp::Named("iso_charge") = af.iso_charge[f],
          Rcpp::Named("iso_step") = af.iso_step[f],
          Rcpp::Named("iso_cat") = af.iso_cat[f],
          Rcpp::Named("iso_isotope") = af.iso_isotope[f],
          Rcpp::Named("iso_mzr") = af.iso_mzr[f],
          Rcpp::Named("iso_relative_intensity") = af.iso_relative_intensity[f],
          Rcpp::Named("iso_theoretical_min_relative_intensity") = af.iso_theoretical_min_relative_intensity[f],
          Rcpp::Named("iso_theoretical_max_relative_intensity") = af.iso_theoretical_max_relative_intensity[f],
          Rcpp::Named("iso_mass_distance") = af.iso_mass_distance[f],
          Rcpp::Named("iso_theoretical_mass_distance") = af.iso_theoretical_mass_distance[f],
          Rcpp::Named("iso_mass_error") = af.iso_mass_distance_error[f],
          Rcpp::Named("iso_time_error") = af.iso_time_error[f],
          Rcpp::Named("iso_number_carbons") = af.iso_number_carbons[f],
          Rcpp::Named("adduct_element") = af.adduct_element[f],
          Rcpp::Named("adduct_cat") = af.adduct_cat[f],
          Rcpp::Named("adduct_time_error") = af.adduct_time_error[f],
          Rcpp::Named("adduct_mass_error") = af.adduct_mass_error[f]);

      temp.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      list_annotation[index] = temp;
    }

    features["annotation"] = list_annotation;

    feature_list[a] = features;

    // list_out["index"] = af.index;
    // list_out["feature"] = af.feature;
    // list_out["mz"] = fdf.mz;
    // list_out["rt"] = fdf.rt;
    // list_out["intensity"] = fdf.intensity;
    // list_out["component_feature"] = af.component_feature;
    // list_out["iso_size"] = af.iso_size;
    // list_out["iso_charge"] = af.iso_charge;
    // list_out["iso_step"] = af.iso_step;
    // list_out["iso_cat"] = af.iso_cat;
    // list_out["iso_isotope"] = af.iso_isotope;
    // list_out["iso_mzr"] = af.iso_mzr;
    // list_out["iso_relative_intensity"] = af.iso_relative_intensity;
    // list_out["iso_theoretical_min_relative_intensity"] = af.iso_theoretical_min_relative_intensity;
    // list_out["iso_theoretical_max_relative_intensity"] = af.iso_theoretical_max_relative_intensity;
    // list_out["iso_mass_distance"] = af.iso_mass_distance;
    // list_out["iso_theoretical_mass_distance"] = af.iso_theoretical_mass_distance;
    // list_out["iso_error"] = af.iso_mass_distance_error;
    // list_out["iso_number_carbons"] = af.iso_number_carbons;
    // list_out["adduct_element"] = af.adduct_element;
    // list_out["adduct_cat"] = af.adduct_cat;
    // list_out["adduct_error"] = af.adduct_error;
    // list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  }

  return feature_list;
};

// MARK: rcpp_ms_load_features_eic
// [[Rcpp::export]]
Rcpp::List rcpp_ms_load_features_eic(std::vector<std::string> analyses_names,
                                     std::vector<std::string> analyses_files,
                                     Rcpp::List headers,
                                     Rcpp::List features,
                                     bool filtered = false,
                                     float rtExpand = 0,
                                     float mzExpand = 0,
                                     float minTracesIntensity = 0)
{

  const int number_analyses = analyses_names.size();

  if (number_analyses == 0)
    return features;

  const int number_features_analyses = features.size();

  if (number_features_analyses != number_analyses)
    return features;

  std::vector<std::string> features_analyses_names = features.names();

  for (int i = 0; i < number_analyses; i++)
  {

    if (features_analyses_names[i] != analyses_names[i])
      return features;

    Rcpp::List features_i = features[i];

    const std::vector<std::string> &features_i_names = features_i.names();

    const int n_features_i_names = features_i_names.size();

    if (n_features_i_names == 0)
      return features;

    std::vector<std::string> must_have_names = {
        "feature", "rt", "rtmax", "rtmin", "mz", "mzmax", "mzmin", "polarity", "filtered", "eic"};

    const int n_must_have_names = must_have_names.size();

    std::vector<bool> has_must_have_names(n_must_have_names, false);

    for (int j = 0; j < n_must_have_names; ++j)
    {
      for (int k = 0; k < n_features_i_names; ++k)
      {
        if (must_have_names[j] == features_i_names[k])
          has_must_have_names[j] = true;
      }
    }

    for (bool value : has_must_have_names)
    {
      if (!value)
      {
        return features;
      }
    }

    const std::vector<std::string> &fts_id = features_i["feature"];
    const std::vector<bool> &fts_filtered = features_i["filtered"];
    const std::vector<int> &fts_polarity = features_i["polarity"];
    const std::vector<float> &fts_rt = features_i["rt"];
    const std::vector<float> &fts_rtmin = features_i["rtmin"];
    const std::vector<float> &fts_rtmax = features_i["rtmax"];
    const std::vector<float> &fts_mz = features_i["mz"];
    const std::vector<float> &fts_mzmin = features_i["mzmin"];
    const std::vector<float> &fts_mzmax = features_i["mzmax"];
    const std::vector<float> &fts_intensity = features_i["intensity"];
    std::vector<Rcpp::List> fts_eic = features_i["eic"];

    const int n_features = fts_id.size();

    if (n_features == 0)
      return features;

    sc::MS_TARGETS targets;

    int counter = 0;
    for (int j = 0; j < n_features; j++)
    {

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const int eic_size = fts_eic[j].size();
      if (eic_size > 0)
        continue;

      targets.index.push_back(counter);
      counter++;
      targets.id.push_back(fts_id[j]);
      targets.level.push_back(1);
      targets.polarity.push_back(fts_polarity[j]);
      targets.precursor.push_back(false);
      targets.mzmin.push_back(fts_mzmin[j] - mzExpand);
      targets.mzmax.push_back(fts_mzmax[j] + mzExpand);
      targets.rtmin.push_back(fts_rtmin[j] - rtExpand);
      targets.rtmax.push_back(fts_rtmax[j] + rtExpand);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
    }

    if (targets.id.size() == 0)
      continue;

    // const Rcpp::List &analysis = analyses[i];
    // 
    // sc::MS_SPECTRA_HEADERS headers = NTS::get_ms_analysis_list_headers(analysis);

    const std::string file = analyses_files[i];

    if (!std::filesystem::exists(file))
      continue;

    sc::MS_FILE ana(file);
    
    const Rcpp::List &hd = headers[i];
    
    const sc::MS_SPECTRA_HEADERS header = NTS::as_MS_SPECTRA_HEADERS(hd);

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header, minTracesIntensity, 0);

    for (int j = 0; j < n_features; j++)
    {

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const std::string &id_j = fts_id[j];

      sc::MS_TARGETS_SPECTRA res_j = res[id_j];

      NTS::merge_traces_within_rt(res_j.rt, res_j.mz, res_j.intensity);

      const int n = res_j.rt.size();
      const std::vector<std::string> id_vec = std::vector<std::string>(n, id_j);
      const std::vector<int> level_vec = std::vector<int>(n, res_j.level[0]);
      const std::vector<int> polarity_vec = std::vector<int>(n, res_j.polarity[0]);

      Rcpp::List eic = Rcpp::List::create(
          Rcpp::Named("feature") = id_vec,
          Rcpp::Named("polarity") = polarity_vec,
          Rcpp::Named("level") = level_vec,
          Rcpp::Named("rt") = res_j.rt,
          Rcpp::Named("mz") = res_j.mz,
          Rcpp::Named("intensity") = res_j.intensity);

      eic.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      fts_eic[j] = eic;
    }

    features_i["eic"] = fts_eic;
    features[i] = features_i;
  }

  return features;
}

// MARK: rcpp_ms_load_features_ms1
// [[Rcpp::export]]
Rcpp::List rcpp_ms_load_features_ms1(std::vector<std::string> analyses_names,
                                     std::vector<std::string> analyses_files,
                                     Rcpp::List headers,
                                     Rcpp::List features,
                                     bool filtered,
                                     std::vector<float> rtWindow,
                                     std::vector<float> mzWindow,
                                     float minTracesIntensity,
                                     float mzClust,
                                     float presence)
{

  const int number_analyses = analyses_names.size();

  if (number_analyses == 0)
    return features;

  const int number_features_analyses = features.size();

  if (number_features_analyses != number_analyses)
    return features;

  std::vector<std::string> features_analyses_names = features.names();

  int rtWindowMax_idx = NTS::find_max_index(rtWindow);
  int rtWindowMin_idx = NTS::find_min_index(rtWindow);
  int mzWindowMax_idx = NTS::find_max_index(mzWindow);
  int mzWindowMin_idx = NTS::find_min_index(mzWindow);

  for (int i = 0; i < number_analyses; i++)
  {

    if (features_analyses_names[i] != analyses_names[i])
      return features;

    Rcpp::List features_i = features[i];

    const std::vector<std::string> &features_i_names = features_i.names();

    const int n_features_i_names = features_i_names.size();

    if (n_features_i_names == 0)
      return features;

    std::vector<std::string> must_have_names = {
        "feature", "rt", "rtmax", "rtmin", "mz", "mzmax", "mzmin", "polarity", "filtered", "eic"};

    const int n_must_have_names = must_have_names.size();

    std::vector<bool> has_must_have_names(n_must_have_names, false);

    for (int j = 0; j < n_must_have_names; ++j)
    {
      for (int k = 0; k < n_features_i_names; ++k)
      {
        if (must_have_names[j] == features_i_names[k])
          has_must_have_names[j] = true;
      }
    }

    for (bool value : has_must_have_names)
    {
      if (!value)
      {
        return features;
      }
    }

    const std::vector<std::string> &fts_id = features_i["feature"];
    const std::vector<bool> &fts_filtered = features_i["filtered"];
    const std::vector<int> &fts_polarity = features_i["polarity"];
    const std::vector<float> &fts_rt = features_i["rt"];
    const std::vector<float> &fts_mz = features_i["mz"];
    const std::vector<float> &fts_intensity = features_i["intensity"];
    std::vector<Rcpp::List> fts_ms1 = features_i["ms1"];

    const int n_features = fts_id.size();

    if (n_features == 0)
      return features;

    sc::MS_TARGETS targets;

    int counter = 0;
    for (int j = 0; j < n_features; j++)
    {
      if (!filtered)
        if (fts_filtered[j])
          continue;

      const int ms1_size = fts_ms1[j].size();
      if (ms1_size > 0)
        continue;

      targets.index.push_back(counter);
      counter++;
      targets.id.push_back(fts_id[j]);
      targets.level.push_back(1);
      targets.polarity.push_back(fts_polarity[j]);
      targets.precursor.push_back(false);
      targets.mzmin.push_back(fts_mz[j] + mzWindow[mzWindowMin_idx]);
      targets.mzmax.push_back(fts_mz[j] + mzWindow[mzWindowMax_idx]);
      targets.rtmin.push_back(fts_rt[j] + rtWindow[rtWindowMin_idx]);
      targets.rtmax.push_back(fts_rt[j] + rtWindow[rtWindowMax_idx]);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
    }

    if (targets.id.size() == 0)
      continue;

    const std::string file = analyses_files[i];

    if (!std::filesystem::exists(file))
      continue;

    sc::MS_FILE ana(file);
    
    const Rcpp::List &hd = headers[i];
    
    const sc::MS_SPECTRA_HEADERS header = NTS::as_MS_SPECTRA_HEADERS(hd);

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header, minTracesIntensity, 0);

    for (int j = 0; j < n_features; j++)
    {

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const std::string &id_j = fts_id[j];

      sc::MS_TARGETS_SPECTRA res_j = res[id_j];

      const int n_res_j = res_j.rt.size();

      if (n_res_j == 0)
        continue;

      const std::vector<std::string> id_vec = std::vector<std::string>(n_res_j, id_j);

      const Rcpp::List ms1 = Rcpp::List::create(
          Rcpp::Named("feature") = id_vec,
          Rcpp::Named("polarity") = res_j.polarity,
          Rcpp::Named("level") = res_j.level,
          Rcpp::Named("pre_mz") = res_j.pre_mz,
          Rcpp::Named("pre_ce") = res_j.pre_ce,
          Rcpp::Named("rt") = res_j.rt,
          Rcpp::Named("mz") = res_j.mz,
          Rcpp::Named("intensity") = res_j.intensity);

      Rcpp::List ms1_clustered = NTS::cluster_spectra(ms1, mzClust, presence);

      ms1_clustered.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      fts_ms1[j] = ms1_clustered;
    }

    features_i["ms1"] = fts_ms1;
    features[i] = features_i;
  }

  return features;
}

// MARK: rcpp_ms_load_features_ms2
// [[Rcpp::export]]
Rcpp::List rcpp_ms_load_features_ms2(std::vector<std::string> analyses_names,
                                     std::vector<std::string> analyses_files,
                                     Rcpp::List headers,
                                     Rcpp::List features,
                                     bool filtered,
                                     float minTracesIntensity,
                                     float isolationWindow,
                                     float mzClust,
                                     float presence)
{

  const int number_analyses = analyses_names.size();

  if (number_analyses == 0)
    return features;

  const int number_features_analyses = features.size();

  if (number_features_analyses != number_analyses)
    return features;

  std::vector<std::string> features_analyses_names = features.names();

  for (int i = 0; i < number_analyses; i++)
  {

    if (features_analyses_names[i] != analyses_names[i])
      return features;

    Rcpp::List features_i = features[i];

    const std::vector<std::string> &features_i_names = features_i.names();

    const int n_features_i_names = features_i_names.size();

    if (n_features_i_names == 0)
      return features;

    std::vector<std::string> must_have_names = {
        "feature", "rt", "rtmax", "rtmin", "mz", "mzmax", "mzmin", "polarity", "filtered", "eic"};

    const int n_must_have_names = must_have_names.size();

    std::vector<bool> has_must_have_names(n_must_have_names, false);

    for (int j = 0; j < n_must_have_names; ++j)
    {
      for (int k = 0; k < n_features_i_names; ++k)
      {
        if (must_have_names[j] == features_i_names[k])
          has_must_have_names[j] = true;
      }
    }

    for (bool value : has_must_have_names)
    {
      if (!value)
      {
        return features;
      }
    }

    const std::vector<std::string> &fts_id = features_i["feature"];
    const std::vector<bool> &fts_filtered = features_i["filtered"];
    const std::vector<int> &fts_polarity = features_i["polarity"];
    const std::vector<float> &fts_rt = features_i["rt"];
    const std::vector<float> &fts_rtmin = features_i["rtmin"];
    const std::vector<float> &fts_rtmax = features_i["rtmax"];
    const std::vector<float> &fts_mz = features_i["mz"];
    const std::vector<float> &fts_intensity = features_i["intensity"];
    std::vector<Rcpp::List> fts_ms2 = features_i["ms2"];

    const int n_features = fts_id.size();

    if (n_features == 0)
      return features;

    sc::MS_TARGETS targets;

    int counter = 0;
    for (int j = 0; j < n_features; j++)
    {

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const int ms2_size = fts_ms2[j].size();
      if (ms2_size > 0)
        continue;

      targets.index.push_back(counter);
      counter++;
      targets.id.push_back(fts_id[j]);
      targets.level.push_back(2);
      targets.polarity.push_back(fts_polarity[j]);
      targets.precursor.push_back(true);
      targets.mzmin.push_back(fts_mz[j] - (isolationWindow / 2));
      targets.mzmax.push_back(fts_mz[j] + (isolationWindow / 2));
      targets.rtmin.push_back(fts_rtmin[j] - 1);
      targets.rtmax.push_back(fts_rtmax[j] + 1);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
    }

    if (targets.id.size() == 0)
      continue;

    const std::string file = analyses_files[i];

    if (!std::filesystem::exists(file))
      continue;

    sc::MS_FILE ana(file);
    
    const Rcpp::List &hd = headers[i];
    
    const sc::MS_SPECTRA_HEADERS header = NTS::as_MS_SPECTRA_HEADERS(hd);

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header, 0, minTracesIntensity);

    for (int j = 0; j < n_features; j++)
    {

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const std::string &id_j = fts_id[j];

      sc::MS_TARGETS_SPECTRA res_j = res[id_j];

      const int n_res_j = res_j.rt.size();

      if (n_res_j == 0)
        continue;

      const std::vector<std::string> id_vec = std::vector<std::string>(n_res_j, id_j);

      const Rcpp::List ms2 = Rcpp::List::create(
          Rcpp::Named("feature") = id_vec,
          Rcpp::Named("polarity") = res_j.polarity,
          Rcpp::Named("level") = res_j.level,
          Rcpp::Named("pre_mz") = res_j.pre_mz,
          Rcpp::Named("pre_ce") = res_j.pre_ce,
          Rcpp::Named("rt") = res_j.rt,
          Rcpp::Named("mz") = res_j.mz,
          Rcpp::Named("intensity") = res_j.intensity);

      Rcpp::List ms2_clustered = NTS::cluster_spectra(ms2, mzClust, presence);

      ms2_clustered.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      fts_ms2[j] = ms2_clustered;
    }

    features_i["ms2"] = fts_ms2;
    features[i] = features_i;
  }

  return features;
}

// MARK: rcpp_ms_fill_features
// [[Rcpp::export]]
Rcpp::List rcpp_ms_fill_features(std::vector<std::string> analyses_names,
                                 std::vector<std::string> analyses_replicates,
                                 std::vector<std::string> analyses_files,
                                 Rcpp::List headers,
                                 Rcpp::DataFrame features,
                                 bool withinReplicate = false,
                                 float rtExpand = 0,
                                 float mzExpand = 0,
                                 float minPeakWidth = 6,
                                 float minTracesIntensity = 0,
                                 float minNumberTraces = 4,
                                 float minIntensity = 0,
                                 float baseCut = 0,
                                 float maxSearchWindow = 5,
                                 float minSignalToNoiseRatio = 3,
                                 float minGaussianFit = 0.5)
{

  Rcpp::List out;
  
  if (minNumberTraces < 5)
  {
    minNumberTraces = 5;
  }
  
  const float minHalfPeakWidth = minPeakWidth / 2;

  const int number_analyses = analyses_names.size();

  if (number_analyses == 0)
    return out;

  std::vector<std::vector<int>> analyses_polarities(number_analyses);

  for (int i = 0; i < number_analyses; i++)
  {
    const Rcpp::List &hd_i = headers[i];
    std::vector<int> pols = hd_i["polarity"];
    std::set<int> pols_set(pols.begin(), pols.end());
    std::vector<int> pols_unique(pols_set.begin(), pols_set.end());
    analyses_polarities[i] = pols_unique;
  }

  int max_presence = number_analyses;

  const int number_features = features.nrow();

  if (number_features == 0)
    return out;

  std::vector<std::string> features_cols = features.names();

  const int ncol_features = features_cols.size();

  if (ncol_features == 0)
    return out;

  std::vector<std::string> must_have_names = {
      "feature", "analysis", "rt", "rtmax", "rtmin",
      "mz", "mzmax", "mzmin", "intensity", "group"};

  std::vector<bool> has_must_have_names(10, false);

  for (size_t i = 0; i < must_have_names.size(); ++i)
  {
    for (size_t j = 0; j < features_cols.size(); ++j)
    {
      if (must_have_names[i] == features_cols[j])
        has_must_have_names[i] = true;
    }
  }

  for (bool value : has_must_have_names)
  {
    if (!value)
    {
      return out;
    }
  }

  const std::vector<std::string> &fts_feature = features["feature"];
  const std::vector<std::string> &fts_group = features["group"];
  const std::vector<std::string> &fts_replicates = features["replicate"];
  const std::vector<std::string> &fts_analysis = features["analysis"];
  const std::vector<float> &fts_mass = features["mass"];
  const std::vector<float> &fts_mz = features["mz"];
  const std::vector<float> &fts_mzmin = features["mzmin"];
  const std::vector<float> &fts_mzmax = features["mzmax"];
  const std::vector<float> &fts_rt = features["rt"];
  const std::vector<float> &fts_rtmin = features["rtmin"];
  const std::vector<float> &fts_rtmax = features["rtmax"];
  // const std::vector<float> fts_mobility = features["mobility"];
  // const std::vector<float> fts_mobilitymin = features["mobilitymin"];
  // const std::vector<float> fts_mobilitymax = features["mobilitymax"];
  const std::vector<float> &fts_intensity = features["intensity"];
  const std::vector<int> &fts_polarity = features["polarity"];
  const std::vector<bool> &fts_filtered = features["filtered"];
  const std::vector<std::string> &fts_filter = features["filter"];
  const std::vector<std::string> &fts_adduct = features["adduct"];
  const std::vector<Rcpp::List> &fts_quality = features["quality"];
  const std::vector<Rcpp::List> &fts_annotation = features["annotation"];
  const std::vector<Rcpp::List> &fts_eic = features["eic"];
  const std::vector<Rcpp::List> &fts_ms1 = features["ms1"];
  const std::vector<Rcpp::List> &fts_ms2 = features["ms2"];
  const std::vector<Rcpp::List> &fts_istd = features["istd"];
  const std::vector<Rcpp::List> &fts_suspects = features["suspects"];
  const std::vector<Rcpp::List> &fts_formulas = features["formulas"];
  const std::vector<Rcpp::List> &fts_compounds = features["compounds"];

  std::vector<std::string> fts_id = fts_group;
  
  std::vector<bool> process_feature(number_features, true);
  
  for (int i = 0; i < number_features; i++)
  {
    if (fts_filtered[i]) process_feature[i] = false;
    if (fts_group[i] == "") process_feature[i] = false;
  }

  if (withinReplicate)
  {

    std::unordered_map<std::string, int> rpl_sizes;

    for (int i = 0; i < number_analyses; i++)
    {
      std::string rpl = analyses_replicates[i];
      rpl_sizes[rpl]++;
    }

    max_presence = 0;
    for (const auto &rpl : rpl_sizes)
      if (rpl.second > max_presence)
        max_presence = rpl.second;

    for (int i = 0; i < number_features; i++)
    {
      if (process_feature[i])
      {
        fts_id[i] = fts_id[i] + "_" + fts_replicates[i];
      }
    }
  }

  if (max_presence == 0)
    return out;

  std::unordered_map<std::string, int> id_presence;
  for (int i = 0; i < number_features; i++)
  {
    if (process_feature[i])
    {
      id_presence[fts_group[i]]++;
    }
  }

  std::vector<std::string> id;
  
  for (int i = 0; i < number_features; i++)
  {
    if (process_feature[i])
    {
      if (id_presence[fts_id[i]] < max_presence)
      {
        id.push_back(fts_id[i]);
      }
    }
  }

  if (id.empty())
    return out;

  std::set<std::string> id_set(id.begin(), id.end());

  std::vector<std::string> unique_id(id_set.begin(), id_set.end());

  const int number_unique_id = unique_id.size();

  std::vector<sc::MS_TARGETS> ana_targets(number_analyses);
  std::vector<std::vector<std::string>> ana_targets_groups(number_analyses);
  std::vector<std::vector<std::string>> ana_targets_replicates(number_analyses);
  std::vector<std::vector<float>> ana_targets_sd_mass(number_analyses);
  std::vector<std::vector<float>> ana_targets_sd_rt(number_analyses);

  Rcpp::Rcout << "Building targets for filling " << number_unique_id << " feature groups...";

  for (int k = 0; k < number_unique_id; k++)
  {

    std::vector<int> indices;
    for (int i = 0; i < number_features; i++)
    {
      if (fts_id[i] == unique_id[k])
      {
        indices.push_back(i);
      }
    }

    if (indices.size() == 0) continue;

    std::vector<std::string> fts_group_k(indices.size());
    std::vector<std::string> fts_replicates_k(indices.size());
    std::vector<std::string> fts_analysis_k(indices.size());
    std::vector<float> fts_mass_k(indices.size());
    std::vector<float> fts_mz_k(indices.size());
    std::vector<float> fts_mzmin_k(indices.size());
    std::vector<float> fts_mzmax_k(indices.size());
    std::vector<float> fts_rt_k(indices.size());
    std::vector<float> fts_rtmin_k(indices.size());
    std::vector<float> fts_rtmax_k(indices.size());
    std::vector<float> fts_intensity_k(indices.size());
    std::vector<float> fts_mzmin_side_k(indices.size());
    std::vector<float> fts_mzmax_side_k(indices.size());

    for (size_t i = 0; i < indices.size(); i++)
    {
      fts_group_k[i] = fts_group[indices[i]];
      fts_replicates_k[i] = fts_replicates[indices[i]];
      fts_analysis_k[i] = fts_analysis[indices[i]];
      fts_mass_k[i] = fts_mass[indices[i]];
      
      const float &mz_k = fts_mz[indices[i]];
      float mzmin_k = fts_mzmin[indices[i]];
      float mzmax_k = fts_mzmax[indices[i]];
      fts_mz_k[i] = mz_k;
      fts_mzmin_k[i] = mzmin_k;
      fts_mzmax_k[i] = mzmax_k;
      fts_mzmin_side_k[i] = mz_k - mzmin_k;
      fts_mzmax_side_k[i] = mzmax_k - mz_k;
      
      const float &rt_k = fts_rt[indices[i]];
      float rtmin_k = fts_rtmin[indices[i]];
      float rtmax_k = fts_rtmax[indices[i]];
      fts_rt_k[i] = rt_k;
      
      if (rt_k - rtmin_k < minHalfPeakWidth)
      {
        rtmin_k = rt_k - minHalfPeakWidth;
      }
      
      if (rtmax_k - rt_k < minHalfPeakWidth)
      {
        rtmax_k = rt_k + minHalfPeakWidth;
      }
      
      fts_rtmin_k[i] = rtmin_k - rtExpand;
      fts_rtmax_k[i] = rtmax_k + rtExpand;
      fts_intensity_k[i] = fts_intensity[indices[i]];
    }

    const float mean_mass = std::accumulate(fts_mass_k.begin(), fts_mass_k.end(), 0.0) / fts_mass_k.size();
    const float mean_rt = std::accumulate(fts_rt_k.begin(), fts_rt_k.end(), 0.0) / fts_rt_k.size();
    const float max_mzmin_side = *std::max_element(fts_mzmin_side_k.begin(), fts_mzmin_side_k.end());
    const float max_mzmax_side = *std::max_element(fts_mzmax_side_k.begin(), fts_mzmax_side_k.end());
    const float min_rtmin = *std::min_element(fts_rtmin_k.begin(), fts_rtmin_k.end());
    const float max_rtmax = *std::max_element(fts_rtmax_k.begin(), fts_rtmax_k.end());
    
    const float sd_mass = std::sqrt(std::inner_product(fts_mass_k.begin(), fts_mass_k.end(), fts_mass_k.begin(), 0.0) / fts_mass_k.size() - mean_mass * mean_mass);
    const float sd_rt = std::sqrt(std::inner_product(fts_rt_k.begin(), fts_rt_k.end(), fts_rt_k.begin(), 0.0) / fts_rt_k.size() - mean_rt * mean_rt);
    
    for (int j = 0; j < number_analyses; j++)
    {

      if (withinReplicate)
      {
        const std::string &rpl = analyses_replicates[j];

        // continues if analysis replicate j is not within the same replicate as the feature group
        if (fts_replicates_k[0] != rpl)
          continue;
      }

      bool has_analysis = false;

      for (size_t i = 0; i < indices.size(); i++)
      {
        if (fts_analysis_k[i] == analyses_names[j])
        {
          has_analysis = true;
        }
      }

      if (has_analysis) continue;

      std::vector<int> pols = analyses_polarities[j];

      for (size_t p = 0; p < pols.size(); p++)
      {
        float mz = mean_mass + (pols[p] * 1.007276);
        float mzmin = mz - max_mzmin_side - mzExpand;
        float mzmax = mz + max_mzmax_side + mzExpand;

        const int n_targets = ana_targets[j].index.size();
        ana_targets[j].index.push_back(n_targets);
        ana_targets[j].id.push_back(unique_id[k]);
        ana_targets[j].level.push_back(1);
        ana_targets[j].polarity.push_back(pols[p]);
        ana_targets[j].precursor.push_back(false);
        ana_targets[j].mz.push_back(mz);
        ana_targets[j].mzmin.push_back(mzmin);
        ana_targets[j].mzmax.push_back(mzmax);
        ana_targets[j].rt.push_back(mean_rt);
        ana_targets[j].rtmin.push_back(min_rtmin);
        ana_targets[j].rtmax.push_back(max_rtmax);
        ana_targets[j].mobilitymin.push_back(0);
        ana_targets[j].mobilitymax.push_back(0);
        ana_targets_groups[j].push_back(fts_group_k[0]);
        ana_targets_replicates[j].push_back(fts_replicates_k[0]);
        ana_targets_sd_mass[j].push_back(sd_mass);
        ana_targets_sd_rt[j].push_back(sd_rt);
      }
    }
  }

  Rcpp::Rcout << "Done!" << std::endl;
  
  for (int j = 0; j < number_analyses; j++)
  {
    Rcpp::List out_analyses;

    int n_j_targets = ana_targets[j].id.size();

    if (n_j_targets == 0)
    {
      out.push_back(out_analyses);
      continue;
    }
    
    Rcpp::Rcout << "Processing analyses " << analyses_names[j] << ":" << std::endl;
    
    Rcpp::Rcout << "    Checking if there are already corresponding features" << "...";
    
    for (int i = n_j_targets - 1; i >= 0; --i)
    {
      // const std::string &id_i = ana_targets[j].id[i];
      const int polarity_i = ana_targets[j].polarity[i];
      // const float mz_i = ana_targets[j].mz[i];
      const float mzmin_i = ana_targets[j].mzmin[i];
      const float mzmax_i = ana_targets[j].mzmax[i];
      const float rt_i = ana_targets[j].rt[i];
      const float rtmin_i = ana_targets[j].rtmin[i];
      const float rtmax_i = ana_targets[j].rtmax[i];
      // float sd_mass_i = ana_targets_sd_mass[j][i];
      float sd_rt_i = ana_targets_sd_rt[j][i];
      // if (sd_mass_i == 0) sd_mass_i = (mz_i - mzmin_i) / 2;
      if (sd_rt_i == 0) sd_rt_i = (rtmax_i - rtmin_i) / 2;
      // const float sd_mass_min = mz_i - (sd_mass_i * 2);
      // const float sd_mass_max = mz_i + (sd_mass_i * 2);
      const float sd_rt_min = rt_i - (sd_rt_i * 2);
      const float sd_rt_max = rt_i + (sd_rt_i * 2);
      
      bool has_feature = false;
      
      for (int k = 0; k < number_features; k++)
      {
        if (fts_analysis[k] == analyses_names[j])
        {
          if (!process_feature[k])
          {
            if (fts_polarity[k] == polarity_i)
            {
              if (fts_mz[k] > mzmin_i && fts_mz[k] < mzmax_i)
              {
                if (fts_rt[k] > sd_rt_min && fts_rt[k] < sd_rt_max)
                {
                  // Rcpp::List empty_dt = Rcpp::List::create();
                  // empty_dt.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  // Rcpp::List empty_list = Rcpp::List::create(empty_dt);
                  
                  Rcpp::List quality_k = fts_quality[k];
                  quality_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  Rcpp::List annotation_k = fts_annotation[k];
                  annotation_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  Rcpp::List eic_k = fts_eic[k];
                  eic_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  Rcpp::List ms1_k = fts_ms1[k];
                  ms1_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  Rcpp::List ms2_k = fts_ms2[k];
                  ms2_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  Rcpp::List istd_k = fts_istd[k];
                  istd_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  Rcpp::List suspects_k = fts_suspects[k];
                  suspects_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  Rcpp::List formulas_k = fts_formulas[k];
                  formulas_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  Rcpp::List compounds_k = fts_compounds[k];
                  compounds_k.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  
                  Rcpp::List quality_k_l = Rcpp::List::create(quality_k);
                  Rcpp::List annotation_k_l = Rcpp::List::create(annotation_k);
                  Rcpp::List eic_k_l = Rcpp::List::create(eic_k);
                  Rcpp::List ms1_k_l = Rcpp::List::create(ms1_k);
                  Rcpp::List ms2_k_l = Rcpp::List::create(ms2_k);
                  Rcpp::List istd_k_l = Rcpp::List::create(istd_k);
                  Rcpp::List suspects_k_l = Rcpp::List::create(suspects_k);
                  Rcpp::List formulas_k_l = Rcpp::List::create(formulas_k);
                  Rcpp::List compounds_k_l = Rcpp::List::create(compounds_k);
                  
                  Rcpp::List out_targets;
                  out_targets["analysis"] = analyses_names[j];
                  out_targets["feature"] = fts_feature[k];
                  out_targets["group"] = ana_targets_groups[j][i];
                  out_targets["rt"] = fts_rt[k];
                  out_targets["mz"] = fts_mz[k];
                  out_targets["intensity"] = fts_intensity[k];
                  out_targets["area"] = fts_intensity[k];
                  out_targets["rtmin"] = fts_rtmin[k];
                  out_targets["rtmax"] = fts_rtmax[k];
                  out_targets["mzmin"] = fts_mzmin[k];
                  out_targets["mzmax"] = fts_mzmax[k];
                  out_targets["mass"] = fts_mass[k];
                  out_targets["polarity"] = fts_polarity[k];
                  out_targets["adduct"] = fts_adduct[k];
                  out_targets["filtered"] = false;
                  out_targets["filter"] = fts_filter[k] + " recovered ";
                  out_targets["filled"] = false;
                  out_targets["eic"] = eic_k_l;
                  out_targets["ms1"] = ms1_k_l;
                  out_targets["ms2"] = ms2_k_l;
                  out_targets["quality"] = quality_k_l;
                  out_targets["annotation"] = annotation_k_l;
                  out_targets["istd"] = istd_k_l;
                  out_targets["suspects"] = suspects_k_l;
                  out_targets["formulas"] = formulas_k_l;
                  out_targets["compounds"] = compounds_k_l;
                  out_targets.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
                  out_analyses[fts_feature[k]] = out_targets;
                  has_feature = true;
                  break;
                }
              }
            }
          }
        }
      }
      
      if (has_feature)
      {
        ana_targets[j].index.erase(ana_targets[j].index.begin() + i);
        ana_targets[j].id.erase(ana_targets[j].id.begin() + i);
        ana_targets[j].level.erase(ana_targets[j].level.begin() + i);
        ana_targets[j].polarity.erase(ana_targets[j].polarity.begin() + i);
        ana_targets[j].precursor.erase(ana_targets[j].precursor.begin() + i);
        ana_targets[j].mz.erase(ana_targets[j].mz.begin() + i);
        ana_targets[j].mzmin.erase(ana_targets[j].mzmin.begin() + i);
        ana_targets[j].mzmax.erase(ana_targets[j].mzmax.begin() + i);
        ana_targets[j].rt.erase(ana_targets[j].rt.begin() + i);
        ana_targets[j].rtmin.erase(ana_targets[j].rtmin.begin() + i);
        ana_targets[j].rtmax.erase(ana_targets[j].rtmax.begin() + i);
        ana_targets[j].mobilitymin.erase(ana_targets[j].mobilitymin.begin() + i);
        ana_targets[j].mobilitymax.erase(ana_targets[j].mobilitymax.begin() + i);
        ana_targets_groups[j].erase(ana_targets_groups[j].begin() + i);
        ana_targets_replicates[j].erase(ana_targets_replicates[j].begin() + i);
      }
    }
    
    Rcpp::Rcout << "recovered " << n_j_targets - ana_targets[j].id.size() << " features!" << std::endl;
    
    n_j_targets = ana_targets[j].id.size();
    
    if (n_j_targets == 0)
    {
      out.push_back(out_analyses);
      continue;
    }

    Rcpp::Rcout << "    Extracting " << n_j_targets << " EICs...";

    const Rcpp::List &hd = headers[j];
    
    const sc::MS_SPECTRA_HEADERS header = NTS::as_MS_SPECTRA_HEADERS(hd);

    const std::string file = analyses_files[j];

    if (!std::filesystem::exists(file))
      continue;

    sc::MS_FILE ana(file);

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(ana_targets[j], header, minTracesIntensity, 0);

    const int n_traces = res.id.size();

    Rcpp::Rcout << " Done! " << std::endl;
    
    if (n_traces == 0)
    {
      out.push_back(out_analyses);
      continue;
    }
    
    Rcpp::Rcout << "    Filtering " << n_traces << " spectra traces...";
    
    for (int i = n_j_targets - 1; i >= 0; --i)
    {

      const std::string &id_i = ana_targets[j].id[i];
      
      // if (id_i == "M268_R916_1302") {
      //   Rcpp::Rcout << std::endl;
      //   for (int z = 0; z < n_traces; z++)
      //     if (res.id[z] == id_i)
      //       Rcpp::Rcout <<  res.rt[z] << " " << res.mz[z] << " " << res.intensity[z] << std::endl;
      //   Rcpp::Rcout << std::endl;
      // }

      int count = 0;
      float max_intensity = 0;

      for (int z = 0; z < n_traces; z++)
      {
        if (res.id[z] == id_i)
        {
          count++;
          if (res.intensity[z] > max_intensity)
          {
            max_intensity = res.intensity[z];
          }
        }
      }

      if (count >= minNumberTraces && max_intensity >= minIntensity)
      {
        continue;
      }
      
      ana_targets[j].index.erase(ana_targets[j].index.begin() + i);
      ana_targets[j].id.erase(ana_targets[j].id.begin() + i);
      ana_targets[j].level.erase(ana_targets[j].level.begin() + i);
      ana_targets[j].polarity.erase(ana_targets[j].polarity.begin() + i);
      ana_targets[j].precursor.erase(ana_targets[j].precursor.begin() + i);
      ana_targets[j].mz.erase(ana_targets[j].mz.begin() + i);
      ana_targets[j].mzmin.erase(ana_targets[j].mzmin.begin() + i);
      ana_targets[j].mzmax.erase(ana_targets[j].mzmax.begin() + i);
      ana_targets[j].rt.erase(ana_targets[j].rt.begin() + i);
      ana_targets[j].rtmin.erase(ana_targets[j].rtmin.begin() + i);
      ana_targets[j].rtmax.erase(ana_targets[j].rtmax.begin() + i);
      ana_targets[j].mobilitymin.erase(ana_targets[j].mobilitymin.begin() + i);
      ana_targets[j].mobilitymax.erase(ana_targets[j].mobilitymax.begin() + i);
      ana_targets_groups[j].erase(ana_targets_groups[j].begin() + i);
      ana_targets_replicates[j].erase(ana_targets_replicates[j].begin() + i);
      
    }

    n_j_targets = ana_targets[j].id.size();
    
    Rcpp::Rcout << " Done! " << std::endl;
    
    if (n_j_targets == 0)
    {
      out.push_back(out_analyses);
      continue;
    }

    Rcpp::Rcout << "    Filling " << n_j_targets << " features...";

    const std::vector<std::string> tg_id = ana_targets[j].id;
    const std::vector<float> tg_rt = ana_targets[j].rt;
    
    int filled_counter = 0;

    for (int i = 0; i < n_j_targets; i++)
    {
      const std::string &id_i = tg_id[i];
      const float &tg_rt_i = tg_rt[i];

      sc::MS_TARGETS_SPECTRA res_i = res[id_i];

      const int n_res_i = res_i.id.size();

      if (n_res_i < minNumberTraces)
      {
        continue;
      }

      NTS::merge_traces_within_rt(res_i.rt, res_i.mz, res_i.intensity);
      
      Rcpp::List quality = NTS::calculate_gaussian_fit(
        id_i, tg_rt_i, res_i.mz, res_i.rt, res_i.intensity, baseCut, maxSearchWindow
      );

      const float &sn = quality["sn"];

      const float &gauss_f = quality["gauss_f"];

      if (sn < minSignalToNoiseRatio)
      {
        continue;
      }

      if (gauss_f < minGaussianFit)
      {
        continue;
      }

      const float area_i = NTS::trapezoidal_area(res_i.rt, res_i.intensity);

      const size_t max_position = NTS::find_central_max_index(res_i.rt, res_i.intensity, tg_rt_i, 0);

      const float rt_i = res_i.rt[max_position];

      const float mz_i = NTS::mean(res_i.mz);

      const float mass_i = mz_i - (res_i.polarity[0] * 1.007276);

      const float mzmin_i = *std::min_element(res_i.mz.begin(), res_i.mz.end());

      const float mzmax_i = *std::max_element(res_i.mz.begin(), res_i.mz.end());

      const float rtmin_i = *std::min_element(res_i.rt.begin(), res_i.rt.end());

      const float rtmax_i = *std::max_element(res_i.rt.begin(), res_i.rt.end());

      std::string adduct_i = "[M+H]+";
      if (res_i.polarity[0] < 0)
        adduct_i = "[M-H]-";

      std::string feature = "FL" + std::to_string(i + 1);
      feature += "_MZ" + std::to_string(static_cast<int>(std::round(mz_i)));
      feature += "_RT" + std::to_string(static_cast<int>(std::round(rt_i)));

      const int n = res_i.rt.size();
      const std::vector<std::string> id_vec = std::vector<std::string>(n, feature);
      const std::vector<int> polarity_vec = std::vector<int>(n, res_i.polarity[0]);
      const std::vector<int> level_vec = std::vector<int>(n, res_i.level[0]);

      Rcpp::List eic = Rcpp::List::create(
          Rcpp::Named("feature") = id_vec,
          Rcpp::Named("polarity") = polarity_vec,
          Rcpp::Named("level") = level_vec,
          Rcpp::Named("rt") = res_i.rt,
          Rcpp::Named("mz") = res_i.mz,
          Rcpp::Named("intensity") = res_i.intensity);

      eic.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      Rcpp::List list_eic;
      list_eic.push_back(eic);

      Rcpp::List list_quality;
      quality["feature"] = feature;
      quality.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      list_quality.push_back(quality);

      Rcpp::List empty_dt = Rcpp::List::create();
      empty_dt.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      Rcpp::List empty_list = Rcpp::List::create(empty_dt);

      Rcpp::List out_targets;
      out_targets["analysis"] = analyses_names[j];
      out_targets["feature"] = feature;
      out_targets["group"] = ana_targets_groups[j][i];
      out_targets["rt"] = rt_i;
      out_targets["mz"] = mz_i;
      out_targets["intensity"] = res_i.intensity[max_position];
      out_targets["area"] = area_i;
      out_targets["rtmin"] = rtmin_i;
      out_targets["rtmax"] = rtmax_i;
      out_targets["mzmin"] = mzmin_i;
      out_targets["mzmax"] = mzmax_i;
      out_targets["mass"] = mass_i;
      out_targets["polarity"] = res_i.polarity[0];
      out_targets["adduct"] = adduct_i;
      out_targets["filtered"] = false;
      out_targets["filled"] = true;
      out_targets["eic"] = list_eic;
      out_targets["ms1"] = empty_list;
      out_targets["ms2"] = empty_list;
      out_targets["quality"] = list_quality;
      out_targets["annotation"] = empty_list;
      out_targets["istd"] = empty_list;
      out_targets["suspects"] = empty_list;
      out_targets["formulas"] = empty_list;
      out_targets["compounds"] = empty_list;
      out_targets.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      out_analyses[feature] = out_targets;
      
      filled_counter++;
    }

    out.push_back(out_analyses);

    Rcpp::Rcout << " Done! " << std::endl;
    
    Rcpp::Rcout << "    Filled " << filled_counter << " features!" << std::endl;
  }

  out.names() = analyses_names;

  return out;
}

// MARK: rcpp_ms_calculate_features_quality
// [[Rcpp::export]]
Rcpp::List rcpp_ms_calculate_features_quality(std::vector<std::string> analyses_names,
                                              std::vector<std::string> analyses_files,
                                              Rcpp::List headers,
                                              Rcpp::List features,
                                              bool filtered = false,
                                              float rtExpand = 0,
                                              float mzExpand = 0,
                                              float minPeakWidth = 6,
                                              float minTracesIntensity = 0,
                                              float minNumberTraces = 5,
                                              float baseCut = 0)
{

  const int number_analyses = analyses_names.size();

  if (number_analyses == 0)
    return features;

  const int number_features_analyses = features.size();

  if (number_features_analyses != number_analyses)
    return features;
  
  if (minNumberTraces < 5)
  {
    minNumberTraces = 5;
  }
  
  const float minHalfPeakWidth = minPeakWidth / 2;

  std::vector<std::string> features_analyses_names = features.names();

  for (int i = 0; i < number_analyses; i++)
  {
    if (features_analyses_names[i] != analyses_names[i])
      continue;

    Rcpp::List features_i = features[i];
    
    const std::vector<std::string> &features_i_names = features_i.names();

    const int n_features_i_names = features_i_names.size();

    if (n_features_i_names == 0)
      continue;

    std::vector<std::string> must_have_names = {
        "feature", "rt", "rtmax", "rtmin", "mz", "mzmax", "mzmin",
        "polarity", "filtered", "quality", "eic"};

    const int n_must_have_names = must_have_names.size();

    std::vector<bool> has_must_have_names(n_must_have_names, false);

    for (int j = 0; j < n_must_have_names; ++j)
    {
      for (int k = 0; k < n_features_i_names; ++k)
      {
        if (must_have_names[j] == features_i_names[k])
          has_must_have_names[j] = true;
      }
    }
    
    bool has_all_must_have_names = true;

    for (bool value : has_must_have_names)
    {
      if (!value)
      {
        has_all_must_have_names = false;
      }
    }
    
    if (!has_all_must_have_names)
      continue;

    const std::vector<std::string> &fts_id = features_i["feature"];
    const std::vector<bool> &fts_filtered = features_i["filtered"];
    const std::vector<int> &fts_polarity = features_i["polarity"];
    const std::vector<float> &fts_rt = features_i["rt"];
    std::vector<float> fts_rtmin = features_i["rtmin"];
    std::vector<float> fts_rtmax = features_i["rtmax"];
    const std::vector<float> &fts_mz = features_i["mz"];
    std::vector<float> fts_mzmin = features_i["mzmin"];
    std::vector<float> fts_mzmax = features_i["mzmax"];
    const std::vector<float> &fts_intensity = features_i["intensity"];
    std::vector<Rcpp::List> fts_quality = features_i["quality"];
    std::vector<Rcpp::List> fts_eic = features_i["eic"];

    const int n_features = fts_id.size();

    if (n_features == 0)
      continue;
    
    Rcpp::Rcout << "Processing " << n_features << " features in analyses " << analyses_names[i] << "...";
    
    sc::MS_TARGETS targets;

    std::vector<bool> has_quality(n_features, false);

    std::vector<bool> has_eic(n_features, false);

    int counter = 0;
    for (int j = 0; j < n_features; j++)
    {

      const Rcpp::List &quality = fts_quality[j];

      const int n_quality = quality.size();

      if (n_quality > 1)
      {
        has_quality[j] = true;
        continue;
      }

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const Rcpp::List &eic = fts_eic[j];

      const int n_eic = eic.size();

      if (n_eic > 0)
      {
        has_eic[j] = true;
        continue;
      }

      targets.index.push_back(counter);
      counter++;
      targets.id.push_back(fts_id[j]);
      targets.level.push_back(1);
      targets.polarity.push_back(fts_polarity[j]);
      targets.precursor.push_back(false);
      targets.mz.push_back(fts_mz[j]);
      targets.mzmin.push_back(fts_mzmin[j] - mzExpand);
      targets.mzmax.push_back(fts_mzmax[j] + mzExpand);
      const float &rt_j = fts_rt[j]; 
      float rtmin_j = fts_rtmin[j];
      float rtmax_j = fts_rtmax[j];
      if (rt_j - rtmin_j < minHalfPeakWidth) rtmin_j = rt_j - minHalfPeakWidth;
      if (rtmax_j - rt_j < minHalfPeakWidth) rtmax_j = rt_j + minHalfPeakWidth;
      targets.rt.push_back(rt_j);
      targets.rtmin.push_back(rtmin_j - rtExpand);
      targets.rtmax.push_back(rtmax_j + rtExpand);
      targets.mobilitymin.push_back(0);
      targets.mobilitymax.push_back(0);
    }

    if (targets.id.size() == 0)
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }
    
    const Rcpp::List &hd = headers[i];
    
    const sc::MS_SPECTRA_HEADERS header = NTS::as_MS_SPECTRA_HEADERS(hd);

    const std::string file = analyses_files[i];

    if (!std::filesystem::exists(file))
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    sc::MS_FILE ana(file);

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header, minTracesIntensity, 0);

    for (int j = 0; j < n_features; j++)
    {

      if (has_quality[j])
        continue;

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const std::string &id_j = fts_id[j];
      const float &rt_j = fts_rt[j];
      
      std::vector<float> rt;
      std::vector<float> mz;
      std::vector<float> intensity;
      
      Rcpp::List quality = Rcpp::List::create(
        Rcpp::Named("feature") = id_j,
        Rcpp::Named("noise") = 0,
        Rcpp::Named("sn") = 0,
        Rcpp::Named("gauss_a") = 0,
        Rcpp::Named("gauss_u") = 0,
        Rcpp::Named("gauss_s") = 0,
        Rcpp::Named("gauss_f") = 0);

      if (has_eic[j])
      {
        const Rcpp::List &eic = fts_eic[j];
        const std::vector<std::string> &feature_ref = eic["feature"];
        const std::vector<int> &pol_ref = eic["polarity"];
        const std::vector<int> &level_ref = eic["level"];
        const std::vector<float> &rt_ref = eic["rt"];
        const std::vector<float> &mz_ref = eic["mz"];
        const std::vector<float> &intensity_ref = eic["intensity"];
        rt = rt_ref;
        mz = mz_ref;
        intensity = intensity_ref;
        
        int n = rt.size();
        
        if (n > minNumberTraces)
        {
          quality = NTS::calculate_gaussian_fit(id_j, rt_j, mz, rt, intensity, baseCut, 0);
          
          n = rt.size();
          
          std::vector<std::string> feature_vec(n, id_j);
          std::vector<int> pol_vec(n, pol_ref[0]);
          std::vector<int> level_vec(n, level_ref[0]);
          
          float mzmin = *std::min_element(mz.begin(), mz.end());
          float mzmax = *std::max_element(mz.begin(), mz.end());
          float rtmin = *std::min_element(rt.begin(), rt.end());
          float rtmax = *std::max_element(rt.begin(), rt.end());

          if (mzmin != fts_mzmin[j] || mzmax != fts_mzmax[j] || rtmin != fts_rtmin[j] || rtmax != fts_rtmax[j])
          {
            fts_mzmin[j] = mzmin;
            fts_mzmax[j] = mzmax;
            fts_rtmin[j] = rtmin;
            fts_rtmax[j] = rtmax;
          }
          
          Rcpp::List eic = Rcpp::List::create(
            Rcpp::Named("feature") = feature_vec,
            Rcpp::Named("polarity") = pol_vec,
            Rcpp::Named("level") = level_vec,
            Rcpp::Named("rt") = rt,
            Rcpp::Named("mz") = mz,
            Rcpp::Named("intensity") = intensity);
          
          eic.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
          
          fts_eic[j] = eic;
        }
      }
      else
      {
        sc::MS_TARGETS_SPECTRA res_j = res[id_j];
        rt = res_j.rt;
        mz = res_j.mz;
        intensity = res_j.intensity;
        NTS::merge_traces_within_rt(rt, mz, intensity);
        
        int n = rt.size();
        
        if (n > minNumberTraces)
        {
          quality = NTS::calculate_gaussian_fit(id_j, rt_j, mz, rt, intensity, baseCut, 0);
          n = rt.size();
          
          float mzmin = *std::min_element(mz.begin(), mz.end());
          float mzmax = *std::max_element(mz.begin(), mz.end());
          float rtmin = *std::min_element(rt.begin(), rt.end());
          float rtmax = *std::max_element(rt.begin(), rt.end());
          
          if (mzmin != fts_mzmin[j] || mzmax != fts_mzmax[j] || rtmin != fts_rtmin[j] || rtmax != fts_rtmax[j])
          {
            fts_mzmin[j] = mzmin;
            fts_mzmax[j] = mzmax;
            fts_rtmin[j] = rtmin;
            fts_rtmax[j] = rtmax;
          }
        }
        
        std::vector<std::string> feature_vec(n, id_j);
        std::vector<int> pol_vec(n, res_j.polarity[0]);
        std::vector<int> level_vec(n, res_j.level[0]);
        
        Rcpp::List eic = Rcpp::List::create(
          Rcpp::Named("feature") = feature_vec,
          Rcpp::Named("polarity") = pol_vec,
          Rcpp::Named("level") = level_vec,
          Rcpp::Named("rt") = rt,
          Rcpp::Named("mz") = mz,
          Rcpp::Named("intensity") = intensity);
        eic.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
        fts_eic[j] = eic;
      }

      quality.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      fts_quality[j] = quality;
    }
  
    features_i["mzmin"] = fts_mzmin;
    features_i["mzmax"] = fts_mzmax;
    features_i["rtmin"] = fts_rtmin;
    features_i["rtmax"] = fts_rtmax;
    features_i["eic"] = fts_eic;
    features_i["quality"] = fts_quality;
    features[i] = features_i;
    
    Rcpp::Rcout << "Done!" << std::endl;
  }

  return features;
};
