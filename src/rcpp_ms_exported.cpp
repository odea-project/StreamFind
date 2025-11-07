#include <vector>
#include <string>
#include <Rcpp.h>
#include <unordered_map>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <iomanip>
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

// MARK: rcpp_ms_load_features_eic
// [[Rcpp::export]]
Rcpp::List rcpp_ms_load_features_eic(std::vector<std::string> analyses_names,
                                     std::vector<std::string> analyses_files,
                                     Rcpp::List headers,
                                     Rcpp::List features,
                                     bool filtered,
                                     float rtExpand,
                                     float mzExpand,
                                     float minTracesIntensity)
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

    const std::string file = analyses_files[i];

    if (!std::filesystem::exists(file))
      continue;

    sc::MS_FILE ana(file);

    const Rcpp::List &hd = headers[i];

    const sc::MS_SPECTRA_HEADERS header = MassSpecResults_NonTargetAnalysis::as_MS_SPECTRA_HEADERS(hd);

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header, minTracesIntensity, 0);

    for (int j = 0; j < n_features; j++)
    {

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const std::string &id_j = fts_id[j];
      const int &pol_j = fts_polarity[j];

      sc::MS_TARGETS_SPECTRA res_j = res[id_j];

      int n_res_j = res_j.rt.size();

      if (n_res_j == 0)
        continue;

      // MassSpecResults_NonTargetAnalysis::merge_traces_within_rt(res_j.rt, res_j.mz, res_j.intensity);

      n_res_j = res_j.rt.size();
      const std::vector<std::string> id_vec = std::vector<std::string>(n_res_j, id_j);
      const std::vector<int> pol_vec = std::vector<int>(n_res_j, pol_j);
      const std::vector<int> level_vec = std::vector<int>(n_res_j, pol_j);

      Rcpp::List eic = Rcpp::List::create(
          Rcpp::Named("feature") = id_vec,
          Rcpp::Named("polarity") = pol_vec,
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

// MARK: rcpp_nts_load_features_eic
// [[Rcpp::export]]
Rcpp::List rcpp_nts_load_features_eic(Rcpp::List info,
                                      Rcpp::List spectra_headers,
                                      Rcpp::List feature_list,
                                      bool filtered = false,
                                      float rtExpand = 0,
                                      float mzExpand = 0,
                                      float minTracesIntensity = 0)
{

  MassSpecResults_NonTargetAnalysis::NTS_DATA data(info, spectra_headers, feature_list);

  if (!data.valid)
  {
    Rcpp::Rcout << "Error: Invalid MassSpecResults_NonTargetAnalysis data!" << std::endl;
    return feature_list;
  }

  for (int i = 0; i < data.size(); i++)
  {
    MassSpecResults_NonTargetAnalysis::FEATURES &fts_i = data.features[i];

    if (fts_i.size() == 0)
      continue;

    sc::MS_TARGETS targets;
    int counter = 0;

    for (int j = 0; j < fts_i.size(); j++)
    {
      const MassSpecResults_NonTargetAnalysis::FEATURE &ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.eic.rt.size() == 0 || ft_j.eic.feature == "")
      {
        targets.index.push_back(counter);
        counter++;
        targets.id.push_back(ft_j.feature);
        targets.level.push_back(1);
        targets.polarity.push_back(ft_j.polarity);
        targets.precursor.push_back(false);
        targets.mz.push_back(ft_j.mz);
        targets.mzmin.push_back(ft_j.mzmin - mzExpand);
        targets.mzmax.push_back(ft_j.mzmax + mzExpand);
        targets.rt.push_back(ft_j.rt);
        targets.rtmin.push_back(ft_j.rtmin - rtExpand);
        targets.rtmax.push_back(ft_j.rtmax + rtExpand);
        targets.mobilitymin.push_back(0);
        targets.mobilitymax.push_back(0);
      }
    }

    if (targets.id.size() == 0)
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    Rcpp::Rcout << "Loading EIC for " << targets.id.size() << " features in analyses " << data.analyses[i] << "...";

    const sc::MS_SPECTRA_HEADERS &header_i = data.headers[i];
    const std::string &file_i = data.files[i];

    if (!std::filesystem::exists(file_i))
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    sc::MS_FILE ana(file_i);
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header_i, minTracesIntensity, 0);

    for (int j = 0; j < fts_i.size(); j++)
    {
      MassSpecResults_NonTargetAnalysis::FEATURE ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.eic.rt.size() == 0 || ft_j.eic.feature == "")
      {
        const sc::MS_TARGETS_SPECTRA &res_j = res[ft_j.feature];
        ft_j.eic.import_from_ms_targets_spectra(res_j);
        fts_i.set_feature(j, ft_j);
      }
    }

    data.features[i] = fts_i;

    Rcpp::Rcout << "Done!" << std::endl;
  }

  return data.features_as_list_of_dt();
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

  int rtWindowMax_idx = MassSpecResults_NonTargetAnalysis::find_max_index(rtWindow);
  int rtWindowMin_idx = MassSpecResults_NonTargetAnalysis::find_min_index(rtWindow);
  int mzWindowMax_idx = MassSpecResults_NonTargetAnalysis::find_max_index(mzWindow);
  int mzWindowMin_idx = MassSpecResults_NonTargetAnalysis::find_min_index(mzWindow);

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

    const sc::MS_SPECTRA_HEADERS header = MassSpecResults_NonTargetAnalysis::as_MS_SPECTRA_HEADERS(hd);

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header, minTracesIntensity, 0);

    for (int j = 0; j < n_features; j++)
    {

      if (!filtered)
        if (fts_filtered[j])
          continue;

      const std::string &id_j = fts_id[j];
      const float &mz_j = fts_mz[j];

      sc::MS_TARGETS_SPECTRA res_j = res[id_j];

      const int n_res_j = res_j.rt.size();

      if (n_res_j == 0)
        continue;

      const std::vector<std::string> id_vec = std::vector<std::string>(n_res_j, id_j);
      const std::vector<float> mz_vec = std::vector<float>(n_res_j, mz_j);

      const Rcpp::List ms1 = Rcpp::List::create(
          Rcpp::Named("feature") = id_vec,
          Rcpp::Named("polarity") = res_j.polarity,
          Rcpp::Named("level") = res_j.level,
          Rcpp::Named("pre_mz") = mz_vec,
          Rcpp::Named("pre_ce") = res_j.pre_ce,
          Rcpp::Named("rt") = res_j.rt,
          Rcpp::Named("mz") = res_j.mz,
          Rcpp::Named("intensity") = res_j.intensity);

      Rcpp::List ms1_clustered = MassSpecResults_NonTargetAnalysis::cluster_spectra(ms1, mzClust, presence);

      ms1_clustered.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      fts_ms1[j] = ms1_clustered;
    }

    features_i["ms1"] = fts_ms1;
    features[i] = features_i;
  }

  return features;
}

// MARK: rcpp_ms_load_features_ms1
// [[Rcpp::export]]
Rcpp::List rcpp_nts_load_features_ms1(Rcpp::List info,
                                      Rcpp::List spectra_headers,
                                      Rcpp::List feature_list,
                                      bool filtered,
                                      std::vector<float> rtWindow,
                                      std::vector<float> mzWindow,
                                      float minTracesIntensity,
                                      float mzClust,
                                      float presence)
{
  MassSpecResults_NonTargetAnalysis::NTS_DATA data(info, spectra_headers, feature_list);

  if (!data.valid)
  {
    Rcpp::Rcout << "Error: Invalid MassSpecResults_NonTargetAnalysis data!" << std::endl;
    return feature_list;
  }

  for (int i = 0; i < data.size(); i++)
  {
    MassSpecResults_NonTargetAnalysis::FEATURES &fts_i = data.features[i];

    if (fts_i.size() == 0)
      continue;

    sc::MS_TARGETS targets;
    int counter = 0;

    int rtWindowMax_idx = MassSpecResults_NonTargetAnalysis::find_max_index(rtWindow);
    int rtWindowMin_idx = MassSpecResults_NonTargetAnalysis::find_min_index(rtWindow);
    int mzWindowMax_idx = MassSpecResults_NonTargetAnalysis::find_max_index(mzWindow);
    int mzWindowMin_idx = MassSpecResults_NonTargetAnalysis::find_min_index(mzWindow);

    for (int j = 0; j < fts_i.size(); j++)
    {
      const MassSpecResults_NonTargetAnalysis::FEATURE &ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms1.rt.size() == 0 || ft_j.ms1.feature == "")
      {
        targets.index.push_back(counter);
        counter++;
        targets.id.push_back(ft_j.feature);
        targets.level.push_back(1);
        targets.polarity.push_back(ft_j.polarity);
        targets.precursor.push_back(false);
        targets.mz.push_back(ft_j.mz);
        targets.mzmin.push_back(ft_j.mzmin + mzWindow[mzWindowMin_idx]);
        targets.mzmax.push_back(ft_j.mzmax + mzWindow[mzWindowMax_idx]);
        targets.rt.push_back(ft_j.rt);
        targets.rtmin.push_back(ft_j.rtmin + rtWindow[rtWindowMin_idx]);
        targets.rtmax.push_back(ft_j.rtmax + rtWindow[rtWindowMax_idx]);
        targets.mobilitymin.push_back(0);
        targets.mobilitymax.push_back(0);
      }
    }

    if (targets.id.size() == 0)
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    Rcpp::Rcout << "Loading MS1 for " << targets.id.size() << " features in analyses " << data.analyses[i] << "...";

    const sc::MS_SPECTRA_HEADERS &header_i = data.headers[i];
    const std::string &file_i = data.files[i];

    if (!std::filesystem::exists(file_i))
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    sc::MS_FILE ana(file_i);
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header_i, minTracesIntensity, 0);

    for (int j = 0; j < fts_i.size(); j++)
    {
      MassSpecResults_NonTargetAnalysis::FEATURE ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms1.rt.size() == 0 || ft_j.ms1.feature == "")
      {
        const sc::MS_TARGETS_SPECTRA &res_j = res[ft_j.feature];
        ft_j.ms1.import_from_ms_targets_spectra(res_j);
        ft_j.ms1.set_precursor_mz(ft_j.mz);
        ft_j.ms1.cluster(mzClust, presence);
        fts_i.set_feature(j, ft_j);
      }
    }

    data.features[i] = fts_i;

    Rcpp::Rcout << "Done!" << std::endl;
  }

  return data.features_as_list_of_dt();
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

    const sc::MS_SPECTRA_HEADERS header = MassSpecResults_NonTargetAnalysis::as_MS_SPECTRA_HEADERS(hd);

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

      Rcpp::List ms2_clustered = MassSpecResults_NonTargetAnalysis::cluster_spectra(ms2, mzClust, presence);

      ms2_clustered.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

      fts_ms2[j] = ms2_clustered;
    }

    features_i["ms2"] = fts_ms2;
    features[i] = features_i;
  }

  return features;
}

// MARK: rcpp_ms_load_features_ms2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_load_features_ms2(Rcpp::List info,
                                      Rcpp::List spectra_headers,
                                      Rcpp::List feature_list,
                                      bool filtered,
                                      float minTracesIntensity,
                                      float isolationWindow,
                                      float mzClust,
                                      float presence)
{
  MassSpecResults_NonTargetAnalysis::NTS_DATA data(info, spectra_headers, feature_list);

  if (!data.valid)
  {
    Rcpp::Rcout << "Error: Invalid MassSpecResults_NonTargetAnalysis data!" << std::endl;
    return feature_list;
  }

  for (int i = 0; i < data.size(); i++)
  {
    MassSpecResults_NonTargetAnalysis::FEATURES fts_i = data.features[i];

    if (fts_i.size() == 0)
      continue;

    sc::MS_TARGETS targets;
    int counter = 0;

    for (int j = 0; j < fts_i.size(); j++)
    {
      const MassSpecResults_NonTargetAnalysis::FEATURE &ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms2.feature == "")
      {
        targets.index.push_back(counter);
        counter++;
        targets.id.push_back(ft_j.feature);
        targets.level.push_back(2);
        targets.polarity.push_back(ft_j.polarity);
        targets.precursor.push_back(true);
        targets.mzmin.push_back(ft_j.mzmin - (isolationWindow / 2));
        targets.mzmax.push_back(ft_j.mzmax + (isolationWindow / 2));
        targets.rtmin.push_back(ft_j.rtmin - 1);
        targets.rtmax.push_back(ft_j.rtmax + 1);
        targets.mobilitymin.push_back(0);
        targets.mobilitymax.push_back(0);
      }
    }

    if (targets.id.size() == 0)
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    Rcpp::Rcout << "Loading MS2 for " << targets.id.size() << " features in analyses " << data.analyses[i] << "...";

    const sc::MS_SPECTRA_HEADERS &header_i = data.headers[i];
    const std::string &file_i = data.files[i];

    if (!std::filesystem::exists(file_i))
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    sc::MS_FILE ana(file_i);
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header_i, 0, minTracesIntensity);

    for (int j = 0; j < fts_i.size(); j++)
    {
      MassSpecResults_NonTargetAnalysis::FEATURE ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.ms2.feature == "")
      {
        const sc::MS_TARGETS_SPECTRA &res_j = res[ft_j.feature];
        ft_j.ms2.import_from_ms_targets_spectra(res_j);
        ft_j.ms2.cluster(mzClust, presence);
        fts_i.set_feature(j, ft_j);
      }
    }

    data.features[i] = fts_i;

    Rcpp::Rcout << "Done!" << std::endl;
  }

  return data.features_as_list_of_dt();
}

// MARK: rcpp_nts_calculate_features_quality
// [[Rcpp::export]]
Rcpp::List rcpp_nts_calculate_features_quality(Rcpp::List info,
                                               Rcpp::List spectra_headers,
                                               Rcpp::List feature_list,
                                               bool filtered = false,
                                               float rtExpand = 0,
                                               float mzExpand = 0,
                                               float minPeakWidth = 6,
                                               float maxPeakWidth = 30,
                                               float minTracesIntensity = 0,
                                               float minNumberTraces = 5,
                                               float baseCut = 0)
{

  MassSpecResults_NonTargetAnalysis::NTS_DATA data(info, spectra_headers, feature_list);

  if (!data.valid)
  {
    Rcpp::Rcout << "Error: Invalid MassSpecResults_NonTargetAnalysis data!" << std::endl;
    return feature_list;
  }

  if (minNumberTraces < 5)
    minNumberTraces = 5;
  const float minHalfPeakWidth = minPeakWidth / 2;
  const float maxHalfPeakWidth = maxPeakWidth / 2;

  for (int i = 0; i < data.size(); i++)
  {
    MassSpecResults_NonTargetAnalysis::FEATURES &fts_i = data.features[i];

    if (fts_i.size() == 0)
      continue;

    sc::MS_TARGETS targets;
    int counter = 0;

    for (int j = 0; j < fts_i.size(); j++)
    {
      const MassSpecResults_NonTargetAnalysis::FEATURE &ft_j = fts_i.get_feature(j);

      if (ft_j.quality.feature != "")
        continue;

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.eic.rt.size() == 0 || ft_j.feature == "")
      {
        targets.index.push_back(counter);
        counter++;
        targets.id.push_back(ft_j.feature);
        targets.level.push_back(1);
        targets.polarity.push_back(ft_j.polarity);
        targets.precursor.push_back(false);
        targets.mz.push_back(ft_j.mz);
        targets.mzmin.push_back(ft_j.mzmin - mzExpand);
        targets.mzmax.push_back(ft_j.mzmax + mzExpand);
        const float &rt_j = ft_j.rt;
        float rtmin_j = ft_j.rtmin;
        float rtmax_j = ft_j.rtmax;
        if (rt_j - rtmin_j < minHalfPeakWidth)
          rtmin_j = rt_j - minHalfPeakWidth;
        if (rtmax_j - rt_j < minHalfPeakWidth)
          rtmax_j = rt_j + minHalfPeakWidth;
        targets.rt.push_back(rt_j);
        targets.rtmin.push_back(rtmin_j - rtExpand);
        targets.rtmax.push_back(rtmax_j + rtExpand);
        targets.mobilitymin.push_back(0);
        targets.mobilitymax.push_back(0);
      }
    }

    if (targets.id.size() == 0)
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    Rcpp::Rcout << "Processing " << targets.id.size() << " features in analyses " << data.analyses[i] << "...";

    const sc::MS_SPECTRA_HEADERS &header_i = data.headers[i];
    const std::string &file_i = data.files[i];

    if (!std::filesystem::exists(file_i))
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    sc::MS_FILE ana(file_i);
    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(targets, header_i, minTracesIntensity, 0);

    for (int j = 0; j < fts_i.size(); j++)
    {
      MassSpecResults_NonTargetAnalysis::FEATURE ft_j = fts_i.get_feature(j);

      if (ft_j.filtered && !filtered)
        continue;

      if (ft_j.quality.feature != "")
        continue;

      if (ft_j.eic.rt.size() == 0 || ft_j.feature == "")
      {
        const sc::MS_TARGETS_SPECTRA &res_j = res[ft_j.feature];
        ft_j.eic.import_from_ms_targets_spectra(res_j);
      }

      ft_j.calculate_quality(baseCut, 0, maxHalfPeakWidth);

      if (ft_j.quality.feature != "")
      {
        ft_j.update_properties();
        fts_i.set_feature(j, ft_j);
      }
    }

    data.features[i] = fts_i;

    Rcpp::Rcout << "Done!" << std::endl;
  }

  return data.features_as_list_of_dt();
};

// MARK: rcpp_nts_annotate_features
// [[Rcpp::export]]
Rcpp::List rcpp_nts_annotate_features(Rcpp::List feature_list,
                                      double rtWindowAlignment = 0.3,
                                      int maxIsotopes = 5,
                                      int maxCharge = 1,
                                      int maxGaps = 1)
{
  MassSpecResults_NonTargetAnalysis::ANNOTATION_ISOTOPE_SET isotopes;
  std::vector<std::string> elements = {"C", "H", "N", "O", "S", "Cl", "Br", "Si"};
  isotopes.filter(elements);

  const int max_number_elements = 5;

  Rcpp::Rcout << "Building combinatorial isotopic chains with length " << max_number_elements << "...";
  MassSpecResults_NonTargetAnalysis::ANNOTATION_ISOTOPE_COMBINATIONS combinations(isotopes, max_number_elements);
  Rcpp::Rcout << "Done!" << std::endl;

  const int number_analyses = feature_list.size();

  if (number_analyses == 0)
    return feature_list;

  const std::vector<std::string> analyses_names = Rcpp::as<std::vector<std::string>>(feature_list.names());

  for (int a = 0; a < number_analyses; a++)
  {
    const Rcpp::List &fts_ref = Rcpp::as<Rcpp::List>(feature_list[a]);

    MassSpecResults_NonTargetAnalysis::FEATURES fts;
    fts.import_from_list(analyses_names[a], fts_ref);

    const int number_features = fts.size();

    if (number_features == 0)
      continue;

    fts.sort_by_mz();

    Rcpp::Rcout << "Annotating isotopes in " << number_features << " features...";

    for (int f = 0; f < number_features; f++)
    {
      MassSpecResults_NonTargetAnalysis::FEATURE main_ft = fts.get_feature(f);

      if (main_ft.annotation.iso_cat != "")
        continue;

      MassSpecResults_NonTargetAnalysis::ANNOTATION_CANDIDATE_CHAIN candidates_chain;

      candidates_chain.find_isotopic_candidates(main_ft, fts, f, maxIsotopes, rtWindowAlignment);

      const int number_candidates = candidates_chain.size();

      if (number_candidates > 1)
      {
        // candidates_chain.sort_by_mz();
        candidates_chain.annotate_isotopes(combinations, maxIsotopes, maxCharge, maxGaps);

        // set always monoisotopic ion
        fts.set_feature(candidates_chain.indices[0], candidates_chain.chain[0]);

        for (int c = 1; c < number_candidates; c++)
        {
          if (candidates_chain.chain[c].annotation.feature != "")
          {
            fts.set_feature(candidates_chain.indices[c], candidates_chain.chain[c]);
          }
        }
      }
      else // only monoisotopic ion
      {
        main_ft.annotation.as_only_monoisotopic_ion(main_ft.feature);
        fts.set_feature(f, main_ft);
      }
    }

    Rcpp::Rcout << "Done!" << std::endl;

    Rcpp::Rcout << "Annotating adducts in " << number_features << " features...";

    for (int f = 0; f < number_features; f++)
    {

      MassSpecResults_NonTargetAnalysis::FEATURE main_ft = fts.get_feature(f);

      // already annotated with adduct
      if (main_ft.annotation.adduct_cat != "")
      {
        continue;
      }

      MassSpecResults_NonTargetAnalysis::ANNOTATION_CANDIDATE_CHAIN adduct_candidates_chain;
      adduct_candidates_chain.find_adduct_candidates(main_ft, fts, f, rtWindowAlignment);
      const int number_candidates = adduct_candidates_chain.size();

      if (number_candidates > 1)
      {
        // adduct_candidates_chain.sort_by_mz();
        adduct_candidates_chain.annotate_adducts();

        for (int c = 1; c < number_candidates; c++)
        {
          if (adduct_candidates_chain.chain[c].annotation.adduct_cat != "")
          {
            fts.set_feature(adduct_candidates_chain.indices[c], adduct_candidates_chain.chain[c]);
          }
        }
      }
    }

    Rcpp::Rcout << "Done!" << std::endl;

    feature_list[a] = fts.to_list_dt();
  }

  return feature_list;
};

// MARK: rcpp_nts_fill_features
// [[Rcpp::export]]
Rcpp::List rcpp_nts_fill_features(Rcpp::List info,
                                  Rcpp::List spectra_headers,
                                  Rcpp::List feature_list,
                                  bool withinReplicate = false,
                                  bool filtered = false,
                                  float rtExpand = 0,
                                  float mzExpand = 0,
                                  float minPeakWidth = 6,
                                  float maxPeakWidth = 30,
                                  float minTracesIntensity = 0,
                                  float minNumberTraces = 4,
                                  float minIntensity = 0,
                                  float baseCut = 0,
                                  float maxSearchWindow = 5,
                                  float minSignalToNoiseRatio = 3,
                                  float minGaussianFit = 0.5)
{

  MassSpecResults_NonTargetAnalysis::NTS_DATA data(info, spectra_headers, feature_list);

  if (!data.valid)
  {
    Rcpp::Rcout << "Error: Invalid MassSpecResults_NonTargetAnalysis data! Not filled." << std::endl;
    return feature_list;
  }

  if (minNumberTraces < 5)
    minNumberTraces = 5;
  const float minHalfPeakWidth = minPeakWidth / 2;
  const float maxHalfPeakWidth = maxPeakWidth / 2;

  const int number_analyses = data.size();

  if (number_analyses == 0)
  {
    Rcpp::Rcout << "No analyses found! Not filled." << std::endl;
    return feature_list;
  }

  std::vector<std::vector<int>> analyses_polarities = data.get_polarities();

  int max_presence = number_analyses;

  int number_features = 0;

  for (int i = 0; i < number_analyses; i++)
  {
    const MassSpecResults_NonTargetAnalysis::FEATURES &fts_i = data.features[i];

    if (fts_i.size() == 0)
      continue;

    number_features += fts_i.size();
  }

  std::vector<std::string> long_vector_analysis;
  std::vector<std::string> long_vector_replicate;
  std::vector<std::string> long_vector_feature;
  std::vector<std::string> long_vector_group;
  std::vector<bool> long_vector_filtered;
  std::vector<float> long_vector_mass;
  std::vector<float> long_vector_mz;
  std::vector<float> long_vector_mzmin;
  std::vector<float> long_vector_mzmax;
  std::vector<float> long_vector_rt;
  std::vector<float> long_vector_rtmin;
  std::vector<float> long_vector_rtmax;
  std::vector<float> long_vector_intensity;
  std::vector<bool> long_vector_process;
  std::vector<std::string> long_vector_id;

  for (int i = 0; i < number_analyses; i++)
  {
    const MassSpecResults_NonTargetAnalysis::FEATURES &fts_i = data.features[i];

    if (fts_i.size() == 0)
      continue;

    for (int j = 0; j < fts_i.size(); j++)
    {
      const MassSpecResults_NonTargetAnalysis::FEATURE &ft_j = fts_i.get_feature(j);

      long_vector_analysis.push_back(data.analyses[i]);
      long_vector_replicate.push_back(data.replicates[i]);

      long_vector_feature.push_back(ft_j.feature);
      long_vector_group.push_back(ft_j.group);
      long_vector_filtered.push_back(ft_j.filtered);
      long_vector_mass.push_back(ft_j.mass);
      long_vector_mz.push_back(ft_j.mz);
      long_vector_mzmin.push_back(ft_j.mzmin);
      long_vector_mzmax.push_back(ft_j.mzmax);
      long_vector_rt.push_back(ft_j.rt);
      long_vector_rtmin.push_back(ft_j.rtmin);
      long_vector_rtmax.push_back(ft_j.rtmax);
      long_vector_intensity.push_back(ft_j.intensity);

      if (ft_j.filtered && filtered)
      {
        if (ft_j.group == "")
        {
          long_vector_process.push_back(false);
        }
        else
        {
          long_vector_process.push_back(true);
        }
      }
      else
      {
        if (ft_j.group == "")
        {
          long_vector_process.push_back(false);
        }
        else
        {
          if (ft_j.filtered)
          {
            long_vector_process.push_back(false);
          }
          else
          {
            long_vector_process.push_back(true);
          }
        }
      }
      long_vector_id.push_back(ft_j.group);
    }
  }

  if (withinReplicate)
  {
    std::unordered_map<std::string, int> rpl_sizes;

    for (int i = 0; i < number_analyses; i++)
    {
      std::string rpl = data.replicates[i];
      rpl_sizes[rpl]++;
    }

    max_presence = 0;
    for (const auto &rpl : rpl_sizes)
    {
      if (rpl.second > max_presence)
        max_presence = rpl.second;
    }

    for (int i = 0; i < number_features; i++)
    {
      if (long_vector_process[i])
      {
        long_vector_id[i] = long_vector_id[i] + "_" + long_vector_replicate[i];
      }
    }
  }

  if (max_presence == 0)
  {
    Rcpp::Rcout << "Maximum presence is 0! Not filled." << std::endl;
    return feature_list;
  }

  std::unordered_map<std::string, int> ids_presence;
  for (int i = 0; i < number_features; i++)
  {
    if (long_vector_process[i])
    {
      ids_presence[long_vector_id[i]]++;
    }
  }

  std::vector<std::string> ids_to_fill;

  for (int i = 0; i < number_features; i++)
  {
    if (long_vector_process[i])
    {
      if (ids_presence[long_vector_id[i]] < max_presence)
      {
        ids_to_fill.push_back(long_vector_id[i]);
      }
    }
  }

  if (ids_to_fill.empty())
  {
    Rcpp::Rcout << "No features to fill! Not filled." << std::endl;
    return feature_list;
  }

  std::set<std::string> ids_set(ids_to_fill.begin(), ids_to_fill.end());
  std::vector<std::string> ids(ids_set.begin(), ids_set.end());
  const int number_ids = ids.size();

  std::vector<sc::MS_TARGETS> ana_targets(number_analyses);
  std::vector<std::vector<std::string>> ana_targets_groups(number_analyses);
  std::vector<std::vector<std::string>> ana_targets_replicates(number_analyses);
  std::vector<std::vector<float>> ana_targets_sd_mass(number_analyses);
  std::vector<std::vector<float>> ana_targets_sd_rt(number_analyses);

  Rcpp::Rcout << "Building targets for filling " << number_ids << " feature groups...";

  for (int k = 0; k < number_ids; k++)
  {

    std::vector<int> indices;
    for (int i = 0; i < number_features; i++)
    {
      if (long_vector_id[i] == ids[k])
      {
        indices.push_back(i);
      }
    }

    if (indices.size() == 0)
      continue;

    std::vector<std::string> fts_analysis_k(indices.size());
    std::vector<std::string> fts_replicate_k(indices.size());
    std::vector<std::string> fts_group_k(indices.size());
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
      fts_analysis_k[i] = long_vector_analysis[indices[i]];
      fts_replicate_k[i] = long_vector_replicate[indices[i]];
      fts_group_k[i] = long_vector_group[indices[i]];
      fts_mass_k[i] = long_vector_mass[indices[i]];

      const float &mz_k = long_vector_mz[indices[i]];
      float mzmin_k = long_vector_mzmin[indices[i]];
      float mzmax_k = long_vector_mzmax[indices[i]];
      fts_mz_k[i] = mz_k;
      fts_mzmin_k[i] = mzmin_k;
      fts_mzmax_k[i] = mzmax_k;
      fts_mzmin_side_k[i] = mz_k - mzmin_k;
      fts_mzmax_side_k[i] = mzmax_k - mz_k;

      const float &rt_k = long_vector_rt[indices[i]];
      float rtmin_k = long_vector_rtmin[indices[i]];
      float rtmax_k = long_vector_rtmax[indices[i]];
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
      fts_intensity_k[i] = long_vector_intensity[indices[i]];
    }

    const float mean_mass = std::accumulate(fts_mass_k.begin(), fts_mass_k.end(), 0.0) / fts_mass_k.size();
    const float mean_rt = std::accumulate(fts_rt_k.begin(), fts_rt_k.end(), 0.0) / fts_rt_k.size();
    const float max_mzmin_side = *std::max_element(fts_mzmin_side_k.begin(), fts_mzmin_side_k.end());
    const float max_mzmax_side = *std::max_element(fts_mzmax_side_k.begin(), fts_mzmax_side_k.end());
    const float min_rtmin = *std::min_element(fts_rtmin_k.begin(), fts_rtmin_k.end());
    const float max_rtmax = *std::max_element(fts_rtmax_k.begin(), fts_rtmax_k.end());

    const float sd_mass = MassSpecResults_NonTargetAnalysis::standard_deviation(fts_mass_k, mean_mass);
    const float sd_rt = MassSpecResults_NonTargetAnalysis::standard_deviation(fts_rt_k, mean_rt);

    for (int j = 0; j < number_analyses; j++)
    {

      if (withinReplicate)
      {
        const std::string &rpl = data.replicates[j];

        // continues if analysis replicate j is not within the same replicate as the feature group
        if (fts_replicate_k[0] != rpl)
          continue;
      }

      bool has_analysis = false;

      for (size_t i = 0; i < indices.size(); i++)
      {
        if (fts_analysis_k[i] == data.analyses[j])
        {
          has_analysis = true;
        }
      }

      if (has_analysis)
        continue;

      std::vector<int> pols = analyses_polarities[j];

      for (size_t p = 0; p < pols.size(); p++)
      {
        float mz = mean_mass + (pols[p] * 1.007276);
        float mzmin = mz - max_mzmin_side - mzExpand;
        float mzmax = mz + max_mzmax_side + mzExpand;

        const int n_targets = ana_targets[j].index.size();
        ana_targets[j].index.push_back(n_targets);
        ana_targets[j].id.push_back(ids[k]);
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
        ana_targets_replicates[j].push_back(fts_replicate_k[0]);
        ana_targets_sd_mass[j].push_back(sd_mass);
        ana_targets_sd_rt[j].push_back(sd_rt);
      }
    }
  }

  Rcpp::Rcout << "Done!" << std::endl;

  for (int j = 0; j < number_analyses; j++)
  {

    int n_j_targets = ana_targets[j].id.size();

    if (n_j_targets == 0)
    {
      continue;
    }

    Rcpp::Rcout << "Processing analyses " << data.analyses[j] << ":" << std::endl;

    Rcpp::Rcout << "    Checking if there are already corresponding features" << "...";

    for (int i = n_j_targets - 1; i >= 0; --i)
    {
      const int &polarity_i = ana_targets[j].polarity[i];
      const float &mzmin_i = ana_targets[j].mzmin[i];
      const float &mzmax_i = ana_targets[j].mzmax[i];
      const float &rt_i = ana_targets[j].rt[i];
      const float &rtmin_i = ana_targets[j].rtmin[i];
      const float &rtmax_i = ana_targets[j].rtmax[i];
      float sd_rt_i = ana_targets_sd_rt[j][i];
      if (sd_rt_i == 0)
        sd_rt_i = (rtmax_i - rtmin_i) / 4;
      const float sd_rt_min = rt_i - (sd_rt_i * 1.5);
      const float sd_rt_max = rt_i + (sd_rt_i * 1.5);

      bool has_feature = false;

      const int number_features_j = data.features[j].size();

      MassSpecResults_NonTargetAnalysis::FEATURES &fts_j = data.features[j];

      for (int k = 0; k < number_features_j; k++)
      {
        if (fts_j.filtered[k] || fts_j.group[k] == "")
        {
          if (fts_j.polarity[k] == polarity_i)
          {
            if (fts_j.mz[k] > mzmin_i && fts_j.mz[k] < mzmax_i)
            {
              if (fts_j.rt[k] > sd_rt_min && fts_j.rt[k] < sd_rt_max)
              {
                if (fts_j.quality[k].feature != "")
                {
                  if (fts_j.quality[k].sn < minSignalToNoiseRatio)
                  {
                    continue;
                  }
                  if (fts_j.quality[k].gauss_f < minGaussianFit)
                  {
                    continue;
                  }
                }
                fts_j.group[k] = ana_targets_groups[j][i];
                fts_j.filtered[k] = false;
                fts_j.filter[k] = fts_j.filter[k] + " recovered";
                has_feature = true;
                break;
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
      continue;
    }

    Rcpp::Rcout << "    Extracting " << n_j_targets << " EICs...";

    const sc::MS_SPECTRA_HEADERS &header_j = data.headers[j];
    const std::string &file_j = data.files[j];

    if (!std::filesystem::exists(file_j))
    {
      Rcpp::Rcout << "Done!" << std::endl;
      continue;
    }

    sc::MS_FILE ana(file_j);

    sc::MS_TARGETS_SPECTRA res = ana.get_spectra_targets(ana_targets[j], header_j, minTracesIntensity, 0);

    const int n_traces = res.id.size();

    Rcpp::Rcout << " Done! " << std::endl;

    if (n_traces == 0)
    {
      continue;
    }

    Rcpp::Rcout << "    Filtering " << n_traces << " spectra traces...";

    for (int i = n_j_targets - 1; i >= 0; --i)
    {

      const std::string &id_i = ana_targets[j].id[i];

      // if (id_i == "M326_R957_2643") {
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
      continue;
    }

    Rcpp::Rcout << "    Filling " << n_j_targets << " features...";

    const std::vector<std::string> tg_id = ana_targets[j].id;
    const std::vector<float> tg_rt = ana_targets[j].rt;
    const std::vector<float> tg_mz = ana_targets[j].mz;

    int filled_counter = 0;

    for (int i = 0; i < n_j_targets; i++)
    {
      const std::string &id_i = tg_id[i];
      sc::MS_TARGETS_SPECTRA res_i = res[id_i];
      const int n_res_i = res_i.id.size();

      if (n_res_i < minNumberTraces)
      {
        continue;
      }

      MassSpecResults_NonTargetAnalysis::FEATURE ft;
      ft.feature = id_i;
      ft.rt = tg_rt[i];
      ft.eic.import_from_ms_targets_spectra(res_i);
      ft.calculate_quality(baseCut, maxSearchWindow, maxHalfPeakWidth);

      if (ft.quality.sn < minSignalToNoiseRatio)
      {
        continue;
      }

      if (ft.quality.gauss_f < minGaussianFit)
      {
        continue;
      }

      ft.update_properties();

      ft.mz = ana_targets[j].mz[i];
      ft.polarity = ana_targets[j].polarity[i];
      ft.adduct = "[M+H]+";
      if (ft.polarity < 0)
        ft.adduct = "[M-H]-";

      ft.mass = ft.mz - (ft.polarity * 1.007276);

      ft.filtered = false;
      ft.filter = "";
      ft.filled = true;
      ft.correction = 1;

      ft.group = ana_targets_groups[j][i];

      ft.feature = "FL" + std::to_string(i + 1);
      ft.feature += "_MZ" + std::to_string(static_cast<int>(std::round(ft.mz)));
      ft.feature += "_RT" + std::to_string(static_cast<int>(std::round(ft.rt)));

      ft.eic.feature = ft.feature;
      ft.quality.feature = ft.feature;

      data.features[j].append_feature(ft);

      filled_counter++;
    }

    Rcpp::Rcout << " Done! " << std::endl;

    Rcpp::Rcout << "    Filled " << filled_counter << " features!" << std::endl;
  }

  return data.features_as_list_of_dt();
};

// Helper function for linear resolution calculation across entire m/z range
// Creates a linear relationship: mzThreshold = mz / resolution
// where resolution = slope * mz + intercept
inline float calculate_mz_threshold_linear(float mz, float slope, float intercept) {
  const float resolution = slope * mz + intercept;
  // Add safety check to avoid division by very small resolutions
  return mz / std::max(resolution, 1.0f);
}

// Calculate linear model parameters from resolution profile
// resolution_profile[0] = resolution at 100 Da (or reference point)
// resolution_profile[1] = resolution at 400 Da
// resolution_profile[2] = resolution at 1000 Da
inline std::pair<float, float> calculate_linear_model_params(const std::vector<int>& resolution_profile) {
  // Use three points to fit linear model: (100, res[0]), (400, res[1]), (1000, res[2])
  // For better fit, use least squares with the three points
  const float mz1 = 100.0f, mz2 = 400.0f, mz3 = 1000.0f;
  const float res1 = static_cast<float>(resolution_profile[0]);
  const float res2 = static_cast<float>(resolution_profile[1]);
  const float res3 = static_cast<float>(resolution_profile[2]);

  // Linear least squares fit: y = ax + b where y=resolution, x=mz
  const float n = 3.0f;
  const float sum_x = mz1 + mz2 + mz3;
  const float sum_y = res1 + res2 + res3;
  const float sum_xx = mz1*mz1 + mz2*mz2 + mz3*mz3;
  const float sum_xy = mz1*res1 + mz2*res2 + mz3*res3;

  const float slope = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x);
  const float intercept = (sum_y - slope * sum_x) / n;

  return std::make_pair(slope, intercept);
}

// MARK: rcpp_nts_find_features
// [[Rcpp::export]]
Rcpp::List rcpp_nts_find_features(Rcpp::List info,
                                  Rcpp::List spectra_headers,
                                  std::vector<float> rtWindowsMin,
                                  std::vector<float> rtWindowsMax,
                                  std::vector<int> resolution_profile,
                                  float noiseThreshold = 15.0,
                                  float minSNR = 3.0,
                                  int minTraces = 3,
                                  float baselineWindow = 200.0,
                                  float maxWidth = 100.0) {
  std::vector<std::string> analyses = Rcpp::as<std::vector<std::string>>(info["analysis"]);
  std::vector<std::string> analyses_files = Rcpp::as<std::vector<std::string>>(info["file"]);
  Rcpp::List feature_list(analyses.size());
  feature_list.names() = analyses;
  if (resolution_profile.size() != 3) {
    Rcpp::Rcout << "Error: resolution_profile must have exactly 3 elements correspondent to 100, 400, and 1000 Da correspondent resolutions!" << std::endl;
    return feature_list;
  }
  if (rtWindowsMin.size() != rtWindowsMax.size()) {
    Rcpp::Rcout << "Error: rtWindowsMin and rtWindowsMax must have the same length!" << std::endl;
    return feature_list;
  }

  // Calculate linear model parameters for resolution once
  const auto [slope, intercept] = calculate_linear_model_params(resolution_profile);
  Rcpp::Rcout << std::endl;
  Rcpp::Rcout << "Linear resolution model threshold = " << slope << " * m/z + " << intercept << std::endl;
  Rcpp::Rcout << "Reference thresholds: " << std::endl;
  for (float test_mz : {100.0f, 400.0f, 1000.0f}) {
    float mzThreshold = calculate_mz_threshold_linear(test_mz, slope, intercept);
    Rcpp::Rcout << "  m/z " << test_mz << " -> threshold " << mzThreshold << std::endl;
  }

  // Debug cluster to track in detail
  const int debug_cluster = 439;
  const bool debug = false;

  const int total_analyses = analyses.size();

  for (size_t a = 0; a < analyses.size(); ++a) {
    Rcpp::Rcout << std::endl;
    Rcpp::Rcout << a + 1 << "/" << total_analyses << " Processing analysis " << analyses[a] << std::endl;
    const Rcpp::List &header_ref = Rcpp::as<Rcpp::List>(spectra_headers[analyses[a]]);
    sc::MS_SPECTRA_HEADERS header = MassSpecResults_NonTargetAnalysis::as_MS_SPECTRA_HEADERS(header_ref);
    std::vector<int> idx_load;
    std::vector<float> rt_load;
    if (rtWindowsMin.size() > 0) {
      const std::vector<float> &rts = header.rt;
      for (size_t w = 0; w < rtWindowsMin.size(); ++w) {
        for (size_t i = 0; i < rts.size(); ++i) {
          if (rts[i] >= rtWindowsMin[w] && rts[i] <= rtWindowsMax[w]) {
            if (header.level[i] == 1) {
              idx_load.push_back(header.index[i]);
              rt_load.push_back(header.rt[i]);
            }
          }
        }
      }
    } else {
      idx_load = header.index;;
      rt_load = header.rt;
    }
    std::string file_path = analyses_files[a];
    sc::MS_FILE ana(file_path);
    std::vector<float> spec_rt, spec_mz, spec_intensity, spec_noise;

    // MARK: Spectra denoising
    Rcpp::Rcout << "  1/5 Denoising " << idx_load.size() << " spectra" << std::endl;

    // Track denoising statistics
    size_t total_raw_points = 0;
    size_t total_clean_points = 0;

    for (size_t i = 0; i < idx_load.size(); ++i) {
      const float &rt = rt_load[i];
      const int spectrum_idx = idx_load[i];
      std::vector<std::vector<std::vector<float>>> single_spectrum = ana.get_spectra({spectrum_idx});
      std::vector<float>& raw_mz = single_spectrum[0][0];
      std::vector<float>& raw_intensity = single_spectrum[0][1];
      const int raw_n_traces = raw_mz.size();
      if (raw_n_traces < minTraces) continue;
      
      // Count raw data points for denoising statistics
      total_raw_points += raw_n_traces;
      
      std::vector<float> raw_noise(raw_n_traces);

      int auto_noiseBins;
      float auto_noiseQuantile;
      std::vector<float> intensity_copy = raw_intensity;
      std::sort(intensity_copy.begin(), intensity_copy.end());
      auto_noiseBins = std::max(10, std::min(200, static_cast<int>(std::sqrt(raw_n_traces) * 1.5)));

      // Calculate intensity statistics
      float q25 = intensity_copy[static_cast<int>(raw_n_traces * 0.25)];
      // float q50 = intensity_copy[static_cast<int>(raw_n_traces * 0.50)]; // median
      // float q75 = intensity_copy[static_cast<int>(raw_n_traces * 0.75)];
      float q90 = intensity_copy[static_cast<int>(raw_n_traces * 0.90)];
      // float q95 = intensity_copy[static_cast<int>(raw_n_traces * 0.95)];

      // Calculate interquartile range and coefficient of variation
      float mean_intensity = std::accumulate(raw_intensity.begin(), raw_intensity.end(), 0.0f) / raw_n_traces;
      float variance = 0.0f;
      for (float val : raw_intensity) {
        variance += (val - mean_intensity) * (val - mean_intensity);
      }
      variance /= raw_n_traces;
      float cv = std::sqrt(variance) / mean_intensity; // coefficient of variation

      // Adaptive quantile estimation based on data characteristics
      if (cv > 2.0f) {
        // High variability - use moderate quantile to balance noise removal vs signal preservation
        auto_noiseQuantile = 0.05f;  // 5% - captures lower noise floor in variable data
      } else if (cv > 1.0f) {
        // Medium variability - standard MS denoising approach
        auto_noiseQuantile = 0.10f;  // 10% - typical for MS data noise removal
      } else {
        // Low variability - more aggressive denoising possible
        auto_noiseQuantile = 0.20f;  // 20% - can remove more noise when data is consistent
      }

      // Adjust quantile based on signal-to-noise characteristics
      float signal_noise_ratio = q90 / q25;
      if (signal_noise_ratio > 100) {
        // High dynamic range - more conservative noise estimation
        auto_noiseQuantile *= 0.5f;
      } else if (signal_noise_ratio < 10) {
        // Low dynamic range - less conservative
        auto_noiseQuantile *= 1.5f;
      }

      // Ensure quantile is within reasonable bounds
      auto_noiseQuantile = std::max(0.01f, std::min(0.30f, auto_noiseQuantile));

      // Adjust number of bins based on data sparsity
      float data_sparsity = static_cast<float>(raw_n_traces) / auto_noiseBins;
      if (data_sparsity < 5) {
        // Too few points per bin - reduce number of bins
        auto_noiseBins = std::max(5, raw_n_traces / 5);
      } else if (data_sparsity > 50) {
        // Too many points per bin - increase number of bins
        auto_noiseBins = std::min(200, raw_n_traces / 20);
      }

      if (i == 0 && debug) {
        Rcpp::Rcout << "DEBUG Auto noise estimation: bins=" << auto_noiseBins
                    << ", quantile = " << auto_noiseQuantile
                    << " (CV = " << cv << ", SNR = " << signal_noise_ratio
                    << ", n = " << raw_n_traces << ")" << std::endl;
      }

      // Create bins for local quantile calculation
      std::vector<int> bins(raw_n_traces);
      for (int j = 0; j < raw_n_traces; ++j) {
        bins[j] = std::min(static_cast<int>(j * auto_noiseBins / raw_n_traces), auto_noiseBins - 1);
      }

      // Calculate quantiles for each bin
      std::vector<float> bin_quantiles(auto_noiseBins, noiseThreshold);
      for (int bin = 0; bin < auto_noiseBins; ++bin) {
        std::vector<float> bin_intensities;
        for (int j = 0; j < raw_n_traces; ++j) {
          if (bins[j] == bin) {
            bin_intensities.push_back(raw_intensity[j]);
          }
        }
        if (!bin_intensities.empty()) {
          std::sort(bin_intensities.begin(), bin_intensities.end());
          int quantile_idx = std::max(0, std::min(static_cast<int>(bin_intensities.size()) - 1,
                                                  static_cast<int>(bin_intensities.size() * auto_noiseQuantile)));
          bin_quantiles[bin] = std::max(bin_intensities[quantile_idx], noiseThreshold);
        }
      }

      // Assign noise levels
      for (int j = 0; j < raw_n_traces; ++j) {
        raw_noise[j] = bin_quantiles[bins[j]];
      }


      std::vector<float> clean_mz, clean_intensity, clean_noise;
      clean_mz.reserve(raw_n_traces);
      clean_intensity.reserve(raw_n_traces);
      clean_noise.reserve(raw_n_traces);

      for (int j = 0; j < raw_n_traces; ++j) {
        if (raw_intensity[j] > raw_noise[j]) {
          clean_mz.push_back(raw_mz[j]);
          clean_intensity.push_back(raw_intensity[j]);
          clean_noise.push_back(raw_noise[j]);
        }
      }

      if (clean_mz.empty()) continue;

      // Sort by mz - optimized version
      const size_t clean_size = clean_mz.size();
      
      // Count clean data points for denoising statistics
      total_clean_points += clean_size;
      
      std::vector<size_t> indices(clean_size);
      std::iota(indices.begin(), indices.end(), 0);
      std::sort(indices.begin(), indices.end(), [&](size_t a, size_t b) {
        return clean_mz[a] < clean_mz[b];
      });

      // Apply sorting - optimized version
      std::vector<float> clean_sorted_mz, clean_sorted_intensity, clean_sorted_noise;
      clean_sorted_mz.reserve(clean_size);
      clean_sorted_intensity.reserve(clean_size);
      clean_sorted_noise.reserve(clean_size);

      for (size_t idx : indices) {
        clean_sorted_mz.push_back(clean_mz[idx]);
        clean_sorted_intensity.push_back(clean_intensity[idx]);
        clean_sorted_noise.push_back(clean_noise[idx]);
      }

      std::vector<int> cent_clusters;
      cent_clusters.reserve(clean_sorted_mz.size());

      if (clean_sorted_mz.size() > 0) {
        std::vector<float> mz_diffs;
        std::vector<float> mz_thresholds;
        mz_diffs.reserve(clean_sorted_mz.size() - 1);
        mz_thresholds.reserve(clean_sorted_mz.size() - 1);

        for (size_t j = 1; j < clean_sorted_mz.size(); ++j) {
          mz_diffs.push_back(clean_sorted_mz[j] - clean_sorted_mz[j-1]);
          mz_thresholds.push_back(calculate_mz_threshold_linear(clean_sorted_mz[j], slope, intercept));
        }

        cent_clusters.push_back(0); // First element always cluster 0
        int current_cluster = 0;

        std::transform(mz_diffs.begin(), mz_diffs.end(), mz_thresholds.begin(),
                      std::back_inserter(cent_clusters),
                      [&current_cluster](float diff, float threshold) {
                        if (diff > threshold) {
                          current_cluster++;
                        }
                        return current_cluster;
                      });
      }

      // Aggregate by cluster (keep max intensity peak per cluster)
      std::map<int, std::tuple<float, float, float>> cent_cluster_data; // cluster -> (mz, intensity, noise)

      for (size_t j = 0; j < clean_sorted_mz.size(); ++j) {
        int cluster = cent_clusters[j];
        if (cent_cluster_data.find(cluster) == cent_cluster_data.end()) {
          cent_cluster_data[cluster] = std::make_tuple(clean_sorted_mz[j], clean_sorted_intensity[j], clean_sorted_noise[j]);
        } else {
          if (clean_sorted_intensity[j] > std::get<1>(cent_cluster_data[cluster])) {
            cent_cluster_data[cluster] = std::make_tuple(clean_sorted_mz[j], clean_sorted_intensity[j], clean_sorted_noise[j]);
          }
        }
      }

      for (const auto& [cluster, data_tuple] : cent_cluster_data) {
        spec_rt.push_back(rt);
        spec_mz.push_back(std::get<0>(data_tuple));
        spec_intensity.push_back(std::get<1>(data_tuple));
        spec_noise.push_back(std::get<2>(data_tuple));
      }

      // Clear single spectrum data to free memory immediately
      single_spectrum.clear();
    }

    const int clean_n_traces = spec_rt.size();
    if (clean_n_traces == 0) {
      Rcpp::List empty_list;
      empty_list.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
      feature_list[analyses[a]] = empty_list;
      continue;
    }

    // MARK: Mass clustering
    Rcpp::Rcout << "  2/5 Clustering " << clean_n_traces << " denoised traces by m/z" << std::endl;

    // Sort by mz
    std::vector<size_t> clust_sort_indices(clean_n_traces);
    std::iota(clust_sort_indices.begin(), clust_sort_indices.end(), 0);
    std::sort(clust_sort_indices.begin(), clust_sort_indices.end(), [&](size_t a, size_t b) {
      return spec_mz[a] < spec_mz[b];
    });

    // Calculate mz differences and assign clusters - optimized vectorized approach
    std::vector<int> clust_clusters(clean_n_traces);

    if (clean_n_traces > 0) {
      // Create sorted mz values for easier processing
      std::vector<float> sorted_mz(clean_n_traces);
      for (int i = 0; i < clean_n_traces; ++i) {
        sorted_mz[i] = spec_mz[clust_sort_indices[i]];
      }

      // Calculate all differences and thresholds at once
      std::vector<float> mz_diffs;
      std::vector<float> mz_thresholds;
      mz_diffs.reserve(clean_n_traces - 1);
      mz_thresholds.reserve(clean_n_traces - 1);

      for (int i = 1; i < clean_n_traces; ++i) {
        mz_diffs.push_back(sorted_mz[i] - sorted_mz[i-1]);
        mz_thresholds.push_back(calculate_mz_threshold_linear(sorted_mz[i], slope, intercept));
      }

      // Vectorized cluster assignment
      std::vector<int> sorted_clusters(clean_n_traces);
      sorted_clusters[0] = 0; // First element always cluster 0
      int current_cluster = 0;

      for (size_t i = 0; i < mz_diffs.size(); ++i) {
        if (mz_diffs[i] > mz_thresholds[i]) {
          current_cluster++;
        }
        sorted_clusters[i + 1] = current_cluster;
      }

      // Map back to original indices
      for (int i = 0; i < clean_n_traces; ++i) {
        clust_clusters[clust_sort_indices[i]] = sorted_clusters[i];
      }
    }

    // Make a cluster map for size, max and min intensity
    std::map<int, std::tuple<int, float, float>> clust_cluster_map;
    for (int i = 0; i < clean_n_traces; ++i) {
      const int cluster = clust_clusters[i];
      const float intensity = spec_intensity[i];
      if (clust_cluster_map.find(cluster) == clust_cluster_map.end()) {
        clust_cluster_map[cluster] = std::make_tuple(1, intensity, intensity);
      } else {
        std::get<0>(clust_cluster_map[cluster])++;
        std::get<1>(clust_cluster_map[cluster]) = std::max(std::get<1>(clust_cluster_map[cluster]), intensity);
        std::get<2>(clust_cluster_map[cluster]) = std::min(std::get<2>(clust_cluster_map[cluster]), intensity);
      }
    }

    std::set<int> valid_clusters;
    for (const auto& [cluster, data] : clust_cluster_map) {
      if (std::get<0>(data) > minTraces) {
        float exp_snr = std::get<1>(data) / std::get<2>(data);
        if (exp_snr > minSNR) {
          valid_clusters.insert(cluster);
        }
      }
    }

    // Collect final data - optimized version
    std::vector<float> clust_rt, clust_mz, clust_intensity, clust_noise;
    std::vector<int> clust_cluster;

    // Reserve space based on estimated valid clusters
    const size_t estimated_valid = clean_n_traces / 2; // Conservative estimate
    clust_rt.reserve(estimated_valid);
    clust_mz.reserve(estimated_valid);
    clust_intensity.reserve(estimated_valid);
    clust_noise.reserve(estimated_valid);
    clust_cluster.reserve(estimated_valid);

    for (int i = 0; i < clean_n_traces; ++i) {
      const int cluster = clust_clusters[i];
      if (valid_clusters.find(cluster) != valid_clusters.end()) {
        clust_rt.push_back(spec_rt[i]);
        clust_mz.push_back(spec_mz[i]);
        clust_intensity.push_back(spec_intensity[i]);
        clust_noise.push_back(spec_noise[i]);
        clust_cluster.push_back(cluster);
      }
    }

    const size_t final_size = clust_rt.size();
    std::vector<size_t> clust_rt_indices(final_size);
    std::iota(clust_rt_indices.begin(), clust_rt_indices.end(), 0);
    std::sort(clust_rt_indices.begin(), clust_rt_indices.end(), [&](size_t a, size_t b) {
      return clust_rt[a] < clust_rt[b];
    });

    std::vector<float> clust_rt_sorted_rt, clust_rt_sorted_mz, clust_rt_sorted_intensity, clust_rt_sorted_noise;
    std::vector<int> clust_rt_sorted_cluster;

    clust_rt_sorted_rt.reserve(final_size);
    clust_rt_sorted_mz.reserve(final_size);
    clust_rt_sorted_intensity.reserve(final_size);
    clust_rt_sorted_noise.reserve(final_size);
    clust_rt_sorted_cluster.reserve(final_size);

    for (size_t idx : clust_rt_indices) {
      clust_rt_sorted_rt.push_back(clust_rt[idx]);
      clust_rt_sorted_mz.push_back(clust_mz[idx]);
      clust_rt_sorted_intensity.push_back(clust_intensity[idx]);
      clust_rt_sorted_noise.push_back(clust_noise[idx]);
      clust_rt_sorted_cluster.push_back(clust_cluster[idx]);
    }

    // Create output list with raw spectra data
    // Rcpp::List spectra_list;
    // spectra_list["rt"] = clust_rt_sorted_rt;
    // spectra_list["mz"] = clust_rt_sorted_mz;
    // spectra_list["intensity"] = clust_rt_sorted_intensity;
    // spectra_list["noise"] = clust_rt_sorted_noise;
    // spectra_list["cluster"] = clust_rt_sorted_cluster;
    // spectra_list.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    // feature_list[analyses[a]] = spectra_list;
    // return feature_list;

    // MARK: Peak detection
    Rcpp::Rcout << "  3/5 Detecting peaks in " << valid_clusters.size() << " valid m/z clusters" << std::endl;
    
    // Create a map to group data by cluster
    std::map<int, std::vector<int>> cluster_indices;
    for (size_t i = 0; i < clust_rt_sorted_cluster.size(); ++i) {
      cluster_indices[clust_rt_sorted_cluster[i]].push_back(i);
    }



    // Peak detection data structures
    std::vector<float> peaks_rt, peaks_mz, peaks_intensity, peaks_noise, peaks_sn;
    std::vector<float> peaks_rtmin, peaks_rtmax, peaks_mzmin, peaks_mzmax;
    std::vector<float> peaks_width, peaks_ppm;
    std::vector<float> peaks_fwhm_rt, peaks_fwhm_mz;  // FWHM in RT and MZ dimensions
    std::vector<float> peaks_area;  // Peak area using trapezoidal integration
    std::vector<int> peaks_cluster, peaks_n_traces;
    std::vector<std::string> peaks_id;

    // Peak profile data as native C++ vectors
    std::vector<std::vector<float>> peaks_profile_rt, peaks_profile_raw_intensity;
    std::vector<std::vector<float>> peaks_profile_baseline, peaks_profile_smoothed_intensity;
    std::vector<std::vector<float>> peaks_fitted_intensity;
    std::vector<std::vector<float>> peaks_fitted_rt;

    int total_clusters_processed = 0;
    int clusters_too_small = 0;

    // Process each cluster sequentially to handle complex overlap merging
    for (const auto& [cluster_id, indices] : cluster_indices) {
      total_clusters_processed++;

      if (indices.size() < static_cast<size_t>(minTraces)) {
        clusters_too_small++;
        if (debug && cluster_id == debug_cluster) {
          Rcpp::Rcout << "DEBUG Cluster " << cluster_id << " skipped: only " << indices.size() << " traces (min: " << minTraces << ")" << std::endl;
        }
        continue;
      }

      if (debug && cluster_id == debug_cluster) {
        Rcpp::Rcout << "DEBUG Processing cluster " << cluster_id << " with " << indices.size() << " traces" << std::endl;
      }

      // Extract cluster data
      std::vector<float> cluster_rt, cluster_mz, cluster_intensity, cluster_noise;
      for (int idx : indices) {
        cluster_rt.push_back(clust_rt_sorted_rt[idx]);
        cluster_mz.push_back(clust_rt_sorted_mz[idx]);
        cluster_intensity.push_back(clust_rt_sorted_intensity[idx]);
        cluster_noise.push_back(clust_rt_sorted_noise[idx]);
      }

      const int n = cluster_rt.size();
      if (n < minTraces) {
        if (debug && cluster_id == debug_cluster) {
          Rcpp::Rcout << "DEBUG Cluster " << cluster_id << " skipped after extraction: only " << n << " points" << std::endl;
        }
        continue;
      }

      if (debug && cluster_id == debug_cluster) {
        Rcpp::Rcout << "DEBUG Cluster " << cluster_id << ": n=" << n << " points" << std::endl;

        // Debug: show intensity statistics
        auto minmax_intensity = std::minmax_element(cluster_intensity.begin(), cluster_intensity.end());
        float min_intensity = *minmax_intensity.first;
        float max_intensity = *minmax_intensity.second;
        float sum_intensity = std::accumulate(cluster_intensity.begin(), cluster_intensity.end(), 0.0f);
        float mean_intensity = sum_intensity / n;

        Rcpp::Rcout << "DEBUG Intensity stats: min=" << min_intensity << ", max=" << max_intensity
                    << ", mean=" << mean_intensity << std::endl;
      }

      // Calculate cycle time and window sizes
      std::vector<float> rt_diffs;
      for (int i = 1; i < n; ++i) {
        rt_diffs.push_back(cluster_rt[i] - cluster_rt[i-1]);
      }
      std::sort(rt_diffs.begin(), rt_diffs.end());
      float cycle_time = rt_diffs[rt_diffs.size() / 2]; // median

      int baseline_window_size = std::max(minTraces, static_cast<int>(std::floor(baselineWindow / cycle_time))) / 2;
      int derivative_window_size = std::max(minTraces, static_cast<int>(std::floor(4.0f / cycle_time)));

      if (debug && cluster_id == debug_cluster) {
        Rcpp::Rcout << "DEBUG Cycle time: " << cycle_time << ", baseline_window: " << baseline_window_size
                    << ", derivative_window: " << derivative_window_size << std::endl;
      }

      // Calculate baseline using moving minimum
      std::vector<float> baseline(n);
      for (int i = 0; i < n; ++i) {
        int start_idx = std::max(0, i - baseline_window_size);
        int end_idx = std::min(n - 1, i + baseline_window_size);

        float min_intensity = cluster_intensity[start_idx];
        for (int j = start_idx; j <= end_idx; ++j) {
          min_intensity = std::min(min_intensity, cluster_intensity[j]);
        }
        baseline[i] = min_intensity;
      }

      // Smooth baseline
      std::vector<float> smoothed_baseline = baseline;
      if (n >= 3) {
        for (int i = 1; i < n - 1; ++i) {
          smoothed_baseline[i] = (baseline[i-1] + baseline[i] + baseline[i+1]) / 3.0f;
        }
        baseline = smoothed_baseline;
      }

      if (debug && cluster_id == debug_cluster) {
        // Debug: baseline statistics
        auto minmax_baseline = std::minmax_element(baseline.begin(), baseline.end());
        float min_baseline = *minmax_baseline.first;
        float max_baseline = *minmax_baseline.second;
        float sum_baseline = std::accumulate(baseline.begin(), baseline.end(), 0.0f);
        float mean_baseline = sum_baseline / n;

        Rcpp::Rcout << "DEBUG Baseline stats: min=" << min_baseline << ", max=" << max_baseline
                    << ", mean=" << mean_baseline << std::endl;
      }

      // Smooth intensity
      std::vector<float> smoothed_intensity = cluster_intensity;
      int window_size_smooth = 2;
      int half_window_smooth = window_size_smooth / 2;
      if (n >= 3) {
        for (int i = 0; i < n; ++i) {
          int start_idx = std::max(0, i - half_window_smooth);
          int end_idx = std::min(n - 1, i + half_window_smooth);

          float sum = 0.0f;
          int count = 0;
          for (int j = start_idx; j <= end_idx; ++j) {
            sum += cluster_intensity[j];
            count++;
          }
          smoothed_intensity[i] = sum / count;
        }
      }

      if (debug && cluster_id == debug_cluster) {
        // Debug: smoothed intensity statistics
        auto minmax_smooth = std::minmax_element(smoothed_intensity.begin(), smoothed_intensity.end());
        float min_smooth = *minmax_smooth.first;
        float max_smooth = *minmax_smooth.second;
        float sum_smooth = std::accumulate(smoothed_intensity.begin(), smoothed_intensity.end(), 0.0f);
        float mean_smooth = sum_smooth / n;

        Rcpp::Rcout << "DEBUG Smoothed intensity stats: min=" << min_smooth << ", max=" << max_smooth
                    << ", mean=" << mean_smooth << std::endl;
      }

      // Calculate first derivative
      std::vector<float> dI(n - 1);
      for (int i = 0; i < n - 1; ++i) {
        dI[i] = smoothed_intensity[i + 1] - smoothed_intensity[i];
      }

      // Calculate second derivative
      std::vector<float> d2I(n - 2);
      for (int i = 0; i < n - 2; ++i) {
        d2I[i] = dI[i + 1] - dI[i];
      }

      // Find peak candidates: where slope changes + to -
      std::vector<int> candidates;
      for (int i = 1; i < static_cast<int>(dI.size()); ++i) {
        if (dI[i] <= 0 && dI[i-1] > 0) {
          candidates.push_back(i + 1); // +1 because dI is offset by 1
        }
      }

      if (debug && cluster_id == debug_cluster) {
        Rcpp::Rcout << "DEBUG Found " << candidates.size() << " peak candidates based on derivative sign change" << std::endl;

        if (candidates.empty()) {
          Rcpp::Rcout << "DEBUG No candidates found - checking derivative values:" << std::endl;
          for (size_t i = 0; i < std::min(static_cast<size_t>(10), dI.size()); ++i) {
            Rcpp::Rcout << "DEBUG dI[" << i << "] = " << dI[i] << std::endl;
          }
          continue;
        }
      } else if (candidates.empty()) {
        continue;
      }

      // Filter candidates based on derivative criteria
      std::vector<int> valid_peaks;
      int candidates_rejected_edge = 0;
      int candidates_rejected_derivative = 0;
      int candidates_rejected_apex = 0;

      for (int idx : candidates) {
        if (idx < derivative_window_size || idx >= n - derivative_window_size) {
          candidates_rejected_edge++;
          continue;
        }

        // Check first derivative before peak (should be positive)
        float pre_avg = 0.0f;
        int pre_count = 0;
        for (int i = std::max(0, idx - derivative_window_size); i < idx - 1; ++i) {
          if (i >= 0 && i < static_cast<int>(dI.size())) {
            pre_avg += dI[i];
            pre_count++;
          }
        }
        if (pre_count > 0) pre_avg /= pre_count;

        // Check first derivative after peak (should be negative)
        float post_avg = 0.0f;
        int post_count = 0;
        for (int i = idx; i < std::min(n - 1, idx + derivative_window_size); ++i) {
          if (i >= 0 && i < static_cast<int>(dI.size())) {
            post_avg += dI[i];
            post_count++;
          }
        }
        if (post_count > 0) post_avg /= post_count;

        // Check second derivative at peak (should be negative)
        float d2_at_peak = 0.0f;
        if (idx - 1 >= 0 && idx - 1 < static_cast<int>(d2I.size())) {
          d2_at_peak = d2I[idx - 1];
        }

        // Check for higher apex in neighborhood
        bool higher_apex_nearby = false;
        // for (int i = std::max(0, idx - derivative_window_size); i < std::min(n, idx + derivative_window_size); ++i) {
        //   if (i != idx && smoothed_intensity[i] >= smoothed_intensity[idx]) {
        //     higher_apex_nearby = true;
        //     break;
        //   }
        // }

        // Keep if criteria are met
        if (pre_count > 0 && post_count > 0 && pre_avg > 0 && post_avg < 0 &&
            d2_at_peak < 0 && !higher_apex_nearby) {
          valid_peaks.push_back(idx);
          if (debug && cluster_id == debug_cluster) {
            Rcpp::Rcout << "DEBUG Candidate at idx " << idx << " ACCEPTED: pre_avg=" << pre_avg
                        << ", post_avg=" << post_avg << ", d2=" << d2_at_peak << std::endl;
          }
        } else {
          if (higher_apex_nearby) {
            candidates_rejected_apex++;
          } else {
            candidates_rejected_derivative++;
          }
          if (debug && cluster_id == debug_cluster) {
            Rcpp::Rcout << "DEBUG Candidate at idx " << idx << " rejected: pre_avg=" << pre_avg
                        << ", post_avg=" << post_avg << ", d2=" << d2_at_peak
                        << ", higher_apex=" << higher_apex_nearby << std::endl;
          }
        }
      }

      if (debug && cluster_id == debug_cluster) {
        Rcpp::Rcout << "DEBUG Peak filtering: " << candidates.size() << " candidates -> " << valid_peaks.size() << " valid peaks" << std::endl;
        Rcpp::Rcout << "DEBUG Rejected: " << candidates_rejected_edge << " (edge), "
                    << candidates_rejected_derivative << " (derivative), "
                    << candidates_rejected_apex << " (apex)" << std::endl;
      }

      // Process each valid peak
      int peaks_rejected_boundary = 0;
      int peaks_rejected_snr = 0;
      int peaks_accepted = 0;

      for (int peak_idx : valid_peaks) {
        if (peak_idx < minTraces / 2 || peak_idx >= n - minTraces / 2) {
          peaks_rejected_boundary++;
          if (debug && cluster_id == debug_cluster) {
            Rcpp::Rcout << "DEBUG Peak at idx " << peak_idx << " rejected: too close to boundary" << std::endl;
          }
          continue;
        }

        if (debug && cluster_id == debug_cluster) {
          Rcpp::Rcout << "DEBUG Processing peak at idx " << peak_idx << std::endl;
        }

        // Find peak bounds using simplified version of get_peak_bounds
        float apex_intensity = smoothed_intensity[peak_idx];
        float min_intensity_threshold = 0.01f * apex_intensity;
        float max_half_width = maxWidth / 2.0f;

        // Left boundary
        int left_idx = std::max(0, peak_idx - 3);
        while (left_idx > 0) {
          if (cluster_rt[peak_idx] - cluster_rt[left_idx] > max_half_width) break;
          if (smoothed_intensity[left_idx] <= baseline[left_idx] * 1.2f) break;
          if (smoothed_intensity[left_idx] <= min_intensity_threshold) break;
          left_idx--;
        }

        // Right boundary
        int right_idx = std::min(n - 1, peak_idx + 3);
        while (right_idx < n - 1) {
          if (cluster_rt[right_idx] - cluster_rt[peak_idx] > max_half_width) break;
          if (smoothed_intensity[right_idx] <= baseline[right_idx] * 1.2f) break;
          if (smoothed_intensity[right_idx] <= min_intensity_threshold) break;
          right_idx++;
        }

        if (left_idx >= right_idx) {
          if (debug && cluster_id == debug_cluster) {
            Rcpp::Rcout << "DEBUG Peak at idx " << peak_idx << " rejected: invalid boundaries (left=" << left_idx << ", right=" << right_idx << ")" << std::endl;
          }
          continue;
        }

        // Extract peak region data
        std::vector<float> peak_intensities, peak_rt_vals, peak_mz_vals;
        for (int i = left_idx; i <= right_idx; ++i) {
          peak_intensities.push_back(cluster_intensity[i]);
          peak_rt_vals.push_back(cluster_rt[i]);
          peak_mz_vals.push_back(cluster_mz[i]);
        }

        if (peak_intensities.empty()) continue;

        // Find maximum intensity and its position
        auto max_it = std::max_element(peak_intensities.begin(), peak_intensities.end());
        float peak_max_intensity = *max_it;
        int max_position = std::distance(peak_intensities.begin(), max_it);

        float rt_at_max = peak_rt_vals[max_position];
        float mz_at_max = peak_mz_vals[max_position];

        // Calculate noise (minimum at edges)
        int peak_n = peak_intensities.size();
        std::vector<float> edge_intensities;
        if (peak_n >= 4) {
          edge_intensities.push_back(peak_intensities[0]);
          edge_intensities.push_back(peak_intensities[1]);
          edge_intensities.push_back(peak_intensities[peak_n-2]);
          edge_intensities.push_back(peak_intensities[peak_n-1]);
        } else {
          edge_intensities = peak_intensities;
        }

        float noise = *std::min_element(edge_intensities.begin(), edge_intensities.end());
        float signal = peak_max_intensity;
        float sn = (noise > 0 && signal > 0) ? signal / noise : 0.0f;

        // Check S/N ratio
        if (sn < minSNR) {
          peaks_rejected_snr++;
          if (debug && cluster_id == debug_cluster) {
            Rcpp::Rcout << "DEBUG Peak at idx " << peak_idx << " rejected: S/N=" << sn << " < " << minSNR
                        << " (signal=" << signal << ", noise=" << noise << ")" << std::endl;
          }
          continue;
        }

        peaks_accepted++;
        if (debug && cluster_id == debug_cluster) {
          Rcpp::Rcout << "DEBUG Peak at idx " << peak_idx << " ACCEPTED: S/N=" << sn << ", intensity=" << signal << std::endl;
        }

        // Calculate m/z statistics
        float mz_sum = 0.0f, intensity_sum = 0.0f;
        float mz_min = peak_mz_vals[0], mz_max = peak_mz_vals[0];

        for (size_t i = 0; i < peak_mz_vals.size(); ++i) {
          mz_sum += peak_mz_vals[i] * peak_intensities[i];
          intensity_sum += peak_intensities[i];
          mz_min = std::min(mz_min, peak_mz_vals[i]);
          mz_max = std::max(mz_max, peak_mz_vals[i]);
        }

        float mz_weighted = intensity_sum > 0 ? mz_sum / intensity_sum : mz_at_max;
        float ppm = (mz_max - mz_min) / mz_weighted * 1e6f;
        float width = cluster_rt[right_idx] - cluster_rt[left_idx];

        // Calculate FWHM in RT dimension
        float half_max_intensity = peak_max_intensity / 2.0f;
        float fwhm_rt = 0.0f;

        // Find left and right indices where intensity drops to half maximum
        int fwhm_left_idx = max_position;
        int fwhm_right_idx = max_position;

        // Search left from apex
        while (fwhm_left_idx > 0 && peak_intensities[fwhm_left_idx] > half_max_intensity) {
          fwhm_left_idx--;
        }

        // Search right from apex
        while (fwhm_right_idx < static_cast<int>(peak_intensities.size()) - 1 &&
               peak_intensities[fwhm_right_idx] > half_max_intensity) {
          fwhm_right_idx++;
        }

        // Calculate FWHM in RT
        if (fwhm_left_idx < fwhm_right_idx && fwhm_right_idx < static_cast<int>(peak_rt_vals.size())) {
          fwhm_rt = peak_rt_vals[fwhm_right_idx] - peak_rt_vals[fwhm_left_idx];
        } else {
          fwhm_rt = width; // fallback to total width
        }

        // Calculate FWHM in MZ dimension based on MZ range at FWHM RT window
        float fwhm_mz = 0.0f;
        
        if (fwhm_left_idx < fwhm_right_idx && 
            fwhm_left_idx < static_cast<int>(peak_mz_vals.size()) && 
            fwhm_right_idx < static_cast<int>(peak_mz_vals.size())) {
          
          // Find min and max m/z values within the FWHM RT window
          float mz_min_fwhm = peak_mz_vals[fwhm_left_idx];
          float mz_max_fwhm = peak_mz_vals[fwhm_left_idx];
          
          for (int i = fwhm_left_idx; i <= fwhm_right_idx; ++i) {
            mz_min_fwhm = std::min(mz_min_fwhm, peak_mz_vals[i]);
            mz_max_fwhm = std::max(mz_max_fwhm, peak_mz_vals[i]);
          }
          
          fwhm_mz = mz_max_fwhm - mz_min_fwhm;
        } else {
          fwhm_mz = mz_max - mz_min; // fallback to total MZ range
        }

        // Collect peak profile data
        std::vector<float> profile_rt_data, profile_raw_intensity_data;
        std::vector<float> profile_baseline_data, profile_smoothed_intensity_data;

        for (int i = left_idx; i <= right_idx; ++i) {
          profile_rt_data.push_back(cluster_rt[i]);
          profile_raw_intensity_data.push_back(cluster_intensity[i]);
          profile_baseline_data.push_back(baseline[i]);
          profile_smoothed_intensity_data.push_back(smoothed_intensity[i]);
        }

        // Check for overlapping peaks within FWHM RT window before storing
        bool merged = false;
        float fwhm_rt_half = fwhm_rt / 2.0f;
        
        for (size_t existing_idx = 0; existing_idx < peaks_rt.size(); ++existing_idx) {
          // Only check peaks from the same cluster
          if (peaks_cluster[existing_idx] != cluster_id) continue;
          
          // Calculate RT distance between current peak and existing peak
          float rt_distance = std::abs(rt_at_max - peaks_rt[existing_idx]);
          float existing_fwhm_half = peaks_fwhm_rt[existing_idx] / 2.0f;
          
          // Check if peaks overlap within their FWHM RT windows
          if (rt_distance < (fwhm_rt_half + existing_fwhm_half)) {
            // Peaks overlap - merge them, keeping the one with higher intensity
            
            // Find the existing peak's boundaries in the cluster data
            int existing_left_idx = -1, existing_right_idx = -1;
            float existing_rtmin = peaks_rtmin[existing_idx];
            float existing_rtmax = peaks_rtmax[existing_idx];
            
            // Find indices corresponding to existing peak boundaries
            for (int i = 0; i < n; ++i) {
              if (existing_left_idx == -1 && cluster_rt[i] >= existing_rtmin) {
                existing_left_idx = i;
              }
              if (cluster_rt[i] <= existing_rtmax) {
                existing_right_idx = i;
              }
            }
            
            // Determine merged boundaries (wider RT window)
            int merged_left_idx = std::min(existing_left_idx != -1 ? existing_left_idx : left_idx, left_idx);
            int merged_right_idx = std::max(existing_right_idx != -1 ? existing_right_idx : right_idx, right_idx);
            
            // Extract merged peak region data
            std::vector<float> merged_intensities, merged_rt_vals, merged_mz_vals;
            std::vector<float> merged_profile_rt, merged_profile_raw_intensity;
            std::vector<float> merged_profile_baseline, merged_profile_smoothed_intensity;
            
            for (int i = merged_left_idx; i <= merged_right_idx; ++i) {
              merged_intensities.push_back(cluster_intensity[i]);
              merged_rt_vals.push_back(cluster_rt[i]);
              merged_mz_vals.push_back(cluster_mz[i]);
              merged_profile_rt.push_back(cluster_rt[i]);
              merged_profile_raw_intensity.push_back(cluster_intensity[i]);
              merged_profile_baseline.push_back(baseline[i]);
              merged_profile_smoothed_intensity.push_back(smoothed_intensity[i]);
            }
            
            if (peak_max_intensity > peaks_intensity[existing_idx]) {
              if (debug && cluster_id == debug_cluster) {
                Rcpp::Rcout << "DEBUG Merging overlapping peaks: keeping current (intensity=" 
                           << peak_max_intensity << ") over existing (intensity=" 
                           << peaks_intensity[existing_idx] << ")" << std::endl;
              }
              
              // Update existing peak with current peak's data (higher intensity)
              peaks_rt[existing_idx] = rt_at_max;
              peaks_mz[existing_idx] = mz_weighted;
              peaks_intensity[existing_idx] = peak_max_intensity;
              peaks_noise[existing_idx] = noise;
              peaks_sn[existing_idx] = sn;
              
              // Update boundaries using min/max approach
              peaks_rtmin[existing_idx] = cluster_rt[merged_left_idx];
              peaks_rtmax[existing_idx] = cluster_rt[merged_right_idx];
              peaks_mzmin[existing_idx] = std::min(peaks_mzmin[existing_idx], mz_min);
              peaks_mzmax[existing_idx] = std::max(peaks_mzmax[existing_idx], mz_max);
              
              // Update peak ID
              std::string updated_peak_id = "CL" + std::to_string(cluster_id) +
                                          "_N" + std::to_string(existing_idx + 1) +
                                          "_MZ" + std::to_string(static_cast<int>(std::round(mz_weighted))) +
                                          "_RT" + std::to_string(static_cast<int>(std::round(rt_at_max)));
              peaks_id[existing_idx] = updated_peak_id;
              
            } else {
              if (debug && cluster_id == debug_cluster) {
                Rcpp::Rcout << "DEBUG Merging overlapping peaks: keeping existing (intensity=" 
                           << peaks_intensity[existing_idx] << ") over current (intensity=" 
                           << peak_max_intensity << ")" << std::endl;
              }
              
              // Keep existing peak's main parameters but update boundaries
              peaks_rtmin[existing_idx] = cluster_rt[merged_left_idx];
              peaks_rtmax[existing_idx] = cluster_rt[merged_right_idx];
              peaks_mzmin[existing_idx] = std::min(peaks_mzmin[existing_idx], mz_min);
              peaks_mzmax[existing_idx] = std::max(peaks_mzmax[existing_idx], mz_max);
            }
            
            // Recalculate FWHM based on merged peak data
            if (!merged_intensities.empty()) {
              auto merged_max_it = std::max_element(merged_intensities.begin(), merged_intensities.end());
              float merged_max_intensity = *merged_max_it;
              int merged_max_position = std::distance(merged_intensities.begin(), merged_max_it);
              
              // Calculate FWHM in RT dimension for merged peak
              float merged_half_max = merged_max_intensity / 2.0f;
              int merged_fwhm_left = merged_max_position;
              int merged_fwhm_right = merged_max_position;
              
              // Search left from apex
              while (merged_fwhm_left > 0 && merged_intensities[merged_fwhm_left] > merged_half_max) {
                merged_fwhm_left--;
              }
              
              // Search right from apex
              while (merged_fwhm_right < static_cast<int>(merged_intensities.size()) - 1 &&
                     merged_intensities[merged_fwhm_right] > merged_half_max) {
                merged_fwhm_right++;
              }
              
              // Calculate merged FWHM in RT
              float merged_fwhm_rt = 0.0f;
              if (merged_fwhm_left < merged_fwhm_right && 
                  merged_fwhm_right < static_cast<int>(merged_rt_vals.size())) {
                merged_fwhm_rt = merged_rt_vals[merged_fwhm_right] - merged_rt_vals[merged_fwhm_left];
              } else {
                merged_fwhm_rt = merged_rt_vals.back() - merged_rt_vals.front(); // fallback to total width
              }
              
              // Calculate merged FWHM in MZ
              float merged_fwhm_mz = 0.0f;
              if (merged_fwhm_left < merged_fwhm_right && 
                  merged_fwhm_left < static_cast<int>(merged_mz_vals.size()) && 
                  merged_fwhm_right < static_cast<int>(merged_mz_vals.size())) {
                
                float mz_min_fwhm = merged_mz_vals[merged_fwhm_left];
                float mz_max_fwhm = merged_mz_vals[merged_fwhm_left];
                
                for (int i = merged_fwhm_left; i <= merged_fwhm_right; ++i) {
                  mz_min_fwhm = std::min(mz_min_fwhm, merged_mz_vals[i]);
                  mz_max_fwhm = std::max(mz_max_fwhm, merged_mz_vals[i]);
                }
                
                merged_fwhm_mz = mz_max_fwhm - mz_min_fwhm;
              } else {
                merged_fwhm_mz = peaks_mzmax[existing_idx] - peaks_mzmin[existing_idx]; // fallback
              }
              
              // Update FWHM values with recalculated merged values
              peaks_fwhm_rt[existing_idx] = merged_fwhm_rt;
              peaks_fwhm_mz[existing_idx] = merged_fwhm_mz;
            }
            
            // Recalculate other merged peak parameters
            float merged_width = peaks_rtmax[existing_idx] - peaks_rtmin[existing_idx];
            float merged_ppm = (peaks_mzmax[existing_idx] - peaks_mzmin[existing_idx]) / peaks_mz[existing_idx] * 1e6f;
            
            peaks_width[existing_idx] = merged_width;
            peaks_ppm[existing_idx] = merged_ppm;
            peaks_n_traces[existing_idx] = merged_right_idx - merged_left_idx + 1;
            
            // Update profile data with merged data
            peaks_profile_rt[existing_idx] = merged_profile_rt;
            peaks_profile_raw_intensity[existing_idx] = merged_profile_raw_intensity;
            peaks_profile_baseline[existing_idx] = merged_profile_baseline;
            peaks_profile_smoothed_intensity[existing_idx] = merged_profile_smoothed_intensity;
            
            // Reset fitted intensity and fitted RT (will be recalculated during Gaussian fitting)
            peaks_fitted_intensity[existing_idx] = std::vector<float>();
            peaks_fitted_rt[existing_idx] = std::vector<float>();
            
            merged = true;
            break;
          }
        }
        
        // If no overlap was found, store as new peak
        if (!merged) {
          // Store peak data
          peaks_rt.push_back(rt_at_max);
          peaks_mz.push_back(mz_weighted);
          peaks_intensity.push_back(peak_max_intensity);
          peaks_noise.push_back(noise);
          peaks_sn.push_back(sn);
          peaks_rtmin.push_back(cluster_rt[left_idx]);
          peaks_rtmax.push_back(cluster_rt[right_idx]);
          peaks_mzmin.push_back(mz_min);
          peaks_mzmax.push_back(mz_max);
          peaks_width.push_back(width);
          peaks_ppm.push_back(ppm);
          peaks_fwhm_rt.push_back(fwhm_rt);
          peaks_fwhm_mz.push_back(fwhm_mz);
          peaks_cluster.push_back(cluster_id);
          peaks_n_traces.push_back(right_idx - left_idx + 1);

          // Store peak profile data as native C++ vectors
          peaks_profile_rt.push_back(profile_rt_data);
          peaks_profile_raw_intensity.push_back(profile_raw_intensity_data);
          peaks_profile_baseline.push_back(profile_baseline_data);
          peaks_profile_smoothed_intensity.push_back(profile_smoothed_intensity_data);
          
          // Initialize empty fitted intensity and fitted RT vectors (will be filled during Gaussian fitting)
          peaks_fitted_intensity.push_back(std::vector<float>());
          peaks_fitted_rt.push_back(std::vector<float>());

          // Generate peak ID
          std::string peak_id = "CL" + std::to_string(cluster_id) +
                               "_N" + std::to_string(peaks_rt.size()) +
                               "_MZ" + std::to_string(static_cast<int>(std::round(mz_weighted))) +
                               "_RT" + std::to_string(static_cast<int>(std::round(rt_at_max)));
          peaks_id.push_back(peak_id);
        }
      }

      if (debug && cluster_id == debug_cluster) {
        Rcpp::Rcout << "DEBUG Cluster " << cluster_id << " summary: " << peaks_accepted << " peaks accepted, "
                    << peaks_rejected_boundary << " rejected (boundary), "
                    << peaks_rejected_snr << " rejected (S/N)" << std::endl;
      }
    }

    // MARK: Calculate Peak Area
    Rcpp::Rcout << "  4/5 Calculating peak areas for " << peaks_rt.size() << " peaks" << std::endl;
    
    // Initialize area vector with correct size
    peaks_area.resize(peaks_rt.size(), 0.0f);
    
    for (size_t peak_idx = 0; peak_idx < peaks_rt.size(); ++peak_idx) {
      float area = 0.0f;
      try {
        // Use the raw intensity profile data for area calculation
        if (peak_idx < peaks_profile_rt.size() && peak_idx < peaks_profile_raw_intensity.size()) {
          const std::vector<float>& profile_rt = peaks_profile_rt[peak_idx];
          const std::vector<float>& profile_intensity = peaks_profile_raw_intensity[peak_idx];
          
          // Validate data
          if (!profile_rt.empty() && !profile_intensity.empty() && 
              profile_rt.size() == profile_intensity.size() && profile_rt.size() >= 2) {
            
            // Check for valid data (no NaN, no infinite values)
            bool valid_data = true;
            for (size_t i = 0; i < profile_rt.size(); ++i) {
              if (std::isnan(profile_rt[i]) || std::isinf(profile_rt[i]) ||
                  std::isnan(profile_intensity[i]) || std::isinf(profile_intensity[i]) ||
                  profile_intensity[i] < 0) {
                valid_data = false;
                break;
              }
            }
            
            if (valid_data) {
              // Calculate area using trapezoidal integration from NTS_utils
              area = MassSpecResults_NonTargetAnalysis::trapezoidal_area(profile_rt, profile_intensity);
              
              // Validate result
              if (std::isnan(area) || std::isinf(area) || area < 0) {
                area = 0.0f;
              }
              
              if (debug && peak_idx < peaks_cluster.size() && peaks_cluster[peak_idx] == debug_cluster) {
                Rcpp::Rcout << "  DEBUG Peak " << peak_idx << " area calculation: "
                            << "RT range [" << profile_rt.front() << " to " << profile_rt.back() << "], "
                            << "intensity range [" << *std::min_element(profile_intensity.begin(), profile_intensity.end())
                            << " to " << *std::max_element(profile_intensity.begin(), profile_intensity.end()) << "], "
                            << "area = " << area << std::endl;
              }
            } else {
              if (debug && peak_idx < peaks_cluster.size() && peaks_cluster[peak_idx] == debug_cluster) {
                Rcpp::Rcout << "  DEBUG Peak " << peak_idx << " area calculation failed: invalid data detected" << std::endl;
              }
            }
          } else {
            if (debug && peak_idx < peaks_cluster.size() && peaks_cluster[peak_idx] == debug_cluster) {
              Rcpp::Rcout << "  DEBUG Peak " << peak_idx << " area calculation failed: insufficient or mismatched data (RT size: " 
                          << profile_rt.size() << ", intensity size: " << profile_intensity.size() << ")" << std::endl;
            }
          }
        }
      } catch (...) {
        // Catch any exception and set safe default
        area = 0.0f;
      }
      
      // Store the calculated area
      peaks_area[peak_idx] = area;
    }
    
    // MARK: Peak Gaussian Fit
    Rcpp::Rcout << "  5/5 Calculating Gaussian fit quality for " << peaks_rt.size() << " peaks" << std::endl;

    // Extra quality parameters by estimating the gaussian fit
    std::vector<float> peaks_gaussian_A, peaks_gaussian_mu, peaks_gaussian_sigma, peaks_gaussian_rsquared;
    
    for (size_t peak_idx = 0; peak_idx < peaks_rt.size(); ++peak_idx) {
      // Initialize Gaussian parameters with safe defaults
      float A = 0.0f, mu = 0.0f, sigma = 0.0f, rsquared = 0.0f;
      
      try {
        // Safely extract RT and smoothed intensity data from stored profile
        if (peak_idx < peaks_profile_rt.size() && peak_idx < peaks_profile_smoothed_intensity.size()) {
          const std::vector<float>& peak_rt_data = peaks_profile_rt[peak_idx];
          const std::vector<float>& peak_smoothed_data = peaks_profile_smoothed_intensity[peak_idx];
          
          // Check if vectors are valid
          if (!peak_rt_data.empty() && !peak_smoothed_data.empty()) {
            
            // Enhanced validation of data
            if (peak_rt_data.size() >= 3 && peak_smoothed_data.size() >= 3 && 
                peak_rt_data.size() == peak_smoothed_data.size() &&
                peak_rt_data.size() <= 1000) { // Prevent excessive memory use
              
              // Check for valid intensity values (no NaN, no infinite values)
              bool valid_data = true;
              for (float intensity : peak_smoothed_data) {
                if (std::isnan(intensity) || std::isinf(intensity) || intensity < 0) {
                  valid_data = false;
                  break;
                }
              }
              
              // Check for valid RT values
              for (float rt : peak_rt_data) {
                if (std::isnan(rt) || std::isinf(rt)) {
                  valid_data = false;
                  break;
                }
              }
              
              if (valid_data) {
                // Find peak maximum first
                auto max_it = std::max_element(peak_smoothed_data.begin(), peak_smoothed_data.end());
                if (max_it == peak_smoothed_data.end()) continue;
                
                size_t max_idx = std::distance(peak_smoothed_data.begin(), max_it);
                
                // Trim data to be centered around the apex with equal points on both sides (max 5 per side)
                const int max_points_per_side = 5;
                int left_points = std::min(static_cast<int>(max_idx), max_points_per_side);
                int right_points = std::min(static_cast<int>(peak_rt_data.size() - max_idx - 1), max_points_per_side);
                
                // Use the minimum of left and right to keep it balanced
                int points_per_side = std::min(left_points, right_points);
                
                // Create trimmed vectors centered on the apex
                std::vector<float> trimmed_rt_data, trimmed_smoothed_data;
                
                if (points_per_side > 0) {
                  size_t start_idx = max_idx - points_per_side;
                  size_t end_idx = max_idx + points_per_side;
                  
                  trimmed_rt_data = std::vector<float>(peak_rt_data.begin() + start_idx, 
                                                     peak_rt_data.begin() + end_idx + 1);
                  trimmed_smoothed_data = std::vector<float>(peak_smoothed_data.begin() + start_idx, 
                                                           peak_smoothed_data.begin() + end_idx + 1);
                  
                  if (debug && peak_idx < peaks_cluster.size() && peaks_cluster[peak_idx] == debug_cluster) {
                    Rcpp::Rcout << "DEBUG Pre-fitting trimmed data - original size: " << peak_rt_data.size() 
                                << ", trimmed size: " << trimmed_rt_data.size() 
                                << ", points per side: " << points_per_side << std::endl;
                    Rcpp::Rcout << "  DEBUG Pre-fitting trimmed RT range: " << trimmed_rt_data.front() 
                                << " to " << trimmed_rt_data.back() << std::endl;
                  }
                } else {
                  // If we can't trim, use original data
                  trimmed_rt_data = peak_rt_data;
                  trimmed_smoothed_data = peak_smoothed_data;
                }
                
                // Initial parameter estimation with safety checks using trimmed data
                auto trimmed_max_it = std::max_element(trimmed_smoothed_data.begin(), trimmed_smoothed_data.end());
                if (trimmed_max_it != trimmed_smoothed_data.end()) {
                  A = *trimmed_max_it;
                  size_t trimmed_max_idx = std::distance(trimmed_smoothed_data.begin(), trimmed_max_it);
                  
                  // Set mu to the RT at the apex of trimmed data
                  if (trimmed_max_idx < trimmed_rt_data.size()) {
                    mu = trimmed_rt_data[trimmed_max_idx];
                  } else {
                    // Fallback: use original data apex
                    mu = peak_rt_data[max_idx];
                  }
                  
                  // Safe sigma estimation
                  if (peak_idx < peaks_fwhm_rt.size()) {
                    float fwhm = peaks_fwhm_rt[peak_idx];
                    if (fwhm > 0 && !std::isnan(fwhm) && !std::isinf(fwhm)) {
                      sigma = fwhm / 2.355f;
                    }
                  }
                  
                  // Fallback sigma estimation using trimmed data for better accuracy
                  if (sigma <= 0 || std::isnan(sigma) || std::isinf(sigma)) {
                    if (trimmed_rt_data.size() >= 2) {
                      float rt_range = trimmed_rt_data.back() - trimmed_rt_data.front();
                      if (rt_range > 0 && !std::isnan(rt_range) && !std::isinf(rt_range)) {
                        sigma = rt_range / 4.0f;
                      }
                    } else if (peak_rt_data.size() >= 2) {
                      // Fallback to original data if trimmed is too small
                      float rt_range = peak_rt_data.back() - peak_rt_data.front();
                      if (rt_range > 0 && !std::isnan(rt_range) && !std::isinf(rt_range)) {
                        sigma = rt_range / 8.0f; // Use smaller sigma for untrimmed data
                      }
                    }
                  }
                  
                  // Ensure sigma is within reasonable bounds
                  if (sigma <= 0 || std::isnan(sigma) || std::isinf(sigma)) {
                    sigma = 1.0f;
                  }
                  sigma = std::max(0.1f, std::min(sigma, 100.0f)); // Clamp sigma
                  
                  // Ensure A and mu are valid
                  if (A <= 0 || std::isnan(A) || std::isinf(A)) {
                    A = 1.0f;
                  }
                  if (std::isnan(mu) || std::isinf(mu)) {
                    mu = peak_rt_data[peak_rt_data.size() / 2]; // Use middle RT as fallback
                  }

                  // Only perform fitting if parameters are reasonable
                  if (A > 0 && A < 1e10 && sigma > 0.01 && sigma < 1000 && 
                      !std::isnan(mu) && !std::isinf(mu)) {
                    
                    // Create copies for fitting (to preserve originals if fitting fails)
                    float A_fit = A, mu_fit = mu, sigma_fit = sigma;
                    
                    // Use trimmed data for fitting (should always be available now)
                    const std::vector<float>* rt_data_ptr = &trimmed_rt_data;
                    const std::vector<float>* smoothed_data_ptr = &trimmed_smoothed_data;
                    bool used_trimmed_data = true;
                    
                    // Perform Gaussian fitting with timeout protection
                    MassSpecResults_NonTargetAnalysis::fit_gaussian(*rt_data_ptr, *smoothed_data_ptr, A_fit, mu_fit, sigma_fit);
                    
                    // Validate fitted parameters
                    if (!std::isnan(A_fit) && !std::isinf(A_fit) && A_fit > 0 &&
                        !std::isnan(mu_fit) && !std::isinf(mu_fit) &&
                        !std::isnan(sigma_fit) && !std::isinf(sigma_fit) && sigma_fit > 0) {
                      
                      A = A_fit;
                      sigma = sigma_fit;
                      
                      // Since we used trimmed data, mu_fit is in trimmed coordinates
                      // We need to use the original RT value at the apex for consistency
                      mu = peak_rt_data[max_idx]; // Use the original RT at max intensity
                      
                      // Calculate R-squared with safety checks
                      rsquared = MassSpecResults_NonTargetAnalysis::calculate_gaussian_rsquared(
                        *rt_data_ptr, *smoothed_data_ptr, A, mu, sigma);
                      
                      // Validate R-squared
                      if (std::isnan(rsquared) || std::isinf(rsquared)) {
                        rsquared = 0.0f;
                      } else {
                        rsquared = std::max(0.0f, std::min(rsquared, 1.0f));
                      }
                      
                      // Calculate fitted intensity values using the Gaussian model for the trimmed RT data
                      std::vector<float> fitted_intensities;
                      fitted_intensities.reserve(trimmed_rt_data.size());
                      
                      for (float rt_val : trimmed_rt_data) {
                        float fitted_val = MassSpecResults_NonTargetAnalysis::gaussian_function(A, mu, sigma, rt_val);
                        // Ensure fitted value is valid
                        if (std::isnan(fitted_val) || std::isinf(fitted_val)) {
                          fitted_val = 0.0f;
                        }
                        fitted_intensities.push_back(fitted_val);
                      }
                      
                      // Check if all fitted values are zero (debugging)
                      bool all_zero = true;
                      for (float val : fitted_intensities) {
                        if (val > 1e-6) { // Consider very small values as effectively zero
                          all_zero = false;
                          break;
                        }
                      }
                      
                      if (debug && all_zero && peak_idx < peaks_cluster.size() && peaks_cluster[peak_idx] == debug_cluster) {
                        Rcpp::Rcout << "DEBUG All fitted intensities are zero for Peak " << peak_idx << std::endl;
                        Rcpp::Rcout << "DEBUG RT range vs mu: RT[" << peak_rt_data.front() << " to " << peak_rt_data.back() 
                                    << "], mu=" << mu << ", sigma=" << sigma << std::endl;
                        // Test Gaussian function at mu
                        float test_at_mu = MassSpecResults_NonTargetAnalysis::gaussian_function(A, mu, sigma, mu);
                        Rcpp::Rcout << "DEBUG Gaussian at mu: " << test_at_mu << " (should be A=" << A << ")" << std::endl;
                      }
                      
                      // Store fitted intensities and corresponding RT values
                      peaks_fitted_intensity[peak_idx] = fitted_intensities;
                      peaks_fitted_rt[peak_idx] = trimmed_rt_data;
                      
                      // Debug message for debug cluster
                      if (debug && peak_idx < peaks_cluster.size() && peaks_cluster[peak_idx] == debug_cluster) {
                        Rcpp::Rcout << "DEBUG Cluster " << debug_cluster << " - Peak " << peak_idx << " fitted intensities: ";
                        for (size_t i = 0; i < std::min(fitted_intensities.size(), static_cast<size_t>(10)); ++i) {
                          Rcpp::Rcout << fitted_intensities[i];
                          if (i < std::min(fitted_intensities.size(), static_cast<size_t>(10)) - 1) Rcpp::Rcout << ", ";
                        }
                        if (fitted_intensities.size() > 10) {
                          Rcpp::Rcout << " ... (" << fitted_intensities.size() << " total)";
                        }
                        Rcpp::Rcout << std::endl;
                        Rcpp::Rcout << "DEBUG Gaussian params: A=" << A << ", mu=" << mu << ", sigma=" << sigma << ", R²=" << rsquared << std::endl;
                        Rcpp::Rcout << "DEBUG Original apex RT: " << peak_rt_data[max_idx] << ", fitted mu: " << mu_fit << std::endl;
                        if (used_trimmed_data) {
                          Rcpp::Rcout << "DEBUG Used trimmed data for fitting, corrected mu to original apex RT" << std::endl;
                        }
                        
                        // Debug RT values and Gaussian calculation details
                        Rcpp::Rcout << "DEBUG RT values: ";
                        for (size_t i = 0; i < std::min(peak_rt_data.size(), static_cast<size_t>(10)); ++i) {
                          Rcpp::Rcout << peak_rt_data[i];
                          if (i < std::min(peak_rt_data.size(), static_cast<size_t>(10)) - 1) Rcpp::Rcout << ", ";
                        }
                        Rcpp::Rcout << std::endl;
                        
                        // Calculate and show some individual Gaussian values manually
                        if (peak_rt_data.size() >= 3) {
                          float rt_min = peak_rt_data[0];
                          float rt_max = peak_rt_data.back();
                          float rt_center = peak_rt_data[peak_rt_data.size()/2];
                          
                          Rcpp::Rcout << "DEBUG RT range: " << rt_min << " to " << rt_max 
                                      << " (center: " << rt_center << ", span: " << (rt_max - rt_min) << ")" << std::endl;
                          
                          // Manual Gaussian calculation for verification
                          float gauss_min = A * std::exp(-0.5f * std::pow((rt_min - mu) / sigma, 2.0f));
                          float gauss_center = A * std::exp(-0.5f * std::pow((rt_center - mu) / sigma, 2.0f));
                          float gauss_max = A * std::exp(-0.5f * std::pow((rt_max - mu) / sigma, 2.0f));
                          
                          Rcpp::Rcout << "DEBUG Manual Gaussian: min_rt=" << gauss_min 
                                      << ", center_rt=" << gauss_center << ", max_rt=" << gauss_max << std::endl;
                          
                          // Show ratio of sigma to RT range
                          float sigma_to_range_ratio = sigma / (rt_max - rt_min);
                          Rcpp::Rcout << "DEBUG Sigma/RT_range ratio: " << sigma_to_range_ratio 
                                      << " (>1 means curve is very flat)" << std::endl;
                        }
                      }
                      
                    } else {
                      // Fitting produced invalid parameters
                      A = mu = sigma = rsquared = 0.0f;
                      
                      // Store empty fitted intensities and RT for failed fits
                      peaks_fitted_intensity[peak_idx] = std::vector<float>(trimmed_rt_data.size(), 0.0f);
                      peaks_fitted_rt[peak_idx] = trimmed_rt_data;
                    }
                  } else {
                    // Parameters not reasonable for fitting - store zeros
                    A = mu = sigma = rsquared = 0.0f;
                    peaks_fitted_intensity[peak_idx] = std::vector<float>(trimmed_rt_data.size(), 0.0f);
                    peaks_fitted_rt[peak_idx] = trimmed_rt_data;
                  }
                }
              }
            }
          }
        }
      } catch (...) {
        // Catch any exception and set safe defaults
        A = mu = sigma = rsquared = 0.0f;
        // Ensure fitted intensity and fitted RT vectors exist even on exception
        if (peak_idx < peaks_fitted_intensity.size()) {
          peaks_fitted_intensity[peak_idx] = std::vector<float>();
        }
        if (peak_idx < peaks_fitted_rt.size()) {
          peaks_fitted_rt[peak_idx] = std::vector<float>();
        }
      }
      
      // Store Gaussian fit parameters (always store something to maintain vector size)
      peaks_gaussian_A.push_back(A);
      peaks_gaussian_mu.push_back(mu);
      peaks_gaussian_sigma.push_back(sigma);
      peaks_gaussian_rsquared.push_back(rsquared);
    }

    Rcpp::Rcout << "  Peak detection summary:" << std::endl;
    
    // Calculate and display denoising statistics
    float denoising_reduction_percentage = 0.0f;
    if (total_raw_points > 0) {
      size_t points_removed = total_raw_points - total_clean_points;
      denoising_reduction_percentage = (static_cast<float>(points_removed) / static_cast<float>(total_raw_points)) * 100.0f;
    }
    
    // Rcpp::Rcout << "    Data points before denoising: " << total_raw_points << std::endl;
    // Rcpp::Rcout << "    Data points after denoising: " << total_clean_points << std::endl;
    Rcpp::Rcout << "    Data reduction by denoising: " << std::fixed << std::setprecision(1) << denoising_reduction_percentage << "%" << std::endl;

    Rcpp::Rcout << "    Total clusters: " << cluster_indices.size() << std::endl;
    Rcpp::Rcout << "    Clusters processed: " << (total_clusters_processed - clusters_too_small) << std::endl;
    Rcpp::Rcout << "    Total peaks found: " << peaks_rt.size() << std::endl;

    // Create output list with detected peaks
    Rcpp::List peaks_list;
    if (!peaks_rt.empty()) {
      peaks_list["id"] = peaks_id;
      peaks_list["rt"] = peaks_rt;
      peaks_list["mz"] = peaks_mz;
      peaks_list["intensity"] = peaks_intensity;
      peaks_list["noise"] = peaks_noise;
      peaks_list["sn"] = peaks_sn;
      peaks_list["rtmin"] = peaks_rtmin;
      peaks_list["rtmax"] = peaks_rtmax;
      peaks_list["mzmin"] = peaks_mzmin;
      peaks_list["mzmax"] = peaks_mzmax;
      peaks_list["width"] = peaks_width;
      peaks_list["area"] = peaks_area;
      peaks_list["ppm"] = peaks_ppm;
      peaks_list["fwhm_rt"] = peaks_fwhm_rt;
      peaks_list["fwhm_mz"] = peaks_fwhm_mz;
      peaks_list["cluster"] = peaks_cluster;
      peaks_list["n_traces"] = peaks_n_traces;
      // Convert native C++ vectors to R lists
      Rcpp::List profile_rt_list(peaks_profile_rt.size());
      Rcpp::List profile_raw_intensity_list(peaks_profile_raw_intensity.size());
      Rcpp::List profile_baseline_list(peaks_profile_baseline.size());
      Rcpp::List profile_smoothed_intensity_list(peaks_profile_smoothed_intensity.size());
      Rcpp::List profile_fitted_intensity_list(peaks_fitted_intensity.size());
      Rcpp::List profile_fitted_rt_list(peaks_fitted_rt.size());
      
      for (size_t i = 0; i < peaks_profile_rt.size(); ++i) {
        profile_rt_list[i] = Rcpp::wrap(peaks_profile_rt[i]);
        profile_raw_intensity_list[i] = Rcpp::wrap(peaks_profile_raw_intensity[i]);
        profile_baseline_list[i] = Rcpp::wrap(peaks_profile_baseline[i]);
        profile_smoothed_intensity_list[i] = Rcpp::wrap(peaks_profile_smoothed_intensity[i]);
        profile_fitted_intensity_list[i] = Rcpp::wrap(peaks_fitted_intensity[i]);
        profile_fitted_rt_list[i] = Rcpp::wrap(peaks_fitted_rt[i]);
      }
      
      peaks_list["profile_rt"] = profile_rt_list;
      peaks_list["profile_raw_intensity"] = profile_raw_intensity_list;
      peaks_list["profile_baseline"] = profile_baseline_list;
      peaks_list["profile_smoothed_intensity"] = profile_smoothed_intensity_list;
      peaks_list["profile_fitted_intensity"] = profile_fitted_intensity_list;
      peaks_list["profile_fitted_rt"] = profile_fitted_rt_list;
      peaks_list["gaussian_A"] = peaks_gaussian_A;
      peaks_list["gaussian_mu"] = peaks_gaussian_mu;
      peaks_list["gaussian_sigma"] = peaks_gaussian_sigma;
      peaks_list["gaussian_rsquared"] = peaks_gaussian_rsquared;
    } else {
      // Return empty data frame with proper column names
      peaks_list["id"] = std::vector<std::string>();
      peaks_list["rt"] = std::vector<float>();
      peaks_list["mz"] = std::vector<float>();
      peaks_list["intensity"] = std::vector<float>();
      peaks_list["noise"] = std::vector<float>();
      peaks_list["sn"] = std::vector<float>();
      peaks_list["rtmin"] = std::vector<float>();
      peaks_list["rtmax"] = std::vector<float>();
      peaks_list["mzmin"] = std::vector<float>();
      peaks_list["mzmax"] = std::vector<float>();
      peaks_list["width"] = std::vector<float>();
      peaks_list["area"] = std::vector<float>();
      peaks_list["ppm"] = std::vector<float>();
      peaks_list["fwhm_rt"] = std::vector<float>();
      peaks_list["fwhm_mz"] = std::vector<float>();
      peaks_list["cluster"] = std::vector<int>();
      peaks_list["n_traces"] = std::vector<int>();
      peaks_list["profile_rt"] = std::vector<Rcpp::List>();
      peaks_list["profile_raw_intensity"] = std::vector<Rcpp::List>();
      peaks_list["profile_baseline"] = std::vector<Rcpp::List>();
      peaks_list["profile_smoothed_intensity"] = std::vector<Rcpp::List>();
      peaks_list["profile_fitted_intensity"] = std::vector<Rcpp::List>();
      peaks_list["profile_fitted_rt"] = std::vector<Rcpp::List>();
      peaks_list["gaussian_A"] = std::vector<float>();
      peaks_list["gaussian_mu"] = std::vector<float>();
      peaks_list["gaussian_sigma"] = std::vector<float>();
      peaks_list["gaussian_rsquared"] = std::vector<float>();
    }
    peaks_list.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

    // Create combined output list
    // Rcpp::List combined_list;
    // combined_list["peaks"] = peaks_list;
    // combined_list["spectra"] = spectra_list;

    feature_list[analyses[a]] = peaks_list;
  }
  return feature_list;
};
