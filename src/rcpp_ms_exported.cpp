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
      
      if (ft_j.eic.rt.size() == 0 || ft_j.eic.feature == "") {
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
      
      if (ft_j.ms1.rt.size() == 0 || ft_j.ms1.feature == "") {
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
      
      if (ft_j.ms2.feature == "") {
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
  
  if (minNumberTraces < 5) minNumberTraces = 5;
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
        if (rt_j - rtmin_j < minHalfPeakWidth) rtmin_j = rt_j - minHalfPeakWidth;
        if (rtmax_j - rt_j < minHalfPeakWidth) rtmax_j = rt_j + minHalfPeakWidth;
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
      
      if (ft_j.eic.rt.size() == 0 || ft_j.feature == "") {
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
  
  if (minNumberTraces < 5) minNumberTraces = 5;
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
      if (rpl.second > max_presence) max_presence = rpl.second;
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
    
    if (indices.size() == 0) continue;
    
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
      
      if (has_analysis) continue;
      
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
      if (sd_rt_i == 0) sd_rt_i = (rtmax_i - rtmin_i) / 4;
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
      if (ft.polarity < 0) ft.adduct = "[M-H]-";
      
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
