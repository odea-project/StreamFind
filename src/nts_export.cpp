#include <vector>
#include <cctype>
#include <cstdlib>
#include <cmath>
#include <Rcpp.h>
#include "nts/nts.h"

namespace
{
  std::string trim_copy(const std::string &s)
  {
    size_t start = 0;
    while (start < s.size() && std::isspace(static_cast<unsigned char>(s[start])))
      ++start;
    size_t end = s.size();
    while (end > start && std::isspace(static_cast<unsigned char>(s[end - 1])))
      --end;
    return s.substr(start, end - start);
  }

  std::vector<std::string> split_string(const std::string &s, char delimiter)
  {
    std::vector<std::string> out;
    std::string cur;
    for (char c : s)
    {
      if (c == delimiter)
      {
        out.push_back(cur);
        cur.clear();
      }
      else
      {
        cur.push_back(c);
      }
    }
    out.push_back(cur);
    return out;
  }

  Rcpp::List get_empty_dt()
  {
    Rcpp::List out = Rcpp::List::create();
    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
  }

  bool check_list_must_have_names(
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

    for (int i = 0; i < must_have_names_size; ++i)
    {
      if (!has_must_have_names[i])
      {
        return false;
      }
    }

    return true;
  }

  nts::NTS_INFO as_nts_info(const Rcpp::List &info)
  {
    nts::NTS_INFO out;
    std::vector<std::string> info_must_have_names = {
        "analysis", "replicate", "blank", "file"};

    if (!check_list_must_have_names(info, info_must_have_names))
    {
      Rcpp::Rcout << "Error: NTS_DATA() - missing required names in the info list." << std::endl;
      return out;
    }

    out.analyses = Rcpp::as<std::vector<std::string>>(info["analysis"]);
    out.replicates = Rcpp::as<std::vector<std::string>>(info["replicate"]);
    out.blanks = Rcpp::as<std::vector<std::string>>(info["blank"]);
    out.files = Rcpp::as<std::vector<std::string>>(info["file"]);
    return out;
  }

  sc::MS_SPECTRA_HEADERS as_MS_SPECTRA_HEADERS(const Rcpp::List &hd)
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

  std::vector<sc::MS_SPECTRA_HEADERS> as_spectra_headers(const Rcpp::List &spectra_headers)
  {
    std::vector<sc::MS_SPECTRA_HEADERS> out;
    if (spectra_headers.size() == 0)
    {
      return out;
    }
    out.resize(spectra_headers.size());
    for (int i = 0; i < spectra_headers.size(); ++i)
    {
      const Rcpp::List &header_ref = Rcpp::as<Rcpp::List>(spectra_headers[i]);
      out[i] = as_MS_SPECTRA_HEADERS(header_ref);
    }
    return out;
  }

  nts::FEATURES features_from_list(const Rcpp::List &fts)
  {
    nts::FEATURES out;
    if (fts.size() == 0)
    {
      return out;
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

    if (!check_list_must_have_names(fts, must_have_names))
    {
      Rcpp::Rcout << "Error: FEATURES::import_from_list() - missing required names in the list." << std::endl;
      return out;
    }

    out.feature = Rcpp::as<std::vector<std::string>>(fts["feature"]);
    out.feature_group = Rcpp::as<std::vector<std::string>>(fts["feature_group"]);
    out.feature_component = Rcpp::as<std::vector<std::string>>(fts["feature_component"]);
    out.adduct = Rcpp::as<std::vector<std::string>>(fts["adduct"]);
    out.rt = Rcpp::as<std::vector<float>>(fts["rt"]);
    out.mz = Rcpp::as<std::vector<float>>(fts["mz"]);
    out.mass = Rcpp::as<std::vector<float>>(fts["mass"]);
    out.intensity = Rcpp::as<std::vector<float>>(fts["intensity"]);
    out.noise = Rcpp::as<std::vector<float>>(fts["noise"]);
    out.sn = Rcpp::as<std::vector<float>>(fts["sn"]);
    out.area = Rcpp::as<std::vector<float>>(fts["area"]);
    out.rtmin = Rcpp::as<std::vector<float>>(fts["rtmin"]);
    out.rtmax = Rcpp::as<std::vector<float>>(fts["rtmax"]);
    out.width = Rcpp::as<std::vector<float>>(fts["width"]);
    out.mzmin = Rcpp::as<std::vector<float>>(fts["mzmin"]);
    out.mzmax = Rcpp::as<std::vector<float>>(fts["mzmax"]);
    out.ppm = Rcpp::as<std::vector<float>>(fts["ppm"]);
    out.fwhm_rt = Rcpp::as<std::vector<float>>(fts["fwhm_rt"]);
    out.fwhm_mz = Rcpp::as<std::vector<float>>(fts["fwhm_mz"]);
    out.gaussian_A = Rcpp::as<std::vector<float>>(fts["gaussian_A"]);
    out.gaussian_mu = Rcpp::as<std::vector<float>>(fts["gaussian_mu"]);
    out.gaussian_sigma = Rcpp::as<std::vector<float>>(fts["gaussian_sigma"]);
    out.gaussian_r2 = Rcpp::as<std::vector<float>>(fts["gaussian_r2"]);
    out.jaggedness = Rcpp::as<std::vector<float>>(fts["jaggedness"]);
    out.sharpness = Rcpp::as<std::vector<float>>(fts["sharpness"]);
    out.asymmetry = Rcpp::as<std::vector<float>>(fts["asymmetry"]);
    out.modality = Rcpp::as<std::vector<int>>(fts["modality"]);
    out.plates = Rcpp::as<std::vector<float>>(fts["plates"]);
    out.polarity = Rcpp::as<std::vector<int>>(fts["polarity"]);
    out.filtered = Rcpp::as<std::vector<bool>>(fts["filtered"]);
    out.filter = Rcpp::as<std::vector<std::string>>(fts["filter"]);
    out.filled = Rcpp::as<std::vector<bool>>(fts["filled"]);
    out.correction = Rcpp::as<std::vector<float>>(fts["correction"]);
    out.eic_size = Rcpp::as<std::vector<int>>(fts["eic_size"]);
    out.eic_rt = Rcpp::as<std::vector<std::string>>(fts["eic_rt"]);
    out.eic_mz = Rcpp::as<std::vector<std::string>>(fts["eic_mz"]);
    out.eic_intensity = Rcpp::as<std::vector<std::string>>(fts["eic_intensity"]);
    out.eic_baseline = Rcpp::as<std::vector<std::string>>(fts["eic_baseline"]);
    out.eic_smoothed = Rcpp::as<std::vector<std::string>>(fts["eic_smoothed"]);
    out.ms1_size = Rcpp::as<std::vector<int>>(fts["ms1_size"]);
    out.ms1_mz = Rcpp::as<std::vector<std::string>>(fts["ms1_mz"]);
    out.ms1_intensity = Rcpp::as<std::vector<std::string>>(fts["ms1_intensity"]);
    out.ms2_size = Rcpp::as<std::vector<int>>(fts["ms2_size"]);
    out.ms2_mz = Rcpp::as<std::vector<std::string>>(fts["ms2_mz"]);
    out.ms2_intensity = Rcpp::as<std::vector<std::string>>(fts["ms2_intensity"]);

    const size_t n = out.feature.size();
    auto check_size = [&](const char *name, size_t sz) {
      if (sz != n)
      {
        Rcpp::stop(std::string("features_from_list: column '") + name +
                   "' length " + std::to_string(sz) +
                   " does not match feature length " + std::to_string(n));
      }
    };

    check_size("feature_group", out.feature_group.size());
    check_size("feature_component", out.feature_component.size());
    check_size("adduct", out.adduct.size());
    check_size("rt", out.rt.size());
    check_size("mz", out.mz.size());
    check_size("mass", out.mass.size());
    check_size("intensity", out.intensity.size());
    check_size("noise", out.noise.size());
    check_size("sn", out.sn.size());
    check_size("area", out.area.size());
    check_size("rtmin", out.rtmin.size());
    check_size("rtmax", out.rtmax.size());
    check_size("width", out.width.size());
    check_size("mzmin", out.mzmin.size());
    check_size("mzmax", out.mzmax.size());
    check_size("ppm", out.ppm.size());
    check_size("fwhm_rt", out.fwhm_rt.size());
    check_size("fwhm_mz", out.fwhm_mz.size());
    check_size("gaussian_A", out.gaussian_A.size());
    check_size("gaussian_mu", out.gaussian_mu.size());
    check_size("gaussian_sigma", out.gaussian_sigma.size());
    check_size("gaussian_r2", out.gaussian_r2.size());
    check_size("jaggedness", out.jaggedness.size());
    check_size("sharpness", out.sharpness.size());
    check_size("asymmetry", out.asymmetry.size());
    check_size("modality", out.modality.size());
    check_size("plates", out.plates.size());
    check_size("polarity", out.polarity.size());
    check_size("filtered", out.filtered.size());
    check_size("filter", out.filter.size());
    check_size("filled", out.filled.size());
    check_size("correction", out.correction.size());
    check_size("eic_size", out.eic_size.size());
    check_size("eic_rt", out.eic_rt.size());
    check_size("eic_mz", out.eic_mz.size());
    check_size("eic_intensity", out.eic_intensity.size());
    check_size("eic_baseline", out.eic_baseline.size());
    check_size("eic_smoothed", out.eic_smoothed.size());
    check_size("ms1_size", out.ms1_size.size());
    check_size("ms1_mz", out.ms1_mz.size());
    check_size("ms1_intensity", out.ms1_intensity.size());
    check_size("ms2_size", out.ms2_size.size());
    check_size("ms2_mz", out.ms2_mz.size());
    check_size("ms2_intensity", out.ms2_intensity.size());

    return out;
  }

  std::vector<nts::FEATURES> as_feature_list(const Rcpp::List &feature_list)
  {
    std::vector<nts::FEATURES> out;
    if (feature_list.size() == 0)
    {
      return out;
    }
    out.resize(feature_list.size());
    for (int i = 0; i < feature_list.size(); ++i)
    {
      const Rcpp::List &feature_ref = Rcpp::as<Rcpp::List>(feature_list[i]);
      out[i] = features_from_list(feature_ref);
    }
    return out;
  }

  std::vector<nts::alignment::InternalStandard> as_internal_standards(const Rcpp::List &internal_standards_list)
  {
    std::vector<nts::alignment::InternalStandard> out;
    if (internal_standards_list.size() == 0)
    {
      return out;
    }

    Rcpp::CharacterVector istd_analysis = internal_standards_list["analysis"];
    Rcpp::CharacterVector istd_name = internal_standards_list["name"];
    Rcpp::NumericVector istd_exp_rt = internal_standards_list["exp_rt"];
    Rcpp::NumericVector istd_avg_rt = internal_standards_list["avg_exp_rt"];
    Rcpp::NumericVector istd_rt_shift = internal_standards_list["rt_shift"];

    for (int i = 0; i < istd_analysis.size(); ++i)
    {
      nts::alignment::InternalStandard istd;
      istd.analysis = Rcpp::as<std::string>(istd_analysis[i]);
      istd.name = Rcpp::as<std::string>(istd_name[i]);
      istd.exp_rt = istd_exp_rt[i];
      istd.avg_rt = istd_avg_rt[i];
      istd.rt_shift = istd_rt_shift[i];
      out.push_back(istd);
    }

    return out;
  }

  Rcpp::List features_to_list_dt(const nts::FEATURES &fts)
  {
    int n = fts.feature.size();
    if (n == 0)
    {
      return get_empty_dt();
    }

    Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("feature") = fts.feature,
        Rcpp::Named("feature_group") = fts.feature_group,
        Rcpp::Named("feature_component") = fts.feature_component,
        Rcpp::Named("adduct") = fts.adduct,
        Rcpp::Named("rt") = fts.rt,
        Rcpp::Named("mz") = fts.mz,
        Rcpp::Named("mass") = fts.mass,
        Rcpp::Named("intensity") = fts.intensity,
        Rcpp::Named("noise") = fts.noise,
        Rcpp::Named("sn") = fts.sn,
        Rcpp::Named("area") = fts.area,
        Rcpp::Named("rtmin") = fts.rtmin,
        Rcpp::Named("rtmax") = fts.rtmax,
        Rcpp::Named("width") = fts.width,
        Rcpp::Named("mzmin") = fts.mzmin,
        Rcpp::Named("mzmax") = fts.mzmax,
        Rcpp::Named("ppm") = fts.ppm,
        Rcpp::Named("fwhm_rt") = fts.fwhm_rt,
        Rcpp::Named("fwhm_mz") = fts.fwhm_mz,
        Rcpp::Named("gaussian_A") = fts.gaussian_A,
        Rcpp::Named("gaussian_mu") = fts.gaussian_mu,
        Rcpp::Named("gaussian_sigma") = fts.gaussian_sigma,
        Rcpp::Named("gaussian_r2") = fts.gaussian_r2,
        Rcpp::Named("jaggedness") = fts.jaggedness,
        Rcpp::Named("sharpness") = fts.sharpness,
        Rcpp::Named("asymmetry") = fts.asymmetry,
        Rcpp::Named("modality") = fts.modality,
        Rcpp::Named("plates") = fts.plates,
        Rcpp::Named("polarity") = fts.polarity,
        Rcpp::Named("filtered") = fts.filtered,
        Rcpp::Named("filter") = fts.filter,
        Rcpp::Named("filled") = fts.filled,
        Rcpp::Named("correction") = fts.correction,
        Rcpp::Named("eic_size") = fts.eic_size,
        Rcpp::Named("eic_rt") = fts.eic_rt,
        Rcpp::Named("eic_mz") = fts.eic_mz,
        Rcpp::Named("eic_intensity") = fts.eic_intensity,
        Rcpp::Named("eic_baseline") = fts.eic_baseline,
        Rcpp::Named("eic_smoothed") = fts.eic_smoothed,
        Rcpp::Named("ms1_size") = fts.ms1_size,
        Rcpp::Named("ms1_mz") = fts.ms1_mz,
        Rcpp::Named("ms1_intensity") = fts.ms1_intensity,
        Rcpp::Named("ms2_size") = fts.ms2_size,
        Rcpp::Named("ms2_mz") = fts.ms2_mz,
        Rcpp::Named("ms2_intensity") = fts.ms2_intensity);

    out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
    return out;
  }

  Rcpp::List features_as_list_of_dt(const nts::NTS_DATA &nts_data)
  {
    const int n = nts_data.features.size();
    Rcpp::List out(n);
    if (n == 0)
    {
      return out;
    }
    for (int i = 0; i < n; i++)
    {
      out[i] = features_to_list_dt(nts_data.features[i]);
    }
    Rcpp::CharacterVector names(n);
    for (int i = 0; i < n; i++)
    {
      names[i] = nts_data.analyses[i];
    }
    out.attr("names") = names;
    return out;
  }

  std::vector<nts::suspect_screening::SuspectQuery> as_suspect_queries(const Rcpp::List &suspects_list)
  {
    std::vector<nts::suspect_screening::SuspectQuery> out;
    if (suspects_list.size() == 0)
      return out;

    Rcpp::DataFrame df(suspects_list);
    const int n = df.nrows();
    if (n == 0)
      return out;

    auto has_col = [&](const std::string &name) -> bool {
      return df.containsElementNamed(name.c_str());
    };

    const bool has_mass = has_col("mass") || has_col("neutralMass");

    Rcpp::CharacterVector name_col = has_col("name") ? df["name"] : Rcpp::CharacterVector(n, "");
    Rcpp::NumericVector mass_col = has_col("mass") ? df["mass"] : (has_col("neutralMass") ? df["neutralMass"] : Rcpp::NumericVector(n, NA_REAL));
    Rcpp::NumericVector rt_col = has_col("rt") ? df["rt"] : Rcpp::NumericVector(n, NA_REAL);
    Rcpp::CharacterVector formula_col = has_col("formula") ? df["formula"] : Rcpp::CharacterVector(n, "");
    Rcpp::CharacterVector smiles_col = has_col("SMILES") ? df["SMILES"] : Rcpp::CharacterVector(n, "");
    Rcpp::CharacterVector inchi_col = has_col("InChI") ? df["InChI"] : Rcpp::CharacterVector(n, "");
    Rcpp::CharacterVector inchikey_col = has_col("InChIKey") ? df["InChIKey"] : Rcpp::CharacterVector(n, "");
    Rcpp::NumericVector score_col = has_col("score") ? df["score"] : Rcpp::NumericVector(n, NA_REAL);

    auto get_xlogp = [&](int i, bool &has_val) -> double {
      std::vector<std::string> cols = {"xLogP", "XLogP", "xlogp", "logp", "LogP"};
      for (const auto &col : cols)
      {
        if (has_col(col))
        {
          Rcpp::NumericVector v = df[col];
          if (!Rcpp::NumericVector::is_na(v[i]))
          {
            has_val = true;
            return v[i];
          }
        }
      }
      has_val = false;
      return 0.0;
    };

    auto get_database_id = [&](int i) -> std::string {
      std::vector<std::string> cols = {"database_id", "id", "ID"};
      for (const auto &col : cols)
      {
        if (has_col(col))
        {
          Rcpp::CharacterVector v = df[col];
          if (!Rcpp::CharacterVector::is_na(v[i]))
          {
            std::string val = Rcpp::as<std::string>(v[i]);
            if (!val.empty())
              return val;
          }
        }
      }
      return "";
    };

    Rcpp::CharacterVector fragments_col = has_col("fragments") ? df["fragments"] : Rcpp::CharacterVector(n, "");
    Rcpp::CharacterVector fragments_mz_col = has_col("fragments_mz") ? df["fragments_mz"] : Rcpp::CharacterVector(n, "");
    Rcpp::CharacterVector fragments_int_col = has_col("fragments_int") ? df["fragments_int"] : Rcpp::CharacterVector(n, "");
    Rcpp::CharacterVector ms2_pos_col = has_col("ms2_positive") ? df["ms2_positive"] : Rcpp::CharacterVector(n, "");
    Rcpp::CharacterVector ms2_neg_col = has_col("ms2_negative") ? df["ms2_negative"] : Rcpp::CharacterVector(n, "");
    Rcpp::CharacterVector ms2_col = has_col("ms2") ? df["ms2"] : Rcpp::CharacterVector(n, "");

    out.reserve(n);
    for (int i = 0; i < n; ++i)
    {
      nts::suspect_screening::SuspectQuery s;
      if (!Rcpp::CharacterVector::is_na(name_col[i]))
        s.name = Rcpp::as<std::string>(name_col[i]);

      if (has_mass && !Rcpp::NumericVector::is_na(mass_col[i]))
      {
        s.has_mass = true;
        s.mass = mass_col[i];
      }
      if (!Rcpp::NumericVector::is_na(rt_col[i]))
        s.rt = rt_col[i];

      if (!Rcpp::CharacterVector::is_na(formula_col[i]))
        s.formula = Rcpp::as<std::string>(formula_col[i]);
      if (!Rcpp::CharacterVector::is_na(smiles_col[i]))
        s.SMILES = Rcpp::as<std::string>(smiles_col[i]);
      if (!Rcpp::CharacterVector::is_na(inchi_col[i]))
        s.InChI = Rcpp::as<std::string>(inchi_col[i]);
      if (!Rcpp::CharacterVector::is_na(inchikey_col[i]))
        s.InChIKey = Rcpp::as<std::string>(inchikey_col[i]);

      if (!Rcpp::NumericVector::is_na(score_col[i]))
        s.score = score_col[i];

      bool has_xlogp = false;
      double xlogp = get_xlogp(i, has_xlogp);
      s.has_xLogP = has_xlogp;
      s.xLogP = xlogp;

      s.database_id = get_database_id(i);

      std::string fragments = (!Rcpp::CharacterVector::is_na(fragments_col[i])) ? Rcpp::as<std::string>(fragments_col[i]) : "";
      std::string fragments_mz = (!Rcpp::CharacterVector::is_na(fragments_mz_col[i])) ? Rcpp::as<std::string>(fragments_mz_col[i]) : "";
      std::string fragments_int = (!Rcpp::CharacterVector::is_na(fragments_int_col[i])) ? Rcpp::as<std::string>(fragments_int_col[i]) : "";
      std::string ms2_pos = (!Rcpp::CharacterVector::is_na(ms2_pos_col[i])) ? Rcpp::as<std::string>(ms2_pos_col[i]) : "";
      std::string ms2_neg = (!Rcpp::CharacterVector::is_na(ms2_neg_col[i])) ? Rcpp::as<std::string>(ms2_neg_col[i]) : "";
      std::string ms2 = (!Rcpp::CharacterVector::is_na(ms2_col[i])) ? Rcpp::as<std::string>(ms2_col[i]) : "";

      auto parse_ms2_pairs = [&](const std::string &src, std::vector<double> &mz_out, std::vector<double> &int_out) {
        if (src.empty())
          return;
        std::vector<std::string> parts = split_string(src, ';');
        for (auto &part : parts)
        {
          std::string token = trim_copy(part);
          if (token.empty())
            continue;
          std::vector<std::string> comps = split_string(token, ' ');
          if (comps.size() < 2)
            continue;
          double mz = std::atof(comps[0].c_str());
          double inten = std::atof(comps[1].c_str());
          mz_out.push_back(mz);
          int_out.push_back(inten);
        }
      };

      if (!ms2_pos.empty())
      {
        parse_ms2_pairs(ms2_pos, s.fragments_mz_pos, s.fragments_intensity_pos);
      }
      if (!ms2_neg.empty())
      {
        parse_ms2_pairs(ms2_neg, s.fragments_mz_neg, s.fragments_intensity_neg);
      }

      if (ms2_pos.empty() && ms2_neg.empty())
      {
        if (!ms2.empty())
        {
          parse_ms2_pairs(ms2, s.fragments_mz_pos, s.fragments_intensity_pos);
          parse_ms2_pairs(ms2, s.fragments_mz_neg, s.fragments_intensity_neg);
        }
        else if (!fragments.empty())
        {
          parse_ms2_pairs(fragments, s.fragments_mz_pos, s.fragments_intensity_pos);
          parse_ms2_pairs(fragments, s.fragments_mz_neg, s.fragments_intensity_neg);
        }
        else if (!fragments_mz.empty())
        {
          std::vector<std::string> mz_vals = split_string(fragments_mz, ';');
          std::vector<std::string> int_vals = split_string(fragments_int, ';');

          std::vector<double> mz_out;
          std::vector<double> int_out;
          mz_out.reserve(mz_vals.size());
          int_out.reserve(mz_vals.size());

          for (size_t k = 0; k < mz_vals.size(); ++k)
          {
            double mz = std::atof(trim_copy(mz_vals[k]).c_str());
            double inten = 0.0;
            if (k < int_vals.size())
              inten = std::atof(trim_copy(int_vals[k]).c_str());
            mz_out.push_back(mz);
            int_out.push_back(inten);
          }

          s.fragments_mz_pos = mz_out;
          s.fragments_intensity_pos = int_out;
          s.fragments_mz_neg = std::move(mz_out);
          s.fragments_intensity_neg = std::move(int_out);
        }
      }

      out.push_back(std::move(s));
    }

    return out;
  }

  Rcpp::CharacterVector as_char_vector(const std::vector<std::string> &values, bool na_on_empty = false)
  {
    Rcpp::CharacterVector out(values.size());
    for (size_t i = 0; i < values.size(); ++i)
    {
      if (na_on_empty && values[i].empty())
      {
        out[i] = NA_STRING;
      }
      else
      {
        out[i] = values[i];
      }
    }
    return out;
  }

  Rcpp::NumericVector as_numeric_vector(const std::vector<double> &values, bool na_on_nan = true)
  {
    Rcpp::NumericVector out(values.size());
    for (size_t i = 0; i < values.size(); ++i)
    {
      if (na_on_nan && std::isnan(values[i]))
      {
        out[i] = NA_REAL;
      }
      else
      {
        out[i] = values[i];
      }
    }
    return out;
  }
} // namespace

// MARK: rcpp_nts_find_features2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_find_features2(Rcpp::List info,
                                   Rcpp::List spectra_headers,
                                   std::vector<float> rtWindowsMin,
                                   std::vector<float> rtWindowsMax,
                                   float ppmThreshold = 15.0,
                                   float noiseThreshold = 15.0,
                                   float minSNR = 3.0,
                                   int minTraces = 3,
                                   float baselineWindow = 200.0,
                                   float maxWidth = 100.0,
                                   float baseQuantile = 0.10,
                                   std::string debugAnalysis = "",
                                   float debugMZ = 0.0,
                                   int debugSpecIdx = -1) {
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp;
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);
  nts_data.find_features(
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
    debugSpecIdx
  );
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_load_features_ms1_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_load_features_ms1_2(Rcpp::List info,
                                        Rcpp::List spectra_headers,
                                        Rcpp::List feature_list,
                                        bool filtered,
                                        std::vector<float> rtWindow,
                                        std::vector<float> mzWindow,
                                        float minTracesIntensity,
                                        float mzClust,
                                        float presence)
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);
  nts_data.load_features_ms1(
      filtered,
      rtWindow,
      mzWindow,
      minTracesIntensity,
      mzClust,
      presence);
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_load_features_ms2_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_load_features_ms2_2(Rcpp::List info,
                                        Rcpp::List spectra_headers,
                                        Rcpp::List feature_list,
                                        bool filtered,
                                        float minTracesIntensity,
                                        float isolationWindow,
                                        float mzClust,
                                        float presence)
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);
  nts_data.load_features_ms2(
      filtered,
      minTracesIntensity,
      isolationWindow,
      mzClust,
      presence);
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_create_components
// [[Rcpp::export]]
Rcpp::List rcpp_nts_create_components(Rcpp::List info,
                                      Rcpp::List spectra_headers,
                                      Rcpp::List feature_list,
                                      std::vector<float> rtWindow,
                                      float minCorrelation = 0.8,
                                      float debugRT = 0.0,
                                      std::string debugAnalysis = "")
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);
  nts_data.create_components(rtWindow, minCorrelation, debugRT, debugAnalysis);
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_annotate_components
// [[Rcpp::export]]
Rcpp::List rcpp_nts_annotate_components(Rcpp::List info,
                                        Rcpp::List spectra_headers,
                                        Rcpp::List feature_list,
                                        int maxIsotopes = 5,
                                        int maxCharge = 1,
                                        int maxGaps = 1,
                                        float ppm = 10.0,
                                        std::string debugComponent = "",
                                        std::string debugAnalysis = "")
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);
  nts_data.annotate_components(maxIsotopes, maxCharge, maxGaps, ppm, debugComponent, debugAnalysis);
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_group_features_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_group_features_2(Rcpp::List info,
                                     Rcpp::List spectra_headers,
                                     Rcpp::List feature_list,
                                     std::string method = "obi_warp",
                                     Rcpp::List internal_standards_list = R_NilValue,
                                     float rtDeviation = 5.0,
                                     float ppm = 5.0,
                                     int minSamples = 1,
                                     float binSize = 5.0,
                                     bool debug = false,
                                     float debugRT = 0.0)
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  std::vector<nts::alignment::InternalStandard> internal_standards_cpp = as_internal_standards(internal_standards_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);
  nts_data.group_features(method, internal_standards_cpp, rtDeviation, ppm, minSamples, binSize, debug, debugRT);
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_fill_features_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_fill_features_2(Rcpp::List info,
                                    Rcpp::List spectra_headers,
                                    Rcpp::List feature_list,
                                    bool withinReplicate = false,
                                    bool filtered = false,
                                    float rtExpand = 10.0,
                                    float mzExpand = 0.01,
                                    float maxPeakWidth = 30.0,
                                    float minTracesIntensity = 1000.0,
                                    int minNumberTraces = 5,
                                    float minIntensity = 5000.0,
                                    float rtApexDeviation = 5.0,
                                    float minSignalToNoiseRatio = 3.0,
                                    float minGaussianFit = 0.2,
                                    std::string debugFG = "")
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);
  nts_data.fill_features(
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
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_blank_subtraction_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_blank_subtraction_2(Rcpp::List info,
                                        Rcpp::List spectra_headers,
                                        Rcpp::List feature_list,
                                        float blankThreshold = 5.0,
                                        float rtExpand = 10.0,
                                        float mzExpand = 0.005)
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);
  nts_data.subtract_blank(blankThreshold, rtExpand, mzExpand);
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_filter_features_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_filter_features_2(
    Rcpp::List info,
    Rcpp::List feature_list,
    double minSN = NA_REAL,
    double minIntensity = NA_REAL,
    double minArea = NA_REAL,
    double minWidth = NA_REAL,
    double maxWidth = NA_REAL,
    double maxPPM = NA_REAL,
    double minFwhmRT = NA_REAL,
    double maxFwhmRT = NA_REAL,
    double minFwhmMZ = NA_REAL,
    double maxFwhmMZ = NA_REAL,
    double minGaussianA = NA_REAL,
    double minGaussianMu = NA_REAL,
    double maxGaussianMu = NA_REAL,
    double minGaussianSigma = NA_REAL,
    double maxGaussianSigma = NA_REAL,
    double minGaussianR2 = NA_REAL,
    double maxJaggedness = NA_REAL,
    double minSharpness = NA_REAL,
    double minAsymmetry = NA_REAL,
    double maxAsymmetry = NA_REAL,
    int maxModality = NA_INTEGER,
    double minPlates = NA_REAL,
    Rcpp::LogicalVector onlyFilled = Rcpp::LogicalVector::create(NA_LOGICAL),
    bool removeFilled = false,
    int minSizeEIC = NA_INTEGER,
    int minSizeMS1 = NA_INTEGER,
    int minSizeMS2 = NA_INTEGER,
    double minRelPresenceReplicate = NA_REAL,
    bool removeIsotopes = false,
    bool removeAdducts = false,
    bool removeLosses = false)
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp;
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);

  bool hasOnlyFilled = (onlyFilled.size() > 0 && onlyFilled[0] != NA_LOGICAL);
  bool onlyFilledValue = hasOnlyFilled ? static_cast<bool>(onlyFilled[0]) : false;
  bool hasMaxModality = (maxModality != NA_INTEGER);
  bool hasMinSizeEIC = (minSizeEIC != NA_INTEGER);
  bool hasMinSizeMS1 = (minSizeMS1 != NA_INTEGER);
  bool hasMinSizeMS2 = (minSizeMS2 != NA_INTEGER);

  nts_data.filter_features(
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
  return features_as_list_of_dt(nts_data);
};

// MARK: rcpp_nts_suspect_screening_2
// [[Rcpp::export]]
Rcpp::List rcpp_nts_suspect_screening_2(
    Rcpp::List info,
    Rcpp::List spectra_headers,
    Rcpp::List feature_list,
    Rcpp::List suspects,
    Rcpp::CharacterVector analyses = Rcpp::CharacterVector::create(""),
    double ppm = 5.0,
    double sec = 10.0,
    double ppmMS2 = 10.0,
    double mzrMS2 = 0.008,
    double minCosineSimilarity = 0.7,
    int minSharedFragments = 3,
    bool filtered = false)
{
  nts::NTS_INFO info_cpp = as_nts_info(info);
  std::vector<sc::MS_SPECTRA_HEADERS> headers_cpp = as_spectra_headers(spectra_headers);
  std::vector<nts::FEATURES> features_cpp = as_feature_list(feature_list);
  nts::NTS_DATA nts_data(info_cpp, headers_cpp, features_cpp);

  std::vector<std::string> analyses_sel;
  if (analyses.size() > 0 && analyses[0] != NA_STRING && Rcpp::as<std::string>(analyses[0]) != "")
  {
    analyses_sel = Rcpp::as<std::vector<std::string>>(analyses);
  }

  std::vector<nts::suspect_screening::SuspectQuery> suspects_cpp = as_suspect_queries(suspects);
  nts::SUSPECTS suspects_out = nts_data.suspect_screening(
      analyses_sel,
      suspects_cpp,
      ppm,
      sec,
      ppmMS2,
      mzrMS2,
      minCosineSimilarity,
      minSharedFragments,
      filtered);

  Rcpp::List out = Rcpp::List::create(
      Rcpp::Named("analysis") = as_char_vector(suspects_out.analysis),
      Rcpp::Named("feature") = as_char_vector(suspects_out.feature),
      Rcpp::Named("candidate_rank") = suspects_out.candidate_rank,
      Rcpp::Named("name") = as_char_vector(suspects_out.name),
      Rcpp::Named("polarity") = suspects_out.polarity,
      Rcpp::Named("db_mass") = as_numeric_vector(suspects_out.db_mass),
      Rcpp::Named("exp_mass") = as_numeric_vector(suspects_out.exp_mass),
      Rcpp::Named("error_mass") = as_numeric_vector(suspects_out.error_mass),
      Rcpp::Named("db_rt") = as_numeric_vector(suspects_out.db_rt),
      Rcpp::Named("exp_rt") = as_numeric_vector(suspects_out.exp_rt),
      Rcpp::Named("error_rt") = as_numeric_vector(suspects_out.error_rt),
      Rcpp::Named("intensity") = as_numeric_vector(suspects_out.intensity),
      Rcpp::Named("area") = as_numeric_vector(suspects_out.area),
      Rcpp::Named("id_level") = as_char_vector(suspects_out.id_level, true),
      Rcpp::Named("score") = as_numeric_vector(suspects_out.score),
      Rcpp::Named("shared_fragments") = suspects_out.shared_fragments,
      Rcpp::Named("cosine_similarity") = as_numeric_vector(suspects_out.cosine_similarity),
      Rcpp::Named("formula") = as_char_vector(suspects_out.formula, true),
      Rcpp::Named("SMILES") = as_char_vector(suspects_out.SMILES, true),
      Rcpp::Named("InChI") = as_char_vector(suspects_out.InChI, true),
      Rcpp::Named("InChIKey") = as_char_vector(suspects_out.InChIKey, true),
      Rcpp::Named("xLogP") = as_numeric_vector(suspects_out.xLogP),
      Rcpp::Named("database_id") = as_char_vector(suspects_out.database_id, true),
      Rcpp::Named("db_ms2_size") = suspects_out.db_ms2_size,
      Rcpp::Named("db_ms2_mz") = as_char_vector(suspects_out.db_ms2_mz, true),
      Rcpp::Named("db_ms2_intensity") = as_char_vector(suspects_out.db_ms2_intensity, true),
      Rcpp::Named("db_ms2_formula") = as_char_vector(suspects_out.db_ms2_formula, true),
      Rcpp::Named("exp_ms2_size") = suspects_out.exp_ms2_size,
      Rcpp::Named("exp_ms2_mz") = as_char_vector(suspects_out.exp_ms2_mz, true),
      Rcpp::Named("exp_ms2_intensity") = as_char_vector(suspects_out.exp_ms2_intensity, true));

  out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  return out;
};
