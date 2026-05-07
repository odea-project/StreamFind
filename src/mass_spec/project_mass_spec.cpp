#include "project_mass_spec.h"

#include "../project/db_helpers.h"

#include <algorithm>
#include <cmath>
#include <filesystem>
#include <numeric>
#include <set>
#include <utility>

namespace mass_spec {

namespace {

using project::CONTEXT;
using project::ERROR;
using project::ERROR_CODE;
using project::PROJECT;
namespace detail = project::detail;

detail::CONNECTION_GUARD connect_checked(const std::shared_ptr<CONTEXT>& ctx) {
  if (!ctx || ctx->db_path.empty() || ctx->project_id.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project context is not initialized");
  }
  return detail::CONNECTION_GUARD(ctx);
}

std::string default_analysis_name(const std::string& file_path) {
  return std::filesystem::path(file_path).stem().string();
}

void bind_optional_varchar(duckdb_prepared_statement statement, idx_t index, const std::string& value) {
  if (value.empty()) {
    duckdb_bind_null(statement, index);
  } else {
    duckdb_bind_varchar(statement, index, value.c_str());
  }
}

std::vector<int> unique_levels(const std::vector<MS_SPECTRA_HEADER_ROW>& headers) {
  std::set<int> levels;
  for (const auto& header : headers) {
    levels.insert(header.level);
  }
  return std::vector<int>(levels.begin(), levels.end());
}

std::string polarity_to_string(int polarity) {
  if (polarity > 0) return "1";
  if (polarity < 0) return "-1";
  return "";
}

std::vector<std::string> collect_unique_polarities(const std::vector<MS_SPECTRA_HEADER_ROW>& headers) {
  std::set<std::string> out;
  for (const auto& header : headers) {
    const auto polarity = polarity_to_string(header.polarity);
    if (!polarity.empty()) {
      out.insert(polarity);
    }
  }
  return std::vector<std::string>(out.begin(), out.end());
}

std::vector<MS_RAW_SPECTRUM_ROW> flatten_target_spectra(const std::string& analysis,
                                                        const std::string& replicate,
                                                        const mass_spec::MS_TARGETS_SPECTRA& spectra) {
  std::vector<MS_RAW_SPECTRUM_ROW> out;
  out.reserve(spectra.size());
  for (std::size_t i = 0; i < spectra.size(); ++i) {
    MS_RAW_SPECTRUM_ROW row;
    row.analysis = analysis;
    row.replicate = replicate;
    row.id = spectra.id[i];
    row.polarity = spectra.polarity[i];
    row.level = spectra.level[i];
    row.pre_mz = spectra.pre_mz[i];
    row.pre_mzlow = spectra.pre_mzlow[i];
    row.pre_mzhigh = spectra.pre_mzhigh[i];
    row.pre_ce = spectra.pre_ce[i];
    row.rt = spectra.rt[i];
    row.mobility = spectra.mobility[i];
    row.mz = spectra.mz[i];
    row.intensity = spectra.intensity[i];
    out.push_back(row);
  }
  return out;
}

std::vector<MS_RAW_SPECTRUM_ROW> flatten_spectra(const std::string& analysis,
                                                 const std::string& replicate,
                                                 const std::vector<mass_spec::MS_SPECTRUM>& spectra,
                                                 float min_intensity_ms1,
                                                 float min_intensity_ms2) {
  std::vector<MS_RAW_SPECTRUM_ROW> out;
  for (const auto& spectrum : spectra) {
    if (spectrum.binary_data.size() < 2) {
      continue;
    }
    const auto& mz = spectrum.binary_data[0];
    const auto& intensity = spectrum.binary_data[1];
    const std::size_t size = std::min(mz.size(), intensity.size());
    for (std::size_t i = 0; i < size; ++i) {
      const float current_intensity = intensity[i];
      if (spectrum.level == 1 && current_intensity < min_intensity_ms1) continue;
      if (spectrum.level >= 2 && current_intensity < min_intensity_ms2) continue;
      MS_RAW_SPECTRUM_ROW row;
      row.analysis = analysis;
      row.replicate = replicate;
      row.polarity = spectrum.polarity;
      row.level = spectrum.level;
      row.pre_mz = spectrum.precursor_mz;
      row.pre_mzlow = spectrum.window_mzlow;
      row.pre_mzhigh = spectrum.window_mzhigh;
      row.pre_ce = spectrum.activation_ce;
      row.rt = spectrum.rt;
      row.mobility = spectrum.mobility;
      row.mz = mz[i];
      row.intensity = current_intensity;
      out.push_back(row);
    }
  }
  return out;
}

bool contains_level(const std::vector<int>& levels, int level) {
  return std::find(levels.begin(), levels.end(), level) != levels.end();
}

bool has_rt_window(double rt_min, double rt_max) {
  return rt_max > 0.0 && rt_max >= rt_min;
}

}  // namespace

PROJECT_MASS_SPEC::PROJECT_MASS_SPEC(std::shared_ptr<project::CONTEXT> ctx) : ctx_(std::move(ctx)) {
  PROJECT root(ctx_->db_path, ctx_->project_id);
  root.set_domain("MS");
  create_schema(ctx_);
  validate_schema(ctx_);
}

void PROJECT_MASS_SPEC::create_schema(const std::shared_ptr<project::CONTEXT>& ctx) {
  PROJECT root(ctx->db_path, ctx->project_id);
  root.set_domain("MS");

  auto guard = connect_checked(ctx);
  detail::run_sql(guard.get(),
                  "CREATE TABLE IF NOT EXISTS MS_ANALYSES (project_id VARCHAR NOT NULL, analysis VARCHAR NOT NULL, replicate VARCHAR, blank VARCHAR, file_name VARCHAR, file_path VARCHAR NOT NULL, file_dir VARCHAR, file_extension VARCHAR, format VARCHAR, type VARCHAR, time_stamp VARCHAR, number_spectra INTEGER, number_chromatograms INTEGER, number_spectra_binary_arrays INTEGER, min_mz DOUBLE, max_mz DOUBLE, start_rt DOUBLE, end_rt DOUBLE, has_ion_mobility BOOLEAN, concentration DOUBLE, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY(project_id, analysis))",
                  "create MS_ANALYSES table");
  detail::run_sql(guard.get(),
                  "CREATE TABLE IF NOT EXISTS MS_SPECTRA_HEADERS (project_id VARCHAR NOT NULL, analysis VARCHAR NOT NULL, index INTEGER NOT NULL, scan INTEGER, array_length INTEGER, level INTEGER, mode INTEGER, polarity INTEGER, configuration INTEGER, lowmz DOUBLE, highmz DOUBLE, bpmz DOUBLE, bpint DOUBLE, tic DOUBLE, rt DOUBLE, mobility DOUBLE, window_mz DOUBLE, window_mzlow DOUBLE, window_mzhigh DOUBLE, precursor_mz DOUBLE, precursor_intensity DOUBLE, precursor_charge INTEGER, activation_ce DOUBLE, PRIMARY KEY(project_id, analysis, index))",
                  "create MS_SPECTRA_HEADERS table");
  detail::run_sql(guard.get(),
                  "CREATE TABLE IF NOT EXISTS MS_CHROMATOGRAMS_HEADERS (project_id VARCHAR NOT NULL, analysis VARCHAR NOT NULL, index INTEGER NOT NULL, id VARCHAR, array_length INTEGER, polarity INTEGER, precursor_mz DOUBLE, activation_ce DOUBLE, product_mz DOUBLE, PRIMARY KEY(project_id, analysis, index))",
                  "create MS_CHROMATOGRAMS_HEADERS table");
}

void PROJECT_MASS_SPEC::validate_schema(const std::shared_ptr<project::CONTEXT>& ctx) {
  auto guard = connect_checked(ctx);
  detail::validate_columns(guard.get(), analyses_table_name(), {{"project_id", "VARCHAR", true},
                                                                {"analysis", "VARCHAR", true},
                                                                {"replicate", "VARCHAR", false},
                                                                {"blank", "VARCHAR", false},
                                                                {"file_name", "VARCHAR", false},
                                                                {"file_path", "VARCHAR", true},
                                                                {"file_dir", "VARCHAR", false},
                                                                {"file_extension", "VARCHAR", false},
                                                                {"format", "VARCHAR", false},
                                                                {"type", "VARCHAR", false},
                                                                {"time_stamp", "VARCHAR", false},
                                                                {"number_spectra", "INTEGER", false},
                                                                {"number_chromatograms", "INTEGER", false},
                                                                {"number_spectra_binary_arrays", "INTEGER", false},
                                                                {"min_mz", "DOUBLE", false},
                                                                {"max_mz", "DOUBLE", false},
                                                                {"start_rt", "DOUBLE", false},
                                                                {"end_rt", "DOUBLE", false},
                                                                {"has_ion_mobility", "BOOLEAN", false},
                                                                {"concentration", "DOUBLE", false},
                                                                {"created_at", "TIMESTAMP", false}});
  detail::validate_columns(guard.get(), spectra_headers_table_name(), {{"project_id", "VARCHAR", true},
                                                                       {"analysis", "VARCHAR", true},
                                                                       {"index", "INTEGER", true},
                                                                       {"scan", "INTEGER", false},
                                                                       {"array_length", "INTEGER", false},
                                                                       {"level", "INTEGER", false},
                                                                       {"mode", "INTEGER", false},
                                                                       {"polarity", "INTEGER", false},
                                                                       {"configuration", "INTEGER", false},
                                                                       {"lowmz", "DOUBLE", false},
                                                                       {"highmz", "DOUBLE", false},
                                                                       {"bpmz", "DOUBLE", false},
                                                                       {"bpint", "DOUBLE", false},
                                                                       {"tic", "DOUBLE", false},
                                                                       {"rt", "DOUBLE", false},
                                                                       {"mobility", "DOUBLE", false},
                                                                       {"window_mz", "DOUBLE", false},
                                                                       {"window_mzlow", "DOUBLE", false},
                                                                       {"window_mzhigh", "DOUBLE", false},
                                                                       {"precursor_mz", "DOUBLE", false},
                                                                       {"precursor_intensity", "DOUBLE", false},
                                                                       {"precursor_charge", "INTEGER", false},
                                                                       {"activation_ce", "DOUBLE", false}});
  detail::validate_columns(guard.get(), chromatograms_headers_table_name(), {{"project_id", "VARCHAR", true},
                                                                             {"analysis", "VARCHAR", true},
                                                                             {"index", "INTEGER", true},
                                                                             {"id", "VARCHAR", false},
                                                                             {"array_length", "INTEGER", false},
                                                                             {"polarity", "INTEGER", false},
                                                                             {"precursor_mz", "DOUBLE", false},
                                                                             {"activation_ce", "DOUBLE", false},
                                                                             {"product_mz", "DOUBLE", false}});
}

void PROJECT_MASS_SPEC::import_files(const std::vector<std::string>& file_paths,
                                     const std::vector<std::string>& analyses,
                                     const std::vector<std::string>& replicates,
                                     const std::vector<std::string>& blanks) {
  if (file_paths.empty()) {
    return;
  }
  if (!analyses.empty() && analyses.size() != file_paths.size()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec import_files analyses length must match file_paths");
  }
  if (!replicates.empty() && replicates.size() != file_paths.size()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec import_files replicates length must match file_paths");
  }
  if (!blanks.empty() && blanks.size() != file_paths.size()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec import_files blanks length must match file_paths");
  }

  PROJECT root(ctx_->db_path, ctx_->project_id);
  root.set_domain("MS");
  create_schema(ctx_);
  validate_schema(ctx_);

  auto guard = connect_checked(ctx_);
  detail::run_sql(guard.get(), "BEGIN TRANSACTION", "begin MS import_files transaction");
  try {
    for (std::size_t i = 0; i < file_paths.size(); ++i) {
      import_file_with_connection(guard.get(),
                                  file_paths[i],
                                  analyses.empty() ? std::string() : analyses[i],
                                  replicates.empty() ? std::string() : replicates[i],
                                  blanks.empty() ? std::string() : blanks[i]);
    }
    detail::run_sql(guard.get(), "COMMIT", "commit MS import_files transaction");
  } catch (...) {
    try {
      detail::run_sql(guard.get(), "ROLLBACK", "rollback MS import_files transaction");
    } catch (...) {
    }
    throw;
  }
}

void PROJECT_MASS_SPEC::import_file_with_connection(duckdb_connection con,
                                                    const std::string& file_path,
                                                    const std::string& analysis,
                                                    const std::string& replicate,
                                                    const std::string& blank) {
  if (file_path.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec import requires a file path");
  }
  const std::string analysis_name = analysis.empty() ? default_analysis_name(file_path) : analysis;
  if (analysis_name.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec import requires a non-empty analysis name");
  }

  mass_spec::MS_FILE file(file_path);
  const mass_spec::MS_SUMMARY summary = file.get_summary();
  const mass_spec::MS_SPECTRA_HEADERS spectra_headers = file.get_spectra_headers();
  const mass_spec::MS_CHROMATOGRAMS_HEADERS chromatograms_headers = file.get_chromatograms_headers();

  detail::run_prepared(con,
                       "DELETE FROM MS_SPECTRA_HEADERS WHERE project_id = ? AND analysis = ?",
                       "delete MS spectra headers",
                       [&](duckdb_prepared_statement statement) {
                         duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                         duckdb_bind_varchar(statement, 2, analysis_name.c_str());
                       },
                       [](duckdb_result&) {});
  detail::run_prepared(con,
                       "DELETE FROM MS_CHROMATOGRAMS_HEADERS WHERE project_id = ? AND analysis = ?",
                       "delete MS chromatogram headers",
                       [&](duckdb_prepared_statement statement) {
                         duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                         duckdb_bind_varchar(statement, 2, analysis_name.c_str());
                       },
                       [](duckdb_result&) {});
  detail::run_prepared(con,
                       "DELETE FROM MS_ANALYSES WHERE project_id = ? AND analysis = ?",
                       "delete MS analysis",
                       [&](duckdb_prepared_statement statement) {
                         duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                         duckdb_bind_varchar(statement, 2, analysis_name.c_str());
                       },
                       [](duckdb_result&) {});

  detail::run_prepared(con,
                       "INSERT INTO MS_ANALYSES (project_id, analysis, replicate, blank, file_name, file_path, file_dir, file_extension, format, type, time_stamp, number_spectra, number_chromatograms, number_spectra_binary_arrays, min_mz, max_mz, start_rt, end_rt, has_ion_mobility, concentration) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                       "insert MS analysis",
                       [&](duckdb_prepared_statement statement) {
                         duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                         duckdb_bind_varchar(statement, 2, analysis_name.c_str());
                         bind_optional_varchar(statement, 3, replicate);
                         bind_optional_varchar(statement, 4, blank);
                         bind_optional_varchar(statement, 5, summary.file_name);
                         duckdb_bind_varchar(statement, 6, summary.file_path.c_str());
                         bind_optional_varchar(statement, 7, summary.file_dir);
                         bind_optional_varchar(statement, 8, summary.file_extension);
                         bind_optional_varchar(statement, 9, summary.format);
                         bind_optional_varchar(statement, 10, summary.type);
                         bind_optional_varchar(statement, 11, summary.time_stamp);
                         duckdb_bind_int32(statement, 12, summary.number_spectra);
                         duckdb_bind_int32(statement, 13, summary.number_chromatograms);
                         duckdb_bind_int32(statement, 14, summary.number_spectra_binary_arrays);
                         duckdb_bind_double(statement, 15, summary.min_mz);
                         duckdb_bind_double(statement, 16, summary.max_mz);
                         duckdb_bind_double(statement, 17, summary.start_rt);
                         duckdb_bind_double(statement, 18, summary.end_rt);
                         duckdb_bind_boolean(statement, 19, summary.has_ion_mobility);
                         duckdb_bind_null(statement, 20);
                       },
                       [](duckdb_result&) {});

  for (std::size_t i = 0; i < spectra_headers.size(); ++i) {
    detail::run_prepared(con,
                         "INSERT INTO MS_SPECTRA_HEADERS (project_id, analysis, index, scan, array_length, level, mode, polarity, configuration, lowmz, highmz, bpmz, bpint, tic, rt, mobility, window_mz, window_mzlow, window_mzhigh, precursor_mz, precursor_intensity, precursor_charge, activation_ce) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                         "insert MS spectrum header",
                         [&](duckdb_prepared_statement statement) {
                           duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                           duckdb_bind_varchar(statement, 2, analysis_name.c_str());
                           duckdb_bind_int32(statement, 3, spectra_headers.index[i]);
                           duckdb_bind_int32(statement, 4, spectra_headers.scan[i]);
                           duckdb_bind_int32(statement, 5, spectra_headers.array_length[i]);
                           duckdb_bind_int32(statement, 6, spectra_headers.level[i]);
                           duckdb_bind_int32(statement, 7, spectra_headers.mode[i]);
                           duckdb_bind_int32(statement, 8, spectra_headers.polarity[i]);
                           duckdb_bind_int32(statement, 9, spectra_headers.configuration[i]);
                           duckdb_bind_double(statement, 10, spectra_headers.lowmz[i]);
                           duckdb_bind_double(statement, 11, spectra_headers.highmz[i]);
                           duckdb_bind_double(statement, 12, spectra_headers.bpmz[i]);
                           duckdb_bind_double(statement, 13, spectra_headers.bpint[i]);
                           duckdb_bind_double(statement, 14, spectra_headers.tic[i]);
                           duckdb_bind_double(statement, 15, spectra_headers.rt[i]);
                           duckdb_bind_double(statement, 16, spectra_headers.mobility[i]);
                           duckdb_bind_double(statement, 17, spectra_headers.window_mz[i]);
                           duckdb_bind_double(statement, 18, spectra_headers.window_mzlow[i]);
                           duckdb_bind_double(statement, 19, spectra_headers.window_mzhigh[i]);
                           duckdb_bind_double(statement, 20, spectra_headers.precursor_mz[i]);
                           duckdb_bind_double(statement, 21, spectra_headers.precursor_intensity[i]);
                           duckdb_bind_int32(statement, 22, spectra_headers.precursor_charge[i]);
                           duckdb_bind_double(statement, 23, spectra_headers.activation_ce[i]);
                         },
                         [](duckdb_result&) {});
  }

  for (std::size_t i = 0; i < chromatograms_headers.size(); ++i) {
    detail::run_prepared(con,
                         "INSERT INTO MS_CHROMATOGRAMS_HEADERS (project_id, analysis, index, id, array_length, polarity, precursor_mz, activation_ce, product_mz) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                         "insert MS chromatogram header",
                         [&](duckdb_prepared_statement statement) {
                           duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                           duckdb_bind_varchar(statement, 2, analysis_name.c_str());
                           duckdb_bind_int32(statement, 3, chromatograms_headers.index[i]);
                           bind_optional_varchar(statement, 4, chromatograms_headers.id[i]);
                           duckdb_bind_int32(statement, 5, chromatograms_headers.array_length[i]);
                           duckdb_bind_int32(statement, 6, chromatograms_headers.polarity[i]);
                           duckdb_bind_double(statement, 7, chromatograms_headers.precursor_mz[i]);
                           duckdb_bind_double(statement, 8, chromatograms_headers.activation_ce[i]);
                           duckdb_bind_double(statement, 9, chromatograms_headers.product_mz[i]);
                         },
                         [](duckdb_result&) {});
  }
}

void PROJECT_MASS_SPEC::remove_analysis(const std::string& analysis) {
  if (analysis.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec remove requires an analysis name");
  }

  auto guard = connect_checked(ctx_);
  detail::run_prepared(guard.get(),
                       "DELETE FROM MS_SPECTRA_HEADERS WHERE project_id = ? AND analysis = ?",
                       "delete MS spectra headers",
                       [&](duckdb_prepared_statement statement) {
                         duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                         duckdb_bind_varchar(statement, 2, analysis.c_str());
                       },
                       [](duckdb_result&) {});
  detail::run_prepared(guard.get(),
                       "DELETE FROM MS_CHROMATOGRAMS_HEADERS WHERE project_id = ? AND analysis = ?",
                       "delete MS chromatogram headers",
                       [&](duckdb_prepared_statement statement) {
                         duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                         duckdb_bind_varchar(statement, 2, analysis.c_str());
                       },
                       [](duckdb_result&) {});
  detail::run_prepared(guard.get(),
                       "DELETE FROM MS_ANALYSES WHERE project_id = ? AND analysis = ?",
                       "delete MS analysis",
                       [&](duckdb_prepared_statement statement) {
                         duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                         duckdb_bind_varchar(statement, 2, analysis.c_str());
                       },
                       [](duckdb_result&) {});
}

std::vector<MS_ANALYSIS_ROW> PROJECT_MASS_SPEC::get_analyses() const {
  auto guard = connect_checked(ctx_);
  std::vector<MS_ANALYSIS_ROW> out;
  detail::run_prepared(guard.get(),
                       "SELECT project_id, analysis, replicate, blank, file_name, file_path, file_dir, file_extension, format, type, time_stamp, number_spectra, number_chromatograms, number_spectra_binary_arrays, min_mz, max_mz, start_rt, end_rt, has_ion_mobility, concentration, created_at FROM MS_ANALYSES WHERE project_id = ? ORDER BY lower(analysis), analysis",
                       "query MS analyses",
                       [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str()); },
                       [&](duckdb_result& result) {
                         out = detail::rows_from_result(&result, [&](idx_t row) {
                           MS_ANALYSIS_ROW value;
                           value.project_id = detail::result_varchar(&result, 0, row);
                           value.analysis = detail::result_varchar(&result, 1, row);
                           value.replicate = detail::result_varchar(&result, 2, row);
                           value.blank = detail::result_varchar(&result, 3, row);
                           value.file_name = detail::result_varchar(&result, 4, row);
                           value.file_path = detail::result_varchar(&result, 5, row);
                           value.file_dir = detail::result_varchar(&result, 6, row);
                           value.file_extension = detail::result_varchar(&result, 7, row);
                           value.format = detail::result_varchar(&result, 8, row);
                           value.type = detail::result_varchar(&result, 9, row);
                           value.time_stamp = detail::result_varchar(&result, 10, row);
                           value.number_spectra = duckdb_value_int32(&result, 11, row);
                           value.number_chromatograms = duckdb_value_int32(&result, 12, row);
                           value.number_spectra_binary_arrays = duckdb_value_int32(&result, 13, row);
                           value.min_mz = duckdb_value_double(&result, 14, row);
                           value.max_mz = duckdb_value_double(&result, 15, row);
                           value.start_rt = duckdb_value_double(&result, 16, row);
                           value.end_rt = duckdb_value_double(&result, 17, row);
                           value.has_ion_mobility = duckdb_value_boolean(&result, 18, row) != 0;
                           value.concentration = duckdb_value_double(&result, 19, row);
                           value.created_at = detail::result_varchar(&result, 20, row);
                           return value;
                         });
                       });
  return out;
}

std::vector<std::string> PROJECT_MASS_SPEC::get_analysis_names() const {
  const auto rows = get_analyses();
  std::vector<std::string> out;
  out.reserve(rows.size());
  for (const auto& row : rows) {
    out.push_back(row.analysis);
  }
  return out;
}

std::vector<std::string> PROJECT_MASS_SPEC::get_replicate_names() const {
  const auto rows = get_analyses();
  std::vector<std::string> out;
  out.reserve(rows.size());
  for (const auto& row : rows) {
    out.push_back(row.replicate);
  }
  return out;
}

std::vector<std::string> PROJECT_MASS_SPEC::get_blank_names() const {
  const auto rows = get_analyses();
  std::vector<std::string> out;
  out.reserve(rows.size());
  for (const auto& row : rows) {
    out.push_back(row.blank);
  }
  return out;
}

std::vector<double> PROJECT_MASS_SPEC::get_concentrations() const {
  const auto rows = get_analyses();
  std::vector<double> out;
  out.reserve(rows.size());
  for (const auto& row : rows) {
    out.push_back(row.concentration);
  }
  return out;
}

void PROJECT_MASS_SPEC::set_replicate_names(const std::vector<std::string>& values) {
  const auto names = get_analysis_names();
  if (values.size() != names.size()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec replicate names length must match analyses");
  }

  auto guard = connect_checked(ctx_);
  detail::run_sql(guard.get(), "BEGIN TRANSACTION", "begin MS set replicate names transaction");
  try {
    for (std::size_t i = 0; i < names.size(); ++i) {
      detail::run_prepared(guard.get(),
                           "UPDATE MS_ANALYSES SET replicate = ? WHERE project_id = ? AND analysis = ?",
                           "update MS replicate name",
                           [&](duckdb_prepared_statement statement) {
                             bind_optional_varchar(statement, 1, values[i]);
                             duckdb_bind_varchar(statement, 2, ctx_->project_id.c_str());
                             duckdb_bind_varchar(statement, 3, names[i].c_str());
                           },
                           [](duckdb_result&) {});
    }
    detail::run_sql(guard.get(), "COMMIT", "commit MS set replicate names transaction");
  } catch (...) {
    try {
      detail::run_sql(guard.get(), "ROLLBACK", "rollback MS set replicate names transaction");
    } catch (...) {
    }
    throw;
  }
}

void PROJECT_MASS_SPEC::set_blank_names(const std::vector<std::string>& values) {
  const auto names = get_analysis_names();
  if (values.size() != names.size()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec blank names length must match analyses");
  }

  auto guard = connect_checked(ctx_);
  detail::run_sql(guard.get(), "BEGIN TRANSACTION", "begin MS set blank names transaction");
  try {
    for (std::size_t i = 0; i < names.size(); ++i) {
      detail::run_prepared(guard.get(),
                           "UPDATE MS_ANALYSES SET blank = ? WHERE project_id = ? AND analysis = ?",
                           "update MS blank name",
                           [&](duckdb_prepared_statement statement) {
                             bind_optional_varchar(statement, 1, values[i]);
                             duckdb_bind_varchar(statement, 2, ctx_->project_id.c_str());
                             duckdb_bind_varchar(statement, 3, names[i].c_str());
                           },
                           [](duckdb_result&) {});
    }
    detail::run_sql(guard.get(), "COMMIT", "commit MS set blank names transaction");
  } catch (...) {
    try {
      detail::run_sql(guard.get(), "ROLLBACK", "rollback MS set blank names transaction");
    } catch (...) {
    }
    throw;
  }
}

void PROJECT_MASS_SPEC::set_concentrations(const std::vector<double>& values) {
  const auto names = get_analysis_names();
  if (values.size() != names.size()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec concentrations length must match analyses");
  }

  auto guard = connect_checked(ctx_);
  detail::run_sql(guard.get(), "BEGIN TRANSACTION", "begin MS set concentrations transaction");
  try {
    for (std::size_t i = 0; i < names.size(); ++i) {
      detail::run_prepared(guard.get(),
                           "UPDATE MS_ANALYSES SET concentration = ? WHERE project_id = ? AND analysis = ?",
                           "update MS concentration",
                           [&](duckdb_prepared_statement statement) {
                             duckdb_bind_double(statement, 1, values[i]);
                             duckdb_bind_varchar(statement, 2, ctx_->project_id.c_str());
                             duckdb_bind_varchar(statement, 3, names[i].c_str());
                           },
                           [](duckdb_result&) {});
    }
    detail::run_sql(guard.get(), "COMMIT", "commit MS set concentrations transaction");
  } catch (...) {
    try {
      detail::run_sql(guard.get(), "ROLLBACK", "rollback MS set concentrations transaction");
    } catch (...) {
    }
    throw;
  }
}

std::vector<MS_SPECTRA_HEADER_ROW> PROJECT_MASS_SPEC::get_spectra_headers(const std::vector<std::string>& analyses) const {
  auto guard = connect_checked(ctx_);
  std::vector<MS_SPECTRA_HEADER_ROW> out;
  const auto selected_analyses = mass_spec::sanitize_analyses(analyses);
  std::string sql = "SELECT project_id, analysis, index, scan, array_length, level, mode, polarity, configuration, lowmz, highmz, bpmz, bpint, tic, rt, mobility, window_mz, window_mzlow, window_mzhigh, precursor_mz, precursor_intensity, precursor_charge, activation_ce FROM MS_SPECTRA_HEADERS WHERE project_id = ?";
  if (!selected_analyses.empty()) {
    sql += " AND analysis IN (";
    for (std::size_t i = 0; i < selected_analyses.size(); ++i) {
      if (i > 0) {
        sql += ", ";
      }
      sql += "?";
    }
    sql += ")";
  }
  sql += " ORDER BY lower(analysis), analysis, index";
  detail::run_prepared(guard.get(),
                       sql,
                       "query MS spectra headers",
                       [&](duckdb_prepared_statement statement) {
                          idx_t bind_index = 1;
                          duckdb_bind_varchar(statement, bind_index++, ctx_->project_id.c_str());
                          for (const auto& analysis : selected_analyses) {
                            duckdb_bind_varchar(statement, bind_index++, analysis.c_str());
                          }
                        },
                       [&](duckdb_result& result) {
                         out = detail::rows_from_result(&result, [&](idx_t row) {
                           MS_SPECTRA_HEADER_ROW value;
                           value.project_id = detail::result_varchar(&result, 0, row);
                           value.analysis = detail::result_varchar(&result, 1, row);
                           value.index = duckdb_value_int32(&result, 2, row);
                           value.scan = duckdb_value_int32(&result, 3, row);
                           value.array_length = duckdb_value_int32(&result, 4, row);
                           value.level = duckdb_value_int32(&result, 5, row);
                           value.mode = duckdb_value_int32(&result, 6, row);
                           value.polarity = duckdb_value_int32(&result, 7, row);
                           value.configuration = duckdb_value_int32(&result, 8, row);
                           value.lowmz = duckdb_value_double(&result, 9, row);
                           value.highmz = duckdb_value_double(&result, 10, row);
                           value.bpmz = duckdb_value_double(&result, 11, row);
                           value.bpint = duckdb_value_double(&result, 12, row);
                           value.tic = duckdb_value_double(&result, 13, row);
                           value.rt = duckdb_value_double(&result, 14, row);
                           value.mobility = duckdb_value_double(&result, 15, row);
                           value.window_mz = duckdb_value_double(&result, 16, row);
                           value.window_mzlow = duckdb_value_double(&result, 17, row);
                           value.window_mzhigh = duckdb_value_double(&result, 18, row);
                           value.precursor_mz = duckdb_value_double(&result, 19, row);
                           value.precursor_intensity = duckdb_value_double(&result, 20, row);
                           value.precursor_charge = duckdb_value_int32(&result, 21, row);
                           value.activation_ce = duckdb_value_double(&result, 22, row);
                           return value;
                         });
                        });
  return out;
}

std::vector<MS_CHROMATOGRAM_HEADER_ROW> PROJECT_MASS_SPEC::get_chromatograms_headers(const std::vector<std::string>& analyses) const {
  auto guard = connect_checked(ctx_);
  std::vector<MS_CHROMATOGRAM_HEADER_ROW> out;
  const auto selected_analyses = mass_spec::sanitize_analyses(analyses);
  std::string sql = "SELECT project_id, analysis, index, id, array_length, polarity, precursor_mz, activation_ce, product_mz FROM MS_CHROMATOGRAMS_HEADERS WHERE project_id = ?";
  if (!selected_analyses.empty()) {
    sql += " AND analysis IN (";
    for (std::size_t i = 0; i < selected_analyses.size(); ++i) {
      if (i > 0) {
        sql += ", ";
      }
      sql += "?";
    }
    sql += ")";
  }
  sql += " ORDER BY lower(analysis), analysis, index";
  detail::run_prepared(guard.get(),
                       sql,
                       "query MS chromatogram headers",
                       [&](duckdb_prepared_statement statement) {
                          idx_t bind_index = 1;
                          duckdb_bind_varchar(statement, bind_index++, ctx_->project_id.c_str());
                          for (const auto& analysis : selected_analyses) {
                            duckdb_bind_varchar(statement, bind_index++, analysis.c_str());
                          }
                        },
                       [&](duckdb_result& result) {
                         out = detail::rows_from_result(&result, [&](idx_t row) {
                           MS_CHROMATOGRAM_HEADER_ROW value;
                           value.project_id = detail::result_varchar(&result, 0, row);
                           value.analysis = detail::result_varchar(&result, 1, row);
                           value.index = duckdb_value_int32(&result, 2, row);
                           value.id = detail::result_varchar(&result, 3, row);
                           value.array_length = duckdb_value_int32(&result, 4, row);
                           value.polarity = duckdb_value_int32(&result, 5, row);
                           value.precursor_mz = duckdb_value_double(&result, 6, row);
                           value.activation_ce = duckdb_value_double(&result, 7, row);
                           value.product_mz = duckdb_value_double(&result, 8, row);
                           return value;
                         });
                        });
  return out;
}

std::vector<MS_SPECTRA_TIC_ROW> PROJECT_MASS_SPEC::get_spectra_tic(const std::vector<std::string>& analyses,
                                                                   const std::vector<int>& levels,
                                                                   double rt_min,
                                                                   double rt_max) const {
  auto guard = connect_checked(ctx_);
  std::vector<MS_SPECTRA_TIC_ROW> out;
  const auto selected_analyses = mass_spec::sanitize_analyses(analyses);
  const bool filter_analyses = !selected_analyses.empty();
  const bool filter_rt = has_rt_window(rt_min, rt_max);
  std::string sql = "SELECT h.analysis, a.replicate, h.polarity, h.level, h.rt, h.tic, h.bpmz, h.bpint FROM MS_SPECTRA_HEADERS h LEFT JOIN MS_ANALYSES a ON a.project_id = h.project_id AND a.analysis = h.analysis WHERE h.project_id = ?";
  if (filter_analyses) {
    sql += " AND h.analysis IN (";
    for (std::size_t i = 0; i < selected_analyses.size(); ++i) {
      if (i > 0) {
        sql += ", ";
      }
      sql += "?";
    }
    sql += ")";
  }
  if (!levels.empty()) {
    sql += " AND h.level IN (";
    for (std::size_t i = 0; i < levels.size(); ++i) {
      if (i > 0) {
        sql += ", ";
      }
      sql += "?";
    }
    sql += ")";
  }
  if (filter_rt) {
    sql += " AND h.rt >= ? AND h.rt <= ?";
  }
  sql += " ORDER BY lower(h.analysis), h.analysis, h.index";
  detail::run_prepared(guard.get(),
                       sql,
                       "query MS spectra traces",
                       [&](duckdb_prepared_statement statement) {
                          idx_t bind_index = 1;
                          duckdb_bind_varchar(statement, bind_index++, ctx_->project_id.c_str());
                          for (const auto& analysis : selected_analyses) {
                            duckdb_bind_varchar(statement, bind_index++, analysis.c_str());
                          }
                          for (int level : levels) {
                            duckdb_bind_int32(statement, bind_index++, level);
                          }
                          if (filter_rt) {
                            duckdb_bind_double(statement, bind_index++, rt_min);
                            duckdb_bind_double(statement, bind_index++, rt_max);
                          }
                        },
                       [&](duckdb_result& result) {
                         out = detail::rows_from_result(&result, [&](idx_t row) {
                            MS_SPECTRA_TIC_ROW value;
                           value.analysis = detail::result_varchar(&result, 0, row);
                           value.replicate = detail::result_varchar(&result, 1, row);
                           value.polarity = duckdb_value_int32(&result, 2, row);
                           value.level = duckdb_value_int32(&result, 3, row);
                           value.rt = duckdb_value_double(&result, 4, row);
                           value.tic = duckdb_value_double(&result, 5, row);
                           value.bpmz = duckdb_value_double(&result, 6, row);
                           value.bpint = duckdb_value_double(&result, 7, row);
                           return value;
                         });
                        });
  return out;
}

std::vector<MS_RAW_SPECTRUM_ROW> PROJECT_MASS_SPEC::get_raw_spectra(const MS_TARGETS_REQUEST& request) const {
  const auto analyses_rows = this->get_analyses();
  const auto selected_analyses = sanitize_analyses(request.analyses);
  if (selected_analyses.empty()) {
    return {};
  }

  const auto headers = get_spectra_headers(selected_analyses);
  auto targets_by_analysis = build_targets_by_analysis(request, selected_analyses, collect_unique_polarities(headers));
  if (!targets_by_analysis.empty() && targets_by_analysis.size() != selected_analyses.size()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Mass spec raw_spectra targets must match analyses");
  }

  std::vector<int> selected_levels = request.levels;
  if (selected_levels.empty()) {
    if (headers.empty()) {
      return {};
    }
    selected_levels = unique_levels(headers);
  }

  bool all_traces = request.all_traces;
  if (!contains_level(selected_levels, 2)) {
    all_traces = true;
  }

  std::vector<MS_RAW_SPECTRUM_ROW> out;
  for (std::size_t analysis_index = 0; analysis_index < selected_analyses.size(); ++analysis_index) {
    const auto& analysis = selected_analyses[analysis_index];
    const auto analysis_it = std::find_if(analyses_rows.begin(), analyses_rows.end(), [&](const auto& row) {
      return row.analysis == analysis;
    });
    if (analysis_it == analyses_rows.end()) {
      continue;
    }

    MS_TARGETS analysis_targets;
    analysis_targets.resize_all(0);
    if (!targets_by_analysis.empty()) {
      analysis_targets = subset_targets(targets_by_analysis[analysis_index],
                                        selected_levels,
                                        all_traces,
                                        request.isolation_window);
    }

    const auto headers_rows = get_spectra_headers({analysis});
    if (headers_rows.empty()) {
      continue;
    }

    MS_FILE file(analysis_it->file_path);

    if (!has_effective_targets(analysis_targets)) {
      std::vector<int> indices;
      indices.reserve(headers_rows.size());
      for (const auto& header : headers_rows) {
        bool keep = selected_levels.empty();
        for (int level : selected_levels) {
          if (header.level == level) {
            keep = true;
            break;
          }
        }
        if (keep && header.configuration < 3) {
          indices.push_back(header.index);
        }
      }

      for (int index : indices) {
        const auto spectrum = file.get_spectrum(index);
        if (spectrum.binary_data.size() < 2) {
          continue;
        }
        const auto& mz = spectrum.binary_data[0];
        const auto& intensity = spectrum.binary_data[1];
        const std::size_t size = std::min(mz.size(), intensity.size());
        for (std::size_t i = 0; i < size; ++i) {
          const float current_intensity = intensity[i];
          if (spectrum.level == 1 && current_intensity < request.min_intensity_ms1) continue;
          if (spectrum.level >= 2 && current_intensity < request.min_intensity_ms2) continue;
          MS_RAW_SPECTRUM_ROW row;
          row.analysis = analysis;
          row.replicate = analysis_it->replicate;
          row.polarity = spectrum.polarity;
          row.level = spectrum.level;
          row.pre_mz = spectrum.precursor_mz;
          row.pre_mzlow = spectrum.window_mzlow;
          row.pre_mzhigh = spectrum.window_mzhigh;
          row.pre_ce = spectrum.activation_ce;
          row.rt = spectrum.rt;
          row.mobility = spectrum.mobility;
          row.mz = mz[i];
          row.intensity = current_intensity;
          out.push_back(row);
        }
      }
      continue;
    }

    MS_SPECTRA_HEADERS headers_native;
    headers_native.resize_all(static_cast<int>(headers_rows.size()));
    for (std::size_t i = 0; i < headers_rows.size(); ++i) {
      headers_native.index[i] = headers_rows[i].index;
      headers_native.scan[i] = headers_rows[i].scan;
      headers_native.array_length[i] = headers_rows[i].array_length;
      headers_native.level[i] = headers_rows[i].level;
      headers_native.mode[i] = headers_rows[i].mode;
      headers_native.polarity[i] = headers_rows[i].polarity;
      headers_native.configuration[i] = headers_rows[i].configuration;
      headers_native.lowmz[i] = static_cast<float>(headers_rows[i].lowmz);
      headers_native.highmz[i] = static_cast<float>(headers_rows[i].highmz);
      headers_native.bpmz[i] = static_cast<float>(headers_rows[i].bpmz);
      headers_native.bpint[i] = static_cast<float>(headers_rows[i].bpint);
      headers_native.tic[i] = static_cast<float>(headers_rows[i].tic);
      headers_native.rt[i] = static_cast<float>(headers_rows[i].rt);
      headers_native.mobility[i] = static_cast<float>(headers_rows[i].mobility);
      headers_native.window_mz[i] = static_cast<float>(headers_rows[i].window_mz);
      headers_native.window_mzlow[i] = static_cast<float>(headers_rows[i].window_mzlow);
      headers_native.window_mzhigh[i] = static_cast<float>(headers_rows[i].window_mzhigh);
      headers_native.precursor_mz[i] = static_cast<float>(headers_rows[i].precursor_mz);
      headers_native.precursor_intensity[i] = static_cast<float>(headers_rows[i].precursor_intensity);
      headers_native.precursor_charge[i] = headers_rows[i].precursor_charge;
      headers_native.activation_ce[i] = static_cast<float>(headers_rows[i].activation_ce);
    }

    const auto spectra = file.get_spectra_targets(
        analysis_targets,
        headers_native,
        request.min_intensity_ms1,
        request.min_intensity_ms2);
    out.reserve(out.size() + spectra.size());
    for (std::size_t i = 0; i < spectra.size(); ++i) {
      MS_RAW_SPECTRUM_ROW row;
      row.analysis = analysis;
      row.replicate = analysis_it->replicate;
      row.id = spectra.id[i];
      row.polarity = spectra.polarity[i];
      row.level = spectra.level[i];
      row.pre_mz = spectra.pre_mz[i];
      row.pre_mzlow = spectra.pre_mzlow[i];
      row.pre_mzhigh = spectra.pre_mzhigh[i];
      row.pre_ce = spectra.pre_ce[i];
      row.rt = spectra.rt[i];
      row.mobility = spectra.mobility[i];
      row.mz = spectra.mz[i];
      row.intensity = spectra.intensity[i];
      out.push_back(row);
    }
  }

  std::sort(out.begin(), out.end(), [](const auto& lhs, const auto& rhs) {
    if (lhs.analysis != rhs.analysis) return lhs.analysis < rhs.analysis;
    if (lhs.id != rhs.id) return lhs.id < rhs.id;
    if (lhs.rt != rhs.rt) return lhs.rt < rhs.rt;
    return lhs.mz < rhs.mz;
  });
  return out;
}

std::vector<std::vector<std::vector<float>>> PROJECT_MASS_SPEC::get_chromatograms_data(const std::string& analysis,
                                                                                        const std::vector<int>& indices) const {
  const auto analyses_rows = this->get_analyses();
  const auto analysis_it = std::find_if(analyses_rows.begin(), analyses_rows.end(), [&](const auto& row) {
    return row.analysis == analysis;
  });
  if (analysis_it == analyses_rows.end()) {
    return {};
  }

  MS_FILE file(analysis_it->file_path);
  return file.get_chromatograms(indices);
}

}  // namespace ms
