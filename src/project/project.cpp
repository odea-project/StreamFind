#include "project.h"

#include "audit_trail.h"
#include "db_helpers.h"
#include "cache.h"
#include <filesystem>
#include <utility>

namespace project {

namespace {

std::filesystem::path normalized_path(const std::string& value) {
  return std::filesystem::absolute(std::filesystem::path(value)).lexically_normal();
}

std::string normalize_domain(std::string value) {
  if (value.empty()) {
    return value;
  }
  std::transform(value.begin(), value.end(), value.begin(), [](unsigned char ch) {
    return static_cast<char>(std::toupper(ch));
  });
  return value;
}

void validate_domain_code(const std::string& value) {
  const std::string domain = normalize_domain(value);
  if (domain.empty()) {
    throw project::ERROR(project::ERROR_CODE::InvalidArgument, "Project domain must not be empty");
  }
  if (domain != "MS" && domain != "RAMAN" && domain != "STAT") {
    throw project::ERROR(project::ERROR_CODE::InvalidArgument, "Unsupported project domain: " + value);
  }
}

}  // namespace

static std::shared_ptr<CONTEXT> require_context(const std::shared_ptr<CONTEXT>& ctx) {
  if (!ctx || ctx->db_path.empty() || ctx->project_id.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project context is not initialized");
  }
  return ctx;
}

static project::detail::CONNECTION_GUARD connect(const std::shared_ptr<CONTEXT>& ctx) {
  return project::detail::CONNECTION_GUARD(require_context(ctx));
}

static void ensure_database_domain_compatible(const std::shared_ptr<CONTEXT>& ctx, const std::string& value) {
  const std::string domain = normalize_domain(value);
  if (domain.empty()) {
    return;
  }

  auto guard = connect(ctx);
  std::optional<std::string> existing;
  project::detail::run_prepared(guard.get(),
                                "SELECT domain FROM Project WHERE domain IS NOT NULL AND domain <> '' GROUP BY domain ORDER BY domain LIMIT 1",
                                "load database domain",
                                [](duckdb_prepared_statement) {},
                                [&](duckdb_result& result) {
                                  if (duckdb_row_count(&result) == 0) {
                                    return;
                                  }
                                  existing = normalize_domain(project::detail::result_varchar(&result, 0, 0));
                                });
  if (existing && *existing != domain) {
    throw ERROR(ERROR_CODE::InvalidArgument,
                "Database domain is already set to " + *existing + " and cannot be changed to " + domain);
  }
}

ERROR::ERROR(ERROR_CODE code, std::string message) : std::runtime_error(std::move(message)), code_(code) {}

ERROR_CODE ERROR::code() const noexcept {
  return code_;
}

void PROJECT::create_schema(const std::shared_ptr<CONTEXT>& ctx) {
  auto guard = connect(ctx);
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS Project (project_id VARCHAR NOT NULL PRIMARY KEY, domain VARCHAR, metadata JSON, workflow JSON, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
          "create Project table");

  const auto columns = project::detail::table_columns(guard.get(), "Project");
  const auto has_domain = std::find_if(columns.begin(), columns.end(), [](const project::detail::COLUMN_INFO& info) {
    return info.name == "domain";
  });
  if (has_domain == columns.end()) {
    project::detail::run_sql(guard.get(), "ALTER TABLE Project ADD COLUMN domain VARCHAR", "add Project domain column");
  }
}

void PROJECT::validate_schema(const std::shared_ptr<CONTEXT>& ctx) {
  auto guard = connect(ctx);
  project::detail::validate_columns_present(guard.get(), "Project", {{"project_id", "VARCHAR", true},
                                                                      {"domain", "VARCHAR", false},
                                                                      {"metadata", "JSON", false},
                                                                      {"workflow", "JSON", false},
                                                                      {"created_at", "TIMESTAMP", false}});
}

void PROJECT::ensure_row_exists(const std::shared_ptr<CONTEXT>& ctx) {
  auto guard = connect(ctx);
  project::detail::run_prepared(guard.get(),
                "INSERT INTO Project (project_id, domain, metadata, workflow) VALUES (?, ?, ?, ?) ON CONFLICT(project_id) DO NOTHING",
                "insert Project row",
                [&](duckdb_prepared_statement statement) {
                  duckdb_bind_varchar(statement, 1, ctx->project_id.c_str());
                  duckdb_bind_null(statement, 2);
                  const std::string metadata = project::detail::json_to_text(json::object());
                  const std::string workflow = project::detail::json_to_text(json::array());
                  duckdb_bind_varchar(statement, 3, metadata.c_str());
                  duckdb_bind_varchar(statement, 4, workflow.c_str());
                },
               [](duckdb_result&) {});
}

PROJECT_ROW PROJECT::read_row(const std::shared_ptr<CONTEXT>& ctx) {
  auto guard = connect(ctx);
  std::optional<PROJECT_ROW> out;
  project::detail::run_prepared(guard.get(),
                                "SELECT project_id, domain, metadata, workflow, created_at FROM Project WHERE project_id = ? LIMIT 1",
                                "load Project row",
                                [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, ctx->project_id.c_str()); },
                                [&](duckdb_result& result) {
                                  if (duckdb_row_count(&result) == 0) {
                                    return;
                                  }
                                  PROJECT_ROW row;
                                  row.project_id = project::detail::result_varchar(&result, 0, 0);
                                  row.domain = normalize_domain(project::detail::result_varchar(&result, 1, 0));
                                  row.metadata = project::detail::json_from_text(project::detail::result_varchar(&result, 2, 0));
                                  row.workflow = project::detail::json_from_text(project::detail::result_varchar(&result, 3, 0));
                                  row.created_at = project::detail::result_varchar(&result, 4, 0);
                                  out = std::move(row);
                                });
  if (!out) {
    throw ERROR(ERROR_CODE::NotFound, "Project row not found: " + ctx->project_id);
  }
  return *out;
}

void PROJECT::update_row(const std::shared_ptr<CONTEXT>& ctx, const PROJECT_ROW& row) {
  if (row.project_id != ctx->project_id) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project row id does not match the active project");
  }

  PROJECT_ROW value = row;
  value.domain = normalize_domain(value.domain);
  if (!value.domain.empty()) {
    validate_domain_code(value.domain);
  }

  auto guard = connect(ctx);
  project::detail::run_prepared(guard.get(),
                "INSERT INTO Project (project_id, domain, metadata, workflow) VALUES (?, ?, ?, ?) ON CONFLICT(project_id) DO UPDATE SET domain = excluded.domain, metadata = excluded.metadata, workflow = excluded.workflow",
                "update Project row",
                [&](duckdb_prepared_statement statement) {
                  duckdb_bind_varchar(statement, 1, value.project_id.c_str());
                  if (value.domain.empty()) {
                    duckdb_bind_null(statement, 2);
                  } else {
                    duckdb_bind_varchar(statement, 2, value.domain.c_str());
                  }
                  const std::string metadata = project::detail::json_to_text(value.metadata);
                  const std::string workflow = project::detail::json_to_text(value.workflow);
                  duckdb_bind_varchar(statement, 3, metadata.c_str());
                  duckdb_bind_varchar(statement, 4, workflow.c_str());
                },
               [](duckdb_result&) {});
}

PROJECT::PROJECT(std::string db_path, std::string project_id)
    : ctx_(std::make_shared<CONTEXT>()) {
  if (db_path.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project requires a database path");
  }
  if (project_id.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project requires a project_id");
  }

  try {
    const auto parent = std::filesystem::path(db_path).parent_path();
    if (!parent.empty()) {
      std::filesystem::create_directories(parent);
    }
  } catch (const std::filesystem::filesystem_error& e) {
    throw ERROR(ERROR_CODE::Io, std::string("Failed to prepare database directory: ") + e.what());
  }

  ctx_->db_path = std::move(db_path);
  ctx_->project_id = std::move(project_id);

  auto guard = connect(ctx_);
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS Project (project_id VARCHAR NOT NULL PRIMARY KEY, domain VARCHAR, metadata JSON, workflow JSON, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
          "create Project table");
  const auto columns = project::detail::table_columns(guard.get(), "Project");
  const auto has_domain = std::find_if(columns.begin(), columns.end(), [](const project::detail::COLUMN_INFO& info) {
    return info.name == "domain";
  });
  if (has_domain == columns.end()) {
    project::detail::run_sql(guard.get(), "ALTER TABLE Project ADD COLUMN domain VARCHAR", "add Project domain column");
  }
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS Cache (project_id VARCHAR NOT NULL, name VARCHAR NOT NULL, description VARCHAR NOT NULL, hash VARCHAR NOT NULL, data BLOB NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY(project_id, hash))",
          "create Cache table");
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS AuditTrail (project_id VARCHAR NOT NULL, operation_type VARCHAR NOT NULL, object_type VARCHAR NOT NULL, operation_details JSON, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
          "create AuditTrail table");

  project::detail::run_prepared(guard.get(),
                "INSERT INTO Project (project_id, domain, metadata, workflow) VALUES (?, ?, ?, ?) ON CONFLICT(project_id) DO NOTHING",
                "insert Project row",
                [&](duckdb_prepared_statement statement) {
                  duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                  duckdb_bind_null(statement, 2);
                  const std::string metadata = project::detail::json_to_text(json::object());
                  const std::string workflow = project::detail::json_to_text(json::array());
                  duckdb_bind_varchar(statement, 3, metadata.c_str());
                  duckdb_bind_varchar(statement, 4, workflow.c_str());
                },
               [](duckdb_result&) {});
}

PROJECT::~PROJECT() {
}

const std::string& PROJECT::db_path() const noexcept {
  return ctx_->db_path;
}

const std::string& PROJECT::project_id() const noexcept {
  return ctx_->project_id;
}

const std::shared_ptr<CONTEXT>& PROJECT::context() const noexcept {
  return ctx_;
}

PROJECT_ROW PROJECT::row() const {
  create_schema(ctx_);
  ensure_row_exists(ctx_);
  return read_row(ctx_);
}

void PROJECT::set_row(const PROJECT_ROW& row) {
  create_schema(ctx_);
  PROJECT_ROW value = row;
  if (value.project_id.empty()) {
    value.project_id = ctx_->project_id;
  }
  if (value.project_id != ctx_->project_id) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project row id does not match the active project");
  }
  ensure_row_exists(ctx_);
  update_row(ctx_, value);
}

json PROJECT::metadata() const {
  create_schema(ctx_);
  return row().metadata;
}

std::string PROJECT::domain() const {
  create_schema(ctx_);
  return row().domain;
}

void PROJECT::set_metadata(const json& value) {
  create_schema(ctx_);
  PROJECT_ROW current = row();
  current.metadata = value;
  update_row(ctx_, current);
}

void PROJECT::set_domain(const std::string& value) {
  create_schema(ctx_);
  const std::string normalized = normalize_domain(value);
  validate_domain_code(normalized);
  ensure_database_domain_compatible(ctx_, normalized);
  PROJECT_ROW current = row();
  if (!current.domain.empty() && current.domain != normalized) {
    throw ERROR(ERROR_CODE::InvalidArgument,
                "Project domain is already set to " + current.domain + " and cannot be changed to " + normalized);
  }
  current.domain = normalized;
  update_row(ctx_, current);
}

json PROJECT::workflow() const {
  create_schema(ctx_);
  return row().workflow;
}

void PROJECT::set_workflow(const json& value) {
  create_schema(ctx_);
  PROJECT_ROW current = row();
  current.workflow = value;
  update_row(ctx_, current);
}

void PROJECT::validate() const {
  create_schema(ctx_);
  validate_schema(ctx_);
  ensure_row_exists(ctx_);
  const PROJECT_ROW current = row();
  if (!current.domain.empty()) {
    validate_domain_code(current.domain);
    ensure_database_domain_compatible(ctx_, current.domain);
  }
}

std::vector<std::string> PROJECT::list_tables() const {
  auto guard = connect(ctx_);
  duckdb_result result{};
  if (duckdb_query(guard.get(),
                   "SELECT table_name FROM information_schema.tables WHERE table_schema = 'main' AND table_type = 'BASE TABLE' ORDER BY table_name",
                   &result) == DuckDBError) {
    std::string message = project::detail::result_error(&result);
    duckdb_destroy_result(&result);
    throw ERROR(ERROR_CODE::DuckDB, std::string("list tables: ") + message);
  }
  project::detail::RESULT_GUARD result_guard(&result);

  std::vector<std::string> tables;
  const idx_t count = duckdb_row_count(&result);
  tables.reserve(static_cast<std::size_t>(count));
  for (idx_t row = 0; row < count; ++row) {
    tables.push_back(project::detail::result_varchar(&result, 0, row));
  }
  return tables;
}

std::vector<AUDIT_TRAIL_ROW> PROJECT::get_audit() const {
  auto guard = connect(ctx_);
  std::vector<AUDIT_TRAIL_ROW> out;
  project::detail::run_prepared(guard.get(),
               "SELECT project_id, operation_type, object_type, operation_details, created_at FROM AuditTrail WHERE project_id = ? ORDER BY created_at DESC",
               "query AuditTrail",
               [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str()); },
               [&](duckdb_result& result) {
                 out = project::detail::rows_from_result(&result, [&](idx_t row) {
                    AUDIT_TRAIL_ROW value;
                   value.project_id = project::detail::result_varchar(&result, 0, row);
                   value.operation_type = project::detail::result_varchar(&result, 1, row);
                   value.object_type = project::detail::result_varchar(&result, 2, row);
                   value.operation_details = project::detail::json_from_text(project::detail::result_varchar(&result, 3, row));
                   value.created_at = project::detail::result_varchar(&result, 4, row);
                   return value;
                 });
               });
  return out;
}

PROJECT* PROJECT::copy(std::string db_path, std::string project_id) const {
  if (db_path.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project copy requires a database path");
  }
  if (project_id.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project copy requires a project_id");
  }

  const auto source_path = normalized_path(ctx_->db_path);
  const auto target_path = normalized_path(db_path);
  if (source_path == target_path && project_id == ctx_->project_id) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Project copy requires a different database path or project_id");
  }

  auto target = std::make_unique<PROJECT>(std::move(db_path), std::move(project_id));

  const PROJECT_ROW source_row = row();
  if (!source_row.domain.empty()) {
    target->set_domain(source_row.domain);
  }
  target->set_metadata(source_row.metadata);
  target->set_workflow(source_row.workflow);

  CACHE source_cache(ctx_);
  CACHE target_cache(target->ctx_);
  target_cache.clear();
  for (const auto& entry : source_cache.all()) {
    CACHE::ROW_TYPE copy = entry;
    copy.project_id = target->project_id();
    target_cache.put(copy);
  }

  AUDIT_TRAIL source_audit(ctx_);
  AUDIT_TRAIL target_audit(target->ctx_);
  target_audit.clear();
  for (const auto& entry : source_audit.all()) {
    target_audit.add(entry.operation_type, entry.object_type, entry.operation_details);
  }

  return target.release();
}

}  // namespace project
