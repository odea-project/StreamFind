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

}  // namespace

static std::shared_ptr<Context> require_context(const std::shared_ptr<Context>& ctx) {
  if (!ctx || ctx->db_path.empty() || ctx->project_id.empty()) {
    throw Error(ErrorCode::InvalidArgument, "Project context is not initialized");
  }
  return ctx;
}

static project::detail::ConnectionGuard connect(const std::shared_ptr<Context>& ctx) {
  return project::detail::ConnectionGuard(require_context(ctx));
}

Error::Error(ErrorCode code, std::string message) : std::runtime_error(std::move(message)), code_(code) {}

ErrorCode Error::code() const noexcept {
  return code_;
}

void Project::create_schema(const std::shared_ptr<Context>& ctx) {
  auto guard = connect(ctx);
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS Project (project_id VARCHAR NOT NULL PRIMARY KEY, metadata JSON, workflow JSON, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
          "create Project table");
}

void Project::validate_schema(const std::shared_ptr<Context>& ctx) {
  auto guard = connect(ctx);
  project::detail::validate_columns(guard.get(), "Project", {{"project_id", "VARCHAR", true},
                                                              {"metadata", "JSON", false},
                                                              {"workflow", "JSON", false},
                                                              {"created_at", "TIMESTAMP", false}});
}

void Project::ensure_row_exists(const std::shared_ptr<Context>& ctx) {
  auto guard = connect(ctx);
  project::detail::run_prepared(guard.get(),
                "INSERT INTO Project (project_id, metadata, workflow) VALUES (?, ?, ?) ON CONFLICT(project_id) DO NOTHING",
                "insert Project row",
                [&](duckdb_prepared_statement statement) {
                  duckdb_bind_varchar(statement, 1, ctx->project_id.c_str());
                  const std::string metadata = project::detail::json_to_text(json::object());
                  const std::string workflow = project::detail::json_to_text(json::array());
                  duckdb_bind_varchar(statement, 2, metadata.c_str());
                  duckdb_bind_varchar(statement, 3, workflow.c_str());
                },
               [](duckdb_result&) {});
}

ProjectRow Project::read_row(const std::shared_ptr<Context>& ctx) {
  auto guard = connect(ctx);
  std::optional<ProjectRow> out;
  project::detail::run_prepared(guard.get(),
                                "SELECT project_id, metadata, workflow, created_at FROM Project WHERE project_id = ? LIMIT 1",
                                "load Project row",
                                [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, ctx->project_id.c_str()); },
                                [&](duckdb_result& result) {
                                  if (duckdb_row_count(&result) == 0) {
                                    return;
                                  }
                                  ProjectRow row;
                                  row.project_id = project::detail::result_varchar(&result, 0, 0);
                                  row.metadata = project::detail::json_from_text(project::detail::result_varchar(&result, 1, 0));
                                  row.workflow = project::detail::json_from_text(project::detail::result_varchar(&result, 2, 0));
                                  row.created_at = project::detail::result_varchar(&result, 3, 0);
                                  out = std::move(row);
                                });
  if (!out) {
    throw Error(ErrorCode::NotFound, "Project row not found: " + ctx->project_id);
  }
  return *out;
}

void Project::update_row(const std::shared_ptr<Context>& ctx, const ProjectRow& row) {
  if (row.project_id != ctx->project_id) {
    throw Error(ErrorCode::InvalidArgument, "Project row id does not match the active project");
  }

  auto guard = connect(ctx);
  project::detail::run_prepared(guard.get(),
                "INSERT INTO Project (project_id, metadata, workflow) VALUES (?, ?, ?) ON CONFLICT(project_id) DO UPDATE SET metadata = excluded.metadata, workflow = excluded.workflow",
                "update Project row",
                [&](duckdb_prepared_statement statement) {
                  duckdb_bind_varchar(statement, 1, row.project_id.c_str());
                  const std::string metadata = project::detail::json_to_text(row.metadata);
                  const std::string workflow = project::detail::json_to_text(row.workflow);
                  duckdb_bind_varchar(statement, 2, metadata.c_str());
                  duckdb_bind_varchar(statement, 3, workflow.c_str());
                },
               [](duckdb_result&) {});
}

Project::Project(std::string db_path, std::string project_id)
    : ctx_(std::make_shared<Context>()) {
  if (db_path.empty()) {
    throw Error(ErrorCode::InvalidArgument, "Project requires a database path");
  }
  if (project_id.empty()) {
    throw Error(ErrorCode::InvalidArgument, "Project requires a project_id");
  }

  try {
    const auto parent = std::filesystem::path(db_path).parent_path();
    if (!parent.empty()) {
      std::filesystem::create_directories(parent);
    }
  } catch (const std::filesystem::filesystem_error& e) {
    throw Error(ErrorCode::Io, std::string("Failed to prepare database directory: ") + e.what());
  }

  ctx_->db_path = std::move(db_path);
  ctx_->project_id = std::move(project_id);

  auto guard = connect(ctx_);
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS Project (project_id VARCHAR NOT NULL PRIMARY KEY, metadata JSON, workflow JSON, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
          "create Project table");
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS Cache (project_id VARCHAR NOT NULL, name VARCHAR NOT NULL, description VARCHAR NOT NULL, hash VARCHAR NOT NULL, data BLOB NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY(project_id, hash))",
          "create Cache table");
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS AuditTrail (project_id VARCHAR NOT NULL, operation_type VARCHAR NOT NULL, object_type VARCHAR NOT NULL, operation_details JSON, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
          "create AuditTrail table");

  project::detail::run_prepared(guard.get(),
                "INSERT INTO Project (project_id, metadata, workflow) VALUES (?, ?, ?) ON CONFLICT(project_id) DO NOTHING",
                "insert Project row",
                [&](duckdb_prepared_statement statement) {
                  duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str());
                  const std::string metadata = project::detail::json_to_text(json::object());
                  const std::string workflow = project::detail::json_to_text(json::array());
                  duckdb_bind_varchar(statement, 2, metadata.c_str());
                  duckdb_bind_varchar(statement, 3, workflow.c_str());
                },
               [](duckdb_result&) {});
}

Project::~Project() {
}

const std::string& Project::db_path() const noexcept {
  return ctx_->db_path;
}

const std::string& Project::project_id() const noexcept {
  return ctx_->project_id;
}

ProjectRow Project::row() const {
  create_schema(ctx_);
  ensure_row_exists(ctx_);
  return read_row(ctx_);
}

void Project::set_row(const ProjectRow& row) {
  create_schema(ctx_);
  ProjectRow value = row;
  if (value.project_id.empty()) {
    value.project_id = ctx_->project_id;
  }
  if (value.project_id != ctx_->project_id) {
    throw Error(ErrorCode::InvalidArgument, "Project row id does not match the active project");
  }
  ensure_row_exists(ctx_);
  update_row(ctx_, value);
}

json Project::metadata() const {
  create_schema(ctx_);
  return row().metadata;
}

void Project::set_metadata(const json& value) {
  create_schema(ctx_);
  ProjectRow current = row();
  current.metadata = value;
  update_row(ctx_, current);
}

json Project::workflow() const {
  create_schema(ctx_);
  return row().workflow;
}

void Project::set_workflow(const json& value) {
  create_schema(ctx_);
  ProjectRow current = row();
  current.workflow = value;
  update_row(ctx_, current);
}

void Project::validate() const {
  create_schema(ctx_);
  validate_schema(ctx_);
  ensure_row_exists(ctx_);
}

std::vector<std::string> Project::list_tables() const {
  auto guard = connect(ctx_);
  duckdb_result result{};
  if (duckdb_query(guard.get(),
                   "SELECT table_name FROM information_schema.tables WHERE table_schema = 'main' AND table_type = 'BASE TABLE' ORDER BY table_name",
                   &result) == DuckDBError) {
    std::string message = project::detail::result_error(&result);
    duckdb_destroy_result(&result);
    throw Error(ErrorCode::DuckDB, std::string("list tables: ") + message);
  }
  project::detail::ResultGuard result_guard(&result);

  std::vector<std::string> tables;
  const idx_t count = duckdb_row_count(&result);
  tables.reserve(static_cast<std::size_t>(count));
  for (idx_t row = 0; row < count; ++row) {
    tables.push_back(project::detail::result_varchar(&result, 0, row));
  }
  return tables;
}

std::vector<AuditTrailRow> Project::get_audit() const {
  auto guard = connect(ctx_);
  std::vector<AuditTrailRow> out;
  project::detail::run_prepared(guard.get(),
               "SELECT project_id, operation_type, object_type, operation_details, created_at FROM AuditTrail WHERE project_id = ? ORDER BY created_at DESC",
               "query AuditTrail",
               [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, ctx_->project_id.c_str()); },
               [&](duckdb_result& result) {
                 out = project::detail::rows_from_result(&result, [&](idx_t row) {
                   AuditTrailRow value;
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

Project* Project::copy(std::string db_path, std::string project_id) const {
  if (db_path.empty()) {
    throw Error(ErrorCode::InvalidArgument, "Project copy requires a database path");
  }
  if (project_id.empty()) {
    throw Error(ErrorCode::InvalidArgument, "Project copy requires a project_id");
  }

  const auto source_path = normalized_path(ctx_->db_path);
  const auto target_path = normalized_path(db_path);
  if (source_path == target_path && project_id == ctx_->project_id) {
    throw Error(ErrorCode::InvalidArgument, "Project copy requires a different database path or project_id");
  }

  auto target = std::make_unique<Project>(std::move(db_path), std::move(project_id));

  const ProjectRow source_row = row();
  target->set_metadata(source_row.metadata);
  target->set_workflow(source_row.workflow);

  Cache source_cache(ctx_);
  Cache target_cache(target->ctx_);
  target_cache.clear();
  for (const auto& entry : source_cache.all()) {
    Cache::Row copy = entry;
    copy.project_id = target->project_id();
    target_cache.put(copy);
  }

  AuditTrail source_audit(ctx_);
  AuditTrail target_audit(target->ctx_);
  target_audit.clear();
  for (const auto& entry : source_audit.all()) {
    target_audit.add(entry.operation_type, entry.object_type, entry.operation_details);
  }

  return target.release();
}

}  // namespace project
