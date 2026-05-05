#include "audit_trail.h"

#include "db_helpers.h"

namespace project {

AUDIT_TRAIL::AUDIT_TRAIL(std::shared_ptr<CONTEXT> ctx) : TABLE_BASE<AUDIT_TRAIL_ROW>(std::move(ctx)) {
  create_schema(context());
  validate_schema(context());
}

void to_json(json& j, const AUDIT_TRAIL_ROW& x) {
  j = json{{"project_id", x.project_id},
           {"operation_type", x.operation_type},
           {"object_type", x.object_type},
           {"operation_details", x.operation_details},
           {"created_at", x.created_at}};
}

void from_json(const json& j, AUDIT_TRAIL_ROW& x) {
  j.at("project_id").get_to(x.project_id);
  j.at("operation_type").get_to(x.operation_type);
  j.at("object_type").get_to(x.object_type);
  x.operation_details = j.value("operation_details", json::object());
  x.created_at = j.value("created_at", std::string());
}

void AUDIT_TRAIL::create_schema(const std::shared_ptr<CONTEXT>& ctx) {
  auto guard = project::detail::CONNECTION_GUARD(ctx);
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS AuditTrail (project_id VARCHAR NOT NULL, operation_type VARCHAR NOT NULL, object_type VARCHAR NOT NULL, operation_details JSON, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)",
          "create AuditTrail table");
}

void AUDIT_TRAIL::validate_schema(const std::shared_ptr<CONTEXT>& ctx) {
  auto guard = project::detail::CONNECTION_GUARD(ctx);
  project::detail::validate_columns(guard.get(), table_name(), {{"project_id", "VARCHAR", true},
                                                                {"operation_type", "VARCHAR", true},
                                                                {"object_type", "VARCHAR", true},
                                                                {"operation_details", "JSON", false},
                                                                {"created_at", "TIMESTAMP", false}});
}

std::vector<AUDIT_TRAIL::ROW_TYPE> AUDIT_TRAIL::all() const {
  auto guard = project::detail::CONNECTION_GUARD(context());
  std::vector<ROW_TYPE> out;
  project::detail::run_prepared(guard.get(),
                "SELECT project_id, operation_type, object_type, operation_details, created_at FROM AuditTrail WHERE project_id = ? ORDER BY created_at DESC",
                "query AuditTrail",
                [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, context()->project_id.c_str()); },
                [&](duckdb_result& result) {
                  out = project::detail::rows_from_result(&result, [&](idx_t row) {
                    ROW_TYPE value;
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

void AUDIT_TRAIL::add(const std::string& operation_type,
                      const std::string& object_type,
                      const json& details) {
  auto guard = project::detail::CONNECTION_GUARD(context());
  project::detail::run_prepared(guard.get(),
                          "INSERT INTO AuditTrail (project_id, operation_type, object_type, operation_details) VALUES (?, ?, ?, ?)",
                          "insert AuditTrail entry",
                          [&](duckdb_prepared_statement statement) {
                            duckdb_bind_varchar(statement, 1, context()->project_id.c_str());
                            duckdb_bind_varchar(statement, 2, operation_type.c_str());
                            duckdb_bind_varchar(statement, 3, object_type.c_str());
                            const std::string payload = project::detail::json_to_text(details);
                            duckdb_bind_varchar(statement, 4, payload.c_str());
                          },
                          [](duckdb_result&) {});
}

void AUDIT_TRAIL::clear() {
  auto guard = project::detail::CONNECTION_GUARD(context());
  project::detail::run_prepared(guard.get(),
                          "DELETE FROM AuditTrail WHERE project_id = ?",
                          "clear AuditTrail",
                          [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, context()->project_id.c_str()); },
                          [](duckdb_result&) {});
}

}  // namespace project
