#include "cache.h"

namespace project {

CACHE::CACHE(std::shared_ptr<CONTEXT> ctx) : TABLE_BASE<CACHE_ROW>(std::move(ctx)) {
  create_schema(context());
  validate_schema(context());
}

void CACHE::create_schema(const std::shared_ptr<CONTEXT>& ctx) {
  auto guard = project::detail::CONNECTION_GUARD(ctx);
  project::detail::run_sql(guard.get(),
          "CREATE TABLE IF NOT EXISTS Cache (project_id VARCHAR NOT NULL, name VARCHAR NOT NULL, description VARCHAR NOT NULL, hash VARCHAR NOT NULL, data BLOB NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY(project_id, hash))",
          "create Cache table");
}

void CACHE::validate_schema(const std::shared_ptr<CONTEXT>& ctx) {
  auto guard = project::detail::CONNECTION_GUARD(ctx);
  project::detail::validate_columns(guard.get(), table_name(), {{"project_id", "VARCHAR", true},
                                                                {"name", "VARCHAR", true},
                                                                {"description", "VARCHAR", true},
                                                                {"hash", "VARCHAR", true},
                                                                {"data", "BLOB", true},
                                                                {"created_at", "TIMESTAMP", false}});
}

std::vector<CACHE::ROW_TYPE> CACHE::all() const {
  auto guard = project::detail::CONNECTION_GUARD(context());
  std::vector<ROW_TYPE> out;
  project::detail::run_prepared(guard.get(),
                "SELECT project_id, name, description, hash, data, created_at FROM Cache WHERE project_id = ? ORDER BY created_at DESC",
                "query Cache",
                [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, context()->project_id.c_str()); },
                [&](duckdb_result& result) {
                  out = project::detail::rows_from_result(&result, [&](idx_t row) {
                    ROW_TYPE value;
                    value.project_id = project::detail::result_varchar(&result, 0, row);
                    value.name = project::detail::result_varchar(&result, 1, row);
                    value.description = project::detail::result_varchar(&result, 2, row);
                    value.hash = project::detail::result_varchar(&result, 3, row);
                    value.data = project::detail::result_blob(&result, 4, row);
                    value.created_at = project::detail::result_varchar(&result, 5, row);
                    return value;
                  });
                });
  return out;
}

std::optional<CACHE::ROW_TYPE> CACHE::get(const std::string& hash) const {
  auto guard = project::detail::CONNECTION_GUARD(context());
  std::optional<ROW_TYPE> out;
  project::detail::run_prepared(guard.get(),
                "SELECT project_id, name, description, hash, data, created_at FROM Cache WHERE project_id = ? AND hash = ? LIMIT 1",
                "select Cache entry",
                [&](duckdb_prepared_statement statement) {
                 duckdb_bind_varchar(statement, 1, context()->project_id.c_str());
                  duckdb_bind_varchar(statement, 2, hash.c_str());
                },
                [&](duckdb_result& result) {
                  if (duckdb_row_count(&result) == 0) {
                    return;
                  }
                  ROW_TYPE row;
                  row.project_id = project::detail::result_varchar(&result, 0, 0);
                  row.name = project::detail::result_varchar(&result, 1, 0);
                  row.description = project::detail::result_varchar(&result, 2, 0);
                  row.hash = project::detail::result_varchar(&result, 3, 0);
                  row.data = project::detail::result_blob(&result, 4, 0);
                  row.created_at = project::detail::result_varchar(&result, 5, 0);
                  out = std::move(row);
                });
  return out;
}

std::optional<std::vector<std::uint8_t>> CACHE::get_bytes(const std::string& hash) const {
  auto row = get(hash);
  if (!row) {
    return std::nullopt;
  }
  return row->data;
}

void CACHE::put(const ROW_TYPE& row) {
  ROW_TYPE value = row;
  if (value.project_id.empty()) {
    value.project_id = context()->project_id;
  }
  if (value.project_id != context()->project_id) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Cache row id does not match the active project");
  }
  if (value.hash.empty()) {
    throw ERROR(ERROR_CODE::InvalidArgument, "Cache hash must not be empty");
  }
  if (value.name.empty()) {
    value.name = value.hash;
  }
  if (value.description.empty()) {
    value.description = "cache entry";
  }

  auto guard = project::detail::CONNECTION_GUARD(context());
  project::detail::run_prepared(guard.get(),
                "INSERT INTO Cache (project_id, name, description, hash, data) VALUES (?, ?, ?, ?, ?) ON CONFLICT(project_id, hash) DO UPDATE SET name = excluded.name, description = excluded.description, data = excluded.data",
                "upsert Cache entry",
                [&](duckdb_prepared_statement statement) {
                 duckdb_bind_varchar(statement, 1, value.project_id.c_str());
                 duckdb_bind_varchar(statement, 2, value.name.c_str());
                  duckdb_bind_varchar(statement, 3, value.description.c_str());
                  duckdb_bind_varchar(statement, 4, value.hash.c_str());
                  const void* payload = value.data.empty() ? nullptr : value.data.data();
                  duckdb_bind_blob(statement, 5, payload, value.data.size());
                },
                [](duckdb_result&) {});
}

void CACHE::put(const std::string& name,
                const std::string& hash,
                const std::string& description,
                const std::vector<std::uint8_t>& data) {
  ROW_TYPE row;
  row.project_id = context()->project_id;
  row.name = name;
  row.hash = hash;
  row.description = description;
  row.data = data;
  put(row);
}

void CACHE::remove(const std::string& hash) {
  auto guard = project::detail::CONNECTION_GUARD(context());
  project::detail::run_prepared(guard.get(),
                "DELETE FROM Cache WHERE project_id = ? AND hash = ?",
                "delete Cache entry",
                [&](duckdb_prepared_statement statement) {
                 duckdb_bind_varchar(statement, 1, context()->project_id.c_str());
                 duckdb_bind_varchar(statement, 2, hash.c_str());
               },
               [](duckdb_result&) {});
}

void CACHE::clear() {
  auto guard = project::detail::CONNECTION_GUARD(context());
  project::detail::run_prepared(guard.get(),
                "DELETE FROM Cache WHERE project_id = ?",
                "clear Cache",
                [&](duckdb_prepared_statement statement) { duckdb_bind_varchar(statement, 1, context()->project_id.c_str()); },
               [](duckdb_result&) {});
}

}  // namespace project
