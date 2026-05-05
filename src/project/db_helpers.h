#pragma once

#include "project.h"

#include <algorithm>
#include <cctype>
#include <sstream>
#include <utility>

namespace project::detail {

/** RAII connection wrapper for a DuckDB connection. */
struct CONNECTION_GUARD {
  explicit CONNECTION_GUARD(const std::shared_ptr<CONTEXT>& ctx) {
    if (!ctx || ctx->db_path.empty()) {
      throw ERROR(ERROR_CODE::InvalidArgument, "Project context is not initialized");
    }
    if (duckdb_open(ctx->db_path.c_str(), &db_) != DuckDBSuccess) {
      throw ERROR(ERROR_CODE::DuckDB, "Failed to open DuckDB database");
    }
    if (duckdb_connect(db_, &con_) != DuckDBSuccess) {
      duckdb_close(&db_);
      db_ = nullptr;
      throw ERROR(ERROR_CODE::DuckDB, "Failed to connect to DuckDB database");
    }
  }

  ~CONNECTION_GUARD() {
    if (con_) {
      duckdb_disconnect(&con_);
    }
    if (db_) {
      duckdb_close(&db_);
    }
  }

  CONNECTION_GUARD(const CONNECTION_GUARD&) = delete;
  CONNECTION_GUARD& operator=(const CONNECTION_GUARD&) = delete;

  CONNECTION_GUARD(CONNECTION_GUARD&& other) noexcept : db_(other.db_), con_(other.con_) {
    other.db_ = nullptr;
    other.con_ = nullptr;
  }

  CONNECTION_GUARD& operator=(CONNECTION_GUARD&& other) noexcept {
    if (this != &other) {
      if (con_) {
        duckdb_disconnect(&con_);
      }
      if (db_) {
        duckdb_close(&db_);
      }
      db_ = other.db_;
      con_ = other.con_;
      other.db_ = nullptr;
      other.con_ = nullptr;
    }
    return *this;
  }

  duckdb_connection get() const noexcept { return con_; }

 private:
  duckdb_database db_ = nullptr;
  duckdb_connection con_ = nullptr;
};

/** RAII wrapper for duckdb_result. */
struct RESULT_GUARD {
  explicit RESULT_GUARD(duckdb_result* result) : result_(result) {}
  ~RESULT_GUARD() {
    if (result_) {
      duckdb_destroy_result(result_);
    }
  }

  RESULT_GUARD(const RESULT_GUARD&) = delete;
  RESULT_GUARD& operator=(const RESULT_GUARD&) = delete;

  RESULT_GUARD(RESULT_GUARD&& other) noexcept : result_(other.result_) {
    other.result_ = nullptr;
  }

  RESULT_GUARD& operator=(RESULT_GUARD&& other) noexcept {
    if (this != &other) {
      if (result_) {
        duckdb_destroy_result(result_);
      }
      result_ = other.result_;
      other.result_ = nullptr;
    }
    return *this;
  }

 private:
  duckdb_result* result_;
};

/** RAII wrapper for duckdb_prepared_statement. */
struct PREPARE_GUARD {
  explicit PREPARE_GUARD(duckdb_prepared_statement* statement) : statement_(statement) {}
  ~PREPARE_GUARD() {
    if (statement_ && *statement_) {
      duckdb_destroy_prepare(statement_);
    }
  }

  PREPARE_GUARD(const PREPARE_GUARD&) = delete;
  PREPARE_GUARD& operator=(const PREPARE_GUARD&) = delete;

  PREPARE_GUARD(PREPARE_GUARD&& other) noexcept : statement_(other.statement_) {
    other.statement_ = nullptr;
  }

  PREPARE_GUARD& operator=(PREPARE_GUARD&& other) noexcept {
    if (this != &other) {
      if (statement_ && *statement_) {
        duckdb_destroy_prepare(statement_);
      }
      statement_ = other.statement_;
      other.statement_ = nullptr;
    }
    return *this;
  }

 private:
  duckdb_prepared_statement* statement_;
};

/** Expected column definition used during schema validation. */
struct COLUMN_SPEC {
  const char* name;
  const char* type;
  bool not_null;
};

/** Convert a string to uppercase for schema comparison. */
inline std::string upper_copy(std::string value) {
  std::transform(value.begin(), value.end(), value.begin(), [](unsigned char ch) {
    return static_cast<char>(std::toupper(ch));
  });
  return value;
}

/** Compare DuckDB column type strings case-insensitively. */
inline bool same_text(const std::string& lhs, const char* rhs) {
  return upper_copy(lhs) == upper_copy(rhs ? std::string(rhs) : std::string());
}

/** Serialize JSON to text for storage in DuckDB. */
inline std::string json_to_text(const json& value) {
  return value.dump();
}

/** Parse JSON from stored text. */
inline json json_from_text(const std::string& value) {
  if (value.empty()) {
    return json();
  }
  try {
    return json::parse(value);
  } catch (const std::exception& e) {
    throw ERROR(ERROR_CODE::SchemaMismatch, std::string("Invalid JSON payload: ") + e.what());
  }
}

/** Serialize a streamable C++ object into a byte buffer. */
template <typename T>
inline std::vector<std::uint8_t> serialize_object(const T& value) {
  std::ostringstream oss;
  oss << value;
  if (!oss) {
    throw ERROR(ERROR_CODE::Unknown, "Failed to serialize cached object");
  }
  const std::string payload = oss.str();
  return std::vector<std::uint8_t>(payload.begin(), payload.end());
}

/** Deserialize a streamable C++ object from a byte buffer. */
template <typename T>
inline T deserialize_object(const std::vector<std::uint8_t>& bytes) {
  std::string payload(bytes.begin(), bytes.end());
  std::istringstream iss(payload);
  T value{};
  iss >> value;
  if (!iss) {
    throw ERROR(ERROR_CODE::Unknown, "Failed to deserialize cached object");
  }
  return value;
}

/** Return the DuckDB error string for a result. */
inline std::string result_error(duckdb_result* result) {
  const char* err = duckdb_result_error(result);
  return err ? std::string(err) : std::string("unknown DuckDB error");
}

/** Read a varchar value from a DuckDB result and free the allocated memory. */
inline std::string result_varchar(duckdb_result* result, idx_t col, idx_t row) {
  char* value = duckdb_value_varchar(result, col, row);
  if (!value) {
    return {};
  }
  std::string out(value);
  duckdb_free(value);
  return out;
}

/** Read a blob value from a DuckDB result and free the allocated memory. */
inline std::vector<std::uint8_t> result_blob(duckdb_result* result, idx_t col, idx_t row) {
  duckdb_blob blob = duckdb_value_blob(result, col, row);
  if (!blob.data || blob.size == 0) {
    if (blob.data) {
      duckdb_free(blob.data);
    }
    return {};
  }

  auto* bytes = static_cast<std::uint8_t*>(blob.data);
  std::vector<std::uint8_t> out(bytes, bytes + blob.size);
  duckdb_free(blob.data);
  return out;
}

/** Materialize all rows from a DuckDB result using a row reader lambda. */
template <typename Reader>
auto rows_from_result(duckdb_result* result, Reader&& reader) {
  using RowT = decltype(reader(idx_t{}));
  const idx_t count = duckdb_row_count(result);
  std::vector<RowT> rows;
  rows.reserve(static_cast<std::size_t>(count));
  for (idx_t row = 0; row < count; ++row) {
    rows.push_back(reader(row));
  }
  return rows;
}

/** Execute a prepared statement and process its result. */
template <typename Binder, typename Consumer>
inline void run_prepared(duckdb_connection con,
                         const std::string& sql,
                         const char* context,
                         Binder&& binder,
                         Consumer&& consumer) {
  duckdb_prepared_statement statement = nullptr;
  if (duckdb_prepare(con, sql.c_str(), &statement) == DuckDBError) {
    std::string message = statement ? duckdb_prepare_error(statement) : std::string("prepare failed");
    if (statement) {
      duckdb_destroy_prepare(&statement);
    }
     throw ERROR(ERROR_CODE::DuckDB, std::string(context) + ": " + message);
  }

  PREPARE_GUARD statement_guard(&statement);
  binder(statement);

  duckdb_result result{};
  if (duckdb_execute_prepared(statement, &result) == DuckDBError) {
    std::string message = result_error(&result);
    duckdb_destroy_result(&result);
    throw ERROR(ERROR_CODE::DuckDB, std::string(context) + ": " + message);
  }

  RESULT_GUARD result_guard(&result);
  consumer(result);
}

/** Execute a direct SQL statement. */
inline void run_sql(duckdb_connection con, const std::string& sql, const char* context) {
  duckdb_result result{};
  if (duckdb_query(con, sql.c_str(), &result) == DuckDBError) {
    std::string message = result_error(&result);
    duckdb_destroy_result(&result);
    throw ERROR(ERROR_CODE::DuckDB, std::string(context) + ": " + message);
  }
  duckdb_destroy_result(&result);
}

/** Verify that the table schema matches the expected column list. */
inline void validate_columns(duckdb_connection con,
                             const char* table_name,
                             const std::vector<COLUMN_SPEC>& expected) {
  std::ostringstream sql;
  sql << "PRAGMA table_info('" << table_name << "')";

  duckdb_result result{};
  if (duckdb_query(con, sql.str().c_str(), &result) == DuckDBError) {
    std::string message = result_error(&result);
    duckdb_destroy_result(&result);
    throw ERROR(ERROR_CODE::SchemaMismatch, std::string("Schema check failed for ") + table_name + ": " + message);
  }
  RESULT_GUARD guard(&result);

  const idx_t count = duckdb_row_count(&result);
  if (count != expected.size()) {
    throw ERROR(ERROR_CODE::SchemaMismatch,
                std::string("Schema mismatch for ") + table_name + ": unexpected column count");
  }

  for (idx_t row = 0; row < count; ++row) {
    const std::string name = result_varchar(&result, 1, row);
    const std::string type = result_varchar(&result, 2, row);
    const bool not_null = duckdb_value_int32(&result, 3, row) != 0;
    const auto& spec = expected[static_cast<std::size_t>(row)];
    if (name != spec.name || !same_text(type, spec.type) || not_null != spec.not_null) {
      throw ERROR(ERROR_CODE::SchemaMismatch,
                  std::string("Schema mismatch for ") + table_name + ": column " + spec.name + " does not match");
    }
  }
}

}  // namespace project::detail
