#pragma once

#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include <duckdb.h>
#include <../external/nlohmann/json.hpp>

namespace project {

using json = nlohmann::json;

/** Shared project state for an open DuckDB-backed project. */
struct CONTEXT {
  /** Absolute path to the DuckDB file. */
  std::string db_path;
  /** Active project identifier within the database. */
  std::string project_id;
};

/** Error categories used by the project layer. */
enum class ERROR_CODE {
  DuckDB,
  InvalidArgument,
  SchemaMismatch,
  NotFound,
  Io,
  Unknown
};

/** Exception type thrown by project operations. */
class ERROR : public std::runtime_error {
 public:
  /** Create an error with a category and message. */
  ERROR(ERROR_CODE code, std::string message);
  /** Return the error category. */
  ERROR_CODE code() const noexcept;

 private:
  ERROR_CODE code_;
};

/** Common base row fields shared by project-scoped tables. */
struct ROW {
  /** Primary key for the owning project. */
  std::string project_id;
  /** Creation time stored by DuckDB. */
  std::string created_at;
};

/** Row representation of the `Project` table, extending Row with JSON columns. */
struct PROJECT_ROW : public ROW {
  /** Domain code stored for this project, such as MS or RAMAN. */
  std::string domain;
  /** Project metadata stored as JSON. */
  json metadata = json::object();
  /** Project workflow stored as JSON. */
  json workflow = json::array();
};

struct AUDIT_TRAIL_ROW;

/** Minimal base class for typed table wrappers. */
template <typename RowT>
class TABLE_BASE {
 public:
  using ROW_TYPE = RowT;

  virtual ~TABLE_BASE() = default;

  /** Return all rows in the table. */
  virtual std::vector<ROW_TYPE> all() const = 0;

  /** Return the table as JSON. */
  json json_all() const { return all(); }
  /** Return the row count. */
  std::size_t size() const { return all().size(); }
  /** Return true when the table has no rows. */
  bool empty() const { return size() == 0; }

 protected:
  /** Store the shared project context. */
  explicit TABLE_BASE(std::shared_ptr<CONTEXT> ctx) : ctx_(std::move(ctx)) {}

  /** Access the shared project context. */
  const std::shared_ptr<CONTEXT>& context() const noexcept { return ctx_; }

  std::shared_ptr<CONTEXT> ctx_;
};

/** Open and manage a single DuckDB-backed StreamFind project. */
class PROJECT {
 public:
  /** Open or create a project database for the given project id. */
  explicit PROJECT(std::string db_path, std::string project_id);
  /** Close the database handle and release owned table wrappers. */
  ~PROJECT();

  PROJECT(const PROJECT&) = delete;
  PROJECT& operator=(const PROJECT&) = delete;
  PROJECT(PROJECT&&) = delete;
  PROJECT& operator=(PROJECT&&) = delete;

  /** Return the database file path. */
  const std::string& db_path() const noexcept;
  /** Return the active project id. */
  const std::string& project_id() const noexcept;
  /** Return the shared context used by project-scoped wrappers. */
  const std::shared_ptr<CONTEXT>& context() const noexcept;

  /** Load the current project row. */
  PROJECT_ROW row() const;
  /** Replace the current project row. */
  void set_row(const PROJECT_ROW& row);

  /** Return the project metadata JSON. */
  json metadata() const;
  /** Return the project domain code. */
  std::string domain() const;
  /** Replace the project metadata JSON. */
  void set_metadata(const json& value);
  /** Set the project domain code. */
  void set_domain(const std::string& value);

  /** Return the project workflow JSON. */
  json workflow() const;
  /** Replace the project workflow JSON. */
  void set_workflow(const json& value);

  /** Validate schema and ensure the project row exists. */
  void validate() const;
  /** List tables present in the database. */
  std::vector<std::string> list_tables() const;

  /** Return audit rows for the active project. */
  std::vector<AUDIT_TRAIL_ROW> get_audit() const;

  /** Copy the project into another database and/or project id. */
  PROJECT* copy(std::string db_path, std::string project_id) const;

 private:
  /** Create the project table schema if needed. */
  static void create_schema(const std::shared_ptr<CONTEXT>& ctx);
  /** Validate the project table schema. */
  static void validate_schema(const std::shared_ptr<CONTEXT>& ctx);
  /** Insert a default project row when none exists. */
  static void ensure_row_exists(const std::shared_ptr<CONTEXT>& ctx);
  /** Read the current project row. */
  static PROJECT_ROW read_row(const std::shared_ptr<CONTEXT>& ctx);
  /** Persist an updated project row. */
  static void update_row(const std::shared_ptr<CONTEXT>& ctx, const PROJECT_ROW& row);

  std::shared_ptr<CONTEXT> ctx_;
};

}  // namespace project
