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
struct Context {
  /** Absolute path to the DuckDB file. */
  std::string db_path;
  /** Active project identifier within the database. */
  std::string project_id;
};

/** Error categories used by the project layer. */
enum class ErrorCode {
  DuckDB,
  InvalidArgument,
  SchemaMismatch,
  NotFound,
  Io,
  Unknown
};

/** Exception type thrown by project operations. */
class Error : public std::runtime_error {
 public:
  /** Create an error with a category and message. */
  Error(ErrorCode code, std::string message);
  /** Return the error category. */
  ErrorCode code() const noexcept;

 private:
  ErrorCode code_;
};

/** Common base row fields shared by project-scoped tables. */
struct Row {
  /** Primary key for the owning project. */
  std::string project_id;
  /** Creation time stored by DuckDB. */
  std::string created_at;
};

/** Row representation of the `Project` table, extending Row with JSON columns. */
struct ProjectRow : public Row {
  /** Project metadata stored as JSON. */
  json metadata = json::object();
  /** Project workflow stored as JSON. */
  json workflow = json::array();
};

struct AuditTrailRow;

/** Minimal base class for typed table wrappers. */
template <typename RowT>
class TableBase {
 public:
  using Row = RowT;

  virtual ~TableBase() = default;

  /** Return all rows in the table. */
  virtual std::vector<Row> all() const = 0;

  /** Return the table as JSON. */
  json json_all() const { return all(); }
  /** Return the row count. */
  std::size_t size() const { return all().size(); }
  /** Return true when the table has no rows. */
  bool empty() const { return size() == 0; }

 protected:
  /** Store the shared project context. */
  explicit TableBase(std::shared_ptr<Context> ctx) : ctx_(std::move(ctx)) {}

  /** Access the shared project context. */
  const std::shared_ptr<Context>& context() const noexcept { return ctx_; }

  std::shared_ptr<Context> ctx_;
};

/** Open and manage a single DuckDB-backed StreamFind project. */
class Project {
 public:
  /** Open or create a project database for the given project id. */
  explicit Project(std::string db_path, std::string project_id);
  /** Close the database handle and release owned table wrappers. */
  ~Project();

  Project(const Project&) = delete;
  Project& operator=(const Project&) = delete;
  Project(Project&&) = delete;
  Project& operator=(Project&&) = delete;

  /** Return the database file path. */
  const std::string& db_path() const noexcept;
  /** Return the active project id. */
  const std::string& project_id() const noexcept;

  /** Load the current project row. */
  ProjectRow row() const;
  /** Replace the current project row. */
  void set_row(const ProjectRow& row);

  /** Return the project metadata JSON. */
  json metadata() const;
  /** Replace the project metadata JSON. */
  void set_metadata(const json& value);

  /** Return the project workflow JSON. */
  json workflow() const;
  /** Replace the project workflow JSON. */
  void set_workflow(const json& value);

  /** Validate schema and ensure the project row exists. */
  void validate() const;
  /** List tables present in the database. */
  std::vector<std::string> list_tables() const;

  /** Return audit rows for the active project. */
  std::vector<AuditTrailRow> get_audit() const;

  /** Copy the project into another database and/or project id. */
  Project* copy(std::string db_path, std::string project_id) const;

 private:
  /** Create the project table schema if needed. */
  static void create_schema(const std::shared_ptr<Context>& ctx);
  /** Validate the project table schema. */
  static void validate_schema(const std::shared_ptr<Context>& ctx);
  /** Insert a default project row when none exists. */
  static void ensure_row_exists(const std::shared_ptr<Context>& ctx);
  /** Read the current project row. */
  static ProjectRow read_row(const std::shared_ptr<Context>& ctx);
  /** Persist an updated project row. */
  static void update_row(const std::shared_ptr<Context>& ctx, const ProjectRow& row);

  std::shared_ptr<Context> ctx_;
};

}  // namespace project
