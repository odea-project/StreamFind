#pragma once

#include "project.h"

namespace project {

/** Row representation of the `AuditTrail` table, extending Row with audit-specific fields. */
struct AuditTrailRow : public Row {
  /** Operation type such as create/update/delete/run. */
  std::string operation_type;
  /** Object type affected by the operation. */
  std::string object_type;
  /** Free-form JSON details for the operation. */
  json operation_details = json::object();
  /** Audit creation time stored by DuckDB. */
  std::string created_at;
};

/** Convert an audit row to JSON. */
void to_json(json& j, const AuditTrailRow& x);
/** Convert JSON into an audit row. */
void from_json(const json& j, AuditTrailRow& x);

/** Project-scoped audit trail table wrapper. */
class AuditTrail : public TableBase<AuditTrailRow> {
 public:
  using Row = AuditTrailRow;

  /** Open the audit trail wrapper for the current project context. */
  explicit AuditTrail(std::shared_ptr<Context> ctx);

  /** Create the audit trail table schema if needed. */
  static void create_schema(const std::shared_ptr<Context>& ctx);
  /** Validate the audit trail table schema. */
  static void validate_schema(const std::shared_ptr<Context>& ctx);

  /** Return all audit rows for the active project. */
  std::vector<Row> all() const override;
  /** Append a new audit entry. */
  void add(const std::string& operation_type,
           const std::string& object_type,
           const json& details = json::object());
  /** Remove all audit rows for the active project. */
  void clear();

 private:
  static constexpr const char* table_name() { return "AuditTrail"; }
};

}  // namespace project
