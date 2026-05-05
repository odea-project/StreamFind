#pragma once

#include "project.h"

namespace project {

/** Row representation of the `AuditTrail` table, extending Row with audit-specific fields. */
struct AUDIT_TRAIL_ROW : public ROW {
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
void to_json(json& j, const AUDIT_TRAIL_ROW& x);
/** Convert JSON into an audit row. */
void from_json(const json& j, AUDIT_TRAIL_ROW& x);

/** Project-scoped audit trail table wrapper. */
class AUDIT_TRAIL : public TABLE_BASE<AUDIT_TRAIL_ROW> {
 public:
  using ROW_TYPE = AUDIT_TRAIL_ROW;

  /** Open the audit trail wrapper for the current project context. */
  explicit AUDIT_TRAIL(std::shared_ptr<CONTEXT> ctx);

  /** Create the audit trail table schema if needed. */
  static void create_schema(const std::shared_ptr<CONTEXT>& ctx);
  /** Validate the audit trail table schema. */
  static void validate_schema(const std::shared_ptr<CONTEXT>& ctx);

  /** Return all audit rows for the active project. */
  std::vector<ROW_TYPE> all() const override;
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
