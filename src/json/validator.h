#pragma once

#include <filesystem>
#include <functional>
#include <unordered_map>

#include <nlohmann/json-schema.hpp>

namespace asm_json {

using json = nlohmann::json;

/** Cached JSON-schema validator for repeated instance validation. */
class SchemaValidator {
 public:
  /** Build a validator from an already parsed schema. */
  SchemaValidator(const json& schema, std::filesystem::path root_dir = {});
  /** Build a validator by loading a schema file. */
  SchemaValidator(const std::filesystem::path& schema_path, std::filesystem::path root_dir = {});

  /** Replace the compiled schema. */
  void set_schema(const json& schema);

  /** Validate an instance and throw on failure. */
  void validate(const json& instance) const;

  /** Return the current schema JSON. */
  const json& schema() const noexcept;

 private:
  using SchemaIndex = std::unordered_map<std::string, std::filesystem::path>;

  static SchemaIndex build_schema_index(const std::filesystem::path& root_dir);
  static void index_schema_file(SchemaIndex& index, const std::filesystem::path& path);
  static nlohmann::json_schema::schema_loader make_loader(const SchemaIndex& index,
                                                         const std::filesystem::path& root_dir);
  static std::filesystem::path resolve_schema_file(const SchemaIndex& index,
                                                   const std::filesystem::path& root_dir,
                                                   const nlohmann::json_uri& id);

  std::filesystem::path root_dir_;
  SchemaIndex schema_index_;
  nlohmann::json_schema::schema_loader loader_;
  json schema_;
  nlohmann::json_schema::json_validator validator_;
};

}  // namespace asm_json
