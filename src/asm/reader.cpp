#include "reader.h"

#include "../json/io.h"
#include "../json/validator.h"

#include <vector>

namespace asm_json {

namespace {

// Recursively check whether a nested object or array contains a key.
bool contains_key_recursive(const json& value, const std::string& key) {
  if (value.is_object()) {
    if (value.contains(key)) {
      return true;
    }
    for (const auto& item : value.items()) {
      if (contains_key_recursive(item.value(), key)) {
        return true;
      }
    }
  } else if (value.is_array()) {
    for (const auto& item : value) {
      if (contains_key_recursive(item, key)) {
        return true;
      }
    }
  }
  return false;
}

std::string bundle_base_name(const std::filesystem::path& path) {
  std::string name = path.filename().string();
  const std::string suffix = ".embed.schema.json";
  if (name.size() > suffix.size() && name.compare(name.size() - suffix.size(), suffix.size(), suffix) == 0) {
    name.resize(name.size() - suffix.size());
    return name;
  }
  const std::string suffix2 = ".schema.json";
  if (name.size() > suffix2.size() && name.compare(name.size() - suffix2.size(), suffix2.size(), suffix2) == 0) {
    name.resize(name.size() - suffix2.size());
    return name;
  }
  const std::string suffix3 = ".json";
  if (name.size() > suffix3.size() && name.compare(name.size() - suffix3.size(), suffix3.size(), suffix3) == 0) {
    name.resize(name.size() - suffix3.size());
  }
  return name;
}

}  // namespace

json load_document(const std::filesystem::path& json_path) {
  return load_json_file(json_path);
}

json load_schema_document(const std::filesystem::path& schema_path) {
  return load_json_file(schema_path);
}

json select_schema_from_bundle(const json& bundle,
                               const json& instance,
                               const std::filesystem::path& schema_path) {
  if (!bundle.is_object() || !bundle.contains("$defs") || !bundle["$defs"].is_object()) {
    return bundle;
  }

  const auto& defs = bundle["$defs"];
  if (defs.size() == 1) {
    return defs.begin().value();
  }

  const std::string base = bundle_base_name(schema_path);
  const bool has_cube = contains_key_recursive(instance, "data cube");

  const std::vector<std::string> preferred = has_cube
      ? std::vector<std::string>{base + "-profile-detector.schema",
                                 base + "-cube-detector.schema",
                                 base + "-area-scan-detector.schema"}
      : std::vector<std::string>{base + "-point-detector.schema",
                                 base + "-detector.schema",
                                 base + "-endpoint-detector.schema"};

  for (const auto& want : preferred) {
    for (const auto& item : defs.items()) {
      if (item.key().find(want) != std::string::npos) {
        return item.value();
      }
    }
  }

  return defs.begin().value();
}

bool validate_document(const std::filesystem::path& json_path,
                       const std::filesystem::path& schema_path,
                       const std::filesystem::path& schema_root_dir) {
  const std::filesystem::path root = schema_root_dir.empty() ? schema_path.parent_path() : schema_root_dir;
  const json instance = load_document(json_path);
  const json schema_bundle = load_schema_document(schema_path);
  const json effective_schema = select_schema_from_bundle(schema_bundle, instance, schema_path);
  SchemaValidator validator(effective_schema, root);
  validator.validate(instance);
  return true;
}

json read_and_validate(const std::filesystem::path& json_path,
                       const std::filesystem::path& schema_path,
                       const std::filesystem::path& schema_root_dir) {
  const std::filesystem::path root = schema_root_dir.empty() ? schema_path.parent_path() : schema_root_dir;
  json document = load_document(json_path);
  const json schema_bundle = load_schema_document(schema_path);
  const json effective_schema = select_schema_from_bundle(schema_bundle, document, schema_path);
  SchemaValidator validator(effective_schema, root);
  validator.validate(document);
  return document;
}

}  // namespace asm_json
