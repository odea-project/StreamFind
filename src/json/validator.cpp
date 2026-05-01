#include "validator.h"

#include "error.h"
#include "io.h"

#include <algorithm>
#include <cstddef>
#include <iterator>
#include <utility>

namespace fs = std::filesystem;

namespace asm_json {

namespace {

// Try a few filename variants when a schema references another file.
bool has_suffix(const std::string& value, const std::string& suffix) {
  return value.size() >= suffix.size() && value.compare(value.size() - suffix.size(), suffix.size(), suffix) == 0;
}

fs::path schema_candidate_path(const fs::path& root_dir, const std::string& name) {
  const fs::path direct = root_dir / name;
  if (fs::exists(direct)) {
    return direct;
  }

  const fs::path with_json = root_dir / (name + ".json");
  if (fs::exists(with_json)) {
    return with_json;
  }

  const fs::path stem_json = root_dir / (fs::path(name).filename().string() + ".json");
  if (fs::exists(stem_json)) {
    return stem_json;
  }

  return {};
}

std::string json_string_or_empty(const json& value) {
  return value.is_string() ? value.get<std::string>() : std::string();
}

}  // namespace

SchemaValidator::SchemaIndex SchemaValidator::build_schema_index(const fs::path& root_dir) {
  SchemaIndex index;
  if (root_dir.empty() || !fs::exists(root_dir)) {
    return index;
  }

  for (const auto& entry : fs::recursive_directory_iterator(root_dir)) {
    if (!entry.is_regular_file()) {
      continue;
    }
    const auto name = entry.path().filename().string();
    if (name.size() >= 5 && name.substr(name.size() - 5) == ".json") {
      index_schema_file(index, entry.path());
    }
  }

  return index;
}

void SchemaValidator::index_schema_file(SchemaIndex& index, const fs::path& path) {
  try {
    const json doc = load_json_file(path);
    if (doc.is_object()) {
      if (doc.contains("$id")) {
        const std::string id = json_string_or_empty(doc.at("$id"));
        if (!id.empty()) {
          index[id] = path;
        }
      }
    }
    index[path.filename().string()] = path;
    index[path.stem().string()] = path;
  } catch (...) {
    // Ignore non-JSON or unreadable files during index construction.
  }
}

nlohmann::json_schema::schema_loader SchemaValidator::make_loader(const SchemaIndex& index,
                                                                  const fs::path& root_dir) {
  return [root_dir, index](const nlohmann::json_uri& id, json& value) {
    const fs::path path = resolve_schema_file(index, root_dir, id);
    if (path.empty()) {
      throw Error(ErrorCode::Validation, "Unresolved schema reference: " + id.location());
    }
    value = load_json_file(path);
  };
}

fs::path SchemaValidator::resolve_schema_file(const SchemaIndex& index,
                                              const fs::path& root_dir,
                                              const nlohmann::json_uri& id) {
  const std::string location = id.location();
  if (location.empty()) {
    return {};
  }

  if (auto it = index.find(location); it != index.end()) {
    return it->second;
  }

  const auto hash_pos = location.find('#');
  const std::string base = hash_pos == std::string::npos ? location : location.substr(0, hash_pos);
  if (auto it = index.find(base); it != index.end()) {
    return it->second;
  }

  const auto slash_pos = base.find_last_of('/');
  const std::string filename = slash_pos == std::string::npos ? base : base.substr(slash_pos + 1);

  if (filename.empty()) {
    return {};
  }

  if (auto candidate = schema_candidate_path(root_dir, filename); !candidate.empty()) {
    return candidate;
  }

  if (!has_suffix(filename, ".json")) {
    if (auto candidate = schema_candidate_path(root_dir, filename + ".json"); !candidate.empty()) {
      return candidate;
    }
  }

  return {};
}

SchemaValidator::SchemaValidator(const json& schema, std::filesystem::path root_dir)
    : root_dir_(std::move(root_dir)),
      schema_index_(build_schema_index(root_dir_)),
      loader_(make_loader(schema_index_, root_dir_)),
      validator_(loader_, nlohmann::json_schema::default_string_format_check, nullptr) {
  set_schema(schema);
}

SchemaValidator::SchemaValidator(const std::filesystem::path& schema_path, std::filesystem::path root_dir)
    : SchemaValidator(load_json_file(schema_path), std::move(root_dir)) {}

void SchemaValidator::set_schema(const json& schema) {
  schema_ = schema;
  validator_.set_root_schema(schema_);
}

void SchemaValidator::validate(const json& instance) const {
  validator_.validate(instance);
}

const json& SchemaValidator::schema() const noexcept {
  return schema_;
}

}  // namespace asm_json
