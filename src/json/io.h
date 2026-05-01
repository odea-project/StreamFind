#pragma once

#include <filesystem>

#include <nlohmann/json.hpp>

namespace asm_json {

using json = nlohmann::json;

/** Load a JSON document from disk without an intermediate text copy. */
json load_json_file(const std::filesystem::path& path);

/** Write JSON to disk using either compact or pretty formatting. */
void write_json_file(const std::filesystem::path& path, const json& value, int indent = 2);

}  // namespace asm_json
