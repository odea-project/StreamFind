#pragma once

#include <filesystem>

#include <nlohmann/json.hpp>

namespace asm_json {

using json = nlohmann::json;

/** Write a JSON document to disk. */
void write_document(const std::filesystem::path& json_path, const json& document, int indent = 2);

}  // namespace asm_json
