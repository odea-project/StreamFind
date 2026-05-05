#pragma once

#include "../json_core/file.h"

#include <filesystem>

namespace asm_json {

using json_core::json;

/** Write a JSON document to disk. */
void write_document(const std::filesystem::path& json_path, const json& document, int indent = 2);

}  // namespace asm_json
