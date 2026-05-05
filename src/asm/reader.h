#pragma once

#include "../json_core/validator.h"

#include <filesystem>

namespace asm_json {

using json_core::JSON_SCHEMA_VALIDATOR;
using json_core::json;

/** Load an ASM instance document from disk. */
json load_document(const std::filesystem::path& json_path);

/** Load a schema bundle or plain schema from disk. */
json load_schema_document(const std::filesystem::path& schema_path);

/** Select the effective schema from a bundle for a given instance. */
json select_schema_from_bundle(const json& bundle,
                               const json& instance,
                               const std::filesystem::path& schema_path);

/** Validate an instance against a schema file. */
bool validate_document(const std::filesystem::path& json_path,
                       const std::filesystem::path& schema_path,
                       const std::filesystem::path& schema_root_dir = {});

/** Load an instance, validate it, and return the parsed JSON. */
json read_and_validate(const std::filesystem::path& json_path,
                       const std::filesystem::path& schema_path,
                       const std::filesystem::path& schema_root_dir = {});

}  // namespace asm_json
