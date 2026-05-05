#include "writer.h"

#include "../json_core/file.h"

namespace asm_json {

using json_core::json;

// Keep writing centralized so future formatting changes stay in one place.
void write_document(const std::filesystem::path& json_path, const json& document, int indent) {
  json_core::JSON_FILE(json_path).write(document, indent);
}

}  // namespace asm_json
