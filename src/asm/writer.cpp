#include "writer.h"

#include "../json/io.h"

namespace asm_json {

// Keep writing centralized so future formatting changes stay in one place.
void write_document(const std::filesystem::path& json_path, const json& document, int indent) {
  write_json_file(json_path, document, indent);
}

}  // namespace asm_json
