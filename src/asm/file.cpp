#include "file.h"

#include "../json_core/error.h"

namespace asm_json {

using json_core::Error;
using json_core::ErrorCode;
using json_core::json;

ASM_FILE::ASM_FILE(std::filesystem::path file_path)
    : json_core::JSON_FILE(file_path), schema_root_dir_(file_path.parent_path()) {}

std::string ASM_FILE::primary_data_path() const {
  std::string fallback;
  // Prefer a top-level `data` node when the file exposes one.
  for (const auto& child_path : index().children("")) {
    const json_core::NodeInfo* info = index().find(child_path);
    if (!info) {
      continue;
    }
    if (info->key == "$asm.manifest") {
      continue;
    }
    if (info->key == "data") {
      return child_path;
    }
    if (fallback.empty()) {
      fallback = child_path;
    }
  }

  return fallback;
}

json ASM_FILE::read_primary_data() const {
  const std::string path = primary_data_path();
  if (path.empty()) {
    throw Error(ErrorCode::NotFound, "ASM primary data node not found");
  }
  return read_subtree(path);
}

const std::filesystem::path& ASM_FILE::schema_root_dir() const noexcept { return schema_root_dir_; }
}  // namespace asm_json
