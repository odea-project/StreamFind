#pragma once

#include "../json_core/file.h"

namespace asm_json {

using json_core::json;

/** ASM-specific file wrapper built on top of `json_core::JSON_FILE`. */
class ASM_FILE : public json_core::JSON_FILE {
 public:
  /** Bind the reader to one JSON file. */
  explicit ASM_FILE(std::filesystem::path file_path);
  /** Read the main payload section using a heuristic. */
  json read_primary_data() const;
  /** Return the JSON pointer chosen by `read_primary_data()`. */
  std::string primary_data_path() const;
  /** Return the schema root directory. */
  const std::filesystem::path& schema_root_dir() const noexcept;

 private:
  std::filesystem::path schema_root_dir_;
};

}  // namespace asm_json
