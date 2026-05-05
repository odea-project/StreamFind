#include "io.h"

#include "error.h"

#include <fstream>

namespace json_core {

namespace {

// Convert a path to a stable UTF-8 string for diagnostics.
std::string path_text(const std::filesystem::path& path) {
  return path.u8string();
}

}  // namespace

json load_json_file(const std::filesystem::path& path) {
  std::ifstream in(path, std::ios::binary);
  if (!in) {
    throw Error(ErrorCode::Io, "Failed to open JSON file: " + path_text(path));
  }

  try {
    return json::parse(in);
  } catch (const std::exception& e) {
    throw Error(ErrorCode::Parse, "Failed to parse JSON file " + path_text(path) + ": " + e.what());
  }
}

void write_json_file(const std::filesystem::path& path, const json& value, int indent) {
  std::ofstream out(path, std::ios::binary | std::ios::trunc);
  if (!out) {
    throw Error(ErrorCode::Io, "Failed to open JSON file for writing: " + path_text(path));
  }

  out << value.dump(indent);
  if (!out) {
    throw Error(ErrorCode::Io, "Failed to write JSON file: " + path_text(path));
  }
}

}  // namespace json_core
