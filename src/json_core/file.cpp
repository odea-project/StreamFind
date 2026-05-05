#include "file.h"

#include "io.h"

namespace json_core {

JSON_FILE::JSON_FILE(std::filesystem::path file_path)
    : file_path_(std::move(file_path)), index_(std::make_shared<Index>(file_path_)) {}

void JSON_FILE::build_index() {
  index_->build();
}

JSON_NODE JSON_FILE::node(const std::string& path) const {
  return JSON_NODE(file_path_, path, index_);
}

JSON_NODE JSON_FILE::root() const {
  return node("");
}

json JSON_FILE::read() const {
  return load_json_file(file_path_);
}

json JSON_FILE::read_subtree(const std::string& path) const {
  const json root = read();
  const json* node = descend_json_pointer(root, path);
  if (!node) {
    throw Error(ErrorCode::NotFound, "JSON node not found: " + path);
  }
  return *node;
}

void JSON_FILE::write(const json& value, int indent) const {
  write_json_file(file_path_, value, indent);
}

const Index& JSON_FILE::index() const { return *index_; }
const std::filesystem::path& JSON_FILE::file_path() const noexcept { return file_path_; }
std::string JSON_FILE::model_name() const { return index_->model_name(); }

JSON_NODE::JSON_NODE(std::filesystem::path file_path, std::string path, std::shared_ptr<const Index> index)
    : file_path_(std::move(file_path)), path_(std::move(path)), index_(std::move(index)) {}

bool JSON_NODE::exists() const {
  return index_ && index_->exists(path_);
}

NodeInfo JSON_NODE::info() const {
  const NodeInfo* info = index_ ? index_->find(path_) : nullptr;
  if (!info) {
    throw Error(ErrorCode::NotFound, "JSON node not found: " + path_);
  }
  return *info;
}

std::vector<JSON_NODE> JSON_NODE::children() const {
  std::vector<JSON_NODE> out;
  if (!index_) {
    return out;
  }
  for (const auto& child : index_->children(path_)) {
    out.emplace_back(file_path_, child, index_);
  }
  return out;
}

json JSON_NODE::to_json() const {
  const json root = load_json_file(file_path_);
  const json* node = descend_json_pointer(root, path_);
  if (!node) {
    throw Error(ErrorCode::NotFound, "JSON node not found: " + path_);
  }
  return *node;
}

}  // namespace json_core
