#include "file.h"

#include "../json/error.h"

#include "../json/io.h"
#include "../json/tree.h"

namespace asm_json {

File::File(std::filesystem::path file_path)
    : file_path_(std::move(file_path)), schema_root_dir_(file_path_.parent_path()), index_(std::make_shared<Index>(file_path_)) {}

void File::build_index() {
  index_->build();
}

Node File::node(const std::string& path) const {
  return Node(file_path_, path, index_);
}

Node File::root() const {
  return node("");
}

json File::read_subtree(const std::string& path) const {
  // This currently materializes the whole file, then selects the subtree.
  const json root = load_json_file(file_path_);
  const json* node = descend_json_pointer(root, path);
  if (!node) {
    throw Error(ErrorCode::NotFound, "ASM node not found: " + path);
  }
  return *node;
}

std::string File::primary_data_path() const {
  if (!index_) {
    return {};
  }

  std::string fallback;
  // Prefer a top-level `data` node when the file exposes one.
  for (const auto& child_path : index_->children("")) {
    const NodeInfo* info = index_->find(child_path);
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

json File::read_primary_data() const {
  const std::string path = primary_data_path();
  if (path.empty()) {
    throw Error(ErrorCode::NotFound, "ASM primary data node not found");
  }
  return read_subtree(path);
}

const Index& File::index() const { return *index_; }
const std::filesystem::path& File::file_path() const noexcept { return file_path_; }
const std::filesystem::path& File::schema_root_dir() const noexcept { return schema_root_dir_; }
std::string File::model_name() const { return index_->model_name(); }

Node::Node(std::filesystem::path file_path, std::string path, std::shared_ptr<const Index> index)
    : file_path_(std::move(file_path)), path_(std::move(path)), index_(std::move(index)) {}

bool Node::exists() const {
  return index_ && index_->exists(path_);
}

NodeInfo Node::info() const {
  const NodeInfo* info = index_ ? index_->find(path_) : nullptr;
  if (!info) {
    throw Error(ErrorCode::NotFound, "ASM node not found: " + path_);
  }
  return *info;
}

std::vector<Node> Node::children() const {
  std::vector<Node> out;
  if (!index_) {
    return out;
  }
  for (const auto& child : index_->children(path_)) {
    out.emplace_back(file_path_, child, index_);
  }
  return out;
}

json Node::to_json() const {
  const json root = load_json_file(file_path_);
  const json* node = descend_json_pointer(root, path_);
  if (!node) {
    throw Error(ErrorCode::NotFound, "ASM node not found: " + path_);
  }
  return *node;
}

}  // namespace asm_json
