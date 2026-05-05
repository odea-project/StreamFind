#include "index.h"

#include "error.h"

#include <nlohmann/json.hpp>

#include <fstream>
#include <utility>
#include <tuple>

namespace json_core {

using json = nlohmann::json;

namespace {

bool ends_with(const std::string& value, const std::string& suffix) {
  return value.size() >= suffix.size() && value.compare(value.size() - suffix.size(), suffix.size(), suffix) == 0;
}

}  // namespace

class SaxIndexBuilder : public nlohmann::json_sax<json> {
 public:
  explicit SaxIndexBuilder(Index& index) : index_(index) {}

  bool null() override { return emit_scalar(NodeKind::Null); }
  bool boolean(bool) override { return emit_scalar(NodeKind::Boolean); }
  bool number_integer(number_integer_t) override { return emit_scalar(NodeKind::Number); }
  bool number_unsigned(number_unsigned_t) override { return emit_scalar(NodeKind::Number); }
  bool number_float(number_float_t, const string_t&) override { return emit_scalar(NodeKind::Number); }
  bool string(string_t& val) override {
    const auto [path, parent, key, ok] = resolve_current_path();
    if (!ok) {
      return false;
    }
    // Capture the manifest URI while the root object is streaming past.
    if (path == "/$asm.manifest") {
      index_.manifest_uri_ = val;
    }
    index_.add_node(path, key, parent, NodeKind::String, true);
    advance_parent();
    return true;
  }
  bool binary(binary_t&) override { return emit_scalar(NodeKind::Unknown); }

  bool start_object(std::size_t) override { return start_container(NodeKind::Object, false); }
  bool key(string_t& val) override {
    if (stack_.empty()) {
      return false;
    }
    stack_.back().pending_key = val;
    return true;
  }
  bool end_object() override { return end_container(); }

  bool start_array(std::size_t) override { return start_container(NodeKind::Array, true); }
  bool end_array() override { return end_container(); }

  bool parse_error(std::size_t, const std::string&, const nlohmann::detail::exception& ex) override {
    error_ = ex.what();
    return false;
  }

  const std::string& error() const noexcept { return error_; }

 private:
  bool emit_scalar(NodeKind kind) {
    const auto [path, parent, key, ok] = resolve_current_path();
    if (!ok) {
      return false;
    }
    index_.add_node(path, key, parent, kind, true);
    advance_parent();
    return true;
  }

  bool start_container(NodeKind kind, bool is_array) {
    const auto [path, parent, key, ok] = resolve_current_path();
    if (!ok) {
      return false;
    }
    index_.add_node(path, key, parent, kind, false);
    stack_.push_back(Index::Frame{path, is_array, 0, std::nullopt});
    return true;
  }

  bool end_container() {
    if (stack_.empty()) {
      return false;
    }
    stack_.pop_back();
    advance_parent();
    return true;
  }

  void advance_parent() {
    if (stack_.empty()) {
      return;
    }
    auto& parent = stack_.back();
    if (parent.is_array) {
      ++parent.next_index;
    } else {
      parent.pending_key.reset();
    }
  }

  std::tuple<std::string, std::string, std::string, bool> resolve_current_path() {
    if (stack_.empty()) {
      return {"", "", "", true};
    }
    auto& parent = stack_.back();
    std::string key;
    std::string path;
    if (parent.is_array) {
      key = std::to_string(parent.next_index);
      path = Index::child_path_for_array(parent.path, parent.next_index);
    } else {
      if (!parent.pending_key.has_value()) {
        return {"", "", "", false};
      }
      key = *parent.pending_key;
      path = Index::child_path_for_object(parent.path, key);
    }
    return {path, parent.path, key, true};
  }

  Index& index_;
  std::vector<Index::Frame> stack_;
  std::string error_;
};

Index::Index(std::filesystem::path file_path)
    : file_path_(std::move(file_path)) {}

void Index::build() {
  nodes_.clear();
  children_.clear();
  model_name_.clear();
  manifest_uri_.clear();

  std::ifstream in(file_path_);
  if (!in) {
    throw Error(ErrorCode::Io, "Failed to open ASM file for indexing: " + file_path_.string());
  }

  model_name_ = file_path_.stem().string();
  if (ends_with(model_name_, ".embed.schema")) {
    model_name_.resize(model_name_.size() - std::string(".embed.schema").size());
  } else if (ends_with(model_name_, ".schema")) {
    model_name_.resize(model_name_.size() - std::string(".schema").size());
  }

  SaxIndexBuilder builder(*this);
  if (!json::sax_parse(in, &builder)) {
    throw Error(ErrorCode::Parse, "Failed to build ASM index: " + builder.error());
  }
}

bool Index::exists(const std::string& path) const {
  return nodes_.find(path) != nodes_.end();
}

const NodeInfo* Index::find(const std::string& path) const {
  const auto it = nodes_.find(path);
  return it == nodes_.end() ? nullptr : &it->second;
}

std::vector<std::string> Index::children(const std::string& path) const {
  const auto it = children_.find(path);
  return it == children_.end() ? std::vector<std::string>{} : it->second;
}

const std::string& Index::model_name() const noexcept { return model_name_; }
const std::string& Index::manifest_uri() const noexcept { return manifest_uri_; }

std::string Index::escape_pointer_token(const std::string& token) {
  std::string out;
  out.reserve(token.size());
  for (char ch : token) {
    if (ch == '~') {
      out += "~0";
    } else if (ch == '/') {
      out += "~1";
    } else {
      out.push_back(ch);
    }
  }
  return out;
}

std::string Index::join_path(const std::string& parent, const std::string& token) {
  return parent.empty() ? "/" + escape_pointer_token(token) : parent + "/" + escape_pointer_token(token);
}

std::string Index::child_path_for_object(const std::string& parent, const std::string& key) {
  return join_path(parent, key);
}

std::string Index::child_path_for_array(const std::string& parent, std::size_t index) {
  return join_path(parent, std::to_string(index));
}

NodeKind Index::kind_from_value(const json& value) {
  switch (value.type()) {
    case json::value_t::object: return NodeKind::Object;
    case json::value_t::array: return NodeKind::Array;
    case json::value_t::string: return NodeKind::String;
    case json::value_t::boolean: return NodeKind::Boolean;
    case json::value_t::number_integer:
    case json::value_t::number_unsigned:
    case json::value_t::number_float: return NodeKind::Number;
    case json::value_t::null: return NodeKind::Null;
    default: return NodeKind::Unknown;
  }
}

void Index::add_node(const std::string& path, const std::string& key, const std::string& parent_path, NodeKind kind, bool is_leaf) {
  NodeInfo info{path, key, parent_path, kind, 0, is_leaf};
  nodes_[path] = info;
  if (!parent_path.empty() || !path.empty()) {
    children_[parent_path].push_back(path);
    nodes_[parent_path].child_count += 1;
  }
}

}  // namespace json_core
