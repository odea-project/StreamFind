#pragma once

#include "io.h"

#include <filesystem>
#include <optional>
#include <unordered_map>
#include <vector>

namespace json_core {

class Index;
class SaxIndexBuilder;

/** Node types recorded by the SAX indexer. */
enum class NodeKind {
  Object,
  Array,
  String,
  Number,
  Boolean,
  Null,
  Unknown
};

/** Metadata captured for one JSON pointer. */
struct NodeInfo {
  std::string path;
  std::string key;
  std::string parent_path;
  NodeKind kind = NodeKind::Unknown;
  std::size_t child_count = 0;
  bool is_leaf = false;
};

/** SAX-built index over one JSON document. */
class Index {
 public:
  /** Create an index tied to a source file. */
  explicit Index(std::filesystem::path file_path);

  /** Parse the file and populate the node map. */
  void build();
  /** Return whether a JSON pointer exists in the index. */
  bool exists(const std::string& path) const;
  /** Look up metadata for a JSON pointer. */
  const NodeInfo* find(const std::string& path) const;
  /** Return direct child pointers for a JSON pointer. */
  std::vector<std::string> children(const std::string& path) const;

  /** Return the model name inferred from the file. */
  const std::string& model_name() const noexcept;
  /** Return the manifest URI if one was recorded. */
  const std::string& manifest_uri() const noexcept;

 private:
  friend class SaxIndexBuilder;

  /** SAX stack frame used while walking the JSON document. */
  struct Frame {
    std::string path;
    bool is_array = false;
    std::size_t next_index = 0;
    std::optional<std::string> pending_key;
  };

  /** Escape one JSON Pointer token. */
  static std::string escape_pointer_token(const std::string& token);
  /** Join a parent pointer with one child token. */
  static std::string join_path(const std::string& parent, const std::string& token);
  /** Build a child pointer for an object key. */
  static std::string child_path_for_object(const std::string& parent, const std::string& key);
  /** Build a child pointer for an array index. */
  static std::string child_path_for_array(const std::string& parent, std::size_t index);
  /** Map a JSON value type to a `NodeKind`. */
  static NodeKind kind_from_value(const json& value);

  /** Add one indexed node and link it to its parent. */
  void add_node(const std::string& path, const std::string& key, const std::string& parent_path, NodeKind kind, bool is_leaf);

  std::filesystem::path file_path_;
  std::unordered_map<std::string, NodeInfo> nodes_;
  std::unordered_map<std::string, std::vector<std::string>> children_;
  std::string model_name_;
  std::string manifest_uri_;
};

}  // namespace json_core
