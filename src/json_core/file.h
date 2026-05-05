#pragma once

#include "error.h"
#include "index.h"
#include "tree.h"

#include <memory>

namespace json_core {

/** Handle for one indexed JSON pointer in a JSON file. */
class JSON_NODE {
 public:
  /** Bind the node handle to a file and index. */
  JSON_NODE(std::filesystem::path file_path, std::string path, std::shared_ptr<const Index> index);

  /** Return whether the path exists in the index. */
  bool exists() const;
  /** Return node metadata. */
  NodeInfo info() const;
  /** Return child node handles. */
  std::vector<JSON_NODE> children() const;
  /** Materialize the pointed-to JSON subtree. */
  json to_json() const;

  /** Materialize and convert the subtree to `T`. */
  template <typename T>
  T get() const {
    return to_json().get<T>();
  }

 private:
  std::filesystem::path file_path_;
  std::string path_;
  std::shared_ptr<const Index> index_;
};

/** Generic indexed JSON file reader and writer. This is the main json_core interface. */
class JSON_FILE {
 public:
  /** Bind the reader to one JSON file. */
  explicit JSON_FILE(std::filesystem::path file_path);

  /** Build the SAX index for the file. */
  void build_index();

  /** Return a handle for a JSON pointer path. */
  JSON_NODE node(const std::string& path) const;
  /** Return the root node handle. */
  JSON_NODE root() const;
  /** Read the entire JSON document. */
  json read() const;
  /** Read an arbitrary subtree by JSON pointer. */
  json read_subtree(const std::string& path) const;
  /** Write a JSON document to disk. */
  void write(const json& value, int indent = 2) const;

  /** Return the built index. */
  const Index& index() const;
  /** Return the source file path. */
  const std::filesystem::path& file_path() const noexcept;
  /** Return the model name inferred from the file. */
  std::string model_name() const;

 private:
  std::filesystem::path file_path_;
  std::shared_ptr<Index> index_;
};

}  // namespace json_core
