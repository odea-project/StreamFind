#pragma once

#include "../json/index.h"

#include <memory>

namespace asm_json {

class Node;

/** Indexed ASM file reader that can materialize subtrees on demand. */
class File {
 public:
  /** Bind the reader to one JSON file. */
  explicit File(std::filesystem::path file_path);

  /** Build the SAX index for the file. */
  void build_index();

  /** Return a handle for a JSON pointer path. */
  Node node(const std::string& path) const;
  /** Return the root node handle. */
  Node root() const;
  /** Read an arbitrary subtree by JSON pointer. */
  json read_subtree(const std::string& path) const;
  /** Read the main payload section using a heuristic. */
  json read_primary_data() const;
  /** Return the JSON pointer chosen by `read_primary_data()`. */
  std::string primary_data_path() const;

  /** Return the built index. */
  const Index& index() const;
  /** Return the source file path. */
  const std::filesystem::path& file_path() const noexcept;
  /** Return the schema root directory. */
  const std::filesystem::path& schema_root_dir() const noexcept;
  /** Return the model name inferred from the file. */
  std::string model_name() const;

 private:
  std::filesystem::path file_path_;
  std::filesystem::path schema_root_dir_;
  std::shared_ptr<Index> index_;
};

/** Handle for one indexed JSON pointer in an ASM file. */
class Node {
 public:
  /** Bind the node handle to a file and index. */
  Node(std::filesystem::path file_path, std::string path, std::shared_ptr<const Index> index);

  /** Return whether the path exists in the index. */
  bool exists() const;
  /** Return node metadata. */
  NodeInfo info() const;
  /** Return child node handles. */
  std::vector<Node> children() const;
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

}  // namespace asm_json
