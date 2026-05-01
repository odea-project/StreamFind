// Generic JSON helpers plus ASM-specific readers for Rcpp.
#include <Rcpp.h>

#include "asm/file.h"
#include "asm/reader.h"
#include "json/error.h"
#include "json/io.h"
#include "json/tree.h"

#include <filesystem>

using namespace Rcpp;

namespace {

const auto json_error_prefix = "JSON error";

std::string node_kind_to_string(asm_json::NodeKind kind) {
  switch (kind) {
    case asm_json::NodeKind::Object: return "object";
    case asm_json::NodeKind::Array: return "array";
    case asm_json::NodeKind::String: return "string";
    case asm_json::NodeKind::Number: return "number";
    case asm_json::NodeKind::Boolean: return "boolean";
    case asm_json::NodeKind::Null: return "null";
    default: return "unknown";
  }
}

template <typename Fn>
auto asm_call(Fn&& fn) {
  try {
    return fn();
  } catch (const asm_json::Error& e) {
    stop(std::string("ASM error: ") + e.what());
  } catch (const std::exception& e) {
    stop(std::string("ASM error [Unknown]: ") + e.what());
  }
}

template <typename Fn>
auto json_call(Fn&& fn) {
  try {
    return fn();
  } catch (const std::exception& e) {
    stop(std::string(json_error_prefix) + ": " + e.what());
  }
}

void collect_index_rows(const asm_json::Node& node,
                       std::vector<std::string>& path,
                       std::vector<std::string>& key,
                       std::vector<std::string>& kind,
                       std::vector<int>& child_count,
                       std::vector<std::string>& parent_path,
                       std::vector<int>& is_leaf) {
  const auto info = node.info();
  path.push_back(info.path);
  key.push_back(info.key);
  kind.push_back(node_kind_to_string(info.kind));
  child_count.push_back(static_cast<int>(info.child_count));
  parent_path.push_back(info.parent_path);
  is_leaf.push_back(info.is_leaf ? 1 : 0);

  for (const auto& child : node.children()) {
    collect_index_rows(child, path, key, kind, child_count, parent_path, is_leaf);
  }
}

}  // namespace

// [[Rcpp::export]]
std::string rcpp_json_read_file(std::string file_path) {
  return json_call([&]() {
    const auto path = std::filesystem::path(file_path);
    return asm_json::load_json_file(path).dump();
  });
}

// [[Rcpp::export]]
std::string rcpp_json_read_subtree(std::string file_path, std::string path) {
  return json_call([&]() {
    const auto root = asm_json::load_json_file(std::filesystem::path(file_path));
    const auto* node = asm_json::descend_json_pointer(root, path);
    if (!node) {
      stop(std::string(json_error_prefix) + ": JSON node not found: " + path);
    }
    return node->dump();
  });
}

// [[Rcpp::export]]
Rcpp::CharacterVector rcpp_json_list_children(std::string file_path, std::string path = "") {
  return json_call([&]() {
    const auto root = asm_json::load_json_file(std::filesystem::path(file_path));
    const auto* node = asm_json::descend_json_pointer(root, path);
    if (!node) {
      stop(std::string(json_error_prefix) + ": JSON node not found: " + path);
    }
    auto children = asm_json::json_child_paths(*node);
    Rcpp::CharacterVector out(children.size());
    for (R_xlen_t i = 0; i < out.size(); ++i) {
      out[i] = children[static_cast<std::size_t>(i)];
    }
    return out;
  });
}

// [[Rcpp::export]]
std::string rcpp_asm_read_file(std::string file_path) {
  return rcpp_json_read_file(std::move(file_path));
}

// [[Rcpp::export]]
Rcpp::DataFrame rcpp_asm_index_table(std::string file_path) {
  return asm_call([&]() {
    asm_json::File file{std::filesystem::path(file_path)};
    file.build_index();

    const auto root = file.root();
    std::vector<std::string> path;
    std::vector<std::string> key;
    std::vector<std::string> kind;
    std::vector<int> child_count;
    std::vector<std::string> parent_path;
    std::vector<int> is_leaf;

    collect_index_rows(root, path, key, kind, child_count, parent_path, is_leaf);

    CharacterVector path_vec(path.begin(), path.end());
    CharacterVector key_vec(key.begin(), key.end());
    CharacterVector kind_vec(kind.begin(), kind.end());
    IntegerVector child_count_vec(child_count.begin(), child_count.end());
    CharacterVector parent_path_vec(parent_path.begin(), parent_path.end());
    LogicalVector is_leaf_vec(is_leaf.begin(), is_leaf.end());

    return DataFrame::create(
        _["path"] = path_vec,
        _["key"] = key_vec,
        _["kind"] = kind_vec,
        _["child_count"] = child_count_vec,
        _["parent_path"] = parent_path_vec,
        _["is_leaf"] = is_leaf_vec,
        _["stringsAsFactors"] = false);
  });
}

// [[Rcpp::export]]
std::string rcpp_asm_read_subtree(std::string file_path, std::string path) {
  return asm_call([&]() {
    asm_json::File file{std::filesystem::path(file_path)};
    return file.read_subtree(path).dump();
  });
}

// [[Rcpp::export]]
std::string rcpp_asm_read_primary_data(std::string file_path) {
  return asm_call([&]() {
    asm_json::File file{std::filesystem::path(file_path)};
    file.build_index();
    return file.read_primary_data().dump();
  });
}

// [[Rcpp::export]]
Rcpp::CharacterVector rcpp_asm_list_children(std::string file_path, std::string path = "") {
  return asm_call([&]() {
    asm_json::File file{std::filesystem::path(file_path)};
    file.build_index();
    auto children = file.node(path).children();
    Rcpp::CharacterVector out(children.size());
    for (R_xlen_t i = 0; i < out.size(); ++i) {
      out[i] = children[static_cast<std::size_t>(i)].info().path;
    }
    return out;
  });
}

// [[Rcpp::export]]
bool rcpp_asm_validate_file(std::string json_file_path,
                            std::string schema_file,
                            std::string schema_root_dir = "") {
  return asm_call([&]() {
    return asm_json::validate_document(std::filesystem::path(json_file_path),
                                       std::filesystem::path(schema_file),
                                       std::filesystem::path(schema_root_dir));
  });
}
