#include "tree.h"

#include <cstddef>

namespace asm_json {

std::string unescape_pointer_token(const std::string& token) {
  std::string out;
  out.reserve(token.size());
  for (std::size_t i = 0; i < token.size(); ++i) {
    if (token[i] == '~' && i + 1 < token.size()) {
      if (token[i + 1] == '0') {
        out.push_back('~');
        ++i;
        continue;
      }
      if (token[i + 1] == '1') {
        out.push_back('/');
        ++i;
        continue;
      }
    }
    out.push_back(token[i]);
  }
  return out;
}

const json* descend_json_pointer(const json& root, const std::string& path) {
  if (path.empty() || path == "/") {
    return &root;
  }

  const json* current = &root;
  std::size_t pos = 0;
  while (pos < path.size()) {
    if (path[pos] != '/') {
      break;
    }
    ++pos;
    const std::size_t next = path.find('/', pos);
    std::string token = path.substr(pos, next == std::string::npos ? std::string::npos : next - pos);
    if (current->is_object()) {
      token = unescape_pointer_token(token);
      if (!current->contains(token)) {
        return nullptr;
      }
      current = &current->at(token);
    } else if (current->is_array()) {
      const std::size_t idx = static_cast<std::size_t>(std::stoul(token));
      if (idx >= current->size()) {
        return nullptr;
      }
      current = &(*current)[idx];
    } else {
      return nullptr;
    }
    if (next == std::string::npos) {
      break;
    }
    pos = next;
  }
  return current;
}

std::vector<std::string> json_child_paths(const json& node) {
  std::vector<std::string> out;
  if (node.is_object()) {
    out.reserve(node.size());
    for (const auto& item : node.items()) {
      out.push_back(item.key());
    }
  } else if (node.is_array()) {
    out.reserve(node.size());
    for (std::size_t i = 0; i < node.size(); ++i) {
      out.push_back(std::to_string(i));
    }
  }
  return out;
}

}  // namespace asm_json
