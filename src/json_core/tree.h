#pragma once

#include <string>
#include <vector>

#include <nlohmann/json.hpp>

namespace json_core {

using json = nlohmann::json;

/** Decode a JSON Pointer token from RFC 6901 form. */
std::string unescape_pointer_token(const std::string& token);

/** Walk a JSON document using a JSON Pointer path and return the pointed node. */
const json* descend_json_pointer(const json& root, const std::string& path);

/** Return the immediate child keys or array indices for a JSON node. */
std::vector<std::string> json_child_paths(const json& node);

}  // namespace json_core
