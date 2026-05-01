// Simple Rcpp tests for json-schema-validator integration
#include <Rcpp.h>

#include <nlohmann/json-schema.hpp>
#include <nlohmann/json.hpp>

using json = nlohmann::json;
using json_validator = nlohmann::json_schema::json_validator;

static json make_schema() {
    return R"({
        "$schema": "http://json-schema.org/draft-07/schema#",
        "type": "object",
        "required": ["name", "version"],
        "properties": {
            "name": {"type": "string"},
            "version": {"type": "integer", "minimum": 1}
        }
    })"_json;
}

// [[Rcpp::export]]
bool rcpp_json_schema_validation_ok() {
    const json schema = make_schema();
    const json instance = R"({"name":"StreamFind","version":1})"_json;

    json_validator validator;
    validator.set_root_schema(schema);
    validator.validate(instance);
    return true;
}

// [[Rcpp::export]]
std::string rcpp_json_schema_validation_error() {
    const json schema = make_schema();
    const json instance = R"({"name":"StreamFind","version":0})"_json;

    json_validator validator;
    validator.set_root_schema(schema);

    try {
        validator.validate(instance);
        return "unexpected success";
    } catch (const std::exception &e) {
        return e.what();
    }
}
