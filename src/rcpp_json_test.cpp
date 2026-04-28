// Simple Rcpp tests for nlohmann::json integration
#include <Rcpp.h>
#include <../external/nlohmann/json.hpp>

using json = nlohmann::json;

// [[Rcpp::export]]
std::string rcpp_json_make_example() {
    json j;
    j["name"] = "StreamFind";
    j["version"] = 1;
    j["features"] = {"search", "filter", "cluster"};
    return j.dump();
}

// [[Rcpp::export]]
Rcpp::List rcpp_json_parse(const std::string& s) {
    json j = json::parse(s);
    Rcpp::List out;
    out["name"] = j.value("name", "");
    out["version"] = j.value("version", 0);
    // convert features array to R character vector
    std::vector<std::string> feats;
    if (j.contains("features") && j["features"].is_array()) {
        for (const auto& it : j["features"]) {
            feats.push_back(it.get<std::string>());
        }
    }
    out["features"] = feats;
    return out;
}
