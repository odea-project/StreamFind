#include <Rcpp.h>
#include "simdutf/simdutf.h"

// Test with simdutf functions
// [[Rcpp::export]]
Rcpp::List test_simdutf() {
    Rcpp::List result;
    
    std::string valid_utf8 = "This is a valid UTF-8 string.";
    std::string invalid_utf8 = "\xFF\xFE\xFD"; // Invalid UTF-8 sequence
    
    // Check if simdutf features are available at compile time
#if SIMDUTF_FEATURE_UTF8
    try {
        // Now try to use simdutf functions
        bool is_valid_utf8 = simdutf::validate_utf8(valid_utf8.data(), valid_utf8.size());
        bool is_invalid_utf8 = simdutf::validate_utf8(invalid_utf8.data(), invalid_utf8.size());
        
        result["valid_utf8"] = is_valid_utf8;
        result["invalid_utf8"] = is_invalid_utf8;
        result["simdutf_available"] = true;
        result["note"] = "Using simdutf library for UTF-8 validation via separate compilation unit";
        
        Rcpp::Rcout << "SIMDUTF: Valid UTF-8 test: " << (is_valid_utf8 ? "PASSED" : "FAILED") << std::endl;
        Rcpp::Rcout << "SIMDUTF: Invalid UTF-8 test: " << (!is_invalid_utf8 ? "PASSED" : "FAILED") << std::endl;
        
    } catch (const std::exception& e) {
        result["error"] = std::string("SIMDUTF error: ") + e.what();
        result["simdutf_available"] = false;
        result["valid_utf8"] = false;
        result["invalid_utf8"] = false;
        result["note"] = "Error occurred while using simdutf";
        Rcpp::Rcout << "SIMDUTF error: " << e.what() << std::endl;
    }
#else
    result["valid_utf8"] = true;  
    result["invalid_utf8"] = false; 
    result["simdutf_available"] = false;
    result["note"] = "SIMDUTF_FEATURE_UTF8 not available";
#endif
    
    return result;
}