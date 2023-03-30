#ifndef SF_PARSE_XML_H
#define SF_PARSE_XML_H

#define PUGIXML_HEADER_ONLY
// #define PUGIXML_NO_XPATH
#include "pugixml/pugixml.hpp"
#include <Rcpp.h>

Rcpp::List parse_msAnalysis_from_mzxml(std::string file_path);

#endif
