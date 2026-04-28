nlohmann/json (single-header)
================================

This directory is reserved for the nlohmann/json single-header distribution.

Usage
- Download the single-header file `json.hpp` from the repository:
  https://github.com/nlohmann/json

  Example (curl):
    mkdir -p src/external/nlohmann && \
    curl -L -o src/external/nlohmann/json.hpp \
      https://raw.githubusercontent.com/nlohmann/json/develop/single_include/nlohmann/json.hpp

- After placing `json.hpp` here, include it from code with either:
    #include <nlohmann/json.hpp>
  or
    #include "nlohmann/json.hpp"

Build
- src/Makevars and src/Makevars.win add -Iexternal/nlohmann so the header will be found during compilation.

Notes
- This project intentionally does not vendor the full nlohmann/json header to avoid licensing surprises in automated edits. Please download the official single-header into this folder to enable compilation.
