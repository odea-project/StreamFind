# ASM Generic JSON Reader/Writer Development Plan (C++ with nlohmann/json)

## Overview

Develop a generic, reusable C++ library for reading, validating, and writing Allotrope Simple Model (ASM) JSON files using the [nlohmann/json](https://github.com/nlohmann/json) and [json-schema-validator](https://github.com/pboettch/json-schema-validator) libraries.  
The library should enable:
- Schema-based validation of any ASM data file.
- Simple API for loading, validating, and writing ASM-compliant JSON.
- Easy integration into other C++ projects.

---

## 1. Dependencies

### a. nlohmann/json
- [Repository](https://github.com/nlohmann/json)
- Header-only, can be used by downloading a single file or as a package/submodule.

### b. json-schema-validator
- [Repository](https://github.com/pboettch/json-schema-validator)
- Draft-07 support, depends on nlohmann/json.
- Built as CMake subproject, or integrated as a package/submodule.

### c. Build instructions
- Use CMake for cross-platform support.
- Recommend two methods for dependency management:
  - **Option 1:** Add both as git submodules in your project (recommended for library-style development).
  - **Option 2:** Use system or package manager installation (e.g., vcpkg, system install, hunter).

## 2. Directory Structure

```
project-root/
|-- include/
|   |-- asm_json_rw/
|       |-- reader.hpp
|       |-- writer.hpp
|       |-- validator.hpp
|-- src/
|   |-- reader.cpp
|   |-- writer.cpp
|   |-- validator.cpp
|-- test/
|   |-- (unit and integration tests)
|-- external/ (optional, for submodules)
|   |-- nlohmann_json/
|   |-- json-schema-validator/
|-- CMakeLists.txt
```

## 3. API Design

### a. Reader

- `bool validate(const std::string& schema_path, const std::string& json_path);`
- `nlohmann::json load(const std::string& json_path);`
- `nlohmann::json read_and_validate(const std::string& schema_path, const std::string& json_path);`

### b. Writer

- `bool write(const nlohmann::json&, const std::string& dest_path);`

### c. Validator (internal or public)

- Wrapper around `json_schema::json_validator`
- Optionally allows direct object-based validation

## 4. Implementation Workflow

1. **Integrate Dependencies**
    - Add nlohmann/json (single header or package).
    - Add and build json-schema-validator.

2. **Build Helper Functions**
    - JSON file loader and serializer.
    - Schema loader (from file or resource).
    - Error handling and error reporting.

3. **Schema-based Validation**
    - Use `json_schema::json_validator` per the examples.
    - Provide useful error messages/callbacks.

4. **Reader and Writer Implementation**
    - Reader: loads and validates JSON file against selected schema.
    - Writer: outputs well-formed, optionally validated JSON.

5. **Testing**
    - Use ASM model test data and schemas for thorough test cases.
    - Add round-trip tests (load-validate-write-validate).

6. **Packaging**
    - Provide CMake config for easy import into other projects.
    - Document usage with example main() or test program.

## 5. Example Usage

```cpp
#include <asm_json_rw/reader.hpp>

bool valid = ASMReader::validate("schemas/balance.embed.schema.json", "data/balance.json");
if(valid) {
    auto j = ASMReader::read_and_validate("schemas/balance.embed.schema.json", "data/balance.json");
    // process JSON...
}
```

---

## 6. References

- [ASM JSON Schemas and Data Samples](https://gitlab.com/allotrope-public/asm/-/tree/main/)
- [nlohmann/json documentation](https://json.nlohmann.me)
- [json-schema-validator examples](https://github.com/pboettch/json-schema-validator/tree/main/example)

---

## 7. Future Work and Extensibility

- Add support for schema version negotiation.
- Support manifest and sample/instance auto-discovery by ASM technique.
- CLI and GUI wrappers for batch validation.
- Richer error APIs for diagnostics.
