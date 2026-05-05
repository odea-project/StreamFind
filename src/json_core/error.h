#pragma once

#include <stdexcept>
#include <string>

namespace json_core {

/** Error categories raised by the generic JSON helpers. */
enum class ErrorCode {
  Io,
  Parse,
  Validation,
  NotFound,
  Unknown
};

/** Exception that carries a JSON-layer error code. */
class Error : public std::runtime_error {
 public:
  /** Construct a JSON-layer error. */
  Error(ErrorCode code, std::string message);

  /** Return the error category. */
  ErrorCode code() const noexcept;

 private:
  ErrorCode code_;
};

}  // namespace json_core
