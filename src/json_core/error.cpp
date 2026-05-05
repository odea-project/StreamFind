#include "error.h"

namespace json_core {

// Store the category separately from the human-readable message.
Error::Error(ErrorCode code, std::string message)
    : std::runtime_error(std::move(message)), code_(code) {}

ErrorCode Error::code() const noexcept {
  return code_;
}

}  // namespace json_core
