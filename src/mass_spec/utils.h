#ifndef MASS_SPEC_UTILS_H
#define MASS_SPEC_UTILS_H

#include <string>
#include <vector>

namespace ms {
namespace utils {

std::string encode_little_endian_from_float(const std::vector<float>& input, int precision);
std::string encode_little_endian_from_double(const std::vector<double>& input, int precision);
std::vector<float> decode_little_endian_to_float(const std::string& str, int precision);
std::vector<double> decode_little_endian_to_double(const std::string& str, int precision);
std::string encode_base64(const std::string& input);
std::string decode_base64(const std::string& input);

} // namespace utils
} // namespace ms

#endif // MASS_SPEC_UTILS_H
