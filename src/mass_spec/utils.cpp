#include "utils.h"

#include <cstring>
#include <stdexcept>
#include <array>

namespace ms {
namespace utils {

namespace {

static const char kB64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

inline unsigned char b64_index(unsigned char c) {
  if (c >= 'A' && c <= 'Z') return c - 'A';
  if (c >= 'a' && c <= 'z') return c - 'a' + 26;
  if (c >= '0' && c <= '9') return c - '0' + 52;
  if (c == '+') return 62;
  if (c == '/') return 63;
  return 255;
}

} // namespace

std::string encode_little_endian_from_float(const std::vector<float>& input, int precision) {
  if (precision == 8) {
    std::vector<unsigned char> bytes(sizeof(double) * input.size());
    for (size_t i = 0; i < input.size(); ++i) {
      double v = static_cast<double>(input[i]);
      std::memcpy(bytes.data() + i * sizeof(double), &v, sizeof(double));
    }
    return std::string(bytes.begin(), bytes.end());
  }
  if (precision == 4) {
    std::vector<unsigned char> bytes(sizeof(float) * input.size());
    std::memcpy(bytes.data(), input.data(), bytes.size());
    return std::string(bytes.begin(), bytes.end());
  }
  throw std::runtime_error("Precision must be 4 or 8");
}

std::string encode_little_endian_from_double(const std::vector<double>& input, int precision) {
  if (precision == 8) {
    std::vector<unsigned char> bytes(sizeof(double) * input.size());
    std::memcpy(bytes.data(), input.data(), bytes.size());
    return std::string(bytes.begin(), bytes.end());
  }
  if (precision == 4) {
    std::vector<unsigned char> bytes(sizeof(float) * input.size());
    for (size_t i = 0; i < input.size(); ++i) {
      float v = static_cast<float>(input[i]);
      std::memcpy(bytes.data() + i * sizeof(float), &v, sizeof(float));
    }
    return std::string(bytes.begin(), bytes.end());
  }
  throw std::runtime_error("Precision must be 4 or 8");
}

std::vector<float> decode_little_endian_to_float(const std::string& str, int precision) {
  if (precision != 4 && precision != 8) throw std::invalid_argument("Precision must be 4 or 8");
  const size_t bytes_size = str.size() / precision;
  std::vector<float> result(bytes_size);
  for (size_t i = 0; i < bytes_size; ++i) {
    if (precision == 8) {
      double v;
      std::memcpy(&v, str.data() + i * precision, sizeof(double));
      result[i] = static_cast<float>(v);
    } else {
      float v;
      std::memcpy(&v, str.data() + i * precision, sizeof(float));
      result[i] = v;
    }
  }
  return result;
}

std::vector<double> decode_little_endian_to_double(const std::string& str, int precision) {
  if (precision != 4 && precision != 8) throw std::invalid_argument("Precision must be 4 or 8");
  const size_t bytes_size = str.size() / precision;
  std::vector<double> result(bytes_size);
  for (size_t i = 0; i < bytes_size; ++i) {
    if (precision == 8) {
      double v;
      std::memcpy(&v, str.data() + i * precision, sizeof(double));
      result[i] = v;
    } else {
      float v;
      std::memcpy(&v, str.data() + i * precision, sizeof(float));
      result[i] = static_cast<double>(v);
    }
  }
  return result;
}

std::string encode_base64(const std::string& input) {
  std::string out;
  out.reserve(((input.size() + 2) / 3) * 4);
  unsigned int val = 0;
  int valb = -6;
  for (unsigned char c : input) {
    val = (val << 8) | c;
    valb += 8;
    while (valb >= 0) {
      out.push_back(kB64[(val >> valb) & 0x3F]);
      valb -= 6;
    }
  }
  if (valb > -6) out.push_back(kB64[((val << 8) >> (valb + 8)) & 0x3F]);
  while (out.size() % 4) out.push_back('=');
  return out;
}

std::string decode_base64(const std::string& input) {
  std::string out;
  std::array<int, 256> T{};
  T.fill(-1);
  for (int i = 0; i < 64; ++i) T[static_cast<unsigned char>(kB64[i])] = i;
  unsigned int val = 0;
  int valb = -8;
  for (unsigned char c : input) {
    if (c == '=') break;
    int d = T[c];
    if (d == -1) continue;
    val = (val << 6) | static_cast<unsigned int>(d);
    valb += 6;
    if (valb >= 0) {
      out.push_back(static_cast<char>((val >> valb) & 0xFF));
      valb -= 8;
    }
  }
  return out;
}

} // namespace utils
} // namespace ms
