#include "utils.h"
#include <simdutf.h>
#include <cstring>
#include <stdexcept>
#include <array>
#include <zlib.h>

namespace mass_spec {
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
};

std::string encode_base64_simduft(const std::string &input) {
  size_t out_len = simdutf::base64_length_from_binary(input.size(), simdutf::base64_default);
  std::vector<char> outbuf(out_len);
  size_t written = simdutf::binary_to_base64(input.data(), input.size(), outbuf.data(), simdutf::base64_default);
  return std::string(outbuf.data(), written);
};

std::string decode_base64_simduft(const std::string &encoded_string)
{
  std::vector<uint8_t> buffer(
  simdutf::maximal_binary_length_from_base64(encoded_string.data(), encoded_string.size()));
  simdutf::result r = simdutf::base64_to_binary(
    encoded_string.data(), encoded_string.size(), (char*)buffer.data()
  );
  if(r.error != simdutf::error_code::SUCCESS) {
    throw std::runtime_error("Base64 decoding failed with error code: " + std::to_string(static_cast<int>(r.error)));
  } else {
    buffer.resize(r.count);
  }
  return std::string(buffer.begin(), buffer.end());
};

std::string compress_zlib(const std::string &str)
{
  std::vector<char> compressed_data;
  z_stream zs;
  memset(&zs, 0, sizeof(zs));
  if (deflateInit(&zs, Z_DEFAULT_COMPRESSION) != Z_OK)
  {
    throw std::runtime_error("deflateInit failed while initializing zlib for compression");
  }
  zs.next_in = reinterpret_cast<Bytef *>(const_cast<char *>(str.data()));
  zs.avail_in = str.size();
  int ret;
  char outbuffer[32768];
  do
  {
    zs.next_out = reinterpret_cast<Bytef *>(outbuffer);
    zs.avail_out = sizeof(outbuffer);
    ret = deflate(&zs, Z_FINISH);
    if (compressed_data.size() < zs.total_out)
    {
      compressed_data.insert(compressed_data.end(), outbuffer, outbuffer + (zs.total_out - compressed_data.size()));
    }
  } while (ret == Z_OK);
  deflateEnd(&zs);
  return std::string(compressed_data.begin(), compressed_data.end());
};

std::string decompress_zlib(const std::string& input) {
  if (input.empty()) return {};
  uLongf out_size = static_cast<uLongf>(input.size() * 8 + 1024);
  std::string out(out_size, '\0');
  int rc = Z_BUF_ERROR;
  for (int i = 0; i < 8 && rc == Z_BUF_ERROR; ++i) {
    out_size = static_cast<uLongf>(out.size());
    rc = ::uncompress(reinterpret_cast<Bytef*>(&out[0]), &out_size,
                      reinterpret_cast<const Bytef*>(input.data()), static_cast<uLongf>(input.size()));
    if (rc == Z_BUF_ERROR) out.resize(out.size() * 2);
  }
  if (rc != Z_OK) return {};
  out.resize(out_size);
  return out;
}

} // namespace utils
} // namespace ms
