#pragma once

#include "db_helpers.h"

namespace project {

/** Row representation of the `Cache` table, extending Row with cache-specific fields. */
struct CacheRow : public Row {
  /** Cache entry name. */
  std::string name;
  /** Human-readable description. */
  std::string description;
  /** Unique cache key. */
  std::string hash;
  /** Cached payload stored as raw serialized bytes. */
  std::vector<std::uint8_t> data;
};

/** Project-scoped cache table wrapper. */
class Cache : public TableBase<CacheRow> {
 public:
  using Row = CacheRow;

  /** Open the cache wrapper for the current project context. */
  explicit Cache(std::shared_ptr<Context> ctx);

  /** Create the cache table schema if needed. */
  static void create_schema(const std::shared_ptr<Context>& ctx);
  /** Validate the cache table schema. */
  static void validate_schema(const std::shared_ptr<Context>& ctx);

  /** Return all cache rows for the active project. */
  std::vector<Row> all() const override;
  /** Load one cache row by hash. */
  std::optional<Row> get(const std::string& hash) const;
  /** Load only the cached byte payload for a cache entry. */
  std::optional<std::vector<std::uint8_t>> get_bytes(const std::string& hash) const;
  /** Insert or update a cache row. */
  void put(const Row& row);
  /** Convenience insert/update using scalar arguments and raw bytes. */
  void put(const std::string& name,
           const std::string& hash,
           const std::string& description,
           const std::vector<std::uint8_t>& data);
  /** Store a stream-serializable C++ object as a cached byte buffer. */
  template <typename T>
  void put_object(const std::string& name,
                  const std::string& hash,
                  const std::string& description,
                  const T& value);
  /** Recover a cached stream-serializable object previously stored with put_object(). */
  template <typename T>
  std::optional<T> get_object(const std::string& hash) const;
  /** Remove one cache row by hash. */
  void remove(const std::string& hash);
  /** Remove all cache rows for the active project. */
  void clear();

 private:
  static constexpr const char* table_name() { return "Cache"; }
};

template <typename T>
void Cache::put_object(const std::string& name,
                       const std::string& hash,
                       const std::string& description,
                       const T& value) {
  std::vector<std::uint8_t> bytes = detail::serialize_object(value);
  put(name, hash, description, bytes);
}

template <typename T>
std::optional<T> Cache::get_object(const std::string& hash) const {
  auto bytes = get_bytes(hash);
  if (!bytes || bytes->empty()) {
    return std::nullopt;
  }

  return detail::deserialize_object<T>(*bytes);
}

}  // namespace project
