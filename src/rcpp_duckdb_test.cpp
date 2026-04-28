// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
extern "C" {
#include "duckdb.h"
}
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector duckdb_list_tables(std::string db_path) {
    duckdb_database db = nullptr;
    duckdb_connection con = nullptr;
    duckdb_result res;
    std::vector<std::string> tables;
    bool opened = false, connected = false, queried = false;
    try {
        if (duckdb_open(db_path.c_str(), &db) != DuckDBSuccess)
            stop("Failed to open DuckDB database");
        opened = true;
        if (duckdb_connect(db, &con) != DuckDBSuccess)
            stop("Failed to connect to DuckDB database");
        connected = true;
        if (duckdb_query(con, "SELECT table_schema, table_name, table_type FROM information_schema.tables WHERE table_type = 'BASE TABLE' ORDER BY table_schema, table_name", &res) != DuckDBSuccess)
            stop("Failed to query table names");
        queried = true;
        idx_t n = duckdb_row_count(&res);
        idx_t col_count = duckdb_column_count(&res);
        Rcpp::Rcout << "duckdb_row_count: " << n << ", duckdb_column_count: " << col_count << std::endl;
        for (idx_t i = 0; i < n; ++i) {
            for (idx_t j = 0; j < col_count; ++j) {
                // duckdb_value_varchar(result, col, row) -> use (j, i)
                const char* val = duckdb_value_varchar(&res, j, i);
                Rcpp::Rcout << "row=" << i << ", col=" << j << ": " << (val ? val : "NULL") << " | ";
                if (val) duckdb_free((void*)val);
            }
            Rcpp::Rcout << std::endl;
        }
        // Build the output vector as intended (use column,row ordering)
        for (idx_t i = 0; i < n; ++i) {
            const char* schema = duckdb_value_varchar(&res, 0, i);
            const char* tname  = duckdb_value_varchar(&res, 1, i);
            const char* ttype  = duckdb_value_varchar(&res, 2, i);
            if (schema && tname && ttype) {
                std::string item = std::string(schema) + "." + std::string(tname) + " (" + std::string(ttype) + ")";
                tables.push_back(item);
            }
            if (schema) duckdb_free((void*)schema);
            if (tname)  duckdb_free((void*)tname);
            if (ttype)  duckdb_free((void*)ttype);
        }
    } catch (std::exception& ex) {
        if (queried) duckdb_destroy_result(&res);
        if (connected) duckdb_disconnect(&con);
        if (opened) duckdb_close(&db);
        stop(ex.what());
    } catch (...) {
        if (queried) duckdb_destroy_result(&res);
        if (connected) duckdb_disconnect(&con);
        if (opened) duckdb_close(&db);
        stop("Unknown error in duckdb_list_tables");
    }
    if (queried) duckdb_destroy_result(&res);
    if (connected) duckdb_disconnect(&con);
    if (opened) duckdb_close(&db);
    return wrap(tables);
}

// [[Rcpp::export]]
List duckdb_json_extension_info(std::string db_path) {
    duckdb_database db = nullptr;
    duckdb_connection con = nullptr;
    duckdb_result res;
    std::vector<std::string> loaded_extensions;
    bool opened = false, connected = false;
    std::string install_status, load_status, json_query_status;
    try {
        if (duckdb_open(db_path.c_str(), &db) != DuckDBSuccess)
            stop("Failed to open DuckDB database");
        opened = true;
        if (duckdb_connect(db, &con) != DuckDBSuccess)
            stop("Failed to connect to DuckDB database");
        connected = true;
        // INSTALL json
        if (duckdb_query(con, "INSTALL json;", &res) == DuckDBSuccess) {
            install_status = "json extension installed or already present";
        } else {
            install_status = std::string("INSTALL json error: ") + duckdb_result_error(&res);
        }
        duckdb_destroy_result(&res);
        // LOAD json
        if (duckdb_query(con, "LOAD json;", &res) == DuckDBSuccess) {
            load_status = "json extension loaded or already loaded";
        } else {
            load_status = std::string("LOAD json error: ") + duckdb_result_error(&res);
        }
        duckdb_destroy_result(&res);
        // List loaded extensions
        if (duckdb_query(con, "PRAGMA show_loaded_extensions;", &res) == DuckDBSuccess) {
            idx_t n = duckdb_row_count(&res);
            for (idx_t i = 0; i < n; ++i) {
                const char* ext = duckdb_value_varchar(&res, i, 0);
                if (ext) {
                    loaded_extensions.push_back(std::string(ext));
                    duckdb_free((void*)ext);
                }
            }
        }
        duckdb_destroy_result(&res);
        // Try using a JSON function to confirm extension is operative
        if (duckdb_query(con, "SELECT json('{\"foo\":42}') AS example_json;", &res) == DuckDBSuccess) {
            const char* v = duckdb_value_varchar(&res, 0, 0);
            json_query_status = std::string("OK: ") + (v ? v : "(NULL)");
            if (v) duckdb_free((void*)v);
        } else {
            json_query_status = std::string("JSON function error: ") + duckdb_result_error(&res);
        }
        duckdb_destroy_result(&res);
    } catch (std::exception& ex) {
        if (connected) duckdb_disconnect(&con);
        if (opened) duckdb_close(&db);
        stop(ex.what());
    } catch (...) {
        if (connected) duckdb_disconnect(&con);
        if (opened) duckdb_close(&db);
        stop("Unknown error in duckdb_json_extension_info");
    }
    if (connected) duckdb_disconnect(&con);
    if (opened) duckdb_close(&db);
    return List::create(
        _["install_status"] = install_status,
        _["load_status"] = load_status,
        _["loaded_extensions"] = wrap(loaded_extensions),
        _["json_query_status"] = json_query_status
    );
}
