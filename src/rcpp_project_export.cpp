#include <Rcpp.h>

#include "project/audit_trail.h"
#include "project/db_helpers.h"
#include "project/project.h"

using namespace Rcpp;

namespace {

std::string error_code_to_string(project::ERROR_CODE code) {
  switch (code) {
    case project::ERROR_CODE::DuckDB:
      return "DuckDB";
    case project::ERROR_CODE::InvalidArgument:
      return "InvalidArgument";
    case project::ERROR_CODE::SchemaMismatch:
      return "SchemaMismatch";
    case project::ERROR_CODE::NotFound:
      return "NotFound";
    case project::ERROR_CODE::Io:
      return "Io";
    case project::ERROR_CODE::Unknown:
    default:
      return "Unknown";
  }
}

template <typename Fn>
auto project_call(Fn&& fn) {
  try {
    return fn();
  } catch (const project::ERROR& e) {
    stop("Project error [" + error_code_to_string(e.code()) + "]: " + e.what());
  } catch (const std::exception& e) {
    stop(std::string("Project error [Unknown]: ") + e.what());
  }
}

project::PROJECT& project_from_xptr(SEXP extptr) {
  Rcpp::XPtr<project::PROJECT> ptr(extptr);
  if (ptr.get() == nullptr) {
    stop("Project pointer is null");
  }
  return *ptr;
}

DataFrame audit_rows_to_df(const std::vector<project::AUDIT_TRAIL::ROW_TYPE>& rows) {
  CharacterVector project_id(rows.size());
  CharacterVector operation_type(rows.size());
  CharacterVector object_type(rows.size());
  CharacterVector operation_details(rows.size());
  CharacterVector created_at(rows.size());
  for (std::size_t i = 0; i < rows.size(); ++i) {
    project_id[i] = rows[i].project_id;
    operation_type[i] = rows[i].operation_type;
    object_type[i] = rows[i].object_type;
    operation_details[i] = project::detail::json_to_text(rows[i].operation_details);
    created_at[i] = rows[i].created_at;
  }
  return DataFrame::create(Named("project_id") = project_id,
                           Named("operation_type") = operation_type,
                           Named("object_type") = object_type,
                           Named("operation_details") = operation_details,
                           Named("created_at") = created_at);
}

}  // namespace

// [[Rcpp::export]]
SEXP rcpp_project_new(std::string db_path, std::string project_id) {
  return project_call([&]() {
    auto* ptr = new project::PROJECT(std::move(db_path), std::move(project_id));
    Rcpp::XPtr<project::PROJECT> out(ptr, true);
    out.attr("class") = "StreamFindProject";
    return SEXP(out);
  });
}

// [[Rcpp::export]]
std::string rcpp_project_get_domain(SEXP project_xptr) {
  return project_call([&]() { return project_from_xptr(project_xptr).domain(); });
}

// [[Rcpp::export]]
void rcpp_project_set_domain(SEXP project_xptr, std::string domain) {
  project_call([&]() {
    project_from_xptr(project_xptr).set_domain(domain);
    return 0;
  });
}

// [[Rcpp::export]]
SEXP rcpp_project_copy(SEXP project_xptr, std::string db_path, std::string project_id) {
  return project_call([&]() {
    auto* ptr = project_from_xptr(project_xptr).copy(std::move(db_path), std::move(project_id));
    Rcpp::XPtr<project::PROJECT> out(ptr, true);
    out.attr("class") = "StreamFindProject";
    return SEXP(out);
  });
}

// [[Rcpp::export]]
void rcpp_project_validate(SEXP project_xptr) {
  project_call([&]() {
    project_from_xptr(project_xptr).validate();
    return 0;
  });
}

// [[Rcpp::export]]
std::string rcpp_project_get_metadata(SEXP project_xptr) {
  return project_call([&]() { return project::detail::json_to_text(project_from_xptr(project_xptr).metadata()); });
}

// [[Rcpp::export]]
void rcpp_project_set_metadata(SEXP project_xptr, std::string metadata_json) {
  project_call([&]() {
    project_from_xptr(project_xptr).set_metadata(project::detail::json_from_text(metadata_json));
    return 0;
  });
}

// [[Rcpp::export]]
std::string rcpp_project_get_workflow(SEXP project_xptr) {
  return project_call([&]() { return project::detail::json_to_text(project_from_xptr(project_xptr).workflow()); });
}

// [[Rcpp::export]]
void rcpp_project_set_workflow(SEXP project_xptr, std::string workflow_json) {
  project_call([&]() {
    project_from_xptr(project_xptr).set_workflow(project::detail::json_from_text(workflow_json));
    return 0;
  });
}

// [[Rcpp::export]]
CharacterVector rcpp_project_list_tables(SEXP project_xptr) {
  return project_call([&]() {
    const auto tables = project_from_xptr(project_xptr).list_tables();
    return CharacterVector(tables.begin(), tables.end());
  });
}

// [[Rcpp::export]]
DataFrame rcpp_project_get_audit(SEXP project_xptr) {
  return project_call([&]() {
    const auto out = project_from_xptr(project_xptr).get_audit();
    return audit_rows_to_df(out);
  });
}
