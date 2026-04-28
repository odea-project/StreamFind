#' @title Project R6 Class
#' @description R6 wrapper for a DuckDB-backed StreamFind project.
#' @details Native bridge calls are inlined in the public methods for a single, flat interface.
#' @param db Path to the project DuckDB file.
#' @param project_id Active project identifier.
#' @export
Project <- R6::R6Class(
  classname = "Project",
  cloneable = FALSE,
  private = list(
    .ptr = NULL,
    .db = NULL,
    .project_id = NULL
  ),
  active = list(
    #' @field db Project database path (read-only).
    db = function(value) {
      if (missing(value)) {
        return(private$.db)
      }
      stop("db is read-only")
    },
    #' @field project_id Active project identifier (read-only).
    project_id = function(value) {
      if (missing(value)) {
        return(private$.project_id)
      }
      stop("project_id is read-only")
    },
    #' @field metadata Project metadata JSON stored in DuckDB.
    metadata = function(value) {
      if (missing(value)) {
        return(self$get_metadata())
      }
      self$set_metadata(value)
      invisible(self)
    },
    #' @field workflow Project workflow JSON stored in DuckDB.
    workflow = function(value) {
      if (missing(value)) {
        return(self$get_workflow())
      }
      self$set_workflow(value)
      invisible(self)
    }
  ),
  public = list(
    #' @description Create a new `Project` handle.
    #' @param db Path to the DuckDB project file.
    #' @param project_id Active project identifier.
    initialize = function(db, project_id, .ptr = NULL) {
      if (!requireNamespace("duckdb", quietly = TRUE)) {
        stop("duckdb package is required for Project")
      }
      checkmate::assert_character(db, len = 1)
      checkmate::assert_character(project_id, len = 1)
      private$.db <- db
      private$.project_id <- project_id
      private$.ptr <- if (is.null(.ptr)) rcpp_project_new(db, project_id) else .ptr
    },
    #' @description Return the native project pointer.
    #' @return External pointer to the C++ `project::Project` object.
    get_ptr = function() {
      private$.ptr
    },
    #' @description Validate the project schema and row state.
    #' @return The `Project` object invisibly.
    validate = function() {
      rcpp_project_validate(private$.ptr)
      invisible(self)
    },
    #' @description Get the project metadata.
    #' @return A list or `NULL`.
    get_metadata = function() {
      value <- rcpp_project_get_metadata(private$.ptr)
      if (is.null(value) || identical(value, "") || identical(value, "null")) NULL else jsonlite::fromJSON(value)
    },
    #' @description Set the project metadata.
    #' @param value A list, JSON string, or `NULL`.
    #' @return The `Project` object invisibly.
    set_metadata = function(value) {
      metadata_json <- if (is.null(value)) {
        "null"
      } else if (is.character(value) && length(value) == 1L) {
        value
      } else {
        as.character(.convert_to_json(value))
      }
      rcpp_project_set_metadata(private$.ptr, metadata_json)
      invisible(self)
    },
    #' @description Get the project workflow.
    #' @return A list or `NULL`.
    get_workflow = function() {
      value <- rcpp_project_get_workflow(private$.ptr)
      if (is.null(value) || identical(value, "") || identical(value, "null")) NULL else jsonlite::fromJSON(value)
    },
    #' @description Set the project workflow.
    #' @param value A list, JSON string, or `NULL`.
    #' @return The `Project` object invisibly.
    set_workflow = function(value) {
      workflow_json <- if (is.null(value)) {
        "null"
      } else if (is.character(value) && length(value) == 1L) {
        value
      } else {
        as.character(.convert_to_json(value))
      }
      rcpp_project_set_workflow(private$.ptr, workflow_json)
      invisible(self)
    },
    #' @description Return all audit entries.
    #' @return A data.frame ordered by newest first.
    get_audit = function() {
      rcpp_project_get_audit(private$.ptr)
    },
    #' @description List tables in the project database.
    #' @return A character vector of table names.
    list_tables = function() {
      rcpp_project_list_tables(private$.ptr)
    },
    #' @description Copy this project to another database and/or project id.
    #' @param db Target DuckDB file path.
    #' @param project_id Target project identifier.
    #' @return A new `Project` object.
    copy = function(db = private$.db, project_id = private$.project_id) {
      copied_ptr <- rcpp_project_copy(private$.ptr, db, project_id)
      Project$new(db, project_id, .ptr = copied_ptr)
    },
    #' @description Print a short summary.
    print = function(...) {
      cat("\nProject\n")
      cat("db: ", private$.db, "\n", sep = "")
      cat("project_id: ", private$.project_id, "\n", sep = "")
      audit_info <- try(self$get_audit(), silent = TRUE)
      if (!inherits(audit_info, "try-error")) {
        cat("audit entries: ", nrow(audit_info), "\n", sep = "")
      }
      invisible(self)
    },
    #' @description Show a short summary.
    show = function(...) {
      self$print(...)
    }
  )
)
