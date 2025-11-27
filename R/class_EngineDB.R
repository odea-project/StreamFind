# MARK: EngineDB
# EngineDB -----
#' @title File-based Database Engine for StreamFind
#' @description The [StreamFind::EngineDB] R6 class provides file-based storage for StreamFind Engine data using DuckDB. Data is accessed by reading from the database instead of keeping it in memory.
#' @export
#'
EngineDB <- R6::R6Class(
  classname = "EngineDB",

  # MARK: private
  # private -----
  private = list(
    .sf_root = NULL,
    .db = NULL,
    .id = NULL,
    .data_type = NULL,

    # Initialize database connection
    .connect = function() {
      if (!requireNamespace("duckdb", quietly = TRUE)) {
        stop("duckdb package is required for EngineDB")
      }
      DBI::dbConnect(duckdb::duckdb(), private$.db)
    },

    # MARK: finalize
    #' @description Cleanup when object is destroyed
    finalize = function() {
      invisible(NULL)
    }
  ),

  # MARK: active bindings
  # active bindings -----
  active = list(

    # MARK: Metadata
    #' @field Metadata A [StreamFind::Metadata] object loaded from database
    Metadata = function(value) {
      if (missing(value)) {
        return(self$get_metadata())
      }
      self$set_metadata(value)
      invisible(self)
    },

    # MARK: Configuration
    #' @field Configuration Engine configuration loaded from database
    Configuration = function(value) {
      if (missing(value)) {
        return(self$get_configuration())
      }
      self$set_configuration(value)
      invisible(self)
    },

    # MARK: Workflow
    #' @field Workflow A [StreamFind::Workflow] object loaded from database
    Workflow = function(value) {
      if (missing(value)) {
        return(self$get_workflow())
      }
      self$set_workflow(value)
      invisible(self)
    },

    # MARK: Analyses
    #' @field Analyses Simple dummy analyses object
    Analyses = function(value) {
      if (missing(value)) {
        return(self$get_analyses())
      }
      self$set_analyses(value)
      invisible(self)
    },

    # MARK: Results
    #' @field Results Simple dummy results object
    Results = function(value) {
      if (missing(value)) {
        return(self$get_results())
      }
      self$set_results(value)
      invisible(self)
    },

    # MARK: AuditTrail
    #' @field AuditTrail Audit trail from database (read-only)
    AuditTrail = function() {
      self$get_audit_trail()
    }
  ),

  # MARK: public methods
  # public methods -----
  public = list(

    # MARK: initialize
    #' @description Initialize EngineDB
    #' @param project_dir Path to StreamFind project folder (.sf). EngineDB will place main.duckdb inside it.
    #' @param metadata Optional metadata object or list to persist on init
    #' @param workflow Optional workflow object/list/json to persist on init
    #' @param analyses Optional analyses object (unused in base EngineDB)
    #' @param configuration Optional configuration list to persist on init
    #' @param data_type Engine data type (internal; defaults to "Unknown")
    initialize = function(project_dir = "data.sf",
                          metadata = NULL,
                          workflow = NULL,
                          analyses = NULL,
                          configuration = NULL,
                          data_type = "Unknown") {

      # Validate inputs
      checkmate::assert_character(project_dir, len = 1)
      checkmate::assert_character(data_type, len = 1)

      # Resolve .sf root and main DuckDB path
      sf_root <- project_dir
      main_db <- project_dir
      if (tolower(tools::file_ext(sf_root)) == "duckdb") {
        sf_root <- dirname(sf_root)
        main_db <- project_dir
      } else {
        main_db <- file.path(sf_root, "main.duckdb")
      }

      # Ensure target directory exists and set private fields
      dir.create(sf_root, recursive = TRUE, showWarnings = FALSE)
      private$.sf_root <- sf_root
      private$.db <- main_db
      private$.id <- .generate_id("engine_")
      private$.data_type <- data_type

      # Create database schema if needed
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      create_engine_duckdb_schema(conn)

      # Check if engine table has required JSON columns (for existing databases)
      tryCatch({
        table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Engines)")
        has_metadata <- "metadata" %in% table_info$name
        has_config <- "configuration" %in% table_info$name
        if (!has_metadata) {
          message("Adding missing metadata column to Engines table...")
          DBI::dbExecute(conn, "
            ALTER TABLE Engines ADD COLUMN metadata JSON
          ")
        }
        if (!has_config) {
          message("Adding missing configuration column to engines table...")
          DBI::dbExecute(conn, "
            ALTER TABLE Engines ADD COLUMN configuration JSON
          ")
        }
      }, error = function(e) {
        # If table doesn't exist, create_engine_duckdb_schema already handled it
        message("Schema migration check: ", e$message)
      })

      # Ensure engine exists (single row)
      existing <- DBI::dbGetQuery(conn, "SELECT id, data_type FROM Engines")
      if (nrow(existing) > 0) {
        if (!is.null(private$.id) && private$.id != existing$id[1]) {
          message("EngineDB: using existing engine id ", existing$id[1], " (ignoring provided id).")
        }
        private$.id <- existing$id[1]
        DBI::dbExecute(conn, "
          UPDATE Engines SET data_type = ?
          WHERE id = ?
        ", list(private$.data_type, private$.id))
      } else {
        DBI::dbExecute(conn, "
          INSERT INTO Engines (id, data_type)
          VALUES (?, ?)
        ", list(private$.id, private$.data_type))
      }

      # Persist project_dir and main_db path into configuration if not already present
      current_cfg <- self$get_configuration()
      if (is.null(current_cfg)) current_cfg <- list()
      path_cfg <- list(project_dir = private$.sf_root, main_db = private$.db)
      missing_keys <- setdiff(names(path_cfg), names(current_cfg))
      if (length(missing_keys) > 0) {
        for (nm in missing_keys) current_cfg[[nm]] <- path_cfg[[nm]]
        self$set_configuration(current_cfg)
      }

      # Seed optional data
      if (!is.null(metadata)) {
        try(self$set_metadata(metadata), silent = TRUE)
      }
      if (!is.null(configuration)) {
        try(self$set_configuration(configuration), silent = TRUE)
      }
      if (!is.null(workflow)) {
        try(self$set_workflow(workflow), silent = TRUE)
      }
      if (!is.null(analyses)) {
        try(self$set_analyses(analyses), silent = TRUE)
      }

      message(paste("EngineDB initialized with ID:", private$.id))
    },

    # MARK: get_metadata
    #' @description Get metadata from database
    #' @return A Metadata object or NULL
    get_metadata = function() {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      metadata_df <- DBI::dbGetQuery(conn, "
        SELECT metadata FROM Engines WHERE id = ?
      ", list(private$.id))

      if (nrow(metadata_df) == 0) {
        return(NULL)
      }

      # Parse JSON metadata
      json_data <- metadata_df$metadata[1]
      if (is.na(json_data) || is.null(json_data)) {
        return(NULL)
      }

      # Convert JSON to R list
      entries <- tryCatch({
        jsonlite::fromJSON(json_data, simplifyVector = FALSE)
      }, error = function(e) {
        warning("Could not parse metadata JSON: ", e$message)
        NULL
      })

      if (is.null(entries)) return(NULL)

      # Create and return Metadata object
      if (exists("Metadata")) {
        Metadata(entries = entries)
      } else {
        entries
      }
    },

    # MARK: set_metadata
    #' @description Set metadata in database
    #' @param metadata A Metadata object or named list
    set_metadata = function(metadata) {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)

      if (is.null(metadata) || length(metadata) == 0) {
        # Clear metadata
        DBI::dbExecute(conn, "
          UPDATE Engines SET metadata = NULL, updated_at = CURRENT_TIMESTAMP
        WHERE id = ?
        ", list(private$.id))
        DBI::dbExecute(conn, "COMMIT")
        rollback_needed <- FALSE
        return(invisible(self))
      }

      # Convert to list if Metadata object
      if (is(metadata, "Metadata")) {
        entries <- as.list(metadata)
      } else if (is.list(metadata)) {
        entries <- metadata
      } else {
        stop("metadata must be a Metadata object or named list")
      }

      # Convert to JSON
      json_data <- jsonlite::toJSON(entries, auto_unbox = TRUE)

      # Update engine metadata
      DBI::dbExecute(conn, "
        UPDATE Engines
        SET metadata = ?, updated_at = CURRENT_TIMESTAMP
        WHERE id = ?
      ", list(json_data, private$.id))
      DBI::dbExecute(conn, "COMMIT")
      rollback_needed <- FALSE

      # Add audit trail entry
      self$add_audit_entry("update", "Metadata", NULL, list(operation = "set_metadata"))

      invisible(self)
    },

    # MARK: get_configuration
    #' @description Get configuration from database
    #' @return A configuration list or NULL
    get_configuration = function() {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      config_df <- DBI::dbGetQuery(conn, "
        SELECT configuration FROM Engines WHERE id = ?
      ", list(private$.id))

      if (nrow(config_df) == 0) {
        return(NULL)
      }

      # Parse JSON configuration
      json_data <- config_df$configuration[1]
      if (is.na(json_data) || is.null(json_data)) {
        return(NULL)
      }

      # Convert JSON to R list
      config <- tryCatch({
        jsonlite::fromJSON(json_data, simplifyVector = FALSE)
      }, error = function(e) {
        warning("Could not parse configuration JSON: ", e$message)
        return(NULL)
      })

      return(config)
    },

    # MARK: set_configuration
    #' @description Set configuration in database
    #' @param configuration A configuration list
    set_configuration = function(configuration) {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)

      if (is.null(configuration) || length(configuration) == 0) {
        # Clear configuration
        DBI::dbExecute(conn, "
          UPDATE Engines SET configuration = NULL, updated_at = CURRENT_TIMESTAMP
        WHERE id = ?
        ", list(private$.id))
        DBI::dbExecute(conn, "COMMIT")
        rollback_needed <- FALSE
        return(invisible(self))
      }

      if (!is.list(configuration)) {
        stop("configuration must be a named list")
      }

      # Convert to JSON
      json_data <- jsonlite::toJSON(configuration, auto_unbox = TRUE)
      json_data <- as.character(json_data)

      # Update engine configuration
      DBI::dbExecute(conn, "
        UPDATE Engines
        SET configuration = ?, updated_at = CURRENT_TIMESTAMP
        WHERE id = ?
      ", list(json_data, private$.id))
      DBI::dbExecute(conn, "COMMIT")
      rollback_needed <- FALSE

      # Add audit trail entry
      self$add_audit_entry("update", "Configuration", NULL, list(operation = "set_configuration"))

      invisible(self)
    },

    # MARK: get_workflow
    #' @description Get workflow from database
    #' @return A Workflow object or NULL
    get_workflow = function() {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      # Get workflow info
      workflow_info <- DBI::dbGetQuery(conn, "
        SELECT id, data_type, methods
        FROM Workflows
        WHERE engine_id = ?
      ", list(private$.id))

      if (nrow(workflow_info) == 0) {
        return(NULL)
      }

      methods_json <- workflow_info$methods[1]
      data_type_val <- workflow_info$data_type[1]

      # Unwrap data_type JSON if present
      if (!is.na(data_type_val) && !is.null(data_type_val) && nzchar(data_type_val)) {
        data_type_val <- tryCatch(
          jsonlite::fromJSON(data_type_val),
          error = function(e) data_type_val
        )
      }

      if (is.na(methods_json) || is.null(methods_json)) {
        return(NULL)
      }

      wf_obj <- tryCatch({
        raw_steps <- jsonlite::fromJSON(methods_json, simplifyVector = FALSE)
        if (length(raw_steps) == 0) {
          Workflow()
        } else {
          steps <- lapply(raw_steps, function(step) {
            if (is(step, "ProcessingStep")) return(step)
            if (is.list(step)) {
              as.ProcessingStep(step)
            } else {
              NULL
            }
          })
          steps <- Filter(Negate(is.null), steps)
          Workflow(steps)
        }
      }, error = function(e) {
        warning("Could not reconstruct workflow: ", e$message)
        return(NULL)
      })

      # If stored type is available and workflow is empty, attach it for reference
      if (!is.null(wf_obj) && length(wf_obj) == 0 && !is.na(data_type_val) && nzchar(data_type_val)) {
        attr(wf_obj, "type") <- data_type_val
      }

      wf_obj
    },

    # MARK: set_workflow
    #' @description Set workflow in database
    #' @param workflow A Workflow object, a list of ProcessingStep definitions, or a JSON string representing such a list
    set_workflow = function(workflow) {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)

      # Clear existing workflow
      DBI::dbExecute(conn, "DELETE FROM Workflows WHERE engine_id = ?",
                     list(private$.id))

      if (is.null(workflow) || length(workflow) == 0) {
        DBI::dbExecute(conn, "COMMIT")
        rollback_needed <- FALSE
        return(invisible(self))
      }

      workflow_id <- .generate_id("workflow_")

      # Normalize to Workflow object
      wf_obj <- NULL
      if (is(workflow, "Workflow")) {
        wf_obj <- workflow
      } else if (is.character(workflow) && length(workflow) == 1) {
        wf_obj <- tryCatch({
          Workflow(jsonlite::fromJSON(workflow, simplifyVector = FALSE))
        }, error = function(e) {
          stop("Invalid workflow JSON: ", e$message)
        })
      } else if (is.list(workflow)) {
        wf_obj <- Workflow(workflow)
      } else {
        stop("workflow must be a Workflow object, list, or JSON string")
      }

      # For Unknown data_type engines, only allow empty workflows (no ProcessingStep classes available)
      if (identical(private$.data_type, "Unknown") && length(wf_obj) > 0) {
        stop("Cannot assign workflow steps for Unknown data_type. Use a typed EngineDB child.")
      }

      wf_type <- attr(wf_obj, "type")
      steps_json <- jsonlite::toJSON(
        lapply(wf_obj, function(step) {
          step_list <- as.list(step)
          attributes(step_list) <- NULL
          step_list
        }),
        auto_unbox = TRUE,
        null = "null"
      )

      data_type_json <- jsonlite::toJSON(wf_type, auto_unbox = TRUE, null = "null")

      # Insert workflow record
      DBI::dbExecute(conn, "
        INSERT INTO Workflows (id, engine_id, data_type, methods, created_at, updated_at)
        VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
      ", list(workflow_id, private$.id, data_type_json, steps_json))
      DBI::dbExecute(conn, "COMMIT")
      rollback_needed <- FALSE

      # Add audit trail entry
      self$add_audit_entry("update", "Workflow", workflow_id, list(operation = "set_workflow"))

      invisible(self)
    },

    # MARK: get_analyses (dummy)
    #' @description Get dummy analyses object
    #' @return A simple analyses list
    get_analyses = function() {
      list(
        type = private$.engine_data_type,
        analyses = list(),
        results = list()
      )
    },

    # MARK: set_analyses (dummy)
    #' @description Set dummy analyses object
    #' @param analyses Analyses object (ignored in dummy implementation)
    set_analyses = function(analyses) {
      # Dummy implementation - just add audit trail
      self$add_audit_entry("update", "Analyses", NULL, list(operation = "set_analyses"))
      invisible(self)
    },

    # MARK: get_results (dummy)
    #' @description Get dummy results object
    #' @return A simple results list
    get_results = function() {
      list()
    },

    # MARK: set_results (dummy)
    #' @description Set dummy results object
    #' @param results Results object (ignored in dummy implementation)
    set_results = function(results) {
      # Dummy implementation - just add audit trail
      self$add_audit_entry("update", "Results", NULL, list(operation = "set_results"))
      invisible(self)
    },

    # MARK: get_audit_trail
    #' @description Get audit trail from database
    #' @return A data.frame with audit trail entries
    get_audit_trail = function() {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      audit_df <- DBI::dbGetQuery(conn, "
        SELECT * FROM AuditTrails
        WHERE engine_id = ?
        ORDER BY timestamp DESC
      ", list(private$.id))

      audit_df
    },

    # MARK: add_audit_entry
    #' @description Add entry to audit trail
    #' @param operation_type Type of operation
    #' @param object_type Type of object
    #' @param object_id Object identifier
    #' @param details Additional details
    add_audit_entry = function(operation_type, object_type, object_id = NULL, details = NULL) {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)

      details_json <- if (!is.null(details)) {
        jsonlite::toJSON(details, auto_unbox = TRUE, null = "null")
      } else {
        "null"
      }

      if (is.null(object_id)) {
        object_id <- "null"
      }

      DBI::dbExecute(conn, "
        INSERT INTO AuditTrails (
          id, engine_id, operation_type, object_type, object_id, operation_details
        ) VALUES (?, ?, ?, ?, ?, ?)
      ", list(
        .generate_id("audit_"),
        private$.id,
        operation_type,
        object_type,
        object_id,
        as.character(details_json)
      ))
      DBI::dbExecute(conn, "COMMIT")
      rollback_needed <- FALSE
      invisible(self)
    },

    # MARK: get_engine_info
    #' @description Get basic engine information
    #' @return List with engine information
    get_engine_info = function() {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      engine_info <- DBI::dbGetQuery(conn, "
        SELECT * FROM Engines WHERE id = ?
      ", list(private$.id))
      if (nrow(engine_info) == 0) {
        return(NULL)
      }
      as.list(engine_info[1, ])
    },

    # MARK: query_db
    #' @description Execute SQL query on the database
    #' @param sql SQL query string
    #' @param params Query parameters
    #' @return Query results
    query_db = function(sql, params = NULL) {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .query_db(conn, sql, params)
    },

    # MARK: list_db_tables
    #' @description List all tables in the database
    #' @return Character vector of table names
    list_db_tables = function() {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .list_db_tables(conn)
    },

    # MARK: get_db_table_info
    #' @description Get information about a specific table
    #' @param table_name Name of the table
    #' @return Data frame with table information
    get_db_table_info = function(table_name) {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .get_db_table_info(conn, table_name)
    }
  )
)

#' Generate a unique identifier
#'
#' @param prefix Character prefix for the ID
#' @return A unique character string
#' @noRd
.generate_id <- function(prefix = "") {
  paste0(prefix, gsub("-", "", uuid::UUIDgenerate()))
}

#' Create DuckDB schema for StreamFind EngineDB
#'
#' @param conn DuckDB connection object
#' @return TRUE if successful
#' @noRd
create_engine_duckdb_schema <- function(conn) {
  # Install JSON extension for enhanced JSON support
  tryCatch({
    DBI::dbExecute(conn, "INSTALL json")
    DBI::dbExecute(conn, "LOAD json")
  }, error = function(e) {
    warning("Could not install/load JSON extension: ", e$message)
  })

  # Core Engine table
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Engines (
      id VARCHAR PRIMARY KEY,
      data_type VARCHAR NOT NULL,
      metadata JSON,
      configuration JSON,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Workflow table
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Workflows (
      id VARCHAR PRIMARY KEY,
      engine_id VARCHAR NOT NULL,
      data_type JSON,
      methods JSON,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (engine_id) REFERENCES engines(id)
    )
  ")

  # Audit Trail table
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS AuditTrails (
      id VARCHAR PRIMARY KEY,
      engine_id VARCHAR NOT NULL,
      operation_type VARCHAR NOT NULL, -- 'create', 'update', 'delete'
      object_type VARCHAR NOT NULL, -- 'Metadata', 'Workflow', 'Analyses', etc.
      object_id VARCHAR,
      operation_details VARCHAR,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (engine_id) REFERENCES engines(id)
    )
  ")

  # Analyses catalog (paths to analysis-specific DuckDB files)
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS AnalysisCatalog (
      id VARCHAR PRIMARY KEY,
      engine_id VARCHAR NOT NULL,
      analysis_type VARCHAR NOT NULL,
      db_path VARCHAR NOT NULL,
      label VARCHAR,
      status VARCHAR DEFAULT 'ready',
      extra JSON,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (engine_id) REFERENCES engines(id)
    )
  ")

  # Create indexes for better performance
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_Workflows_engine ON Workflows(engine_id)")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_AuditTrails_engine ON AuditTrails(engine_id)")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_AnalysisCatalog_engine ON AnalysisCatalog(engine_id)")

  message("DuckDB schema for StreamFind Engine created successfully!")
  TRUE
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
