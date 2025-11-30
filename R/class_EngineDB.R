# MARK: EngineDB
# EngineDB -----
#' @title File-based Database Engine for StreamFind
#' @description The [StreamFind::EngineDB] R6 class provides file-based storage for StreamFind Engine data using DuckDB.
#' @template arg-core-project-dir
#' @template arg-core-metadata
#' @template arg-core-workflow
#' @template arg-core-configuration
#' @template arg-core-audit-trail-operation_type
#' @template arg-core-audit-trail-object_type
#' @template arg-core-audit-trail-object_id
#' @template arg-core-audit-trail-details
#' @template arg-sql-tableName
#' @template arg-sql-sql
#' @template arg-sql-params
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

    # MARK: sf_root
    #' @field sf_root Path to the StreamFind (.sf) project directory
    sf_root = function() {
      private$.sf_root
    },

    # MARK: Metadata
    #' @field Metadata A [StreamFind::Metadata] object loaded from database
    Metadata = function(value) {
      if (missing(value)) {
        return(self$get_metadata())
      }
      self$add_metadata(value)
      invisible(self)
    },

    # MARK: Configuration
    #' @field Configuration Engine configuration loaded from database
    Configuration = function(value) {
      if (missing(value)) {
        return(self$get_configuration())
      }
      self$add_configuration(value)
      invisible(self)
    },

    # MARK: Workflow
    #' @field Workflow A [StreamFind::Workflow] object loaded from database
    Workflow = function(value) {
      if (missing(value)) {
        return(self$get_workflow())
      }
      self$add_workflow(value)
      invisible(self)
    },

    # ...existing code...

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
    #' @param data_type Engine data type (internal; defaults to "Unknown")
    initialize = function(project_dir = "data.sf",
                          metadata = NULL,
                          workflow = NULL,
                          configuration = NULL,
                          data_type = "Unknown") {

      # Validate inputs
      checkmate::assert_character(project_dir, len = 1)
      checkmate::assert_character(data_type, len = 1)

      # Resolve .sf root and Engine DuckDB path
      sf_root <- project_dir
      engine_db <- project_dir
      if (tolower(tools::file_ext(sf_root)) == "duckdb") {
        sf_root <- dirname(sf_root)
        engine_db <- project_dir
      } else {
        engine_db <- file.path(sf_root, "Engine.duckdb")
      }

      # Ensure target directory exists and set private fields
      dir.create(sf_root, recursive = TRUE, showWarnings = FALSE)
      private$.sf_root <- sf_root
      private$.db <- engine_db
      private$.id <- .generate_id("engine_")
      private$.data_type <- data_type

      # Create or validate database schema
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .create_engine_duckdb_schema(conn)

      # Ensure engine exists (single row)
      existing <- DBI::dbGetQuery(conn, "SELECT id, data_type FROM Engines")
      if (nrow(existing) > 0) {
        private$.id <- existing$id[1]
        sql <- " UPDATE Engines SET data_type = ? WHERE id = ?"
        DBI::dbExecute(conn, sql, list(private$.data_type, private$.id))
      } else {
        sql <- "INSERT INTO Engines (id, data_type) VALUES (?, ?)"
        DBI::dbExecute(conn, sql, list(private$.id, private$.data_type))
      }

      # Persist project_dir and engine_db path into configuration if not already present
      current_cfg <- self$get_configuration()
      if (is.null(current_cfg)) current_cfg <- list()
      path_cfg <- list(project_dir = private$.sf_root, engine_db = private$.db)
      missing_keys <- setdiff(names(path_cfg), names(current_cfg))
      if (length(missing_keys) > 0) {
        for (nm in missing_keys) current_cfg[[nm]] <- path_cfg[[nm]]
        self$add_configuration(current_cfg)
      }

      # Seed optional data
      if (!is.null(metadata)) {
        try(self$add_metadata(metadata), silent = TRUE)
      }
      if (!is.null(configuration)) {
        try(self$add_configuration(configuration), silent = TRUE)
      }
      if (!is.null(workflow)) {
        try(self$add_workflow(workflow), silent = TRUE)
      }
      
      message(private$.data_type, " engine initialized on ", private$.sf_root)
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

    # MARK: add_metadata
    #' @description Set metadata in database
    add_metadata = function(metadata) {
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
      self$add_audit_entry("update", "Metadata", NULL, list(operation = "add_metadata"))

      invisible(self)
    },

    # MARK: get_configuration
    #' @description Get configuration from database
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

    # MARK: add_configuration
    #' @description Set configuration in database
    add_configuration = function(configuration) {
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
      self$add_audit_entry("update", "Configuration", NULL, list(operation = "add_configuration"))

      invisible(self)
    },

    # MARK: get_workflow
    #' @description Get workflow from database
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

    # MARK: add_workflow
    #' @description Set workflow in database
    add_workflow = function(workflow) {
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
      self$add_audit_entry("update", "Workflow", workflow_id, list(operation = "add_workflow"))

      invisible(self)
    },

    # ...existing code...

    # MARK: get_audit_trail
    #' @description Get audit trail from database
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
    query_db = function(sql, params = NULL) {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .query_db(conn, sql, params)
    },

    # MARK: list_db_tables
    #' @description List all tables in the database
    list_db_tables = function() {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .list_db_tables(conn)
    },

    # MARK: get_db_table_info
    #' @description Get information about a specific table
    get_db_table_info = function(tableName) {
      conn <- private$.connect()
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .get_db_table_info(conn, tableName)
    }
  )
)

# MARK: Utilities

# MARK: .generate_id
#' Generate a unique identifier
#'
#' @param prefix Character prefix for the ID
#' @return A unique character string
#' @noRd
.generate_id <- function(prefix = "") {
  paste0(prefix, gsub("-", "", uuid::UUIDgenerate()))
}

# MARK: .create_engine_duckdb_schema
#' Create DuckDB schema for StreamFind EngineDB
#'
#' @param conn DuckDB connection object
#' @return TRUE if successful
#' @noRd
.create_engine_duckdb_schema <- function(conn) {
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

  # Ensure all columns for Engines table
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Engines)")
    required <- list(
      metadata = "JSON",
      configuration = "JSON",
      created_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
      updated_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to Engines table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Engines ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    message("Schema migration check (Engines): ", e$message)
  })

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

  # Ensure all columns for Workflows table
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Workflows)")
    required <- list(
      data_type = "JSON",
      methods = "JSON",
      created_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
      updated_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to Workflows table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Workflows ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    message("Schema migration check (Workflows): ", e$message)
  })

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

  # Ensure all columns for AuditTrails table
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(AuditTrails)")
    required <- list(
      operation_type = "VARCHAR NOT NULL",
      object_type = "VARCHAR NOT NULL",
      object_id = "VARCHAR",
      operation_details = "VARCHAR",
      timestamp = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to AuditTrails table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE AuditTrails ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    message("Schema migration check (AuditTrails): ", e$message)
  })


  # Create indexes for better performance
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_Workflows_engine ON Workflows(engine_id)")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_AuditTrails_engine ON AuditTrails(engine_id)")
  # ...existing code...

  invisible(TRUE)
}
