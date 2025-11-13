# MARK: EngineDataBase
# EngineDataBase -----
#' @title File-based Database Engine for StreamFind
#' @description The [StreamFind::EngineDataBase] R6 class provides file-based storage for StreamFind Engine data using DuckDB. Data is accessed by reading from the database instead of keeping it in memory.
#' @export
#'
EngineDataBase <- R6::R6Class(
  classname = "EngineDataBase",

  # MARK: private
  # private -----
  private = list(
    .db = NULL,
    .conn = NULL,
    .id = NULL,
    .data_type = NULL,

    # Initialize database connection
    .connect = function() {
      if (is.null(private$.conn) || !DBI::dbIsValid(private$.conn)) {
        if (!requireNamespace("duckdb", quietly = TRUE)) {
          stop("duckdb package is required for EngineDataBase")
        }
        private$.conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      }
    },

    # Disconnect from database
    .disconnect = function() {
      if (!is.null(private$.conn) && DBI::dbIsValid(private$.conn)) {
        DBI::dbDisconnect(private$.conn)
        private$.conn <- NULL
      }
    },

    # Ensure engine exists in database
    .ensure_engine_exists = function() {
      private$.connect()

      # Check if engine exists
      existing <- DBI::dbGetQuery(private$.conn, "
        SELECT COUNT(*) as count FROM engines WHERE id = ?
      ", list(private$.id))

      if (existing$count[1] == 0) {
        # Create new engine record
        DBI::dbExecute(private$.conn, "
          INSERT INTO Engines (id, data_type)
          VALUES (?, ?)
        ", list(private$.id, private$.data_type))
      }
    },

    # MARK: finalize
    #' @description Cleanup when object is destroyed
    finalize = function() {
      private$.disconnect()
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
    #' @description Initialize EngineDataBase
    #' @param db Path to DuckDB database file
    #' @param id Unique engine identifier
    #' @param data_type Type of engine data (e.g., "MassSpec", "Raman")
    initialize = function(db, id = NULL, data_type = "Generic") {

      # Validate inputs
      checkmate::assert_character(db, len = 1)
      checkmate::assert_character(data_type, len = 1)

      # Set private fields
      private$.db <- db
      private$.id <- id %||% .generate_id("engine_")
      private$.data_type <- data_type

      # Create database schema if needed
      private$.connect()
      create_engine_duckdb_schema(private$.conn)

      # Check if engine table has required JSON columns (for existing databases)
      tryCatch({
        table_info <- DBI::dbGetQuery(private$.conn, "PRAGMA table_info(Engines)")
        has_metadata <- "metadata" %in% table_info$name
        has_config <- "configuration" %in% table_info$name
        if (!has_metadata) {
          message("Adding missing metadata column to Engines table...")
          DBI::dbExecute(private$.conn, "
            ALTER TABLE Engines ADD COLUMN metadata JSON
          ")
        }
        if (!has_config) {
          message("Adding missing configuration column to engines table...")
          DBI::dbExecute(private$.conn, "
            ALTER TABLE Engines ADD COLUMN configuration JSON
          ")
        }
      }, error = function(e) {
        # If table doesn't exist, create_engine_duckdb_schema already handled it
        message("Schema migration check: ", e$message)
      })

      # Ensure engine exists
      private$.ensure_engine_exists()
      private$.disconnect()

      message(paste("✓ EngineDataBase initialized with ID:", private$.id))
    },

    # MARK: get_metadata
    #' @description Get metadata from database
    #' @return A Metadata object or NULL
    get_metadata = function() {
      private$.connect()

      metadata_df <- DBI::dbGetQuery(private$.conn, "
        SELECT metadata FROM Engines WHERE id = ?
      ", list(private$.id))

      private$.disconnect()

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
      private$.connect()

      if (is.null(metadata) || length(metadata) == 0) {
        # Clear metadata
        DBI::dbExecute(private$.conn, "
          UPDATE Engines SET metadata = NULL, updated_at = CURRENT_TIMESTAMP
          WHERE id = ?
        ", list(private$.id))
        private$.disconnect()
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
      DBI::dbExecute(private$.conn, "
        UPDATE Engines
        SET metadata = ?, updated_at = CURRENT_TIMESTAMP
        WHERE id = ?
      ", list(json_data, private$.id))

      private$.disconnect()

      # Add audit trail entry
      self$add_audit_entry("update", "Metadata", NULL, list(operation = "set_metadata"))

      invisible(self)
    },

    # MARK: get_configuration
    #' @description Get configuration from database
    #' @return A configuration list or NULL
    get_configuration = function() {
      private$.connect()

      config_df <- DBI::dbGetQuery(private$.conn, "
        SELECT configuration FROM Engines WHERE id = ?
      ", list(private$.id))

      private$.disconnect()

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
      private$.connect()

      if (is.null(configuration) || length(configuration) == 0) {
        # Clear configuration
        DBI::dbExecute(private$.conn, "
          UPDATE Engines SET configuration = NULL, updated_at = CURRENT_TIMESTAMP
          WHERE id = ?
        ", list(private$.id))
        private$.disconnect()
        return(invisible(self))
      }

      if (!is.list(configuration)) {
        stop("configuration must be a named list")
      }

      # Convert to JSON
      json_data <- jsonlite::toJSON(configuration, auto_unbox = TRUE)
      json_data <- as.character(json_data)

      # Update engine configuration
      DBI::dbExecute(private$.conn, "
        UPDATE Engines
        SET configuration = ?, updated_at = CURRENT_TIMESTAMP
        WHERE id = ?
      ", list(json_data, private$.id))

      private$.disconnect()

      # Add audit trail entry
      self$add_audit_entry("update", "Configuration", NULL, list(operation = "set_configuration"))

      invisible(self)
    },

    # MARK: get_workflow
    #' @description Get workflow from database
    #' @return A Workflow object or NULL
    get_workflow = function() {
      private$.connect()

      # Get workflow info
      workflow_info <- DBI::dbGetQuery(private$.conn, "
        SELECT id, data_type, methods
        FROM Workflows
        WHERE engine_id = ?
      ", list(private$.id))

      private$.disconnect()

      if (nrow(workflow_info) == 0) {
        return(NULL)
      }

      workflow_id <- workflow_info$id[1]
      data_type <- workflow_info$data_type[1]
      methods <- workflow_info$methods[1]

      # Parse workflow data and methods (both stored as JSON)
      workflow_data <- list()

      # Parse data_type JSON
      if (!is.na(data_type) && !is.null(data_type)) {
        workflow_data$data_type <- tryCatch({
          jsonlite::fromJSON(data_type, simplifyVector = FALSE)
        }, error = function(e) {
          warning("Could not parse workflow data_type: ", e$message)
          NULL
        })
      }

      # Parse methods JSON
      if (!is.na(methods) && !is.null(methods)) {
        workflow_data$methods <- tryCatch({
          jsonlite::fromJSON(methods, simplifyVector = FALSE)
        }, error = function(e) {
          warning("Could not parse workflow methods: ", e$message)
          NULL
        })
      }

      # Return workflow or list
      if (exists("Workflow")) {
        return(Workflow(workflow_data))
      } else {
        return(workflow_data)  # Fallback
      }
    },

    # MARK: set_workflow
    #' @description Set workflow in database
    #' @param workflow A Workflow object or list with data_type and methods
    set_workflow = function(workflow) {
      private$.connect()

      # Clear existing workflow
      DBI::dbExecute(private$.conn, "DELETE FROM Workflows WHERE engine_id = ?",
                     list(private$.id))

      if (is.null(workflow) || length(workflow) == 0) {
        private$.disconnect()
        return(invisible(self))
      }

      workflow_id <- .generate_id("workflow_")

      # Extract data_type and methods from workflow
      data_type_json <- NULL
      methods_json <- NULL

      if (is.list(workflow)) {
        if (!is.null(workflow$data_type)) {
          data_type_json <- as.character(jsonlite::toJSON(workflow$data_type, auto_unbox = TRUE, null = "null"))
        }
        if (!is.null(workflow$methods)) {
          methods_json <- as.character(jsonlite::toJSON(workflow$methods, auto_unbox = TRUE, null = "null"))
        }
      } else {
        # If workflow is not a list, treat as methods
        methods_json <- as.character(jsonlite::toJSON(workflow, auto_unbox = TRUE, null = "null"))
      }

      # Insert workflow record
      DBI::dbExecute(private$.conn, "
        INSERT INTO Workflows (id, engine_id, data_type, methods, created_at, updated_at)
        VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
      ", list(workflow_id, private$.id, data_type_json, methods_json))

      private$.disconnect()

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
      private$.connect()

      audit_df <- DBI::dbGetQuery(private$.conn, "
        SELECT * FROM AuditTrails
        WHERE engine_id = ?
        ORDER BY timestamp DESC
      ", list(private$.id))

      private$.disconnect()

      audit_df
    },

    # MARK: add_audit_entry
    #' @description Add entry to audit trail
    #' @param operation_type Type of operation
    #' @param object_type Type of object
    #' @param object_id Object identifier
    #' @param details Additional details
    add_audit_entry = function(operation_type, object_type, object_id = NULL, details = NULL) {
      private$.connect()

      details_json <- if (!is.null(details)) {
        jsonlite::toJSON(details, auto_unbox = TRUE, null = "null")
      } else {
        "null"
      }

      if (is.null(object_id)) {
        object_id <- "null"
      }

      DBI::dbExecute(private$.conn, "
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

      private$.disconnect()
      invisible(self)
    },

    # MARK: query_database
    #' @description Execute SQL query on the database
    #' @param sql SQL query string
    #' @param params Query parameters
    #' @return Query results
    query_database = function(sql, params = NULL) {
      private$.connect()
      if (is.null(params)) {
        res <- DBI::dbGetQuery(private$.conn, sql)
      } else {
        res <- DBI::dbGetQuery(private$.conn, sql, params)
      }
      private$.disconnect()
      res
    },

    # MARK: get_engine_info
    #' @description Get basic engine information
    #' @return List with engine information
    get_engine_info = function() {
      private$.connect()
      engine_info <- DBI::dbGetQuery(private$.conn, "
        SELECT * FROM Engines WHERE id = ?
      ", list(private$.id))
      private$.disconnect()
      if (nrow(engine_info) == 0) {
        return(NULL)
      }
      as.list(engine_info[1, ])
    },

    # MARK: list_tables
    #' @description List all tables in the database
    #' @return Character vector of table names
    list_tables = function() {
      private$.connect()
      res <- DBI::dbListTables(private$.conn)
      private$.disconnect()
      res
    },

    # MARK: get_table_info
    #' @description Get information about a specific table
    #' @param table_name Name of the table
    #' @return Data frame with table information
    get_table_info = function(table_name) {
      private$.connect()
      res <- NULL
      tryCatch({
        # Get row count
        count_result <- DBI::dbGetQuery(private$.conn,
          paste("SELECT COUNT(*) as row_count FROM", table_name))
        # Get column info
        columns <- DBI::dbGetQuery(private$.conn,
          paste("PRAGMA table_info(", table_name, ")"))
        private$.disconnect()
        res <- list(
          table_name = table_name,
          row_count = count_result$row_count[1],
          columns = columns
        )
      }, error = function(e) {
        warning("Error getting table info for ", table_name, ": ", e$message)
      })
      private$.disconnect()
      res
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

#' Create DuckDB schema for StreamFind EngineDataBase
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

  # Create indexes for better performance
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_Workflows_engine ON workflows(engine_id)")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_AuditTrails_engine ON AuditTrails(engine_id)")

  message("✓ DuckDB schema for StreamFind Engine created successfully!")
  TRUE
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x