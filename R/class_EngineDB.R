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
    .db_cache = NULL,
    .data_type = NULL
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
    initialize = function(
      project_dir = "data.sf",
      metadata = NULL,
      workflow = NULL,
      configuration = NULL,
      data_type = "Unknown") {
      
      if (!requireNamespace("duckdb", quietly = TRUE)) {
        stop("duckdb package is required for EngineDB")
      }

      checkmate::assert_character(project_dir, len = 1)
      checkmate::assert_character(data_type, len = 1)
      
      sf_root <- project_dir
      engine_db <- project_dir
      if (tolower(tools::file_ext(sf_root)) == "duckdb") {
        sf_root <- dirname(sf_root)
        engine_db <- project_dir
      } else {
        engine_db <- file.path(sf_root, "Engine.duckdb")
      }
      dir.create(sf_root, recursive = TRUE, showWarnings = FALSE)
      private$.sf_root <- sf_root
      private$.db <- engine_db
      private$.db_cache <- file.path(sf_root, "Cache.duckdb")
      private$.data_type <- data_type
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      conn_cache <- DBI::dbConnect(duckdb::duckdb(), private$.db_cache)
      on.exit(DBI::dbDisconnect(conn_cache), add = TRUE)
      
      .create_EngineDB_db_schema(conn)
      .validate_EngineDB_db_schema(conn)
      
      existing <- DBI::dbGetQuery(conn, "SELECT rowid, data_type FROM Engine")
      if (nrow(existing) > 0) {
        sql <- "UPDATE Engine SET data_type = ? WHERE rowid = ?"
        DBI::dbExecute(conn, sql, list(private$.data_type, existing$rowid[1]))
      } else {
        sql <- "INSERT INTO Engine (data_type) VALUES (?)"
        DBI::dbExecute(conn, sql, list(private$.data_type))
        self$add_metadata(Metadata())
      }

      existing_wf <- DBI::dbGetQuery(conn, "SELECT rowid FROM Workflow")
      if (nrow(existing_wf) == 0) {
        sql_wf <- "INSERT INTO Workflow (data_type, methods) VALUES (?, NULL)"
        DBI::dbExecute(conn, sql_wf, list(private$.data_type))
      } else {
        sql_wf <- "UPDATE Workflow SET data_type = ? WHERE rowid = ?"
        DBI::dbExecute(conn, sql_wf, list(private$.data_type, existing_wf$rowid[1]))
      }

      # current_cfg <- self$get_configuration()
      # if (is.null(current_cfg)) current_cfg <- list()
      # path_cfg <- list(sf_root = private$.sf_root, db = private$.db)
      # missing_keys <- setdiff(names(path_cfg), names(current_cfg))
      # if (length(missing_keys) > 0) {
      #   for (nm in missing_keys) current_cfg[[nm]] <- path_cfg[[nm]]
      #   self$add_configuration(current_cfg)
      # }

      if (!is.null(metadata)) {
        try(self$add_metadata(metadata), silent = TRUE)
      }
      # if (!is.null(configuration)) {
      #   try(self$add_configuration(configuration), silent = TRUE)
      # }
      if (!is.null(workflow)) {
        try(self$add_workflow(workflow), silent = TRUE)
      }
      message(private$.data_type, " engine initialized on ", private$.sf_root)
    },

    # MARK: print
    #' @description Prints a summary to the console.
    print = function() {
      cat("\n")
      cat(paste0(private$.data_type, " Engine with DuckDB Backend\n"))
      cat("\n")
      cat("Metadata\n")
      show(self$Metadata)
      cat("\n")
      # cat("Workflow\n")
      # show(self$Workflow)
    },

    # MARK: get_metadata
    #' @description Get metadata from database
    #' @return A Metadata object or NULL
    get_metadata = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      metadata_df <- DBI::dbGetQuery(conn, "SELECT metadata FROM Engine LIMIT 1")
      if (nrow(metadata_df) == 0) return(NULL)
      json_data <- metadata_df$metadata[1]
      if (is.na(json_data) || is.null(json_data)) return(NULL)
      mtd <- tryCatch({
        Metadata(jsonlite::fromJSON(json_data))
      }, error = function(e) {
        warning("Could not parse metadata JSON: ", e$message)
        NULL
      })
      mtd
    },

    # MARK: add_metadata
    #' @description Set metadata in database
    add_metadata = function(metadata) {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
      metadata <- Metadata(metadata)
      json_data <- .convert_to_json(metadata)
      DBI::dbExecute(conn, "UPDATE Engine SET metadata = ? WHERE rowid = ?", list(json_data, 0))
      DBI::dbExecute(conn, "COMMIT")
      rollback_needed <- FALSE
      invisible(self)
    },

    # MARK: get_configuration
    #' @description Get configuration from database
    get_configuration = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      config_df <- DBI::dbGetQuery(conn, "SELECT configuration FROM Engine LIMIT 1")
      if (nrow(config_df) == 0) return(NULL)
      json_data <- config_df$configuration[1]
      if (is.na(json_data) || is.null(json_data)) return(NULL)
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
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)

      if (is.null(configuration) || length(configuration) == 0) {
        # Clear configuration
        DBI::dbExecute(conn, "
          UPDATE Engine SET configuration = NULL WHERE id = ?", list(private$.id))
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
        UPDATE Engine
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
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      workflow_info <- DBI::dbGetQuery(conn, "SELECT methods FROM Workflow LIMIT 1")
      if (nrow(workflow_info) == 0) {
        wf_obj <- Workflow()
        attr(wf_obj, "type") <- private$.data_type
        return(wf_obj)
      }
      methods_json <- workflow_info$methods[1]
      if (is.na(methods_json) || is.null(methods_json)) return(NULL)
      wf_obj <- tryCatch({
        Workflow(jsonlite::fromJSON(methods_json))
      }, error = function(e) {
        warning("Could not reconstruct workflow: ", e$message)
        wf_obj <- Workflow()
        attr(wf_obj, "type") <- private$.data_type
      })
      wf_obj
    },

    # MARK: add_workflow
    #' @description Set workflow in database
    add_workflow = function(workflow) {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
      wf_obj <- Workflow(workflow)
      processing_steps <- lapply(wf_obj, function(s) unclass(s))
      names(processing_steps) <- names(wf_obj)
      json_processing_steps <- .convert_to_json(processing_steps)
      DBI::dbExecute(conn, "UPDATE Workflow SET methods = ? WHERE rowid = ?", list(json_processing_steps, 0))
      DBI::dbExecute(conn, "COMMIT")
      rollback_needed <- FALSE
      self$add_audit_entry("add", "Workflow", list(processing_steps = names(wf_obj)))
      invisible(self)
    },

    # MARK: run
    #' @description Runs a processing method defined by the [StreamFind::ProcessingStep] object.
    #' @param step A [StreamFind::ProcessingStep] object.
    #'
    run = function(step = NULL) {
      if (is.null(step)) {
        warning("No ProcessingStep provided!")
        return(invisible(self))
      }
      if (!inherits(step, "ProcessingStep")) {
        warning("ProcessingStep not valid!")
        return(invisible(self))
      }
      if (!is.null(validate_object(step))) {
        warning("Invalid ProcessingStep object! Not run.")
        return(invisible(self))
      }
      type <- step$type
      if (!checkmate::test_true(type %in% private$.data_type)) {
        warning("Data type ", type, " not matching with current engine! Not done.")
        return(invisible(self))
      }
      call <- class(step)[1]
      available_processing_steps <- .get_available_processing_methods(step$type)
      if (!call %in% available_processing_steps) {
        warning(paste0(call, " not available!"))
        return(invisible(self))
      }
      if (length(self$Workflow) == 0) {
        self$Workflow <- Workflow()
        
      }
      message("\U2699 Running ", step$method, " using ", step$algorithm)
      processed <- run(step, self)
      
      if (processed) {
        if (step$method %in% get_methods(self$Workflow)) {
          if (step$number_permitted > 1) {
            self$Workflow[length(self$Workflow) + 1] <- step
          } else {
            step_idx <- which(get_methods(self$Workflow) %in% step$method)
            self$Workflow[step_idx] <- step
          }
        } else {
          self$Workflow[[length(self$Workflow) + 1]] <- step
        }
        self$add_audit_entry(
          operation_type = "run",
          object_type = "ProcessingStep",
          details = list(
            method = step$method,
            algorithm = step$algorithm
          )
        )
      }
      invisible(self)
    },

    # MARK: get_audit_trail
    #' @description Get audit trail from database
    get_audit_trail = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      audit_df <- DBI::dbGetQuery(conn, "SELECT * FROM AuditTrail ORDER BY timestamp DESC")
      audit_df
    },

    # MARK: add_audit_entry
    #' @description Add entry to audit trail
    add_audit_entry = function(operation_type, object_type, details = NULL) {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
      if (!is.null(details)) {
        details_json <- .convert_to_json(details)
      } else {
        details_json <- "null"
      }
      DBI::dbExecute(conn, "
        INSERT INTO AuditTrail (
          operation_type, object_type, operation_details
        ) VALUES (?, ?, ?)
      ", list(
        operation_type,
        object_type,
        as.character(details_json)
      ))
      DBI::dbExecute(conn, "COMMIT")
      rollback_needed <- FALSE
      invisible(self)
    },

    # MARK: get_engine_info
    #' @description Get basic engine information
    get_engine_info = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      engine_info <- DBI::dbGetQuery(conn, "SELECT * FROM Engine LIMIT 1")
      if (nrow(engine_info) == 0) {
        return(NULL)
      }
      as.list(engine_info[1, ])
    },

    # MARK: query_db
    #' @description Execute SQL query on the database
    query_db = function(sql, params = NULL) {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .query_db(conn, sql, params)
    },

    # MARK: list_db_tables
    #' @description List all tables in the database
    list_db_tables = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .list_db_tables(conn)
    },

    # MARK: get_db_table_info
    #' @description Get information about a specific table
    get_db_table_info = function(tableName) {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .get_db_table_info(conn, tableName)
    }
  )
)

# MARK: .create_EngineDB_db_schema
#' @noRd
.create_EngineDB_db_schema <- function(conn) {
  tryCatch({
    DBI::dbExecute(conn, "INSTALL json")
    DBI::dbExecute(conn, "LOAD json")
  }, error = function(e) {
    warning("Could not load JSON extension: ", e$message)
  })

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Engine (
      data_type VARCHAR NOT NULL,
      metadata JSON,
      configuration JSON,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Workflow (
      data_type VARCHAR NOT NULL,
      methods JSON,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS AuditTrail (
      operation_type VARCHAR NOT NULL, -- 'create', 'update', 'delete'
      object_type VARCHAR NOT NULL, -- 'Metadata', 'Workflow', 'Analyses', etc.
      operation_details VARCHAR,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  invisible(TRUE)
}

# MARK: .validate_EngineDB_db_schema
#' @noRd
.validate_EngineDB_db_schema <- function(conn) {
  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Engine)")
    required <- list(
      metadata = "JSON",
      configuration = "JSON",
      created_at = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to Engine table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Engine ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (Engine): ", e$message)
  })

  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Workflow)")
    required <- list(
      data_type = "VARCHAR NOT NULL",
      methods = "JSON"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to Workflow table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE Workflow ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (Workflow): ", e$message)
  })

  tryCatch({
    table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(AuditTrail)")
    required <- list(
      operation_type = "VARCHAR NOT NULL",
      object_type = "VARCHAR NOT NULL",
      operation_details = "VARCHAR",
      timestamp = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    )
    for (col in names(required)) {
      if (!(col %in% table_info$name)) {
        message(sprintf("Adding missing %s column to AuditTrail table...", col))
        DBI::dbExecute(conn, sprintf("ALTER TABLE AuditTrails ADD COLUMN %s %s", col, required[[col]]))
      }
    }
  }, error = function(e) {
    stop("Schema migration check (AuditTrail): ", e$message)
  })

  invisible(TRUE)
}
