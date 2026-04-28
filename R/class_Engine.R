# MARK: Engine
# Engine -----
#' @title Unified DuckDB-based Engine for StreamFind
#' @description The [StreamFind::Engine] R6 class provides project storage in a single unified DuckDB file for all tables and components within StreamFind.
#' @param db Path to DuckDB project file (with .duckdb extension)
#' @template arg-core-metadata
#' @template arg-core-workflow
#' @template arg-core-audit-trail-operation_type
#' @template arg-core-audit-trail-object_type
#' @template arg-core-audit-trail-object_id
#' @template arg-core-audit-trail-details
#' @template arg-sql-tableName
#' @template arg-sql-sql
#' @template arg-sql-params
#' @export
#'
#' @details The unified DuckDB file holds all Engine and project-related data for this project (all tables: Engine, Workflow, AuditTrail, Cache, etc).
Engine <- R6::R6Class(
  classname = "Engine",

  # MARK: private
  # private -----
  private = list(
    .db = NULL,
    .dataType = NULL
  ),

  # MARK: active bindings
  # active bindings -----
  active = list(
    # MARK: Metadata
    #' @field Metadata A [StreamFind::Metadata] object loaded from database.
    Metadata = function(value) {
      if (missing(value)) {
        return(self$get_metadata())
      }
      self$add_metadata(value)
      invisible(self)
    },

    # MARK: Workflow
    #' @field Workflow A [StreamFind::Workflow] object loaded from database.
    Workflow = function(value) {
      if (missing(value)) {
        return(self$get_workflow())
      }
      self$add_workflow(value)
      invisible(self)
    },

    # MARK: Analyses
    #' @field Analyses A [StreamFind::Analyses] child object.
    Analyses = function() {
      NULL
    },

    # MARK: AuditTrail
    #' @field AuditTrail A [StreamFind::AuditTrail] object for this engine (DB-backed, see [AuditTrail] class)
    AuditTrail = function() {
      AuditTrail(db = private$.db)
    },

    # MARK: Cache
    #' @field Cache A [StreamFind::Cache] object for managing cached data.
    Cache = function() {
      Cache(db = private$.db)
    },

    # MARK: db
    #' @field db Path to the DuckDB database file (read-only).
    db = function() {
      private$.db
    }
  ),

  # MARK: public methods
  # public methods -----
  public = list(
    # MARK: initialize
    #' @description Initialize Engine.
    #' @param dataType Engine data type (internal; defaults to "Unknown").
    #' @param db Path to DuckDB file for the project.
    initialize = function(db = NULL,
                          metadata = NULL,
                          workflow = NULL,
                          dataType = "Unknown") {
      if (!requireNamespace("duckdb", quietly = TRUE)) {
        stop("duckdb package is required for Engine")
      }
      checkmate::assert_character(db, len = 1)
      checkmate::assert_character(dataType, len = 1)
      engine_db <- db
      db_dir <- dirname(engine_db)
      dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
      private$.db <- engine_db
      private$.dataType <- dataType
      conn <- DBI::dbConnect(duckdb::duckdb(), engine_db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .create_Engine_db_schema(conn, private$.dataType)
      .validate_Engine_db_schema(conn)
      .create_Cache_db_schema(conn)
      .validate_Cache_db_schema(conn)
      if (!is.null(metadata)) {
        try(self$add_metadata(metadata), silent = TRUE)
      }
      if (!is.null(workflow)) {
        try(self$add_workflow(workflow), silent = TRUE)
      }
      message(private$.dataType, " engine initialized on ", private$.db)
    },

    # MARK: get_project_path
    #' @description Get the project path.
    #' @return Character string with the path to the DuckDB database file.
    get_db_path = function() {
      private$.db
    },

    # MARK: print
    #' @description Prints a summary to the console.
    print = function() {
      cat("\n")
      cat(paste0(private$.dataType, " Engine Overview\n"))
      show(self$Metadata)
      wf <- self$Workflow
      show(wf)
      anas <- self$Analyses
      if (!is.null(anas)) show(anas)
      if (file.exists(private$.db)) {
        size_bytes <- file.info(private$.db)$size
        size_mb <- round(size_bytes / (1024^2), 2)
        cat("\n")
        cat("Database file: ", basename(private$.db), " (", size_mb, " MB)\n")
      } else {
        cat("\nDatabase file not found: ", private$.db, "\n")
      }
    },

    # MARK: get_metadata
    #' @description Get metadata from database.
    #' @return A Metadata object or NULL.
    get_metadata = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      metadata_df <- DBI::dbGetQuery(conn, "SELECT metadata FROM Engine LIMIT 1")
      if (nrow(metadata_df) == 0) {
        return(NULL)
      }
      json_data <- metadata_df$metadata[1]
      if (is.na(json_data) || is.null(json_data)) {
        return(NULL)
      }
      mtd <- tryCatch(
        {
          Metadata(jsonlite::fromJSON(json_data))
        },
        error = function(e) {
          warning("Could not parse metadata JSON: ", e$message)
          NULL
        }
      )
      mtd
    },

    # MARK: add_metadata
    #' @description Set metadata in database.
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

    # MARK: get_workflow
    #' @description Get workflow from database.
    get_workflow = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      workflow_info <- DBI::dbGetQuery(conn, "SELECT methods FROM Workflow LIMIT 1")
      if (nrow(workflow_info) == 0) {
        wf_obj <- Workflow()
        attr(wf_obj, "type") <- private$.dataType
        return(wf_obj)
      }
      methods_json <- workflow_info$methods[1]
      if (is.na(methods_json) || is.null(methods_json)) {
        return(NULL)
      }
      wf_obj <- tryCatch(
        {
          Workflow(jsonlite::fromJSON(methods_json))
        },
        error = function(e) {
          warning("Could not reconstruct workflow: ", e$message)
          wf_obj <- Workflow()
          attr(wf_obj, "type") <- private$.dataType
          self$Workflow <- wf_obj
          wf_obj
        }
      )
      wf_obj
    },

    # MARK: add_workflow
    #' @description Set workflow in database.
    add_workflow = function(workflow) {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbExecute(conn, "BEGIN")
      rollback_needed <- TRUE
      on.exit(if (rollback_needed) try(DBI::dbExecute(conn, "ROLLBACK"), silent = TRUE), add = TRUE)
      wf_obj <- Workflow(workflow)
      if (length(wf_obj) == 0) attr(wf_obj, "type") <- private$.dataType
      wf_dataType <- attr(wf_obj, "type")
      if (is.null(wf_dataType) || wf_dataType != private$.dataType) {
        warning("Workflow data type (", wf_dataType, ") does not match engine data type (", private$.dataType, ")! Not added.")
        return(invisible(self))
      }
      processing_steps <- lapply(wf_obj, function(s) unclass(s))
      names(processing_steps) <- names(wf_obj)
      json_processing_steps <- .convert_to_json(processing_steps)
      DBI::dbExecute(conn, "UPDATE Workflow SET methods = ? WHERE rowid = ?", list(json_processing_steps, 0))
      DBI::dbExecute(conn, "COMMIT")
      rollback_needed <- FALSE
      invisible(self)
    },

    # MARK: run
    #' @description Runs a processing method defined by the [StreamFind::ProcessingStep] object.
    #' @param step A [StreamFind::ProcessingStep] object.
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
      if (!checkmate::test_true(type %in% private$.dataType)) {
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
            # If multiple instances are allowed, append
            self$Workflow[length(self$Workflow) + 1] <- step
          } else {
            # Otherwise, replace existing
            step_idx <- which(get_methods(self$Workflow) %in% step$method)
            self$Workflow[step_idx] <- step
          }
        } else {
          # New method, append
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

    # MARK: run_workflow
    #' @description Runs all [StreamFind::ProcessingStep] objects in the [StreamFind::Workflow].
    run_workflow = function() {
      if (length(self$Workflow) > 0) {
        steps <- self$Workflow
        results_files <- list.files(private$.db, pattern = "^Results.*\\.duckdb$", full.names = TRUE)
        if (length(results_files) > 0) {
          message("\U1F5D1 Removing existing results database files...", appendLF = FALSE)
          file.remove(results_files)
          message("Done.")
        }
        wf <- Workflow()
        self$Workflow <- wf
        lapply(steps, function(x) self$run(x))
      } else {
        warning("There are no processing steps to run!")
      }
      invisible(self)
    },

    # MARK: get_audit_trail
    #' @description Get audit trail via AuditTrail class instance (DB-backed).
    get_audit_trail = function() {
      get_audit_trail(self$AuditTrail)
    },

    # MARK: add_audit_entry
    #' @description Add entry to audit trail via AuditTrail class instance (DB-backed).
    add_audit_entry = function(operation_type, object_type, details = NULL) {
      add_audit_entry(self$AuditTrail, operation_type, object_type, details)
      invisible(self)
    },

    # MARK: clear_cache
    #' @description Clear all cached data.
    get_cache_info = function() {
      get_cache_info(self$Cache)
    },

    # MARK: get_cache_size
    #' @description Get size of cache in bytes.
    get_cache_size = function() {
      size(self$Cache)
    },

    # MARK: clear_cache
    #' @description Clear all cached data.
    clear_cache = function() {
      clear_cache(self$Cache)
      invisible(self)
    },

    # MARK: clear_result_databases
    #' @description Remove all result database files from the project directory.
    clear_result_databases = function() {
      results_files <- list.files(private$.db, pattern = "Results.*\\.duckdb$", full.names = TRUE)
      if (length(results_files) > 0) {
        message("\U1F5D1 Removing existing results database files...", appendLF = FALSE)
        file.remove(results_files)
        message("Done.")
      } else {
        message("No results database files found.")
      }
      invisible(self)
    },

    # MARK: get_engine_info
    #' @description Get basic engine information.
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
    #' @description Execute SQL query on the database.
    query_db = function(sql, params = NULL) {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .query_db(conn, sql, params)
    },

    # MARK: list_db_tables
    #' @description List all tables in the database.
    list_db_tables = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .list_db_tables(conn)
    },

    # MARK: get_db_table_info
    #' @description Get information about a specific table.
    get_db_table_info = function(tableName) {
      conn <- DBI::dbConnect(duckdb::duckdb(), private$.db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .get_db_table_info(conn, tableName)
    },

    # MARK: report_quarto
    #' @description Generates a Quarto report using the database project path as execute parameter.
    #' @param template A string with the full file path to the Quarto (.qmd) template file.
    #' @param output_file A string with the output file name (without extension). If NULL, uses the template name without extension.
    #' If a path is included, the output directory is set to `dirname(output_file)`. If only a file name is given, output is written to `execute_dir`.
    #' @param execute_dir A string with the execution directory. Default is the current working directory.
    #' @param ... Additional arguments passed to quarto::quarto_render().
    report_quarto = function(template = NULL, output_file = NULL, execute_dir = getwd(), ...) {
      if (is.null(template) || !file.exists(template)) {
        warning("Template not found!")
        return(invisible(self))
      }
      if (!requireNamespace("quarto", quietly = TRUE)) {
        warning("quarto package not installed! Please install it with: install.packages('quarto')")
        return(invisible(self))
      }

      template <- normalizePath(template, mustWork = TRUE)
      template_dir <- dirname(template)

      if (is.null(execute_dir) || !nzchar(trimws(execute_dir))) {
        execute_dir <- getwd()
      } else {
        execute_dir <- trimws(execute_dir)
      }
      execute_dir <- normalizePath(execute_dir, mustWork = FALSE)

      if (is.null(output_file)) {
        output_file <- tools::file_path_sans_ext(basename(template))
      } else {
        checkmate::assert_character(output_file, len = 1)
        output_file <- trimws(output_file)
      }

      # Always derive output_dir from output_file. If output_file has no path,
      # write to execute_dir.
      output_file_dir <- dirname(output_file)
      if (identical(output_file_dir, ".")) {
        output_dir <- execute_dir
        output_file <- basename(output_file)
      } else {
        if (grepl("^([A-Za-z]:|/|\\\\)", output_file)) {
          output_file_abs <- normalizePath(output_file, mustWork = FALSE)
        } else {
          output_file_abs <- normalizePath(file.path(execute_dir, output_file), mustWork = FALSE)
        }
        output_dir <- dirname(output_file_abs)
        output_file <- basename(output_file_abs)
      }

      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      dots <- list(...)
      if ("output_dir" %in% names(dots)) {
        warning("Argument output_dir is deprecated for Engine$report_quarto() and will be ignored.")
        dots$output_dir <- NULL
      }
      execute_params <- dots$execute_params
      dots$execute_params <- NULL
      if (is.null(execute_params)) {
        execute_params <- list()
      }
      checkmate::assert_list(execute_params)
      execute_params$projectPath <- normalizePath(private$.db, mustWork = TRUE)

      quarto_args <- dots$quarto_args
      dots$quarto_args <- NULL
      if (is.null(quarto_args)) {
        quarto_args <- character()
      } else {
        checkmate::assert_character(quarto_args)
      }
      if (!"--output-dir" %in% quarto_args) {
        quarto_args <- c(quarto_args, "--output-dir", output_dir)
      }

      tryCatch(
        {
          do.call(
            quarto::quarto_render,
            c(
              list(
                input = template,
                output_file = output_file,
                execute_dir = execute_dir,
                execute_params = execute_params,
                quarto_args = quarto_args
              ),
              dots
            )
          )
          message("\U2713 Quarto report generated successfully!")
        },
        error = function(e) {
          warning("Error generating Quarto report: ", e$message)
        }
      )

      invisible(self)
    },

    # MARK: run_app
    #' @description Runs the StreamFind Shiny app to explore, process and manage the engine data.
    #'
    #' @note The engine data is saved in an **rds** file and loaded in the app. If save file is
    #' defined in the engine it is used, otherwise the save file name is automatically set to the
    #' engine class name and the date in the format **rds**. Changes made in the app can be saved
    #' in the **rds** file and then loaded to continue working on the engine by scripting.
    #'
    run_app = function() {
      run_app(db = private$.db, engine_type = is(self))
    }
  )
)

# MARK: .create_Engine_db_schema
#' @noRd
.create_Engine_db_schema <- function(conn, dataType) {
  tryCatch(
    {
      DBI::dbExecute(conn, "INSTALL json")
      DBI::dbExecute(conn, "LOAD json")
    },
    error = function(e) {
      warning("Could not load JSON extension: ", e$message)
    }
  )

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Engine (
      dataType VARCHAR NOT NULL,
      metadata JSON,
      configuration JSON,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Workflow (
      dataType VARCHAR NOT NULL,
      methods JSON
    )
  ")

  invisible(TRUE)
}

# MARK: .validate_Engine_db_schema
#' @noRd
.validate_Engine_db_schema <- function(conn) {
  tryCatch(
    {
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
    },
    error = function(e) {
      stop("Schema migration check (Engine): ", e$message)
    }
  )

  tryCatch(
    {
      table_info <- DBI::dbGetQuery(conn, "PRAGMA table_info(Workflow)")
      required <- list(
        dataType = "VARCHAR NOT NULL",
        methods = "JSON"
      )
      for (col in names(required)) {
        if (!(col %in% table_info$name)) {
          message(sprintf("Adding missing %s column to Workflow table...", col))
          DBI::dbExecute(conn, sprintf("ALTER TABLE Workflow ADD COLUMN %s %s", col, required[[col]]))
        }
      }
    },
    error = function(e) {
      stop("Schema migration check (Workflow): ", e$message)
    }
  )

  # AuditTrail table schema is validated/created by the AuditTrail S3 class, not by Engine. Only Engine and Workflow tables are handled here.
  invisible(TRUE)
}
