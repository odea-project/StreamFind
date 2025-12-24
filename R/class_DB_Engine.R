# MARK: DB_Engine
# DB_Engine -----
#' @title File-based Database Engine for StreamFind
#' @description The [StreamFind::DB_Engine] R6 class provides file-based storage for StreamFind Engine data using DuckDB.
#' @template arg-core-project-path
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
DB_Engine <- R6::R6Class(
  classname = "DB_Engine",

  # MARK: private
  # private -----
  private = list(
    .project_path = NULL,
    .data_type = NULL
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
    #' @field Analyses A [StreamFind::DB_Analyses] child object.
    Analyses = function() {
      NULL
    },

    # MARK: AuditTrail
    #' @field AuditTrail Audit trail from database (read-only).
    AuditTrail = function() {
      self$get_audit_trail()
    },

    # MARK: Cache
    #' @field Cache A [StreamFind::CacheManager] object for managing cached data.
    Cache = function() {
      CacheManager(db = file.path(private$.project_path, "Cache.duckdb"))
    }
  ),

  # MARK: public methods
  # public methods -----
  public = list(

    # MARK: initialize
    #' @description Initialize DB_Engine.
    #' @param data_type Engine data type (internal; defaults to "Unknown").
    initialize = function(project_path = "data",
                          metadata = NULL,
                          workflow = NULL,
                          configuration = NULL,
                          data_type = "Unknown") {
      if (!requireNamespace("duckdb", quietly = TRUE)) {
        stop("duckdb package is required for DB_Engine")
      }
      checkmate::assert_character(project_path, len = 1)
      checkmate::assert_character(data_type, len = 1)
      sf_root <- project_path
      engine_db <- project_path
      if (tolower(tools::file_ext(sf_root)) == "duckdb") {
        sf_root <- dirname(sf_root)
        engine_db <- project_path
      } else {
        engine_db <- file.path(sf_root, "Engine.duckdb")
      }
      dir.create(sf_root, recursive = TRUE, showWarnings = FALSE)
      .create_sf_data_project_icon(sf_root)
      private$.project_path <- sf_root
      private$.data_type <- data_type
      conn <- DBI::dbConnect(duckdb::duckdb(), engine_db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      conn_cache <- DBI::dbConnect(duckdb::duckdb(), file.path(sf_root, "Cache.duckdb"))
      on.exit(DBI::dbDisconnect(conn_cache), add = TRUE)
      .create_DB_Engine_db_schema(conn, private$.data_type)
      .validate_DB_Engine_db_schema(conn)
      .create_Cache_db_schema(conn_cache)
      .validate_Cache_db_schema(conn_cache)
      if (!is.null(metadata)) {
        try(self$add_metadata(metadata), silent = TRUE)
      }
      if (!is.null(workflow)) {
        try(self$add_workflow(workflow), silent = TRUE)
      }
      message(private$.data_type, " engine initialized on ", private$.project_path)
    },

    # MARK: project_path
    #' @description Get the project path.
    #' @return Character string with the path to the StreamFind (.sf) project directory.
    project_path = function() {
      private$.project_path
    },

    # MARK: print
    #' @description Prints a summary to the console.
    print = function() {
      cat("\n")
      cat(paste0(private$.data_type, " Engine Overview\n"))
      show(self$Metadata)
      wf <- self$Workflow
      show(wf)
      anas <- self$Analyses
      show(anas)
      db_files <- list.files(private$.project_path, pattern = "\\.duckdb$", full.names = TRUE)
      cat("\n")
      cat("Database files (", length(db_files), ")\n")
      if (length(db_files) > 0) {
        for (f in db_files) {
          size_bytes <- file.info(f)$size
          size_mb <- round(size_bytes / (1024^2), 2)
          cat(paste0(" - ", basename(f), " (", size_mb, " MB)\n"))
        }
      }
    },

    # MARK: get_metadata
    #' @description Get metadata from database.
    #' @return A Metadata object or NULL.
    get_metadata = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
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
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
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
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      workflow_info <- DBI::dbGetQuery(conn, "SELECT methods FROM Workflow LIMIT 1")
      if (nrow(workflow_info) == 0) {
        wf_obj <- Workflow()
        attr(wf_obj, "type") <- private$.data_type
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
          attr(wf_obj, "type") <- private$.data_type
        }
      )
      wf_obj
    },

    # MARK: add_workflow
    #' @description Set workflow in database.
    add_workflow = function(workflow) {
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
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
        results_files <- list.files(private$.project_path, pattern = "^Results.*\\.duckdb$", full.names = TRUE)
        if (length(results_files) > 0) {
          message("\U1F5D1 Removing existing results database files...", appendLF = FALSE)
          file.remove(results_files)
          message("Done.")
        }
        self$Workflow <- Workflow()
        lapply(steps, function(x) self$run(x))
      } else {
        warning("There are no processing steps to run!")
      }
      invisible(self)
    },

    # MARK: get_audit_trail
    #' @description Get audit trail from database.
    get_audit_trail = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      audit_df <- DBI::dbGetQuery(conn, "SELECT * FROM AuditTrail ORDER BY timestamp DESC")
      audit_df
    },

    # MARK: add_audit_entry
    #' @description Add entry to audit trail.
    add_audit_entry = function(operation_type, object_type, details = NULL) {
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
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
      results_files <- list.files(private$.project_path, pattern = "Results.*\\.duckdb$", full.names = TRUE)
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
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
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
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .query_db(conn, sql, params)
    },

    # MARK: list_db_tables
    #' @description List all tables in the database.
    list_db_tables = function() {
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .list_db_tables(conn)
    },

    # MARK: get_db_table_info
    #' @description Get information about a specific table.
    get_db_table_info = function(tableName) {
      conn <- DBI::dbConnect(duckdb::duckdb(), file.path(private$.project_path, "Engine.duckdb"))
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      .get_db_table_info(conn, tableName)
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
      if (!requireNamespace("shiny", quietly = TRUE)) {
        warning("Shiny package not installed!")
        return(invisible(self))
      }
      if (!requireNamespace("htmltools", quietly = TRUE)) {
        warning("htmltools package not installed!")
        return(invisible(self))
      }
      if (!requireNamespace("shinydashboard", quietly = TRUE)) {
        warning("shinydashboard package not installed!")
        return(invisible(self))
      }
      if (!requireNamespace("shinycssloaders", quietly = TRUE)) {
        warning("shinycssloaders package not installed!")
        return(invisible(self))
      }
      if (!requireNamespace("shinyFiles", quietly = TRUE)) {
        warning("shinyFiles package not installed!")
        return(invisible(self))
      }
      if (!requireNamespace("sortable", quietly = TRUE)) {
        warning("sortable package not installed!")
        return(invisible(self))
      }
      run_app(project_path = private$.project_path, engine_type = is(self))
    }
  )
)

# MARK: .create_DB_Engine_db_schema
#' @noRd
.create_DB_Engine_db_schema <- function(conn, data_type) {
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
      data_type VARCHAR NOT NULL,
      metadata JSON,
      configuration JSON,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS Workflow (
      data_type VARCHAR NOT NULL,
      methods JSON
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

  # Engine table initialization
  existing <- DBI::dbGetQuery(conn, "SELECT rowid, data_type FROM Engine")
  if (nrow(existing) > 0) {
    sql <- "UPDATE Engine SET data_type = ? WHERE rowid = ?"
    DBI::dbExecute(conn, sql, list(data_type, existing$rowid[1]))
  } else {
    metadata <- Metadata()
    workflow <- Workflow()
    attr(workflow, "type") <- data_type
    json_metadata <- .convert_to_json(metadata)
    sql <- "INSERT INTO Engine (data_type, metadata) VALUES (?, ?)"
    DBI::dbExecute(conn, sql, list(data_type, json_metadata))
    json_workflow <- .convert_to_json(workflow)
    sql_wf <- "INSERT INTO Workflow (data_type, methods) VALUES (?, ?)"
    DBI::dbExecute(conn, sql_wf, list(data_type, json_workflow))
  }
  invisible(TRUE)
}

# MARK: .validate_DB_Engine_db_schema
#' @noRd
.validate_DB_Engine_db_schema <- function(conn) {
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
        data_type = "VARCHAR NOT NULL",
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

  tryCatch(
    {
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
    },
    error = function(e) {
      stop("Schema migration check (AuditTrail): ", e$message)
    }
  )

  invisible(TRUE)
}

# MARK: .create_sf_data_project_icon
#' @noRd
.create_sf_data_project_icon <- function(sf_root) {
  if (.Platform$OS.type != "windows") {
    return(invisible(FALSE))
  }

  icon_path <- file.path(sf_root, "streamfind.ico")
  if (!file.exists(icon_path)) {
    sf_icon <- c(
      system.file("app/www/streamfind.ico", package = "StreamFind", mustWork = FALSE),
      file.path(getwd(), "inst", "app", "www", "streamfind.ico")
    )
    sf_icon <- sf_icon[file.exists(sf_icon)]
    if (length(sf_icon) == 0) {
      return(invisible(FALSE))
    }
    sf_icon <- sf_icon[[1]]
    icon_path <- file.path(sf_root, basename(sf_icon))
    if (!file.exists(icon_path)) {
      file.copy(sf_icon, icon_path, overwrite = TRUE)
      if (!file.exists(icon_path)) {
        return(invisible(FALSE))
      }
    }
    try(system2("attrib", c("+h", shQuote(icon_path))), silent = TRUE)
  }

  ini_path <- file.path(sf_root, "desktop.ini")
  if (!file.exists(ini_path)) {
    ini <- c(
      "[.ShellClassInfo]",
      sprintf("IconResource=%s,0", basename(sf_icon)),
      "IconIndex=0"
    )
    writeLines(ini, ini_path, useBytes = TRUE)
    try(system2("attrib", c("+s", shQuote(sf_root))), silent = TRUE)
    try(system2("attrib", c("+h", shQuote(ini_path))), silent = TRUE)
  }

  invisible(TRUE)

  # Approach using magick to create icon from PNG
  # icon_filename <- "streamfind.ico"
  # icon_path <- file.path(sf_root, icon_filename)
  # create_icon_from_png <- function(path) {
  #   if (!requireNamespace("magick", quietly = TRUE)) return(invisible(FALSE))
  #   icon_candidates <- c(
  #     system.file("app/www/sf_icon.png", package = "StreamFind", mustWork = FALSE),
  #     file.path(getwd(), "inst", "app", "www", "sf_icon.png")
  #   )
  #   icon_candidates <- icon_candidates[file.exists(icon_candidates)]
  #   if (length(icon_candidates) == 0) return(invisible(FALSE))
  #   fav <- icon_candidates[[1]]
  #   img <- try(magick::image_read(fav), silent = TRUE)
  #   if (inherits(img, "try-error")) return(invisible(FALSE))
  #   sizes <- c(16, 20, 24, 32, 48, 64, 128, 256, 512)
  #   frames <- lapply(sizes, function(px) try(magick::image_scale(img, sprintf("%dx%d", px, px)), silent = TRUE))
  #   frames <- frames[!vapply(frames, inherits, logical(1), "try-error")]
  #   if (length(frames) == 0) return(invisible(FALSE))
  #   icon_stack <- do.call(c, frames)
  #   ok <- try(magick::image_write(icon_stack, path = path, format = "ico"), silent = TRUE)
  #   if (inherits(ok, "try-error")) return(invisible(FALSE))
  #   invisible(TRUE)
  # }
}
