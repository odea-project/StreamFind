# MARK: CoreEngine
# CoreEngine -----
#' **CoreEngine** R6 class and methods
#'
#' @description The `CoreEngine` R6 class is used harmonizing method across different data specific
#' engines. Users should not use this class directly as core method are available from the specific
#' data engines.
#'  
#' @template arg-core-Metadata
#' @template arg-core-Workflow
#' @template arg-core-Analyses
#'
#' @export
#' 
CoreEngine <- R6::R6Class(
  classname = "CoreEngine",
  
  # MARK: private
  # private -----
  private = list(
    .Metadata = NULL,
    .Workflow = NULL,
    .Analyses = NULL,
    .AuditTrail = NULL,
    .Config = NULL
  ),

  # MARK: active bindings
  # active bindings -----
  active = list(

    # MARK: Metadata
    #' @field Metadata `Metadata` S7 class object. A value argument can be given with a `Metadata`
    #' object or a list with metadata entries.
    Metadata = function(value) {
      
      if (missing(value)) {
        return(private$.Metadata)
      }
      
      if (is(value, "StreamFind::EngineMetadata")) {
        
        if (value@engine %in% is(self)) {
          private$.Metadata <- value
        } else {
          warning("Engine type not matching with current engine! Not added.")
        }
        
        if (!is.null(private$.AuditTrail)) {
          private$.AuditTrail <- add(private$.AuditTrail, private$.Metadata)
        }
        
      } else if (is(value, "StreamFind::Metadata")) {
        private$.Metadata <- StreamFind::EngineMetadata(entries = value@entries, engine = is(self))
        if (!is.null(private$.AuditTrail)) {
          private$.AuditTrail <- add(private$.AuditTrail, private$.Metadata)
        }
        
      } else if (is(value, "list")) {
        tryCatch(
          {
            private$.Metadata <- StreamFind::EngineMetadata(entries = value, engine = is(self))
            if (!is.null(private$.AuditTrail)) {
              private$.AuditTrail <- add(private$.AuditTrail, private$.Metadata)
            }
          },
          error = function(e) {
            warning(e)
          },
          warning = function(w) {
            warning(w)
          }
        )
      } else {
        warning("Invalid EngineMetadata object! Not added.")
      }
      invisible(self)
    },

    # MARK: Workflow
    #' @field Workflow `Workflow` S7 class object. A value argument can be given with a `Workflow`
    #' object, a list with `ProcessingStep` objects or a file path to a **rds** or **json** file
    #' with a `Workflow` object.
    Workflow = function(value) {
      if (missing(value)) {
        return(private$.Workflow)
      }
      if (is(value, "StreamFind::Workflow")) {
        private$.Workflow <- value
        if (!is.null(private$.AuditTrail)) {
          private$.AuditTrail <- add(private$.AuditTrail, value)
        }
      } else if (is(value, "list")) {
        tryCatch(
          {
            private$.Workflow <- StreamFind::Workflow(value)
            if (!is.null(private$.AuditTrail)) {
              private$.AuditTrail <- add(private$.AuditTrail, private$.Workflow)
            }
          },
          error = function(e) {
            warning(e)
          },
          warning = function(w) {
            warning(w)
          }
        )
      } else if (tools::file_ext(value) %in% c("rds", "json")) {
        tryCatch(
          {
            private$.Workflow <- read(private$.Workflow, value)
            if (!is.null(private$.AuditTrail)) {
              private$.AuditTrail <- add(private$.AuditTrail, private$.Workflow)
            }
          },
          error = function(e) {
            warning(e)
          },
          warning = function(w) {
            warning(w)
          }
        )
      } else {
        warning("Invalid Workflow object! Not added.")
      }
      invisible(self)
    },

    # MARK: Analyses
    #' @field Analyses `Analyses` S7 class object. A value argument can be given with a child of
    #' `Analyses` class or a specific input for the data dedicated child `Analyses` class.
    Analyses = function(value) {
      if (missing(value)) {
        return(private$.Analyses)
      }
      if (is(value, "StreamFind::Analyses")) {
        names_analyses <- names(private$.Analyses)
        old_results <- list()
        
        if (length(names_analyses) > 0) {
          if (length(private$.Analyses$results) > 0) {
            old_results <- private$.Analyses$results
          }
        }
        
        private$.Analyses <- value
        if (!is.null(private$.AuditTrail)) {
          if (length(names_analyses) == 0 || !identical(names(private$.Analyses), names(value))) {
            private$.AuditTrail <- add(private$.AuditTrail, value)
          }
          
          if (length(names(private$.Analyses)) > 0) {
            if (length(private$.Analyses$results) > 0) {
              for (r in names(private$.Analyses$results)) {
                if (!identical(private$.Analyses$results[[r]], old_results[[r]])) {
                  private$.AuditTrail <- add(private$.AuditTrail, private$.Analyses$results[[r]])
                }
              }
            }
          }
        }
      } else {
        warning("Invalid Analyses object! Not added.")
      }
      invisible(self)
    },
    
    # MARK: AuditTrail
    #' @field AuditTrail `AuditTrail` S7 class object. No set method available!
    AuditTrail = function() {
      private$.AuditTrail
    },
    
    # MARK: Config
    #' @field Config `EngineConfig` S7 class object. A value argument can be given with an
    #' `EngineConfig` object.
    Config = function(value) {
      if (missing(value)) {
        return(private$.Config)
      }
      if (is(value, "StreamFind::EngineConfig")) {
        private$.Config <- value
        if (!is.null(private$.AuditTrail)) {
          private$.AuditTrail <- add(private$.AuditTrail, value)
        }
      } else {
        warning("Invalid Config object! Not added.")
      }
      invisible(self)
    }
  ),
  
  # MARK: public
  # public -----
  public = list(
    
    # MARK: initialize
    #' @description Creates a `CoreEngine` R6 class object.
    #' @param ... Additional data specific engine arguments.
    initialize = function(metadata = NULL, workflow = NULL, analyses = NULL, ...) {
      private$.Metadata <- StreamFind::EngineMetadata(engine = is(self))
      private$.AuditTrail <- StreamFind::AuditTrail()
      private$.Config <- StreamFind::EngineConfig()
      
      if (!is.null(metadata)) {
        if (is(metadata, "StreamFind::Metadata")) {
          self$Metadata <- metadata
          
        } else if (is(metadata, "list")) {
          tryCatch(
            {
              self$Metadata <- metadata
            },
            error = function(e) {
              warning(e)
            },
            warning = function(w) {
              warning(w)
            }
          )
        }
      }
      
      if (!is.na(self$Metadata@entries$file)) {
        tryCatch(
          {
            self$load()
            return(invisible(self))
          },
          error = function(e) {
            warning(e)
          },
          warning = function(w) {
            warning(w)
          }
        )
      }
      
      if (!is.null(workflow)) {
        if (is(workflow, "StreamFind::Workflow")) {
          self$Workflow <- workflow
        } else if (is(workflow, "list")) {
          tryCatch(
            {
              self$Workflow <- StreamFind::Workflow(workflow)
            },
            error = function(e) {
              warning(e)
            },
            warning = function(w) {
              warning(w)
            }
          )
        }
      } else {
        self$Workflow <- StreamFind::Workflow()
      }
      
      engine_type <- gsub("Engine", "", is(self))
      
      if (engine_type == "Core") {
        private$.Analyses <- StreamFind::Analyses()
      } else {
        Analyses_call <- paste0(engine_type, "Analyses")
        private$.Analyses <- do.call(Analyses_call, list())
      }
      
      if (!is.null(analyses)) {
        if (is(Analyses, "StreamFind::Analyses")) {
          self$Analyses <- analyses
        } else {
          tryCatch(
            {
              analyses <- do.call(Analyses_call, c(list(analyses), list(...)))
              if (is(analyses, "StreamFind::Analyses")) {
                self$Analyses <- analyses
              } else {
                warning("Analyses not added! Not valid.")
              }
            },
            error = function(e) {
              warning(e)
            },
            warning = function(w) {
              warning(w)
            }
          )
        }
      }
      
      message("\U2713 Engine created!")
      invisible(self)
    },
    
    # MARK: Methods
    # Methods -----
    
    # MARK: clear_cache
    #' @description Clears the cache.
    #' 
    #' @param value A character vector with the names of the cache categories to clear. An integer
    #' vector with the indices of the categories to clear can alternatively be given to remove
    #' categories. If `NULL` (the default), the entire cache is cleared. Use the method
    #' `get_cache_info` to get the cached categories.
    #' 
    clear_cache = function(value = NULL) {
      if (is.null(value)) value = "all"
      StreamFind::clear_cache(value, self$Config$ConfigCache$file)
      message("\U2713 Cache cleared!")
    },
    
    #' @description Clears all result objects in the `Analyses`.
    clear_results = function() {
      private$.Analyses$results <- list()
      message("\U2713 Results cleared!")
    },
    
    # MARK: get_cache_info
    #' @description Gets a data.table with the cached data categories.
    get_cache_info = function() {
      self$Config$ConfigCache$info
    },
    
    # MARK: get_cache_size
    #' @description Gets the current size of the cache file.
    get_cache_size = function() {
      self$Config$ConfigCache$size
    },
    
    # MARK: has_analyses
    #' @description Checks if there are analyses files/objects in the `Analyses`.
    has_analyses = function() {
      length(self$Analyses) > 0
    },
    
    # MARK: has_results
    #' @description Checks if there are `Results` in the engine.
    #'
    #' @param value A string or a vector of strings with the name/s of the `Results` child/s for
    #' checking the presence in the engine.
    #'
    has_results = function(value = NULL) {
      if (is.null(value)) value <- names(self$Analyses$results)
      !all(vapply(private$.Analyses$results[value], is.null, FALSE))
    },
    
    # MARK: load
    #' @description Loads engine data from an **sqlite** or **rds** file.
    #'
    #' @param file A string with the full file path of the **sqlite** or **rds** file.
    #'
    #' @return Invisible.
    #'
    load = function(file = NA_character_) {
      if (is.na(file)) file <- self$Metadata@entries$file
      
      if (!file.exists(file)) {
        warning("File does not exist!")
        return(invisible(self))
      }
      
      if (!self$Metadata@entries$file %in% file) {
        tryCatch(
          {
            self$Metadata@entries$file <- file
          },
          error = function(e) {
            warning("File not valid! Not loaded.")
            return(invisible(self))
          },
          warning = function(w) {
            warning(w)
            return(invisible(self))
          }
        )
      }
      
      if (tools::file_ext(file) %in% "sqlite") {
        hash <- .make_hash(is(self))
        data <- .load_cache_backend(file, is(self), hash)
        if (!is.null(data)) {
          private$.Metadata <- data$Metadata
          private$.Workflow <- data$Workflow
          private$.Analyses <- data$Analyses
          private$.AuditTrail <- data$AuditTrail
          self$Metadata@entries$file <- file
          message("\U2713 Engine data loaded from ", file, "!")
        } else {
          warning("No data loaded from cache!")
        }
        
      } else if (tools::file_ext(file) %in% "rds") {
        data <- readRDS(file)
        if (is(data, "list")) {
          if (data$engine %in% is(self)) {
            private$.Metadata <- data$Metadata
            private$.Workflow <- data$Workflow
            private$.Analyses <- data$Analyses
            private$.AuditTrail <- data$AuditTrail
            self$Metadata@entries$file <- file
            message("\U2713 Engine data loaded from ", file, "!")
          } else {
            warning("Engine type not matching with current engine! Not done.")
          }
        } else {
          warning("The object in file is not a list!")
        }
      } else {
        warning("File format not valid!")
      }
      
      invisible(self)
    },
    
    # MARK: print
    #' @description Prints a summary to the console.
    print = function() {
      cat("\n")
      cat(paste(is(self), collapse = "; "))
      cat("\n")
      cat("\n")
      cat("Metadata\n")
      StreamFind::show(self$Metadata)
      cat("\n")
      cat("Workflow\n")
      StreamFind::show(self$Workflow)
      cat("\n")
      cat("\n")
      cat("Analyses\n")
      StreamFind::show(self$Analyses)
    },
    
    # MARK: save
    #' @description Saves the engine data as an **sqlite** or **rds** file. If no file path is
    #' given, the engine data is saved in the file of the `Metadata`. If no file is specified
    #' in the `Metadata` the engine data is saved as **rds** format with the engine class
    #' name and date in the `Metadata` as file name.
    #'
    #' @param file A string with the full file path of the **sqlite** or **rds** file.
    #'
    #' @return Invisible.
    #'
    save = function(file = NA_character_) {
      if (is.na(file)) file <- self$Metadata@entries$file
      
      if (is.na(file)) {
        file <- paste0(
          getwd(), "/", is(self), "_", format(self$Metadata@entries$date, "%Y%m%d%H%M%S"), ".rds"
        )
      }
      
      if (!self$Metadata@entries$file %in% file) {
        tryCatch(
          {
            mtd <- self$Metadata
            mtd@entries$file <- file
            self$Metadata <- mtd
          },
          error = function(e) {
            warning(e)
            return(invisible(self))
          },
          warning = function(w) {
            warning(w)
            return(invisible(self))
          }
        )
      }
      
      if (tools::file_ext(file) %in% "sqlite") {
        data <- list(
          engine = is(self),
          Metadata = self$Metadata,
          Workflow = self$Workflow,
          Analyses = self$Analyses,
          AuditTrail = self$AuditTrail
        )
        
        hash <- .make_hash(is(self))
        .save_cache(is(self), data, hash, file)
        
      } else if (tools::file_ext(file) %in% "rds") {
        data <- list(
          engine = is(self),
          Metadata = self$Metadata,
          Workflow = self$Workflow,
          Analyses = self$Analyses,
          AuditTrail = self$AuditTrail
        )
        
        saveRDS(data, file)
        
      } else {
        warning("File format not valid!")
        return(invisible(self))
      }
      
      if (file.exists(file)) {
        message("\U2713 Engine data saved in ", file, "!")
      } else {
        warning("Data not saved!")
      }
      
      invisible(self)
    },
    
    # MARK: run
    #' @description Runs a processing method defined by the `ProcessingStep` object.
    #' 
    #' @param step A `ProcessingStep` object.
    #'
    run = function(step = NULL) {
      if (is.null(step)) {
        warning("No ProcessingStep provided!")
        return(invisible(self))
      } 
      if (!inherits(step, "StreamFind::ProcessingStep")) {
        warning("ProcessingStep not valid!")
        return(invisible(self))
      }
      engine <- step$engine
      if (!checkmate::test_choice(paste0(engine, "Engine"), is(self))) {
        warning("Engine type ", engine, " not matching with current engine! Not done.")
        return(invisible(self))
      }
      call <- step$call
      available_processing_steps <- .get_available_processing_methods(step$engine)
      if (!call %in% available_processing_steps) {
        warning(paste0(call, " not available!"))
        return(invisible(self))
      }
      
      message("\U2699 Running ", step$method, " using ", step$algorithm)
      
      processed <- FALSE
      loaded_cached <- FALSE
      
      if (self$Config$ConfigCache$value) {
        cache_category <- paste0("results_", step$method, "_", step$algorithm)
        
        cache <- .load_chache(
          cache_category,
          as.list(self$Workflow),
          as.list(step),
          self$Analyses$info,
          names(self$Analyses$results)
        )
        
        if (!is.null(cache$data)) {
          
          message(
            "\U2139 Results from ",
            step$method,
            " using ",
            step$algorithm, " loaded from cache!"
          )
          
          tryCatch(
            {
              self$Analyses$results <- cache$data
              processed <- TRUE
              loaded_cached <- TRUE
            },
            error = function(e) {
              warning(
                "Error when adding results from ",
                step$method, ":\n", e, "\n",
                "Results deleted from cache!"
              )
              clear_cache(cache_category)
            },
            warning = function(w) {
              warning(
                "Warning when adding results from ",
                step$method, ":\n", w, "\n",
                "Results deleted from cache!"
              )
              clear_cache(cache_category)
            }
          )
        }
      }
      
      if (!processed) {
        processed <- StreamFind::run(step, self)
      }
      
      if (processed) {
        
        if (self$Config$ConfigCache$value && !loaded_cached) {
          if (!is.null(cache$hash)) {
            .save_cache(cache_category, self$Analyses$results, cache$hash)
            message(
              "\U1f5ab Results from ",
              step$method,
              " using ",
              step$algorithm,
              " cached!"
            )
          }
        }
        
        if (step$method %in% self$Workflow@methods) {
          if (step$number_permitted > 1) {
            self$Workflow[length(self$Workflow) + 1] <- step
          } else {
            step_idx <- which(self$Workflow@methods %in% step$method)
            self$Workflow[step_idx] <- step
          }
        } else {
          self$Workflow[[length(self$Workflow) + 1]] <- step
        }
        
        if (!is.null(private$.AuditTrail)) {
          private$.AuditTrail <- add(private$.AuditTrail, step)
        }
      }
      invisible(self)
    },
    
    # MARK: run_workflow
    #' @description Runs all processing steps in Workflow.
    run_workflow = function() {
      if (length(self$Workflow) > 0) {
        steps <- self$Workflow$processing_steps
        self$Workflow <- StreamFind::Workflow()
        if (length(self$Analyses$results) > 0) self$Analyses$results <- list()
        lapply(steps, function(x) self$run(x))
      } else {
        warning("There are no processing steps to run!")
      }
      invisible(self)
    },
    
    # MARK: run_app
    #' @description Runs the StreamFind Shiny app to explore and manage the engine data.
    #'
    #' @note The engine data is saved in an **rds** file and loaded in the app. If save file is
    #' defined in the engine it is used, otherwise the save file name is automatically set to the
    #' engine class name and the date in the format **rds**. Changes made in the app can be saved
    #' in the **rds** file and then loaded to continue working on the engine by scripting.
    #'
    run_app = function() {
      self$save()
      file <- self$Metadata$file
      engine_type <- is(self)

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
      
      StreamFind::run_app(file = file, engine_type = engine_type)
    }
  )
)
