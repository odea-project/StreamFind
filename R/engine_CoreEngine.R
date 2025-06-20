# MARK: CoreEngine
# CoreEngine -----
#' @title Core Engine (Internal Use Only)
#'
#' @description The [StreamFind::CoreEngine] R6 class is used to harmonize methods across different
#' data specific engines. Users should not use this class directly but data specific engines.
#'  
#' @template arg-core-metadata
#' @template arg-core-workflow
#' @template arg-core-analyses
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
    #' @field Metadata A [StreamFind::Metadata] or [StreamFind::EngineMetadata]. When setting can
    #' also be a named list with elements of length one.
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
    #' @field Workflow A [StreamFind::Workflow] S7 class object. When settings can also be a list
    #' of [StreamFind::ProcessingStep] objects or a full path string to an **rds** or **json** file
    #' containing a [StreamFind::Workflow] object.
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
    #' @field Analyses An [StreamFind::Analyses] S7 class object or a child of it.
    Analyses = function(value) {
      if (missing(value)) {
        return(private$.Analyses)
      }
      if (is(value, "StreamFind::Analyses")) {
        names_analyses <- names(private$.Analyses)
        old_results <- list()
        
        if (length(names_analyses) > 0) {
          if (length(private$.Analyses@results) > 0) {
            old_results <- private$.Analyses@results
          }
        }
        
        private$.Analyses <- value
        if (!is.null(private$.AuditTrail)) {
          if (length(names_analyses) == 0 || !identical(names(private$.Analyses), names(value))) {
            private$.AuditTrail <- add(private$.AuditTrail, value)
          }
          
          if (length(names(private$.Analyses)) > 0) {
            if (length(private$.Analyses@results) > 0) {
              for (r in names(private$.Analyses@results)) {
                if (!identical(private$.Analyses@results[[r]], old_results[[r]])) {
                  private$.AuditTrail <- add(private$.AuditTrail, private$.Analyses@results[[r]])
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
    
    #MARK: Results
    #' @field Results A named list of [StreamFind::Results] S7 class objects or a child of it.
    Results = function(value) {
      if (missing(value)) {
        return(private$.Analyses@results)
      }
      if (is(value, "StreamFind::Results")) {
        if (grepl(value[[i]]@data_type, is(self))) {
          private$.Analyses@results[[gsub("StreamFind::", "", is(value)[1])]] <- value
          if (!is.null(private$.AuditTrail)) {
            private$.AuditTrail <- add(private$.AuditTrail, value)
          }
        }
      } else if (is(value, "list")) {
        tryCatch(
          {
            for (i in seq_along(value)) {
              if (is(value[[i]], "StreamFind::Results")) {
                if (grepl(value[[i]]@data_type, is(self))) {
                  private$.Analyses@results[[gsub("StreamFind::", "", is(value)[1])]] <- value[[i]]
                  if (!is.null(private$.AuditTrail)) {
                    private$.AuditTrail <- add(private$.AuditTrail, value[[i]])
                  }
                }
              }
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
        warning("Invalid Results object! Not added.")
      }
      invisible(self)
    },
    
    # MARK: AuditTrail
    #' @field AuditTrail An [StreamFind::AuditTrail] S7 class object. Only getter method.
    AuditTrail = function(value) {
      private$.AuditTrail
    },
    
    # MARK: Config
    #' @field Config An [StreamFind::EngineConfig] S7 class object.
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
    #' @description Creates a [StreamFind::CoreEngine] R6 class object.
    #' @param ... Additional arguments for data specific engines.
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
      
      if (!is.na(self$Metadata@entries[["file"]])) {
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
      config_cache <- self$Config[["ConfigCache"]]
      StreamFind::clear_cache(config_cache, value)
      message("\U2713 Cache cleared!")
    },
    
    #' @description Clears all result objects in the `Analyses` field.
    clear_results = function() {
      private$.Analyses@results <- list()
      message("\U2713 Results cleared!")
    },
    
    # MARK: get_cache_info
    #' @description Gets a `data.table` with the cached data categories.
    get_cache_info = function() {
      config_cache <- self$Config[["ConfigCache"]]
      config_cache@info
    },
    
    # MARK: get_cache_size
    #' @description Gets the current size of the cache file.
    get_cache_size = function() {
      config_cache <- self$Config[["ConfigCache"]]
      config_cache@size
    },
    
    # MARK: has_analyses
    #' @description Checks if there are analyses files/objects in the `Analyses` field.
    has_analyses = function() {
      length(self$Analyses) > 0
    },
    
    # MARK: has_results
    #' @description Checks if there are [StreamFind::Results] in the `Analyses` field.
    #'
    #' @param value A string or a vector of strings with the name/s of the [StreamFind::Results]
    #' child/s for checking the presence.
    #'
    has_results = function(value = NULL) {
      if (is.null(value)) value <- names(self$Analyses@results)
      !all(vapply(private$.Analyses@results[value], is.null, FALSE))
    },
    
    # MARK: load
    #' @description Loads engine data from an **sqlite** or **rds** file.
    #'
    #' @param file A string with the full file path of the **sqlite** or **rds** file.
    #'
    #' @return Invisible.
    #'
    load = function(file = NA_character_) {
      if (is.na(file)) file <- self$Metadata@entries[["file"]]
      
      if (!file.exists(file)) {
        warning("File does not exist!")
        return(invisible(self))
      }
      
      if (!self$Metadata@entries[["file"]] %in% file) {
        tryCatch(
          {
            self$Metadata@entries[["file"]] <- file
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
        data <- .load_cache_sqlite_backend(file, is(self), hash)
        if (!is.null(data)) {
          private$.Metadata <- data$Metadata
          private$.Workflow <- data$Workflow
          private$.Analyses <- data$Analyses
          private$.AuditTrail <- data$AuditTrail
          self$Metadata@entries[["file"]] <- file
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
            self$Metadata@entries[["file"]] <- file
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
    #' given, the engine data is saved in the file of the [StreamFind::Metadata] field. If no file
    #' is specified in the `Metadata` the engine data is saved as **rds** format with the engine
    #' class and date in the `Metadata` as file name.
    #'
    #' @param file A string with the full file path of the **sqlite** or **rds** file.
    #'
    #' @return Invisible.
    #'
    save = function(file = NA_character_) {
      if (is.na(file)) file <- self$Metadata@entries[["file"]]
      
      if (is.na(file)) {
        file <- paste0(
          getwd(), "/", is(self), "_", format(self$Metadata@entries[["date"]], "%Y%m%d%H%M%S"), ".rds"
        )
      }
      
      if (!self$Metadata@entries[["file"]] %in% file) {
        tryCatch(
          {
            mtd <- self$Metadata
            mtd@entries[["file"]] <- file
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
        .save_cache_sqlite(is(self), data, hash, file)
        
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
    #' @description Runs a processing method defined by the [StreamFind::ProcessingStep] object.
    #' 
    #' @param step A [StreamFind::ProcessingStep] object.
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
      data_type <- step@data_type
      if (!checkmate::test_choice(paste0(data_type, "Engine"), is(self))) {
        warning("Data type ", data_type, " not matching with current engine! Not done.")
        return(invisible(self))
      }
      call <- step@call
      available_processing_steps <- .get_available_processing_methods(step@data_type)
      if (!call %in% available_processing_steps) {
        warning(paste0(call, " not available!"))
        return(invisible(self))
      }
      
      message("\U2699 Running ", step@method, " using ", step@algorithm)
      config_cache <- self$Config[["ConfigCache"]]
      processed <- FALSE
      loaded_cached <- FALSE
      
      if (config_cache@value) {
        
        engine_name <- self$Metadata@entries[["name"]]
        if (is.null(engine_name) || is.na(engine_name)) engine_name <- is(self)
        
        cache_category <- paste0(engine_name, "_results_", step@method, "_", step@algorithm)
        
        cache <- StreamFind::load_cache(
          config_cache,
          category = cache_category,
          as.list(self$Workflow),
          as.list(step),
          self$Analyses@info,
          names(self$Analyses@results)
        )
        
        if (!is.null(cache$data)) {
          
          message(
            "\U2139 Results from ",
            step@method,
            " using ",
            step@algorithm, " loaded from cache!"
          )
          
          tryCatch(
            {
              self$Analyses@results <- cache$data
              processed <- TRUE
              loaded_cached <- TRUE
            },
            error = function(e) {
              warning(
                "Error when adding results from ",
                step@method, ":\n", e, "\n",
                "Results deleted from cache!"
              )
              StreamFind::clear_cache(config_cache, cache_category)
            },
            warning = function(w) {
              warning(
                paste0(
                  "Warning when adding results from ",
                  step@method, ":\n", w, "\n",
                  "Results deleted from cache!"
                )
              )
              StreamFind::clear_cache(config_cache, cache_category)
            }
          )
        }
      }
      
      if (!processed) {
        processed <- StreamFind::run(step, self)
      }
      
      if (processed) {
        
        if (config_cache@value && !loaded_cached) {
          if (!is.null(cache$hash)) {
            StreamFind::save_cache(
              config_cache,
              category = cache_category,
              data = self$Analyses@results,
              hash = cache$hash
            )
            message(
              "\U1f5ab Results from ",
              step@method,
              " using ",
              step@algorithm,
              " cached!"
            )
          }
        }
        
        if (step@method %in% self$Workflow@methods) {
          if (step@number_permitted > 1) {
            self$Workflow[length(self$Workflow) + 1] <- step
          } else {
            step_idx <- which(self$Workflow@methods %in% step@method)
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
    #' @description Runs all [StreamFind::ProcessingStep] objects in the [StreamFind::Workflow].
    run_workflow = function() {
      if (length(self$Workflow) > 0) {
        steps <- self$Workflow@processing_steps
        self$Workflow <- StreamFind::Workflow()
        if (length(self$Analyses@results) > 0) self$Analyses@results <- list()
        lapply(steps, function(x) self$run(x))
      } else {
        warning("There are no processing steps to run!")
      }
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
      self$save()
      file <- self$Metadata@entries[["file"]]
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
