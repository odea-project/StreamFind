# MARK: CoreEngine
#' **CoreEngine** R6 class and methods
#'
#' @description The `CoreEngine` R6 class is a basic engine with generic methods for handling data. It is the base class
#' for the other engines and it does not have any specific methods for processing data.
#'
#' @template arg-file
#'
#' @export
#'
CoreEngine <- R6::R6Class("CoreEngine",

  # MARK: private fields
  # _ private fields -----
  private = list(
    .headers = NULL,
    .workflow = NULL,
    .analyses = NULL,
    .file = NULL,
    .audit_trail = NULL
  ),

  # MARK: active bindings
  # _ active bindings -----
  active = list(

    # MARK: headers
    # __headers -----
    #' @field headers `ProjectHeaders` S7 class object.
    headers = function(value) {
      if (missing(value)) {
        return(private$.headers)
      }
      if (is(value, "StreamFind::ProjectHeaders")) {
        private$.headers <- value
        if (!is.null(private$.audit_trail)) private$.audit_trail <- add(private$.audit_trail, value)
      } else {
        warning("Invalid headers object! Not added.")
      }
      invisible(self)
    },

    # MARK: workflow
    # __workflow -----
    #' @field workflow `Workflow` S7 class object.
    workflow = function(value) {
      if (missing(value)) {
        return(private$.workflow)
      }
      if (is(value, "StreamFind::Workflow")) {
        private$.workflow <- value
        if (!is.null(private$.audit_trail)) private$.audit_trail <- add(private$.audit_trail, value)
      } else {
        warning("Invalid workflow object! Not added.")
      }
      invisible(self)
    },

    # MARK: analyses
    # __analyses -----
    #' @field analyses `Analyses` S7 class object.
    analyses = function(value) {
      if (missing(value)) {
        return(private$.analyses)
      }
      if (is(value, "StreamFind::Analyses")) {
        names_analyses <- names(private$.analyses)
        
        if (length(names_analyses) > 0) {
          if (!is.null(private$.analyses$results)) {
            old_results <- private$.analyses$results
          }
        }
        
        private$.analyses <- value
        if (!is.null(private$.audit_trail)) {
          if (length(names_analyses) == 0 || !identical(names(private$.analyses), names(value))) {
            private$.audit_trail <- add(private$.audit_trail, value)
          }
          
          if (length(names(private$.analyses)) > 0) {
            if (!is.null(private$.analyses$results)) {
              for (r in names(private$.analyses$results)) {
                if (!identical(private$.analyses$results[[r]], old_results[[r]])) {
                  private$.audit_trail <- add(private$.audit_trail, private$.analyses$results[[r]])
                }
              }
            }
          }
        }
      } else {
        warning("Invalid analyses object! Not added.")
      }
      invisible(self)
    },

    # MARK: results
    # __results -----
    #' @field results List of results in the analyses.
    results = function(value) {
      if (missing(value)) {
        return(self$analyses$results)
      }
      if (is(value, "list")) {
        if (all(vapply(value, function(x) is(x, "StreamFind::Results"), FALSE))) {
          results_names <- vapply(value, function(x) x@name, "")
          for (i in seq_along(results_names)) {
            self$analyses$results[[results_names[i]]] <- value[[i]]
          }
        } else {
          warning("Invalid results object! Not added.")
        }
      } else if (is(value, "StreamFind::Results")) {
        self$analyses$results[[value@name]] <- value
      } else {
        warning("Invalid results object! Not added.")
      }
      invisible(self)
    },

    # MARK: file
    # __file -----
    #' @field file An `EngineSaveFile` S7 class object. When setting the value it
    #' can also be a character with an `sqlite` or `rds` file path to save the engine.
    file = function(value) {
      if (missing(value)) {
        return(private$.file)
      }
      if (is(value, "StreamFind::EngineSaveFile")) {
        private$.file <- value
        if (!is.null(private$.audit_trail)) private$.audit_trail <- add(private$.audit_trail, value)
      } else if (is.character(value)) {
        tryCatch(
          {
            value <- EngineSaveFile(file = value)
            private$.file <- value
            if (!is.null(private$.audit_trail)) private$.audit_trail <- add(private$.audit_trail, value)
          },
          error = function(e) {
            warning(e)
          }
        )
      } else {
        warning("Invalid file object! Not added.")
      }
      invisible(self)
    },
    
    # MARK: history
    # __history -----
    #' @field audit_trail AuditTrail S7 class object.
    audit_trail = function() {
      private$.audit_trail
    }
  ),

  # MARK: public fields/methods
  # _ public fields/methods -----
  public = list(

    # MARK: initialize
    ## __ initialize -----
    #' @description Creates a `CoreEngine` R6 class object.
    #'
    #' @param file Character of length one with the full path to the `sqlite` or `rds` save file of the engine.
    #' @param headers A `ProjectHeaders` S7 class object.
    #' @param analyses An `Analyses` S7 class object.
    #' @param workflow A `Workflow` S7 class object.
    #' @param ... Additional arguments.
    #'
    initialize = function(file = NULL, headers = NULL, workflow = NULL, analyses = NULL, ...) {
      private$.audit_trail <- AuditTrail()
      
      if (!is.null(file)) {
        tryCatch(
          {
            self$file <- file
            if (self$file$engine %in% is(self)) {
              self$load()
              return(invisible(self))
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
        self$file <- EngineSaveFile()
      }

      if (!is.null(headers)) {
        if (is(headers, "StreamFind::ProjectHeaders")) {
          self$headers <- headers
        } else {
          warning("Headers not added! Not valid.")
        }
      } else {
        self$headers <- ProjectHeaders()
      }

      if (!is.null(workflow)) {
        if (is(workflow, "StreamFind::Workflow")) {
          self$workflow <- workflow
        } else {
          warning("Workflow not added! Not valid.")
        }
      } else {
        self$workflow <- Workflow()
      }

      engine_type <- gsub("Engine", "", is(self))

      if (engine_type == "Core") {
        private$.analyses <- Analyses()
      } else {
        analyses_call <- paste0(engine_type, "Analyses")
        private$.analyses <- do.call(analyses_call, list())
      }

      if (!is.null(analyses)) {
        if (is(analyses, "StreamFind::Analyses")) {
          self$analyses <- analyses
        } else {
          analyses <- do.call(analyses_call, c(list(analyses), list(...)))
          if (is(analyses, "StreamFind::Analyses")) {
            self$analyses <- analyses
          } else {
            warning("Analyses not added! Not valid.")
          }
        }
      }

      message("\U2713 Engine created!")
      invisible(self)
    },

    # MARK: print
    ## __ print -----
    #' @description Prints a summary to the console.
    print = function() {
      cat("\n")
      cat(paste(is(self), collapse = "; "))
      cat("\n")
      cat("File: \n")
      cat(self$file@path)
      cat("\n")
      cat("\n")
      cat("Headers:")
      self$print_headers()
      cat("\n")
      cat("\n")
      cat("Workflow: \n")
      self$print_workflow()
      cat("\n")
      cat("\n")
      cat("Analyses: \n")
      self$print_analyses()
    },

    # MARK: print_headers
    ## __ print_headers -----
    #' @description Prints the headers.
    print_headers = function() {
      show(self$headers)
    },

    # MARK: print_analyses
    ## __ print_analyses -----
    #' @description Prints the analyses.
    print_analyses = function() {
      show(self$analyses)
    },

    # MARK: print_workflow
    ## __ print_workflow -----
    #' @description Prints the workflow.
    print_workflow = function() {
      show(self$workflow)
    },

    # MARK: save
    ## __ save -----
    #' @description Saves the engine data as an **sqlite** or **rds** file.
    #'
    #' @param file A string with the full file path of the **sqlite** or **rds** file. If \code{NA} (the default) and
    #' no save file is defined in the engine, the file name is automatically created with the engine class name and the
    #' date in the headers in the **rds** format.
    #'
    #' @return Invisible.
    #'
    save = function(file = NA_character_) {
      if (is.na(file)) file <- self$file$path
      if (is.na(file)) file <- paste0(getwd(), "/", is(self), "_", format(private$.headers$date, "%Y%m%d%H%M%S"), ".rds")
      if (!self$file$path %in% file) {
        tryCatch(
          {
            self$file <- EngineSaveFile(file = file)
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

      if (self$file$format %in% "sqlite") {
        data <- list(
          engine = is(self),
          headers = self$headers,
          workflow = self$workflow,
          analyses = self$analyses,
          audit = self$audit_trail
        )

        hash <- .make_hash(is(self))
        .save_cache(is(self), data, hash, file)
      } else if (self$file$format %in% "rds") {
        data <- list(
          engine = is(self),
          headers = self$headers,
          workflow = self$workflow,
          analyses = self$analyses,
          audit = self$audit_trail
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

    # MARK: load
    ## __ load -----
    #' @description Loads the engine data from an **sqlite** or **rds** file.
    #'
    #' @param file A string with the full file path of the **sqlite** or **rds** file containing the engine data saved.
    #'
    #' @return Invisible.
    #'
    load = function(file = NA_character_) {
      if (is.na(file)) file <- self$file$path

      if (!file.exists(file)) {
        warning("File does not exist!")
        return(invisible(self))
      }

      if (!self$file$path %in% file) {
        tryCatch(
          {
            self$file <- EngineSaveFile(file = file)
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

      if (is(self) == self$file$engine) {
        if (self$file$format %in% "sqlite") {
          hash <- .make_hash(is(self))
          data <- .load_cache_backend(self$file$path, is(self), hash)
          if (!is.null(data)) {
            private$.headers <- data$headers
            private$.workflow <- data$workflow
            private$.analyses <- data$analyses
            private$.audit_trail <- data$audit
            message("\U2713 Engine data loaded from ", file, "!")
          } else {
            warning("No data loaded from cache!")
          }
        } else if (self$file$format %in% "rds") {
          data <- readRDS(file)
          if (is(data, "list")) {
            if (data$engine %in% is(self)) {
              private$.headers <- data$headers
              private$.workflow <- data$workflow
              private$.analyses <- data$analyses
              private$.audit_trail <- data$audit
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
      } else {
        warning("No data loaded from cache!")
      }
      invisible(self)
    },

    # MARK: add_analyses
    ## __ add_analyses -----
    #' @description Adds analyses. Note that when adding new analyses, any existing results are removed.
    #'
    #' @param analyses An engine specific analysis object or a list of engine specific analysis objects.
    #'
    #' @details By adding analyses to the engine the results are removed as data reprocessing must be done. The
    #' analyses object can be a specific engine analysis object or a file path to a engine specific format.
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      self$analyses <- add(self$analyses, analyses)
      invisible(self)
    },

    # MARK: remove_analyses
    ## __ remove_analyses -----
    #' @description Removes analyses.
    #'
    #' @param analyses A string or a vector of strings with the name/s or numeric with indices of the analyses to remove.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      analyses <- .check_analyses_argument(self$analyses, analyses)
      self$analyses <- remove(self$analyses, analyses)
      invisible(self)
    },

    # MARK: has_settings
    ## __ has_settings -----
    #' @description Checks if there are processing settings, returning `TRUE` or `FALSE`.
    has_settings = function() {
      length(self$workflow) > 0
    },

    # MARK: has_analyses
    ## __ has_analyses -----
    #' @description Checks if analyses are present, returning `TRUE` or `FALSE`.
    has_analyses = function() {
      length(self$analyses) > 0
    },

    # MARK: has_results
    ## __ has_results -----
    #' @description Checks if results are present, returning `TRUE` or `FALSE`.
    #'
    #' @param names A string or a vector of strings with the name/s of the
    #' results data. The actual names depends of the applied algorithms. For
    #' instance, when algorithms via patRoon are used, the name of the module
    #' data is patRoon.
    #'
    has_results = function(names = NULL) {
      if (is.null(names)) names <- names(self$results)
      !all(vapply(private$.results[names], is.null, FALSE))
    },

    # MARK: run
    ## __ run -----
    #' @description Runs a processing method according to the provided settings.
    #'
    #' @param settings A `ProcessingSettings` object or a `character` string with the method name of a previously added
    #' `ProcessingSettings` in the engine.
    #'
    #' @note If there are `ProcessingSettings` objects with the same method names it is better to use `run_workflow()`.
    #'
    run = function(settings = NULL) {
      if (is.null(settings)) {
        warning("No processing settings provided!")
        return(invisible(self))
      }
      if (is.character(settings)) {
        settings <- self$workflow[settings %in% self$workflow@methods]
        if (length(settings) > 1) {
          warning("More than one processing settings with the same call name! Better to use run_workflow().")
          return(invisible(self))
        }
        if (is.null(settings)) {
          warning("Processing settings ", settings, " not found! Not done.")
          return(invisible(self))
        }
      }
      if (is(settings, "SreamFind::ProcessingSettings")) {
        warning("Processing settings not valid!")
        return(invisible(self))
      }
      engine <- settings$engine
      if (!checkmate::test_choice(paste0(engine, "Engine"), is(self))) {
        warning("Engine type ", engine, " not matching with current engine! Not done.")
        return(invisible(self))
      }
      call <- settings$call
      available_settings <- .get_available_settings(settings$engine)
      if (!call %in% available_settings) {
        warning(paste0(call, " not available!"))
        return(invisible(self))
      }

      message("\U2699 Running ", settings$method, " using ", settings$algorithm)
      processed <- run(settings, self)

      if (processed) {
        if (settings$method %in% self$workflow@methods) {
          if (settings$number_permitted > 1) {
            self$workflow[length(self$workflow) + 1] <- settings
          } else {
            setting_idx <- which(self$workflow@methods %in% settings$method)
            self$workflow[setting_idx] <- settings
          }
        } else {
          self$workflow[[length(self$workflow) + 1]] <- settings
        }
        if (!is.null(private$.audit_trail)) private$.audit_trail <- add(private$.audit_trail, settings)
      }
      invisible(self)
    },

    # MARK: run_workflow
    ## __ run_workflow -----
    #' @description Runs all processing methods in workflow.
    #'
    #' @return Invisible.
    #'
    run_workflow = function() {
      if (self$has_settings()) {
        settings_list <- self$workflow@settings
        self$workflow <- Workflow()
        if (self$has_results()) self$analyses$results <- list()
        lapply(settings_list, function(x) self$run(x))
      } else {
        warning("There are no processing settings to run!")
      }
      invisible(self)
    },

    # MARK: export_headers
    ## __ export_headers -----
    #' @description Exports the headers as \emph{json} (the default) or \emph{rds}.
    export_headers = function(file = "headers.json") {
      save(self$headers, file)
      invisible(self)
    },

    # MARK: export_workflow
    ## __ export_workflow -----
    #' @description Exports the workflow as \emph{json} (the default) or \emph{rds}.
    export_workflow = function(file = "workflow.json") {
      save(self$workflow, file)
      invisible(self)
    },

    # MARK: export_analyses
    ## __ export_analyses -----
    #' @description Exports the analyses as \emph{json} (the default) or \emph{rds}.
    export_analyses = function(file = "analyses.json") {
      save(self$analyses, file)
      invisible(self)
    },

    # MARK: import_headers
    ## __ import_headers -----
    #' @description Imports headers from an \emph{rds} or \emph{json} file.
    #'
    #' @param file A \emph{json} or \emph{rds} file.
    #'
    #' @return Invisible.
    #'
    import_headers = function(file = NA_character_) {
      if (file.exists(file)) {
        headers <- ProjectHeaders()
        headers <- read(headers, file)
        self$headers <- headers
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    # MARK: import_workflow
    ## __ import_workflow -----
    #' @description Imports workflow from an \emph{rds} or \emph{json} file.
    #'
    #' @param file A \emph{json} or \emph{rds} file.
    #'
    #' @return Invisible.
    #'
    import_workflow = function(file = NA_character_) {
      if (file.exists(file)) {
        workflow <- Workflow()
        workflow <- read(workflow, file)
        self$workflow <- workflow
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    # MARK: import_analyses
    ## __ import_analyses -----
    #' @description Imports analyses from an \emph{rds} or \emph{json} file.
    #'
    #' @param file A \emph{json} or \emph{rds} file.
    #'
    #' @return Invisible.
    #'
    import_analyses = function(file = NA_character_) {
      if (file.exists(file)) {
        analyses <- Analyses()
        analyses <- read(analyses, file)
        self$analyses <- analyses
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },

    # MARK: run_app
    ## __ run_app -----
    #' @description Runs a Shiny app to explore and manage the engine.
    #'
    #' @note The engine data is saved in an **rds** file and loaded in the app. If save file is defined in the engine it
    #' is used, otherwise the save file name is automatically set to the engine class name and the date in the format
    #' **rds**. Changes made in the app can be saved in the **rds** file and then loaded to continue working on the
    #' engine by scripting.
    #'
    run_app = function() {
      self$save()
      file <- self$file$path
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

      run_app(file = file, engine_type = engine_type)
    }
  )
)
