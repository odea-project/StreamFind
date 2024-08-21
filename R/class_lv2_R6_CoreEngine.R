#' **CoreEngine** R6 class and methods
#'
#' @description The `CoreEngine` R6 class is a basic data processor with generic methods for handling data.
#'
#' @template arg-save-format
#' @template arg-save-name
#' @template arg-save-path
#'
#' @export
#'
CoreEngine <- R6::R6Class("CoreEngine",

  # _ private fields -----
  private = list(
    
    ## ___ .headers -----
    .headers = NULL,
    
    ## ___ .workflow -----
    .workflow = NULL,
    
    ## ___ .analyses -----
    .analyses = NULL,
    
    ## ___ .results -----
    .results = NULL,
    
    ## ___ .file -----
    .file = NULL,
    
    ## ___ .history -----
    .history = NULL
  ),
  
  # _ active bindings -----
  active = list(
    
    #' @field headers `ProjectHeaders` S7 class object.
    headers = function(value) {
      if (missing(value)) return(private$.headers)
      if (is(value, "StreamFind::ProjectHeaders")) {
        private$.headers <- value
      } else {
        warning("Invalid headers object! Not added.")
      }
    },
    
    #' @field workflow `Workflow` S7 class object.
    workflow = function(value) {
      if (missing(value)) return(private$.workflow)
      if (is(value, "StreamFind::Workflow")) {
        private$.workflow <- value
      } else {
        warning("Invalid workflow object! Not added.")
      }
    },
    
    #' @field analyses `Analyses` S7 class object.
    analyses = function(value) {
      if (missing(value)) return(private$.analyses)
      if (is(value, "StreamFind::Analyses")) {
        private$.analyses <- value
      } else {
        warning("Invalid analyses object! Not added.")
      }
    },
    
    #' @field results List of results.
    #' 
    results = function(value) {
      if (missing(value)) return(private$.results)
      if (is(value, "list")) {
        if (all(vapply(value, is, "StreamFind::Results"))) {
          results_names <- vapply(value, function(x) x@name, "")
          for (i in seq_along(results_names)) {
            private$.results[[results_names[i]]] <- value[[i]]
          }
        } else {
          warning("Invalid results object! Not added.")
        }
      } else {
        warning("Invalid results object! Not added.")
      }
    },
    
    #' @field history Audit trail of changes.
    #' 
    history = function() {
      private$.history 
    },
    
    #' @field file `EngineSaveFile` S7 class object. 
    file = function(value) {
      if (missing(value)) return(private$.file)
      if (is(value, "StreamFind::EngineSaveFile")) {
        private$.file <- value
      } else {
        warning("Invalid file path! Not added.")
      }
    }
  ),
    
  # _ public fields/methods -----
  public = list(
    ## ___ create -----
    
    #' @description Creates a `CoreEngine` R6 class object.
    #' 
    #' @param file Character of length one with the full path to the `sqlite` save file of the engine.
    #' @param headers A `ProjectHeaders` S7 class object.
    #' @param analyses An `Analyses` S7 class object.
    #' @param workflow A `Workflow` S7 class object.
    #' @param results A list of `Results` S7 class objects.
    #'
    initialize = function(file = NULL, headers = NULL, workflow = NULL, analyses = NULL, results = NULL) {
      private$.headers <- ProjectHeaders()
      private$.workflow <- Workflow()
      engine_type <- gsub("Engine", "", is(self))
      if (engine_type == "Core") {
        analyses_call <- "Analyses"
        private$.analyses <- Analyses()
      } else {
        analyses_call <- paste0(engine_type, "Analyses")
        private$.analyses <- do.call(analyses_call, list())
      }
      private$.results <- list() 
      self$file <- EngineSaveFile(engine = is(self), file = NA_character_)
      
      if (!is.null(file)) self$file <- EngineSaveFile(engine = is(self), file = file)
      
      if (self$file@is_saved) {
        self$load()
        message("\U2713 Engine loaded!")
        return(invisible(self))
      }
      
      if (!is.null(headers)) {
        if (is(headers, "StreamFind::ProjectHeaders")) {
          self$headers <- headers
        } else {
          warning("Headers not added! Not valid.")
        }
      }
      
      if (!is.null(workflow)) {
        if (is(workflow, "StreamFind::Workflow")) {
          self$workflow <- workflow
        } else {
          warning("Workflow not added! Not valid.")
        }
      }
      
      if (!is.null(analyses)) {
        if (is(analyses, "StreamFind::Analyses")) {
          self$analyses <- analyses
        } else {
          analyses <- do.call(analyses_call, list(analyses))
          if (is(analyses, "StreamFind::Analyses")) {
            self$analyses <- analyses
          } else {
            warning("Analyses not added! Not valid.")
          }
        }
      }
      
      if (!is.null(results)) {
        if (is(results, "list")) {
          if (all(vapply(results, is, "StreamFind::Results"))) {
            private$.results <- results
          } else {
            warning("Results not added! Not valid.")
          }
        } else {
          warning("Results not added! Not valid.")
        }
      }
      
      message("\U2713 Engine created!")
      invisible(self)
    },
    
    ## ___ print -----
    
    #' @description Prints a summary to the console.
    #'
    print = function() {
      cat("\n")
      cat(paste(is(self), collapse = "; "))
      cat("\n")
      cat(
        "name          ", self$headers$name, "\n",
        "date          ", as.character(self$headers$date), "\n",
        "file          ", self$file@path, "\n",
        sep = ""
      )
      
      self$print_workflow()
      
      self$print_analyses()
      
      self$print_results()
    },
    
    #' @description Prints the headers.
    #'
    print_headers = function() {
      show(self$headers)
    },
    
    #' @description Prints the analyses.
    #'
    print_analyses = function() {
      show(self$analyses)
    },
    
    #' @description Prints the workflow.
    #'
    print_workflow = function() {
      show(self$workflow)
    },
    
    #' @description Prints the results names in the engine.
    #'
    print_results = function() {
      cat("\n")
      cat("Results")
      if (length(private$.results) > 0) {
        cat("\n")
        names_results <- names(private$.results)
        cat(
          paste0(" ", seq_len(length(names_results)), ": ", names_results),
          sep = "\n"
        )
      } else {
        cat(" empty \n")
      }
    },
    
    ## ___ save/load -----
    
    #' @description Saves the engine data as an **sqlite** file.
    #' 
    #' @param file A string with the full file path of the **sqlite** file. If \code{NA} (the default) and no save file 
    #' is defined in the engine, the file name is automatically created with the engine class name and the date in the 
    #' headers.
    #'
    save = function(file = NA_character_) {
      if (is.na(file)) file <- self$file@path
      if (is.na(file)) file <- paste0(getwd(), "/" ,is(self), "_", format(private$.headers$date, "%Y%m%d%H%M%S"), ".sqlite")
      if (!file.exists(file)) file.create(file)
      if (!self$file@path %in% file) self$file@path <- file
      data <- list(
        headers = self$headers,
        workflow = self$workflow,
        analyses = self$analyses,
        history = self$history,
        results = self$results,
        file = self$file
      )
      hash <- .make_hash(is(self))
      .save_cache(is(self), data, hash, file)
      if (file.exists(file)) {
        message("\U2713 Engine data saved in ", file, "!")
      } else {
        warning("Data not saved!")
      }
      invisible(self)
    },
    
    #' @description Loads the engine data from an **sqlite** file.
    #' 
    #' @param file A string with the full file path of the **sqlite** file containing the engine data saved.
    #' 
    load = function(file = NA_character_) {
      if (is.na(file)) file <- self$file@path
      if (is.na(file)) file <- file.choose()
      
      if (!file.exists(file)) {
        warning("File does not exist!")
        return(invisible(self))
      }
      
      if (!self$file@path %in% file) self$file@path <- file
      
      if (self$file@is_saved && is(self) == self$file@engine) {
        hash <- .make_hash(is(self))
        data <- .load_cache_backend(self$file@path, is(self), hash)
        
        if (!is.null(data)) {
          private$.headers <- data$headers
          private$.workflow <- data$workflow
          private$.analyses <- data$analyses
          private$.history <- data$history
          private$.results <- data$results
          message("\U2713 Engine data loaded from ", file, "!")
          
        } else {
          warning("No data loaded from cache!")
        }
      } else {
        warning("No data loaded from cache!")
      }
      
      invisible(self)
    },
    
    ## ___ get -----
    
    ## ___ add -----
    
    ## ___ remove -----
    
    ## ___ has -----
    
    #' @description Checks if there are processing settings, returning `TRUE` or `FALSE`.
    #'
    has_settings = function() {
      length(self$workflow) > 0
    },
    
    #' @description Checks if analyses are present, returning `TRUE` or `FALSE`.
    #'
    has_analyses = function() {
      length(self$analyses) > 0
    },
    
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
    
    ## ___ processing -----
    
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
      if (!checkmate::test_choice(paste0(engine,"Engine"), is(self))) {
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
      processed <- do.call("run", list(settings, self, private))
      if (processed) {
        if (settings$method %in% self$workflow@methods) {
          if (settings$number_permitted > 1) {
            self$workflow[length(self$workflow) + 1] <- settings
          } else {
            setting_idx <- which(self$workflow@methods %in% settings$method)
            self$workflow[setting_idx] <- settings
          }
        } else {
          self$workflow[length(self$workflow) + 1] <- settings
        }
      }
      invisible(self)
    },
    
    #' @description Runs all processing methods in workflow.
    #'
    #' @return Invisible.
    #'
    run_workflow = function() {
      if (self$has_settings()) {
        settings_list <- self$workflow@settings
        self$workflow <- Workflow()
        lapply(settings_list, function(x) self$run(x))
      } else {
        warning("There are no processing settings to run!")
      }
      invisible(self)
    },
    
    ## ___ export -----
    
    #' @description Exports the headers as \emph{json} (the default) or \emph{rds}.
    #'
    export_headers = function(format = "json", name = "headers", path = getwd()) {
      save(self$headers, format, name, path)
    },
    
    #' @description Exports the workflow as \emph{json} (the default) or \emph{rds}.
    #'
    export_workflow = function(format = "json", name = "settings", path = getwd()) {
      save(self$workflow, format, name, path)
    },
    
    #' @description Exports the analyses as \emph{json} (the default) or \emph{rds}.
    #'
    export_analyses = function(format = "json", name = "analyses", path = getwd()) {
      save(self$analyses, format, name, path)
      invisible(self)
    },
    
    #' @description Exports the engine data as as \emph{json} (the default) or \emph{rds}.
    #'
    export = function(format = "json", name = "EngineData", path = getwd()) {
      if (format %in% "json") {
        list_all = list(
          headers = as.list(self$headers),
          settings = as.list(self$workflow),
          analyses = as.list(self$analyses),
          history = as.list(self$history),
          results = as.list(self$results)
        )
        js_all <- .convert_to_json(list_all)
        write(js_all, file = paste0(path, "/", name, ".", "json"))
      }
      if (format %in% "rds") saveRDS(self, file = paste0(path, "/", name, ".rds"))
      invisible(self)
    },
    
    ## ___ import -----
    
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
    
    #' @description Imports a CoreEngine saved as \emph{json} or \emph{rds}.
    #'
    #' @param file A \emph{json} or \emph{rds} file.
    #'
    #' @return Invisible.
    #'
    import = function(file = NA_character_) {
      if (file.exists(file)) {
        if (tools::file_ext(file) %in% "json") {
          data <- fromJSON(file)
          if (is(data, "list")) {
            if ("headers" %in% names(data)) self$headers <- ProjectHeaders(data$headers)
            if ("workflow" %in% names(data)) self$workflow <- Workflow(data$workflow)
            if ("analyses" %in% names(data)) self$analyses <- Analyses(data$analyses)
            # if ("history" %in% names(data)) self$history <- data$history
            # if ("results" %in% names(data)) self$results <- data$results
          } else {
            warning("The object in file is not a list!")
          }
        }
        
        if (file_ext(file) %in% "rds") {
          data <- readRDS(file)
          if (is(data, "list")) {
            if ("headers" %in% names(data)) self$headers <- ProjectHeaders(data$headers)
            if ("workflow" %in% names(data)) self$workflow <- Workflow(data$workflow)
            if ("analyses" %in% names(data)) self$analyses <- Analyses(data$analyses)
            # if ("history" %in% names(data)) self$history <- data$history
            # if ("results" %in% names(data)) self$results <- data$results
          } else {
            warning("The object in file is not a list!")
          }
        }
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },
    
    ## ___ app -----
    
    #' @description Runs a Shiny app to explore and manage the engine.
    #' 
    #' @note The engine data is saved in an **sqlite** file and loaded in the app. If save file is defined in the engine
    #' it is used, otherwise the save file name is automatically set to the engine class name and the date. Changes made
    #' in the app can be saved in the **sqlite** file and then loaded for scripting.
    #' 
    run_app = function() {
      self$save()
      file <- self$save_file
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
