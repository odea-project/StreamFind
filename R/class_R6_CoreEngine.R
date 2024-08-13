#' **CoreEngine** R6 class and methods
#'
#' @description The CoreEngine R6 class is a basic data processor with generic methods for handling data.
#'
#' @template arg-headers
#' @template arg-settings-and-list
#' @template arg-results
#' @template arg-analyses
#' @template arg-save-format
#' @template arg-save-name
#' @template arg-save-path
#' @template arg-import-file
#'
#' @export
#'
CoreEngine <- R6::R6Class("CoreEngine",

  # _ private fields -----
  private = list(
    
    ## ___ .headers -----
    .headers = NULL,
    
    ## ___ .settings -----
    .settings = NULL,
    
    ## ___ .history -----
    .history = NULL,
    
    ## ___ .analyses -----
    .analyses = NULL,
    
    ## ___ .results -----
    .results = NULL,
    
    ## ___ .utils -----
    
    # Registers changes in the history private field.
    #
    .register = function(action = NA_character_, data = NA_character_, name = NA_character_, details = NA_character_) {
      date_time <- Sys.time()
      if (is.null(private$.history)) private$.history <- list()
      private$.history[[as.character.POSIXt(date_time)]] <- data.table(
        "time" = date_time,
        "action" = action,
        "data" = data,
        "name" = name,
        "details" = details
      )
      invisible(self)
    },
  
    # Checks the analyses argument as a character/integer vector to match
    # analyses names. Returns a valid character vector with analysis names 
    # or `NULL` for non-matching.
    #
    .check_analyses_argument = function(analyses = NULL) {
      if (is.null(analyses)) {
        self$get_analysis_names()
      } else {
        analyses <- self$get_analysis_names(analyses)
        if (!all(analyses %in% self$get_analysis_names())) {
          warning("Defined analyses not found!")
          NULL
        } else {
          analyses
        }
      }
    },
  
    # Gets an entry from the analyses private field.
    #
    .get_analyses_entry = function(analyses = NULL, value = NA_character_) {
      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(NULL)
      output <- lapply(private$.analyses, function(x, value) {
        temp <- x[[value]]
        names(temp) <- rep(x$name, length(temp))
        temp
      }, value = value)
      output <- unname(output)
      output <- unlist(output, recursive = FALSE, use.names = TRUE)
      output[names(output) %in% analyses]
    },
    
    # Converts each element in the list `analyses` to an Analysis S3 class object.
    #
    .validate_list_analyses = function(analyses = NULL, childClass = "Analysis") {
      
      if (is.list(analyses)) {
        
        if (all(c("name") %in% names(analyses))) {
          ana_name <- analyses$name
          analyses <- list(analyses)
          names(analyses) <- ana_name
        }
        
        analyses <- lapply(analyses, function(x) do.call(childClass, x))
        
        if (all(vapply(analyses, function(x) is(x, childClass), FALSE))) {
          ana_names <- vapply(analyses, function(x) x$name, "")
          names(analyses) <- ana_names
          
        } else {
          warning("Not done, check the conformity of the analyses list!")
          analyses <- NULL
        }
        
      } else {
        warning("Not done, check the conformity of the analyses list!")
        analyses <- NULL
      }
      
      analyses
    },
    
    # Checks if settings are already stored.
    #
    .settings_already_stored = function(settings) {
      any(vapply(private$.settings, function(x, settings) {
        identical(x, settings)
      }, settings = settings, FALSE))
    }
  ),
  
  # _ active bindings -----
  active = list(
    
    #' @field headers List of project headers.
    #' 
    headers = function() {
      private$.headers 
    },
    
    #' @field settings List of processing settings.
    #' 
    settings = function() {
      private$.settings
    },
    
    #' @field history Audit trail of changes.
    #' 
    history = function() {
      private$.history 
    },
    
    #' @field analyses List of analyses.
    #' 
    analyses = function() {
      private$.analyses
    },
    
    #' @field results List of results.
    #' 
    results = function() {
      private$.results
    },
    
    #' @field save_file Full path to the save file of the engine data. 
    #' 
    save_file = function() {
      self$headers$file
    }
  ),
    
  # _ public fields/methods -----
  public = list(
    ## ___ create -----
    
    #' @description Creates an R6 CoreEngine class object.
    #' 
    #' @param analyses An Analysis S3 class object or a list with Analysis S3  class objects as elements 
    #' (see `?Analysis` for more information).
    #'
    initialize = function(headers = NULL, settings = NULL, analyses = NULL, results = NULL) {
      private$.register("created", "CoreEngine", headers$name, paste(c(headers$author, headers$path), collapse = ", "))
      if (is.null(headers)) headers <- ProjectHeaders()
      if (!is.null(headers)) suppressMessages(self$add_headers(headers))
      if (!is.null(settings)) suppressMessages(self$add_settings(settings))
      if (!is.null(analyses)) suppressMessages(self$add_analyses(analyses))
      if (!is.null(results)) suppressMessages(self$add_results(results))
      message("\U2713 Engine created!")
    },
    
    ## ___ print -----
    
    #' @description Prints a summary to the console.
    #'
    print = function() {
      cat("\n")
      cat(paste(is(self), collapse = "; "))
      cat("\n")
      cat(
        "name          ", private$.headers$name, "\n",
        "author        ", private$.headers$author, "\n",
        "file          ", private$.headers$file, "\n",
        "date          ", as.character(private$.headers$date), "\n",
        sep = ""
      )
      
      self$print_workflow()
      
      self$print_analyses()
      
      self$print_results()
    },
    
    #' @description Prints the headers list.
    #'
    print_headers = function() {
      cat("\n")
      cat("Headers")
      if (length(private$.headers) > 0) {
        cat("\n")
        names <- names(private$.headers)
        vals <- unlist(private$.headers)
        cat(paste0(" ", names, ": ", vals), sep = "\n")
      } else {
        cat(" empty \n")
      }
    },
    
    #' @description Prints the analyses.
    #'
    print_analyses = function() {
      cat("\n")
      cat("Analyses")
      if (length(private$.analyses) > 0) {
        cat("\n")
        overview <- self$get_overview()
        overview[["file"]] <- NULL
        row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
        print(overview)
      } else {
        cat(" empty \n")
      }
    },
    
    #' @description Prints the order of processing methods for all added processing settings.
    #'
    print_workflow = function() {
      cat("\n")
      cat("Workflow")
      if (self$has_settings()) {
        cat("\n")
        names_settings <- vapply(private$.settings, function(x) x$call, "")
        algorithms <- vapply(private$.settings, function(x) x$algorithm, "")
        cat(
          paste0(" ", seq_len(length(names_settings)), ": ", names_settings, " (", algorithms, ")"),
          sep = "\n"
        )
      } else {
        cat(" empty \n")
      }
    },
    
    #' @description Prints the data results added to the engine.
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
    
    ## ___ get -----
    
    #' @description Gets the headers list.
    #'
    #' @param value A character vector with the name/s of the header elements. When `NULL` (the default), the entire 
    #' headers list is returned.
    #'
    get_headers = function(value = NULL) {
      if (is.null(value)) {
        private$.headers
      } else {
        private$.headers[value]
      }
    },
    
    #' @description Gets the object history (audit trail) as a data.table.
    #'
    get_history = function() {
      
      if (is.list(private$.history)) {
        data.table::rbindlist(private$.history, fill = TRUE)
        
      } else {
        private$.history
      }
    },
    
    #' @description Gets the list of analyses.
    #'
    get_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      private$.analyses[vapply(analyses, function(x) which(names(private$.analyses) %in% x), 0)]
    },
    
    #' @description Gets the number of analyses.
    #'
    get_number_analyses = function() {
      length(private$.analyses)
    },
    
    #' @description Gets an overview data.table with all the analysis names,replicates, associated blank replicates, 
    #' and full file paths.
    #'
    get_overview = function() {
      if (length(private$.analyses) > 0) {
        df <- data.table::data.table(
          "analysis" = vapply(private$.analyses, function(x) x$name, ""),
          "replicate" = vapply(private$.analyses, function(x) x$replicate, ""),
          "blank" = vapply(private$.analyses, function(x) x$blank, "")
        )
        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    },
    
    #' @description Gets the analysis names.
    #'
    get_analysis_names = function(analyses = NULL) {
      if (length(private$.analyses) > 0) {
        ana <- vapply(private$.analyses, function(x) x$name, "")
        names(ana) <- vapply(private$.analyses, function(x) x$name, "")
        if (!is.null(analyses)) {
          ana[analyses]
        } else {
          ana
        }
      } else {
        NULL
      }
    },
    
    #' @description Gets the analysis replicate names.
    #'
    get_replicate_names = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "replicate")
    },
    
    #' @description Gets the analysis blank replicate names.
    #'
    get_blank_names = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "blank")
    },
    
    #' @description Gets the full file paths of each analysis.
    #'
    get_files = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "file")
    },
    
    #' @description Gets the file format of each analysis.'
    #'
    get_formats = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "format")
    }, 
    
    #' @description Gets the type of each analysis.
    #'
    get_types = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "type")
    },
    
    #' @description Gets the processing settings list.
    #'
    #' @param call A string or a vector of strings with the name/s of the processing method/s.
    #'
    get_settings = function(call = NULL) {
      if (is.null(call)) {
        private$.settings
      } else {
        call_names <- vapply(private$.settings, function(x) x$call, NA_character_)
        if (any(call %in% call_names)) {
          private$.settings[call_names %in% call]
        } else {
          warning("Settings for ", call, " not found!")
          NULL
        }
      }
    },
    
    #' @description Gets the call names of all processing methods in added processing settings as a character vector.
    #'
    get_settings_names = function() {
      vapply(private$.settings, function(x) x$call, NA_character_)
    },
    
    #' @description A `data.table` with the overview of all processing methods from added processing settings.
    #'
    get_workflow_overview = function() {
      if (self$has_settings()) {
        data.table::data.table(
          "call" = vapply(private$.settings, function(x) x$call, ""),
          "algorithm" = vapply(private$.settings, function(x) x$algorithm, ""),
          "developer" = vapply(private$.settings, function(x) paste(x$developer, collapse = "; "), ""),
          "contact" = vapply(private$.settings, function(x) x$contact, ""),
          "link" = vapply(private$.settings, function(x) x$link, "")
        )
      } else {
        data.table::data.table()
      }
    },
    
    #' @description Gets names of present results data.
    #'
    get_results_names = function() {
      names(private$.results)
    },
    
    #' @description Gets the list of results data.
    #'
    #' @param results A character vector with the name of the module/s.
    #'
    get_results = function(results = NULL) {
      if (is.null(results)) results <- names(private$.results)
      private$.results[results]
    },
    
    ## ___ add -----
    
    #' @description Adds headers. If an argument or element "name" is given, it must be type character. If an argument 
    #' or element path is given, it must be type character and exist. If an argument or element date is  given, it must 
    #' be class POSIXct or POSIXt. If given date is character, conversion to class POSIXct or POSIXt is attempted.
    #' See `?ProjectHeaders` for more information.
    #'
    #' @template arg-headers-ellipsis
    #'
    #' @return Invisible.
    #'
    add_headers = function(...) {
      dots <- list(...)
      if (length(dots) == 1) if (is.list(dots[[1]])) dots <- dots[[1]]
      
      if (is.list(dots)) {
        if (!is.null(names(dots))) {
          if (all(vapply(dots, function(x) length(x) == 1, FALSE))) {
            old_headers <- private$.headers
            if (is.null(old_headers)) old_headers <- list()
            if (length(old_headers) > 0) {
              new_headers <- old_headers[!names(old_headers) %in% names(dots)]
              new_headers[names(dots)] <- dots
            } else {
              new_headers <- dots
            }
            new_headers <- as.ProjectHeaders(new_headers)
            
            if (!identical(new_headers, old_headers) & is(new_headers, "ProjectHeaders")) {
              private$.headers <- new_headers
              lapply(names(dots), function(x, new_headers) {
                private$.register("added", "headers", x, new_headers[x])
              }, new_headers = new_headers)
              message("\U2713 Added headers!")
            }
          } else {
            warning("Invalid headers content or structure! Not added.")
          }
        } else {
          warning("Invalid headers content or structure! Not added.")
        }
      } else {
        warning("Invalid headers content or structure! Not added.")
      }
      invisible(self)
    },
    
    #' @description Adds processing settings.
    #'
    #' @param replace Logical of length one. When `TRUE`, existing settings are 
    #' replaced by the new settings with the same call name, except settings for
    #' methods that can be applied more than once. The default is `FALSE`.
    #'
    #' @return Invisible.
    #'
    add_settings = function(settings = NULL, replace = FALSE) {
      
      if (is.list(settings)) {
        cols_check <- c("call", "algorithm", "parameters")
        if (all(cols_check %in% names(settings))) settings <- list(settings)
        settings <- lapply(settings, as.ProcessingSettings)
        names(settings) <- NULL
        all_ps <- vapply(settings, function(x) class(x)[1], "")
        if (all(all_ps %in% "ProcessingSettings")) {
          possible_methods <- self$processing_methods()
          if (nrow(possible_methods) == 0) {
            warning("No processing methods avaiable in the engine!")
            return(invisible(self))
          }
          only_one_possible <- possible_methods$name[possible_methods$max == 1]
          call_names <- vapply(settings, function(x) x$call, NA_character_)
          duplicated_names <- call_names[duplicated(call_names)]
          if (any(duplicated_names %in% only_one_possible)) {
            if (length(duplicated_names) == 1) {
              warning("\U2139 ", duplicated_names, " is duplicate and only one is possible! Not added.")
            } else {
              warning(paste0("\U2139 Duplicate settings for the following methods and only one is possible! Not added.\n",
                paste(only_one_possible[only_one_possible %in% duplicated_names], collapse = "\n"))
              )
            }
            return(invisible(self))
          }
          
          if (is.null(private$.settings)) private$.settings <- list()
          
          lapply(settings, function(x, only_one_possible, replace) {
            stored_calls <- vapply(private$.settings, function(z) z$call, NA_character_)
            
            if (replace) {
              if (x$call %in% stored_calls) {
                # case when repeating can happen with replace = TRUE as long as duplicated in call_names
                if (!(x$call %in% only_one_possible) & any(duplicated(call_names))) {
                  private$.settings <- c(private$.settings, list(x))
                  private$.register("added", "settings", x$call, x$algorithm)
                  message(paste0("\U2713 ", x$call, " processing settings added!"))
                  
                } else {
                  private$.settings[which(x$call %in% stored_calls)] <- list(x)
                  private$.register("replaced", "settings", x$call, x$algorithm)
                  message(paste0("\U2713 ", x$call, " processing settings replaced!"))
                }
                
              } else {
                private$.settings <- c(private$.settings, list(x))
                private$.register("added", "settings", x$call, x$algorithm)
                message(paste0("\U2713 ", x$call, " processing settings added!"))
              }
            # not replacing, appending
            } else {
              if (x$call %in% only_one_possible && x$call %in% stored_calls) {
                warning(x$call, " not added as only one is possible and is already in engine settings!")
                
              } else {
                private$.settings <- c(private$.settings, list(x))
                private$.register("added", "settings", x$call, x$algorithm)
                message(paste0("\U2713 ", x$call, " processing settings added!"))
              }
            }
          }, only_one_possible = only_one_possible, replace = replace)
          
        } else {
          not_conform <- which(!all_ps %in% "ProcessingSettings")
          warning("Settings (number/s: ", paste(not_conform, collapse = "; "), ") content or structure not conform! Not added.")
        }
      } else {
        warning("Settings must be a list! Not added.")
      }
      
      invisible(self)
    },
    
    #' @description Adds analyses.
    #'
    #' @param analyses An Analysis S3 class object or a list with Analysis S3 objects as elements.
    #'
    #' @return Invisible.
    #'
    add_analyses = function(analyses = NULL) {
      
      if (is.list(analyses)) {
        if ("name" %in% names(analyses)) {
          ana_name <- analyses$name
          analyses <- list(analyses)
          names(analyses) <- ana_name
        } else if (all(vapply(analyses, function(x) "name" %in% names(x), FALSE))) {
          ana_names <- vapply(analyses, function(x) x$name, "")
          names(analyses) <- ana_names
        } else {
          warning("Not done, check the conformity of the analyses list!")
          analyses <- NULL
        }
      } else {
        warning("Not done, check the conformity of the analyses list!")
        analyses <- NULL
      }
      
      if (!is.null(analyses)) {
        if (all(vapply(analyses, validate, FALSE))) {
          old_analyses <- self$get_analyses()
          old_names <- NULL
          if (length(old_analyses) > 0) old_names <- vapply(old_analyses, function(x) x$name, "")
          new_names <- c(old_names, vapply(analyses, function(x) x$name, ""))
          
          if (!any(duplicated(new_names))) {
            new_analyses <- c(old_analyses, analyses)
            names(new_analyses) <- new_names
            new_analyses <- new_analyses[order(names(new_analyses))]
            old_size <- length(private$.analyses)
            private$.analyses <- new_analyses
            lapply(analyses, function(x) private$.register("added", "analysis", x$name, x$file))
            message(paste0("\U2713 ", length(new_analyses) - old_size, " analyses added!"))
          } else {
            warning("Duplicated analysis names not allowed! Not done.")
          }
        } else {
          warning("No conform analyses to add!")
        }
      }
      invisible(self)
    },
    
    #' @description Adds or redefines the analysis replicate names.
    #'
    #' @param value A character vector with the analysis replicate names. Must be of the same length as the number of analyses.
    #' 
    #' @note Removes all results if present in the engine as may be affected but modified correspondence.
    #'
    add_replicate_names = function(value = NULL) {
      if (is.character(value) && length(value) == self$get_number_analyses()) {
        self$remove_results()
        private$.analyses <- Map(
          function(x, y) {
            x$replicate <- y
            x
          },
          private$.analyses, value
        )
        private$.register("added", "analyses", "replicate names")
        message("\U2713 Replicate names added!")
      } else {
        warning("Not done, check the value!")
      }
      invisible(self)
    },
    
    #' @description Adds or redefines the analysis blank replicate names.
    #'
    #' @param value A character vector with the analysis blank replicate names. Must be of the same length as the number of analyses.
    #' 
    #' @note Removes all results if present in the engine as may be affected but modified correspondence.
    #'
    add_blank_names = function(value = NULL) {
      if (is.character(value) & length(value) == self$get_number_analyses()) {
        if (all(value %in% c(self$get_replicate_names(), NA_character_))) {
          self$remove_results()
          private$.analyses <- Map(
            function(x, y) {
              x$blank <- y
              x
            },
            private$.analyses, value
          )
          private$.register("added", "analyses", "blank names")
          message("\U2713 Blank names added!")
        } else {
          warning("Not done, blank names not among replicate names!")
        }
      } else {
        warning("Not done, check the value!")
      }
    },
    
    #' @description Adds metadata to analyses.
    #'
    #' @param value A data.frame or data.table with metadata for the analyses.
    #' The data.frame or data.table must have an analysis column and the same 
    #' number of rows as the number of analyses in the engine. Metadata is added
    #' using any extra columns of the data.frame or data.table that do not repeat 
    #' with the names in the overview data.table of the project (e.g., `analysis`, 
    #' `replicate`, `blank`, `file`, `polarity`, `traces`, `features`, `groups`).
    #'
    #' @return Invisible.
    #'
    add_metadata = function(value = NULL) {
      
      if (is.data.frame(value)) {
        value <- as.data.table(value)
        
        if (nrow(value) == self$get_number_analyses()) {
          
          if ("analysis" %in% colnames(value)) {
            
            if (ncol(value) >= 2) {
              value <- value[order(value$analysis), ]
              
              if (identical(value$analysis, self$get_analysis_names())) {
                
                setcolorder(value, "analysis")
                col_names <- colnames(value)
                col_names <- col_names[2:length(col_names)]
                value <- split(value, value$analysis)
                
                private$.analyses <- Map(
                  function(x, y) {
                    
                    cols <- colnames(y)
                    cols <- cols[2:length(cols)]
                    
                    for (i in cols) x$metadata[[i]] <- y[[i]][1]
                    
                    x
                  },
                  private$.analyses, value
                )
              }
              
              private$.register("added", "analyses", "metadata", paste(col_names, collapse = "; "))
              message("\U2713 Metadata ", paste(col_names, collapse = ", "), " added!")
              
            } else {
              warning("No metadata found in the data.frame/data.table!")
            }
          } else {
            warning("The column analysis must be in the data.frame/data.table!")
          }
        } else {
          warning("The data.frame/data.table must have the same number of rows as the number of analyses in the MassSpecData!")
        }
      } else {
        warning("The argument value must be a data.frame/data.table!")
      }
      invisible(self)
    },
    
    #' @description Adds data from results to the engine.
    #'
    #' @param value A named list with data from results.
    #'
    #' @return Invisible.
    #'
    add_results = function(value = NULL) {
      value_names <- names(value)
      if (!is.null(value_names)) {
        if (is.null(private$.results)) private$.results <- list()
        lapply(value_names, function(x, value) {
          private$.results[x] <- value[x]
          # TODO add check for replicate or analyses names
          private$.register("added", "results", x)
          message(paste0("\U2713 ", x, " data added to results!"))
        }, value = value)
      } else {
        warning("Not done, the value must be a named list!")
      }
      invisible(self)
    },
    
    ## ___ remove -----
    
    #' @description Removes headers entries. Note that the name, path and date headers cannot be removed only changed.
    #'
    #' @param value A character vector with the name/s of the elements in headers to be removed.
    #'
    #' @return Invisible.
    #'
    remove_headers = function(value = NULL) {
      if (!is.null(value)) {
        value <- value[!(value %in% c("name", "author", "file", "date"))]
        if (length(value) == 0) {
          warning("Name, author, file and date headers cannot be removed!")
          return(invisible(self))
        }
        message("\U2713 Removed headers: ", paste(value[value %in% names(private$.headers)], collapse = ", "))
        lapply(value, function(x) {
          if (x %in% names(private$.headers)) {
            private$.register("removed", "headers", x, private$.headers[[x]])
            private$.headers[x] <- NULL
          }
        })
      } else {
        to_remove <- names(private$.headers) %in% c("name", "author", "file", "date")
        to_remove <- names(private$.headers)[!to_remove]
        private$.headers[to_remove] <- NULL
        if (length(to_remove) > 1) {
          details <- paste(to_remove, collapse = ", ")
          private$.register("removed", "headers", "all", details)
          message("\U2713 Removed headers: \n", paste(to_remove, collapse = "\n"))
        } else {
          private$.register("removed", "headers", "all")
          message("\U2713 Removed all headers except name, author, path and date!")
        }
      }
      invisible(self)
    },
    
    #' @description Removes settings.
    #'
    #' @param call A string or a vector of strings with the name/s of the processing method/s to be removed. 
    #' Alternatively, an integer vector with the index/es of the settings to be removed. When `call` is
    #' \code{NULL} all settings are removed.
    #'
    #' @return Invisible.
    #'
    remove_settings = function(call = NULL) {
      if (is.null(call)) {
        lapply(private$.settings, function(x) private$.register("removed", "settings", x$call, x$algorithm))
        private$.settings <- NULL
        cat("Removed all processing settings! \n")
      } else {
        all_calls <- vapply(private$.settings, function(x) x$call, NA_character_)
        if (is.numeric(call)) {
          to_remove <- call
        } else {
          to_remove <- which(all_calls %in% call)
        }
        if (length(call) > 0) {
          lapply(private$.settings[to_remove], function(x) private$.register("removed", "settings", x$call, x$algorithm))
          private$.settings[to_remove] <- NULL
          message("\U2713 Removed settings for:\n", paste(all_calls[to_remove], collapse = "\n"))
        } else {
          message("\U2717 There are no settings to remove!")
        }
      }
      invisible(self)
    },
    
    #' @description Removes analyses.
    #'
    #' @return Invisible.
    #'
    remove_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (!is.null(analyses)) {
        allNames <- self$get_analysis_names()
        keepAnalyses <- unname(allNames[!(allNames %in% analyses)])
        removeAnalyses <- unname(allNames[allNames %in% analyses])
        analysesLeft <- self$get_analyses(keepAnalyses)
        if (length(removeAnalyses) > 0) {
          private$.analyses <- analysesLeft
          lapply(removeAnalyses, function(x) private$.register("removed", "analysis", x))
          message("\U2713 Removed analyses:\n", paste(removeAnalyses, collapse = "\n"))
        } else {
          message("\U2717 There are no analyses to remove!")
        }
      } else {
        lapply(private$.analyses, function(x) private$.register("removed", "analysis", x))
        private$.analyses <- NULL
        message("\U2713 Removed all analyses!")
      }
      invisible(self)
    },
    
    #' @description Removes results data.
    #'
    #' @return Invisible.
    #'
    remove_results = function(results) {
      if (missing(results)) {
        private$.results <- NULL
        private$.register("removed", "results", "all")
      } else {
        for (i in results) {
          if (self$has_results(i)) {
            private$.register("removed", "results", "i")
            private$.results[[i]] <- NULL
          }
        }
      }
      invisible(self)
    },
    
    ## ___ subset -----
    
    #' @description Subsets on analyses returning a new cloned object with only the analyses to keep.
    #'
    subset_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      if (!is.null(analyses)) {
        allNames <- self$get_analysis_names()
        keepAnalyses <- unname(allNames[allNames %in% analyses])
        if (length(keepAnalyses) > 0) {
          newAnalyses <- self$get_analyses(keepAnalyses)
          new_core <- suppressMessages(CoreEngine$new(
            headers = self$get_headers(),
            settings = self$get_settings(),
            analyses = newAnalyses,
          ))
          message("\U2713 Subset with ", new_core$get_number_analyses(), " analyses created!")
          return(new_core)
        }
      }
      message("\U2717 There are no analyses selected to subset!")
      suppressMessages(CoreEngine$new(
        headers = self$get_headers(),
        settings = self$get_settings())
      )
    },
    
    ## ___ has -----
    
    #' @description Checks if there are processing settings, returning `TRUE` or `FALSE`.
    #'
    #' @param call A string or a vector of strings with the name/s of the processing method/s.
    #'
    has_settings = function(call = NULL) {
      if (is.null(call)) {
        length(private$.settings) > 0
      } else if (length(private$.settings) > 0) {
        call %in% vapply(private$.settings, function(x) x$call, NA_character_)
      } else {
        FALSE
      }
    },
    
    #' @description Checks if analyses are present, returning `TRUE` or `FALSE`.
    #'
    has_analyses = function() {
      length(private$.analyses) > 0
    },
    
    #' @description Checks if results are present, returning `TRUE` or `FALSE`.
    #' 
    #' @param names A string or a vector of strings with the name/s of the
    #' results data. The actual names depends of the applied algorithms. For 
    #' instance, when algorithms via patRoon are used, the name of the module 
    #' data is patRoon.
    #'
    has_results = function(names = NULL) {
      if (is.null(names)) names <- names(private$.results)
      !all(vapply(private$.results[names], is.null, FALSE))
    },
    
    ## ___ processing -----
    
    #' @description Runs a processing method with the provided settings.
    #' 
    #' @param settings A ProcessingSettings object or a character string with the call name of a previously added 
    #' ProcessingSettings in the engine.
    #' 
    #' @note If there are ProcessingSettings objects with the same call names it is better to use `run_workflow()`.
    #' 
    process = function(settings = NULL) {
      if (is.null(settings)) {
        warning("No processing settings provided!")
        return(invisible(self))
      }
      if (is.character(settings)) {
        settings <- self$get_settings(settings)
        if (length(settings) > 1) {
          warning("More than one processing settings with the same call name! Not done.")
          return(invisible(self))
        }
        if (is.null(settings)) {
          warning("Processing settings ", settings, " not found! Not done.")
          return(invisible(self))
        }
      }
      if (!validate(settings)) {
        warning("Processing settings not valid!")
        return(invisible(self))
      }
      engine <- settings$engine
      if (!checkmate::test_choice(paste0(engine,"Engine"), is(self))) {
        warning("Engine type ", engine, " not matching with current engine! Not done.")
        return(invisible(self))
      }
      call <- settings$call
      if (!checkmate::test_choice(call, self$processing_methods()$name)) {
        warning("Processing method ", call, " not available in the engine! Not done.")
        return(invisible(self))
      }
      message("\U2699 Running ", call, " using ", settings$algorithm)
      processed <- do.call(".process", list(settings, self, private))
      if (processed) {
        settings_can_repeat <- self$processing_methods()$name[self$processing_methods()$max > 1]
        settings_can_repeat <- length(settings_can_repeat) > 0
        if (!private$.settings_already_stored(settings) || settings_can_repeat) self$add_settings(settings)
        private$.register("processed", settings$call, settings$algorithm, settings$software)
      }
      invisible(self)
    },
    
    #' @description Runs all processing results represented by added processing settings.
    #'
    #' @return Invisible.
    #'
    run_workflow = function() {
      if (self$has_settings()) {
        settings_list <- self$get_settings()
        private$.settings <- NULL
        lapply(settings_list, function(x) self$process(x))
      } else {
        warning("There are no processing settings to run!")
      }
      invisible(self)
    },
    
    ## ___ save/load -----
    
    #' @description Saves the engine data as **sqlite** file.
    #' 
    #' @param file A string with the full file path of the **sqlite** file. If \code{NA} (the default) and no file 
    #' header is defined in the engine, the file name is automatically created with the engine class name and the date.
    #'
    save = function(file = NA_character_) {
      if (is.na(file)) file <- self$save_file
      if (is.na(file)) file <- paste0(getwd(), "/" ,is(self), "_", format(private$.headers$date, "%Y%m%d%H%M%S"), ".sqlite")
      if (!file.exists(file)) file.create(file)
      self$add_headers(file = file)
      data <- list(
        headers = private$.headers,
        settings = private$.settings,
        analyses = private$.analyses,
        history = private$.history,
        results = private$.results
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
    #' @param file A string with the full file path of the **sqlite** file. If \code{NA} (the default) and no file 
    #' header is defined in the engine, the file name is automatically created with the engine class name and the date.
    #' 
    load = function(file = NA_character_) {
      if (is.na(file)) file <- self$save_file
      if (is.na(file)) file <- file.choose()
      if (!file.exists(file)) {
        warning("File does not exist!")
        return(invisible(self))
      }
      hash <- .make_hash(is(self))
      data <- .load_cache_backend(file, is(self), hash)
      if (!is.null(data)) {
        private$.headers <- data$headers
        private$.settings <- data$settings
        private$.analyses <- data$analyses
        private$.history <- data$history
        private$.results <- data$results
        message("\U2713 Engine data loaded from ", file, "!")
      } else {
        warning("No data loaded from cache!")
      }
      invisible(self)
    },
    
    ## ___ export -----
    
    #' @description Exports the headers as \emph{json} (the default) or \emph{rds}.
    #'
    export_headers = function(format = "json", name = "headers", path = getwd()) {
      if (format %in% "json") {
        js_headers <- .convert_to_json(private$.headers)
        write(js_headers, file = paste0(path, "/", name, ".json"))
      }
      if (format %in% "rds") saveRDS(private$.headers, file = paste0(path, "/", name, ".rds"))
      invisible(self)
    },
    
    #' @description Exports the settings as \emph{json} (the default) or \emph{rds}.
    #'
    #' @param call A string or a vector of strings with the name/s of the processing method/s to be exported. 
    #' When `call` is \code{NULL} all settings are saved.
    #'
    export_settings = function(call = NULL, format = "json", name = "settings", path = getwd()) {
      if (format %in% "json") {
        js_settings <- self$get_settings(call)
        names(js_settings) <- vapply(js_settings, function(x) x$call, NA_character_)
        js_settings <- .convert_to_json(js_settings)
        write(js_settings, file = paste0(path, "/", name, ".json"))
      }
      if (format %in% "rds") saveRDS(self$get_settings(call), file = paste0(path, "/", name, ".rds"))
      invisible(self)
    },
    
    #' @description Exports the analyses as \emph{json} (the default) or \emph{rds}.
    #'
    export_analyses = function(analyses = NULL, format = "json", name = "analyses", path = getwd()) {
      analyses <- self$get_analyses(analyses)
      if (format %in% "json") {
        js_analyses <- .convert_to_json(analyses)
        write(js_analyses, file = paste0(path, "/", name, ".json"))
      }
      if (format %in% "rds") saveRDS(analyses, file = paste0(path, "/", name, ".rds"))
      invisible(self)
    },
    
    #' @description Exports the engine data as as \emph{json} (the default) or \emph{rds}.
    #'
    export = function(format = "json", name = "EngineData", path = getwd()) {
      if (format %in% "json") {
        list_all <- list()
        headers <- private$.headers
        settings <- private$.settings
        analyses <- private$.analyses
        history <- private$.history
        results <- private$.results
        if (length(headers) > 0) list_all$headers <- headers
        if (!is.null(settings)) list_all$settings <- settings
        if (!is.null(analyses)) list_all$analyses <- analyses
        if (!is.null(history)) list_all$history <- history
        if (!is.null(results)) list_all$results <- results
        js_all <- .convert_to_json(list_all)
        write(js_all, file = paste0(path, "/", name, ".", "json"))
      }
      if (format %in% "rds") saveRDS(self, file = paste0(path, "/", name, ".rds"))
      invisible(self)
    },
    
    ## ___ import -----
    
    #' @description Imports headers from an \emph{rds} or \emph{json} file.
    #'
    #' @return Invisible.
    #'
    import_headers = function(file = NA_character_) {
      if (file.exists(file)) {
        headers <- NULL
        if (tools::file_ext(file) %in% "json") headers <- jsonlite::fromJSON(file)
        if (tools::file_ext(file) %in% "rds") headers <- readRDS(file)
        self$add_headers(headers)
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },
    
    #' @description Imports settings from an \emph{rds} or \emph{json} file.
    #'
    #' @param replace Logical. When `TRUE`, existing settings are replaced by the new settings with the same call name.
    #'
    import_settings = function(file = NA_character_, replace = TRUE) {
      if (file.exists(file)) {
        settings <- NULL
        if (tools::file_ext(file) %in% "json") settings <- jsonlite::fromJSON(file)
        if (tools::file_ext(file) %in% "rds") settings <- readRDS(file)
        self$add_settings(settings, replace)
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },
    
    #' @description Imports analyses from an \emph{rds} or \emph{json} file.
    #'
    import_analyses = function(file = NA_character_) {
      if (file.exists(file)) {
        analyses <- NULL
        if (tools::file_ext(file) %in% "json") analyses <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)
        if (tools::file_ext(file) %in% "rds") analyses <- readRDS(file)
        self$add_analyses(analyses)
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },
    
    #' @description Imports a CoreEngine saved as \emph{json} or \emph{rds}.
    #'
    #' @param file A \emph{json} or \emph{rds} file representing a CoreEngine.
    #'
    #' @return Invisible.
    #'
    import = function(file = NA_character_) {
      if (file.exists(file)) {
        if (tools::file_ext(file) %in% "json") {
          js_ms <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)
          fields_present <- names(js_ms)
          
          if ("headers" %in% fields_present) self$add_headers(js_ms[["headers"]])
          
          if ("settings" %in% fields_present) {
            if (!is.null(js_ms[["settings"]])) {
              self$add_settings(js_ms[["settings"]], replace = TRUE)
            }
          }
          
          if ("analyses" %in% fields_present) {
            if (!is.null(js_ms[["analyses"]])) {
              self$add_analyses(js_ms[["analyses"]])
            }
          }
          
          if ("history" %in% fields_present) {
            private$.history <- js_ms[["history"]]
          }
          
          if ("results" %in% fields_present) {
            warning("Results cannot be yet imported from a json file!")
          }
        }
        
        if (file_ext(file) %in% "rds") {
          ms <- readRDS(file)
          if (is(ms, "CoreEngine")) {
            private$.headers <- ms$headers
            private$.settings <- ms$settings
            private$.analyses <- ms$analyses
            private$.history <- ms$history
            private$.results <- ms$results
          } else {
            warning("The object in file is not a CoreEngine or a child of the CoreEngine!")
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
      engine_save_file <- self$save_file
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
      
      shiny::shinyApp(ui = .make_app_ui(), server = .make_app_server(engine_type, engine_save_file))
    },
    
    ## ___ info -----
    
    ### ___ processing_methods -----
    
    #' @description Data.table with available data processing methods.
    #'
    processing_methods = function() {
      data.table::data.table(name = character(), max = numeric())
    },
    
    ### ___ help -----
    
    #' @field help (`list()`)\cr
    #' List of function elements to access specific reference help pages.
    help = list(
      methods = function() {
        utils::browseURL("https://odea-project.github.io/StreamFind/reference/CoreEngine.html#methods")
      }
    )
  )
)
