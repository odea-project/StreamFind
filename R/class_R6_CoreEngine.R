#' **CoreEngine** R6 class and methods
#'
#' @description
#' The *CoreEngine* R6 class is a basic data processor with generic methods for handling data.
#'
#' @template arg-headers
#' @template arg-ms-analyses
#' @template arg-verbose
#' @template arg-ms-features
#' @template arg-ms-settings
#' @template arg-ms-save-format
#' @template arg-ms-save-name
#' @template arg-ms-save-path
#' @template arg-ms-import-file
#' @template arg-ms-xlim-ylim
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
    
    ## ___ .modules -----
    .modules = NULL,
    
    ## ___ .utils -----
    
    # Registers changes in the history private field.
    #
    .register = function(
      action = NA_character_,
      object = NA_character_,
      name = NA_character_,
      software = NA_character_,
      version = NA_character_,
      details = NA_character_) {
      
      date_time <- Sys.time()
      
      if (is.null(private$.history)) private$.history <- list()
      
      private$.history[[as.character.POSIXt(date_time)]] <- data.table(
        "time" = date_time,
        "action" = action,
        "object" = object,
        "name" = name,
        "software" = software,
        "version" = version,
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
    .validate_list_analyses = function(analyses = NULL) {
      lapply(analyses, function(x) as.Analysis(x))
    },
    
    # Extracts and validates ProcessingSettings for a given call.
    #
    .get_call_settings = function(settings, call) {
      
      checkmate::assert_choice(call, self$processing_methods()$name)
      
      if (is.null(settings)) settings <- self$get_settings(call)
      
      if (is.null(settings)) return(NULL)
      
      cols_check <- c("call", "algorithm", "parameters")
      
      if (all(cols_check %in% names(settings))) settings <- list(settings)
      
      if (length(settings) > 1) {
        warning("More then one settings for ", call, " found!")
        return(NULL)
      }
      
      settings <- as.ProcessingSettings(settings)
      
      if (checkmate::test_choice(call, settings$call)) {
        settings
        
      } else {
        warning("Settings call must be ", call, "!")
        NULL
      }
    },
    
    # Checks if settings are already stored.
    #
    .settings_already_stored = function(settings) {
      any(vapply(private$.settings, function(x, settings) {
        identical(x, settings)
      }, settings = settings, FALSE))
    }
  ),
    
  # _ public fields/methods -----
  public = list(
    ## ___ create -----
    
    #' @description
    #' Creates an R6 *CoreEngine* class object.
    #'
    initialize = function(headers = NULL,
                          settings = NULL,
                          analyses = NULL) {
      
      if (is.null(headers)) headers <- ProjectHeaders()
      
      if (!is.null(headers)) suppressMessages(self$add_headers(headers))
      
      if (!is.null(settings)) suppressMessages(self$add_settings(settings))
      
      if (!is.null(analyses)) suppressMessages(self$add_analyses(analyses))
      
      private$.register(
        "created",
        "CoreEngine",
        headers$name,
        "StreamFind",
        as.character(packageVersion("StreamFind")),
        paste(c(headers$author, headers$path), collapse = ", ")
      )
      
      message("\U2713 Engine created!")
    },
    
    ## ___ print -----
    
    #' @description
    #' Prints a summary to the console.
    #'
    print = function() {
      cat("\n")
      cat(paste(is(self), collapse = "; "))
      cat("\n")
      cat(
        "name          ", private$.headers$name, "\n",
        "author        ", private$.headers$author, "\n",
        "path          ", private$.headers$path, "\n",
        "date          ", as.character(private$.headers$date), "\n",
        sep = ""
      )
      
      self$print_workflow()
      
      self$print_analyses()
    },
    
    #' @description Prints the headers.
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
    
    #' @description Prints the analyses present.
    #'
    print_analyses = function() {
      cat("\n")
      cat("Analyses")
      if (length(private$.analyses) > 0) {
        cat("\n")
        overview <- self$get_overview()
        overview$file <- NULL
        row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
        print(overview)
      } else {
        cat(" empty \n")
      }
    },
    
    #' @description Prints all processing methods present by order.
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
    
    ## ___ get -----
    
    #' @description Gets the headers.
    #'
    #' @param value A character vector with the name/s of the header elements.
    #' When `NULL` (the default), the entire headers list is returned.
    #'
    #' @return The headers list or the header elements as defined by `value`.
    #'
    get_headers = function(value = NULL) {
      if (is.null(value)) {
        private$.headers
      } else {
        private$.headers[value]
      }
    },
    
    #' @description Gets the object history.
    #'
    #' @return The history list of processing steps applied.
    #'
    get_history = function() {
      
      if (is.list(private$.history)) {
        rbindlist(private$.history, fill = TRUE)
        
      } else {
        private$.history
      }
    },
    
    #' @description Gets analyses.
    #'
    #' @return The list of analyses or the analyses as defined by `analyses`
    #' argument.
    #'
    get_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      private$.analyses[vapply(analyses, function(x) which(names(private$.analyses) %in% x), 0)]
    },
    
    #' @description Gets the number of analyses present.
    #'
    get_number_analyses = function() {
      length(private$.analyses)
    },
    
    #' @description Gets an overview data.table with all the analysis names, 
    #' replicates, associated blank replicates, and full file paths.
    #'
    #' @return A data.table with columns analysis, replicate, blank and file.
    #'
    get_overview = function() {
      if (length(private$.analyses) > 0) {
        
        df <- data.table(
          "analysis" = vapply(private$.analyses, function(x) x$name, ""),
          "replicate" = vapply(private$.analyses, function(x) x$replicate, ""),
          "blank" = vapply(private$.analyses, function(x) x$blank, ""),
          "file" = vapply(private$.analyses, function(x) x$file, "")
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
    
    #' @description A data.table with the overview of all processing methods present.
    #'
    get_workflow_overview = function() {
      if (self$has_settings()) {
        data.table(
          "call" = vapply(private$.settings, function(x) x$call, ""),
          "algorithm" = vapply(private$.settings, function(x) x$algorithm, ""),
          "developer" = vapply(private$.settings, function(x) paste(x$developer, collapse = "; "), ""),
          "contact" = vapply(private$.settings, function(x) x$contact, ""),
          "link" = vapply(private$.settings, function(x) x$link, "")
        )
      } else {
        data.table()
      }
    },
    
    ## ___ add -----
    
    #' @description Adds headers. If an argument or element "name" is given, 
    #' it must be type character. If an argument or element path is given, it 
    #' must be type character and exist. If an argument or element date is 
    #' given, it must be class POSIXct or POSIXt. If given date is character, 
    #' conversion to class POSIXct or POSIXt is attempted.
    #' See `?ProjectHeaders` for more information.
    #'
    #' @template arg-headers-ellipsis
    #'
    #' @return Invisible.
    #'
    add_headers = function(...) {
      
      headers <- ProjectHeaders(...)
      
      if (is(headers, "ProjectHeaders")) {
        old_headers <- private$.headers
        if (is.null(old_headers)) old_headers <- list()
        
        if (length(old_headers) > 0) {
          new_headers <- old_headers[!names(old_headers) %in% names(headers)]
          new_headers[names(headers)] <- headers
        } else {
          new_headers <- headers
        }
        
        new_headers <- as.ProjectHeaders(new_headers)
        
        if (!identical(new_headers, old_headers) & is(new_headers, "ProjectHeaders")) {
          private$.headers <- new_headers
          
          lapply(names(headers), function(x, new_headers) {
            private$.register(
              "added",
              "ProjectHeaders",
              x,
              NA_character_,
              NA_character_,
              new_headers[x]
            )
          }, new_headers = new_headers)
          
          
          message("\U2713 Added headers!")
        }
        
      } else {
        warning("Invalid headers content or structure! Not added.")
      }
      invisible(self)
    },
    
    #' @description Adds processing settings.
    #'
    #' @param settings A named list of ProcessingSettings S3 class objects or a
    #' single ProcessingSettings S3 class object. The list names should match
    #' the call name of each ProcessingSettings object. Alternatively, a named
    #' list with call name, algorithm and parameters to be transformed and added
    #' as ProcessingSettings S3 class object.
    #'
    #' @param replace Logical of length one. When `TRUE`, existing settings are 
    #' replaced by the new settings with the same call name, except settings for
    #' methods that can be applied more than once.
    #'
    #' @return Invisible.
    #'
    add_settings = function(settings = NULL, replace = FALSE) {
      
      if (is.list(settings)) {
        
        cols_check <- c("call", "algorithm", "parameters")
        if (all(cols_check %in% names(settings))) {
          settings <- list(settings)
        }
        
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
              message("\U2139 ", duplicated_names, " duplicate not added as only one is possible!")
              
            } else {
              message(paste0("\U2713 Duplicate settings for the following methods not added as only one is possible!\n",
                paste(duplicated_names, collapse = "\n"))
              )
            }
            
            settings <- settings[!(duplicated(call_names) & call_names %in% only_one_possible)]
            
            call_names <- vapply(settings, function(x) x$call, NA_character_)
          }
          
          if (is.null(private$.settings)) private$.settings <- list()
          
          lapply(settings, function(x, only_one_possible, replace) {
            
            stored_calls <- vapply(private$.settings, function(z) z$call, NA_character_)
            
            if (replace) {
              
              if (x$call %in% stored_calls) {
                
                # case when repeating can happen with replace = TRUE as long as duplicated in call_names
                if (!(x$call %in% only_one_possible) & any(duplicated(call_names))) {
                  private$.settings <- c(private$.settings, list(x))
                  
                  private$.register(
                    "added",
                    "ProcessingSettings",
                    x$call,
                    "StreamFind",
                    x$version,
                    x$algorithm
                  )
                  
                  message(paste0("\U2713 ", x$call, " processing settings added!"))
                  
                } else {
                  private$.settings[which(x$call %in% stored_calls)] <- list(x)
                  
                  private$.register(
                    "replaced",
                    "ProcessingSettings",
                    x$call,
                    "StreamFind",
                    x$version,
                    x$algorithm
                  )
                  
                  message(paste0("\U2713 ", x$call, " processing settings replaced!"))
                }
                
              } else {
                private$.settings <- c(private$.settings, list(x))
                
                private$.register(
                  "added",
                  "ProcessingSettings",
                  x$call,
                  "StreamFind",
                  x$version,
                  x$algorithm
                )
                
                message(paste0("\U2713 ", x$call, " processing settings added!"))
              }
              
            } else {
              
              if (x$call %in% only_one_possible && x$call %in% stored_calls) {
                message("\U2139 ", x$call, " replaced as only one is possible!")
                
                private$.settings[which(x$call %in% stored_calls)] <- list(x)
                
                private$.register(
                  "replaced",
                  "ProcessingSettings",
                  x$call,
                  "StreamFind",
                  x$version,
                  x$algorithm
                )
                
                message(paste0("\U2713 ", x$call, " processing settings replaced!"))
                
              } else {
                private$.settings <- c(private$.settings, list(x))
                
                private$.register(
                  "added",
                  "ProcessingSettings",
                  x$call,
                  "StreamFind",
                  x$version,
                  x$algorithm
                )
                
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
    
    #' @description
    #' Adds analyses.
    #'
    #' @param analyses An S3 class object or a list with
    #' analyses as elements.
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
        analyses <- private$.validate_list_analyses(analyses)
        analyses <- analyses[!vapply(analyses, is.null, TRUE)]
        
        if (length(analyses) > 0) {
          old_analyses <- self$get_analyses()
          old_names <- NULL
          
          if (length(old_analyses) > 0) {
            old_names <- vapply(old_analyses, function(x) x$name, "")
          }
          
          new_names <- c(old_names, vapply(analyses, function(x) x$name, ""))
          
          if (!any(duplicated(new_names))) {
            new_analyses <- c(old_analyses, analyses)
            names(new_analyses) <- new_names
            new_analyses <- new_analyses[order(names(new_analyses))]
            old_size <- length(private$.analyses)
            
            private$.analyses <- new_analyses
            
            lapply(analyses, function(x) {
              private$.register(
                "added",
                class(x),
                x$name,
                "StreamFind",
                x$version,
                x$file
              )
            })
            
            message(
              paste0(
                "\U2713 ",
                length(new_analyses) - old_size,
                " analyses added!"
              )
            )
            
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
    #' @param value A character vector with the analysis replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_replicate_names = function(value = NULL) {
      if (is.character(value) && length(value) == self$get_number_analyses()) {
        
        private$.analyses <- Map(
          function(x, y) {
            x$replicate <- y
            x
          },
          private$.analyses, value
        )
        
        private$.register(
          "added",
          "analyses",
          "replicate names",
          NA_character_,
          NA_character_,
          NA_character_
        )
        
        message("\U2713 Replicate names added!")
        
      } else {
        warning("Not done, check the value!")
      }
      invisible(self)
    },
    
    #' @description Adds or redefines the analysis blank replicate names.
    #'
    #' @param value A character vector with the analysis blank replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_blank_names = function(value = NULL) {
      if (is.character(value) & length(value) == self$get_number_analyses()) {
        
        if (all(value %in% self$get_replicate_names())) {
          
          private$.analyses <- Map(
            function(x, y) {
              x$blank <- y
              x
            },
            private$.analyses, value
          )
          
          private$.register(
            "added",
            "analyses",
            "blank names",
            NA_character_,
            NA_character_,
            NA_character_
          )
          
          message("\U2713 Blank names added!")
          
        } else {
          warning("Not done, blank names not among replicate names!")
        }
        
      } else {
        warning("Not done, check the value!")
      }
      invisible(self)
    },
    
    #' @description Adds metadata to analyses.
    #'
    #' @param value A data.frame or data.table with metadata for the analyses.
    #' The data.frame or data.table must have an analysis column and the same 
    #' number of rows as the number of analyses in the engine. Metadata is added
    #' using any extra columns of the data.frame or data.table.
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
              
              private$.register(
                "added",
                "analyses",
                "metadata",
                NA_character_,
                NA_character_,
                paste(col_names, collapse = "; ")
              )
              
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
    
    #' @description Adds data from modules to the engine.
    #'
    #' @param value A named list with data from modules.
    #'
    #' @return Invisible.
    #'
    add_modules_data = function(value = NULL) {
      
      value_names <- names(value)
      
      if (!is.null(value_names)) {
        
        if (is.null(private$.modules)) private$.modules <- list()
        
        lapply(value_names, function(x, value) {
          
          if (is.null(value[[x]]$software)) value[[x]]$software <- NA_character_
          if (is.null(value[[x]]$version)) value[[x]]$version <- NA_character_
          
          private$.modules <- c(private$.modules, value[x])
          
          private$.register(
            "added",
            "module",
            x,
            value[[x]]$software,
            value[[x]]$version,
            length(value[[x]])
          )
          
          message(paste0("\U2713 ", x, " data added to modules!"))
          
        }, value = value)
        
      } else {
        warning("Not done, the value must be a named list!")
      }
      
      invisible(self)
    },
    
    ## ___ remove -----
    
    #' @description Removes headers entries. Note that the name, path and date 
    #' headers cannot be removed only changed.
    #'
    #' @param value A character vector with the name/s of the elements in headers
    #' to be removed.
    #'
    #' @return Invisible.
    #'
    remove_headers = function(value = NULL) {
      if (!is.null(value)) {
        value <- value[!(value %in% c("name", "author", "path", "date"))]
        
        if (length(value) == 0) {
          warning("Name, author, path and date headers cannot be removed!")
          value <- NA_character_
        }
        
        message("\U2713 Removed headers: ", paste(value[value %in% names(private$.headers)], collapse = ", "))
        
        lapply(value, function(x) {
          if (x %in% names(private$.headers)) {
            private$.register(
              "removed",
              "ProjectHeaders",
              x,
              NA_character_,
              NA_character_,
              private$.headers[[x]]
            )
            
            private$.headers[x] <- NULL
          }
        })
        
      } else {
        to_remove <- names(private$.headers) %in% c("name", "author", "path", "date")
        to_remove <- names(private$.headers)[!to_remove]
        private$.headers[to_remove] <- NULL
        
        if (length(to_remove) > 1) {
          details <- paste(to_remove, collapse = ", ")
          
          private$.register(
            "removed",
            "ProjectHeaders",
            "all",
            NA_character_,
            NA_character_,
            details
          )
          
          message("\U2713 Removed headers: \n",paste(to_remove, collapse = "\n"))
          
        } else {
          message("\U2713 Removed all headers except name, author, path and date!")
        }
      }
      invisible(self)
    },
    
    #' @description Removes settings.
    #'
    #' @param call A string or a vector of strings with the name/s of the
    #' processing method/s to be removed. Alternatively, an integer vector
    #' with the index/es of the settings to be removed. When `call` is
    #' \code{NULL} all settings are removed.
    #'
    #' @return Invisible.
    #'
    remove_settings = function(call = NULL) {
      
      if (is.null(call)) {
        lapply(private$.settings, function(x) {
          private$.register(
            "removed",
            "ProcessingSettings",
            x$call,
            "StreamFind",
            x$version,
            x$algorithm
          )
        })
        
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
          lapply(private$.settings[to_remove], function(x) {
            private$.register(
              "removed",
              "ProcessingSettings",
              x$call,
              "StreamFind",
              x$version,
              x$algorithm
            )
          })
          
          private$.settings[to_remove] <- NULL
          
          message("\U2713 Removed settings for:\n",paste(all_calls[to_remove], collapse = "\n"))
          
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
      
      if (!is.null(analyses)) {
        analyses <- private$.check_analyses_argument(analyses)
        allNames <- self$get_analysis_names()
        keepAnalyses <- unname(allNames[!(allNames %in% analyses)])
        removeAnalyses <- unname(allNames[allNames %in% analyses])
        analysesLeft <- self$get_analyses(keepAnalyses)
        
        if (length(removeAnalyses) > 0) {
          
          private$.analyses <- analysesLeft
          private$.alignment <- private$.alignment[keepAnalyses]
          
          lapply(removeAnalyses, function(x) {
            private$.register(
              "removed",
              "MassSpecAnalysis",
              x,
              NA_character_,
              NA_character_,
              NA_character_
            )
          })
          
          message("\U2713 Removed analyses:\n", paste(removeAnalyses, collapse = "\n"))
          
        } else {
          message("\U2717 There are no analyses to remove!")
        }
        
      } else {
        lapply(private$.analyses, function(x) {
          private$.register(
            "removed",
            "MassSpecAnalysis",
            x,
            NA_character_,
            NA_character_,
            NA_character_
          )
        })
        
        private$.analyses <- NULL
        private$.alignment <- NULL
        
        message("\U2713 Removed all analyses!")
      }
      
      invisible(self)
    },
    
    ## ___ subset -----
    
    #' @description Subsets on analyses.
    #'
    #' @return A new cloned object with only the analyses as
    #' defined by the `analyses` argument.
    #'
    subset_analyses = function(analyses = NULL) {
      analyses <- private$.check_analyses_argument(analyses)
      
      if (!is.null(analyses)) {
        allNames <- self$get_analysis_names()
        removeAnalyses <- unname(allNames[!(allNames %in% analyses)])
        keepAnalyses <- unname(allNames[allNames %in% analyses])
        
        if (length(keepAnalyses) > 0) {
          newAnalyses <- self$get_analyses(keepAnalyses)
          newAlignment <- self$get_alignment()[keepAnalyses]
          
          new_ms <- suppressMessages(CoreEngine$new(
            files = NULL,
            headers = self$get_headers(),
            settings = self$get_settings(),
            analyses = newAnalyses,
          ))
          
          message("\U2713 Subset with ", new_ms$get_number_analyses(), " analyses created!")
          
          return(new_ms)
        }
      }
      
      message("\U2717 There are no analyses selected to subset!")
      
      suppressMessages(CoreEngine$new(
        headers = self$get_headers(),
        settings = self$get_settings())
      )
    },
    
    ## ___ has -----
    
    #' @description Checks if analyses are present.
    #'
    #' @return Logical value.
    #'
    has_analyses = function() {
      length(private$.analyses) > 0
    },
    
    #' @description Checks if there are processing settings.
    #'
    #' @param call A string or a vector of strings with the name/s of the
    #' processing method/s.
    #'
    #' @return Logical value.
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
    
    ## ___ plot -----
    
    ## ___ processing -----
    
    #' @description Runs all modules represented by the added ProcessingSettings.
    #'
    #' @return Invisible.
    #'
    run_workflow = function() {
      
      if (self$has_settings()) {
        
        lapply(self$get_settings(), function(x) {
          call <- x$call
          message("\U2699 Running ", call, " with ", x$algorithm)
          do.call(self[[call]], list("settings" = x))
        })
        
      } else {
        warning("There are no processing settings to run!")
      }
      
      invisible(self)
    },
    
    ## ___ save -----
    
    #' @description Saves the headers list.
    #'
    #' @return Saves the headers list as the defined \code{format} in
    #' \code{path} and returns invisible.
    #'
    save_headers = function(format = "json", name = "headers", path = getwd()) {
      if (format %in% "json") {
        js_headers <- toJSON(
          private$.headers,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )
        write(js_headers, file = paste0(path, "/", name, ".json"))
      }
      
      if (format %in% "rds") {
        saveRDS(private$.headers, file = paste0(path, "/", name, ".rds"))
      }
      
      invisible(self)
    },
    
    #' @description Saves settings.
    #'
    #' @param call A string or a vector of strings with the name/s of the
    #' processing method/s to be saved. When `call` is \code{NULL} all
    #' settings are saved.
    #'
    #' @return Saves the settings list as the defined \code{format} in
    #' \code{path} and returns invisible.
    #'
    save_settings = function(call = NULL, format = "json", name = "settings", path = getwd()) {
      
      js_settings <- self$get_settings(call)
      
      names(js_settings) <- vapply(js_settings, function(x) x$call, NA_character_)
      
      if (format %in% "json") {
        js_settings <- toJSON(
          js_settings,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )
        write(js_settings, file = paste0(path, "/", name, ".json"))
      }
      
      if (format %in% "rds") {
        saveRDS(self$get_settings(call), file = paste0(path, "/", name, ".rds"))
      }
      
      invisible(self)
    },
    
    #' @description Saves analyses.
    #'
    #' @return Saves the list of analyses as the defined \code{format} in
    #' \code{path} and returns invisible.
    #'
    save_analyses = function(analyses = NULL, format = "json", name = "analyses", path = getwd()) {
      
      analyses <- self$get_analyses(analyses)
      
      if (format %in% "json") {
        js_analyses <- toJSON(
          analyses,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )
        
        write(js_analyses, file = paste0(path, "/", name, ".json"))
      }
      
      if (format %in% "rds") {
        saveRDS(analyses, file = paste0(path, "/", name, ".rds"))
      }
      
      invisible(self)
    },
    
    #' @description Saves the private fields of the engine.
    #'
    #' @return Saves the private fields of the engine as the defined `format`
    #' in the \code{path} and returns invisible.
    #'
    save = function(format = "json", name = "EngineData", path = getwd()) {
      
      if (format %in% "json") {
        list_all <- list()
        
        headers <- private$.headers
        settings <- private$.settings
        analyses <- private$.analyses
        history <- private$.history
        modules <- private$.modules
        
        if (length(headers) > 0) list_all$headers <- headers
        if (!is.null(settings)) list_all$settings <- settings
        if (!is.null(analyses)) list_all$analyses <- analyses
        if (!is.null(history)) list_all$history <- history
        if (!is.null(modules)) list_all$modules <- modules
        
        js_all <- toJSON(
          list_all,
          dataframe = "columns",
          Date = "ISO8601",
          POSIXt = "string",
          factor = "string",
          complex = "string",
          null = "null",
          na = "null",
          auto_unbox = FALSE,
          digits = 8,
          pretty = TRUE,
          force = TRUE
        )
        
        write(js_all, file = paste0(path, "/", name, ".", "json"))
      }
      
      if (format %in% "rds") {
        saveRDS(self, file = paste0(path, "/", name, ".rds"))
      }
      
      invisible(self)
    },
    
    ## ___ import -----
    
    #' @description
    #' Imports headers from a \emph{rds} or \emph{json} file.
    #'
    #' @return Invisible.
    #'
    import_headers = function(file = NA_character_) {
      if (file.exists(file)) {
        headers <- NULL
        if (file_ext(file) %in% "json") headers <- fromJSON(file)
        if (file_ext(file) %in% "rds") headers <- readRDS(file)
        self$add_headers(headers)
        
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },
    
    #' @description
    #' Imports processing settings from a \emph{rds} or \emph{json} file.
    #'
    #' @param replace Logical. When `TRUE`, existing settings are replaced by
    #' the new settings with the same call name.
    #'
    #' @return Invisible.
    #'
    import_settings = function(file = NA_character_, replace = TRUE) {
      if (file.exists(file)) {
        settings <- NULL
        if (file_ext(file) %in% "json") settings <- fromJSON(file)
        if (file_ext(file) %in% "rds") settings <- readRDS(file)
        self$add_settings(settings, replace)
        
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },
    
    #' @description
    #' Imports analyses from a \emph{rds} or \emph{json} file.
    #'
    #' @return Invisible.
    #'
    import_analyses = function(file = NA_character_) {
      if (file.exists(file)) {
        analyses <- NULL
        if (file_ext(file) %in% "json") {
          analyses <- fromJSON(file, simplifyDataFrame = FALSE)
        }
        if (file_ext(file) %in% "rds") analyses <- readRDS(file)
        self$add_analyses(analyses)
      } else {
        warning("File not found in given path!")
      }
      invisible(self)
    },
    
    #' @description
    #' Imports a *CoreEngine* saved as \emph{json}.
    #'
    #' @param file A \emph{json} file representing a *CoreEngine*.
    #'
    #' @return Invisible.
    #'
    import = function(file = NA_character_) {
      
      if (file.exists(file)) {
        if (file_ext(file) %in% "json") {
          js_ms <- fromJSON(file, simplifyDataFrame = FALSE)
          
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
          
          if ("modules" %in% fields_present) {
            private$.modules <- js_ms[["modules"]]
          }
        }
      } else {
        warning("File not found in given path!")
        NULL
      }
      
      invisible(self)
    },
    
    ## ___ report -----
    
    ## ___ info -----
    
    ### ___ processing_methods -----
    
    #' @description
    #' Possible data processing methods.
    #'
    #' @return A character vector with ordered possible data  processing methods.
    #'
    processing_methods = function() {
      data.table(name = character(), max = numeric())
    },
    
    ### ___ help -----
    
    #' @field help (`list()`)\cr
    #' List of function elements to access specific reference help pages.
    help = list(
      methods = function() {
        browseURL("https://odea-project.github.io/StreamFind/reference/CoreEngine.html#methods")
      }
    )
  )
)