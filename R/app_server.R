#' .make_app_server
#' 
#' @description Creates a Shiny app server for a given engine type and save file.
#' 
#' @param engine_type Character (length 1) with the engine type.
#' @param engine_save_file Character (length 1) with the engine save file.
#' 
#' @noRd
#'  
.make_app_server <- function(engine_type, engine_save_file) {

  server <- function(input, output, session) {
    
    # _Utility functions -----
    .add_notifications <- function(warnings, name_msg, msg) {
      shiny::showNotification(msg, duration = 5, type = "warning")
      warnings[[name_msg]] <- msg
      return(warnings)
    }
    
    .remove_notifications <- function(warnings, name_msgs) {
      warnings[name_msgs] <- NULL
      return(warnings)
    }
    
    .wrap_analyses_ui_in_divs <- function(elements) {
      lapply(elements, function(x) {
        htmltools::div(style = sprintf("min-width: %dpx; height: %dpx; display: flex; align-items: center;", 40, 40),x)
      })
    }
    
    .get_volumes <- function() {
      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        drives <- system("wmic logicaldisk get name", intern = TRUE)
        drives <- drives[grepl(":", drives)]
        drives <- gsub("\\s+", "", drives)
        names(drives) <- drives
      } else {
        drives <- list.files("/media", full.names = TRUE)
        names(drives) <- basename(drives)
        if (length(drives) == 0) {
          drives <- list.files("/mnt", full.names = TRUE)
          names(drives) <- basename(drives)
        }
      }
      c("wd" = getwd(), drives)
    }
    
    .get_valid_file_types <- function(engine_type) {
      if (engine_type %in% "MassSpecEngine") {
        c("mzML", "mzXML")
      } else if (engine_type %in% "RamanEngine") {
        c("asc")
      } else {
        c("txt", "csv")
      }
    }
    
    # _Constants -----
    
    ## Engine -----
    engine_call <- get(engine_type, envir = .GlobalEnv)
    engine_call_new <- engine_call[["new"]]
    engine <- suppressMessages(do.call(engine_call_new, list()))
    engine$load(engine_save_file)
    
    ## Other -----
    wdir <- getwd()
    save_file <- engine$save_file
    mandatory_header_names <- c("name", "author", "file", "date")
    volumes <- .get_volumes()
    file_types <- .get_valid_file_types(engine_type)
  
    # _Reactive values -----
    reactive_warnings <- shiny::reactiveVal(list())
    reactive_headers <- shiny::reactiveVal(engine$get_headers())
    reactive_files <- shiny::reactiveVal(engine$get_files())
    reactive_overview <- shiny::reactiveVal(engine$get_overview())
    reactive_history <- shiny::reactiveVal(engine$history)
    reactive_saved_history <- shiny::reactiveVal(engine$history)
    reactive_workflow <- shiny::reactiveVal(engine$settings)
    
    # _Warnings -----
    
    ## obs Unsaved engine -----
    shiny::observe({
      has_unsaved_changes <- "unsaved_changes" %in% names(reactive_warnings())
      equal_history <- identical(reactive_history(), reactive_saved_history())
      if (!equal_history && !has_unsaved_changes) {
        reactive_warnings(.add_notifications(reactive_warnings(), "unsaved_changes", "Unsaved changes in the engine!"))
      }
    })
    
    ## out Warnings menu -----
    output$warningMenu <- shinydashboard::renderMenu({
      warnings <- reactive_warnings()
      msgs <- lapply(warnings, function(x) { shinydashboard::notificationItem(text = x) })
      shinydashboard::dropdownMenu(type = "notifications", .list = msgs)
    })
    
    ## out Save engine -----
    output$save_engine <- shiny::renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shiny::actionButton("save_engine_button", label = "Save Engine", width = 200, class = "btn-danger")
        )
      }
    })
    
    ## event Save -----
    shiny::observeEvent(input$save_engine_button, {
      engine$save(save_file)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_files(engine$get_files())
      reactive_overview(engine$get_overview())
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
      reactive_workflow(engine$settings)
    })
    
    ## out Reset engine -----
    output$reset_engine <- shiny::renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shiny::actionButton("reset_engine_button", label = "Discard Changes", width = 200, class = "btn-danger")
        )
      }
    })
    
    ## event Reset -----
    shiny::observeEvent(input$reset_engine_button, {
      engine$load(save_file)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_files(engine$get_files())
      reactive_overview(engine$get_overview())
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
      reactive_workflow(engine$settings)
    })
  
    # _Overview -----
    
    ## _Info -----
    output$wdir <- shiny::renderUI({ htmltools::HTML(paste("<b>Working directory:</b>", wdir)) })
    
    ## _Headers -----
    output$headers_ui <- shiny::renderUI({
      .mod_headers_Server("headers", engine, reactive_headers)
      .mod_headers_UI("headers")
    })
    
    ### obs Update headers -----
    shiny::observe({
      if (!identical(reactive_headers(), engine$headers)) {
        reactive_header_names <- names(reactive_headers())
        engine_header_names <- names(engine$headers)
        headers_to_remove <- engine_header_names[!engine_header_names %in% reactive_header_names]
        if (length(headers_to_remove) > 0) {
          tryCatch({
            engine$remove_headers(headers_to_remove)
            reactive_history(engine$history)
          }, warning = function(w) {
            msg <- paste("Warning for headers:", conditionMessage(w))
            shiny::showNotification(msg, duration = 10, type = "warning")
            reactive_headers(engine$get_headers())
          }, error = function(e) {
            msg <- paste("Error for headers:", conditionMessage(e))
            shiny::showNotification(msg, duration = 10, type = "error")
            reactive_headers(engine$get_headers())
          })
        }
        if (length(headers_to_remove) == 0) {
          tryCatch({
            engine$add_headers(reactive_headers())
            reactive_history(engine$history)
          }, warning = function(w) {
            msg <- paste("Warning for headers:", conditionMessage(w))
            shiny::showNotification(msg, duration = 10, type = "warning")
            reactive_headers(engine$get_headers())
          }, error = function(e) {
            msg <- paste("Error for headers:", conditionMessage(e))
            shiny::showNotification(msg, duration = 10, type = "error")
            reactive_headers(engine$get_headers())
          })
        }
      }
    })
    
    ## _Analyses -----
    output$analyses_ui <- shiny::renderUI({
      .mod_analyses_Server("analyses", engine, reactive_files, reactive_overview, reactive_warnings, reactive_history, volumes, file_types)
      .mod_analyses_UI("analyses")
    })
    
    ### obs Add files -----
    shiny::observe({
      files <- reactive_files()
      engine_files <- engine$get_files()
      files_to_add <- files[!files %in% engine_files]
      number_files <- length(files_to_add)
      if (number_files > 0) {
        output$loading_spinner <- shiny::renderUI({ htmltools::div(style = "height: 100px; width: 100px;") })
        shiny::withProgress(message = 'Loading files...', value = 0, {
          for (i in seq_len(number_files)) {
            tryCatch({
              engine$add_files(files_to_add[i])
            }, warning = function(w) {
              msg <- paste("Warning for", files_to_add[i], ":", conditionMessage(w))
              shiny::showNotification(msg, duration = 10, type = "warning")
            }, error = function(e) {
              msg <- paste("Error for", files_to_add[i], ":", conditionMessage(e))
              shiny::showNotification(msg, duration = 10, type = "error")
            })
            shiny::incProgress(i/number_files)
          }
        })
        output$loading_spinner <- shiny::renderUI({ NULL })
        reactive_files(engine$get_files())
        reactive_overview(engine$get_overview())
        reactive_history(engine$history)
      }
    })
    
    ### obs Updates overview -----
    shiny::observe({
      analyses <- reactive_overview()$analysis
      engine_analyses <- engine$get_overview()$analysis
      if (!identical(analyses, engine_analyses)) {
        analyses_to_remove <- engine_analyses[!engine_analyses %in% analyses]
        if (length(analyses_to_remove) > 0) engine$remove_analyses(analyses_to_remove)
        reactive_overview(engine$get_overview())
        reactive_history(engine$history)
      }
      
      replicates <- reactive_overview()$replicate
      if (!identical(replicates, engine$get_overview()$replicate)) {
        tryCatch({
          engine$add_replicate_names(replicates)
          reactive_overview(engine$get_overview())
          reactive_history(engine$history)
        }, warning = function(w) {
          shiny::showNotification(conditionMessage(w), duration = 10, type = "warning")
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), duration = 10, type = "error")
        })
      }
      
      blanks <- reactive_overview()$blank
      if (!identical(blanks, engine$get_overview()$blank)) {
        tryCatch({
          engine$add_blank_names(blanks)
          reactive_overview(engine$get_overview())
          reactive_history(engine$history)
        }, warning = function(w) {
          shiny::showNotification(conditionMessage(w), duration = 10, type = "warning")
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), duration = 10, type = "error")
        })
      }
    })
    
    # _Explorer -----
    output$explorer_ui <- shiny::renderUI({
      if (engine_type %in% "MassSpecEngine") {
        .mod_MassSpecEngine_summary_Server("summary", engine, reactive_overview, volumes)
        .mod_MassSpecEngine_summary_UI("summary", engine)
        
      } else if (engine_type %in% "RamanEngine") {
        .mod_RamanEngine_summary_Server("summary", engine, reactive_overview, volumes)
        .mod_RamanEngine_summary_UI("summary", engine)
        
      } else {
        htmltools::div("Explorer not implemented for engine type ", engine_type)
      }
    })
    
    # _Workflow -----
    output$workflow_ui <- shiny::renderUI({
      .mod_workflow_Server("workflow", engine, engine_type, reactive_workflow, reactive_warnings, reactive_history, volumes)
      .mod_workflow_UI("workflow")
    })
    
    
    # _History -----
    output$"history_table" <- shiny::renderDataTable({
      h_list <- reactive_history()
      h_dt <- data.table::rbindlist(h_list, fill = TRUE)
      h_dt$time <- format(h_dt$time, "%Y-%m-%d %H:%M:%S")
      h_dt
    })
  }

  return(server)
}