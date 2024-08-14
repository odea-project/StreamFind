#' .make_app_server
#' 
#' @description Creates a Shiny app server for a given engine type and save file.
#' 
#' @param engine_type Character (length 1) with the engine type.
#' @param engine_save_file Character (length 1) with the engine save file.
#' 
#' @noRd
#'  
.make_app_server <- function(engine_type = NULL, engine_save_file = NULL) {

  server <- function(input, output, session) {
    
    # _Utility Functions -----
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
    
    # _Constants/Mutable -----
    mandatory_header_names <- c("name", "author", "file", "date")
    volumes <- .get_volumes()
    engine <- NULL
    
    # _Reactive Variables -----
    reactive_wdir <- shiny::reactiveVal(getwd())
    reactive_volumes <- shiny::reactiveVal(volumes)
    reactive_engine_type <- shiny::reactiveVal(NA_character_)
    reactive_engine_save_file <- shiny::reactiveVal(NA_character_)
    reactive_file_types <- shiny::reactiveVal(NA_character_)
    reactive_warnings <- shiny::reactiveVal(list())
    reactive_headers <- shiny::reactiveVal(list())
    reactive_files <- shiny::reactiveVal(NA_character_)
    reactive_analyses <- shiny::reactiveVal(data.table())
    reactive_history <- shiny::reactiveVal(list())
    reactive_saved_history <- shiny::reactiveVal(list())
    reactive_workflow <- shiny::reactiveVal(list())
    
    # _Setup App -----
    if (is.null(engine_type)) engine_type <- "CoreEngine"
    if (is.null(engine_save_file)) engine_save_file <- NA_character_
    reactive_engine_type(engine_type)
    reactive_engine_save_file(engine_save_file)
    
    ## obs Engine Save File -----
    observeEvent(reactive_engine_save_file(), {
      engine_save_file <- reactive_engine_save_file()
      if (!is.na(engine_save_file)) {
        if (!grepl(".sqlite$", engine_save_file)) {
          msg <- paste("The file", engine_save_file, "is not an sqlite file!")
          shiny::showNotification(msg, duration = 10, type = "error")
          reactive_engine_save_file(NA_character_)
        }
      }
    })
    
    ## obs Engine Type -----
    observeEvent(reactive_engine_type(), {
      engine_type <- reactive_engine_type()
      engine_call <- get(engine_type, envir = .GlobalEnv)
      engine_call_new <- engine_call[["new"]]
      engine <<- suppressMessages(do.call(engine_call_new, list()))
      if (!is.na(engine_save_file)) engine$load(engine_save_file)
      reactive_headers(engine$get_headers())
      reactive_files(engine$get_files())
      reactive_analyses(engine$get_overview())
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
      reactive_workflow(engine$settings)
      reactive_file_types(.get_valid_file_types(engine_type))
    })
    
    ## out Engine Type -----
    output$engine_type_ui <- shiny::renderUI({ tags$span(reactive_engine_type()) })
    
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
      
      shinyFiles::shinyFileSave(input, "save_engine_button_file", roots = reactive_volumes(), defaultRoot = "wd", session = session)
      
      if ("unsaved_changes" %in% names(reactive_warnings()) && !is.na(reactive_engine_save_file())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shiny::actionButton("save_engine_button", label = "Save Engine", width = 200, class = "btn-danger")
        )
      } else if ("unsaved_changes" %in% names(reactive_warnings()) && is.na(reactive_engine_save_file())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shinyFiles::shinySaveButton("save_engine_button_file", label = "Save Engine", title = "Save the engine as .sqlite", width = 200, class = "btn-success", filetype = list(sqlite = "sqlite"))
        )
      } else {
        htmltools::div(style = "margin-bottom: 20px;",
          shinyFiles::shinySaveButton("save_engine_button_file", label = "Save Engine", title = "Save the engine as .sqlite", width = 200, class = "btn-success", filetype = list(sqlite = "sqlite"))
        )
      }
    })
    
    ## event Save -----
    shiny::observeEvent(input$save_engine_button, {
      engine$save(reactive_engine_save_file())
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_files(engine$get_files())
      reactive_analyses(engine$get_overview())
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
      reactive_workflow(engine$settings)
    })
    
    ## event Save Engine File -----
    shiny::observeEvent(input$save_engine_button_file, {
      req(input$save_engine_button_file)
      file_info <- shinyFiles::parseSavePath(roots = reactive_volumes(), input$save_engine_button_file)
      if (nrow(file_info) > 0) {
        file_path <- file_info$datapath
        engine$save(file_path)
        reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
        reactive_headers(engine$get_headers())
        reactive_files(engine$get_files())
        reactive_analyses(engine$get_overview())
        reactive_history(engine$history)
        reactive_saved_history(engine$history)
        reactive_workflow(engine$settings)
        reactive_engine_save_file(engine$save_file)
      }
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
      engine$load(reactive_engine_save_file())
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_files(engine$get_files())
      reactive_analyses(engine$get_overview())
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
      reactive_workflow(engine$settings)
    })
  
    # _Project -----
    
    ## Working Directory -----
    output$wdir <- shiny::renderUI({ 
      
      shinyFiles::shinyDirChoose(input, "set_wdir_button", roots = reactive_volumes(), defaultRoot = "wd", session = session)
      
      shinydashboard::box(width = 12, title = "Working Directory", solidHeader = TRUE,
        shinyFiles::shinyDirButton("set_wdir_button", "Change Working Directory", "Select Working Directory", "wd"),
        htmltools::HTML(paste("  ", reactive_wdir()))
      )
    })
    
    ## event Change Working Directory -----
    shiny::observeEvent(input$set_wdir_button, {
      req(input$set_wdir_button)
      file_info <- shinyFiles::parseDirPath(roots = reactive_volumes(), input$set_wdir_button)
      if (length(file_info) > 0) {
        setwd(file_info)
        reactive_wdir(file_info)
        volumes <<- .get_volumes()
        reactive_volumes(volumes)
      }
    })
    
    ## Headers -----
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
      
      if (reactive_engine_type() %in% "CoreEngine") {
        shiny::showNotification("Analyses not implemented for CoreEngine", duration = 5, type = "warning")
        return(htmltools::div(" "))
      }
      
      .mod_analyses_Server("analyses", engine, reactive_files, reactive_analyses, reactive_warnings, reactive_history, reactive_file_types, volumes)
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
        reactive_analyses(engine$get_overview())
        reactive_history(engine$history)
      }
    })
    
    ### obs Updates overview -----
    shiny::observe({
      analyses <- reactive_analyses()$analysis
      engine_analyses <- engine$get_overview()$analysis
      if (!identical(analyses, engine_analyses)) {
        analyses_to_remove <- engine_analyses[!engine_analyses %in% analyses]
        if (length(analyses_to_remove) > 0) engine$remove_analyses(analyses_to_remove)
        reactive_analyses(engine$get_overview())
        reactive_history(engine$history)
      }
      
      replicates <- reactive_analyses()$replicate
      if (!identical(replicates, engine$get_overview()$replicate)) {
        tryCatch({
          engine$add_replicate_names(replicates)
          reactive_analyses(engine$get_overview())
          reactive_history(engine$history)
        }, warning = function(w) {
          shiny::showNotification(conditionMessage(w), duration = 10, type = "warning")
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), duration = 10, type = "error")
        })
      }
      
      blanks <- reactive_analyses()$blank
      if (!identical(blanks, engine$get_overview()$blank)) {
        tryCatch({
          engine$add_blank_names(blanks)
          reactive_analyses(engine$get_overview())
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
      engine_type <- reactive_engine_type()
      
      if (engine_type %in% "MassSpecEngine") {
        
        if (length(reactive_files()) == 0) {
          shiny::showNotification("No files loaded for MassSpecEngine", duration = 5, type = "warning")
          return(htmltools::div(" "))
        }
        
        .mod_MassSpecEngine_summary_Server("summary", engine, reactive_analyses, volumes)
        .mod_MassSpecEngine_summary_UI("summary", engine)
        
      } else if (engine_type %in% "RamanEngine") {
        
        if (length(reactive_files()) == 0) {
          shiny::showNotification("No files loaded for RamanEngine", duration = 5, type = "warning")
          return(htmltools::div(" "))
        }
        
        .mod_RamanEngine_summary_Server("summary", engine, reactive_analyses, volumes)
        .mod_RamanEngine_summary_UI("summary", engine)
        
      } else {
        shiny::showNotification(paste0("Explorer not implemented for ", engine_type), duration = 5, type = "warning")
        htmltools::div(" ")
      }
    })
    
    # _Workflow -----
    output$workflow_ui <- shiny::renderUI({
      
      if (reactive_engine_type() %in% "CoreEngine") {
        shiny::showNotification("Workflow not implemented for CoreEngine", duration = 5, type = "warning")
        return(htmltools::div(" "))
      }
      
      .mod_workflow_Server("workflow", engine, reactive_engine_type, reactive_workflow, reactive_warnings, reactive_history, volumes)
      .mod_workflow_UI("workflow")
    })
    
    # _History -----
    output$historyTable <- DT::renderDT({
      h_list <- reactive_history()
      h_dt <- data.table::rbindlist(h_list, fill = TRUE)
      h_dt$time <- format(h_dt$time, "%Y-%m-%d %H:%M:%S")
      DT::datatable(
        h_dt,
        filter = "top",
        selection = list(mode = 'single', selected = 1, target = 'row'),
        options = list(pageLength = 20)
      )
    })
    
    # _Select/Load Engine -----
    if (engine_type %in% "CoreEngine") {
      StreamFind_env <- as.environment("package:StreamFind")
      available_engines <- ls(envir = StreamFind_env, pattern = "Engine")
      available_engines <- available_engines[sapply(available_engines, function(x) "R6ClassGenerator" %in% is(get(x, envir = .GlobalEnv)))]
      available_engines <- available_engines[!available_engines %in% "CoreEngine"]
      
      engine_buttons <- lapply(available_engines, function(obj) shiny::actionButton(inputId = paste0("select_", obj), label = obj))
      shinyFiles::shinyFileChoose(input, "select_LoadEngine", roots = volumes, defaultRoot = "wd", session = session, filetypes = list(sqlite = "sqlite"))
      engine_buttons[[length(engine_buttons) + 1]] <- shinyFiles::shinyFilesButton("select_LoadEngine", "Load Engine (.sqlite)", "Load Engine from .sqlite file", multiple = FALSE)
      
      shiny::showModal(shiny::modalDialog(
        title = "Select an Engine",
        easyClose = TRUE,
        footer = NULL,
        do.call(tagList, engine_buttons)
      ))
      
      available_engines <- c(available_engines, "LoadEngine")
      
      lapply(available_engines, function(obj) {
        shiny::observeEvent(input[[paste0("select_", obj)]], {
          if ("select_LoadEngine" %in% paste0("select_", obj) ) {
            req(input$select_LoadEngine)
            
            
            fileinfo <- shinyFiles::parseFilePaths(volumes, input$select_LoadEngine)
            if (nrow(fileinfo) > 0) {
              file <- fileinfo$datapath
              engine_save_file <<- file
              
              db <- .openCacheDBScope(file = engine_save_file)
              engine <- DBI::dbListTables(db)
              
              if (length(engine) == 0) {
                msg <- paste("The file", engine_save_file, "is not a valid engine file!")
                shiny::showNotification(msg, duration = 10, type = "error")
                reactive_engine_save_file(NA_character_)
              }
              
              if (!engine %in% available_engines) {
                msg <- paste("The engine", engine, "is not valid!")
                shiny::showNotification(msg, duration = 10, type = "error")
                reactive_engine_save_file(NA_character_)
                return()
              }
              
              if (engine %in% available_engines) {
                reactive_engine_type(engine)
                reactive_engine_save_file(file)
                shiny::removeModal()
              }
            }
            
          } else {
            reactive_engine_type(obj)
            shiny::removeModal()
          }
        })
      })
    }
  }

  return(server)
}