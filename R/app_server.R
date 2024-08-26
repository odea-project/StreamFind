#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
app_server <- function(input, output, session) {
  
  if (!requireNamespace("StreamFind", quietly = TRUE)) {
    warning("StreamFind package not installed!")
    return(invisible(self))
  }
  
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
  
  library(StreamFind)
  library(shiny)
  library(htmltools)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyFiles)
  library(sortable)
  library(data.table)
  
  # _Utility Functions -----
  .use_initial_model <- function(reactive_engine_type, reactive_engine_save_file, reactive_clean_start, volumes) {
    
    available_engines <- .get_available_engines()
    
    time_var <- format(Sys.time(), "%Y%m%d%H%M%S")
    
    model_elements <- list()
    
    model_elements[[1]] <- shiny::img(src = "www/logo_StreamFind.png", width = 250, style = "display: block; margin-left: auto; margin-right: auto;")
    
    model_elements[[2]] <- shiny::fluidRow(shiny::p("Select an engine to start a new project: ", style = "text-align: center;margin-top: 40px;"))
    
    model_elements[[3]] <- htmltools::div(lapply(available_engines, function(obj) shiny::actionButton(inputId = paste0(time_var, "_select_", obj), label = obj)), style = "text-align: center;")
    
    model_elements[[4]] <- shiny::fluidRow(shiny::p("Load an existing engine: ", style = "text-align: center;margin-top: 40px;"))
    
    shinyFiles::shinyFileChoose(input, paste0(time_var, "_select_LoadEngine"), roots = volumes, defaultRoot = "wd", session = session, filetypes = list(sqlite = "sqlite"))
    
    model_elements[[5]] <- htmltools::div(shinyFiles::shinyFilesButton(paste0(time_var, "_select_LoadEngine"), "Load Engine (.sqlite)", "Load Engine from .sqlite file", multiple = FALSE), style = "text-align: center;")
    
    shiny::showModal(shiny::modalDialog(
      title = " ",
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton("Cancel")),
      do.call(tagList, model_elements)
    ))
    
    available_engines <- c(available_engines, "LoadEngine")
    
    lapply(available_engines, function(obj) {
      
      shiny::observeEvent(input[[paste0(time_var, "_select_", obj)]], {
        
        if (paste0(time_var, "_select_LoadEngine") %in% paste0(time_var, "_select_", obj) ) {
          input_name <- paste0(time_var, "_select_LoadEngine")
          req(input[[input_name]])
          fileinfo <- shinyFiles::parseFilePaths(volumes, input[[input_name]])
          if (nrow(fileinfo) > 0) {
            engine_save_file <- fileinfo$datapath
            
            db <- .openCacheDBScope(file = engine_save_file)
            engine_name <- DBI::dbListTables(db)
            
            if (length(engine_name) == 0) {
              msg <- paste("The file", engine_save_file, "is not a valid engine file!")
              shiny::showNotification(msg, duration = 10, type = "error")
              reactive_engine_save_file(NA_character_)
              shiny::removeModal()
              return()
            }
            
            if (!engine_name %in% available_engines) {
              msg <- paste("The engine", engine_name, "is not valid!")
              shiny::showNotification(msg, duration = 10, type = "error")
              reactive_engine_save_file(NA_character_)
              shiny::removeModal()
              return()
            }
            
            if (engine_name %in% available_engines) {
              reactive_engine_type(engine_name)
              reactive_engine_save_file(engine_save_file)
              reactive_clean_start(TRUE)
              shiny::removeModal()
              return()
            }
          }
          
        } else {
          reactive_engine_save_file(NA_character_)
          reactive_engine_type(obj)
          reactive_clean_start(TRUE)
          shiny::removeModal()
          return()
        }
      })
    })
  }
  
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
  pkg_resources <- system.file(package = "StreamFind", dir = "extdata")
  mandatory_header_names <- c("name", "author", "file", "date")
  volumes <- .get_volumes()
  engine <- NULL
  available_engines <- .get_available_engines()
  
  # _Reactive Variables -----
  reactive_wdir <- shiny::reactiveVal(getwd())
  reactive_volumes <- shiny::reactiveVal(volumes)
  reactive_clean_start <- shiny::reactiveVal(TRUE)
  reactive_engine_type <- shiny::reactiveVal("CoreEngine")
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
  init_engine_type <- golem::get_golem_options("engine_type")
  if (!is.null(init_engine_type)) {
    if (init_engine_type %in% available_engines) {
      reactive_engine_type(init_engine_type)
    } else {
      init_engine_type <- "CoreEngine"
    }
  } else {
    init_engine_type <- "CoreEngine"
  }
  
  init_engine_save_file <- golem::get_golem_options("file")
  if (!is.null(init_engine_save_file)) reactive_engine_save_file(init_engine_save_file)
  
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
    reactive_clean_start(TRUE)
  })
  
  ## obs Clean Start -----
  observeEvent(reactive_clean_start(), {
    if (reactive_clean_start()) {
      engine_type <- reactive_engine_type()
      engine_call <- get(engine_type, envir = .GlobalEnv)
      engine_call_new <- engine_call[["new"]]
      engine <<- suppressMessages(do.call(engine_call_new, list()))
      if (!is.na(reactive_engine_save_file())) engine$load(reactive_engine_save_file())
      
      browser()
      
      reactive_headers(engine$headers)
      reactive_analyses(engine$analyses)
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
      reactive_workflow(engine$workflow)
      reactive_file_types(.get_valid_file_types(engine_type))
      reactive_clean_start(FALSE)
    }
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
  
  ## out Restart app -----
  output$restart_app <- shiny::renderUI({
    htmltools::div(style = "margin-bottom: 20px;",
      shiny::actionButton("restart_app_bottom", label = "Select/Load Engine", width = 200, class = "btn-dark")
    )
  })
  
  ## event Restart app -----
  shiny::observeEvent(input$restart_app_bottom, {
    .use_initial_model(reactive_engine_type, reactive_engine_save_file, reactive_clean_start)
  })
  
  ## out Save engine -----
  output$save_engine <- shiny::renderUI({
    
    shinyFiles::shinyFileSave(input, "save_engine_button_file", roots = reactive_volumes(), defaultRoot = "wd", session = session)
    
    if ("unsaved_changes" %in% names(reactive_warnings()) && !is.na(reactive_engine_save_file())) {
      htmltools::div(style = "margin-bottom: 20px;",
        shiny::actionButton("save_engine_button", label = "Save Engine", width = 200, class = "btn-danger")
      )
    } else if ("unsaved_changes" %in% names(reactive_warnings()) && is.na(reactive_engine_save_file())) {
      htmltools::div(style = "margin-bottom: 20px",
        shinyFiles::shinySaveButton("save_engine_button_file", label = "Save Engine", title = "Save the engine as .sqlite", class = "btn-success", filetype = list(sqlite = "sqlite"), style = "width: 200px;")
      )
    } else {
      htmltools::div(style = "margin-bottom: 20px",
        shinyFiles::shinySaveButton("save_engine_button_file", label = "Save Engine", title = "Save the engine as .sqlite", class = "btn-success", filetype = list(sqlite = "sqlite"), style = "width: 200px;")
      )
    }
  })
  
  ## event Save -----
  shiny::observeEvent(input$save_engine_button, {
    engine$save(reactive_engine_save_file())
    reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
    reactive_headers(engine$headers)
    reactive_analyses(engine$analyses)
    reactive_history(engine$history)
    reactive_saved_history(engine$history)
    reactive_workflow(engine$workflow)
  })
  
  ## event Save Engine File -----
  shiny::observeEvent(input$save_engine_button_file, {
    req(input$save_engine_button_file)
    file_info <- shinyFiles::parseSavePath(roots = reactive_volumes(), input$save_engine_button_file)
    if (nrow(file_info) > 0) {
      file_path <- file_info$datapath
      engine$save(file_path)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$headers)
      reactive_analyses(engine$analyses)
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
    if (is.na(reactive_engine_save_file())) {
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$headers)
      reactive_analyses(engine$analyses)
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
      reactive_workflow(engine$workflow)
    } else {
      engine$load(reactive_engine_save_file())
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$headers)
      reactive_analyses(engine$analyses)
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
      reactive_workflow(engine$settings)
    }
  })

  # _Project -----
  
  ## Working Directory -----
  output$wdir <- shiny::renderUI({ 
    shinyFiles::shinyDirChoose(input, "set_wdir_button", roots = reactive_volumes(), defaultRoot = "wd", session = session)
    shinydashboard::box(width = 12, title = "Working Directory", solidHeader = TRUE,
      shinyFiles::shinyDirButton("set_wdir_button", "Change Working Directory", "Select Working Directory", "wd", style = "width: 200px;"),
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
  if (init_engine_type %in% "CoreEngine") .use_initial_model(reactive_engine_type, reactive_engine_save_file, reactive_clean_start)
}
