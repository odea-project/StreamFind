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
  .use_initial_model <- function(reactive_app_mode, reactive_engine_type, reactive_engine_save_file, reactive_clean_start, reactive_show_init_modal, volumes) {
    
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
              reactive_show_init_modal(TRUE)
              return()
            }
            
            if (!engine_name %in% available_engines) {
              msg <- paste("The engine", engine_name, "is not valid!")
              shiny::showNotification(msg, duration = 10, type = "error")
              reactive_engine_save_file(NA_character_)
              shiny::removeModal()
              reactive_show_init_modal(TRUE)
              return()
            }
            
            if (engine_name %in% available_engines) {
              reactive_app_mode("workflow_assembler")
              reactive_engine_type(engine_name)
              reactive_engine_save_file(engine_save_file)
              reactive_clean_start(TRUE)
              reactive_show_init_modal(FALSE)
              shiny::removeModal()
              return()
            }
          }
          
        } else {
          reactive_engine_save_file(NA_character_)
          reactive_engine_type(obj)
          shiny::removeModal()
          if (!obj %in% "CoreEngine") {
            reactive_show_init_modal(FALSE)
            reactive_app_mode("workflow_assembler")
            reactive_clean_start(TRUE)
            reactive
          } else {
            reactive_show_init_modal(TRUE)
          }
          return()
        }
      })
    })
  }
  
  # _Global Reactive Variables -----
  reactive_app_mode <- shiny::reactiveVal(NA_character_)
  reactive_engine_type <- shiny::reactiveVal(NA_character_)
  reactive_engine_save_file <- shiny::reactiveVal(NA_character_)
  reactive_clean_start <- shiny::reactiveVal(TRUE)
  reactive_show_init_modal <- shiny::reactiveVal(FALSE)
  reactive_warnings <- shiny::reactiveVal(list())
  
  # _Setup App -----
  
  ## Engine is defined during call to run app
  init_engine_type <- golem::get_golem_options("engine_type")
  if (!is.null(init_engine_type)) {
    if (init_engine_type %in% .get_available_engines()) {
      reactive_engine_type(init_engine_type)
      if (!init_engine_type %in% "CoreEngine") reactive_app_mode("workflow_assembler")
    } else {
      init_engine_type <- "CoreEngine"
    }
  } else {
    init_engine_type <- "CoreEngine"
  }
  
  ## Engine save file is defined during call to run app
  init_engine_save_file <- golem::get_golem_options("file")
  if (!is.null(init_engine_save_file)) reactive_engine_save_file(init_engine_save_file)
  
  ## obs Engine Save File Validity -----
  shiny::observeEvent(reactive_engine_save_file(), {
    engine_save_file <- reactive_engine_save_file()
    if (!is.na(engine_save_file)) {
      if (!grepl(".sqlite$", engine_save_file)) {
        msg <- paste("The file", engine_save_file, "is not an sqlite file!")
        shiny::showNotification(msg, duration = 10, type = "error")
        reactive_engine_save_file(NA_character_)
      }
    }
  })
  
  ## out App Mode -----
  output$app_mode_ui <- shiny::renderUI({
    if (reactive_app_mode() %in% "workflow_assembler") {
      tags$span(reactive_engine_type())
    } else if (is.na(reactive_app_mode())) {
      tags$span("StreamFind")
    } else {
      tags$span(reactive_app_mode())
    }
  })
  
  ## out App Sidebar -----
  output$sidebar_ui <- shinydashboard::renderMenu({
    if (reactive_app_mode() %in% "workflow_assembler") {
      shinydashboard::sidebarMenu(
        shiny::actionButton("restart_app", "Restart", width = "90%"),
        shinydashboard::menuItem("Project", tabName = "workflow_assembler-project", icon = NULL, selected = TRUE),
        shinydashboard::menuItem("Analyses", tabName = "workflow_assembler-analyses", icon = NULL),
        shinydashboard::menuItem("Explorer", tabName = "workflow_assembler-explorer", icon = NULL),
        shinydashboard::menuItem("Workflow", tabName = "workflow_assembler-workflow", icon = NULL),
        shinydashboard::menuItem("Results", tabName = "workflow_assembler-results", icon = NULL)
      )
    } else {
      shinydashboard::sidebarMenu(shiny::actionButton("restart_app", "Restart", width = "90%"))
    }
  })
  
  ## out Warnings -----
  output$warningMenu <- shinydashboard::renderMenu({
    warnings <- reactive_warnings()
    msgs <- lapply(warnings, function(x) { shinydashboard::notificationItem(text = x) })
    shinydashboard::dropdownMenu(type = "notifications", .list = msgs)
  })
  
  ## out App Body -----
  output$body_ui <- shiny::renderUI({
    if (reactive_app_mode() %in% "workflow_assembler") {
      .mod_workflow_assembler_Server("workflow_assembler", reactive_clean_start, reactive_engine_type, reactive_engine_save_file, reactive_warnings)
      .mod_workflow_assembler_UI("workflow_assembler")
    } else {
      shinydashboard::tabItems()
    }
  })
  
  shiny::observe({
    if (reactive_app_mode() %in% "workflow_assembler") {
      shinydashboard::updateTabItems(session, "workflow_assembler-tabs", selected = "workflow_assembler-project")
    }
  })
  
  ## obs Show Init Modal -----
  shiny::observe({
    if (reactive_show_init_modal()) {
      reactive_show_init_modal(FALSE)
      .use_initial_model(reactive_app_mode, reactive_engine_type, reactive_engine_save_file, reactive_clean_start, reactive_show_init_modal, .get_volumes())
    }
  })
  
  ## obs Restart App -----
  shiny::observeEvent(input$restart_app, {
    time_var <- format(Sys.time(), "%Y%m%d%H%M%S")
    shiny::showModal(shiny::modalDialog(
      "Are you sure you want to restart StreamFind?",
      title = "Restart StreamFind",
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton("Cancel")),
      shiny::actionButton(paste0("confirm_restart_", time_var), "Confirm", class = "btn-danger")
    ))
    shiny::observeEvent(input[[paste0("confirm_restart_", time_var)]], {
      shiny::removeModal()
      reactive_show_init_modal(TRUE)
    })
  })
  
  # obs Show Init Modal -----
  shiny::observe({
    if (is.na(reactive_app_mode())) reactive_show_init_modal(TRUE)
  })
}
