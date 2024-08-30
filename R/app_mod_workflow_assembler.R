#' @title .mod_workflow_assembler_UI
#' 
#' @description Template for Shiny module UI.
#' 
#' @noRd
#'
.mod_workflow_assembler_UI <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabItems(
    
    shinydashboard::tabItem(tabName = ns("project"),
      shiny::fluidRow(
        shiny::uiOutput(ns("wdir")),
        shiny::column(12, shiny::uiOutput(ns("save_engine")), shiny::uiOutput(ns("reset_engine"))),
        shiny::uiOutput(ns("engine_save_file_ui")),
        shiny::fluidRow(shiny::uiOutput(ns("headers_ui")))
      )
    ),
    
    shinydashboard::tabItem(tabName = ns("analyses"), shiny::fluidRow(shiny::uiOutput(ns("analyses_ui")))),
    
    shinydashboard::tabItem(tabName = ns("explorer"), shiny::fluidRow(shiny::uiOutput(ns("explorer_ui")))),
    
    shinydashboard::tabItem(tabName = ns("workflow"), shiny::fluidRow(shiny::uiOutput(ns("workflow_ui")))),
    
    shinydashboard::tabItem(tabName = ns("results"), shiny::fluidRow())
  )
}

#' @title .mod_workflow_assembler_Server
#' 
#' @description Template for Shiny module server.
#' 
#' @noRd
#'
.mod_workflow_assembler_Server <- function(id, reactive_clean_start, reactive_engine_type, reactive_engine_save_file, reactive_warnings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
    
    .get_valid_file_types <- function(engine_type) {
      if (engine_type %in% "MassSpecEngine") {
        c("mzML", "mzXML")
      } else if (engine_type %in% "RamanEngine") {
        c("asc")
      } else {
        c("txt", "csv")
      }
    }
    
    # _Global Constants/Mutable -----
    pkg_resources <- system.file(package = "StreamFind", dir = "extdata")
    mandatory_header_names <- c("name", "author", "file", "date")
    volumes <- .get_volumes()
    engine <- NULL
    
    # _Global Reactive Variables -----
    reactive_wdir <- shiny::reactiveVal(getwd())
    reactive_volumes <- shiny::reactiveVal(volumes)
    
    reactive_headers <- shiny::reactiveVal(NULL)
    reactive_analyses <- shiny::reactiveVal(NULL)
    reactive_workflow <- shiny::reactiveVal(NULL)
    
    reactive_saved_headers <- shiny::reactiveVal(NULL)
    reactive_saved_analyses <- shiny::reactiveVal(NULL)
    reactive_saved_workflow <- shiny::reactiveVal(NULL)
    
    ## obs Engine Save File -----
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
    
    ## obs Clean Start -----
    shiny::observeEvent(reactive_clean_start(), {
      if (reactive_clean_start()) {
        engine_type <- reactive_engine_type()
        engine_call <- get(engine_type, envir = .GlobalEnv)
        engine_call_new <- engine_call[["new"]]
        engine <<- suppressMessages(do.call(engine_call_new, list()))
        
        if (!is.na(reactive_engine_save_file())) {
          tryCatch({
            engine$load(reactive_engine_save_file())
          }, warning = function(w) {
            msg <- paste("Warning for", reactive_engine_save_file(), ":", conditionMessage(w))
            reactive_warnings(.add_notifications(reactive_warnings(), "load_engine", msg))
          }, error = function(e) {
            msg <- paste("Error for", reactive_engine_save_file(), ":", conditionMessage(e))
            reactive_warnings(.add_notifications(reactive_warnings(), "load_engine", msg))
          })
        }
        
        reactive_headers(engine$headers)
        reactive_analyses(engine$analyses)
        reactive_workflow(engine$workflow)
        reactive_saved_headers(engine$headers)
        reactive_saved_analyses(engine$analyses)
        reactive_saved_workflow(engine$workflow)
        reactive_clean_start(FALSE)
      }
    })
    
    # _Warnings -----
    
    ## obs Unsaved engine -----
    shiny::observe({
      has_unsaved_changes <- "unsaved_changes" %in% names(reactive_warnings())
      equal_history <- all(
        identical(reactive_headers(), reactive_saved_headers()),
        identical(reactive_analyses(), reactive_saved_analyses()),
        identical(reactive_workflow(), reactive_saved_workflow())
      )
      if (!equal_history && !has_unsaved_changes) {
        reactive_warnings(.add_notifications(reactive_warnings(), "unsaved_changes", "Unsaved changes in the engine!"))
      }
      if (equal_history) {
        reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      }
    })
    
    ## out Save engine -----
    output$save_engine <- shiny::renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        shinyFiles::shinyFileSave(input, "save_engine_button_file", roots = reactive_volumes(), defaultRoot = "wd", session = session)
        filename <- reactive_engine_save_file()
        if (is.na(filename)) filename <- ""
        htmltools::div(style = "margin-bottom: 20px;",
          shinyFiles::shinySaveButton(
            ns("save_engine_button_file"),
            label = "Save Engine",
            title = "Save the engine as .sqlite",
            class = "btn-success",
            filename = gsub(".sqlite", "", basename(filename)),
            filetype = list(sqlite = "sqlite"), style = "width: 200px;")
        )
      }
    })
    
    ## event Save -----
    shiny::observeEvent(input$save_engine_button, {
      engine$save(reactive_engine_save_file())
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$headers)
      reactive_analyses(engine$analyses)
      reactive_workflow(engine$workflow)
      reactive_saved_headers(engine$headers)
      reactive_saved_analyses(engine$analyses)
      reactive_saved_workflow(engine$workflow)
    })
    
    ## event Save Engine File -----
    shiny::observeEvent(input$save_engine_button_file, {
      shiny::req(input$save_engine_button_file)
      file_info <- shinyFiles::parseSavePath(roots = reactive_volumes(), input$save_engine_button_file)
      if (nrow(file_info) > 0) {
        file_path <- file_info$datapath
        engine$headers <- reactive_headers()
        engine$analyses <- reactive_analyses()
        engine$workflow <- reactive_workflow()
        engine$save(file_path)
        reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
        reactive_headers(engine$headers)
        reactive_analyses(engine$analyses)
        reactive_workflow(engine$workflow)
        reactive_engine_save_file(engine$file$path)
        reactive_saved_headers(engine$headers)
        reactive_saved_analyses(engine$analyses)
        reactive_saved_workflow(engine$workflow)
      }
    })
    
    ## out Reset engine -----
    output$reset_engine <- shiny::renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shiny::actionButton(ns("reset_engine_button"), label = "Discard Changes", width = 200, class = "btn-danger")
        )
      }
    })
    
    ## event Reset -----
    shiny::observeEvent(input$reset_engine_button, {
      if (is.na(reactive_engine_save_file())) {
        reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
        reactive_headers(engine$headers)
        reactive_analyses(engine$analyses)
        reactive_workflow(engine$workflow)
        reactive_saved_headers(engine$headers)
        reactive_saved_analyses(engine$analyses)
        reactive_saved_workflow(engine$workflow)
      } else {
        engine$load(reactive_engine_save_file())
        reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
        reactive_headers(engine$headers)
        reactive_analyses(engine$analyses)
        reactive_workflow(engine$workflow)
        reactive_saved_headers(engine$headers)
        reactive_saved_analyses(engine$analyses)
        reactive_saved_workflow(engine$workflow)
      }
    })
    
    # _Project -----
    
    ## out Working Directory -----
    output$wdir <- shiny::renderUI({
      shinyFiles::shinyDirChoose(input, "set_wdir_button", roots = reactive_volumes(), defaultRoot = "wd", session = session)
      shinydashboard::box(width = 12, title = "Working Directory", solidHeader = TRUE,
        shinyFiles::shinyDirButton(ns("set_wdir_button"), "Change Working Directory", "Select Working Directory", "wd", style = "width: 200px;"),
        htmltools::HTML(paste("  ", reactive_wdir()))
      )
    })
    
    ## event Change Working Directory -----
    shiny::observeEvent(input$set_wdir_button, {
      shiny::req(input$set_wdir_button)
      file_info <- shinyFiles::parseDirPath(roots = reactive_volumes(), input$set_wdir_button)
      if (length(file_info) > 0) {
        setwd(file_info)
        reactive_wdir(file_info)
        volumes <<- .get_volumes()
        reactive_volumes(volumes)
      }
    })
    
    ## out Engine Save File -----
    output$engine_save_file_ui <- shiny::renderUI({
      if (!is.na(reactive_engine_save_file())) {
        shinydashboard::box(width = 12, title =  "Engine Save File", solidHeader = TRUE,
          htmltools::div(style = "margin-bottom: 20px;", shiny::p(reactive_engine_save_file()))
        )
      }
    })
    
    ## module Headers -----
    output$headers_ui <- shiny::renderUI({
      .mod_headers_Server("headers", ns, reactive_headers)
      .mod_headers_UI("headers", ns)
    })
    
    ## _Analyses -----
    output$analyses_ui <- shiny::renderUI({
      if (reactive_engine_type() %in% "CoreEngine") {
        shiny::showNotification("Analyses not implemented for CoreEngine", duration = 5, type = "warning")
        return(htmltools::div(" "))
      }
      .mod_analyses_Server("analyses", ns, reactive_analyses, reactive_warnings, reactive_volumes)
      .mod_analyses_UI("analyses", ns)
    })
    
    # _Explorer -----
    output$explorer_ui <- shiny::renderUI({
      engine_type <- reactive_engine_type()

      if (engine_type %in% "MassSpecEngine") {

        if (length(reactive_analyses()) == 0) {
          shiny::showNotification("No analyses loaded for MassSpecEngine", duration = 5, type = "warning")
          return(htmltools::div(" "))
        }

        .mod_MassSpecEngine_summary_Server("summary", ns, engine, reactive_analyses, reactive_volumes)
        .mod_MassSpecEngine_summary_UI("summary", ns)

      } else if (engine_type %in% "RamanEngine") {

        if (length(reactive_analyses()) == 0) {
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
    # output$workflow_ui <- shiny::renderUI({
    #   
    #   if (reactive_engine_type() %in% "CoreEngine") {
    #     shiny::showNotification("Workflow not implemented for CoreEngine", duration = 5, type = "warning")
    #     return(htmltools::div(" "))
    #   }
    #   
    #   .mod_workflow_Server("workflow", engine, reactive_engine_type, reactive_workflow, reactive_warnings, volumes)
    #   .mod_workflow_UI("workflow")
    # })
    
    # _History -----
    # output$historyTable <- DT::renderDT({
    #   h_list <- reactive_history()
    #   h_dt <- data.table::rbindlist(h_list, fill = TRUE)
    #   h_dt$time <- format(h_dt$time, "%Y-%m-%d %H:%M:%S")
    #   DT::datatable(
    #     h_dt,
    #     filter = "top",
    #     selection = list(mode = 'single', selected = 1, target = 'row'),
    #     options = list(pageLength = 20)
    #   )
    # })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  })
}