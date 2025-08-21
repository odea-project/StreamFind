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

  # MARK: Global Reactive Variables
  # Global Reactive Variables -----
  reactive_app_mode <- shiny::reactiveVal(NA_character_)
  reactive_engine_type <- shiny::reactiveVal(NA_character_)
  reactive_engine_save_file <- shiny::reactiveVal(NA_character_)
  reactive_clean_start <- shiny::reactiveVal(TRUE)
  reactive_show_init_modal <- shiny::reactiveVal(FALSE)
  reactive_warnings <- shiny::reactiveVal(list())

  # MARK: Engine Type and Save File
  # Init Engine Type -----
  init_engine_type <- golem::get_golem_options("engine_type")
  if (!is.null(init_engine_type)) {
    if (init_engine_type %in% .get_available_engines()) {
      reactive_engine_type(init_engine_type)
      if (!init_engine_type %in% "Engine") {
        reactive_app_mode("WorkflowAssembler")
      }
    } else {
      init_engine_type <- "Engine"
    }
  } else {
    init_engine_type <- "Engine"
  }

  # MARK: Init Engine Save File
  # Init Engine Save File -----
  init_engine_save_file <- golem::get_golem_options("file")
  if (!is.null(init_engine_save_file)) {
    reactive_engine_save_file(init_engine_save_file)
  }

  # MARK: obs Engine Type and Save File
  # obs Engine Save File Format Validity -----
  shiny::observeEvent(reactive_engine_save_file(), {
    engine_save_file <- reactive_engine_save_file()
    if (!is.na(engine_save_file)) {
      if (!grepl(".sqlite$|.rds$", engine_save_file)) {
        msg <- paste(
          "The file",
          engine_save_file,
          "is not an sqlite or rds file!"
        )
        shiny::showNotification(msg, duration = 10, type = "error")
        reactive_engine_save_file(NA_character_)
      } else {
        if (file.exists(engine_save_file)) {
          file_format <- tools::file_ext(engine_save_file)
          if (file_format %in% "sqlite") {
            db <- .openCacheDBScope(file = engine_save_file)
            engine_name <- DBI::dbListTables(db)
            if (length(engine_name) == 0) engine_name <- NA_character_
          } else if (file_format %in% "rds") {
            data <- readRDS(engine_save_file)
            if (is.list(data)) {
              if ("type" %in% names(data)) {
                engine_name <- paste0(data$type, "Engine")
              } else {
                engine_name <- NA_character_
              }
            } else {
              engine_name <- NA_character_
            }
          } else {
            engine_name <- NA_character_
          }
          available_engines <- .get_available_engines()
          if (engine_name %in% available_engines) {
            reactive_app_mode("WorkflowAssembler")
            reactive_engine_type(engine_name)
            reactive_engine_save_file(engine_save_file)
            reactive_clean_start(TRUE)
            reactive_show_init_modal(FALSE)
          }
        } else {
          msg <- paste("The file", engine_save_file, "does not exist!")
          shiny::showNotification(msg, duration = 10, type = "error")
          reactive_engine_save_file(NA_character_)
          shiny::removeModal()
          reactive_show_init_modal(TRUE)
          return()
        }
      }
    }
  })


  # MARK: out App Mode UI
  # out App Mode UI -----
  output$app_mode_ui <- shiny::renderUI({
    if (reactive_app_mode() %in% "WorkflowAssembler") {
      shiny::tags$span(reactive_engine_type())
    } else if (is.na(reactive_app_mode())) {
      shiny::tags$span("StreamFind")
    } else {
      shiny::tags$span(reactive_app_mode())
    }
  })

  # MARK: out App Sidebar UI
  # out App Sidebar UI -----
  output$sidebar_ui <- shinydashboard::renderMenu({
    if (reactive_app_mode() %in% "WorkflowAssembler") {
      shinydashboard::sidebarMenu(
        shiny::actionButton("restart_app", "Restart", width = "90%"),
        shinydashboard::menuItem(
          "Project",
          tabName = "WorkflowAssembler-project",
          icon = NULL,
          selected = TRUE
        ),
        shinydashboard::menuItem(
          "Analyses",
          tabName = "WorkflowAssembler-analyses",
          icon = NULL
        ),
        shinydashboard::menuItem(
          "Explorer",
          tabName = "WorkflowAssembler-explorer",
          icon = NULL
        ),
        shinydashboard::menuItem(
          "Workflow",
          tabName = "WorkflowAssembler-workflow",
          icon = NULL
        ),
        shinydashboard::menuItem(
          "Results",
          tabName = "WorkflowAssembler-results",
          icon = NULL
        ),
        shinydashboard::menuItem(
          "Audit Trail",
          tabName = "WorkflowAssembler-audit",
          icon = NULL
        ),
        shinydashboard::menuItem(
          "Configuration",
          tabName = "WorkflowAssembler-config",
          icon = NULL
        ),
        shiny::div(
          style = "position: absolute; bottom: 10px; left: 10px; right: 10px;",
          shiny::div(
            style = "color: #b8c7ce; font-size: 12px; margin-bottom: 8px;",
            shiny::strong("CACHE INFORMATION")
          ),
          shiny::div(
            style = "color: #b8c7ce; font-size: 11px; margin-bottom: 8px;",
            "Cache Size: ",
            shiny::textOutput("WorkflowAssembler-cache_size", inline = TRUE)
          ),
          shiny::actionButton(
            "WorkflowAssembler-clear_cache_button",
            "Clear Cache",
            icon = shiny::icon("trash"),
            style = "width: 90%; background-color: #3c8dbc; border-color: #367fa9; color: white;"
          )
        )
      )
    } else {
      shinydashboard::sidebarMenu(shiny::actionButton(
        "restart_app",
        "Restart",
        width = "90%"
      ))
    }
  })

  # MARK: out Warnings
  # out Warnings -----
  output$warningMenu <- shinydashboard::renderMenu({
    warnings <- reactive_warnings()
    msgs <- lapply(warnings, function(x) {
      shinydashboard::notificationItem(text = x)
    })
    shinydashboard::dropdownMenu(type = "notifications", .list = msgs)
  })

  # MARK: out App Body UI
  # out App Body UI -----
  output$body_ui <- shiny::renderUI({
    if (reactive_app_mode() %in% "WorkflowAssembler") {
      .mod_WorkflowAssembler_Server(
        "WorkflowAssembler",
        reactive_clean_start,
        reactive_engine_type,
        reactive_engine_save_file,
        reactive_warnings
      )
      .mod_WorkflowAssembler_UI("WorkflowAssembler")
    } else {
      shinydashboard::tabItems()
    }
  })

  # MARK: obs App Mode
  # obs App Mode -----
  shiny::observe({
    if (reactive_app_mode() %in% "WorkflowAssembler") {
      shinydashboard::updateTabItems(
        session,
        "WorkflowAssembler-tabs",
        selected = "WorkflowAssembler-project"
      )
    }
  })

  # MARK: obs Show Init Modal
  # obs Show Init Modal -----
  shiny::observe({
    if (reactive_show_init_modal()) {
      reactive_show_init_modal(FALSE)
      .app_util_use_initial_model(
        reactive_app_mode,
        reactive_engine_type,
        reactive_engine_save_file,
        reactive_clean_start,
        reactive_show_init_modal,
        .app_util_get_volumes(),
        input,
        output,
        session
      )
    }
  })

  # MARK: obs Restart App
  # obs Restart App -----
  shiny::observeEvent(input$restart_app, {
    time_var <- format(Sys.time(), "%Y%m%d%H%M%S")
    shiny::showModal(shiny::modalDialog(
      "Are you sure you want to restart StreamFind?",
      title = "Restart StreamFind",
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton("Cancel")),
      shiny::actionButton(
        paste0("confirm_restart_", time_var),
        "Confirm",
        class = "btn-danger"
      )
    ))
    shiny::observeEvent(input[[paste0("confirm_restart_", time_var)]], {
      shiny::removeModal()
      reactive_show_init_modal(TRUE)
    })
  })

  # MARK: obs Show Init Modal
  # obs Show Init Modal -----
  shiny::observe({
    if (is.na(reactive_app_mode())) reactive_show_init_modal(TRUE)
  })
}
