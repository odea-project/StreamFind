#' @title .mod_workflow_UI
#' 
#' @description Shiny module UI for workflow tab.
#' 
#' @noRd
#' 
.mod_workflow_UI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("workflow_settings"))),
      shiny::column(6, shiny::uiOutput(ns("selected_settings_details")))
    ),
    # CSS
    htmltools::tags$style(htmltools::HTML("
      .custom-button {
        background-color: #3498DB;
        color: white;
        border: none;
        padding: 5px 10px;
        margin: 5px;
        cursor: pointer;
      }
      .custom-buttonred {
        background-color: #F1948A;
        color: white;
        border: none;
        padding: 5px 10px;
        margin: 5px;
        cursor: pointer;
        padding: 2px 6px;
        font-size: 12px;
      }
      .custom-button:hover {
        background-color: #5DADE2;
      }
      .custom-buttonred:hover {
        background-color: #F5B7B1;
      }
      .workflow-item {
        display: flex;
        align-items: center;
        margin-bottom: 10px;
      }
      .workflow-item span {
        flex-grow: 1;
      }
      .workflow-item button {
        margin-left: auto;
      }
      .workflow-box {
        background-color: white;
        border-radius: 4px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .function-details {
        background-color: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 5px;
        padding: 20px;
      }
      .function-details dt {
        font-weight: bold;
        float: left;
        clear: left;
        width: 120px;
      }
      .function-details dd {
        margin-left: 130px;
      }
      .parameters-section {
        margin-top: 20px;
        border-top: 1px solid #e9ecef;
        padding-top: 20px;
      }
    ")) ##f9f9f9 border: 1px solid #ddd;
  )
}

#' @title .mod_workflow_Server
#' 
#' @description Shiny module server for workflow tab.
#' 
#' @noRd
#'
.mod_workflow_Server <- function(id, engine, reactive_engine_type, reactive_workflow, reactive_warnings, reactive_history, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    .app_util_add_notifications <- function(warnings, name_msg, msg) {
      shiny::showNotification(msg, duration = 5, type = "warning")
      warnings[[name_msg]] <- msg
      return(warnings)
    }
    
    engine_type <- reactive_engine_type()
    
    # list of settings and quantity possible in engine
    available_processing_methods <- engine$processing_methods()
    
    # get all settings functions for engine in the package
    StreamFind_env <- as.environment("package:StreamFind")
    engine_data_type <- gsub("Engine", "", is(engine))
    engine_settings_key <- paste0(engine_data_type, "Settings_")
    Settings_functions <- ls(envir = StreamFind_env, pattern = paste0("^", engine_settings_key))
    
    if (length(Settings_functions) == 0) {
      shiny::showNotification("No settings functions found for ", engine_type, " engine!", duration = 5, type = "warning")
      Settings_functions_short <- NULL
    } else {
      Settings_functions <- Settings_functions[sapply(Settings_functions, function(x) is.function(get(x, envir = .GlobalEnv)))]
      Settings_functions <- Settings_functions[sapply(Settings_functions, function(x) any(sapply(available_processing_methods$name, function(y) grepl(y, x))))]
      Settings_functions_short <- gsub(engine_settings_key, "", Settings_functions)
      names(Settings_functions) <- Settings_functions_short
    }
    
    reactive_workflow_local <- shiny::reactiveVal(list())
    
    init_workflow <- reactive_workflow()
    reactive_workflow_local(init_workflow)
    
    shiny::observe({
      rw <- reactive_workflow_local()
      if (length(rw) > 0) {
        rw_idx <- seq_along(rw)
        rw_names <- vapply(rw, function(x) paste0(x$call, "_", x$algorithm, " - "), NA_character_)
        rw_names <- paste0(rw_names, rw_idx)
        names(rw) <- rw_names
        reactive_workflow_local(rw)
      }
    })
    
    # conditional render for save workflow button
    output$save_workflow_ui <- shiny::renderUI({
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (!identical(init_rw, rw)) {
        if (!"workflow_not_updated" %in% names(reactive_warnings())) {
          msg <- "Workflow not updated in engine"
          reactive_warnings(.app_util_add_notifications(reactive_warnings(), "workflow_not_updated", msg))
        }
        shiny::actionButton(ns("save_workflow"), "Save Workflow", class = "btn-danger")
      }
    })
    
    # conditional render for discard workflow button
    output$discard_workflow_ui <- shiny::renderUI({
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (!identical(init_rw, rw)) shiny::actionButton(ns("discard_workflow"), "Discard Workflow", class = "btn-danger")
    })
    
    # conditional render for run workflow button
    output$run_workflow_ui <- shiny::renderUI({
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (identical(init_rw, rw) && length(rw) > 0) {
        shiny::actionButton(ns("run_workflow"), "Run Workflow", class = "btn-success")
      }
    })
    
    reactive_selected_settings <- shiny::reactiveVal(NULL)
    
    output$workflow_settings <- shiny::renderUI({
      
      rw <- reactive_workflow_local()
      
      #list of custom HTML for each function
      labels <- lapply(names(rw), function(i) {
        htmltools::tagList(
          htmltools::div(class = "workflow-item",
            shiny::actionButton(ns(paste0("workflow_del_", i)), "X", class = "custom-buttonred", style = "margin-right: 10px;"),
            shiny::span(i),
            shiny::actionButton(ns(paste0("workflow_edit_", i)), "Details", class = "custom-button")
          )
        )
      })
      
      lapply(names(rw), function(i) {
        shiny::observeEvent(input[[paste0("workflow_edit_", i)]], {
          reactive_selected_settings(i)
        }, ignoreInit = TRUE)
      })
      
      lapply(names(rw), function(i) {
        shiny::observeEvent(input[[paste0("workflow_del_", i)]], {
          rw <- reactive_workflow_local()
          rw <- rw[!names(rw) %in% i]
          reactive_workflow_local(rw)
        }, ignoreInit = TRUE)
      })
      
      shinydashboard::box(width = 12, title = NULL, solidHeader = TRUE, class = "workflow-box",
        
        shiny::column(12,
          htmltools::div(style = "display: flex; align-items: center; margin-bottom: 20px;",
            htmltools::div(style = "margin-right: 10px;", shinyFiles::shinyFilesButton(ns("load_workflow"), "Load Workflow", "Select a JSON or RDS file with workflow processing settings", multiple = FALSE)),
            htmltools::div(style = "margin-right: 10px;", shiny::actionButton(ns("clear_workflow"), "Clear Workflow")),
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns("save_workflow_ui"))),
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns("discard_workflow_ui"))),
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns("run_workflow_ui"))),
            htmltools::div(style =  "margin-bottom: 20px;")
          )
        ),
        
        shiny::column(12, htmltools::p(" ")),
        shiny::column(12, htmltools::p("Select Processing Settings")),
        shiny::column(9, shiny::selectInput(ns("settings_selector"), label = NULL, choices = Settings_functions_short, multiple = FALSE)),
        shiny::column(3, shiny::actionButton(ns("add_workflow_step"), "Add Workflow Step")),
        
        shiny::column(12, htmltools::h3("Workflow")),
        shiny::column(12,
          sortable::rank_list(
            text = "Drag to order",
            labels = labels,
            input_id = ns("rank_workflow_names")
          )
        )
      )
    })
    
    # observer for workflow order
    shiny::observeEvent(input$rank_workflow_names, {
      rw <- reactive_workflow_local()
      new_order <- input$rank_workflow_names
      new_order <- vapply(new_order, function(x) strsplit(x, "\n")[[1]][2], NA_character_)
      rw <- rw[new_order]
      reactive_workflow_local(rw)
    })
    
    # observer for adding settings bottom
    shiny::observeEvent(input$add_workflow_step, {
      rw <- reactive_workflow_local()
      settings_name <- input$settings_selector
      if (length(settings_name) > 0) {
        settings <- do.call(Settings_functions[settings_name], list())
        if (length(rw) > 0) {
          max <- available_processing_methods$max[available_processing_methods$name == settings$call]
          calls <- vapply(rw, function(x) x$call, NA_character_)
          if (sum(calls == settings$call) >= max) {
            shiny::showNotification("Maximum number of settings for ", settings$call, " reached!", duration = 5, type = "warning")
            return()
          }
        }
        rw <- c(rw, list(settings))
        reactive_workflow_local(rw)
      }
    })
    
    shinyFiles::shinyFileChoose(input, "load_workflow", roots = volumes, defaultRoot = "wd", session = session, filetypes = c("json", "rds"))
    
    reactive_workflow_file <- shiny::reactiveVal(NULL)
    
    # observer for loading workflow
    shiny::observeEvent(input$load_workflow, {
      settings <- NULL
      fileinfo <- shinyFiles::parseFilePaths(volumes, input$load_workflow)
      if (nrow(fileinfo) > 0) {
        file <- fileinfo$datapath
        if (length(file) == 1) {
          if (file.exists(file)) {
            if (tools::file_ext(file) %in% "json") settings <- jsonlite::fromJSON(file)
            if (tools::file_ext(file) %in% "rds") settings <- readRDS(file)
          } else {
            shiny::showNotification("File does not exist!", duration = 5, type = "warning")
          }
        }
      }
      
      if (is.list(settings)) {
        cols_check <- c("call", "algorithm", "parameters")
        if (all(cols_check %in% names(settings))) settings <- list(settings)
        settings <- lapply(settings, as.ProcessingSettings)
        names(settings) <- NULL
        all_ps <- vapply(settings, function(x) class(x)[1], "")
        if (all(all_ps %in% "ProcessingSettings")) {
          possible_methods <- engine$processing_methods()
          if (nrow(possible_methods) == 0) {
            shiny::showNotification("No processing methods available in engine!", duration = 5, type = "warning")
            return()
          }
          only_one_possible <- possible_methods$name[possible_methods$max == 1]
          call_names <- vapply(settings, function(x) x$call, NA_character_)
          duplicated_names <- call_names[duplicated(call_names)]
          if (any(duplicated_names %in% only_one_possible)) {
            if (length(duplicated_names) == 1) {
              shiny::showNotification("\U2139 ", duplicated_names, " is duplicate and only one is possible!", duration = 5, type = "warning")
            } else {
              shiny::showNotification(
                paste0("\U2139 Duplicate settings for the following methods and only one is possible! Not added.\n",
                paste(only_one_possible[only_one_possible %in% duplicated_names], collapse = "\n")),
                duration = 5, type = "warning"
              )
            }
            return()
          }
          
          if (!identical(settings, unname(reactive_workflow_local()))) reactive_workflow_local(settings)
          
        } else {
          not_conform <- which(!all_ps %in% "ProcessingSettings")
          shiny::showNotification("Settings (number/s: ", paste(not_conform, collapse = "; "), ") content or structure not conform! Not added.", duration = 5, type = "warning")
        }
      }
    })
    
    # observer for clear workflow
    shiny::observeEvent(input$clear_workflow, {
      reactive_workflow_local(list())
    })
    
    # observer for saving workflow
    shiny::observeEvent(input$save_workflow, {
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (!identical(init_rw, rw)) {
        engine$remove_settings()
        engine$add_settings(unname(reactive_workflow_local()))
        reactive_workflow(engine$settings)
        reactive_history(engine$history)
        warnings <- reactive_warnings()
        warnings[["workflow_not_updated"]] <- NULL
        reactive_warnings(warnings)
      }
    })
    
    # observer for discarding workflow
    shiny::observeEvent(input$discard_workflow, {
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (!identical(init_rw, rw)) {
        reactive_workflow_local(init_rw)
        warnings <- reactive_warnings()
        warnings[["workflow_not_updated"]] <- NULL
        reactive_warnings(warnings)
      }
    })
    
    # render settings details
    output$selected_settings_details <- shiny::renderUI({
      shiny::req(reactive_selected_settings())
      function_name <- reactive_selected_settings()
      if (function_name %in% names(reactive_workflow_local())) {
        htmltools::tagList(
          shiny::h3(paste("Details of", function_name), style = "color: #3498DB; margin-bottom: 20px;"),
          shiny::uiOutput(ns("function_code"))
        )
      }
    })

    # function settings details
    output$function_code <- shiny::renderUI({
      shiny::req(reactive_selected_settings())
      rw <- reactive_workflow_local()
      function_name <- reactive_selected_settings()
      settings <- rw[[function_name]]
      
      create_parameter_ui <- function(param_name, param_value) {
        value_display <- if (is.atomic(param_value) && length(param_value) == 1) {
          as.character(param_value)
        } else if (is.list(param_value)) {
          shiny::tags$ul(
            style = "list-style-type: none; padding-left: 0;",
            lapply(names(param_value), function(sub_param) {
              shiny::tags$li(
                shiny::tags$span(style = "color: #2980B9; font-weight: bold;", sub_param), ": ",
                if (is.atomic(param_value[[sub_param]])) {
                  as.character(param_value[[sub_param]])
                } else {
                  # TODO check if processing settings constructors can be simplified
                  "Complex structure"
                }
              )
            })
          )
        } else {
          "Complex structure"
        }
        
        shiny::tagList(
          shiny::tags$dt(shiny::tags$strong(param_name)),
          shiny::tags$dd(value_display)
        )
      }
  
      # Separate parameters from other details
      param_names <- names(settings$parameters)
      other_names <- setdiff(names(settings), c("parameters"))
  
      shiny::tags$div(
        class = "function-details",
        shiny::tags$dl(
          lapply(other_names, function(param) {
            create_parameter_ui(param, settings[[param]])
          })
        ),
        shiny::tags$div(
          class = "parameters-section",
          shiny::tags$h4("Parameters"),
          shiny::tags$dl(
            lapply(param_names, function(param) {
              create_parameter_ui(param, settings$parameters[[param]])
            })
          )
        )
      )
    })
  })
}
