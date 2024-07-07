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
      shiny::column(6, 
        shiny::selectInput(ns("workflow_settings_select"), "Select Processing Settings", choices = c("a", "b"), multiple = FALSE),
        shiny::uiOutput(ns("selected_function_details"))
      )
    ),
    # CSS
    htmltools::tags$style(HTML("
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
        background-color: #f9f9f9;
        border: 1px solid #ddd;
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
    "))
  )
}

#' @title .mod_workflow_Server
#' 
#' @description Shiny module server for workflow tab.
#' 
#' @noRd
#'
.mod_workflow_Server <- function(id, engine, reactive_workflow) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    available_processing_methods <- engine$processing_methods()$name
    StreamFind_env <- as.environment("package:StreamFind")
    Settings_functions <- ls(envir = StreamFind_env, pattern = "^Settings_")
    Settings_functions <- Settings_functions[sapply(Settings_functions, function(x) is.function(get(x, envir = .GlobalEnv)))]
    Settings_functions <- Settings_functions[sapply(Settings_functions, function(x) any(sapply(available_processing_methods, function(y) grepl(y, x))))]
    Settings_functions_short <- gsub("Settings_", "", Settings_functions)
    names(Settings_functions_short) <- Settings_functions
    
    Settings_functions_list <- as.list(Settings_functions_short)
    
    reactive_workflow_names <- shiny::reactiveVal(character(0))
    selected_function <- shiny::reactiveVal(NULL)
    
    output$workflow_settings <- shiny::renderUI({
      number_of_settings <- length(Settings_functions_short)
      
      #list of custom HTML for each function
      labels <- lapply(seq_along(Settings_functions_short), function(i) {
        name <- Settings_functions_short[i]
        tagList(
          div(class = "workflow-item",
            shiny::actionButton(ns(paste0("workflow_del_", i)), "X", class = "custom-buttonred", style = "margin-right: 10px;"),
            shiny::span(name),
            shiny::actionButton(ns(paste0("workflow_edit_", i)), "Details", class = "custom-button")
          )
        )
      })
      
      shinydashboard::box(width = 12, title = "Workflow", class = "workflow-box",
        shiny::column(12, htmltools::h3("Workflow Settings")),
        shiny::column(12,
          sortable::rank_list(
            text = "Drag to order",
            labels = labels,
            input_id = ns("rank_workflow_names")
          )
        )
      )
    })
    
    lapply(Settings_functions_short, function(name) {
      observeEvent(input[[paste0("function_link_", name)]], {
        selected_function(name)
      }, ignoreInit = TRUE)
    })
    
    lapply(seq_len(length(Settings_functions_short)), function(i) {
      observeEvent(input[[paste0("workflow_edit_", i)]], {
        selected_function(Settings_functions_short[i])
      }, ignoreInit = TRUE)
    })
    
# render details
output$selected_function_details <- shiny::renderUI({
  req(selected_function())
  function_name <- selected_function()
  function_details <- get(paste0("Settings_", function_name), envir = .GlobalEnv)
  
  htmltools::tagList(
    shiny::h3(paste("Details of", function_name), style = "color: #3498DB; margin-bottom: 20px;"),
    shiny::uiOutput(ns("function_code"))
  )
})

# Function details
output$function_code <- shiny::renderUI({
  req(selected_function())
  function_name <- selected_function()
  function_details <- get(paste0("Settings_", function_name), envir = .GlobalEnv)
  settings <- function_details()
  
  create_parameter_ui <- function(param_name, param_value) {
    value_display <- if(is.atomic(param_value) && length(param_value) == 1) {
      as.character(param_value)
    } else if(is.list(param_value)) {
      shiny::tags$ul(
        style = "list-style-type: none; padding-left: 0;",
        lapply(names(param_value), function(sub_param) {
          shiny::tags$li(
            shiny::tags$span(style = "color: #2980B9; font-weight: bold;", sub_param), ": ",
            if(is.atomic(param_value[[sub_param]])) {
              as.character(param_value[[sub_param]])
            } else {
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
})})}
