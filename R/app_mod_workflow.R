.mod_workflow_UI <- function(id) {
  ns <- NS(id)
  tagList(
    column(6, uiOutput(ns("workflow_settings"))),
    column(6, selectInput(ns("workflow_settings_select"), "Select Processing Settings", choices = c("a", "b"), multiple = FALSE))
  )
}

.mod_workflow_Server <- function(id, engine, reactive_workflow) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    available_processing_methods <- engine$processing_methods()$name
    StreamFind_env <- as.environment("package:StreamFind")
    Settings_functions <- ls(envir = StreamFind_env, pattern = "^Settings_")
    Settings_functions <- Settings_functions[sapply(Settings_functions, function(x) is.function(get(x, envir = .GlobalEnv)))]
    Settings_functions <- Settings_functions[sapply(Settings_functions, function(x) any(sapply(available_processing_methods, function(y) grepl(y, x, ))))]
    Settings_functions_short <- gsub("Settings_", "", Settings_functions)
    names(Settings_functions_short) <- Settings_functions
    
    Settings_functions_list <- as.list(Settings_functions_short)
    
    reactive_workflow_names <- reactiveVal(character(0))
    
    rank_list_basic <- sortable::rank_list(
      text = "Drag to order",
      labels = Settings_functions_short,
      input_id = "rank_workflow_names"
    )
    
    output$workflow_settings <- renderUI({
      
      number_of_settings <- length(Settings_functions_short)
      button_id <- seq_len(number_of_settings)
      button_id_del <- paste0("workflow_del_", button_id)
      button_id_edit <- paste0("workflow_edit_", button_id)
      
      box(width = 12, title = "Workflow",
        column(12, h3("   ")),
        column(1,
          div(
            div(style = "margin-bottom: 51px; margin-left: 0px;"),
            lapply(1:number_of_settings, function(i) {
              div(style = "height: 42px; display: flex; align-items: center; justify-content: flex-end; margin-left: 0px; margin-right: 0px;", actionButton(ns(button_id_del[i]), "X"))
            })
          )
        ),
        column(10, rank_list_basic),
        column(1,
          tagList(
            div(style = "margin-bottom: 51px;"),
            lapply(1:number_of_settings, function(i) {
              div(style = "height: 42px; display: flex; align-items: center; justify-content: flex-start; margin-left: 0px; margin-right: 0px;", actionButton(ns(button_id_edit[i]), "Edit"))
            })
          )
        ),
      )
    })
    
  })
}