#' @noRd
.mod_WorkflowAssembler_Metadata_UI <- function(id, ns) {
  ns2 <- shiny::NS(id)
  shiny::column(
    width = 12,
    shinydashboard::box(
      title = "Metadata",
      width = 12,
      solidHeader = TRUE,
      shiny::uiOutput(ns(ns2("metadata_list")))
    ),
    shiny::column(
      width = 12,
      shiny::fluidRow(
        shiny::column(
          width = 12,
          htmltools::div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            htmltools::tags$b(style = "width: 110px; margin-bottom: 15px;", "Entry name: "),
            shiny::textInput(ns(ns2("new_header_name")), label = NULL, width = "100%")
          )
        ),
        shiny::column(
          width = 12,
          htmltools::div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            htmltools::tags$b(style = "width: 110px; margin-bottom: 15px;", "Entry value: "),
            shiny::textInput(ns(ns2("new_header_value")), label = NULL, width = "100%")
          )
        )
      ),
      shiny::actionButton(ns(ns2("add_header_button")), label = "Add Entry", width = 200),
      htmltools::div(style = "margin-bottom: 20px;")
    )
  )
}

#' @noRd
.mod_WorkflowAssembler_Metadata_Server <- function(id,
                                                   ns,
                                                   reactive_metadata,
                                                   reactive_config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)
    mandatory_metadata_names <- c("name", "author", "file", "date")
    
    # out metadata list -----
    output$metadata_list <- shiny::renderUI({
      metadata <- reactive_metadata()
      lapply(names(metadata), function(name) {
        if (name %in% mandatory_metadata_names) {
          htmltools::div(htmltools::tags$b(name), ": ", metadata[[name]], htmltools::br())
        } else {
          button_id <- paste0("button_header_del_", name)
          shiny::observeEvent(
            input[[button_id]],
            {
              metadata <- reactive_metadata()
              metadata[[name]] <- NULL
              reactive_metadata(metadata)
            },
            ignoreInit = TRUE
          )
          htmltools::div(
            shiny::actionButton(
              ns(ns2(button_id)),
              label = NULL,
              icon = shiny::icon("trash"),
              width = "40px"
            ),
            htmltools::tags$b(name), ": ", metadata[[name]], htmltools::br()
          )
        }
      })
    })
    
    # obs add header button -----
    shiny::observeEvent(input$add_header_button, {
      if (input$new_header_name != "" && input$new_header_value != "") {
        metadata <- reactive_metadata()
        metadata[[input$new_header_name]] <- input$new_header_value
        reactive_metadata(metadata)
        shiny::updateTextInput(session, "new_header_name", value = "")
        shiny::updateTextInput(session, "new_header_value", value = "")
      }
    })
  })
}
