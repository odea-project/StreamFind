#' @noRd
.mod_WorkflowAssembler_ProjectHeaders_UI <- function(id, ns) {
  ns2 <- shiny::NS(id)

  shiny::column(
    12,
    shinydashboard::box(title = "Headers", width = 12, solidHeader = TRUE, shiny::uiOutput(ns(ns2("headers_list")))),
    shiny::column(
      12,
      shiny::fluidRow(
        shiny::column(
          12,
          htmltools::div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            htmltools::tags$b(style = "width: 110px; margin-bottom: 15px;", "Header name: "),
            shiny::textInput(ns(ns2("new_header_name")), label = NULL, width = "100%")
          )
        ),
        shiny::column(
          12,
          htmltools::div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            htmltools::tags$b(style = "width: 110px; margin-bottom: 15px;", "Header value: "),
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
.mod_WorkflowAssembler_ProjectHeaders_Server <- function(id, ns, reactive_headers) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)

    mandatory_header_names <- c("name", "author", "file", "date")

    output$headers_list <- shiny::renderUI({
      headers <- reactive_headers()
      lapply(names(headers), function(name) {
        if (name %in% mandatory_header_names) {
          htmltools::div(htmltools::tags$b(name), ": ", headers[[name]], htmltools::br())
        } else {
          button_id <- paste0("button_header_del_", name)
          shiny::observeEvent(input[[button_id]],
            {
              headers <- reactive_headers()
              headers[[name]] <- NULL
              reactive_headers(headers)
            },
            ignoreInit = TRUE
          )
          htmltools::div(
            shiny::actionButton(ns(ns2(button_id)), label = NULL, icon = shiny::icon("trash"), width = "40px"),
            htmltools::tags$b(name), ": ", headers[[name]], htmltools::br()
          )
        }
      })
    })

    shiny::observeEvent(input$add_header_button, {
      if (input$new_header_name != "" && input$new_header_value != "") {
        headers <- reactive_headers()
        headers[[input$new_header_name]] <- input$new_header_value
        reactive_headers(headers)
        shiny::updateTextInput(session, "new_header_name", value = "")
        shiny::updateTextInput(session, "new_header_value", value = "")
      }
    })
  })
}
