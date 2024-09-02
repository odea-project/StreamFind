
#' @noRd
.mod_WorkflowAssembler_Explorer_Raman_UI <- function(id, engine) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabBox(width = 12, height = "1080px",
    shiny::tabPanel("Spectra",
      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput(ns("summary_plot_controls"))),
        shiny::column(12, shiny::uiOutput(ns("summary_plot_ui"))),
        shiny::column(12, DT::dataTableOutput(ns("spectraAnalysesTable")))
      )
    )
  )
}

#' @noRd
.mod_WorkflowAssembler_Explorer_Raman_Server <- function(id, engine, analyses, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      shinyFiles::shinyFileSave(input, "summary_plot_save", roots = volumes, defaultRoot = "wd", session = session)
      
      # out summary plot UI -----
      output$summary_plot_ui <- shiny::renderUI({
        if (nrow(analyses()) == 0) {
          htmltools::div(style = "margin-top: 20px;", htmltools::h4("No analyses found!"))
        } else if (!is.null(input$summary_plot_interactive)) {
          if (input$summary_plot_interactive) {
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns("summary_plotly"), height = "600px"), color = "black")
          } else {
            shinycssloaders::withSpinner(shiny::plotOutput(ns("summary_plot"), height = "600px"), color = "black")
          }
        }
      })
      
      # out summary controls -----
      output$summary_plot_controls <- shiny::renderUI({
        if (nrow(analyses()) == 0) return()
        htmltools::div(style = "display: flex; align-items: center;",
          htmltools::div(style = "margin-left: 20px;", shiny::checkboxInput(ns("summary_plot_interactive"), label = "Interactive", value = TRUE, width = 100)),
          htmltools::div(style = "margin-left: 20px;", shiny::selectInput(ns("summary_plot_colorby"), label = "Color by", choices = c("analyses", "replicates"), selected = "analyses", width = 100)),
          htmltools::div(style = "margin-left: 20px;", shiny::checkboxInput(ns("summary_plot_raw"), label = "Raw Spectra", value = TRUE, width = 100)),
          htmltools::div(style = "margin-left: 20px;", shinyFiles::shinySaveButton(ns("summary_plot_save"), "Save Plot Data (.csv)", "Save Plot Data (.csv)", filename = "spectra_summary_data", filetype = list(csv = "csv"))),
          htmltools::div(style =  "margin-bottom: 20px;")
        )
      })
      
      # out spectra analyses table -----
      output$spectraAnalysesTable <- DT::renderDT({
        DT::datatable(
          analyses()[, c("analysis", "replicate", "blank"), with = FALSE],
          selection = list(mode = 'multiple', selected = 1, target = 'row'),
          options = list(pageLength = 10)
        )
      })
      
      # out Summary plotly -----
      output$summary_plotly <- plotly::renderPlotly({
        if (nrow(analyses()) == 0) return()
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) return()
        engine$plot_spectra(analyses = selected, colorBy = input$summary_plot_colorby, interactive = input$summary_plot_interactive, raw_spectra = input$summary_plot_raw)
      })
      
      # out Summary plot -----
      output$summary_plot <- shiny::renderPlot({
        if (nrow(analyses()) == 0) return()
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) return()
        engine$plot_spectra(analyses = selected, colorBy = input$summary_plot_colorby, interactive = input$summary_plot_interactive, raw_spectra = input$summary_plot_raw)
      })
      
      # event Summary plot export -----
      shiny::observeEvent(input$summary_plot_save, {
        if (nrow(analyses()) == 0) {
          msg <- "No analyses found!"
          shiny::showNotification(msg, duration = 5, type = "warning")
          return()
        }
        if (!is.null(input$summary_plot_interactive)) {
          selected <- input$spectraAnalysesTable_rows_selected
          if (length(selected) == 0) return()
          csv <- engine$get_spectra(analyses = selected, raw_spectra = input$summary_plot_raw)
          fileinfo <- shinyFiles::parseSavePath(volumes, input$summary_plot_save)
          if (nrow(fileinfo) > 0) {
            write.csv(csv, fileinfo$datapath, row.names = FALSE)
            shiny::showNotification("File saved successfully!", duration = 5, type = "message")
          }
        }
      })
    }
  )
}