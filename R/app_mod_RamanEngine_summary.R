
.mod_RamanEngine_summary_UI <- function(id, engine) {
  ns <- NS(id)
  tagList(
    box(title = "Spectra Summary", width = 12, solidHeader = TRUE, uiOutput(ns("summary_plot_ui"))),
    column(12, uiOutput(ns("summary_plot_controls")))
  )
}

.mod_RamanEngine_summary_Server <- function(id, engine, analyses, volumes) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      shinyFiles::shinyFileSave(input, "summary_plot_save", roots = volumes, defaultRoot = "wd", session = session)
      
      # out summary plot UI -----
      output$summary_plot_ui <- renderUI({
        if (nrow(analyses()) == 0) {
          div(style = "margin-top: 20px;", h4("No analyses found!"))
        } else if (!is.null(input$summary_plot_interactive)) {
          if (input$summary_plot_interactive) {
            withSpinner(plotly::plotlyOutput(ns("summary_plotly"), height = "500px"), color = "black")
          } else {
            withSpinner(plotOutput(ns("summary_plot"), height = "500px"), color = "black")
          }
        }
      })
      
      # out summary controls -----
      output$summary_plot_controls <- renderUI({
        if (nrow(analyses()) == 0) return()
        div(style = "display: flex; align-items: center;",
          div(style = "margin-left: 20px;", selectInput(ns("summary_plot_colorby"), label = "Color by", choices = c("analyses", "replicates"), selected = "analyses", width = 100)),
          div(style = "margin-left: 20px;", checkboxInput(ns("summary_plot_interactive"), label = "Interactive", value = FALSE, width = 100)),
          div(style = "margin-left: 20px;", checkboxInput(ns("summary_plot_raw"), label = "Raw Spectra", value = TRUE, width = 100)),
          div(style = "margin-left: 20px;", shinyFiles::shinySaveButton(ns("summary_plot_save"), "Save Plot Data (.csv)", "Save Plot Data (.csv)", filename = "spectra_summary_data", filetype = list(csv = "csv"))),
          div(style =  "margin-bottom: 20px;")
        )
      })
      
      # out Summary plotly -----
      output$summary_plotly <- plotly::renderPlotly({
        if (nrow(analyses()) == 0) return()
        engine$plot_spectra(colorBy = input$summary_plot_colorby, interactive = input$summary_plot_interactive, raw_spectra = input$summary_plot_raw)
      })
      
      # out Summary plot -----
      output$summary_plot <- renderPlot({
        if (nrow(analyses()) == 0) return()
        engine$plot_spectra(colorBy = input$summary_plot_colorby, interactive = input$summary_plot_interactive, raw_spectra = input$summary_plot_raw)
      })
      
      # event Summary plot export -----
      observeEvent(input$summary_plot_save, {
        if (nrow(analyses()) == 0) {
          msg <- "No analyses found!"
          shiny::showNotification(msg, duration = 5, type = "warning")
          return()
        }
        if (!is.null(input$summary_plot_interactive)) {
          csv <- engine$get_spectra(raw_spectra = input$summary_plot_raw)
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