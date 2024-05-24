
.mod_RamanEngine_summary_UI <- function(id, engine) {
  ns <- NS(id)
  tagList(
    box(title = "Spectra Summary", width = 12, solidHeader = TRUE, withSpinner(uiOutput(ns("summary_plot_ui")), color = "black")),
    column(12,
      div(style = "display: flex; align-items: center;",
        div(style = "margin-left: 20px;", selectInput(ns("summary_plot_colorby"), label = "Color by", choices = c("analyses", "replicates"), selected = "analyses", width = 100)),
        div(style = "margin-left: 20px;", checkboxInput(ns("summary_plot_interactive"), label = "Interactive", value = FALSE, width = 100)),
        div(style =  "margin-bottom: 20px;")
      )
    ),
  )
}

.mod_RamanEngine_summary_Server <- function(id, engine, analyses) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      ### out summary plot UI
      output$summary_plot_ui <- renderUI({
        if (nrow(analyses()) == 0) return()
        if (input$summary_plot_interactive) {
          plotly::plotlyOutput(ns("summary_plotly"), height = "500px")
        } else {
          plotOutput(ns("summary_plot"), height = "500px")
        }
      })
      
      ### out Summary plotly -----
      output$summary_plotly <- plotly::renderPlotly({
        if (nrow(analyses()) == 0) return()
        engine$plot_spectra(colorBy = input$summary_plot_colorby, interactive = input$summary_plot_interactive)
      })
      
      ### out Summary plot -----
      output$summary_plot <- renderPlot({
        if (nrow(analyses()) == 0) return()
        engine$plot_spectra(colorBy = input$summary_plot_colorby, interactive = input$summary_plot_interactive)
      })
    }
  )
}