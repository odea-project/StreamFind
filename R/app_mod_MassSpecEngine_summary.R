
.mod_MassSpecEngine_summary_UI <- function(id, engine) {
  ns <- NS(id)
  levels <- unique(engine$get_spectra_level())
  colorby_spectra <- c("analyses", "replicates", "polarities", "analyses+polarities", "replicates+polarities")
  colorby_chromatograms <- c(
    "targets", "analyses", "replicates", "polarities",
    "targets+polarities", "analyses+polarities", "replicates+polarities", "targets+analyses", "targets+replicates0"
  )
  tagList(
    box(title = "Spectra summary", width = 12, solidHeader = TRUE, withSpinner(uiOutput(ns("summary_plot_ui")), color = "black")),
    column(12,
      div(style = "display: flex; align-items: center;",
        div(style = "margin-left: 20px;", selectInput(ns("summary_plot_type"), label = "Type", choices = c("TIC", "BPC"), selected = "TIC", width = 100)),
        div(style = "margin-left: 20px;", selectInput(ns("summary_plot_colorby"), label = "Color by", choices = colorby_spectra, selected = "analyses", width = 200)),
        div(style = "margin-left: 20px;", sliderInput(ns("summary_plot_level"), label = "MS levels", min = min(levels), max = max(levels), value = levels, step = 1, width = 200)),
        div(style = "margin-left: 20px;", checkboxInput(ns("summary_plot_interactive"), label = "Interactive", value = FALSE, width = 100)),
        div(style =  "margin-bottom: 20px;")
      )
    ),
    box(title = "Chromatograms", width = 12, solidHeader = TRUE, withSpinner(uiOutput(ns("chrom_plot_ui")), color = "black")),
    column(12,
      div(style = "display: flex; align-items: center;",
        div(style = "margin-left: 20px;", selectInput(ns("summary_chrom_colorby"), label = "Color by", choices = colorby_chromatograms, selected = "targets", width = 200)),
        div(style = "margin-left: 20px;", checkboxInput(ns("summary_chrom_interactive"), label = "Interactive", value = FALSE, width = 100)),
        div(style =  "margin-bottom: 20px;")
      )
    ),
  )
}

.mod_MassSpecEngine_summary_Server <- function(id, engine, analyses) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      ### out Summary plot UI -----
      output$summary_plot_ui <- renderUI({
        if (nrow(analyses()) == 0) return(NULL)
        if (input$summary_plot_interactive) {
          plotly::plotlyOutput(ns("summary_plotly"), height = "500px")
        } else {
          plotOutput(ns("summary_plot"), height = "500px")
        }
      })
      
      ### out Summary plotly -----
      output$summary_plotly <- plotly::renderPlotly({
        if (nrow(analyses()) == 0) return()
        if (!is.null(input$summary_plot_type) && input$summary_plot_interactive) {
          if (input$summary_plot_type %in% "TIC") {
            engine$plot_spectra_tic(colorBy = input$summary_plot_colorby, level = input$summary_plot_level, interactive = input$summary_plot_interactive)
          } else if (input$summary_plot_type %in% "BPC") {
            engine$plot_spectra_bpc(colorBy = input$summary_plot_colorby, level = input$summary_plot_level, interactive = input$summary_plot_interactive)
          }
        }
      })
      
      ### out Summary plot -----
      output$summary_plot <- renderPlot({
        if (nrow(analyses()) == 0) return()
        if (!is.null(input$summary_plot_type) && !input$summary_plot_interactive) {
          if (input$summary_plot_type %in% "TIC") {
            engine$plot_spectra_tic(colorBy = input$summary_plot_colorby, level = input$summary_plot_level, interactive = input$summary_plot_interactive)
          } else if (input$summary_plot_type %in% "BPC") {
            engine$plot_spectra_bpc(colorBy = input$summary_plot_colorby, level = input$summary_plot_level, interactive = input$summary_plot_interactive)
          }
        }
      })
      
      ### out Chromatograms plot UI -----
      output$chrom_plot_ui <- renderUI({
        if (nrow(analyses()) == 0) return(NULL)
        if (input$summary_chrom_interactive) {
          plotly::plotlyOutput(ns("chrom_plotly"), height = "500px")
        } else {
          plotOutput(ns("chrom_plot"), height = "500px")
        }
      })
      
      ### out Chromatograms plotly -----
      output$chrom_plotly <- plotly::renderPlotly({
        if (nrow(analyses()) == 0) return()
        engine$plot_chromatograms(colorBy = input$summary_chrom_colorby, interactive = input$summary_chrom_interactive)
      })
      
      ### out Chromatograms plot -----
      output$chrom_plot <- renderPlot({
        if (nrow(analyses()) == 0) return()
        engine$plot_chromatograms(colorBy = input$summary_chrom_colorby, interactive = input$summary_chrom_interactive)
      })
    }
  )
}