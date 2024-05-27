
.mod_MassSpecEngine_summary_UI <- function(id, engine) {
  ns <- NS(id)
  tagList(
    box(title = "Spectra summary", width = 12, solidHeader = TRUE, uiOutput(ns("summary_plot_ui"))),
    column(12, uiOutput(ns("summary_plot_controls"))),
    box(title = "Chromatograms", width = 12, solidHeader = TRUE, uiOutput(ns("chrom_plot_ui"))),
    column(12, uiOutput(ns("chrom_plot_controls")))
  )
}

.mod_MassSpecEngine_summary_Server <- function(id, engine, analyses, volumes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shinyFiles::shinyFileSave(input, "summary_plot_save", roots = volumes, defaultRoot = "wd", session = session)
    shinyFiles::shinyFileSave(input, "chrom_plot_save", roots = volumes, defaultRoot = "wd", session = session)
    
    has_spectra <- reactiveVal(nrow(engine$get_spectra_headers()) > 0)
    has_chromatograms <- reactiveVal(nrow(engine$get_chromatograms_headers()) > 0)
    
    levels <- unique(engine$get_spectra_level())
    colorby_spectra <- c("analyses", "replicates", "polarities", "analyses+polarities", "replicates+polarities")
    colorby_chromatograms <- c(
      "targets", "analyses", "replicates", "polarities",
      "targets+polarities", "analyses+polarities", "replicates+polarities", "targets+analyses", "targets+replicates"
    )
    
    # out Summary plot UI -----
    output$summary_plot_ui <- renderUI({
      if (nrow(analyses()) == 0) {
        div(style = "margin-top: 20px;", h4("No analyses found!"))
      } else if (has_spectra()) {
        if (!is.null(input$summary_plot_interactive)) {
          if (input$summary_plot_interactive) {
            withSpinner(plotly::plotlyOutput(ns("summary_plotly"), height = "500px"), color = "black")
          } else {
            withSpinner(plotOutput(ns("summary_plot"), height = "500px"), color = "black")
          }
        }
      } else {
        div(style = "margin-top: 20px;", h4("No spectra found!"))
      }
    })
    
    # out Summary controls -----
    output$summary_plot_controls <- renderUI({
      if (nrow(analyses()) == 0) return()
      if (has_spectra()) {
        div(style = "display: flex; align-items: center;",
            div(style = "margin-left: 20px;", selectInput(ns("summary_plot_type"), label = "Type", choices = c("TIC", "BPC"), selected = "TIC", width = 100)),
            div(style = "margin-left: 20px;", selectInput(ns("summary_plot_colorby"), label = "Color by", choices = colorby_spectra, selected = "analyses", width = 200)),
            div(style = "margin-left: 20px;", sliderInput(ns("summary_plot_level"), label = "MS levels", min = min(levels), max = max(levels), value = levels, step = 1, width = 200)),
            div(style = "margin-left: 20px;", checkboxInput(ns("summary_plot_interactive"), label = "Interactive", value = FALSE, width = 100)),
            div(style = "margin-left: 20px;", shinyFiles::shinySaveButton(ns("summary_plot_save"), "Save Plot Data (.csv)", "Save Plot Data (.csv)", filename = "spectra_summary_data", filetype = list(csv = "csv"))),
            div(style =  "margin-bottom: 20px;")
        )
      }
    })
    
    # out Summary plotly -----
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
    
    # out Summary plot -----
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
    
    # event Summary plot export -----
    observeEvent(input$summary_plot_save, {
      if (nrow(analyses()) == 0) {
        msg <- "No analyses found!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      if (!is.null(input$summary_plot_type)) {
        if (input$summary_plot_type %in% "TIC") {
          csv <- engine$get_spectra_tic(level = input$summary_plot_level)
        } else if (input$summary_plot_type %in% "BPC") {
          csv <- engine$get_spectra_bpc(level = input$summary_plot_level)
        }
        fileinfo <- shinyFiles::parseSavePath(volumes, input$summary_plot_save)
        if (nrow(fileinfo) > 0) {
          write.csv(csv, fileinfo$datapath, row.names = FALSE)
          shiny::showNotification("File saved successfully!", duration = 5, type = "message")
        }
      }
    })
    
    # out Chromatograms plot UI -----
    output$chrom_plot_ui <- renderUI({
      if (nrow(analyses()) == 0) {
        div(style = "margin-top: 20px;", h4("No analyses found!"))
      } else if (has_chromatograms()) {
        if (!is.null(input$summary_chrom_interactive)) {
          if (input$summary_chrom_interactive) {
            plotly::plotlyOutput(ns("chrom_plotly"), height = "500px")
          } else {
            plotOutput(ns("chrom_plot"), height = "500px")
          }
        }
      } else {
        div(style = "margin-top: 20px;", h4("No chromatograms found!"))
      }
    })
    
    # out Chromatograms controls -----
    output$chrom_plot_controls <- renderUI({
      if (nrow(analyses()) == 0) return()
      if (has_chromatograms()) {
        div(style = "display: flex; align-items: center;",
            div(style = "margin-left: 20px;", selectInput(ns("summary_chrom_colorby"), label = "Color by", choices = colorby_chromatograms, selected = "targets", width = 200)),
            div(style = "margin-left: 20px;", checkboxInput(ns("summary_chrom_interactive"), label = "Interactive", value = FALSE, width = 100)),
            div(style = "margin-left: 20px;", shinyFiles::shinySaveButton(ns("chrom_plot_save"), "Save Plot Data (.csv)", "Save Plot Data (.csv)", filename = "chrom_data", filetype = list(csv = "csv"))),
            div(style =  "margin-bottom: 20px;")
        )
      }
    })
    
    # out Chromatograms plotly -----
    output$chrom_plotly <- plotly::renderPlotly({
      if (nrow(analyses()) == 0) return()
      engine$plot_chromatograms(colorBy = input$summary_chrom_colorby, interactive = input$summary_chrom_interactive)
    })
    
    # out Chromatograms plot -----
    output$chrom_plot <- renderPlot({
      if (nrow(analyses()) == 0) return()
      engine$plot_chromatograms(colorBy = input$summary_chrom_colorby, interactive = input$summary_chrom_interactive)
    })
    
    # event Chromatograms plot export
    observeEvent(input$chrom_plot_save, {
      if (nrow(analyses()) == 0) {
        msg <- "No analyses found!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      csv <- engine$get_chromatograms()
      fileinfo <- shinyFiles::parseSavePath(volumes, input$chrom_plot_save)
      if (nrow(fileinfo) > 0) {
        write.csv(csv, fileinfo$datapath, row.names = FALSE)
        shiny::showNotification("File saved successfully!", duration = 5, type = "message")
      }
    })
  })
}