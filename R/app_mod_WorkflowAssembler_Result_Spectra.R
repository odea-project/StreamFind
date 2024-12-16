#' @noRd
S7::method(.mod_WorkflowAssembler_Result_UI, Spectra) <- function(x, id, ns) {
  ns2 <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(12, DT::dataTableOutput(ns(ns2("spectraAnalysesTable")))),
    shiny::column(12, shiny::uiOutput(ns(ns2("spectra_plot_controls")))),
    shiny::column(12, shiny::uiOutput(ns(ns2("spectra_plot_ui"))))
  )
}

#' @noRd
S7::method(.mod_WorkflowAssembler_Result_Server, Spectra) <- function(x,
                                                                      id,
                                                                      ns,
                                                                      reactive_analyses,
                                                                      reactive_volumes,
                                                                      reactive_config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)

    shinyFiles::shinyFileSave(
      input,
      "spectra_plot_save",
      roots = reactive_volumes(),
      defaultRoot = "wd",
      session = session
    )
    
    # out spectra plot UI -----
    output$spectra_plot_ui <- shiny::renderUI({
      if (length(reactive_analyses()) == 0) {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No Spectra found!"))
      } else if (!is.null(input$spectra_plot_interactive)) {
        if (input$spectra_plot_interactive) {
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns(ns2("spectra_plotly")), height = "600px"), color = "black"
          )
        } else {
          shinycssloaders::withSpinner(
            shiny::plotOutput(ns(ns2("spectra_plot")), height = "600px"), color = "black"
          )
        }
      }
    })

    # out spectra controls -----
    output$spectra_plot_controls <- shiny::renderUI({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      htmltools::div(
        style = "display: flex; align-items: center;",
        htmltools::div(
          style = "margin-left: 20px;",
          shiny::checkboxInput(
            ns(ns2("spectra_plot_interactive")),
            label = "Interactive",
            value = TRUE,
            width = 100
          )
        ),
        htmltools::div(
          style = "margin-left: 20px;",
          shiny::selectInput(
            ns(ns2("spectra_plot_colorby")),
            label = "Color by",
            choices = c("analyses", "replicates"),
            selected = "analyses",
            width = 100
          )
        ),
        htmltools::div(
          style = "margin-left: 20px;",
          shiny::checkboxInput(
            ns(ns2("spectra_plot_raw")),
            label = "Raw Spectra",
            value = TRUE,
            width = 100
          )
        ),
        htmltools::div(
          style = "margin-left: 20px;",
          shinyFiles::shinySaveButton(
            ns(ns2("spectra_plot_save")),
            "Save Plot Data (.csv)",
            "Save Plot Data (.csv)",
            filename = "spectra_spectra_data",
            filetype = list(csv = "csv")
          )
        ),
        htmltools::div(style = "margin-bottom: 20px;")
      )
    })

    # out spectra analyses table -----
    output$spectraAnalysesTable <- DT::renderDT({
      analyses <- reactive_analyses()
      if (length(analyses) == 0) {
        return()
      }
      info <- analyses$info[, c("analysis", "replicate", "blank"), with = FALSE]
      sel_analyses_names <- vapply(analyses$results$spectra$spectra, function(z) nrow(z) > 0, FALSE)
      analyses_names <- names(analyses$results$spectra$spectra[sel_analyses_names])
      if (x$is_averaged) {
        info <- info[info$replicate %in% analyses_names, ]
      } else {
        info <- info[info$analysis %in% analyses_names, ]
      }
      DT::datatable(
        info,
        selection = list(mode = "multiple", selected = 1, target = "row"),
        options = list(pageLength = 10)
      )
    })

    # out spectra plotly -----
    output$spectra_plotly <- plotly::renderPlotly({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      selected <- input$spectraAnalysesTable_rows_selected
      if (length(selected) == 0) {
        return()
      }
      plot_spectra(
        reactive_analyses(),
        analyses = selected,
        colorBy = input$spectra_plot_colorby,
        interactive = input$spectra_plot_interactive,
        useRawData = FALSE
      )
    })

    # out spectra plot -----
    output$spectra_plot <- shiny::renderPlot({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      selected <- input$spectraAnalysesTable_rows_selected
      if (length(selected) == 0) {
        return()
      }
      plot_spectra(
        reactive_analyses(),
        analyses = selected,
        colorBy = input$spectra_plot_colorby,
        interactive = input$spectra_plot_interactive,
        useRawData = FALSE
      )
    })

    # event Summary plot export -----
    shiny::observeEvent(input$spectra_plot_save, {
      if (length(reactive_analyses()) == 0) {
        msg <- "No analyses found!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      if (!is.null(input$spectra_plot_interactive)) {
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) {
          return()
        }
        csv <- get_spectra(reactive_analyses(), analyses = selected, useRawData = FALSE)
        fileinfo <- shinyFiles::parseSavePath(reactive_volumes(), input$spectra_plot_save)
        if (nrow(fileinfo) > 0) {
          write.csv(csv, fileinfo$datapath, row.names = FALSE)
          shiny::showNotification("File saved successfully!", duration = 5, type = "message")
        }
      }
    })
  })
}
